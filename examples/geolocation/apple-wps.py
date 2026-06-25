#!/usr/bin/env python3
"""apple-wps -- find your location from nearby WiFi via Apple's positioning service.

  CAVEAT, READ FIRST
  ------------------
  This queries gs-loc.apple.com, an UNDOCUMENTED Apple endpoint, using a
  reverse-engineered protobuf protocol.  It is not a public or supported API,
  and using it may be contrary to Apple's terms of service.  It is provided as
  an example only -- decide for yourself whether to use it.  It needs no API
  key and no account.

An example adapter for wttrin's `wttrin-geolocation-command'.  It scans nearby
WiFi access points, asks Apple for their coordinates, averages the located ones
weighted by signal strength to estimate your position, and prints JSON on stdout:

    {"lat": 41.3222, "lng": -71.8113, "address": "Westerly, Rhode Island, USA"}

wttrin reads `lat' and `lng' to fetch weather, and shows `address' on the
buffer's "Location:" line.

Note the difference from Google's API: Google solves your position from a scan
and returns one point.  Apple returns the location of *each access point*, so
this script does the averaging itself.

Requirements:
  - Python 3 (standard library only; the protobuf is hand-encoded below).
  - nmcli (NetworkManager) for the WiFi scan.  Replace `scan_wifi' for a system
    without NetworkManager; it is the only platform-specific part.

Wire it into Emacs:

    (setq wttrin-geolocation-command "/path/to/apple-wps.py")

On any failure this exits non-zero, so wttrin falls back to its IP provider.
"""

import json
import re
import struct
import subprocess
import sys
import urllib.error
import urllib.request
from typing import NoReturn

WLOC_URL = "https://gs-loc.apple.com/clls/wloc"
NOMINATIM_URL = "https://nominatim.openstreetmap.org/reverse?lat={lat}&lon={lng}&format=jsonv2"
# Apple's service inspects the User-Agent; a locationd-like string is expected.
APPLE_USER_AGENT = "locationd/1753.17 CFNetwork/889.9 Darwin/17.2.0"
NOMINATIM_USER_AGENT = "wttrin-apple-wps-example/1.0"

MAX_ACCESS_POINTS = 12          # cap the request size; the strongest APs suffice
APPLE_UNKNOWN = -18000000000    # value Apple returns for an access point it can't place


def fail(message) -> NoReturn:
    """Print MESSAGE to stderr and exit non-zero (wttrin falls back to IP)."""
    print(f"apple-wps: {message}", file=sys.stderr)
    sys.exit(1)


# --------------------------------------------------------------------------
# Minimal protobuf -- only the few wire-format pieces this request and response
# need, so there is no dependency on a protobuf library.
# --------------------------------------------------------------------------

def encode_varint(value):
    """Encode a non-negative integer as a protobuf base-128 varint."""
    out = bytearray()
    while True:
        seven_bits = value & 0x7F
        value >>= 7
        out.append(seven_bits | (0x80 if value else 0))
        if not value:
            return bytes(out)


def read_varint(buf, offset):
    """Decode a varint from BUF starting at OFFSET; return (value, next_offset)."""
    result = shift = 0
    while True:
        byte = buf[offset]
        offset += 1
        result |= (byte & 0x7F) << shift
        if not byte & 0x80:
            return result, offset
        shift += 7


def tag(field_number, wire_type):
    """Encode a protobuf field tag (field number + wire type)."""
    return encode_varint((field_number << 3) | wire_type)


def length_delimited(field_number, data):
    """Encode a length-delimited field (wire type 2): tag, length, then DATA."""
    return tag(field_number, 2) + encode_varint(len(data)) + data


def as_signed_64(value):
    """Interpret an unsigned 64-bit varint as a two's-complement signed integer."""
    return value - (1 << 64) if value >= (1 << 63) else value


# --------------------------------------------------------------------------
# Request and response
# --------------------------------------------------------------------------

def build_request(bssids):
    """Build the binary Apple WLOC request for a list of BSSID strings.

    The protobuf is an AppleWLoc message: field 2 is a repeated WifiDevice, and
    each WifiDevice has field 1 = the BSSID string.  Fields 3 and 4 are small
    integers the real client sends (an unknown flag and a single-result flag,
    both left 0 here so Apple returns every access point it can).  The protobuf
    is wrapped in a fixed frame: a start marker, three length-prefixed strings
    (locale, the locationd identifier, a client version), and a 2-byte
    big-endian payload length.
    """
    wifi_devices = b"".join(
        length_delimited(2, length_delimited(1, bssid.encode()))
        for bssid in bssids)
    payload = (wifi_devices
               + tag(3, 0) + encode_varint(0)
               + tag(4, 0) + encode_varint(0))
    frame = (b"\x00\x01\x00\x05en_US"
             b"\x00\x13com.apple.locationd"
             b"\x00\x0a8.1.12B411"
             b"\x00\x00\x00\x01\x00\x00")
    return frame + struct.pack(">H", len(payload)) + payload


def query_apple(request_bytes):
    """POST REQUEST_BYTES to Apple's WLOC endpoint and return the raw response."""
    request = urllib.request.Request(
        WLOC_URL, data=request_bytes,
        headers={"User-Agent": APPLE_USER_AGENT,
                 "Content-Type": "application/x-www-form-urlencoded"})
    try:
        with urllib.request.urlopen(request, timeout=20) as response:
            return response.read()
    except urllib.error.HTTPError as error:
        fail(f"Apple WLOC error {error.code}")
    except urllib.error.URLError as error:
        fail(f"network error: {error.reason}")


def parse_response(raw):
    """Parse Apple's WLOC response into {normalized_bssid: (lat, lng)}.

    The body begins with a 10-byte prefix before the AppleWLoc protobuf.  We walk
    its fields, decoding each WifiDevice (field 2) into a BSSID and a location.
    """
    buf = raw[10:]
    located = {}
    offset, end = 0, len(buf)
    while offset < end:
        key, offset = read_varint(buf, offset)
        field_number, wire_type = key >> 3, key & 0x7
        if wire_type == 2:
            length, offset = read_varint(buf, offset)
            chunk = buf[offset:offset + length]
            offset += length
            if field_number == 2:
                bssid, coords = parse_wifi_device(chunk)
                if bssid and coords:
                    located[normalize_bssid(bssid)] = coords
        elif wire_type == 0:
            _, offset = read_varint(buf, offset)   # skip a varint we don't use
        else:
            break                                  # unexpected wire type; stop
    return located


def parse_wifi_device(buf):
    """Decode a WifiDevice message: field 1 is the BSSID, field 2 the location."""
    bssid = None
    coords = None
    offset, end = 0, len(buf)
    while offset < end:
        key, offset = read_varint(buf, offset)
        field_number, wire_type = key >> 3, key & 0x7
        if wire_type == 2:
            length, offset = read_varint(buf, offset)
            chunk = buf[offset:offset + length]
            offset += length
            if field_number == 1:
                bssid = chunk.decode(errors="replace")
            elif field_number == 2:
                coords = parse_location(chunk)
        elif wire_type == 0:
            _, offset = read_varint(buf, offset)
        else:
            break
    return bssid, coords


def parse_location(buf):
    """Decode a Location message: field 1 latitude, field 2 longitude.

    Both are int64 scaled by 1e8.  Apple returns APPLE_UNKNOWN for an access
    point it cannot place; those are dropped.  Returns (lat, lng) or None.
    """
    latitude = longitude = None
    offset, end = 0, len(buf)
    while offset < end:
        key, offset = read_varint(buf, offset)
        field_number, wire_type = key >> 3, key & 0x7
        if wire_type == 0:
            value, offset = read_varint(buf, offset)
            value = as_signed_64(value)
            if field_number == 1:
                latitude = value
            elif field_number == 2:
                longitude = value
        elif wire_type == 2:
            length, offset = read_varint(buf, offset)
            offset += length
        else:
            break
    if latitude is None or longitude is None:
        return None
    if latitude == APPLE_UNKNOWN or longitude == APPLE_UNKNOWN:
        return None
    return latitude * 1e-8, longitude * 1e-8


# --------------------------------------------------------------------------
# WiFi scan and position estimate
# --------------------------------------------------------------------------

def normalize_bssid(bssid):
    """Canonicalize a BSSID to lowercase, zero-padded octets, for reliable matching."""
    return ":".join(octet.rjust(2, "0").lower() for octet in bssid.split(":"))


def scan_wifi():
    """Return a list of (normalized_bssid, signal_quality) for visible access points.

    Uses nmcli; SIGNAL is a 0-100 quality percentage used here only to weight the
    position average (stronger means nearer).  Replace this function to support a
    system without NetworkManager.
    """
    try:
        completed = subprocess.run(
            ["nmcli", "-t", "-f", "SIGNAL,BSSID",
             "device", "wifi", "list", "--rescan", "yes"],
            capture_output=True, text=True, timeout=20, check=True)
    except FileNotFoundError:
        fail("nmcli not found; replace scan_wifi for your system")
    except subprocess.SubprocessError as error:
        fail(f"wifi scan failed: {error}")

    access_points = []
    for line in completed.stdout.splitlines():
        # nmcli -t separates fields with colons and backslash-escapes the colons
        # inside the BSSID; split on the first unescaped colon, then unescape.
        parts = [p.replace("\\:", ":")
                 for p in re.split(r"(?<!\\):", line, maxsplit=1)]
        if len(parts) != 2:
            continue
        signal, bssid = parts
        bssid = normalize_bssid(bssid)
        if not re.fullmatch(r"(?:[0-9a-f]{2}:){5}[0-9a-f]{2}", bssid):
            continue
        try:
            quality = int(signal)
        except ValueError:
            continue
        access_points.append((bssid, quality))
    return access_points


def signal_weighted_centroid(access_points, located):
    """Average the coordinates of access points we see and Apple located.

    Each is weighted by its signal quality, so nearer access points pull the
    estimate toward them.  Returns (lat, lng) or None when none matched.
    """
    total_weight = latitude_sum = longitude_sum = 0.0
    for bssid, weight in access_points:
        coords = located.get(bssid)
        if coords:
            latitude_sum += coords[0] * weight
            longitude_sum += coords[1] * weight
            total_weight += weight
    if total_weight == 0:
        return None
    return latitude_sum / total_weight, longitude_sum / total_weight


def reverse_geocode(lat, lng):
    """Return a human-readable address for LAT, LNG, or None on any failure.

    Best-effort via OpenStreetMap Nominatim, which requires a descriptive
    User-Agent and rate-limits heavy use.
    """
    request = urllib.request.Request(
        NOMINATIM_URL.format(lat=lat, lng=lng),
        headers={"User-Agent": NOMINATIM_USER_AGENT})
    try:
        with urllib.request.urlopen(request, timeout=20) as response:
            return json.load(response).get("display_name")
    except (urllib.error.URLError, ValueError):
        return None


def main():
    access_points = scan_wifi()
    if len(access_points) < 2:
        fail(f"only {len(access_points)} access point(s) visible; need at least 2")

    # Query the strongest access points; Apple also returns nearby ones it knows.
    access_points.sort(key=lambda ap: ap[1], reverse=True)
    query_bssids = [bssid for bssid, _ in access_points[:MAX_ACCESS_POINTS]]

    located = parse_response(query_apple(build_request(query_bssids)))
    position = signal_weighted_centroid(access_points, located)
    if position is None:
        # If this happens with many APs visible, Apple may want the BSSIDs in a
        # different form (some clients strip leading zeros from each octet rather
        # than padding them); adjust normalize_bssid and retry.
        fail("Apple located none of the visible access points")

    lat, lng = position
    result = {"lat": round(lat, 7), "lng": round(lng, 7)}
    address = reverse_geocode(lat, lng)
    if address:
        result["address"] = address
    print(json.dumps(result))


if __name__ == "__main__":
    main()
