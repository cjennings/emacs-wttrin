#!/usr/bin/env python3
"""google-geolocate -- find your location from nearby WiFi via the Google Geolocation API.

An example adapter for wttrin's `wttrin-geolocation-command'.  It scans nearby
WiFi access points, sends them to Google's Geolocation API, and prints the
resulting coordinates (plus a reverse-geocoded address) as JSON on stdout:

    {"lat": 41.3222, "lng": -71.8113, "accuracy_m": 25.0, "address": "Westerly, Rhode Island, USA"}

wttrin reads `lat' and `lng' to fetch weather, and shows `address' on the
buffer's "Location:" line.

Why this beats IP geolocation: a real WiFi scan fed to Google's database
resolves to street level, where IP geolocation only finds your network's exit
point -- often the wrong city on a VPN or a cellular hotspot.

Requirements:
  - Python 3 (standard library only).
  - nmcli (NetworkManager) for the WiFi scan.  On a system without
    NetworkManager, replace `scan_wifi' with your platform's scanner.
  - A Google API key with the Geolocation API enabled, in the environment
    variable GOOGLE_GEOLOCATION_API_KEY.  See the README for setup and pricing.

Wire it into Emacs:

    (setq wttrin-geolocation-command "/path/to/google-geolocate.py")

On any failure this exits non-zero, so wttrin falls back to its IP provider.
"""

import json
import os
import re
import subprocess
import sys
import urllib.error
import urllib.request
from typing import NoReturn

GEOLOCATE_URL = "https://www.googleapis.com/geolocation/v1/geolocate?key={key}"
NOMINATIM_URL = "https://nominatim.openstreetmap.org/reverse?lat={lat}&lon={lng}&format=jsonv2"
USER_AGENT = "wttrin-google-geolocate-example/1.0"


AUTHINFO_MACHINE = "googleapis.com"   # the "machine" entry this looks for in authinfo


def fail(message) -> NoReturn:
    """Print MESSAGE to stderr and exit non-zero (wttrin falls back to IP)."""
    print(f"google-geolocate: {message}", file=sys.stderr)
    sys.exit(1)


def read_api_key():
    """Return the Google API key, or None if it cannot be found.

    Checks the environment variable GOOGLE_GEOLOCATION_API_KEY first, then
    ~/.authinfo.gpg (the encrypted store Emacs's auth-source uses).  See the
    README for the authinfo line format.
    """
    key = os.environ.get("GOOGLE_GEOLOCATION_API_KEY")
    if key:
        return key
    return read_key_from_authinfo()


def read_key_from_authinfo():
    """Return the password for the AUTHINFO_MACHINE entry in ~/.authinfo.gpg, or None.

    Decrypts the file with gpg (gpg-agent supplies the passphrase) and reads a
    netrc-style line: machine <host> login <user> password <secret>.
    """
    path = os.path.expanduser("~/.authinfo.gpg")
    if not os.path.exists(path):
        return None
    try:
        decrypted = subprocess.run(
            ["gpg", "--quiet", "--batch", "--decrypt", path],
            capture_output=True, text=True, timeout=30, check=True).stdout
    except (FileNotFoundError, subprocess.SubprocessError):
        return None
    for line in decrypted.splitlines():
        tokens = line.split()
        if "machine" in tokens and "password" in tokens:
            pairs = dict(zip(tokens[::2], tokens[1::2]))
            if pairs.get("machine") == AUTHINFO_MACHINE:
                return pairs.get("password")
    return None


def scan_wifi():
    """Return a list of {"macAddress", "signalStrength"} for visible access points.

    Uses nmcli.  Its SIGNAL column is a 0-100 quality percentage; NetworkManager
    maps quality = 2 * (dBm + 100), so dBm = quality / 2 - 100, which is the unit
    Google's API expects.  Replace this function to support a non-NetworkManager
    system (for example macOS via CoreWLAN); it is the only platform-specific part.
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
        # In nmcli -t output, fields are colon-separated and literal colons
        # inside a field (the BSSID) are backslash-escaped.  Splitting on the
        # first unescaped colon separates SIGNAL from the BSSID, then we unescape.
        parts = [p.replace("\\:", ":")
                 for p in re.split(r"(?<!\\):", line, maxsplit=1)]
        if len(parts) != 2:
            continue
        signal, bssid = parts
        if not re.fullmatch(r"(?:[0-9A-Fa-f]{2}:){5}[0-9A-Fa-f]{2}", bssid):
            continue
        try:
            quality = int(signal)
        except ValueError:
            continue
        access_points.append({"macAddress": bssid,
                              "signalStrength": quality // 2 - 100})
    return access_points


def google_geolocate(access_points, key):
    """Ask Google for a position from ACCESS_POINTS; return (lat, lng, accuracy)."""
    body = json.dumps({"considerIp": False,
                       "wifiAccessPoints": access_points}).encode()
    request = urllib.request.Request(
        GEOLOCATE_URL.format(key=key), data=body,
        headers={"Content-Type": "application/json", "User-Agent": USER_AGENT})
    try:
        with urllib.request.urlopen(request, timeout=20) as response:
            data = json.load(response)
    except urllib.error.HTTPError as error:
        detail = error.read().decode(errors="replace")[:200]
        fail(f"Google API error {error.code}: {detail}")
    except urllib.error.URLError as error:
        fail(f"network error: {error.reason}")

    location = data.get("location", {})
    return location.get("lat"), location.get("lng"), data.get("accuracy")


def reverse_geocode(lat, lng):
    """Return a human-readable address for LAT, LNG, or None on any failure.

    Best-effort via OpenStreetMap Nominatim, which requires a descriptive
    User-Agent and rate-limits heavy use.
    """
    request = urllib.request.Request(
        NOMINATIM_URL.format(lat=lat, lng=lng),
        headers={"User-Agent": USER_AGENT})
    try:
        with urllib.request.urlopen(request, timeout=20) as response:
            return json.load(response).get("display_name")
    except (urllib.error.URLError, ValueError):
        return None


def main():
    key = read_api_key()
    if not key:
        fail("no API key: set GOOGLE_GEOLOCATION_API_KEY or add it to "
             "~/.authinfo.gpg (see README)")

    access_points = scan_wifi()
    if len(access_points) < 2:
        fail(f"only {len(access_points)} access point(s) visible; need at least 2")

    lat, lng, accuracy = google_geolocate(access_points, key)
    if lat is None or lng is None:
        fail("Google returned no location")

    result = {"lat": lat, "lng": lng}
    if accuracy is not None:
        result["accuracy_m"] = accuracy
    address = reverse_geocode(lat, lng)
    if address:
        result["address"] = address
    print(json.dumps(result))


if __name__ == "__main__":
    main()
