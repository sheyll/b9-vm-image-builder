#!/usr/bin/env bash

set -e

b9c="${1? b9c path parameter missing}"

declare SUCCESS

echo
echo "= Systemd-Nspawn TESTS ="
echo

echo "=== Systemd-Nspawn (minimalistic happy case) ==="
$b9c -v -c systemd-nspawn.b9.conf build -f minimal.b9

echo "=== Systemd-Nspawn Bad Extra Args ==="
( $b9c -v -c systemd-nspawn-bad-extra-args.b9.conf build -f minimal.b9 || echo "Error expected" ) | grep -q "systemd-nspawn: unrecognized option '--some'"

echo "=== Systemd-Nspawn Console Interactive ==="
$b9c -v -c systemd-nspawn-console-interactive.b9.conf build -f minimal-interactive.b9 | grep -q "TEST PaSsEd!!"  <<EOF
echo "TEST PaSsEd!!"
exit 0
EOF

echo "=== Systemd-Nspawn Console Passive ==="
$b9c -c systemd-nspawn-console-passive.b9.conf build -f minimal.b9

echo "=== Systemd-Nspawn Console Pipe ==="
$b9c -c systemd-nspawn-console-pipe.b9.conf build -f minimal.b9

echo "=== Systemd-Nspawn Executable Non Existing ==="
SUCCESS="1"
$b9c -c systemd-nspawn-executable-non-existing.b9.conf build -f minimal.b9 || export SUCCESS="0"
if [[ "$SUCCESS" -eq "1" ]]
then
  echo "FAILED: Expected error not raised!"
  exit 1
fi

echo "=== Systemd-Nspawn No-Sudo ==="
( $b9c -v -c systemd-nspawn-no-sudo.b9.conf build -f minimal.b9 || echo "Error expected") | grep -q "Need to be root."

echo "=== Systemd-Nspawn No Timeout ==="
$b9c -c systemd-nspawn-console-pipe.b9.conf build -f minimal.b9

echo "=== Systemd-Nspawn Short Timeout ==="
$b9c -v -c systemd-nspawn-short-timeout.b9.conf build -f minimal-sleep-5.b9 | grep -q "COMMAND TIMED OUT"

echo
echo "OK: TESTS PASSED"
