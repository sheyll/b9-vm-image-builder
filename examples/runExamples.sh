#!/usr/bin/env bash

set -e

b9c="${1? b9c path parameter missing}"

echo "=== Systemd-Nspawn Happy ==="
$b9c -v -c systemd-nspawn.b9.conf build -f minimal.b9 

echo "=== Systemd-Nspawn Bad Extra Args ==="
( $b9c -v -c systemd-nspawn-bad-extra-args.b9.conf build -f minimal.b9 ; echo "Error expected!"; exit 1) || echo "Passed"

echo "=== Systemd-Nspawn Console Interactive ==="
$b9c -v -c systemd-nspawn-console-interactive.b9.conf build -f minimal.b9 

echo "=== Systemd-Nspawn Console Passive ==="
$b9c -v -c systemd-nspawn-console-passive.b9.conf build -f minimal.b9 

echo "=== Systemd-Nspawn Console Pipe ==="
$b9c -v -c systemd-nspawn-console-pipe.b9.conf build -f minimal.b9 

echo "=== Systemd-Nspawn Executable Non Existing ==="
( $b9c -v -c systemd-nspawn-executable-non-existing.b9.conf build -f minimal.b9 ; echo "Error expected!"; exit 1) || echo "Passed"

echo "=== Systemd-Nspawn No-Sudo ==="
( $b9c -v -c systemd-nspawn-no-sudo.b9.conf build -f minimal.b9 ; echo "Error expected!"; exit 1) || echo "Passed"

echo "=== Systemd-Nspawn No Timeout ==="
$b9c -v -c systemd-nspawn-console-pipe.b9.conf build -f minimal.b9 

echo "=== Systemd-Nspawn Short Timeout ==="
( $b9c -v -c systemd-nspawn-short-timeout.b9.conf build -f minimal.b9 ; echo "Error expected!"; exit 1) || echo "Passed"

echo "=== LibVirt-LXC ==="
$b9c -v -c libvirt-lxc.b9.conf build -f minimal.b9 
