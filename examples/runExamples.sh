#!/usr/bin/env bash

set -e

b9c="${1? b9c path parameter missing}"

echo "=== Systemd-Nspawn ==="
$b9c -v -c systemd-nspawn.b9.conf build -f minimal.b9 

echo "=== LibVirt-LXC ==="
$b9c -v -c libvirt-lxc.b9.conf build -f minimal.b9 
