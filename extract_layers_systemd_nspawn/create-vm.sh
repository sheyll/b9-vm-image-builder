#! /usr/bin/env nix-shell 
#! nix-shell -i bash --pure 
#! nix-shell -p systemd bash qemu gnutar e2fsprogs coreutils utillinux conmon runc

set -ex

BASE_IMG=${1?BASE_IMG parameter missing}

BUILD_DIR_REL=$(mktemp -d /mnt/b9-nspawn-XXXXXXXXXXX)
mkdir -p "$BUILD_DIR_REL"
BUILD_DIR=$(realpath "$BUILD_DIR_REL")

STEP1=$BUILD_DIR/step1.raw
DATA=$BUILD_DIR/data.raw
DATAMNT=$BUILD_DIR/data

function cleanup() {
  set +ex
  trap - ERR
  umount "$DATAMNT"  || echo "umount-loopback of /data failed"
  rm -rf "$BUILD_DIR"
}

function cleanup_happy() {
  cleanup 
  exit 0
}

function cleanup_unhappy() {
  cleanup 
  exit 1
}

trap cleanup_happy EXIT
trap cleanup_unhappy ERR

qemu-img convert -q -f qcow2 -O raw "$BASE_IMG" "$STEP1"

fallocate -l4000000000 "$DATA"
mkfs.ext4 -Ldata "$DATA"

mkdir -p "$DATAMNT"
mount -o loop "$DATA" "$DATAMNT"

systemd-nspawn -i "$STEP1" \
  --bind=$(pwd)/vm_scripts:/mnt/vm_scripts \
  --bind=$DATAMNT:/data \
  --chdir=/mnt/vm_scripts \
  --console=pipe \
  '/bin/bash' -i -c \
  'set -e
export PATH=/usr/local/bin:/bin:/sbin:/usr/bin
source /etc/profile
source /etc/bashrc
echo $PATH; 
./install.sh'

umount "$DATAMNT"

qemu-img convert -q -f raw -O qcow2 "$DATA" data.qcow2
qemu-img convert -q -f raw -O qcow2 "$STEP1" 0.qcow2


