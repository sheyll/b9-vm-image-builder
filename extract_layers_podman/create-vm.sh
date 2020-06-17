#! /usr/bin/env nix-shell 
#! nix-shell -i bash --pure 
#! nix-shell -p podman bash qemu gnutar e2fsprogs coreutils utillinux conmon runc

set -ex

BASE_IMG=${1?BASE_IMG parameter missing}

BUILD_DIR_REL=$(mktemp -d /mnt/b9-podman-XXXXXXXXXXX)
mkdir -p "$BUILD_DIR_REL"
BUILD_DIR=$(realpath "$BUILD_DIR_REL")

STEP1=$BUILD_DIR/step1.raw
STEP1MNT=$BUILD_DIR/step1-mnt
STEP1TAR=$BUILD_DIR/step1.tar
CID=$BUILD_DIR/cid.txt
DATA=$BUILD_DIR/data.raw
DATAMNT=$BUILD_DIR/data
OUT_IMAGE_ID=$BUILD_DIR/out_img_id.txt 
OUT_LAYER_UNPACK_DIR=$BUILD_DIR/out_layers
STEP1_IMPORTED_IMAGE_ID=


function cleanup() {
  set +ex
  trap - ERR
  umount "$STEP1MNT" || echo "umount-loopback of / failed"
  umount "$DATAMNT"  || echo "umount-loopback of /data failed"

  if [[ -e "$CID" ]] 
  then
    podman container rm "$(cat "$CID")"
  fi 
  if [[ -e "$OUT_IMAGE_ID" ]]
  then
    podman image rm "$(cat "$OUT_IMAGE_ID")"
  fi
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

mkdir -p "$BUILD_DIR/step1-mnt"

mount -o loop "$STEP1" "$STEP1MNT"
mkdir -p "$STEP1MNT/mnt/vm_scripts"
tar -cf "$STEP1TAR" -C "$STEP1MNT" .

set +x
STEP1_IMPORTED_IMAGE_ID=$(podman import "$STEP1TAR" | cut -d: -f2 | tr -c -d '0123456789abcdef')
echo "Imported base image with podman as: $STEP1_IMPORTED_IMAGE_ID"
if [[ -n "$STEP1_IMPORTED_IMAGE_ID" ]]
then
  echo "podman import failed."
  umount "$STEP1MNT"
  exit 1
fi
set -x

fallocate -l4000000000 "$DATA"
mkfs.ext4 -Ldata "$DATA"

mkdir -p "$DATAMNT"
mount -o loop "$DATA" "$DATAMNT"

podman run \
  -v "$(realpath vm_scripts)":/mnt/vm_scripts \
  -v "$DATAMNT":/data \
  --network=host \
  --tty=true \
  --cidfile="$CID" \
  --privileged=true \
  --workdir=/mnt/vm_scripts \
  "$STEP1_IMPORTED_IMAGE_ID" \
  ./install.sh

qemu-img convert -q -f raw -O qcow2 "$DATA" data.qcow2

podman container commit "$(cat "$CID")" \
  | cut -d: -f2 \
  | tr -c -d '0123456789abcdef' \
  > "$OUT_IMAGE_ID"

mkdir -p "$OUT_LAYER_UNPACK_DIR"

podman image save "$(cat "$OUT_IMAGE_ID")" | tar -C "$OUT_LAYER_UNPACK_DIR" -xf -

OUT_LAYER_TAR="${OUT_LAYER_UNPACK_DIR}/$(jq '.[0].Layers[-1]' < "$OUT_LAYER_UNPACK_DIR/manifest.json"  | tr -d '"')"

tar -C "$STEP1MNT" -xf "$OUT_LAYER_TAR"
qemu-img convert -q -f raw -O qcow2 "$STEP1" 0.qcow2



