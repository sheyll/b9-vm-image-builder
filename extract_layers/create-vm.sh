#!/usr/bin/env bash

set -ex

BASE_IMG=${1?BASE_IMG parameter missing}

BUILD_DIR_REL=_build
mkdir -p $BUILD_DIR_REL  
BUILD_DIR=$(realpath $BUILD_DIR_REL)

STEP1=$BUILD_DIR/step1.raw
STEP1MNT=$BUILD_DIR/step1-mnt
STEP1TAR=$BUILD_DIR/step1.tar
CID=$BUILD_DIR/cid.txt
DATA=$BUILD_DIR/data.raw
DATAMNT=$BUILD_DIR/data
OUT_IMAGE_ID=$BUILD_DIR/out_img_id.txt 
OUT_LAYER_UNPACK_DIR=$BUILD_DIR/out_layers

qemu-img convert -q -f qcow2 -O raw $BASE_IMG $BUILD_DIR/step1.raw 

mkdir -p $BUILD_DIR/step1-mnt

sudo mount -o loop $STEP1 $STEP1MNT
sudo mkdir -p $STEP1MNT/mnt/vm_scripts
sudo tar -cf $STEP1TAR -C $STEP1MNT .

STEP1_IMPORTED_IMAGE_ID=$(docker import $STEP1TAR | cut -d: -f2 | tr -c -d '0123456789abcdef')

fallocate -l4000000000 $DATA
mkfs.ext3 -Ldata $DATA

mkdir -p $DATAMNT
sudo mount -o loop $DATA $DATAMNT

docker run \
  -v $(realpath vm_scripts):/mnt/vm_scripts \
  -v $DATAMNT:/data \
  --network=host \
  --tty=true \
  --cidfile=$CID \
  --privileged=true \
  --workdir=/mnt/vm_scripts \
  $STEP1_IMPORTED_IMAGE_ID \
  ./install.sh

sudo umount $DATAMNT 
qemu-img convert -q -f raw -O qcow2 $DATA data.qcow2

docker container commit $(cat $CID) \
  | cut -d: -f2 \
  | tr -c -d '0123456789abcdef' \
  > $OUT_IMAGE_ID

mkdir -p $OUT_LAYER_UNPACK_DIR 

docker image save $(cat $OUT_IMAGE_ID) | tar -C $OUT_LAYER_UNPACK_DIR -xf -

OUT_LAYER_TAR=${OUT_LAYER_UNPACK_DIR}/$(cat $OUT_LAYER_UNPACK_DIR/manifest.json | jq '.[0].Layers[-1]' | tr -d '"')

sudo tar -C $STEP1MNT -xf $OUT_LAYER_TAR 
sudo umount $STEP1MNT 
qemu-img convert -q -f raw -O qcow2 $STEP1 0.qcow2


docker container rm $(cat $CID)
docker image rm $(cat $OUT_IMAGE_ID)
rm -rf $BUILD

