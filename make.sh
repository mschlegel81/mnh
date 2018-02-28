#!/bin/bash
if [ ! -f core/code_hash.inc ]; then echo "CODE_HASH='xxxxxxxx';" > core/code_hash.inc;fi
if [ ! -f core/build_number.inc ]; then echo "BUILD_NUMBER=0;" > core/build_number.inc;fi
if [ ! -f mnh_light ]; then
  lazbuild -B --bm=deployment consoles/mnh_light.lpi
  mv consoles/mnh_light .
fi
cp mnh_light mnh_tmp 
./mnh_tmp make.mnh $@
rm mnh_tmp
