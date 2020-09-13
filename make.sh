#!/bin/bash
if [ ! -f core/res_version.inc ]; then echo "CODE_HASH='x'; BUILD_NUMBER=1; VERSION='0.0.0';" > core/res_version.inc;fi
if [ ! -f target/Linux/mnh_light ]; then
  lazbuild -B --bm=deployment consoles/mnh_light.lpi
  mv consoles/mnh_light mnh_tmp
else 
  cp target/Linux/mnh_light mnh_tmp 
fi
./mnh_tmp make.mnh $@
rm mnh_tmp
