#!/bin/bash
if [ ! -f core/res_version.inc ]; then echo "CODE_HASH='x'; BUILD_NUMBER=1; VERSION='0.0.0';" > core/res_version.inc;fi
if [ ! -f target/Linux/mnh ]; then
  lazbuild -B --bm=deployment consoles/mnh_light.lpi
  mkdir target
  mkdir target/Linux
  mv consoles/mnh_light target/Linux/mnh_light
  target/Linux/mnh_light make.mnh target/Linux/mnh
fi
cp target/Linux/mnh mnh_tmp
./mnh_tmp make.mnh $@
rm mnh_tmp
