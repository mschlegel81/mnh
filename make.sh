if [ ! -f core/code_hash.inc ]; then echo "writeln(':xxxxxxxx');" > core/code_hash.inc;fi
lazbuild -B --bm=deployment consoles/mnh_light.lpi
consoles/mnh_light make.mnh prepare
lazbuild --bm=deployment consoles/mnh_light.lpi
mv consoles/mnh_light ./mnh_tmp
./mnh_tmp make.mnh test
rm mnh_tmp
