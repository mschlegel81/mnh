mnh make.mnh prepare
lazbuild -B --bm=deployment consoles/mnh_light.lpi
lazbuild -B --bm=deployment gui/mnh_gui.lpi
mv consoles/mnh_light .
mv gui/mnh .
./mnh demos/regTest.mnh doc/examples.txt 
./mnh_light demos/regTest.mnh doc/examples.txt 

