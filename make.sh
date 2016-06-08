if [ ! -f core/res_ensureDemos.inc               ]; then echo "CONST ensureDemos_mnh: array[0..0] of string=('');" > core/res_ensureDemos.inc;fi
if [ ! -f core/res_ensureMnhFileAssociations.inc ]; then echo "CONST ensureMnhFileAssociations_mnh: array[0..0] of string=('');" > core/res_ensureMnhFileAssociations.inc;fi
if [ ! -f core/res_ensureNppHighlighting.inc     ]; then echo "CONST ensureNotepad__Highlighting_mnh: array[0..0] of string=('');" > core/res_ensureNppHighlighting.inc;fi
if [ ! -f core/res_ensurePackages.inc            ]; then echo "CONST ensurePackages_mnh: array[0..0] of string=('');" > core/res_ensurePackages.inc;fi
if [ ! -f core/res_examples.inc                  ]; then echo "CONST doc_examples_txt: array[0..0] of string=('');" > core/res_examples.inc;fi
if [ ! -f core/res_html_template.inc             ]; then echo "CONST doc_html_template_txt: array[0..0] of string=('');" > core/res_html_template.inc;fi
if [ ! -f core/res_removeMnhFileAssociations.inc ]; then echo "CONST removeMnhFileAssociations_mnh: array[0..0] of string=('');" > core/res_removeMnhFileAssociations.inc;fi
if [ ! -f core/code_hash.inc                     ]; then echo "writeln(':xxxxxxxx');" > core/code_hash.inc;fi
lazbuild -B --bm=deployment consoles/mnh_light.lpi
consoles/mnh_light make.mnh prepare
lazbuild --bm=deployment consoles/mnh_light.lpi
mv consoles/mnh_light .
./mnh_light regTest/regTest.mnh doc/examples.txt
lazbuild -B --bm=deployment gui/mnh_gui.lpi
mv gui/mnh .
./mnh regTest/regTest.mnh doc/examples.txt 

