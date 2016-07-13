@if not exist core\code_hash.inc echo writeln(':xxxxxxxx'); > core\code_hash.inc
@if not exist mnh_light.exe (
  ..\lazarus64\lazbuild.exe -B --bm=deployment consoles/mnh_light.lpi
  move consoles/mnh_light.exe .)  
@copy mnh_light.exe mnh_temp.exe
mnh_temp.exe make.mnh %*
@del mnh_temp.exe
