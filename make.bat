@rem You may have to edit the path to lazbuild.exe to make this work...

@if not exist core\code_hash.inc echo CODE_HASH='xxxxxxxx'; > core\code_hash.inc
@if not exist core\build_number.inc echo BUILD_NUMBER=0; > core\build_number.inc
@if not exist mnh_light.exe (
  ..\lazarus64\lazbuild.exe -B --bm=deployment consoles\mnh_light.lpi
  move consoles\mnh_light.exe .)  
@copy mnh_light.exe mnh_temp.exe
@mnh_temp.exe make.mnh %* && del mnh_temp.exe
