@rem You may have to edit the path to lazbuild.exe to make this work...

@if not exist core\res_version.inc echo CODE_HASH='x'; CODE_HASH='x'; BUILD_NUMBER=1; VERSION='0.0.0'; > core\res_version.inc
@if not exist target\Win64\mnh_light.exe (
  ..\lazarus64\lazbuild.exe -B --bm=deployment consoles\mnh_light.lpi
  move consoles\mnh_light.exe mnh_temp.exe)  
@copy target\Win64\mnh_light.exe mnh_temp.exe
@mnh_temp.exe make.mnh %* && del mnh_temp.exe
