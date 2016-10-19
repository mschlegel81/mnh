@if not exist .gitignore "C:\Program Files\7-Zip\7z.exe" x -y git.7z
@if not exist make.mnh git reset --keep
@if not exist core\code_hash.inc echo CODE_HASH='xxxxxxxx'; > core\code_hash.inc
@if not exist mnh_light.exe (
  ..\lazarus64\lazbuild.exe -B --bm=deployment consoles\mnh_light.lpi
  move consoles\mnh_light.exe .)  
@copy mnh_light.exe mnh_temp.exe
mnh_temp.exe make.mnh %*
@del mnh_temp.exe
