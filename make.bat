@if not exist core\code_hash.inc                     echo writeln(':xxxxxxxx'); > core\code_hash.inc

@if not exist mnh_light.exe (
  ..\lazarus32\fpc\3.0.0\bin\i386-win32\fpc.exe consoles\mnh_light.lpr -o.\mnh_light.exe -gl -Fu..\lazarus32\components\synedit\units\i386-win32\win32 -Fu..\lazarus32\lcl\units\i386-win32\win32 -Fu..\lazarus32\lcl\units\i386-win32 -Fu..\lazarus32\components\lazutils\lib\i386-win32 -Fu..\lazarus32\packager\units\i386-win32 -Fugui -l -dLCL -dLCLwin32 -MObjFPC -Scgh -Fucore -Fu..\common -Fu..\common\* -Ficore -dversion64bit
  ..\lazarus32\fpc\3.0.0\bin\i386-win32\delp . core ..\common test consoles bin32 gui\lib\i386-win32 gui\lib\x86_64-win64 gui)
@copy mnh_light.exe mnh_temp.exe
mnh_temp.exe make.mnh %*
@del mnh_temp.exe
