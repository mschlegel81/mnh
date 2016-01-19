@if not exist mnh_light.exe (
  d:\dev\lazarus32\fpc\2.6.4\bin\i386-win32\fpc.exe consoles\mnh_console.pas -o.\mnh_light.exe -gl -FuD:\dev\lazarus32\components\synedit\units\i386-win32\win32 -FuD:\dev\lazarus32\lcl\units\i386-win32\win32 -FuD:\dev\lazarus32\lcl\units\i386-win32 -FuD:\dev\lazarus32\components\lazutils\lib\i386-win32 -FuD:\dev\lazarus32\packager\units\i386-win32 -Fugui -l -dLCL -dLCLwin32 -MObjFPC -Scgh -Fucore -Fuutil -Ficore -dversion64bit
  d:\dev\lazarus32\fpc\2.6.4\bin\i386-win32\delp . core util test consoles bin32 gui\lib\i386-win32 gui\lib\x86_64-win64 gui)
@copy mnh_light.exe mnh_temp.exe
mnh_temp.exe make.mnh %*
@del mnh_temp.exe
