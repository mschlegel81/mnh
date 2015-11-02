@if not exist mnh_light.exe (
  C:\lazarus64\fpc\2.6.4\bin\x86_64-win64\fpc.exe consoles\mnh_console.pas -o.\mnh_light.exe -gl -FuC:\lazarus64\components\synedit\units\x86_64-win64\win32 -FuC:\lazarus64\lcl\units\x86_64-win64\win32 -FuC:\lazarus64\lcl\units\x86_64-win64 -FuC:\lazarus64\components\lazutils\lib\x86_64-win64 -FuC:\lazarus64\packager\units\x86_64-win64 -Fugui -l -dLCL -dLCLwin32 -MObjFPC -Scgh -Fucore -Fuutil -Ficore -dversion64bit
  C:\lazarus64\fpc\2.6.4\bin\x86_64-win64\delp . core util test consoles bin32 gui\lib\i386-win32 gui\lib\x86_64-win64 gui)
@copy mnh_light.exe mnh_temp.exe
mnh_temp.exe make.mnh %*
@del mnh_temp.exe
