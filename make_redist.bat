c:\lazarus\fpc\2.6.4\bin\i386-win32\delp . core util test consoles bin

mkdir bin

c:\lazarus\fpc\2.6.4\bin\i386-win32\fpc consoles\mnh_console.pas ^
  -S2 -Si -O2 -Fucore -Fuutil              -obin\mnh_console.exe 

c:\lazarus\fpc\2.6.4\bin\i386-win32\fpc gui\mnh_gui.lpr ^
  -S2 -Si -O2 -Fucore -Fuutil         -obin\mnh_gui.exe ^
  -dLCL -dLCLwin32 -CX -XX -WG ^
  -FuC:\lazarus\lcl\units\i386-win32\win32 ^
  -FuC:\lazarus\lcl\units\i386-win32 ^
  -FuC:\lazarus\components\lazutils\lib\i386-win32 ^
  -FuC:\lazarus\components\synedit\units\i386-win32\win32 ^
  -FiC:\lazarus\components\synedit 
c:\lazarus\fpc\2.6.4\bin\i386-win32\delp . core util test consoles bin

c:\lazarus\fpc\2.6.4\bin\i386-win32\delp . core util test consoles bin

del bin\*.lfm bin\*.res

c:\lazarus\fpc\2.6.4\bin\i386-win32\fpc consoles\mnh_console.pas ^
  -S2 -glch -CirRt -Fucore -Fuutil          -obin\mnh_console_debug.exe 

c:\lazarus\fpc\2.6.4\bin\i386-win32\fpc gui\mnh_gui.lpr ^
  -S2 -glch -CirRt -Fucore -Fuutil    -obin\mnh_gui_debug.exe ^
  -dLCL -dLCLwin32 ^
  -FuC:\lazarus\lcl\units\i386-win32\win32 ^
  -FuC:\lazarus\lcl\units\i386-win32 ^
  -FuC:\lazarus\components\lazutils\lib\i386-win32 ^
  -FuC:\lazarus\components\synedit\units\i386-win32\win32 ^
  -FiC:\lazarus\components\synedit 

c:\lazarus\fpc\2.6.4\bin\i386-win32\delp . core util test consoles bin

del bin\*.lfm bin\*.res

