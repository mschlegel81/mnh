call runpas cleanup
@For /f "tokens=1 delims=." %%a in ('dir /B consoles\*.pas') do c:\lazarus\fpc\2.6.4\bin\i386-win32\fpc consoles\%%a.pas -S2 -gl -gh -Fucore -Fuutil -Fu..\core -Fu..\util