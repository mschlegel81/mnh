rmdir /S /Q gui\lib
rmdir /S /Q gui\backup
@set sevenZip="c:\Program Files\7-Zip\7z.exe"
@c:\lazarus\fpc\2.6.4\bin\i386-win32\delp . core util test consoles gui gui\lib\i386-win32
@For /f "tokens=1-3 delims=/. " %%a in ('date /t') do (set mydate=%%c%%b%%a)
@del versions\mnh5_%mydate%.7z
@%sevenZip% a -mx=9 -xr!*.7z -xr!*.exe -xr!*.o -xr!*.ppu -xr!*.bak -xr!*.dbg versions\mnh5_%mydate%.7z