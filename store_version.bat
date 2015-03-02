@rmdir /S /Q gui\lib
@rmdir /S /Q gui\backup
@set sevenZip="c:\Program Files\7-Zip\7z.exe"
@For /f "tokens=1-3 delims=/. " %%a in ('date /t') do (set mydate=%%c%%b%%a)
@del versions\mnh5_%mydate%.7z
@%sevenZip% a -mx=9 -xr!*.7z -xr!*.exe -xr!*.o -xr!*.ppu -xr!*.bak -xr!*.dbg versions\mnh5_%mydate%.7z