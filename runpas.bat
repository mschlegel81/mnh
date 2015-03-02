@if "%1"=="clean" @goto cleanup
@goto run

:cleanup
  @c:\lazarus\fpc\2.6.4\bin\i386-win32\delp . core util test consoles
  @del *.exe test\*.exe consoles\*.exe
  @goto end

:run
  @For /f "tokens=1 delims=\." %%a in ('echo %1') do @del %%a.exe
  @rem @For /f "tokens=1 delims=." %%a in ('echo %1') do c:\lazarus\fpc\2.6.4\bin\i386-win32\fpc %%a.pas -S2 -O2 -Fucore -Fuutil -Fu..\core -Fu..\util
  @For /f "tokens=1 delims=." %%a in ('echo %1') do c:\lazarus\fpc\2.6.4\bin\i386-win32\fpc %%a.pas -S2 -glch -CirRt -Fucore -Fuutil -Fu..\core -Fu..\util
  @For /f "tokens=1 delims=." %%a in ('echo %1') do %%a.exe %2 %3 %4 %5 %6 %7 %8 %9
:end