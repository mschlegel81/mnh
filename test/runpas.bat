@set delp=C:\lazarus32\fpc\2.6.4\bin\i386-win32\delp 
@set fpc=C:\lazarus32\fpc\2.6.4\bin\i386-win32\fpc

@if "%1"=="clean" @goto cleanup
@goto run


:cleanup
  @%delp% c:\lazarus\fpc\2.6.4\bin\i386-win32\delp . core util test consoles bin gui 
  @goto end

:run
  set path=c:\lazarus\fpc\2.6.4\bin\i386-win32\
  @For /f "tokens=1 delims=\." %%a in ('echo %1') do @del %%a.exe

  @For /f "tokens=1 delims=." %%a in ('echo %1') do %fpc% %%a.pas -S2 -gl -Fucore -Fuutil -Fu..\core -Fu..\util -Filib\i386-win32 -FuC:\lazarus\components\synedit\units\i386-win32\win32 -FuC:\lazarus\lcl\units\i386-win32\win32 -FuC:\lazarus\lcl\units\i386-win32 -FuC:\lazarus\components\lazutils\lib\i386-win32 -FuC:\lazarus\packager\units\i386-win32 -Fugui -l -dLCL -dLCLwin32

  @rem @For /f "tokens=1 delims=." %%a in ('echo %1') do %fpc% %%a.pas -Sh2 -O2 -Fucore -Fuutil ^
  @rem   -MObjFPC -Scghi -Filib\i386-win32 -FuC:\lazarus\components\synedit\units\i386-win32\win32 -FuC:\lazarus\lcl\units\i386-win32\win32 -FuC:\lazarus\lcl\units\i386-win32 -FuC:\lazarus\components\lazutils\lib\i386-win32 -FuC:\lazarus\packager\units\i386-win32 -Fugui -l -dLCL -dLCLwin32
  @For /f "tokens=1 delims=." %%a in ('echo %1') do %%a.exe %2 %3 %4 %5 %6 %7 %8 %9
:end
