:start
@if "%1"=="help" goto help
@mnh_light.exe make_mnh.mnh updateHash
@if "%1"=="" %0 32o 32d 64o 64d
@if "%1"=="all" %0 32o 32d 64o 64d distro

@set delp=C:\lazarus32\fpc\2.6.4\bin\i386-win32\delp . core util test consoles bin32 gui\lib\i386-win32 gui\lib\x86_64-win64 gui
@set fpc32=C:\lazarus32\fpc\2.6.4\bin\i386-win32\fpc
@set fpc64=c:\lazarus64\fpc\2.6.4\bin\x86_64-win64\fpc.exe
@set optimize=-O3 -CX -XX -Si
@set debug=-g -gl -Si -ddebugMode
@set guiOpt=-Fugui -l -dLCL -dLCLwin32 -MObjFPC -Scgh -Fucore -Fuutil -Ficore
@set guiOpt32=-FuC:\lazarus32\components\synedit\units\i386-win32\win32     -FuC:\lazarus32\lcl\units\i386-win32\win32     -FuC:\lazarus32\lcl\units\i386-win32     -FuC:\lazarus32\components\lazutils\lib\i386-win32     -FuC:\lazarus32\packager\units\i386-win32     %guiOpt%
@set guiOpt64=-FuC:\lazarus64\components\synedit\units\x86_64-win64\win32 -FuC:\lazarus64\lcl\units\x86_64-win64\win32 -FuC:\lazarus64\lcl\units\x86_64-win64 -FuC:\lazarus64\components\lazutils\lib\x86_64-win64 -FuC:\lazarus64\packager\units\x86_64-win64 %guiOpt% -dversion64bit
@set sevenZip="c:\Program Files\7-Zip\7z.exe"
@For /f "tokens=1-3 delims=/. " %%a in ('date /t') do @(set mydate=%%c%%b%%a)
@%delp%

@if "%1"=="32o" goto 32bitOpt
@if "%1"=="32d" goto 32bitDeb
@if "%1"=="64o" goto 64bitOpt
@if "%1"=="64d" goto 64bitDeb
@if "%1"=="distro" goto pack
@if "%1"=="clean" goto cleanup
@if "%1"=="smartbuild" goto smartbuild
@goto help

:64bitOpt
@echo --------------------- building optimized 64 bit binaries ---------------------------
@%fpc64% gui\mnh_gui.lpr          -o.\mnh.exe      %optimize% %guiOpt64% -dfullversion
@%delp%
@%fpc64% consoles\mnh_console.pas -o.\mnh_light.exe %optimize% %guiOpt64%
@%delp%
@del *.lfm *.res *.obj
@echo ------------------------------------------------------------------------------------
@goto loop

:32bitOpt
@echo --------------------- building optimized 32 bit binaries ---------------------------
@%fpc32% gui\mnh_gui.lpr          -obin32\mnh.exe     %optimize% %guiOpt32% -dfullversion
@%delp%
@%fpc32% consoles\mnh_console.pas -obin32\mnh_light.exe   %optimize% %guiOpt32%
@%delp%
@del bin32\*.lfm bin32\*.res
@echo ------------------------------------------------------------------------------------
@goto loop

:64bitDeb
@echo ------------------ building 64 bit binaries with debug info ------------------------
@%fpc64% gui\mnh_gui.lpr          -o.\mnh_debug.exe     %debug% %guiOpt64% -dfullversion
@%delp%
@%fpc64% consoles\mnh_console.pas -o.\mnh_light_debug.exe   %debug% %guiOpt64%
@%delp%
@del *.lfm *.res *.obj
@echo ------------------------------------------------------------------------------------
@goto loop

:32bitDeb
@echo ------------------ building 32 bit binaries with debug info ------------------------
@%fpc32% gui\mnh_gui.lpr          -obin32\mnh_debug.exe     %debug% %guiOpt32% -dfullversion
@%delp%
@%fpc32% consoles\mnh_console.pas -obin32\mnh_light_debug.exe   %debug% %guiOpt32%
@%delp%
@del bin32\*.lfm bin32\*.res
@echo ------------------------------------------------------------------------------------
@goto loop

:pack
@echo ---------------------------------- packaging ---------------------------------------
@del versions\mnh5_%mydate%.7z
@%sevenZip% a -mx=9 versions\mnh5_%mydate%.7z @distro.list
@echo -----------------------------------------------------------------------------------
@goto loop

:smartbuild
@if exist mnh_light.exe goto smartbuildSkipCompile
@%delp%
@%fpc64% consoles\mnh_console.pas -o.\mnh_light.exe %optimize% %guiOpt64%
:smartbuildSkipCompile
@move mnh_light.exe temp.exe
@temp.exe make_mnh.mnh
@del temp.exe
@goto end

:cleanup
@%delp%
@del bin32\*.lfm bin32\*.res test\*.exe bin32\*.exe gui\*.exe *.exe gui\lib\i386-win32\*.lfm gui\lib\x86_64-win64\*.lfm *.png demos\*.png packages\*.png regtest\*.last demos\inputs\*.txt
@goto loop

:loop
@shift
@if "%1"=="" goto end
@goto start

:help
@echo %0 usage:
@echo   32o     - builds 32bit optimized binaries
@echo   32d     - builds 32bit binaried for debugging
@echo   64o     - builds 32bit optimized binaries
@echo   64d     - builds 32bit binaried for debugging
@echo   distro  - makes a distro package
@echo   all     - all of the above
@echo   clean   - cleanup
@echo   smartbuild
:end
