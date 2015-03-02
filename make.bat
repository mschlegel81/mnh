:start
@if "%1"=="" goto help
@if "%1"=="all" %0 32o 32d 64o 64d distro backup

@set delp=C:\lazarus\fpc\2.6.4\bin\i386-win32\delp . core util test consoles bin32 gui\lib\i386-win32
@set fpc32=C:\lazarus\fpc\2.6.4\bin\i386-win32\fpc         
@set fpc64=c:\lazarus64\fpc\2.6.4\bin\x86_64-win64\fpc.exe 
@rem @set optimize=-O2 -CX -XX -Scghi2 
@set optimize=-O3 -CX -XX -Si
@set debug=-g -glh -Si -ddebugMode
@set guiOpt=-Fugui -l -dLCL -dLCLwin32 -MObjFPC -Scgh -Fucore -Fuutil -Ficore 
@set guiOpt32=-FuC:\lazarus\components\synedit\units\i386-win32\win32     -FuC:\lazarus\lcl\units\i386-win32\win32     -FuC:\lazarus\lcl\units\i386-win32     -FuC:\lazarus\components\lazutils\lib\i386-win32     -FuC:\lazarus\packager\units\i386-win32     %guiOpt%
@set guiOpt64=-FuC:\lazarus64\components\synedit\units\x86_64-win64\win32 -FuC:\lazarus64\lcl\units\x86_64-win64\win32 -FuC:\lazarus64\lcl\units\x86_64-win64 -FuC:\lazarus64\components\lazutils\lib\x86_64-win64 -FuC:\lazarus64\packager\units\x86_64-win64 %guiOpt% -dversion64bit
@set sevenZip="c:\Program Files\7-Zip\7z.exe"
@For /f "tokens=1-3 delims=/. " %%a in ('date /t') do @(set mydate=%%c%%b%%a)
@%delp%

@if "%1"=="32o" goto 32bitOpt
@if "%1"=="32d" goto 32bitDeb
@if "%1"=="64o" goto 64bitOpt
@if "%1"=="64d" goto 64bitDeb
@if "%1"=="distro" goto pack
@if "%1"=="backup" goto backup
@goto help

:64bitOpt
@echo --------------------- building optimized 64 bit binaries ---------------------------
@%fpc64% consoles\mnh_console.pas -o.\mnh_light.exe        %optimize% %guiOpt64% 
@%fpc64% consoles\mnh_console.pas -o.\mnh_console.exe      %optimize% %guiOpt64% -dplots
@%fpc64% gui\mnh_gui.lpr          -o.\mnh_gui.exe          %optimize% %guiOpt64% -WG 
@%delp%
@%fpc64% consoles\mnh_console.pas -o.\mnh_console_prof.exe %optimize% %guiOpt64% -dplots -dPROFILING
@echo ------------------------------------------------------------------------------------
@%delp%
@del *.lfm *.res *.obj
@goto loop

:64bitDeb
@echo ------------------ building 64 bit binaries with debug info ------------------------
@%fpc64% consoles\mnh_console.pas -o.\mnh_light_debug.exe   %debug% %guiOpt64%
@%fpc64% consoles\mnh_console.pas -o.\mnh_console_debug.exe %debug% %guiOpt64% -dplots
@%fpc64% gui\mnh_gui.lpr          -o.\mnh_gui_debug.exe     %debug% %guiOpt64%
@%delp%
@del *.lfm *.res *.obj
@echo ------------------------------------------------------------------------------------
@goto loop

:32bitOpt
@echo --------------------- building optimized 32 bit binaries ---------------------------
@%fpc32% consoles\mnh_console.pas -obin32\mnh_light.exe   %optimize% %guiOpt32% 
@%fpc32% consoles\mnh_console.pas -obin32\mnh_console.exe %optimize% %guiOpt32% -dplots
@%fpc32% gui\mnh_gui.lpr          -obin32\mnh_gui.exe     %optimize% %guiOpt32% -WG 
@%delp%
@del bin32\*.lfm bin32\*.res
@echo ------------------------------------------------------------------------------------
@goto loop

:32bitDeb
@echo ------------------ building 32 bit binaries with debug info ------------------------
@%fpc32% consoles\mnh_console.pas -obin32\mnh_light_debug.exe   %debug% %guiOpt32%
@%fpc32% consoles\mnh_console.pas -obin32\mnh_console_debug.exe %debug% %guiOpt32% -dplots
@%fpc32% gui\mnh_gui.lpr          -obin32\mnh_gui_debug.exe     %debug% %guiOpt32%
@%delp%
@del bin32\*.lfm bin32\*.res
@echo ------------------------------------------------------------------------------------
@goto loop

:pack
@echo ---------------------------------- packaging ---------------------------------------
@mkdir distro\packages
@mkdir distro\demos
@mkdir distro\doc
@copy packages\*.mnh distro\packages
@copy demos\*.mnh distro\demos
@copy demos\*.txt distro\demos
@copy doc\* distro\doc
@cd distro
@cd doc 
@type builtin.head > builtin.html
@type builtin.foot >> builtin.html
@type packages.head > packages.html
@type packages.foot >> packages.html
@cd ..
@copy ..\*.exe .
@del ..\versions\mnh5_win??_%mydate%.7z
@%sevenZip% a -mx=9 ..\versions\mnh5_win64_%mydate%.7z
@del /Q *
@copy ..\bin32\*.exe .
@%sevenZip% a -mx=9 ..\versions\mnh5_win32_%mydate%.7z
@cd ..
@rmdir /S /Q distro
@echo ------------------------------------------------------------------------------------
@goto loop

:backup
@echo ---------------------------------- making backup -----------------------------------
@rmdir /S /Q gui\lib
@rmdir /S /Q gui\backup
@del versions\mnh5_%mydate%.7z
@%sevenZip% a -mx=9 -xr!*.7z -xr!*.exe -xr!*.o -xr!*.ppu -xr!*.bak -xr!*.dbg -xr!*.obj -xr!*.png versions\mnh5_%mydate%.7z
@echo ------------------------------------------------------------------------------------
@goto loop

:loop
@shift
@if "%1"=="" goto end
@goto start

:help
@echo %0 usage:
@echo   32o    - builds 32bit optimized binaries
@echo   32d    - builds 32bit binaried for debugging
@echo   64o    - builds 32bit optimized binaries
@echo   64d    - builds 32bit binaried for debugging
@echo   distro - makes a distro package
@echo   backup - creates a backup
@echo   backup - creates a backup
@echo   all - all of the above
:end