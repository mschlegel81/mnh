@set delp=C:\lazarus\fpc\2.6.4\bin\i386-win32\delp . core util test consoles bin32 bin64 gui\lib\i386-win32
@set fpc32=C:\lazarus\fpc\2.6.4\bin\i386-win32\fpc         
@set fpc64=c:\lazarus64\fpc\2.6.4\bin\x86_64-win64\fpc.exe 
@set optimize=-O2 -CX -XX -Scghi2 
@set debug=-g -glh -Scgh2 -ddebugMode 
@set guiOpt=-Fugui -l -dLCL -dLCLwin32 -MObjFPC -Scghi -Fucore -Fuutil -Ficore 
@set guiOpt32=-FuC:\lazarus\components\synedit\units\i386-win32\win32     -FuC:\lazarus\lcl\units\i386-win32\win32     -FuC:\lazarus\lcl\units\i386-win32     -FuC:\lazarus\components\lazutils\lib\i386-win32     -FuC:\lazarus\packager\units\i386-win32     %guiOpt%
@set guiOpt64=-FuC:\lazarus64\components\synedit\units\x86_64-win64\win32 -FuC:\lazarus64\lcl\units\x86_64-win64\win32 -FuC:\lazarus64\lcl\units\x86_64-win64 -FuC:\lazarus64\components\lazutils\lib\x86_64-win64 -FuC:\lazarus64\packager\units\x86_64-win64 %guiOpt% -dversion64bit
@%delp%

@echo ------------------------ building optimized binaries -------------------------------
@%fpc64% consoles\mnh_console.pas -obin64\mnh_light.exe   %optimize% %guiOpt64% 
@%fpc64% consoles\mnh_console.pas -obin64\mnh_console.exe %optimize% %guiOpt64% -dplots
@%fpc64% gui\mnh_gui.lpr          -obin64\mnh_gui.exe     %optimize% %guiOpt64% -WG 
@echo ------------------------------------------------------------------------------------
@%delp%
@del bin64\*.lfm bin64\*.res bin64\*.obj

@echo --------------------- building binaries with debug info ----------------------------
@%fpc64% consoles\mnh_console.pas -obin64\mnh_light_debug.exe   %debug% %guiOpt64%
@%fpc64% consoles\mnh_console.pas -obin64\mnh_console_debug.exe %debug% %guiOpt64% -dplots
@%fpc64% gui\mnh_gui.lpr          -obin64\mnh_gui_debug.exe     %debug% %guiOpt64%
@echo ------------------------------------------------------------------------------------
@%delp%
@del bin64\*.lfm bin64\*.res bin64\*.obj

@echo ------------------------ building optimized binaries -------------------------------
@%fpc32% consoles\mnh_console.pas -obin32\mnh_light.exe   %optimize% %guiOpt32% 
@%fpc32% consoles\mnh_console.pas -obin32\mnh_console.exe %optimize% %guiOpt32% -dplots
@%fpc32% gui\mnh_gui.lpr          -obin32\mnh_gui.exe     %optimize% %guiOpt32% -WG 
@echo ------------------------------------------------------------------------------------
@%delp%
@del bin32\*.lfm bin32\*.res

@echo --------------------- building binaries with debug info ----------------------------
@%fpc32% consoles\mnh_console.pas -obin32\mnh_light_debug.exe   %debug% %guiOpt32%
@%fpc32% consoles\mnh_console.pas -obin32\mnh_console_debug.exe %debug% %guiOpt32% -dplots
@%fpc32% gui\mnh_gui.lpr          -obin32\mnh_gui_debug.exe     %debug% %guiOpt32%
@echo ------------------------------------------------------------------------------------
@%delp%
@del bin32\*.lfm bin32\*.res
