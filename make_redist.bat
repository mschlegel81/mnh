@set delp=C:\lazarus\fpc\2.6.4\bin\i386-win32\delp . core util test consoles bin gui\lib\i386-win32
@set fpc=C:\lazarus\fpc\2.6.4\bin\i386-win32\fpc -Fucore -Fuutil -Ficore 
@rem -vewnhi 
@set optimize=-O2 -CX -XX -Scghi2 
@set debug=-g -glh -Scgh2 -ddebugMode 
@set guiOpt= -MObjFPC -Scghi -Filib\i386-win32 -FuC:\lazarus\components\synedit\units\i386-win32\win32 -FuC:\lazarus\lcl\units\i386-win32\win32 -FuC:\lazarus\lcl\units\i386-win32 -FuC:\lazarus\components\lazutils\lib\i386-win32 -FuC:\lazarus\packager\units\i386-win32 -Fugui -l -dLCL -dLCLwin32
@%delp%

@echo ------------------------ building optimized binaries -------------------------------
@%fpc% consoles\mnh_console.pas -obin\mnh_console.exe %optimize% %guiOpt% 
@%fpc% gui\mnh_gui.lpr -obin\mnh_gui.exe %optimize% -WG %guiOpt%
@echo ------------------------------------------------------------------------------------
@%delp%
@del bin\*.lfm bin\*.res

@echo --------------------- building binaries with debug info ----------------------------
@%fpc% consoles\mnh_console.pas -obin\mnh_console_debug.exe %debug% %guiOpt%
@%fpc% gui\mnh_gui.lpr -obin\mnh_gui_debug.exe %debug% %guiOpt%
@echo ------------------------------------------------------------------------------------
@%delp%
@del bin\*.lfm bin\*.res

@rem -FuC:\lazarus\packager\units\i386-win32 -Fu. -FUlib\i386-win32 -l -dLCL -dLCLwin32

 