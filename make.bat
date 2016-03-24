@if not exist mnh_light.exe (
  @if not exist core\res_ensureDemos.inc               echo CONST ensureDemos_mnh: array[0..0] of string=(''); > res_ensureDemos.inc
  @if not exist core\res_ensureMnhFileAssociations.inc echo CONST removeMnhFileAssociations_mnh: array[0..0] of string=(''); > res_ensureMnhFileAssociations.inc
  @if not exist core\res_ensureNppHighlighting.inc     echo CONST ensureNotepad__Highlighting_mnh: array[0..0] of string=(''); > res_ensureNppHighlighting.inc
  @if not exist core\res_ensurePackages.inc            echo CONST ensurePackages_mnh: array[0..0] of string=(''); > res_ensurePackages.inc
  @if not exist core\res_examples.inc                  echo CONST doc_examples_txt: array[0..0] of string=(''); > res_examples.inc
  @if not exist core\res_html_template.inc             echo CONST doc_html_template_txt: array[0..0] of string=(''); > res_html_template.inc
  @if not exist core\res_removeMnhFileAssociations.inc echo CONST ensureMnhFileAssociations_mnh: array[0..0] of string=(''); > res_removeMnhFileAssociations.inc
  ..\lazarus32\fpc\2.6.4\bin\i386-win32\fpc.exe consoles\mnh_console.pas -o.\mnh_light.exe -gl -Fu..\lazarus32\components\synedit\units\i386-win32\win32 -Fu..\lazarus32\lcl\units\i386-win32\win32 -Fu..\lazarus32\lcl\units\i386-win32 -Fu..\lazarus32\components\lazutils\lib\i386-win32 -Fu..\lazarus32\packager\units\i386-win32 -Fugui -l -dLCL -dLCLwin32 -MObjFPC -Scgh -Fucore -Fu..\common  -Ficore -dversion64bit
  ..\lazarus32\fpc\2.6.4\bin\i386-win32\delp . core ..\common test consoles bin32 gui\lib\i386-win32 gui\lib\x86_64-win64 gui)
@copy mnh_light.exe mnh_temp.exe
mnh_temp.exe make.mnh %*
@del mnh_temp.exe
