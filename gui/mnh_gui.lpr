{$MAXSTACKSIZE 100000000}
PROGRAM mnh_gui;

{$mode objfpc}{$H+}

USES {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mnh_gui_settings, mnh_gui_main, closeDialog, askDialog,
  mnh_cmdLineInterpretation, mnh_tokLoc, mnh_funcs, mnh_funcs_list,
  mnh_funcs_math, mnh_funcs_mnh, mnh_funcs_regex, mnh_funcs_strings,
  mnh_funcs_system, mnh_litVar, mnh_tokens, mnh_out_adapters, mnh_plotData,
  consoleAsk, mnh_caches, mnh_constants, mnh_doc, mnh_html,
  mnh_debugForm, SynHighlighterMnh, mnh_evalThread, mySys
  {$ifndef debugMode},windows,sysutils{$endif};

{$R *.res}
{$ifndef debugMode}
FUNCTION GetConsoleWindow: HWND; stdcall; external kernel32;
{$endif}

begin
  locationToOpenOnFormStartup:=parseCmdLine;
  {$ifndef debugMode} ShowWindow(GetConsoleWindow, SW_HIDE); {$endif}

  mnh_gui_main.lateInitialization;
  Application.Title:='MNH5 - GUI';
  RequireDerivedFormResource := True;
  Application.Initialize;

  Application.CreateForm(TMnhForm, MnhForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TcloseDialogForm, closeDialogForm);
  Application.CreateForm(TaskForm, askForm);
  Application.CreateForm(TDebugForm, DebugForm);
  Application.Run;
  {$ifndef debugMode} ShowWindow(GetConsoleWindow, SW_SHOW); {$endif}
end.
