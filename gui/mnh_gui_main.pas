unit mnh_gui_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, StdCtrls, ComCtrls, ColorBox, ButtonPanel, CheckLst, Grids,
  mnh_fileWrappers, mnh_tokens, mnh_gui_settings, mnh_tokloc, mnh_out_adapters,mnh_constants;

type

  { TMnhForm }

  TMnhForm = class(TForm)
    ErrorGroupBox: TGroupBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    mi_settings: TMenuItem;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    ErrorStringGrid: TStringGrid;
    InputEdit: TSynEdit;
    OutputEdit: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InputEditChange(Sender: TObject);
    procedure mi_settingsClick(Sender: TObject);

  private
    procedure processSettings;
    { private declarations }
  public
    { public declarations }
  end;

var
  MnhForm: TMnhForm;
  inputWrapper: P_directInputWrapper;

  haltEvaluationPosted : boolean=false;
  evaluationRunning    : boolean=false;
  evaluationLoopRunning: boolean=false;

implementation

{$R *.lfm}

{ TMnhForm }
procedure TMnhForm.FormCreate(Sender: TObject);
  begin
    StatusBar.SimpleText:=
      'compiled on: '+{$I %DATE%}+
      ' at: '+{$I %TIME%}+
      ' with FPC'+{$I %FPCVERSION%}+
      ' for '+{$I %FPCTARGET%};
  end;

procedure TMnhForm.FormShow(Sender: TObject);
  begin
    processSettings;
  end;

procedure TMnhForm.InputEditChange(Sender: TObject);
  VAR L:T_stringList;
      i:longint;
  begin
    if inputWrapper<>nil then begin
      setLength(L,InputEdit.Lines.Count);
      for i:=0 to InputEdit.Lines.Count-1 do L[i]:=InputEdit.Lines[i];
      inputWrapper^.setInput(L);
    end;
  end;

procedure TMnhForm.mi_settingsClick(Sender: TObject);
  begin
    SettingsForm.ShowModal;
    processSettings;
  end;

procedure TMnhForm.processSettings;
  begin
    InputEdit.Font.name:=SettingsForm.getEditorFontName;
    InputEdit.Font.Size:=SettingsForm.fontSize;
    if SettingsForm.AntialiasCheckbox.Checked
    then InputEdit.Font.Quality:=fqCleartypeNatural
    else InputEdit.Font.Quality:=fqNonAntialiased;

    OutputEdit.Font     :=InputEdit.Font;
    ErrorStringGrid.Font:=InputEdit.Font;
  end;

PROCEDURE registerEvaluationEnd(CONST evalTime:double);
  VAR i,totalHeight:longint;
  begin
    if errorLevel>el0_allOkay then begin
      MnhForm.ErrorStringGrid.RowCount:=length(storedErrors);
      for i:=0 to length(storedErrors)-1 do with storedErrors[i] do begin
        MnhForm.ErrorStringGrid.Cells[0,i]:=C_errorLevelTxt[errorLevel];
        MnhForm.ErrorStringGrid.Cells[1,i]:=errorMessage;
        MnhForm.ErrorStringGrid.Cells[2,i]:=errorLocation;
        inc(totalHeight,MnhForm.ErrorStringGrid.RowHeights[i]);
      end;
      MnhForm.ErrorStringGrid.AutoSizeColumns;
      MnhForm.ErrorGroupBox.ClientHeight:=totalHeight;
      MnhForm.ErrorGroupBox.Visible:=true;
    end else MnhForm.ErrorGroupBox.Visible:=false;
    sleep(10);
    MnhForm.OutputEdit.Repaint;
  end;

FUNCTION evaluationThread(p:pointer):ptrint;
  VAR sleepTime:longint=1;
      time:double;
  PROCEDURE enterEval;
    begin
      try
        MnhForm.OutputEdit.ClearAll;
      except end;
      repeat evaluationRunning:=true until evaluationRunning;
      time:=now;
    end;

  PROCEDURE doneEval;
    begin
      registerEvaluationEnd(now-time); sleepTime:=1;
      repeat evaluationRunning:=false until not(evaluationRunning);
    end;

  begin
    repeat
      repeat evaluationLoopRunning:=true until evaluationLoopRunning;
      if isReloadOfAllPackagesIndicated then begin
        enterEval;
        reloadAllPackages;
        doneEval;
      end else if isReloadOfMainPackageIndicated then begin
        enterEval;
        reloadMainPackage;
        doneEval;
      end else if not(haltEvaluationPosted) then begin
        Sleep(sleepTime);
        if (sleepTime<500) then inc(sleepTime);
      end;
    until haltEvaluationPosted;
    inputWrapper:=nil;
    clearAllPackages;
    repeat evaluationLoopRunning:=false until not(evaluationLoopRunning);
  end;

PROCEDURE writeDeclEcho(CONST s:ansistring);
  begin
    MnhForm.OutputEdit.Lines.Append('in> '+s);
  end;

PROCEDURE writeExprEcho(CONST s:ansistring);
  begin
    MnhForm.OutputEdit.Lines.Append('in> '+s);
  end;

PROCEDURE writeExprOut (CONST s:ansistring);
  begin
    MnhForm.OutputEdit.Lines.Append('out> '+s);
  end;

PROCEDURE writePrint   (CONST s:ansistring);
  begin
    MnhForm.OutputEdit.Lines.Append(s);
  end;


INITIALIZATION
  mnh_out_adapters.inputDeclEcho:=@writeDeclEcho;
  mnh_out_adapters.inputExprEcho:=@writeExprEcho;
  mnh_out_adapters.exprOut:=@writeExprOut;
  mnh_out_adapters.printOut:=@writePrint;
  new(inputWrapper,create);
  inputWrapper^.logCheck;
  initMainPackage(inputWrapper);
  BeginThread(@evaluationThread);
  errorOut:=nil;

FINALIZATION
  haltEvaluation;
  repeat
    haltEvaluationPosted:=true;
    sleep(1);
  until not(evaluationLoopRunning) and not(evaluationRunning);


end.

