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
    haltEvaluationButton: TButton;
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
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure haltEvaluationButtonClick(Sender: TObject);
    procedure InputEditChange(Sender: TObject);
    procedure mi_settingsClick(Sender: TObject);

  private
    settingsHaveBeenProcessed:boolean;
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

FUNCTION evaluationThread(p:pointer):ptrint;
PROCEDURE killEvaluationSoftly;
PROCEDURE initEvaluation;
implementation

{$R *.lfm}

{ TMnhForm }
procedure TMnhForm.FormCreate(Sender: TObject);
  begin
    settingsHaveBeenProcessed:=false;
    StatusBar.SimpleText:=
      'compiled on: '+{$I %DATE%}+
      ' at: '+{$I %TIME%}+
      ' with FPC'+{$I %FPCVERSION%}+
      ' for '+{$I %FPCTARGET%};
    BeginThread(@evaluationThread);
  end;

procedure killEvaluationSoftly;
  begin
    inputWrapper:=nil;
    haltEvaluation;
    repeat
      haltEvaluationPosted:=true;
      sleep(1);
    until not(evaluationLoopRunning) and not(evaluationRunning);
  end;

procedure initEvaluation;
  begin
    new(inputWrapper,create);
    inputWrapper^.logCheck;
    initMainPackage(inputWrapper);
  end;

procedure TMnhForm.FormDestroy(Sender: TObject);
  begin
    killEvaluationSoftly;
  end;

procedure TMnhForm.FormResize(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      SettingsForm.mainForm.top   :=top;
      SettingsForm.mainForm.left  :=left;
      SettingsForm.mainForm.width :=width;
      SettingsForm.mainForm.height:=height;
    end;
  end;

procedure TMnhForm.FormShow(Sender: TObject);
  begin
    processSettings;
  end;

procedure TMnhForm.haltEvaluationButtonClick(Sender: TObject);
  begin
    mnh_out_adapters.haltEvaluation;
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

    top   :=SettingsForm.mainForm.top;
    left  :=SettingsForm.mainForm.left;
    width :=SettingsForm.mainForm.width;
    height:=SettingsForm.mainForm.height;
    settingsHaveBeenProcessed:=true;
  end;


function evaluationThread(p: pointer): ptrint;
  VAR sleepTime:longint=1;
      time:double;
  PROCEDURE enterEval;
    begin
      try
        MnhForm.OutputEdit.ClearAll;
        mnhForm.haltEvaluationButton.Visible:=true;
      except end;
      repeat evaluationRunning:=true until evaluationRunning;
      time:=now;
    end;

  PROCEDURE doneEval;
    VAR i,totalHeight:longint;
    begin
      mnhForm.haltEvaluationButton.Visible:=false;
      MnhForm.StatusBar.SimpleText:='Done in '+FloatToStr((now-time)*24*60*60)+' sec.';
      if not(haltEvaluationPosted) then begin
        if errorLevel>el0_allOkay then begin
          totalHeight:=0;
          MnhForm.ErrorStringGrid.RowCount:=length(storedErrors);
          for i:=0 to length(storedErrors)-1 do with storedErrors[i] do begin
            MnhForm.ErrorStringGrid.Cells[0,i]:=C_errorLevelTxt[errorLevel];
            MnhForm.ErrorStringGrid.Cells[1,i]:=errorMessage;
            MnhForm.ErrorStringGrid.Cells[2,i]:=errorLocation;
            inc(totalHeight,MnhForm.ErrorStringGrid.RowHeights[i]);
          end;
          inc(totalHeight,MnhForm.ErrorStringGrid.RowHeights[0]);
          MnhForm.ErrorStringGrid.AutoSizeColumns;
          MnhForm.ErrorGroupBox.Visible:=true;
        end else MnhForm.ErrorGroupBox.Visible:=false;
        sleep(1);
        MnhForm.OutputEdit.Repaint;
        sleepTime:=1;
      end;
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
  errorOut:=nil;
  initEvaluation;

end.

