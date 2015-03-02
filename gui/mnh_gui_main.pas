unit mnh_gui_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, StdCtrls, ComCtrls, Grids, SynHighlighterMnh,
  mnh_fileWrappers, mnh_tokens, mnh_gui_settings, mnh_tokloc, mnh_out_adapters, mnh_constants,mnh_stringutil;

type

  { TMnhForm }

  TMnhForm = class(TForm)
    haltEvaluationButton: TButton;
    MenuItem2: TMenuItem;
    miEvalModeWatch: TMenuItem;
    miEvalModeDirect: TMenuItem;
    miEvaluateNow: TMenuItem;
    miEvalModeDirectOnKeypress: TMenuItem;
    miClear: TMenuItem;
    miOpen: TMenuItem;
    miOpenToWatch: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    popMiOpenPackage: TMenuItem;
    popMiOpenDeclaration: TMenuItem;
    myhl:TSynMnhSyn;
    ErrorGroupBox: TGroupBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem4: TMenuItem;
    miExpressionEcho: TMenuItem;
    miExpressionResult: TMenuItem;
    miDeclarationEcho: TMenuItem;
    miDecFontSize: TMenuItem;
    miIncFontSize: TMenuItem;
    mi_settings: TMenuItem;
    EditorPopup: TPopupMenu;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    ErrorStringGrid: TStringGrid;
    InputEdit: TSynEdit;
    OutputEdit: TSynEdit;
    UpdateTimeTimer: TTimer;
    procedure EditorPopupPopup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure haltEvaluationButtonClick(Sender: TObject);
    procedure InputEditChange(Sender: TObject);
    procedure InputEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure InputEditMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure miClearClick(Sender: TObject);
    procedure miDecFontSizeClick(Sender: TObject);
    procedure miDeclarationEchoClick(Sender: TObject);
    procedure miEvalModeDirectClick(Sender: TObject);
    procedure miEvalModeDirectOnKeypressClick(Sender: TObject);
    procedure miEvalModeWatchClick(Sender: TObject);
    procedure miEvaluateNowClick(Sender: TObject);
    procedure miExpressionEchoClick(Sender: TObject);
    procedure miExpressionResultClick(Sender: TObject);
    procedure miIncFontSizeClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miOpenToWatchClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure mi_settingsClick(Sender: TObject);
    procedure OutputEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OutputEditMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure popMiOpenDeclarationClick(Sender: TObject);
    procedure popMiOpenPackageClick(Sender: TObject);
    procedure UpdateTimeTimerTimer(Sender: TObject);

  private
    lastWordUnderCursor:string;
    ruleUnderCursorDeclared:record
      filename:string;
      fileline:longint;
    end;
    packageUnderCursorIsFile:string;

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
PROCEDURE initEvaluation(CONST filename:ansistring);
implementation

{$R *.lfm}

PROCEDURE writeDeclEcho(CONST s:ansistring);
  begin
    MnhForm.OutputEdit.Lines.Append(C_DeclEchoHead+' '+s);
  end;

PROCEDURE writeExprEcho(CONST s:ansistring);
  begin
    MnhForm.OutputEdit.Lines.Append(C_ExprEchoHead+' '+s);
  end;

PROCEDURE writeExprOut (CONST s:ansistring);
  begin
    MnhForm.OutputEdit.Lines.Append(C_ExprOutHead+' '+s);
  end;

PROCEDURE writePrint   (CONST s:ansistring);
  begin
    MnhForm.OutputEdit.Lines.Append(s);
  end;

{ TMnhForm }
procedure TMnhForm.FormCreate(Sender: TObject);
  begin
    myhl:=TSynMnhSyn.Create(nil);
    InputEdit.Highlighter:=myhl;
    OutputEdit.Highlighter:=myhl;
    settingsHaveBeenProcessed:=false;
    StatusBar.SimpleText:=
      'compiled on: '+{$I %DATE%}+
      ' at: '+{$I %TIME%}+
      ' with FPC'+{$I %FPCVERSION%}+
      ' for '+{$I %FPCTARGET%};
    BeginThread(@evaluationThread);
  end;

procedure TMnhForm.EditorPopupPopup(Sender: TObject);
  VAR location:T_tokenLocation;
  begin
    location:=getRuleLocation(lastWordUnderCursor);
    if (location.provider<>nil) and (location.provider^.filePath<>'') then
    with ruleUnderCursorDeclared do begin
      filename:=location.provider^.filePath;
      fileline:=location.line;
    end else ruleUnderCursorDeclared.filename:='';
    if location.column=-111
    then popMiOpenDeclaration.Caption:='"'+lastWordUnderCursor+'" is a built in function'
    else popMiOpenDeclaration.Caption:='Open declaration of "'+lastWordUnderCursor+'" ('+ruleUnderCursorDeclared.filename+')';
    popMiOpenDeclaration.Enabled:=ruleUnderCursorDeclared.filename<>'';

    location:=getPackageLocation(lastWordUnderCursor);
    if (location.provider<>nil) and (location.provider^.filePath<>'')
    then packageUnderCursorIsFile:=location.provider^.filePath
    else packageUnderCursorIsFile:='';

    popMiOpenPackage.Caption:='Open package "'+lastWordUnderCursor+'" ('+packageUnderCursorIsFile+')';
    popMiOpenPackage.Enabled:=packageUnderCursorIsFile<>'';
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

procedure initEvaluation(CONST filename:ansistring);
  begin
    if trim(filename)='' then begin
      new(P_directInputWrapper(inputWrapper),create);
      inputWrapper^.logCheck;
    end else begin
      new(P_fileWrapper(inputWrapper),create(filename));
    end;
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

procedure TMnhForm.InputEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  VAR wordUnderCursor:string;
  begin
    if (key>=33) and (key<=40) then begin
      wordUnderCursor:=InputEdit.GetWordAtRowCol(InputEdit.CaretXY);
      if isIdentifier(wordUnderCursor,true) then lastWordUnderCursor:=wordUnderCursor;
    end;
  end;

procedure TMnhForm.InputEditMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  VAR point:TPoint;
      wordUnderCursor:string;
  begin
    point.x:=x;
    point.y:=y;
    wordUnderCursor:=InputEdit.GetWordAtRowCol(InputEdit.PixelsToRowColumn(point));
    if isIdentifier(wordUnderCursor,true) then lastWordUnderCursor:=wordUnderCursor;
  end;

procedure TMnhForm.miClearClick(Sender: TObject);
begin
  {$WARNING TODO unimplemented}
end;

procedure TMnhForm.miDecFontSizeClick(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      SettingsForm.fontSize:=SettingsForm.fontSize-1;
      processSettings;
    end;
  end;

procedure TMnhForm.miDeclarationEchoClick(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      miDeclarationEcho.Checked:=not(miDeclarationEcho.Checked);
      with SettingsForm.outputBehaviour do begin
        doEchoDeclaration:=miDeclarationEcho.Checked;
        if doEchoDeclaration then mnh_out_adapters.inputDeclEcho:=@writeDeclEcho
                             else mnh_out_adapters.inputDeclEcho:=nil;
      end;
    end;
  end;

procedure TMnhForm.miEvalModeDirectClick(Sender: TObject);
begin
  {$WARNING TODO unimplemented}
end;

procedure TMnhForm.miEvalModeDirectOnKeypressClick(Sender: TObject);
begin
  {$WARNING TODO unimplemented}
end;

procedure TMnhForm.miEvalModeWatchClick(Sender: TObject);
begin
  {$WARNING TODO unimplemented}
end;

procedure TMnhForm.miEvaluateNowClick(Sender: TObject);
  begin
    inputWrapper^.markAsDirty;
  end;

procedure TMnhForm.miExpressionEchoClick(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      miExpressionEcho.Checked:=not(miExpressionEcho.Checked);
      with SettingsForm.outputBehaviour do begin
        doEchoInput:=miExpressionEcho.Checked;
        if doEchoInput then mnh_out_adapters.inputExprEcho:=@writeExprEcho
                       else mnh_out_adapters.inputExprEcho:=nil;
      end;
    end;
  end;

procedure TMnhForm.miExpressionResultClick(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      miExpressionResult.Checked:=not(miExpressionResult.Checked);
      with SettingsForm.outputBehaviour do begin
        doShowExpressionOut:=miExpressionResult.Checked;
        if doShowExpressionOut then mnh_out_adapters.exprOut:=@writeExprOut
                               else mnh_out_adapters.exprOut:=nil;
      end;
    end;
  end;

procedure TMnhForm.miIncFontSizeClick(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      SettingsForm.fontSize:=SettingsForm.fontSize+1;
      processSettings;
    end;
  end;

procedure TMnhForm.miOpenClick(Sender: TObject);
begin
   {$WARNING TODO unimplemented}
end;

procedure TMnhForm.miOpenToWatchClick(Sender: TObject);
begin
  {$WARNING TODO unimplemented}
end;

procedure TMnhForm.miSaveAsClick(Sender: TObject);
begin
  {$WARNING TODO unimplemented}
end;

procedure TMnhForm.miSaveClick(Sender: TObject);
begin
  {$WARNING TODO unimplemented}
end;

procedure TMnhForm.mi_settingsClick(Sender: TObject);
  begin
    SettingsForm.ShowModal;
    processSettings;
  end;

procedure TMnhForm.OutputEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  VAR wordUnderCursor:string;
  begin
    if (key>=33) and (key<=40) then begin
      wordUnderCursor:=OutputEdit.GetWordAtRowCol(OutputEdit.CaretXY);
      if isIdentifier(wordUnderCursor,true) then lastWordUnderCursor:=wordUnderCursor;
    end;
  end;

procedure TMnhForm.OutputEditMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  VAR point:TPoint;
      wordUnderCursor:string;
  begin
    point.x:=x;
    point.y:=y;
    wordUnderCursor:=OutputEdit.GetWordAtRowCol(OutputEdit.PixelsToRowColumn(point));
    if isIdentifier(wordUnderCursor,true) then lastWordUnderCursor:=wordUnderCursor;
  end;

procedure TMnhForm.popMiOpenDeclarationClick(Sender: TObject);
  begin
    with ruleUnderCursorDeclared do
    if filename<>''
    then SettingsForm.canOpenFile(filename,fileline);
  end;

procedure TMnhForm.popMiOpenPackageClick(Sender: TObject);
  begin
    if packageUnderCursorIsFile<>''
    then SettingsForm.canOpenFile(packageUnderCursorIsFile,-1);

  end;

VAR evaluationStarted:double=-1;

procedure TMnhForm.UpdateTimeTimerTimer(Sender: TObject);
  begin
    if (evaluationStarted>0) and (evaluationRunning) then
      StatusBar.SimpleText:='Evaluating '+FloatToStr((now-evaluationStarted)*24*60*60)+' sec.'
    else if haltEvaluationButton.Visible then haltEvaluationButton.Visible:=false;
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

    with SettingsForm.outputBehaviour do begin
      miDeclarationEcho.Checked:=doEchoDeclaration;
      if doEchoDeclaration   then mnh_out_adapters.inputDeclEcho:=@writeDeclEcho
                             else mnh_out_adapters.inputDeclEcho:=nil;
      miExpressionEcho.Checked:=doEchoInput;
      if doEchoInput         then mnh_out_adapters.inputExprEcho:=@writeExprEcho
                             else mnh_out_adapters.inputExprEcho:=nil;
      miExpressionResult.Checked:=doShowExpressionOut;
      if doShowExpressionOut then mnh_out_adapters.exprOut:=@writeExprOut
                             else mnh_out_adapters.exprOut:=nil;
    end;
    settingsHaveBeenProcessed:=true;
  end;

FUNCTION evaluationThread(p: pointer): ptrint;
  VAR sleepTime:longint=1;

  PROCEDURE enterEval;
    begin
      try
        MnhForm.OutputEdit.ClearAll;
        MnhForm.OutputEdit.Lines.Clear;
        mnhForm.haltEvaluationButton.Visible:=true;
      except end;
      repeat evaluationRunning:=true until evaluationRunning;
      evaluationStarted:=now;
    end;

  PROCEDURE doneEval;
    VAR i,totalHeight:longint;
    begin
      mnhForm.haltEvaluationButton.Visible:=false;
      MnhForm.StatusBar.SimpleText:='Done in '+FloatToStr((now-evaluationStarted)*24*60*60)+' sec.';
      evaluationStarted:=-1;
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

INITIALIZATION
  mnh_out_adapters.inputDeclEcho:=@writeDeclEcho;
  mnh_out_adapters.inputExprEcho:=@writeExprEcho;
  mnh_out_adapters.exprOut:=@writeExprOut;
  mnh_out_adapters.printOut:=@writePrint;
  errorOut:=nil;
  initEvaluation('');

end.

