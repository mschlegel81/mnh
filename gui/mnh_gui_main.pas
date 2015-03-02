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
    MenuItem2: TMenuItem;
    miHaltEvalutaion: TMenuItem;
    miEvalModeWatch: TMenuItem;
    miEvalModeDirect: TMenuItem;
    miEvaluateNow: TMenuItem;
    miEvalModeDirectOnKeypress: TMenuItem;
    miClear: TMenuItem;
    miOpen: TMenuItem;
    miOpenToWatch: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    OpenDialog: TOpenDialog;
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
    SaveDialog: TSaveDialog;
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
    procedure miHaltEvalutaionClick(Sender: TObject);
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

FUNCTION evaluationThread(p: pointer): ptrint;
procedure ad_initEvaluation;
implementation
var adapter:record
      state:(as_directNoFile,
             as_directFile,
             as_watching);
      currentlyEditingFile:string;
      fileWasEdited:boolean;

      directInputWrapper: P_directInputWrapper;
      fileWrapper:        P_fileWrapper;

      haltEvaluationPosted : boolean;
      evaluationRunning    : boolean;
      evaluationLoopRunning: boolean;
    end;
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
    ad_initEvaluation;
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

procedure ad_killEvaluationSoftly;
  begin with adapter do begin
    directInputWrapper:=nil;
    fileWrapper:=nil;
    haltEvaluation;
    repeat
      haltEvaluationPosted:=true;
      sleep(1);
    until not(evaluationLoopRunning) and not(evaluationRunning);
  end; end;

procedure ad_initEvaluation;
  begin with adapter do if not(evaluationLoopRunning) then begin
    fileWrapper:=nil;
    directInputWrapper:=nil;
    if state=as_watching then begin
      new(fileWrapper,create(currentlyEditingFile));
      initMainPackage(fileWrapper);
    end else begin
      new(directInputWrapper,create);
      directInputWrapper^.logCheck;
      if state=as_directFile then addSourceScanPath(ExtractFilePath(currentlyEditingFile));
      initMainPackage(directInputWrapper);
    end;
    BeginThread(@evaluationThread);
  end; end;

procedure ad_loadInputToWrapper;
  VAR i:longint;
      L:T_stringList;
  begin with adapter do if (state<>as_watching) and (directInputWrapper<>nil) and (evaluationLoopRunning) then begin
    if state=as_directFile then fileWasEdited:=true;
    setLength(L,MnhForm.InputEdit.Lines.Count);
    for i:=0 to MnhForm.InputEdit.Lines.Count-1 do L[i]:=MnhForm.InputEdit.Lines[i];
    directInputWrapper^.setInput(L);
  end; end;

procedure TMnhForm.FormDestroy(Sender: TObject);
  begin
    ad_killEvaluationSoftly;
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

procedure TMnhForm.InputEditChange(Sender: TObject);
  begin
    if (miEvalModeDirectOnKeypress.Checked) then ad_loadInputToWrapper;
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
    {$WARNING Unimplemented!}
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
    miEvalModeDirect.Checked:=true;
  end;

procedure TMnhForm.miEvalModeDirectOnKeypressClick(Sender: TObject);
  begin
    miEvalModeDirectOnKeypress.Checked:=true;
  end;

procedure TMnhForm.miEvalModeWatchClick(Sender: TObject);
  begin
    {$WARNING Unimplemented!}
  end;

procedure TMnhForm.miEvaluateNowClick(Sender: TObject);
  begin
    {$WARNING Unimplemented!}
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

procedure TMnhForm.miHaltEvalutaionClick(Sender: TObject);
  begin
    mnh_out_adapters.haltEvaluation;
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
    if (evaluationStarted>0) and (adapter.evaluationRunning) then begin
      miHaltEvalutaion.Enabled:=true;
      StatusBar.SimpleText:='Evaluating '+FloatToStr((now-evaluationStarted)*24*60*60)+' sec.'
    end else miHaltEvalutaion.Enabled:=false;
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
        mnhForm.miHaltEvalutaion.Enabled:=true;
        MnhForm.OutputEdit.ClearAll;
        MnhForm.OutputEdit.Lines.Clear;
        MnhForm.ErrorStringGrid.RowCount:=0;
        MnhForm.ErrorGroupBox.Visible:=false;
      except end;
      repeat adapter.evaluationRunning:=true until adapter.evaluationRunning;
      evaluationStarted:=now;
    end;

  PROCEDURE doneEval;
    VAR totalHeight:longint;
    begin
      mnhForm.miHaltEvalutaion.Enabled:=false;
      MnhForm.StatusBar.SimpleText:='Done in '+FloatToStr((now-evaluationStarted)*24*60*60)+' sec.';
      evaluationStarted:=-1;
      repeat adapter.evaluationRunning:=false until not(adapter.evaluationRunning);
    end;

  begin
    repeat
      repeat adapter.evaluationLoopRunning:=true until adapter.evaluationLoopRunning;
      if isReloadOfAllPackagesIndicated then begin
        enterEval;
        reloadAllPackages;
        doneEval;
      end else if isReloadOfMainPackageIndicated then begin
        enterEval;
        reloadMainPackage;
        doneEval;
      end else if not(adapter.haltEvaluationPosted) then begin
        Sleep(sleepTime);
        if (sleepTime<500) then inc(sleepTime);
      end;
    until adapter.haltEvaluationPosted;
    adapter.directInputWrapper:=nil;
    adapter.fileWrapper:=nil;
    clearAllPackages;
    repeat adapter.evaluationLoopRunning:=false until not(adapter.evaluationLoopRunning);
  end;

PROCEDURE logError(CONST errorLevel:T_errorLevel; CONST errorMessage:ansistring; CONST errorLocation:T_tokenLocation);
  VAR row:longint;
  begin
    MnhForm.ErrorGroupBox.Visible:=true;
    row:=MnhForm.ErrorStringGrid.RowCount;
    MnhForm.ErrorStringGrid.RowCount:=row+1;
    MnhForm.ErrorStringGrid.Cells[0,row]:=C_errorLevelTxt[errorLevel];
    MnhForm.ErrorStringGrid.Cells[1,row]:=errorMessage;
    MnhForm.ErrorStringGrid.Cells[2,row]:=errorLocation;
    {$WARNING TODO Improve error logging (filename / fileline)!}
    MnhForm.ErrorStringGrid.AutoSizeColumns;
  end;

INITIALIZATION
  with adapter do begin
    state:=as_directNoFile;
    currentlyEditingFile:='';
    fileWasEdited:=false;

    directInputWrapper:=nil;
    fileWrapper:=nil;

    haltEvaluationPosted :=false;
    evaluationRunning    :=false;
    evaluationLoopRunning:=false;
  end;
  mnh_out_adapters.inputDeclEcho:=@writeDeclEcho;
  mnh_out_adapters.inputExprEcho:=@writeExprEcho;
  mnh_out_adapters.exprOut      :=@writeExprOut;
  mnh_out_adapters.printOut     :=@writePrint;
  mnh_out_adapters.errorOut     :=@logError;

end.

