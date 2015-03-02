unit mnh_gui_main;

{$mode objfpc}{$H+}
{$define useAdapterCS}
interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, StdCtrls, ComCtrls, Grids, SynHighlighterMnh,
  mnh_fileWrappers, mnh_tokens, mnh_gui_settings, mnh_tokloc,
  mnh_out_adapters, mnh_constants,mnh_stringutil, mnh_askForm;

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
      {$ifdef useAdapterCS} cs:TRTLCriticalSection; {$endif}
      state:(as_directNoFile,
             as_directFile,
             as_watching);
      currentlyEditingFile:string;
      fileWasEdited:boolean;
      watchedFileAge:longint;

      errorGridRequiresResize:boolean;

      directInputWrapper: P_directInputWrapper;
      fileWrapper:        P_fileWrapper;

      evaluationStarted        : double;
      haltEvaluationLoopPosted : boolean;
      evaluationRunning        : boolean;
      evaluationLoopRunning    : boolean;
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

procedure ad_haltEvaluation;
  begin with adapter do begin
    haltEvaluation;
    while evaluationRunning do sleep(1);
  end; end;

procedure ad_killEvaluationLoopSoftly;
  begin with adapter do begin
    haltEvaluation;
    //this must happen outside of the critical section to avoid a deadlock
    repeat
      haltEvaluationLoopPosted:=true;
      sleep(1);
    until not(evaluationLoopRunning) and not(evaluationRunning);
  end; end;

procedure ad_initEvaluation;
  begin with adapter do if not(evaluationLoopRunning) then begin
    {$ifdef useAdapterCS} EnterCriticalsection(cs); {$endif}
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
    {$ifdef useAdapterCS} LeaveCriticalsection(cs); {$endif}
  end; end;

procedure ad_loadInputToWrapper;
  VAR i:longint;
      L:T_stringList;
  begin with adapter do if (state<>as_watching) and (directInputWrapper<>nil) then begin
    {$ifdef useAdapterCS} EnterCriticalsection(cs); {$endif}
    if state=as_directFile then fileWasEdited:=true;
    setLength(L,MnhForm.InputEdit.Lines.Count);
    for i:=0 to MnhForm.InputEdit.Lines.Count-1 do L[i]:=MnhForm.InputEdit.Lines[i];
    directInputWrapper^.setInput(L);
    {$ifdef useAdapterCS} LeaveCriticalsection(cs); {$endif}
  end; end;

procedure ad_updateForm;
  VAR caption:string;
  begin with adapter do begin
    {$ifdef useAdapterCS} EnterCriticalsection(cs); {$endif}
    caption:='MNH5 '+currentlyEditingFile; if MnhForm.Caption<>caption then MnhForm.Caption:=caption;
    MnhForm.miSave          .Enabled:=(currentlyEditingFile<>'') and (state<>as_watching);
    MnhForm.miSaveAs        .Enabled:=                               (state<>as_watching);
    MnhForm.miEvaluateNow   .Enabled:=evaluationLoopRunning and not(evaluationRunning);
    MnhForm.miHaltEvalutaion.Enabled:=evaluationRunning;
    if evaluationRunning then MnhForm.StatusBar.SimpleText:='Evaluating '+FloatToStr(round((now-evaluationStarted)*24*60*60*100)*0.01)+' sec.';
    if (state=as_watching) then begin
      if not(MnhForm.miEvalModeWatch.Checked) then MnhForm.miEvalModeWatch.Checked:=true;
      if not(MnhForm.InputEdit.ReadOnly)      then MnhForm.InputEdit.ReadOnly:=true;
    end else begin
      if     MnhForm.InputEdit.ReadOnly       then MnhForm.InputEdit.ReadOnly:=false;
    end;
    //MnhForm.ErrorStringGrid.AutoSizeColumns;
    if (state=as_watching) and
       (currentlyEditingFile<>'') and
       (FileAge(currentlyEditingFile)<>watchedFileAge)
    then try
      MnhForm.InputEdit.Lines.LoadFromFile(currentlyEditingFile);
      watchedFileAge:=FileAge(currentlyEditingFile);
    except
      watchedFileAge:=0;
    end;
    if errorGridRequiresResize then try
      MnhForm.ErrorStringGrid.AutoSizeColumns;
      errorGridRequiresResize:=false;
    except
      errorGridRequiresResize:=true;
    end;
    {$ifdef useAdapterCS} LeaveCriticalsection(cs); {$endif}
  end; end;

procedure ad_triggerEvaluation;
  begin with adapter do if evaluationLoopRunning then begin
    {$ifdef useAdapterCS} EnterCriticalsection(cs); {$endif}
    if directInputWrapper<>nil then directInputWrapper^.markAsDirty;
    if fileWrapper       <>nil then fileWrapper       ^.markAsDirty;
    {$ifdef useAdapterCS} LeaveCriticalsection(cs); {$endif}
  end; end;

procedure ad_clearFile;
  begin with adapter do begin
    {$ifdef useAdapterCS} EnterCriticalsection(cs); {$endif}
    currentlyEditingFile:='';
    if state=as_watching then begin
      ad_killEvaluationLoopSoftly;
      state:=as_directNoFile;
      ad_initEvaluation;
    end else begin
      directInputWrapper^.setInput('');
      ad_haltEvaluation;
      state:=as_directNoFile;
      clearImportedPackages;
    end;
    MnhForm.InputEdit.ClearAll;
    MnhForm.InputEdit.Lines.Clear;
    ad_updateForm;
    {$ifdef useAdapterCS} LeaveCriticalsection(cs); {$endif}
  end; end;

PROCEDURE ad_switchToDirectWithFile(CONST filename:string);
  VAR mr:integer;
  begin with adapter do begin
    case state of
      as_directNoFile: if filename<>'' then begin
        ad_haltEvaluation;
        currentlyEditingFile:=filename;
        MnhForm.InputEdit.Lines.LoadFromFile(filename);
        ad_updateForm;
        ad_loadInputToWrapper;
      end;
      as_directFile: begin
        if (filename<>'') and (filename<>currentlyEditingFile) and fileWasEdited then begin
          mr:=AskForm.showModalOnFileChange();
          if mr=mrYes then MnhForm.InputEdit.Lines.SaveToFile(currentlyEditingFile)
          else if mr<>mrNo then exit;
        end;
        ad_haltEvaluation;
        currentlyEditingFile:=filename;
        MnhForm.InputEdit.Lines.LoadFromFile(filename);
        ad_updateForm;
        ad_loadInputToWrapper;
      end;
      as_watching: begin
        ad_killEvaluationLoopSoftly;
        state:=as_directFile;
        currentlyEditingFile:=filename;
        MnhForm.InputEdit.Lines.LoadFromFile(filename);
        ad_updateForm;
        ad_initEvaluation;
      end;
    end;
  end; end;

PROCEDURE ad_switchToWatchFile(CONST filename:string);
  VAR mr:integer;
  begin with adapter do begin
    case state of
      as_directNoFile: if filename<>'' then begin
        ad_killEvaluationLoopSoftly;
        state:=as_watching;
        currentlyEditingFile:=filename;
        MnhForm.InputEdit.Lines.LoadFromFile(filename);
        ad_updateForm;
        ad_initEvaluation;
      end;
      as_directFile: begin
        if (filename<>'') and (filename<>currentlyEditingFile) and fileWasEdited then begin
          mr:=AskForm.showModalOnFileChange();
          if (mr<>mrYes) and (mr<>mrNo) then exit;
        end else if fileWasEdited then mr:=mrYes;
        if mr=mrYes then MnhForm.InputEdit.Lines.SaveToFile(currentlyEditingFile);

        ad_killEvaluationLoopSoftly;
        state:=as_watching;
        currentlyEditingFile:=filename;
        MnhForm.InputEdit.Lines.LoadFromFile(filename);
        ad_updateForm;
        ad_initEvaluation;
      end;
      as_watching: if (filename<>'') and (filename<>currentlyEditingFile) then begin
        ad_killEvaluationLoopSoftly;
        currentlyEditingFile:=filename;
        MnhForm.InputEdit.Lines.LoadFromFile(filename);
        ad_updateForm;
        ad_initEvaluation;
      end;
    end;
  end; end;


procedure TMnhForm.FormDestroy(Sender: TObject);
  begin
    ad_killEvaluationLoopSoftly;
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
    if not(settingsHaveBeenProcessed) then processSettings;
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
    if miEvalModeWatch.Checked then miEvalModeDirectOnKeypress.Checked:=true;
    ad_clearFile;
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
    if miEvalModeDirect.Checked then exit;
    if adapter.state=as_watching then ad_switchToDirectWithFile(adapter.currentlyEditingFile);
    miEvalModeDirect.Checked:=true;
  end;

procedure TMnhForm.miEvalModeDirectOnKeypressClick(Sender: TObject);
  begin
    if miEvalModeDirectOnKeypress.Checked then exit;
    if adapter.state=as_watching then ad_switchToDirectWithFile(adapter.currentlyEditingFile);
    miEvalModeDirectOnKeypress.Checked:=true;
  end;

procedure TMnhForm.miEvalModeWatchClick(Sender: TObject);
  begin
    if miEvalModeWatch.Checked then exit;
    if adapter.state=as_directFile then begin
      ad_switchToWatchFile(adapter.currentlyEditingFile);
      miEvalModeWatch.Checked:=true;
    end else miOpenToWatchClick(Sender);
  end;

procedure TMnhForm.miEvaluateNowClick(Sender: TObject);
  begin
    ad_loadInputToWrapper;
    ad_triggerEvaluation;
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
    ad_haltEvaluation;
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
    OpenDialog.Title:='Open file for editing';
    if OpenDialog.Execute and FileExists(OpenDialog.FileName)
    then ad_switchToDirectWithFile(OpenDialog.FileName);
  end;

procedure TMnhForm.miOpenToWatchClick(Sender: TObject);
  begin
    OpenDialog.Title:='Open file for watching';
    if OpenDialog.Execute and FileExists(OpenDialog.FileName)
    then ad_switchToWatchFile(OpenDialog.FileName);
  end;

procedure TMnhForm.miSaveAsClick(Sender: TObject);
  begin with adapter do if state=as_directFile then begin
    if SaveDialog.Execute then begin
      currentlyEditingFile:=SaveDialog.FileName;
      MnhForm.InputEdit.Lines.SaveToFile(currentlyEditingFile);
      ad_updateForm;
    end;
  end; end;

procedure TMnhForm.miSaveClick(Sender: TObject);
  begin with adapter do begin
    if currentlyEditingFile<>'' then MnhForm.InputEdit.Lines.SaveToFile(currentlyEditingFile);
  end; end;

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

procedure TMnhForm.UpdateTimeTimerTimer(Sender: TObject);
  begin
    ad_updateForm;
  end;

procedure TMnhForm.processSettings;
  begin
    InputEdit.Font.name:=SettingsForm.getEditorFontName;
    InputEdit.Font.Size:=SettingsForm.fontSize;
    if SettingsForm.AntialiasCheckbox.Checked
    then InputEdit.Font.Quality:=fqCleartypeNatural
    else InputEdit.Font.Quality:=fqNonAntialiased;

    OutputEdit.Font     :=InputEdit.Font;
    //ErrorStringGrid.Font:=InputEdit.Font;

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
    begin with adapter do begin
      {$ifdef useAdapterCS} EnterCriticalsection(cs); {$endif}
      try
        mnhForm.miHaltEvalutaion.Enabled:=true;
        MnhForm.OutputEdit.ClearAll;
        MnhForm.OutputEdit.Lines.Clear;
        MnhForm.ErrorStringGrid.RowCount:=0;
        MnhForm.ErrorGroupBox.Visible:=false;
        errorGridRequiresResize:=false;
      finally
        repeat evaluationRunning:=true; until evaluationRunning;
        evaluationStarted:=now;
        ad_updateForm;
        {$ifdef useAdapterCS} LeaveCriticalsection(cs); {$endif}
      end;
    end; end;

  PROCEDURE doneEval;
    begin with adapter do begin
      {$ifdef useAdapterCS} EnterCriticalsection(cs); {$endif}
      repeat evaluationRunning:=false; until not(evaluationRunning);
      if hasMessage(el5_systemError,HALT_MESSAGE) then begin
        MnhForm.StatusBar.SimpleText:='Halted after '+FloatToStr(round((now-evaluationStarted)*24*60*60*100)*0.01)+' sec.';
        sleepTime:=1000;
      end else begin
        MnhForm.StatusBar.SimpleText:='Done in '+FloatToStr(round((now-evaluationStarted)*24*60*60*100)*0.01)+' sec.';
        sleepTime:=1;
      end;
      {$ifdef useAdapterCS} LeaveCriticalsection(cs); {$endif}
      raiseError(el0_allOkay,'Evaluation done.',C_nilTokenLocation);
    end; end;

  begin
    with adapter do begin
      {$ifdef useAdapterCS} EnterCriticalsection(cs); {$endif}
      repeat evaluationLoopRunning:=true; until evaluationLoopRunning;
      {$ifdef useAdapterCS} LeaveCriticalsection(cs); {$endif}
    end;
    repeat
      if isReloadOfAllPackagesIndicated then begin
        enterEval;
        reloadAllPackages;
        raiseError(el0_allOkay,'Reload of all packages finished.',C_nilTokenLocation);
        doneEval;
      end else if isReloadOfMainPackageIndicated then begin
        enterEval;
        reloadMainPackage;
        raiseError(el0_allOkay,'Reload of main package finished.',C_nilTokenLocation);
        doneEval;
      end else if not(adapter.haltEvaluationLoopPosted) then begin
        Sleep(sleepTime);
        if (sleepTime<500) then inc(sleepTime)
        else if (sleepTime>500) then sleepTime:=500;
      end;
    until adapter.haltEvaluationLoopPosted;
    with adapter do begin
      {$ifdef useAdapterCS} EnterCriticalsection(cs); {$endif}
      clearAllPackages;
      directInputWrapper:=nil;
      fileWrapper:=nil;
      repeat evaluationLoopRunning:=false; until not(evaluationLoopRunning);
      {$ifdef useAdapterCS} LeaveCriticalsection(cs); {$endif}
    end;
  end;

PROCEDURE logError(CONST errorLevel:T_errorLevel; CONST errorMessage:ansistring; CONST errorLocation:T_tokenLocation);
  VAR row:longint;
  begin
    try
      MnhForm.ErrorGroupBox.Visible:=true;
      row:=MnhForm.ErrorStringGrid.RowCount;
      MnhForm.ErrorStringGrid.RowCount:=row+1;
      MnhForm.ErrorStringGrid.Cells[0,row]:=C_errorLevelTxt[errorLevel];
      MnhForm.ErrorStringGrid.Cells[1,row]:=errorMessage;
      MnhForm.ErrorStringGrid.Cells[2,row]:=errorLocation;
      if (errorLocation.provider=nil) or (errorLocation.provider^.filePath='') then begin
        MnhForm.ErrorStringGrid.Cells[3,row]:='';
        MnhForm.ErrorStringGrid.Cells[4,row]:='';
      end else begin
        MnhForm.ErrorStringGrid.Cells[3,row]:=errorLocation.provider^.filePath;
        MnhForm.ErrorStringGrid.Cells[4,row]:=IntToStr(errorLocation.line);
      end;
    finally
      adapter.errorGridRequiresResize:=true;
    end;
  end;

INITIALIZATION
  with adapter do begin
    {$ifdef useAdapterCS} InitCriticalSection(cs); {$endif}

    state:=as_directNoFile;
    currentlyEditingFile:='';
    fileWasEdited:=false;
    watchedFileAge:=0;

    directInputWrapper:=nil;
    fileWrapper:=nil;

    haltEvaluationLoopPosted :=false;
    evaluationRunning    :=false;
    evaluationLoopRunning:=false;
  end;
  mnh_out_adapters.inputDeclEcho:=@writeDeclEcho;
  mnh_out_adapters.inputExprEcho:=@writeExprEcho;
  mnh_out_adapters.exprOut      :=@writeExprOut;
  mnh_out_adapters.printOut     :=@writePrint;
  //mnh_out_adapters.errorOut     :=@logError;

FINALIZATION
  {$ifdef useAdapterCS} DoneCriticalsection(adapter.cs); {$endif}

end.

