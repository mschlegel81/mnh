unit mnh_gui_main;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, StdCtrls, ComCtrls, Grids, SynHighlighterMnh,
  mnh_fileWrappers, mnh_gui_settings, mnh_tokloc,
  mnh_out_adapters, mnh_stringutil, mnh_evalThread,mnh_constants;

type

  { TMnhForm }

  TMnhForm = class(TForm)
    MenuItem2: TMenuItem;
    miHaltEvalutaion: TMenuItem;
    miEvalModeDirect: TMenuItem;
    miEvaluateNow: TMenuItem;
    miEvalModeDirectOnKeypress: TMenuItem;
    miClear: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    OpenDialog: TOpenDialog;
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
    procedure miEvaluateNowClick(Sender: TObject);
    procedure miExpressionEchoClick(Sender: TObject);
    procedure miExpressionResultClick(Sender: TObject);
    procedure miHaltEvalutaionClick(Sender: TObject);
    procedure miIncFontSizeClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure mi_settingsClick(Sender: TObject);
    procedure OutputEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OutputEditMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure popMiOpenDeclarationClick(Sender: TObject);
    procedure UpdateTimeTimerTimer(Sender: TObject);

  private
    lastWordUnderCursor:string;
    underCursor:T_idInfo;

    settingsHaveBeenProcessed:boolean;
    procedure processSettings;
    PROCEDURE flushThroughput;
    PROCEDURE clearThroughput;
    { private declarations }
  public
    { public declarations }
  end;

var
  MnhForm: TMnhForm;

implementation
VAR throughputCS:TRTLCriticalSection;
    errorThroughput:array of T_storedError;
    outputThroughput:array of ansistring;
    lastFormRepaint:double=0;
    repaintNecessary:boolean=false;

{$R *.lfm}

PROCEDURE appendToOutputThroughput(CONST text:ansistring);
  begin
    EnterCriticalsection(throughputCS);
    setLength(outputThroughput,length(outputThroughput)+1);
    outputThroughput[length(outputThroughput)-1]:=text;
    LeaveCriticalsection(throughputCS);
  end;

PROCEDURE writeDeclEcho(CONST s:ansistring);
  begin
    appendToOutputThroughput(C_DeclEchoHead+' '+s);
  end;

PROCEDURE writeExprEcho(CONST s:ansistring);
  begin
    appendToOutputThroughput(C_ExprEchoHead+' '+s);
  end;

PROCEDURE writeExprOut (CONST s:ansistring);
  begin
    appendToOutputThroughput(C_ExprOutHead+' '+s);
  end;

PROCEDURE writePrint   (CONST s:ansistring);
  begin
    appendToOutputThroughput(s);
  end;

PROCEDURE logError(CONST error:T_storedError);
  begin
    {$ifdef debugMode}
    mnh_out_adapters.plainStdErrOut(error);
    {$endif}
    if error.errorLevel<el2_warning then exit;
    EnterCriticalsection(throughputCS);
    SetLength(errorThroughput,length(errorThroughput)+1);
    errorThroughput[length(errorThroughput)-1]:=error;
    LeaveCriticalsection(throughputCS);
  end;

procedure TMnhForm.flushThroughput;
  VAR i:longint;
      row:longint;
  begin
    EnterCriticalsection(throughputCS);
    for i:=0 to length(outputThroughput)-1 do OutputEdit.Lines.Append(outputThroughput[i]);
    SetLength(outputThroughput,0);

    //ErrorGroupBox.Visible:=ErrorGroupBox.Visible or (length(errorThroughput)>0);
    for i:=0 to length(errorThroughput)-1 do with errorThroughput[i] do begin
      row:=ErrorStringGrid.RowCount;
      ErrorStringGrid.RowCount:=row+1;
      ErrorStringGrid.Cells[0,row]:=C_errorLevelTxt[errorLevel];
      ErrorStringGrid.Cells[1,row]:=errorMessage;
      ErrorStringGrid.Cells[2,row]:=errorLocation;
      if (errorLocation.provider=nil) or (errorLocation.provider^.getPath='') then begin
        ErrorStringGrid.Cells[3,row]:='';
        ErrorStringGrid.Cells[4,row]:='';
      end else begin
        ErrorStringGrid.Cells[3,row]:=errorLocation.provider^.getPath;
        ErrorStringGrid.Cells[4,row]:=IntToStr(errorLocation.line);
      end;
    end;
    if (length(errorThroughput)>0) then ErrorStringGrid.AutoSizeColumns;
    setLength(errorThroughput,0);
    lastFormRepaint:=now;
    repaintNecessary:=false;
    LeaveCriticalsection(throughputCS);
  end;

procedure TMnhForm.clearThroughput;
  begin
    EnterCriticalsection(throughputCS);
    SetLength(outputThroughput,0);
    setLength(errorThroughput,0);
    LeaveCriticalsection(throughputCS);
  end;

PROCEDURE startOfEvaluationCallback;
  begin
    try
      EnterCriticalsection(throughputCS);
      MnhForm.clearThroughput;
      MnhForm.OutputEdit.Lines.Clear;
      MnhForm.ErrorStringGrid.RowCount:=0;
      //MnhForm.ErrorGroupBox.Visible:=false;
      repaintNecessary:=true;
      lastFormRepaint:=now;
      LeaveCriticalsection(throughputCS);
    except
    end;
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
    mnh_out_adapters.errorOut:=@logError;
    mnh_evalThread.startOfEvaluationCallback:=@startOfEvaluationCallback;
    mnh_out_adapters.inputDeclEcho:=@writeDeclEcho;
    mnh_out_adapters.inputExprEcho:=@writeExprEcho;
    mnh_out_adapters.exprOut      :=@writeExprOut;
    mnh_out_adapters.printOut     :=@writePrint;
  end;

procedure TMnhForm.EditorPopupPopup(Sender: TObject);
  begin
    underCursor:=ad_getIdInfo(lastWordUnderCursor);
    if underCursor.isBuiltIn then popMiOpenDeclaration.Caption:='"'+lastWordUnderCursor+'" is a built in function'
    else if underCursor.isUserDefined then popMiOpenDeclaration.Caption:='Open declaration of "'+lastWordUnderCursor+'" ('+underCursor.filename+')'
    else if underCursor.filename<>'' then popMiOpenDeclaration.Caption:='Open package "'+underCursor.filename+'"'
    else popMiOpenDeclaration.Caption:='"'+lastWordUnderCursor+'" cannot be resolved';
    popMiOpenDeclaration.Enabled:=underCursor.filename<>'';
  end;

procedure TMnhForm.FormDestroy(Sender: TObject);
  begin
    mnh_out_adapters.errorOut:=@mnh_out_adapters.plainStdErrOut;
    myhl.Destroy;
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
    if (miEvalModeDirectOnKeypress.Checked) then begin
      ad_evaluate(InputEdit.Lines);
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
    miEvalModeDirect.Checked:=true;
  end;

procedure TMnhForm.miEvalModeDirectOnKeypressClick(Sender: TObject);
  begin
    if miEvalModeDirectOnKeypress.Checked then exit;
    miEvalModeDirectOnKeypress.Checked:=true;
  end;

procedure TMnhForm.miEvaluateNowClick(Sender: TObject);
  begin
    ad_evaluate(InputEdit.Lines);
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
    OpenDialog.Title:='Open file';
    if OpenDialog.Execute and FileExists(OpenDialog.FileName)
    then ad_setFile(OpenDialog.FileName,InputEdit.Lines);
  end;

procedure TMnhForm.miSaveAsClick(Sender: TObject);
  begin
    if SaveDialog.Execute then begin
      MnhForm.InputEdit.Lines.SaveToFile(SaveDialog.FileName);
      ad_setFile(SaveDialog.FileName,InputEdit.Lines);
    end;
  end;

procedure TMnhForm.miSaveClick(Sender: TObject);
  begin
    if ad_currentFile<>'' then MnhForm.InputEdit.Lines.SaveToFile(ad_currentFile);
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
    with underCursor do
    if filename<>''
    then SettingsForm.canOpenFile(filename,fileline);
  end;

procedure TMnhForm.UpdateTimeTimerTimer(Sender: TObject);
  CONST MIN_INTERVALL=50;
        MAX_INTERVALL=1000;
        REPAINT_INTERVAL_IN_SECONDS=1;
  VAR aid:string;
      flag:boolean;
  begin
    EnterCriticalsection(throughputCS);
    //Form caption:-------------------------------------------------------------
    aid:='MNH5 '+ad_currentFile;
    if aid<>Caption then begin Caption:=aid; UpdateTimeTimer.Interval:=MIN_INTERVALL; end;
    //-------------------------------------------------------------:Form caption
    //Halt/Run enabled states:--------------------------------------------------
    flag:=ad_evaluationRunning;
    if flag<>miHaltEvalutaion.Enabled then begin miHaltEvalutaion.Enabled:=flag; UpdateTimeTimer.Interval:=MIN_INTERVALL; end;
    flag:=not(flag);
    if flag<>miEvaluateNow.Enabled then begin miEvaluateNow.Enabled:=flag; UpdateTimeTimer.Interval:=MIN_INTERVALL; end;
    //--------------------------------------------------:Halt/Run enabled states
    //progress time:------------------------------------------------------------
    flag:=(evaluationState.value=es_running);
    if flag then aid:=formatFloat('0.000',(now-startOfEvaluation.value)*(24*60*60))+'s'
            else aid:=endOfEvaluationText.value;
    if StatusBar.SimpleText<>aid then begin
      StatusBar.SimpleText:=aid;
      UpdateTimeTimer.Interval:=MIN_INTERVALL;
    end;
    //------------------------------------------------------------:progress time
    repaintNecessary:=repaintNecessary or (UpdateTimeTimer.Interval=MIN_INTERVALL);
    if UpdateTimeTimer.Interval<MAX_INTERVALL then UpdateTimeTimer.Interval:=UpdateTimeTimer.Interval+1;
    if ((now-lastFormRepaint)*24*60*60>REPAINT_INTERVAL_IN_SECONDS) and repaintNecessary then begin
      lastFormRepaint:=now;
      flushThroughput;
    end;
    LeaveCriticalsection(throughputCS);
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

initialization
  InitCriticalSection(throughputCS);
  setLength(errorThroughput,0);
  setLength(outputThroughput,0);

finalization;
  DoneCriticalsection(throughputCS);


end.

