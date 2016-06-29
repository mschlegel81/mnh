UNIT mnh_gui_main;

{$mode objfpc}{$H+}
INTERFACE
USES
  Classes, sysutils, FileUtil, SynEdit, SynEditTypes, SynCompletion, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, Menus, ComCtrls, Grids,
  SynHighlighterMnh, mnh_settings, mnh_gui_settings, mnh_tokLoc,
  mnh_out_adapters, myStringUtil, mnh_evalThread, mnh_constants,
  types, LCLType,mnh_plotData,mnh_funcs,mnh_litVar,mnh_doc,lclintf, StdCtrls,
  mnh_packages,closeDialog,askDialog,SynEditKeyCmds, SynMemo,
  myGenerics,mnh_fileWrappers,mySys,mnh_html,mnh_plotFuncs,mnh_cmdLineInterpretation,
  mnh_plotForm,newCentralPackageDialog,SynGutterMarks,SynEditMarks,mnh_contexts,
  SynEditMiscClasses, mnh_tokens, LazUTF8, mnh_tables;

CONST DEBUG_LINE_COUNT=200;

TYPE
  {$define includeInterface}
  {$include editorMeta.inc}
  {$include guiOutAdapter.inc}
  {$undef includeInterface}
  {$WARN 5024 OFF}
  { TMnhForm }

  TMnhForm = class(TForm)
    autosizeToggleBox: TToggleBox;
    debugItemsImageList: TImageList;
    MenuItem3: TMenuItem;
    miOpenDocumentationPack: TMenuItem;
    miWrapEcho: TMenuItem;
    mi_insertFilename: TMenuItem;
    miClose: TMenuItem;
    miMinErrorlevel5: TMenuItem;
    miOpenDocumentation: TMenuItem;
    MenuItem4: TMenuItem;
    miMinErrorlevel1: TMenuItem;
    miMinErrorlevel2: TMenuItem;
    miMinErrorlevel3: TMenuItem;
    miMinErrorlevel4: TMenuItem;
    miTimingInfo: TMenuItem;
    miDebug: TMenuItem;
    miCallMain: TMenuItem;
    miHelp: TMenuItem;
    miHelpExternally: TMenuItem;
    OutputEdit: TSynEdit;
    miFileHistory6: TMenuItem;
    miFileHistory7: TMenuItem;
    miFileHistory8: TMenuItem;
    miFileHistory9: TMenuItem;
    MenuItem2: TMenuItem;
    miFileHistory0: TMenuItem;
    miFileHistory1: TMenuItem;
    miFileHistory2: TMenuItem;
    miFileHistory3: TMenuItem;
    miFileHistory4: TMenuItem;
    miFileHistory5: TMenuItem;
    miHaltEvalutaion: TMenuItem;
    miEvalModeDirect: TMenuItem;
    miEvaluateNow: TMenuItem;
    miEvalModeDirectOnKeypress: TMenuItem;
    miClear: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    OpenDialog: TOpenDialog;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    Splitter1: TSplitter;
    submenuEditorAppearance: TMenuItem;
    miExpressionEcho: TMenuItem;
    miExpressionResult: TMenuItem;
    miDeclarationEcho: TMenuItem;
    miDecFontSize: TMenuItem;
    miIncFontSize: TMenuItem;
    mi_settings: TMenuItem;
    PageControl: TPageControl;
    SaveDialog: TSaveDialog;
    StatusBar: TStatusBar;
    SynCompletion: TSynCompletion;
    tbMicroStep: TToolButton;
    UpdateTimeTimer: TTimer;
    helpPopupMemo: TSynMemo;
    miOpenDemo: TMenuItem;
    miNewCentralPackage: TMenuItem;
    FindDialog: TFindDialog;
    ReplaceDialog: TReplaceDialog;
    MenuItem5: TMenuItem;
    miFind: TMenuItem;
    miReplace: TMenuItem;
    miFindNext: TMenuItem;
    miFindPrevious: TMenuItem;
    DebugToolbar: TToolBar;
    tbRun: TToolButton;
    tbStepIn: TToolButton;
    tbStep: TToolButton;
    tbStepOut: TToolButton;
    tbStop: TToolButton;
    SynGutterMarks0: TSynGutterMarks;
    breakpointsImagesList: TImageList;
    outputPageControl: TPageControl;
    outputTabSheet: TTabSheet;
    variablesTabSheet: TTabSheet;
    variablesStringGrid: TStringGrid;
    currentExpressionGroupBox: TGroupBox;
    currentExpressionMemo: TSynMemo;
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormDropFiles(Sender: TObject; CONST FileNames: array of string);
    PROCEDURE FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE InputEditChange(Sender: TObject);
    PROCEDURE InputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE InputEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE InputEditProcessUserCommand(Sender: TObject;
      VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
    PROCEDURE MenuItem4Click(Sender: TObject);
    PROCEDURE miClearClick(Sender: TObject);
    PROCEDURE miCloseClick(Sender: TObject);
    PROCEDURE miDebugCancelClick(Sender: TObject);
    PROCEDURE miDebugClick(Sender: TObject);
    PROCEDURE miDecFontSizeClick(Sender: TObject);
    PROCEDURE miDeclarationEchoClick(Sender: TObject);
    PROCEDURE miEvalModeDirectClick(Sender: TObject);
    PROCEDURE miEvalModeDirectOnKeypressClick(Sender: TObject);
    PROCEDURE miEvaluateNowClick(Sender: TObject);
    PROCEDURE miExpressionEchoClick(Sender: TObject);
    PROCEDURE miExpressionResultClick(Sender: TObject);
    PROCEDURE miFileHistory0Click(Sender: TObject);
    PROCEDURE miFileHistory1Click(Sender: TObject);
    PROCEDURE miFileHistory2Click(Sender: TObject);
    PROCEDURE miFileHistory3Click(Sender: TObject);
    PROCEDURE miFileHistory4Click(Sender: TObject);
    PROCEDURE miFileHistory5Click(Sender: TObject);
    PROCEDURE miFileHistory6Click(Sender: TObject);
    PROCEDURE miFileHistory7Click(Sender: TObject);
    PROCEDURE miFileHistory8Click(Sender: TObject);
    PROCEDURE miFileHistory9Click(Sender: TObject);
    PROCEDURE miHaltEvalutaionClick(Sender: TObject);
    PROCEDURE miHelpClick(Sender: TObject);
    PROCEDURE miHelpExternallyClick(Sender: TObject);
    PROCEDURE miIncFontSizeClick(Sender: TObject);
    PROCEDURE miMinErrorlevel1Click(Sender: TObject);
    PROCEDURE miMinErrorlevel2Click(Sender: TObject);
    PROCEDURE miMinErrorlevel3Click(Sender: TObject);
    PROCEDURE miMinErrorlevel4Click(Sender: TObject);
    PROCEDURE miMinErrorlevel5Click(Sender: TObject);
    PROCEDURE miOpenClick(Sender: TObject);
    PROCEDURE miOpenDocumentationPackClick(Sender: TObject);
    PROCEDURE miSaveAsClick(Sender: TObject);
    PROCEDURE miSaveClick(Sender: TObject);
    PROCEDURE miTimingInfoClick(Sender: TObject);
    PROCEDURE miWrapEchoClick(Sender: TObject);
    PROCEDURE mi_insertFilenameClick(Sender: TObject);
    PROCEDURE mi_settingsClick(Sender: TObject);
    PROCEDURE OutputEditKeyDown(Sender: TObject; VAR key: word;
      Shift: TShiftState);
    PROCEDURE OutputEditMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE OutputEditMouseUp(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE PageControlChange(Sender: TObject);
    PROCEDURE PopupNotifier1Close(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE Splitter1Moved(Sender: TObject);
    PROCEDURE SynCompletionCodeCompletion(VAR value: string;
      sourceValue: string; VAR SourceStart, SourceEnd: TPoint;
      KeyChar: TUTF8Char; Shift: TShiftState);
    PROCEDURE SynCompletionExecute(Sender: TObject);
    PROCEDURE SynCompletionSearchPosition(VAR APosition: integer);
    PROCEDURE tbMicroStepClick(Sender: TObject);
    PROCEDURE UpdateTimeTimerTimer(Sender: TObject);
    PROCEDURE miOpenDemoClick(Sender: TObject);
    PROCEDURE miNewCentralPackageClick(Sender: TObject);
    PROCEDURE miFindClick(Sender: TObject);
    PROCEDURE miReplaceClick(Sender: TObject);
    PROCEDURE FindDialogFind(Sender: TObject);
    PROCEDURE ReplaceDialogReplace(Sender: TObject);
    PROCEDURE ReplaceDialogFind(Sender: TObject);
    PROCEDURE miFindNextClick(Sender: TObject);
    PROCEDURE miFindPreviousClick(Sender: TObject);
    PROCEDURE tbRunClick(Sender: TObject);
    PROCEDURE tbStepInClick(Sender: TObject);
    PROCEDURE tbStepClick(Sender: TObject);
    PROCEDURE tbStepOutClick(Sender: TObject);
    PROCEDURE tbStopClick(Sender: TObject);
    PROCEDURE InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);

  private
    outputHighlighter,debugHighlighter,helpHighlighter:TSynMnhSyn;
    underCursor:T_tokenInfo;
    settingsReady:boolean;
    evaluation:record
      required:boolean;
      start:double;
      deferredUntil:double;
    end;
    lastStart:record
      mainCall:boolean;
      parameters:string;
    end;

    outputFocusedOnFind:boolean;
    forceInputEditFocusOnOutputEditMouseUp:boolean;

    doNotMarkWordBefore:double;
    doNotCheckFileBefore:double;
    breakPointHandlingPending:boolean;
    debugLine:record
      editor:TSynEdit;
      line:longint;
    end;
    wordsInEditor:T_listOfString;

    FUNCTION editForSearch(CONST replacing:boolean):TSynEdit;
    PROCEDURE processSettings;
    PROCEDURE processFileHistory;
    FUNCTION autosizeBlocks(CONST forceOutputFocus:boolean):boolean;
    PROCEDURE positionHelpNotifier;
    PROCEDURE setUnderCursor(CONST wordText:ansistring; CONST updateMarker:boolean);
    PROCEDURE ensureWordsInEditorForCompletion;

    PROCEDURE doConditionalPlotReset;
    PROCEDURE openFromHistory(CONST historyIdx:byte);
    PROCEDURE doStartEvaluation(CONST clearOutput,reEvaluating:boolean);
    PROCEDURE inputEditReposition(CONST caret:TPoint; CONST doJump,updateMarker:boolean);
    PROCEDURE outputEditReposition(CONST caret:TPoint; CONST doJump:boolean);
    PROCEDURE _setErrorlevel_(CONST i: byte);
    FUNCTION _doSaveAs_(CONST index:longint):boolean;
    FUNCTION _doSave_(CONST index:longint):boolean;
    PROCEDURE updateDebugParts;
    PROCEDURE handleBreak;
  public
    editorMeta:array of T_editorMeta;
    FUNCTION addEditorMetaForNewFile(CONST newFileName: ansistring=''):longint;
    FUNCTION addOrGetEditorMetaForFile(CONST fileName: ansistring):longint;
  end;

VAR MnhForm: TMnhForm;

PROCEDURE lateInitialization;
PROCEDURE formCycle(CONST ownId:longint; CONST next:boolean);
IMPLEMENTATION
VAR guiOutAdapter: T_guiOutAdapter;
    guiAdapters: T_adapters;
    tempAdapter: P_abstractOutAdapter;
    closeGuiFlag:boolean=false;
{$R *.lfm}
{$define includeImplementation}
{$include editorMeta.inc}
{$include guiOutAdapter.inc}
{$undef includeImplementation}


VAR inputHeightSpeed:longint=0;
FUNCTION TMnhForm.autosizeBlocks(CONST forceOutputFocus: boolean): boolean;
  CONST SAMPLE_TEXT='1!gPQ|';
  VAR temp,
      idealInputHeight,
      idealOutputHeight,
      availableTotalHeight,
      scrollbarHeight:longint;
      inputFocus:boolean;
  begin
    result:=false;
    if autosizeToggleBox.Checked and (PageControl.ActivePageIndex>=0) then with editorMeta[PageControl.ActivePageIndex] do begin
      scrollbarHeight     :=editor.height-editor.ClientHeight;
      idealInputHeight    :=scrollbarHeight+ editor    .Font.GetTextHeight(SAMPLE_TEXT)*(editor    .lines.count+1);
      if outputPageControl.activePage=variablesTabSheet
      then idealOutputHeight:=currentExpressionGroupBox.height+variablesStringGrid.RowCount*variablesStringGrid.DefaultRowHeight
      else idealOutputHeight:=scrollbarHeight+ OutputEdit.Font.GetTextHeight(SAMPLE_TEXT)*(OutputEdit.lines.count+1);
      //Are both editors large enough? Then return right now.
      if (editor.height>=idealInputHeight) and (OutputEdit.height>=idealOutputHeight) then exit;

      availableTotalHeight:=editor.height+outputPageControl.activePage.height;
      inputFocus:=not(forceOutputFocus or OutputEdit.Focused);
      if (idealInputHeight+idealOutputHeight<=availableTotalHeight) then begin
        //There is enough space for both -> priorize input before output
        idealInputHeight:=availableTotalHeight-idealOutputHeight;
      end else begin
        //There is not enough space for both
        temp:=round(0.9*availableTotalHeight);
        if inputFocus then begin
          if idealInputHeight>temp then idealInputHeight:=temp; //There is not even enough room for input edit!
          if idealInputHeight< availableTotalHeight-idealOutputHeight then
             idealInputHeight:=availableTotalHeight-idealOutputHeight;
          idealOutputHeight:=availableTotalHeight-idealInputHeight;
        end else begin
          if idealOutputHeight>temp then idealOutputHeight:=temp; //There is not even enough room for output edit!
          if idealOutputHeight< availableTotalHeight-idealInputHeight then
             idealOutputHeight:=availableTotalHeight-idealInputHeight;
          idealInputHeight:=availableTotalHeight-idealOutputHeight;
        end;
      end;
      if idealInputHeight<>editor.height then begin
        if idealInputHeight<editor.height then begin
          if inputHeightSpeed>=0 then inputHeightSpeed:=-1
                                 else dec(inputHeightSpeed);
        end else begin
          if inputHeightSpeed<=0 then inputHeightSpeed:=1
                                 else inc(inputHeightSpeed);
        end;
        PageControl.height:=PageControl.height+inputHeightSpeed;
        autosizeToggleBox.top:=outputPageControl.top;
        result:=true;
      end;
    end;
  end;

PROCEDURE TMnhForm.positionHelpNotifier;
  VAR maxLineLength:longint=0;
      i:longint;
      p:TPoint;
  begin
    p:=editorMeta[PageControl.ActivePageIndex].caretInMainFormCoordinates;

    helpPopupMemo.visible:=true;
    helpPopupMemo.Left:=p.x;
    helpPopupMemo.top :=p.y;
    for i:=0 to helpPopupMemo.lines.count-1 do if length(helpPopupMemo.lines[i])>maxLineLength then maxLineLength:=length(helpPopupMemo.lines[i]);
    if (maxLineLength=0) then begin
      helpPopupMemo.width:=0;
      helpPopupMemo.height:=0;
    end else begin
      helpPopupMemo.width:=helpPopupMemo.CharWidth*(maxLineLength+1);
      helpPopupMemo.height:=helpPopupMemo.LineHeight*(helpPopupMemo.lines.count+1);
    end;
    if helpPopupMemo.Left>width-helpPopupMemo.width then helpPopupMemo.Left:=width-helpPopupMemo.width;
    if helpPopupMemo.Left<0 then helpPopupMemo.Left:=0;
  end;

PROCEDURE TMnhForm.setUnderCursor(CONST wordText: ansistring; CONST updateMarker: boolean);
  VAR i:longint;
  begin
    if updateMarker then begin
      outputHighlighter.setMarkedWord(wordText);
      for i:=0 to length(editorMeta)-1 do editorMeta[i].setMarkedWord(wordText);
    end;
    if miHelp.Checked then begin
      i:=PageControl.ActivePageIndex;
      docEvaluator.explainIdentifier(editorMeta[i].editor.lines[editorMeta[i].editor.CaretY-1],editorMeta[i].editor.CaretY,editorMeta[i].editor.CaretX,underCursor);
      helpPopupMemo.text:=underCursor.tokenText+C_lineBreakChar+underCursor.tokenExplanation;
      positionHelpNotifier;
    end;
  end;

PROCEDURE TMnhForm.doConditionalPlotReset;
  begin
    if plotForm.miAutoReset.Checked then begin
      guiAdapters.plot.setDefaults;
      plotForm.pullPlotSettingsToGui();
    end;
  end;

PROCEDURE TMnhForm.openFromHistory(CONST historyIdx: byte);
  begin
    with settings.value^ do begin
      if fileExists(historyItem(historyIdx))
      then PageControl.ActivePageIndex:=addOrGetEditorMetaForFile(historyItem(historyIdx))
      else if polishHistory then processFileHistory;
    end;
  end;

PROCEDURE TMnhForm.doStartEvaluation(CONST clearOutput, reEvaluating: boolean);
  VAR i:longint;
      logName:string;
  begin
    with evaluation do begin
      required:=false;
      deferredUntil:=now+0.1*ONE_SECOND;
      start:=now;
    end;
    if not(reEvaluating) then begin
      editorMeta[PageControl.ActivePageIndex].setWorkingDir;
      if clearOutput then begin
        guiOutAdapter.flushClear;
        UpdateTimeTimerTimer(self);
        doConditionalPlotReset;
      end;
      logName:=settings.value^.getLogName;
      if logName<>'' then begin
        if tempAdapter=nil
        then tempAdapter:=addOutfile(guiAdapters,logName)
        else if settings.value^.logPerRun then begin
          guiAdapters.removeOutAdapter(tempAdapter);
          tempAdapter:=addOutfile(guiAdapters,logName);
        end;
      end;
    end;
    underCursor.tokenText:='';
    if miDebug.Checked then begin
      currentlyDebugging:=true;
      for i:=0 to length(editorMeta)-1 do editorMeta[i].startDebugging;
      stepper.doStart(false);
      updateDebugParts;
      breakPointHandlingPending:=true;
    end else currentlyDebugging:=false;
    UpdateTimeTimer.interval:=20;
  end;

PROCEDURE TMnhForm.inputEditReposition(CONST caret: TPoint; CONST doJump,
  updateMarker: boolean);
  VAR wordUnderCursor:string;
      newCaret:TPoint;
      pageIdx:longint;
  begin
    with editorMeta[PageControl.ActivePageIndex] do begin
      wordUnderCursor:=editor.GetWordAtRowCol(caret);
      setUnderCursor(wordUnderCursor,updateMarker);
      if not(doJump) then exit;
      if (underCursor.tokenText<>wordUnderCursor) or
         (underCursor.location.column<=0) then exit;
      if (underCursor.location.fileName='') or (underCursor.location.fileName='?') then exit;
      pageIdx:=addOrGetEditorMetaForFile(underCursor.location.fileName);
      if pageIdx>=0 then begin
        PageControl.ActivePageIndex:=pageIdx;
        newCaret.x:=underCursor.location.column;
        newCaret.y:=underCursor.location.line;
        editorMeta[pageIdx].editor.CaretXY:=newCaret;
      end;
    end;
  end;

PROCEDURE TMnhForm.outputEditReposition(CONST caret: TPoint;
  CONST doJump: boolean);
  VAR loc:T_searchTokenLocation;
      newCaret:TPoint;
      pageIdx:longint;
  begin
    forceInputEditFocusOnOutputEditMouseUp:=false;
    setUnderCursor(OutputEdit.GetWordAtRowCol(caret),true);
    loc:=guessLocationFromString(OutputEdit.lines[caret.y-1],false);
    if not(doJump) then exit;
    if (loc.fileName='') or (loc.fileName='?') then exit;
    pageIdx:=addOrGetEditorMetaForFile(loc.fileName);
    if pageIdx<0 then exit;
    PageControl.ActivePageIndex:=pageIdx;
    with editorMeta[pageIdx] do begin
      editor.SetFocus;
      highlighter.setMarkedToken(loc.line-1,loc.column-1);
      newCaret.x:=loc.column;
      newCaret.y:=loc.line;
      editor.CaretXY:=newCaret;
      forceInputEditFocusOnOutputEditMouseUp:=true;
    end;
  end;

PROCEDURE TMnhForm.FormCreate(Sender: TObject);
  VAR i:longint;
  begin
    wordsInEditor.create;
    forceInputEditFocusOnOutputEditMouseUp:=false;
    settingsReady:=false;
    with evaluation do begin
      required:=false;
      deferredUntil:=now;
      start:=now;
    end;
    lastStart.mainCall:=false;
    doNotMarkWordBefore:=now;
    doNotCheckFileBefore:=now+ONE_SECOND;
    outputHighlighter:=TSynMnhSyn.create(nil,msf_output);
    OutputEdit.highlighter:=outputHighlighter;
    helpHighlighter:=TSynMnhSyn.create(nil,msf_guessing);
    helpPopupMemo.highlighter:=helpHighlighter;
    OutputEdit.ClearAll;
    debugHighlighter:=TSynMnhSyn.create(nil,msf_input);
    currentExpressionMemo.highlighter:=debugHighlighter;
    for i:=0 to length(LOGO)-1 do OutputEdit.lines.append(LOGO[i]);
    {$ifdef debugMode}
    if wantConsoleAdapter then guiAdapters.addConsoleOutAdapter;
    {$endif}
    mnh_out_adapters.gui_started:=true;
    updateDebugParts;
  end;

PROCEDURE TMnhForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  VAR i:integer;
  PROCEDURE uninstall;
    {$i res_removeMnhFileAssociations.inc}
    begin
      runAlone(removeMnhFileAssociations_mnh);
      runAlone('deleteDir('+escapeString(configDir)+')');
      DeleteFile('mnh_light.exe');
      {$ifdef Windows}
      deleteMyselfOnExit;
      {$endif}
      halt;
    end;

  begin
    if SettingsForm.uninstallToggleBox.Checked then begin
      i:=closeDialogForm.showOnUninstall;
      if i=mrCancel then begin
        CloseAction:=caNone;
        exit;
      end;
      if i=mrOk then uninstall;
    end;

    if runEvaluator.evaluationRunning then runEvaluator.haltEvaluation;
    stepper.doStop;
    for i:=0 to length(editorMeta)-1 do editorMeta[i].writeToEditorState(settings.value);
  end;

PROCEDURE TMnhForm.FormDestroy(Sender: TObject);
  begin
    if not(reEvaluationWithGUIrequired) then saveSettings;
    guiAdapters.removeOutAdapter(@guiOutAdapter);
    outputHighlighter.destroy;
    debugHighlighter.destroy;
    helpHighlighter.destroy;
    wordsInEditor.destroy;
  end;

PROCEDURE TMnhForm.FormDropFiles(Sender: TObject; CONST FileNames: array of string);
  VAR i:longint;
  begin
    for i:=0 to length(FileNames)-1 do PageControl.ActivePageIndex:=addOrGetEditorMetaForFile(FileNames[i]);
  end;

PROCEDURE TMnhForm.FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=9) and (ssCtrl in Shift) then formCycle(0,ssShift in Shift)
    else if (key=87) and (Shift=[ssCtrl]) then miCloseClick(Sender);
  end;

PROCEDURE TMnhForm.FormResize(Sender: TObject);
  begin
    if settingsReady then with settings.value^ do begin
      mainForm.top   :=top;
      mainForm.Left  :=Left;
      mainForm.width :=width;
      mainForm.height:=height;
      mainForm.isFullscreen:=(WindowState=wsMaximized);
    end else plotForm.pullPlotSettingsToGui();
    if helpPopupMemo.visible then positionHelpNotifier;
  end;

PROCEDURE TMnhForm.FormShow(Sender: TObject);
  VAR i:longint;
  begin
    if not(settingsReady) then begin
      processSettings;
      i:=settings.value^.activePage;
      PageControl.ActivePageIndex:=i;
      if (i>=0) and (i<length(editorMeta)) then SynCompletion.editor:=editorMeta[PageControl.ActivePageIndex].editor;
      {$ifdef UNIX}
      miIncFontSize.ShortCut:=16605;
      {$endif}
    end;
    KeyPreview:=true;
    UpdateTimeTimer.Enabled:=true;
    if reEvaluationWithGUIrequired then begin
      showConsole;
      {$ifndef debugMode}guiAdapters.addConsoleOutAdapter;{$endif}
      doStartEvaluation(true,true);
      runEvaluator.reEvaluateWithGUI;
      plotForm.Caption:=plotForm.Caption+' - close to quit';
      sleep(UpdateTimeTimer.interval);
    end;
  end;

PROCEDURE TMnhForm.InputEditChange(Sender: TObject);
  begin
    if (PageControl.ActivePageIndex<0) or
       (PageControl.ActivePageIndex>=length(editorMeta)) or
       (not(editorMeta[PageControl.ActivePageIndex].sheet.TabVisible)) then exit;

    if (miEvalModeDirectOnKeypress.Checked) and not(SynCompletion.IsActive) then begin
      if now>evaluation.deferredUntil then begin
        doStartEvaluation(false,false);
        lastStart.mainCall:=false;
        with editorMeta[PageControl.ActivePageIndex] do runEvaluator.evaluate(pseudoName,editor.lines,true);
      end else evaluation.required:=true;
    end;
    with editorMeta[PageControl.ActivePageIndex] do docEvaluator.evaluate(pseudoName,editor.lines,false);
    editorMeta[PageControl.ActivePageIndex].changed:=editorMeta[PageControl.ActivePageIndex].editor.Modified;
    Caption:=editorMeta[PageControl.ActivePageIndex].updateSheetCaption;
  end;

PROCEDURE TMnhForm.InputEditKeyDown(Sender: TObject; VAR key: word;
  Shift: TShiftState);
  begin
    if (key=13) and ((ssCtrl in Shift) or (ssAlt in Shift))
    then inputEditReposition(editorMeta[PageControl.ActivePageIndex].editor.CaretXY,ssCtrl in Shift,true)
    else inputEditReposition(editorMeta[PageControl.ActivePageIndex].editor.CaretXY,false,false);
    if currentlyDebugging and runEvaluator.evaluationRunning then begin
      if (key=116) and tbRun      .Enabled then tbRunClick(Sender);
      if (key=117) and tbStepIn   .Enabled then tbStepInClick(Sender);
      if (key=118) and tbStep     .Enabled then tbStepClick(Sender);
      if (key=119) and tbStepOut  .Enabled then tbStepOutClick(Sender);
      if (key=122) and tbMicroStep.Enabled then tbMicroStepClick(Sender);
    end;
  end;

PROCEDURE TMnhForm.InputEditMouseDown(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    inputEditReposition(editorMeta[PageControl.ActivePageIndex].editor.PixelsToRowColumn(point),ssCtrl in Shift,true);
  end;

PROCEDURE TMnhForm.InputEditProcessUserCommand(Sender: TObject;
  VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
  VAR i:longint;
  begin
    if command=ecUserDefinedFirst then editorMeta[PageControl.ActivePageIndex].toggleComment;
    if command=ecUserDefinedFirst+1 then begin
      for i:=1 to length(editorMeta)-1 do if editorMeta[(i+PageControl.ActivePageIndex) mod length(editorMeta)].sheet.TabVisible then begin
        PageControl.ActivePageIndex:=(i+PageControl.ActivePageIndex) mod length(editorMeta);
        editorMeta[PageControl.ActivePageIndex].editor.SetFocus;
        exit;
      end;
    end;
    if command=ecUserDefinedFirst+2 then begin
      for i:=length(editorMeta)-1 downto 1 do if editorMeta[(i+PageControl.ActivePageIndex) mod length(editorMeta)].sheet.TabVisible then begin
        PageControl.ActivePageIndex:=(i+PageControl.ActivePageIndex) mod length(editorMeta);
        editorMeta[PageControl.ActivePageIndex].editor.SetFocus;
        exit;
      end;
    end;
    if (command=ecUserDefinedFirst+3) then begin
      editorMeta[PageControl.ActivePageIndex].toggleBreakpoint;
      stepper.clearBreakpoints;
      for i:=0 to length(editorMeta)-1 do editorMeta[i].setStepperBreakpoints;
      if runEvaluator.evaluationRunning and not(miDebug.Checked) then runEvaluator.haltEvaluation;
      miDebug.Checked:=true;
      updateDebugParts;
    end;
  end;

PROCEDURE TMnhForm.MenuItem4Click(Sender: TObject);
  begin
    if PageControl.ActivePageIndex<0 then exit;
    askForm.initWithQuestion('Please give command line parameters');
    if askForm.ShowModal=mrOk then begin
      doStartEvaluation(true,false);
      lastStart.mainCall:=true;
      lastStart.parameters:=askForm.getLastAnswerReleasing(nil);
      with editorMeta[PageControl.ActivePageIndex] do runEvaluator.callMain(pseudoName,editor.lines,lastStart.parameters);
    end else askForm.getLastAnswerReleasing(nil);
  end;

PROCEDURE TMnhForm.miClearClick(Sender: TObject);
  begin
    PageControl.ActivePageIndex:=addEditorMetaForNewFile();
  end;

PROCEDURE TMnhForm.miCloseClick(Sender: TObject);
  VAR i,mr:longint;
  begin
    if (PageControl.ActivePageIndex<0) or (PageControl.ActivePageIndex>=length(editorMeta)) then exit;
    with editorMeta[PageControl.ActivePageIndex] do begin
      if changed then begin
        mr:=closeDialogForm.showOnLoad;
        if mr=mrOk then if not(_doSave_(PageControl.ActivePageIndex)) then exit;
        if mr=mrCancel then exit;
      end;
      if filePath<>'' then begin
        settings.value^.fileClosed(filePath);
        processFileHistory;
      end;
      closeEditor;
    end;

    mr:=-1;
    for i:=0 to length(editorMeta)-1 do if editorMeta[i].sheet.TabVisible then mr:=i;
    if mr=-1 then PageControl.ActivePageIndex:=addEditorMetaForNewFile()
             else PageControl.ActivePageIndex:=mr;
  end;

PROCEDURE TMnhForm.miDebugCancelClick(Sender: TObject);
  begin
    runEvaluator.haltEvaluation;
    stepper.doStop;
  end;

PROCEDURE TMnhForm.miDebugClick(Sender: TObject);
  begin
    if runEvaluator.evaluationRunning then runEvaluator.haltEvaluation;
    miDebug.Checked:=not(miDebug.Checked);
    updateDebugParts;
  end;

PROCEDURE TMnhForm.miDecFontSizeClick(Sender: TObject);
  begin
    if settingsReady then begin
      SettingsForm.fontSize:=SettingsForm.fontSize-1;
      processSettings;
    end;
  end;

PROCEDURE TMnhForm.miDeclarationEchoClick(Sender: TObject);
  begin
    if settingsReady then begin
      miDeclarationEcho.Checked:=not(miDeclarationEcho.Checked);
      guiAdapters.doEchoDeclaration:=miDeclarationEcho.Checked;
      settings.value^.outputBehaviour:=guiAdapters.outputBehaviour;
    end;
  end;

PROCEDURE TMnhForm.miEvalModeDirectClick(Sender: TObject);
  begin
    if miEvalModeDirect.Checked then exit;
    miEvalModeDirect.Checked:=true;
    miEvalModeDirectOnKeypress.Checked:=false;
    settings.value^.instantEvaluation:=false;
  end;

PROCEDURE TMnhForm.miEvalModeDirectOnKeypressClick(Sender: TObject);
  begin
    if miEvalModeDirectOnKeypress.Checked then exit;
    if runEvaluator.evaluationRunning and (miDebug.Checked) then runEvaluator.haltEvaluation;
    miDebug.Checked:=false;
    updateDebugParts;
    miEvalModeDirect.Checked:=false;
    miEvalModeDirectOnKeypress.Checked:=true;
    settings.value^.instantEvaluation:=true;
  end;

PROCEDURE TMnhForm.miEvaluateNowClick(Sender: TObject);
  begin
    if now>evaluation.deferredUntil then begin
      doStartEvaluation(true,false);
      lastStart.mainCall:=false;
      with editorMeta[PageControl.ActivePageIndex] do runEvaluator.evaluate(pseudoName,editor.lines,false);
    end else evaluation.required:=true;
  end;

PROCEDURE TMnhForm.miExpressionEchoClick(Sender: TObject);
  begin
    if settingsReady then begin
      miExpressionEcho.Checked:=not(miExpressionEcho.Checked);
      guiAdapters.doEchoInput:=miExpressionEcho.Checked;
      settings.value^.outputBehaviour:=guiAdapters.outputBehaviour;
    end;
  end;

PROCEDURE TMnhForm.miExpressionResultClick(Sender: TObject);
  begin
    if settingsReady then begin
      miExpressionResult.Checked:=not(miExpressionResult.Checked);
      guiAdapters.doShowExpressionOut:=miExpressionResult.Checked;
      settings.value^.outputBehaviour:=guiAdapters.outputBehaviour;
    end;
  end;

PROCEDURE TMnhForm.miFileHistory0Click(Sender: TObject); begin openFromHistory(0); end;
PROCEDURE TMnhForm.miFileHistory1Click(Sender: TObject); begin openFromHistory(1); end;
PROCEDURE TMnhForm.miFileHistory2Click(Sender: TObject); begin openFromHistory(2); end;
PROCEDURE TMnhForm.miFileHistory3Click(Sender: TObject); begin openFromHistory(3); end;
PROCEDURE TMnhForm.miFileHistory4Click(Sender: TObject); begin openFromHistory(4); end;
PROCEDURE TMnhForm.miFileHistory5Click(Sender: TObject); begin openFromHistory(5); end;
PROCEDURE TMnhForm.miFileHistory6Click(Sender: TObject); begin openFromHistory(6); end;
PROCEDURE TMnhForm.miFileHistory7Click(Sender: TObject); begin openFromHistory(7); end;
PROCEDURE TMnhForm.miFileHistory8Click(Sender: TObject); begin openFromHistory(8); end;
PROCEDURE TMnhForm.miFileHistory9Click(Sender: TObject); begin openFromHistory(9); end;

PROCEDURE TMnhForm.miHaltEvalutaionClick(Sender: TObject);
  begin
    runEvaluator.haltEvaluation;
  end;

PROCEDURE TMnhForm.miHelpClick(Sender: TObject);
  begin
    miHelp.Checked:=not(miHelp.Checked);
    if not(miHelp.Checked) then helpPopupMemo.visible:=false
                           else begin
                             helpPopupMemo.visible:=true;
                             inputEditReposition(editorMeta[PageControl.ActivePageIndex].editor.CaretXY,false,false);
                           end;
  end;

PROCEDURE TMnhForm.miHelpExternallyClick(Sender: TObject);
  begin
    makeAndShowDoc(false);
  end;

PROCEDURE TMnhForm.miIncFontSizeClick(Sender: TObject);
  begin
    if settingsReady then begin
      SettingsForm.fontSize:=SettingsForm.fontSize+1;
      processSettings;
    end;
  end;

PROCEDURE TMnhForm._setErrorlevel_(CONST i: byte);
  begin
    if settingsReady then begin
      case i of
        1: miMinErrorlevel1.Checked:=true;
        2: miMinErrorlevel2.Checked:=true;
        3: miMinErrorlevel3.Checked:=true;
        4: miMinErrorlevel4.Checked:=true;
        5: miMinErrorlevel5.Checked:=true;
      end;
      guiAdapters.minErrorLevel:=i;
      settings.value^.outputBehaviour:=guiAdapters.outputBehaviour;
    end;
  end;

FUNCTION TMnhForm._doSaveAs_(CONST index: longint): boolean;
  VAR arr:T_arrayOfString;
      i:longint;
  begin
    if index<0 then exit(false);
    if SaveDialog.execute then with editorMeta[index] do begin
      filePath:=expandFileName(SaveDialog.fileName);
      setLength(arr,editor.lines.count);
      for i:=0 to length(arr)-1 do arr[i]:=editor.lines[i];
      writeFileLines(filePath,arr,'');
      fileAge(filePath,fileAccessAge);
      changed:=false;
      result:=true;
      Caption:=editorMeta[index].updateSheetCaption;
    end else result:=false;
  end;

FUNCTION TMnhForm._doSave_(CONST index: longint): boolean;
  VAR arr:T_arrayOfString;
      i:longint;
  begin
    if index<0 then exit(false);
    with editorMeta[index] do if filePath='' then result:=_doSaveAs_(index)
    else begin
      setLength(arr,editor.lines.count);
      for i:=0 to length(arr)-1 do arr[i]:=editor.lines[i];
      writeFileLines(filePath,arr,'');
      fileAge(filePath,fileAccessAge);
      changed:=false;
      result:=true;
      Caption:=editorMeta[index].updateSheetCaption;
    end;
  end;

PROCEDURE TMnhForm.updateDebugParts;

  PROCEDURE handleButton(VAR button:TToolButton; CONST Enabled:boolean; CONST enabledImageIndex:longint);
    begin
      button.Enabled:=Enabled;
      if Enabled then button.ImageIndex:=enabledImageIndex
                 else button.ImageIndex:=enabledImageIndex+1;
    end;

  VAR i:longint;
  begin
    if miDebug.Checked then begin
      for i:=0 to length(editorMeta)-1 do editorMeta[i].editor.Gutter.MarksPart.visible:=true;
      DebugToolbar.visible:=true;
      DebugToolbar.Enabled:=true;
      DebugToolbar.top:=0;
      outputPageControl.ShowTabs:=true;
      handleButton(tbStop,runEvaluator.evaluationRunning,2);
      handleButton(tbRun,not(runEvaluator.evaluationRunning) or stepper.haltet,0);
      handleButton(tbStep,runEvaluator.evaluationRunning and stepper.haltet,4);
      handleButton(tbStepIn,runEvaluator.evaluationRunning and stepper.haltet,6);
      handleButton(tbStepOut,runEvaluator.evaluationRunning and stepper.haltet,8);
      handleButton(tbMicroStep,runEvaluator.evaluationRunning and stepper.haltet,10);
    end else begin
      for i:=0 to length(editorMeta)-1 do editorMeta[i].editor.Gutter.MarksPart.visible:=false;
      outputPageControl.ShowTabs:=false;
      outputPageControl.activePage:=outputTabSheet;
      DebugToolbar.visible:=false;
      DebugToolbar.Enabled:=false;
    end;
  end;

PROCEDURE TMnhForm.handleBreak;
  PROCEDURE jumpToFile;
    VAR pageIdx:longint;
        newCaret:TPoint;
    begin
      debugLine.line:=-1;
      if (stepper.loc.package=nil) then exit;
      pageIdx:=addOrGetEditorMetaForFile(stepper.loc.package^.getPath);
      if pageIdx>=0 then begin
        PageControl.ActivePageIndex:=pageIdx;
        newCaret.x:=stepper.loc.column;
        newCaret.y:=stepper.loc.line;
        editorMeta[pageIdx].editor.CaretXY:=newCaret;
        debugLine.editor:=editorMeta[pageIdx].editor;
        debugLine.line:=newCaret.y;
        editorMeta[pageIdx].editor.Repaint;
      end;
    end;

  VAR context:P_evaluationContext;
      report:T_variableReport;
      i:longint;
      first:P_token;
      stack:P_tokenStack;
  begin
    if not(stepper).haltet then exit;
    breakPointHandlingPending:=false;
    jumpToFile;

    context:=P_evaluationContext(stepper.context);
    report.create;
    runEvaluator.reportVariables(report);
    if context<>nil then context^.reportVariables(report);

    variablesStringGrid.RowCount:=length(report.dat)+1;
    for i:=0 to length(report.dat)-1 do begin
      variablesStringGrid.Cells[0,length(report.dat)-i]:=report.dat[i].location;
      variablesStringGrid.Cells[1,length(report.dat)-i]:=report.dat[i].id;
      variablesStringGrid.Cells[2,length(report.dat)-i]:=report.dat[i].value^.typeString;
      variablesStringGrid.Cells[3,length(report.dat)-i]:=report.dat[i].value^.toString;
    end;

    first:=stepper.token;
    stack:=stepper.stack;
    currentExpressionMemo.clear;
    if first<>nil then currentExpressionMemo.lines.append(stack^.toString(first,30));

    updateDebugParts;
  end;

FUNCTION TMnhForm.addEditorMetaForNewFile(CONST newFileName: ansistring):longint;
  VAR i:longint;
  begin
    i:=length(editorMeta)-1;
    //decrease i until a visible meta is encountered
    while (i>=0) and not(editorMeta[i].sheet.TabVisible) do dec(i);
    inc(i);
    //i now is the index of the last visible editor meta +1
    if (i>=0) and (i<length(editorMeta)) then begin
      editorMeta[i].initForNewFile;
      exit(i);
    end;

    i:=length(editorMeta)-1;
    while (i>=0) and (editorMeta[i].sheet.visible) do dec(i);
    i:=length(editorMeta);
    setLength(editorMeta,i+1);
    editorMeta[i].create(i);
    editorMeta[i].editor.Font:=OutputEdit.Font;
    if newFileName<>'' then _doSave_(i);
    result:=i;
    if miDebug.Checked then editorMeta[i].editor.Gutter.MarksPart.visible:=true;
  end;

FUNCTION TMnhForm.addOrGetEditorMetaForFile(CONST fileName: ansistring): longint;
  FUNCTION isPseudoName:boolean;
    begin
      result:=(length(fileName)>1)
          and (fileName[1]='<')
          and (fileName[length(fileName)]='>');
    end;

  VAR filePath:ansistring;
      i:longint;
  begin
    if isPseudoName then begin
      for i:=0 to length(editorMeta)-1 do if (editorMeta[i].sheet.TabVisible) and (editorMeta[i].pseudoName=fileName) then exit(i);
      result:=-1;
    end else begin
      filePath:=expandFileName(fileName);
      for i:=0 to length(editorMeta)-1 do if (editorMeta[i].sheet.TabVisible) and (editorMeta[i].filePath=filePath) then exit(i);
      result:=addEditorMetaForNewFile();
      editorMeta[result].setFile(filePath);
      editorMeta[result].editor.Font:=OutputEdit.Font;
    end;
  end;

PROCEDURE TMnhForm.miMinErrorlevel1Click(Sender: TObject); begin _setErrorlevel_(1); end;
PROCEDURE TMnhForm.miMinErrorlevel2Click(Sender: TObject); begin _setErrorlevel_(2); end;
PROCEDURE TMnhForm.miMinErrorlevel3Click(Sender: TObject); begin _setErrorlevel_(3); end;
PROCEDURE TMnhForm.miMinErrorlevel4Click(Sender: TObject); begin _setErrorlevel_(4); end;
PROCEDURE TMnhForm.miMinErrorlevel5Click(Sender: TObject); begin _setErrorlevel_(5); end;

PROCEDURE TMnhForm.miOpenClick(Sender: TObject);
  begin
    OpenDialog.FilterIndex:=1;
    OpenDialog.options:=OpenDialog.options+[ofPathMustExist,ofFileMustExist];
    OpenDialog.title:='Open file';
    if OpenDialog.execute and fileExists(OpenDialog.fileName)
    then PageControl.ActivePageIndex:=addOrGetEditorMetaForFile(OpenDialog.fileName);
  end;

PROCEDURE TMnhForm.miOpenDocumentationPackClick(Sender: TObject);
  begin
    makeAndShowDoc(true);
  end;

PROCEDURE TMnhForm.miSaveAsClick(Sender: TObject);
  begin
    _doSaveAs_(PageControl.ActivePageIndex);
  end;

PROCEDURE TMnhForm.miSaveClick(Sender: TObject);
  begin
    _doSave_(PageControl.ActivePageIndex);
  end;

PROCEDURE TMnhForm.miTimingInfoClick(Sender: TObject);
  begin
    if settingsReady then begin
      miTimingInfo.Checked:=not(miTimingInfo.Checked);
      guiAdapters.doShowTimingInfo:=miTimingInfo.Checked;
      settings.value^.outputBehaviour:=guiAdapters.outputBehaviour;
    end;
  end;

PROCEDURE TMnhForm.miWrapEchoClick(Sender: TObject);
  begin
    settings.value^.wordWrapEcho:=not(settings.value^.wordWrapEcho);
    miWrapEcho.Checked:=settings.value^.wordWrapEcho;
  end;

PROCEDURE TMnhForm.mi_insertFilenameClick(Sender: TObject);
  begin
    OpenDialog.FilterIndex:=2;
    OpenDialog.options:=OpenDialog.options-[ofPathMustExist,ofFileMustExist];
    if OpenDialog.execute then editorMeta[PageControl.ActivePageIndex].insertText(escapeString(OpenDialog.fileName));
  end;

PROCEDURE TMnhForm.mi_settingsClick(Sender: TObject);
  begin
    SettingsForm.ShowModal;
    processSettings;
  end;

PROCEDURE TMnhForm.OutputEditKeyDown(Sender: TObject; VAR key: word;
  Shift: TShiftState);
  begin
    if ((key=13) and (ssCtrl in Shift)) then outputEditReposition(OutputEdit.CaretXY,true);
    if forceInputEditFocusOnOutputEditMouseUp and (PageControl.ActivePageIndex>=0) then ActiveControl:=editorMeta[PageControl.ActivePageIndex].editor;
    forceInputEditFocusOnOutputEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.OutputEditMouseDown(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    outputEditReposition(OutputEdit.PixelsToRowColumn(point),ssCtrl in Shift);
  end;

PROCEDURE TMnhForm.OutputEditMouseUp(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  begin
    if forceInputEditFocusOnOutputEditMouseUp then ActiveControl:=editorMeta[PageControl.ActivePageIndex].editor;
    forceInputEditFocusOnOutputEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.PageControlChange(Sender: TObject);
  begin
    if PageControl.ActivePageIndex>=0 then begin
      SynCompletion.editor:=editorMeta[PageControl.ActivePageIndex].editor;
      settings.value^.activePage:=PageControl.ActivePageIndex;
      with editorMeta[PageControl.ActivePageIndex] do docEvaluator.evaluate(pseudoName,editor.lines,false);
    end;
  end;

PROCEDURE TMnhForm.PopupNotifier1Close(Sender: TObject;
  VAR CloseAction: TCloseAction);
  begin
    miHelp.Checked:=false;
  end;

PROCEDURE TMnhForm.Splitter1Moved(Sender: TObject);
  begin
    if helpPopupMemo.visible then positionHelpNotifier;
    autosizeToggleBox.top:=outputPageControl.top;
    autosizeToggleBox.Checked:=false;
  end;

PROCEDURE TMnhForm.SynCompletionCodeCompletion(VAR value: string; sourceValue: string; VAR SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
  begin
    value:=copy(sourceValue,1,LastDelimiter('.',sourceValue)-1)+value;
    wordsInEditor.clear;
  end;

PROCEDURE TMnhForm.ensureWordsInEditorForCompletion;
  VAR i:longint;
      caret:TPoint;
  begin
    if wordsInEditor.size>0 then exit;
    if PageControl.ActivePageIndex>=0 then with editorMeta[PageControl.ActivePageIndex] do begin
      caret:=editor.CaretXY;
      for i:=0 to editor.lines.count-1 do
        if i+1=caret.y then collectIdentifiers(editor.lines[i],wordsInEditor,caret.x)
                       else collectIdentifiers(editor.lines[i],wordsInEditor,-1);
      wordsInEditor.addAll(completionList.elementArray);
      wordsInEditor.unique;
    end;
  end;

PROCEDURE TMnhForm.SynCompletionExecute(Sender: TObject);
  VAR i:longint;
      s:string;
  begin
    ensureWordsInEditorForCompletion;
    SynCompletion.ItemList.clear;
    s:=SynCompletion.CurrentString;
    for i:=0 to wordsInEditor.size-1 do
      if (s='') or (pos(s,wordsInEditor[i])=1) then SynCompletion.ItemList.add(wordsInEditor[i]);
  end;

PROCEDURE TMnhForm.SynCompletionSearchPosition(VAR APosition: integer);
  VAR i:longint;
      s:string;
  begin
    ensureWordsInEditorForCompletion;
    SynCompletion.ItemList.clear;
    s:=SynCompletion.CurrentString;
    i:=LastDelimiter('.',s);
    if i>1 then begin
      s:=copy(s,i,length(s));
      SynCompletion.CurrentString:=s;
    end;
    for i:=0 to wordsInEditor.size-1 do
      if pos(s,wordsInEditor[i])=1 then SynCompletion.ItemList.add(wordsInEditor[i]);
    if SynCompletion.ItemList.count>0 then APosition:=0 else APosition:=-1;
  end;

PROCEDURE TMnhForm.tbMicroStepClick(Sender: TObject);
  begin
    stepper.doMicrostep;
    updateDebugParts;
    breakPointHandlingPending:=true;
  end;

PROCEDURE TMnhForm.UpdateTimeTimerTimer(Sender: TObject);
  CONST MIN_INTERVALL=1;
        MAX_INTERVALL=250;
  VAR aid:ansistring;
      isEvaluationRunning:boolean;
      flushPerformed:boolean;
      autosizingDone:boolean;
      i,modalRes:longint;
      currentFileAge:double;
  begin
    isEvaluationRunning:=runEvaluator.evaluationRunning;
    //fast ones:================================================================
    //Show ask form?
    if askForm.displayPending then askForm.Show;
    tableForm.conditionalDoShow;
    //Form caption:-------------------------------------------------------------
    if (PageControl.ActivePageIndex>=0) and (PageControl.ActivePageIndex<length(editorMeta))
    then begin
      aid:=editorMeta[PageControl.ActivePageIndex].updateSheetCaption;
      editorMeta[PageControl.ActivePageIndex].repaintWithStateCounter(docEvaluator.getStateCounter,docEvaluator.getErrorHint);
    end else aid:='MNH5';
    if aid<>Caption then Caption:=aid;
    //-------------------------------------------------------------:Form caption
    //progress time:------------------------------------------------------------
    if PageControl.ActivePageIndex>=0
    then aid:=C_tabChar+intToStr(editorMeta[PageControl.ActivePageIndex].editor.CaretY)+','+intToStr(editorMeta[PageControl.ActivePageIndex].editor.CaretX)
    else aid:='';
    if isEvaluationRunning then begin
      if currentlyDebugging then begin
        if stepper.haltet then begin
          StatusBar.SimpleText:='Debugging [HALTED]'+aid;
          if breakPointHandlingPending then handleBreak;
        end else StatusBar.SimpleText:='Debugging...'+aid;
      end else StatusBar.SimpleText:='Evaluating: '+myTimeToStr(now-evaluation.start)+aid;
    end else StatusBar.SimpleText:=runEvaluator.getEndOfEvaluationText+aid;
    //------------------------------------------------------------:progress time
    //Halt/Run enabled states:--------------------------------------------------
    if isEvaluationRunning<>miHaltEvalutaion.Enabled then miHaltEvalutaion.Enabled:=isEvaluationRunning;
    if not(isEvaluationRunning)<>miEvaluateNow.Enabled then begin
      miEvaluateNow.Enabled:=not(isEvaluationRunning);
      miCallMain.Enabled:=not(isEvaluationRunning);
    end;
    //--------------------------------------------------:Halt/Run enabled states
    //================================================================:fast ones
    //slow ones:================================================================
    //if not(isEvaluationRunning) then for i:=0 to 9 do inputRec[i].editor.readonly:=false;
    flushPerformed:=guiOutAdapter.flushToGui(OutputEdit);
    autosizingDone:=autosizeBlocks(isEvaluationRunning);

    if guiAdapters.hasMessageOfType[mt_plotCreatedWithDeferredDisplay] and
       not(runEvaluator.evaluationRunning) then plotForm.doPlot();

    if isEvaluationRunning then evaluation.deferredUntil:=now+0.1*ONE_SECOND else
    if evaluation.required and not(runEvaluator.evaluationRunning) and (now>evaluation.deferredUntil) then begin
      doStartEvaluation(false,false);
      lastStart.mainCall:=false;
      with editorMeta[PageControl.ActivePageIndex] do runEvaluator.evaluate(pseudoName,editor.lines,true);
      UpdateTimeTimer.interval:=MIN_INTERVALL;
    end;

    if not(flushPerformed) and not(autosizingDone) then begin
      UpdateTimeTimer.interval:=UpdateTimeTimer.interval+10;
      if UpdateTimeTimer.interval>MAX_INTERVALL then UpdateTimeTimer.interval:=MAX_INTERVALL;
    end else UpdateTimeTimer.interval:=MIN_INTERVALL;
    //================================================================:slow ones
    if settings.value^.savingRequested then begin
      for i:=0 to length(editorMeta)-1 do editorMeta[i].writeToEditorState(settings.value);
      saveSettings;
    end;

    if (now>doNotCheckFileBefore) then begin
      doNotCheckFileBefore:=now+1;
      for i:=0 to length(editorMeta)-1 do with editorMeta[i] do if sheet.TabVisible and (filePath<>'') and not(changed) then begin
        fileAge(filePath,currentFileAge);
        if currentFileAge<>fileAccessAge then begin
          modalRes:=closeDialogForm.showOnOutOfSync(filePath);
          if modalRes=mrOk then reloadFile(filePath);
          if modalRes=mrClose then begin if not(_doSave_(i)) then changed:=true; end else
          changed:=true;
        end;
      end;
      doNotCheckFileBefore:=now+ONE_SECOND;
    end;

    if reEvaluationWithGUIrequired then begin
      Hide;
      if not(isEvaluationRunning) and (not(plotForm.showing) and not(tableForm.showing) or closeGuiFlag) then begin
        guiAdapters.setExitCode;
        close;
      end;
    end;
  end;

PROCEDURE TMnhForm.miOpenDemoClick(Sender: TObject);
  begin
    ensureDemos;
    OpenDialog.fileName:=configDir+'demos';
    miOpenClick(Sender);
  end;

PROCEDURE TMnhForm.miNewCentralPackageClick(Sender: TObject);
  begin
    if newCentralPackageForm.ShowModal=mrOk then
      PageControl.ActivePageIndex:=addEditorMetaForNewFile(newCentralPackageForm.fileNameEdit.Caption);
  end;

PROCEDURE TMnhForm.miFindClick(Sender: TObject);
  VAR wordUnderCursor:string;
  begin
    if OutputEdit.Focused then begin
      OutputEdit.GetWordAtRowCol(OutputEdit.CaretXY);
      wordUnderCursor:=OutputEdit.TextBetweenPoints[OutputEdit.BlockBegin,OutputEdit.BlockEnd];
      if wordUnderCursor='' then wordUnderCursor:=OutputEdit.GetWordAtRowCol(OutputEdit.CaretXY);
      outputFocusedOnFind:=true;
    end else begin
      with editorMeta[PageControl.ActivePageIndex] do begin
        wordUnderCursor:=editor.TextBetweenPoints[editor.BlockBegin,editor.BlockEnd];
        if wordUnderCursor='' then wordUnderCursor:=editor.GetWordAtRowCol(editor.CaretXY);
      end;
      outputFocusedOnFind:=false;
    end;
    if wordUnderCursor<>'' then FindDialog.FindText:=wordUnderCursor;
    FindDialog.execute;
    ReplaceDialog.FindText:=FindDialog.FindText;
  end;

PROCEDURE TMnhForm.miReplaceClick(Sender: TObject);
  VAR wordUnderCursor:string;
  begin
    if OutputEdit.Focused then begin
      OutputEdit.GetWordAtRowCol(OutputEdit.CaretXY);
      wordUnderCursor:=OutputEdit.TextBetweenPoints[OutputEdit.BlockBegin,OutputEdit.BlockEnd];
      if wordUnderCursor='' then wordUnderCursor:=OutputEdit.GetWordAtRowCol(OutputEdit.CaretXY);
      outputFocusedOnFind:=true;
    end else begin
      with editorMeta[PageControl.ActivePageIndex] do begin
        wordUnderCursor:=editor.TextBetweenPoints[editor.BlockBegin,editor.BlockEnd];
        if wordUnderCursor='' then wordUnderCursor:=editor.GetWordAtRowCol(editor.CaretXY);
      end;
      outputFocusedOnFind:=false;
    end;
    if wordUnderCursor<>'' then ReplaceDialog.FindText:=wordUnderCursor;
    ReplaceDialog.execute;
    FindDialog.FindText:=ReplaceDialog.FindText;
  end;

FUNCTION FindOptionsToSearchOptions (CONST FindOptions: TFindOptions): TSynSearchOptions;
  begin
    result:=[];
    if frMatchCase       in FindOptions then include(result,ssoMatchCase);
    if frWholeWord       in FindOptions then include(result,ssoWholeWord);
    if frReplace         in FindOptions then include(result,ssoReplace);
    if frReplaceAll      in FindOptions then include(result,ssoReplaceAll);
    if frHideEntireScope in FindOptions then include(result,ssoEntireScope);
    if frPromptOnReplace in FindOptions then include(result,ssoPrompt);
    if frFindNext        in FindOptions then include(result,ssoFindContinue);
    if not(frDown in FindOptions) then include(result,ssoBackwards);
  end;

PROCEDURE TMnhForm.FindDialogFind(Sender: TObject);
  begin
    editForSearch(false).SearchReplace(FindDialog.FindText,FindDialog.FindText,FindOptionsToSearchOptions(FindDialog.options));
  end;

PROCEDURE TMnhForm.ReplaceDialogReplace(Sender: TObject);
  begin
    editForSearch(true).SearchReplace(ReplaceDialog.FindText,ReplaceDialog.ReplaceText,FindOptionsToSearchOptions(ReplaceDialog.options));
  end;

PROCEDURE TMnhForm.ReplaceDialogFind(Sender: TObject);
  begin
    editForSearch(true).SearchReplace(ReplaceDialog.FindText,ReplaceDialog.FindText,FindOptionsToSearchOptions(ReplaceDialog.options));
  end;

PROCEDURE TMnhForm.miFindNextClick(Sender: TObject);
  begin
    outputFocusedOnFind:=OutputEdit.Focused;
    editForSearch(false).SearchReplace(FindDialog.FindText,FindDialog.FindText,FindOptionsToSearchOptions(FindDialog.options)-[ssoBackwards]);
  end;

PROCEDURE TMnhForm.miFindPreviousClick(Sender: TObject);
  begin
    outputFocusedOnFind:=OutputEdit.Focused;
    editForSearch(false).SearchReplace(FindDialog.FindText,FindDialog.FindText,FindOptionsToSearchOptions(FindDialog.options)+[ssoBackwards]);
  end;

PROCEDURE TMnhForm.tbRunClick(Sender: TObject);
  begin
    if not(runEvaluator.evaluationRunning) then begin
      doStartEvaluation(true,false);
      if lastStart.mainCall then begin
        with editorMeta[PageControl.ActivePageIndex] do runEvaluator.callMain(pseudoName,editor.lines,lastStart.parameters);
      end else begin
        with editorMeta[PageControl.ActivePageIndex] do runEvaluator.evaluate(pseudoName,editor.lines,true);
      end;
    end else stepper.doStart(true);
    updateDebugParts;
    breakPointHandlingPending:=true;
  end;

PROCEDURE TMnhForm.tbStepInClick(Sender: TObject);
  begin
    stepper.doStepInto;
    updateDebugParts;
    breakPointHandlingPending:=true;
  end;

PROCEDURE TMnhForm.tbStepClick(Sender: TObject);
  begin
    stepper.doStep;
    updateDebugParts;
    breakPointHandlingPending:=true;
  end;

PROCEDURE TMnhForm.tbStepOutClick(Sender: TObject);
  begin
    stepper.doStepOut;
    updateDebugParts;
    breakPointHandlingPending:=true;
  end;

PROCEDURE TMnhForm.tbStopClick(Sender: TObject);
  begin
    runEvaluator.haltEvaluation;
    stepper.doStop;
  end;

PROCEDURE TMnhForm.InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);
  begin
    Special:=currentlyDebugging and runEvaluator.evaluationRunning and (Sender=debugLine.editor) and (line=debugLine.line);
  end;

FUNCTION TMnhForm.editForSearch(CONST replacing: boolean): TSynEdit;
  begin
    if outputFocusedOnFind and not(replacing) then exit(OutputEdit);
    if (PageControl.ActivePageIndex>=0) and (PageControl.ActivePageIndex<length(editorMeta))
    then result:=editorMeta[PageControl.ActivePageIndex].editor
    else exit(OutputEdit); //not nice, but a valid fallback
  end;

PROCEDURE TMnhForm.processSettings;
  VAR formPosition:T_formPosition;
      i:longint;
  begin
    if not(settingsReady) then begin
      formPosition:=settings.value^.mainForm;
      top   :=formPosition.top;
      Left  :=formPosition.Left;
      width :=formPosition.width;
      height:=formPosition.height;
      if formPosition.isFullscreen then WindowState:=wsMaximized;

      with settings.value^.outputBehaviour do begin
        miDeclarationEcho.Checked:=doEchoDeclaration;
        miExpressionEcho.Checked:=doEchoInput;
        miExpressionResult.Checked:=doShowExpressionOut;
        miWrapEcho.Checked:=settings.value^.wordWrapEcho;
        miTimingInfo.Checked:=doShowTimingInfo;
        miMinErrorlevel1.Checked:=minErrorLevel<=1;
        miMinErrorlevel2.Checked:=minErrorLevel=2;
        miMinErrorlevel3.Checked:=minErrorLevel=3;
        miMinErrorlevel4.Checked:=minErrorLevel=4;
        miMinErrorlevel5.Checked:=minErrorLevel>=5;
        guiAdapters.outputBehaviour:=settings.value^.outputBehaviour;
      end;

      plotForm.miAutoReset.Checked:=settings.value^.doResetPlotOnEvaluation;
      miEvalModeDirect.Checked:=not(settings.value^.instantEvaluation);
      miEvalModeDirectOnKeypress.Checked:=settings.value^.instantEvaluation;
      processFileHistory;
      SettingsForm.ensureFont(OutputEdit.Font);
      settingsReady:=true;

      setLength(editorMeta,length(settings.value^.editorState));
      stepper.clearBreakpoints;
      for i:=0 to length(editorMeta)-1 do begin
        editorMeta[i].create(i,settings.value^.editorState[i]);
        editorMeta[i].setStepperBreakpoints;
      end;

      if not(reEvaluationWithGUIrequired) then with editorMeta[PageControl.ActivePageIndex] do docEvaluator.evaluate(pseudoName,editor.lines,false);
    end;

    OutputEdit.Font.name:=settings.value^.editorFontname;
    OutputEdit.Font.size:=SettingsForm.fontSize;
    if SettingsForm.AntialiasCheckbox.Checked
    then OutputEdit.Font.quality:=fqCleartypeNatural
    else OutputEdit.Font.quality:=fqNonAntialiased;
    for i:=0 to length(editorMeta)-1 do editorMeta[i].editor.Font:=OutputEdit.Font;

    currentExpressionMemo.Font:=OutputEdit.Font;
    helpPopupMemo.Font:=OutputEdit.Font;
    helpPopupMemo.Font.size:=helpPopupMemo.Font.size-2;

    if (tempAdapter<>nil) and not(settings.value^.getLogName<>'') then guiAdapters.removeOutAdapter(tempAdapter);
  end;

PROCEDURE TMnhForm.processFileHistory;
  FUNCTION historyMenuItem(index:byte):TMenuItem;
    begin
      case index of
        0: result:=miFileHistory0;
        1: result:=miFileHistory1;
        2: result:=miFileHistory2;
        3: result:=miFileHistory3;
        4: result:=miFileHistory4;
        5: result:=miFileHistory5;
        6: result:=miFileHistory6;
        7: result:=miFileHistory7;
        8: result:=miFileHistory8;
        9: result:=miFileHistory9;
      else result:=nil;
      end;
    end;
  VAR i:longint;
  begin
    for i:=0 to 9 do if settings.value^.historyItem(i)='' then begin
      historyMenuItem(i).Enabled:=false;
      historyMenuItem(i).visible:=false;
    end else begin
      historyMenuItem(i).Enabled:=true;
      historyMenuItem(i).visible:=true;
      historyMenuItem(i).Caption:=intToStr(i)+': '+settings.value^.historyItem(i);
    end;
  end;

PROCEDURE formCycle(CONST ownId:longint; CONST next:boolean);
  VAR newId:byte;
      form:TForm;
  begin
    if next then newId:=(ownId and 255+1) mod 3
            else newId:=(ownId and 255+2) mod 3;
    case newId of
      0: form:=MnhForm;
      1: form:=plotForm;
      2: form:=tableForm;
    end;
    form.Show;
    form.BringToFront;
    form.SetFocus;
  end;

PROCEDURE lateInitialization;
  VAR i:longint;
  begin
    guiAdapters.addOutAdapter(@guiOutAdapter,false);
    for i:=0 to consoleAdapters.adapterCount-1 do
      if consoleAdapters.getAdapter(i)^.adapterType in [at_textFile,at_htmlFile] then
        guiAdapters.addOutAdapter(consoleAdapters.getAdapter(i),false);

    mnh_plotForm.formCycleCallback:=@formCycle;
    mnh_tables.formCycleCallback:=@formCycle;
    registerRule(SYSTEM_BUILTIN_NAMESPACE,'ask', @ask_impl,'');
    initLists;
    mnh_evalThread.initUnit(@guiAdapters);
  end;

FUNCTION closeGUI_impl(CONST params: P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=newVoidLiteral;
    closeGuiFlag:=true;
  end;

INITIALIZATION
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'closeGui',@closeGUI_impl,'closeGui;#Closes the GUI even if plot window or table is showing#Returns void');
  guiOutAdapter.create;
  guiAdapters.create;
  mnh_plotForm.guiAdapters:=@guiAdapters;
  tempAdapter:=nil;

FINALIZATION
  guiAdapters.destroy;
  guiOutAdapter.destroy;
end.


