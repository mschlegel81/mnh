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
  SynEditMiscClasses, SynEditMarkupSpecialLine,mnh_tokens;

CONST DEBUG_LINE_COUNT=200;
      RUN_SILENT_ICON_INDEX:array[false..true] of longint=(5,2);
      RUN_VERBOSE_ICON_INDEX:array[false..true] of longint=(6,3);
TYPE
  {$define includeInterface}
  {$include editorMeta.inc}
  {$include guiOutAdapter.inc}
  {$undef includeInterface}

  { TMnhForm }

  TMnhForm = class(TForm)
    autosizeToggleBox: TToggleBox;
    debugItemsImageList: TImageList;
    InputEdit0: TSynEdit;
    InputEdit1: TSynEdit;
    InputEdit2: TSynEdit;
    InputEdit3: TSynEdit;
    InputEdit4: TSynEdit;
    InputEdit5: TSynEdit;
    InputEdit6: TSynEdit;
    InputEdit7: TSynEdit;
    InputEdit8: TSynEdit;
    InputEdit9: TSynEdit;
    miClose: TMenuItem;
    miMinErrorlevel5: TMenuItem;
    miOpenDocumentation: TMenuItem;
    MenuItem4: TMenuItem;
    miMinErrorlevel1: TMenuItem;
    miMinErrorlevel2: TMenuItem;
    miMinErrorlevel3: TMenuItem;
    miMinErrorlevel4: TMenuItem;
    miTimingInfo: TMenuItem;
    miStartAnotherInstance: TMenuItem;
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
    EditorTabSheet: TTabSheet;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    UpdateTimeTimer: TTimer;
    helpPopupMemo: TSynMemo;
    miOpenDemo: TMenuItem;
    miNewCentralPackage: TMenuItem;
    MenuItem3: TMenuItem;
    miOpenTableEditor: TMenuItem;
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
    GroupBox1: TGroupBox;
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
    PROCEDURE miSaveAsClick(Sender: TObject);
    PROCEDURE miSaveClick(Sender: TObject);
    PROCEDURE miStartAnotherInstanceClick(Sender: TObject);
    PROCEDURE miTimingInfoClick(Sender: TObject);
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
    PROCEDURE UpdateTimeTimerTimer(Sender: TObject);
    PROCEDURE miOpenDemoClick(Sender: TObject);
    PROCEDURE miNewCentralPackageClick(Sender: TObject);
    PROCEDURE miOpenTableEditorClick(Sender: TObject);
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

    PROCEDURE setupInputRecForNewFile    (CONST index:longint; CONST newFileName:ansistring='');
    PROCEDURE setupInputRecForLoadingFile(CONST index:longint; CONST fileName:ansistring);
    FUNCTION updateSheetCaption(CONST index:longint):ansistring;
    FUNCTION getInputEditIndexForFilename(CONST fileName:ansistring):longint;
    PROCEDURE updateDebugParts;
    PROCEDURE handleBreak;
  public
    editorMeta:array[0..9] of T_editorMeta;
  end;

VAR MnhForm: TMnhForm;
    locationToOpenOnFormStartup:T_tokenLocation;

PROCEDURE lateInitialization;
PROCEDURE formCycle(CONST ownId:longint);
IMPLEMENTATION
VAR guiOutAdapter: T_guiOutAdapter;
    guiAdapters: T_adapters;

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
      idealOutputHeight   :=scrollbarHeight+ OutputEdit.Font.GetTextHeight(SAMPLE_TEXT)*(OutputEdit.lines.count+1);
      //Are both editors large enough? Then return right now.
      if (editor.height>=idealInputHeight) and (OutputEdit.height>=idealOutputHeight) then exit;

      availableTotalHeight:=editor.height+OutputEdit.height;
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
    if not(isIdentifier(wordText,true)) then begin
      if miHelp.Checked then positionHelpNotifier;
      exit;
    end;
    if updateMarker then begin
      outputHighlighter.setMarkedWord(wordText);
      for i:=0 to 9 do editorMeta[i].setMarkedWord(wordText);
    end;
    if miHelp.Checked then begin
      ad_explainIdentifier(wordText,underCursor);
      helpPopupMemo.text:=underCursor.tokenExplanation;
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
  VAR index:longint;
  begin
    with settings.value^ do begin
      if fileExists(historyItem(historyIdx)) then begin
        index:=getInputEditIndexForFilename(historyItem(historyIdx));
        if index>=0 then PageControl.ActivePageIndex:=index;
      end else if polishHistory then processFileHistory;
    end;
  end;

PROCEDURE TMnhForm.doStartEvaluation(CONST clearOutput, reEvaluating: boolean);
  VAR i:longint;
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
    end;
    underCursor.tokenText:='';
    if miDebug.Checked then begin
      currentlyDebugging:=true;
      for i:=0 to 9 do editorMeta[i].startDebugging;
      stepper.doStart(false);
      updateDebugParts;
      breakPointHandlingPending:=true;
    end else currentlyDebugging:=false;
    UpdateTimeTimer.interval:=20;
  end;

PROCEDURE TMnhForm.inputEditReposition(CONST caret: TPoint; CONST doJump, updateMarker: boolean);
  VAR wordUnderCursor:string;
      newCaret:TPoint;
      pageIdx:longint;
  begin
    with editorMeta[PageControl.ActivePageIndex] do begin
      wordUnderCursor:=editor.GetWordAtRowCol(caret);
      setUnderCursor(wordUnderCursor,updateMarker);
      if not(doJump) then exit;
      if not(miHelp.Checked) then ad_explainIdentifier(wordUnderCursor,underCursor);
      if (underCursor.tokenText<>wordUnderCursor) or
         (underCursor.location.column<=0) then exit;
      if (underCursor.location.fileName='') or (underCursor.location.fileName='?') then exit;
      pageIdx:=getInputEditIndexForFilename(underCursor.location.fileName);
      if pageIdx>=0 then begin
        PageControl.ActivePageIndex:=pageIdx;
        newCaret.x:=underCursor.location.column;
        newCaret.y:=underCursor.location.line;
        editorMeta[pageIdx].editor.CaretXY:=newCaret;
      end;
    end;
  end;

PROCEDURE TMnhForm.outputEditReposition(CONST caret: TPoint; CONST doJump: boolean);
  VAR loc:T_tokenLocation;
      newCaret:TPoint;
      pageIdx:longint;
  begin
    forceInputEditFocusOnOutputEditMouseUp:=false;
    setUnderCursor(OutputEdit.GetWordAtRowCol(caret),true);
    loc:=guessLocationFromString(OutputEdit.lines[caret.y-1],false);
    if not(doJump) then exit;
    if (loc.fileName='') or (loc.fileName='?') then exit;
    pageIdx:=getInputEditIndexForFilename(loc.fileName);
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
  CONST msg='compiled on: '+{$I %DATE%}+' at: '+{$I %TIME%}+' with FPC'+{$I %FPCVERSION%}+' for '+{$I %FPCTARGET%};
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
    editorMeta[0].editor:=InputEdit0;
    editorMeta[1].editor:=InputEdit1;
    editorMeta[2].editor:=InputEdit2;
    editorMeta[3].editor:=InputEdit3;
    editorMeta[4].editor:=InputEdit4;
    editorMeta[5].editor:=InputEdit5;
    editorMeta[6].editor:=InputEdit6;
    editorMeta[7].editor:=InputEdit7;
    editorMeta[8].editor:=InputEdit8;
    editorMeta[9].editor:=InputEdit9;
    editorMeta[0].sheet:=EditorTabSheet;
    editorMeta[1].sheet:=TabSheet1;
    editorMeta[2].sheet:=TabSheet2;
    editorMeta[3].sheet:=TabSheet3;
    editorMeta[4].sheet:=TabSheet4;
    editorMeta[5].sheet:=TabSheet5;
    editorMeta[6].sheet:=TabSheet6;
    editorMeta[7].sheet:=TabSheet7;
    editorMeta[8].sheet:=TabSheet8;
    editorMeta[9].sheet:=TabSheet9;
    for i:=0 to 9 do with editorMeta[i] do begin
      index:=i;
      highlighter:=TSynMnhSyn.create(self,msf_input);
      editor.highlighter:=highlighter;
    end;

    doNotMarkWordBefore:=now;
    doNotCheckFileBefore:=now+ONE_SECOND;
    outputHighlighter:=TSynMnhSyn.create(nil,msf_output);
    OutputEdit.highlighter:=outputHighlighter;
    helpHighlighter:=TSynMnhSyn.create(nil,msf_guessing);
    helpPopupMemo.highlighter:=helpHighlighter;
    OutputEdit.ClearAll;
    debugHighlighter:=TSynMnhSyn.create(nil,msf_input);
    currentExpressionMemo.highlighter:=debugHighlighter;
    endOfEvaluationText.value:=msg;
    for i:=0 to length(LOGO)-1 do OutputEdit.lines.append(LOGO[i]);
    {$ifdef debugMode}
    guiAdapters.addConsoleOutAdapter;
    {$else}
    MenuItem3.Enabled:=false;
    MenuItem3.visible:=false;
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
      runAlone('deleteDir('+escapeString(GetAppConfigDir(true))+')');
      DeleteFile('mnh_light.exe');
      deleteMyselfOnExit;
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

    if ad_evaluationRunning then ad_haltEvaluation;
    stepper.doStop;
    for i:=0 to 9 do settings.value^.editorState[i]:=editorMeta[i].stateForSaving;
  end;

PROCEDURE TMnhForm.FormDestroy(Sender: TObject);
  begin
    if not(reEvaluationWithGUIrequired) then saveSettings;
    guiAdapters.removeOutAdapter(@guiOutAdapter);
    outputHighlighter.destroy;
    ad_killEvaluationLoopSoftly;
    wordsInEditor.destroy;
  end;

PROCEDURE TMnhForm.FormDropFiles(Sender: TObject; CONST FileNames: array of string);
  VAR i:longint;
  begin
    for i:=0 to length(FileNames)-1 do if uppercase(extractFileExt(FileNames[i]))=SCRIPT_EXTENSION then begin
      if getInputEditIndexForFilename(FileNames[i])<0 then exit;
    end;
  end;

PROCEDURE TMnhForm.FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=9) and (ssCtrl in Shift) then formCycle(0)
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
  VAR newCaret:TPoint;
      i:longint;
  begin
    if not(settingsReady) then begin
      processSettings;
      for i:=0 to 9 do begin
        editorMeta[i].defineByState(settings.value^.editorState[i]);
        updateSheetCaption(i);
      end;
      i:=settings.value^.activePage;
      while (i>=0) and not(editorMeta[i].sheet.TabVisible) do dec(i);
      if i<0 then begin
        i:=9;
        while (i>=0) and not(editorMeta[i].sheet.TabVisible) do dec(i);
      end;
      if i>=0 then begin
        PageControl.ActivePageIndex:=i;
        editorMeta[i].editor.SetFocus;
      end else setupInputRecForNewFile(0);
      SynCompletion.editor:=editorMeta[PageControl.ActivePageIndex].editor;

      if (locationToOpenOnFormStartup.fileName<>'') and
         (locationToOpenOnFormStartup.fileName<>C_nilTokenLocation.fileName) and
         fileExists(locationToOpenOnFormStartup.fileName) then begin

        i:=getInputEditIndexForFilename(locationToOpenOnFormStartup.fileName);
        if i>=0 then begin
          PageControl.ActivePageIndex:=i;
          newCaret.x:=locationToOpenOnFormStartup.column;
          if newCaret.x<=0 then newCaret.x:=1;
          newCaret.y:=locationToOpenOnFormStartup.line;
          if newCaret.y<=0 then newCaret.y:=1;
          editorMeta[i].editor.CaretXY:=newCaret;
        end;
      end;
    end;
    KeyPreview:=true;
    UpdateTimeTimer.Enabled:=true;
    if reEvaluationWithGUIrequired then begin
      showConsole;
      {$ifndef debugMode}guiAdapters.addConsoleOutAdapter;{$endif}
      doStartEvaluation(true,true);
      ad_reEvaluateWithGUI;
      plotForm.Caption:=plotForm.Caption+' - close to quit';
      sleep(UpdateTimeTimer.interval);
    end;
  end;

PROCEDURE TMnhForm.InputEditChange(Sender: TObject);
  begin
    if (miEvalModeDirectOnKeypress.Checked) and not(SynCompletion.IsActive) then begin
      if now>evaluation.deferredUntil then begin
        doStartEvaluation(false,false);
        lastStart.mainCall:=false;
        with editorMeta[PageControl.ActivePageIndex] do ad_evaluate(pseudoName,editor.lines,true);
      end else evaluation.required:=true;
    end;
    editorMeta[PageControl.ActivePageIndex].changed:=true;
    updateSheetCaption(PageControl.ActivePageIndex);
  end;

PROCEDURE TMnhForm.InputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=13) and ((ssCtrl in Shift) or (ssAlt in Shift))
    then inputEditReposition(editorMeta[PageControl.ActivePageIndex].editor.CaretXY,ssCtrl in Shift,true)
    else inputEditReposition(editorMeta[PageControl.ActivePageIndex].editor.CaretXY,false,false);
  end;

PROCEDURE TMnhForm.InputEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    inputEditReposition(editorMeta[PageControl.ActivePageIndex].editor.PixelsToRowColumn(point),ssCtrl in Shift,true);
  end;

PROCEDURE TMnhForm.InputEditProcessUserCommand(Sender: TObject; VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
  VAR i:longint;
  begin
    if command=ecUserDefinedFirst then editorMeta[PageControl.ActivePageIndex].toggleComment;
    if command=ecUserDefinedFirst+1 then begin
      for i:=1 to 9 do if editorMeta[(i+PageControl.ActivePageIndex) mod 10].sheet.TabVisible then begin
        PageControl.ActivePageIndex:=(i+PageControl.ActivePageIndex) mod 10;
        editorMeta[PageControl.ActivePageIndex].editor.SetFocus;
        exit;
      end;
    end;
    if command=ecUserDefinedFirst+2 then begin
      for i:=9 downto 1 do if editorMeta[(i+PageControl.ActivePageIndex) mod 10].sheet.TabVisible then begin
        PageControl.ActivePageIndex:=(i+PageControl.ActivePageIndex) mod 10;
        editorMeta[PageControl.ActivePageIndex].editor.SetFocus;
        exit;
      end;
    end;
    if (command=ecUserDefinedFirst+3) then begin
      editorMeta[PageControl.ActivePageIndex].toggleBreakpoint;
      stepper.clearBreakpoints;
      for i:=0 to 9 do editorMeta[i].setStepperBreakpoints;
      if ad_evaluationRunning and not(miDebug.Checked) then ad_haltEvaluation;
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
      lastStart.parameters:=askForm.getLastAnswerReleasing;
      with editorMeta[PageControl.ActivePageIndex] do ad_callMain(pseudoName,editor.lines,lastStart.parameters);
    end else askForm.getLastAnswerReleasing;
  end;

PROCEDURE TMnhForm.miClearClick(Sender: TObject);
  VAR mr:integer;
  begin
    for mr:=0 to 9 do if not(editorMeta[mr].sheet.TabVisible) then begin
      setupInputRecForNewFile(mr);
      PageControl.ActivePageIndex:=mr;
      exit;
    end;
    with editorMeta[PageControl.ActivePageIndex] do begin
      if changed then begin
        mr:=closeDialogForm.showOnLoad;
        if mr=mrOk then if not(_doSave_(PageControl.ActivePageIndex)) then exit;
        if mr=mrCancel then exit;
      end;
      if filePath<>'' then settings.value^.fileClosed(filePath);
      processFileHistory;
    end;
    setupInputRecForNewFile(mr);
  end;

PROCEDURE TMnhForm.miCloseClick(Sender: TObject);
  VAR i,mr:longint;
  begin
    if PageControl.ActivePageIndex<0 then exit;
    with editorMeta[PageControl.ActivePageIndex] do begin
      if changed then begin
        mr:=closeDialogForm.showOnLoad;
        if mr=mrOk then if not(_doSave_(PageControl.ActivePageIndex)) then exit;
        if mr=mrCancel then exit;
      end;
      sheet.TabVisible:=false;
      if filePath<>'' then begin
        settings.value^.fileClosed(filePath);
        processFileHistory;
      end;
    end;
    if PageControl.ActivePageIndex>0 then
    for i:=9+PageControl.ActivePageIndex downto
             PageControl.ActivePageIndex do if editorMeta[i mod 10].sheet.TabVisible then begin
      PageControl.ActivePageIndex:=i mod 10;
      exit;
    end;
    //If we reach that line, closing just closed the last tab
    setupInputRecForNewFile(0);
  end;

PROCEDURE TMnhForm.miDebugCancelClick(Sender: TObject);
  begin
    ad_haltEvaluation;
    stepper.doStop;
  end;

PROCEDURE TMnhForm.miDebugClick(Sender: TObject);
  begin
    if ad_evaluationRunning then ad_haltEvaluation;
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
    if ad_evaluationRunning and (miDebug.Checked) then ad_haltEvaluation;
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
      with editorMeta[PageControl.ActivePageIndex] do ad_evaluate(pseudoName,editor.lines,false);
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
    ad_haltEvaluation;
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
    makeAndShowDoc;
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
      updateSheetCaption(index);
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
      updateSheetCaption(index);
    end;
  end;

PROCEDURE TMnhForm.setupInputRecForNewFile(CONST index: longint; CONST newFileName: ansistring);
  begin
    if (index>=0) and (index<length(editorMeta)) then with editorMeta[index] do begin
      filePath:=newFileName;
      fileAccessAge:=0;
      changed:=false;
      editor.lines.clear;
      if newFileName<>'' then _doSave_(index);
      sheet.TabVisible:=true;
      updateSheetCaption(index);
    end;
  end;

PROCEDURE TMnhForm.setupInputRecForLoadingFile(CONST index: longint; CONST fileName: ansistring);
  begin
    with editorMeta[index] do begin
      filePath:=expandFileName(fileName);
      fileAge(filePath,fileAccessAge);
      changed:=false;

      editor.lines.loadFromFile(fileName);
      sheet.TabVisible:=true;
      updateSheetCaption(index);
    end;
  end;

FUNCTION TMnhForm.updateSheetCaption(CONST index: longint): ansistring;
  VAR i:longint;
  begin
    result:='';
    if (index<0) or (index>9)
    then for i:=0 to 9 do updateSheetCaption(i)
    else with editorMeta[index] do begin
      if filePath<>'' then begin
        if changed then begin
                          sheet.Caption:=extractFileName(filePath)+' *';
                          result:=filePath+' *';
                        end
                   else begin
                          sheet.Caption:=extractFileName(filePath);
                          result:=filePath;
                        end;
      end else begin
        result:='<new '+intToStr(index)+'>';
        sheet.Caption:=result;
      end;
      result:='MNH5 '+result;
    end;
  end;

FUNCTION TMnhForm.getInputEditIndexForFilename(CONST fileName: ansistring): longint;
  VAR i:longint;
      uName:ansistring;
  begin
    uName:=expandFileName(fileName);
    for i:=0 to 9 do with editorMeta[i] do if sheet.TabVisible and ((filePath=uName) or (pseudoName=fileName)) then exit(i);
    if copy(fileName,1,4)='<new' then exit;
    for i:=0 to 9 do with editorMeta[i] do
    if not(sheet.TabVisible) then begin
      setupInputRecForLoadingFile(i,uName);
      exit(i);
    end;
    result:=-1;
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
      for i:=0 to 9 do editorMeta[i].editor.Gutter.MarksPart.visible:=true;
      DebugToolbar.visible:=true;
      DebugToolbar.Enabled:=true;
      DebugToolbar.top:=0;
      outputPageControl.ShowTabs:=true;
      handleButton(tbStop,ad_evaluationRunning,2);
      handleButton(tbRun,not(ad_evaluationRunning) or stepper.haltet,0);
      handleButton(tbStep,ad_evaluationRunning and stepper.haltet,4);
      handleButton(tbStepIn,ad_evaluationRunning and stepper.haltet,6);
      handleButton(tbStepOut,ad_evaluationRunning and stepper.haltet,8);
    end else begin
      for i:=0 to 9 do editorMeta[i].editor.Gutter.MarksPart.visible:=false;
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
      if (stepper.loc.fileName='') or (stepper.loc.fileName='?') then exit;
      pageIdx:=getInputEditIndexForFilename(stepper.loc.fileName);
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
      package:P_package;
      report:T_variableReport;
      i:longint;
      first:P_token;
  begin
    if not(stepper).haltet then exit;
    jumpToFile;

    context:=P_evaluationContext(stepper.context);
    report.create;
    if environment.mainPackage<>nil then environment.mainPackage^.reportVariables(report);
    if context<>nil then context^.reportVariables(report);

    variablesStringGrid.RowCount:=length(report.dat)+1;
    for i:=0 to length(report.dat)-1 do begin
      variablesStringGrid.Cells[0,i+1]:=report.dat[i].location;
      variablesStringGrid.Cells[1,i+1]:=report.dat[i].id;
      variablesStringGrid.Cells[2,i+1]:=report.dat[i].value^.typeString;
      variablesStringGrid.Cells[3,i+1]:=report.dat[i].value^.toString;
    end;

    first:=stepper.token;
    currentExpressionMemo.clear;
    if first<>nil then currentExpressionMemo.lines.append(tokensToString(first));

    updateDebugParts;
    breakPointHandlingPending:=false;
  end;

PROCEDURE TMnhForm.miMinErrorlevel1Click(Sender: TObject); begin _setErrorlevel_(1); end;
PROCEDURE TMnhForm.miMinErrorlevel2Click(Sender: TObject); begin _setErrorlevel_(2); end;
PROCEDURE TMnhForm.miMinErrorlevel3Click(Sender: TObject); begin _setErrorlevel_(3); end;
PROCEDURE TMnhForm.miMinErrorlevel4Click(Sender: TObject); begin _setErrorlevel_(4); end;
PROCEDURE TMnhForm.miMinErrorlevel5Click(Sender: TObject); begin _setErrorlevel_(5); end;

PROCEDURE TMnhForm.miOpenClick(Sender: TObject);
  VAR mr:integer;
  begin
    OpenDialog.title:='Open file';
    if OpenDialog.execute and fileExists(OpenDialog.fileName)
    then begin
      mr:=getInputEditIndexForFilename(OpenDialog.fileName);
      if mr>=0 then PageControl.ActivePageIndex:=mr else begin
        with editorMeta[PageControl.ActivePageIndex] do begin
          if changed then begin
            mr:=closeDialogForm.showOnLoad;
            if mr=mrOk then if not(_doSave_(PageControl.ActivePageIndex)) then exit;
            if mr=mrCancel then exit;
          end;
          if filePath<>'' then settings.value^.fileClosed(filePath);
          processFileHistory;
        end;
        setupInputRecForNewFile(PageControl.ActivePageIndex);
      end;
    end;
  end;

PROCEDURE TMnhForm.miSaveAsClick(Sender: TObject);
  begin
    _doSaveAs_(PageControl.ActivePageIndex);
  end;

PROCEDURE TMnhForm.miSaveClick(Sender: TObject);
  begin
    _doSave_(PageControl.ActivePageIndex);
  end;

PROCEDURE TMnhForm.miStartAnotherInstanceClick(Sender: TObject);
  begin
    runCommandAsyncOrPipeless(paramStr(0),C_EMPTY_STRING_ARRAY,true);
  end;

PROCEDURE TMnhForm.miTimingInfoClick(Sender: TObject);
  begin
    if settingsReady then begin
      miTimingInfo.Checked:=not(miTimingInfo.Checked);
      guiAdapters.doShowTimingInfo:=miTimingInfo.Checked;
      settings.value^.outputBehaviour:=guiAdapters.outputBehaviour;
    end;
  end;

PROCEDURE TMnhForm.mi_settingsClick(Sender: TObject);
  begin
    SettingsForm.ShowModal;
    processSettings;
  end;

PROCEDURE TMnhForm.OutputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if ((key=13) and (ssCtrl in Shift)) then outputEditReposition(OutputEdit.CaretXY,true);
    if forceInputEditFocusOnOutputEditMouseUp and (PageControl.ActivePageIndex>=0) then ActiveControl:=editorMeta[PageControl.ActivePageIndex].editor;
    forceInputEditFocusOnOutputEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.OutputEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    outputEditReposition(OutputEdit.PixelsToRowColumn(point),ssCtrl in Shift);
  end;

PROCEDURE TMnhForm.OutputEditMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if forceInputEditFocusOnOutputEditMouseUp then ActiveControl:=editorMeta[PageControl.ActivePageIndex].editor;
    forceInputEditFocusOnOutputEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.PageControlChange(Sender: TObject);
  begin
    if PageControl.ActivePageIndex>=0 then begin
      SynCompletion.editor:=editorMeta[PageControl.ActivePageIndex].editor;
      settings.value^.activePage:=PageControl.ActivePageIndex;
    end;
  end;

PROCEDURE TMnhForm.PopupNotifier1Close(Sender: TObject; VAR CloseAction: TCloseAction);
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
    if (pos(C_ID_QUALIFY_CHARACTER,value)>0) then begin
      if pos(sourceValue,value)<>1 then
        value:=copy(value,pos(C_ID_QUALIFY_CHARACTER,value)+1,length(value));
    end;
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
    for i:=0 to wordsInEditor.size-1 do
      if pos(s,wordsInEditor[i])=1 then SynCompletion.ItemList.add(wordsInEditor[i]);
    if SynCompletion.ItemList.count>0 then APosition:=0 else APosition:=-1;
  end;

PROCEDURE TMnhForm.UpdateTimeTimerTimer(Sender: TObject);
  CONST MIN_INTERVALL=1;
        MAX_INTERVALL=250;
  VAR aid:ansistring;
      isEvaluationRunning:boolean;
      flushPerformed:boolean;
      autosizingDone:boolean;
      i,modalRes:longint;
      state:T_editorState;
      currentFileAge:double;
  begin
    isEvaluationRunning:=ad_evaluationRunning;
    //fast ones:================================================================
    //Show ask form?
    if askForm.displayPending then askForm.ShowModal;
    //Form caption:-------------------------------------------------------------
    if PageControl.ActivePageIndex>=0
    then aid:=updateSheetCaption(PageControl.ActivePageIndex)
    else aid:='MNH5';
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
    end else StatusBar.SimpleText:=endOfEvaluationText.value+aid;
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
       not(ad_evaluationRunning) then plotForm.doPlot();

    if isEvaluationRunning then evaluation.deferredUntil:=now+0.1*ONE_SECOND else
    if evaluation.required and not(ad_evaluationRunning) and (now>evaluation.deferredUntil) then begin
      doStartEvaluation(false,false);
      lastStart.mainCall:=false;
      with editorMeta[PageControl.ActivePageIndex] do ad_evaluate(pseudoName,editor.lines,true);
      UpdateTimeTimer.interval:=MIN_INTERVALL;
    end;

    if not(flushPerformed) and not(autosizingDone) then begin
      UpdateTimeTimer.interval:=UpdateTimeTimer.interval+10;
      if UpdateTimeTimer.interval>MAX_INTERVALL then UpdateTimeTimer.interval:=MAX_INTERVALL;
    end else UpdateTimeTimer.interval:=MIN_INTERVALL;
    //================================================================:slow ones
    if settings.value^.savingRequested then begin
      for i:=0 to 9 do with editorMeta[i] do settings.value^.editorState[i]:=stateForSaving;
      saveSettings;
    end;

    if (now>doNotCheckFileBefore) then begin
      doNotCheckFileBefore:=now+1;
      for i:=0 to 9 do with editorMeta[i] do if sheet.TabVisible and (filePath<>'') and not(changed) then begin
        fileAge(filePath,currentFileAge);
        if currentFileAge>fileAccessAge then begin
          modalRes:=closeDialogForm.showOnOutOfSync(filePath);
          if modalRes=mrOk then setupInputRecForLoadingFile(i,filePath) else
          if modalRes=mrClose then begin if not(_doSave_(i)) then changed:=true; end else
          changed:=true;
        end;
      end;
      doNotCheckFileBefore:=now+ONE_SECOND;
    end;

    if reEvaluationWithGUIrequired then begin
      Hide;
      if not(isEvaluationRunning) and not(plotForm.showing) then close;
    end;
  end;

PROCEDURE TMnhForm.miOpenDemoClick(Sender: TObject);
  begin
    ensureDemos;
    OpenDialog.fileName:=GetAppConfigDir(true)+'demos';
    miOpenClick(Sender);
  end;

PROCEDURE TMnhForm.miNewCentralPackageClick(Sender: TObject);
  VAR mr:integer;
  begin
    if newCentralPackageForm.ShowModal=mrOk then begin
      for mr:=0 to 9 do if not(editorMeta[mr].sheet.TabVisible) then begin
        setupInputRecForNewFile(mr,newCentralPackageForm.fileNameEdit.Caption);
        PageControl.ActivePageIndex:=mr;
        exit;
      end;
      with editorMeta[PageControl.ActivePageIndex] do begin
        if changed then begin
          mr:=closeDialogForm.showOnLoad;
          if mr=mrOk then if not(_doSave_(PageControl.ActivePageIndex)) then exit;
          if mr=mrCancel then exit;
        end;
        if filePath<>'' then settings.value^.fileClosed(filePath);
        processFileHistory;
      end;
      setupInputRecForNewFile(mr,newCentralPackageForm.fileNameEdit.Caption);
    end;
  end;

PROCEDURE TMnhForm.miOpenTableEditorClick(Sender: TObject);
begin
  //tableForm.initForEditing;
end;

PROCEDURE TMnhForm.miFindClick(Sender: TObject);
  begin
    outputFocusedOnFind:=OutputEdit.Focused;
    FindDialog.execute;
  end;

PROCEDURE TMnhForm.miReplaceClick(Sender: TObject);
  begin
    outputFocusedOnFind:=OutputEdit.Focused;
    ReplaceDialog.execute;
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
    if not(ad_evaluationRunning) then begin
      doStartEvaluation(true,false);
      if lastStart.mainCall then begin
        with editorMeta[PageControl.ActivePageIndex] do ad_callMain(pseudoName,editor.lines,lastStart.parameters);
      end else begin
        with editorMeta[PageControl.ActivePageIndex] do ad_evaluate(pseudoName,editor.lines,true);
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
    ad_haltEvaluation;
    stepper.doStop;
  end;

PROCEDURE TMnhForm.InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);
  begin
    Special:=currentlyDebugging and ad_evaluationRunning and (Sender=debugLine.editor) and (line=debugLine.line);
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
      settingsReady:=true;
    end;

    InputEdit0.Font.name:=settings.value^.editorFontname;
    InputEdit0.Font.size:=SettingsForm.fontSize;
    if SettingsForm.AntialiasCheckbox.Checked
    then InputEdit0.Font.quality:=fqCleartypeNatural
    else InputEdit0.Font.quality:=fqNonAntialiased;
    for i:=1 to 9 do editorMeta[i].editor.Font:=InputEdit0.Font;

    OutputEdit  .Font:=InputEdit0.Font;
    currentExpressionMemo.Font:=InputEdit0.Font;
    helpPopupMemo.Font:=InputEdit0.Font;
    helpPopupMemo.Font.size:=helpPopupMemo.Font.size-2;
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

PROCEDURE formCycle(CONST ownId:longint);
  begin
    if ownId=0 then begin plotForm.Show; plotForm.BringToFront; plotForm.SetFocus; end
               else begin MnhForm.Show; MnhForm.BringToFront; MnhForm.SetFocus; end;
  end;

PROCEDURE lateInitialization;
  VAR i:longint;
  begin
    guiAdapters.addOutAdapter(@guiOutAdapter,false);
    for i:=0 to consoleAdapters.adapterCount-1 do
      if consoleAdapters.getAdapter(i)^.adapterType in [at_textFile,at_htmlFile] then
        guiAdapters.addOutAdapter(consoleAdapters.getAdapter(i),false);

    mnh_evalThread.guiOutAdapters:=@guiAdapters;
    mnh_plotForm.formCycleCallback:=@formCycle;
    registerRule(SYSTEM_BUILTIN_NAMESPACE,'ask', @ask_impl,'',fc_asking);
    mnh_evalThread.initUnit;
  end;

INITIALIZATION
  guiOutAdapter.create;
  guiAdapters.create;
  mnh_plotForm.guiAdapters:=@guiAdapters;
  locationToOpenOnFormStartup:=C_nilTokenLocation;

FINALIZATION
  guiAdapters.destroy;
  guiOutAdapter.destroy;
end.


