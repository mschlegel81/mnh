UNIT mnh_gui_main;

{$mode objfpc}{$H+}
INTERFACE

USES
  Classes, sysutils, FileUtil, SynEdit, SynCompletion, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, Menus, ComCtrls, Grids, PopupNotifier,
  SynHighlighterMnh, mnh_gui_settings, mnh_tokLoc,
  mnh_out_adapters, myStringUtil, mnh_evalThread, mnh_constants,
  types, LCLType,mnh_plotData,mnh_funcs,mnh_litVar,mnh_doc,lclintf, StdCtrls,
  mnh_tokens,closeDialog,askDialog,SynEditKeyCmds,mnh_debugForm,
  myGenerics,mnh_fileWrappers,mySys,mnh_html,mnh_plotFuncs,mnh_cmdLineInterpretation,
  mnh_plotForm;

TYPE

  { TMnhForm }

  TMnhForm = class(TForm)
    autosizeToggleBox: TToggleBox;
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
    MenuItem3: TMenuItem;
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
    PopupNotifier1: TPopupNotifier;
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
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormDropFiles(Sender: TObject; CONST FileNames: array of string);
    PROCEDURE FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE InputEditChange(Sender: TObject);
    PROCEDURE InputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE InputEditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE InputEditProcessUserCommand(Sender: TObject;
      VAR Command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
    PROCEDURE MenuItem4Click(Sender: TObject);
    PROCEDURE miClearClick(Sender: TObject);
    PROCEDURE miCloseClick(Sender: TObject);
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
    PROCEDURE OutputEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE OutputEditMouseUp(Sender: TObject; Button: TMouseButton;
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

  private
    outputHighlighter:TSynMnhSyn;
    underCursor:T_tokenInfo;
    settingsReady:boolean;
    evaluation:record
      required:boolean;
      start:double;
      deferredUntil:double;
    end;
    forceInputEditFocusOnOutputEditMouseUp:boolean;

    doNotMarkWordBefore:double;
    doNotCheckFileBefore:double;

    wordsInEditor:T_listOfString;

    PROCEDURE processSettings;
    PROCEDURE processFileHistory;
    FUNCTION autosizeBlocks(CONST forceOutputFocus:boolean):boolean;
    PROCEDURE positionHelpNotifier;
    PROCEDURE setUnderCursor(CONST wordText:ansistring);
    PROCEDURE ensureWordsInEditorForCompletion;

    PROCEDURE doConditionalPlotReset;
    PROCEDURE openFromHistory(CONST historyIdx:byte);
    PROCEDURE doStartEvaluation;
    PROCEDURE inputEditReposition(CONST caret:TPoint; CONST doJump:boolean);
    PROCEDURE outputEditReposition(CONST caret:TPoint; CONST doJump:boolean);
    PROCEDURE _setErrorlevel_(CONST i: byte);
    FUNCTION _doSaveAs_(CONST index:longint):boolean;
    FUNCTION _doSave_(CONST index:longint):boolean;


    PROCEDURE setupInputRecForNewFile    (CONST index:longint);
    PROCEDURE setupInputRecForLoadingFile(CONST index:longint; CONST fileName:ansistring);
    PROCEDURE updateSheetCaption(CONST index:longint);
    FUNCTION getInputEditIndexForFilename(CONST fileName:ansistring):longint;
    FUNCTION pseudoName(CONST index:longint):ansistring;
  public
    inputRec:array[0..9] of record
      filePath:ansistring;
      fileAccessAge:double;
      changed:boolean;

      sheet       : TTabSheet;
      editor      : TSynEdit;
      highlighter : TSynMnhSyn;
    end;
  end;

  { T_guiOutAdapter }

  T_guiOutAdapter=object(T_collectingOutAdapter)
    CONSTRUCTOR create;
    DESTRUCTOR destroy; virtual;
    FUNCTION flushToGui(VAR syn:TSynEdit):boolean;
    PROCEDURE flushClear;
  end;

VAR MnhForm: TMnhForm;
    locationToOpenOnFormStartup:T_tokenLocation;

PROCEDURE lateInitialization;
PROCEDURE formCycle(ownId:longint; reverse:boolean);
IMPLEMENTATION
VAR guiOutAdapter: T_guiOutAdapter;
    guiAdapters: T_adapters;

{$R *.lfm}

{ T_guiOutAdapter }

CONSTRUCTOR T_guiOutAdapter.create;
  begin
    inherited create(at_gui,'*GUI*');
  end;

DESTRUCTOR T_guiOutAdapter.destroy;
  begin
    inherited destroy;
  end;

FUNCTION T_guiOutAdapter.flushToGui(VAR syn: TSynEdit): boolean;
  VAR i,j:longint;
      instantPlotRequested:boolean=false;
      debugLocation:T_tokenLocation;
      hasDebugLocation:boolean=false;
  begin
    system.enterCriticalSection(cs);
    result:=length(storedMessages)>0;
    for i:=0 to length(storedMessages)-1 do with storedMessages[i] do begin
      case messageType of
        mt_clearConsole: syn.lines.clear;
        mt_plotSettingsChanged: plotForm.pullPlotSettingsToGui;
        mt_plotCreatedWithInstantDisplay: instantPlotRequested:=true;
        mt_plotCreatedWithDeferredDisplay: begin end;
        mt_printline:
          begin
            if (length(multiMessage)>0) and (multiMessage[0]=C_formFeedChar) then begin
              syn.lines.clear;
              for j:=1 to length(multiMessage)-1 do  syn.lines.append(multiMessage[j]);
            end else for j:=0 to length(multiMessage)-1 do syn.lines.append(multiMessage[j]);
          end;
        mt_debug_varInfo: DebugForm.variablesPut(simpleMessage);
        mt_debug_step: begin
          DebugForm.rollingAppend(simpleMessage);
          hasDebugLocation:=true;
          debugLocation:=location;
        end;
        mt_endOfEvaluation: begin
          DebugForm.rollingAppend('Evaluation finished');
          DebugForm.updateFromRoll;
          hasDebugLocation:=false;
          for j:=0 to 9 do begin
            MnhForm.inputRec[j].highlighter.setMarkedToken(-1,-1);
            MnhForm.inputRec[j].editor.readonly:=false;
          end;
        end;
        mt_reloadRequired: begin
          for j:=0 to 9 do with MnhForm.inputRec[i] do
          if sheet.TabVisible and (filePath=simpleMessage) and not(changed) then begin
            editor.lines.loadFromFile(filePath);
            fileAge(filePath,fileAccessAge);
            changed:=false;
          end;
        end;
        mt_echo_input: begin
          syn.lines.append(C_errorLevelTxt[messageType]+' '+simpleMessage);
          DebugForm.rollingAppend(C_errorLevelTxt[messageType]+' '+simpleMessage);
        end;
        mt_echo_declaration,
        mt_echo_output:      syn.lines.append(C_errorLevelTxt[messageType]+                         ' '+simpleMessage);
        else begin           syn.lines.append(C_errorLevelTxt[messageType]+' '+ansistring(location)+' '+simpleMessage);
          DebugForm.rollingAppend(C_errorLevelTxt[messageType]+' '+ansistring(location)+' '+simpleMessage);
        end;
      end;
    end;
    clearMessages;
    if result then begin
      syn.ExecuteCommand(ecEditorBottom,' ',nil);
      syn.ExecuteCommand(ecLineStart,' ',nil);
    end;
    if instantPlotRequested then plotForm.doPlot();
    if hasDebugLocation then begin
      DebugForm.updateFromRoll;
      if not((debugLocation.fileName='') or (debugLocation.fileName='?')) then begin
        j:=MnhForm.getInputEditIndexForFilename(debugLocation.fileName);
        if j>=0 then begin
          MnhForm.PageControl.ActivePageIndex:=j;
          MnhForm.inputRec[j].highlighter.setMarkedToken(debugLocation.line-1,debugLocation.column-1);
          MnhForm.inputRec[j].editor.Repaint;
        end;
      end;
    end;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_guiOutAdapter.flushClear;
  begin
    system.enterCriticalSection(cs);
    clearMessages;
    clearConsole;
    system.leaveCriticalSection(cs);
  end;

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
    if autosizeToggleBox.Checked and (PageControl.ActivePageIndex>=0) then with inputRec[PageControl.ActivePageIndex] do begin
      scrollbarHeight     :=editor.height-editor.ClientHeight;
      idealInputHeight    :=scrollbarHeight+ editor    .Font.GetTextHeight(SAMPLE_TEXT)*(editor    .lines.count+1);
      idealOutputHeight   :=scrollbarHeight+ OutputEdit.Font.GetTextHeight(SAMPLE_TEXT)*(OutputEdit.lines.count+1);
      availableTotalHeight:=editor.height+OutputEdit.height;
      inputFocus:=not(forceOutputFocus or OutputEdit.Focused);

      //Are both editors large enough?
      if (editor.height>=idealInputHeight) and (OutputEdit.height>=idealOutputHeight) then exit;
      if (idealInputHeight+idealOutputHeight<=availableTotalHeight) then begin
        //There is enough room for both
        if inputFocus then idealInputHeight:=availableTotalHeight-idealOutputHeight;
      end else begin
        //There is NOT enough room for both
        temp:=round(0.9*availableTotalHeight);
        if inputFocus
        then begin
          if idealInputHeight>=temp then idealInputHeight:=temp;
        end else begin
          if idealOutputHeight>=temp then idealOutputHeight:=temp;
          idealInputHeight:=availableTotalHeight-idealOutputHeight;
        end;
      end;
      if idealInputHeight<>PageControl.height then begin
        if idealInputHeight<PageControl.height then begin
          if inputHeightSpeed>=0 then inputHeightSpeed:=-1
                                 else dec(inputHeightSpeed);
        end else begin
          if inputHeightSpeed<=0 then inputHeightSpeed:=1
                                 else inc(inputHeightSpeed);
        end;
        PageControl.height:=PageControl.height+inputHeightSpeed;
        if PopupNotifier1.visible then positionHelpNotifier;
        autosizeToggleBox.top:=OutputEdit.top;
        result:=true;
      end;
    end;
  end;

PROCEDURE TMnhForm.positionHelpNotifier;
  begin
    PopupNotifier1.ShowAtPos(left+PageControl.width-PopupNotifier1.vNotifierForm.width,
                             ClientToScreen(point(left,OutputEdit.top)).y);
    if (PageControl.ActivePageIndex>=0) then inputRec[PageControl.ActivePageIndex].editor.SetFocus;
  end;

PROCEDURE TMnhForm.setUnderCursor(CONST wordText: ansistring);
  VAR i:longint;
  begin
    if not(isIdentifier(wordText,true)) then exit;
    outputHighlighter.setMarkedWord(wordText);
    for i:=0 to 9 do with inputRec[i] do if highlighter.setMarkedWord(wordText) then editor.Repaint;
    if miHelp.Checked then ad_explainIdentifier(wordText,underCursor)
                      else begin
                        underCursor.tokenText:=wordText;
                        underCursor.tokenExplanation:='';
                      end;
    if (underCursor.tokenText<>'') and (underCursor.tokenText<>PopupNotifier1.title) and (miHelp.Checked) then begin
      PopupNotifier1.title:=underCursor.tokenText;
      PopupNotifier1.text:=underCursor.tokenExplanation;
      if not(PopupNotifier1.visible) then positionHelpNotifier;
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
    if fileExists(SettingsForm.historyItem(historyIdx)) then begin
      index:=getInputEditIndexForFilename(SettingsForm.historyItem(historyIdx));
      if index>=0 then PageControl.ActivePageIndex:=index;
    end else if SettingsForm.polishHistory then processFileHistory;
  end;

PROCEDURE TMnhForm.doStartEvaluation;
  VAR i:longint;
  begin
    with evaluation do begin
      required:=false;
      deferredUntil:=now+0.1*ONE_SECOND;
      start:=now;
    end;

    guiOutAdapter.flushClear;
    UpdateTimeTimerTimer(self);
    UpdateTimeTimer.Interval:=20;
    doConditionalPlotReset;
    underCursor.tokenText:='';
    if miDebug.Checked then begin
      DebugForm.clearRoll;
      for i:=0 to 9 do inputRec[i].editor.readonly:=true;
      stepper.doStart;
      DebugForm.Show;
    end else stepper.setSignal(ds_run);
  end;

PROCEDURE TMnhForm.inputEditReposition(CONST caret: TPoint;
  CONST doJump: boolean);
  VAR wordUnderCursor:string;
      newCaret:TPoint;
      pageIdx:longint;
  begin
    with inputRec[PageControl.ActivePageIndex] do begin
      wordUnderCursor:=editor.GetWordAtRowCol(caret);
      setUnderCursor(wordUnderCursor);
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
        inputRec[pageIdx].editor.CaretXY:=newCaret;
      end;
    end;
  end;

PROCEDURE TMnhForm.outputEditReposition(CONST caret: TPoint; CONST doJump: boolean);
  VAR loc:T_tokenLocation;
      newCaret:TPoint;
      pageIdx:longint;
  begin
    forceInputEditFocusOnOutputEditMouseUp:=false;
    setUnderCursor(OutputEdit.GetWordAtRowCol(caret));
    loc:=guessLocationFromString(OutputEdit.lines[caret.y-1],false);
    if not(doJump) then exit;
    if (loc.fileName='') or (loc.fileName='?') then exit;
    pageIdx:=getInputEditIndexForFilename(loc.fileName);
    if pageIdx<0 then exit;
    PageControl.ActivePageIndex:=pageIdx;
    with inputRec[pageIdx] do begin
      editor.SetFocus;
      highlighter.setMarkedToken(loc.line-1,loc.column-1);
      newCaret.x:=loc.column;
      newCaret.y:=loc.line;
      editor.CaretXY:=newCaret;
      forceInputEditFocusOnOutputEditMouseUp:=true;
    end;



    //TODO if necessary change tab
    //if (loc.column>0) and (loc.fileName=environment.mainPackageProvider^.getPath)
    //then begin
    //  inputHighlighter.setMarkedToken(loc.line-1,loc.column-1);
    //  if doJump then begin
    //    newCaret.x:=loc.column;
    //    newCaret.y:=loc.line;
    //    ActiveInputEdit.CaretXY:=newCaret;
    //    forceInputEditFocusOnOutputEditMouseUp:=true;
    //    exit;
    //  end;
    //end else inputHighlighter.setMarkedToken(-1,-1);
    //ActiveInputEdit.Repaint;
  end;

{ TMnhForm }
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
    inputRec[0].editor:=InputEdit0;
    inputRec[1].editor:=InputEdit1;
    inputRec[2].editor:=InputEdit2;
    inputRec[3].editor:=InputEdit3;
    inputRec[4].editor:=InputEdit4;
    inputRec[5].editor:=InputEdit5;
    inputRec[6].editor:=InputEdit6;
    inputRec[7].editor:=InputEdit7;
    inputRec[8].editor:=InputEdit8;
    inputRec[9].editor:=InputEdit9;
    inputRec[0].sheet:=EditorTabSheet;
    inputRec[1].sheet:=TabSheet1;
    inputRec[2].sheet:=TabSheet2;
    inputRec[3].sheet:=TabSheet3;
    inputRec[4].sheet:=TabSheet4;
    inputRec[5].sheet:=TabSheet5;
    inputRec[6].sheet:=TabSheet6;
    inputRec[7].sheet:=TabSheet7;
    inputRec[8].sheet:=TabSheet8;
    inputRec[9].sheet:=TabSheet9;
    for i:=0 to 9 do with inputRec[i] do begin
      highlighter:=TSynMnhSyn.create(nil,msf_input);
      editor.highlighter:=highlighter;
    end;

    doNotMarkWordBefore:=now;
    doNotCheckFileBefore:=now+ONE_SECOND;
    outputHighlighter:=TSynMnhSyn.create(nil,msf_output);
    OutputEdit.highlighter:=outputHighlighter;
    OutputEdit.ClearAll;
    endOfEvaluationText.value:=msg;
    for i:=0 to length(LOGO)-1 do OutputEdit.lines.append(LOGO[i]);
    {$ifdef debugMode}
    guiAdapters.addConsoleOutAdapter;
    {$endif}
  end;

PROCEDURE TMnhForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  VAR i:integer;
      state:T_editorState;
  begin
    if ad_evaluationRunning then ad_haltEvaluation;
    stepper.onAbort;
    for i:=0 to 9 do with inputRec[i] do begin
      if sheet.TabVisible
      then state.create(filePath,fileAccessAge,changed,editor.lines)
      else state.create;
      SettingsForm.setEditorState(i,state);
    end;
  end;

PROCEDURE TMnhForm.FormDestroy(Sender: TObject);
  VAR i:longint;
  begin
    guiAdapters.removeOutAdapter(@guiOutAdapter);
    for i:=0 to 9 do with inputRec[i] do begin
      highlighter.destroy;
    end;
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
    if (key=9) and (ssCtrl in Shift) then formCycle(0,ssShift in Shift)
    else if (key=87) and (Shift=[ssCtrl]) then miCloseClick(Sender);
  end;

PROCEDURE TMnhForm.FormResize(Sender: TObject);
  VAR formPosition:T_formPosition;
  begin
    if settingsReady then begin
      formPosition.create;
      formPosition.top   :=top;
      formPosition.left  :=left;
      formPosition.width :=width;
      formPosition.height:=height;
      formPosition.isFullscreen:=(WindowState=wsMaximized);
      SettingsForm.mainFormPosition:=formPosition;
    end else plotForm.pullPlotSettingsToGui();
    if PopupNotifier1.visible then positionHelpNotifier;
  end;

PROCEDURE TMnhForm.FormShow(Sender: TObject);
  VAR newCaret:TPoint;
      state:T_editorState;
      i:longint;
  begin
    if not(settingsReady) then begin
      processSettings;
      for i:=0 to 9 do begin
        state:=SettingsForm.getEditorState(i);
        inputRec[i].sheet.TabVisible:=state.visible;
        if state.visible then begin
          inputRec[i].changed:=state.changed;
          inputRec[i].fileAccessAge:=state.fileAccessAge;
          inputRec[i].filePath:=state.filePath;
          state.getLines(inputRec[i].editor.lines);
        end;
        updateSheetCaption(i);
      end;
      i:=SettingsForm.pageIndex;
      while (i>=0) and not(inputRec[i].sheet.TabVisible) do dec(i);
      if i<0 then begin
        i:=9;
        while (i>=0) and not(inputRec[i].sheet.TabVisible) do dec(i);
      end;
      if i>=0 then begin
        PageControl.ActivePageIndex:=i;
        inputRec[i].editor.SetFocus;
      end else setupInputRecForNewFile(0);

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
          inputRec[i].editor.CaretXY:=newCaret;
        end;
      end;
    end;
    KeyPreview:=true;
    UpdateTimeTimer.Enabled:=true;
  end;

PROCEDURE TMnhForm.InputEditChange(Sender: TObject);
  begin
    if (miEvalModeDirectOnKeypress.Checked) and not(SynCompletion.IsActive) then begin
      if now>evaluation.deferredUntil then begin
        doStartEvaluation;
        with inputRec[PageControl.ActivePageIndex] do ad_evaluate(pseudoName(PageControl.ActivePageIndex),editor.lines);
      end else evaluation.required:=true;
    end;
    with inputRec[PageControl.ActivePageIndex] do begin
      changed:=true;
      updateSheetCaption(PageControl.ActivePageIndex);
    end;
  end;

PROCEDURE TMnhForm.InputEditKeyDown(Sender: TObject; VAR key: word;
  Shift: TShiftState);
  begin
    if (key=13) and ((ssCtrl in Shift) or (ssAlt in Shift)) then inputEditReposition(inputRec[PageControl.ActivePageIndex].editor.CaretXY,ssCtrl in Shift);
  end;

PROCEDURE TMnhForm.InputEditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    inputEditReposition(inputRec[PageControl.ActivePageIndex].editor.PixelsToRowColumn(point),ssCtrl in Shift);
  end;

PROCEDURE TMnhForm.InputEditProcessUserCommand(Sender: TObject;
  VAR Command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
  VAR i:longint;
      commented:boolean=true;
  begin
    with inputRec[PageControl.ActivePageIndex] do if (Command=ecUserDefinedFirst) and (editor.BlockBegin.y>=1) then begin
      for i:=editor.BlockBegin.y-1 to editor.BlockEnd.y-1 do
        commented:=commented and (copy(trim(editor.lines[i]),1,2)='//');
      if commented
      then for i:=editor.BlockBegin.y-1 to editor.BlockEnd.y-1 do
        editor.lines[i]:=replaceOne(editor.lines[i],'//','')
      else for i:=editor.BlockBegin.y-1 to editor.BlockEnd.y-1 do
      editor.lines[i]:='//'+editor.lines[i];
    end;
    if Command=ecUserDefinedFirst+1 then begin
      for i:=1 to 9 do if inputRec[(i+PageControl.ActivePageIndex) mod 10].sheet.TabVisible then begin
        PageControl.ActivePageIndex:=(i+PageControl.ActivePageIndex) mod 10;
        inputRec[PageControl.ActivePageIndex].editor.SetFocus;
        exit;
      end;
    end;
    if Command=ecUserDefinedFirst+2 then begin
      for i:=9 downto 1 do if inputRec[(i+PageControl.ActivePageIndex) mod 10].sheet.TabVisible then begin
        PageControl.ActivePageIndex:=(i+PageControl.ActivePageIndex) mod 10;
        inputRec[PageControl.ActivePageIndex].editor.SetFocus;
        exit;
      end;
    end;
    if (Command=ecUserDefinedFirst+3) then with inputRec[PageControl.ActivePageIndex] do begin
      stepper.toggleBreakpoint(pseudoName(PageControl.ActivePageIndex),editor.CaretY);
      DebugForm.updateBreakpointGrid;
    end;
  end;

PROCEDURE TMnhForm.MenuItem4Click(Sender: TObject);
  begin
    if PageControl.ActivePageIndex<0 then exit;
    askForm.initWithQuestion('Please give command line parameters');
    if askForm.ShowModal=mrOk then begin
      doStartEvaluation;
      with inputRec[PageControl.ActivePageIndex] do ad_callMain(pseudoName(PageControl.ActivePageIndex),editor.lines,askForm.getLastAnswerReleasing);
    end else askForm.getLastAnswerReleasing;
  end;

PROCEDURE TMnhForm.miClearClick(Sender: TObject);
  VAR mr:integer;
  begin
    for mr:=0 to 9 do if not(inputRec[mr].sheet.TabVisible) then begin
      setupInputRecForNewFile(mr);
      PageControl.ActivePageIndex:=mr;
      exit;
    end;
    with inputRec[PageControl.ActivePageIndex] do begin
      if changed then begin
        mr:=closeDialogForm.showOnLoad;
        if mr=mrOk then if not(_doSave_(PageControl.ActivePageIndex)) then exit;
        if mr=mrCancel then exit;
      end;
      if filePath<>'' then SettingsForm.fileClosed(filePath);
      processFileHistory;
    end;
    setupInputRecForNewFile(mr);
  end;

PROCEDURE TMnhForm.miCloseClick(Sender: TObject);
  VAR i,mr:longint;
  begin
    if PageControl.ActivePageIndex<0 then exit;
    with inputRec[PageControl.ActivePageIndex] do begin
      if changed then begin
        mr:=closeDialogForm.showOnLoad;
        if mr=mrOk then if not(_doSave_(PageControl.ActivePageIndex)) then exit;
        if mr=mrCancel then exit;
      end;
      sheet.TabVisible:=false;
      if filePath<>'' then begin
        SettingsForm.fileClosed(filePath);
        processFileHistory;
      end;
    end;
    if PageControl.ActivePageIndex>0 then
    for i:=9+PageControl.ActivePageIndex downto
             PageControl.ActivePageIndex do if inputRec[i mod 10].sheet.TabVisible then begin
      PageControl.ActivePageIndex:=i mod 10;
      exit;
    end;
    //If we reach that line, closing just closed the last tab
    setupInputRecForNewFile(0);
  end;

PROCEDURE TMnhForm.miDebugClick(Sender: TObject);
  VAR i:longint;
  begin
    if miDebug.Checked
    then begin
      miDebug.Checked:=false;
      if ad_evaluationRunning then stepper.setSignal(ds_run);
    end else begin
      miDebug.Checked:=true;
      miEvalModeDirect.Checked:=true;
      miEvalModeDirectOnKeypress.Checked:=false;
      SettingsForm.wantInstantEvaluation:=false;
      if ad_evaluationRunning then begin
        DebugForm.clearRoll;
        for i:=0 to 9 do with inputRec[i] do editor.readonly:=true;
        stepper.doStep;
      end;
      DebugForm.Show;
    end;
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
      SettingsForm.behaviour:=guiAdapters.outputBehaviour;
    end;
  end;

PROCEDURE TMnhForm.miEvalModeDirectClick(Sender: TObject);
  begin
    if miEvalModeDirect.Checked then exit;
    miEvalModeDirect.Checked:=true;
    miEvalModeDirectOnKeypress.Checked:=false;
    SettingsForm.wantInstantEvaluation:=false;
  end;

PROCEDURE TMnhForm.miEvalModeDirectOnKeypressClick(Sender: TObject);
  begin
    if miEvalModeDirectOnKeypress.Checked then exit;
    miDebug.Checked:=false;
    miEvalModeDirect.Checked:=false;
    miEvalModeDirectOnKeypress.Checked:=true;
    SettingsForm.wantInstantEvaluation:=true;
  end;

PROCEDURE TMnhForm.miEvaluateNowClick(Sender: TObject);
  begin
    if now>evaluation.deferredUntil then begin
      doStartEvaluation;
      with inputRec[PageControl.ActivePageIndex] do ad_evaluate(pseudoName(PageControl.ActivePageIndex),editor.lines);
    end else evaluation.required:=true;
  end;

PROCEDURE TMnhForm.miExpressionEchoClick(Sender: TObject);
  begin
    if settingsReady then begin
      miExpressionEcho.Checked:=not(miExpressionEcho.Checked);
      guiAdapters.doEchoInput:=miExpressionEcho.Checked;
      SettingsForm.behaviour:=guiAdapters.outputBehaviour;
    end;
  end;

PROCEDURE TMnhForm.miExpressionResultClick(Sender: TObject);
  begin
    if settingsReady then begin
      miExpressionResult.Checked:=not(miExpressionResult.Checked);
      guiAdapters.doShowExpressionOut:=miExpressionResult.Checked;
      SettingsForm.behaviour:=guiAdapters.outputBehaviour;
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
  if not(miHelp.Checked) then PopupNotifier1.visible:=false
                         else if underCursor.tokenText<>'' then setUnderCursor(underCursor.tokenText);
end;

PROCEDURE TMnhForm.miHelpExternallyClick(Sender: TObject);
  begin
    findAndDocumentAllPackages;
    OpenURL('file:///'+replaceAll(expandFileName(htmlRoot+'\index.html'),'\','/'));
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
      SettingsForm.behaviour:=guiAdapters.outputBehaviour;
    end;
  end;

FUNCTION TMnhForm._doSaveAs_(CONST index:longint): boolean;
  VAR arr:T_arrayOfString;
      i:longint;
  begin
    if index<0 then exit(false);
    if SaveDialog.execute then with inputRec[index] do begin
      filePath:=extractRelativePath(expandFileName(''),SaveDialog.fileName);
      setLength(arr,editor.lines.count);
      for i:=0 to length(arr)-1 do arr[i]:=editor.lines[i];
      writeFileLines(filePath,arr,'');
      fileAge(filePath,fileAccessAge);
      changed:=false;
      result:=true;
      updateSheetCaption(index);
    end else result:=false;
  end;

FUNCTION TMnhForm._doSave_(CONST index:longint): boolean;
  VAR arr:T_arrayOfString;
      i:longint;
  begin
    if index<0 then exit(false);
    with inputRec[index] do if filePath='' then result:=_doSaveAs_(index)
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

PROCEDURE TMnhForm.setupInputRecForNewFile    (CONST index:longint);
  begin
    with inputRec[index] do begin
      filePath:='';
      fileAccessAge:=0;
      changed:=false;

      editor.lines.clear;
      sheet.TabVisible:=true;
      updateSheetCaption(index);
    end;
  end;

PROCEDURE TMnhForm.setupInputRecForLoadingFile(CONST index:longint; CONST fileName:ansistring);
  begin
    with inputRec[index] do begin
      filePath:=extractRelativePath(expandFileName(''),fileName);
      fileAge(filePath,fileAccessAge);
      changed:=false;

      editor.lines.loadFromFile(fileName);
      sheet.TabVisible:=true;
      updateSheetCaption(index);
    end;
  end;

PROCEDURE TMnhForm.updateSheetCaption(CONST index:longint);
  VAR i:longint;
  begin
    if (index<0) or (index>9)
    then for i:=0 to 9 do updateSheetCaption(i)
    else with inputRec[index] do begin
      if filePath<>'' then begin
        if changed then sheet.Caption:=filePath+' *'
                   else sheet.Caption:=filePath;
      end else sheet.Caption:='<new '+intToStr(index)+'>';
    end;
  end;

FUNCTION TMnhForm.getInputEditIndexForFilename(CONST fileName:ansistring):longint;
  VAR i:longint;
      uName:ansistring;
  begin
    uName:=extractRelativePath(expandFileName(''),fileName);
    for i:=0 to 9 do with inputRec[i] do if sheet.TabVisible and ((filePath=uName) or (pseudoName(i)=fileName)) then exit(i);
    if copy(fileName,1,4)='<new' then exit;
    for i:=0 to 9 do with inputRec[i] do
    if not(sheet.TabVisible) then begin
      setupInputRecForLoadingFile(i,uName);
      exit(i);
    end;
    result:=-1;
  end;

FUNCTION TMnhForm.pseudoName(CONST index:longint):ansistring;
  begin
    with inputRec[index] do if filePath<>'' then result:=filePath
                                            else result:='<new '+intToStr(index)+'>';
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
        with inputRec[PageControl.ActivePageIndex] do begin
          if changed then begin
            mr:=closeDialogForm.showOnLoad;
            if mr=mrOk then if not(_doSave_(PageControl.ActivePageIndex)) then exit;
            if mr=mrCancel then exit;
          end;
          if filePath<>'' then SettingsForm.fileClosed(filePath);
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
      SettingsForm.behaviour:=guiAdapters.outputBehaviour;
    end;
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
    if forceInputEditFocusOnOutputEditMouseUp and (PageControl.ActivePageIndex>=0) then ActiveControl:=inputRec[PageControl.ActivePageIndex].editor;
    forceInputEditFocusOnOutputEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.OutputEditMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    outputEditReposition(OutputEdit.PixelsToRowColumn(point),ssCtrl in Shift);
  end;

PROCEDURE TMnhForm.OutputEditMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if forceInputEditFocusOnOutputEditMouseUp then ActiveControl:=InputEdit0;
  forceInputEditFocusOnOutputEditMouseUp :=false;
end;

PROCEDURE TMnhForm.PageControlChange(Sender: TObject);
  begin
    if PageControl.ActivePageIndex>=0 then begin
      SynCompletion.editor:=inputRec[PageControl.ActivePageIndex].editor;
      SettingsForm.pageIndex:=PageControl.ActivePageIndex;
    end;
  end;

PROCEDURE TMnhForm.PopupNotifier1Close(Sender: TObject;
  VAR CloseAction: TCloseAction);
  begin
    miHelp.Checked:=false;
  end;

PROCEDURE TMnhForm.Splitter1Moved(Sender: TObject);
  begin
    if PopupNotifier1.visible then positionHelpNotifier;
    autosizeToggleBox.top:=OutputEdit.top;
    autosizeToggleBox.Checked:=false;
  end;

PROCEDURE TMnhForm.SynCompletionCodeCompletion(VAR value: string; sourceValue: string; VAR SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
  begin
    if (pos('.',value)>0) then begin
      if pos(sourceValue,value)<>1 then
        value:=copy(value,pos('.',value)+1,length(value));
    end;
    wordsInEditor.clear;
  end;

PROCEDURE TMnhForm.ensureWordsInEditorForCompletion;
  VAR i:longint;
      caret:TPoint;
  begin
    if wordsInEditor.size>0 then exit;
    if PageControl.ActivePageIndex>=0 then with inputRec[PageControl.ActivePageIndex] do begin
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
    then aid:='MNH5 '+inputRec[PageControl.ActivePageIndex].sheet.Caption
    else aid:='MNH5';
    if aid<>Caption then Caption:=aid;
    //-------------------------------------------------------------:Form caption
    //progress time:------------------------------------------------------------
    if stepper.haltet and isEvaluationRunning then aid:=C_tabChar+'[haltet]'
    else if PageControl.ActivePageIndex>=0
    then aid:=C_tabChar+intToStr(inputRec[PageControl.ActivePageIndex].editor.CaretY)+','+intToStr(inputRec[PageControl.ActivePageIndex].editor.CaretX)
    else aid:='';
    if isEvaluationRunning then StatusBar.SimpleText:='Evaluating: '+myTimeToStr(now-evaluation.start)+aid
                           else StatusBar.SimpleText:=endOfEvaluationText.value                       +aid;
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
      doStartEvaluation;
      with inputRec[PageControl.ActivePageIndex] do ad_evaluate(pseudoName(PageControl.ActivePageIndex),editor.lines);
      UpdateTimeTimer.Interval:=MIN_INTERVALL;
    end;

    if not(flushPerformed) and not(autosizingDone) then begin
      UpdateTimeTimer.Interval:=UpdateTimeTimer.Interval+10;
      if UpdateTimeTimer.Interval>MAX_INTERVALL then UpdateTimeTimer.Interval:=MAX_INTERVALL;
    end else UpdateTimeTimer.Interval:=MIN_INTERVALL;
    //================================================================:slow ones
    if SettingsForm.timeForSaving then begin
      for i:=0 to 9 do with inputRec[i] do begin
        if sheet.TabVisible
        then state.create(filePath,fileAccessAge,changed,editor.lines)
        else state.create;
        SettingsForm.setEditorState(i,state);
      end;
      SettingsForm.saveSettings;
    end;

    if (now>doNotCheckFileBefore) then begin
      doNotCheckFileBefore:=now+1;
      for i:=0 to 9 do with inputRec[i] do if sheet.TabVisible and (filePath<>'') and not(changed) then begin
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
  end;

PROCEDURE TMnhForm.processSettings;
  VAR formPosition:T_formPosition;
      i:longint;
  begin
    if not(settingsReady) then begin

      formPosition:=SettingsForm.mainFormPosition;
      top   :=formPosition.top;
      left  :=formPosition.left;
      width :=formPosition.width;
      height:=formPosition.height;
      if formPosition.isFullscreen then WindowState:=wsMaximized;

      with SettingsForm.behaviour do begin
        miDeclarationEcho.Checked:=doEchoDeclaration;
        miExpressionEcho.Checked:=doEchoInput;
        miExpressionResult.Checked:=doShowExpressionOut;
        miTimingInfo.Checked:=doShowTimingInfo;
        miMinErrorlevel1.Checked:=minErrorLevel<=1;
        miMinErrorlevel2.Checked:=minErrorLevel=2;
        miMinErrorlevel3.Checked:=minErrorLevel=3;
        miMinErrorlevel4.Checked:=minErrorLevel=4;
        miMinErrorlevel5.Checked:=minErrorLevel>=5;
        guiAdapters.outputBehaviour:=SettingsForm.behaviour;
      end;

      plotForm.miAutoReset.Checked:=SettingsForm.resetPlotOnEvaluation;
      miEvalModeDirect.Checked:=not(SettingsForm.wantInstantEvaluation);
      miEvalModeDirectOnKeypress.Checked:=SettingsForm.wantInstantEvaluation;
      processFileHistory;
      settingsReady:=true;
    end;

    InputEdit0.Font.name:=SettingsForm.getEditorFontName;
    InputEdit0.Font.size:=SettingsForm.fontSize;
    if SettingsForm.AntialiasCheckbox.Checked
    then InputEdit0.Font.Quality:=fqCleartypeNatural
    else InputEdit0.Font.Quality:=fqNonAntialiased;
    for i:=1 to 9 do inputRec[i].editor.Font:=InputEdit0.Font;

    OutputEdit.Font            :=InputEdit0.Font;
    DebugForm.debugEdit.Font   :=InputEdit0.Font;
    DebugForm.variableEdit.Font:=InputEdit0.Font;
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
    for i:=0 to 9 do if SettingsForm.historyItem(i)='' then begin
      historyMenuItem(i).Enabled:=false;
      historyMenuItem(i).visible:=false;
    end else begin
      historyMenuItem(i).Enabled:=true;
      historyMenuItem(i).visible:=true;
      historyMenuItem(i).Caption:=intToStr(i)+': '+SettingsForm.historyItem(i);
    end;
  end;

PROCEDURE debugForm_stopDebugging;
  begin
    MnhForm.miDebug.Checked:=false;
  end;

PROCEDURE debugForm_debuggingStep;
  begin
    MnhForm.UpdateTimeTimerTimer(nil);
    MnhForm.UpdateTimeTimer.Interval:=20;
  end;

PROCEDURE formCycle(ownId:longint; reverse:boolean);
  begin
    if reverse then ownId:=(ownId+3-1) mod 3
               else ownId:=(ownId  +1) mod 3;
    if      ownId=0 then MnhForm.Show
    else if ownId=1 then plotForm.Show
                    else DebugForm.Show;
  end;

PROCEDURE lateInitialization;
  VAR i:longint;
  begin
    guiAdapters.addOutAdapter(@guiOutAdapter,false);
    for i:=0 to consoleAdapters.adapterCount-1 do
      if consoleAdapters.getAdapter(i)^.adapterType in [at_textFile,at_htmlFile] then
        guiAdapters.addOutAdapter(consoleAdapters.getAdapter(i),false);

    mnh_evalThread.guiOutAdapters:=@guiAdapters;
    StopDebuggingCallback:=@debugForm_stopDebugging;
    DebuggingStepCallback:=@debugForm_debuggingStep;
    mnh_plotForm.formCycleCallback:=@formCycle;
    mnh_debugForm.formCycleCallback:=@formCycle;
    registerRule(SYSTEM_BUILTIN_NAMESPACE,'ask', @ask_impl,'');
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

