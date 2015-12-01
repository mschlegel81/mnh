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
    miMinErrorlevel5: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miMinErrorlevel1: TMenuItem;
    miMinErrorlevel2: TMenuItem;
    miMinErrorlevel3: TMenuItem;
    miMinErrorlevel4: TMenuItem;
    miTimingInfo: TMenuItem;
    miStartAnotherInstance: TMenuItem;
    miDebugFrom: TMenuItem;
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
    inputHighlighter,outputHighlighter:TSynMnhSyn;
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
    UpdateTimeTimer: TTimer;
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE InputEditChange(Sender: TObject);
    PROCEDURE InputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE InputEditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE InputEditProcessUserCommand(Sender: TObject;
      VAR Command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
    PROCEDURE MenuItem4Click(Sender: TObject);
    PROCEDURE miClearClick(Sender: TObject);
    PROCEDURE miDebugClick(Sender: TObject);
    PROCEDURE miDebugFromClick(Sender: TObject);
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
    PROCEDURE PopupNotifier1Close(Sender: TObject; VAR CloseAction: TCloseAction
      );
    PROCEDURE Splitter1Moved(Sender: TObject);
    PROCEDURE SynCompletionCodeCompletion(VAR value: string;
      sourceValue: string; VAR SourceStart, SourceEnd: TPoint;
      KeyChar: TUTF8Char; Shift: TShiftState);
    PROCEDURE SynCompletionExecute(Sender: TObject);
    PROCEDURE SynCompletionSearchPosition(VAR APosition: integer);
    PROCEDURE UpdateTimeTimerTimer(Sender: TObject);

  private
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
    FUNCTION _doSaveAs_:boolean;
    FUNCTION _doSave_:boolean;
    { private declarations }
  public
    { public declarations }
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
IMPLEMENTATION
VAR guiOutAdapter: T_guiOutAdapter;
    guiAdapters: T_adapters;

{$R *.lfm}

{ T_guiOutAdapter }

CONSTRUCTOR T_guiOutAdapter.create;
  begin
    inherited create(at_gui);
  end;

DESTRUCTOR T_guiOutAdapter.destroy;
  begin
    inherited destroy;
  end;

FUNCTION T_guiOutAdapter.flushToGui(VAR syn: TSynEdit): boolean;
  VAR i,j:longint;
      instantPlotRequested:boolean=false;
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
            end else for j:=0 to length(multiMessage)-1 do  syn.lines.append(multiMessage[j]);
          end;
        mt_debug_step: begin
          DebugForm.rollingAppend(simpleMessage);
          if location.fileName=ad_currentFile then begin
            MnhForm.inputHighlighter.setMarkedToken(location.line-1,location.column-1);
            MnhForm.InputEdit0.Repaint;
          end;
        end;
        mt_endOfEvaluation: begin
          DebugForm.rollingAppend('Evaluation finished');
          MnhForm.InputEdit0.readonly:=false;
          MnhForm.inputHighlighter.setMarkedToken(-1,-1);
        end;
        mt_reloadRequired: ad_doReload(MnhForm.InputEdit0.lines);
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
    if autosizeToggleBox.Checked then begin
      scrollbarHeight     :=InputEdit0.height-InputEdit0.ClientHeight;
      idealInputHeight    :=scrollbarHeight+ InputEdit0 .Font.GetTextHeight(SAMPLE_TEXT)*(InputEdit0 .lines.count+1);
      idealOutputHeight   :=scrollbarHeight+ OutputEdit.Font.GetTextHeight(SAMPLE_TEXT)*(OutputEdit.lines.count+1);
      availableTotalHeight:=InputEdit0.height+OutputEdit.height;
      inputFocus:=not(forceOutputFocus or OutputEdit.Focused);

      //Are both editors large enough?
      if (InputEdit0.height>=idealInputHeight) and (OutputEdit.height>=idealOutputHeight) then exit;
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
        if PopupNotifier1.Visible then positionHelpNotifier;
        autosizeToggleBox.top:=OutputEdit.top;
        result:=true;
      end;
    end;
  end;

PROCEDURE TMnhForm.positionHelpNotifier;
  begin
    PopupNotifier1.ShowAtPos(left+InputEdit0.width-PopupNotifier1.vNotifierForm.width,
                             ClientToScreen(point(left,OutputEdit.top)).y);
    InputEdit0.SetFocus;
  end;

PROCEDURE TMnhForm.setUnderCursor(CONST wordText: ansistring);
  begin
    if not(isIdentifier(wordText,true)) then exit;
    if (inputHighlighter.setMarkedWord(wordText) and outputHighlighter.setMarkedWord(wordText)) then InputEdit0.Repaint;
    if miHelp.Checked then ad_explainIdentifier(wordText,underCursor)
                      else begin
                        underCursor.tokenText:=wordText;
                        underCursor.tokenExplanation:='';
                      end;
    if (underCursor.tokenText<>'') and (underCursor.tokenText<>PopupNotifier1.title) and (miHelp.Checked) then begin
      PopupNotifier1.title:=underCursor.tokenText;
      PopupNotifier1.text:=underCursor.tokenExplanation;
      if not(PopupNotifier1.Visible) then positionHelpNotifier;
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
  VAR mr:integer;
  begin
    if fileExists(SettingsForm.historyItem(historyIdx)) then begin
      if (ad_needSave(InputEdit0.lines)) then begin
        mr:=closeDialogForm.showOnLoad;
        if mr=mrOk then if not(_doSave_) then exit;
        if mr=mrCancel then exit;
      end;
      ad_setFile(SettingsForm.historyItem(historyIdx),InputEdit0.lines);
      if SettingsForm.setFileInEditor(SettingsForm.historyItem(historyIdx)) then processFileHistory;
    end else if SettingsForm.polishHistory then processFileHistory;
  end;

PROCEDURE TMnhForm.doStartEvaluation;
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
      DebugForm.debugEdit.ClearAll;
      InputEdit0.readonly:=true;
      stepper.doStep;
      DebugForm.Show;
    end else stepper.setFreeRun;
  end;

PROCEDURE TMnhForm.inputEditReposition(CONST caret: TPoint;
  CONST doJump: boolean);
  VAR wordUnderCursor:string;
      newCaret:TPoint;
  begin
    wordUnderCursor:=InputEdit0.GetWordAtRowCol(caret);
    setUnderCursor(wordUnderCursor);
    if not(doJump) then exit;
    if not(miHelp.Checked) then ad_explainIdentifier(wordUnderCursor,underCursor);
    if (underCursor.tokenText<>wordUnderCursor) or
       (underCursor.location.column<=0) then exit;
    if underCursor.location.fileName=ad_currentFile then begin
      newCaret.x:=underCursor.location.column;
      newCaret.y:=underCursor.location.line;
      InputEdit0.CaretXY:=newCaret;
    end else begin
      runCommandAsyncOrPipeless(paramStr(0),'-open'+ansistring(underCursor.location),true);
    end;
  end;

PROCEDURE TMnhForm.outputEditReposition(CONST caret: TPoint; CONST doJump: boolean);
  VAR loc:T_tokenLocation;
      newCaret:TPoint;
  begin
    forceInputEditFocusOnOutputEditMouseUp:=false;
    setUnderCursor(OutputEdit.GetWordAtRowCol(caret));
    loc:=guessLocationFromString(OutputEdit.lines[caret.y-1],false);
    if (loc.column>0) and (loc.fileName=environment.mainPackageProvider^.getPath)
    then begin
      inputHighlighter.setMarkedToken(loc.line-1,loc.column-1);
      if doJump then begin
        newCaret.x:=loc.column;
        newCaret.y:=loc.line;
        InputEdit0.CaretXY:=newCaret;
        forceInputEditFocusOnOutputEditMouseUp:=true;
        exit;
      end;
    end else inputHighlighter.setMarkedToken(-1,-1);
    InputEdit0.Repaint;
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

    doNotMarkWordBefore:=now;
    doNotCheckFileBefore:=now+10*ONE_SECOND;
    OpenDialog.fileName:=paramStr(0);
    SaveDialog.fileName:=paramStr(0);
    inputHighlighter:=TSynMnhSyn.create(nil,msf_input);
    outputHighlighter:=TSynMnhSyn.create(nil,msf_output);
    InputEdit0.highlighter:=inputHighlighter;
    OutputEdit.highlighter:=outputHighlighter;
    OutputEdit.ClearAll;
    endOfEvaluationText.value:=msg;
    for i:=0 to length(LOGO)-1 do OutputEdit.lines.append(LOGO[i]);
    {$ifdef debugMode}
    guiAdapters.addConsoleOutAdapter;
    {$endif}
  end;

PROCEDURE TMnhForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  VAR mr:integer;
  begin
    if (ad_currentFile<>'') and (ad_needSave(InputEdit0.lines)) then begin
      mr:=closeDialogForm.showOnQuit;
      if mr=mrOk then if not(_doSave_) then CloseAction:=caNone;
      if mr=mrCancel then CloseAction:=caNone;
    end;
    if CloseAction<>caNone then SettingsForm.setFileContents(InputEdit0.lines);
  end;

PROCEDURE TMnhForm.FormDestroy(Sender: TObject);
  begin
    guiAdapters.removeOutAdapter(@guiOutAdapter);
    inputHighlighter.destroy;
    outputHighlighter.destroy;
    ad_killEvaluationLoopSoftly;
    wordsInEditor.destroy;
  end;

PROCEDURE TMnhForm.FormResize(Sender: TObject);
  VAR formPosition:T_formPosition;
  begin
    if settingsReady then begin
      formPosition.top   :=top;
      formPosition.left  :=left;
      formPosition.width :=width;
      formPosition.height:=height;
      formPosition.isFullscreen:=(WindowState=wsMaximized);
      SettingsForm.mainFormPosition:=formPosition;
    end else plotForm.pullPlotSettingsToGui();
    if PopupNotifier1.Visible then positionHelpNotifier;
  end;

PROCEDURE TMnhForm.FormShow(Sender: TObject);
  VAR newCaret:TPoint;
  begin
    if not(settingsReady) then begin
      processSettings;
      InputEdit0.SetFocus;
      if (locationToOpenOnFormStartup.fileName<>'') and
         (locationToOpenOnFormStartup.fileName<>C_nilTokenLocation.fileName) and
         fileExists(locationToOpenOnFormStartup.fileName) then begin
        SettingsForm.setFileInEditor(locationToOpenOnFormStartup.fileName);
        ad_setFile(expandFileName(locationToOpenOnFormStartup.fileName),InputEdit0.lines);
        SettingsForm.setFileContents(InputEdit0.lines);
        newCaret.x:=locationToOpenOnFormStartup.column;
        if newCaret.x<=0 then newCaret.x:=1;
        newCaret.y:=locationToOpenOnFormStartup.line;
        if newCaret.y<=0 then newCaret.y:=1;
        InputEdit0.CaretXY:=newCaret;
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
        ad_evaluate(InputEdit0.lines);
      end else evaluation.required:=true;
    end;
  end;

PROCEDURE TMnhForm.InputEditKeyDown(Sender: TObject; VAR key: word;
  Shift: TShiftState);
  begin
    if (key=13) and ((ssCtrl in Shift) or (ssAlt in Shift)) then inputEditReposition(InputEdit0.CaretXY,ssCtrl in Shift);
  end;

PROCEDURE TMnhForm.InputEditMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    inputEditReposition(InputEdit0.PixelsToRowColumn(point),ssCtrl in Shift);
  end;

PROCEDURE TMnhForm.InputEditProcessUserCommand(Sender: TObject;
  VAR Command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
  VAR i:longint;
      commented:boolean=true;
  begin
    if (Command=ecUserDefinedFirst) and (InputEdit0.BlockBegin.y>=1) then begin
      for i:=InputEdit0.BlockBegin.y-1 to InputEdit0.BlockEnd.y-1 do
        commented:=commented and (copy(trim(InputEdit0.lines[i]),1,2)='//');
      if commented
      then for i:=InputEdit0.BlockBegin.y-1 to InputEdit0.BlockEnd.y-1 do
        InputEdit0.lines[i]:=replaceOne(InputEdit0.lines[i],'//','')
      else for i:=InputEdit0.BlockBegin.y-1 to InputEdit0.BlockEnd.y-1 do
      InputEdit0.lines[i]:='//'+InputEdit0.lines[i];
    end;
  end;

PROCEDURE TMnhForm.MenuItem4Click(Sender: TObject);
  begin
    askForm.initWithQuestion('Please give command line parameters');
    if askForm.ShowModal=mrOk then begin
      doStartEvaluation;
      ad_callMain(InputEdit0.lines,askForm.getLastAnswerReleasing);
    end else askForm.getLastAnswerReleasing;
  end;

PROCEDURE TMnhForm.miClearClick(Sender: TObject);
  VAR mr:integer;
  begin
    if (ad_needSave(InputEdit0.lines)) then begin
      mr:=closeDialogForm.showOnLoad;
      if mr=mrOk then if not(_doSave_) then exit;
      if mr=mrCancel then exit;
    end;
    ad_clearFile;
    InputEdit0.ClearAll;
    if SettingsForm.setFileInEditor('') then processFileHistory;
  end;

PROCEDURE TMnhForm.miDebugClick(Sender: TObject);
  begin
    if miDebug.Checked
    then miDebug.Checked:=false
    else begin
      miDebug.Checked:=true;
      miEvalModeDirect.Checked:=true;
      miEvalModeDirectOnKeypress.Checked:=false;
      SettingsForm.wantInstantEvaluation:=false;
      if ad_evaluationRunning then begin
        DebugForm.debugEdit.ClearAll;
        InputEdit0.readonly:=true;
        stepper.doStep;
        DebugForm.Show;
      end;
    end;
  end;

PROCEDURE TMnhForm.miDebugFromClick(Sender: TObject);
  VAR lineIdx:longint;
  begin
    if ad_evaluationRunning then exit;

    askForm.initWithFileLines(1,InputEdit0.lines.count);
    if askForm.ShowModal=mrOk then begin
      lineIdx:=strToIntDef(askForm.getLastAnswerReleasing,-1);
      if lineIdx<0 then exit;

      miDebug.Checked:=true;
      miEvalModeDirect.Checked:=true;
      miEvalModeDirectOnKeypress.Checked:=false;
      SettingsForm.wantInstantEvaluation:=false;

      doStartEvaluation;
      stepper.setBreakpoint(environment.mainPackageProvider^.fileName,lineIdx);
      ad_evaluate(InputEdit0.lines);
    end else askForm.getLastAnswerReleasing;
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
      ad_evaluate(InputEdit0.lines);
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
  if not(miHelp.Checked) then PopupNotifier1.Visible:=false
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

FUNCTION TMnhForm._doSaveAs_: boolean;
  begin
    if SaveDialog.execute then begin
      ad_saveFile(expandFileName(SaveDialog.fileName),InputEdit0.lines);
      if SettingsForm.setFileInEditor(expandFileName(SaveDialog.fileName)) then processFileHistory;
      result:=true;
    end else result:=false;
  end;

FUNCTION TMnhForm._doSave_: boolean;
  begin
    if ad_currentFile='' then result:=_doSaveAs_
    else begin
      ad_saveFile(ad_currentFile,InputEdit0.lines);
      if SettingsForm.setFileInEditor(ad_currentFile) then processFileHistory;
      result:=true;
    end;
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
      if (ad_needSave(InputEdit0.lines)) then begin
        mr:=closeDialogForm.showOnLoad;
        if mr=mrOk then if not(_doSave_) then exit;
        if mr=mrCancel then exit;
      end;
      ad_setFile(expandFileName(OpenDialog.fileName),InputEdit0.lines);
      if SettingsForm.setFileInEditor(expandFileName(OpenDialog.fileName)) then processFileHistory;
    end;
  end;

PROCEDURE TMnhForm.miSaveAsClick(Sender: TObject);
  begin
    _doSaveAs_;
  end;

PROCEDURE TMnhForm.miSaveClick(Sender: TObject);
  begin
    _doSave_;
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
    if forceInputEditFocusOnOutputEditMouseUp then ActiveControl:=InputEdit0;
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

PROCEDURE TMnhForm.PopupNotifier1Close(Sender: TObject;
  VAR CloseAction: TCloseAction);
  begin
    miHelp.Checked:=false;
  end;

PROCEDURE TMnhForm.Splitter1Moved(Sender: TObject);
  begin
    if PopupNotifier1.Visible then positionHelpNotifier;
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
    caret:=InputEdit0.CaretXY;
    for i:=0 to InputEdit0.lines.count-1 do
      if i+1=caret.y then collectIdentifiers(InputEdit0.lines[i],wordsInEditor,caret.x)
                     else collectIdentifiers(InputEdit0.lines[i],wordsInEditor,-1);
    wordsInEditor.addAll(completionList.elementArray);
    wordsInEditor.unique;
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
  begin
    isEvaluationRunning:=ad_evaluationRunning;
    //fast ones:================================================================
    //Show ask form?
    if askForm.displayPending then askForm.ShowModal;
    //Form caption:-------------------------------------------------------------
    aid:='MNH5 '+ad_currentFile;
    if aid<>Caption then Caption:=aid;
    //-------------------------------------------------------------:Form caption
    //progress time:------------------------------------------------------------
    aid:=C_tabChar+intToStr(InputEdit0.CaretY)+','+intToStr(InputEdit0.CaretX);
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
    if not(isEvaluationRunning) then InputEdit0.readonly:=false;
    flushPerformed:=guiOutAdapter.flushToGui(OutputEdit);
    autosizingDone:=autosizeBlocks(isEvaluationRunning);

    if guiAdapters.hasMessageOfType[mt_plotCreatedWithDeferredDisplay] and
       not(ad_evaluationRunning) then plotForm.doPlot();

    if isEvaluationRunning then evaluation.deferredUntil:=now+0.1*ONE_SECOND else
    if evaluation.required and not(ad_evaluationRunning) and (now>evaluation.deferredUntil) then begin
      doStartEvaluation;
      ad_evaluate(InputEdit0.lines);
      UpdateTimeTimer.Interval:=MIN_INTERVALL;
    end;
    SettingsForm.saveSettingsMaybe;

    if not(flushPerformed) and not(autosizingDone) then begin
      UpdateTimeTimer.Interval:=UpdateTimeTimer.Interval+10;
      if UpdateTimeTimer.Interval>MAX_INTERVALL then UpdateTimeTimer.Interval:=MAX_INTERVALL;
    end else UpdateTimeTimer.Interval:=MIN_INTERVALL;
    //================================================================:slow ones
    //if (now>doNotCheckFileBefore) and ad_needReload then begin
    //  writeln(StdErr,'Opening modal dialog.');
    //  UpdateTimeTimer.Enabled:=false;
    //  i:=closeDialogForm.showOnOutOfSync;
    //  writeln(StdErr, 'Modal result received...');
    //  if i=mrOk     then begin ad_doReload(MnhForm.InputEdit.lines);                doNotCheckFileBefore:=now+ONE_SECOND; end;
    //  if i=mrClose  then begin ad_saveFile(ad_currentFile,MnhForm.InputEdit.lines); doNotCheckFileBefore:=now+ONE_SECOND; end;
    //  if i=mrCancel then begin                                                      doNotCheckFileBefore:=now+ONE_MINUTE; end;
    //  writeln(StdErr, 'Modal result evaluation done.');
    //  UpdateTimeTimer.Enabled:=true;
    //end;
  end;

PROCEDURE TMnhForm.processSettings;
  VAR formPosition:T_formPosition;
  begin
    if not(settingsReady) then begin
      InputEdit0.BeginUpdate();
      SettingsForm.getFileContents(InputEdit0.lines);
      InputEdit0.EndUpdate();

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

      if ad_currentFile<>SettingsForm.getFileInEditor then begin
        if SettingsForm.getFileInEditor=''
        then ad_clearFile
        else ad_setFile(SettingsForm.getFileInEditor,InputEdit0.lines);
      end;
      processFileHistory;
      settingsReady:=true;
    end;

    InputEdit0.Font.name:=SettingsForm.getEditorFontName;
    InputEdit0.Font.size:=SettingsForm.fontSize;
    if SettingsForm.AntialiasCheckbox.Checked
    then InputEdit0.Font.Quality:=fqCleartypeNatural
    else InputEdit0.Font.Quality:=fqNonAntialiased;

    OutputEdit.Font         :=InputEdit0.Font;
    DebugForm.debugEdit.Font:=InputEdit0.Font;
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
      historyMenuItem(i).Visible:=false;
    end else begin
      historyMenuItem(i).Enabled:=true;
      historyMenuItem(i).Visible:=true;
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

