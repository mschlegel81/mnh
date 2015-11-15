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
  myGenerics,mnh_fileWrappers,mySys,mnh_html;

CONST
  STATE_SAVE_INTERVAL=ONE_MINUTE;

TYPE

  { TMnhForm }

  TMnhForm = class(TForm)
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
    miAntiAliasingOff: TMenuItem;
    miAntiAliasing2: TMenuItem;
    miAntiAliasing3: TMenuItem;
    miAntiAliasing4: TMenuItem;
    miAntiAliasing5: TMenuItem;
    miAutoReset: TMenuItem;
    Panel1: TPanel;
    submenuPlotGrid: TMenuItem;
    MenuItem17: TMenuItem;
    miXTics: TMenuItem;
    miXGrid: TMenuItem;
    miXFinerGrid: TMenuItem;
    MenuItem21: TMenuItem;
    miYTics: TMenuItem;
    miYGrid: TMenuItem;
    miYFinerGrid: TMenuItem;
    submenuPlotScaling: TMenuItem;
    miPreserveAspect: TMenuItem;
    miAutoscaleX: TMenuItem;
    miAutoscaleY: TMenuItem;
    miLogscaleX: TMenuItem;
    miLogscaleY: TMenuItem;
    submenuPlotOptions: TMenuItem;
    miExportPlot: TMenuItem;
    MenuItem9: TMenuItem;
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
    submenuEditorAppearance: TMenuItem;
    miExpressionEcho: TMenuItem;
    miExpressionResult: TMenuItem;
    miDeclarationEcho: TMenuItem;
    miDecFontSize: TMenuItem;
    miIncFontSize: TMenuItem;
    mi_settings: TMenuItem;
    PageControl: TPageControl;
    plotImage: TImage;
    PopupNotifier1: TPopupNotifier;
    SaveDialog: TSaveDialog;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    InputEdit: TSynEdit;
    OutputEdit: TSynEdit;
    SynCompletion: TSynCompletion;
    EditorTabSheet: TTabSheet;
    PlotTabSheet: TTabSheet;
    autosizeToggleBox: TToggleBox;
    UpdateTimeTimer: TTimer;
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormKeyPress(Sender: TObject; VAR key: char);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE InputEditChange(Sender: TObject);
    PROCEDURE InputEditKeyDown(Sender: TObject; VAR key: word;
      Shift: TShiftState);
    PROCEDURE InputEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE MenuItem4Click(Sender: TObject);
    PROCEDURE miClearClick(Sender: TObject);
    PROCEDURE miDebugClick(Sender: TObject);
    PROCEDURE miDebugFromClick(Sender: TObject);
    PROCEDURE miDecFontSizeClick(Sender: TObject);
    PROCEDURE miDeclarationEchoClick(Sender: TObject);
    PROCEDURE miEvalModeDirectClick(Sender: TObject);
    PROCEDURE miEvalModeDirectOnKeypressClick(Sender: TObject);
    PROCEDURE miEvaluateNowClick(Sender: TObject);
    PROCEDURE miExportPlotClick(Sender: TObject);
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
    procedure miMinErrorlevel1Click(Sender: TObject);
    procedure miMinErrorlevel2Click(Sender: TObject);
    procedure miMinErrorlevel3Click(Sender: TObject);
    procedure miMinErrorlevel4Click(Sender: TObject);
    procedure miMinErrorlevel5Click(Sender: TObject);
    PROCEDURE miOpenClick(Sender: TObject);
    PROCEDURE miSaveAsClick(Sender: TObject);
    PROCEDURE miSaveClick(Sender: TObject);
    PROCEDURE miStartAnotherInstanceClick(Sender: TObject);
    procedure miTimingInfoClick(Sender: TObject);
    PROCEDURE mi_settingsClick(Sender: TObject);
    PROCEDURE miAntialiasingOffClick(Sender: TObject);
    PROCEDURE miAutoResetClick(Sender: TObject);
    PROCEDURE miAutoscaleXClick(Sender: TObject);
    PROCEDURE miAutoscaleYClick(Sender: TObject);
    PROCEDURE miLogscaleXClick(Sender: TObject);
    PROCEDURE miLogscaleYClick(Sender: TObject);
    PROCEDURE miPreserveAspectClick(Sender: TObject);
    PROCEDURE miXFinerGridClick(Sender: TObject);
    PROCEDURE miXGridClick(Sender: TObject);
    PROCEDURE miXTicsClick(Sender: TObject);
    PROCEDURE miYFinerGridClick(Sender: TObject);
    PROCEDURE miYGridClick(Sender: TObject);
    PROCEDURE miYTicsClick(Sender: TObject);
    PROCEDURE OutputEditKeyDown(Sender: TObject; VAR key: word;
      Shift: TShiftState);
    PROCEDURE OutputEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE OutputEditMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE PageControlChange(Sender: TObject);
    PROCEDURE plotImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE plotImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: integer);
    PROCEDURE plotImageMouseUp(Sender: TObject; Button: TMouseButton;
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
    subTimerCounter:longint;
    underCursor:T_tokenInfo;
    settings:record
      ready:boolean;
      storedAt:double;
    end;
    evaluation:record
      required:boolean;
      start:double;
      deferredUntil:double;
    end;
    forceInputEditFocusOnOutputEditMouseUp:boolean;

    doNotMarkWordBefore:double;
    doNotCheckFileBefore:double;

    PROCEDURE processSettings;
    PROCEDURE processFileHistory;
    FUNCTION autosizeBlocks(CONST forceOutputFocus:boolean):boolean;
    PROCEDURE positionHelpNotifier;
    PROCEDURE setUnderCursor(CONST wordText:ansistring);

    PROCEDURE doPlot();
    PROCEDURE pullPlotSettingsToGui();
    PROCEDURE pushSettingsToPlotContainer(CONST plotImmediately:boolean);
    PROCEDURE doConditionalPlotReset;
    PROCEDURE openFromHistory(CONST historyIdx:byte);
    PROCEDURE doStartEvaluation;
    PROCEDURE inputEditReposition(CONST caret:TPoint; CONST doJump:boolean);
    PROCEDURE outputEditReposition(CONST caret:TPoint; CONST doJump:boolean);
    { private declarations }
  public
    { public declarations }
  end;

  { T_guiOutAdapter }

  T_guiOutAdapter=object(T_collectingOutAdapter)
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION flushToGui(VAR syn:TSynEdit):boolean;
    PROCEDURE flushClear;
  end;

VAR
  MnhForm: TMnhForm;
PROCEDURE lateInitialization;
IMPLEMENTATION
VAR guiOutAdapter: T_guiOutAdapter;
    guiAdapters: T_adapters;
    plotSubsystem:record
      rendering:boolean;
      mouseUpTriggersPlot:boolean;
      renderNotBefore:double;
      state:(pss_neutral, pss_plotAfterCalculation, pss_plotOnShow);
      lastMouseX,lastMouseY:longint;
    end;

{$R *.lfm}

{ T_guiOutAdapter }

CONSTRUCTOR T_guiOutAdapter.create;
  begin
    inherited create;
    outputBehaviour.doShowTimingInfo:=false;
  end;

DESTRUCTOR T_guiOutAdapter.destroy;
  begin
    inherited destroy;
  end;

FUNCTION T_guiOutAdapter.flushToGui(VAR syn: TSynEdit): boolean;
  VAR i,j:longint;
  begin
    system.enterCriticalSection(cs);
    result:=length(storedMessages)>0;
    for i:=0 to length(storedMessages)-1 do with storedMessages[i] do begin
      case messageType of
        mt_clearConsole: syn.lines.clear;
        mt_printline:
          for j:=0 to length(multiMessage)-1 do  syn.lines.append(multiMessage[j]);
        mt_debug_step: begin
          DebugForm.rollingAppend(simpleMessage);
          MnhForm.inputHighlighter.setMarkedToken(location.line-1,location.column-1);
          MnhForm.InputEdit.Repaint;
        end;
        mt_endOfEvaluation: begin
          DebugForm.rollingAppend('Evaluation finished');
          MnhForm.InputEdit.readonly:=false;
          MnhForm.inputHighlighter.setMarkedToken(-1,-1);
        end;
        mt_reloadRequired: ad_doReload(MnhForm.InputEdit.lines);
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
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_guiOutAdapter.flushClear;
  begin
    system.enterCriticalSection(cs);
    clearMessages;
    clearConsole;
    system.leaveCriticalSection(cs);
  end;

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
      scrollbarHeight     :=InputEdit.height-InputEdit.ClientHeight;
      idealInputHeight    :=scrollbarHeight+ InputEdit .Font.GetTextHeight(SAMPLE_TEXT)* InputEdit .lines.count;
      idealOutputHeight   :=scrollbarHeight+ OutputEdit.Font.GetTextHeight(SAMPLE_TEXT)*(OutputEdit.lines.count+1);
      availableTotalHeight:=InputEdit.height+OutputEdit.height;
      inputFocus:=not(forceOutputFocus or OutputEdit.Focused);

      //Are both editors large enough?
      if (InputEdit.height>=idealInputHeight) and (OutputEdit.height>=idealOutputHeight) then exit;
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
      if idealInputHeight<>InputEdit.height then begin
        InputEdit.height:=idealInputHeight;
        if PopupNotifier1.Visible then positionHelpNotifier;
        autosizeToggleBox.top:=OutputEdit.top;
        result:=true;
      end;
    end;
  end;

PROCEDURE TMnhForm.positionHelpNotifier;
  begin
    PopupNotifier1.ShowAtPos(left+InputEdit.width-PopupNotifier1.vNotifierForm.width,
                             ClientToScreen(point(left,OutputEdit.top)).y);
    InputEdit.SetFocus;
  end;

PROCEDURE TMnhForm.setUnderCursor(CONST wordText: ansistring);
  begin
    if not(isIdentifier(wordText,true)) then exit;
    if (inputHighlighter.setMarkedWord(wordText) and outputHighlighter.setMarkedWord(wordText)) then InputEdit.Repaint;
    if miHelp.Checked then ad_explainIdentifier(wordText,underCursor);
    if (underCursor.tokenText<>'') and (underCursor.tokenText<>PopupNotifier1.title) then begin
      PopupNotifier1.title:=underCursor.tokenText;
      PopupNotifier1.text:=underCursor.tokenExplanation;
      if miHelp.Checked and not(PopupNotifier1.Visible) then positionHelpNotifier;
    end;
  end;

PROCEDURE TMnhForm.doPlot;
  VAR factor:longint;
  begin
    plotSubsystem.state:=pss_neutral;
    PageControl.ActivePageIndex:=1;
    plotSubsystem.rendering:=true;
    if      miAntiAliasing5.Checked then factor:=5
    else if miAntiAliasing4.Checked then factor:=4
    else if miAntiAliasing3.Checked then factor:=3
    else if miAntiAliasing2.Checked then factor:=2
    else                                 factor:=1;
    activePlot.setScreenSize(PlotTabSheet.width,PlotTabSheet.height);
    activePlot.renderPlot(plotImage,factor);
    plotSubsystem.rendering:=false;
  end;

PROCEDURE TMnhForm.pullPlotSettingsToGui;
  begin
    miXTics.Checked         :=(activePlot.axisStyle['x'] and C_tics)=C_tics;
    miXGrid.Checked         :=(activePlot.axisStyle['x'] and C_grid)=C_grid;
    miXFinerGrid.Checked    :=(activePlot.axisStyle['x'] and C_finerGrid)=C_finerGrid;
    miYTics.Checked         :=(activePlot.axisStyle['y'] and C_tics)=C_tics;
    miYGrid.Checked         :=(activePlot.axisStyle['y'] and C_grid)=C_grid;
    miYFinerGrid.Checked    :=(activePlot.axisStyle['y'] and C_finerGrid)=C_finerGrid;
    miPreserveAspect.Checked:=activePlot.preserveAspect;
    miAutoscaleX.Checked    :=activePlot.autoscale['x'];
    miAutoscaleY.Checked    :=activePlot.autoscale['y'];
    miLogscaleX.Checked     :=activePlot.logscale['x'];
    miLogscaleY.Checked     :=activePlot.logscale['y'];
  end;

PROCEDURE TMnhForm.pushSettingsToPlotContainer(CONST plotImmediately: boolean);
  VAR aidX,aidY:longint;
  begin
    aidX:=0;
    if miXTics.Checked      then aidX:=C_tics;
    if miXGrid.Checked      then aidX:=aidX or C_grid;
    if miXFinerGrid.Checked then aidX:=aidX or C_finerGrid;
    aidY:=0;
    if miYTics.Checked      then aidY:=C_tics;;
    if miYGrid.Checked      then aidY:=aidY or C_grid;
    if miYFinerGrid.Checked then aidY:=aidY or C_finerGrid;
    activePlot.setAxisStyle(aidX,aidY);

    activePlot.setPreserveAspect(miPreserveAspect.Checked);
    activePlot.setLogscale(miLogscaleX.Checked,miLogscaleY.Checked);
    activePlot.setAutoscale(miAutoscaleX.Checked,miAutoscaleY.Checked);
    pullPlotSettingsToGui();
    if plotImmediately then begin
      if ad_evaluationRunning or plotSubsystem.rendering or (PageControl.ActivePageIndex<>1)
         then plotSubsystem.state:=pss_plotOnShow
         else doPlot();
    end else plotSubsystem.state:=pss_plotAfterCalculation;
  end;

PROCEDURE TMnhForm.doConditionalPlotReset;
  begin
    if miAutoReset.Checked then begin
      activePlot.setDefaults;
      pullPlotSettingsToGui();
    end;
  end;

PROCEDURE TMnhForm.openFromHistory(CONST historyIdx: byte);
  VAR mr:integer;
  begin
    if fileExists(SettingsForm.fileHistory[historyIdx]) then begin
      if (ad_currentFile<>'') and (ad_needSave(InputEdit.lines)) then begin
        mr:=closeDialogForm.showOnLoad;
        if mr=mrOk then MnhForm.InputEdit.lines.saveToFile(ad_currentFile);
        if mr=mrCancel then exit;
      end;
      ad_setFile(SettingsForm.fileHistory[historyIdx],InputEdit.lines);
      if SettingsForm.setFileInEditor(SettingsForm.fileHistory[historyIdx]) then processFileHistory;
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
      InputEdit.readonly:=true;
      stepper.doStep;
      DebugForm.Show;
    end else stepper.setFreeRun;
  end;

PROCEDURE TMnhForm.inputEditReposition(CONST caret: TPoint; CONST doJump:boolean);
  VAR wordUnderCursor:string;
      newCaret:TPoint;
  begin
    wordUnderCursor:=InputEdit.GetWordAtRowCol(caret);
    setUnderCursor(wordUnderCursor);
    if not(doJump) then exit;
    if not(miHelp.Checked) then ad_explainIdentifier(wordUnderCursor,underCursor);
    if (underCursor.tokenText<>wordUnderCursor) or
       (underCursor.location.column<=0) then exit;
    newCaret.x:=underCursor.location.column;
    newCaret.y:=underCursor.location.line;
    InputEdit.CaretXY:=newCaret;
  end;

PROCEDURE TMnhForm.outputEditReposition(CONST caret: TPoint; CONST doJump:boolean);
  VAR loc:T_tokenLocation;
      newCaret:TPoint;
  begin
    forceInputEditFocusOnOutputEditMouseUp:=false;
    setUnderCursor(OutputEdit.GetWordAtRowCol(caret));
    loc:=guessLocationFromString(OutputEdit.lines[caret.y-1]);
    if (loc.column>0) and (loc.fileName=mainPackageProvider.getPath)
    then begin
      inputHighlighter.setMarkedToken(loc.line-1,loc.column-1);
      if doJump then begin
        newCaret.x:=loc.column;
        newCaret.y:=loc.line;
        InputEdit.CaretXY:=newCaret;
        forceInputEditFocusOnOutputEditMouseUp:=true;
        exit;
      end;
    end else inputHighlighter.setMarkedToken(-1,-1);
    InputEdit.Repaint;
  end;

{ TMnhForm }
PROCEDURE TMnhForm.FormCreate(Sender: TObject);
  VAR i:longint;
  begin
    forceInputEditFocusOnOutputEditMouseUp:=false;
    subTimerCounter:=0;
    with settings do begin
      ready:=false;
      storedAt:=now;
    end;
    with evaluation do begin
      required:=false;
      deferredUntil:=now;
      start:=now;
    end;

    doNotMarkWordBefore:=now;
    doNotCheckFileBefore:=now+10*ONE_SECOND;
    OpenDialog.fileName:=paramStr(0);
    SaveDialog.fileName:=paramStr(0);
    inputHighlighter:=TSynMnhSyn.create(nil,false);
    outputHighlighter:=TSynMnhSyn.create(nil,true);
    InputEdit.highlighter:=inputHighlighter;
    OutputEdit.highlighter:=outputHighlighter;
    OutputEdit.ClearAll;
    StatusBar.SimpleText:=
      'compiled on: '+{$I %DATE%}+
      ' at: '+{$I %TIME%}+
      ' with FPC'+{$I %FPCVERSION%}+
      ' for '+{$I %FPCTARGET%};
    for i:=0 to length(LOGO)-1 do OutputEdit.lines.append(LOGO[i]);
    OutputEdit.lines.append('       compiled on: '+{$I %DATE%});
    OutputEdit.lines.append('       at: '+{$I %TIME%});
    OutputEdit.lines.append('       with FPC'+{$I %FPCVERSION%});
    OutputEdit.lines.append('       for '+{$I %FPCTARGET%});
    {$ifdef debugMode}
    guiAdapters.addConsoleOutAdapter;
    {$endif}
  end;

PROCEDURE TMnhForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  VAR mr:integer;
  begin
    if (ad_currentFile<>'') and (ad_needSave(InputEdit.lines)) then begin
      mr:=closeDialogForm.showOnQuit;
      if mr=mrOk then MnhForm.InputEdit.lines.saveToFile(ad_currentFile);
      if mr=mrCancel then CloseAction:=caNone;
    end;
    SettingsForm.mainForm.isFullscreen:=(WindowState=wsMaximized);
    if CloseAction<>caNone then SettingsForm.setFileContents(InputEdit.lines);
  end;

PROCEDURE TMnhForm.FormDestroy(Sender: TObject);
  begin
    guiAdapters.removeOutAdapter(@guiOutAdapter);
    inputHighlighter.destroy;
    outputHighlighter.destroy;
    ad_killEvaluationLoopSoftly;
  end;

PROCEDURE TMnhForm.FormKeyPress(Sender: TObject; VAR key: char);
begin
  if (PageControl.ActivePageIndex=1) and (key in ['+','-']) then begin
    if key='+' then activePlot.zoomOnPoint(plotSubsystem.lastMouseX,plotSubsystem.lastMouseY,  0.9,plotImage)
               else activePlot.zoomOnPoint(plotSubsystem.lastMouseX,plotSubsystem.lastMouseY,1/0.9,plotImage);
    pullPlotSettingsToGui();
    plotSubsystem.state:=pss_plotOnShow;
    plotSubsystem.renderNotBefore:=now+(1/(24*60*60));
  end;
end;

PROCEDURE TMnhForm.FormResize(Sender: TObject);
  begin
    if settings.ready then begin
      SettingsForm.mainForm.top   :=top;
      SettingsForm.mainForm.left  :=left;
      SettingsForm.mainForm.width :=width;
      SettingsForm.mainForm.height:=height;
      SettingsForm.mainForm.isFullscreen:=(WindowState=wsMaximized);
      if ad_evaluationRunning or plotSubsystem.rendering or (PageControl.ActivePageIndex<>1)
        then plotSubsystem.state:=pss_plotOnShow
        else doPlot();
    end else pullPlotSettingsToGui();
    if PopupNotifier1.Visible then positionHelpNotifier;
  end;

PROCEDURE TMnhForm.FormShow(Sender: TObject);
  begin
    if not(settings.ready) then begin
      processSettings;
      InputEdit.SetFocus;
    end;
    KeyPreview:=true;
    UpdateTimeTimer.Enabled:=true;
  end;

PROCEDURE TMnhForm.InputEditChange(Sender: TObject);
  begin
    if (miEvalModeDirectOnKeypress.Checked) and not(SynCompletion.IsActive) then begin
      if now>evaluation.deferredUntil then begin
        doStartEvaluation;
        ad_evaluate(InputEdit.lines);
      end else evaluation.required:=true;
    end;
  end;

PROCEDURE TMnhForm.InputEditKeyDown(Sender: TObject; VAR key: word;
  Shift: TShiftState);
  begin
    if (key>=37) and (key<=40) or ((key=13) and (ssCtrl in Shift)) then inputEditReposition(InputEdit.CaretXY,(key=13) and (ssCtrl in Shift));
  end;

PROCEDURE TMnhForm.InputEditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    inputEditReposition(InputEdit.PixelsToRowColumn(point),ssCtrl in Shift);
  end;

PROCEDURE TMnhForm.MenuItem4Click(Sender: TObject);
  begin
    askForm.initWithQuestion('Please give command line parameters');
    if askForm.ShowModal=mrOk then begin
      doStartEvaluation;
      ad_callMain(InputEdit.lines,askForm.getLastAnswerReleasing);
    end else askForm.getLastAnswerReleasing;
  end;

PROCEDURE TMnhForm.miClearClick(Sender: TObject);
  VAR mr:integer;
  begin
    if (ad_currentFile<>'') and (ad_needSave(InputEdit.lines)) then begin
      mr:=closeDialogForm.showOnLoad;
      if mr=mrOk then MnhForm.InputEdit.lines.saveToFile(ad_currentFile);
      if mr=mrCancel then exit;
    end;
    ad_clearFile;
    InputEdit.ClearAll;
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
      SettingsForm.instantEvaluation:=false;
      if ad_evaluationRunning then begin
        DebugForm.debugEdit.ClearAll;
        InputEdit.readonly:=true;
        stepper.doStep;
        DebugForm.Show;
      end;
    end;
  end;

PROCEDURE TMnhForm.miDebugFromClick(Sender: TObject);
  VAR lineIdx:longint;
  begin
    if ad_evaluationRunning then exit;

    askForm.initWithFileLines(1,InputEdit.lines.count);
    if askForm.ShowModal=mrOk then begin
      lineIdx:=strToIntDef(askForm.getLastAnswerReleasing,-1);
      if lineIdx<0 then exit;

      miDebug.Checked:=true;
      miEvalModeDirect.Checked:=true;
      miEvalModeDirectOnKeypress.Checked:=false;
      SettingsForm.instantEvaluation:=false;

      doStartEvaluation;
      stepper.setBreakpoint(mainPackageProvider.fileName,lineIdx);
      ad_evaluate(InputEdit.lines);
    end else askForm.getLastAnswerReleasing;
  end;

PROCEDURE TMnhForm.miDecFontSizeClick(Sender: TObject);
  begin
    if settings.ready then begin
      SettingsForm.fontSize:=SettingsForm.fontSize-1;
      processSettings;
    end;
  end;

PROCEDURE TMnhForm.miDeclarationEchoClick(Sender: TObject);
  begin
    if settings.ready then begin
      miDeclarationEcho.Checked:=not(miDeclarationEcho.Checked);
      with SettingsForm.outputBehaviour do doEchoDeclaration:=miDeclarationEcho.Checked;
      guiAdapters.outputBehaviour:=SettingsForm.outputBehaviour;
    end;
  end;

PROCEDURE TMnhForm.miEvalModeDirectClick(Sender: TObject);
  begin
    if miEvalModeDirect.Checked then exit;
    miEvalModeDirect.Checked:=true;
    miEvalModeDirectOnKeypress.Checked:=false;
    SettingsForm.instantEvaluation:=false;
  end;

PROCEDURE TMnhForm.miEvalModeDirectOnKeypressClick(Sender: TObject);
  begin
    if miEvalModeDirectOnKeypress.Checked then exit;
    miDebug.Checked:=false;
    miEvalModeDirect.Checked:=false;
    miEvalModeDirectOnKeypress.Checked:=true;
    SettingsForm.instantEvaluation:=true;
  end;

PROCEDURE TMnhForm.miEvaluateNowClick(Sender: TObject);
  begin
    if now>evaluation.deferredUntil then begin
      doStartEvaluation;
      ad_evaluate(InputEdit.lines);
    end else evaluation.required:=true;
  end;

PROCEDURE TMnhForm.miExportPlotClick(Sender: TObject);
  VAR storeImage:TImage;
      rect:TRect;
  begin
    SaveDialog.Filter:='Portable network graphics (PNG)|*.png';
    if SaveDialog.execute then begin
      storeImage:=TImage.create(self);
      storeImage.SetInitialBounds(0,0,PlotTabSheet.width,PlotTabSheet.height);
      rect.top:=0;
      rect.left:=0;
      rect.Right:=PlotTabSheet.width;
      rect.Bottom:=PlotTabSheet.height;
      storeImage.Canvas.CopyRect(rect,plotImage.Canvas,rect);
      SaveDialog.fileName:=ChangeFileExt(SaveDialog.fileName,'.png');
      storeImage.Picture.PNG.saveToFile(SaveDialog.fileName);
      storeImage.free;
    end;
  end;

PROCEDURE TMnhForm.miExpressionEchoClick(Sender: TObject);
  begin
    if settings.ready then begin
      miExpressionEcho.Checked:=not(miExpressionEcho.Checked);
      with SettingsForm.outputBehaviour do doEchoInput:=miExpressionEcho.Checked;
      guiAdapters.outputBehaviour:=SettingsForm.outputBehaviour;
    end;
  end;

PROCEDURE TMnhForm.miExpressionResultClick(Sender: TObject);
  begin
    if settings.ready then begin
      miExpressionResult.Checked:=not(miExpressionResult.Checked);
      with SettingsForm.outputBehaviour do doShowExpressionOut:=miExpressionResult.Checked;
      guiAdapters.outputBehaviour:=SettingsForm.outputBehaviour;
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
                         else if underCursor.tokenText<>'' then positionHelpNotifier;
end;

PROCEDURE TMnhForm.miHelpExternallyClick(Sender: TObject);
  begin
    findAndDocumentAllPackages;
    OpenURL('file:///'+replaceAll(expandFileName(htmlRoot+'\index.html'),'\','/'));
  end;

PROCEDURE TMnhForm.miIncFontSizeClick(Sender: TObject);
  begin
    if settings.ready then begin
      SettingsForm.fontSize:=SettingsForm.fontSize+1;
      processSettings;
    end;
  end;

procedure TMnhForm.miMinErrorlevel1Click(Sender: TObject);
  begin
    if settings.ready then begin
      miMinErrorlevel1.Checked:=true;
      SettingsForm.outputBehaviour.minErrorLevel:=1;
      guiAdapters.outputBehaviour:=SettingsForm.outputBehaviour;
    end;
  end;

procedure TMnhForm.miMinErrorlevel2Click(Sender: TObject);
  begin
    if settings.ready then begin
      miMinErrorlevel2.Checked:=true;
      SettingsForm.outputBehaviour.minErrorLevel:=2;
      guiAdapters.outputBehaviour:=SettingsForm.outputBehaviour;
    end;
  end;

procedure TMnhForm.miMinErrorlevel3Click(Sender: TObject);
  begin
    if settings.ready then begin
      miMinErrorlevel3.Checked:=true;
      SettingsForm.outputBehaviour.minErrorLevel:=3;
      guiAdapters.outputBehaviour:=SettingsForm.outputBehaviour;
    end;
  end;

procedure TMnhForm.miMinErrorlevel4Click(Sender: TObject);
  begin
    if settings.ready then begin
      miMinErrorlevel4.Checked:=true;
      SettingsForm.outputBehaviour.minErrorLevel:=4;
      guiAdapters.outputBehaviour:=SettingsForm.outputBehaviour;
    end;
  end;

procedure TMnhForm.miMinErrorlevel5Click(Sender: TObject);
  begin
    if settings.ready then begin
      miMinErrorlevel5.Checked:=true;
      SettingsForm.outputBehaviour.minErrorLevel:=5;
      guiAdapters.outputBehaviour:=SettingsForm.outputBehaviour;
    end;
  end;

PROCEDURE TMnhForm.miOpenClick(Sender: TObject);
  VAR mr:integer;
  begin
    OpenDialog.title:='Open file';
    if OpenDialog.execute and fileExists(OpenDialog.fileName)
    then begin
      if (ad_currentFile<>'') and (ad_needSave(InputEdit.lines)) then begin
        mr:=closeDialogForm.showOnLoad;
        if mr=mrOk then MnhForm.InputEdit.lines.saveToFile(ad_currentFile);
        if mr=mrCancel then exit;
      end;
      ad_setFile(expandFileName(OpenDialog.fileName),InputEdit.lines);
      if SettingsForm.setFileInEditor(expandFileName(OpenDialog.fileName)) then processFileHistory;
    end;
  end;

PROCEDURE TMnhForm.miSaveAsClick(Sender: TObject);
  begin
    if SaveDialog.execute then begin
      ad_saveFile(expandFileName(SaveDialog.fileName),InputEdit.lines);
      if SettingsForm.setFileInEditor(expandFileName(SaveDialog.fileName)) then processFileHistory;
      SettingsForm.saveSettings;
    end;
  end;

PROCEDURE TMnhForm.miSaveClick(Sender: TObject);
  begin
    if ad_currentFile='' then miSaveAsClick(Sender)
    else begin
      ad_saveFile(ad_currentFile,InputEdit.lines);
      if SettingsForm.setFileInEditor(ad_currentFile) then processFileHistory;
      SettingsForm.saveSettings;
    end;
  end;

PROCEDURE TMnhForm.miStartAnotherInstanceClick(Sender: TObject);
  begin
    runCommandAsyncOrPipeless(paramStr(0),C_EMPTY_STRING_ARRAY,true);
  end;

procedure TMnhForm.miTimingInfoClick(Sender: TObject);
  begin
    if settings.ready then begin
      miTimingInfo.Checked:=not(miTimingInfo.Checked);
      with SettingsForm.outputBehaviour do doShowTimingInfo:=miTimingInfo.Checked;
      guiAdapters.outputBehaviour:=SettingsForm.outputBehaviour;
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
    if (key>=37) and (key<=40) or ((key=13) and (ssCtrl in Shift)) then outputEditReposition(OutputEdit.CaretXY,(key=13) and (ssCtrl in Shift));
    if forceInputEditFocusOnOutputEditMouseUp then ActiveControl:=InputEdit;
    forceInputEditFocusOnOutputEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.OutputEditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    outputEditReposition(OutputEdit.PixelsToRowColumn(point),ssCtrl in Shift);
  end;

PROCEDURE TMnhForm.OutputEditMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if forceInputEditFocusOnOutputEditMouseUp then ActiveControl:=InputEdit;
  forceInputEditFocusOnOutputEditMouseUp :=false;
end;


PROCEDURE TMnhForm.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePageIndex=0 then begin
    submenuEditorAppearance.Visible:=true;
    submenuPlotGrid.Visible:=false;
    submenuPlotOptions.Visible:=false;
    submenuPlotScaling.Visible:=false;
  end else begin
    submenuEditorAppearance.Visible:=false;
    submenuPlotGrid.Visible:=true;
    submenuPlotOptions.Visible:=true;
    submenuPlotScaling.Visible:=true;
    if plotSubsystem.state=pss_plotOnShow then doPlot();
    PopupNotifier1.Visible:=false;
  end;
end;

PROCEDURE TMnhForm.plotImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if ssLeft in Shift then begin
    plotSubsystem.lastMouseX:=x;
    plotSubsystem.lastMouseY:=y;
  end;
end;

PROCEDURE TMnhForm.plotImageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: integer);
  VAR p:T_point;
  begin
    p:=activePlot.screenToReal(x,y);
    StatusBar.SimpleText:='x='+FloatToStr(p[0])+'; y='+FloatToStr(p[1]);
    if ssLeft in Shift then with plotSubsystem do begin
      activePlot.panByPixels(lastMouseX-x,lastMouseY-y,plotImage);
      mouseUpTriggersPlot:=true;
    end;
    with plotSubsystem do begin
      lastMouseX:=x;
      lastMouseY:=y;
    end;
  end;

PROCEDURE TMnhForm.plotImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  begin
    with plotSubsystem do if mouseUpTriggersPlot then begin
      pullPlotSettingsToGui();
      lastMouseX:=x;
      lastMouseY:=y;
      doPlot();
    end;
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
  end;

PROCEDURE TMnhForm.SynCompletionCodeCompletion(VAR value: string;
  sourceValue: string; VAR SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
  begin
    if (pos('.',value)>0) then begin
      if pos(sourceValue,value)<>1 then begin
        value:=copy(value,pos('.',value)+1,length(value));
      end;
    end;
  end;

PROCEDURE TMnhForm.SynCompletionExecute(Sender: TObject);
  VAR i:longint;
      s:string;
  begin
    SynCompletion.ItemList.clear;
    s:=SynCompletion.CurrentString;
    for i:=0 to completionList.size-1 do
      if (s='') or (pos(s,completionList[i])=1) then SynCompletion.ItemList.add(completionList[i]);
  end;

PROCEDURE TMnhForm.SynCompletionSearchPosition(VAR APosition: integer);
  VAR i:longint;
      s:string;
  begin
    SynCompletion.ItemList.clear;
    s:=SynCompletion.CurrentString;
    for i:=0 to completionList.size-1 do
      if pos(s,completionList[i])=1 then SynCompletion.ItemList.add(completionList[i]);
    if SynCompletion.ItemList.count>0 then APosition:=0 else APosition:=-1;
  end;

PROCEDURE TMnhForm.UpdateTimeTimerTimer(Sender: TObject);
  CONST MIN_INTERVALL=20;
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
    aid:=C_tabChar+intToStr(InputEdit.CaretY)+','+intToStr(InputEdit.CaretX);
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
    inc(subTimerCounter);
    //slow ones:================================================================
    if subTimerCounter and 15=0 then begin
      if not(isEvaluationRunning) then InputEdit.readonly:=false;
      flushPerformed:=guiOutAdapter.flushToGui(OutputEdit);
      autosizingDone:=autosizeBlocks(isEvaluationRunning);

      if ((plotSubsystem.state=pss_plotAfterCalculation) or
          (plotSubsystem.state=pss_plotOnShow) and (PageControl.ActivePageIndex=1)) and
         not(ad_evaluationRunning) and
         not(plotSubsystem.rendering) and
         not(now<plotSubsystem.renderNotBefore) then doPlot();

      if isEvaluationRunning then evaluation.deferredUntil:=now+0.1*ONE_SECOND else
      if evaluation.required and not(ad_evaluationRunning) and (now>evaluation.deferredUntil) then begin
        doStartEvaluation;
        ad_evaluate(InputEdit.lines);
        UpdateTimeTimer.Interval:=MIN_INTERVALL;
      end;

      if (now>settings.storedAt+STATE_SAVE_INTERVAL) then begin
        settings.storedAt:=now;
        SettingsForm.saveSettings;
      end;

      if not(flushPerformed) and not(autosizingDone) then begin
        UpdateTimeTimer.Interval:=UpdateTimeTimer.Interval+10;
        if UpdateTimeTimer.Interval>MAX_INTERVALL then UpdateTimeTimer.Interval:=MAX_INTERVALL;
      end else UpdateTimeTimer.Interval:=MIN_INTERVALL;
    end;
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
  begin
    if not(settings.ready) then begin
      InputEdit.BeginUpdate();
      SettingsForm.getFileContents(InputEdit.lines);
      InputEdit.EndUpdate();
    end;

    InputEdit.Font.name:=SettingsForm.getEditorFontName;
    InputEdit.Font.size:=SettingsForm.fontSize;
    if SettingsForm.AntialiasCheckbox.Checked
    then InputEdit.Font.Quality:=fqCleartypeNatural
    else InputEdit.Font.Quality:=fqNonAntialiased;

    OutputEdit.Font         :=InputEdit.Font;
    DebugForm.debugEdit.Font:=InputEdit.Font;

    top   :=SettingsForm.mainForm.top;
    left  :=SettingsForm.mainForm.left;
    width :=SettingsForm.mainForm.width;
    height:=SettingsForm.mainForm.height;

    with SettingsForm.outputBehaviour do begin
      miDeclarationEcho.Checked:=doEchoDeclaration;
      miExpressionEcho.Checked:=doEchoInput;
      miExpressionResult.Checked:=doShowExpressionOut;
      miTimingInfo.Checked:=doShowTimingInfo;
      miMinErrorlevel1.Checked:=minErrorLevel<=1;
      miMinErrorlevel2.Checked:=minErrorLevel=2;
      miMinErrorlevel3.Checked:=minErrorLevel=3;
      miMinErrorlevel4.Checked:=minErrorLevel=4;
      miMinErrorlevel5.Checked:=minErrorLevel>=5;
      guiAdapters.outputBehaviour:=SettingsForm.outputBehaviour;
    end;
    if not(settings.ready) then begin
      if SettingsForm.mainForm.isFullscreen then WindowState:=wsMaximized;
      miAutoReset.Checked:=SettingsForm.resetPlotOnEvaluation;
      miEvalModeDirect.Checked:=not(SettingsForm.instantEvaluation);
      miEvalModeDirectOnKeypress.Checked:=SettingsForm.instantEvaluation;
    end;
    if ad_currentFile<>SettingsForm.getFileInEditor then begin
      if SettingsForm.getFileInEditor=''
      then ad_clearFile
      else ad_setFile(SettingsForm.getFileInEditor,InputEdit.lines);
    end;
    if not(settings.ready) then processFileHistory;
    settings.ready:=true;
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
    for i:=0 to 9 do if SettingsForm.fileHistory[i]='' then begin
      historyMenuItem(i).Enabled:=false;
      historyMenuItem(i).Caption:=intToStr(i)+': <no file>';
    end else begin
      historyMenuItem(i).Enabled:=true;
      historyMenuItem(i).Caption:=intToStr(i)+': '+SettingsForm.fileHistory[i];
    end;
  end;

PROCEDURE TMnhForm.miAntialiasingOffClick(Sender: TObject);
  begin
    if ad_evaluationRunning or plotSubsystem.rendering or (PageControl.ActivePageIndex<>1)
       then plotSubsystem.state:=pss_plotOnShow
       else doPlot();
  end;

PROCEDURE TMnhForm.miAutoResetClick(Sender: TObject);
  begin
    miAutoReset.Checked:=not(miAutoReset.Checked);
    SettingsForm.resetPlotOnEvaluation:=miAutoReset.Checked;
  end;

PROCEDURE TMnhForm.miAutoscaleXClick(Sender: TObject);
  begin
    miAutoscaleX.Checked:=not(miAutoscaleX.Checked);
    pushSettingsToPlotContainer(true);
  end;

PROCEDURE TMnhForm.miAutoscaleYClick(Sender: TObject);
  begin
    miAutoscaleY.Checked:=not(miAutoscaleY.Checked);
    pushSettingsToPlotContainer(true);
  end;

PROCEDURE TMnhForm.miLogscaleXClick(Sender: TObject);
  begin
    miLogscaleX.Checked:=not(miLogscaleX.Checked);
    pushSettingsToPlotContainer(true);
  end;

PROCEDURE TMnhForm.miLogscaleYClick(Sender: TObject);
  begin
    miLogscaleY.Checked:=not(miLogscaleY.Checked);
    pushSettingsToPlotContainer(true);
  end;

PROCEDURE TMnhForm.miPreserveAspectClick(Sender: TObject);
  begin
    miPreserveAspect.Checked:=not(miPreserveAspect.Checked);
    pushSettingsToPlotContainer(true);
  end;

PROCEDURE TMnhForm.miXFinerGridClick(Sender: TObject);
begin
  miXFinerGrid.Checked:=not(miXFinerGrid.Checked);
  if miXFinerGrid.Checked then miXGrid.Checked:=true;
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TMnhForm.miXGridClick(Sender: TObject);
begin
  miXGrid.Checked:=not(miXGrid.Checked);
  if not(miXGrid.Checked) then miXFinerGrid.Checked:=false;
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TMnhForm.miXTicsClick(Sender: TObject);
begin
  miXTics.Checked:=not(miXTics.Checked);
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TMnhForm.miYFinerGridClick(Sender: TObject);
begin
  miYFinerGrid.Checked:=not(miYFinerGrid.Checked);
  if miYFinerGrid.Checked then miYGrid.Checked:=true;
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TMnhForm.miYGridClick(Sender: TObject);
begin
  miYGrid.Checked:=not(miYGrid.Checked);
  if not(miYGrid.Checked) then miYFinerGrid.Checked:=false;
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TMnhForm.miYTicsClick(Sender: TObject);
begin
  miYTics.Checked:=not(miYTics.Checked);
  pushSettingsToPlotContainer(true);
end;

FUNCTION plot(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=mnh_plotData.plot(params,tokenLocation,adapters);
    if result<>nil then plotSubsystem.state:=pss_plotAfterCalculation;
  end;

FUNCTION addPlot(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  begin
    activePlot.setScreenSize(MnhForm.PlotTabSheet.width,
                             MnhForm.PlotTabSheet.height);
    result:=mnh_plotData.addPlot(params,tokenLocation,adapters);
    if result<>nil then plotSubsystem.state:=pss_plotAfterCalculation;
  end;

FUNCTION setAutoscale(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=mnh_plotData.setAutoscale(params,tokenLocation,adapters);
    if result<>nil then MnhForm.pullPlotSettingsToGui();
  end;

FUNCTION setLogscale(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=mnh_plotData.setLogscale(params,tokenLocation,adapters);
    if result<>nil then MnhForm.pullPlotSettingsToGui();
  end;

FUNCTION setPlotRange(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=mnh_plotData.setPlotRange(params,tokenLocation,adapters);
    if result<>nil then MnhForm.pullPlotSettingsToGui();
  end;

FUNCTION setAxisStyle(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=mnh_plotData.setAxisStyle(params,tokenLocation,adapters);
    if result<>nil then MnhForm.pullPlotSettingsToGui();
  end;

FUNCTION setPreserveAspect(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=mnh_plotData.setPreserveAspect(params,tokenLocation,adapters);
    if result<>nil then MnhForm.pullPlotSettingsToGui();
  end;

PROCEDURE lateInitialization;
  begin
    plotSubsystem.renderNotBefore:=now;
    plotSubsystem.state:=pss_neutral;
    plotSubsystem.rendering:=false;
    registerRule(SYSTEM_BUILTIN_NAMESPACE,'ask', @ask_impl,
      'ask(q:string);#Asks the user question q and returns the user input#'+
      'ask(q:string,options:stringList);#Asks the user question q, giving the passed options and returns the chosen option');
    mnh_funcs.registerRule(PLOT_NAMESPACE,'plot',@plot,'');
    mnh_funcs.registerRule(PLOT_NAMESPACE,'addPlot',@addPlot,'');
    mnh_funcs.registerRule(PLOT_NAMESPACE,'setAutoscale',@setAutoscale,'');
    mnh_funcs.registerRule(PLOT_NAMESPACE,'setLogscale',@setLogscale,'');
    mnh_funcs.registerRule(PLOT_NAMESPACE,'setRange',@setPlotRange,'');
    mnh_funcs.registerRule(PLOT_NAMESPACE,'setAxisStyle',@setAxisStyle,'');
    mnh_funcs.registerRule(PLOT_NAMESPACE,'setPreserveAspect',@setPreserveAspect,'');
    mnh_evalThread.initUnit;
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

INITIALIZATION
  guiOutAdapter.create;
  guiAdapters.create;
  guiAdapters.addOutAdapter(@guiOutAdapter,false);
  mnh_evalThread.guiOutAdapters:=@guiAdapters;
  StopDebuggingCallback:=@debugForm_stopDebugging;
  DebuggingStepCallback:=@debugForm_debuggingStep;

FINALIZATION
  guiAdapters.destroy;
  guiOutAdapter.destroy;
end.

