UNIT mnh_gui_main;

{$mode objfpc}{$H+}
INTERFACE

USES
  Classes, SysUtils, FileUtil, SynEdit, SynCompletion, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, Menus, StdCtrls, ComCtrls, Grids, PopupNotifier,
  SynHighlighterMnh, mnh_fileWrappers, mnh_gui_settings, mnh_tokloc,
  mnh_out_adapters, myStringutil, mnh_evalThread, mnh_constants, myGenerics,
  types, LCLType,mnh_plotData,mnh_funcs,mnh_litvar,mnh_doc,lclintf,mnh_tokens,closeDialog,askDialog,SynEditKeyCmds;

CONST ONE_SECOND=1/(24*60*60);

TYPE

  { TMnhForm }

  TMnhForm = class(TForm)
    MenuItem3: TMenuItem;
    miCallMain: TMenuItem;
    miHelp: TMenuItem;
    miHelpExternally: TMenuItem;
    miAntiAliasingOff: TMenuItem;
    miAntiAliasing2: TMenuItem;
    miAntiAliasing3: TMenuItem;
    miAntiAliasing4: TMenuItem;
    miAntiAliasing5: TMenuItem;
    miAutoReset: TMenuItem;
    errorStringGrid: TStringGrid;
    Panel1: TPanel;
    SplitterH: TSplitter;
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
    miOpenPlot: TMenuItem;
    miSavePlot: TMenuItem;
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
    miOpenNpp: TMenuItem;
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
    ErrorGroupBox: TGroupBox;
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
    UpdateTimeTimer: TTimer;
    PROCEDURE errorStringGridClick(Sender: TObject);
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormKeyPress(Sender: TObject; VAR Key: char);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE InputEditChange(Sender: TObject);
    PROCEDURE InputEditKeyDown(Sender: TObject; VAR Key: word;
      Shift: TShiftState);
    PROCEDURE InputEditMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    PROCEDURE MenuItem4Click(Sender: TObject);
    PROCEDURE miClearClick(Sender: TObject);
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
    PROCEDURE miOpenClick(Sender: TObject);
    PROCEDURE miOpenNppClick(Sender: TObject);
    PROCEDURE miOpenPlotClick(Sender: TObject);
    PROCEDURE miSaveAsClick(Sender: TObject);
    PROCEDURE miSaveClick(Sender: TObject);
    PROCEDURE miSavePlotClick(Sender: TObject);
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
    PROCEDURE OutputEditKeyDown(Sender: TObject; VAR Key: word;
      Shift: TShiftState);
    PROCEDURE OutputEditMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    PROCEDURE PageControlChange(Sender: TObject);
    PROCEDURE plotImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    PROCEDURE plotImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    PROCEDURE plotImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    PROCEDURE PopupNotifier1Close(Sender: TObject; VAR CloseAction: TCloseAction
      );
    PROCEDURE Splitter1Moved(Sender: TObject);
    procedure SplitterHMoved(Sender: TObject);
    PROCEDURE SynCompletionCodeCompletion(VAR value: string;
      SourceValue: string; VAR SourceStart, SourceEnd: TPoint;
      KeyChar: TUTF8Char; Shift: TShiftState);
    PROCEDURE SynCompletionExecute(Sender: TObject);
    PROCEDURE SynCompletionSearchPosition(VAR APosition: integer);
    PROCEDURE UpdateTimeTimerTimer(Sender: TObject);

  private
    underCursor:T_tokenInfo;
    settingsHaveBeenProcessed:boolean;
    needEvaluation:boolean;
    doNotEvaluateBefore:double;
    doNotMarkWordBefore:double;
    needMarkPaint:boolean;
    autosizingEnabled:boolean;

    PROCEDURE processSettings;
    PROCEDURE processFileHistory;
    PROCEDURE autosizeBlocks(CONST forceOutputFocus:boolean);
    FUNCTION flushThroughput:boolean;
    PROCEDURE positionHelpNotifier;
    PROCEDURE setUnderCursor(CONST lines:TStrings; CONST caret:TPoint);

    PROCEDURE doPlot();
    PROCEDURE pullPlotSettingsToGui();
    PROCEDURE pushSettingsToPlotContainer(CONST plotImmediately:boolean);
    PROCEDURE doConditionalPlotReset;
    PROCEDURE openFromHistory(CONST historyIdx:byte);
    PROCEDURE startOfEvaluation;
    { private declarations }
  public
    { public declarations }
  end;

VAR
  MnhForm: TMnhForm;

PROCEDURE lateInitialization;
IMPLEMENTATION
VAR errorThroughput:specialize G_safeArray<T_storedError>;
    output         :specialize G_safeArray<ansistring>;
    wantClear      :specialize G_safeVar<boolean>;
    plotSubsystem:record
      rendering:boolean;
      mouseUpTriggersPlot:boolean;
      renderNotBefore:double;
      state:(pss_neutral, pss_plotAfterCalculation, pss_plotOnShow);
      lastMouseX,lastMouseY:longint;
    end;

{$R *.lfm}

PROCEDURE writeDeclEcho(CONST s:ansistring);
  begin
    output.append(C_DeclEchoHead+' '+s);
  end;

PROCEDURE writeExprEcho(CONST s:ansistring);
  begin
    output.append(C_ExprEchoHead+' '+s);
  end;

PROCEDURE writeExprOut (CONST s:ansistring);
  begin
    output.append(C_ExprOutHead+' '+s);
  end;

PROCEDURE writePrint   (CONST list:T_arrayOfString);
  begin
    output.appendAll(list);
  end;

PROCEDURE clearPrint();
  begin
    wantClear.value:=true;
    output.clear;
  end;


PROCEDURE logError(CONST error:T_storedError);
  begin
    {$ifdef debugMode}
    mnh_out_adapters.plainStdErrOut(error);
    {$endif}
    if error.errorLevel<el2_warning then exit;
    errorThroughput.append(error);
  end;

PROCEDURE TMnhForm.autosizeBlocks(CONST forceOutputFocus:boolean);
  CONST SAMPLE_TEXT='1!gPQ|';
  VAR temp,
      idealInputHeight,
      idealOutputHeight,
      availableTotalHeight,
      scrollbarHeight:longint;
      inputFocus:boolean;
      needRepositioningOfHelp:boolean=false;
  begin
    if autosizingEnabled then begin
      if errorStringGrid.RowCount=0
      then temp:=0
      else temp:=round(Width/3);
      needRepositioningOfHelp:=temp<>ErrorGroupBox.Width;
      ErrorGroupBox.Width:=temp;

      scrollbarHeight     :=InputEdit.Height-InputEdit.ClientHeight;
      idealInputHeight    :=scrollbarHeight+ InputEdit .Font.GetTextHeight(SAMPLE_TEXT)* InputEdit .Lines.Count;
      idealOutputHeight   :=scrollbarHeight+ OutputEdit.Font.GetTextHeight(SAMPLE_TEXT)*(OutputEdit.Lines.Count+1);
      availableTotalHeight:=InputEdit.Height+OutputEdit.Height;
      inputFocus:=not(forceOutputFocus or OutputEdit.Focused);

      //Are both editors large enough?
      if (InputEdit.Height>=idealInputHeight) and (OutputEdit.Height>=idealOutputHeight) then exit;
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

      idealInputHeight:=round(idealInputHeight*0.2+InputEdit.Height*0.8);
      if idealInputHeight<>InputEdit.Height then begin
        if UpdateTimeTimer.Interval>200 then UpdateTimeTimer.Interval:=200;
        InputEdit.Height:=idealInputHeight;
        needRepositioningOfHelp:=true;
      end;
      if PopupNotifier1.Visible and needRepositioningOfHelp then positionHelpNotifier;
    end;
  end;

FUNCTION TMnhForm.flushThroughput:boolean;
  VAR i:longint;
      r0:longint;
  begin
    result:=false;
    //--------------------------------------------------------------------------
    if wantClear.value then begin
      wantClear.value:=false;
      OutputEdit.ClearAll;
      result:=true;
    end;
    //--------------------------------------------------------------------------
    if output.size>0 then begin
      OutputEdit.BeginUpdate();
      output.lock;
      for i:=0 to output.size-1 do OutputEdit.Lines.Append(output[i]);
      output.clear;
      output.unlock;
      OutputEdit.ExecuteCommand(ecEditorBottom,' ',nil);
      OutputEdit.ExecuteCommand(ecLineStart,' ',nil);
      OutputEdit.EndUpdate;
      result:=true;
    end;
    //--------------------------------------------------------------------------
    r0:=errorStringGrid.RowCount;
    errorThroughput.lock;
    errorStringGrid.RowCount:=r0+errorThroughput.size;
    for i:=0 to errorThroughput.size-1 do with errorThroughput[i] do begin
      if i>=errorStringGrid.RowCount then errorStringGrid.RowCount:=i+1;
      errorStringGrid.Cells[0,i+r0]:=C_errorLevelTxt[errorLevel];
      errorStringGrid.Cells[1,i+r0]:=errorMessage;
      if errorLocation.provider<>nil then try
        if errorLocation.provider=@mainPackageProvider
          then errorStringGrid.Cells[2,i+r0]:='#'
          else errorStringGrid.Cells[2,i+r0]:='@'+errorLocation.provider^.filename;
        errorStringGrid.Cells[3,i+r0]:=':'+IntToStr(errorLocation.line);
        errorStringGrid.Cells[4,i+r0]:=','+IntToStr(errorLocation.column);
      except
        errorStringGrid.Cells[3,i+r0]:='';
        errorStringGrid.Cells[4,i+r0]:='';
      end;
    end;
    if errorThroughput.size>0 then errorStringGrid.AutoSizeColumns;
    errorThroughput.clear;
    errorThroughput.unlock;
  end;

PROCEDURE TMnhForm.positionHelpNotifier;
  begin
    PopupNotifier1.ShowAtPos(left+InputEdit.Width-PopupNotifier1.vNotifierForm.Width,
                             ClientToScreen(Point(left,OutputEdit.Top)).y);
    InputEdit.SetFocus;
  end;

PROCEDURE TMnhForm.setUnderCursor(CONST lines: TStrings; CONST caret: TPoint);
  begin
    if (caret.y>0) and (caret.y<=lines.Count) then begin
      underCursor:=ad_getTokenInfo(lines[caret.y-1],caret.x+1);
      if isIdentifier(underCursor.tokenText,true) and
         (inputHighlighter.setMarkedWord(underCursor.tokenText) and outputHighlighter.setMarkedWord(underCursor.tokenText)) then begin
        needMarkPaint:=true;
      end;
      if (underCursor.tokenText<>'') and (underCursor.tokenText<>PopupNotifier1.Title) then begin
        PopupNotifier1.Title:=underCursor.tokenText;
        PopupNotifier1.Text:=replaceAll(underCursor.tokenExplanation,'#',C_lineBreakChar);
        miOpenNpp.Enabled:=underCursor.declaredInFile<>'';
        if miHelp.Checked and not(PopupNotifier1.Visible) then positionHelpNotifier;
      end;
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
    activePlot.setScreenSize(PlotTabSheet.Width,PlotTabSheet.Height);
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
    if FileExists(SettingsForm.fileHistory[historyIdx]) then begin
      if (ad_currentFile<>'') and (ad_needSave(InputEdit.Lines)) then begin
        mr:=closeDialogForm.showOnLoad;
        if mr=mrOK then MnhForm.InputEdit.Lines.SaveToFile(ad_currentFile);
        if mr=mrCancel then exit;
      end;
      ad_setFile(SettingsForm.fileHistory[historyIdx],InputEdit.Lines);
      if SettingsForm.setFileInEditor(SettingsForm.fileHistory[historyIdx]) then processFileHistory;
    end else if SettingsForm.polishHistory then processFileHistory;
  end;

PROCEDURE TMnhForm.startOfEvaluation;
  begin
    autosizingEnabled:=true;
    MnhForm.doConditionalPlotReset;
    errorThroughput.clear;
    MnhForm.OutputEdit.Lines.Clear;
    MnhForm.errorStringGrid.RowCount:=0;
    if MnhForm.inputHighlighter.setMarkedLine(-1,-1) then MnhForm.Repaint;
  end;

{ TMnhForm }
PROCEDURE TMnhForm.FormCreate(Sender: TObject);
  begin
    autosizingEnabled:=true;
    needEvaluation:=false;
    needMarkPaint:=false;
    doNotEvaluateBefore:=now;
    doNotMarkWordBefore:=now;
    OpenDialog.FileName:=paramstr(0);
    SaveDialog.FileName:=paramstr(0);
    inputHighlighter:=TSynMnhSyn.create(nil,false);
    outputHighlighter:=TSynMnhSyn.create(nil,true);
    InputEdit.Highlighter:=inputHighlighter;
    OutputEdit.Highlighter:=outputHighlighter;
    settingsHaveBeenProcessed:=false;
    StatusBar.SimpleText:=
      'compiled on: '+{$I %DATE%}+
      ' at: '+{$I %TIME%}+
      ' with FPC'+{$I %FPCVERSION%}+
      ' for '+{$I %FPCTARGET%};
    OutputEdit.Lines.Append('       compiled on: '+{$I %DATE%});
    OutputEdit.Lines.Append('       at: '+{$I %TIME%});
    OutputEdit.Lines.Append('       with FPC'+{$I %FPCVERSION%});
    OutputEdit.Lines.Append('       for '+{$I %FPCTARGET%});
    mnh_out_adapters.errorOut:=@logError;
    mnh_out_adapters.inputDeclEcho:=@writeDeclEcho;
    mnh_out_adapters.inputExprEcho:=@writeExprEcho;
    mnh_out_adapters.exprOut      :=@writeExprOut;
    mnh_out_adapters.printOut     :=@writePrint;
    mnh_out_adapters.clearConsole :=@clearPrint;
  end;

PROCEDURE TMnhForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  VAR mr:integer;
  begin
    if (ad_currentFile<>'') and (ad_needSave(InputEdit.Lines)) then begin
      mr:=closeDialogForm.showOnQuit;
      if mr=mrOK then MnhForm.InputEdit.Lines.SaveToFile(ad_currentFile);
      if mr=mrCancel then CloseAction:=caNone;
    end;
    SettingsForm.mainForm.isFullscreen:=(WindowState=wsMaximized);
    if CloseAction<>caNone then SettingsForm.setFileContents(InputEdit.Lines);
  end;

PROCEDURE TMnhForm.errorStringGridClick(Sender: TObject);
  VAR row:longint;
  begin
    row:=errorStringGrid.Selection.Top;
    if (row>=0) and (row<errorStringGrid.RowCount) and (errorStringGrid.Cells[2,row]='#') then begin
      if inputHighlighter.setMarkedLine(StrToIntDef(copy(errorStringGrid.Cells[3,row],2,20),0)-1,
                                        StrToIntDef(copy(errorStringGrid.Cells[4,row],2,20),0)-1)
      then Repaint;
    end;
  end;

PROCEDURE TMnhForm.FormDestroy(Sender: TObject);
  begin
    mnh_out_adapters.setDefaultCallbacks;
    inputHighlighter.destroy;
    outputHighlighter.destroy;
    ad_killEvaluationLoopSoftly;
  end;

PROCEDURE TMnhForm.FormKeyPress(Sender: TObject; VAR Key: char);
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
    if settingsHaveBeenProcessed then begin
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
  PROCEDURE optionalReload;
    VAR mr:integer;
    begin
      mr:=closeDialogForm.showOnOutOfSync;
      if mr=mrOK then begin
        InputEdit.BeginUpdate();
        ad_doReload(InputEdit.Lines);
        InputEdit.EndUpdate;
        UpdateTimeTimer.Interval:=200;
        OutputEdit.ClearAll;
      end else if mr=mrCancel then begin
        InputEdit.Lines.SaveToFile(ad_currentFile);
      end;
    end;

  begin
    if not(settingsHaveBeenProcessed) then begin
      processSettings;
      InputEdit.SetFocus;
    end;
    KeyPreview:=true;
    UpdateTimeTimer.Enabled:=true;
  end;

PROCEDURE TMnhForm.InputEditChange(Sender: TObject);
  begin
    if (miEvalModeDirectOnKeypress.Checked) and not(SynCompletion.IsActive) then begin
      if now>doNotEvaluateBefore then begin
        startOfEvaluation;
        ad_evaluate(InputEdit.Lines);
        needEvaluation:=false;
        doNotEvaluateBefore:=now+ONE_SECOND;
      end else needEvaluation:=true;
    end;
  end;

PROCEDURE TMnhForm.InputEditKeyDown(Sender: TObject; VAR Key: word;
  Shift: TShiftState);
  begin
    setUnderCursor(InputEdit.Lines,InputEdit.CaretXY);
  end;

PROCEDURE TMnhForm.InputEditMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    setUnderCursor(InputEdit.Lines,InputEdit.PixelsToRowColumn(point));
  end;

PROCEDURE TMnhForm.MenuItem4Click(Sender: TObject);
  begin
    askForm.initWithQuestion('Please give command line parameters');
    askForm.ShowModal;
    startOfEvaluation;
    ad_callMain(InputEdit.Lines,askForm.getLastAnswerReleasing);
  end;

PROCEDURE TMnhForm.miClearClick(Sender: TObject);
  VAR mr:integer;
  begin
    if (ad_currentFile<>'') and (ad_needSave(InputEdit.Lines)) then begin
      mr:=closeDialogForm.showOnLoad;
      if mr=mrOK then MnhForm.InputEdit.Lines.SaveToFile(ad_currentFile);
      if mr=mrCancel then exit;
    end;
    ad_clearFile;
    InputEdit.ClearAll;
    if SettingsForm.setFileInEditor('') then processFileHistory;
  end;

PROCEDURE TMnhForm.miDecFontSizeClick(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      SettingsForm.fontSize:=SettingsForm.fontSize-1;
      processSettings;
    end;
  end;

PROCEDURE TMnhForm.miDeclarationEchoClick(Sender: TObject);
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

PROCEDURE TMnhForm.miEvalModeDirectClick(Sender: TObject);
  begin
    if miEvalModeDirect.Checked then exit;
    miEvalModeDirect.Checked:=true;
    SettingsForm.instantEvaluation:=false;
  end;

PROCEDURE TMnhForm.miEvalModeDirectOnKeypressClick(Sender: TObject);
  begin
    if miEvalModeDirectOnKeypress.Checked then exit;
    miEvalModeDirectOnKeypress.Checked:=true;
    SettingsForm.instantEvaluation:=true;
  end;

PROCEDURE TMnhForm.miEvaluateNowClick(Sender: TObject);
  begin
    if now>doNotEvaluateBefore then begin
      startOfEvaluation;
      ad_evaluate(InputEdit.Lines);
      needEvaluation:=false;
      doNotEvaluateBefore:=now+ONE_SECOND;
    end else needEvaluation:=true;
  end;

PROCEDURE TMnhForm.miExportPlotClick(Sender: TObject);
  VAR storeImage:TImage;
      rect:TRect;
  begin
    SaveDialog.Filter:='Portable network graphics (PNG)|*.png';
    if SaveDialog.Execute then begin
      storeImage:=TImage.create(Self);
      storeImage.SetInitialBounds(0,0,PlotTabSheet.Width,PlotTabSheet.Height);
      rect.Top:=0;
      rect.Left:=0;
      rect.Right:=PlotTabSheet.Width;
      rect.Bottom:=PlotTabSheet.Height;
      storeImage.Canvas.CopyRect(rect,plotImage.Canvas,rect);
      SaveDialog.FileName:=ChangeFileExt(SaveDialog.FileName,'.png');
      storeImage.Picture.PNG.SaveToFile(SaveDialog.FileName);
      storeImage.Free;
    end;
  end;

PROCEDURE TMnhForm.miExpressionEchoClick(Sender: TObject);
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

PROCEDURE TMnhForm.miExpressionResultClick(Sender: TObject);
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
    OpenURL('file:///'+replaceAll(ExpandFileName(htmlRoot+'\index.html'),'\','/'));
  end;

PROCEDURE TMnhForm.miIncFontSizeClick(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      SettingsForm.fontSize:=SettingsForm.fontSize+1;
      processSettings;
    end;
  end;

PROCEDURE TMnhForm.miOpenClick(Sender: TObject);
  VAR mr:integer;
  begin
    OpenDialog.Title:='Open file';
    if OpenDialog.Execute and FileExists(OpenDialog.FileName)
    then begin
      if (ad_currentFile<>'') and (ad_needSave(InputEdit.Lines)) then begin
        mr:=closeDialogForm.showOnLoad;
        if mr=mrOK then MnhForm.InputEdit.Lines.SaveToFile(ad_currentFile);
        if mr=mrCancel then exit;
      end;
      ad_setFile(OpenDialog.FileName,InputEdit.Lines);
      if SettingsForm.setFileInEditor(OpenDialog.FileName) then processFileHistory;
    end;
  end;

PROCEDURE TMnhForm.miOpenNppClick(Sender: TObject);
begin
  if underCursor.declaredInFile<>'' then
    SettingsForm.canOpenFile(underCursor.declaredInFile,underCursor.declaredInLine);
end;

PROCEDURE TMnhForm.miOpenPlotClick(Sender: TObject);
begin
  OpenDialog.Filter:='MNH-Plot|*.mnh_plot';
  if OpenDialog.Execute then begin
    OpenDialog.FileName:=ChangeFileExt(OpenDialog.FileName,'.mnh_plot');;
    if FileExistsUTF8(OpenDialog.FileName) then begin
      if not(activePlot.loadFromFile(OpenDialog.FileName))
      then activePlot.setDefaults
      else begin
        if ad_evaluationRunning or plotSubsystem.rendering
           then plotSubsystem.state:=pss_plotAfterCalculation
           else doPlot();
      end;
    end;
  end;
end;

PROCEDURE TMnhForm.miSaveAsClick(Sender: TObject);
  begin
    if SaveDialog.Execute then begin
      ad_saveFile(SaveDialog.FileName,InputEdit.Lines);
      if SettingsForm.setFileInEditor(SaveDialog.FileName) then processFileHistory;
      SettingsForm.saveSettings;
    end;
  end;

PROCEDURE TMnhForm.miSaveClick(Sender: TObject);
  begin
    if ad_currentFile='' then miSaveAsClick(Sender)
    else begin
      ad_saveFile(ad_currentFile,InputEdit.Lines);
      if SettingsForm.setFileInEditor(ad_currentFile) then processFileHistory;
      SettingsForm.saveSettings;
    end;
  end;

PROCEDURE TMnhForm.miSavePlotClick(Sender: TObject);
  begin
    SaveDialog.Filter:='MNH-Plot|*.mnh_plot';
    if SaveDialog.Execute then begin
      SaveDialog.FileName:=ChangeFileExt(SaveDialog.FileName,'.mnh_plot');
      activePlot.saveToFile(SaveDialog.FileName);
    end;
  end;

PROCEDURE TMnhForm.mi_settingsClick(Sender: TObject);
  begin
    SettingsForm.ShowModal;
    processSettings;
  end;

PROCEDURE TMnhForm.OutputEditKeyDown(Sender: TObject; VAR Key: word;
  Shift: TShiftState);
  begin
    setUnderCursor(OutputEdit.Lines,OutputEdit.CaretXY);
  end;


PROCEDURE TMnhForm.OutputEditMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    setUnderCursor(OutputEdit.Lines,OutputEdit.PixelsToRowColumn(point));
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
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then begin
    plotSubsystem.lastMouseX:=x;
    plotSubsystem.lastMouseY:=y;
  end;
end;

PROCEDURE TMnhForm.plotImageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
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

PROCEDURE TMnhForm.plotImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
    with plotSubsystem do if mouseUpTriggersPlot then begin
      pullPlotSettingsToGui();
      lastMouseX:=x;
      lastMouseY:=y;
      doPlot();
    end;
  end;

PROCEDURE TMnhForm.PopupNotifier1Close(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    miHelp.Checked:=false;
  end;

PROCEDURE TMnhForm.Splitter1Moved(Sender: TObject);
  begin
    if PopupNotifier1.Visible then positionHelpNotifier;
    autosizingEnabled:=false;
  end;

procedure TMnhForm.SplitterHMoved(Sender: TObject);
  begin
    if PopupNotifier1.Visible then positionHelpNotifier;
    autosizingEnabled:=false;
  end;

PROCEDURE TMnhForm.SynCompletionCodeCompletion(VAR value: string;
  SourceValue: string; VAR SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
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
    SynCompletion.ItemList.Clear;
    s:=SynCompletion.CurrentString;
    for i:=0 to completionList.size-1 do
      if (s='') or (pos(s,completionList[i])=1) then SynCompletion.ItemList.Add(completionList[i]);
  end;

PROCEDURE TMnhForm.SynCompletionSearchPosition(VAR APosition: integer);
  VAR i:longint;
      s:string;
  begin
    SynCompletion.ItemList.Clear;
    s:=SynCompletion.CurrentString;
    for i:=0 to completionList.size-1 do
      if pos(s,completionList[i])=1 then SynCompletion.ItemList.Add(completionList[i]);
    if SynCompletion.ItemList.Count>0 then APosition:=0 else APosition:=-1;
  end;

PROCEDURE TMnhForm.UpdateTimeTimerTimer(Sender: TObject);
  CONST MIN_INTERVALL=200;
        MAX_INTERVALL=5000;
  VAR aid:string;
      flag:boolean;
      i:longint;
      updateStart:double;
  begin
    //Show ask form?
    if askForm.displayPending then askForm.ShowModal;

    updateStart:=now;
    //Form caption:-------------------------------------------------------------
    aid:='MNH5 '+ad_currentFile;
    if aid<>Caption then begin Caption:=aid; UpdateTimeTimer.Interval:=MIN_INTERVALL; end;
    //-------------------------------------------------------------:Form caption
    //Halt/Run enabled states:--------------------------------------------------
    flag:=ad_evaluationRunning;
    if flag<>miHaltEvalutaion.Enabled then begin miHaltEvalutaion.Enabled:=flag; UpdateTimeTimer.Interval:=MIN_INTERVALL; end;
    flag:=not(flag);
    if flag<>miEvaluateNow.Enabled then begin
      miEvaluateNow.Enabled:=flag;
      miCallMain.Enabled:=flag;
      UpdateTimeTimer.Interval:=MIN_INTERVALL;
    end;
    //--------------------------------------------------:Halt/Run enabled states
    //progress time:------------------------------------------------------------
    flag:=ad_evaluationRunning;
    if flag then aid:='Evaluating: '+formatFloat('0.000',(now-mnh_evalThread.startOfEvaluation.value )*(24*60*60))+'s'
    else aid:=endOfEvaluationText.value;
    if (StatusBar.SimpleText<>aid) and (PageControl.ActivePageIndex=0) or flag  then begin
      StatusBar.SimpleText:=aid;
      UpdateTimeTimer.Interval:=MIN_INTERVALL;
    end;
    //------------------------------------------------------------:progress time
    autosizeBlocks(flushThroughput or flag);

    if ((plotSubsystem.state=pss_plotAfterCalculation) or
        (plotSubsystem.state=pss_plotOnShow) and (PageControl.ActivePageIndex=1)) and
       not(ad_evaluationRunning) and
       not(plotSubsystem.rendering) and
       not(now<plotSubsystem.renderNotBefore) then doPlot();

    if flag then doNotEvaluateBefore:=now+ONE_SECOND else
    if needEvaluation and (now>doNotEvaluateBefore) then begin
      ad_evaluate(InputEdit.Lines);
      needEvaluation:=false;
    end;

    //paint marks:--------------------------------------------------------------
    if needMarkPaint and (now>doNotMarkWordBefore) then begin
      needMarkPaint:=false;
      doNotMarkWordBefore:=now+ONE_SECOND;
      Repaint;
    end;
    //--------------------------------------------------------------:paint marks

    if UpdateTimeTimer.Interval<MAX_INTERVALL then UpdateTimeTimer.Interval:=UpdateTimeTimer.Interval+1;
    i:=round((now-updateStart)*24*60*60*1000);
    if i>UpdateTimeTimer.Interval then UpdateTimeTimer.Interval:=i;
  end;

PROCEDURE TMnhForm.processSettings;
  begin
    if not(settingsHaveBeenProcessed) then begin
      InputEdit.BeginUpdate();
      SettingsForm.getFileContents(InputEdit.Lines);
      InputEdit.EndUpdate();
    end;

    InputEdit.Font.name:=SettingsForm.getEditorFontName;
    InputEdit.Font.Size:=SettingsForm.fontSize;
    if SettingsForm.AntialiasCheckbox.Checked
    then InputEdit.Font.Quality:=fqCleartypeNatural
    else InputEdit.Font.Quality:=fqNonAntialiased;

    OutputEdit.Font     :=InputEdit.Font;

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
    if not(settingsHaveBeenProcessed) then begin
      if SettingsForm.mainForm.isFullscreen then WindowState:=wsMaximized;
      miAutoReset.Checked:=SettingsForm.resetPlotOnEvaluation;
      miEvalModeDirect.Checked:=not(SettingsForm.instantEvaluation);
      miEvalModeDirectOnKeypress.Checked:=SettingsForm.instantEvaluation;
    end;
    if ad_currentFile<>SettingsForm.getFileInEditor then begin
      if SettingsForm.getFileInEditor=''
      then ad_clearFile
      else ad_setFile(SettingsForm.getFileInEditor,InputEdit.Lines);
    end;
    if not(settingsHaveBeenProcessed) then processFileHistory;

    settingsHaveBeenProcessed:=true;
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
      historyMenuItem(i).Caption:=IntToStr(i)+': <no file>';
    end else begin
      historyMenuItem(i).Enabled:=true;
      historyMenuItem(i).Caption:=IntToStr(i)+': '+SettingsForm.fileHistory[i];
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

FUNCTION plot(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=mnh_plotData.plot(params,tokenLocation);
    if result<>nil then plotSubsystem.state:=pss_plotAfterCalculation;
  end;

FUNCTION addPlot(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    activePlot.setScreenSize(MnhForm.PlotTabSheet.Width,
                             MnhForm.PlotTabSheet.Height);
    result:=mnh_plotData.addPlot(params,tokenLocation);
    if result<>nil then plotSubsystem.state:=pss_plotAfterCalculation;
  end;

FUNCTION setAutoscale(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=mnh_plotData.setAutoscale(params,tokenLocation);
    if result<>nil then MnhForm.pullPlotSettingsToGui();
  end;

FUNCTION setLogscale(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=mnh_plotData.setLogscale(params,tokenLocation);
    if result<>nil then MnhForm.pullPlotSettingsToGui();
  end;

FUNCTION setPlotRange(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=mnh_plotData.setPlotRange(params,tokenLocation);
    if result<>nil then MnhForm.pullPlotSettingsToGui();
  end;

FUNCTION setAxisStyle(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=mnh_plotData.setAxisStyle(params,tokenLocation);
    if result<>nil then MnhForm.pullPlotSettingsToGui();
  end;

FUNCTION setPreserveAspect(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=mnh_plotData.setPreserveAspect(params,tokenLocation);
    if result<>nil then MnhForm.pullPlotSettingsToGui();
  end;

PROCEDURE lateInitialization;
  begin
    plotSubsystem.renderNotBefore:=now;
    plotSubsystem.state:=pss_neutral;
    plotSubsystem.rendering:=false;
    errorThroughput.clear;


    registerRule('ask', @ask_impl,
      'ask(q:string);#Asks the user question q and returns the user input#'+
      'ask(q:string,options:stringList);#Asks the user question q, giving the passed options and returns the chosen option');
    mnh_funcs.registerRule('plot',@plot,'');
    mnh_funcs.registerRule('addPlot',@addPlot,'');
    mnh_funcs.registerRule('setPlotAutoscale',@setAutoscale,'');
    mnh_funcs.registerRule('setPlotLogscale',@setLogscale,'');
    mnh_funcs.registerRule('setPlotRange',@setPlotRange,'');
    mnh_funcs.registerRule('setPlotAxisStyle',@setAxisStyle,'');
    mnh_funcs.registerRule('setPlotPreserveAspect',@setPreserveAspect,'');
    mnh_evalThread.initUnit;
  end;

INITIALIZATION
  output.create;
  wantClear.create(false);
  errorThroughput.create();
FINALIZATION
  output.destroy;
  wantClear.destroy;
  errorThroughput.destroy;
end.

