UNIT mnh_plotForm;

{$mode objfpc}{$H+}

INTERFACE
{$WARN 5024 OFF}
USES Classes,
     Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ComCtrls, StdCtrls,
     EpikTimer,
     mnh_plotData,
     myGenerics,
     plotMath,
     ideLayoutUtil,
     mnh_messages,
     out_adapters;
TYPE
  TplotForm = class;

  P_queryPlotClosedMessage=^T_queryPlotClosedMessage;

  T_queryPlotClosedMessage=object(T_payloadMessage)
    private
      response:boolean;
      retrieved:boolean;
    protected
      FUNCTION internalType:shortstring; virtual;
    public
      CONSTRUCTOR createRetrieveRequest;
      PROCEDURE setResponse(CONST r:boolean);
      FUNCTION getResponseWaiting(CONST errorFlagProvider:P_messages):boolean;
  end;

  P_guiPlotSystem= ^T_guiPlotSystem;
  T_guiPlotSystem = object(T_plotSystem)
    protected
      PROCEDURE processMessage(CONST message:P_storedMessage); virtual;
    private
      myPlotForm:TplotForm;
      cap:string;
      connected:boolean;
      formWasClosedByUser:boolean;
      PROCEDURE ensureForm;
    public
      CONSTRUCTOR create(CONST plotFormCaption:string='MNH plot');
      DESTRUCTOR destroy; virtual;
      PROCEDURE doPlot;
      PROCEDURE formDestroyed;
      FUNCTION plotFormForConnecting:TplotForm;
      PROCEDURE disconnect;
  end;

  PMouseEvent=PROCEDURE(CONST realPoint:T_point) of object;
  TplotForm = class(T_mnhComponentForm)
    animateCheckBox: TCheckBox;
    cycleCheckbox: TCheckBox;
    AnimationGroupBox: TGroupBox;
    animationFPSLabel: TLabel;
    frameIndexLabel: TLabel;
    MainMenu: TMainMenu;
    MainMenu1: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    miAntiAliasing5: TMenuItem;
    miAntiAliasing6: TMenuItem;
    miAntiAliasing7: TMenuItem;
    miAntiAliasing8: TMenuItem;
    miAutoReset1: TMenuItem;
    miAutoscaleX1: TMenuItem;
    miAutoscaleY1: TMenuItem;
    miCacheFrames1: TMenuItem;
    miCreateScript1: TMenuItem;
    miDecFontSize1: TMenuItem;
    miIncFontSize1: TMenuItem;
    miLogscaleX1: TMenuItem;
    miLogscaleY1: TMenuItem;
    miPreserveAspect1: TMenuItem;
    miRenderToFile1: TMenuItem;
    miScriptFromAnimation: TMenuItem;
    miScriptFromAnimation1: TMenuItem;
    miScriptFromFrame: TMenuItem;
    miCreateScript: TMenuItem;
    miCacheFrames: TMenuItem;
    miRenderToFile: TMenuItem;
    miAntiAliasing4: TMenuItem;
    miIncFontSize: TMenuItem;
    miDecFontSize: TMenuItem;
    miAutoReset: TMenuItem;
    miLogscaleX: TMenuItem;
    miLogscaleY: TMenuItem;
    miAutoscaleX: TMenuItem;
    miAutoscaleY: TMenuItem;
    miPreserveAspect: TMenuItem;
    miScriptFromFrame1: TMenuItem;
    miXFinerGrid1: TMenuItem;
    miXGrid1: TMenuItem;
    miXTics1: TMenuItem;
    miYFinerGrid1: TMenuItem;
    miYGrid1: TMenuItem;
    miYTics: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miXGrid: TMenuItem;
    miXFinerGrid: TMenuItem;
    miXTics: TMenuItem;
    miYGrid: TMenuItem;
    miYFinerGrid: TMenuItem;
    miAntiAliasing1: TMenuItem;
    miAntiAliasing2: TMenuItem;
    miAntiAliasing3: TMenuItem;
    miYTics1: TMenuItem;
    plotImage: TImage;
    SaveDialog: TSaveDialog;
    StatusBar: TStatusBar;
    animationSpeedTrackbar: TTrackBar;
    frameTrackBar: TTrackBar;
    PROCEDURE FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormKeyPress(Sender: TObject; VAR key: char);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE frameTrackBarChange(Sender: TObject);
    PROCEDURE miAntiAliasing1Click(Sender: TObject);
    PROCEDURE miAntiAliasing2Click(Sender: TObject);
    PROCEDURE miAntiAliasing3Click(Sender: TObject);
    PROCEDURE miAntiAliasing4Click(Sender: TObject);
    PROCEDURE miAutoResetClick(Sender: TObject);
    PROCEDURE miAutoscaleXClick(Sender: TObject);
    PROCEDURE miAutoscaleYClick(Sender: TObject);
    PROCEDURE miCacheFramesClick(Sender: TObject);
    PROCEDURE miCreateScriptClick(Sender: TObject);
    PROCEDURE miDecFontSizeClick(Sender: TObject);
    PROCEDURE miIncFontSizeClick(Sender: TObject);
    PROCEDURE miLogscaleXClick(Sender: TObject);
    PROCEDURE miLogscaleYClick(Sender: TObject);
    PROCEDURE miPreserveAspectClick(Sender: TObject);
    PROCEDURE miRenderToFileClick(Sender: TObject);
    PROCEDURE miScriptFromAnimationClick(Sender: TObject);
    PROCEDURE miScriptFromFrameClick(Sender: TObject);
    PROCEDURE miXFinerGridClick(Sender: TObject);
    PROCEDURE miXGridClick(Sender: TObject);
    PROCEDURE miXTicsClick(Sender: TObject);
    PROCEDURE miYFinerGridClick(Sender: TObject);
    PROCEDURE miYGridClick(Sender: TObject);
    PROCEDURE miYTicsClick(Sender: TObject);
    PROCEDURE plotImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE plotImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    PROCEDURE plotImageMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);

    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
  private
    animationFrameIndex:longint;
    fpsSamplingStart:double;
    framesSampled:longint;
    eTimer:TEpikTimer;
    mouseUpTriggersPlot:boolean;
    lastMouseX,lastMouseY:longint;
    attachedToMainForm:boolean;
    relatedPlot:P_guiPlotSystem;
    FUNCTION getPlotQuality:byte;
    PROCEDURE createScriptEditOrFile(CONST contents:T_arrayOfString);
  public
    onPlotRescale:TNotifyEvent;
    onPlotMouseMove,
    onPlotMouseClick:PMouseEvent;

    PROCEDURE pullPlotSettingsToGui();
    PROCEDURE pushSettingsToPlotContainer();
    PROCEDURE pushFontSizeToPlotContainer(CONST newSize:double);
    PROCEDURE doPlot;
    FUNCTION timerTick:boolean;
    FUNCTION wantTimerInterval:longint;
  end;

PROCEDURE initializePlotForm(CONST coordLabel:TLabel);
IMPLEMENTATION
USES sysutils, FileUtil,
     mnh_constants, basicTypes,
     recyclers,
     mnh_settings, litVar, funcs,
     contexts, plotstyles,
     plotExport,
     editScripts,
     fileWrappers;
VAR mainFormCoordinatesLabel:TLabel;
FUNCTION T_queryPlotClosedMessage.internalType: shortstring;
begin
  result:='T_queryPlotClosedMessage';
end;

CONSTRUCTOR T_queryPlotClosedMessage.createRetrieveRequest;
  begin
    inherited create(mt_plot_queryClosedByUser);
    retrieved:=false;
  end;

PROCEDURE T_queryPlotClosedMessage.setResponse(CONST r: boolean);
  begin
    enterCriticalSection(messageCs);
    retrieved:=true;
    response:=r;
    leaveCriticalSection(messageCs);
  end;

FUNCTION T_queryPlotClosedMessage.getResponseWaiting(CONST errorFlagProvider: P_messages): boolean;
  begin
    enterCriticalSection(messageCs);
    while not(retrieved) and (errorFlagProvider^.continueEvaluation) do begin
      leaveCriticalSection(messageCs);
      sleep(1); ThreadSwitch;
      enterCriticalSection(messageCs);
    end;
    result:=response;
    leaveCriticalSection(messageCs);
  end;

PROCEDURE T_guiPlotSystem.processMessage(CONST message: P_storedMessage);
  begin
    case message^.messageType of
      mt_startOfEvaluation: begin
        formWasClosedByUser:=false;
        inherited processMessage(message);
      end;
      mt_plot_queryClosedByUser: begin
        P_queryPlotClosedMessage(message)^.setResponse(formWasClosedByUser);
        formWasClosedByUser:=false;
      end;
      else inherited processMessage(message);
    end;
  end;

PROCEDURE T_guiPlotSystem.ensureForm;
  begin
    if myPlotForm=nil then begin
      myPlotForm:=TplotForm.create(Application);
      myPlotForm.caption:=cap;
      myPlotForm.relatedPlot:=@self;
      pullSettingsToGui:=@myPlotForm.pullPlotSettingsToGui;
      myPlotForm                    .pullPlotSettingsToGui();
      dockNewForm(myPlotForm);
    end;
  end;

CONSTRUCTOR T_guiPlotSystem.create(CONST plotFormCaption: string);
  begin
    inherited create(@doPlot,false);
    cap:=plotFormCaption;
    myPlotForm:=nil;
    connected:=false;
  end;

DESTRUCTOR T_guiPlotSystem.destroy;
  begin
    if myPlotForm<>nil then begin
      disconnect;
      myPlotForm.relatedPlot:=nil;
      FreeAndNil(myPlotForm);
    end;
    connected:=false;
    inherited destroy;
  end;

PROCEDURE T_guiPlotSystem.doPlot;
  begin
    ensureForm;
    myPlotForm.doPlot;
  end;

PROCEDURE T_guiPlotSystem.formDestroyed;
  begin
    pullSettingsToGui:=nil;
    myPlotForm:=nil;
  end;

FUNCTION T_guiPlotSystem.plotFormForConnecting: TplotForm;
  begin
    connected:=true;
    ensureForm;
    result:=myPlotForm;
  end;

PROCEDURE T_guiPlotSystem.disconnect;
  begin
    connected:=false;
    if myPlotForm=nil then exit;
    myPlotForm.onPlotRescale:=nil;
    myPlotForm.onPlotMouseClick:=nil;
    myPlotForm.onPlotMouseMove:=nil;
  end;

{$R *.lfm}
PROCEDURE TplotForm.FormKeyPress(Sender: TObject; VAR key: char);
  begin
    if (key in ['+','-']) then begin
      relatedPlot^.startGuiInteraction;
      if key='+' then relatedPlot^.currentPlot.zoomOnPoint(lastMouseX,lastMouseY,  0.9,plotImage)
                 else relatedPlot^.currentPlot.zoomOnPoint(lastMouseX,lastMouseY,1/0.9,plotImage);
      pullPlotSettingsToGui();
      doPlot();
      relatedPlot^.doneGuiInteraction;
      if Assigned(onPlotRescale) then onPlotRescale(Sender);
    end;
  end;

PROCEDURE TplotForm.FormCreate(Sender: TObject);
  begin
    attachedToMainForm:=false;
    miAutoReset .checked:=settings.doResetPlotOnEvaluation;
    miAutoReset1.checked:=settings.doResetPlotOnEvaluation;
    miCacheFrames .checked:=settings.cacheAnimationFrames;
    miCacheFrames1.checked:=settings.cacheAnimationFrames;
    fpsSamplingStart:=now;
    framesSampled:=0;
    onPlotRescale:=nil;
    onPlotMouseMove:=nil;
    onPlotMouseClick:=nil;
    eTimer:=TEpikTimer.create(self);
    eTimer.clear;
    eTimer.start;
  end;

PROCEDURE TplotForm.FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
  begin

  end;

PROCEDURE TplotForm.FormDestroy(Sender: TObject);
  begin
    if relatedPlot<>nil then relatedPlot^.formDestroyed;
  end;

PROCEDURE TplotForm.FormResize(Sender: TObject);
  begin
    doPlot();
  end;

PROCEDURE TplotForm.FormShow(Sender: TObject);
  begin
    {$ifdef UNIX}
    miIncFontSize.ShortCut:=16605;
    {$endif}
    position:=poDefault;
  end;

PROCEDURE TplotForm.frameTrackBarChange(Sender: TObject);
  begin
    if animationFrameIndex=frameTrackBar.position then exit;
    animationFrameIndex:=frameTrackBar.position;
    relatedPlot^.startGuiInteraction;
    relatedPlot^.animation.getFrame(plotImage,animationFrameIndex,getPlotQuality,timedPlotExecution(nil,0));
    relatedPlot^.doneGuiInteraction;
    animateCheckBox.checked:=false;
  end;

PROCEDURE TplotForm.miAntiAliasing1Click(Sender: TObject);
  begin
    miAntiAliasing1.checked:=true;
    miAntiAliasing5.checked:=true;
    doPlot;
  end;

PROCEDURE TplotForm.miAntiAliasing2Click(Sender: TObject);
  begin
    miAntiAliasing2.checked:=true;
    miAntiAliasing6.checked:=true;
    doPlot;
  end;

PROCEDURE TplotForm.miAntiAliasing3Click(Sender: TObject);
  begin
    miAntiAliasing3.checked:=true;
    miAntiAliasing7.checked:=true;
    doPlot;
  end;

PROCEDURE TplotForm.miAntiAliasing4Click(Sender: TObject);
  begin
    miAntiAliasing4.checked:=true;
    miAntiAliasing8.checked:=true;
    doPlot;
  end;

PROCEDURE TplotForm.miAutoResetClick(Sender: TObject);
  begin
    miAutoReset .checked:=not(miAutoReset.checked);
    miAutoReset1.checked:=    miAutoReset.checked;
    settings.doResetPlotOnEvaluation:=miAutoReset.checked;
  end;

PROCEDURE TplotForm.miAutoscaleXClick(Sender: TObject);
  begin
    miAutoscaleX .checked:=not(miAutoscaleX.checked);
    miAutoscaleX1.checked:=    miAutoscaleX.checked;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miAutoscaleYClick(Sender: TObject);
  begin
    miAutoscaleY .checked:=not(miAutoscaleY.checked);
    miAutoscaleY1.checked:=    miAutoscaleY.checked;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miCacheFramesClick(Sender: TObject);
  begin
    miCacheFrames .checked:=not(miCacheFrames.checked);
    miCacheFrames1.checked:=    miCacheFrames.checked ;
    settings.cacheAnimationFrames:=miCacheFrames.checked;
  end;

PROCEDURE TplotForm.miDecFontSizeClick(Sender: TObject);
  begin
    pushFontSizeToPlotContainer(relatedPlot^.currentPlot.options.relativeFontSize/1.1);
  end;

PROCEDURE TplotForm.miIncFontSizeClick(Sender: TObject);
  begin
    pushFontSizeToPlotContainer(relatedPlot^.currentPlot.options.relativeFontSize*1.1);
  end;

PROCEDURE TplotForm.miLogscaleXClick(Sender: TObject);
  begin
    miLogscaleX .checked:=not(miLogscaleX.checked);
    miLogscaleX1.checked:=    miLogscaleX.checked ;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miLogscaleYClick(Sender: TObject);
  begin
    miLogscaleY .checked:=not(miLogscaleY.checked);
    miLogscaleY1.checked:=    miLogscaleY.checked ;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miPreserveAspectClick(Sender: TObject);
  begin
    miPreserveAspect .checked:=not(miPreserveAspect.checked);
    miPreserveAspect1.checked:=    miPreserveAspect.checked ;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miRenderToFileClick(Sender: TObject);
  begin
    relatedPlot^.startGuiInteraction;
    exportPlotForm.showModalFor(relatedPlot,animationFrameIndex);
    relatedPlot^.doneGuiInteraction;
  end;

PROCEDURE TplotForm.miCreateScriptClick(Sender: TObject);
  begin
    miScriptFromAnimation .enabled:=relatedPlot^.animation.frameCount>0;
    miScriptFromAnimation1.enabled:=relatedPlot^.animation.frameCount>0;
  end;

PROCEDURE TplotForm.createScriptEditOrFile(CONST contents:T_arrayOfString);
  VAR task:P_editScriptTask;
  begin
    if mainForm=nil then begin
      if SaveDialog.execute then begin
        writeFileLines(SaveDialog.fileName,contents,LineEnding,false);
      end;
    end else begin
      new(task,createForNewEditor(contents));
      mainForm.onEditFinished(task);
    end;
  end;

PROCEDURE TplotForm.miScriptFromAnimationClick(Sender: TObject);
  begin
    createScriptEditOrFile(relatedPlot^.getPlotStatement(-1));
  end;

PROCEDURE TplotForm.miScriptFromFrameClick(Sender: TObject);
  begin
    createScriptEditOrFile(relatedPlot^.getPlotStatement(animationFrameIndex));
  end;

PROCEDURE TplotForm.miXFinerGridClick(Sender: TObject);
  begin
    miXFinerGrid.checked:=not(miXFinerGrid.checked);
    if miXFinerGrid.checked then miXGrid.checked:=true;
    miXFinerGrid1.checked:=miXFinerGrid.checked;
    miXGrid1     .checked:=miXGrid     .checked;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miXGridClick(Sender: TObject);
  begin
    miXGrid.checked:=not(miXGrid.checked);
    if not(miXGrid.checked) then miXFinerGrid.checked:=false;
    miXFinerGrid1.checked:=miXFinerGrid.checked;
    miXGrid1     .checked:=miXGrid     .checked;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miXTicsClick(Sender: TObject);
  begin
    miXTics .checked:=not(miXTics.checked);
    miXTics1.checked:=    miXTics.checked ;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miYFinerGridClick(Sender: TObject);
  begin
    miYFinerGrid.checked:=not(miYFinerGrid.checked);
    if miYFinerGrid.checked then miYGrid.checked:=true;
    miYFinerGrid1.checked:=miYFinerGrid.checked;
    miYGrid1     .checked:=miYGrid     .checked;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miYGridClick(Sender: TObject);
  begin
    miYGrid.checked:=not(miYGrid.checked);
    if not(miYGrid.checked) then miYFinerGrid.checked:=false;
    miYFinerGrid1.checked:=miYFinerGrid.checked;
    miYGrid1     .checked:=miYGrid     .checked;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miYTicsClick(Sender: TObject);
  begin
    miYTics .checked:=not(miYTics.checked);
    miYTics1.checked:=    miYTics.checked ;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.plotImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if ssLeft in Shift then begin
      lastMouseX:=x;
      lastMouseY:=y;
      if Assigned(onPlotMouseClick) then onPlotMouseClick(relatedPlot^.currentPlot.options.screenToReal(x,y));
    end;
  end;

PROCEDURE TplotForm.plotImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  VAR p:T_point;
      statusText:string;
  begin
    if (animationFrameIndex>=0) and (animationFrameIndex<relatedPlot^.animation.frameCount)
    then p:=relatedPlot^.animation  .options[animationFrameIndex].screenToReal(x,y)
    else p:=relatedPlot^.currentPlot.options                     .screenToReal(x,y);
    statusText:='x='+floatToStr(p[0])+'; y='+floatToStr(p[1]);
    if attachedToMainForm
    then mainFormCoordinatesLabel.caption:=statusText
    else StatusBar.SimpleText:=statusText;

    if (ssLeft in Shift) and (relatedPlot^.animation.frameCount=0) then begin
      if (x<>lastMouseX) or (y<>lastMouseY) then begin
        relatedPlot^.startGuiInteraction;
        relatedPlot^.currentPlot.panByPixels(lastMouseX-x,lastMouseY-y,plotImage);
        relatedPlot^.doneGuiInteraction;
        mouseUpTriggersPlot:=true;
      end;
    end else if Assigned(onPlotMouseMove) then onPlotMouseMove(p);
    lastMouseX:=x;
    lastMouseY:=y;
  end;

PROCEDURE TplotForm.plotImageMouseUp(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  begin
    if mouseUpTriggersPlot then begin
      pullPlotSettingsToGui();
      if Assigned(onPlotRescale) then onPlotRescale(Sender);
      lastMouseX:=x;
      lastMouseY:=y;
      doPlot;
    end;
  end;

PROCEDURE TplotForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    if relatedPlot<>nil then relatedPlot^.formWasClosedByUser:=true;
    animateCheckBox.checked:=false;
    CloseAction:=caFree;
  end;

FUNCTION TplotForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icPlot;
  end;

PROCEDURE TplotForm.performSlowUpdate;
  begin

  end;

PROCEDURE TplotForm.performFastUpdate;
  begin
    timerTick;
  end;

FUNCTION TplotForm.getPlotQuality: byte;
  begin
    if      miAntiAliasing4.checked then result:=PLOT_QUALITY_HIGH
    else if miAntiAliasing3.checked then result:=PLOT_QUALITY_MEDIUM_2
    else if miAntiAliasing2.checked then result:=PLOT_QUALITY_MEDIUM_1
    else                                 result:=PLOT_QUALITY_LOW;
  end;

PROCEDURE TplotForm.pullPlotSettingsToGui();
  VAR currentScalingOptions:T_scalingOptions;
  begin
    relatedPlot^.startGuiInteraction;
    currentScalingOptions:=relatedPlot^.currentPlot.options;
    relatedPlot^.doneGuiInteraction;
    miXTics         .checked:=gse_tics       in currentScalingOptions.axisStyle['x'];
    miXGrid         .checked:=gse_coarseGrid in currentScalingOptions.axisStyle['x'];
    miXFinerGrid    .checked:=gse_fineGrid   in currentScalingOptions.axisStyle['x'];
    miYTics         .checked:=gse_tics       in currentScalingOptions.axisStyle['y'];
    miYGrid         .checked:=gse_coarseGrid in currentScalingOptions.axisStyle['y'];
    miYFinerGrid    .checked:=gse_fineGrid   in currentScalingOptions.axisStyle['y'];
    miPreserveAspect.checked:=currentScalingOptions.preserveAspect;
    miAutoscaleX    .checked:=currentScalingOptions.axisTrafo['x'].autoscale;
    miAutoscaleY    .checked:=currentScalingOptions.axisTrafo['y'].autoscale;
    miLogscaleX     .checked:=currentScalingOptions.axisTrafo['x'].logscale;
    miLogscaleY     .checked:=currentScalingOptions.axisTrafo['y'].logscale;

    miXTics1         .checked:=miXTics         .checked;
    miXGrid1         .checked:=miXGrid         .checked;
    miXFinerGrid1    .checked:=miXFinerGrid    .checked;
    miYTics1         .checked:=miYTics         .checked;
    miYGrid1         .checked:=miYGrid         .checked;
    miYFinerGrid1    .checked:=miYFinerGrid    .checked;
    miPreserveAspect1.checked:=miPreserveAspect.checked;
    miAutoscaleX1    .checked:=miAutoscaleX    .checked;
    miAutoscaleY1    .checked:=miAutoscaleY    .checked;
    miLogscaleX1     .checked:=miLogscaleX     .checked;
    miLogscaleY1     .checked:=miLogscaleY     .checked;
  end;

PROCEDURE TplotForm.pushFontSizeToPlotContainer(CONST newSize: double);
  VAR currentScalingOptions:T_scalingOptions;
      i:longint;
  begin
    relatedPlot^.startGuiInteraction;
    currentScalingOptions:=relatedPlot^.currentPlot.options;
    currentScalingOptions.relativeFontSize:=newSize;
    relatedPlot^.currentPlot.options:=currentScalingOptions;
    pullPlotSettingsToGui();
    i:=0;
    while i<relatedPlot^.animation.frameCount do begin
      currentScalingOptions:=relatedPlot^.animation.options[i];
      currentScalingOptions.relativeFontSize:=newSize;
      relatedPlot^.animation.options[i]:=currentScalingOptions;
      inc(i);
    end;
    doPlot;
    relatedPlot^.doneGuiInteraction;
  end;

PROCEDURE TplotForm.pushSettingsToPlotContainer();
  VAR currentScalingOptions:T_scalingOptions;
      i:longint;
  PROCEDURE updateCurrent;
    begin
      currentScalingOptions.axisStyle['x']:=[];
      if miXTics.checked      then include(currentScalingOptions.axisStyle['x'],gse_tics      );
      if miXGrid.checked      then include(currentScalingOptions.axisStyle['x'],gse_coarseGrid);
      if miXFinerGrid.checked then include(currentScalingOptions.axisStyle['x'],gse_fineGrid  );
      currentScalingOptions.axisStyle['y']:=[];
      if miYTics.checked      then include(currentScalingOptions.axisStyle['y'],gse_tics      );
      if miYGrid.checked      then include(currentScalingOptions.axisStyle['y'],gse_coarseGrid);
      if miYFinerGrid.checked then include(currentScalingOptions.axisStyle['y'],gse_fineGrid  );
      currentScalingOptions.preserveAspect:=miPreserveAspect.checked;
      currentScalingOptions.axisTrafo['x'].logscale:=miLogscaleX.checked;
      currentScalingOptions.axisTrafo['y'].logscale:=miLogscaleY.checked;
      currentScalingOptions.axisTrafo['x'].autoscale:=miAutoscaleX.checked;
      currentScalingOptions.axisTrafo['y'].autoscale:=miAutoscaleY.checked;
    end;

  begin
    relatedPlot^.startGuiInteraction;
    currentScalingOptions:=relatedPlot^.currentPlot.options;
    updateCurrent;
    relatedPlot^.currentPlot.options:=currentScalingOptions;
    pullPlotSettingsToGui();
    i:=0;
    while i<relatedPlot^.animation.frameCount do begin
      currentScalingOptions:=relatedPlot^.animation.options[i];
      updateCurrent;
      relatedPlot^.animation.options[i]:=currentScalingOptions;
      inc(i);
    end;
    doPlot;
    relatedPlot^.doneGuiInteraction;
  end;

PROCEDURE TplotForm.doPlot;
  PROCEDURE updateInteractiveSection;
    begin
      AnimationGroupBox.visible:=(relatedPlot^.animation.frameCount>0);
      AnimationGroupBox.enabled:=(relatedPlot^.animation.frameCount>0);
      if relatedPlot^.animation.frameCount>0 then begin
        AnimationGroupBox.AutoSize:=true;
      end else begin
        AnimationGroupBox.AutoSize:=false;
        AnimationGroupBox.height:=0;
      end;
    end;

  PROCEDURE updateAttachment;
    begin
      if myComponentParent=cpNone then begin
        attachedToMainForm:=false;
        StatusBar.visible:=true;
      end else begin
        attachedToMainForm:=true;
        StatusBar.visible:=false;
      end;
    end;

  begin
    if relatedPlot=nil then exit;
    updateAttachment;
    relatedPlot^.startGuiInteraction;
    updateInteractiveSection;
    plotImage.picture.Bitmap.setSize(plotImage.width,plotImage.height);

    if relatedPlot^.animation.frameCount<>0 then begin
      relatedPlot^.animation.getFrame(plotImage,animationFrameIndex,getPlotQuality,timedPlotExecution(nil,0));
      relatedPlot^.doneGuiInteraction;
      exit;
    end;
    relatedPlot^.currentPlot.renderPlot(plotImage,getPlotQuality);
    relatedPlot^.logPlotDone;
    relatedPlot^.doneGuiInteraction;
  end;

FUNCTION TplotForm.timerTick: boolean;
  FUNCTION frameInterval:double;
    CONST intendedSecPerFrame:array[0..10] of double=(1,1/2,1/5,1/10,1/15,1/20,1/25,1/30,1/40,1/50,0);
    begin
      result:=intendedSecPerFrame[animationSpeedTrackbar.position];
    end;

  begin
    if relatedPlot=nil then exit(false);
    result:=false;
    relatedPlot^.startGuiInteraction;
    if gui_started and (showing) and (relatedPlot^.animation.frameCount>0) then begin

      if animateCheckBox.checked and
         //tick interval is 10ms; Try to plot if next frame is less than 20ms ahead
         (frameInterval-eTimer.elapsed<0.02) and
         relatedPlot^.animation.nextFrame(animationFrameIndex,cycleCheckbox.checked,plotImage.width,plotImage.height,getPlotQuality)
      then begin
        relatedPlot^.animation.getFrame(plotImage,animationFrameIndex,getPlotQuality,timedPlotExecution(eTimer,frameInterval));
        eTimer.clear;
        eTimer.start;
        inc(framesSampled);
        if (framesSampled>10) or (now-fpsSamplingStart>1/(24*60*60)) then begin
          animationFPSLabel.caption:=intToStr(round(framesSampled/((now-fpsSamplingStart)*24*60*60)))+'fps';
          fpsSamplingStart:=now;
          framesSampled:=0;
        end;
        result:=true;
      end;
      frameTrackBar.max:=relatedPlot^.animation.frameCount-1;
      frameTrackBar.position:=animationFrameIndex;
      frameIndexLabel.caption:=intToStr(animationFrameIndex);
      if frameTrackBar.max>90 then frameTrackBar.frequency:=10 else
      if frameTrackBar.max>20 then frameTrackBar.frequency:= 5
                              else frameTrackBar.frequency:= 1;
    end else result:=false;
    relatedPlot^.doneGuiInteraction;
  end;

FUNCTION TplotForm.wantTimerInterval: longint;
  begin
    if animateCheckBox.checked then result:=1
                               else result:=50;
  end;

{$i func_defines.inc}
FUNCTION plotClosedByUser_impl intFuncSignature;
  VAR closedRequest:P_queryPlotClosedMessage;
  begin if (params=nil) or (params^.size=0) then begin
    new(closedRequest,createRetrieveRequest);
    context.messages^.postCustomMessage(closedRequest);
    result:=newBoolLiteral(closedRequest^.getResponseWaiting(context.messages));
    disposeMessage(closedRequest);
  end else result:=nil; end;

FUNCTION clearPlotAnim_impl intFuncSignature;
  begin if (params=nil) or (params^.size=0) then begin
    result:=newVoidLiteral;
    context.messages^.postSingal(mt_plot_clearAnimation,C_nilTokenLocation);
  end else result:=nil; end;

FUNCTION addAnimFrame_impl intFuncSignature;
  begin if (params=nil) or (params^.size=0) then begin
    result:=newVoidLiteral;
    context.messages^.postSingal(mt_plot_addAnimationFrame,C_nilTokenLocation);
  end else result:=nil; end;

FUNCTION display_imp intFuncSignature;
  VAR displayRequest:P_plotDisplayRequest;
  begin if (params=nil) or (params^.size=0) then begin
    new(displayRequest,create());
    context.messages^.postCustomMessage(displayRequest);
    displayRequest^.waitForExecution(context.messages);
    disposeMessage(displayRequest);
    result:=newVoidLiteral;
  end else result:=nil; end;

FUNCTION postdisplay_imp intFuncSignature;
  VAR displayRequest:P_plotDisplayRequest;
  begin if (params=nil) or (params^.size=0) then begin
    new(displayRequest,create());
    context.messages^.postCustomMessage(displayRequest,true);
    result:=newVoidLiteral;
  end else result:=nil; end;

FUNCTION uninitialized_fallback intFuncSignature;
  begin
    context.messages^.logGuiNeeded;
    result:=nil;
  end;

PROCEDURE initializePlotForm(CONST coordLabel:TLabel);
  begin
    mainFormCoordinatesLabel:=coordLabel;
    reregisterRule(PLOT_NAMESPACE,'plotClosed'       ,@plotClosedByUser_impl);
    reregisterRule(PLOT_NAMESPACE,'clearAnimation'   ,@clearPlotAnim_impl   );
    reregisterRule(PLOT_NAMESPACE,'addAnimationFrame',@addAnimFrame_impl    );
    reregisterRule(PLOT_NAMESPACE,'display'          ,@display_imp          );
    reregisterRule(PLOT_NAMESPACE,'postDisplay'      ,@postdisplay_imp      );
  end;

INITIALIZATION
  registerRule(PLOT_NAMESPACE,'plotClosed'       ,@uninitialized_fallback,ak_nullary,'plotClosed;//Returns true if the plot has been closed by user interaction');
  registerRule(PLOT_NAMESPACE,'clearAnimation'   ,@uninitialized_fallback,ak_nullary,'clearAnimation;//Clears the animated plot');
  registerRule(PLOT_NAMESPACE,'addAnimationFrame',@uninitialized_fallback,ak_nullary,'addAnimationFrame;//Adds the current plot to the animation');
  registerRule(PLOT_NAMESPACE,'display'          ,@uninitialized_fallback,ak_nullary,'display;//Displays the plot as soon as possible and waits for execution');
  registerRule(PLOT_NAMESPACE,'postDisplay'      ,@uninitialized_fallback,ak_nullary,'display;//Displays the plot as soon as possible and returns immediately');

end.

