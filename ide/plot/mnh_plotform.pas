UNIT mnh_plotForm;

{$mode objfpc}{$H+}

INTERFACE
{$WARN 5024 OFF}
USES Classes,
     Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ComCtrls, StdCtrls,
     EpikTimer,
     mnh_plotData,
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
      PROCEDURE ensureForm(CONST dockToMain:boolean);
    public
      CONSTRUCTOR create(CONST plotFormCaption:string='MNH plot');
      DESTRUCTOR destroy; virtual;
      PROCEDURE doPlot;
      PROCEDURE formDestroyed;
      FUNCTION plotFormForConnecting(CONST forDocking:boolean):TplotForm;
      PROCEDURE disconnect;
      PROCEDURE logPlotChanged;
  end;

  PMouseEvent=PROCEDURE(CONST realPoint:T_point) of object;
  TplotForm = class(T_mnhComponentForm)
    animateCheckBox: TCheckBox;
    cycleCheckbox: TCheckBox;
    AnimationGroupBox: TGroupBox;
    animationFPSLabel: TLabel;
    frameIndexLabel: TLabel;
    MainMenu: TMainMenu;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    miCacheFrames: TMenuItem;
    miRenderToFile: TMenuItem;
    miIncFontSize: TMenuItem;
    miDecFontSize: TMenuItem;
    miAutoReset: TMenuItem;
    miLogscaleX: TMenuItem;
    miLogscaleY: TMenuItem;
    miAutoscaleX: TMenuItem;
    miAutoscaleY: TMenuItem;
    miPreserveAspect: TMenuItem;
    miYTics: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miXGrid: TMenuItem;
    miXFinerGrid: TMenuItem;
    miXTics: TMenuItem;
    miYGrid: TMenuItem;
    miYFinerGrid: TMenuItem;
    plotImage: TImage;
    PopupMenu1: TPopupMenu;
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
    PROCEDURE miAutoResetClick(Sender: TObject);
    PROCEDURE miAutoscaleXClick(Sender: TObject);
    PROCEDURE miAutoscaleYClick(Sender: TObject);
    PROCEDURE miCacheFramesClick(Sender: TObject);
    PROCEDURE miDecFontSizeClick(Sender: TObject);
    PROCEDURE miIncFontSizeClick(Sender: TObject);
    PROCEDURE miLogscaleXClick(Sender: TObject);
    PROCEDURE miLogscaleYClick(Sender: TObject);
    PROCEDURE miPreserveAspectClick(Sender: TObject);
    PROCEDURE miRenderToFileClick(Sender: TObject);
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
    PROCEDURE performSlowUpdate(CONST isEvaluationRunning:boolean); override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE dockChanged; override;
  private
    animationFrameIndex:longint;
    fpsSamplingStart:double;
    framesSampled:longint;
    eTimer:TEpikTimer;
    mouseUpTriggersPlot:boolean;
    lastMouseX,lastMouseY:longint;
    relatedPlot:P_guiPlotSystem;
    PROCEDURE updateInteractiveSection;
  public
    onPlotRescale:TNotifyEvent;
    onPlotMouseMove,
    onPlotMouseClick:PMouseEvent;

    PROCEDURE pullPlotSettingsToGui();
    PROCEDURE pushSettingsToPlotContainer();
    PROCEDURE pushFontSizeToPlotContainer(CONST newSize:double);
    PROCEDURE doPlot;
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
        plotChangedSinceLastDisplay:=false;
        inherited processMessage(message);
      end;
      mt_plot_queryClosedByUser: begin
        P_queryPlotClosedMessage(message)^.setResponse(formWasClosedByUser);
        formWasClosedByUser:=false;
      end;
      else inherited processMessage(message);
    end;
  end;

PROCEDURE T_guiPlotSystem.ensureForm(CONST dockToMain:boolean);
  begin
    if myPlotForm=nil then begin
      myPlotForm:=TplotForm.create(Application);
      myPlotForm.caption:=cap;
      myPlotForm.relatedPlot:=@self;
      pullSettingsToGui:=@myPlotForm.pullPlotSettingsToGui;
      myPlotForm                    .pullPlotSettingsToGui();
      if dockToMain
      then dockNewForm(myPlotForm)
      else myPlotForm.Show;
    end else if dockToMain then myPlotForm.showComponent(true);
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
    ensureForm(true);
    myPlotForm.doPlot;
  end;

PROCEDURE T_guiPlotSystem.formDestroyed;
  begin
    pullSettingsToGui:=nil;
    myPlotForm:=nil;
  end;

FUNCTION T_guiPlotSystem.plotFormForConnecting(CONST forDocking:boolean): TplotForm;
  begin
    connected:=true;
    ensureForm(not(forDocking));
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

PROCEDURE T_guiPlotSystem.logPlotChanged;
  begin
    enterCriticalSection(adapterCs);
    if not(isEmpty) then plotChangedSinceLastDisplay:=true;
    leaveCriticalSection(adapterCs);
  end;

{$R *.lfm}
PROCEDURE TplotForm.FormKeyPress(Sender: TObject; VAR key: char);
  begin
    if (key in ['+','-']) then begin
      relatedPlot^.startGuiInteraction;
      try
        if key='+' then relatedPlot^.currentPlot.zoomOnPoint(lastMouseX,lastMouseY,  0.9,plotImage)
                   else relatedPlot^.currentPlot.zoomOnPoint(lastMouseX,lastMouseY,1/0.9,plotImage);
        pullPlotSettingsToGui();
        relatedPlot^.logPlotChanged;
      finally
        relatedPlot^.doneGuiInteraction;
      end;
      if Assigned(onPlotRescale) then onPlotRescale(Sender);
    end;
  end;

PROCEDURE TplotForm.FormCreate(Sender: TObject);
  begin
    miAutoReset .checked:=settings.doResetPlotOnEvaluation;
    miCacheFrames .checked:=settings.cacheAnimationFrames;
    fpsSamplingStart:=now;
    framesSampled:=0;
    onPlotRescale:=nil;
    onPlotMouseMove:=nil;
    onPlotMouseClick:=nil;
    eTimer:=TEpikTimer.create(self);
    eTimer.clear;
    eTimer.start;
    initDockMenuItems(MainMenu,nil);
    initDockMenuItems(PopupMenu1,nil);
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
    if relatedPlot<>nil then begin
      updateInteractiveSection;
      relatedPlot^.logPlotChanged;
    end;
    plotImage.picture.Bitmap.setSize(plotImage.width,plotImage.height);
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
    try
      relatedPlot^.animation.getFrame(plotImage,animationFrameIndex,timedPlotExecution(nil,0));
    finally
      relatedPlot^.doneGuiInteraction;
    end;
    animateCheckBox.checked:=false;
  end;

PROCEDURE TplotForm.miAutoResetClick(Sender: TObject);
  begin
    miAutoReset .checked:=not(miAutoReset.checked);
    settings.doResetPlotOnEvaluation:=miAutoReset.checked;
  end;

PROCEDURE TplotForm.miAutoscaleXClick(Sender: TObject);
  begin
    miAutoscaleX .checked:=not(miAutoscaleX.checked);
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miAutoscaleYClick(Sender: TObject);
  begin
    miAutoscaleY .checked:=not(miAutoscaleY.checked);
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miCacheFramesClick(Sender: TObject);
  begin
    miCacheFrames .checked:=not(miCacheFrames.checked);
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
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miLogscaleYClick(Sender: TObject);
  begin
    miLogscaleY .checked:=not(miLogscaleY.checked);
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miPreserveAspectClick(Sender: TObject);
  begin
    miPreserveAspect .checked:=not(miPreserveAspect.checked);
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miRenderToFileClick(Sender: TObject);
  begin
    relatedPlot^.startGuiInteraction;
    try
      exportPlotForm.showModalFor(relatedPlot,animationFrameIndex);
    finally
      relatedPlot^.doneGuiInteraction;
    end;
  end;

PROCEDURE TplotForm.miXFinerGridClick(Sender: TObject);
  begin
    miXFinerGrid.checked:=not(miXFinerGrid.checked);
    if miXFinerGrid.checked then miXGrid.checked:=true;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miXGridClick(Sender: TObject);
  begin
    miXGrid.checked:=not(miXGrid.checked);
    if not(miXGrid.checked) then miXFinerGrid.checked:=false;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miXTicsClick(Sender: TObject);
  begin
    miXTics .checked:=not(miXTics.checked);
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miYFinerGridClick(Sender: TObject);
  begin
    miYFinerGrid.checked:=not(miYFinerGrid.checked);
    if miYFinerGrid.checked then miYGrid.checked:=true;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miYGridClick(Sender: TObject);
  begin
    miYGrid.checked:=not(miYGrid.checked);
    if not(miYGrid.checked) then miYFinerGrid.checked:=false;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miYTicsClick(Sender: TObject);
  begin
    miYTics .checked:=not(miYTics.checked);
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.plotImageMouseDown(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  begin
    if (myComponentParent<>cpNone) then mainForm.ActiveControl:=self;
    if ssLeft in Shift then begin
      lastMouseX:=x;
      lastMouseY:=y;
      if Assigned(onPlotMouseClick) then onPlotMouseClick(relatedPlot^.currentPlot.options.screenToReal(x,y));
    end;
  end;

PROCEDURE TplotForm.plotImageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: integer);
  VAR p:T_point;
      statusText:string;
  begin
    if (animationFrameIndex>=0) and (animationFrameIndex<relatedPlot^.animation.frameCount)
    then p:=relatedPlot^.animation  .options[animationFrameIndex].screenToReal(x,y)
    else p:=relatedPlot^.currentPlot.options                     .screenToReal(x,y);
    statusText:='x='+floatToStr(p[0])+'; y='+floatToStr(p[1]);
    if (myComponentParent<>cpNone)
    then mainFormCoordinatesLabel.caption:=statusText
    else StatusBar.SimpleText:=statusText;

    if (ssLeft in Shift) and (relatedPlot^.animation.frameCount=0) then begin
      if (x<>lastMouseX) or (y<>lastMouseY) then begin
        relatedPlot^.startGuiInteraction;
        try
          relatedPlot^.currentPlot.panByPixels(lastMouseX-x,lastMouseY-y,plotImage);
        finally
          relatedPlot^.doneGuiInteraction;
        end;
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
      if relatedPlot<>nil then relatedPlot^.logPlotChanged;
    end;
  end;

PROCEDURE TplotForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    if relatedPlot<>nil then relatedPlot^.formWasClosedByUser:=true;
    animateCheckBox.checked:=false;
    if mainFormCoordinatesLabel<>nil then mainFormCoordinatesLabel.caption:='';
    CloseAction:=caFree;
  end;

FUNCTION TplotForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icPlot;
  end;

PROCEDURE TplotForm.performSlowUpdate(CONST isEvaluationRunning:boolean);
  begin
    if (relatedPlot<>nil) and (relatedPlot^.isPlotChanged) then doPlot;
  end;

PROCEDURE TplotForm.performFastUpdate;
  FUNCTION frameInterval:double;
    CONST intendedSecPerFrame:array[0..10] of double=(1,1/2,1/5,1/10,1/15,1/20,1/25,1/30,1/40,1/50,0);
    begin
      result:=intendedSecPerFrame[animationSpeedTrackbar.position];
    end;

  begin
    if relatedPlot=nil then exit;
    relatedPlot^.startGuiInteraction;
    try
      if gui_started and (showing) and (relatedPlot^.animation.frameCount>0) then begin
        plotImage.picture.Bitmap.setSize(plotImage.width,plotImage.height);
        if animateCheckBox.checked and
           //tick interval is 10ms; Try to plot if next frame is less than 50ms ahead
           (frameInterval-eTimer.elapsed<0.05) and
           relatedPlot^.animation.nextFrame(animationFrameIndex,cycleCheckbox.checked,plotImage.width,plotImage.height)
        then begin
          relatedPlot^.animation.getFrame(plotImage,animationFrameIndex,timedPlotExecution(eTimer,frameInterval));
          eTimer.clear;
          eTimer.start;
          inc(framesSampled);
          if (framesSampled>10) or (now-fpsSamplingStart>1/(24*60*60)) then begin
            animationFPSLabel.caption:=formatFloat('#0.00',(framesSampled/((now-fpsSamplingStart)*24*60*60)))+'fps';
            fpsSamplingStart:=now;
            framesSampled:=0;
          end;
        end;
        frameTrackBar.max:=relatedPlot^.animation.frameCount-1;
        frameTrackBar.position:=animationFrameIndex;
        frameIndexLabel.caption:=intToStr(animationFrameIndex);
        if frameTrackBar.max>90 then frameTrackBar.frequency:=10 else
        if frameTrackBar.max>20 then frameTrackBar.frequency:= 5
                                else frameTrackBar.frequency:= 1;
      end;
    finally
      relatedPlot^.doneGuiInteraction;
    end;
  end;

PROCEDURE TplotForm.dockChanged;
  begin
    if myComponentParent=cpNone
    then moveAllItems(PopupMenu1.items,MainMenu.items)
    else moveAllItems(MainMenu.items,PopupMenu1.items);
    if myComponentParent=cpNone then begin
      StatusBar.visible:=true;
      if mainFormCoordinatesLabel<>nil then
         mainFormCoordinatesLabel.caption:='';
    end else StatusBar.visible:=false;
    resize;
  end;

PROCEDURE TplotForm.pullPlotSettingsToGui();
  VAR currentScalingOptions:T_scalingOptions;
  begin
    relatedPlot^.startGuiInteraction;
    try
      currentScalingOptions:=relatedPlot^.currentPlot.options;
    finally
      relatedPlot^.doneGuiInteraction;
    end;
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
  end;

PROCEDURE TplotForm.pushFontSizeToPlotContainer(CONST newSize: double);
  VAR currentScalingOptions:T_scalingOptions;
      i:longint;
  begin
    relatedPlot^.startGuiInteraction;
    try
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
      relatedPlot^.logPlotChanged;
    finally
      relatedPlot^.doneGuiInteraction;
    end;
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
    try
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
      relatedPlot^.logPlotChanged;
    finally
      relatedPlot^.doneGuiInteraction;
    end;
  end;

PROCEDURE TplotForm.updateInteractiveSection;
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

PROCEDURE TplotForm.doPlot;
  begin
    if relatedPlot=nil then exit;
    relatedPlot^.startGuiInteraction;
    try
      if relatedPlot^.isPlotChanged then begin
        updateInteractiveSection;
        plotImage.picture.Bitmap.setSize(plotImage.width,plotImage.height);
        if relatedPlot^.animation.frameCount<>0 then begin
          relatedPlot^.animation.getFrame(plotImage,animationFrameIndex,timedPlotExecution(nil,0));
        end else begin
          relatedPlot^.currentPlot.renderPlot(plotImage);
          relatedPlot^.logPlotDone;
        end;
      end;
    finally
      relatedPlot^.doneGuiInteraction;
    end;
  end;

{$i func_defines.inc}
FUNCTION plotClosedByUser_impl intFuncSignature;
  VAR closedRequest:P_queryPlotClosedMessage;
  begin if (params=nil) or (params^.size=0) then begin
    if not(gui_started) then context.messages^.logGuiNeeded;
    new(closedRequest,createRetrieveRequest);
    context.messages^.postCustomMessage(closedRequest);
    result:=newBoolLiteral(closedRequest^.getResponseWaiting(context.messages));
    disposeMessage(closedRequest);
  end else result:=nil; end;

FUNCTION clearPlotAnim_impl intFuncSignature;
  begin if (params=nil) or (params^.size=0) then begin
    if not(gui_started) then context.messages^.logGuiNeeded;
    result:=newVoidLiteral;
    context.messages^.postSingal(mt_plot_clearAnimation,C_nilTokenLocation);
  end else result:=nil; end;

FUNCTION addAnimFrame_impl intFuncSignature;
  begin if (params=nil) or (params^.size=0) then begin
    if not(gui_started) then context.messages^.logGuiNeeded;
    result:=newVoidLiteral;
    context.messages^.postSingal(mt_plot_addAnimationFrame,C_nilTokenLocation);
  end else result:=nil; end;

FUNCTION display_imp intFuncSignature;
  VAR displayRequest:P_plotDisplayRequest;
  begin if (params=nil) or (params^.size=0) then begin
    if not(gui_started) then context.messages^.logGuiNeeded;
    new(displayRequest,create());
    context.messages^.postCustomMessage(displayRequest);
    displayRequest^.waitForExecution(context.messages);
    disposeMessage(displayRequest);
    result:=newVoidLiteral;
  end else result:=nil; end;

FUNCTION postdisplay_imp intFuncSignature;
  VAR displayRequest:P_plotDisplayRequest;
  begin if (params=nil) or (params^.size=0) then begin
    if not(gui_started) then context.messages^.logGuiNeeded;
    new(displayRequest,create());
    context.messages^.postCustomMessage(displayRequest,true);
    result:=newVoidLiteral;
  end else result:=nil; end;

PROCEDURE initializePlotForm(CONST coordLabel:TLabel);
  begin
    mainFormCoordinatesLabel:=coordLabel;
  end;

INITIALIZATION
  registerRule(PLOT_NAMESPACE,'plotClosed'       ,@plotClosedByUser_impl,ak_nullary,'plotClosed;//Returns true if the plot has been closed by user interaction');
  registerRule(PLOT_NAMESPACE,'clearAnimation'   ,@clearPlotAnim_impl   ,ak_nullary,'clearAnimation;//Clears the animated plot');
  registerRule(PLOT_NAMESPACE,'addAnimationFrame',@addAnimFrame_impl    ,ak_nullary,'addAnimationFrame;//Adds the current plot to the animation');
  registerRule(PLOT_NAMESPACE,'display'          ,@display_imp          ,ak_nullary,'display;//Displays the plot as soon as possible and waits for execution');
  registerRule(PLOT_NAMESPACE,'postDisplay'      ,@postdisplay_imp      ,ak_nullary,'display;//Displays the plot as soon as possible and returns immediately');

end.

