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
    public
      FUNCTION internalType:shortstring; virtual;
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
      anyPlotMessage:boolean;
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
      FUNCTION append(CONST message: P_storedMessage): boolean; virtual;
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
    miCopyOptions: TMenuItem;
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
    PROCEDURE miCopyOptionsClick(Sender: TObject);
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
    fpsSamplingStart:TimerData;
    secondsPerFrameOverhead:double;
    secondsPerFrame:double;
    framesTimer:TEpikTimer;

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
USES sysutils, FileUtil, myStringUtil,
     mnh_constants, basicTypes,
     recyclers,
     mnh_settings, litVar, funcs,
     contexts, plotstyles,
     plotExport,
     editScripts,
     fileWrappers,
     Clipbrd;
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

FUNCTION T_guiPlotSystem.append(CONST message: P_storedMessage): boolean;
  begin
    if message^.messageType=mt_plot_queryClosedByUser then begin
      P_queryPlotClosedMessage(message)^.setResponse(formWasClosedByUser);
      plotChangedSinceLastDisplay:=not(formWasClosedByUser);
      formWasClosedByUser:=false;
      result:=true;
    end else result:=inherited;
  end;

PROCEDURE T_guiPlotSystem.processMessage(CONST message: P_storedMessage);
  begin
    case message^.messageType of
      mt_startOfEvaluation: begin
        anyPlotMessage:=false;
        formWasClosedByUser:=false;
        plotChangedSinceLastDisplay:=false;
        inherited processMessage(message);
      end;
      mt_endOfEvaluation:begin
        inherited processMessage(message);
        if not(anyPlotMessage) then begin
          if myPlotForm<>nil then myPlotForm.close;
          myPlotForm:=nil;
        end;
      end;
      else begin
        anyPlotMessage:=anyPlotMessage or
         (message^.messageType in [mt_plot_addText,
                                   mt_plot_addRow,
                                   mt_plot_rasterImage,
                                   mt_plot_dropRow]);
        inherited processMessage(message);
      end;
    end;
  end;

PROCEDURE T_guiPlotSystem.ensureForm(CONST dockToMain:boolean);
  begin
    if myPlotForm=nil then begin
      myPlotForm:=TplotForm.create(Application);
      if cap=''
      then myPlotForm.caption:=myPlotForm.getCaption
      else myPlotForm.caption:=cap;
      myPlotForm.relatedPlot:=@self;
      pullSettingsToGui:=@myPlotForm.pullPlotSettingsToGui;
      myPlotForm                    .pullPlotSettingsToGui();
      if dockToMain
      then dockNewForm(myPlotForm)
      else myPlotForm.Show;
    end else if dockToMain and myPlotForm.isDocked and not(myPlotForm.showing) then myPlotForm.showComponent(true);
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
    miAutoReset .checked:=ideSettings.doResetPlotOnEvaluation;
    miCacheFrames .checked:=ideSettings.cacheAnimationFrames;
    secondsPerFrameOverhead:=0;
    secondsPerFrame:=0;
    onPlotRescale:=nil;
    onPlotMouseMove:=nil;
    onPlotMouseClick:=nil;
    framesTimer:=TEpikTimer.create(self);
    framesTimer.clear;
    framesTimer.start;
    framesTimer.clear(fpsSamplingStart);
    framesTimer.start(fpsSamplingStart);
    initDockMenuItems(MainMenu,nil);
    initDockMenuItems(PopupMenu1,nil);
    registerFontControl(StatusBar,ctGeneral);
    registerFontControl(AnimationGroupBox,ctGeneral);
  end;

PROCEDURE TplotForm.FormDestroy(Sender: TObject);
  begin
    if relatedPlot<>nil then relatedPlot^.formDestroyed;
    unregisterFontControl(StatusBar);
    unregisterFontControl(AnimationGroupBox);
  end;

PROCEDURE TplotForm.FormResize(Sender: TObject);
  begin
    if relatedPlot<>nil then begin
      updateInteractiveSection;
      relatedPlot^.logPlotChanged;
    end;
    plotImage.picture.Bitmap.setSize(plotImage.width,plotImage.height);
    relatedPlot^.animation.resolutionChanged(plotImage.width,plotImage.height);
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
    ideSettings.doResetPlotOnEvaluation:=miAutoReset.checked;
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
    ideSettings.cacheAnimationFrames:=miCacheFrames.checked;
  end;

PROCEDURE TplotForm.miCopyOptionsClick(Sender: TObject);
  begin
    if Clipboard=nil then exit;
    Clipboard.AsText:=relatedPlot^.currentPlot.options.getOptionString;
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
    animateCheckBox.checked:=false;
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

PROCEDURE TplotForm.plotImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if (myComponentParent<>cpNone) then mainForm.ActiveControl:=self;
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
  VAR start:double;
  begin
    start:=now;
    if relatedPlot=nil then exit;
    if relatedPlot^.isPlotChanged then doPlot;
    if (now-start)>ONE_SECOND then postIdeMessage('Slow update of plot took a long time: '+myTimeToStr(now-start),true);
  end;

PROCEDURE TplotForm.performFastUpdate;
  FUNCTION frameInterval:double;
    CONST intendedSecPerFrame:array[0..10] of double=(1,1/2,1/5,1/10,1/15,1/20,1/25,1/30,1/40,1/50,0);
    begin
      result:=intendedSecPerFrame[animationSpeedTrackbar.position];
    end;

  PROCEDURE updateSecondsPerFrame;
    CONST weights:array[0..10] of double=(0.0,0.17328679513998632,0.40235947810852507,0.57564627324851136,0.6770125502755525,0.7489330683884977,0.8047189562170501,0.85029934541553887,0.92221986352848406,0.97800575135703649,0.97800575135703649);
    VAR w:double;
    begin
      w:=weights[animationSpeedTrackbar.position];
      secondsPerFrameOverhead:=secondsPerFrameOverhead*0.9+0.1*(framesTimer.elapsed(fpsSamplingStart)-frameInterval);
      if secondsPerFrameOverhead<0 then secondsPerFrameOverhead:=0;
      secondsPerFrame:=secondsPerFrame*w+(1-w)*(framesTimer.elapsed(fpsSamplingStart));
    end;

  VAR start:double;
  begin
    start:=now;
    if relatedPlot=nil then exit;
    if relatedPlot^.canStartGuiInteraction then
    try
      if (gui_started<>NO) and (showing) and (relatedPlot^.animation.frameCount>0) then begin
        plotImage.picture.Bitmap.setSize(plotImage.width,plotImage.height);
        if animateCheckBox.checked and
          (frameInterval-framesTimer.elapsed-secondsPerFrameOverhead<0.05) and
          relatedPlot^.animation.nextFrame(animationFrameIndex,cycleCheckbox.checked,plotImage.width,plotImage.height)
        then begin
          relatedPlot^.animation.getFrame(plotImage,animationFrameIndex,timedPlotExecution(framesTimer,frameInterval-secondsPerFrameOverhead));
          framesTimer.clear;
          framesTimer.start;
          //FPS label:
          updateSecondsPerFrame;
          framesTimer.clear(fpsSamplingStart);
          framesTimer.start(fpsSamplingStart);
          animationFPSLabel.caption:=formatFloat('#0.0',1/secondsPerFrame)+'fps';
          //:FPS label
        end;
        frameTrackBar.max:=relatedPlot^.animation.frameCount-1;
        frameTrackBar.position:=animationFrameIndex;
        frameIndexLabel.caption:=intToStr(animationFrameIndex);
        if frameTrackBar.max>5000 then frameTrackBar.frequency:=500 else
        if frameTrackBar.max>1000 then frameTrackBar.frequency:=100 else
        if frameTrackBar.max> 500 then frameTrackBar.frequency:= 50 else
        if frameTrackBar.max> 100 then frameTrackBar.frequency:= 10 else
        if frameTrackBar.max>  50 then frameTrackBar.frequency:=  5
                                  else frameTrackBar.frequency:=  1;
      end;
    finally
      relatedPlot^.doneGuiInteraction;
    end;
    if (now-start)>ONE_SECOND then postIdeMessage('Fast update of plot took a long time: '+myTimeToStr(now-start),true);
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
      if (relatedPlot^.animation.frameCount<>0) and (animationFrameIndex>=0) and (animationFrameIndex<relatedPlot^.animation.frameCount)
      then currentScalingOptions:=relatedPlot^.animation.options[animationFrameIndex]
      else currentScalingOptions:=relatedPlot^.currentPlot.options;
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
      i:=0;
      while i<relatedPlot^.animation.frameCount do begin
        currentScalingOptions:=relatedPlot^.animation.options[i];
        updateCurrent;
        relatedPlot^.animation.options[i]:=currentScalingOptions;
        inc(i);
      end;
      pullPlotSettingsToGui();
      relatedPlot^.logPlotChanged;
    finally
      relatedPlot^.doneGuiInteraction;
    end;
  end;

PROCEDURE TplotForm.updateInteractiveSection;
  VAR hasInteractiveAnimation:boolean=false;
      hasVolatileAnimation   :boolean=false;
  begin
    if (relatedPlot^.animation.frameCount>0) then begin
      if relatedPlot^.animation.isSeriesVolatile
      then hasVolatileAnimation   :=true
      else hasInteractiveAnimation:=true;
    end;
    AnimationGroupBox.visible:=hasInteractiveAnimation or hasVolatileAnimation;
    AnimationGroupBox.enabled:=hasInteractiveAnimation or hasVolatileAnimation;
    if hasInteractiveAnimation then begin
      AnimationGroupBox.AutoSize:=true;
      cycleCheckbox.enabled:=true;
      frameTrackBar.enabled:=true;
    end else if hasVolatileAnimation then begin
      AnimationGroupBox.AutoSize:=true;
      cycleCheckbox.checked:=false;
      cycleCheckbox.enabled:=false;
      frameTrackBar.enabled:=false;
    end else begin
      AnimationGroupBox.AutoSize:=false;
      AnimationGroupBox.height:=0;
      animateCheckBox.checked:=false;
    end;
  end;

PROCEDURE TplotForm.doPlot;
  begin
    if relatedPlot=nil then exit;
    if relatedPlot^.canStartGuiInteraction then
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
        pullPlotSettingsToGui();
      end;
    finally
      relatedPlot^.doneGuiInteraction;
    end;
  end;

{$i func_defines.inc}
FUNCTION plotClosedByUser_impl intFuncSignature;
  VAR closedRequest:P_queryPlotClosedMessage;
  begin if (params=nil) or (params^.size=0) then begin
    if (gui_started=NO) then context^.messages^.logGuiNeeded;
    if not(se_input in context^.sideEffectWhitelist) then exit(newBoolLiteral(false));
    new(closedRequest,createRetrieveRequest);
    context^.messages^.postCustomMessage(closedRequest);
    result:=newBoolLiteral(closedRequest^.getResponseWaiting(context^.messages));
    disposeMessage(closedRequest);
  end else result:=nil; end;

FUNCTION clearPlotAnim_impl intFuncSignature;
  begin
    if not(context^.checkSideEffects('clearAnimation',tokenLocation,[se_alterGuiState])) then exit(nil);
    if (params=nil) or (params^.size=0) then begin
      if (gui_started=NO) then context^.messages^.logGuiNeeded;
      result:=newVoidLiteral;
      context^.messages^.postSingal(mt_plot_clearAnimation,C_nilSearchTokenLocation);
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_boolean) then begin
      if (gui_started=NO) then context^.messages^.logGuiNeeded;
      result:=newVoidLiteral;
      if bool0^.value
      then context^.messages^.postSingal(mt_plot_clearAnimationVolatile,C_nilSearchTokenLocation)
      else context^.messages^.postSingal(mt_plot_clearAnimation        ,C_nilSearchTokenLocation);
    end else result:=nil;
  end;

FUNCTION addAnimFrame_impl intFuncSignature;
  VAR request:P_plotAddAnimationFrameRequest;
      sleepInSeconds:double;
  begin
    if not(context^.checkSideEffects('clearAnimation',tokenLocation,[se_alterGuiState])) then exit(nil);
    if (params=nil) or (params^.size=0) then begin
      if (gui_started=NO) then context^.messages^.logGuiNeeded;
      new(request,create());
      context^.messages^.postCustomMessage(request);
      sleepInSeconds:=request^.getProposedSleepTime(context^.messages);
      disposeMessage(request);
      if sleepInSeconds>0 then begin
        {$ifdef debugMode}
        writeln(stdErr,'after adding animation frame I sleep for ',sleepInSeconds:0:5,' seconds');
        {$endif}
        sleep(round(1000*sleepInSeconds));
      end;
      result:=newVoidLiteral;
    end else result:=nil;
  end;

FUNCTION display_imp intFuncSignature;
  VAR displayRequest:P_plotDisplayRequest;
  begin if ((params=nil) or (params^.size=0)) and context^.checkSideEffects('display',tokenLocation,[se_alterGuiState]) then begin
    if (gui_started=NO) then context^.messages^.logGuiNeeded;
    new(displayRequest,create());
    context^.messages^.postCustomMessage(displayRequest);
    displayRequest^.waitForExecution(context^.messages);
    disposeMessage(displayRequest);
    result:=newVoidLiteral;
  end else result:=nil; end;

FUNCTION postdisplay_imp intFuncSignature;
  VAR displayRequest:P_plotDisplayRequest;
  begin
    if not(context^.checkSideEffects('postDisplay',tokenLocation,[se_alterGuiState])) then exit(nil);
    if (params=nil) or (params^.size=0) then begin
      if (gui_started=NO) then context^.messages^.logGuiNeeded;
      new(displayRequest,create());
      context^.messages^.postCustomMessage(displayRequest,true);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

PROCEDURE initializePlotForm(CONST coordLabel:TLabel);
  begin
    mainFormCoordinatesLabel:=coordLabel;
  end;

INITIALIZATION
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'plotClosed'       ,@plotClosedByUser_impl,ak_nullary,'plotClosed;//Returns true if the plot has been closed by user interaction',[se_readGuiState]);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'clearAnimation'   ,@clearPlotAnim_impl   ,ak_variadic,'clearAnimation;//Clears the animated plot#clearAnimation(true);//Clears the animated plot and switches to volatile mode',[se_alterGuiState]);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'addAnimationFrame',@addAnimFrame_impl    ,ak_nullary,'addAnimationFrame;//Adds the current plot to the animation',[se_alterGuiState]);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'display'          ,@display_imp          ,ak_nullary,'display;//Displays the plot as soon as possible and waits for execution',[se_alterGuiState]);
  builtinFunctionMap.registerRule(PLOT_NAMESPACE,'postDisplay'      ,@postdisplay_imp      ,ak_nullary,'display;//Displays the plot as soon as possible and returns immediately',[se_alterGuiState]);

end.

