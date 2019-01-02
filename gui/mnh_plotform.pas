UNIT mnh_plotForm;

{$mode objfpc}{$H+}

INTERFACE
{$WARN 5024 OFF}
USES
  Classes, sysutils, FileUtil,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ComCtrls, StdCtrls,
  mnh_constants, basicTypes,
  mnhFormHandler,
  mnh_messages,
  recyclers,
  mnh_plotData, mnh_settings, out_adapters, litVar, funcs,
  contexts, evalThread, plotstyles, plotMath, EpikTimer,
  plotExport;

TYPE
  PMouseEvent=PROCEDURE(CONST realPoint:T_point) of object;

  TplotForm = class(TForm)
    animateCheckBox: TCheckBox;
    cycleCheckbox: TCheckBox;
    AnimationGroupBox: TGroupBox;
    animationFPSLabel: TLabel;
    frameIndexLabel: TLabel;
    exportingGroupBox: TGroupBox;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    miScriptFromAnimation: TMenuItem;
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
    plotImage: TImage;
    exportingProgressBar: TProgressBar;
    StatusBar: TStatusBar;
    animationSpeedTrackbar: TTrackBar;
    frameTrackBar: TTrackBar;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormKeyPress(Sender: TObject; VAR key: char);
    PROCEDURE FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE frameTrackBarChange(Sender: TObject);
    PROCEDURE miAntiAliasing1Click(Sender: TObject);
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
  private
    animationFrameIndex:longint;
    fpsSamplingStart:double;
    framesSampled:longint;
    closedByUser:boolean;
    eTimer:TEpikTimer;
    mouseUpTriggersPlot:boolean;
    lastMouseX,lastMouseY:longint;

    FUNCTION getPlotQuality:byte;
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

FUNCTION plotForm: TplotForm;
FUNCTION plotFormIsInitialized:boolean;
PROCEDURE resetPlot(CONST hideWindow:boolean);
PROCEDURE initializePlotForm(CONST mainForm:T_abstractMnhForm);
VAR plotSystem:T_plotSystem;
    currentlyExporting:boolean=false;
IMPLEMENTATION
VAR myPlotForm:TplotForm=nil;
    main:T_abstractMnhForm;

FUNCTION plotForm: TplotForm;
  begin
    if myPlotForm=nil then begin
      myPlotForm:=TplotForm.create(nil);
      myPlotForm.pullPlotSettingsToGui();
      registerForm(myPlotForm,ft_plot);
    end;
    result:=myPlotForm;
  end;

FUNCTION plotFormIsInitialized:boolean;
  begin
    result:=myPlotForm<>nil;
  end;

PROCEDURE resetPlot(CONST hideWindow:boolean);
  begin
    if myPlotForm=nil then exit;
    myPlotForm.closedByUser:=false;
    myPlotForm.onPlotRescale   :=nil;
    myPlotForm.onPlotMouseClick:=nil;
    myPlotForm.onPlotMouseMove :=nil;
    if hideWindow then begin
      myPlotForm.Hide;
      unregisterForm(myPlotForm);
    end;
  end;

{$R *.lfm}
PROCEDURE TplotForm.FormKeyPress(Sender: TObject; VAR key: char);
  begin
    if (key in ['+','-']) then begin
      plotSystem.startGuiInteraction;
      if key='+' then plotSystem.currentPlot.zoomOnPoint(lastMouseX,lastMouseY,  0.9,plotImage)
                 else plotSystem.currentPlot.zoomOnPoint(lastMouseX,lastMouseY,1/0.9,plotImage);
      pullPlotSettingsToGui();
      doPlot();
      plotSystem.doneGuiInteraction;
      if Assigned(onPlotRescale) then onPlotRescale(Sender);
    end;
  end;

PROCEDURE TplotForm.FormCreate(Sender: TObject);
  begin
    miAutoReset.checked:=settings.doResetPlotOnEvaluation;
    miCacheFrames.checked:=settings.cacheAnimationFrames;
    fpsSamplingStart:=now;
    framesSampled:=0;
    closedByUser:=false;
    onPlotRescale:=nil;
    onPlotMouseMove:=nil;
    onPlotMouseClick:=nil;
    plotSystem.registerPlotForm(@pullPlotSettingsToGui);
    eTimer:=TEpikTimer.create(self);
    eTimer.clear;
    eTimer.start;
    if not(anyFormShowing(ft_main)) then ShowInTaskBar:=stAlways;
  end;

PROCEDURE TplotForm.FormDestroy(Sender: TObject);
  begin
  end;

PROCEDURE TplotForm.FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=9) and (ssCtrl in Shift) then formCycle(self,ssShift in Shift);
  end;

PROCEDURE TplotForm.FormResize(Sender: TObject);
  begin
    plotImage.Align:=alClient;
    doPlot();
  end;

PROCEDURE TplotForm.FormShow(Sender: TObject);
  begin
    {$ifdef UNIX}
    miIncFontSize.ShortCut:=16605;
    {$endif}
    position:=poDefault;
    registerForm(myPlotForm,ft_plot);
    if anyFormShowing(ft_main)
    then ShowInTaskBar:=stDefault
    else ShowInTaskBar:=stAlways;
  end;

PROCEDURE TplotForm.frameTrackBarChange(Sender: TObject);
  begin
    if animationFrameIndex=frameTrackBar.position then exit;
    animationFrameIndex:=frameTrackBar.position;
    plotSystem.startGuiInteraction;
    plotSystem.animation.getFrame(plotImage,animationFrameIndex,getPlotQuality);
    plotSystem.doneGuiInteraction;
    animateCheckBox.checked:=false;
  end;

PROCEDURE TplotForm.miAntiAliasing1Click(Sender: TObject);
  begin
    doPlot;
  end;

PROCEDURE TplotForm.miAutoResetClick(Sender: TObject);
  begin
    miAutoReset.checked:=not(miAutoReset.checked);
    settings.doResetPlotOnEvaluation:=miAutoReset.checked;
  end;

PROCEDURE TplotForm.miAutoscaleXClick(Sender: TObject);
  begin
    miAutoscaleX.checked:=not(miAutoscaleX.checked);
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miAutoscaleYClick(Sender: TObject);
  begin
    miAutoscaleY.checked:=not(miAutoscaleY.checked);
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miCacheFramesClick(Sender: TObject);
  begin
    miCacheFrames.checked:=not(miCacheFrames.checked);
    settings.cacheAnimationFrames:=miCacheFrames.checked;
  end;

PROCEDURE TplotForm.miDecFontSizeClick(Sender: TObject);
  begin
    pushFontSizeToPlotContainer(plotSystem.currentPlot.options.relativeFontSize/1.1);
  end;

PROCEDURE TplotForm.miIncFontSizeClick(Sender: TObject);
  begin
    pushFontSizeToPlotContainer(plotSystem.currentPlot.options.relativeFontSize*1.1);
  end;

PROCEDURE TplotForm.miLogscaleXClick(Sender: TObject);
  begin
    miLogscaleX.checked:=not(miLogscaleX.checked);
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miLogscaleYClick(Sender: TObject);
  begin
    miLogscaleY.checked:=not(miLogscaleY.checked);
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miPreserveAspectClick(Sender: TObject);
  begin
    miPreserveAspect.checked:=not(miPreserveAspect.checked);
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miRenderToFileClick(Sender: TObject);
  VAR frameIndex:longint;
  begin
    plotSystem.startGuiInteraction;
    if exportPlotForm.showModalFor(plotSystem.animation.frameCount>0)=mrOk then begin
      exportingGroupBox.width:=round(ClientWidth*0.9);
      exportingGroupBox.Left:=round(ClientWidth*0.05);
      exportingGroupBox.visible:=true;
      enabled:=false;
      currentlyExporting:=true;
      exportingProgressBar.position:=0;
      if (exportPlotForm.rbExportAll.checked) and (plotSystem.animation.frameCount>0) then begin
        exportingProgressBar.max:=plotSystem.animation.frameCount;
        for frameIndex:=0 to plotSystem.animation.frameCount-1 do begin
          exportingProgressBar.position:=frameIndex;
          Application.ProcessMessages;
          plotSystem.animation.renderFrame(frameIndex,
                                exportPlotForm.animationFileName(
                                     frameIndex,
                                     plotSystem.animation.frameCount),
                                exportPlotForm.renderWidth,
                                exportPlotForm.renderHeight,
                                exportPlotForm.QualityTrackbar.position,true);
        end;
      end else begin
        exportingProgressBar.max:=1;
        if (plotSystem.animation.frameCount>0)
        then plotSystem.animation.renderFrame(
               animationFrameIndex,
               exportPlotForm.OutputFileNameEdit.caption,
               exportPlotForm.renderWidth,
               exportPlotForm.renderHeight,
               exportPlotForm.QualityTrackbar.position,false)
        else plotSystem.currentPlot.renderToFile(
               exportPlotForm.OutputFileNameEdit.caption,
               exportPlotForm.renderWidth,
               exportPlotForm.renderHeight,
               exportPlotForm.QualityTrackbar.position);
        exportingProgressBar.position:=1;
        Application.ProcessMessages;
      end;
      exportingGroupBox.visible:=false;
      enabled:=true;
      currentlyExporting:=false;
    end;
    plotSystem.doneGuiInteraction;
  end;

PROCEDURE TplotForm.miCreateScriptClick(Sender: TObject);
  begin
    miScriptFromAnimation.enabled:=plotSystem.animation.frameCount>0;
  end;

PROCEDURE TplotForm.miScriptFromAnimationClick(Sender: TObject);
  VAR task:P_editScriptTask;
  begin
    new(task,createForNewEditor(plotSystem.getPlotStatement(-1)));
    main.onEditFinished(task);
  end;

PROCEDURE TplotForm.miScriptFromFrameClick(Sender: TObject);
  VAR task:P_editScriptTask;
  begin
    new(task,createForNewEditor(plotSystem.getPlotStatement(animationFrameIndex)));
    main.onEditFinished(task);
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
    miXTics.checked:=not(miXTics.checked);
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
    miYTics.checked:=not(miYTics.checked);
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.plotImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if ssLeft in Shift then begin
      lastMouseX:=x;
      lastMouseY:=y;
      if Assigned(onPlotMouseClick) then onPlotMouseClick(plotSystem.currentPlot.options.screenToReal(x,y));
    end;
  end;

PROCEDURE TplotForm.plotImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  VAR p:T_point;
  begin
    if (animationFrameIndex>=0) and (animationFrameIndex<plotSystem.animation.frameCount)
    then p:=plotSystem.animation  .options[animationFrameIndex].screenToReal(x,y)
    else p:=plotSystem.currentPlot.options                     .screenToReal(x,y);
    StatusBar.SimpleText:='x='+floatToStr(p[0])+'; y='+floatToStr(p[1]);
    if (ssLeft in Shift) and (plotSystem.animation.frameCount=0) then begin
      if (x<>lastMouseX) or (y<>lastMouseY) then begin
        plotSystem.startGuiInteraction;
        plotSystem.currentPlot.panByPixels(lastMouseX-x,lastMouseY-y,plotImage);
        plotSystem.doneGuiInteraction;
        mouseUpTriggersPlot:=true;
      end;
    end else if Assigned(onPlotMouseMove) then onPlotMouseMove(p);
    lastMouseX:=x;
    lastMouseY:=y;
  end;

PROCEDURE TplotForm.plotImageMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
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
    closedByUser:=true;
    animateCheckBox.checked:=false;
  end;

FUNCTION TplotForm.getPlotQuality: byte;
  begin
    if      miAntiAliasing4.checked then result:=PLOT_QUALITY_HIGH
    else if miAntiAliasing3.checked then result:=PLOT_QUALITY_MEDIUM_2
    else if miAntiAliasing2.checked then result:=PLOT_QUALITY_MEDIUM_1
    else                                 result:=PLOT_QUALITY_LOW;
  end;

PROCEDURE TplotForm.pullPlotSettingsToGui;
  VAR currentScalingOptions:T_scalingOptions;
  begin
    plotSystem.startGuiInteraction;
    currentScalingOptions:=plotSystem.currentPlot.options;
    plotSystem.doneGuiInteraction;
    miXTics.checked         :=gse_tics       in currentScalingOptions.axisStyle['x'];
    miXGrid.checked         :=gse_coarseGrid in currentScalingOptions.axisStyle['x'];
    miXFinerGrid.checked    :=gse_fineGrid   in currentScalingOptions.axisStyle['x'];
    miYTics.checked         :=gse_tics       in currentScalingOptions.axisStyle['y'];
    miYGrid.checked         :=gse_coarseGrid in currentScalingOptions.axisStyle['y'];
    miYFinerGrid.checked    :=gse_fineGrid   in currentScalingOptions.axisStyle['y'];
    miPreserveAspect.checked:=currentScalingOptions.preserveAspect;
    miAutoscaleX.checked    :=currentScalingOptions.axisTrafo['x'].autoscale;
    miAutoscaleY.checked    :=currentScalingOptions.axisTrafo['y'].autoscale;
    miLogscaleX.checked     :=currentScalingOptions.axisTrafo['x'].logscale;
    miLogscaleY.checked     :=currentScalingOptions.axisTrafo['y'].logscale;
  end;

PROCEDURE TplotForm.pushFontSizeToPlotContainer(CONST newSize:double);
  VAR currentScalingOptions:T_scalingOptions;
      i:longint;
  begin
    plotSystem.startGuiInteraction;
    currentScalingOptions:=plotSystem.currentPlot.options;
    currentScalingOptions.relativeFontSize:=newSize;
    plotSystem.currentPlot.options:=currentScalingOptions;
    pullPlotSettingsToGui();
    i:=0;
    while i<plotSystem.animation.frameCount do begin
      currentScalingOptions:=plotSystem.animation.options[i];
      currentScalingOptions.relativeFontSize:=newSize;
      plotSystem.animation.options[i]:=currentScalingOptions;
      inc(i);
    end;
    doPlot;
    plotSystem.doneGuiInteraction;
  end;

PROCEDURE TplotForm.pushSettingsToPlotContainer;
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
    plotSystem.startGuiInteraction;
    currentScalingOptions:=plotSystem.currentPlot.options;
    updateCurrent;
    plotSystem.currentPlot.options:=currentScalingOptions;
    pullPlotSettingsToGui();
    i:=0;
    while i<plotSystem.animation.frameCount do begin
      currentScalingOptions:=plotSystem.animation.options[i];
      updateCurrent;
      plotSystem.animation.options[i]:=currentScalingOptions;
      inc(i);
    end;
    doPlot;
    plotSystem.doneGuiInteraction;
  end;

PROCEDURE TplotForm.doPlot;
  PROCEDURE updateInteractiveSection;
    begin
      AnimationGroupBox.visible:=(plotSystem.animation.frameCount>0);
      AnimationGroupBox.enabled:=(plotSystem.animation.frameCount>0);
      if plotSystem.animation.frameCount>0 then begin
        AnimationGroupBox.AutoSize:=true;
      end else begin
        AnimationGroupBox.AutoSize:=false;
        AnimationGroupBox.height:=0;
      end;
    end;

  begin
    if not(showing) then Show;
    plotSystem.startGuiInteraction;
    updateInteractiveSection;
    if plotSystem.animation.frameCount<>0 then begin
      plotSystem.animation.getFrame(plotImage,animationFrameIndex,getPlotQuality);
      plotSystem.doneGuiInteraction;
      exit;
    end;
    plotSystem.currentPlot.renderPlot(plotImage,getPlotQuality);
    plotSystem.logPlotDone;
    plotSystem.doneGuiInteraction;
  end;

FUNCTION TplotForm.timerTick:boolean;
  FUNCTION frameInterval:longint;
    CONST intendedMsPerFrame:array[0..10] of longint=(2000,1000,500,200,100,50,30,20,15,10,1);
    begin
      result:=intendedMsPerFrame[animationSpeedTrackbar.position];
    end;

  begin
    result:=false;
    plotSystem.startGuiInteraction;
    if gui_started and (showing) and (plotSystem.animation.frameCount>0) then begin
      if animateCheckBox.checked and (round(eTimer.elapsed*1000)>=frameInterval) and plotSystem.animation.nextFrame(animationFrameIndex,cycleCheckbox.checked,plotImage.width,plotImage.height,getPlotQuality) then begin
        eTimer.clear;
        eTimer.start;
        plotSystem.animation.getFrame(plotImage,animationFrameIndex,getPlotQuality);
        inc(framesSampled);
        if (framesSampled>10) or (now-fpsSamplingStart>1/(24*60*60)) then begin
          animationFPSLabel.caption:=intToStr(round(framesSampled/((now-fpsSamplingStart)*24*60*60)))+'fps';
          fpsSamplingStart:=now;
          framesSampled:=0;
        end;
        result:=true;
      end;
      frameTrackBar.max:=plotSystem.animation.frameCount-1;
      frameTrackBar.position:=animationFrameIndex;
      frameIndexLabel.caption:=intToStr(animationFrameIndex);
      if frameTrackBar.max>90 then frameTrackBar.frequency:=10 else
      if frameTrackBar.max>20 then frameTrackBar.frequency:= 5
                              else frameTrackBar.frequency:= 1;
    end else result:=false;
    plotSystem.doneGuiInteraction;
  end;

FUNCTION TplotForm.wantTimerInterval: longint;
  begin
    if animateCheckBox.checked then result:=1
                               else result:=50;
  end;

{$i func_defines.inc}
FUNCTION plotClosedByUser_impl intFuncSignature;
  begin if (params=nil) or (params^.size=0) then begin
    result:=newBoolLiteral((myPlotForm<>nil) and myPlotForm.closedByUser);
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

PROCEDURE initializePlotForm(CONST mainForm:T_abstractMnhForm);
  begin
    reregisterRule(PLOT_NAMESPACE,'plotClosed'       ,@plotClosedByUser_impl);
    reregisterRule(PLOT_NAMESPACE,'clearAnimation'   ,@clearPlotAnim_impl   );
    reregisterRule(PLOT_NAMESPACE,'addAnimationFrame',@addAnimFrame_impl    );
    reregisterRule(PLOT_NAMESPACE,'display'          ,@display_imp          );
    reregisterRule(PLOT_NAMESPACE,'postDisplay'      ,@postdisplay_imp      );
    main:=mainForm;
  end;

PROCEDURE executePlot;
  begin
    plotForm.doPlot;
  end;

INITIALIZATION
  registerRule(PLOT_NAMESPACE,'plotClosed'       ,@uninitialized_fallback,ak_nullary,'plotClosed;//Returns true if the plot has been closed by user interaction');
  registerRule(PLOT_NAMESPACE,'clearAnimation'   ,@uninitialized_fallback,ak_nullary,'clearAnimation;//Clears the animated plot');
  registerRule(PLOT_NAMESPACE,'addAnimationFrame',@uninitialized_fallback,ak_nullary,'addAnimationFrame;//Adds the current plot to the animation');
  registerRule(PLOT_NAMESPACE,'display'          ,@uninitialized_fallback,ak_nullary,'display;//Displays the plot as soon as possible and waits for execution');
  registerRule(PLOT_NAMESPACE,'postDisplay'      ,@uninitialized_fallback,ak_nullary,'display;//Displays the plot as soon as possible and returns immediately');
  plotSystem.create(@executePlot)
FINALIZATION
  if myPlotForm<>nil then FreeAndNil(myPlotForm);
  plotSystem.destroy;

end.

