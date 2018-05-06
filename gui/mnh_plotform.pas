UNIT mnh_plotForm;

{$mode objfpc}{$H+}

INTERFACE
{$WARN 5024 OFF}
USES
  Classes, sysutils, FileUtil,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ComCtrls, StdCtrls,
  mnh_constants, mnh_basicTypes,
  mnhFormHandler,
  mnh_plotData, mnh_settings, mnh_out_adapters, mnh_litVar, mnh_funcs,
  mnh_contexts, mnh_evalThread, plotstyles, plotMath,
  plotExport;

TYPE
  PMouseEvent=PROCEDURE(CONST realPoint:T_point) of object;

  TplotForm = class(TForm)
    animateCheckBox: TCheckBox;
    cycleCheckbox: TCheckBox;
    AnimationGroupBox: TGroupBox;
    animationFPSLabel: TLabel;
    frameIndexLabel: TLabel;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
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
  private
    animationFrameIndex:longint;
    tempPlot:T_plot;
    tempPlotShown:boolean;
    animation:T_plotSeries;
    fpsSamplingStart:double;
    framesSampled:longint;
    closedByUser:boolean;
    FUNCTION getPlotQuality:byte;
  public
    onPlotRescale:TNotifyEvent;
    onPlotMouseMove,
    onPlotMouseClick:PMouseEvent;
    PROCEDURE pullPlotSettingsToGui();
    PROCEDURE pushSettingsToPlotContainer();
    PROCEDURE doPlot(CONST useTemporary:boolean=false);
    PROCEDURE doOrPostPlot();
    FUNCTION timerTick:boolean;
    FUNCTION wantTimerInterval:longint;
  end;

VAR guiAdapters:P_adapters;

FUNCTION plotForm: TplotForm;
FUNCTION plotFormIsInitialized:boolean;
PROCEDURE resetPlot(CONST hideWindow:boolean);
PROCEDURE initializePlotForm;
IMPLEMENTATION
VAR plotSubsystem:record
      mouseUpTriggersPlot:boolean;
      lastMouseX,lastMouseY:longint;
    end;
    myPlotForm:TplotForm=nil;

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
    myPlotForm.animation.clear;
    myPlotForm.tempPlot.clear;
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
      if key='+' then guiAdapters^.plot^.zoomOnPoint(plotSubsystem.lastMouseX,plotSubsystem.lastMouseY,  0.9,plotImage)
                 else guiAdapters^.plot^.zoomOnPoint(plotSubsystem.lastMouseX,plotSubsystem.lastMouseY,1/0.9,plotImage);
      pullPlotSettingsToGui();
      doOrPostPlot();
      if Assigned(onPlotRescale) then onPlotRescale(Sender);
    end;
  end;

PROCEDURE TplotForm.FormCreate(Sender: TObject);
  begin
    miAutoReset.checked:=settings.value^.doResetPlotOnEvaluation;
    tempPlot.createWithDefaults;
    tempPlotShown:=false;
    animation.create;
    fpsSamplingStart:=now;
    framesSampled:=0;
    closedByUser:=false;
    onPlotRescale:=nil;
    onPlotMouseMove:=nil;
    onPlotMouseClick:=nil;
  end;

PROCEDURE TplotForm.FormDestroy(Sender: TObject);
  begin
    tempPlot.destroy;
    animation.destroy;
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
  end;

PROCEDURE TplotForm.frameTrackBarChange(Sender: TObject);
  begin
    if animationFrameIndex=frameTrackBar.position then exit;
    animationFrameIndex:=frameTrackBar.position;
    animation.getFrame(plotImage,animationFrameIndex,getPlotQuality);
  end;

PROCEDURE TplotForm.miAntiAliasing1Click(Sender: TObject);
  begin
    doOrPostPlot();
  end;

PROCEDURE TplotForm.miAutoResetClick(Sender: TObject);
  begin
    miAutoReset.checked:=not(miAutoReset.checked);
    settings.value^.doResetPlotOnEvaluation:=miAutoReset.checked;
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

PROCEDURE TplotForm.miDecFontSizeClick(Sender: TObject);
  VAR o:T_scalingOptions;
  begin
    o:=guiAdapters^.plot^.options;
    o.relativeFontSize:=o.relativeFontSize/1.1;
    guiAdapters^.plot^.options:=o;
    doOrPostPlot();
  end;

PROCEDURE TplotForm.miIncFontSizeClick(Sender: TObject);
  VAR o:T_scalingOptions;
  begin
    o:=guiAdapters^.plot^.options;
    o.relativeFontSize:=o.relativeFontSize*1.1;
    guiAdapters^.plot^.options:=o;
    doOrPostPlot();
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
    if exportPlotForm.showModalFor(animation.frameCount>0)=mrOk then begin
      if (exportPlotForm.rbExportAll.checked) and (animation.frameCount>0) then begin
        for frameIndex:=0 to animation.frameCount-1 do begin
          animation.renderFrame(frameIndex,
                                exportPlotForm.animationFileName(
                                     frameIndex,
                                     animation.frameCount),
                                exportPlotForm.renderWidth,
                                exportPlotForm.renderHeight,
                                exportPlotForm.QualityTrackbar.position);
        end;
      end else begin
        if (animation.frameCount>0)
        then animation.renderFrame          (animationFrameIndex,
                                             exportPlotForm.OutputFileNameEdit.caption,
                                             exportPlotForm.renderWidth,
                                             exportPlotForm.renderHeight,
                                             exportPlotForm.QualityTrackbar.position)
        else guiAdapters^.plot^.renderToFile(exportPlotForm.OutputFileNameEdit.caption,
                                             exportPlotForm.renderWidth,
                                             exportPlotForm.renderHeight,
                                             exportPlotForm.QualityTrackbar.position);
      end;
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
      plotSubsystem.lastMouseX:=x;
      plotSubsystem.lastMouseY:=y;
      if Assigned(onPlotMouseClick) then onPlotMouseClick(guiAdapters^.plot^.options.screenToReal(x,y));
    end;
  end;

PROCEDURE TplotForm.plotImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  VAR p:T_point;
  begin
    if (animationFrameIndex>=0) and (animationFrameIndex<animation.frameCount)
    then p:=animation.options[animationFrameIndex].screenToReal(x,y)
    else if tempPlotShown then p:=tempPlot.options.screenToReal(x,y)
                else p:=guiAdapters^.plot^.options.screenToReal(x,y);
    StatusBar.SimpleText:='x='+floatToStr(p[0])+'; y='+floatToStr(p[1]);
    if ssLeft in Shift then with plotSubsystem do begin
      if (x<>lastMouseX) or (y<>lastMouseY) then begin
        guiAdapters^.plot^.panByPixels(lastMouseX-x,lastMouseY-y,plotImage);
        guiAdapters^.plot^.options:=guiAdapters^.plot^.options;
        mouseUpTriggersPlot:=true;
      end;
    end else if Assigned(onPlotMouseMove) then onPlotMouseMove(p);
    with plotSubsystem do begin
      lastMouseX:=x;
      lastMouseY:=y;
    end;
  end;

PROCEDURE TplotForm.plotImageMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    with plotSubsystem do if mouseUpTriggersPlot then begin
      pullPlotSettingsToGui();
      if Assigned(onPlotRescale) then onPlotRescale(Sender);
      lastMouseX:=x;
      lastMouseY:=y;
      doOrPostPlot();
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
    currentScalingOptions:=guiAdapters^.plot^.options;
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
    currentScalingOptions:=guiAdapters^.plot^.options;
    updateCurrent;
    guiAdapters^.plot^.options:=currentScalingOptions;
    if tempPlotShown then begin
      currentScalingOptions:=tempPlot.options;
      updateCurrent;
      tempPlot.options:=currentScalingOptions;
    end;
    pullPlotSettingsToGui();
    if animation.frameCount<=0 then doOrPostPlot();
    i:=0;
    while i<animation.frameCount do begin
      currentScalingOptions:=animation.options[i];
      updateCurrent;
      animation.options[i]:=currentScalingOptions;
      inc(i);
    end;
  end;

PROCEDURE TplotForm.doPlot(CONST useTemporary: boolean);
  PROCEDURE updateInteractiveSection;
    begin
      AnimationGroupBox.visible:=(animation.frameCount>0);
      AnimationGroupBox.enabled:=(animation.frameCount>0);
      if animation.frameCount>0 then begin
        AnimationGroupBox.AutoSize:=true;
      end else begin
        AnimationGroupBox.AutoSize:=false;
        AnimationGroupBox.height:=0;
      end;
    end;

  begin
    if not(showing) then Show;
    updateInteractiveSection;
    if animation.frameCount<>0 then begin
      animation.getFrame(plotImage,animationFrameIndex,getPlotQuality);
      guiAdapters^.resetFlagsAfterPlotDone;
      exit;
    end;
    if useTemporary then begin
      tempPlot.renderPlot(plotImage,getPlotQuality);
      tempPlotShown:=true;
      guiAdapters^.resetFlagsAfterPlotDone;
    end else begin
      guiAdapters^.plot^.renderPlot(plotImage,getPlotQuality);
      tempPlotShown:=false;
      guiAdapters^.resetFlagsAfterPlotDone;
    end;
  end;

PROCEDURE TplotForm.doOrPostPlot;
  begin
    if runEvaluator.evaluationRunning
    then begin
      if tempPlotShown then doPlot(true);
      guiAdapters^.logDeferredPlot;
    end else doPlot();
  end;

FUNCTION TplotForm.timerTick:boolean;
  begin
    result:=false;
    if gui_started and (showing) and (animation.frameCount>0) then begin
      if animateCheckBox.checked and animation.nextFrame(animationFrameIndex,cycleCheckbox.checked) then begin
        animation.getFrame(plotImage,animationFrameIndex,getPlotQuality);
        inc(framesSampled);
        if (framesSampled>10) or (now-fpsSamplingStart>1/(24*60*60)) then begin
          animationFPSLabel.caption:=intToStr(round(framesSampled/((now-fpsSamplingStart)*24*60*60)))+'fps';
          fpsSamplingStart:=now;
          framesSampled:=0;
        end;
        result:=true;
      end;
      frameTrackBar.max:=animation.frameCount-1;
      frameTrackBar.position:=animationFrameIndex;
      frameIndexLabel.caption:=intToStr(animationFrameIndex);
      if frameTrackBar.max>20 then frameTrackBar.frequency:=5
                              else frameTrackBar.frequency:=1;
    end else result:=false;
  end;

FUNCTION TplotForm.wantTimerInterval: longint;
  CONST intendedMsPerFrame:array[0..10] of longint=(2000,1000,500,200,100,50,30,20,15,10,1);
  begin
    if animateCheckBox.checked then result:=intendedMsPerFrame[animationSpeedTrackbar.position]
                               else result:=50;
  end;

{$i mnh_func_defines.inc}
FUNCTION plotClosedByUser_impl intFuncSignature;
  begin if (params=nil) or (params^.size=0) then begin
    result:=newBoolLiteral((myPlotForm<>nil) and myPlotForm.closedByUser);
  end else result:=nil; end;

FUNCTION clearPlotAnim_impl intFuncSignature;
  begin if (params=nil) or (params^.size=0) then begin
    result:=newVoidLiteral;
    plotForm.animation.clear;
  end else result:=nil; end;

FUNCTION addAnimFrame_impl intFuncSignature;
  begin if (params=nil) or (params^.size=0) then begin
    result:=newVoidLiteral;
    plotForm.animation.addFrame(context.adapters^.plot^);
  end else result:=nil; end;

FUNCTION display_imp intFuncSignature;
  begin if (params=nil) or (params^.size=0) then begin
    context.adapters^.logInstantPlot;
    plotForm.tempPlot.copyFrom(context.adapters^.plot^);
    result:=newVoidLiteral;
  end else result:=nil; end;

FUNCTION uninitialized_fallback intFuncSignature;
  begin
    context.adapters^.hasNeedGUIerror:=true;
    result:=nil;
  end;

PROCEDURE initializePlotForm;
  begin
    reregisterRule(PLOT_NAMESPACE,'plotClosed'       ,@plotClosedByUser_impl);
    reregisterRule(PLOT_NAMESPACE,'clearAnimation'   ,@clearPlotAnim_impl   );
    reregisterRule(PLOT_NAMESPACE,'addAnimationFrame',@addAnimFrame_impl    );
    reregisterRule(PLOT_NAMESPACE,'display'          ,@display_imp          );
  end;

INITIALIZATION
  registerRule(PLOT_NAMESPACE,'plotClosed'       ,@uninitialized_fallback,ak_nullary,'plotClosed;//Returns true if the plot has been closed by user interaction');
  registerRule(PLOT_NAMESPACE,'clearAnimation'   ,@uninitialized_fallback,ak_nullary,'clearAnimation;//Clears the animated plot');
  registerRule(PLOT_NAMESPACE,'addAnimationFrame',@uninitialized_fallback,ak_nullary,'addAnimationFrame;//Adds the current plot to the animation');
  registerRule(PLOT_NAMESPACE,'display'          ,@uninitialized_fallback,ak_nullary,'display;//Displays the plot as soon as possible, even during evaluation.');

FINALIZATION
  if myPlotForm<>nil then FreeAndNil(myPlotForm);

end.

