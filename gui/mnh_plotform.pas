UNIT mnh_plotForm;

{$mode objfpc}{$H+}

INTERFACE
{$WARN 5024 OFF}
USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, ComCtrls, StdCtrls, mnhFormHandler, mnh_constants, mnh_basicTypes,
  mnh_plotData, mnh_settings, mnh_out_adapters, mnh_litVar, mnh_funcs,
  mnh_contexts, mnh_evalThread, dynamicPlotting, plotstyles, plotMath;

TYPE

  { TplotForm }

  TplotForm = class(TForm)
    animateCheckBox: TCheckBox;
    CustomEventButton0: TButton;
    ButtonLeaveInteractiveMode: TButton;
    CustomEventButton1: TButton;
    CustomEventButton2: TButton;
    CustomEventButton3: TButton;
    CustomEventButton4: TButton;
    CustomEventButton5: TButton;
    CustomEventButton6: TButton;
    CustomEventButton7: TButton;
    AnimationGroupBox: TGroupBox;
    InteractionPanel: TFlowPanel;
    InteractiveLabel: TLabel;
    animationFPSLabel: TLabel;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
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
    PROCEDURE ButtonLeaveInteractiveModeClick(Sender: TObject);
    PROCEDURE CustomEventButton0Click(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormKeyPress(Sender: TObject; VAR key: char);
    PROCEDURE FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE miAntiAliasing1Click(Sender: TObject);
    PROCEDURE miAutoResetClick(Sender: TObject);
    PROCEDURE miAutoscaleXClick(Sender: TObject);
    PROCEDURE miAutoscaleYClick(Sender: TObject);
    PROCEDURE miDecFontSizeClick(Sender: TObject);
    PROCEDURE miIncFontSizeClick(Sender: TObject);
    PROCEDURE miLogscaleXClick(Sender: TObject);
    PROCEDURE miLogscaleYClick(Sender: TObject);
    PROCEDURE miPreserveAspectClick(Sender: TObject);
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
    animation:T_plotSeries;
    fpsSamplingStart:double;
    framesSampled:longint;
    FUNCTION getPlotQuality:byte;
    { private declarations }
  public
    PROCEDURE pullPlotSettingsToGui();
    PROCEDURE pushSettingsToPlotContainer();
    PROCEDURE doPlot(CONST useTemporary:boolean=false);
    PROCEDURE doOrPostPlot();
    FUNCTION timerTick:boolean;
    FUNCTION wantTimerInterval:longint;
    { public declarations }
  end;

VAR guiAdapters:P_adapters;

FUNCTION plotForm: TplotForm;
FUNCTION plotFormIsInitialized:boolean;
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
      registerForm(myPlotForm,false,true);
    end;
    result:=myPlotForm;
  end;

FUNCTION plotFormIsInitialized:boolean;
  begin
    result:=myPlotForm<>nil;
  end;

{$R *.lfm}
PROCEDURE TplotForm.FormKeyPress(Sender: TObject; VAR key: char);
  begin
    if (key in ['+','-']) then begin
      if key='+' then guiAdapters^.plot^.zoomOnPoint(plotSubsystem.lastMouseX,plotSubsystem.lastMouseY,  0.9,plotImage)
                 else guiAdapters^.plot^.zoomOnPoint(plotSubsystem.lastMouseX,plotSubsystem.lastMouseY,1/0.9,plotImage);
      pullPlotSettingsToGui();
      doOrPostPlot();
      dynamicPlotting.postRescale;
    end;
  end;

PROCEDURE TplotForm.FormCreate(Sender: TObject);
  begin
    miAutoReset.Checked:=settings.value^.doResetPlotOnEvaluation;
    tempPlot.createWithDefaults;
    animation.create;
    fpsSamplingStart:=now;
    framesSampled:=0;
  end;

PROCEDURE TplotForm.FormDestroy(Sender: TObject);
  begin
    tempPlot.destroy;
    animation.destroy;
  end;

PROCEDURE TplotForm.ButtonLeaveInteractiveModeClick(Sender: TObject);
  begin
    postEndOfInteractiveMode;
    doOrPostPlot();
  end;

PROCEDURE TplotForm.CustomEventButton0Click(Sender: TObject);
  begin
    postCustomEvent(TButton(Sender).Tag);
  end;

PROCEDURE TplotForm.FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState
  );
  begin
    if (key=9) and (ssCtrl in Shift) then formCycle(self,ssShift in Shift);
  end;

PROCEDURE TplotForm.FormResize(Sender: TObject);
  begin
    plotImage.Align:=alClient;
    InteractionPanel.Align:=alTop;
    InteractionPanel.AutoSize:=true;
    doPlot();
  end;

PROCEDURE TplotForm.FormShow(Sender: TObject);
  begin
    {$ifdef UNIX}
    miIncFontSize.ShortCut:=16605;
    {$endif}
    position:=poDefault;
  end;

PROCEDURE TplotForm.miAntiAliasing1Click(Sender: TObject);
  begin
    doOrPostPlot();
  end;

PROCEDURE TplotForm.miAutoResetClick(Sender: TObject);
  begin
    miAutoReset.Checked:=not(miAutoReset.Checked);
    settings.value^.doResetPlotOnEvaluation:=miAutoReset.Checked;
  end;

PROCEDURE TplotForm.miAutoscaleXClick(Sender: TObject);
  begin
    miAutoscaleX.Checked:=not(miAutoscaleX.Checked);
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miAutoscaleYClick(Sender: TObject);
  begin
    miAutoscaleY.Checked:=not(miAutoscaleY.Checked);
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
    miLogscaleX.Checked:=not(miLogscaleX.Checked);
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miLogscaleYClick(Sender: TObject);
  begin
    miLogscaleY.Checked:=not(miLogscaleY.Checked);
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miPreserveAspectClick(Sender: TObject);
  begin
    miPreserveAspect.Checked:=not(miPreserveAspect.Checked);
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miXFinerGridClick(Sender: TObject);
  begin
    miXFinerGrid.Checked:=not(miXFinerGrid.Checked);
    if miXFinerGrid.Checked then miXGrid.Checked:=true;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miXGridClick(Sender: TObject);
  begin
    miXGrid.Checked:=not(miXGrid.Checked);
    if not(miXGrid.Checked) then miXFinerGrid.Checked:=false;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miXTicsClick(Sender: TObject);
  begin
    miXTics.Checked:=not(miXTics.Checked);
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miYFinerGridClick(Sender: TObject);
  begin
    miYFinerGrid.Checked:=not(miYFinerGrid.Checked);
    if miYFinerGrid.Checked then miYGrid.Checked:=true;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miYGridClick(Sender: TObject);
  begin
    miYGrid.Checked:=not(miYGrid.Checked);
    if not(miYGrid.Checked) then miYFinerGrid.Checked:=false;
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.miYTicsClick(Sender: TObject);
  begin
    miYTics.Checked:=not(miYTics.Checked);
    pushSettingsToPlotContainer;
  end;

PROCEDURE TplotForm.plotImageMouseDown(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  VAR p:T_point;
  begin
    if ssLeft in Shift then begin
      plotSubsystem.lastMouseX:=x;
      plotSubsystem.lastMouseY:=y;
      p:=guiAdapters^.plot^.options.screenToReal(x,y);
      postMouseClick(p[0],p[1]);
    end;
  end;

PROCEDURE TplotForm.plotImageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: integer);
  VAR p:T_point;
  begin
    p:=guiAdapters^.plot^.options.screenToReal(x,y);
    StatusBar.SimpleText:='x='+floatToStr(p[0])+'; y='+floatToStr(p[1]);
    if ssLeft in Shift then with plotSubsystem do begin
      if (x<>lastMouseX) or (y<>lastMouseY) then begin
        guiAdapters^.plot^.panByPixels(lastMouseX-x,lastMouseY-y,plotImage);
        guiAdapters^.plot^.options:=guiAdapters^.plot^.options;
        mouseUpTriggersPlot:=true;
      end;
    end else postMouseMove(p[0],p[1]);
    with plotSubsystem do begin
      lastMouseX:=x;
      lastMouseY:=y;
    end;
  end;

PROCEDURE TplotForm.plotImageMouseUp(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  begin
    with plotSubsystem do if mouseUpTriggersPlot then begin
      pullPlotSettingsToGui();
      dynamicPlotting.postRescale;
      lastMouseX:=x;
      lastMouseY:=y;
      doOrPostPlot();
    end;
  end;

PROCEDURE TplotForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    postPlotClosed;
  end;

FUNCTION TplotForm.getPlotQuality: byte;
  begin
    if      miAntiAliasing4.Checked then result:=PLOT_QUALITY_HIGH
    else if miAntiAliasing3.Checked then result:=PLOT_QUALITY_MEDIUM_2
    else if miAntiAliasing2.Checked then result:=PLOT_QUALITY_MEDIUM_1
    else                                 result:=PLOT_QUALITY_LOW;
  end;

PROCEDURE TplotForm.pullPlotSettingsToGui;
  VAR currentScalingOptions:T_scalingOptions;
  begin
    currentScalingOptions:=guiAdapters^.plot^.options;
    miXTics.Checked         :=gse_tics       in currentScalingOptions.axisStyle['x'];
    miXGrid.Checked         :=gse_coarseGrid in currentScalingOptions.axisStyle['x'];
    miXFinerGrid.Checked    :=gse_fineGrid   in currentScalingOptions.axisStyle['x'];
    miYTics.Checked         :=gse_tics       in currentScalingOptions.axisStyle['y'];
    miYGrid.Checked         :=gse_coarseGrid in currentScalingOptions.axisStyle['y'];
    miYFinerGrid.Checked    :=gse_fineGrid   in currentScalingOptions.axisStyle['y'];
    miPreserveAspect.Checked:=currentScalingOptions.preserveAspect;
    miAutoscaleX.Checked    :=currentScalingOptions.autoscale['x'];
    miAutoscaleY.Checked    :=currentScalingOptions.autoscale['y'];
    miLogscaleX.Checked     :=currentScalingOptions.axisTrafo['x'].logscale;
    miLogscaleY.Checked     :=currentScalingOptions.axisTrafo['y'].logscale;
  end;

PROCEDURE TplotForm.pushSettingsToPlotContainer;
  VAR currentScalingOptions:T_scalingOptions;
  begin
    currentScalingOptions:=guiAdapters^.plot^.options;
    currentScalingOptions.axisStyle['x']:=[];
    if miXTics.Checked      then include(currentScalingOptions.axisStyle['x'],gse_tics      );
    if miXGrid.Checked      then include(currentScalingOptions.axisStyle['x'],gse_coarseGrid);
    if miXFinerGrid.Checked then include(currentScalingOptions.axisStyle['x'],gse_fineGrid  );
    currentScalingOptions.axisStyle['y']:=[];
    if miYTics.Checked      then include(currentScalingOptions.axisStyle['y'],gse_tics      );
    if miYGrid.Checked      then include(currentScalingOptions.axisStyle['y'],gse_coarseGrid);
    if miYFinerGrid.Checked then include(currentScalingOptions.axisStyle['y'],gse_fineGrid  );
    currentScalingOptions.preserveAspect:=miPreserveAspect.Checked;
    currentScalingOptions.axisTrafo['x'].logscale:=miLogscaleX.Checked;
    currentScalingOptions.axisTrafo['y'].logscale:=miLogscaleY.Checked;
    currentScalingOptions.autoscale['x']:=miAutoscaleX.Checked;
    currentScalingOptions.autoscale['y']:=miAutoscaleY.Checked;
    guiAdapters^.plot^.options:=currentScalingOptions;
    pullPlotSettingsToGui();
    doOrPostPlot();
  end;

VAR broughtToFront:double;
PROCEDURE TplotForm.doPlot(CONST useTemporary: boolean);
  PROCEDURE updateInteractiveSection;
    FUNCTION CustomEventButton(CONST index:byte):TButton;
      begin
        case index of
          0: result:=CustomEventButton0;
          1: result:=CustomEventButton1;
          2: result:=CustomEventButton2;
          3: result:=CustomEventButton3;
          4: result:=CustomEventButton4;
          5: result:=CustomEventButton5;
          6: result:=CustomEventButton6;
          7: result:=CustomEventButton7;
        end;
      end;

    VAR i:byte;
        buttonCaption:string;
    begin
      InteractionPanel.visible:=isPlotInteractive or (animation.frameCount>0);
      if not(InteractionPanel.visible) then begin
        InteractionPanel.height:=0;
        exit;
      end else InteractionPanel.AutoSize:=true;
      AnimationGroupBox.visible:=(animation.frameCount>0);
      AnimationGroupBox.enabled:=(animation.frameCount>0);
      ButtonLeaveInteractiveMode.visible:=isPlotInteractive;
      ButtonLeaveInteractiveMode.enabled:=isPlotInteractive;
      InteractiveLabel.visible:=isPlotInteractive;
      InteractiveLabel.caption:=dynamicPlotLabelText.value;
      for i:=0 to 7 do begin
        CustomEventButton(i).visible:=isCustomEventEnabled(i,buttonCaption);
        CustomEventButton(i).enabled:=CustomEventButton(i).visible;
        CustomEventButton(i).caption:=buttonCaption;
      end;
      InteractiveLabel.height:=0;
      InteractiveLabel.Constraints.MinHeight:=0;
      InteractiveLabel.AutoSize:=true;
    end;

  begin
    if not(showing) then Show;
    updateInteractiveSection;
    if (now-broughtToFront)>5/(24*60*60) then begin
      BringToFront;
      broughtToFront:=now;
    end;
    if animation.frameCount<>0 then begin
      animation.getFrame(plotImage,animationFrameIndex,getPlotQuality);
      guiAdapters^.resetFlagsAfterPlotDone;
      exit;
    end;
    if useTemporary then begin
      tempPlot.CopyFrom(guiAdapters^.plot^);
      guiAdapters^.resetFlagsAfterPlotDone;
      tempPlot.renderPlot(plotImage,getPlotQuality);
    end else begin
      guiAdapters^.plot^.renderPlot(plotImage,getPlotQuality);
      guiAdapters^.resetFlagsAfterPlotDone;
    end;
  end;

PROCEDURE TplotForm.doOrPostPlot;
  begin
    if runEvaluator.evaluationRunning
    then guiAdapters^.logDeferredPlot
    else doPlot();
  end;

FUNCTION TplotForm.timerTick:boolean;
  begin
    if gui_started and (showing) and (animation.frameCount>0) and (animateCheckBox.Checked) then begin
      animation.nextFrame(animationFrameIndex);
      animation.getFrame(plotImage,animationFrameIndex,getPlotQuality);
      inc(framesSampled);
      if now-fpsSamplingStart>1/(24*60*60) then begin
        animationFPSLabel.caption:=intToStr(round(framesSampled/((now-fpsSamplingStart)*24*60*60)))+' FPS';
        fpsSamplingStart:=now;
        framesSampled:=0;
      end;
      result:=true;
    end else result:=false;
  end;

FUNCTION TplotForm.wantTimerInterval: longint;
  begin
    if animateCheckBox.Checked then result:=animationSpeedTrackbar.position*10
                               else result:=50;
    if result<=0 then result:=1;
  end;

FUNCTION plotShowing(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  begin
    result:=newBoolLiteral(gui_started and plotForm.showing);
  end;

FUNCTION clearPlotAnim_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  begin
    result:=newVoidLiteral;
    plotForm.animation.clear;
  end;

FUNCTION addAnimFrame_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  begin
    result:=newVoidLiteral;
    plotForm.animation.addFrame(context.adapters^.plot^);
  end;

INITIALIZATION
  broughtToFront:=0;
  registerRule(PLOT_NAMESPACE,'plotShowing',@plotShowing,[se_readGuiState],ak_nullary,'plotShowing;//Returns true if the plot is currently showing, false otherwise');
  registerRule(PLOT_NAMESPACE,'clearAnimation',@clearPlotAnim_impl,[se_alterGuiState],ak_nullary,'clearAnimation;//Clears the animated plot');
  registerRule(PLOT_NAMESPACE,'addAnimationFrame',@addAnimFrame_impl,[se_alterGuiState],ak_nullary,'addAnimationFrame;//Adds the current plot to the animation');
FINALIZATION
  if myPlotForm<>nil then FreeAndNil(myPlotForm);

end.

