UNIT mnh_plotForm;

{$mode objfpc}{$H+}

INTERFACE
{$WARN 5024 OFF}
USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ComCtrls,
  mnhFormHandler,
  mnh_constants, mnh_basicTypes,
  mnh_plotData,
  mnh_settings,
  mnh_out_adapters,
  mnh_litVar, mnh_funcs,
  mnh_contexts,
  mnh_evalThread,
  dynamicPlotting;

TYPE

  { TplotForm }

  TplotForm = class(TForm)
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
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
    miAntiAliasing4: TMenuItem;
    miAntiAliasing5: TMenuItem;
    plotImage: TImage;
    StatusBar: TStatusBar;
    PROCEDURE FormCreate(Sender: TObject);
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

    { private declarations }
  public
    PROCEDURE pullPlotSettingsToGui();
    PROCEDURE pushSettingsToPlotContainer();
    PROCEDURE doPlot();
    PROCEDURE doOrPostPlot();
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

PROCEDURE TplotForm.plotImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if ssLeft in Shift then begin
      plotSubsystem.lastMouseX:=x;
      plotSubsystem.lastMouseY:=y;
    end;
  end;

PROCEDURE TplotForm.plotImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  VAR p:T_point;
  begin
    p:=guiAdapters^.plot^.screenToReal(x,y);
    StatusBar.SimpleText:='x='+floatToStr(p[0])+'; y='+floatToStr(p[1]);
    if ssLeft in Shift then with plotSubsystem do begin
      guiAdapters^.plot^.panByPixels(lastMouseX-x,lastMouseY-y,plotImage);
      mouseUpTriggersPlot:=true;
    end else postMouseMove(p[0],p[1]);
    with plotSubsystem do begin
      lastMouseX:=x;
      lastMouseY:=y;
    end;
  end;

PROCEDURE TplotForm.plotImageMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
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
  end;

PROCEDURE TplotForm.pullPlotSettingsToGui;
  VAR o:T_scalingOptions;
  begin
    o:=guiAdapters^.plot^.options;
    miXTics.Checked         :=(o.axisStyle['x'] and C_tics)=C_tics;
    miXGrid.Checked         :=(o.axisStyle['x'] and C_grid)=C_grid;
    miXFinerGrid.Checked    :=(o.axisStyle['x'] and C_finerGrid)=C_finerGrid;
    miYTics.Checked         :=(o.axisStyle['y'] and C_tics)=C_tics;
    miYGrid.Checked         :=(o.axisStyle['y'] and C_grid)=C_grid;
    miYFinerGrid.Checked    :=(o.axisStyle['y'] and C_finerGrid)=C_finerGrid;
    miPreserveAspect.Checked:=o.preserveAspect;
    miAutoscaleX.Checked    :=o.autoscale['x'];
    miAutoscaleY.Checked    :=o.autoscale['y'];
    miLogscaleX.Checked     :=o.logscale['x'];
    miLogscaleY.Checked     :=o.logscale['y'];
  end;

PROCEDURE TplotForm.pushSettingsToPlotContainer;
  VAR o:T_scalingOptions;
  begin
    o:=guiAdapters^.plot^.options;
    o.axisStyle['x']:=0;
    if miXTics.Checked      then o.axisStyle['x']:=C_tics;
    if miXGrid.Checked      then o.axisStyle['x']:=o.axisStyle['x'] or C_grid;
    if miXFinerGrid.Checked then o.axisStyle['x']:=o.axisStyle['x'] or C_finerGrid;
    o.axisStyle['y']:=0;
    if miYTics.Checked      then o.axisStyle['y']:=C_tics;
    if miYGrid.Checked      then o.axisStyle['y']:=o.axisStyle['y'] or C_grid;
    if miYFinerGrid.Checked then o.axisStyle['y']:=o.axisStyle['y'] or C_finerGrid;
    o.preserveAspect:=miPreserveAspect.Checked;
    o.logscale['x']:=miLogscaleX.Checked;
    o.logscale['y']:=miLogscaleY.Checked;
    o.autoscale['x']:=miAutoscaleX.Checked;
    o.autoscale['y']:=miAutoscaleY.Checked;
    guiAdapters^.plot^.options:=o;
    pullPlotSettingsToGui();
    doOrPostPlot();
  end;

VAR broughtToFront:double;
PROCEDURE TplotForm.doPlot;
  VAR factor:longint;
  begin
    if not(showing) then Show;
    if (now-broughtToFront)>5/(24*60*60) then begin
      BringToFront;
      broughtToFront:=now;
    end;
    if      miAntiAliasing5.Checked then factor:=5
    else if miAntiAliasing4.Checked then factor:=4
    else if miAntiAliasing3.Checked then factor:=3
    else if miAntiAliasing2.Checked then factor:=2
    else                                 factor:=1;
    guiAdapters^.resetFlagsAfterPlotDone;
    guiAdapters^.plot^.renderPlot(plotImage,factor);
  end;

PROCEDURE TplotForm.doOrPostPlot;
  begin
    if runEvaluator.evaluationRunning
    then guiAdapters^.logDeferredPlot
    else doPlot();
  end;

FUNCTION plotShowing(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):P_literal;
  begin
    result:=newBoolLiteral(gui_started and plotForm.showing);
  end;

INITIALIZATION
  broughtToFront:=0;
  registerRule(PLOT_NAMESPACE,'plotShowing',@plotShowing,[se_readingInternal],ak_nullary,'plotShowing;#Returns true if the plot is currently showing, false otherwise');
FINALIZATION
  if myPlotForm<>nil then FreeAndNil(myPlotForm);

end.

