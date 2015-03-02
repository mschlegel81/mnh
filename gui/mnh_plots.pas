unit mnh_plots;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, mnh_funcs, mnh_litvar, mnh_tokens, math, mnh_plotData, mnh_constants, mnh_tokloc,InterfaceBase,mnh_evalThread;

type
  { TplotForm }

  TplotForm = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem5: TMenuItem;
    miAntiAliasing5: TMenuItem;
    miAntiAliasing3: TMenuItem;
    miAntiAliasing4: TMenuItem;
    miAntiAliasing2: TMenuItem;
    miLoadPlot: TMenuItem;
    miSavePlot: TMenuItem;
    miExportBmp: TMenuItem;
    miAutoReset: TMenuItem;
    miLogscaleY: TMenuItem;
    MenuItem3: TMenuItem;
    miPreserveAspect: TMenuItem;
    miAutoscaleX: TMenuItem;
    miAutoscaleY: TMenuItem;
    miLogscaleX: TMenuItem;
    miYFinerGrid: TMenuItem;
    MenuItem2: TMenuItem;
    miXGridOptions: TMenuItem;
    MenuItem4: TMenuItem;
    miYGrid: TMenuItem;
    miXTics: TMenuItem;
    miXGrid: TMenuItem;
    miXFinerGrid: TMenuItem;
    miYTics: TMenuItem;
    miAntialiasingOff: TMenuItem;
    OpenDialog: TOpenDialog;
    plotImage: TImage;
    SaveDialog: TSaveDialog;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE miAntialiasingOffClick(Sender: TObject);
    PROCEDURE miAutoResetClick(Sender: TObject);
    PROCEDURE miAutoscaleXClick(Sender: TObject);
    PROCEDURE miAutoscaleYClick(Sender: TObject);
    PROCEDURE miExportBmpClick(Sender: TObject);
    PROCEDURE miLoadPlotClick(Sender: TObject);
    PROCEDURE miLogscaleXClick(Sender: TObject);
    PROCEDURE miLogscaleYClick(Sender: TObject);
    PROCEDURE miPreserveAspectClick(Sender: TObject);
    PROCEDURE miSavePlotClick(Sender: TObject);
    PROCEDURE miXFinerGridClick(Sender: TObject);
    PROCEDURE miXGridClick(Sender: TObject);
    PROCEDURE miXTicsClick(Sender: TObject);
    PROCEDURE miYFinerGridClick(Sender: TObject);
    PROCEDURE miYGridClick(Sender: TObject);
    PROCEDURE miYTicsClick(Sender: TObject);
  private
    { private declarations }
  public
    rendering:boolean;
    renderStartTime:double;
    { public declarations }
    PROCEDURE doPlot();
    PROCEDURE pullSettingsToGui();
    PROCEDURE pushSettingsToPlotContainer(CONST plotImmediately:boolean);
    PROCEDURE doConditionalReset;
  end;


VAR
  plotForm: TplotForm;
  plotDisplayRequired:boolean=false;

implementation

{$R *.lfm}

{ TplotForm }

PROCEDURE TplotForm.FormCreate(Sender: TObject);
  begin
    pullSettingsToGui();
    rendering:=false;
  end;

PROCEDURE TplotForm.FormResize(Sender: TObject);
  begin
    plotImage.Align:=alClient;
    doPlot();
  end;

PROCEDURE TplotForm.miAntialiasingOffClick(Sender: TObject);
  begin
    if ad_evaluationRunning then plotDisplayRequired:=true
                            else doPlot();
  end;

PROCEDURE TplotForm.miAutoResetClick(Sender: TObject);
begin
  miAutoReset.Checked:=not(miAutoReset.Checked);
end;

PROCEDURE TplotForm.miAutoscaleXClick(Sender: TObject);
begin
  miAutoscaleX.Checked:=not(miAutoscaleX.Checked);
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TplotForm.miAutoscaleYClick(Sender: TObject);
begin
  miAutoscaleY.Checked:=not(miAutoscaleY.Checked);
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TplotForm.miExportBmpClick(Sender: TObject);
VAR storeImage:TImage;
    rect:TRect;
begin
  SaveDialog.Filter:='Portable network graphics (PNG)|*.png';
  if SaveDialog.Execute then begin
    storeImage:=TImage.Create(Self);
    storeImage.SetInitialBounds(0,0,plotImage.Width,plotImage.Height);
    rect.Top:=0;
    rect.Left:=0;
    rect.Right:=plotImage.Width;
    rect.Bottom:=plotImage.Height;
    storeImage.Canvas.CopyRect(rect,plotImage.Canvas,rect);
    SaveDialog.FileName:=ChangeFileExt(SaveDialog.FileName,'.png');
    storeImage.Picture.PNG.SaveToFile(SaveDialog.FileName);
    storeImage.Free;
  end;
end;

PROCEDURE TplotForm.miLoadPlotClick(Sender: TObject);
  begin
    OpenDialog.Filter:='MNH-Plot|*.mnh_plot';
    if OpenDialog.Execute then begin
      OpenDialog.FileName:=ChangeFileExt(OpenDialog.FileName,'.mnh_plot');;
      if FileExistsUTF8(OpenDialog.FileName) then begin
        if not(activePlot.loadFromFile(OpenDialog.FileName))
        then activePlot.setDefaults
        else doPlot();
      end;
    end;
  end;

PROCEDURE TplotForm.miLogscaleXClick(Sender: TObject);
begin
  miLogscaleX.Checked:=not(miLogscaleX.Checked);
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TplotForm.miLogscaleYClick(Sender: TObject);
begin
  miLogscaleY.Checked:=not(miLogscaleY.Checked);
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TplotForm.miPreserveAspectClick(Sender: TObject);
  begin
    miPreserveAspect.Checked:=not(miPreserveAspect.Checked);
    pushSettingsToPlotContainer(true);
  end;

PROCEDURE TplotForm.miSavePlotClick(Sender: TObject);
  begin
    SaveDialog.Filter:='MNH-Plot|*.mnh_plot';
    if SaveDialog.Execute then begin
      SaveDialog.FileName:=ChangeFileExt(SaveDialog.FileName,'.mnh_plot');
      activePlot.saveToFile(SaveDialog.FileName);
    end;
  end;

PROCEDURE TplotForm.miXFinerGridClick(Sender: TObject);
begin
  miXFinerGrid.Checked:=not(miXFinerGrid.Checked);
  if miXFinerGrid.Checked then miXGrid.Checked:=true;
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TplotForm.miXGridClick(Sender: TObject);
begin
  miXGrid.Checked:=not(miXGrid.Checked);
  if not(miXGrid.Checked) then miXFinerGrid.Checked:=false;
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TplotForm.miXTicsClick(Sender: TObject);
begin
  miXTics.Checked:=not(miXTics.Checked);
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TplotForm.miYFinerGridClick(Sender: TObject);
begin
  miYFinerGrid.Checked:=not(miYFinerGrid.Checked);
  if miYFinerGrid.Checked then miYGrid.Checked:=true;
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TplotForm.miYGridClick(Sender: TObject);
begin
  miYGrid.Checked:=not(miYGrid.Checked);
  if not(miYGrid.Checked) then miYFinerGrid.Checked:=false;
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TplotForm.miYTicsClick(Sender: TObject);
begin
  miYTics.Checked:=not(miYTics.Checked);
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TplotForm.doPlot;
  PROCEDURE drawGridAndRows(CONST target:TCanvas; CONST scalingFactor:longint);
    VAR rowId,i,x,y,yBaseLine,lastX,lastY:longint;
        symSize:double;
        lastWasValid,currentIsValid:boolean;
        sample:T_point;
        patternIdx:byte;
        rowColor:longint;

    PROCEDURE drawPatternRect(x0,y0,x1,y1:longint);
      VAR x,y,locY:longint;
      begin
        if x1<x0 then begin
          x:=x1; x1:=x0; x0:=x;
        end;
        for x:=x0 to x1 do begin
          locY:=round(y0+(y1-y0)*(x-x0)/(x1-x0));
          if locY>yBaseLine then begin
            for y:=yBaseLine to locY do
            if (x and 1)+2*(y and 1)=patternIdx then
              target.Pixels[x,y]:=rowColor;
          end else begin
            for y:=yBaseLine downto locY do
            if (x and 1)+2*(y and 1)=patternIdx then
              target.Pixels[x,y]:=rowColor;
          end;
        end;
      end;

    begin
      //Clear:------------------------------------------------------------------
      target.Brush.Style:=bsSolid;
      target.Brush.Color:=clWhite;
      target.Pen.Style  :=psClear;
      target.Pen.EndCap :=pecSquare;
      target.FillRect(0,0,target.Width-1,target.Height-1);
      target.Clear;
      //------------------------------------------------------------------:Clear
      //coordinate grid:========================================================
      target.Pen.Style:=psSolid;
      target.Pen.Width:=scalingFactor;
      for i:=0 to length(activePlot.tic['y'])-1 do with activePlot.tic['y'][i] do begin
        y:=round(pos*scalingFactor);
        if major then target.Pen.Color:=$BBBBBB
                 else target.Pen.Color:=$DDDDDD;
        target.Line(0,y,activePlot.screenWidth*scalingFactor,y);
      end;
      for i:=0 to length(activePlot.tic['x'])-1 do with activePlot.tic['x'][i] do begin
        x:=round(pos*scalingFactor);
        if major then target.Pen.Color:=$BBBBBB
                 else target.Pen.Color:=$DDDDDD;
        target.Line(x,0,x,activePlot.screenHeight*scalingFactor);
      end;
      //========================================================:coordinate grid
      //row data:===============================================================
      if activePlot.logscale['y'] then yBaseLine:=round(activePlot.realToScreen('y',1)*scalingFactor)
                                  else yBaseLine:=round(activePlot.realToScreen('y',0)*scalingFactor);
      if yBaseLine<0 then yBaseLine:=0 else if yBaseLine>=target.Height then yBaseLine:=target.Height-1;
      for rowId:=0 to length(activePlot.row)-1 do begin
        rowColor:=activePlot.row[rowId].style.getTColor;
        patternIdx:=rowId and 3;

        if activePlot.row[rowId].style.wantStraightLines then begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecRound;
          lastWasValid:=false;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=activePlot.isSampleValid(sample);
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              x:=round(sample[0]*scalingFactor);
              y:=round(sample[1]*scalingFactor);
              if lastWasValid then begin
                target.LineTo(x,y);
                if activePlot.row[rowId].style.wantFill then drawPatternRect(lastX,lastY,x,y);
              end else target.MoveTo(x,y);
              lastX:=x;
              lastY:=y;
            end;
            lastWasValid:=currentIsValid;
          end;
        end else if activePlot.row[rowId].style.wantLeftSteps then begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecRound;
          lastWasValid:=false;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=activePlot.isSampleValid(sample);
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              x:=round(sample[0]*scalingFactor);
              y:=round(sample[1]*scalingFactor);
              if lastWasValid then begin
                target.LineTo(lastX,y);
                target.LineTo(    x,y);
                if activePlot.row[rowId].style.wantFill then drawPatternRect(lastX,y,x,y);
              end else target.MoveTo(x,y);
              lastX:=x;
              lastY:=y;
            end;
            lastWasValid:=currentIsValid;
          end;
        end else if activePlot.row[rowId].style.wantRightSteps then begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecRound;
          lastWasValid:=false;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=activePlot.isSampleValid(sample);
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              x:=round(sample[0]*scalingFactor);
              y:=round(sample[1]*scalingFactor);
              if lastWasValid then begin
                target.LineTo(x,lastY);
                target.LineTo(x,    y);
                if activePlot.row[rowId].style.wantFill then drawPatternRect(lastX,lastY,x,lastY);
              end else target.MoveTo(x,y);
              lastX:=x;
              lastY:=y;
            end;
            lastWasValid:=currentIsValid;
          end;
        end else if activePlot.row[rowId].style.wantBars then begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecRound;

          lastWasValid:=false;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=activePlot.isSampleValid(sample);
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              x:=round(sample[0]*scalingFactor);
              y:=round(sample[1]*scalingFactor);
              if lastWasValid then begin
                drawPatternRect(round(lastX*0.95+x*0.05),lastY,
                                round(lastX*0.05+x*0.95),lastY);
                target.Line(round(lastX*0.95+x*0.05),yBaseLine,round(lastX*0.95+x*0.05),lastY);
                target.Line(round(lastX*0.95+x*0.05),lastY    ,round(lastX*0.05+x*0.95),lastY);
                target.Line(round(lastX*0.05+x*0.95),yBaseLine,round(lastX*0.05+x*0.95),lastY);
              end;
              lastX:=x;
              lastY:=y;
              lastWasValid:=currentIsValid;
            end;
          end;

        end else if activePlot.row[rowId].style.wantBoxes then begin
          target.Pen.Style:=psClear;
          target.Brush.Style:=bsSolid;
          target.Brush.Color:=rowColor;
          lastWasValid:=false;
          i:=0;
          while i+1<length(activePlot.row[rowId].sample) do begin
            sample:=activePlot.row[rowId].sample[i];
            if activePlot.isSampleValid(sample) then begin
              sample:=activePlot.realToScreen(sample);
              lastX:=round(sample[0]*scalingFactor);
              lastY:=round(sample[1]*scalingFactor);
              sample:=activePlot.row[rowId].sample[i+1];
              if activePlot.isSampleValid(sample) then begin
                sample:=activePlot.realToScreen(sample);
                x:=round(sample[0]*scalingFactor);
                y:=round(sample[1]*scalingFactor);
                target.Rectangle(lastX,lastY,x,y);
              end;
            end;
            inc(i,2);
          end;
        end;
        if activePlot.row[rowId].style.wantDot then begin
          target.Pen.Style:=psClear;
          target.Brush.Style:=bsSolid;
          target.Brush.Color:=rowColor;
          symSize:=activePlot.row[rowId].style.getSymbolWidth*scalingFactor;

          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=activePlot.isSampleValid(sample);
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              target.Ellipse(round(sample[0]*scalingFactor-symSize),round(sample[1]*scalingFactor-symSize),
                                       round(sample[0]*scalingFactor+symSize),round(sample[1]*scalingFactor+symSize));
            end;
          end;
        end;
        if activePlot.row[rowId].style.wantPlus then begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecSquare;
          symSize:=activePlot.row[rowId].style.getSymbolWidth*scalingFactor;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=activePlot.isSampleValid(sample);
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              target.Line(round(sample[0]*scalingFactor-symSize),round(sample[1]*scalingFactor),
                                    round(sample[0]*scalingFactor+symSize),round(sample[1]*scalingFactor));
              target.Line(round(sample[0]*scalingFactor),round(sample[1]*scalingFactor-symSize),
                                    round(sample[0]*scalingFactor),round(sample[1]*scalingFactor+symSize));
            end;
          end;
        end;
        if activePlot.row[rowId].style.wantCross then begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecSquare;
          symSize:=activePlot.row[rowId].style.getSymbolRad;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=activePlot.isSampleValid(sample);
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              target.Line(round(sample[0]*scalingFactor-symSize),round(sample[1]*scalingFactor-symSize),
                                    round(sample[0]*scalingFactor+symSize),round(sample[1]*scalingFactor+symSize));
              target.Line(round(sample[0]*scalingFactor+symSize),round(sample[1]*scalingFactor-symSize),
                                    round(sample[0]*scalingFactor-symSize),round(sample[1]*scalingFactor+symSize));
            end;
          end;
        end;
        if activePlot.row[rowId].style.wantImpulses then begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecSquare;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=activePlot.isSampleValid(sample);
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              target.Line(round(sample[0]*scalingFactor),yBaseLine,
                                    round(sample[0]*scalingFactor),round(sample[1]*scalingFactor));
            end;
          end;
        end;
      end;
      //===============================================================:row data
    end;

  PROCEDURE scaleDown(CONST source,target:TCanvas; CONST scalingFactor:longint);
    VAR r,g,b:longint;
    PROCEDURE resetRGB; begin r:=0; g:=0; b:=0; end;
    PROCEDURE addRGB(CONST rgba:longint);
      begin
        inc(r, rgba         and 255);
        inc(g,(rgba shr  8) and 255);
        inc(b,(rgba shr 16) and 255);
      end;

    FUNCTION averageRGB:longint;
      begin
        r:=r div sqr(scalingFactor); if r<0 then r:=0 else if r>255 then r:=255;
        g:=g div sqr(scalingFactor); if g<0 then g:=0 else if g>255 then g:=255;
        b:=b div sqr(scalingFactor); if b<0 then b:=0 else if b>255 then b:=255;

        result:=r         or
               (g shl  8) or
               (b shl 16);
      end;

    VAR x,y,ix,iy:longint;
    begin
      for y:=0 to activePlot.screenHeight-1 do
      for x:=0 to activePlot.screenWidth-1 do begin
        resetRGB;
        for iy:=y*scalingFactor to (y+1)*scalingFactor-1 do
        for ix:=x*scalingFactor to (x+1)*scalingFactor-1 do addRGB(source.Pixels[ix,iy]);
        target.Pixels[x,y]:=averageRGB;
      end;
    end;

  PROCEDURE drawCoordSys(CONST target:TCanvas);
    VAR i,x,y:longint;
    begin
      //coordinate system:======================================================
      //clear border:-----------------------------------------------------------
      target.Brush.Style:=bsSolid;
      target.Brush.Color:=clWhite;
      target.Pen.Style:=psClear;
      target.Pen.Width:=1;
      target.Pen.EndCap:=pecSquare;
      if activePlot.wantTics('y') then
        target.FillRect(0,0,activePlot.xOffset,plotImage.Height);
      if activePlot.wantTics('x') then
        target.FillRect(activePlot.xOffset,activePlot.yOffset,
                        plotImage.Width   ,plotImage.Height);
      //-----------------------------------------------------------:clear border
      //axis:-------------------------------------------------------------------
      target.Pen.Style:=psSolid;
      target.Pen.Color:=clBlack;
      target.Pen.Width:=1;
      if activePlot.wantTics('y') then
        target.Line(activePlot.xOffset    ,0                 ,
                              activePlot.xOffset,activePlot.yOffset);
      if activePlot.wantTics('x') then
        target.Line(activePlot.screenWidth,activePlot.yOffset,
                              activePlot.xOffset    ,activePlot.yOffset);
      //-------------------------------------------------------------------:axis
      //tics:-------------------------------------------------------------------
      if activePlot.wantTics('y') then begin
        for i:=0 to length(activePlot.tic['y'])-1 do with activePlot.tic['y'][i] do if major then begin
          y:=round(pos);
          target.Line(activePlot.xOffset-5,y,activePlot.xOffset,y);
          target.TextOut(activePlot.xOffset-5-target.TextWidth(txt),y-target.TextHeight(txt) shr 1,txt);
        end;
      end;
      if activePlot.wantTics('x') then begin
        for i:=0 to length(activePlot.tic['x'])-1 do with activePlot.tic['x'][i] do if major then begin
          x:=round(pos);
          target.Line(x,activePlot.yOffset+5,x,activePlot.yOffset);
          target.TextOut(x-target.TextWidth(txt) shr 1 ,activePlot.yOffset+5,txt);
        end;
      end;

      //-------------------------------------------------------------------:tics
      //======================================================:coordinate system
    end;

  VAR factor:longint;
      renderImage:TImage;
  begin
    plotDisplayRequired:=false;
    rendering:=true;
    renderStartTime:=now;
    plotImage.Canvas.AntialiasingMode:=amOn;
    //Prepare transformations:--------------------------------------------------
    activePlot.setScreenSize(plotImage.Width,plotImage.Height);
    repeat until not(
      activePlot.setTextSize(
        plotImage.Canvas.TextHeight(activePlot.longtestYTic),
        plotImage.Canvas.TextWidth (activePlot.longtestYTic)));
    //--------------------------------------------------:Prepare transformations
    if miAntialiasingOff.Checked then begin
      drawGridAndRows(plotImage.Canvas,1);
      drawCoordSys(plotImage.Canvas);
    end else begin
      if      miAntiAliasing5.Checked then factor:=5
      else if miAntiAliasing4.Checked then factor:=4
      else if miAntiAliasing3.Checked then factor:=3
      else factor:=2;
      renderImage:=TImage.Create(Self);
      renderImage.SetInitialBounds(0,0,plotImage.Width*factor,plotImage.Height*factor);
      drawGridAndRows(renderImage.Canvas,factor);
      scaleDown(renderImage.Canvas,plotImage.Canvas,factor);
      renderImage.Free;
      drawCoordSys(plotImage.Canvas);
    end;
    rendering:=false;
  end;

PROCEDURE TplotForm.pullSettingsToGui;
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

PROCEDURE TplotForm.pushSettingsToPlotContainer(CONST plotImmediately: boolean);
  VAR aidX,aidY:longint;
  begin
    aidX:=0;
    if miXTics.Checked      then aidX:=C_tics;;
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
    pullSettingsToGui();
    if plotImmediately then doPlot()
                       else plotDisplayRequired:=true;
  end;

PROCEDURE TplotForm.doConditionalReset;
  begin
    if miAutoReset.Checked then begin
      activePlot.setDefaults;
      pullSettingsToGui();
    end;
  end;

FUNCTION addPlot(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR options:ansistring='';
      sizeWithoutOptions:longint;
      rowId,i,iMax:longint;
      X,Y:P_listLiteral;

  begin
    if (params<>nil) and (params^.size>=1) then begin
      activePlot.setScreenSize(plotForm.plotImage.Width,
                               plotForm.plotImage.Height);
      if (params^.value(params^.size-1)^.literalType=lt_string) then begin
        options:=P_stringLiteral(params^.value(params^.size-1))^.value;
        sizeWithoutOptions:=params^.size-1;
      end else begin
        options:='';
        sizeWithoutOptions:=params^.size
      end;
      if (sizeWithoutOptions=1) and
         (params^.value(0)^.literalType=lt_list)
      then begin
        rowId:=activePlot.addRow(options);
        X:=P_listLiteral(params^.value(0));
        for i:=0 to X^.size-1 do begin
          if (X^.value(i)^.literalType in [lt_intList,lt_realList,lt_numList]) then begin
            Y:=P_listLiteral(X^.value(i));
            if Y^.size=2 then activePlot.row[rowId].addSample(fReal(Y^.value(0)),fReal(Y^.value(1)))
                         else activePlot.row[rowId].addSample(Nan,Nan);
          end else activePlot.row[rowId].addSample(Nan,Nan);
        end;
        plotDisplayRequired:=true;
        result:=newBoolLiteral(true);
      end else if (sizeWithoutOptions=1) and
         (params^.value(0)^.literalType in [lt_intList,lt_realList,lt_numList])
      then begin
        rowId:=activePlot.addRow(options);
        X:=P_listLiteral(params^.value(0));
        for i:=0 to X^.size-1 do activePlot.row[rowId].addSample(i,fReal(X^.value(i)));
        plotDisplayRequired:=true;
        result:=newBoolLiteral(true);
      end else if (sizeWithoutOptions=2) and
         (params^.value(0)^.literalType in [lt_intList,lt_realList,lt_numList]) and
         (params^.value(1)^.literalType in [lt_intList,lt_realList,lt_numList])
      then begin
        rowId:=activePlot.addRow(options);
        X:=P_listLiteral(params^.value(0));
        Y:=P_listLiteral(params^.value(1));
        iMax:=Min(X^.size,Y^.size);
        for i:=0 to iMax-1 do activePlot.row[rowId].addSample(fReal(X^.value(i)),fReal(Y^.value(i)));
        plotDisplayRequired:=true;
        result:=newBoolLiteral(true);
      end else if (sizeWithoutOptions=3) and
         (params^.value(0)^.literalType = lt_expression) and
         (params^.value(1)^.literalType in [lt_int,lt_real]) and
         (params^.value(2)^.literalType in [lt_int,lt_real])
      then begin
        rowId:=activePlot.addRow(options);
        activePlot.row[rowId].setRules(
          nil,
          P_expressionLiteral(params^.value(0)),
          fReal(params^.value(1)),
          fReal(params^.value(2)));
        plotDisplayRequired:=true;
        result:=newBoolLiteral(true);
      end else if (sizeWithoutOptions=4) and
         (params^.value(0)^.literalType = lt_expression) and
         (params^.value(1)^.literalType = lt_expression) and
         (params^.value(2)^.literalType in [lt_int,lt_real]) and
         (params^.value(3)^.literalType in [lt_int,lt_real])
      then begin
        rowId:=activePlot.addRow(options);
        activePlot.row[rowId].setRules(
          P_expressionLiteral(params^.value(0)),
          P_expressionLiteral(params^.value(1)),
          fReal(params^.value(2)),
          fReal(params^.value(3)));
        plotDisplayRequired:=true;
        result:=newBoolLiteral(true);
      end else result:=newErrorLiteralRaising('Functions plot and addPlot cannot be applied to parameter list'+params^.toParameterListString(true),tokenLocation);
    end else result:=newErrorLiteralRaising('Function plot and addPlot cannot be applied to empty parameter list',tokenLocation);
  end;

FUNCTION plot(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotForm.rendering do sleep(1);
    activePlot.clear;
    result:=addPlot(params,tokenLocation,callDepth);
  end;

FUNCTION setAutoscale(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotForm.rendering do sleep(1);
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_booleanList) and (P_listLiteral(params^.value(0))^.size=2) then begin
      activePlot.setAutoscale(P_boolLiteral(P_listLiteral(params^.value(0))^.value(0))^.value,
                              P_boolLiteral(P_listLiteral(params^.value(0))^.value(1))^.value);
      plotForm.pullSettingsToGui();
      result:=newBoolLiteral(true);
    end else result:=newErrorLiteralRaising('Function setPlotAutoscale expects a list of 2 booleans as parameter.',tokenLocation);
  end;

FUNCTION getAutoscale(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotForm.rendering do sleep(1);
    result:=activePlot.getAutoscale;
  end;

FUNCTION setLogscale(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotForm.rendering do sleep(1);
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_booleanList) and (P_listLiteral(params^.value(0))^.size=2) then begin
      activePlot.setLogscale(P_boolLiteral(P_listLiteral(params^.value(0))^.value(0))^.value,
                             P_boolLiteral(P_listLiteral(params^.value(0))^.value(1))^.value);
      plotForm.pullSettingsToGui();
      result:=newBoolLiteral(true);
    end else result:=newErrorLiteralRaising('Function setPlotLogscale expects a list of 2 booleans as parameter.',tokenLocation);
  end;

FUNCTION getLogscale(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotForm.rendering do sleep(1);
    result:=activePlot.getLogscale;
  end;

FUNCTION setPlotRange(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR x,y:P_literal;
      x0,y0,x1,y1:double;
  begin
    while plotForm.rendering do sleep(1);
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_list) and (P_listLiteral(params^.value(0))^.size=2) then begin
      x:=P_listLiteral(params^.value(0))^.value(0);
      y:=P_listLiteral(params^.value(0))^.value(1);
      if (x^.literalType in [lt_intList,lt_realList,lt_numList]) and (P_listLiteral(x)^.size=2) and
         (y^.literalType in [lt_intList,lt_realList,lt_numList]) and (P_listLiteral(y)^.size=2) then begin
        x0:=fReal(P_listLiteral(x)^.value(0));
        x1:=fReal(P_listLiteral(x)^.value(1));
        y0:=fReal(P_listLiteral(y)^.value(0));
        y1:=fReal(P_listLiteral(y)^.value(1));
        if not(IsNan(x0)) and not(IsInfinite(x0)) and
           not(IsNan(x1)) and not(IsInfinite(x1)) and
           not(IsNan(y0)) and not(IsInfinite(y0)) and
           not(IsNan(y1)) and not(IsInfinite(y1)) then begin
          activePlot.setRange(x0,y0,x1,y1);
          plotForm.pullSettingsToGui();
          result:=newBoolLiteral(true);
        end else result:=newErrorLiteralRaising('Function setPlotRange expects a list of structure [[x0,x1],[y0,y1]] as parameter. Infinite and NaN values are forbidden.',tokenLocation);
      end else result:=newErrorLiteralRaising('Function setPlotRange expects a list of structure [[x0,x1],[y0,y1]] as parameter. Infinite and NaN values are forbidden.',tokenLocation);
    end else result:=newErrorLiteralRaising('Function setPlotRange expects a list of structure [[x0,x1],[y0,y1]] as parameter. Infinite and NaN values are forbidden.',tokenLocation);
  end;

FUNCTION getPlotRange(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotForm.rendering do sleep(1);
    result:=activePlot.getRange;
  end;

FUNCTION setAxisStyle(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotForm.rendering do sleep(1);
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_intList) and (P_listLiteral(params^.value(0))^.size=2) then begin
      activePlot.setAxisStyle(P_intLiteral(P_listLiteral(params^.value(0))^.value(0))^.value,
                              P_intLiteral(P_listLiteral(params^.value(0))^.value(1))^.value);
      plotForm.pullSettingsToGui();
      result:=newBoolLiteral(true);
    end else result:=newErrorLiteralRaising('Function setPlotAxisStyle expects a list of 2 integers as parameter.',tokenLocation);
  end;

FUNCTION getAxisStyle(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotForm.rendering do sleep(1);
    result:=activePlot.getAxisStyle;
  end;

FUNCTION setPreserveAspect(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotForm.rendering do sleep(1);
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_boolean) then begin
      activePlot.setPreserveAspect(P_boolLiteral(params^.value(0))^.value);
      plotForm.pullSettingsToGui();
      result:=newBoolLiteral(true);
    end else result:=newErrorLiteralRaising('Function setPlotPreserveAspect expects a boolean as parameter.',tokenLocation);
  end;

FUNCTION getPreserveAspect(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotForm.rendering do sleep(1);
    result:=activePlot.getPreserveAspect;
  end;

FUNCTION true_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    result:=newBoolLiteral(true);
  end;

INITIALIZATION
  mnh_funcs.registerRule('plotAvailable' ,@true_impl,'returns true (because plotting is available)');
  mnh_funcs.registerRule('plot',@plot,'plot(list,[options]); //plots flat numeric list or xy-list'+
  '#plot(xList,yList,[options]); //plots flat numeric list or xy-list'+
  '#plot(yExpression,t0,t1,[options]); //plots yExpression versus t in [t0,t1]'+
  '#plot(xExpression,yExpression,t0,t1,[options]); //plots yExpression versus xExpression for t in [t0,t1]');
  mnh_funcs.registerRule('addPlot',@addPlot,'addPlot(list,[options]); //adds plot flat numeric list or xy-list'+
  '#addPlot(xList,yList,[options]); //plots flat numeric list or xy-list'+
  '#addPlot(yExpression,t0,t1,[options]); //plots yExpression versus t in [t0,t1]'+
  '#addPlot(xExpression,yExpression,t0,t1,[options]); //plots yExpression versus xExpression for t in [t0,t1]');
  mnh_funcs.registerRule('setPlotAutoscale',@setAutoscale,'setPlotAutoscale([forX,forY]);#Sets autoscale per axis and returns true#Expects a tuple of two booleans as parameter.');
  mnh_funcs.registerRule('getPlotAutoscale',@getAutoscale,'getPlotAutoscale;#Returns the current autoscale settings per axis as a tuple of two booleans.');
  mnh_funcs.registerRule('setPlotLogscale',@setLogscale,'setPlotLogscale([forX,forY]);#Sets log-scale per axis and returns true#Expects a tuple of two booleans as parameter.');
  mnh_funcs.registerRule('getPlotLogscale',@getLogscale,'getPlotLogscale;#Returns the current log-scale settings per axis as a tuple of two booleans.');
  mnh_funcs.registerRule('setPlotRange',@setPlotRange,'setPlotRange([[x0,x1],[y0,y1]]);#Sets the plot-range for the next plot and returns true.');
  mnh_funcs.registerRule('getPlotRange',@getPlotRange,'getPlotRange;#Returns the plot-range of the last plot as a nested list: [[x0,x1],[y0,y1]]');
  mnh_funcs.registerRule('setPlotAxisStyle',@setAxisStyle,'setPlotAxisStyle([sx,sy]);#Sets the axis style for the next plot and returns true.');
  mnh_funcs.registerRule('getPlotAxisStyle',@getAxisStyle,'getPlotAxisStyle([sx,sy]);#Returns the current axis-style as a tuple of two integers.');
  mnh_funcs.registerRule('setPlotPreserveAspect',@setPreserveAspect,'setPlotPreserveAspect(b:boolean);#Sets or un-sets preservation of aspect ratio for the next plot.');
  mnh_funcs.registerRule('getPlotPreserveAspect',@getPreserveAspect,'getPlotPreserveAspect;#Returns a boolean indicating whether the aspect ratio will be preserverd for the next plot');
  mnh_evalThread.initIntrinsicRuleList;
end.

