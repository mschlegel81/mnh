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
    miAntialiasing: TMenuItem;
    OpenDialog: TOpenDialog;
    plotImage: TImage;
    SaveDialog: TSaveDialog;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE miAntialiasingClick(Sender: TObject);
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
  end;

PROCEDURE TplotForm.FormResize(Sender: TObject);
  begin
    plotImage.Align:=alClient;
    doPlot();
  end;

PROCEDURE TplotForm.miAntialiasingClick(Sender: TObject);
  begin
    miAntialiasing.Checked:=not(miAntialiasing.Checked);
    doPlot();
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
    storeImage.Destroy;
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

PROCEDURE TplotForm.doPlot();
  VAR wcol:array of word;
  PROCEDURE readPixels;
    //VAR  ScanLineImage,                 //image with representation as in T_24BitImage
    //     tempIntfImage: TLazIntfImage;  //image with representation as in TBitmap
    //     y: Integer;                    //line counter
    //     ImgFormatDescription: TRawImageDescription;
    //
    {$WARNING this routine needs performance tuning!}
    VAR i,j,k,abgr:longint;
    begin
      if length(wcol)<3*plotImage.Height*plotImage.Width then begin
        setLength(wcol,3*plotImage.Height*plotImage.Width);
        for i:=0 to length(wcol)-1 do wcol[i]:=0;
      end;

      for j:=0 to plotImage.Height-1 do
      for i:=0 to plotImage.Width-1 do begin
        k:=(j*plotImage.Width+i)*3;
        abgr:=plotImage.Canvas.Pixels[i,j];
        inc(wcol[k  ], abgr         and 255);
        inc(wcol[k+1],(abgr shr  8) and 255);
        inc(wcol[k+2],(abgr shr 16) and 255);
      end;
    end;

  PROCEDURE averagePixels;
    //VAR  ScanLineImage,                 //image with representation as in T_24BitImage
    //     tempIntfImage: TLazIntfImage;  //image with representation as in TBitmap
    //     y: Integer;                    //line counter
    //     ImgFormatDescription: TRawImageDescription;
    {$WARNING this routine needs performance tuning!}
    VAR i,j,k:longint;
    begin
      for j:=0 to plotImage.Height-1 do
      for i:=0 to plotImage.Width-1 do begin
        k:=(j*plotImage.Width+i)*3;
        if length(wcol)>=k+3 then plotImage.Canvas.Pixels[i,j]:=
           (wcol[k  ] shr 4)        +
          ((wcol[k+1] shr 4) shl  8)+
          ((wcol[k+2] shr 4) shl 16);
      end;
    end;

  PROCEDURE plotWithOffset(CONST dx,dy:double; CONST runIdx:byte);
    VAR rowId,i,x,y,yBaseLine,lastX,lastY:longint;
        symSize:double;
        lastWasValid,currentIsValid:boolean;
        sample:T_point;
//        firstPattern:boolean;
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
            if (x and 1)+2*(y and 1)=(runIdx+rowId) and 3 then
              plotImage.Canvas.Pixels[x,y]:=rowColor;
          end else begin
            for y:=yBaseLine downto locY do
            if (x and 1)+2*(y and 1)=(runIdx+rowId) and 3 then
              plotImage.Canvas.Pixels[x,y]:=rowColor;
          end;
        end;
      end;

    begin
      //Clear:------------------------------------------------------------------
      plotImage.Canvas.Brush.Style:=bsSolid;
      plotImage.Canvas.Brush.Color:=clWhite;
      plotImage.Canvas.Pen.Style:=psClear;
      plotImage.Canvas.Pen.EndCap:=pecSquare;
      plotImage.Canvas.Clear;
      //------------------------------------------------------------------:Clear
      //coordinate grid:========================================================
      plotImage.Canvas.Pen.Style:=psSolid;
      plotImage.Canvas.Pen.Width:=1;
      for i:=0 to length(activePlot.tic['y'])-1 do with activePlot.tic['y'][i] do begin
        y:=round(pos+dy);
        if major then plotImage.Canvas.Pen.Color:=$BBBBBB
                 else plotImage.Canvas.Pen.Color:=$DDDDDD;
        plotImage.Canvas.Line(0,y,plotImage.Canvas.Width,y);
      end;
      for i:=0 to length(activePlot.tic['x'])-1 do with activePlot.tic['x'][i] do begin
        x:=round(pos+dx);
        if major then plotImage.Canvas.Pen.Color:=$BBBBBB
                 else plotImage.Canvas.Pen.Color:=$DDDDDD;
        plotImage.Canvas.Line(x,0,x,plotImage.Canvas.Height);
      end;
      //========================================================:coordinate grid
      //row data:===============================================================
      if activePlot.logscale['y'] then yBaseLine:=round(activePlot.realToScreen('y',1))
                                  else yBaseLine:=round(activePlot.realToScreen('y',0));
      if yBaseLine<0 then yBaseLine:=0 else if yBaseLine>=plotImage.Height then yBaseLine:=plotImage.Height-1;

      for rowId:=0 to length(activePlot.row)-1 do begin
        rowColor:=activePlot.row[rowId].style.getTColor;
        //case byte(rowId mod 15) of
        //  0: firstPattern:=odd( runIdx         );
        //  1: firstPattern:=odd( runIdx    shr 1);
        //  2: firstPattern:=odd((runIdx+1) shr 1);
        //  3: firstPattern:=odd( runIdx    shr 2);
        //  4: firstPattern:=odd((runIdx+2) shr 2);
        //  5: firstPattern:=odd( runIdx    shr 3);
        //  6: firstPattern:=odd((runIdx+4) shr 3);
        //  7: firstPattern:=odd((runIdx+1) shr 2);
        //  8: firstPattern:=odd((runIdx+3) shr 2);
        //  9: firstPattern:=odd((runIdx+2) shr 3);
        // 10: firstPattern:=odd((runIdx+6) shr 3);
        // 11: firstPattern:=odd((runIdx+1) shr 3);
        // 12: firstPattern:=odd((runIdx+3) shr 3);
        // 13: firstPattern:=odd((runIdx+5) shr 3);
        // 14: firstPattern:=odd((runIdx+7) shr 3);
        //end;
        if activePlot.row[rowId].style.wantStraightLines then begin
          plotImage.Canvas.Pen.Style:=psSolid;
          plotImage.Canvas.Pen.Color:=rowColor;
          plotImage.Canvas.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth;
          plotImage.Canvas.Pen.EndCap:=pecRound;
          lastWasValid:=false;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=not(IsNan(sample[0])) and not(IsNan(sample[1]));
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              x:=round(sample[0]+dx);
              y:=round(sample[1]+dy);
              if lastWasValid then begin
                plotImage.Canvas.LineTo(round(sample[0]+dx),round(sample[1]+dy));
                if activePlot.row[rowId].style.wantFill then drawPatternRect(lastX,lastY,x,y);
              end else plotImage.Canvas.MoveTo(round(sample[0]+dx),round(sample[1]+dy));
              lastX:=x;
              lastY:=y;
            end;
            lastWasValid:=currentIsValid;
          end;
        end else if activePlot.row[rowId].style.wantLeftSteps then begin
          plotImage.Canvas.Pen.Style:=psSolid;
          plotImage.Canvas.Pen.Color:=rowColor;
          plotImage.Canvas.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth;
          plotImage.Canvas.Pen.EndCap:=pecRound;
          lastWasValid:=false;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=not(IsNan(sample[0])) and not(IsNan(sample[1]));
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              x:=round(sample[0]+dx);
              y:=round(sample[1]+dy);
              if lastWasValid then begin
                plotImage.Canvas.LineTo(lastX,y);
                plotImage.Canvas.LineTo(    x,y);
                if activePlot.row[rowId].style.wantFill then drawPatternRect(lastX,y,x,y);
              end else plotImage.Canvas.MoveTo(x,y);
              lastX:=x;
              lastY:=y;
            end;
            lastWasValid:=currentIsValid;
          end;
        end else if activePlot.row[rowId].style.wantRightSteps then begin
          plotImage.Canvas.Pen.Style:=psSolid;
          plotImage.Canvas.Pen.Color:=rowColor;
          plotImage.Canvas.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth;
          plotImage.Canvas.Pen.EndCap:=pecRound;
          lastWasValid:=false;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=not(IsNan(sample[0])) and not(IsNan(sample[1]));
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              x:=round(sample[0]+dx);
              y:=round(sample[1]+dy);
              if lastWasValid then begin
                plotImage.Canvas.LineTo(x,lastY);
                plotImage.Canvas.LineTo(x,    y);
                if activePlot.row[rowId].style.wantFill then drawPatternRect(lastX,lastY,x,lastY);
              end else plotImage.Canvas.MoveTo(x,y);
              lastX:=x;
              lastY:=y;
            end;
            lastWasValid:=currentIsValid;
          end;
        end else if activePlot.row[rowId].style.wantBars then begin
          plotImage.Canvas.Pen.Style:=psSolid;
          plotImage.Canvas.Pen.Color:=rowColor;
          plotImage.Canvas.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth;
          plotImage.Canvas.Pen.EndCap:=pecRound;

          lastWasValid:=false;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=not(IsNan(sample[0])) and not(IsNan(sample[1]));
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              x:=round(sample[0]+dx);
              y:=round(sample[1]+dy);
              if lastWasValid then begin
                drawPatternRect(round(lastX*0.95+x*0.05),lastY,
                                round(lastX*0.05+x*0.95),lastY);
                plotImage.Canvas.Line(round(lastX*0.95+x*0.05),yBaseLine,round(lastX*0.95+x*0.05),lastY);
                plotImage.Canvas.Line(round(lastX*0.95+x*0.05),lastY    ,round(lastX*0.05+x*0.95),lastY);
                plotImage.Canvas.Line(round(lastX*0.05+x*0.95),yBaseLine,round(lastX*0.05+x*0.95),lastY);
              end;
              lastX:=x;
              lastY:=y;
              lastWasValid:=currentIsValid;
            end;
          end;

        end else if activePlot.row[rowId].style.wantBoxes then begin
          plotImage.Canvas.Pen.Style:=psClear;
          plotImage.Canvas.Brush.Style:=bsSolid;
          plotImage.Canvas.Brush.Color:=rowColor;
          lastWasValid:=false;
          i:=0;
          while i+1<length(activePlot.row[rowId].sample) do begin
            sample:=activePlot.row[rowId].sample[i];
            if not(IsNan(sample[0])) and not(IsNan(sample[1])) then begin
              sample:=activePlot.realToScreen(sample);
              lastX:=round(sample[0]+dx);
              lastY:=round(sample[1]+dy);
              sample:=activePlot.row[rowId].sample[i+1];
              if not(IsNan(sample[0])) and not(IsNan(sample[1])) then begin
                sample:=activePlot.realToScreen(sample);
                x:=round(sample[0]+dx);
                y:=round(sample[1]+dy);
                plotImage.Canvas.Rectangle(lastX,lastY,x,y);
              end;
            end;
            inc(i,2);
          end;
        end;
        if activePlot.row[rowId].style.wantDot then begin
          plotImage.Canvas.Pen.Style:=psClear;
          plotImage.Canvas.Brush.Style:=bsSolid;
          plotImage.Canvas.Brush.Color:=rowColor;
          symSize:=activePlot.row[rowId].style.getSymbolWidth;

          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=not(IsNan(sample[0])) and not(IsNan(sample[1]));
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              plotImage.Canvas.Ellipse(round(sample[0]+dx-symSize),round(sample[1]+dy-symSize),
                                       round(sample[0]+dx+symSize),round(sample[1]+dy+symSize));
            end;
          end;
        end;
        if activePlot.row[rowId].style.wantPlus then begin
          plotImage.Canvas.Pen.Style:=psSolid;
          plotImage.Canvas.Pen.Color:=rowColor;
          plotImage.Canvas.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth;
          plotImage.Canvas.Pen.EndCap:=pecSquare;
          symSize:=activePlot.row[rowId].style.getSymbolWidth;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=not(IsNan(sample[0])) and not(IsNan(sample[1]));
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              plotImage.Canvas.Line(round(sample[0]+dx-symSize),round(sample[1]+dy),
                                    round(sample[0]+dx+symSize),round(sample[1]+dy));
              plotImage.Canvas.Line(round(sample[0]+dx),round(sample[1]+dy-symSize),
                                    round(sample[0]+dx),round(sample[1]+dy+symSize));
            end;
          end;
        end;
        if activePlot.row[rowId].style.wantCross then begin
          plotImage.Canvas.Pen.Style:=psSolid;
          plotImage.Canvas.Pen.Color:=rowColor;
          plotImage.Canvas.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth;
          plotImage.Canvas.Pen.EndCap:=pecSquare;
          symSize:=activePlot.row[rowId].style.getSymbolRad;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=not(IsNan(sample[0])) and not(IsNan(sample[1]));
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              plotImage.Canvas.Line(round(sample[0]+dx-symSize),round(sample[1]+dy-symSize),
                                    round(sample[0]+dx+symSize),round(sample[1]+dy+symSize));
              plotImage.Canvas.Line(round(sample[0]+dx+symSize),round(sample[1]+dy-symSize),
                                    round(sample[0]+dx-symSize),round(sample[1]+dy+symSize));
            end;
          end;
        end;
        if activePlot.row[rowId].style.wantImpulses then begin
          plotImage.Canvas.Pen.Style:=psSolid;
          plotImage.Canvas.Pen.Color:=rowColor;
          plotImage.Canvas.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth;
          plotImage.Canvas.Pen.EndCap:=pecSquare;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=not(IsNan(sample[0])) and not(IsNan(sample[1]));
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              plotImage.Canvas.Line(round(sample[0]+dx),yBaseLine,
                                    round(sample[0]+dx),round(sample[1]+dy));
            end;
          end;
        end;
      end;
      //===============================================================:row data
      //coordinate system:======================================================
      //clear border:-----------------------------------------------------------
      plotImage.Canvas.Brush.Style:=bsSolid;
      plotImage.Canvas.Brush.Color:=clWhite;
      plotImage.Canvas.Pen.Style:=psClear;
      plotImage.Canvas.Pen.Width:=1;
      plotImage.Canvas.Pen.EndCap:=pecSquare;
      if activePlot.wantTics('y') then
        plotImage.Canvas.FillRect(0,0,activePlot.xOffset,plotImage.Height);
      if activePlot.wantTics('x') then
        plotImage.Canvas.FillRect(activePlot.xOffset,activePlot.yOffset,
                                  plotImage.Width   ,plotImage.Height);
      //-----------------------------------------------------------:clear border
      //axis:-------------------------------------------------------------------
      plotImage.Canvas.Pen.Style:=psSolid;
      plotImage.Canvas.Pen.Color:=clBlack;
      plotImage.Canvas.Pen.Width:=1;
      if activePlot.wantTics('y') then
        plotImage.Canvas.Line(activePlot.xOffset    ,0                 ,
                              activePlot.xOffset,activePlot.yOffset);
      if activePlot.wantTics('x') then
        plotImage.Canvas.Line(activePlot.screenWidth,activePlot.yOffset,
                              activePlot.xOffset    ,activePlot.yOffset);
      //-------------------------------------------------------------------:axis
      //tics:-------------------------------------------------------------------
      if activePlot.wantTics('y') then begin
        for i:=0 to length(activePlot.tic['y'])-1 do with activePlot.tic['y'][i] do if major then begin
          y:=round(pos+dy);
          plotImage.Canvas.Line(activePlot.xOffset-5,y,activePlot.xOffset,y);
          y:=round(pos);
          plotImage.Canvas.TextOut(activePlot.xOffset-5-plotImage.Canvas.TextWidth(txt),y-plotImage.Canvas.TextHeight(txt) shr 1,txt);
        end;
      end;
      if activePlot.wantTics('x') then begin
        for i:=0 to length(activePlot.tic['x'])-1 do with activePlot.tic['x'][i] do if major then begin
          x:=round(pos+dx);
          plotImage.Canvas.Line(x,activePlot.yOffset+5,x,activePlot.yOffset);
          x:=round(pos);
          plotImage.Canvas.TextOut(x-plotImage.Canvas.TextWidth(txt) shr 1 ,activePlot.yOffset+5,txt);
        end;
      end;

      //-------------------------------------------------------------------:tics
      //======================================================:coordinate system
    end;

  VAR i,j:longint;
  begin
    plotDisplayRequired:=false;
    plotImage.Canvas.AntialiasingMode:=amOn;
    //Prepare transformations:--------------------------------------------------
    activePlot.setScreenSize(plotImage.Width,plotImage.Height);
    repeat until not(
      activePlot.setTextSize(
        plotImage.Canvas.TextHeight(activePlot.longtestYTic),
        plotImage.Canvas.TextWidth (activePlot.longtestYTic)));
    //--------------------------------------------------:Prepare transformations
    if miAntialiasing.Checked then begin
      for i:=0 to 3 do for j:=0 to 3 do begin
        plotWithOffset(cos(0.1)*(i*0.25-0.375)+sin(0.1)*(j*0.25-0.375),
                      -sin(0.1)*(i*0.25-0.375)+cos(0.1)*(j*0.25-0.375),i*4+j);
        readPixels;
      end;
      averagePixels;
    end else plotWithOffset(0,0,0);
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

FUNCTION fReal(CONST X:P_literal):double; inline;
  begin
    case X^.literalType of
      lt_real: begin
        result:=P_realLiteral(x)^.value;
        if IsInfinite(result) then result:=NaN;
      end;
      lt_int: result:=P_intLiteral(x)^.value;
      else result:=NaN;
    end;
  end;

FUNCTION addPlot(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR options:ansistring='';
      sizeWithoutOptions:longint;
      rowId,i,iMax:longint;
      X,Y:P_listLiteral;
      t0,t1:double;

  PROCEDURE addFuncRow(CONST fx:P_subrule);
    VAR j:longint;
        t:P_realLiteral;
        td:double;
        res:P_literal;
    begin
      for j:=0 to 100 do begin
        td:=t0+(t1-t0)*(j*0.01);
        t:=newRealLiteral(td);
        res:=fx^.directEvaluateUnary(t,callDepth+1);
        activePlot.row[rowId].addSample(td,fReal(res));
        disposeLiteral(t);
        disposeLiteral(res);
      end;
    end;

  PROCEDURE addFuncRow(CONST fx,fy:P_subrule);
    VAR j:longint;
        t:P_realLiteral;
        resX,resY:P_literal;
    begin
      for j:=0 to 100 do begin
        t:=newRealLiteral(t0+(t1-t0)*(j*0.01));
        resX:=fx^.directEvaluateUnary(t,callDepth+1);
        resY:=fy^.directEvaluateUnary(t,callDepth+1);
        activePlot.row[rowId].addSample(fReal(resX),fReal(resY));
        disposeLiteral(t);
        disposeLiteral(resX);
        disposeLiteral(resY);
      end;
    end;

  begin
    if (params<>nil) and (params^.size>=1) then begin
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
        t0:=fReal(params^.value(1));
        t1:=fReal(params^.value(2));
        addFuncRow(P_expressionLiteral(params^.value(0))^.value);
        plotDisplayRequired:=true;
        result:=newBoolLiteral(true);
      end else if (sizeWithoutOptions=4) and
         (params^.value(0)^.literalType = lt_expression) and
         (params^.value(1)^.literalType = lt_expression) and
         (params^.value(2)^.literalType in [lt_int,lt_real]) and
         (params^.value(3)^.literalType in [lt_int,lt_real])
      then begin
        rowId:=activePlot.addRow(options);
        t0:=fReal(params^.value(2));
        t1:=fReal(params^.value(3));
        addFuncRow(P_expressionLiteral(params^.value(0))^.value,
                   P_expressionLiteral(params^.value(1))^.value);
        plotDisplayRequired:=true;
        result:=newBoolLiteral(true);
      end else result:=newErrorLiteralRaising('Functions plot and addPlot cannot be applied to parameter list'+params^.toParameterListString(true),tokenLocation);
    end else result:=newErrorLiteralRaising('Function plot and addPlot cannot be applied to empty parameter list',tokenLocation);
  end;

FUNCTION plot(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    activePlot.clear;
    result:=addPlot(params,tokenLocation,callDepth);
  end;

FUNCTION setAutoscale(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
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
    result:=activePlot.getAutoscale;
  end;

FUNCTION setLogscale(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
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
    result:=activePlot.getLogscale;
  end;

FUNCTION setPlotRange(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR x,y:P_literal;
      x0,y0,x1,y1:double;
  begin
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
          result:=newBoolLiteral(true);
        end else result:=newErrorLiteralRaising('Function setPlotRange expects a list of structure [[x0,x1],[y0,y1]] as parameter. Infinite and NaN values are forbidden.',tokenLocation);
      end else result:=newErrorLiteralRaising('Function setPlotRange expects a list of structure [[x0,x1],[y0,y1]] as parameter. Infinite and NaN values are forbidden.',tokenLocation);
    end else result:=newErrorLiteralRaising('Function setPlotRange expects a list of structure [[x0,x1],[y0,y1]] as parameter. Infinite and NaN values are forbidden.',tokenLocation);
  end;

FUNCTION getPlotRange(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    result:=activePlot.getRange;
  end;

FUNCTION setAxisStyle(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
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
    result:=activePlot.getAxisStyle;
  end;

FUNCTION setPreserveAspect(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_boolean) then begin
      activePlot.setPreserveAspect(P_boolLiteral(params^.value(0))^.value);
      plotForm.pullSettingsToGui();
      result:=newBoolLiteral(true);
    end else result:=newErrorLiteralRaising('Function setPlotPreserveAspect expects a boolean as parameter.',tokenLocation);
  end;

FUNCTION getPreserveAspect(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    result:=activePlot.getPreserveAspect;
  end;

INITIALIZATION
  mnh_funcs.registerRule('plot',@plot,''{$WARNING uncommented!});
  mnh_funcs.registerRule('addPlot',@addPlot,''{$WARNING uncommented!});
  mnh_funcs.registerRule('setPlotAutoscale',@setAutoscale,''{$WARNING uncommented!});
  mnh_funcs.registerRule('getPlotAutoscale',@getAutoscale,''{$WARNING uncommented!});
  mnh_funcs.registerRule('setPlotLogscale',@setLogscale,''{$WARNING uncommented!});
  mnh_funcs.registerRule('getPlotLogscale',@getLogscale,''{$WARNING uncommented!});
  mnh_funcs.registerRule('setPlotRange',@setPlotRange,''{$WARNING uncommented!});
  mnh_funcs.registerRule('getPlotRange',@getPlotRange,''{$WARNING uncommented!});
  mnh_funcs.registerRule('setPlotAxisStyle',@setAxisStyle,''{$WARNING uncommented!});
  mnh_funcs.registerRule('getPlotAxisStyle',@getAxisStyle,''{$WARNING uncommented!});
  mnh_funcs.registerRule('setPlotPreserveAspect',@setPreserveAspect,''{$WARNING uncommented!});
  mnh_funcs.registerRule('getPlotPreserveAspect',@getPreserveAspect,''{$WARNING uncommented!});
  mnh_evalThread.initIntrinsicRuleList;
end.

