UNIT mnh_plotData;
INTERFACE
USES sysutils,
     Interfaces, Classes, ExtCtrls, Graphics, types,
     myGenerics,myStringUtil,
     mnh_basicTypes, mnh_constants,
     plotstyles,plotMath;
TYPE
  T_point = array[0..1] of double;
  T_dataRow = array of T_point;
  T_boundingBox = array['x'..'y', 0..1] of double;
  P_plot =^T_plot;
  T_plot = object
    private
      cs: TRTLCriticalSection;
      scalingOptions:T_scalingOptions;
      row: array of T_sampleRow;

      PROCEDURE setScalingOptions(CONST value:T_scalingOptions);
      FUNCTION  getScalingOptions:T_scalingOptions;
      PROCEDURE drawGridAndRows(CONST target: TCanvas; CONST scalingFactor: longint; VAR gridTic: T_ticInfos);
      PROCEDURE drawCoordSys(CONST target: TCanvas; CONST intendedWidth,intendedHeight:longint; VAR gridTic: T_ticInfos);
      PROCEDURE drawCustomText(CONST target: TCanvas; CONST intendedWidth,intendedHeight:longint);
      FUNCTION  obtainPlot(CONST width,height,supersampling:longint):TImage;
    public
      PROPERTY options:T_scalingOptions read getScalingOptions write setScalingOptions;

      CONSTRUCTOR createWithDefaults;
      PROCEDURE setDefaults;
      DESTRUCTOR destroy;
      PROCEDURE clear;
      PROCEDURE addRow(CONST styleOptions: string; CONST rowData:T_dataRow);
      PROCEDURE removeRows(CONST numberOfRowsToRemove:longint);

      PROCEDURE zoomOnPoint(CONST pixelX, pixelY: longint; CONST factor: double; VAR plotImage: TImage);
      PROCEDURE panByPixels(CONST pixelDX, pixelDY: longint; VAR plotImage: TImage);

      PROCEDURE renderPlot(VAR plotImage: TImage; CONST supersampling:longint);
      PROCEDURE renderToFile(CONST fileName:string; CONST width,height,supersampling:longint);
      FUNCTION renderToString(CONST width,height,supersampling:longint):ansistring;

      PROCEDURE CopyFrom(VAR p:T_plot);
  end;

IMPLEMENTATION
VAR MAJOR_TIC_STYLE, MINOR_TIC_STYLE:T_style;

CONSTRUCTOR T_plot.createWithDefaults;
  begin
    system.initCriticalSection(cs);
    setDefaults;
  end;

PROCEDURE T_plot.setDefaults;
  VAR axis: char;
  begin
    system.enterCriticalSection(cs);
    with scalingOptions do begin
      for axis:='x' to 'y' do begin
        axisTrafo[axis].reset;
        autoscale[axis]:=true;
        axisStyle[axis]:=[gse_tics,gse_coarseGrid,gse_fineGrid];
      end;
      preserveAspect:=true;
      relativeFontSize:=10;
      autoscaleFactor:=1;
    end;
    clear;
    system.leaveCriticalSection(cs);
  end;

DESTRUCTOR T_plot.destroy;
  begin
    system.enterCriticalSection(cs);
    clear;
    system.leaveCriticalSection(cs);
    doneCriticalSection(cs);
  end;

PROCEDURE T_plot.clear;
  VAR i: longint;
  begin
    system.enterCriticalSection(cs);
    for i:=0 to length(row)-1 do row[i].destroy;
    setLength(row, 0);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_plot.addRow(CONST styleOptions: string; CONST rowData: T_dataRow);
  VAR index:longint;
  begin
    system.enterCriticalSection(cs);
    index:=length(row);
    setLength(row, index+1);
    row[index].create(index,rowData);
    if trim(styleOptions)<>'' then row[index].style.parseStyle(styleOptions);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_plot.removeRows(CONST numberOfRowsToRemove:longint);
  VAR i0,i:longint;
  begin
    if numberOfRowsToRemove<=0 then exit;
    system.enterCriticalSection(cs);
    i0:=length(row)-numberOfRowsToRemove; if i0<0 then i0:=0;
    for i:=i0 to length(row)-1 do row[i].destroy;
    setLength(row,i0);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_plot.setScalingOptions(CONST value: T_scalingOptions);
  begin
    system.enterCriticalSection(cs);
    scalingOptions:=value;
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_plot.getScalingOptions: T_scalingOptions;
  begin
    system.enterCriticalSection(cs);
    result:=scalingOptions;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_plot.zoomOnPoint(CONST pixelX, pixelY: longint; CONST factor: double; VAR plotImage: TImage);
  VAR rectA, rectB: TRect;
  begin with scalingOptions do begin
    system.enterCriticalSection(cs);
    autoscale['x']:=false;
    autoscale['y']:=false;
    scalingOptions.axisTrafo['x'].zoom(pixelX,factor);
    scalingOptions.axisTrafo['y'].zoom(pixelY,factor);

    rectA.top:=0;
    rectA.Left:=0;
    rectA.Right:=plotImage.width;
    rectA.Bottom:=plotImage.height;

    rectB.top:=round((-pixelY)*factor+pixelY);
    rectB.Left:=round((-pixelX)*factor+pixelX);
    rectB.Right:=round((plotImage.width-pixelX)*factor+pixelX);
    rectB.Bottom:=round((plotImage.height-pixelY)*factor+pixelY);

    plotImage.Canvas.CopyRect(rectA, plotImage.Canvas, rectB);
    system.leaveCriticalSection(cs);
  end; end;

PROCEDURE T_plot.panByPixels(CONST pixelDX, pixelDY: longint; VAR plotImage: TImage);
  VAR rectA, rectB: TRect;
  begin with scalingOptions do begin
    system.enterCriticalSection(cs);
    autoscale['x']:=false;
    autoscale['y']:=false;
    scalingOptions.axisTrafo['x'].pan(pixelDX);
    scalingOptions.axisTrafo['y'].pan(pixelDY);

    rectA.top:=0;
    rectA.Left:=0;
    rectA.Right:=plotImage.width;
    rectA.Bottom:=plotImage.height;
    rectB.top:=0+pixelDY;
    rectB.Left:=0+pixelDX;
    rectB.Right:=plotImage.width+pixelDX;
    rectB.Bottom:=plotImage.height+pixelDY;

    plotImage.Canvas.CopyRect(rectA, plotImage.Canvas, rectB);
    system.leaveCriticalSection(cs);
  end; end;

PROCEDURE T_plot.drawGridAndRows(CONST target: TCanvas; CONST scalingFactor: longint; VAR gridTic: T_ticInfos);
  VAR rowId, i, yBaseLine:longint;
      lastX: longint = 0;
      lastY: longint = 0;
      lastWasValid: boolean;
      scaleAndColor:T_scaleAndColor;
      screenRow:T_rowToPaint;

  PROCEDURE drawPatternRect(x0, y0, x1, y1: longint);
    VAR points:array[0..3] of TPoint;
    begin
      points[0].x:=x0; points[0].y:=y0;
      points[1].x:=x0; points[1].y:=yBaseLine;
      points[2].x:=x1; points[2].y:=yBaseLine;
      points[3].x:=x1; points[3].y:=y1;
      target.Brush.color:=scaleAndColor.solidColor;
      case byte(rowId and 3) of
        0: target.Brush.style:=bsFDiagonal ;
        1: target.Brush.style:=bsBDiagonal ;
        2: target.Brush.style:=bsHorizontal;
        3: target.Brush.style:=bsVertical  ;
      end;
      target.Pen.style:=psClear;
      target.Polygon(points);
      target.Pen.style:=psSolid;
    end;

  begin
    //Clear:------------------------------------------------------------------
    target.Brush.style:=bsSolid;
    target.Brush.color:=clWhite;
    target.Pen.style:=psClear;
    target.Pen.EndCap:=pecSquare;
    target.FillRect(0, 0, target.width-1, target.height-1);
    target.clear;
    //------------------------------------------------------------------:Clear
    //coordinate grid:========================================================
    target.Pen.style:=psSolid;
    //minor grid:-------------------------------------------------------------
    scaleAndColor:=MINOR_TIC_STYLE.getLineScaleAndColor(target.width,target.height);
    target.Pen.color:=scaleAndColor.lineColor;
    target.Pen.width:=scaleAndColor.lineWidth;
    if (gse_fineGrid in scalingOptions.axisStyle['y']) then
    for i:=0 to length(gridTic['y'])-1 do with gridTic['y'][i] do if not(major) then begin
      lastY:=round(pos*scalingFactor);
      target.line(0, lastY, target.width, lastY);
    end;
    if (gse_fineGrid in scalingOptions.axisStyle['x']) then
    for i:=0 to length(gridTic['x'])-1 do with gridTic['x'][i] do if not(major) then begin
      lastX:=round(pos*scalingFactor);
      target.line(lastX, 0, lastX, target.height);
    end;
    //-------------------------------------------------------------:minor grid
    //major grid:-------------------------------------------------------------
    scaleAndColor:=MAJOR_TIC_STYLE.getLineScaleAndColor(target.width,target.height);
    target.Pen.color:=scaleAndColor.lineColor;
    target.Pen.width:=scaleAndColor.lineWidth;
    if (gse_coarseGrid in scalingOptions.axisStyle['y']) then
    for i:=0 to length(gridTic['y'])-1 do with gridTic['y'][i] do if major then begin
      lastY:=round(pos*scalingFactor);
      target.line(0, lastY, target.width, lastY);
    end;
    if (gse_coarseGrid in scalingOptions.axisStyle['x']) then
    for i:=0 to length(gridTic['x'])-1 do with gridTic['x'][i] do if major then begin
      lastX:=round(pos*scalingFactor);
      target.line(lastX, 0, lastX, target.height);
    end;
    //-------------------------------------------------------------:major grid
    //========================================================:coordinate grid
    if scalingOptions.axisTrafo['y'].logscale
    then yBaseLine:=scalingOptions.axisTrafo['y'].screenMin
    else yBaseLine:=round(scalingOptions.axisTrafo['y'].apply(0)*scalingFactor);
    if      yBaseLine<0 then yBaseLine:=0
    else if yBaseLine>=target.height then yBaseLine:=target.height-1;
    //row data:===============================================================
    for rowId:=0 to length(row)-1 do begin
      {$ifdef debugMode}
      writeln('Drawing row #',rowId,' with style: ',row[rowId].style.toString);
      {$endif}
      screenRow:=scalingOptions.transformRow(row[rowId].sample,scalingFactor);
      scaleAndColor:=row[rowId].style.getLineScaleAndColor(target.width,target.height);

      if ps_straight in row[rowId].style.style then begin
        target.Pen.style:=psSolid;
        target.Pen.color:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecRound;
        lastWasValid:=false;
        for i:=0 to length(screenRow)-1 do begin
          if screenRow[i].valid then begin
            if lastWasValid then begin
              target.LineTo(screenRow[i].x, screenRow[i].y);
              if ps_filled in row[rowId].style.style then
                drawPatternRect(lastX, lastY, screenRow[i].x, screenRow[i].y);
            end else
              target.MoveTo(screenRow[i].x, screenRow[i].y);
            lastX:=screenRow[i].x;
            lastY:=screenRow[i].y;
          end;
          lastWasValid:=screenRow[i].valid;
        end;
      end else if ps_stepLeft in row[rowId].style.style then begin
        target.Pen.style:=psSolid;
        target.Pen.color:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecRound;
        lastWasValid:=false;
        for i:=0 to length(screenRow)-1 do begin
          if screenRow[i].valid then begin
            if lastWasValid then begin
              target.LineTo(lastX, screenRow[i].y);
              target.LineTo(screenRow[i].x, screenRow[i].y);
              if ps_filled in row[rowId].style.style then
                drawPatternRect(lastX, screenRow[i].y, screenRow[i].x, screenRow[i].y);
            end else target.MoveTo(screenRow[i].x, screenRow[i].y);
            lastX:=screenRow[i].x;
            lastY:=screenRow[i].y;
          end;
          lastWasValid:=screenRow[i].valid;
        end;
      end else if ps_stepRight in row[rowId].style.style then begin
        target.Pen.style:=psSolid;
        target.Pen.color:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecRound;
        lastWasValid:=false;
        for i:=0 to length(screenRow)-1 do begin
          if screenRow[i].valid then begin
            if lastWasValid then begin
              target.LineTo(screenRow[i].x, lastY);
              target.LineTo(screenRow[i].x, screenRow[i].y);
              if ps_filled in row[rowId].style.style then
                drawPatternRect(lastX, lastY, screenRow[i].x, lastY);
            end else target.MoveTo(screenRow[i].x, screenRow[i].y);
            lastX:=screenRow[i].x;
            lastY:=screenRow[i].y;
          end;
          lastWasValid:=screenRow[i].valid;
        end;
      end else if ps_bar in row[rowId].style.style then begin
        target.Pen.style:=psSolid;
        target.Pen.color:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecRound;

        lastWasValid:=false;
        for i:=0 to length(screenRow)-1 do begin
          if screenRow[i].valid then begin
            if lastWasValid then
              drawPatternRect(round(lastX*0.95+screenRow[i].x*0.05), lastY,
                              round(lastX*0.05+screenRow[i].x*0.95), lastY);
            lastX:=screenRow[i].x;
            lastY:=screenRow[i].y;
            lastWasValid:=screenRow[i].valid;
          end;
        end;
      end else if ps_box in row[rowId].style.style then begin
        target.Pen.style:=psClear;
        target.Brush.style:=bsSolid;
        target.Brush.color:=scaleAndColor.solidColor;
        lastWasValid:=false;
        i:=0;
        while i+1<length(screenRow) do begin
          if screenRow[i].valid and
             screenRow[i+1].valid then begin
            target.FillRect(screenRow[i  ].x, screenRow[i  ].y,
                            screenRow[i+1].x, screenRow[i+1].y);
          end;
          inc(i, 2);
        end;
      end;
      if ps_dot in row[rowId].style.style then begin
        target.Pen.style:=psClear;
        target.Brush.style:=bsSolid;
        target.Brush.color:=scaleAndColor.solidColor;
        if scaleAndColor.symbolWidth>=1 then begin
          for i:=0 to length(screenRow)-1 do if screenRow[i].valid then
            target.Ellipse(screenRow[i].x-scaleAndColor.symbolWidth,
                           screenRow[i].y-scaleAndColor.symbolWidth,
                           screenRow[i].x+scaleAndColor.symbolWidth,
                           screenRow[i].y+scaleAndColor.symbolWidth);
        end else begin
          for i:=0 to length(screenRow)-1 do if screenRow[i].valid then
            target.Pixels[screenRow[i].x,
                          screenRow[i].y]:=scaleAndColor.lineColor;
        end;
      end;
      if ps_plus in row[rowId].style.style then begin
        target.Pen.style:=psSolid;
        target.Pen.color:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecSquare;
        for i:=0 to length(screenRow)-1 do if screenRow[i].valid then begin
          target.line(screenRow[i].x-scaleAndColor.symbolWidth,
                      screenRow[i].y,
                      screenRow[i].x+scaleAndColor.symbolWidth,
                      screenRow[i].y);
          target.line(screenRow[i].x,
                      screenRow[i].y-scaleAndColor.symbolWidth,
                      screenRow[i].x,
                      screenRow[i].y+scaleAndColor.symbolWidth);
        end;
      end;
      if ps_cross in row[rowId].style.style then begin
        target.Pen.style:=psSolid;
        target.Pen.color:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecSquare;
        for i:=0 to length(screenRow)-1 do if screenRow[i].valid then begin
          target.line(screenRow[i].x-scaleAndColor.symbolRadius,
                      screenRow[i].y-scaleAndColor.symbolRadius,
                      screenRow[i].x+scaleAndColor.symbolRadius,
                      screenRow[i].y+scaleAndColor.symbolRadius);
          target.line(screenRow[i].x+scaleAndColor.symbolRadius,
                      screenRow[i].y-scaleAndColor.symbolRadius,
                      screenRow[i].x-scaleAndColor.symbolRadius,
                      screenRow[i].y+scaleAndColor.symbolRadius);
        end;
      end;
      if ps_impulse in row[rowId].style.style then begin
        target.Pen.style:=psSolid;
        target.Pen.color:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecSquare;
        for i:=0 to length(screenRow)-1 do if screenRow[i].valid then
          target.line(screenRow[i].x,
                      yBaseLine,
                      screenRow[i].x,
                      screenRow[i].y);
      end;
    end;
    //===============================================================:row data
  end;

PROCEDURE T_plot.drawCustomText(CONST target: TCanvas; CONST intendedWidth,intendedHeight:longint);
  VAR rowId, i, k,TextWidth,TextHeight:longint;
      scaleAndColor:T_scaleAndColor;
      screenRow:T_rowToPaint;
      lines:T_arrayOfString;
  begin
    for rowId:=0 to length(row)-1 do if ps_textOut in row[rowId].style.style then begin
      {$ifdef debugMode}
      writeln('Drawing row #',rowId,' with style: ',row[rowId].style.toString);
      {$endif}
      screenRow:=scalingOptions.transformRow(row[rowId].sample,1);
      scaleAndColor:=row[rowId].style.getLineScaleAndColor(intendedWidth,intendedHeight);
      lines:=split(row[rowId].style.txt);
      TextWidth :=0;
      TextHeight:=0;
      for k:=0 to length(lines)-1 do begin
        i:=target.TextWidth (lines[k]); if i>TextWidth then TextWidth:=i;
        inc(TextHeight,target.TextHeight(lines[k]));
      end;
      target.Font.color:=scaleAndColor.solidColor;
      target.Font.size:=scaleAndColor.fontSize;
      target.Brush.style:=bsClear;
      for i:=0 to length(screenRow)-1 do if screenRow[i].valid then begin
        dec(screenRow[i].x,TextWidth shr 1);
        dec(screenRow[i].y,TextHeight shr 1);
        for k:=0 to length(lines)-1 do begin
          target.textOut(screenRow[i].x,
                         screenRow[i].y,
                         lines[k]);
          inc(screenRow[i].y,target.TextHeight(lines[k]));
        end;
      end;
    end;
  end;

PROCEDURE T_plot.drawCoordSys(CONST target: TCanvas; CONST intendedWidth,intendedHeight:longint; VAR gridTic: T_ticInfos);
  VAR i, x, y: longint;
      cSysX,cSysY:longint;
  begin
    target.Font.size:=scalingOptions.absoluteFontSize(intendedWidth,intendedHeight);
    target.Font.color:=clBlack;
    cSysX:=scalingOptions.axisTrafo['x'].screenMin;
    cSysY:=scalingOptions.axisTrafo['y'].screenMin;
    //coordinate system:======================================================
    //clear border:-----------------------------------------------------------
    target.Brush.style:=bsSolid;
    target.Brush.color:=clWhite;
    target.Pen.style:=psClear;
    target.Pen.width:=1;
    target.Pen.EndCap:=pecSquare;
    if (scalingOptions.axisStyle['y']<>[]) then target.FillRect(0,0,cSysX,target.height);
    if (scalingOptions.axisStyle['x']<>[]) then target.FillRect(cSysX,cSysY,target.width,target.height);
    //-----------------------------------------------------------:clear border
    //axis:-------------------------------------------------------------------
    target.Pen.style:=psSolid;
    target.Pen.color:=clBlack;
    target.Pen.width:=1;
    if (scalingOptions.axisStyle['y']<>[]) then target.line(cSysX, 0, cSysX, cSysY);
    if (scalingOptions.axisStyle['x']<>[]) then target.line(target.width, cSysY, cSysX, cSysY);
    //-------------------------------------------------------------------:axis
    //tics:-------------------------------------------------------------------
    if (scalingOptions.axisStyle['y']<>[]) then for i:=0 to length(gridTic['y'])-1 do with gridTic['y'][i] do if major then begin
      y:=round(pos);
      target.line(cSysX-5, y, cSysX, y);
      if (gse_tics in scalingOptions.axisStyle['y']) then target.textOut(cSysX-5-target.TextWidth(txt), y-target.TextHeight(txt) shr 1, txt);
    end;
    if (scalingOptions.axisStyle['x']<>[]) then for i:=0 to length(gridTic['x'])-1 do with gridTic['x'][i] do if major then begin
      x:=round(pos);
      target.line(x, cSysY+5, x, cSysY);
      if (gse_tics in scalingOptions.axisStyle['x']) then target.textOut(x-target.TextWidth(txt) shr 1, cSysY+5, txt);
    end;
    //-------------------------------------------------------------------:tics
    //======================================================:coordinate system
  end;

PROCEDURE scale(source: TImage; VAR dest: TImage; CONST factor: double);
  VAR ARect: TRect;
      X, Y: integer;
  begin
    X:=round(source.width*factor);
    Y:=round(source.height*factor);
    ARect:=Rect(0, 0, X, Y);
    dest.Canvas.AntialiasingMode:=amOn;
    dest.Canvas.StretchDraw(ARect, source.picture.Bitmap);
  end;

PROCEDURE T_plot.renderPlot(VAR plotImage: TImage; CONST supersampling:longint);
  VAR renderImage:TImage;
      gridTics:T_ticInfos;
  begin
    system.enterCriticalSection(cs);
    try
      scalingOptions.updateForPlot(plotImage.Canvas,plotImage.width,plotImage.height,row,gridTics);
      renderImage:=TImage.create(nil);
      renderImage.SetInitialBounds(0,0,plotImage.width*supersampling,plotImage.height*supersampling);
      drawGridAndRows(renderImage.Canvas,supersampling,gridTics);
      scale(renderImage,plotImage,1/supersampling);
      renderImage.destroy;
      drawCustomText(plotImage.Canvas,plotImage.width,plotImage.height);
      drawCoordSys(plotImage.Canvas,plotImage.width,plotImage.height,gridTics);
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_plot.obtainPlot(CONST width,height,supersampling:longint):TImage;
  VAR renderImage:TImage;
      gridTics:T_ticInfos;
  begin
    system.enterCriticalSection(cs);
    try
      renderImage:=TImage.create(nil);
      renderImage.SetInitialBounds(0,0,width*supersampling,height*supersampling);
      scalingOptions.updateForPlot(renderImage.Canvas,width,height,row,gridTics);
      drawGridAndRows(renderImage.Canvas,supersampling,gridTics);
      result:=TImage.create(nil);
      result.SetInitialBounds(0,0,width,height);
      scale(renderImage,result,1/supersampling);
      renderImage.destroy;
      drawCustomText(result.Canvas,width,height);
      drawCoordSys(result.Canvas,width,height,gridTics);
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

PROCEDURE T_plot.renderToFile(CONST fileName: string; CONST width, height, supersampling: longint);
  VAR storeImage:TImage;
  begin
    storeImage:=obtainPlot(width,height,supersampling);
    storeImage.picture.PNG.saveToFile(ChangeFileExt(fileName, '.png'));
    storeImage.destroy;
  end;

FUNCTION T_plot.renderToString(CONST width, height, supersampling: longint): ansistring;
  VAR storeImage: TImage;
      memStream: TStringStream;
  begin
    storeImage:=obtainPlot(width,height,supersampling);
    memStream := TStringStream.create('');
    storeImage.picture.PNG.saveToStream(memStream);
    memStream.position:=0;
    result:=memStream.DataString;
    memStream.free;
    storeImage.destroy;
  end;

PROCEDURE T_plot.CopyFrom(VAR p:T_plot);
  VAR i:longint;
  begin
    system.enterCriticalSection(cs);
    system.enterCriticalSection(p.cs);
    scalingOptions:=p.scalingOptions;
    for i:=0 to length(row)-1 do row[i].destroy;
    setLength(row,length(p.row));
    for i:=0 to length(row)-1 do begin
      row[i].create(i,p.row[i].sample);
      row[i].style:=p.row[i].style;
    end;
    system.leaveCriticalSection(p.cs);
    system.leaveCriticalSection(cs);
  end;

INITIALIZATION
  MAJOR_TIC_STYLE.create(0); MAJOR_TIC_STYLE.styleModifier:=0.2;
  MINOR_TIC_STYLE.create(0); MINOR_TIC_STYLE.styleModifier:=0.1;
FINALIZATION
  MAJOR_TIC_STYLE.destroy;
  MINOR_TIC_STYLE.destroy;
end.
