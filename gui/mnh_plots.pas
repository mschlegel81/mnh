unit mnh_plots;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, mnh_funcs, mnh_litvar, mnh_tokens, math, mnh_plotData;

type
  { TplotForm }

  TplotForm = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    miAntialiasing: TMenuItem;
    miLogscaleY: TMenuItem;
    miLogscaleX: TMenuItem;
    miPreserveAspect: TMenuItem;
    plotImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure miAntialiasingClick(Sender: TObject);
    procedure miPreserveAspectClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    PROCEDURE doPlot();
  end;


var
  plotForm: TplotForm;

implementation

{$R *.lfm}

{ TplotForm }

procedure TplotForm.FormCreate(Sender: TObject);
  begin
  end;

procedure TplotForm.FormResize(Sender: TObject);
  begin
    plotImage.Align:=alClient;
    doPlot();
  end;

procedure TplotForm.miAntialiasingClick(Sender: TObject);
begin
  miAntialiasing.Checked:=not(miAntialiasing.Checked);
  doPlot();
end;

procedure TplotForm.miPreserveAspectClick(Sender: TObject);
  begin
    miPreserveAspect.Checked:=true;
    if     miLogscaleX.Checked  and not(miLogscaleY.Checked) then miLogscaleY.Checked:= true;
    if not(miLogscaleX.Checked) and     miLogscaleY.Checked  then miLogscaleY.Checked:=false;
  end;

procedure TplotForm.doPlot();
  VAR wcol:array of word;
  PROCEDURE readPixels;
    VAR i,j,k,abgr:longint;
    begin
      for j:=0 to plotImage.Height-1 do
      for i:=0 to plotImage.Width-1 do begin
        k:=(j*plotImage.Width+i)*3;
        abgr:=plotImage.Canvas.Pixels[i,j];
        if length(wcol)<k+3 then begin
          SetLength(wcol,k+3);
          wcol[k  ]:= abgr         and 255;
          wcol[k+1]:=(abgr shr  8) and 255;
          wcol[k+2]:=(abgr shr 16) and 255;
        end else begin
          inc(wcol[k  ], abgr         and 255);
          inc(wcol[k+1],(abgr shr  8) and 255);
          inc(wcol[k+2],(abgr shr 16) and 255);
        end;
      end;
    end;

  PROCEDURE averagePixels;
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

  PROCEDURE plotWithOffset(CONST dx,dy:double);
    VAR rowId,i,x,y:longint;
        lastWasValid,currentIsValid:boolean;
        sample:T_point;
    begin
      //Clear:------------------------------------------------------------------
      plotImage.Canvas.Brush.Style:=bsSolid;
      plotImage.Canvas.Brush.Color:=clWhite;
      plotImage.Canvas.Pen.Style:=psClear;
      plotImage.Canvas.Clear;
      //------------------------------------------------------------------:Clear
      //coordinate grid:========================================================
      plotImage.Canvas.Pen.Style:=psSolid;
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
      for rowId:=0 to length(activePlot.row)-1 do begin
        if activePlot.row[rowId].style.wantStraightLines then begin
          lastWasValid:=false;
          plotImage.Canvas.Pen.Color:=activePlot.row[rowId].style.getTColor;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=not(IsNan(sample[0])) and not(IsNan(sample[1]));
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              if lastWasValid then plotImage.Canvas.LineTo(round(sample[0]+dx),round(sample[1]+dy))
                              else plotImage.Canvas.MoveTo(round(sample[0]+dx),round(sample[1]+dy));
            end;
            lastWasValid:=currentIsValid;
          end;
        end;
      end;
      //===============================================================:row data
      //coordinate system:======================================================
      //clear border:-----------------------------------------------------------
      plotImage.Canvas.Brush.Style:=bsSolid;
      plotImage.Canvas.Brush.Color:=clWhite;
      plotImage.Canvas.Pen.Style:=psClear;
      if activePlot.wantTics('y') then
        plotImage.Canvas.FillRect(0,0,activePlot.xOffset,plotImage.Height);
      if activePlot.wantTics('x') then
        plotImage.Canvas.FillRect(activePlot.xOffset,activePlot.yOffset,
                                  plotImage.Width   ,plotImage.Height);
      //-----------------------------------------------------------:clear border
      //axis:-------------------------------------------------------------------
      plotImage.Canvas.Pen.Style:=psSolid;
      plotImage.Canvas.Pen.Color:=clBlack;
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
    plotImage.Canvas.AntialiasingMode:=amOff;
    //Prepare transformations:--------------------------------------------------
    activePlot.setScreenSize(plotImage.Width,plotImage.Height);
    repeat until not(
      activePlot.setTextSize(
        plotImage.Canvas.TextHeight(activePlot.longtestYTic),
        plotImage.Canvas.TextWidth (activePlot.longtestYTic)));
    //--------------------------------------------------:Prepare transformations
    if miAntialiasing.Checked then begin
      for i:=0 to 3 do for j:=0 to 3 do begin
        plotWithOffset(i*0.25-0.375,j*0.25-0.375);
        readPixels;
      end;
      averagePixels;
    end else plotWithOffset(0,0);
  end;

end.

