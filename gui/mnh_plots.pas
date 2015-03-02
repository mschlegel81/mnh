unit mnh_plots;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, mnh_funcs, mnh_tokloc, mnh_litvar, mnh_tokens, math, myFloat,myFiles,mnh_plotData;

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
    doPlot(true);
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

    begin
      //Clear:------------------------------------------------------------------
      plotImage.Canvas.Brush.Style:=bsSolid;
      plotImage.Canvas.Brush.Color:=clWhite;
      plotImage.Canvas.Clear;
      //------------------------------------------------------------------:Clear
      //coordinate system:======================================================
      //axis:-------------------------------------------------------------------
      plotImage.Canvas.Pen.Style:=bsSolid;
      plotImage.Canvas.Pen.Color:=clBlack;
      plotImage.Canvas.MoveTo(activePlot.xOffset,0);
      plotImage.Canvas.LineTo(activePlot.xOffset    ,activePlot.yOffset);
      plotImage.Canvas.LineTo(activePlot.screenWidth,activePlot.yOffset);
      //-------------------------------------------------------------------:axis
      //tics:-------------------------------------------------------------------
      if activePlot.axisStyle['x'];
      //-------------------------------------------------------------------:tics
      //======================================================:coordinate system
    end;

  VAR i:longint;
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
      for i:=0 to 15 do begin
        plotWithOffset(0.5-random,0.5-random);
        readPixels;
      end;
      averagePixels;
    end else plotWithOffset(0,0);
  end;

end.

