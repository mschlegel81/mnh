unit mnh_plots;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, mnh_funcs, mnh_tokloc, mnh_litvar, mnh_tokens;

type

  { TplotForm }

  TplotForm = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    miLogscaleY: TMenuItem;
    miLogscaleX: TMenuItem;
    miPreserveAspect: TMenuItem;
    plotImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    axInfo:array['x'..'y'] of record
      pixelExtend,
      pixelOffset:longint;
      lower:double;
      range:double;
    end;


    { private declarations }
  public
    { public declarations }
    PROCEDURE plotFrame(CONST clearBefore:boolean);
  end;


var
  plotForm: TplotForm;


implementation

{$R *.lfm}

{ TplotForm }

procedure TplotForm.FormCreate(Sender: TObject);
  VAR axis:char;
  begin
    for axis:='x' to 'y' do with axInfo[axis] do begin
      lower:=-1;
      range:= 2;
      pixelOffset:=20;
      pixelExtend:=500;
    end;
  end;

procedure TplotForm.FormResize(Sender: TObject);
  begin
    plotImage.Align:=alClient;
    plotFrame(true);
  end;

procedure TplotForm.plotFrame(const clearBefore: boolean);
  VAR tics:array['x'..'y'] of array of record sp:longint; wp:extended; prio:longint; end;
  PROCEDURE prepareTics;
    VAR axis:char;
    begin
      for axis:='x' to 'y' do with axInfo[axis] do begin
        setLength(tics[axis],0);



      end;


    end;

  begin
    plotImage.Canvas.AntialiasingMode:=amOff;
    //Clear:--------------------------------------------------------------------
    plotImage.Canvas.Brush.Style:=bsSolid;
    plotImage.Canvas.Brush.Color:=clWhite;
    plotImage.Canvas.Clear;
    //--------------------------------------------------------------------:Clear

    plotImage.Canvas.Pen.Color:=clBlack;
    plotImage.Canvas.Pen.Style:=psSolid;
    axInfo['y'].pixelOffset:=plotImage.Canvas.TextHeight('0123456789E+-.')+5;
    axInfo['y'].pixelExtend:=plotImage.Height-axInfo['y'].pixelOffset;


    plotImage.Canvas.Line(  axInfo['x'].pixelOffset,0,
                            axInfo['x'].pixelOffset,axInfo['y'].pixelExtend);
    plotImage.Canvas.LineTo(plotImage.Width      ,axInfo['y'].pixelExtend);

    plotImage.Canvas.TextOut(  0,plotImage.Height-axInfo['y'].pixelOffset+5,'132');
    plotImage.Canvas.TextOut(100,plotImage.Height-axInfo['y'].pixelOffset+5,'23.4');
    plotImage.Canvas.TextOut(200,plotImage.Height-axInfo['y'].pixelOffset+5,'3.45');
  end;

end.

