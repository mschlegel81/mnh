unit mnh_plots;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, mnh_funcs, mnh_tokloc, mnh_litvar, mnh_tokens, math;

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
    procedure miPreserveAspectClick(Sender: TObject);
  private
    axInfo:array['x'..'y'] of record
      pixelExtend,
      pixelOffset:longint;
      lower:double;
      range:double;
    end;
    FUNCTION w2s(CONST axis:char; CONST w:double):double;
    FUNCTION s2w(CONST axis:char; CONST s:double):double;
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

procedure TplotForm.miPreserveAspectClick(Sender: TObject);
  begin
    miPreserveAspect.Checked:=true;
    if     miLogscaleX.Checked  and not(miLogscaleY.Checked) then miLogscaleY.Checked:= true;
    if not(miLogscaleX.Checked) and     miLogscaleY.Checked  then miLogscaleY.Checked:=false;
  end;

function TplotForm.w2s(CONST axis:char; const w: double): double;
  VAR logscale:boolean;
  begin
    logscale:=(axis='x' and miLogscaleX.Checked)
           or (axis='y' and miLogscaleY.Checked);
    with axInfo[axis] do result:=(w-lower)/range*pixelExtend+pixelOffset;
  end;

function TplotForm.s2w(CONST axis:char; const s: double): double;
  begin with axInfo[axis] do result:=(s-pixelOffset)/pixelExtend*range+lower; end;

procedure TplotForm.plotFrame(const clearBefore: boolean);
  VAR tics:array['x'..'y'] of array of record sp:longint; wp:extended; prio:longint; ticTxt:ansistring; end;
  FUNCTION prioByIdx(idx:longint):longint;
    begin
      if      idx mod 100000=0 then result:=10
      else if idx mod  50000=0 then result:= 9
      else if idx mod  10000=0 then result:= 8
      else if idx mod   5000=0 then result:= 7
      else if idx mod   1000=0 then result:= 6
      else if idx mod    500=0 then result:= 5
      else if idx mod    100=0 then result:= 4
      else if idx mod     50=0 then result:= 3
      else if idx mod     10=0 then result:= 2
      else if idx mod      5=0 then result:= 1
      else                          result:= 0;
    end;

  PROCEDURE prepareTics;
    PROCEDURE addTic(CONST axis:char; CONST worldPos:double; CONST priority:longint);
      begin
        setLength(tics[axis],length(tics[axis])+1);
        with tics[axis][length(tics[axis])-1] do begin
          wp:=worldPos;
          sp:=round(w2s(axis,worldPos));
          prio:=priority;

        end;
      end;

    VAR axis:char;
        i0,i1,i,pow:longint;
        smallStep:double;
    begin
      for axis:='x' to 'y' do with axInfo[axis] do begin
        setLength(tics[axis],0);
        if ((axis='x') and miLogscaleX.Checked) or
           ((axis='y') and miLogscaleY.Checked) then begin
          //log:----------------------------------------------------------------
          i0:=floor(ln(lower      )/ln(10));
          i1:=ceil (ln(lower+range)/ln(10));
          for pow:=i0 to i1 do begin
            smallStep:=10**pow;
            for i:=1 to 9 do addTic(axis,i*smallStep);

          end;
          //----------------------------------------------------------------:log
        end else begin
          //lin:----------------------------------------------------------------
          pow:=floor(ln(range)/ln(10))-2;
          smallStep:=10**pow;
          i0:=floor(lower        /smallStep);
          i1:=ceil ((lower+range)/smallStep);
          setLength(tics[axis],i1-i0+1);
          for i:=0 to i1-i0 do begin
            tics[axis,i].wp  :=i*smallStep;
            tics[axis,i].prio:=prioByIdx(i);
            tics[axis,i].sp  :=pixelOffset;
          end;
          //----------------------------------------------------------------:lin
        end;
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
    axInfo['y'].pixelExtend:=axInfo['y'].pixelOffset-plotImage.Height;

    plotImage.Canvas.Line(  axInfo['x'].pixelOffset,0,
                            axInfo['x'].pixelOffset,axInfo['y'].pixelExtend);
    plotImage.Canvas.LineTo(plotImage.Width        ,axInfo['y'].pixelExtend);

    plotImage.Canvas.TextOut(  0,plotImage.Height-axInfo['y'].pixelOffset+5,'132');
    plotImage.Canvas.TextOut(100,plotImage.Height-axInfo['y'].pixelOffset+5,'23.4');
    plotImage.Canvas.TextOut(200,plotImage.Height-axInfo['y'].pixelOffset+5,'3.45');
  end;

end.

