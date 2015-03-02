unit mnh_chartForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TATransformations, TASources, TASeries,
  TALegendPanel, TAFuncSeries, Forms, Controls, Graphics, Dialogs, Menus;

type

  { TChartForm }

  TChartForm = class(TForm)
    Chart: TChart;
    ChartLineSeries1: TLineSeries;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miExport: TMenuItem;
    procedure ChartParametricCurveSeries1Calculate(const AT: Double; out AX,
      AY: Double);
  private
    series:array of TLineSeries;
    currentSeries:array of TLineSeries;
    { private declarations }
  public
    PROCEDURE testChart;
    { public declarations }
  end;

var
  ChartForm: TChartForm;

implementation

{$R *.lfm}

{ TChartForm }

procedure TChartForm.ChartParametricCurveSeries1Calculate(const AT: Double; out AX, AY: Double);
begin

end;

procedure TChartForm.testChart;
begin

end;


end.

