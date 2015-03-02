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
    PROCEDURE ChartParametricCurveSeries1Calculate(CONST AT: Double; OUT AX,
      AY: Double);
  private
    series:array of TLineSeries;
    currentSeries:array of TLineSeries;
    { private declarations }
  public
    PROCEDURE testChart;
    { public declarations }
  end;

VAR
  ChartForm: TChartForm;

implementation

{$R *.lfm}

{ TChartForm }

PROCEDURE TChartForm.ChartParametricCurveSeries1Calculate(CONST AT: Double; OUT AX, AY: Double);
begin

end;

PROCEDURE TChartForm.testChart;
begin

end;


end.

