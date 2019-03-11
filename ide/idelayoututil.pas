UNIT ideLayoutUtil;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms,Controls,ComCtrls;

TYPE
  T_ideComponent=(icEditor,
                  icOutline,
                  icHelp,
                  icAssistance,
                  icOutput,
                  icQuickEval,
                  icPlot,
                  icCustomForm,
                  icTable,
                  icVariableView,
                  icDebugger);

  T_componentParent=(cpNone,
                     cpPageControl1,
                     cpPageControl2,
                     cpPageControl3,
                     cpPageControl4,
                     cpPageControl5);

  T_splitterPositions=array[1..4] of longint;

  T_componentPosition=record
    parent:T_componentParent;
    top,Left,width,height:longint;
  end;

  T_mnhComponentForm=class(TForm)
    private
      myComponentParent:T_componentParent;
    published
      CONSTRUCTOR create(TheOwner: TComponent); override;
      PROCEDURE defaultEndDock(Sender, target: TObject; X,Y: integer);
      FUNCTION getIdeComponentType:T_ideComponent; virtual; abstract;
  end;

VAR lastDockLocationFor:array[T_ideComponent] of T_componentParent
    {icEditor}    =(cpPageControl5,
    {icOutline}     cpPageControl3,
    {icHelp}        cpPageControl2,
    {icAssistance}  cpPageControl2,
    {icOutput}      cpPageControl2,
    {icQuickEval}   cpPageControl2,
    {icPlot}        cpNone,
    {icCustomForm}  cpNone,
    {icTable}       cpNone,
    {icVariableView}cpNone,
    {icDebugger}    cpPageControl4);
IMPLEMENTATION
CONSTRUCTOR T_mnhComponentForm.create(TheOwner: TComponent);
  begin
    inherited create(TheOwner);
    OnEndDock:=@defaultEndDock;
  end;

PROCEDURE T_mnhComponentForm.defaultEndDock(Sender, target: TObject; X, Y: integer);
  VAR n:string;
  begin
    if (target<>nil) and target.ClassNameIs('TPageControl') then begin
      n:=TPageControl(target).name;
      writeln('Dock @',n);
      if (n.endsWith('1')) then myComponentParent:=cpPageControl1;
      if (n.endsWith('2')) then myComponentParent:=cpPageControl2;
      if (n.endsWith('3')) then myComponentParent:=cpPageControl3;
      if (n.endsWith('4')) then myComponentParent:=cpPageControl4;
      if (n.endsWith('5')) then myComponentParent:=cpPageControl5;
    end;
    myComponentParent:=cpNone;
    lastDockLocationFor[getIdeComponentType]:=myComponentParent;
  end;

INITIALIZATION
  initialize(lastDockLocationFor);

end.

