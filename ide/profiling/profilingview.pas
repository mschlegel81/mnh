UNIT profilingView;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Grids, Menus,profiling,ideLayoutUtil,mnh_messages,out_adapters;

TYPE
  TprofilingOutputForm = class(T_mnhComponentForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    MainMenu1: TMainMenu;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    StringGrid3: TStringGrid;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE StringGrid1HeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
    PROCEDURE StringGrid1PrepareCanvas(Sender: TObject; aCol, aRow: integer;
      aState: TGridDrawState);
    PROCEDURE StringGrid1Selection(Sender: TObject; aCol, aRow: integer);
    PROCEDURE StringGrid2HeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
    PROCEDURE StringGrid1KeyPress(Sender: TObject; VAR key: char);
    PROCEDURE StringGrid2HeaderSized(Sender: TObject; IsColumn: boolean; index: integer);
    PROCEDURE StringGrid2KeyPress(Sender: TObject; VAR key: char);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate(CONST isEvaluationRunning:boolean); override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE StringGrid3HeaderSized(Sender: TObject; IsColumn: boolean;index: integer);
    PROCEDURE StringGrid3KeyPress(Sender: TObject; VAR key: char);
    PROCEDURE dockChanged; override;
  private
    grid1Sorting:byte;
    grid2Sorting:byte;
    profilingList:T_profilingList;
    PROCEDURE setProfilingList(CONST list:T_profilingList);
    PROCEDURE fillGrid1;
    PROCEDURE fillGrids2and3;
  public

  end;

  P_profileAdapter=^T_profileAdapter;
  T_profileAdapter=object(T_abstractGuiOutAdapter)
    defaultCaption:string;
    CONSTRUCTOR create();
    FUNCTION flushToGui(CONST forceFlush:boolean):T_messageTypeSet; virtual;
  end;

IMPLEMENTATION
USES editorMeta,basicTypes,mnh_settings;
{$R *.lfm}
VAR myProfilingForm:TprofilingOutputForm=nil;
FUNCTION ensureProfileView:TprofilingOutputForm;
  begin
    if myProfilingForm=nil then begin
      myProfilingForm:=TprofilingOutputForm.create(Application);
      dockNewForm(myProfilingForm);
    end;
    myProfilingForm.showComponent(true);
    result:=myProfilingForm;
  end;

CONSTRUCTOR T_profileAdapter.create();
  begin
    inherited create(at_profilingView,[mt_profile_call_info]);
  end;

FUNCTION T_profileAdapter.flushToGui(CONST forceFlush: boolean): T_messageTypeSet;
  VAR i:longint;
  begin
    result:=[];
    enterCriticalSection(adapterCs);
    try
      for i:=0 to collectedFill-1 do case collected[i]^.messageType of
        mt_profile_call_info:
          begin
            include(result,collected[i]^.messageType);
            ensureProfileView.setProfilingList(P_profileMessage(collected[i])^.content);
          end;
      end;
      clear;
    finally
      leaveCriticalSection(adapterCs);
    end;
  end;

PROCEDURE TprofilingOutputForm.StringGrid2HeaderClick(Sender: TObject;
  IsColumn: boolean; index: integer);
  VAR k:longint;
  begin
    if not(IsColumn) then exit;
    if index*2 = grid2Sorting
    then grid2Sorting:=index*2+1
    else grid2Sorting:=index*2;
    for k:=0 to length(profilingList)-1 do begin
      sortCallerList(profilingList[k].callers,grid2Sorting);
      sortCallerList(profilingList[k].callees,grid2Sorting);
    end;
    fillGrids2and3;
  end;

PROCEDURE TprofilingOutputForm.StringGrid1HeaderClick(Sender: TObject;
  IsColumn: boolean; index: integer);
  begin
    if not(IsColumn) then exit;
    if index*2 = grid1Sorting
    then grid1Sorting:=index*2+1
    else grid1Sorting:=index*2;
    sortProfilingList(profilingList,grid1Sorting);
    fillGrid1;
  end;

PROCEDURE TprofilingOutputForm.StringGrid1PrepareCanvas(Sender: TObject; aCol,
  aRow: integer; aState: TGridDrawState);
  VAR style:TTextStyle;
      myGrid:TStringGrid;
begin
  if Sender.ClassType<>TStringGrid.ClassType then exit;
  myGrid:=TStringGrid(Sender);
  style:=myGrid.Canvas.TextStyle;
  if (aCol>=1) and (aRow>=1)
  then style.Alignment:=taRightJustify
  else style.Alignment:=taLeftJustify;
  myGrid.Canvas.TextStyle:=style;
end;

PROCEDURE TprofilingOutputForm.FormCreate(Sender: TObject);
  begin
    grid1Sorting:=255;
    grid2Sorting:=255;
    registerFontControl(StringGrid1,ctTable);
    registerFontControl(StringGrid2,ctTable);
    registerFontControl(StringGrid3,ctTable);
    initDockMenuItems(MainMenu1,nil);
    initDockMenuItems(PopupMenu1,PopupMenu1.items);
  end;

PROCEDURE TprofilingOutputForm.FormDestroy(Sender: TObject);
  begin
    unregisterFontControl(StringGrid1);
    unregisterFontControl(StringGrid2);
    unregisterFontControl(StringGrid3);
  end;

PROCEDURE TprofilingOutputForm.StringGrid1Selection(Sender: TObject; aCol,
  aRow: integer);
  begin
    fillGrids2and3;
  end;

PROCEDURE TprofilingOutputForm.StringGrid1KeyPress(Sender: TObject;
  VAR key: char);
  begin
    if key=#13 then workspace.openLocation(guessLocationFromString( StringGrid1.Cells[1,StringGrid1.selection.top],false));
  end;

PROCEDURE TprofilingOutputForm.StringGrid2HeaderSized(Sender: TObject;
  IsColumn: boolean; index: integer);
  begin
    if IsColumn then
    StringGrid3.ColWidths[index]:=StringGrid2.ColWidths[index];
  end;

PROCEDURE TprofilingOutputForm.StringGrid3HeaderSized(Sender: TObject;
  IsColumn: boolean; index: integer);
  begin
    if IsColumn then
    StringGrid2.ColWidths[index]:=StringGrid3.ColWidths[index];
  end;

PROCEDURE TprofilingOutputForm.StringGrid2KeyPress(Sender: TObject;
  VAR key: char);
  begin
    if key=#13 then workspace.openLocation(guessLocationFromString(StringGrid2.Cells[1,StringGrid2.selection.top],false));
  end;

PROCEDURE TprofilingOutputForm.StringGrid3KeyPress(Sender: TObject;
  VAR key: char);
  begin
    if key=#13 then workspace.openLocation(guessLocationFromString(StringGrid3.Cells[1,StringGrid3.selection.top],false));
  end;

PROCEDURE TprofilingOutputForm.dockChanged;
  begin
  end;

FUNCTION TprofilingOutputForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icProfilingOutput;
  end;

PROCEDURE TprofilingOutputForm.performSlowUpdate(CONST isEvaluationRunning:boolean); begin end;
PROCEDURE TprofilingOutputForm.performFastUpdate; begin end;

PROCEDURE TprofilingOutputForm.setProfilingList(CONST list: T_profilingList);
  VAR k:longint;
  begin
    profilingList:=list;
    if grid1Sorting<>255 then                                        sortProfilingList(profilingList        ,grid1Sorting);;
    if grid2Sorting<>255 then for k:=0 to length(profilingList)-1 do begin
      sortCallerList(profilingList[k].callers,grid2Sorting);
      sortCallerList(profilingList[k].callees,grid2Sorting);
    end;
    fillGrid1;
    StringGrid2.RowCount:=1;
    StringGrid3.RowCount:=1;
  end;

PROCEDURE TprofilingOutputForm.fillGrid1;
  VAR i:longint;
  begin
    StringGrid1.RowCount:=1+length(profilingList);
    for i:=0 to length(profilingList)-1 do begin
      StringGrid1.Cells[0,i+1]:=profilingList[i].id;
      StringGrid1.Cells[1,i+1]:=profilingList[i].calleeLocation;
      StringGrid1.Cells[2,i+1]:=intToStr(profilingList[i].aggTime.callCount);
      StringGrid1.Cells[3,i+1]:=formatFloat('0.000',profilingList[i].aggTime.timeSpent_inclusive*1E3)+'ms';
      StringGrid1.Cells[4,i+1]:=formatFloat('0.000',profilingList[i].aggTime.timeSpent_exclusive*1E3)+'ms';
    end;
  end;

PROCEDURE TprofilingOutputForm.fillGrids2and3;
  VAR i,k:longint;
  begin
    k:=StringGrid1.selection.top-1;
    if (k>=0) and (k<length(profilingList)) then begin
      StringGrid2.RowCount:=length(profilingList[k].callers)+1;
      for i:=0 to length(profilingList[k].callers)-1 do begin
        StringGrid2.Cells[0,i+1]:=profilingList[k].callers[i].id;
        StringGrid2.Cells[1,i+1]:=profilingList[k].callers[i].location;
        StringGrid2.Cells[2,i+1]:=intToStr(profilingList[k].callers[i].time.callCount);
        StringGrid2.Cells[3,i+1]:=formatFloat('0.000',profilingList[k].callers[i].time.timeSpent_inclusive*1E3)+'ms';
        StringGrid2.Cells[4,i+1]:=formatFloat('0.000',profilingList[k].callers[i].time.timeSpent_exclusive*1E3)+'ms';
      end;

      StringGrid3.RowCount:=length(profilingList[k].callees)+1;
      for i:=0 to length(profilingList[k].callees)-1 do begin
        StringGrid3.Cells[0,i+1]:=profilingList[k].callees[i].id;
        StringGrid3.Cells[1,i+1]:=profilingList[k].callees[i].location;
        StringGrid3.Cells[2,i+1]:=intToStr(profilingList[k].callees[i].time.callCount);
        StringGrid3.Cells[3,i+1]:=formatFloat('0.000',profilingList[k].callees[i].time.timeSpent_inclusive*1E3)+'ms';
        StringGrid3.Cells[4,i+1]:=formatFloat('0.000',profilingList[k].callees[i].time.timeSpent_exclusive*1E3)+'ms';
      end;
    end else begin
      StringGrid2.RowCount:=1;
      StringGrid3.RowCount:=1;
    end;
  end;

end.

