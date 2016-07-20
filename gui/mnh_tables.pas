UNIT mnh_tables;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids, Menus,
  mnh_litVar, mnh_funcs,mnh_constants,mnh_contexts,mnh_out_adapters,mnh_tokLoc,
  myGenerics,myStringUtil,mnh_fileWrappers;

TYPE

  { TtableForm }

  TtableForm = class(TForm)
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miIncreaseFontSize: TMenuItem;
    miDecreaseFontSize: TMenuItem;
    mi_exportText: TMenuItem;
    mi_exportCsvTab: TMenuItem;
    mi_exportCsvSemicolon: TMenuItem;
    mi_transpose: TMenuItem;
    mi_comma: TMenuItem;
    SaveTableDialog: TSaveDialog;
    tableMenu: TMainMenu;
    StringGrid: TStringGrid;
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE miDecreaseFontSizeClick(Sender: TObject);
    PROCEDURE miIncreaseFontSizeClick(Sender: TObject);
    PROCEDURE mi_commaClick(Sender: TObject);
    PROCEDURE mi_exportCsvSemicolonClick(Sender: TObject);
    PROCEDURE mi_exportCsvTabClick(Sender: TObject);
    PROCEDURE mi_exportTextClick(Sender: TObject);
    PROCEDURE mi_transposeClick(Sender: TObject);
    PROCEDURE stringGridKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  private
    { private declarations }
    cs:TRTLCriticalSection;
    currentLiteral,requestedLiteral:P_listLiteral;
    requestedCaption:string;
    displayPending:boolean;
  public
    { public declarations }
    PROCEDURE initWithLiteral(CONST L:P_listLiteral; CONST newCaption:string);
    FUNCTION isDisplayPending:boolean;
    PROCEDURE conditionalDoShow;
    PROCEDURE fillTable;
  end;

VAR
  tableForm: TtableForm;
  formCycleCallback    : PROCEDURE(CONST ownId:longint; CONST next:boolean) = nil;

IMPLEMENTATION
{$R *.lfm}

FUNCTION showTable_impl(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation; VAR context:T_evaluationContext): P_literal;
  VAR Caption:string='MNH table';
  begin
    context.adapters^.raiseCustomMessage(mt_displayTable,'',tokenLocation);
    if not(gui_started) then exit(nil);
    if (params<>nil) and
       (params^.size>0) and
       (params^.value(0)^.literalType in C_validListTypes) and
       ((params^.size=1) or (params^.size=2) and (params^.value(1)^.literalType=lt_string)) then begin
      if params^.size=2 then Caption:=P_stringLiteral(params^.value(1))^.value;
      tableForm.initWithLiteral(P_listLiteral(params^.value(0)),Caption);
      result:=newVoidLiteral;
    end else result:=nil;
  end;

PROCEDURE TtableForm.FormCreate(Sender: TObject);
  begin
    initCriticalSection(cs);
    currentLiteral:=nil;
    requestedLiteral:=nil;
  end;

PROCEDURE TtableForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    enterCriticalSection(cs);
    if currentLiteral<>nil then begin
      disposeLiteral(currentLiteral);
      currentLiteral:=nil;
    end;
    leaveCriticalSection(cs);
  end;

PROCEDURE TtableForm.FormDestroy(Sender: TObject);
begin
  doneCriticalSection(cs);
end;

PROCEDURE TtableForm.FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=9) and (ssCtrl in Shift) and (formCycleCallback<>nil) then formCycleCallback(2,ssShift in Shift);
    if (key=65) and (ssCtrl in Shift) then StringGrid.selection:=Rect(0,1,StringGrid.ColCount-1,StringGrid.RowCount-1);
  end;

PROCEDURE TtableForm.FormShow(Sender: TObject);
  begin
    {$ifdef UNIX}
    miIncreaseFontSize.ShortCut:=16605;
    {$endif}
    position:=poDefault;
  end;

PROCEDURE TtableForm.miDecreaseFontSizeClick(Sender: TObject);
  begin
    StringGrid.Font.size:=StringGrid.Font.size-1;
    StringGrid.AutoSizeColumns;
  end;

PROCEDURE TtableForm.miIncreaseFontSizeClick(Sender: TObject);
  begin
    StringGrid.Font.size:=StringGrid.Font.size+1;
    StringGrid.AutoSizeColumns;
  end;

PROCEDURE TtableForm.mi_commaClick(Sender: TObject);
  begin
    mi_comma.Checked:=not(mi_comma.Checked);
    fillTable;
  end;

PROCEDURE TtableForm.mi_exportCsvSemicolonClick(Sender: TObject);
  begin
    if SaveTableDialog.execute then StringGrid.SaveToCSVFile(SaveTableDialog.fileName,';',false,true);
  end;

PROCEDURE TtableForm.mi_exportCsvTabClick(Sender: TObject);
  begin
    if SaveTableDialog.execute then StringGrid.SaveToCSVFile(SaveTableDialog.fileName,C_tabChar,false,true);
  end;

PROCEDURE TtableForm.mi_exportTextClick(Sender: TObject);
  VAR i,j:longint;
      content:T_arrayOfString;
      row:ansistring;
  begin
    if SaveTableDialog.execute then begin
      setLength(content,StringGrid.RowCount-1);
      for i:=1 to StringGrid.RowCount-1 do begin
        row:='';
        for j:=0 to StringGrid.ColCount-1 do row:=row+StringGrid.Cells[j,i]+C_tabChar;
        content[i-1]:=row;
      end;
      content:=formatTabs(content);
      writeFileLines(SaveTableDialog.fileName,formatTabs(content),LineEnding);
    end;
  end;

PROCEDURE TtableForm.mi_transposeClick(Sender: TObject);
  begin
    mi_transpose.Checked:=not(mi_transpose.Checked);
    fillTable;
  end;

PROCEDURE TtableForm.stringGridKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    FormKeyUp(Sender,key,Shift);
  end;

PROCEDURE TtableForm.initWithLiteral(CONST L: P_listLiteral;
  CONST newCaption: string);
  begin
    enterCriticalSection(cs);
    displayPending:=true;
    if requestedLiteral<>nil then disposeLiteral(requestedLiteral);
    requestedLiteral:=L;
    requestedLiteral^.rereference;
    requestedCaption:=newCaption;
    leaveCriticalSection(cs);
  end;

PROCEDURE TtableForm.conditionalDoShow;
  begin
    enterCriticalSection(cs);
    if displayPending then begin
      displayPending:=false;
      Caption:=requestedCaption;
      if currentLiteral<>nil then disposeLiteral(currentLiteral);
      currentLiteral:=requestedLiteral;
      requestedLiteral:=nil;
      Show;
      fillTable;
      StringGrid.AutoSizeColumns;
    end;
    leaveCriticalSection(cs);
  end;

PROCEDURE TtableForm.fillTable;
  VAR dataRows:longint;
      dataColumns:longint=0;
      cellContents:array of T_arrayOfString;
      i,j:longint;
      rowLit:P_literal;
      cellLit:P_literal;
  begin
    if currentLiteral=nil then exit;
    dataRows:=currentLiteral^.size;
    setLength(cellContents,dataRows);
    if currentLiteral^.literalType=lt_stringList then begin
      for i:=0 to currentLiteral^.size-1 do begin
        cellContents[i]:=split(P_stringLiteral(currentLiteral^.value(i))^.value,C_tabChar);
        if length(cellContents[i])>dataColumns then dataColumns:=length(cellContents[i]);
      end;
    end else begin
      for i:=0 to currentLiteral^.size-1 do begin
        rowLit:=currentLiteral^.value(i);
        if rowLit^.literalType in C_validListTypes then begin
          setLength(cellContents[i],P_listLiteral(rowLit)^.size);
          for j:=0 to P_listLiteral(rowLit)^.size-1 do begin
            cellLit:=P_listLiteral(rowLit)^.value(j);
            case cellLit^.literalType of
              lt_string:cellContents[i,j]:=P_stringLiteral(cellLit)^.value;
              lt_real,lt_realList,lt_numList:if mi_comma.Checked then cellContents[i,j]:=replaceAll(cellLit^.toString,'.',',')
                                                                 else cellContents[i,j]:=           cellLit^.toString;
              else cellContents[i,j]:=cellLit^.toString;
            end;
          end;
        end else begin
          setLength(cellContents[i],1);
          cellLit:=rowLit; j:=0;
          case cellLit^.literalType of
            lt_string:cellContents[i,j]:=P_stringLiteral(cellLit)^.value;
            lt_real,lt_realList,lt_numList:if mi_comma.Checked then cellContents[i,j]:=replaceAll(cellLit^.toString,'.',',')
                                                               else cellContents[i,j]:=           cellLit^.toString;
            else cellContents[i,j]:=cellLit^.toString;
          end;
        end;
        if length(cellContents[i])>dataColumns then dataColumns:=length(cellContents[i]);
      end;
    end;
    StringGrid.clear;
    if mi_transpose.Checked then begin
      StringGrid.RowCount:=dataColumns+1;
      StringGrid.ColCount:=dataRows;
      for i:=0 to length(cellContents)-1 do
      for j:=0 to length(cellContents[i])-1 do
      StringGrid.Cells[i,j+1]:=cellContents[i,j];
    end else begin
      StringGrid.RowCount:=dataRows+1;
      StringGrid.ColCount:=dataColumns;
      for i:=0 to length(cellContents)-1 do
      for j:=0 to length(cellContents[i])-1 do
      StringGrid.Cells[j,i+1]:=cellContents[i,j];
    end;
    StringGrid.FixedRows:=1;
  end;

FUNCTION TtableForm.isDisplayPending: boolean;
  begin
    enterCriticalSection(cs);
    result:=displayPending;
    leaveCriticalSection(cs);
  end;

INITIALIZATION
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'showTable',@showTable_impl,'showTable(L:list);#Shows L in a table.#showTable(L:list,caption:string);#Shows L in a table with given caption.');

end.

