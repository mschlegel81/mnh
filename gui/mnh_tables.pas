UNIT mnh_tables;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  mnh_litVar, mnh_constants, mnh_tokLoc, mnh_out_adapters, mnh_funcs,mnh_contexts;

CONST
  C_READ_ONLY_KEY='readOnly';
  C_VALUE_KEY='value';

TYPE
  T_cellIndex=record
    row,col:longint;
  end;

  T_cell=object
    index:T_cellIndex;
    givenValue,
    calculatedValue:P_literal;
    readonly,
    calculated,
    ready:boolean;

    CONSTRUCTOR create(CONST cellIndex:T_cellIndex; CONST initialValue:P_literal; CONST isReadOnly:boolean);
    DESTRUCTOR destroy;
    FUNCTION getValue:P_literal;
    PROCEDURE setValue(CONST L:P_literal; CONST EditorMode:boolean=false);
    PROCEDURE resetBeforeTableUpdate;

    FUNCTION getDefiningLiteral(VAR adapters:T_adapters):P_listLiteral;
  end;

  P_sparseTable=^T_sparseTable;
  T_sparseTable=object
    definedCells:array of T_cell;
    colCount,RowCount:longint;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE initialize(CONST map:T_listLiteral; CONST location:T_tokenLocation; VAR adapters:T_adapters);

    FUNCTION getCellValue(CONST row,col:longint):P_literal;
    FUNCTION getCellValue(CONST index:T_cellIndex):P_literal;
    FUNCTION getCellValue(CONST cellId:ansistring):P_literal;
    FUNCTION getCellText(CONST row,col:longint):ansistring;
    PROCEDURE setValue(CONST row,col:longint; CONST value:ansistring);
    PROCEDURE setValue(CONST row,col:longint; CONST value:P_literal);
    FUNCTION isCellEditable(CONST row,col:longint):boolean;
    FUNCTION getDefiningLiteral(VAR adapters:T_adapters):P_listLiteral;
  end;

  { TtableForm }

  TtableForm = class(TForm)
    StringGrid: TStringGrid;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE StringGridValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
    PROCEDURE StringGridSelectEditor(Sender: TObject; aCol, aRow: integer; VAR editor: TWinControl);
    PROCEDURE StringGridButtonClick(Sender: TObject; aCol, aRow: integer);
    PROCEDURE StringGridSelectCell(Sender: TObject; aCol, aRow: integer; VAR CanSelect: boolean);
    PROCEDURE StringGridKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  private
    autoGrow:boolean;
    editMode:boolean;
    editModeResult_:ansistring;
    runModeResult_:P_listLiteral;
    cs:TRTLCriticalSection;
    table:T_sparseTable;
    PROCEDURE updateTable;
  public
    displayPending:boolean;
    PROCEDURE initForEditing;
    PROCEDURE initForRunning;
    PROPERTY editModeResult:ansistring read editModeResult_;
    FUNCTION runModeResult:P_listLiteral;
  end;

VAR tableForm: TtableForm;

IMPLEMENTATION

{$R *.lfm}

OPERATOR =(CONST x,y:T_cellIndex):boolean;
  begin
    result:=(x.row=y.row) and (x.col=y.col);
  end;

FUNCTION cellIdToCellIdx(CONST id:ansistring):T_cellIndex;
  VAR columnId:ansistring='';
      i:longint=1;
  begin
    while (i<=length(id)) and (id[i] in ['A'..'Z']) do begin
      columnId:=columnId+id[i];
      inc(i);
    end;
    result.row:=strToIntDef(copy(id,i,length(id)),0)-1;
    if      length(columnId)=1 then result.col:=                                 ord(columnId[1])-ord('A')
    else if length(columnId)=2 then result.col:=(ord(columnId[1])-ord('A')+1)*26+ord(columnId[2])-ord('A')
    else result.col:=-1;
    if (result.col<0) or (result.row<0) then begin
      result.row:=-1;
      result.col:=-1;
    end;
  end;

FUNCTION rowName(CONST row:longint):ansistring;
  begin
    if row<=25 then result:=                                   chr(ord('A')+row       )
               else result:=chr(ord('A')+row div 26-1) + chr(ord('A')+row mod 26);
  end;

FUNCTION cellIndexToId(CONST index:T_cellIndex):ansistring;
  begin
    if (index.col<0) or (index.row<0) or (index.row>701) then exit('');
    result:=rowName(index.row)+intToStr(index.col);
  end;

{ T_sparseTable }

CONSTRUCTOR T_sparseTable.create;
  begin
    colCount:=0;
    RowCount:=0;
  end;

DESTRUCTOR T_sparseTable.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(definedCells)-1 do definedCells[i].destroy;
    setLength(definedCells,0);
  end;

PROCEDURE T_sparseTable.initialize(CONST map: T_listLiteral; CONST location: T_tokenLocation; VAR adapters: T_adapters);
  PROCEDURE initCell(CONST key:ansistring; CONST value:P_literal);
    FUNCTION getValueForKey(CONST L:P_literal; CONST key:ansistring):P_literal;
      VAR i:longint;
          keyValuePair:P_listLiteral;
      begin
        if L^.literalType<>lt_keyValueList then exit(nil);
        for i:=0 to P_listLiteral(L)^.size-1 do begin
          keyValuePair:=P_listLiteral(P_listLiteral(L)^.value(i));
          if P_stringLiteral(keyValuePair^.value(0))^.value=key then exit(keyValuePair^.value(1));
        end;
        result:=nil;
      end;

    VAR index:T_cellIndex;
        i:longint;
        tempValue:P_literal;
        tempReadOnly:P_literal;
    begin
      index:=cellIdToCellIdx(key);
      if (index.col<0) or (index.row<0) then begin
        adapters.raiseError('Invalid cell specifier "'+key+'".',location);
        exit;
      end;
      for i:=0 to length(definedCells)-1 do if definedCells[i].index=index then begin
        adapters.raiseError('Duplicate cell specificaion for "'+key+'".',location);
        exit;
      end;
      if (value^.literalType=lt_keyValueList) then begin
        tempValue:=getValueForKey(value,C_VALUE_KEY);
        tempReadOnly:=getValueForKey(value,C_READ_ONLY_KEY);
      end else begin
        tempReadOnly:=nil;
        tempValue:=value;
      end;
      if value=nil then begin
        adapters.raiseError('Missing value for cell "'+key+'".',location);
        exit;
      end;
      if (tempReadOnly<>nil) and (tempReadOnly^.literalType<>lt_boolean) then begin
        adapters.raiseError('Flag "'+C_READ_ONLY_KEY+'" must be a boolean for cell "'+key+'".',location);
        exit;
      end;
      i:=length(definedCells);
      setLength(definedCells,i+1);
      definedCells[i].create(index,tempValue,(tempReadOnly<>nil) and P_boolLiteral(tempReadOnly)^.value);
    end;

  VAR i:longint;
  begin
    if length(definedCells)>0 then adapters.raiseError('Duplicate initialization of table',location);
    if map.literalType=lt_emptyList then exit;
    if map.literalType=lt_keyValueList then for i:=0 to map.size-1 do if adapters.noErrors then
      initCell(P_stringLiteral(P_listLiteral(map.value(i))^.value(0))^.value,
                               P_listLiteral(map.value(i))^.value(1));
  end;

FUNCTION T_sparseTable.getCellValue(CONST row, col: longint): P_literal;
  VAR index:T_cellIndex;
  begin
    index.row:=row;
    index.col:=col;
    result:=getCellValue(index);
  end;

FUNCTION T_sparseTable.getCellValue(CONST index: T_cellIndex): P_literal;
  VAR i:longint;
  begin
    for i:=0 to length(definedCells)-1 do
      if definedCells[i].index=index then exit(definedCells[i].getValue);
    result:=newVoidLiteral;
  end;

FUNCTION T_sparseTable.getCellValue(CONST cellId: ansistring): P_literal;
  VAR index:T_cellIndex;
  begin
    index:=cellIdToCellIdx(cellId);
    result:=getCellValue(index);
  end;

FUNCTION T_sparseTable.getCellText(CONST row, col: longint): ansistring;
  VAR index:T_cellIndex;
      i:longint;
      value:P_literal;
  begin
    index.row:=row;
    index.col:=col;
    for i:=0 to length(definedCells)-1 do if definedCells[i].index=index then begin
      value:=definedCells[i].getValue;
      if value^.literalType=lt_void then result:=''
                                    else result:=value^.toString;
      disposeLiteral(value);
      exit(result);
    end;
    result:='';
  end;

PROCEDURE T_sparseTable.setValue(CONST row, col: longint; CONST value: ansistring);
  VAR stringValue:P_stringLiteral;
      softCastValue:P_scalarLiteral;
  begin
    stringValue:=newStringLiteral(value);
    softCastValue:=stringValue^.softCast;
    disposeLiteral(stringValue);
    setValue(row,col,softCastValue);
  end;

PROCEDURE T_sparseTable.setValue(CONST row, col: longint; CONST value: P_literal);
  VAR index:T_cellIndex;
      i:longint;
  begin
    index.row:=row;
    index.col:=col;
    for i:=0 to length(definedCells)-1 do if definedCells[i].index=index then begin
      definedCells[i].setValue(value);
      exit;
    end;
    i:=length(definedCells);
    setLength(definedCells,i+1);
    definedCells[i].create(index,value,false);
    if row>=RowCount then RowCount:=row+1;
    if col>=colCount then colCount:=col+1;
  end;

FUNCTION T_sparseTable.isCellEditable(CONST row, col: longint): boolean;
  VAR index:T_cellIndex;
      i:longint;
  begin
    index.row:=row;
    index.col:=col;
    for i:=0 to length(definedCells)-1 do if definedCells[i].index=index then exit(definedCells[i].readonly);
    result:=true;
  end;

FUNCTION T_sparseTable.getDefiningLiteral(VAR adapters: T_adapters): P_listLiteral;
  VAR i:longint;
  begin
    result:=newListLiteral;
    for i:=0 to length(definedCells)-1 do result^.append(definedCells[i].getDefiningLiteral(adapters),false,adapters);
  end;

{ T_cell }

CONSTRUCTOR T_cell.create(CONST cellIndex: T_cellIndex; CONST initialValue: P_literal; CONST isReadOnly: boolean);
  begin
    index:=cellIndex;
    givenValue:=initialValue;
    givenValue^.rereference;
    readonly:=isReadOnly;
    if givenValue^.literalType=lt_expression then begin
      calculatedValue:=newVoidLiteral;
      calculated:=true;
      ready:=false;
      readonly:=true;
    end else begin
      calculatedValue:=givenValue;
      calculatedValue^.rereference;
      calculated:=false;
      ready:=true;
    end;
  end;

DESTRUCTOR T_cell.destroy;
  begin
    disposeLiteral(calculatedValue);
    disposeLiteral(givenValue);
  end;

FUNCTION T_cell.getValue: P_literal;
  begin
    result:=calculatedValue;
    result^.rereference;
  end;

PROCEDURE T_cell.setValue(CONST L: P_literal; CONST EditorMode: boolean);
  begin
    if EditorMode then begin
      disposeLiteral(givenValue);
      givenValue:=L;
      L^.rereference;
      disposeLiteral(calculatedValue);
      if givenValue^.literalType=lt_expression then begin
        calculatedValue:=newVoidLiteral;
        calculated:=true;
        ready:=false;
        readonly:=true;
      end else begin
        calculatedValue:=givenValue;
        calculatedValue^.rereference;
        calculated:=false;
        ready:=true;
      end;
    end else begin
      disposeLiteral(calculatedValue);
      calculatedValue:=L;
    end;
  end;

PROCEDURE T_cell.resetBeforeTableUpdate;
  begin
    ready:=not(calculated);
  end;

FUNCTION T_cell.getDefiningLiteral(VAR adapters:T_adapters): P_listLiteral;
  begin
    if readonly and not(calculated) then begin
      result:=
        newListLiteral^ //Specifying Key-Value-Pair (Cell= CellName + CellContent)
        .appendString(cellIndexToId(index))^
        .append(newListLiteral^ //(CellContent = [ValuePair,ReadOnlyPair])
                .append(newListLiteral^ //ValuePair = (ValueKey + value)
                        .appendString(C_VALUE_KEY)^
                        .append(givenValue,true,adapters),false,adapters)^
                .append(newListLiteral^ //ReadOnlyPair = (ReadOnlyKey + flag)
                        .appendString(C_READ_ONLY_KEY)^
                        .appendBool(true),false,adapters),false,adapters);
    end else begin
      result:=newListLiteral^
              .appendString(cellIndexToId(index))^
              .append(givenValue,true,adapters);
    end;
  end;

FUNCTION displayTable_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR tab:P_sparseTable;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_emptyList,lt_keyValueList]) then begin
      new(tab,create);
      tab^.initialize(P_listLiteral(params^.value(0))^,tokenLocation,context.adapters^);

      //!! RUN TABLE!
      result:=tab^.getDefiningLiteral(context.adapters^);
      dispose(tab,destroy);
    end;
  end;

PROCEDURE TtableForm.FormCreate(Sender: TObject);
  begin
    initCriticalSection(cs);
    displayPending:=false;
  end;

PROCEDURE TtableForm.FormDestroy(Sender: TObject);
  begin
    doneCriticalSection(cs);
  end;

PROCEDURE TtableForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    enterCriticalSection(cs);
    runModeResult_:=table.getDefiningLiteral(nullAdapter);
    if editMode then begin
      editModeResult_:=runModeResult_^.toString;
      disposeLiteral(runModeResult_);
      runModeResult_:=nil;
    end;
    table.destroy;
    leaveCriticalSection(cs);
  end;

PROCEDURE TtableForm.StringGridValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  begin
    if newValue=oldValue then exit;
    updateTable;
  end;

PROCEDURE TtableForm.StringGridSelectEditor(Sender: TObject; aCol, aRow: integer; VAR editor: TWinControl);
  begin
  end;

PROCEDURE TtableForm.StringGridButtonClick(Sender: TObject; aCol, aRow: integer);
begin

end;

PROCEDURE TtableForm.StringGridSelectCell(Sender: TObject; aCol, aRow: integer; VAR CanSelect: boolean);
begin
end;

PROCEDURE TtableForm.StringGridKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
begin
  if autoGrow and (key=39) and (StringGrid.col=StringGrid.colCount-1) then begin
    StringGrid.colCount:=StringGrid.colCount+1;
    updateTable;
  end;
  if autoGrow and (key=40) and (StringGrid.row=StringGrid.RowCount-1) then begin
    StringGrid.RowCount:=StringGrid.RowCount+1;
    updateTable;
  end;
end;

PROCEDURE TtableForm.updateTable;
  VAR i:longint;
      allEmpty:boolean;
      offset:longint=0;
  begin
    if editMode then begin
      for i:=1 to StringGrid.colCount-1 do StringGrid.Cells[i,0]:=rowName(i-1);
      for i:=1 to StringGrid.RowCount-1 do StringGrid.Cells[0,i]:=intToStr(i);
      offset:=1;
    end;


    StringGrid.AutoSizeColumns;
    while StringGrid.RowCount>StringGrid.row+2 do begin
      allEmpty:=true;
      for i:=offset to StringGrid.colCount-1 do allEmpty:=allEmpty and (trim(StringGrid.Cells[i,StringGrid.RowCount-1])='');
      if allEmpty then StringGrid.RowCount:=StringGrid.RowCount-1
                  else break;
    end;
    while StringGrid.colCount>StringGrid.col+2 do begin
      allEmpty:=true;
      for i:=offset to StringGrid.RowCount-1 do allEmpty:=allEmpty and (trim(StringGrid.Cells[StringGrid.colCount-1,i])='');
      if allEmpty then StringGrid.colCount:=StringGrid.colCount-1
                  else break;
    end;

  end;

PROCEDURE TtableForm.initForEditing;
  begin
    enterCriticalSection(cs);
    editMode:=true;
    table.create;
    StringGrid.clear;
    StringGrid.RowCount:=3;
    StringGrid.colCount:=3;
    StringGrid.FixedCols:=1;
    StringGrid.FixedRows:=1;
    StringGrid.GridLineWidth:=1;
    autoGrow:=true;
    updateTable;
    ShowModal;
    leaveCriticalSection(cs);
  end;

PROCEDURE TtableForm.initForRunning;
  begin
    enterCriticalSection(cs);
    editMode:=false;
    table.create;
    StringGrid.clear;
    StringGrid.FixedCols:=0;
    StringGrid.FixedRows:=0;
    StringGrid.GridLineWidth:=0;
    leaveCriticalSection(cs);
  end;

FUNCTION TtableForm.runModeResult: P_listLiteral;
  begin
    while displayPending or showing do sleep(1);
    result:=runModeResult_;
  end;


end.

