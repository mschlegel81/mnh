UNIT mnh_tables;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids, Menus,
  myGenerics,
  mnh_constants,basicTypes,
  mnh_messages,
  litVar, funcs,
  contexts,
  out_adapters,
  recyclers,
  fileWrappers,
  mnh_settings,
  ideLayoutUtil;

TYPE
  P_tableAdapter=^T_tableAdapter;
  TtableForm = class(T_mnhComponentForm)
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miDockInMain: TMenuItem;
    mi_exportIncHeader: TMenuItem;
    miIncreaseFontSize: TMenuItem;
    miDecreaseFontSize: TMenuItem;
    mi_exportText: TMenuItem;
    mi_exportCsvTab: TMenuItem;
    mi_exportCsvSemicolon: TMenuItem;
    mi_transpose: TMenuItem;
    mi_comma: TMenuItem;
    PopupMenu1: TPopupMenu;
    SaveTableDialog: TSaveDialog;
    tableMenu: TMainMenu;
    StringGrid: TStringGrid;
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE miDecreaseFontSizeClick(Sender: TObject);
    PROCEDURE miIncreaseFontSizeClick(Sender: TObject);
    PROCEDURE mi_commaClick(Sender: TObject);
    PROCEDURE mi_exportCsvSemicolonClick(Sender: TObject);
    PROCEDURE mi_exportCsvTabClick(Sender: TObject);
    PROCEDURE mi_exportTextClick(Sender: TObject);
    PROCEDURE mi_transposeClick(Sender: TObject);
    PROCEDURE stringGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);

    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate(CONST isEvaluationRunning:boolean); override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE dockChanged; override;
    FUNCTION getDefaultControl:TWinControl; override;
    PROCEDURE stringGridKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE exportToCsv(Separator:char);
  private
    { private declarations }
    literal:P_listLiteral;
    FixedRows,fixedColumns:longint;
    adapter:P_tableAdapter;
    sorted:record
      ascending:boolean;
      byColumn:longint;
    end;
  public
    { public declarations }
    PROCEDURE initWithLiteral(CONST L:P_listLiteral; CONST newCaption:string; CONST fixedRows_,fixedColumns_:longint; CONST adapter_:P_tableAdapter);
    PROCEDURE fillTable(CONST firstFill:boolean);
  end;

  P_tableDisplayRequest=^T_tableDisplayRequest;
  T_tableDisplayRequest=object(T_payloadMessage)
    private
      tableContent:P_listLiteral;
      tableCaption:string;
      FixedRows,fixedColumns:longint;
    protected
      FUNCTION internalType:shortstring; virtual;
    public
      CONSTRUCTOR create(CONST L:P_listLiteral; CONST newCaption:string; CONST fixedRows_,fixedColumns_:longint);
      DESTRUCTOR destroy; virtual;
  end;

  T_tableAdapterTemplate=specialize G_multiChildGuiOutAdapter<TtableForm>;
  T_tableAdapter=object(T_tableAdapterTemplate)
    defaultCaption:string;
    CONSTRUCTOR create(CONST defaultCaption_:string);
    FUNCTION flushToGui(CONST forceFlush:boolean):T_messageTypeSet; virtual;
  end;

IMPLEMENTATION
USES myStringUtil,strutils,math,LCLType;
{$R *.lfm}

FUNCTION showTable_impl(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler): P_literal;
  VAR caption:string='';
      fixedRowsRead:boolean=false;
      FixedCols:longint=0;
      FixedRows:longint=0;
      i:longint;
      tableDisplayRequest:P_tableDisplayRequest;
  begin
    if not(context.checkSideEffects('showTable',tokenLocation,[se_output,se_input])) then exit(nil);
    if (gui_started=NO) then begin
      context.messages^.logGuiNeeded;
      exit(nil);
    end;
    if (params<>nil) and
       (params^.size>0) and
       (params^.value[0]^.literalType in C_listTypes) then begin
      for i:=1 to 3 do if params^.size>i then begin
        case params^.value[i]^.literalType of
          //Additional string parameter indicates caption
          lt_string : caption:=P_stringLiteral(params^.value[i])^.value;
          //Additional boolean parameter indicates boolean flag for header
          lt_boolean: begin
            if P_boolLiteral  (params^.value[i])^.value then FixedRows:=1 else FixedRows:=0;
            fixedRowsRead:=true;
          end;
          //Additional int parameter indicates fixed rows or fixed columns
          lt_smallint: begin
            if fixedRowsRead
            then FixedCols:=P_smallIntLiteral(params^.value[i])^.value
            else begin
              FixedRows:=P_smallIntLiteral(params^.value[i])^.value;
              fixedRowsRead:=true;
            end;
          end;
          else exit(nil);
        end;
      end;
      if (gui_started<>NO) then begin
        new(tableDisplayRequest,create(P_listLiteral(params^.value[0]),caption,FixedRows,FixedCols));
        context.messages^.postCustomMessage(tableDisplayRequest,true);
        result:=newVoidLiteral;
      end else result:=nil;
    end else result:=nil;
  end;

CONSTRUCTOR T_tableAdapter.create(CONST defaultCaption_: string);
  begin
    inherited create(at_table,[mt_startOfEvaluation,mt_displayTable]);
    defaultCaption:=defaultCaption_;
  end;

FUNCTION T_tableAdapter.flushToGui(CONST forceFlush:boolean): T_messageTypeSet;
  VAR i:longint;
      tab:TtableForm;
      caption:string;
  begin
    result:=[];
    enterCriticalSection(adapterCs);
    try
      for i:=0 to collectedFill-1 do case collected[i]^.messageType of
        mt_displayTable:
          begin
            include(result,collected[i]^.messageType);
            tab:=addChild(TtableForm.create(nil));
            with P_tableDisplayRequest(collected[i])^ do begin
              if tableCaption=''
              then caption:=defaultCaption+' ('+intToStr(length(children))+')'
              else caption:=tableCaption;
              tab.initWithLiteral(tableContent,caption,FixedRows,fixedColumns,@self);
            end;
            dockNewForm(tab);
            tab.fillTable(true);
            tab.showComponent(true);
          end;
        mt_startOfEvaluation:
          begin
            include(result,collected[i]^.messageType);
            destroyAllChildren;
          end;
      end;
      clear;
    finally
      leaveCriticalSection(adapterCs);
    end;
  end;

FUNCTION T_tableDisplayRequest.internalType: shortstring;
  begin
    result:='T_tableDisplayRequest';
  end;

CONSTRUCTOR T_tableDisplayRequest.create(CONST L: P_listLiteral; CONST newCaption: string; CONST fixedRows_,fixedColumns_:longint);
  begin
    inherited create(mt_displayTable);
    tableContent:=P_listLiteral(L^.rereferenced);
    tableCaption:=newCaption;
    FixedRows   :=fixedRows_;
    fixedColumns:=fixedColumns_;
  end;

DESTRUCTOR T_tableDisplayRequest.destroy;
  begin
    disposeLiteral(tableContent);
    inherited destroy;
  end;

PROCEDURE TtableForm.FormCreate(Sender: TObject);
  begin
    literal:=nil;
    registerFontControl(StringGrid,ctTable);
    initDockMenuItems(tableMenu,miDockInMain);
    initDockMenuItems(PopupMenu1,nil);
  end;

PROCEDURE TtableForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    CloseAction:=caFree;
  end;

PROCEDURE TtableForm.FormDestroy(Sender: TObject);
  begin
    if literal<>nil then disposeLiteral(literal);
    unregisterFontControl(StringGrid);
    adapter^.childDestroyed(self);
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
    setFontSize(ctTable,getFontSize(ctTable)-1);
    StringGrid.AutoSizeColumns;
  end;

PROCEDURE TtableForm.miIncreaseFontSizeClick(Sender: TObject);
  begin
    setFontSize(ctTable,getFontSize(ctTable)+1);
    StringGrid.AutoSizeColumns;
  end;

PROCEDURE TtableForm.mi_commaClick(Sender: TObject);
  begin
    mi_comma.checked:=not(mi_comma.checked);
    fillTable(false);
  end;

PROCEDURE TtableForm.exportToCsv(Separator: char);
  VAR fixedColsBefore:longint;
      fixedRowsBefore:longint;
      message:ansistring;
  begin
    if SaveTableDialog.execute then begin
      fixedColsBefore:=StringGrid.FixedCols;
      fixedRowsBefore:=StringGrid.FixedRows;
      if mi_exportIncHeader.checked
      then begin
        StringGrid.FixedRows:=0;
        StringGrid.FixedCols:=0;
      end;
      try
        StringGrid.SaveToCSVFile(SaveTableDialog.fileName,';',false,true);
      except
        beep;
        message:='Could not access file '+SaveTableDialog.fileName;
        Application.MessageBox('Export to csv failed',PChar(message),MB_ICONERROR+MB_OK);
      end;
      StringGrid.FixedCols:=fixedColsBefore;
      StringGrid.FixedRows:=fixedRowsBefore;
    end;
  end;

PROCEDURE TtableForm.mi_exportCsvSemicolonClick(Sender: TObject);
  begin
    exportToCsv(';');
  end;

PROCEDURE TtableForm.mi_exportCsvTabClick(Sender: TObject);
  begin
    exportToCsv(C_tabChar);
  end;

PROCEDURE TtableForm.mi_exportTextClick(Sender: TObject);
  VAR i0:longint=1;
      i,j:longint;
      content:T_arrayOfString;
      row:ansistring;
  begin
    if SaveTableDialog.execute then begin
      if mi_exportIncHeader.checked then i0:=0;
      setLength(content,StringGrid.RowCount-i0);
      for i:=i0 to StringGrid.RowCount-1 do begin
        row:='';
        for j:=0 to StringGrid.colCount-1 do row:=row+StringGrid.Cells[j,i]+C_tabChar;
        content[i-i0]:=row;
      end;
      content:=formatTabs(content);
      writeFileLines(SaveTableDialog.fileName,formatTabs(content),LineEnding,false);
    end;
  end;

PROCEDURE TtableForm.mi_transposeClick(Sender: TObject);
  VAR newLiteral:P_listLiteral;
      tmp:longint;
  begin
    mi_transpose.checked:=not(mi_transpose.checked);
    newLiteral:=literal^.transpose(@emptyStringSingleton);
    disposeLiteral(literal);
    literal:=newLiteral;
    tmp         :=fixedColumns;
    fixedColumns:=FixedRows;
    FixedRows   :=tmp;
    fillTable(true);
  end;

PROCEDURE TtableForm.stringGridHeaderClick(Sender: TObject; IsColumn: boolean;
  index: integer);
  VAR dummyLocation:T_tokenLocation;
      newLiteral:P_listLiteral;
      headerRows:P_listLiteral;
      i:longint;
  begin
    dummyLocation.package:=nil;
    dummyLocation.column:=0;
    dummyLocation.line:=0;
    if not(IsColumn) then exit;
    with sorted do if byColumn=index then begin
      byColumn:=index;
      ascending:=not(ascending);
      //Initialize the literal with fixed rows
      newLiteral:=newListLiteral(literal^.size);
      for i:=0 to FixedRows-1 do newLiteral^.append(literal^.value[i],true);
      //Append the remaining rows in reversed order
      for i:=literal^.size-1 downto FixedRows do
      newLiteral^.append(literal^.value[i],true);
      disposeLiteral(literal);
      literal:=newLiteral;
    end else begin
      byColumn:=index;
      ascending:=true;
      headerRows:=literal^.head(FixedRows);
      newLiteral:=literal^.tail(FixedRows);
      disposeLiteral(literal);

      newLiteral^.sortBySubIndex(index,dummyLocation,nil);
      headerRows^.appendAll(newLiteral);
      disposeLiteral(newLiteral);

      literal:=headerRows;
    end;
    fillTable(false);
  end;

FUNCTION TtableForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icTable;
  end;

PROCEDURE TtableForm.performSlowUpdate(CONST isEvaluationRunning: boolean);
  begin
  end;

PROCEDURE TtableForm.performFastUpdate;
  begin
  end;

PROCEDURE TtableForm.dockChanged;
  begin
    if (myComponentParent=cpNone)
    then moveAllItems(PopupMenu1.items,tableMenu.items)
    else moveAllItems(tableMenu.items,PopupMenu1.items);
  end;

FUNCTION TtableForm.getDefaultControl: TWinControl;
  begin
    result:=StringGrid;
  end;

PROCEDURE TtableForm.stringGridKeyUp(Sender: TObject; VAR key: word;
  Shift: TShiftState);
  begin
    tabNextKeyHandling(Sender,key,Shift);
  end;

PROCEDURE TtableForm.initWithLiteral(CONST L: P_listLiteral;
  CONST newCaption: string; CONST fixedRows_, fixedColumns_: longint;
  CONST adapter_: P_tableAdapter);
  begin
    adapter:=adapter_;
    with sorted do begin
      ascending:=false;
      byColumn:=-1;
    end;
    FixedRows:=fixedRows_;
    fixedColumns:=fixedColumns_;
    mi_exportIncHeader.enabled:=FixedRows>0;
    mi_exportIncHeader.checked:=mi_exportIncHeader.checked and (FixedRows>0);
    literal:=P_listLiteral(L^.rereferenced);
    caption:=newCaption;
  end;

PROCEDURE TtableForm.fillTable(CONST firstFill:boolean);
  VAR dataRows:longint;
      dataColumns:longint=0;
      cellContents:array of T_arrayOfString;
      i,j:longint;
      rowLit:P_literal;
      cellLit:P_literal;
      iter:T_arrayOfLiteral;
      additionalHeaderRow:longint=0;
  FUNCTION sortMarker(CONST input:string; CONST i:longint):string;
    CONST arrow:array[false..true] of string=(#226#150#188,#226#150#178);
    begin
      if sorted.byColumn=i then begin
        if input='' then result:=arrow[sorted.ascending]
                    else result:=arrow[sorted.ascending]+' '+input+' '+arrow[sorted.ascending];
      end else result:=input;
    end;

  FUNCTION excelStyleColumnIndex(i:longint):string;
    begin
      if i<0 then exit('');
      if i<26 then exit(chr(ord('A')+i));
      i-=26;
      result:=chr(ord('A')+(i div 26))+chr(ord('A')+(i mod 26));
    end;

  begin
    dataRows:=literal^.size;
    setLength(cellContents,dataRows);
    if literal^.literalType=lt_stringList then begin
      for i:=0 to literal^.size-1 do begin
        cellContents[i]:=split(P_stringLiteral(literal^.value[i])^.value,C_tabChar);
        if length(cellContents[i])>dataColumns then dataColumns:=length(cellContents[i]);
      end;
    end else begin
      for i:=0 to literal^.size-1 do begin
        rowLit:=literal^.value[i];
        if rowLit^.literalType in C_compoundTypes then begin
          iter:=P_compoundLiteral(rowLit)^.iteratableList;
          setLength(cellContents[i],length(iter));
          for j:=0 to length(iter)-1 do begin
            cellLit:=iter[j];
            case cellLit^.literalType of
              lt_string:cellContents[i,j]:=P_stringLiteral(cellLit)^.value;
              lt_real,lt_realList,lt_numList:if mi_comma.checked then cellContents[i,j]:=ansiReplaceStr(cellLit^.toString,'.',',')
                                                                 else cellContents[i,j]:=               cellLit^.toString;
              lt_void: cellContents[i,j]:='';
              else cellContents[i,j]:=cellLit^.toString;
            end;
          end;
          disposeLiteral(iter);
        end else begin
          setLength(cellContents[i],1);
          cellLit:=rowLit; j:=0;
          case cellLit^.literalType of
            lt_string:cellContents[i,j]:=P_stringLiteral(cellLit)^.value;
            lt_real,lt_realList,lt_numList:if mi_comma.checked then cellContents[i,j]:=ansiReplaceStr(cellLit^.toString,'.',',')
                                                               else cellContents[i,j]:=               cellLit^.toString;
            lt_void: cellContents[i,j]:='';
            else cellContents[i,j]:=cellLit^.toString;
          end;
        end;
        if length(cellContents[i])>dataColumns then dataColumns:=length(cellContents[i]);
      end;
    end;

    if FixedRows=0 then additionalHeaderRow:=1;
    if firstFill then begin
      StringGrid.clear;
      if (dataRows=0) or (dataColumns=0) then exit;
      StringGrid.RowCount:=dataRows   +additionalHeaderRow;
      StringGrid.colCount:=dataColumns;
      StringGrid.FixedRows:=min(dataRows   ,FixedRows)+additionalHeaderRow;
      StringGrid.FixedCols:=min(dataColumns,fixedColumns);
    end;
    if (dataRows=0) or (dataColumns=0) then exit;
    for i:=0 to length(cellContents)-1 do begin
      for j:=0 to length(cellContents[i])-1                   do StringGrid.Cells[j,i+additionalHeaderRow]:=cellContents[i,j];
      for j:=length(cellContents[i]) to StringGrid.colCount-1 do StringGrid.Cells[j,i+additionalHeaderRow]:='';
    end;
    if additionalHeaderRow=1 then for j:=0 to dataColumns-1 do StringGrid.Cells[j,0]:=excelStyleColumnIndex(j-fixedColumns);
    for j:=0 to StringGrid.colCount-1 do StringGrid.Cells[j,0]:=sortMarker(StringGrid.Cells[j,0],j);
    if firstFill then StringGrid.AutoSizeColumns;
  end;

INITIALIZATION
  registerRule(GUI_NAMESPACE,'showTable',@showTable_impl,ak_variadic_1,'showTable(L:list);//Shows L in a table.#'+
    'showTable(L:list,caption:string);//Shows L in a table with given caption.#'+
    'showTable(L:list,caption:string,firstRowIsHeader:boolean);//Shows L in a table with given caption.#'+
    'showTable(L:list,caption:string,fixedRows:Int,fixedColumns:Int);//Shows L with customized fixed rows and columns',sfr_needs_gui);

end.

