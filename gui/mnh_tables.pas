UNIT mnh_tables;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids, Menus,
  mnhFormHandler,
  myGenerics,myStringUtil,
  mnh_constants,basicTypes,
  mnh_messages,
  litVar, funcs,
  contexts,
  out_adapters,
  recyclers,
  fileWrappers,
  profiling;

TYPE
  TtableForm = class(TForm)
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mi_exportIncHeader: TMenuItem;
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
    PROCEDURE stringGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
    PROCEDURE stringGridKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  private
    { private declarations }
    literal:P_listLiteral;
    headerData:T_arrayOfString;
    firstIsHeader:boolean;

    displayPending:boolean;
    sorted:record
      ascending:boolean;
      byColumn:longint;
    end;
  public
    { public declarations }
    PROCEDURE initWithLiteral(CONST L:P_listLiteral; CONST newCaption:string; CONST firstIsHeader_:boolean);
    PROCEDURE conditionalDoShow;
    PROCEDURE fillTable;
  end;

PROCEDURE resetTableForms;
PROCEDURE conditionalShowTables;
IMPLEMENTATION
VAR tableForms: array of TtableForm;
    tableFormCs:TRTLCriticalSection;

PROCEDURE resetTableForms;
  VAR i:longint;
  begin
    enterCriticalSection(tableFormCs);
    for i:=0 to length(tableForms)-1 do begin
      unregisterForm(tableForms[i]);
      FreeAndNil(tableForms[i]);
    end;
    setLength(tableForms,0);
    leaveCriticalSection(tableFormCs);
  end;

FUNCTION newTableForm:TtableForm;
  begin
    enterCriticalSection(tableFormCs);
    result:=TtableForm.create(nil);
    setLength(tableForms,length(tableForms)+1);
    tableForms[length(tableForms)-1]:=result;
    registerForm(result,ft_table);
    leaveCriticalSection(tableFormCs);
  end;

{$R *.lfm}

PROCEDURE conditionalShowTables;
  VAR i:longint;
  begin
    enterCriticalSection(tableFormCs);
    for i:=0 to length(tableForms)-1 do tableForms[i].conditionalDoShow;
    leaveCriticalSection(tableFormCs);
  end;

FUNCTION showTable_impl(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler): P_literal;
  VAR caption:string='MNH table';
      header:boolean=false;
      i:longint;
  begin
    if not(context.checkSideEffects('showTable',tokenLocation,[se_output])) then exit(nil);
    if not(gui_started) then begin
      context.messages^.logGuiNeeded;
      exit(nil);
    end;
    if (params<>nil) and
       (params^.size>0) and
       (params^.value[0]^.literalType in C_listTypes) then begin
      for i:=1 to 2 do if params^.size>i then begin
        case params^.value[i]^.literalType of
          lt_string : caption:=P_stringLiteral(params^.value[i])^.value;
          lt_boolean: header :=P_boolLiteral  (params^.value[i])^.value;
          else exit(nil);
        end;
      end;
      enterCriticalSection(tableFormCs);
      newTableForm.initWithLiteral(P_listLiteral(params^.value[0]),caption,header);
      context.messages^.postSingal(mt_displayTable,C_nilTokenLocation);
      leaveCriticalSection(tableFormCs);
      if gui_started then result:=newVoidLiteral else result:=nil;
    end else result:=nil;
  end;

PROCEDURE showProfilingTable(CONST data: P_listLiteral);
  begin
    enterCriticalSection(tableFormCs);
    newTableForm.initWithLiteral(data,'Profiling info',true);
    leaveCriticalSection(tableFormCs);
  end;

PROCEDURE TtableForm.FormCreate(Sender: TObject);
  begin
    literal:=nil;
    if not(anyFormShowing(ft_main)) then ShowInTaskBar:=stAlways;
  end;

PROCEDURE TtableForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
  end;

PROCEDURE TtableForm.FormDestroy(Sender: TObject);
  begin
    if literal<>nil then disposeLiteral(literal);
  end;

PROCEDURE TtableForm.FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=9) and (ssCtrl in Shift) then formCycle(self,ssShift in Shift);
    if (key=65) and (ssCtrl in Shift) then StringGrid.selection:=rect(0,1,StringGrid.colCount-1,StringGrid.RowCount-1);
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
    mi_comma.checked:=not(mi_comma.checked);
    fillTable;
  end;

PROCEDURE TtableForm.mi_exportCsvSemicolonClick(Sender: TObject);
  begin
    if SaveTableDialog.execute then StringGrid.SaveToCSVFile(SaveTableDialog.fileName,';',mi_exportIncHeader.checked,true);
  end;

PROCEDURE TtableForm.mi_exportCsvTabClick(Sender: TObject);
  begin
    if SaveTableDialog.execute then StringGrid.SaveToCSVFile(SaveTableDialog.fileName,C_tabChar,mi_exportIncHeader.checked,true);
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
  begin
    mi_transpose.checked:=not(mi_transpose.checked);
    newLiteral:=literal^.transpose(@emptyStringSingleton);
    disposeLiteral(literal);
    literal:=newLiteral;
    fillTable;
  end;

PROCEDURE TtableForm.stringGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
  VAR dummyLocation:T_tokenLocation;
      newLiteral:P_listLiteral;
      i:longint;
  begin
    dummyLocation.package:=nil;
    dummyLocation.column:=0;
    dummyLocation.line:=0;
    if not(IsColumn) or (firstIsHeader and mi_transpose.checked) then exit;
    with sorted do if byColumn=index then begin
      byColumn:=index;
      ascending:=not(ascending);

      newLiteral:=newListLiteral(literal^.size);
      for i:=literal^.size-1 downto 0 do
      newLiteral^.append(literal^.value[i],true);
      disposeLiteral(literal);
      literal:=newLiteral;
    end else begin
      byColumn:=index;
      ascending:=true;

      literal^.sortBySubIndex(index,dummyLocation,nil);
    end;
    fillTable;
  end;

PROCEDURE TtableForm.stringGridKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    FormKeyUp(Sender,key,Shift);
  end;

PROCEDURE TtableForm.initWithLiteral(CONST L: P_listLiteral; CONST newCaption: string; CONST firstIsHeader_:boolean);
  VAR i:longint;
      headerLiteral:P_listLiteral;
  begin
    with sorted do begin
      ascending:=false;
      byColumn:=-1;
    end;
    firstIsHeader:=firstIsHeader_;
    mi_exportIncHeader.enabled:=firstIsHeader_;
    mi_exportIncHeader.checked:=mi_exportIncHeader.checked and firstIsHeader_;
    if firstIsHeader and (L^.size>0) and (L^.value[0]^.literalType in C_listTypes) then begin
      headerLiteral:=P_listLiteral(L^.value[0]);
      setLength(headerData,headerLiteral^.size);
      for i:=0 to headerLiteral^.size-1 do case headerLiteral^.value[i]^.literalType of
        lt_string: headerData[i]:=P_stringLiteral(headerLiteral^.value[i])^.value
        else       headerData[i]:=                headerLiteral^.value[i]^.toString;
      end;
      literal:=P_listLiteral(L)^.tail;
    end else begin
      setLength(headerData,0);
      literal:=P_listLiteral(L^.rereferenced);
    end;

    displayPending:=true;
    caption:=newCaption;
  end;

PROCEDURE TtableForm.conditionalDoShow;
  begin
    if displayPending then begin
      displayPending:=false;
      Show;
      fillTable;
    end;
  end;

PROCEDURE TtableForm.fillTable;
  VAR dataRows:longint;
      dataColumns:longint=0;
      cellContents:array of T_arrayOfString;
      i,j:longint;
      rowLit:P_literal;
      cellLit:P_literal;
      iter:T_arrayOfLiteral;

  FUNCTION getHeaderCell(CONST i:longint):string;
    begin
      if (firstIsHeader) and (i>=0) and (i<length(headerData)) then result:=headerData[i] else result:='';
      if not(mi_transpose.checked) and (sorted.byColumn=i) then begin
        if sorted.ascending then result:=result+' v'
                            else result:=result+' ^';
      end;
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
              lt_real,lt_realList,lt_numList:if mi_comma.checked then cellContents[i,j]:=replaceAll(cellLit^.toString,'.',',')
                                                                 else cellContents[i,j]:=           cellLit^.toString;
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
            lt_real,lt_realList,lt_numList:if mi_comma.checked then cellContents[i,j]:=replaceAll(cellLit^.toString,'.',',')
                                                               else cellContents[i,j]:=           cellLit^.toString;
            lt_void: cellContents[i,j]:='';
            else cellContents[i,j]:=cellLit^.toString;
          end;
        end;
        if length(cellContents[i])>dataColumns then dataColumns:=length(cellContents[i]);
      end;
    end;
    StringGrid.clear;

    if firstIsHeader and mi_transpose.checked then begin
      StringGrid.RowCount:=dataRows+1;
      StringGrid.colCount:=dataColumns+1;
      StringGrid.FixedCols:=1;
      StringGrid.FixedRows:=1;
      for i:=0 to length(cellContents)-1 do
      for j:=0 to length(cellContents[i])-1 do
      StringGrid.Cells[j+1,i+1]:=cellContents[i,j];
      for i:=1 to StringGrid.RowCount-1 do StringGrid.Cells[0,i]:=getHeaderCell(i-1);
    end else begin
      StringGrid.RowCount:=dataRows+1;
      StringGrid.colCount:=dataColumns;
      StringGrid.FixedCols:=0;
      StringGrid.FixedRows:=1;
      for i:=0 to length(cellContents)-1 do
      for j:=0 to length(cellContents[i])-1 do
      StringGrid.Cells[j,i+1]:=cellContents[i,j];
      for i:=0 to StringGrid.colCount-1 do StringGrid.Cells[i,0]:=getHeaderCell(i);
    end;
    StringGrid.AutoSizeColumns;
  end;

INITIALIZATION
  registerRule(GUI_NAMESPACE,'showTable',@showTable_impl,ak_variadic_1,'showTable(L:list);//Shows L in a table.#showTable(L:list,caption:string);//Shows L in a table with given caption.#showTable(L:list,caption:string,firstRowIsHeader:boolean);//Shows L in a table with given caption.');
  setLength(tableForms,0);
  initialize(tableFormCs);
  initCriticalSection(tableFormCs);
FINALIZATION
  resetTableForms;
  doneCriticalSection(tableFormCs);

end.

