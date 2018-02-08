UNIT openDemoDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  sysutils,
  Classes,LazFileUtils,
  Forms, Controls, Grids,
  myGenerics, myStringUtil,
  mnh_constants,mnh_fileWrappers,mnh_doc,mnh_packages,mnh_subrules;

TYPE
  TopenDemoDialogForm = class(TForm)
    demosGrid: TStringGrid;
    PROCEDURE demosGridDblClick(Sender: TObject);
    PROCEDURE demosGridHeaderClick(Sender: TObject; IsColumn: boolean;
      index: integer);
    PROCEDURE demosGridKeyPress(Sender: TObject; VAR key: char);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE paintGrid;
  private
    groupByFile:boolean;
    reverseOrder:boolean;
    dataByDemo,dataByTag:array of array[0..1] of string;
  public
    selectedFile:string;
  end;

FUNCTION openDemoDialogForm: TopenDemoDialogForm;
IMPLEMENTATION
VAR myOpenDemoDialogForm: TopenDemoDialogForm=nil;
FUNCTION openDemoDialogForm: TopenDemoDialogForm;
  begin
    if myOpenDemoDialogForm=nil then myOpenDemoDialogForm:=TopenDemoDialogForm.create(nil);
    result:=myOpenDemoDialogForm;
  end;

{$R *.lfm}

FUNCTION demosFolder:string;
  begin
    result:=configDir+'demos'+DirectorySeparator;
  end;

PROCEDURE TopenDemoDialogForm.FormShow(Sender: TObject);
  begin
    paintGrid;
  end;

PROCEDURE TopenDemoDialogForm.paintGrid;
  VAR i:longint;
  begin
    if groupByFile then begin
      demosGrid.RowCount:=length(dataByDemo)+1;
      if reverseOrder then for i:=0 to length(dataByDemo)-1 do begin
        demosGrid.Cells[0,i+1]:=dataByDemo[length(dataByDemo)-1-i,0];
        demosGrid.Cells[1,i+1]:=dataByDemo[length(dataByDemo)-1-i,1];
      end else for i:=0 to length(dataByDemo)-1 do begin
        demosGrid.Cells[0,i+1]:=dataByDemo[i,0];
        demosGrid.Cells[1,i+1]:=dataByDemo[i,1];
      end;
    end else begin
      demosGrid.RowCount:=length(dataByTag)+1;
      if reverseOrder then for i:=0 to length(dataByTag)-1 do begin
        demosGrid.Cells[0,i+1]:=dataByTag[length(dataByTag)-1-i,0];
        demosGrid.Cells[1,i+1]:=dataByTag[length(dataByTag)-1-i,1];
      end else for i:=0 to length(dataByTag)-1 do begin
        demosGrid.Cells[0,i+1]:=dataByTag[i,0];
        demosGrid.Cells[1,i+1]:=dataByTag[i,1];
      end;
    end;
  end;

PROCEDURE TopenDemoDialogForm.FormCreate(Sender: TObject);
  TYPE T_keyAndValues=record key:string; values:T_arrayOfString; end;
       T_metaArray=array of T_keyAndValues;

  PROCEDURE sort(VAR a:T_metaArray);
    VAR i,j:longint;
        tmp:T_keyAndValues;
    begin
      for i:=1 to length(a)-1 do for j:=0 to i-1 do
      if uppercase(a[i].key)<uppercase(a[j].key) then begin
        tmp:=a[i]; a[i]:=a[j]; a[j]:=tmp;
      end;
    end;

  VAR files:T_arrayOfString;
      i,j,k:longint;
      pack:T_package;
      sub:P_subruleExpression;
      tagText:string;

      demoMetaData:T_metaArray;
      tagMetaData:T_metaArray;
  begin
    ensureDemos;
    files:=find(demosFolder+'*.mnh',true,false);
    setLength(demoMetaData,length(files));
    for i:=0 to length(files)-1 do begin
      demoMetaData[i].key:=ExtractFileNameOnly(files[i]);
      demoMetaData[i].values:=C_EMPTY_STRING_ARRAY;
      pack.create(newFileCodeProvider(files[i]),nil);
      sandbox^.loadForCodeAssistance(pack);
      for sub in pack.getSubrulesByAttribute('demo_for') do begin
        for tagText in split(sub^.metaData.getAttribute('demo_for').value,',') do
        appendIfNew(demoMetaData[i].values,trim(tagText));
      end;
      pack.destroy;
    end;
    sort(demoMetaData);

    setLength(tagMetaData,0);
    for i:=0 to length(demoMetaData)-1 do begin
      for tagText in demoMetaData[i].values do begin
        j:=0;
        while (j<length(tagMetaData)) and (tagMetaData[j].key<>tagText) do inc(j);
        if j>=length(tagMetaData) then begin
          setLength(tagMetaData,j+1);
          tagMetaData[j].key:=tagText;
          tagMetaData[j].values:=C_EMPTY_STRING_ARRAY;
        end;
        append(tagMetaData[j].values,demoMetaData[i].key);
      end;
    end;
    sort(tagMetaData);

    groupByFile:=true;
    reverseOrder:=false;

    setLength(dataByDemo,length(demoMetaData));
    for i:=0 to length(demoMetaData)-1 do begin
      dataByDemo[i,0]:=demoMetaData[i].key;
      dataByDemo[i,1]:=join(demoMetaData[i].values,', ');
      setLength(demoMetaData[i].values,0);
    end;
    setLength(demoMetaData,0);

    setLength(dataByTag,0);
    k:=0;
    for i:=0 to length(tagMetaData)-1 do begin
      for j:=0 to length(tagMetaData[i].values)-1 do begin
        setLength(dataByTag,k+1);
        dataByTag[k,0]:=tagMetaData[i].values[j];
        dataByTag[k,1]:=tagMetaData[i].key;
        inc(k);
      end;
    end;
  end;

PROCEDURE TopenDemoDialogForm.demosGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
  begin
    if index=0 then begin
      if groupByFile then reverseOrder:=not(reverseOrder)
      else begin
        groupByFile:=true;
        reverseOrder:=false;
      end;
    end else begin
      if groupByFile then begin
        groupByFile:=false;
        reverseOrder:=false;
      end else reverseOrder:=not(reverseOrder);
    end;
    paintGrid;
  end;

PROCEDURE TopenDemoDialogForm.demosGridKeyPress(Sender: TObject; VAR key: char);
  begin
    if key=#13 then demosGridDblClick(Sender);
  end;

PROCEDURE TopenDemoDialogForm.demosGridDblClick(Sender: TObject);
  begin
    selectedFile:=demosFolder+demosGrid.Cells[0,demosGrid.selection.top]+'.mnh';
    ModalResult:=mrOk;
  end;

FINALIZATION
  if myOpenDemoDialogForm<>nil then FreeAndNil(myOpenDemoDialogForm);

end.

