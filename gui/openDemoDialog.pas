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
    demoMetaData:array of record
      demoName:string;
      demoTags:T_arrayOfString;
    end;
    tagMetaData:array of record
      tagName:string;
      demos:T_arrayOfString;
    end;
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
  VAR i,j,k:longint;
  begin
    if groupByFile then begin
      demosGrid.RowCount:=length(demoMetaData)+1;
      for i:=0 to length(demoMetaData)-1 do begin
        demosGrid.Cells[0,i+1]:=demoMetaData[i].demoName;
        demosGrid.Cells[1,i+1]:=join(demoMetaData[i].demoTags,', ');
      end;
    end else begin
      j:=0;
      for i:=0 to length(tagMetaData)-1 do inc(j,length(tagMetaData[i].demos));
      demosGrid.RowCount:=j+1;
      j:=1;
      for i:=0 to length(tagMetaData)-1 do begin
        for k:=0 to length(tagMetaData[i].demos)-1 do begin
          demosGrid.Cells[0,j]:=tagMetaData[i].demos[k];
          demosGrid.Cells[1,j]:=tagMetaData[i].tagName;
          inc(j);
        end;
      end;
    end;
  end;

PROCEDURE TopenDemoDialogForm.FormCreate(Sender: TObject);
  VAR files:T_arrayOfString;
      i,j:longint;
      pack:T_package;
      sub:P_subruleExpression;
      tagText:string;
  begin
    ensureDemos;
    files:=find(demosFolder+'*.mnh',true,false);
    setLength(demoMetaData,length(files));
    for i:=0 to length(files)-1 do begin
      demoMetaData[i].demoName:=ExtractFileNameOnly(files[i]);
      demoMetaData[i].demoTags:=C_EMPTY_STRING_ARRAY;
      pack.create(newFileCodeProvider(files[i]),nil);
      sandbox^.loadForCodeAssistance(pack);
      for sub in pack.getSubrulesByAttribute('demo_for') do begin
        for tagText in split(sub^.metaData.getAttribute('demo_for').value,',') do
        appendIfNew(demoMetaData[i].demoTags,trim(tagText));
      end;
      pack.destroy;
    end;

    setLength(tagMetaData,0);
    for i:=0 to length(demoMetaData)-1 do begin
      for tagText in demoMetaData[i].demoTags do begin
        j:=0;
        while (j<length(tagMetaData)) and (tagMetaData[j].tagName<>tagText) do inc(j);
        if j>=length(tagMetaData) then begin
          setLength(tagMetaData,j+1);
          tagMetaData[j].tagName:=tagText;
          tagMetaData[j].demos:=C_EMPTY_STRING_ARRAY;
        end;
        append(tagMetaData[j].demos,demoMetaData[i].demoName);
      end;
    end;

    groupByFile:=true;
  end;

PROCEDURE TopenDemoDialogForm.demosGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
  begin
    groupByFile:=not(groupByFile);
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

