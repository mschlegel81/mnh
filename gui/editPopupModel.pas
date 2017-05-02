UNIT editPopupModel;
INTERFACE
USES Menus;
TYPE
  T_openFile=PROCEDURE(CONST fileName:string) of object;
  T_editPopupModel=object
    private
      popupMi1  ,popupMi2  :TMenuItem;
      popupFile1,popupFile2:string;
      openfileCallback:T_openFile;
      PROCEDURE openFile1Click(Sender: TObject);
      PROCEDURE openFile2Click(Sender: TObject);
    public
      CONSTRUCTOR create(CONST mi1,mi2:TMenuItem; CONST openMethod:T_openFile);
      DESTRUCTOR destroy;
      PROCEDURE setFiles(CONST file1,file2:string);
  end;

VAR popupModel:T_editPopupModel;
IMPLEMENTATION

PROCEDURE T_editPopupModel.openFile1Click(Sender: TObject);
  begin
    openfileCallback(popupFile1);
  end;

PROCEDURE T_editPopupModel.openFile2Click(Sender: TObject);
  begin
    openfileCallback(popupFile2);
  end;

CONSTRUCTOR T_editPopupModel.create(CONST mi1, mi2: TMenuItem; CONST openMethod: T_openFile);
  begin
    popupMi1:=mi1; popupMi1.OnClick:=@openFile1Click;
    popupMi2:=mi2; popupMi2.OnClick:=@openFile2Click;
    openfileCallback:=openMethod;
  end;

DESTRUCTOR T_editPopupModel.destroy;
begin
end;

PROCEDURE T_editPopupModel.setFiles(CONST file1, file2: string);
  begin
    popupFile1:=file1; popupMi1.caption:='Open: "'+popupFile1+'"';
    popupFile2:=file2; popupMi2.caption:='Open: "'+popupFile2+'"';
  end;

end.
