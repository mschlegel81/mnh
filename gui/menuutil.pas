UNIT menuUtil;
INTERFACE
USES
  Menus,myStringUtil,Classes,myGenerics,sysutils;
TYPE
  P_submenuModel=^T_submenuModel;
  T_submenuModel=object
    private
      item:TMenuItem;
      OnClick:TNotifyEvent;
      subNodes:array of P_submenuModel;
    public
      CONSTRUCTOR create(CONST root:TMenuItem; CONST onChildClick:TNotifyEvent);
      DESTRUCTOR destroy;
      PROCEDURE clear;
      PROCEDURE addItem(CONST paths:T_arrayOfString; CONST Tag:ptrint);
      PROCEDURE addItem(CONST path:string; CONST Tag:ptrint);
  end;

IMPLEMENTATION

CONSTRUCTOR T_submenuModel.create(CONST root: TMenuItem; CONST onChildClick: TNotifyEvent);
  begin
    item:=root;
    OnClick:=onChildClick;
    setLength(subNodes,0);
  end;

DESTRUCTOR T_submenuModel.destroy;
  begin
    clear;
  end;

PROCEDURE T_submenuModel.clear;
  VAR k:longint;
      subItem:TMenuItem;
  begin
    for k:=length(subNodes)-1 downto 0 do begin
      subItem:=subNodes[k]^.item;
      dispose(subNodes[k],destroy);
      item.remove(subItem);
      FreeAndNil(subItem);
    end;
    setLength(subNodes,0);
  end;

PROCEDURE T_submenuModel.addItem(CONST paths: T_arrayOfString; CONST Tag: ptrint);
  FUNCTION ensureSubmenu:P_submenuModel;
    VAR k:longint;
        newItem:TMenuItem;
    begin
      for k:=0 to length(subNodes)-1 do if subNodes[k]^.item.caption=paths[0] then exit(subNodes[k]);
      newItem:=TMenuItem.create(item);
      newItem.caption:=paths[0];
      item.add(newItem);

      new(result,create(newItem,OnClick));
      k:=length(subNodes);
      setLength(subNodes,k+1);
      subNodes[k]:=result;
    end;

  VAR sub:P_submenuModel;
      pathTail:T_arrayOfString;
      k:longint;
  begin
    if length(paths)=0 then exit;
    sub:=ensureSubmenu;
    if length(paths)=1 then begin
      //clickable item
      sub^.item.OnClick:=OnClick;
      sub^.item.Tag:=Tag;
    end else begin
      //submenu root
      setLength(pathtail,length(paths)-1);
      for k:=0 to length(pathTail)-1 do pathTail[k]:=paths[k+1];
      sub^.addItem(pathTail,Tag);
    end;
  end;

PROCEDURE T_submenuModel.addItem(CONST path: string; CONST Tag: ptrint);
  begin
    addItem(split(path,'/'),Tag);
  end;

end.

