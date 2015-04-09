{$ifdef include_interface}
  T_idStack=object
    ids:array of array of ansistring;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE clear;
    PROCEDURE scopePush;
    PROCEDURE scopePop;
    FUNCTION oneAboveBottom:boolean;
    FUNCTION scopeBottom:boolean;
    PROCEDURE addId(CONST id:ansistring);
    FUNCTION hasId(CONST id:ansistring):boolean;
  end;
{$endif}
{$ifdef include_implementation}
CONSTRUCTOR T_idStack.create;
  begin
    setLength(ids,0);
  end;

DESTRUCTOR T_idStack.destroy;
  begin clear;  end;

PROCEDURE T_idStack.clear;
  begin
    while not(scopeBottom) do scopePop;
  end;

PROCEDURE T_idStack.scopePush;
  begin
    setLength(ids,length(ids)+1);
  end;

PROCEDURE T_idStack.scopePop;
  begin
    setLength(ids,length(ids)-1);
  end;

FUNCTION T_idStack.oneAboveBottom:boolean;
  begin
    result:=length(ids)=1;
  end;

FUNCTION T_idStack.scopeBottom:boolean;
  begin
    result:=length(ids)=0;
  end;

PROCEDURE T_idStack.addId(CONST id:ansistring);
  VAR i,j:longint;
  begin
    i:=length(ids)-1;
    j:=length(ids[i]);
    setLength(ids[i],j+1);
    ids[i,j]:=id;
  end;

FUNCTION T_idStack.hasId(CONST id:ansistring):boolean;
  VAR i,j:longint;
  begin
    result:=false;
    for i:=length(ids)-1 downto 0 do for j:=0 to length(ids[i])-1 do if ids[i,j]=id then exit(true);
  end;
{$endif}
