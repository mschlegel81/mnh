UNIT mnh_basicTypes;
INTERFACE
USES sysutils;
TYPE
  T_hashInt  =dword;
  T_idString =ansistring;
  T_myFloat = extended;

  P_objectWithPath=^T_objectWithPath;
  T_objectWithPath=object
    FUNCTION getId:T_idString; virtual; abstract;
    FUNCTION getPath:ansistring; virtual; abstract;
  end;

  T_searchTokenLocation = record
    fileName: ansistring;
    line, column: longint;
  end;
CONST
  C_nilTokenLocation: T_searchTokenLocation = (fileName:'?'; line: 0; column: 0);
TYPE
  T_tokenLocation = record
    package:P_objectWithPath;
    line, column: longint;
  end;

  P_objectWithIdAndLocation=^T_objectWithIdAndLocation;
  T_objectWithIdAndLocation=object
    FUNCTION getId:T_idString; virtual; abstract;
    FUNCTION getLocation:T_tokenLocation; virtual; abstract;
  end;

FUNCTION packageTokenLocation(CONST package:P_objectWithPath):T_tokenLocation;
FUNCTION lineLocation(CONST loc:T_tokenLocation):T_tokenLocation;
OPERATOR := (CONST x: T_tokenLocation): ansistring;
OPERATOR := (CONST x: T_searchTokenLocation): ansistring;
OPERATOR := (CONST x: T_tokenLocation): T_searchTokenLocation;
OPERATOR = (CONST x,y:T_tokenLocation):boolean;
OPERATOR = (CONST x,y:T_searchTokenLocation):boolean;
OPERATOR < (CONST x,y:T_tokenLocation):boolean;
FUNCTION positionIsBeforeOrAtLocation(CONST posLine,posColumn:longint; CONST location:T_tokenLocation):boolean;
FUNCTION positionIsBeforeLocation(CONST posLine,posColumn:longint; CONST location:T_tokenLocation):boolean;
FUNCTION guessLocationFromString(CONST s:ansistring; CONST acceptFilenameWithoutCaret:boolean):T_searchTokenLocation;
IMPLEMENTATION
FUNCTION packageTokenLocation(CONST package:P_objectWithPath):T_tokenLocation;
  begin
    result.package:=package;
    result.line := 1;
    result.column := 1;
  end;

FUNCTION lineLocation(CONST loc:T_tokenLocation):T_tokenLocation;
  begin
    result.package:=loc.package;
    result.line:=loc.line;
    result.column:=-1;
  end;

OPERATOR := (CONST x: T_tokenLocation): ansistring;
  begin
    if (x.package=nil) or (x.line=0) and (x.column=0) then exit('');
    if x.column<0
    then result:='@'+x.package^.getPath+':'+intToStr(x.line)+',1'
    else result:='@'+x.package^.getPath+':'+intToStr(x.line)+','+intToStr(x.column);
  end;

OPERATOR := (CONST x: T_searchTokenLocation): ansistring;
  begin
    if (x.fileName='?') and (x.line=0) and (x.column=0) then exit('');
    if x.column<0
    then result:='@'+x.fileName+':'+intToStr(x.line)+',1'
    else result:='@'+x.fileName+':'+intToStr(x.line)+','+intToStr(x.column);
  end;

OPERATOR := (CONST x: T_tokenLocation): T_searchTokenLocation;
  begin
    try
      if x.package=nil then result.fileName:='?' else result.fileName:=x.package^.getPath;
    except
      result.fileName:='?';
    end;
    result.column:=x.column;
    result.line:=x.line;
  end;

OPERATOR = (CONST x,y:T_tokenLocation):boolean;
  begin
    result:=(x.package=y.package) and (x.line=y.line) and (x.column=y.column);
  end;

OPERATOR = (CONST x,y:T_searchTokenLocation):boolean;
  begin
    result:=(x.fileName=y.fileName) and (x.line=y.line) and (x.column=y.column);
  end;

OPERATOR < (CONST x,y:T_tokenLocation):boolean;
  begin
    if (x.package^.getPath<y.package^.getPath) then exit(true);
    if (x.package^.getPath>y.package^.getPath) then exit(false);
    if (x.line            <y.line            ) then exit(true);
    if (x.line            >y.line            ) then exit(false);
    result:=x.column      <y.column;
  end;

FUNCTION positionIsBeforeOrAtLocation(CONST posLine,posColumn:longint; CONST location:T_tokenLocation):boolean;
  begin
    if (posLine      <=location.line) then exit(true);
    if (posLine      >=location.line) then exit(false);
    result:=posColumn<=location.column;
  end;

FUNCTION positionIsBeforeLocation(CONST posLine,posColumn:longint; CONST location:T_tokenLocation):boolean;
  begin
    if (posLine      <location.line) then exit(true);
    if (posLine      >location.line) then exit(false);
    result:=posColumn<location.column;
  end;

FUNCTION guessLocationFromString(CONST s:ansistring; CONST acceptFilenameWithoutCaret:boolean):T_searchTokenLocation;
  VAR i0,i1,i2:longint;
  begin
    result.fileName:='';
    result.line:=0;
    result.column:=0;
    i0:=1;
    while (i0<=length(s)) and (s[i0]<>'@') do inc(i0);
    result.fileName:='';
    inc(i0);
    while (i0<=length(s)) and ((s[i0]<>':') or (length(result.fileName)=1)) do begin
      result.fileName:=result.fileName+s[i0];
      inc(i0);
    end;
    i1:=i0+1;
    while (i1<=length(s)) and (s[i1] in ['0'..'9']) do inc(i1);
    if (i1>length(s)) or (s[i1]<>',') then begin
      if not(acceptFilenameWithoutCaret) then begin
        result.fileName:='';
        result.line:=0;
        result.column:=0;
      end;
      exit(result);
    end;
    i2:=i1+1;
    while (i1<=length(s)) and (i2<=length(s)) and (s[i2] in ['0'..'9']) do inc(i2);
    result.line  :=strToIntDef(copy(s,i0+1,i1-i0-1),0);
    result.column:=strToIntDef(copy(s,i1+1,i2-i1-1),0);
  end;

end.

