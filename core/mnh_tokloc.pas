UNIT mnh_tokLoc;

INTERFACE

USES sysutils, mnh_fileWrappers;
TYPE
  T_tokenLocation = record
    fileName:ansistring;
    line, column: longint;
  end;

CONST
  C_nilTokenLocation: T_tokenLocation = (fileName:'?'; line: 0; column: 0);

FUNCTION fileTokenLocation(provider: P_codeProvider): T_tokenLocation;
OPERATOR := (x: T_tokenLocation): ansistring;

IMPLEMENTATION

FUNCTION fileTokenLocation(provider: P_codeProvider): T_tokenLocation;
  begin
    result.fileName := provider^.getPath;
    result.line := 1;
    result.column := 1;
  end;

OPERATOR := (x: T_tokenLocation): ansistring;
  begin
    result:=x.fileName+':'+intToStr(x.line)+','+intToStr(x.column);
  end;


end.
