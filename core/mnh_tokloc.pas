UNIT mnh_tokloc;

INTERFACE

USES SysUtils, mnh_fileWrappers;
TYPE
  T_tokenLocation = record
    filename:ansistring;
    line, column: longint;
  end;

CONST
  C_nilTokenLocation: T_tokenLocation = (filename:'?'; line: 0; column: 0);

FUNCTION fileTokenLocation(provider: P_codeProvider): T_tokenLocation;
OPERATOR := (x: T_tokenLocation): ansistring;

IMPLEMENTATION

FUNCTION fileTokenLocation(provider: P_codeProvider): T_tokenLocation;
  begin
    result.filename := provider^.getPath;
    result.line := 1;
    result.column := 1;
  end;

OPERATOR := (x: T_tokenLocation): ansistring;
  begin
    result:=x.filename+':'+IntToStr(x.line)+','+IntToStr(x.column);
  end;


end.
