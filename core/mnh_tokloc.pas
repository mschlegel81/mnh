UNIT mnh_tokloc;
INTERFACE
USES sysutils, mnh_fileWrappers;
TYPE
  T_tokenLocation=record
    provider:P_codeProvider;
    line,column:longint;
  end;

CONST
  C_nilTokenLocation:T_tokenLocation=(provider:nil; line:0; column:0);

FUNCTION fileTokenLocation(provider:P_codeProvider):T_tokenLocation;
OPERATOR :=(x:T_tokenLocation):ansistring;
IMPLEMENTATION

FUNCTION fileTokenLocation(provider: P_codeProvider): T_tokenLocation;
begin
  result.provider:=provider;
  result.line:=1;
  result.column:=1;
end;

OPERATOR :=(x:T_tokenLocation):ansistring;
  begin
    if x.provider=nil then result:='?'
    else result:=x.provider^.fileName+':'
                +intToStr(x.line)+','
                +intToStr(x.column);
  end;


end.
