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
  
OPERATOR :=(x:T_tokenLocation):ansistring;
IMPLEMENTATION
OPERATOR :=(x:T_tokenLocation):ansistring;
  begin
    if x.provider=nil then result:='?'
    else result:=x.provider^.fileIdentifier+':'
                +intToStr(x.line)+','
                +intToStr(x.column);
  end;


end.