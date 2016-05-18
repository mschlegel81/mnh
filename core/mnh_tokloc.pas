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
OPERATOR = (CONST x,y:T_tokenLocation):boolean;
FUNCTION guessLocationFromString(CONST s:ansistring; CONST acceptFilenameWithoutCaret:boolean):T_tokenLocation;
IMPLEMENTATION

FUNCTION fileTokenLocation(provider: P_codeProvider): T_tokenLocation;
  begin
    result.fileName := provider^.getPath;
    result.line := 1;
    result.column := 1;
  end;

OPERATOR := (x: T_tokenLocation): ansistring;
  begin
    if (x.fileName='?') and (x.line=0) and (x.column=0) then exit('');
    result:='@'+x.fileName+':'+intToStr(x.line)+','+intToStr(x.column);
  end;

OPERATOR = (CONST x,y:T_tokenLocation):boolean;
  begin
    result:=(x.fileName=y.fileName) and (x.line=y.line) and (x.column=y.column);
  end;

FUNCTION guessLocationFromString(CONST s:ansistring; CONST acceptFilenameWithoutCaret:boolean):T_tokenLocation;
  VAR i0,i1,i2:longint;
  begin
    result:=C_nilTokenLocation;
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
      if acceptFilenameWithoutCaret
      then exit(result)
      else exit(C_nilTokenLocation);
    end;
    i2:=i1+1;
    while (i1<=length(s)) and (s[i2] in ['0'..'9']) do inc(i2);
    result.line  :=strToIntDef(copy(s,i0+1,i1-i0-1),0);
    result.column:=strToIntDef(copy(s,i1+1,i2-i1-1),0);
  end;

end.
