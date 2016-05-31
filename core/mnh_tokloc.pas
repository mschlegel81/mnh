// MIT License
//
// Copyright (c) 2016 Martin Schlegel
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

UNIT mnh_tokLoc;
INTERFACE
USES sysutils, mnh_fileWrappers,mnh_constants;
TYPE
  T_searchTokenLocation = record
    fileName: ansistring;
    line, column: longint;
  end;

  T_tokenLocation = record
    package:P_abstractPackage;
    line, column: longint;
  end;

FUNCTION fileTokenLocation(CONST provider: P_codeProvider): T_searchTokenLocation;
FUNCTION packageTokenLocation(CONST package:P_abstractPackage):T_tokenLocation;
OPERATOR := (CONST x: T_tokenLocation): ansistring;
OPERATOR := (CONST x: T_searchTokenLocation): ansistring;
OPERATOR := (CONST x: T_tokenLocation): T_searchTokenLocation;
OPERATOR = (CONST x,y:T_tokenLocation):boolean;
FUNCTION guessLocationFromString(CONST s:ansistring; CONST acceptFilenameWithoutCaret:boolean):T_searchTokenLocation;
IMPLEMENTATION

FUNCTION fileTokenLocation(CONST provider: P_codeProvider): T_searchTokenLocation;
  begin
    result.fileName := provider^.getPath;
    result.line := 1;
    result.column := 1;
  end;

FUNCTION packageTokenLocation(CONST package:P_abstractPackage):T_tokenLocation;
  begin
    result.package:=package;
    result.line := 1;
    result.column := 1;
  end;

OPERATOR := (CONST x: T_tokenLocation): ansistring;
  begin
    if (x.package=nil) or (x.line=0) and (x.column=0) then exit('');
    result:='@'+x.package^.getPath+':'+intToStr(x.line)+','+intToStr(x.column);
  end;

OPERATOR := (CONST x: T_searchTokenLocation): ansistring;
  begin
    if (x.fileName='?') and (x.line=0) and (x.column=0) then exit('');
    result:='@'+x.fileName+':'+intToStr(x.line)+','+intToStr(x.column);
  end;

OPERATOR := (CONST x: T_tokenLocation): T_searchTokenLocation;
  begin
    if x.package=nil then result.fileName:='?' else result.fileName:=x.package^.getPath;
    result.column:=x.column;
    result.line:=x.line;
  end;

OPERATOR = (CONST x,y:T_tokenLocation):boolean;
  begin
    result:=(x.package=y.package) and (x.line=y.line) and (x.column=y.column);
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
    while (i1<=length(s)) and (s[i2] in ['0'..'9']) do inc(i2);
    result.line  :=strToIntDef(copy(s,i0+1,i1-i0-1),0);
    result.column:=strToIntDef(copy(s,i1+1,i2-i1-1),0);
  end;

end.
