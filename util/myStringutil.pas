UNIT myStringUtil;

INTERFACE
USES math, strutils, sysutils,  myGenerics;

TYPE charSet=set of char;


{ T_format }

T_format=object
  category:(fmtCat_decimal,
            fmtCat_scientific,
            fmtCat_fixedPoint,
            fmtCat_general,
            fmtCat_currency,
            fmtCat_number,
            fmtCat_string,
            fmtCat_hex);
  intFmt,realFmt,strFmt:string;

  CONSTRUCTOR create(CONST formatString:string);
  FUNCTION format(CONST s:string):string;
  FUNCTION format(CONST i:int64):string;
  FUNCTION format(CONST e:extended):string;
  DESTRUCTOR destroy;
end;

CONST
  C_lineBreakChar = chr(10);
  C_carriageReturnChar = chr(13);
  C_tabChar = chr(9);
  BLANK_TEXT = '';
  IDENTIFIER_CHARS:charSet=['a'..'z','A'..'Z','0'..'9','.','_'];
  FORMAT_CHARS:charSet=['d','D','e','E','f','F','g','G','m','M','n','N','s','S','x','X'];

FUNCTION formatTabs(CONST s: T_arrayOfString): T_arrayOfString;
FUNCTION isBlank(CONST s: ansistring): boolean;
FUNCTION replaceAll(CONST original, lookFor, replaceBy: ansistring): ansistring; inline;
FUNCTION replaceRecursively(CONST original, lookFor, replaceBy: ansistring; OUT isValid: boolean): ansistring; inline;
FUNCTION replaceOne(CONST original, lookFor, replaceBy: ansistring): ansistring; inline;
FUNCTION escapeString(CONST s: ansistring): ansistring;
FUNCTION unescapeString(CONST input: ansistring; CONST offset:longint; OUT parsedLength: longint): ansistring;
FUNCTION isIdentifier(CONST s: ansistring; CONST allowDot: boolean): boolean;
FUNCTION startsWith(CONST input, head: ansistring): boolean;
FUNCTION unbrace(CONST s:ansistring):ansistring;
FUNCTION split(CONST s:ansistring):T_arrayOfString;
FUNCTION reSplit(CONST s:T_arrayOfString):T_arrayOfString;
FUNCTION split(CONST s:ansistring; CONST splitters:T_arrayOfString):T_arrayOfString;
FUNCTION join(CONST lines:T_arrayOfString; CONST joiner:ansistring):ansistring;
FUNCTION cleanString(CONST s:ansistring; CONST whiteList:charSet; CONST instead:char):ansistring;
FUNCTION myTimeToStr(dt:double):string;

IMPLEMENTATION

FUNCTION formatTabs(CONST s: T_arrayOfString): T_arrayOfString;
  VAR matrix: array of T_arrayOfString;
      i, j, maxJ, maxLength, dotPos: longint;
      anyTab:boolean=false;
  FUNCTION isNumeric(s: ansistring): boolean;
    VAR i: longint;
        hasDot, hasExpo: boolean;
    begin
      result := length(s)>0;
      hasDot := false;
      hasExpo := false;
      for i := 1 to length(s)-1 do if s [i] = '.' then begin
        result := result and not (hasDot);
        hasDot := true;
      end else if (s [i] in ['e', 'E']) and (i>1) then begin
        result := result and not (hasExpo);
        hasExpo := true;
      end else result := result and ((s [i] in ['0'..'9']) or ((i = 1) or (s [i-1] in ['e', 'E'])) and (s [i] in ['-', '+']));
    end;

  FUNCTION posOfDot(s: ansistring): longint;
    begin
      result := pos('.', s);
      if result = 0 then result := length(s)+1;
    end;

  begin
    for i:=0 to length(s)-1 do anyTab:=anyTab or (pos(C_tabChar,s[i])>0);
    if not(anyTab) then exit(s);

    result:=s;
    setLength(matrix,length(result));
    j := -1;
    maxJ := -1;
    for i:=0 to length(result)-1 do begin
      matrix[i]:=split(result[i],C_tabChar);
      j:=length(matrix[i])-1;
      if j>maxJ then maxJ:=j;
    end;
    //expand columns to equal size:
    for j := 0 to maxJ do begin
      dotPos := 0;
      for i := 0 to length(matrix)-1 do
        if (length(matrix [i])>j) and (isNumeric(matrix [i] [j])) and
          (posOfDot(matrix [i] [j])>dotPos) then
          dotPos := posOfDot(matrix [i] [j]);
      if dotPos>0 then
        for i := 0 to length(matrix)-1 do
          if (length(matrix [i])>j) and (isNumeric(matrix [i] [j])) then
            while posOfDot(matrix [i] [j])<dotPos do
              matrix[i][j] := ' '+matrix [i] [j];

      maxLength := 0;
      for i := 0 to length(matrix)-1 do
        if (length(matrix [i])>j) and (length(matrix [i] [j])>maxLength) then
          maxLength := length(matrix [i] [j]);
      for i := 0 to length(matrix)-1 do
        if (length(matrix [i])>j) then
          while length(matrix [i] [j])<=maxLength do
            matrix[i][j] := matrix [i] [j]+' ';
    end;

    //join matrix to result;
    for i:=0 to length(matrix)-1 do result[i]:=trimRight(join(matrix[i],''));
  end;

FUNCTION isBlank(CONST s: ansistring): boolean;
  VAR
    i: longint;
  begin
    result := true;
    for i := 1 to length(s) do
      if not (s [i] in [C_lineBreakChar, C_carriageReturnChar, C_tabChar, ' ']) then
        exit(false);
  end;

FUNCTION replaceOne(CONST original, lookFor, replaceBy: ansistring): ansistring; inline;
  VAR
    p: longint;
  begin
    p := pos(lookFor, original);
    if p>0 then
      result := copy(original, 1, p-1)+replaceBy+
        copy(original, p+length(lookFor), length(original))
    else
      result := original;
  end;

FUNCTION replaceAll(CONST original, lookFor, replaceBy: ansistring): ansistring; inline;
  FUNCTION anyOfLookFor(CONST c:char):boolean;
    VAR k:longint;
    begin
      for k:=1 to length(lookFor) do if lookFor[k]=c then exit(true);
      result:=false;
    end;

  VAR i:longint;
  begin
    if length(original)>65536 then begin
      i:=round(length(original)*0.49);
      while (i<=length(original)) and anyOfLookFor(original[i]) do inc(i);
      result:=replaceAll(copy(original,1,                 i-1),lookFor,replaceBy)+
              replaceAll(copy(original,i,length(original)+1-i),lookFor,replaceBy);
    end else result:=AnsiReplaceStr(original,lookFor,replaceBy);
  end;

FUNCTION replaceRecursively(CONST original, lookFor, replaceBy: ansistring; OUT isValid: boolean): ansistring; inline;
  FUNCTION anyOfLookFor(CONST c:char):boolean;
    VAR k:longint;
    begin
      for k:=1 to length(lookFor) do if lookFor[k]=c then exit(true);
      result:=false;
    end;

  VAR prev:ansistring;
      i:longint;
  begin
    if pos(lookFor, replaceBy)>0 then begin
      isValid := false;
      exit(replaceAll(original, lookFor, replaceBy));
    end else isValid := true;
    if length(original)>65536 then begin
      i:=round(length(original)*0.49);
      while (i<=length(original)) and anyOfLookFor(original[i]) do inc(i);
      result:=replaceAll(
              replaceRecursively(copy(original,1,                 i-1),lookFor,replaceBy,isValid)+
              replaceRecursively(copy(original,i,length(original)+1-i),lookFor,replaceBy,isValid),
                                                                       lookFor,replaceBy);
    end else begin
      result:=original;
      repeat
        prev:=result;
        result:=AnsiReplaceStr(prev,lookFor,replaceBy);
      until prev=result;
    end;
  end;

FUNCTION escapeString(CONST s: ansistring): ansistring;
  FUNCTION pascalStyle:ansistring;
    CONST DELIM='''';
    begin
      result:=DELIM+replaceAll(s,DELIM,DELIM+DELIM)+DELIM;
    end;

  FUNCTION javaStyle:ansistring;
    begin
      result:='"'+replaceAll(replaceAll(replaceAll(replaceAll(replaceAll(
                                s, '\', '\\'),
                             C_tabChar, '\t'),
                       C_lineBreakChar, '\n'),
                  C_carriageReturnChar, '\r'),
                                   '"', '\"')+'"';
    end;

  VAR tmp:ansistring;
  begin
    //choose the delimiter leading to a shorter representation
    if (pos(C_tabChar,s)>0) or
       (pos(C_lineBreakChar,s)>0) or
       (pos(C_carriageReturnChar,s)>0) then result:=javaStyle
    else begin
      tmp:=javaStyle;
      result:=pascalStyle;
      if length(tmp)<length(result) then result:=tmp;
    end;
  end;

FUNCTION unescapeString(CONST input: ansistring; CONST offset:longint; OUT parsedLength: longint): ansistring;
  {$MACRO ON}
  {$define exitFailing:=begin parsedLength:=0; exit(''); end}
  CONST SQ='''';
        DQ='"';
  VAR i,i0,i1: longint;
      continue:boolean;
  begin
    if length(input)>=offset+1 then begin //need at least a leading and a trailing delimiter
      if input[offset]=SQ then begin
        i0:=offset+1; i:=i0; i1:=offset; continue:=true; result:='';
        while (i<=length(input)) and continue do if input[i]=SQ then begin
          if (i<length(input)) and (input[i+1]=SQ) then begin
            result:=result+copy(input,i0,i1-i0+1)+SQ;
            inc(i,2);
            i0:=i;
            i1:=i0-1;
          end else continue:=false;
        end else begin
          i1:=i;
          inc(i);
        end;
        if continue then exitFailing;
        result:=result+copy(input,i0,i1-i0+1);
        parsedLength:=i+1-offset;
        exit(result);
      end else if input[offset]=DQ then begin
        i0:=offset+1; i:=i0; i1:=offset; continue:=true; result:='';
        while (i<=length(input)) and (input[i]<>DQ) do if input[i]='\' then begin
          if (i<length(input)) and (input[i+1] in ['\','t','n','r',DQ]) then begin
            case input[i+1] of
              't': result:=result+copy(input,i0,i1-i0+1)+C_tabChar;
              'n': result:=result+copy(input,i0,i1-i0+1)+C_lineBreakChar;
              'r': result:=result+copy(input,i0,i1-i0+1)+C_carriageReturnChar;
              else result:=result+copy(input,i0,i1-i0+1)+input[i+1];
            end;
            inc(i,2);
            i0:=i;
            i1:=i0-1;
          end else exitFailing;
        end else begin
          i1:=i;
          inc(i);
        end;
        result:=result+copy(input,i0,i1-i0+1);
        parsedLength:=i+1-offset;
        exit(result);
      end;
    end;
    exitFailing;
  end;

FUNCTION isIdentifier(CONST s: ansistring; CONST allowDot: boolean): boolean;
  VAR i: longint;
      dotAllowed: boolean;
  begin
    dotAllowed := allowDot;
    result := (length(s)>=1) and (s [1] in ['a'..'z', 'A'..'Z']);
    i := 2;
    while result and (i<=length(s)) do
      if (s [i] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) then
        inc(i)
      else if (s [i] = '.') and dotAllowed then
        begin
        inc(i);
        dotAllowed := false;
        end
      else
        result := false;
  end;

FUNCTION startsWith(CONST input, head: ansistring): boolean;
  begin
    result := copy(input, 1, length(head)) = head;
  end;

FUNCTION unbrace(CONST s:ansistring):ansistring;
  begin
    if (length(s)>=2) and (
        (s[1]='(') and (s[length(s)]=')') or
        (s[1]='[') and (s[length(s)]=']') or
        (s[1]='{') and (s[length(s)]='}'))
    then result:=copy(s,2,length(s)-2)
    else result:=s;
  end;


FUNCTION split(CONST s:ansistring):T_arrayOfString;
  VAR lineSplitters:T_arrayOfString;
  begin
    lineSplitters:=(C_carriageReturnChar+C_lineBreakChar);
    append(lineSplitters,C_lineBreakChar+C_carriageReturnChar);
    append(lineSplitters,C_lineBreakChar);
    result:=split(s,lineSplitters);
  end;

FUNCTION reSplit(CONST s:T_arrayOfString):T_arrayOfString;
  VAR i:longint;
  begin
    setLength(result,0);
    for i:=0 to length(s)-1 do append(result,split(s[i]));
  end;

FUNCTION split(CONST s:ansistring; CONST splitters:T_arrayOfString):T_arrayOfString;
  PROCEDURE firstSplitterPos(CONST s:ansistring; OUT splitterStart,splitterEnd:longint);
    VAR i,p:longint;
    begin
      splitterStart:=0;
      for i:=0 to length(splitters)-1 do begin
        p:=pos(splitters[i],s);
        if (p>0) and ((splitterStart=0) or (p<splitterStart)) then begin
          splitterStart:=p;
          splitterEnd:=p+length(splitters[i]);
        end;
      end;
    end;

  VAR sp0,sp1:longint;
      rest:ansistring;
  begin
    setLength(result,0);
    firstSplitterPos(s,sp0,sp1);
    if sp0<=0 then exit(s);
    rest:=s;
    while sp0>0 do begin
      append(result,copy(rest,1,sp0-1));
      rest:=copy(rest,sp1,length(rest));
      firstSplitterPos(rest,sp0,sp1);
    end;
    append(result,rest);
  end;

FUNCTION join(CONST lines:T_arrayOfString; CONST joiner:ansistring):ansistring;
  VAR i:longint;
  begin
    if length(lines)>0 then result:=lines[0] else result:='';
    for i:=1 to length(lines)-1 do result:=result+joiner+lines[i];
  end;

FUNCTION cleanString(CONST s:ansistring; CONST whiteList:charSet; CONST instead:char):ansistring;
  VAR k:longint;
      tmp:shortString;
  begin
    if length(s)<=255 then begin
      tmp:=s;
      for k:=1 to length(s) do if not(tmp[k] in whiteList) then tmp[k]:=instead;
      exit(tmp);
    end;
    result:='';
    for k:=1 to length(s) do if s[k] in whiteList then result:=result+s[k] else result:=result+instead;
  end;

FUNCTION myTimeToStr(dt:double):string;
  CONST oneMinute=1/(24*60);
        oneSecond=oneMinute/60;
  begin
    if dt<oneMinute
      then begin
        result:=formatFloat('#0.00',dt/oneSecond)+'sec';
        if length(result)<8 then result:=' '+result;
      end
    else if dt>1
      then begin
        dt:=dt*24;             result:=       formatFloat('00',floor(dt))+':';
        dt:=(dt-floor(dt))*60; result:=result+formatFloat('00',floor(dt))+':';
        dt:=(dt-floor(dt))*60; result:=result+formatFloat('00',floor(dt));
      end
    else result:=timeToStr(dt);
  end;

{ T_format }

CONSTRUCTOR T_format.create(CONST formatString: string);
  begin
    if length(formatString)>0 then case formatString[length(formatString)] of
      'd','D': begin
        category:=fmtCat_decimal;
        intFmt :=formatString;
        strFmt :=copy(formatString,1,length(formatString)-1)+'s';
        realFmt:=copy(formatString,1,length(formatString)-1)+'f';
      end;
      'e','E': begin
        category:=fmtCat_scientific;
        realFmt:=formatString;
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
      end;
      'f','F': begin
        category:=fmtCat_fixedPoint;
        realFmt:=formatString;
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
      end;
      'g','G': begin
        category:=fmtCat_general;
        realFmt:=formatString;
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
      end;
      'm','M': begin
        category:=fmtCat_currency;
        realFmt:=formatString;
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
      end;
      'n','N': begin
        category:=fmtCat_number;
        realFmt:=formatString;
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        strFmt :='%'+intToStr(length(sysutils.format(realFmt,[0.0])))+'s';
      end;
      's','S': begin
        category:=fmtCat_string;
        strFmt :=formatString;
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        realFmt:=copy(formatString,1,length(formatString)-1)+'f';
      end;
      'x','X': begin
        category:=fmtCat_hex;
        intFmt :=formatString;
        strFmt :=copy(formatString,1,length(formatString)-1)+'s';
        realFmt:=copy(formatString,1,length(formatString)-1)+'f';
      end;
      else begin
        intFmt :=copy(formatString,1,length(formatString)-1)+'d';
        strFmt :=copy(formatString,1,length(formatString)-1)+'s';
        realFmt:=copy(formatString,1,length(formatString)-1)+'f';
      end;
    end;
  end;

FUNCTION T_format.format(CONST s: string): string;
  begin
    result:=sysutils.format(strFmt,[s]);
  end;

FUNCTION T_format.format(CONST i: int64): string;
  begin
    if category in [fmtCat_scientific..fmtCat_number]
      then result:=sysutils.format(realFmt,[extended(i)])
      else result:=sysutils.format(intFmt,[i]);
  end;

FUNCTION T_format.format(CONST e: extended): string;
  begin
    result:=sysutils.format(realFmt,[e]);
  end;

DESTRUCTOR T_format.destroy;
  begin

  end;

end.
