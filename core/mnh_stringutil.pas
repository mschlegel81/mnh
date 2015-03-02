UNIT mnh_stringutil;
INTERFACE
CONST
  C_lineBreakChar     =chr(13);
  C_carriageReturnChar=chr(10);
  C_tabChar           =chr(9);

FUNCTION formatTabs(s:ansistring):ansistring;

FUNCTION getNiceHead(VAR inputAndRest:ansistring; CONST minLineLength,maxLineLength:longint):ansistring;
FUNCTION replaceAll(original,lookFor,replaceBy:ansistring):ansistring; inline;
FUNCTION replaceRecursively(CONST original,lookFor,replaceBy:ansistring; OUT isValid:boolean):ansistring; inline;
FUNCTION replace(CONST original,lookFor,replaceBy:ansistring):ansistring; inline;
FUNCTION escapeString(CONST s:ansistring):ansistring;
FUNCTION unescapeString(CONST input:ansistring; OUT parsedLength:longint):ansistring;
FUNCTION isIdentifier(CONST s:ansistring; CONST allowDot:boolean):boolean;
FUNCTION startsWith(CONST input,head:ansistring):boolean;
//PROCEDURE removeLeadingBlanks(VAR input:ansistring);

IMPLEMENTATION
FUNCTION formatTabs(s:ansistring):ansistring;
  VAR matrix:array of array of ansistring;
      pb,pt,i,j,maxJ,maxLength,dotpos:longint;

  FUNCTION isNumeric(s:AnsiString):boolean;
    VAR i:longint;
        hasDot,hasExpo:boolean;
    begin
      result:=length(s)>0;
      hasDot:=false;
      hasExpo:=false;
      for i:=1 to length(s)-1 do begin
        if s[i]='.' then begin
          result:=result and not(hasDot);
          hasDot:=true;
        end else if (s[i] in ['e','E']) and (i>1) then begin
          result:=result and not(hasExpo);
          hasExpo:=true;
        end else result:=result and ((s[i] in ['0'..'9']) or ((i=1) or (s[i-1] in ['e','E'])) and (s[i] in ['-','+']));
      end;
    end;

  FUNCTION posOfDot(s:AnsiString):longint;
    begin
      result:=pos('.',s);
      if result=0 then result:=length(s)+1;
    end;

  begin
    pb:=pos(C_lineBreakChar,s);
    pt:=pos(C_tabChar,s);
    if pt<=0 then begin
      //s contains no tabs -> no processing needed
      result:=s;
    end else if pb<=0 then begin
      //s contains no line break but tabs -> replace all tabs by spaces;
      while pt>0 do begin
        s[pt]:=' ';
        pt:=pos(C_tabChar,s);
      end;
      result:=s;
    end else begin
      //s contains both line breaks and tabs -> full processing
      setLength(matrix,1); i:=0; j:=-1; maxJ:=-1;
      repeat
        pb:=pos(C_lineBreakChar,s); if pb<=0 then pb:=length(s)+1;
        pt:=pos(C_tabChar,s);       if pt<=0 then pt:=length(s)+1;
        if pt<pb then begin
          //if tab comes before break, split string at tab and add new column
          inc(j); if j>maxJ then maxJ:=j;
          setLength(matrix[i],j+1);
          matrix[i][j]:=copy(s,1,pt-1);
          s:=copy(s,pt+1,length(s));
          pb:=pos(C_lineBreakChar,s); if pb<=0 then pb:=length(s)+1;
          pt:=pos(C_tabChar,s);       if pt<=0 then pt:=length(s)+1;
        end else if pb<pt then begin
          //if break comes before tab, add rest to current row and start new row
          inc(j); if j>maxJ then maxJ:=j;
          setLength(matrix[i],j+1);
          matrix[i][j]:=copy(s,1,pb-1);
          s:=copy(s,pb+1,length(s));
          inc(i); j:=-1;
          setLength(matrix,i+1);
          pb:=pos(C_lineBreakChar,s); if pb<=0 then pb:=length(s)+1;
          pt:=pos(C_tabChar,s);       if pt<=0 then pt:=length(s)+1;
        end else begin
          //if break and tab are at equal position (meaning none are present), add rest to current row
          inc(j); if j>maxJ then maxJ:=j;
          setLength(matrix[i],j+1);
          matrix[i][j]:=s;
          s:='';
        end;
      until s='';
      //expand columns to equal size:
      for j:=0 to maxJ do begin
        dotpos:=0;
        for i:=0 to length(matrix)-1 do if (length(matrix[i])>j) and (isNumeric(matrix[i][j])) and (posOfDot(matrix[i][j])>dotpos)
          then dotpos:=posOfDot(matrix[i][j]);
        if dotPos>0 then begin
          for i:=0 to length(matrix)-1 do if (length(matrix[i])>j) and (isNumeric(matrix[i][j])) then begin
            while posOfDot(matrix[i][j])<dotpos do matrix[i][j]:=' '+matrix[i][j];
          end;
        end;

        maxLength:=0;
        for i:=0 to length(matrix)-1 do if (length(matrix[i])>j) and (length(matrix[i][j])>maxLength) then maxLength:=length(matrix[i][j]);
        for i:=0 to length(matrix)-1 do if (length(matrix[i])>j) then begin
          while length(matrix[i][j])<=maxLength do matrix[i][j]:=matrix[i][j]+' ';
        end;
      end;
      //join matrix to result;
      result:='';
      for i:=0 to length(matrix)-1 do begin
        if (i>0) then result:=result+C_lineBreakChar;
        for j:=0 to length(matrix[i])-1 do result:=result+matrix[i][j];
      end;
    end;
  end;

FUNCTION getNiceHead(VAR inputAndRest:ansistring; CONST minLineLength,maxLineLength:longint):ansistring;
  VAR i:longint;
  begin
    if length(inputAndRest)<maxLineLength then begin
      result:=inputAndRest;
      inputAndRest:='';
      exit(result);
    end;
    i:=maxLineLength;
    while (i>minLineLength) and (inputAndRest[i]<>' ') do dec(i);
    result:=copy(inputAndRest,1,i);
    inputAndRest:=copy(inputAndRest,i+1,length(inputAndRest)-1);
  end;

FUNCTION replace(CONST original,lookFor,replaceBy:ansistring):ansistring; inline;
  VAR p:longint;
  begin
    p:=pos(lookFor,original);
    if p>0 then begin
      result:=copy(original,1,p-1)+replaceBy+copy(original,p+length(lookFor),length(original));
    end else result:=original;
  end;

FUNCTION replaceRecursively(CONST original,lookFor,replaceBy:ansistring; OUT isValid:boolean):ansistring; inline;
  VAR p:longint;
  begin
    if pos(lookFor,replaceBy)>0 then begin
      isValid:=false;
      exit('');
    end else isValid:=true;
    p:=pos(lookFor,original);
    if p>0 then begin
      result:=copy(original,1,p-1)+replaceBy+copy(original,p+length(lookFor),length(original));
      p:=pos(lookFor,original);
    end else result:=original;
  end;


FUNCTION replaceAll(original,lookFor,replaceBy:ansistring):ansistring; inline;
    VAR p:longint;
    begin
      result:='';
      p:=pos(lookFor,original);
      while p>0 do begin
        result:=result+copy(original,1,p-1)+replaceBy;
        original:=copy(original,p+length(lookFor),length(original));
        p:=pos(lookFor,original);
      end;
      result:=result+original;
    end;

FUNCTION escapeString(CONST s:ansistring):ansistring;
  FUNCTION replaceAll(input,original,replacement:string):string;
    VAR p:longint;
    begin
      result:='';
      p:=pos(original,input);
      while p>0 do begin
        result:=result+copy(input,1,p-1)+replacement;
        input :=copy(input,p+length(original),length(input));
        p:=pos(original,input);
      end;
      result:=result+input;
    end;

  begin
    result:='"'+replaceAll(replaceAll(replaceAll(replaceAll(replaceAll(s,
      '\'                 ,'\\'),
      C_tabChar           ,'\t'),
      C_lineBreakChar     ,'\n'),
      C_carriageReturnChar,'\r'),
      '"'                 ,'\"')+'"';
  end;

FUNCTION unescapeString(CONST input:ansistring; OUT parsedLength:longint):ansistring;
  VAR i:longint;
  begin
    if (length(input)>=1) and (input[1]='"') then begin
      result:='';
      i:=2;
      while (i<=length(input)) and (input[i]<>'"') do
        if (input[i]='\') then begin
          if i<length(input) then case input[i+1] of
            '\': begin result:=result+'\';             inc(i,2); end;
            't': begin result:=result+C_tabChar;       inc(i,2); end;
            'n': begin result:=result+C_lineBreakChar; inc(i,2); end;
            'r': begin result:=result+C_carriageReturnChar; inc(i,2); end;
            '"': begin result:=result+'"';             inc(i,2); end;
            else i:=length(input)+1;
          end else i:=length(input)+1;
        end else begin
          result:=result+input[i];
          inc(i);
        end;
      parsedLength:=i;
    end else begin
      parsedLength:=0;
      result:='';
    end;
  end;

FUNCTION isIdentifier(CONST s:ansistring; CONST allowDot:boolean):boolean;
  VAR i:longint;
      dotAllowed:boolean;
  begin
    dotAllowed:=allowDot;
    result:=(length(s)>=1) and (s[1] in ['a'..'z','A'..'Z']);
    i:=2;
    while result and (i<=length(s)) do begin
      if (s[i] in ['a'..'z','A'..'Z','0'..'9','_']) then inc(i)
      else if (s[i]='.') and dotAllowed then begin
        inc(i);
        dotAllowed:=false;
      end else result:=false;
    end;
  end;

FUNCTION startsWith(CONST input,head:ansistring):boolean;
  begin
    result:=copy(input,1,length(head))=head;
  end;

end.