PROGRAM replaceTest;
USES sysutils,strutils;




FUNCTION randomString(CONST len:longint):ansistring;
  FUNCTION randomChar:char;
    begin
      case byte(random(4)) of
        0: result:=' ';
        1: result:=chr(ord('0')+random(10));
        2: result:=chr(ord('a')+random(26));
        3: result:=chr(ord('A')+random(26));
      end;
    end;
  VAR i:longint;
  begin
    result:='';
    for i:=1 to len do result:=result+randomChar;
  end;

FUNCTION splitReplace(CONST original,lookFor,replaceBy:ansistring):ansistring;
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
      result:=splitReplace(copy(original,1,                 i-1),lookFor,replaceBy)+
              splitReplace(copy(original,i,length(original)+1-i),lookFor,replaceBy);
    end else result:=AnsiReplaceStr(original,lookFor,replaceBy);
  end;

PROCEDURE measureCost(CONST problemSize:longint);
  VAR t:double;
      txt:ansistring;
      res1,res2:ansistring;
  begin
    t:=now;
    txt:=randomString(problemSize);
    res1:=AnsiReplaceStr(AnsiReplaceStr(txt, 'a', ' '), 'b ', 'c');
    write((now-t)*24*60*60:0:3,' ');
    t:=now;
    res2:=splitReplace(splitReplace(txt, 'a', ' '), 'b ', 'c');
    writeln((now-t)*24*60*60:0:3,' ',problemSize,' ',res1=res2);
  end;

VAR k:longint=1;
begin
  while k<100000000 do begin
    measureCost(k);
    inc(k,k);
  end;
end.
