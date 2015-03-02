PROGRAM ulam;
USES mypics;

FUNCTION numberOfFactors(CONST n:longint):longint;
  VAR k:longint;
  begin
    k:=2;
    result:=1;
    while k*k<=n do begin
      while (n mod k)=0 do begin
        inc(result);
        n:=n div k;
      end;
      inc(k);
    end;
  end;
  
FUNCTION numberAt(x,y:longint):longint;
  begin
    
  end;