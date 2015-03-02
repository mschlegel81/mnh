PROGRAM recMe;

FUNCTION f(CONST i:longint):longint;
  begin
    if i>0 then result:=f(i-1)
           else result:=0;
  end;

 VAR j:longint;
begin
  j:=1;
  repeat
    write(j,' '); writeln(f(j));
    j:=j*2;
  until false;
end.