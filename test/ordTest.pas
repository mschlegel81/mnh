PROGRAM ordTest;
VAR f:text;
    o,i:longint;
    maxO:longint=0;
    line:string;

begin
  assign(f,paramStr(1));
  reset(f);
  while not(eof(f)) do begin
    readln(f,line);
    for i:=1 to length(line) do if ord(line[i])>maxO then begin
      maxO:=ord(line[i]);
      writeln('MAX ORD= ',maxO,' (',line[i],')');
    end;
  end;
  close(f);
end.
