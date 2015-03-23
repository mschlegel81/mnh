PROGRAM testSys;
USES mySys;
VAR i:longint;
begin
  for i:=0 to length(getEnvironment)-1 do writeln(getEnvironment[i]);
  writeln(CMD_PATH.value);
  writeln(SEVEN_ZIP_PATH.value);
  writeln(NOTEPAD_PATH.value);
end.