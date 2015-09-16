{$h+}
PROGRAM example92;

{ This program demonstrates the
  GetEnvironmentVariableCount FUNCTION }

USES sysutils;

VAR
  I : integer;

begin
  for I:=1 to GetEnvironmentVariableCount do
    writeln(i:3,' : ',GetEnvironmentString(i));
end.
