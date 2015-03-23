{$h+}
PROGRAM example92;

{ This program demonstrates the
  GetEnvironmentVariableCount FUNCTION }

USES sysutils;

VAR
  I : Integer;

begin
  For I:=1 to GetEnvironmentVariableCount do
    Writeln(i:3,' : ',GetEnvironmentString(i));
end.
