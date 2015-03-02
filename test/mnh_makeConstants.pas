PROGRAM mnh_makeConstants;

USES
  mnh_constants,
  SysUtils;

  PROCEDURE tokList;
    TYPE
      info = record
        tokType: T_tokenType;
        tokTxt: ansistring;
        end;
    VAR
      t: T_tokenType;
      l: array of info;
      temp: info;
      i, j: longint;
    begin
      setLength(l, 0);
      for t := tt_literal to tt_eol do
        if C_tokenString[t] <> '' then
          begin
          setLength(l, length(l) + 1);
          with l[length(l) - 1] do
            begin
            tokType := t;
            tokTxt := trim(C_tokenString[t]);
            end;
          end;
      for i := 1 to length(l) - 1 do
        for j := 0 to i - 1 do
          if (l[i].tokTxt[1] > l[j].tokTxt[1]) or (l[i].tokTxt[1] = l[j].tokTxt[1]) and
            (length(l[i].tokTxt) > length(l[j].tokTxt)) then
            begin
            temp := l[i];
            l[i] := l[j];
            l[j] := temp;
            end;
      for i := 0 to length(l) - 1 do
        with l[i] do
          writeln('''', tokTxt[1], '''', ': ', '''', tokTxt, '''', ',', tokType);
    end;

begin
  tokList;
end.
