UNIT mnh_stringutil;

INTERFACE

CONST
  C_lineBreakChar = chr(13);
  C_carriageReturnChar = chr(10);
  C_tabChar = chr(9);
  BLANK_TEXT = '';

FUNCTION formatTabs(s: ansistring): ansistring;
FUNCTION isBlank(CONST s: ansistring): boolean;
FUNCTION replaceAll(original, lookFor, replaceBy: ansistring): ansistring; inline;
FUNCTION replaceRecursively(CONST original, lookFor, replaceBy: ansistring;
  OUT isValid: boolean): ansistring; inline;
FUNCTION replaceOne(CONST original, lookFor, replaceBy: ansistring): ansistring; inline;
FUNCTION escapeString(CONST s: ansistring): ansistring;
FUNCTION unescapeString(CONST input: ansistring; OUT parsedLength: longint): ansistring;
FUNCTION isIdentifier(CONST s: ansistring; CONST allowDot: boolean): boolean;
FUNCTION startsWith(CONST input, head: ansistring): boolean;

IMPLEMENTATION

FUNCTION formatTabs(s: ansistring): ansistring;
  VAR
    matrix: array of array of ansistring;
    pb, pt, i, j, maxJ, maxLength, dotpos: longint;

  FUNCTION isNumeric(s: ansistring): boolean;
    VAR
      i: longint;
      hasDot, hasExpo: boolean;
    begin
      result := length(s) > 0;
      hasDot := False;
      hasExpo := False;
      for i := 1 to length(s) - 1 do
        if s[i] = '.' then
          begin
          result := result and not (hasDot);
          hasDot := True;
          end
        else if (s[i] in ['e', 'E']) and (i > 1) then
            begin
            result := result and not (hasExpo);
            hasExpo := True;
            end
          else
            result := result and ((s[i] in ['0'..'9']) or
              ((i = 1) or (s[i - 1] in ['e', 'E'])) and (s[i] in ['-', '+']));
    end;

  FUNCTION posOfDot(s: ansistring): longint;
    begin
      result := pos('.', s);
      if result = 0 then
        result := length(s) + 1;
    end;

  begin
    pb := pos(C_lineBreakChar, s);
    pt := pos(C_tabChar, s);
    if pt <= 0 then
      result := s//s contains no tabs -> no processing needed

    else if pb <= 0 then
        begin
        //s contains no line break but tabs -> replaceOne all tabs by spaces;
        while pt > 0 do
          begin
          s[pt] := ' ';
          pt := pos(C_tabChar, s);
          end;
        result := s;
        end
      else
        begin
        //s contains both line breaks and tabs -> full processing
        setLength(matrix, 1);
        i := 0;
        j := -1;
        maxJ := -1;
        repeat
          pb := pos(C_lineBreakChar, s);
          if pb <= 0 then
            pb := length(s) + 1;
          pt := pos(C_tabChar, s);
          if pt <= 0 then
            pt := length(s) + 1;
          if pt < pb then
            begin
            //if tab comes before break, split string at tab and add new column
            Inc(j);
            if j > maxJ then
              maxJ := j;
            setLength(matrix[i], j + 1);
            matrix[i][j] := copy(s, 1, pt - 1);
            s := copy(s, pt + 1, length(s));
            pb := pos(C_lineBreakChar, s);
            if pb <= 0 then
              pb := length(s) + 1;
            pt := pos(C_tabChar, s);
            if pt <= 0 then
              pt := length(s) + 1;
            end
          else if pb < pt then
              begin
              //if break comes before tab, add rest to current row and start new row
              Inc(j);
              if j > maxJ then
                maxJ := j;
              setLength(matrix[i], j + 1);
              matrix[i][j] := copy(s, 1, pb - 1);
              s := copy(s, pb + 1, length(s));
              Inc(i);
              j := -1;
              setLength(matrix, i + 1);
              pb := pos(C_lineBreakChar, s);
              if pb <= 0 then
                pb := length(s) + 1;
              pt := pos(C_tabChar, s);
              if pt <= 0 then
                pt := length(s) + 1;
              end
            else
              begin
              //if break and tab are at equal position (meaning none are present), add rest to current row
              Inc(j);
              if j > maxJ then
                maxJ := j;
              setLength(matrix[i], j + 1);
              matrix[i][j] := s;
              s := '';
              end;
        until s = '';
        //expand columns to equal size:
        for j := 0 to maxJ do
          begin
          dotpos := 0;
          for i := 0 to length(matrix) - 1 do
            if (length(matrix[i]) > j) and (isNumeric(matrix[i][j])) and
              (posOfDot(matrix[i][j]) > dotpos) then
              dotpos := posOfDot(matrix[i][j]);
          if dotPos > 0 then
            for i := 0 to length(matrix) - 1 do
              if (length(matrix[i]) > j) and (isNumeric(matrix[i][j])) then
                while posOfDot(matrix[i][j]) < dotpos do
                  matrix[i][j] := ' ' + matrix[i][j];

          maxLength := 0;
          for i := 0 to length(matrix) - 1 do
            if (length(matrix[i]) > j) and (length(matrix[i][j]) > maxLength) then
              maxLength := length(matrix[i][j]);
          for i := 0 to length(matrix) - 1 do
            if (length(matrix[i]) > j) then
              while length(matrix[i][j]) <= maxLength do
                matrix[i][j] := matrix[i][j] + ' ';
          end;
        //join matrix to result;
        result := '';
        for i := 0 to length(matrix) - 1 do
          begin
          if (i > 0) then
            result := result + C_lineBreakChar;
          for j := 0 to length(matrix[i]) - 1 do
            result := result + matrix[i][j];
          end;
        end;
  end;

FUNCTION isBlank(CONST s: ansistring): boolean;
  VAR
    i: longint;
  begin
    result := True;
    for i := 1 to length(s) do
      if not (s[i] in [C_lineBreakChar, C_carriageReturnChar, C_tabChar, ' ']) then
        exit(False);
  end;

FUNCTION replaceOne(CONST original, lookFor, replaceBy: ansistring): ansistring; inline;
  VAR
    p: longint;
  begin
    p := pos(lookFor, original);
    if p > 0 then
      result := copy(original, 1, p - 1) + replaceBy +
        copy(original, p + length(lookFor), length(original))
    else
      result := original;
  end;

FUNCTION replaceRecursively(CONST original, lookFor, replaceBy: ansistring;
  OUT isValid: boolean): ansistring; inline;
  VAR
    p: longint;
  begin
    if pos(lookFor, replaceBy) > 0 then
      begin
      isValid := False;
      exit(replaceAll(original, lookFor, replaceBy));
      end
    else
      isValid := True;
    result := original;
    p := pos(lookFor, result);
    while p > 0 do
      begin
      result := copy(result, 1, p - 1) + replaceBy + copy(result, p +
        length(lookFor), length(result));
      p := pos(lookFor, result);
      end;
  end;

FUNCTION replaceAll(original, lookFor, replaceBy: ansistring): ansistring; inline;
  VAR
    p: longint;
  begin
    result := '';
    p := pos(lookFor, original);
    while p > 0 do
      begin
      result := result + copy(original, 1, p - 1) + replaceBy;
      original := copy(original, p + length(lookFor), length(original));
      p := pos(lookFor, original);
      end;
    result := result + original;
  end;

FUNCTION escapeString(CONST s: ansistring): ansistring;
  begin
    result := '"' + replaceAll(replaceAll(
      replaceAll(replaceAll(replaceAll(s, '\', '\\'), C_tabChar, '\t'),
      C_lineBreakChar, '\n'), C_carriageReturnChar, '\r'), '"', '\"') + '"';
  end;

FUNCTION unescapeString(CONST input: ansistring; OUT parsedLength: longint): ansistring;
  VAR
    i: longint;
    doubleQuoted: boolean;
  begin
    if (length(input) >= 1) and (input[1] in ['"', '''']) then
      begin
      result := '';
      doubleQuoted := input[1] = '"';
      i := 2;
      while (i <= length(input)) and (doubleQuoted and (input[i] <> '"') or
          not (doubleQuoted) and (input[i] <> '''')) do
        if (input[i] = '\') THEN
          BEGIN
          IF i < length(input) THEN
            CASE input[i + 1] OF
              '\': BEGIN
                Result := Result + '\';
                Inc(i, 2);
                END;
              't': BEGIN
                Result := Result + C_tabChar;
                Inc(i, 2);
                END;
              'n': BEGIN
                Result := Result + C_lineBreakChar;
                Inc(i, 2);
                END;
              'r': BEGIN
                Result := Result + C_carriageReturnChar;
                Inc(i, 2);
                END;
              '"': IF doubleQuoted THEN
                  BEGIN
                  Result := Result + '"';
                  Inc(i, 2);
                  END
                ELSE
                  BEGIN
                  parsedLength := 0;
                  exit('');
                  END;
              '''': IF not (doubleQuoted) THEN
                  BEGIN
                  Result := Result + '''';
                  Inc(i, 2);
                  END
                ELSE
                  BEGIN
                  parsedLength := 0;
                  exit('');
                  END;
              ELSE BEGIN
                parsedLength := 0;
                exit('');
                END;
              END
          ELSE
            i := length(input) + 1;
          END
        ELSE
          BEGIN
            if input[i] in [C_carriageReturnChar,C_lineBreakChar,C_tabChar] then begin
              parsedLength := 0;
              exit('');
            end;
          Result := Result + input[i];
          Inc(i);
          END;
      parsedLength := i;
      IF i > length(input) THEN
        parsedLength := 0;
      END
    ELSE
      BEGIN
      parsedLength := 0;
      Result := '';
      END;
  END;

FUNCTION isIdentifier(CONST s: ansistring; CONST allowDot: boolean): boolean;
  VAR
    i: longint;
    dotAllowed: boolean;
  BEGIN
    dotAllowed := allowDot;
    Result := (length(s) >= 1) and (s[1] in ['a'..'z', 'A'..'Z']);
    i := 2;
    WHILE Result and (i <= length(s)) DO
      IF (s[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) THEN
        Inc(i)
      ELSE IF (s[i] = '.') and dotAllowed then
          begin
          Inc(i);
          dotAllowed := False;
          end
        else
          result := False;
  end;

FUNCTION startsWith(CONST input, head: ansistring): boolean;
  begin
    result := copy(input, 1, length(head)) = head;
  end;




















end.
