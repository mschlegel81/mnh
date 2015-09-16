PROGRAM testRead;
USES mnh_fileWrappers, mnh_stringUtil;
VAR accessOk:boolean=false;
begin
  if paramCount=1 then writeln(escapeString(fileContent(paramStr(1),accessOk)));
  writeln('accessOk= ',accessOk);
end.
