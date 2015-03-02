PROGRAM testRead;
USES mnh_fileWrappers, mnh_stringUtil;
VAR accessOk:boolean=false;
begin
  if paramcount=1 then writeln(escapeString(fileContent(paramstr(1),accessOk)));
  writeln('accessOk= ',accessOk);
end.