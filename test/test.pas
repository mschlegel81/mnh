PROGRAM test;
USES heaptrc,mnh_contexts, mnh_packages,mnh_out_adapters,myGenerics;

VAR package:T_package;
    adapters:T_adapters;
    context:T_evaluationContext;
begin
  if paramCount=0 then exit;
  adapters.create;
  adapters.addConsoleOutAdapter;
  adapters.minErrorLevel:=0;
  context.createNormalContext(@adapters);
  package.create(nil);
  package.setSourcePath(paramStr(1));
  package.load(lu_forCodeAssistance,context,C_EMPTY_STRING_ARRAY);

  package.destroy;
  context.destroy;
  adapters.destroy;
end.
