PROGRAM ide;

{$mode objfpc}{$H+}
{$apptype console} // to read debug output on windows

USES
  {$ifdef UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$endif}{$endif}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, ideMain, evalThread, guiOutAdapters,
  editorMetaBase, fileWrappers,
  contexts, out_adapters, codeAssistance;

{$R *.res}

begin
  Application.initialize;
  Application.CreateForm(TIdeMainForm, IdeMainForm);
  Application.run;
end.

