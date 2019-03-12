PROGRAM ide;

{$mode objfpc}{$H+}
{$apptype console} // to read debug output on windows

USES
  {$ifdef UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$endif}{$endif}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, ideMain, serializationUtil, myGenerics,
  myStringUtil, editorForm, editorMeta, outlines, codeAssistance;

{$R *.res}

begin
  Application.initialize;
  Application.CreateForm(TIdeMainForm, IdeMainForm);
  Application.CreateForm(TeditForm, editForm);
  Application.CreateForm(TOutlineForm, OutlineForm);
  Application.CreateForm(TAssistanceForm, AssistanceForm);
  Application.run;
end.

