UNIT simpleMnh;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, SynEdit, SynCompletion, Forms, Controls,
  Graphics, Dialogs, StdCtrls, SynHighlighterMnh, mnh_contexts, mnh_packages,
  myGenerics,
  guiOutAdapters,
  mnh_fileWrappers,
  mnh_out_adapters,
  editorMeta,
  mnhCompletion,
  mnh_evalThread;

TYPE

  { TSimpleMnhForm }

  TSimpleMnhForm = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    SimpleMnhInputEdit: TSynEdit;
    SimpleMnhOutputEdit: TSynEdit;
    SynCompletion1: TSynCompletion;
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE SimpleMnhInputEditChange(Sender: TObject);
    PROCEDURE showModalFor(CONST meta:P_editorMeta);
  private
    inputHighlighter,
    outputHighlighter:TSynMnhSyn;
    evalContext:T_evaluationContext;
    blankCodeProvider:P_blankCodeProvider;
    package:P_package;
    completion:T_completionLogic;
    { private declarations }
  public
    { public declarations }
  end;

FUNCTION getSimpleMnh: TSimpleMnhForm;
FUNCTION isSimpleMnhShowing:boolean;
IMPLEMENTATION
VAR SimpleMnhForm: TSimpleMnhForm=nil;

FUNCTION getSimpleMnh: TSimpleMnhForm;
  begin
    if SimpleMnhForm=nil then SimpleMnhForm:=TSimpleMnhForm.create(nil);
    result:=SimpleMnhForm;
  end;

FUNCTION isSimpleMnhShowing: boolean;
  begin
    result:=(SimpleMnhForm<>nil) and (SimpleMnhForm.showing);
  end;

{$R *.lfm}

{ TSimpleMnhForm }

PROCEDURE TSimpleMnhForm.SimpleMnhInputEditChange(Sender: TObject);
  VAR input:T_arrayOfString;
      k:longint;
  begin
    if package=nil then raise Exception.create('There must be a package');
    guiOutAdapter.flushClear;
    guiAdapters.resetErrorFlags;
    SimpleMnhOutputEdit.clear;
    Cursor:=crHourGlass;
    evalContext.resetForEvaluation(package,false,false,false);
    setLength(input,SimpleMnhInputEdit.lines.count);
    for k:=0 to length(input)-1 do input[k]:=SimpleMnhInputEdit.lines[k];
    package^.interpretInPackage(input,evalContext.threadContext^);
    setLength(input,0);
    Cursor:=crDefault;
  end;

PROCEDURE TSimpleMnhForm.showModalFor(CONST meta:P_editorMeta);
  begin
    meta^.setWorkingDir;
    meta^.assignAdditionalHighlighter(inputHighlighter);
    package:=runEvaluator.getPackageForPostEvaluation(meta);
    ShowModal;
  end;

PROCEDURE TSimpleMnhForm.FormShow(Sender: TObject);
  begin
    if (package=nil) then begin
      new(package,create(blankCodeProvider,nil));
    end;
    SimpleMnhInputEdit.clear;
    SimpleMnhOutputEdit.clear;
    completion.assignEditor(SimpleMnhInputEdit,package);
    caption:=package^.getId;
  end;

PROCEDURE TSimpleMnhForm.FormCreate(Sender: TObject);
  begin
    inputHighlighter:=TSynMnhSyn.create(self,msf_input);
    SimpleMnhInputEdit.highlighter:=inputHighlighter;
    outputHighlighter:=TSynMnhSyn.create(self,msf_output);
    SimpleMnhOutputEdit.highlighter:=outputHighlighter;
    evalContext.create(@guiAdapters);
    new(blankCodeProvider,create);
    completion.create;
  end;

PROCEDURE TSimpleMnhForm.FormDestroy(Sender: TObject);
  begin
    evalContext.destroy;
    dispose(blankCodeProvider,destroy);
    completion.destroy;
  end;

PROCEDURE TSimpleMnhForm.FormClose(Sender: TObject;
  VAR CloseAction: TCloseAction);
  begin
    if package^.getCodeProvider=P_codeProvider(blankCodeProvider) then dispose(package,destroy);
    package:=nil;
  end;

FINALIZATION
  if SimpleMnhForm<>nil then FreeAndNil(SimpleMnhForm);

end.

