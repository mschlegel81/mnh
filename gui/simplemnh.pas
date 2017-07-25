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
  TSimpleMnhForm = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    SimpleMnhInputEdit: TSynEdit;
    SimpleMnhOutputEdit: TSynEdit;
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE SimpleMnhInputEditChange(Sender: TObject);
    PROCEDURE showModalFor(CONST meta:P_editorMeta);
  private
    inputHighlighter,
    outputHighlighter:TSynMnhSyn;
    blankCodeProvider:P_blankCodeProvider;
    package:P_package;
    completion:T_completionLogic;
    PROCEDURE setCursor(CONST c:TCursor);
  public
  end;

FUNCTION getSimpleMnh: TSimpleMnhForm;
FUNCTION isSimpleMnhShowing:boolean;
FUNCTION delayFlush:boolean;
IMPLEMENTATION
VAR SimpleMnhForm: TSimpleMnhForm=nil;
    interpretation:record
      criticalSection:TRTLCriticalSection;
      active,busy:boolean;
      request:T_arrayOfString;
      requestId,completedId:longint;
    end;

FUNCTION getSimpleMnh: TSimpleMnhForm;
  begin
    if SimpleMnhForm=nil then begin
      SimpleMnhForm:=TSimpleMnhForm.create(nil);
      with interpretation do begin
        active:=false;
        completedId:=maxLongint;
        requestId:=-1;
        initCriticalSection(criticalSection);
      end;
    end;
    result:=SimpleMnhForm;
  end;

FUNCTION delayFlush:boolean;
  begin
    with interpretation do begin
      enterCriticalSection(criticalSection);
      result:=busy;
      leaveCriticalSection(criticalSection);
      if not(result) then SimpleMnhForm.setCursor(crDefault);
    end;
  end;

FUNCTION isSimpleMnhShowing: boolean;
  begin
    result:=(SimpleMnhForm<>nil) and (SimpleMnhForm.showing);
  end;

{$R *.lfm}
FUNCTION interpretationLoop(p:pointer):ptrint;
  CONST idleTimeout=10/(24*60*60); //10 seconds
  VAR idleSince:double;
      input:T_arrayOfString;
      inputId:longint;

  FUNCTION hasRequest:boolean;
    VAR k:longint;
    begin
      with interpretation do begin
        enterCriticalSection(criticalSection);
        if completedId=requestId then result:=false
        else begin
          result:=true;
          setLength(input,length(request));
          for k:=0 to length(input)-1 do input[k]:=request[k];
          inputId:=requestId;
          busy:=true;
        end;
        leaveCriticalSection(criticalSection);
      end;
    end;

  VAR evalContext:T_evaluationContext;
  begin
    evalContext.create(@guiAdapters);
    with interpretation do begin
      idleSince:=now;
      while SimpleMnhForm.showing and (now-idleSince<idleTimeout) do if hasRequest then begin
        evalContext.resetForEvaluation(SimpleMnhForm.package,false,false,false);
        SimpleMnhForm.package^.interpretInPackage(input,evalContext.threadContext^);
        idleSince:=now;
        enterCriticalSection(criticalSection);
        completedId:=inputId;
        busy:=false;
        leaveCriticalSection(criticalSection);
      end else begin
        sleep(10);
      end;
      enterCriticalSection(criticalSection);
      active:=false;
      leaveCriticalSection(criticalSection);
    end;
    evalContext.destroy;
    result:=0;
  end;

PROCEDURE TSimpleMnhForm.SimpleMnhInputEditChange(Sender: TObject);
  VAR k:longint;
  begin
    if package=nil then raise Exception.create('There must be a package');
    with interpretation do begin
      enterCriticalSection(criticalSection);
      if not(active) then begin
        beginThread(@interpretationLoop);
        active:=true;
      end;
      setLength(request,SimpleMnhInputEdit.lines.count);
      for k:=0 to length(request)-1 do request[k]:=SimpleMnhInputEdit.lines[k];
      inc(requestId);
      setCursor(crHourGlass);
      if not(busy) then begin
        guiAdapters.clearAll;
        guiOutAdapter.flushClear;
      end;
      leaveCriticalSection(criticalSection);
    end;
  end;

PROCEDURE TSimpleMnhForm.showModalFor(CONST meta: P_editorMeta);
  begin
    meta^.setWorkingDir;
    meta^.assignAdditionalHighlighter(inputHighlighter);
    package:=runEvaluator.getPackageForPostEvaluation(meta);
    ShowModal;
  end;

PROCEDURE TSimpleMnhForm.setCursor(CONST c: TCursor);
  begin
    Cursor:=c;
    SimpleMnhInputEdit.Cursor:=c;
    SimpleMnhOutputEdit.Cursor:=c;
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
    new(blankCodeProvider,create);
    completion.create;
  end;

PROCEDURE TSimpleMnhForm.FormDestroy(Sender: TObject);
  begin
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
  if SimpleMnhForm<>nil then begin
    with interpretation do begin
      enterCriticalSection(criticalSection);
      while active do begin
        leaveCriticalSection(criticalSection);
        ThreadSwitch;
        sleep(10);
        enterCriticalSection(criticalSection);
      end;
      leaveCriticalSection(criticalSection);
      doneCriticalSection(criticalSection);
    end;
    FreeAndNil(SimpleMnhForm);
  end;

end.

