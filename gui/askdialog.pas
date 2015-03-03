UNIT askDialog;

{$mode objfpc}{$H+}

INTERFACE

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, mnh_funcs, mnh_litVar, mnh_tokloc, mnh_constants, mnh_out_adapters;
type

  { TaskForm }

  TaskForm = class(TForm)
    ComboBox1: TComboBox;
    PROCEDURE ComboBox1KeyDown(Sender: TObject; VAR Key: word; Shift: TShiftState);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
  private
    rejectNonmatchingInput: boolean;
    { private declarations }
  public
    { public declarations }
    ownerThread: TThreadID;
    displayPending: boolean;
    lastAnswer: ansistring;
    PROCEDURE initWithQuestion(CONST question: ansistring);
    PROCEDURE initWithQuestionAndOptions(CONST question: ansistring; CONST options: array of ansistring);
    PROCEDURE lock;
    FUNCTION getLastAnswerReleasing: ansistring;
  end;

VAR
  askForm: TaskForm;

IMPLEMENTATION

{$R *.lfm}

{ TaskForm }

PROCEDURE TaskForm.ComboBox1KeyDown(Sender: TObject; VAR Key: word; Shift: TShiftState);
  begin
    if (key = 13) and (not(rejectNonmatchingInput) or (ComboBox1.ItemIndex>=0)) then
      begin
      lastAnswer := ComboBox1.Text;
      ModalResult := mrOk;
      end;
  end;

PROCEDURE TaskForm.FormCreate(Sender: TObject);
  begin
    ownerThread := 0;
  end;

PROCEDURE TaskForm.FormShow(Sender: TObject);
  begin
    displayPending := false;
  end;

PROCEDURE TaskForm.initWithQuestion(CONST question: ansistring);
  begin
    lock;
    rejectNonmatchingInput := false;
    lastAnswer := '';
    ComboBox1.Items.Clear;
    Caption := question;
    ComboBox1.AutoComplete := false;
    ComboBox1.Text := '';
    displayPending := true;
  end;

PROCEDURE TaskForm.initWithQuestionAndOptions(CONST question: ansistring; CONST options: array of ansistring);
  VAR i: longint;
  begin
    lock;
    rejectNonmatchingInput := true;
    lastAnswer := '';
    ComboBox1.Items.Clear;
    Caption := question;
    if length(options) = 0 then exit;
    for i := 0 to length(options)-1 do ComboBox1.Items.Add(options [i]);
    ComboBox1.AutoComplete := true;
    ComboBox1.Text := '';
    displayPending := true;
  end;

PROCEDURE TaskForm.lock;
  begin
    while Showing or (ownerThread<>0) do sleep(1);
    ownerThread := ThreadID;
  end;

FUNCTION TaskForm.getLastAnswerReleasing: ansistring;
  begin
    while displayPending or showing do sleep(1);
    result := lastAnswer;
    ownerThread := 0;
  end;

FUNCTION ask_impl(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
  VAR opt: array of ansistring;
    i: longint;
  begin
    if threadId<>MainThread then
      begin
      raiseError(el3_evalError,
        'I/O functions (fileContents in this case) may only be called from the main thread',
        tokenLocation);
      exit(nil);
      end;
    result := nil;
    if (params<>nil) and (params^.size = 1) and
      (params^.value(0)^.literalType = lt_string) then
      begin
      askForm.initWithQuestion(P_stringLiteral(params^.value(0))^.value);
      result := newStringLiteral(askForm.getLastAnswerReleasing);
      end
    else if (params<>nil) and (params^.size = 2) and
      (params^.value(0)^.literalType = lt_string) and
      (params^.value(1)^.literalType = lt_stringList) then
      begin
      setLength(opt, P_listLiteral(params^.value(1))^.size);
      for i := 0 to length(opt)-1 do
        opt[i] := P_stringLiteral(P_listLiteral(params^.value(1))^.value(i))^.value;
      askForm.initWithQuestionAndOptions(P_stringLiteral(params^.value(0))^.value, opt);
      result := newStringLiteral(askForm.getLastAnswerReleasing);


      end
    else raiseNotApplicableError('ask', params, tokenLocation);
  end;

INITIALIZATION
  registerRule('ask', @ask_impl,false,
    'ask(q:string);#Asks the user question q and returns the user input#'+
    'ask(q:string,options:stringList);#Asks the user question q, giving the passed options and returns the chosen option');

end.
