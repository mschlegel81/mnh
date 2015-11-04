UNIT askDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, mnh_funcs, mnh_litVar, mnh_tokLoc, mnh_constants, mnh_out_adapters,myGenerics;
TYPE

  { TaskForm }

  TaskForm = class(TForm)
    ComboBox1: TComboBox;
    Label1: TLabel;
    PROCEDURE ComboBox1KeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
  private
    rejectNonmatchingInput: boolean;
    previousAnswers:array[0..31] of ansistring;
    { private declarations }
  public
    { public declarations }
    ownerThread: TThreadID;
    displayPending: boolean;
    lastAnswer: ansistring;
    PROCEDURE initWithQuestion(CONST question: ansistring);
    PROCEDURE initWithQuestionAndOptions(CONST question: ansistring; CONST options: T_arrayOfString);
    PROCEDURE initWithFileLines(CONST minIdx,maxIdx:longint);
    PROCEDURE lock;
    FUNCTION getLastAnswerReleasing: ansistring;
  end;

VAR
  askForm: TaskForm;

FUNCTION ask_impl(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
IMPLEMENTATION
VAR cs:TRTLCriticalSection;
{$R *.lfm}

{ TaskForm }

PROCEDURE TaskForm.ComboBox1KeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if key=27 then ModalResult:=mrCancel;
    if (key = 13) and (not(rejectNonmatchingInput) or (ComboBox1.ItemIndex>=0)) then begin
      lastAnswer := ComboBox1.text;
      ModalResult := mrOk;
    end;
  end;

PROCEDURE TaskForm.FormCreate(Sender: TObject);
  VAR i:longint;
  begin
    ownerThread := 0;
    for i:=0 to length(previousAnswers)-1 do previousAnswers[i]:='';
  end;

PROCEDURE TaskForm.FormShow(Sender: TObject);
  begin
    displayPending := false;
  end;

PROCEDURE TaskForm.initWithQuestion(CONST question: ansistring);
  VAR i:longint;
  begin
    lock;
    rejectNonmatchingInput := false;
    lastAnswer := '';
    ComboBox1.Items.clear;
    for i:=0 to length(previousAnswers)-1 do if previousAnswers[i]<>'' then ComboBox1.Items.add(previousAnswers[i]);
    Caption := question;
    Label1.Caption:=question;
    ComboBox1.AutoComplete := false;
    ComboBox1.text := '';
    displayPending := true;
  end;

PROCEDURE TaskForm.initWithQuestionAndOptions(CONST question: ansistring; CONST options: T_arrayOfString);
  VAR i: longint;
  begin
    lock;
    rejectNonmatchingInput := true;
    lastAnswer := '';
    ComboBox1.Items.clear;
    Caption := question;
    Label1.Caption:=question;
    if length(options) = 0 then exit;
    for i := 0 to length(options)-1 do ComboBox1.Items.add(options [i]);
    ComboBox1.AutoComplete := true;
    ComboBox1.text := '';
    displayPending := true;
  end;

PROCEDURE TaskForm.initWithFileLines(CONST minIdx,maxIdx:longint);
  VAR i:longint;
  begin
    lock;
    rejectNonmatchingInput := true;
    lastAnswer := '';
    ComboBox1.Items.clear;
    Caption := 'Breakpoint at line No.';
    Label1.Caption := Caption;
    for i := minIdx to maxIdx do ComboBox1.Items.add(intToStr(i));
    ComboBox1.AutoComplete := true;
    ComboBox1.text := '';
    displayPending := true;
  end;

PROCEDURE TaskForm.lock;
  begin
    while showing or (ownerThread<>0) do sleep(1);
    ownerThread := ThreadID;
  end;

FUNCTION TaskForm.getLastAnswerReleasing: ansistring;
  VAR i:longint;
  begin
    while displayPending or showing do sleep(1);
    result := lastAnswer;
    for i:=length(previousAnswers)-1 downto 1 do previousAnswers[i]:=previousAnswers[i-1];
    previousAnswers[0]:=lastAnswer;
    ownerThread := 0;
  end;

FUNCTION ask_impl(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation): P_literal;
  VAR opt: T_arrayOfString;
      i: longint;
  begin
    result := nil;
    if (params<>nil) and (params^.size = 1) and
      (params^.value(0)^.literalType = lt_string) then begin
      system.enterCriticalSection(cs);
      askForm.initWithQuestion(P_stringLiteral(params^.value(0))^.value);
      result := newStringLiteral(askForm.getLastAnswerReleasing);
      system.leaveCriticalSection(cs);
    end
    else if (params<>nil) and (params^.size = 2) and
      (params^.value(0)^.literalType = lt_string) and
      (params^.value(1)^.literalType = lt_stringList) then begin
      system.enterCriticalSection(cs);
      setLength(opt, P_listLiteral(params^.value(1))^.size);
      for i := 0 to length(opt)-1 do
        opt[i] := P_stringLiteral(P_listLiteral(params^.value(1))^.value(i))^.value;
      askForm.initWithQuestionAndOptions(P_stringLiteral(params^.value(0))^.value, opt);
      result := newStringLiteral(askForm.getLastAnswerReleasing);
      system.leaveCriticalSection(cs);
    end;
  end;

INITIALIZATION
  system.initCriticalSection(cs);

FINALIZATION
  system.doneCriticalSection(cs);


end.
