UNIT askDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, mnh_funcs, mnh_litVar, mnh_tokLoc, mnh_constants, mnh_out_adapters,myGenerics,mnh_contexts;
TYPE

  { TaskForm }

  TaskForm = class(TForm)
    ComboBox1: TComboBox;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    PROCEDURE ComboBox1KeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE Button1Click(Sender: TObject);
    PROCEDURE Button2Click(Sender: TObject);
    PROCEDURE Button3Click(Sender: TObject);
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
    PROCEDURE lock;
    FUNCTION getLastAnswerReleasing: ansistring;
    PROCEDURE setButtons(CONST enable:boolean; CONST count:byte);
  end;

VAR
  askForm: TaskForm;

FUNCTION ask_impl(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation; VAR context:T_evaluationContext): P_literal;
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

PROCEDURE TaskForm.Button1Click(Sender: TObject);
  begin
    lastAnswer := Button1.Caption;
    ModalResult := mrOk;
  end;

PROCEDURE TaskForm.Button2Click(Sender: TObject);
  begin
    lastAnswer := Button2.Caption;
    ModalResult := mrOk;
  end;

PROCEDURE TaskForm.Button3Click(Sender: TObject);
  begin
    lastAnswer := Button3.Caption;
    ModalResult := mrOk;
  end;

PROCEDURE TaskForm.initWithQuestion(CONST question: ansistring);
  VAR i:longint;
  begin
    lock;
    setButtons(false,0);
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
    setButtons(length(options)<=3,length(options));
    rejectNonmatchingInput := true;
    lastAnswer := '';
    ComboBox1.Items.clear;
    Caption := question;
    Label1.Caption:=question;
    if length(options) = 0 then exit;
    for i := 0 to length(options)-1 do ComboBox1.Items.add(options [i]);
    if length(options)>0 then Button1.Caption:=options[0];
    if length(options)>1 then Button2.Caption:=options[1];
    if length(options)>2 then button3.Caption:=options[2];
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

PROCEDURE TaskForm.setButtons(CONST enable: boolean; CONST count: byte);
  VAR h:longint=0;
  begin
    if enable then begin
      if count>=1 then begin Button1.Enabled:=true; Button1.visible:=true; end;
      if count>=2 then begin Button2.Enabled:=true; Button2.visible:=true; end;
      if count>=3 then begin Button3.Enabled:=true; button3.visible:=true; end;
      ComboBox1.Enabled:=false;
      ComboBox1.visible:=false;
    end else begin
      ComboBox1.Enabled:=true;
      ComboBox1.visible:=true;
      Button1.Enabled:=false; Button1.visible:=false;
      Button2.Enabled:=false; Button2.visible:=false;
      Button3.Enabled:=false; button3.visible:=false;
    end;
  end;

FUNCTION ask_impl(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation; VAR context:T_evaluationContext): P_literal;
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
