UNIT askDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Menus, funcs, litVar, basicTypes,
  mnh_constants, out_adapters, myGenerics, contexts,recyclers;
TYPE
  {$WARN 5024 OFF}
  TaskForm = class(TForm)
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    MainMenu1: TMainMenu;
    miPickFile: TMenuItem;
    miPickDirectory: TMenuItem;
    OpenDialog1: TOpenDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    PROCEDURE ComboBox1KeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE ButtonClick(Sender: TObject);
    PROCEDURE miPickDirectoryClick(Sender: TObject);
    PROCEDURE miPickFileClick(Sender: TObject);
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
    FUNCTION getLastAnswerReleasing(CONST adapters:P_messages): ansistring;
    PROCEDURE setButtons(CONST enable:boolean; CONST options: T_arrayOfString);
  end;

{$i func_defines.inc}

PROCEDURE initAskForm;
FUNCTION ask_impl intFuncSignature;
VAR askForm:TaskForm;
IMPLEMENTATION
USES consoleAsk,mySys;
VAR cs:TRTLCriticalSection;

{$R *.lfm}
PROCEDURE TaskForm.ComboBox1KeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if key=27 then begin
      ModalResult:=mrCancel;
      Hide;
    end;
    if (key = 13) and (not(rejectNonmatchingInput) or (ComboBox1.ItemIndex>=0)) then begin
      lastAnswer := ComboBox1.text;
      ModalResult := mrOk;
      Hide;
    end;
  end;

PROCEDURE TaskForm.FormCreate(Sender: TObject);
  VAR i:longint;
  begin
    ownerThread := 0;
    for i:=0 to length(previousAnswers)-1 do previousAnswers[i]:='';
  end;

PROCEDURE TaskForm.FormKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if key=27 then begin
      ModalResult:=mrCancel;
      Hide;
    end;
  end;

PROCEDURE TaskForm.FormShow(Sender: TObject);
  begin
    displayPending := false;
    position:=poDefault;
    AutoSize:=false;
    AutoSize:=true;
  end;

PROCEDURE TaskForm.ButtonClick(Sender: TObject);
  begin
    lastAnswer := TButton(Sender).caption;
    ModalResult := mrOk;
    Hide;
  end;

PROCEDURE TaskForm.miPickDirectoryClick(Sender: TObject);
  begin
    if SelectDirectoryDialog1.execute then ComboBox1.text:=SelectDirectoryDialog1.fileName;
  end;

PROCEDURE TaskForm.miPickFileClick(Sender: TObject);
  begin
    if OpenDialog1.execute then ComboBox1.text:=OpenDialog1.fileName;
  end;

PROCEDURE TaskForm.initWithQuestion(CONST question: ansistring);
  VAR i:longint;
  begin
    lock;
    setButtons(false,C_EMPTY_STRING_ARRAY);
    rejectNonmatchingInput := false;
    lastAnswer := '';
    ComboBox1.items.clear;
    for i:=0 to length(previousAnswers)-1 do if previousAnswers[i]<>'' then ComboBox1.items.add(previousAnswers[i]);
    caption := question;
    Label1.caption:=question;
    ComboBox1.AutoComplete := false;
    ComboBox1.text := '';
    displayPending := true;
  end;

PROCEDURE TaskForm.initWithQuestionAndOptions(CONST question: ansistring; CONST options: T_arrayOfString);
  VAR i: longint;
  begin
    lock;
    setButtons(length(options)<=16,options);
    rejectNonmatchingInput := true;
    lastAnswer := '';
    ComboBox1.items.clear;
    caption := question;
    Label1.caption:=question;
    if length(options) = 0 then exit;
    for i := 0 to length(options)-1 do ComboBox1.items.add(options [i]);
    if length(options)>0 then Button1.caption:=options[0];
    if length(options)>1 then Button2.caption:=options[1];
    if length(options)>2 then Button3.caption:=options[2];
    ComboBox1.AutoComplete := true;
    ComboBox1.text := '';
    displayPending := true;
  end;

PROCEDURE TaskForm.lock;
  begin
    while showing or (ownerThread<>0) do sleep(1);
    ownerThread := ThreadID;
  end;

FUNCTION TaskForm.getLastAnswerReleasing(CONST adapters:P_messages): ansistring;
  VAR i:longint;
  begin
    while displayPending or showing do begin
      sleep(10);
      if (adapters<>nil) and not(adapters^.continueEvaluation) then begin
        displayPending:=false;
        ModalResult:=0;
        Hide;
      end;
    end;
    result := lastAnswer;
    for i:=length(previousAnswers)-1 downto 1 do previousAnswers[i]:=previousAnswers[i-1];
    previousAnswers[0]:=lastAnswer;
    ownerThread := 0;
  end;

PROCEDURE TaskForm.setButtons(CONST enable: boolean; CONST options: T_arrayOfString);
  FUNCTION button(CONST index:byte):TButton;
    begin
      result:=nil;
      case index of
        0: result:=Button1;
        1: result:=Button2;
        2: result:=Button3;
        3: result:=Button4;
        4: result:=Button5;
        5: result:=Button6;
        6: result:=Button7;
        7: result:=Button8;
        8: result:=Button9;
        9: result:=Button10;
       10: result:=Button11;
       11: result:=Button12;
       12: result:=Button13;
       13: result:=Button14;
       14: result:=Button15;
       15: result:=Button16;
      end;
    end;

  VAR i:longint;
  begin
    if enable then begin
      ComboBox1.enabled:=false;
      ComboBox1.visible:=false;
      for i:=0 to 15 do begin
        button(i).enabled:=length(options)>i;
        button(i).visible:=length(options)>i;
        button(i).OnKeyDown:=@FormKeyDown;
        if length(options)>i then begin
          button(i).caption:=options[i];
        end;
      end;
    end else begin
      ComboBox1.enabled:=true;
      ComboBox1.visible:=true;
      for i:=0 to 15 do begin
        button(i).enabled:=false;
        button(i).visible:=false;
      end;
    end;
    AutoSize:=false;
    AutoSize:=true;
  end;

PROCEDURE initAskForm;
  begin
    doneConsoleAsk;
    askForm:=TaskForm.create(nil);
    builtinFunctionMap.reregisterRule(SYSTEM_BUILTIN_NAMESPACE,'ask',@askDialog.ask_impl);
    initialize(cs);
    system.initCriticalSection(cs);
  end;

FUNCTION ask_impl intFuncSignature;
  VAR opt: T_arrayOfString;
      i: longint;
  begin
    if not(context.checkSideEffects('ask',tokenLocation,[se_input])) then exit(nil);
    threadStartsSleeping;
    result := nil;
    if (params<>nil) and (params^.size = 1) and
      (arg0^.literalType = lt_string) then begin
      system.enterCriticalSection(cs);
      try
        askForm.initWithQuestion(str0^.value);
        result := literalRecycler.newStringLiteral(askForm.getLastAnswerReleasing(context.messages));
      finally
        system.leaveCriticalSection(cs);
      end;
    end
    else if (params<>nil) and (params^.size = 2) and
      (arg0^.literalType = lt_string) and
      (arg1^.literalType = lt_stringList) then begin
      system.enterCriticalSection(cs);
      try
        setLength(opt, list1^.size);
        for i := 0 to length(opt)-1 do
          opt[i] := P_stringLiteral(list1^.value[i])^.value;
        askForm.initWithQuestionAndOptions(str0^.value, opt);
        result := literalRecycler.newStringLiteral(askForm.getLastAnswerReleasing(context.messages));
      finally
        system.leaveCriticalSection(cs);
      end;
    end;
    threadStopsSleeping;
  end;

FINALIZATION
  if askForm<>nil then begin
    FreeAndNil(askForm);
    system.doneCriticalSection(cs);
  end;
end.
