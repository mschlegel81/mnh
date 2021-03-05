UNIT consoleAsk;
INTERFACE
USES sysutils,
     myGenerics, myStringUtil,
     mnh_constants,basicTypes,
     out_adapters,
     litVar,
     contexts,recyclers,
     funcs,
     Keyboard;

PROCEDURE doneConsoleAsk;
PROCEDURE initConsoleAsk;
IMPLEMENTATION
USES mySys;
VAR keyboardIsUp:boolean=false;
    cs:TRTLCriticalSection;
PROCEDURE initConsoleAsk;
  begin
    enterCriticalSection(cs);
    if not(keyboardIsUp) then InitKeyboard;
    keyboardIsUp:=true;
    leaveCriticalSection(cs);
  end;

PROCEDURE doneConsoleAsk;
  begin
    enterCriticalSection(cs);
    if keyboardIsUp then DoneKeyboard;
    keyboardIsUp:=false;
    leaveCriticalSection(cs);
  end;

{$i func_defines.inc}
FUNCTION ask(CONST question: ansistring; CONST messages:P_messages): ansistring;
  begin
    threadStartsSleeping;
    initConsoleAsk;
    writeln(' ?> ', question);
    write(' !> ');
    while (messages^.continueEvaluation) and (PollKeyEvent=0) do sleep(1);
    doneConsoleAsk;
    if messages^.continueEvaluation
    then readln(result)
    else result:='';
    threadStopsSleeping;
  end;

FUNCTION ask(CONST question: ansistring; CONST options: T_arrayOfString; CONST messages:P_messages): ansistring;
  VAR i: longint=-1;
  FUNCTION stringIdx(s: string): longint;
    VAR j: longint;
    begin
      if length(options)=1 then exit(0);
      for j:=0 to length(options)-1 do if trim(options [j]) = trim(s) then exit(j);
      result:=strToIntDef(s, -1);
      if (result>=0) and (result<length(options)) and (result<=9)  then exit(result) else result:=-1;
      if (length(options)>=10) and (length(s)=1) and (ord(s[1])-ord('a')+10<length(options)) then exit(ord(s[1])-ord('a')+10);

      for j:=0 to length(options)-1 do if copy(options[j],1,length(s))=s then begin
        if result=-1 then result:=j else result:=-2;
      end;
      if result>=0 then exit(result);
      result:=-1;
      for j:=0 to length(options)-1 do if uppercase(copy(options[j],1,length(s)))=uppercase(s) then begin
        if result=-1 then result:=j else result:=-2;
      end;
    end;

  VAR questionLines:T_arrayOfString;
      keyEvent:TKeyEvent;
  begin
    if length(options) = 0 then exit('');
    threadStartsSleeping;
    initConsoleAsk;
    questionLines:=formatTabs(split(question));
    for i:=0 to length(questionLines)-1 do writeln(' ?> ',questionLines[i]);
    for i:=0 to length(options)-1 do if i<=9 then writeln('  [', i,                '] ', options [i])
                                             else writeln('  [',chr(ord('a')-10+i),'] ', options [i]);
    repeat
      write(' !> ');
      repeat
        while (messages^.continueEvaluation) and (PollKeyEvent=0) do sleep(1);
        if not(messages^.continueEvaluation) then begin
          doneConsoleAsk;
          threadStopsSleeping;
          exit('');
        end;
        keyEvent:=TranslateKeyEvent(GetKeyEvent);
      until (kbASCII=GetKeyEventFlags(keyEvent)) or not(messages^.continueEvaluation);
      if kbASCII=GetKeyEventFlags(keyEvent)
      then i:=stringIdx(KeyEventToString(keyEvent));
      if i<0 then writeln('Invalid anwer. Please give one of the options above.');
    until (i>=0) or not(messages^.continueEvaluation);
    doneConsoleAsk;
    threadStopsSleeping;
    result:=options[i];
  end;

FUNCTION ask_impl intFuncSignature;
  VAR opt: T_arrayOfString;
      iter:T_arrayOfLiteral;
      i: longint;
  begin
    if not(context.checkSideEffects('ask',tokenLocation,[se_input])) then exit(nil);
    result:=nil;
    if (params<>nil) and
       (params^.size = 1) and
       (arg0^.literalType = lt_string)
    then result:=newStringLiteral(ask(str0^.value,context.messages))
    else if (params<>nil) and (params^.size = 2) and
            (arg0^.literalType = lt_string) and
            (arg1^.literalType = lt_stringList) then begin
      system.enterCriticalSection(cs);
      try
        setLength(opt, list1^.size);
        iter:=list1^.iteratableList;
        setLength(opt,length(iter));
        for i:=0 to length(opt)-1 do opt[i]:=P_stringLiteral(iter[i])^.value;
        disposeLiteral(iter);
        result:=newStringLiteral(ask(str0^.value, opt,context.messages));
      finally
        system.leaveCriticalSection(cs);
      end;
    end;
  end;

INITIALIZATION
  {$WARN 5058 OFF}
  system.initCriticalSection(cs);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE, 'ask', @ask_impl,ak_variadic_1{$ifdef fullVersion},
               'ask(q:string);//Asks the user question q and returns the user input#'+
               'ask(q:string,options:stringList);//Asks the user question q, giving the passed options and returns the chosen option'{$endif},[se_input]);
FINALIZATION;
  system.doneCriticalSection(cs);
end.
