// MIT License
//
// Copyright (c) 2016 Martin Schlegel
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

UNIT consoleAsk;
INTERFACE
USES mnh_funcs, sysutils, mnh_litVar, mnh_tokLoc, mnh_constants, mnh_out_adapters, myGenerics, myStringUtil,mnh_contexts;
IMPLEMENTATION
VAR cs:TRTLCriticalSection;
FUNCTION ask(CONST question: ansistring): ansistring;
  begin
    writeln(' ?> ', question);
    write(' !> '); readln(result);
  end;

FUNCTION ask(CONST question: ansistring; CONST options: T_arrayOfString): ansistring;
  VAR i: longint;
  FUNCTION stringIdx(s: string): longint;
    VAR j: longint;
    begin
      if length(options)=1 then exit(0);
      for j:=0 to length(options)-1 do if trim(options [j]) = trim(s) then exit(j);
      result:=strToIntDef(s, -1);
      if (result>=0) and (result<length(options)) then exit(result) else result:=-1;
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
  begin
    if length(options) = 0 then exit('');
    questionLines:=formatTabs(split(question));
    for i:=0 to length(questionLines)-1 do writeln(' ?> ',questionLines[i]);
    for i:=0 to length(options)-1 do writeln('  [', i, '] ', options [i]);
    repeat
      write(' !> '); readln(result);
      i:=stringIdx(result);
      if i<0 then writeln('Invalid anwer. Please give one of the options above.');
    until i>=0;
    result:=options[i];
  end;

FUNCTION ask_impl(CONST params: P_listLiteral; CONST tokenLocation: T_tokenLocation; VAR context:T_evaluationContext):  P_literal;
  VAR opt: T_arrayOfString;
      i: longint;
  begin
    result:=nil;
    if (params<>nil) and
       (params^.size = 1) and
       (params^.value(0)^.literalType = lt_string)
    then result:=newStringLiteral(ask(P_stringLiteral(params^.value(0))^.value))
    else if (params<>nil) and (params^.size = 2) and
            (params^.value(0)^.literalType = lt_string) and
            (params^.value(1)^.literalType = lt_stringList) then begin
      system.enterCriticalSection(cs);
      setLength(opt, P_listLiteral(params^.value(1))^.size);
      for i:=0 to length(opt)-1 do opt[i]:=P_stringLiteral(P_listLiteral(params^.value(1))^.value(i))^.value;
      result:=newStringLiteral(ask(P_stringLiteral(params^.value(0))^.value, opt));
      system.leaveCriticalSection(cs);
    end;
  end;

INITIALIZATION
  system.initCriticalSection(cs);
  registerRule(SYSTEM_BUILTIN_NAMESPACE, 'ask', @ask_impl, 'ask(q:string);#Asks the user question q and returns the user input#'+
    'ask(q:string,options:stringList);#Asks the user question q, giving the passed options and returns the chosen option');
FINALIZATION;
  system.doneCriticalSection(cs);
end.
