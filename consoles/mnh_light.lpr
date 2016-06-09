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

{$ifdef WINDOWS}{$MAXSTACKSIZE 100000000}{$endif}
PROGRAM mnh_light;
USES {$ifdef UNIX}cmem, cthreads,{$endif}
     {$ifdef DEBUGMODE}heaptrc,{$endif}
     mnh_constants,myGenerics,mnh_cmdLineInterpretation, mnh_packages, mnh_contexts, sysutils,mnh_out_adapters, mnh_fileWrappers;

PROCEDURE interactiveMode;
  VAR hasExitSignal:boolean=false;
      consolePackage:T_package;
  PROCEDURE readInputFromConsole;
    VAR nextInput:ansistring;
    begin
      write('>'); readln(nextInput);
      nextInput:=trim(nextInput);
      if uppercase(nextInput)='EXIT' then begin
        hasExitSignal:=true;
        exit;
      end else if nextInput='\' then consolePackage.clearSource
      else consolePackage.appendSource(nextInput);
    end;

  VAR i:longint;
      context:T_evaluationContext;
  begin
    consolePackage.create(nil);
    for i:=0 to length(LOGO)-1 do writeln(LOGO[i]);
    writeln;
    writeln('No command line parameters were given. You are in interactive mode.');
    writeln('Type "exit" (case insensitive) to quit.');
    writeln('Type \ to clear and restart.');
    context.createNormalContext(P_adapters(@consoleAdapters));

    readInputFromConsole;
    while not(hasExitSignal) do begin
      consolePackage.load(lu_interactiveMode,context,C_EMPTY_STRING_ARRAY);
      readInputFromConsole;
    end;
    context.destroy;
    consolePackage.destroy;
  end;

begin
  parseCmdLine;
  interactiveMode;
end.

