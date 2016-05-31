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

{$ifdef include_interface}
  T_idStack=object
    ids:array of array of ansistring;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE clear;
    PROCEDURE scopePush;
    PROCEDURE scopePop;
    FUNCTION oneAboveBottom:boolean;
    FUNCTION scopeBottom:boolean;
    PROCEDURE addId(CONST id:ansistring);
    FUNCTION hasId(CONST id:ansistring):boolean;
  end;
{$endif}
{$ifdef include_implementation}
CONSTRUCTOR T_idStack.create;
  begin
    setLength(ids,0);
  end;

DESTRUCTOR T_idStack.destroy;
  begin clear;  end;

PROCEDURE T_idStack.clear;
  begin
    while not(scopeBottom) do scopePop;
  end;

PROCEDURE T_idStack.scopePush;
  begin
    setLength(ids,length(ids)+1);
  end;

PROCEDURE T_idStack.scopePop;
  begin
    setLength(ids,length(ids)-1);
  end;

FUNCTION T_idStack.oneAboveBottom:boolean;
  begin
    result:=length(ids)=1;
  end;

FUNCTION T_idStack.scopeBottom:boolean;
  begin
    result:=length(ids)=0;
  end;

PROCEDURE T_idStack.addId(CONST id:ansistring);
  VAR i,j:longint;
  begin
    i:=length(ids)-1;
    j:=length(ids[i]);
    setLength(ids[i],j+1);
    ids[i,j]:=id;
  end;

FUNCTION T_idStack.hasId(CONST id:ansistring):boolean;
  VAR i,j:longint;
  begin
    result:=false;
    for i:=length(ids)-1 downto 0 do for j:=0 to length(ids[i])-1 do if ids[i,j]=id then exit(true);
  end;
{$endif}
