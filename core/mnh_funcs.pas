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

UNIT mnh_funcs;
INTERFACE
{$WARN 5024 OFF}
USES sysutils,myGenerics,mnh_constants,mnh_litVar,mnh_out_adapters,mnh_tokLoc,mnh_contexts,
     myStringUtil,Classes{$ifdef fullVersion},mnh_doc{$endif};
TYPE
  T_intFuncCallback=FUNCTION(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;

VAR
  intrinsicRuleMap:specialize G_stringKeyMap<T_intFuncCallback>;
  print_cs        :system.TRTLCriticalSection;

PROCEDURE registerRule(CONST namespace:T_namespace; CONST name:ansistring; CONST ptr:T_intFuncCallback; CONST explanation:ansistring; CONST fullNameOnly:boolean=false);
PROCEDURE raiseNotApplicableError(CONST functionName:ansistring; CONST typ:T_literalType; CONST messageTail:ansistring; CONST tokenLocation:T_tokenLocation; VAR adapters:T_adapters);
PROCEDURE setMnhParameters(CONST p:T_arrayOfString);
IMPLEMENTATION
VAR mnhParameters:P_listLiteral=nil;

PROCEDURE registerRule(CONST namespace: T_namespace; CONST name: ansistring; CONST ptr: T_intFuncCallback; CONST explanation: ansistring; CONST fullNameOnly: boolean);
  begin
    if not(fullNameOnly) then
    intrinsicRuleMap.put(                                                    name,ptr);
    intrinsicRuleMap.put(C_namespaceString[namespace]+C_ID_QUALIFY_CHARACTER+name,ptr);
    {$ifdef fullVersion}registerDoc(C_namespaceString[namespace]+C_ID_QUALIFY_CHARACTER+name,explanation,fullNameOnly);{$endif}
  end;

PROCEDURE raiseNotApplicableError(CONST functionName: ansistring; CONST typ: T_literalType; CONST messageTail: ansistring; CONST tokenLocation: T_tokenLocation; VAR adapters: T_adapters);
  VAR complaintText:ansistring;
  begin
    complaintText:='Built in function '+functionName+' cannot be applied to type '+C_typeString[typ]+messageTail;
    adapters.raiseError(complaintText,tokenLocation);
  end;

PROCEDURE setMnhParameters(CONST p: T_arrayOfString);
  VAR i:longint;
  begin
    if mnhParameters<>nil then disposeLiteral(mnhParameters);
    mnhParameters:=newListLiteral;
    for i:=0 to length(p)-1 do mnhParameters^.appendString(p[i]);
  end;

{$undef INNER_FORMATTING}

FUNCTION clearPrint_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    if (params<>nil) and (params^.size>0) then exit(nil);
    system.enterCriticalSection(print_cs);
    context.adapters^.clearPrint();
    system.leaveCriticalSection(print_cs);
    result:=newVoidLiteral;
  end;

FUNCTION print_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR stringToPrint:ansistring='';
      i:longint;
  begin
    if params<>nil then for i:=0 to params^.size-1 do case params^.value(i)^.literalType of
      lt_boolean,
      lt_int,
      lt_real,
      lt_string,
      lt_expression: stringToPrint:=stringToPrint + P_scalarLiteral(params^.value(i))^.stringForm;
      lt_list..lt_listWithError: stringToPrint:=stringToPrint + params^.value(i)^.toString;
    end;
    system.enterCriticalSection(print_cs);
    context.adapters^.printOut(formatTabs(split(stringToPrint)));
    system.leaveCriticalSection(print_cs);
    result:=newVoidLiteral;
  end;

FUNCTION type_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1)
    then exit(newStringLiteral(params^.value(0)^.typeString))
    else exit(newStringLiteral(params^.parameterListTypeString));
  end;

FUNCTION mnhParameters_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    if (params=nil) or (params^.size=0) then begin
      if mnhParameters=nil then mnhParameters:=newListLiteral;
      result:=mnhParameters;
      mnhParameters^.rereference;
    end else result:=nil;
  end;

INITIALIZATION
  intrinsicRuleMap.create;
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'clearPrint',@clearPrint_imp,'clearPrint(...);#Clears the output and returns void.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'print',@print_imp,'print(...);#Prints out the given parameters and returns void#if tabs and line breaks are part of the output, a default pretty-printing is used');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'type',@type_imp,'type(x);#Prints out the type of x');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'mnhParameters',@mnhParameters_imp,'mnhParameters;#Returns the command line parameters/switches passed on program startup');
  system.initCriticalSection(print_cs);
FINALIZATION
  if mnhParameters<>nil then disposeLiteral(mnhParameters);
  intrinsicRuleMap.destroy;
  system.doneCriticalSection(print_cs);

end.
