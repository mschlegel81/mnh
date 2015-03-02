UNIT consoleAsk;
INTERFACE
USES mnh_funcs,sysutils,mnh_litVar,mnh_tokloc,mnh_constants;

IMPLEMENTATION
FUNCTION ask(CONST question:ansistring):ansistring;
  begin
    writeln(' ?> ',question);
    write  (' !> '); readln(result);    
  end;
  
FUNCTION ask(CONST question:ansistring; CONST options:array of ansistring):ansistring;
  VAR i:longint;
  FUNCTION stringIdx(s:string):longint;
    VAR j:longint;
    begin
      for j:=0 to length(options)-1 do if trim(options[j])=trim(s) then exit(j);
      result:=strToIntDef(s,-1);
    end;
    
  begin
    if length(options)=0 then exit('');
    writeln(' ?> ',question);
    for i:=0 to length(options)-1 do writeln('  [',i,'] ',options[i]);    
    repeat
      write  (' !> '); readln(result);    
      i:=stringIdx(result);
      if i<0 then writeln('Invalid anwer. Please give one of the options above.');
    until i>=0;
    result:=options[i];
  end;
  
FUNCTION ask_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR opt:array of ansistring;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      result:=newStringLiteral(ask(P_stringLiteral(params^.value(0))^.value));
    end else if (params<>nil) and (params^.size=2) and (params^.value(0)^.literalType=lt_string) and (params^.value(1)^.literalType=lt_stringList) then begin
      setLength(opt,P_listLiteral(params^.value(1))^.size);
      for i:=0 to length(opt)-1 do opt[i]:=P_stringLiteral(P_listLiteral(params^.value(1))^.value(i))^.value;
      result:=newStringLiteral(ask(P_stringLiteral(params^.value(0))^.value,opt));
    end else raiseNotApplicableError('ask',params,tokenLocation);
  end;
  
INITIALIZATION
  registerRule('ask',@ask_impl,'ask(q:string);#Asks the user question q and returns the user input#'+
                               'ask(q:string,options:stringList);#Asks the user question q, giving the passed options and returns the chosen option');
end.