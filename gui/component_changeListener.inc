TYPE
  T_changeListeningMeta=object(T_guiElementMeta)
    config_onChange:P_expressionLiteral;
    state_changeTriggered:boolean;

    CONSTRUCTOR create(CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext; CONST consideredKeys:T_definingMapKeys);
    PROCEDURE triggerChange;
    FUNCTION evaluate(CONST location:T_tokenLocation; VAR context:T_threadContext):boolean; virtual;
    DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_changeListeningMeta.create(CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext; CONST consideredKeys:T_definingMapKeys);
  VAR tmp:P_literal;
  begin
    inherited create(def,location,context,consideredKeys+[dmk_onChange]);
    config_onChange:=nil;
    state_changeTriggered:=false;

    tmp:=mapGet(def,key[dmk_onChange]);
    if tmp<>nil then begin
      if (tmp^.literalType=lt_expression)
      then config_onChange:=P_expressionLiteral(tmp)
      else context.adapters^.raiseError('onChange is: '+tmp^.typeString+'; must be expression',location);
    end;
  end;
  
PROCEDURE T_changeListeningMeta.triggerChange;
  begin
    if config_onChange<>nil then state_changeTriggered:=true;
  end;

DESTRUCTOR T_changeListeningMeta.destroy;
  begin
    inherited destroy;
    if config_onChange<>nil then disposeLiteral(config_onChange);
  end;

FUNCTION T_changeListeningMeta.evaluate(CONST location:T_tokenLocation; VAR context:T_threadContext):boolean;
  VAR tmp:P_literal;
  begin
    result:=inherited evaluate(location,context);

    if state_changeTriggered then begin
      tmp:=config_onChange^.evaluateToLiteral(location,@context);
      if tmp<>nil then disposeLiteral(tmp);
      state_changeTriggered:=false;
      result:=true;
    end;
  end;
