TYPE
  T_changeListeningMeta=object(T_guiElementMeta)
    config_onChange:P_expressionLiteral;
    state_changeTriggered:boolean;
    state_onChangeParam:P_literal;

    CONSTRUCTOR create(CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST consideredKeys:T_definingMapKeys; VAR literalRecycler:T_literalRecycler);
    PROCEDURE triggerChange(CONST changeValue:string);
    FUNCTION evaluate(CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):boolean; virtual;
    DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_changeListeningMeta.create(CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST consideredKeys:T_definingMapKeys; VAR literalRecycler:T_literalRecycler);
  VAR tmp:P_literal;
  begin
    inherited create(def,location,context,literalRecycler,consideredKeys+[dmk_onChange]);
    config_onChange:=nil;
    state_onChangeParam:=nil;
    state_changeTriggered:=false;

    tmp:=mapGet(def,key[dmk_onChange],literalRecycler);
    if tmp<>nil then begin
      if (tmp^.literalType=lt_expression)
      then config_onChange:=P_expressionLiteral(tmp)
      else context.raiseError('onChange is: '+tmp^.typeString+'; must be expression',location);
    end;
  end;

PROCEDURE T_changeListeningMeta.triggerChange(CONST changeValue:string);
  VAR newLit:P_stringLiteral=nil;
      literalRecycler:T_literalRecycler;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit;
    try
      literalRecycler.initRecycler;
      if (config.bindingTo<>'') then begin
        if state.bindingValue<>nil then literalRecycler.disposeLiteral(state.bindingValue);
        newLit:=literalRecycler.newStringLiteral(changeValue);
        state.bindingValue:=newLit;
        propagateCursor(getCustomForm,crHourGlass);
      end;
      if config_onChange<>nil then begin
        state_changeTriggered:=true;
        if state_onChangeParam<>nil then literalRecycler.disposeLiteral(state_onChangeParam);
        if newLit<>nil then state_onChangeParam:=newLit^.rereferenced
                       else state_onChangeParam:=literalRecycler.newStringLiteral(changeValue);
      end;
    finally
      leaveCriticalSection(elementCs);
      literalRecycler.cleanup;
    end;
  end;

DESTRUCTOR T_changeListeningMeta.destroy;
  VAR literalRecycler:T_literalRecycler;
  begin
    inherited destroy;
    literalRecycler.initRecycler;
    if config_onChange<>nil then literalRecycler.disposeLiteral(config_onChange);
    if state_onChangeParam<>nil then literalRecycler.disposeLiteral(state_onChangeParam);
    literalRecycler.cleanup;
  end;

FUNCTION T_changeListeningMeta.evaluate(CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):boolean;
  VAR tmp:P_literal;
      literalRecycler:T_literalRecycler;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit(false);
    try
      literalRecycler.initRecycler;
      result:=inherited evaluate(location,context,recycler);

      if state_changeTriggered then begin
        if config_onChange^.canApplyToNumberOfParameters(1) and (state_onChangeParam<>nil)
        then tmp:=config_onChange^.evaluateToLiteral(location,@context,@recycler,state_onChangeParam,nil).literal
        else tmp:=config_onChange^.evaluateToLiteral(location,@context,@recycler,nil                ,nil).literal;
        if state_onChangeParam<>nil then literalRecycler.disposeLiteral(state_onChangeParam);
        if tmp<>nil then literalRecycler.disposeLiteral(tmp);
        state_changeTriggered:=false;
        result:=true;
      end;
    finally
      leaveCriticalSection(elementCs);
      literalRecycler.cleanup;
    end;
  end;
