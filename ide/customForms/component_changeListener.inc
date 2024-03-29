TYPE
  T_changeListeningMeta=object(T_guiElementMeta)
    config_onChange:P_expressionLiteral;
    state_changeTriggered:boolean;
    state_onChangeParam:P_literal;

    CONSTRUCTOR create(CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST consideredKeys:T_definingMapKeys; CONST recycler:P_literalRecycler);
    PROCEDURE triggerChange(CONST changeValue:string);
    FUNCTION evaluate(CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):boolean; virtual;
    DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_changeListeningMeta.create(CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST consideredKeys:T_definingMapKeys; CONST recycler:P_literalRecycler);
  VAR tmp:P_literal;
  begin
    inherited create(def,location,context,recycler,consideredKeys+[dmk_onChange]);
    config_onChange:=nil;
    state_onChangeParam:=nil;
    state_changeTriggered:=false;

    tmp:=mapGet(def,key[dmk_onChange],recycler);
    if tmp<>nil then begin
      if (tmp^.literalType=lt_expression)
      then config_onChange:=P_expressionLiteral(tmp)
      else context.raiseError('onChange is: '+tmp^.typeString+'; must be expression',location);
    end;
  end;

PROCEDURE T_changeListeningMeta.triggerChange(CONST changeValue:string);
  VAR newLit:P_stringLiteral=nil;
      recycler:P_recycler;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit;
    try
      recycler:=newRecycler;
      if (config.bindingTo<>'') then begin
        if state.bindingValue<>nil then recycler^.disposeLiteral(state.bindingValue);
        newLit:=recycler^.newStringLiteral(changeValue);
        state.bindingValue:=newLit;
        propagateCursor(getCustomForm,crHourGlass);
      end;
      if config_onChange<>nil then begin
        state_changeTriggered:=true;
        if state_onChangeParam<>nil then recycler^.disposeLiteral(state_onChangeParam);
        if newLit<>nil then state_onChangeParam:=newLit^.rereferenced
                       else state_onChangeParam:=recycler^.newStringLiteral(changeValue);
      end;
    finally
      leaveCriticalSection(elementCs);
      freeRecycler(recycler);
    end;
  end;

DESTRUCTOR T_changeListeningMeta.destroy;
  VAR recycler:P_recycler;
  begin
    inherited destroy;
    recycler:=newRecycler;
    if config_onChange<>nil then recycler^.disposeLiteral(config_onChange);
    if state_onChangeParam<>nil then recycler^.disposeLiteral(state_onChangeParam);
    freeRecycler(recycler);
  end;

FUNCTION T_changeListeningMeta.evaluate(CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):boolean;
  VAR tmp:P_literal;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit(false);
    try
      result:=inherited evaluate(location,context,recycler);

      if state_changeTriggered then begin
        if config_onChange^.canApplyToNumberOfParameters(1) and (state_onChangeParam<>nil)
        then tmp:=evaluteExpression(config_onChange,location,context,recycler,state_onChangeParam).literal
        else tmp:=config_onChange^.evaluate(location,context,recycler).literal;
        if state_onChangeParam<>nil then recycler^.disposeLiteral(state_onChangeParam);
        if tmp<>nil then recycler^.disposeLiteral(tmp);
        state_changeTriggered:=false;
        result:=true;
      end;
    finally
      leaveCriticalSection(elementCs);
    end;
  end;
