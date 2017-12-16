UNIT mnh_evaluation;
INTERFACE
USES sysutils,
     myGenerics,
     mnh_constants,mnh_basicTypes,
     mnh_litVar,
     mnh_tokens,
     mnh_tokenArray,
     tokenStack,
     {$ifdef fullVersion}mnh_settings,{$endif}
     mnh_contexts,
     mnh_funcs, mnh_funcs_mnh, mnh_funcs_math, mnh_funcs_list, mnh_funcs_types,
     mnh_patterns,
     mnh_subrules,
     mnh_rule,
     mnh_aggregators,
     listProcessing;

IMPLEMENTATION
PROCEDURE reduceExpression(VAR first:P_token; VAR context:T_threadContext);
  VAR stack:T_TokenStack;
      newLit:P_literal;
      didSubstitution:boolean;
      cTokType:array[-1..2] of T_tokenType;

  PROCEDURE initTokTypes; {$ifndef DEBUGMODE}inline;{$endif}
    begin
      if stack.topIndex>=0 then cTokType[-1]:=stack.topType
                           else cTokType[-1]:=tt_EOL;
      if first<>nil then begin
        cTokType[0]:=first^.tokType;
        if first^.next<>nil then begin
          cTokType[1]:=first^.next^.tokType;
          if first^.next^.next<>nil then cTokType[2]:=first^.next^.next^.tokType
                                    else cTokType[2]:=tt_EOL;
        end else begin
          cTokType[1]:=tt_EOL;
          cTokType[2]:=tt_EOL;
        end;
      end else begin
        cTokType[0]:=tt_EOL;
        cTokType[1]:=tt_EOL;
        cTokType[2]:=tt_EOL;
      end;
    end;

  FUNCTION errorLocation(CONST preferredToken:P_token=nil):T_searchTokenLocation;
    begin
      if preferredToken<>nil then exit(preferredToken           ^.location);
      if first         <>nil then exit(first                    ^.location);
      if stack.topIndex>=0   then exit(stack.dat[stack.topIndex]^.location);
      result:=C_nilTokenLocation;
    end;

  PROCEDURE raiseLazyBooleanError(CONST location:T_tokenLocation; CONST LHS:P_literal);
    begin
      context.adapters^.raiseError('Lazy boolean operators can only be applied to scalar booleans. Got '+LHS^.typeString,location);
    end;

  PROCEDURE resolveEach(eachType:T_tokenType);
    VAR bracketClosingEach:P_token=nil;
        bodyRule:T_expressionList;
        aggregator:P_aggregator=nil;
        aggregatorPresent:boolean=false;
        eachToken:P_token;

    FUNCTION parseBodyOk:boolean;
      VAR i,lastPart:longint;
          t:P_token;
          bodyParts:T_bodyParts;

      FUNCTION isPureAggregator:boolean;
        begin
          result:=eachToken^.txt='';
        end;

      begin
        //first^.next token is <each>-Token
        eachToken:=first^.next;
        if (eachToken^.next=nil) or (eachToken^.next^.tokType=tt_braceClose) then begin
          //This can only happen with the agg-construct!
          context.adapters^.raiseError('Invalid agg-construct: aggregator is missing',eachToken^.location);
          exit(false);
        end;
        //find closing bracket and body parts
        bodyParts:=getBodyParts(eachToken,0,context.recycler,context.adapters,bracketClosingEach);
        if (bracketClosingEach=nil) or not(context.adapters^.noErrors) then exit(false);
        if (length(bodyParts)>1) and isPureAggregator then begin
          context.adapters^.raiseError('Invalid agg-construct: argument must be an aggregator or aggregator prototype.',eachToken^.location);
          exit(false);
        end;

        //process aggregator part (if any)----------------------------------------------
        lastPart:=length(bodyParts)-1;
        t:=bodyParts[lastPart].first;
        if (t^.tokType=tt_aggregatorConstructor) or (t^.tokType=tt_pseudoFuncPointer) and isPureAggregator then begin
          reduceExpression(bodyParts[lastPart].first,context);
          //Handle error in evaluation of aggregator
          if bodyParts[lastPart].first=nil then setLength(bodyParts,lastPart) else begin
            bodyParts[lastPart].last:=bodyParts[lastPart].first^.last;
            t:=bodyParts[lastPart].first;
          end;
        end;
        if t<>nil then
        if t^.next=nil then begin
          case t^.tokType of
            tt_comparatorEq..tt_operatorConcatAlt: aggregator:=newAggregator(t^.tokType);
            tt_aggregatorExpressionLiteral: aggregator:=newCustomAggregator(P_expressionLiteral(t^.data),@context);
            tt_literal: if isPureAggregator and (P_literal(t^.data)^.literalType=lt_expression)
              then aggregator:=newCustomAggregator(P_expressionLiteral(t^.data),@context)
              else if isPureAggregator then context.adapters^.raiseError('Invalid agg-construct: argument must be an aggregator or aggregator prototype.',eachToken^.location);
            tt_intrinsicRule:
              if (P_intFuncCallback(t^.data)=BUILTIN_MIN)      then aggregator:=newMinAggregator      else
              if (P_intFuncCallback(t^.data)=BUILTIN_MAX)      then aggregator:=newMaxAggregator      else
              if (P_intFuncCallback(t^.data)=BUILTIN_TRAILING) then aggregator:=newTrailingAggregator else
              if (P_intFuncCallback(t^.data)=BUILTIN_TOSET)    then aggregator:=newSetAggregator      else
              if (P_intFuncCallback(t^.data)=BUILTIN_TOLIST)   then aggregator:=newListAggregator     else
              if (P_intFuncCallback(t^.data)=BUILTIN_HEAD)     then aggregator:=newHeadAggregator;
          end;
        end else if isPureAggregator then begin
          if t^.tokType=tt_expBraceOpen then begin
            digestInlineExpression(t,context);
            if context.adapters^.noErrors
            then aggregator:=newCustomAggregator(P_expressionLiteral(t^.data),@context);
          end else context.adapters^.raiseError('Invalid agg-construct: argument must be an aggregator or aggregator prototype.',eachToken^.location);
        end;
        result:=true;
        aggregatorPresent:=(aggregator<>nil);
        if aggregatorPresent then begin
          context.recycler.disposeToken(bodyParts[lastPart].first);
          setLength(bodyParts,length(bodyParts)-1);
        end else begin
          aggregator:=newListAggregator;
          if isPureAggregator then begin
            result:=false;
            context.adapters^.raiseError('Invalid agg-construct: aggregator is missing.',eachToken^.location);
          end;
        end;
        //----------------------------------------------process aggregator part (if any)
        //process other body parts (if any)---------------------------------------------
        setLength(bodyRule,length(bodyParts));
        for i:=0 to length(bodyParts)-1 do
          new(P_inlineExpression(bodyRule[i]),createForEachBody(eachToken^.txt,bodyParts[i].first,context));
        //---------------------------------------------process other body parts (if any)
      end;

    VAR iterator:P_expressionLiteral;
        i:longint;
        eachLocation:T_tokenLocation;

    PROCEDURE finalizeAggregation;
      begin
        first^.data:=aggregator^.getResult;
        first^.txt:='';
        first^.next:=context.recycler.disposeToken(bracketClosingEach);
        dispose(aggregator,destroy);
      end;
    begin
      if (eachType=tt_parallelEach) and
         ((context.callDepth>=STACK_DEPTH_LIMIT-16) or
         not(tco_spawnWorker in context.threadOptions) or
         (workerThreadCount<=0))
      then eachType:=tt_each;
      eachLocation:=first^.location;
      initialize(bodyRule);

      if (first^.data=nil) or (P_literal(first^.data)^.literalType in [lt_error,lt_void]) then begin
        context.adapters^.raiseError('Cannot apply each construct to void literal',eachLocation);
        exit
      end;
      if context.callDepth>STACK_DEPTH_LIMIT then begin
        {$ifdef debugMode}
        raise Exception.create('Stack overflow in (p)each construct.');
        {$else}
        context.adapters^.raiseSystemError('Stack overflow in (p)each construct.',eachLocation);
        {$endif}
        exit;
      end;
      if not(parseBodyOk) then exit;
      iterator:=newIterator(P_literal(first^.data));
      disposeLiteral(first^.data);
      first^.next:=context.recycler.disposeToken(first^.next);
      //iterate over itList----------------------------------------------------------
      if length(bodyRule)>0 then begin
        if eachType = tt_parallelEach
        then processListParallel(iterator,bodyRule,aggregator,eachLocation,context)
        else processListSerial  (iterator,bodyRule,aggregator,eachLocation,context);
      end else begin
        if eachType = tt_parallelEach then context.adapters^.raiseNote('There is no paralellization for pEach statements without body (i.e. pure aggregators)',eachLocation);
        aggregate(iterator,aggregator,eachLocation,context);
      end;
      //----------------------------------------------------------iterate over itList
      //cleanup----------------------------------------------------------------------
      finalizeAggregation;
      disposeLiteral(iterator);
      for i:=0 to length(bodyRule)-1 do dispose(bodyRule[i],destroy);
      //----------------------------------------------------------------------cleanup
      didSubstitution:=true;
    end;

  PROCEDURE resolveWhile;
    VAR bracketClosingWhile:P_token=nil;
        headRule:P_inlineExpression=nil;
        bodyRule:P_inlineExpression=nil;

    FUNCTION parseBodyOk:boolean;
      VAR i:longint;
          bodyParts:T_bodyParts;
          emptyPattern:T_pattern;
      begin
        result:=false;
        //first token is <while>-Token
        //find closing bracket and body parts
        bodyParts:=getBodyParts(first,1,context.recycler,context.adapters,bracketClosingWhile);
        if bracketClosingWhile=nil then exit(false);
        if length(bodyParts)<>2 then begin
          context.adapters^.raiseError('Invalid while-construct; Exactly two arguments (head and body) are expected.',errorLocation);
          exit(false);
        end;

        for i:=0 to length(bodyParts)-1 do begin
          if bodyParts[i].last^.next<>bracketClosingWhile then context.recycler.disposeToken(bodyParts[i].last^.next);
          bodyParts[i].last^.next:=nil;
        end;

        //create head/body rules------------------------------------------------
        emptyPattern.create;
        new(headRule,createForWhile(bodyParts[0].first,bodyParts[0].first^.location,context));
        new(bodyRule,createForWhile(bodyParts[1].first,bodyParts[1].first^.location,context));
        emptyPattern.destroy;
        //------------------------------------------------create head/body rules
        result:=true;
      end;

    VAR whileLocation:T_tokenLocation;
    PROCEDURE evaluateBody; inline;
      VAR toReduce,dummy:P_token;
      begin
        if bodyRule^.replaces(nil,whileLocation,toReduce,dummy,context,false) then begin
          reduceExpression(toReduce,context);
          context.recycler.cascadeDisposeToken(toReduce);
        end;
      end;

    begin
      whileLocation:=first^.location;
      if context.callDepth>STACK_DEPTH_LIMIT then begin
        context.adapters^.raiseSystemError('Stack overflow in while construct.',whileLocation);
        exit;
      end;
      if not(parseBodyOk) then exit;
      while headRule^.evaluateToBoolean(whileLocation,@context) and (context.adapters^.noErrors) do evaluateBody;
      first^.txt:='';
      first^.tokType:=tt_literal;
      first^.data:=newVoidLiteral;
      first^.next:=context.recycler.disposeToken(bracketClosingWhile);
      //cleanup----------------------------------------------------------------------
      dispose(headRule,destroy);
      dispose(bodyRule,destroy);
      //----------------------------------------------------------------------cleanup
      didSubstitution:=true;
    end;

  PROCEDURE applyRule(CONST parameterListToken:P_token; CONST firstTokenAfterCall:P_token);
    VAR firstReplace,lastReplace:P_token;
        newLiteral:P_literal;
        parameterListLiteral:P_listLiteral;
        inlineRule:P_inlineExpression;
        violations:T_sideEffects;

    begin
      if parameterListToken=nil then parameterListLiteral:=nil
                                else parameterListLiteral:=parameterListToken^.data;
      if (first^.tokType in [tt_localUserRule,tt_importedUserRule,tt_customTypeRule]) then begin
        {$ifndef DEBUGMODE}
        try
        {$endif}
          if not(P_rule(first^.data)^.replaces(parameterListLiteral,first^.location,firstReplace,lastReplace,first^.tokType in [tt_localUserRule,tt_customTypeRule],@context)) then begin
            if (P_rule(first^.data)^.getId=MAIN_RULE_ID)
            then context.raiseCannotApplyError('user defined rule '+P_rule(first^.data)^.getId,parameterListLiteral,first^.location,P_rule(first^.data)^.getCmdLineHelpText,true)
            else context.raiseCannotApplyError('user defined rule '+P_rule(first^.data)^.getId,parameterListLiteral,first^.location,C_EMPTY_STRING_ARRAY);
            exit;
          end;
        {$ifndef DEBUGMODE}
        except
          on e:Exception do begin
            context.adapters^.raiseSystemError('Severe error trying to apply user defined rule '+P_rule(first^.data)^.getId,first^.location);
            context.adapters^.raiseSystemError(e.message,first^.location);
            exit;
          end;
        end;
        {$endif}
      end else if (first^.tokType=tt_rulePutCacheValue) then begin
        newLiteral:=P_memoizedRule(first^.data)^.doPutCache(parameterListLiteral);
        firstReplace:=context.recycler.newToken(first^.location,'',tt_literal,newLiteral);
        lastReplace:=firstReplace;
      end else if (first^.tokType=tt_aggregatorConstructor) then begin
        if (parameterListLiteral<>nil) and (parameterListLiteral^.size=1) and
           (parameterListLiteral^.value[0]^.literalType=lt_expression) and (P_expressionLiteral(parameterListLiteral^.value[0])^.canApplyToNumberOfParameters(2))
        then begin
          newLiteral:=parameterListLiteral^.value[0]^.rereferenced;
          firstReplace:=context.recycler.newToken(first^.location,'',tt_aggregatorExpressionLiteral,newLiteral);
          lastReplace:=firstReplace;
        end else context.adapters^.raiseError('Aggregators can only be constructed from expression(2) literals!',errorLocation);
      end else if (first^.tokType=tt_intrinsicRule) then begin
        {$ifndef DEBUGMODE}
        try
        {$endif}
        violations:=violatingSideEffects(first^.data,context.sideEffectWhitelist);
        if violations=[] then newLiteral:=P_intFuncCallback(first^.data)(parameterListLiteral,first^.location,context)
        else begin
          context.raiseSideEffectError('function '+first^.txt,first^.location, violations);
          exit;
        end;
        {$ifndef DEBUGMODE}
        except
          on e:Exception do begin
            context.adapters^.raiseSystemError('Severe error trying to apply builtin rule '+first^.txt,first^.location);
            context.adapters^.raiseSystemError(e.message,first^.location);
            exit;
          end;
        end;
        {$endif}
        if newLiteral<>nil then begin
          firstReplace:=context.recycler.newToken(first^.location,'',tt_literal,newLiteral);
          lastReplace:=firstReplace;
        end else if not(context.adapters^.noErrors) then exit else begin
          context.raiseCannotApplyError('intrinsic rule '+first^.txt,parameterListLiteral,first^.location,C_EMPTY_STRING_ARRAY);
          exit;
        end;
      end else if (first^.tokType in [tt_literal,tt_aggregatorExpressionLiteral]) and (P_literal(first^.data)^.literalType=lt_expression) then begin
        if P_expressionLiteral(first^.data)^.typ in C_builtinExpressionTypes then begin
          newLiteral:=P_expressionLiteral(first^.data)^.evaluate(first^.location,@context,parameterListLiteral);
          if newLiteral<>nil then begin
            firstReplace:=context.recycler.newToken(first^.location,'',tt_literal,newLiteral);
            lastReplace:=firstReplace;
          end else if not(context.adapters^.noErrors) then exit else begin
            context.raiseCannotApplyError('wrapped intrinsic rule '+first^.txt,parameterListLiteral,first^.location,C_EMPTY_STRING_ARRAY);
            exit;
          end;
        end else begin
          inlineRule:=first^.data;
          //failing "replaces" for inline rules will raise evaluation error.
          if not(inlineRule^.replaces(parameterListLiteral,first^.location,firstReplace,lastReplace,context,false)) and
             not(inlineRule^.replaces(parameterListLiteral,first^.location,firstReplace,lastReplace,context,true )) then exit;
        end;
      end else begin
        context.adapters^.raiseError('Trying to apply a rule which is no rule!',first^.location);
        exit;
      end;
      context.recycler.disposeToken(first);
      if parameterListToken<>nil then context.recycler.disposeToken(parameterListToken);
      first:=firstReplace;
      lastReplace^.next:=firstTokenAfterCall;
      didSubstitution:=true;
    end;

  PROCEDURE resolveInlineIf(CONST conditionLit:boolean); inline;
    VAR p,prev,tokenBeforeElse,lastThen:P_token;
        bracketLevel:longint=0;
    begin
      prev:=first;
      stack.push(first); //push "?"
      p:=first;
      while (p<>nil) and not((p^.tokType=tt_iifElse) and (bracketLevel=0)) do begin
        if p^.tokType in      C_openingBrackets then inc(bracketLevel)
        else if p^.tokType in C_closingBrackets then dec(bracketLevel);
        prev:=p; p:=p^.next;
      end;
      if not((p<>nil) and (p^.tokType=tt_iifElse) and (bracketLevel=0)) then begin
        stack.popLink(first);
        context.adapters^.raiseError('Cannot evaluate inline-if; cannot locate then-marker',errorLocation);
        exit;
      end;
      tokenBeforeElse:=prev;
      while (p<>nil) and not((p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon]) and (bracketLevel=-1)) do begin
        if p^.tokType in      C_openingBrackets then inc(bracketLevel)
        else if p^.tokType in C_closingBrackets then dec(bracketLevel);
        prev:=p; p:=p^.next;
      end;
      if  not((p=nil) or (p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon]) and (bracketLevel=-1)) then begin
        stack.popLink(first);
        context.adapters^.raiseError('Cannot evaluate inline-if; cannot locate end of then-expression',errorLocation);
        exit;
      end;
      lastThen:=prev;
      if conditionLit then begin
        //take then-subexpression -> drop else-subexpression
        p:=tokenBeforeElse^.next;              //store tt_iifElse-token
        tokenBeforeElse^.next:=lastThen^.next; //unlink else-expression (head)
        lastThen^.next:=nil;                   //unlink else-expression (tail);
        context.recycler.cascadeDisposeToken(p);       //dispose else-expression
      end else begin
        //take else-subexpression -> drop then-subexpression
        p:=first;
        first:=tokenBeforeElse^.next^.next;
        tokenBeforeElse^.next^.next:=nil;
        context.recycler.cascadeDisposeToken(p);
      end;
      stack.popDestroy(context.recycler); //pop "?"
      stack.popDestroy(context.recycler); //pop condition literal
      didSubstitution:=true;
    end;

  PROCEDURE startOrPushParameterList; {$ifndef DEBUGMODE}inline;{$endif}
    begin
      stack.push(first);
      if first^.tokType=tt_braceOpen then begin
        first^.tokType:=tt_parList_constructor;
        first^.data:=newListLiteral;
      end;
      stack.push(first);
      didSubstitution:=true;
    end;

  PROCEDURE applyMutation;
    VAR newValue:P_literal;
    begin
      if not(se_alterPackageState in context.sideEffectWhitelist) then begin
        context.raiseSideEffectError('assingment to mutable '+P_mutableRule(first^.data)^.getId,first^.location, [se_alterPackageState]);
        exit;
      end;
      newValue:=first^.next^.data;
      P_mutableRule(first^.data)^.setMutableValue(newValue,false);
      first:=context.recycler.disposeToken(first);
      didSubstitution:=true;
    end;

  PROCEDURE applyLocalAssignment(CONST kind:T_tokenType);
    VAR newValue:P_literal;
    begin
      newValue:=first^.next^.data;
      case kind of
        tt_assignNewBlockLocal: begin
          context.valueStore^.createVariable(first^.txt,newValue,false);
          first:=context.recycler.disposeToken(first);
        end;
        tt_assignExistingBlockLocal: begin
          context.valueStore^.setVariableValue(first^.txt,newValue,first^.location,context.adapters);
          first:=context.recycler.disposeToken(first);
        end;
        tt_mut_nested_assign..tt_mut_nestedDrop: if first^.data=nil then begin
          newValue:=context.valueStore^.mutateVariableValue(first^.txt,kind,newValue,first^.location,context.adapters,@context);
          if context.adapters^.noErrors then begin
            first:=context.recycler.disposeToken(first);
            disposeLiteral(first^.data);
            first^.data:=newValue;
          end;
        end else begin
          if not(se_alterPackageState in context.sideEffectWhitelist) then begin
            context.raiseSideEffectError('modification of mutable '+P_mutableRule(first^.data)^.getId,first^.location,[se_alterPackageState]);
            exit;
          end;
          newValue:=P_mutableRule(first^.data)^.mutateInline(kind,newValue,first^.location,context);
          if context.adapters^.noErrors then begin
            first:=context.recycler.disposeToken(first);
            disposeLiteral(first^.data);
            first^.data:=newValue;
          end;
        end;
      end;
      didSubstitution:=true;
    end;

  PROCEDURE pon_flip;
    VAR newFunctionToken:P_token;
        newParameterListToken:P_token;
        oldSecond:P_token;
        expression:P_literal;
        ruleIdResolved:boolean=false;
        cTokType3:T_tokenType;
    begin
      if first^.next^.next=nil then exit;
      //Assuming:
      //cTokType[0]=tt_literal
      //cTokType[1]=tt_ponFlipper
      //cTokType[2] is a resolved identifier or a blocklocal variable;
      //Transforms:
      //  <Lit> . func     -> func(<Lit>)
      //  <Lit> . func(... -> func(<Lit>,...
      newFunctionToken:=context.recycler.newToken(first^.next^.next);

      if cTokType[2]=tt_EOL then cTokType3:=tt_EOL else begin
        oldSecond:=first^.next^.next^.next;
        if oldSecond=nil then cTokType3:=tt_EOL
        else cTokType3:=oldSecond^.tokType;
      end;

      with newFunctionToken^ do begin
        ruleIdResolved:=not(tokType in [tt_identifier,tt_blockLocalVariable]);
        if tokType=tt_blockLocalVariable then begin
          expression:=context.valueStore^.getVariableValue(newFunctionToken^.txt);
          if (expression<>nil) then begin
            if expression^.literalType<>lt_expression then expression^.unreference
            else begin
              newFunctionToken^.data:=expression;
              newFunctionToken^.tokType:=tt_literal;
              ruleIdResolved:=true;
            end;
          end;
        end else if tokType in C_operators then begin
          txt:=C_tokenInfo[tokType].defaultId;
          data:=intFuncForOperator[tokType];
          tokType:=tt_intrinsicRule;
          ruleIdResolved:=true;
        end;
      end;
      if ruleIdResolved then begin
        //resolved rule ID mutate x . y -> y(x)
        newParameterListToken:=context.recycler.newToken(first^.next^.next^.location,'',tt_parList,newListLiteral(1)^.append(first^.data,false));

        first^.data:=nil; first^.tokType:=tt_identifier;
                               //Disposing from:   <Lit> . func ...
        oldSecond:=context.recycler.disposeToken(         //|   | | ^^^^
                   context.recycler.disposeToken(         //|   | ^
                   context.recycler.disposeToken(first)));//^^^^^

        first:=newFunctionToken; newFunctionToken^.next:=newParameterListToken; newParameterListToken^.next:=oldSecond;

        if cTokType3=tt_braceOpen then begin
          newParameterListToken^.next:=context.recycler.disposeToken(oldSecond);
          newParameterListToken^.tokType:=tt_parList_constructor;
        end;
        didSubstitution:=true;
        exit;
      end else begin
        context.adapters^.raiseError('Unresolved identifier: '+newFunctionToken^.txt,newFunctionToken^.location);
        context.recycler.disposeToken(newFunctionToken);
        exit;
      end;
    end;

  PROCEDURE process_op_lit; {$ifndef DEBUGMODE} inline;{$endif}
    begin
      case cTokType[1] of
        tt_comparatorEq..tt_operatorConcatAlt:
          if C_opPrecedence[cTokType[1]]>=C_opPrecedence[cTokType[-1]] then begin
            newLit:=resolveOperator(stack.dat[stack.topIndex-1]^.data,
                                    cTokType[-1],
                                    first^.data,
                                    stack.dat[stack.topIndex]^.location,
                                    context.adapters^,
                                    @context);
            disposeLiteral(first^.data);
            first^.data:=newLit; //store new literal in head
            first^.location:=stack.dat[stack.topIndex]^.location;
            stack.popDestroy(context.recycler); //pop operator from stack
            stack.popDestroy(context.recycler); //pop LHS-Literal from stack
            didSubstitution:=true;
          end else begin
            stack.push(first);
            stack.push(first);
            didSubstitution:=true;
          end;
        tt_braceClose,tt_listBraceClose,tt_EOL,tt_separatorComma,tt_semicolon, tt_separatorCnt, tt_iifCheck, tt_iifElse:
          begin
            newLit:=resolveOperator(stack.dat[stack.topIndex-1]^.data,
                                    cTokType[-1],
                                    first^.data,
                                    stack.dat[stack.topIndex]^.location,
                                    context.adapters^,
                                    @context);
            disposeLiteral(first^.data);
            first^.data:=newLit; //store new literal in head
            first^.location:=stack.dat[stack.topIndex]^.location;
            stack.popDestroy(context.recycler); //pop operator from stack
            stack.popDestroy(context.recycler); //pop LHS-Literal from stack
            didSubstitution:=true;
          end;
        tt_parList:
          begin
            if cTokType[2]=tt_listToParameterList then begin
              stack.push(first);
              stack.push(first);
              stack.push(first);
              didSubstitution:=true;
            end else applyRule(first^.next,first^.next^.next)
          end;
        tt_parList_constructor,tt_braceOpen,tt_listToParameterList:
          startOrPushParameterList;
        tt_ponFlipper:
          pon_flip;
        tt_each,tt_parallelEach:
          resolveEach(cTokType[1]);
        tt_listBraceOpen:
          begin
            stack.push(first);
            didSubstitution:=true;
          end;
        else context.adapters^.raiseError('Unexpected token after literal: '+safeTokenToString(first^.next)+' ('+C_tokenInfo[cTokType[1]].helpText+')',errorLocation);
      end;
    end;

  PROCEDURE operator_and_literal_push;
    VAR p:P_token;
        bracketLevel:longint=0;
    begin
      case cTokType[1] of
        tt_operatorLazyAnd: if (cTokType[0]=tt_literal) and (P_literal(first^.data)^.literalType=lt_boolean) then begin
          if (P_boolLiteral(first^.data)^.value) then begin
            //true AND ... -> ...
            first:=context.recycler.disposeToken(first); //drop true
            first:=context.recycler.disposeToken(first); //drop AND
            didSubstitution:=true;
          end else begin
            //false AND ... -> false
            p:=first^.next;
            while not((p=nil) or (p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon,tt_iifCheck,tt_iifElse,tt_operatorLazyOr,tt_operatorOr]) and (bracketLevel=0)) do begin
              if      p^.tokType in C_openingBrackets then inc(bracketLevel)
              else if p^.tokType in C_closingBrackets then dec(bracketLevel);
              p:=context.recycler.disposeToken(p);
            end;
            first^.next:=p;
            didSubstitution:=true;
          end;
        end else raiseLazyBooleanError(first^.next^.location,P_literal(first^.data));
        tt_operatorLazyOr:if (cTokType[0]=tt_literal) and (P_literal(first^.data)^.literalType=lt_boolean) then begin
          if (P_boolLiteral(first^.data)^.value) then begin
            //true OR ... -> true
            p:=first^.next;
            while not((p=nil) or (p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon,tt_iifCheck,tt_iifElse]) and (bracketLevel=0)) do begin
              if      p^.tokType in C_openingBrackets then inc(bracketLevel)
              else if p^.tokType in C_closingBrackets then dec(bracketLevel);
              p:=context.recycler.disposeToken(p);
            end;
            first^.next:=p;
            didSubstitution:=true;
          end else begin
            //false OR ... -> ...
            first:=context.recycler.disposeToken(first); //drop false
            first:=context.recycler.disposeToken(first); //drop OR
            didSubstitution:=true;
          end;
        end else raiseLazyBooleanError(first^.next^.location,P_literal(first^.data));
        tt_operatorOrElse: if (cTokType[0]=tt_literal) then begin
          if (P_literal(first^.data)^.literalType = lt_void) then begin
            //void orElse ... -> ...
            first:=context.recycler.disposeToken(first); //drop void
            first:=context.recycler.disposeToken(first); //drop orElse
            didSubstitution:=true;
          end else begin
            //<Lit> orElse ... -> <Lit>
            p:=first^.next;
            while not((p=nil) or (p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon,tt_iifCheck,tt_iifElse,tt_operatorLazyOr,tt_operatorOr]) and (bracketLevel=0)) do begin
              if      p^.tokType in C_openingBrackets then inc(bracketLevel)
              else if p^.tokType in C_closingBrackets then dec(bracketLevel);
              p:=context.recycler.disposeToken(p);
            end;
            first^.next:=p;
            didSubstitution:=true;
          end;
        end;
        else begin
          stack.push(first);
          stack.push(first);
          didSubstitution:=true;
        end;
      end;
    end;

  PROCEDURE resolvePseudoFuncPointer;
    VAR exRule:P_builtinExpression;
        ruleToken:P_token;
        temp:P_token;
        location:T_tokenLocation;
    begin
      //state @pre: ::f ...
      location:=first^.location;;
      ruleToken:=context.recycler.disposeToken(first); //dispose ::, store f
      temp:=ruleToken^.next; //store ...
      if (ruleToken^.tokType in [tt_localUserRule, tt_importedUserRule, tt_customTypeRule])
      then begin
        ruleToken^.data:=P_rule(ruleToken^.data)^.getFunctionPointer(context,ruleToken^.tokType,ruleToken^.location);
        ruleToken^.tokType:=tt_literal;
        first:=ruleToken;
      end else begin
        new(exRule,create(P_intFuncCallback(ruleToken^.data),ruleToken^.location));
        context.recycler.disposeToken(ruleToken);
        first:=context.recycler.newToken(location,'',tt_literal,exRule); // {f@$params}
      end;
      first^.next:=temp; //-> {f@$params} ...
      didSubstitution:=true;
    end;

  PROCEDURE cleanupStackAndExpression;
    VAR clean:boolean=true;
    begin
      while (first<>nil) and clean do begin
        case first^.tokType of
          tt_beginBlock,tt_beginExpression,tt_beginRule: context.valueStore^.scopePush( first^.tokType=tt_beginRule);
          tt_endBlock,tt_endExpression,tt_endRule: begin
            while (stack.topIndex>=0) and not(stack.topType in [tt_beginBlock,tt_beginExpression,tt_beginRule]) do
            stack.popDestroy(context.recycler);
            if (stack.topIndex>=0) and (first^.tokType=C_compatibleEnd[stack.topType]) then begin
              {$ifdef fullVersion}
              if (stack.topType in [tt_beginRule,tt_beginExpression]) then context.callStackPop;
              {$endif}
              stack.popDestroy(context.recycler);
              context.valueStore^.scopePop;
            end else clean:=false;
          end;
        end;
        first:=context.recycler.disposeToken(first);
      end;
      while (stack.topIndex>=0) do stack.popDestroy(context.recycler);
      if first<>nil then context.recycler.cascadeDisposeToken(first);
    end;

  PROCEDURE processReturnStatement;
    VAR returnToken:P_token;
        level:longint=1;
    begin
      if not(stack.hasTokenTypeAnywhere(tt_beginRule)) then begin
        context.adapters^.raiseError('You cannot return from here (not a subrule)',stack.dat[stack.topIndex]^.location);
        exit;
      end;

      stack.popDestroy(context.recycler); //pop "return" from stack
      returnToken:=first; //result is first (tt_literal);
      first:=context.recycler.disposeToken(first^.next); //drop semicolon

      while not(level=0) and (context.adapters^.noErrors) do begin
        while not(stack.topType  in [tt_beginRule,tt_beginExpression,tt_beginBlock,
                                     tt_endRule  ,tt_endExpression  ,tt_endBlock,tt_EOL]) do stack.popDestroy(context.recycler);
        while (first<>nil) and
              not(first^.tokType in [tt_beginRule,tt_beginExpression,tt_beginBlock,
                                     tt_endRule  ,tt_endExpression  ,tt_endBlock,tt_EOL]) do first:=context.recycler.disposeToken(first);
        if first=nil
        then context.adapters^.raiseError('Invalid stack state (processing return statement)',first^.location)
        else case first^.tokType of
          tt_beginBlock:            begin stack.push(first); context.valueStore^.scopePush(false); end;
          tt_beginExpression:       begin stack.push(first); context.valueStore^.scopePush(false); end;
          tt_beginRule: begin inc(level); stack.push(first); context.valueStore^.scopePush(true);  end;
          tt_endBlock:
            if stack.topType=tt_beginBlock
            then begin
              context.valueStore^.scopePop;
              stack.popDestroy(context.recycler);
              first:=context.recycler.disposeToken(first);
            end else context.adapters^.raiseError('Invalid stack state (processing return statement)',first^.location);
          tt_endExpression:
            if stack.topType=tt_beginExpression
            then begin
              {$ifdef fullVersion} context.callStackPop; {$endif}
              context.valueStore^.scopePop;
              stack.popDestroy(context.recycler);
              first:=context.recycler.disposeToken(first);
            end else context.adapters^.raiseError('Invalid stack state (processing return statement)',first^.location);
          tt_endRule:
            if stack.topType=tt_beginRule
            then begin
              {$ifdef fullVersion} context.callStackPop; {$endif}
              context.valueStore^.scopePop;
              stack.popDestroy(context.recycler);
              first:=context.recycler.disposeToken(first);
              dec(level);
            end else context.adapters^.raiseError('Invalid stack state (processing return statement)',first^.location);
          else context.adapters^.raiseError('Invalid stack state (processing return statement)',first^.location);
        end;
      end;
      returnToken^.next:=first;
      first:=returnToken;
      didSubstitution:=true;
    end;

  PROCEDURE resolveElementAccess;
    begin
      newLit:=newListLiteral;
      P_listLiteral(newLit)^
      .append(first^.data,false)^
      .appendAll(first^.next^.data);
      disposeLiteral(first^.next^.data);
      first^.tokType:=tt_intrinsicRule;
      first^.data:=BUILTIN_GET;
      first^.txt:='get';
      first^.next^.tokType:=tt_parList;
      first^.next^.data:=newLit;
      applyRule(first^.next,first^.next^.next);
    end;

{$MACRO ON}
{$define COMMON_CASES:=
tt_listBraceOpen: begin
  first^.next^.data:=newListLiteral;
  first^.next^.tokType:=tt_list_constructor;
  stack.push(first);
  didSubstitution:=true;
end;
tt_ponFlipper: if (cTokType[2]=tt_expBraceOpen) then begin
  stack.push(first);
  stack.push(first);
  didSubstitution:=true;
end else pon_flip;
tt_braceOpen, tt_parList_constructor, tt_listToParameterList: startOrPushParameterList;
tt_parList:  if (cTokType[2]=tt_listToParameterList) then begin
               stack.push(first);
               stack.push(first);
               stack.push(first);
               didSubstitution:=true;
             end else applyRule(first^.next,first^.next^.next);
tt_comparatorEq..tt_operatorConcatAlt: operator_and_literal_push;
tt_iifCheck: begin stack.push(first); didSubstitution:=true; end;
tt_each,tt_parallelEach: resolveEach(cTokType[1])}

{$define FORBIDDEN_SEPARATORS:=
tt_separatorCnt:   context.adapters^.raiseError('Token .. is only allowed in list constructors.',first^.next^.location);
tt_separatorComma: context.adapters^.raiseError('Token , is only allowed in parameter lists and list constructors.',first^.next^.location)}

{$WARN 2005 OFF}
//COMMON_SEMICOLON_HANDLING is defined for cTokType[0]=tt_literal; case C_tokType[1] of ...
{$define COMMON_SEMICOLON_HANDLING:=
tt_semicolon: if (cTokType[-1] in [tt_beginBlock,tt_beginRule,tt_beginExpression]) then begin
  if (cTokType[2]=C_compatibleEnd[cTokType[-1]]) then begin
    {$ifdef fullVersion}
    if (cTokType[-1] in [tt_beginRule,tt_beginExpression]) then context.callStackPop;
    {$endif}
    stack.popDestroy(context.recycler);
    first^.next:=context.recycler.disposeToken(first^.next);
    first^.next:=context.recycler.disposeToken(first^.next);
    context.valueStore^.scopePop;
    didSubstitution:=true;
  end else begin
    first:=context.recycler.disposeToken(first);
    first:=context.recycler.disposeToken(first);
    didSubstitution:=true;
  end;
end else begin
  first:=context.recycler.disposeToken(first);
  first:=context.recycler.disposeToken(first);
  didSubstitution:=true;
end}

  {$ifdef fullVersion}
  VAR debugRun:boolean=true;
  {$endif}
  begin
    inc(context.callDepth);
    stack.create;
    {$ifndef DEBUGMODE}try{$endif}
    repeat
      didSubstitution:=false;
      initTokTypes;

      //writeln(cTokType[-1],' # ',cTokType[0],' ',cTokType[1],' ',cTokType[2]);
      //writeln(stack.toString(first,20));

      {$ifdef fullVersion}
      debugRun:=debugRun and context.stepping(first,@stack);
      {$endif}
      case cTokType[0] of
        tt_literal,tt_aggregatorExpressionLiteral: case cTokType[-1] of
          tt_ponFlipper: if (P_literal(first^.data)^.literalType=lt_expression)
            and (stack.topIndex>0) and (stack.dat[stack.topIndex-1]^.tokType=tt_literal) then begin
            // <Lit> . # {$x}
            stack.popLink(first);
            stack.popLink(first);
            // -> # <Lit> {$x}
            initTokTypes;
            pon_flip;
          end;
          tt_listToParameterList: if P_literal(first^.data)^.literalType in C_listTypes then begin
            stack.popDestroy(context.recycler);
            first^.tokType:=tt_parList;
            stack.popLink(first);
            while first^.tokType=tt_parList do begin
              P_listLiteral(first^.data)^.appendAll(P_listLiteral(first^.next^.data));
              first^.next:=context.recycler.disposeToken(first^.next);
              stack.popLink(first);
            end;
            didSubstitution:=true;
          end;
          tt_unaryOpPlus: begin
            stack.popDestroy(context.recycler);
            didSubstitution:=true;
          end;
          tt_unaryOpMinus: begin
            newLit:=P_literal(first^.data)^.negate(stack.dat[stack.topIndex]^.location,context.adapters^,@context);
            disposeLiteral(first^.data);
            first^.data:=newLit;
            stack.popDestroy(context.recycler);
            didSubstitution:=true;
          end;
          tt_comparatorEq..tt_comparatorListEq: begin //operators with special cascading
            if (cTokType[1] in [tt_comparatorEq..tt_comparatorListEq]) then begin
              // x < y < z -> [x < y] and y < z
              newLit:=resolveOperator(stack.dat[stack.topIndex-1]^.data,
                                      stack.topType,
                                      first^.data,
                                      stack.dat[stack.topIndex]^.location,
                                      context.adapters^,
                                      @context);
              //LHS literal is now result of first comparison (still a literal)
              disposeLiteral(stack.dat[stack.topIndex-1]^.data);
              stack.dat[stack.topIndex-1]^.data:=newLit;
              stack.dat[stack.topIndex-1]^.location:=stack.dat[stack.topIndex]^.location;
              //applied comparator is replaced by operator 'and'
              stack.dat[stack.topIndex]^.tokType:=tt_operatorAnd;
              didSubstitution:=true;
            end else process_op_lit;
          end;
          tt_operatorIn..tt_operatorConcatAlt: process_op_lit;
          tt_braceOpen : case cTokType[1] of // ( | <Lit>
            tt_braceClose: begin  // ( | <Lit> )
              stack.popDestroy(context.recycler);
              first^.next:=context.recycler.disposeToken(first^.next);
              didSubstitution:=true;
            end;
            COMMON_SEMICOLON_HANDLING;
            COMMON_CASES;
            FORBIDDEN_SEPARATORS;
            else context.adapters^.raiseError('Unable to resolve paranthesis!',stack.dat[stack.topIndex]^.location);
          end;
          tt_list_constructor,tt_list_constructor_ranging: case cTokType[1] of
            tt_separatorComma, tt_separatorCnt: begin // [ | <Lit> ,
              repeat
                P_listLiteral(stack.dat[stack.topIndex]^.data)^.appendConstructing(first^.data,first^.next^.location,context.adapters,
                              stack.topType=tt_list_constructor_ranging);
                if first^.next^.tokType=tt_separatorCnt
                then stack.dat[stack.topIndex]^.tokType:=tt_list_constructor_ranging
                else stack.dat[stack.topIndex]^.tokType:=tt_list_constructor;
                first:=context.recycler.disposeToken(first);
                first:=context.recycler.disposeToken(first);
              until (first=nil) or (first^.tokType<>tt_literal) or
                    (first^.next=nil) or not(first^.next^.tokType in [tt_separatorComma,tt_separatorCnt]);
              didSubstitution:=true;
            end;
            tt_listBraceClose: begin // [ | <Lit> ] ...
              P_listLiteral(stack.dat[stack.topIndex]^.data)^.appendConstructing(first^.data,first^.next^.location,context.adapters,
                            stack.topType=tt_list_constructor_ranging);
              first:=context.recycler.disposeToken(first);
              first:=context.recycler.disposeToken(first);
              stack.popLink(first);   // -> ? | [ ...
              first^.tokType:=tt_literal; // -> ? | <NewList>
              didSubstitution:=true;
              if (stack.topType in [tt_blockLocalVariable,tt_localUserRule,tt_importedUserRule]) then begin
                // x # [y]:=... -> x<<[y]|...;
                stack.popLink(first);
                if first^.tokType=tt_blockLocalVariable then first^.data:=nil;
                if cTokType[2] in [tt_assign,tt_mutate,
                                   tt_mut_assignPlus,
                                   tt_mut_assignMinus,
                                   tt_mut_assignMult,
                                   tt_mut_assignDiv,
                                   tt_mut_assignStrConcat,
                                   tt_mut_assignAppend,
                                   tt_mut_assignAppendAlt,
                                   tt_mut_assignDrop] then begin
                  //first=                   x
                  //first^.next=             [y]
                  //first^.next^.next=       :=
                  //first^.next^.next^.next= ... (some expression)
                  case cTokType[2] of
                    tt_assign,tt_mutate   : first^.tokType:=tt_mut_nested_assign;
                    tt_mut_assignPlus     : first^.tokType:=tt_mut_nestedPlus     ;
                    tt_mut_assignMinus    : first^.tokType:=tt_mut_nestedMinus    ;
                    tt_mut_assignMult     : first^.tokType:=tt_mut_nestedMult     ;
                    tt_mut_assignDiv      : first^.tokType:=tt_mut_nestedDiv      ;
                    tt_mut_assignStrConcat: first^.tokType:=tt_mut_nestedStrConcat;
                    tt_mut_assignAppend   : first^.tokType:=tt_mut_nestedAppend   ;
                    tt_mut_assignAppendAlt: first^.tokType:=tt_mut_nestedAppendAlt;
                    tt_mut_assignDrop     : first^.tokType:=tt_mut_nestedDrop     ;
                  end;
                  first^.next^.next^.tokType:=tt_operatorConcatAlt;
                  //first=                   x<<
                  //first^.next=             [y]
                  //first^.next^.next=       ||
                  //first^.next^.next^.next= ... (some expression)
                  stack.push(first);
                end else begin
                  if first^.tokType=tt_blockLocalVariable
                  then newLit:=context.valueStore^.getVariableValue(first^.txt)
                  else newLit:=P_mutableRule(first^.data)^.getValue(context);
                  if newLit<>nil then begin
                    first^.data:=newLit;
                    first^.tokType:=tt_literal;
                    resolveElementAccess;
                  end else begin
                    context.adapters^.raiseError('Cannot resolve variable '+first^.txt,first^.location);
                    didSubstitution:=false;
                  end;
                end;
              end else if (stack.topType=tt_literal) then begin
                // <Lit> | <NewList> ...
                stack.popLink(first); // -> | <Lit> <NewList> ...
                resolveElementAccess;
              end;
            end;
            COMMON_SEMICOLON_HANDLING;
            COMMON_CASES;
          end;
          tt_parList_constructor: case cTokType[1] of
            tt_braceClose: begin // <F> <par(> | <Lit> ) -> <F> <par>
              P_listLiteral(stack.dat[stack.topIndex]^.data)^.append(first^.data,true);
              stack.dat[stack.topIndex]^.tokType:=tt_parList; //mutate <tt_parList_constructor> -> <tt_parList>
              first:=context.recycler.disposeToken(first); //dispose literal
              first:=context.recycler.disposeToken(first); //dispose closing bracket
              stack.popLink(first); //pop parameter list
              stack.popLink(first); //pop FUNCTION
              didSubstitution:=true;
            end;
            tt_separatorComma: begin // <F> <par(> | <Lit> , -> <F> <par(> |
              P_listLiteral(stack.dat[stack.topIndex]^.data)^.append(first^.data,true);
              first:=context.recycler.disposeToken(first);
              first:=context.recycler.disposeToken(first);
              didSubstitution:=true;
            end;
            tt_separatorCnt:   context.adapters^.raiseError('Token .. is only allowed in list constructors.',first^.next^.location);
            COMMON_SEMICOLON_HANDLING;
            COMMON_CASES;
          end;
          tt_mutate: case cTokType[1] of
            tt_semicolon: if (cTokType[-1] in [tt_beginBlock,tt_beginRule,tt_beginExpression]) and (cTokType[2]=C_compatibleEnd[cTokType[-1]])  then begin
              {$ifdef fullVersion}
              if (cTokType[-1] in [tt_beginRule,tt_beginExpression]) then context.callStackPop();
              {$endif}
              stack.popDestroy(context.recycler);
              first^.next:=context.recycler.disposeToken(first^.next);
              first^.next:=context.recycler.disposeToken(first^.next);
              context.valueStore^.scopePop;
              didSubstitution:=true;
            end else begin
              stack.popLink(first);
              applyMutation;
            end;
            tt_braceClose,tt_separatorCnt,tt_separatorComma,tt_EOL,tt_expBraceClose,tt_listBraceClose: begin
              stack.popLink(first);
              applyMutation;
            end;
            COMMON_CASES;
          end;
          tt_assignNewBlockLocal, tt_assignExistingBlockLocal,tt_mut_nested_assign..tt_mut_nestedDrop: case cTokType[1] of
            tt_semicolon: if (cTokType[-1] in [tt_beginBlock,tt_beginRule,tt_beginExpression]) and (cTokType[2]=C_compatibleEnd[cTokType[-1]]) then begin
              first:=context.recycler.disposeToken(first);
              {$ifdef fullVersion}
              if (cTokType[-1] in [tt_beginRule,tt_beginExpression]) then context.callStackPop();
              {$endif}
              first^.next:=context.recycler.disposeToken(first^.next);
              first^.next:=context.recycler.disposeToken(first^.next);
              context.valueStore^.scopePop;
              didSubstitution:=true;
            end else begin
              stack.popLink(first);
              applyLocalAssignment(cTokType[-1]);
            end;
            tt_braceClose,tt_separatorCnt,tt_separatorComma,tt_EOL,tt_expBraceClose,tt_listBraceClose: begin
              stack.popLink(first);
              applyLocalAssignment(cTokType[-1]);
            end;
            COMMON_CASES;
          end;
          tt_return: case cTokType[1] of
            tt_semicolon: processReturnStatement;
            COMMON_CASES;
          end;
          else begin
            case cTokType[1] of
              COMMON_SEMICOLON_HANDLING;
              COMMON_CASES;
              FORBIDDEN_SEPARATORS;
            end;
          end;
        end;
{cT[0]=}tt_beginBlock,tt_beginRule,tt_beginExpression: begin
          context.valueStore^.scopePush(cTokType[0]=tt_beginRule);
          stack.push(first);
          didSubstitution:=true;
        end;
{cT[0]=}tt_assignNewBlockLocal, tt_assignExistingBlockLocal,tt_mut_nested_assign..tt_mut_nestedDrop: begin
          stack.push(first);
          didSubstitution:=true;
        end;
{cT[0]=}tt_blockLocalVariable: if cTokType[1]=tt_listBraceOpen then begin
          stack.push(first);
          didSubstitution:=true;
        end else begin
          first^.data:=context.valueStore^.getVariableValue(first^.txt);
          if first^.data<>nil then begin
            first^.tokType:=tt_literal;
            didSubstitution:=true;
          end else context.adapters^.raiseError('Cannot find value for local id "'+first^.txt+'"',errorLocation);
        end;
{cT[0]=}tt_operatorPlus:                 begin first^.tokType:=tt_unaryOpPlus;  stack.push(first); didSubstitution:=true; end;
{cT[0]=}tt_operatorMinus:                begin first^.tokType:=tt_unaryOpMinus; stack.push(first); didSubstitution:=true; end;
{cT[0]=}tt_unaryOpPlus, tt_unaryOpMinus: begin                                  stack.push(first); didSubstitution:=true; end;
{cT[0]=}tt_comparatorEq..tt_operatorLazyOr,
        tt_operatorMult..tt_operatorConcatAlt:
          context.adapters^.raiseError('Undefined prefix operator '+first^.singleTokenToString,errorLocation);
{cT[0]=}tt_braceOpen: begin stack.push(first); didSubstitution:=true; end;
{cT[0]=}tt_expBraceOpen: begin
          digestInlineExpression(first,context);
          didSubstitution:=true;
        end;
{cT[0]=}tt_braceClose: if cTokType[-1]=tt_parList_constructor then begin
          first:=context.recycler.disposeToken(first);
          stack.popLink(first);
          first^.tokType:=tt_parList;
          stack.popLink(first);
          didSubstitution:=true;
        end;
{cT[0]=}tt_listBraceOpen: if cTokType[1]=tt_listBraceClose then begin
          //empty list
          first^.data:=newListLiteral;
          first^.tokType:=tt_literal;
          first^.next:=context.recycler.disposeToken(first^.next);
          didSubstitution:=true;
        end else begin
          first^.data:=newListLiteral;
          first^.tokType:=tt_list_constructor;
          stack.push(first);
          didSubstitution:=true;
        end;
{cT[0]=}tt_list_constructor, tt_list_constructor_ranging: begin stack.push(first); didSubstitution:=true; end;
{cT[0]=}tt_identifier: begin
          P_abstractPackage(first^.location.package)^.resolveId(first^,context.adapters);
          didSubstitution:=true;
        end;
        tt_parameterIdentifier: begin
          context.resolveMainParameter(first);
          didSubstitution:=true;
        end;
{cT[0]=}tt_mutate: begin stack.push(first); didSubstitution:=true; end;
{cT[0]=}tt_aggregatorConstructor: case cTokType[1] of
          tt_braceOpen, tt_parList_constructor, tt_listToParameterList: begin
            if ((cTokType[2] in C_operatorsForAggregators) or (cTokType[2]=tt_intrinsicRule)) and
               (first^.next^.next^.next<>nil) and
               (first^.next^.next^.next^.tokType=tt_braceClose) then begin
              // || aggregator ( + )
              first^.tokType:=tt_aggregatorExpressionLiteral;
              first^.data:=createPrimitiveAggregatorLiteral(first^.next^.next,context);
              first^.next:=context.recycler.disposeToken(first^.next); //drop (
              first^.next:=context.recycler.disposeToken(first^.next); //drop +
              first^.next:=context.recycler.disposeToken(first^.next); //drop )
              didSubstitution:=true;
            end else startOrPushParameterList;
          end;
          tt_parList: if (cTokType[2]=tt_listToParameterList) then begin
            stack.push(first);
            stack.push(first);
            stack.push(first);
            didSubstitution:=true;
          end else applyRule(first^.next,first^.next^.next);
        end;

{cT[0]=}tt_localUserRule, tt_importedUserRule, tt_customTypeRule, tt_intrinsicRule, tt_rulePutCacheValue: case cTokType[1] of
          tt_braceOpen, tt_parList_constructor, tt_listToParameterList: startOrPushParameterList;
          tt_listBraceOpen: begin
            if (cTokType[0] in [tt_localUserRule,tt_importedUserRule]) and
               (P_rule(first^.data)^.getRuleType in [rt_datastore,rt_mutable]) then begin
              stack.push(first);
              didSubstitution:=true;
            end else applyRule(nil,first^.next);
          end;
          tt_parList: if (cTokType[2]=tt_listToParameterList) then begin
            stack.push(first);
            stack.push(first);
            stack.push(first);
            didSubstitution:=true;
          end else applyRule(first^.next,first^.next^.next);
          tt_braceClose,tt_listBraceClose,tt_comparatorEq..tt_operatorConcatAlt,tt_EOL,tt_iifCheck,tt_iifElse,tt_separatorCnt,tt_separatorComma,tt_semicolon,
          tt_ponFlipper, tt_each,tt_parallelEach: applyRule(nil,first^.next);
        end;
{cT[0]=}tt_while: if (cTokType[1]=tt_braceOpen) then begin
          first^.next:=context.recycler.disposeToken(first^.next);
          resolveWhile;
        end;
{cT[0]=}tt_iifCheck: if (cTokType[-1]=tt_literal) then begin
          if (P_literal(stack.dat[stack.topIndex]^.data)^.literalType=lt_boolean)
          then resolveInlineIf(P_boolLiteral(stack.dat[stack.topIndex]^.data)^.value)
          else context.adapters^.raiseError('Invalid syntax for inline-if; first operand is expected to be a boolean. Instead I found a '+P_literal(stack.dat[stack.topIndex]^.data)^.typeString+': '+stack.dat[stack.topIndex]^.singleTokenToString,errorLocation);
        end else context.adapters^.raiseError('Invalid syntax for inline-if; first operand is expected to be a boolean. Here, the first operand is not even a literal.',errorLocation);
        tt_pseudoFuncPointer: case cTokType[1] of
          tt_localUserRule, tt_importedUserRule, tt_customTypeRule, tt_intrinsicRule: resolvePseudoFuncPointer;
          low(intFuncForOperator)..high(intFuncForOperator): begin
            first^.data:=createPrimitiveAggregatorLiteral(first^.next,context);
            first^.tokType:=tt_literal;
            first^.next:=context.recycler.disposeToken(first^.next);
            didSubstitution:=true;
          end;
        end;
{cT[0]=}tt_save: if cTokType[1]=tt_semicolon then begin
          first:=context.recycler.disposeToken(first);
          first:=context.recycler.disposeToken(first);
          didSubstitution:=true;
        end;
{cT[0]=}tt_return: begin
          stack.push(first);
          didSubstitution:=true;
        end;
      end;
    until not(didSubstitution) or not(context.adapters^.noErrors);
    {$ifndef DEBUGMODE}
    except
      on e:Exception do begin
        context.adapters^.raiseSystemError('An unhandled, exception was caught in reduceExpression on callDepth='+intToStr(context.callDepth));
        if first<>nil then context.adapters^.raiseSystemError(e.message,errorLocation)
                      else context.adapters^.raiseSystemError(e.message);
      end;
    end;
    {$endif}
    dec(context.callDepth);
    if context.adapters^.noErrors then begin
      if (stack.topIndex>=0) or (first<>nil) and (first^.next<>nil) then begin
        context.adapters^.raiseError('Irreducible expression: '+stack.toString(first,100),errorLocation);
        {$ifdef fullVersion}
        if not(context.adapters^.hasStackTrace) then context.callStackPrint;
        {$endif}
        cleanupStackAndExpression;
      end;
    end else if (context.adapters^.hasFatalError) then begin
      {$ifdef fullVersion}
      if not(context.adapters^.hasHaltMessage) and
         not(context.adapters^.hasStackTrace) then context.callStackPrint;
      context.callStackClear;
      {$endif}
      while (stack.topIndex>=0) do stack.popDestroy(context.recycler);
      if (context.callDepth=0) then context.recycler.cascadeDisposeToken(first);
    end else begin
      {$ifdef fullVersion}
      if not(context.adapters^.hasStackTrace) then context.callStackPrint;
      {$endif}
      cleanupStackAndExpression;
    end;
    stack.destroy;
  end;

TYPE
  P_asyncTask=^T_asyncTask;
  T_asyncTask=record
    payload:P_futureLiteral;
    myContext:P_threadContext;
  end;

FUNCTION doAsync(p:pointer):ptrint;
  begin
    result:=0;
    with P_asyncTask(p)^ do begin
      payload^.executeInContext(myContext);
      myContext^.doneEvaluating;

      disposeLiteral(payload);
      dispose(myContext,destroy);
      myContext:=nil;
    end;
    freeMem(p,sizeOf(T_asyncTask));
  end;

{$i mnh_func_defines.inc}
//FUNCTION asyncOrFuture(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext; CONST wantFuture:boolean):P_literal;
//  VAR p:P_futureLiteral;
//      childContext:P_threadContext;
//      parameters:P_listLiteral=nil;
//  begin
//    result:=nil;
//    if (params^.size>=1) and (arg0^.literalType=lt_expression) and
//       ((params^.size=1) or (params^.size=2) and (arg1^.literalType in C_listTypes)) then begin
//      childContext:=context.getNewAsyncContext;
//      if childContext<>nil then begin
//        if params^.size=2 then parameters:=list1;
//        new(p,create(P_expressionLiteral(arg0),parameters,childContext,tokenLocation,wantFuture));
//        beginThread(@doAsync,p);
//        result:=p^.rereferenced;
//      end else begin
//        context.adapters^.raiseError('Creation of asynchronous/future tasks is forbidden for the current context',tokenLocation);
//      end;
//    end;
//  end;

FUNCTION async_imp intFuncSignature;
  VAR payload:P_futureLiteral;
      task:P_asyncTask;
      childContext:P_threadContext;
      parameters:P_listLiteral=nil;
  begin
    result:=nil;
    if (params^.size>=1) and (arg0^.literalType=lt_expression) and
       ((params^.size=1) or (params^.size=2) and (arg1^.literalType in C_listTypes)) then begin
      childContext:=context.getNewAsyncContext;
      if childContext<>nil then begin
        if params^.size=2 then parameters:=list1;
        new(payload,create(P_expressionLiteral(arg0),parameters,tokenLocation,{blocking=}false));
        getMem(task,sizeOf(T_asyncTask));
        task^.myContext:=childContext;
        task^.payload  :=payload;
        beginThread(@doAsync,task);
        result:=payload^.rereferenced;
      end else begin
        context.adapters^.raiseError('Creation of asynchronous/future tasks is forbidden for the current context',tokenLocation);
      end;
    end;
  end;

FUNCTION future_imp intFuncSignature;
  VAR future:P_futureLiteral;
      parameters:P_listLiteral=nil;
  begin
    result:=nil;
    if (params^.size>=1) and (arg0^.literalType=lt_expression) and
       ((params^.size=1) or (params^.size=2) and (arg1^.literalType in C_listTypes)) then begin
      if params^.size=2 then parameters:=list1;
      new(future,create(P_expressionLiteral(arg0),parameters,tokenLocation,{blocking=}true));
      if (tco_spawnWorker in context.threadOptions) then begin
        future^.rereference;
        enqueueFutureTask(future,context);
      end;
      result:=future;
    end;
  end;

INITIALIZATION
  reduceExpressionCallback:=@reduceExpression;
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'async',@async_imp,[se_detaching],ak_variadic_1,
               'async(E:expression);//Calls E asynchronously (without parameters) and returns an expression to access the result.#'+
               'async(E:expression,par:list);//Calls E@par and asynchronously and returns an expression to access the result.#//Asynchronous tasks are killed at the end of (synchonous) evaluation.#//The resulting expression returns void until the task is finished.');
  registerRule(SYSTEM_BUILTIN_NAMESPACE,'future',@future_imp,[se_detaching],ak_variadic_1,
               'future(E:expression);//Calls E asynchronously (without parameters) and returns an expression to access the result.#'+
               'future(E:expression,par:list);//Calls E@par and asynchronously and returns an expression to access the result.#//Future tasks are killed at the end of (synchonous) evaluation.#//The resulting expression blocks until the task is finished.');
end.
