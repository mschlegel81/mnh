UNIT evaluation;
INTERFACE
{$ifndef debugMode}
  {$define useTryCatchBlocks}
{$endif}

IMPLEMENTATION
USES sysutils,
     Classes,
     myGenerics,
     mySys,
     {$ifdef useTryCatchBlocks}
     myStringUtil,
     {$endif}
     {$ifdef fullVersion}
     debuggingVar,
     {$endif}
     mnh_constants,basicTypes,
     litVar,
     tokens,
     tokenArray,
     tokenStack,
     mnh_messages,
     mnh_settings,
     valueStore,
     contexts,
     funcs,
     operators,
     funcs_math,
     funcs_list,
     funcs_types,
     funcs_format,
     subrules,
     rules,
     aggregators,
     recyclers,
     listProcessing;

FUNCTION reduceExpression(VAR first:P_token; CONST context:P_context; CONST recycler:P_recycler):T_reduceResult;
  VAR stack:T_TokenStack;
      newLit:P_literal;
      didSubstitution:boolean;
      cTokType:array[-1..2] of T_tokenType;

  PROCEDURE initTokTypes; {$ifndef profilingFlavour}{$ifndef debugMode}inline;{$endif}{$endif}
    begin
      if stack.top<>nil then cTokType[-1]:=stack.top^.tokType
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
      if preferredToken<>nil then exit(preferredToken^.location);
      if first         <>nil then exit(first         ^.location);
      if stack.top     <>nil then exit(stack.top     ^.location);
      result:=C_nilSearchTokenLocation;
    end;

  PROCEDURE raiseLazyBooleanError(CONST location:T_tokenLocation; CONST LHS:P_literal);
    begin
      result:=rr_fail;
      context^.raiseError('Lazy boolean operators can only be applied to scalar booleans. Got '+LHS^.typeString,location);
    end;

  PROCEDURE processReturnStatement;
    VAR returnToken:P_token;
        level:longint=1;
    begin
      returnToken:=first; //result is first (tt_literal);
      first:=recycler^.disposeToken(first^.next); //drop semicolon
      while not(level=0) and (context^.messages^.continueEvaluation) do begin
        while (stack.top<>nil) and not(stack.top^.tokType in
                                    [tt_beginRule,tt_beginExpression,tt_beginBlock,
                                     tt_endRule  ,tt_endExpression  ,tt_endBlock,tt_EOL]) do stack.top:=recycler^.disposeToken(stack.top);
        while (first<>nil) and
              not(first^.tokType in [tt_beginRule,tt_beginExpression,tt_beginBlock,
                                     tt_endRule  ,tt_endExpression  ,tt_endBlock,tt_EOL]) do first:=recycler^.disposeToken(first);
        if first=nil then begin
          if level=1
          then level:=0
          else context^.raiseError('Invalid stack state (processing return statement) - empty stack',errorLocation);
        end else case first^.tokType of
          tt_beginBlock:            begin stack.push(first); recycler^.scopePush(context^.valueScope); end;
          tt_beginExpression,
          tt_beginRule: begin inc(level); stack.push(first); recycler^.scopePush(context^.valueScope); end;
          tt_endBlock:
            if (stack.top<>nil) and (stack.top^.tokType=tt_beginBlock)
            then begin
              context^.valueScope^.checkVariablesOnPop(recycler,first^.location,context);
              recycler^.scopePop(context^.valueScope);
              stack.top:=recycler^.disposeToken(stack.top);
              first:=recycler^.disposeToken(first);
            end else context^.raiseError('Invalid stack state (processing return statement) - begin/end mismatch (endBlock)',errorLocation);
          tt_endExpression,tt_endRule:
            if (stack.top<>nil) and (stack.top^.tokType=C_compatibleBegin[first^.     tokType])
            then begin
              {$ifdef fullVersion} context^.callStackPop(returnToken); {$endif}
              context^.valueScope^.checkVariablesOnPop(recycler,first^.location,context);
              recycler^.scopePop(context^.valueScope);
              stack.top:=recycler^.disposeToken(stack.top);
              first:=recycler^.disposeToken(first);
              dec(level);
            end else context^.raiseError('Invalid stack state (processing return statement) - begin/end mismatch (endRule)',first^.location);
          else context^.raiseError('Invalid stack state (processing return statement) - WTF?',first^.location);
        end;
      end;
      if first=nil then result:=rr_okWithReturn;
      returnToken^.next:=first;
      first:=returnToken;
      didSubstitution:=true;
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
          context^.raiseError('Invalid agg-construct: aggregator is missing',eachToken^.location);
          exit(false);
        end;
        //find closing bracket and body parts
        bodyParts:=getBodyParts(eachToken,0,context,bracketClosingEach);
        if (bracketClosingEach=nil) or not(context^.messages^.continueEvaluation) then exit(false);
        if (length(bodyParts)>1) and isPureAggregator then begin
          context^.raiseError('Invalid agg-construct: argument must be an aggregator or aggregator prototype.',eachToken^.location);
          exit(false);
        end;

        //process aggregator part (if any)----------------------------------------------
        lastPart:=length(bodyParts)-1;
        t:=bodyParts[lastPart].first;
        if (t^.tokType=tt_aggregatorConstructor) or (t^.tokType=tt_pseudoFuncPointer) and isPureAggregator then begin
          reduceExpression(bodyParts[lastPart].first,context,recycler);
          //Handle error in evaluation of aggregator
          if bodyParts[lastPart].first=nil then setLength(bodyParts,lastPart) else begin
            bodyParts[lastPart].last:=bodyParts[lastPart].first^.last;
            t:=bodyParts[lastPart].first;
          end;
        end;
        if t<>nil then
        if t^.next=nil then begin
          case t^.tokType of
            tt_comparatorEq..tt_operatorConcatAlt: aggregator:=newAggregator(t^.tokType,recycler);
            tt_aggregatorExpressionLiteral: aggregator:=newCustomAggregator(P_expressionLiteral(t^.data),t^.location,context);
            tt_literal: if isPureAggregator and (P_literal(t^.data)^.literalType=lt_expression)
              then aggregator:=newCustomAggregator(P_expressionLiteral(t^.data),t^.location,context)
              else if isPureAggregator then context^.raiseError('Invalid agg-construct: argument must be an aggregator or aggregator prototype.',eachToken^.location);
            tt_intrinsicRule:
              if (P_intFuncCallback(t^.data)=BUILTIN_MIN)      then aggregator:=newMinAggregator      else
              if (P_intFuncCallback(t^.data)=BUILTIN_MAX)      then aggregator:=newMaxAggregator      else
              if (P_intFuncCallback(t^.data)=BUILTIN_TRAILING) then aggregator:=newTrailingAggregator else
              if (P_intFuncCallback(t^.data)=BUILTIN_TOSET)    then aggregator:=newSetAggregator (recycler) else
              if (P_intFuncCallback(t^.data)=BUILTIN_TOLIST)   then aggregator:=newListAggregator(recycler) else
              if (P_intFuncCallback(t^.data)=BUILTIN_HEAD)     then aggregator:=newHeadAggregator     else
              if (P_intFuncCallback(t^.data)=BUILTIN_ELEMENT_FREQUENCY) then aggregator:=newElementFrequencyAggregator;
          end;
        end else if isPureAggregator then begin
          if t^.tokType in [tt_expBraceOpen,tt_functionPattern] then begin
            digestInlineExpression(t,context,recycler);
            if context^.messages^.continueEvaluation
            then aggregator:=newCustomAggregator(P_expressionLiteral(t^.data),t^.location,context);
          end else context^.raiseError('Invalid agg-construct: argument must be an aggregator or aggregator prototype.',eachToken^.location);
        end;
        result:=context^.continueEvaluation;
        aggregatorPresent:=(aggregator<>nil);
        if aggregatorPresent then begin
          recycler^.disposeToken(bodyParts[lastPart].first);
          setLength(bodyParts,length(bodyParts)-1);
        end else begin
          aggregator:=newListAggregator(recycler);
          if isPureAggregator then begin
            result:=false;
            context^.raiseError('Invalid agg-construct: aggregator is missing.',eachToken^.location);
            exit(result);
          end;
        end;
        //----------------------------------------------process aggregator part (if any)
        //process other body parts (if any)---------------------------------------------
        setLength(bodyRule,length(bodyParts));
        for i:=0 to length(bodyParts)-1 do
          new(P_inlineExpression(bodyRule[i]),createForEachBody(eachToken^.txt,bodyParts[i].first,eachToken^.location,context,recycler));
        //---------------------------------------------process other body parts (if any)
      end;

    VAR input:P_literal;
        i:longint;
        eachLocation:T_tokenLocation;

    PROCEDURE finalizeAggregation;
      begin
        first^.data:=aggregator^.getResult(recycler);
        first^.txt:='';
        first^.next:=recycler^.disposeToken(bracketClosingEach);
        if aggregator^.hasReturn then processReturnStatement;
        aggregator^.cleanup(recycler);
        dispose(aggregator,destroy);
      end;

    begin
      if (eachType=tt_parallelEach) and
         ((context^.callDepth>=STACK_DEPTH_LIMIT-16) or
         not(tco_spawnWorker in context^.threadOptions) or
         (settings.cpuCount<=1) or
         not(memoryCleaner.isMemoryInComfortZone))
      then eachType:=tt_each;
      eachLocation:=first^.next^.location;
      initialize(bodyRule);

      if (first^.data=nil) or (P_literal(first^.data)^.literalType=lt_void) then begin
        context^.raiseError('Cannot apply each construct to void literal',eachLocation);
        exit
      end;
      if context^.callDepth>STACK_DEPTH_LIMIT then begin
        {$ifdef debugMode}
        raise Exception.create('Stack overflow in (p)each construct.');
        {$else}
        context^.raiseError('Stack overflow in (p)each construct.',eachLocation,mt_el4_systemError);
        {$endif}
        exit;
      end;
      if not(parseBodyOk) then exit;
      input:=P_literal(first^.data);
      first^.next:=recycler^.disposeToken(first^.next);
      //iterate over itList----------------------------------------------------------
      if length(bodyRule)>0 then begin
        if eachType = tt_parallelEach
        then processListParallel(input,bodyRule,aggregator,eachLocation,context,recycler)
        else processListSerial  (input,bodyRule,aggregator,eachLocation,context,recycler);
      end else begin
        if eachType = tt_parallelEach then context^.messages^.postTextMessage(mt_el1_note,eachLocation,'There is no paralellization for pEach statements without body (i.e. pure aggregators)');
        aggregate(input,aggregator,eachLocation,context,recycler);
      end;
      //----------------------------------------------------------iterate over itList
      //cleanup----------------------------------------------------------------------
      finalizeAggregation;
      recycler^.disposeLiteral(input);
      for i:=0 to length(bodyRule)-1 do begin
        bodyRule[i]^.cleanup(recycler);
        dispose(bodyRule[i],destroy);
      end;
      //----------------------------------------------------------------------cleanup
      didSubstitution:=context^.continueEvaluation;
    end;

  PROCEDURE resolveFor;
    VAR aggregator: P_aggregator=nil;
        closingToken:P_token;
    PROCEDURE finalizeAggregation;
      begin
        first^.data:=aggregator^.getResult(recycler);
        first^.txt:='';
        first^.next:=closingToken;
        if aggregator^.hasReturn then processReturnStatement;
        aggregator^.cleanup(recycler);
        dispose(aggregator,destroy);
      end;

    VAR sourceLiteral:P_literal;
        bodyRule:T_expressionList;
        p, bodyRuleStart, prev:P_token;
        bracketLevel:longint=0;
        i: integer;
        parallel:boolean;
    begin
      sourceLiteral:=first^.next^.data;
      //first   = for
      // next   = literal
      //  next  = do        - disposed at (**)
      //   next = body...
      setLength(bodyRule,0);

      prev:=first^.next;
      p:=   prev^.next;
      assert(p^.tokType=tt_do); //ensured by condition before call
      parallel:=(p^.getDoType=dt_for_related_do_parallel);
      prev^.next:=nil;
      repeat
        p:=recycler^.disposeToken(p); (**)
        bodyRuleStart:=p;
        while (p<>nil) and (bracketLevel>=0) and not(((p^.tokType in [tt_aggregatorConstructor,tt_aggregatorExpressionLiteral,tt_semicolon]) or (p^.tokType=tt_do) and (p^.getDoType in [dt_for_related_do_parallel,dt_for_related_do])) and (bracketLevel=0)) do begin
          if      (p^.tokType in C_openingBrackets) then inc(bracketLevel)
          else if (p^.tokType in C_closingBrackets) then dec(bracketLevel);
          prev:=p;
          p:=p^.next;
        end;
        closingToken:=p;
        prev^.next:=nil;
        setLength(bodyRule,length(bodyRule)+1);
        new(P_inlineExpression(bodyRule[length(bodyRule)-1]),createForEachBody(first^.txt,bodyRuleStart,first^.location,context,recycler));
      until (p=nil) or (p^.tokType<>tt_do) or not(p^.getDoType in [dt_for_related_do,dt_for_related_do_parallel]);

      if (p<>nil) and (p^.tokType=tt_aggregatorConstructor) then begin
        bodyRuleStart:=p;
        while (p<>nil) and (bracketLevel>=0) do begin
          if      (p^.tokType in C_openingBrackets) then inc(bracketLevel)
          else if (p^.tokType in C_closingBrackets) then dec(bracketLevel);
          prev:=p;
          p:=p^.next;
        end;
        closingToken:=p;
        prev^.next:=nil;
        reduceExpression(bodyRuleStart,context,recycler);
        p:=bodyRuleStart;
      end;
      if not context^.continueEvaluation then exit;

      if (p<>nil) and (p^.tokType=tt_aggregatorExpressionLiteral)
      then aggregator:=newCustomAggregator(P_expressionLiteral(p^.data),p^.location,context)
      else aggregator:=newListAggregator(recycler);

      // Syntax: for ... in ... do parallel ... ?
      if parallel
      then processListParallel(sourceLiteral,bodyRule,aggregator,first^.location,context,recycler)
      else processListSerial  (sourceLiteral,bodyRule,aggregator,first^.location,context,recycler);

      //cleanup----------------------------------------------------------------------
      first:=recycler^.disposeToken(first);
      finalizeAggregation;
      recycler^.disposeLiteral(sourceLiteral);
      for i:=0 to length(bodyRule)-1 do begin
        bodyRule[i]^.cleanup(recycler);
        dispose(bodyRule[i],destroy);
      end;
      didSubstitution:=context^.continueEvaluation;
      //----------------------------------------------------------------------cleanup
    end;

  PROCEDURE resolveRepeat;
    VAR bodyRule:P_inlineExpression=nil;
        conditionRule:P_inlineExpression=nil;

    FUNCTION parseBodyOk:boolean;
      VAR bracketLevel:longint=0;
          firstTokenOfHead,
          firstTokenOfCondition,
          lastTokenOfHead:P_token;
          statementsInBody:longint=1;
          p,prev:P_token;
      begin
        result:=false;
        firstTokenOfHead:=first^.next;
        firstTokenOfCondition     :=nil;
        prev:=first;
        p:=firstTokenOfHead;
        //Error case: repeat until ...
        if p^.tokType=tt_until then begin
          context^.raiseError('Invalid repeat-construct.',errorLocation);
          exit(false);
        end;

        while (p<>nil) and not((p^.tokType =tt_until) and (bracketLevel=0)) do begin
          if      (p^.tokType in C_openingBrackets) then inc(bracketLevel)
          else if (p^.tokType in C_closingBrackets) then dec(bracketLevel);
          if (bracketLevel=0) and (p^.tokType=tt_semicolon) then inc(statementsInBody);
          prev:=p;
          p:=p^.next;
        end;

        if (p<>nil) and (p^.tokType=tt_until) then begin
          //"normal" case: repeat <condition> until ...
          lastTokenOfHead:=prev;
          p:=p^.next;
          firstTokenOfCondition:=p;
          while (p<>nil) and not((p^.tokType = tt_semicolon) and (bracketLevel=0)) do begin
            if      (p^.tokType in C_openingBrackets) then inc(bracketLevel)
            else if (p^.tokType in C_closingBrackets) then dec(bracketLevel);
            prev:=p;
            p:=p^.next;
          end;
        end else begin
          context^.raiseError('Invalid repeat-construct.',errorLocation);
          exit(false);
        end;

        if firstTokenOfCondition<>nil then begin
          //dipose do:
          lastTokenOfHead^.next:=recycler^.disposeToken(lastTokenOfHead^.next);
          prev^.next:=nil;
          new(conditionRule,createForWhile(firstTokenOfCondition,firstTokenOfCondition^.location,context,recycler));
        end;
        //Unlink head:
        assert(p<>nil);
        assert(p^.tokType=tt_semicolon);
        first^.next:=p;
        lastTokenOfHead^.next:=nil;

        if statementsInBody>1 then begin
          //Implicitly transform  repeat ... until ... -> repeat begin ... end until ...
          p:=recycler^.newToken(firstTokenOfHead^.location,C_tokenDefaultId[tt_beginBlock],tt_beginBlock);
          p^.next:=firstTokenOfHead;
          firstTokenOfHead:=p;
          lastTokenOfHead^.next:=recycler^.newToken(lastTokenOfHead^.location,C_tokenDefaultId[tt_endBlock],tt_endBlock);
        end;

        new(bodyRule,createForWhile(firstTokenOfHead,firstTokenOfHead^.location,context,recycler));

        result:=true;
      end;

    VAR repeatLocation:T_tokenLocation;
        returnValue:T_evaluationResult;
    PROCEDURE evaluateBody;
      VAR toReduce:T_tokenRange;
      begin
        if (bodyRule<>nil) and bodyRule^.matchesPatternAndReplaces(nil,repeatLocation,toReduce,context,recycler) then begin
          if reduceExpression(toReduce.first,context,recycler)=rr_okWithReturn then begin
            returnValue.literal:=P_literal(toReduce.first^.data)^.rereferenced;
            returnValue.reasonForStop:=rr_okWithReturn;
          end;
          recycler^.cascadeDisposeToken(toReduce.first);
        end;
      end;

    begin
      returnValue:=NIL_EVAL_RESULT;
      repeatLocation:=first^.location;
      if not(parseBodyOk) then exit;
      {$ifdef fullVersion}
      if tco_stackTrace in context^.threadOptions then context^.callStackPush(repeatLocation,'while',stepForward(repeatLocation,6),nil);
      {$endif}
      repeat evaluateBody
      until (returnValue.reasonForStop<>rr_ok) or
            evaluateToBoolean_strict(conditionRule,conditionRule^.getLocation,context,recycler) or
            not(context^.continueEvaluation);

      first^.txt:='';
      first^.tokType:=tt_literal;
      first^.data:=newVoidLiteral;
      while (first^.next      <>nil) and (first^.next^      .tokType=tt_semicolon) and
            (first^.next^.next<>nil) and (first^.next^.next^.tokType=tt_semicolon)
      do first^.next:=recycler^.disposeToken(first^.next);

      //cleanup----------------------------------------------------------------------
      bodyRule^.cleanup(recycler);      dispose(bodyRule,destroy);
      conditionRule^.cleanup(recycler); dispose(conditionRule,destroy);
      //----------------------------------------------------------------------cleanup
      if returnValue.reasonForStop=rr_okWithReturn then begin
        recycler^.disposeLiteral(first^.data);
        first^.data:=returnValue.literal;
        processReturnStatement;
      end;
      {$ifdef fullVersion}
      if tco_stackTrace in context^.threadOptions
      then context^.callStackPop(nil);
      {$endif}

      didSubstitution:=true;
    end;

  PROCEDURE resolveWhile;
    VAR headRule:P_inlineExpression=nil;
        bodyRule:P_inlineExpression=nil;

    FUNCTION parseBodyOk:boolean;
      VAR bracketLevel:longint=0;
          firstTokenOfCondition,
          firstTokenOfBody,
          lastTokenOfCondition:P_token;

          p,prev:P_token;
      begin
        result:=false;
        firstTokenOfCondition:=first^.next;
        firstTokenOfBody     :=nil;
        prev:=first;
        p:=firstTokenOfCondition;
        //Error case: while do ...
        if p^.tokType=tt_do then begin
          context^.raiseError('Invalid while-construct; Empty entry condition.',errorLocation);
          exit(false);
        end;

        while (p<>nil) and not(((p^.tokType=tt_do) and (p^.getDoType=dt_while_related_do) or (p^.tokType=tt_semicolon)) and (bracketLevel=0)) do begin
          if      (p^.tokType in C_openingBrackets) then inc(bracketLevel)
          else if (p^.tokType in C_closingBrackets) then dec(bracketLevel);
          prev:=p;
          p:=p^.next;
        end;

        if (p<>nil) and (p^.tokType=tt_do) and (p^.getDoType=dt_while_related_do) then begin
          //"normal" case: while <condition> do ...
          lastTokenOfCondition:=prev;
          p:=p^.next;
          firstTokenOfBody:=p;
          while (p<>nil) and not((p^.tokType = tt_semicolon) and (bracketLevel=0)) do begin
            if      (p^.tokType in C_openingBrackets) then inc(bracketLevel)
            else if (p^.tokType in C_closingBrackets) then dec(bracketLevel);
            prev:=p;
            p:=p^.next;
          end;
        end else if (p<>nil) and (p^.tokType=tt_semicolon) then begin
          //short case: while <condition> ;
          lastTokenOfCondition:=prev;
        end else begin
          context^.raiseError('Invalid while-construct; One or two arguments (head only or head + body) are expected.',errorLocation);
          exit(false);
        end;

        if firstTokenOfBody<>nil then begin
          //dipose do:
          lastTokenOfCondition^.next:=recycler^.disposeToken(lastTokenOfCondition^.next);
          prev^.next:=nil;
          new(bodyRule,createForWhile(firstTokenOfBody,firstTokenOfBody^.location,context,recycler));
        end;
        //Unlink head:
        first^.next:=p;
        lastTokenOfCondition^.next:=nil;
        new(headRule,createForWhile(firstTokenOfCondition,firstTokenOfCondition^.location,context,recycler));

        result:=true;
      end;

    VAR whileLocation:T_tokenLocation;
        returnValue:T_evaluationResult;
    PROCEDURE evaluateBody;
      VAR toReduce:T_tokenRange;
      begin
        if (bodyRule<>nil) and bodyRule^.matchesPatternAndReplaces(nil,whileLocation,toReduce,context,recycler) then begin
          if reduceExpression(toReduce.first,context,recycler)=rr_okWithReturn then begin
            returnValue.literal:=P_literal(toReduce.first^.data)^.rereferenced;
            returnValue.reasonForStop:=rr_okWithReturn;
          end;
          recycler^.cascadeDisposeToken(toReduce.first);
        end;
      end;

    begin
      returnValue:=NIL_EVAL_RESULT;
      whileLocation:=first^.location;
      if not(parseBodyOk) then exit;
      {$ifdef fullVersion}
      if tco_stackTrace in context^.threadOptions then context^.callStackPush(whileLocation,'while',stepForward(whileLocation,6),nil);
      {$endif}
      while (returnValue.reasonForStop=rr_ok)
            and evaluateToBoolean_strict(headRule,headRule^.getLocation,context,recycler)
            and (context^.continueEvaluation) do evaluateBody;
      first^.txt:='';
      first^.tokType:=tt_literal;
      first^.data:=newVoidLiteral;
      while (first^.next      <>nil) and (first^.next^      .tokType=tt_semicolon) and
            (first^.next^.next<>nil) and (first^.next^.next^.tokType=tt_semicolon)
      do first^.next:=recycler^.disposeToken(first^.next);

      //cleanup----------------------------------------------------------------------
      headRule^.cleanup(recycler);
      dispose(headRule,destroy);
      if bodyRule<>nil then begin
        bodyRule^.cleanup(recycler);
        dispose(bodyRule,destroy);
      end;
      //----------------------------------------------------------------------cleanup
      if returnValue.reasonForStop=rr_okWithReturn then begin
        recycler^.disposeLiteral(first^.data);
        first^.data:=returnValue.literal;
        processReturnStatement;
      end;
      {$ifdef fullVersion}
      if tco_stackTrace in context^.threadOptions
      then context^.callStackPop(nil);
      {$endif}

      didSubstitution:=true;
    end;

  PROCEDURE applyRule(CONST parameterListToken:P_token; CONST firstTokenAfterCall:P_token);
    VAR replace:T_tokenRange;
        newLiteral:P_literal;
        parameterListLiteral:P_listLiteral;
        inlineRule:P_inlineExpression;
    FUNCTION isNullaryArity(CONST arity:T_arityInfo):boolean;
      begin
        result:=(arity.minPatternLength=0) and (arity.maxPatternLength=0);
      end;

    begin
      if parameterListToken=nil
      then parameterListLiteral:=nil
      else parameterListLiteral:=parameterListToken^.data;
      case first^.tokType of
        tt_userRule: begin
          {$ifdef useTryCatchBlocks}
          try
          {$endif}
            if not(P_rule(first^.data)^.canBeApplied(first^.location,parameterListLiteral,replace,context,recycler)) then begin
              if (parameterListToken<>nil) and (parameterListLiteral<>nil) and (parameterListLiteral^.size>0) and isNullaryArity(P_rule(first^.data)^.arity) then begin
                if P_rule(first^.data)^.canBeApplied(first^.location,nil,replace,context,recycler)
                then begin
                  //Automatic uncurrying for nullary functions
                  replace.last^.next:=recycler^.newToken(parameterListToken^.location,'',parameterListToken^.tokType,parameterListLiteral^.rereferenced);
                  replace.last:=replace.last^.next;
                end else begin
                  context^.raiseCannotApplyError('user defined rule '+P_rule(first^.data)^.getId,parameterListLiteral,first^.location);
                  exit;
                end;
              end else begin
                context^.raiseCannotApplyError('user defined rule '+P_rule(first^.data)^.getId,parameterListLiteral,first^.location);
                exit;
              end;
            end;
          {$ifdef useTryCatchBlocks}
          except
            on e:Exception do begin
              context^.raiseError('Severe error trying to apply user defined rule '+P_rule(first^.data)^.getId+C_lineBreakChar+e.message+C_lineBreakChar+'Call depth: '+intToStr(context^.callDepth),first^.location);
              exit;
            end;
          end;
          {$endif}
        end;
        tt_rulePutCacheValue: begin
          newLiteral:=P_memoizedRule(first^.data)^.doPutCache(parameterListLiteral,recycler);
          replace.first:=recycler^.newToken(first^.location,'',tt_literal,newLiteral);
          replace.last:=replace.first;
        end;
        tt_aggregatorConstructor: begin
          if (parameterListLiteral<>nil) and (parameterListLiteral^.size=1) and
             (parameterListLiteral^.value[0]^.literalType=lt_expression) and
             (P_expressionLiteral(parameterListLiteral^.value[0])^.canApplyToNumberOfParameters(2) or
              P_expressionLiteral(parameterListLiteral^.value[0])^.canApplyToNumberOfParameters(1))
          then begin
            newLiteral:=parameterListLiteral^.value[0]^.rereferenced;
            replace.first:=recycler^.newToken(first^.location,'',tt_aggregatorExpressionLiteral,newLiteral);
            replace.last:=replace.first;
          end else begin
            context^.raiseError('Aggregators can only be constructed from expression literals!',errorLocation);
            exit;
          end;
        end;
        tt_intrinsicRule: begin
          {$ifdef useTryCatchBlocks}
          try
          {$endif}
          {$ifdef fullVersion}
          if tco_stackTrace in context^.threadOptions then begin
            context^.callStackPush(first^.location,builtinFunctionMap.getIntrinsicRuleAsExpression(P_intFuncCallback(first^.data),false),newCallParametersNode(parameterListLiteral));
            newLiteral:=P_intFuncCallback(first^.data)(parameterListLiteral,first^.location,context,recycler);
            context^.callStackPop(nil);
          end else
          {$endif}
          newLiteral:=P_intFuncCallback(first^.data)(parameterListLiteral,first^.location,context,recycler);
          {$ifdef useTryCatchBlocks}
          except
            on e:Exception do begin
              context^.raiseError('Severe error trying to apply builtin rule '+first^.txt+C_lineBreakChar+e.message+C_lineBreakChar+'Call depth: '+intToStr(context^.callDepth),first^.location);
              exit;
            end;
          end;
          {$endif}
          if newLiteral<>nil then begin
            replace.first:=recycler^.newToken(first^.location,'',tt_literal,newLiteral);
            replace.last:=replace.first;
          end else if not(context^.continueEvaluation) then exit else begin
            context^.raiseCannotApplyError('intrinsic rule '+first^.txt,parameterListLiteral,first^.location);
            exit;
          end;
        end;
        tt_literal,tt_aggregatorExpressionLiteral: if (P_literal(first^.data)^.literalType=lt_expression) then begin
          if P_expressionLiteral(first^.data)^.typ in C_builtinExpressionTypes then begin
            newLiteral:=P_expressionLiteral(first^.data)^.evaluate(first^.location,context,recycler,parameterListLiteral).literal;
            if newLiteral<>nil then begin
              replace.first:=recycler^.newToken(first^.location,'',tt_literal,newLiteral);
              replace.last:=replace.first;
            end else if not(context^.continueEvaluation) then exit else begin
              context^.raiseCannotApplyError('wrapped intrinsic rule '+first^.txt,parameterListLiteral,first^.location);
              exit;
            end;
          end else begin
            inlineRule:=first^.data;
            if not(inlineRule^.matchesPatternAndReplaces(parameterListLiteral,first^.location,replace,context,recycler)) then begin
              context^.raiseCannotApplyError('inline function '+inlineRule^.toString(20),parameterListLiteral,first^.location);
              exit;
            end;
          end
        end else begin
          context^.raiseError('Trying to apply a rule which is no rule! ('+P_literal(first^.data)^.typeString+')',errorLocation);
          exit;
        end;
        else begin
          context^.raiseError('Trying to apply a rule which is no rule!',errorLocation);
          exit;
        end;
      end;
      recycler^.disposeToken(first);
      if parameterListToken<>nil then recycler^.disposeToken(parameterListToken);
      first:=replace.first;
      replace.last^.next:=firstTokenAfterCall;
      didSubstitution:=true;
    end;

  PROCEDURE resolveInlineIf(CONST conditionLit:boolean);
    VAR p,prev:P_token;
        bracketLevel:longint=0;
        initialLocation:T_tokenLocation;
    begin
      stack.top:=recycler^.disposeToken(stack.top); //pop condition literal
      first:=recycler^.disposeToken(first); //dispose ?
      initialLocation:=first^.location;

      prev:=first;
      if conditionLit then begin
        //look up ":" and dispose everything after that
        if first^.tokType in C_openingBrackets then inc(bracketLevel);
        p:=first^.next;
        while (p<>nil) and (bracketLevel>=0) and not((p^.tokType=tt_iifElse) and (bracketLevel=0)) do begin
          if p^.tokType in      C_openingBrackets then inc(bracketLevel)
          else if p^.tokType in C_closingBrackets then dec(bracketLevel);
          prev:=p; p:=p^.next;
        end;
        prev^.next:=nil;
        //It is possible that there is no ":" as in : true ? 1;
        if (p<>nil) and (p^.tokType=tt_iifElse) and (bracketLevel=0) then begin
          while (p<>nil) and not((p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon]) and (bracketLevel=-1)) do begin
            if p^.tokType in      C_openingBrackets then inc(bracketLevel)
            else if p^.tokType in C_closingBrackets then dec(bracketLevel);
            p:=recycler^.disposeToken(p);
          end;
        end else begin
          context^.raiseError('Could not resolve inline-if; :-token could not be found',initialLocation);
          prev^.next:=p;
          exit;
        end;
        prev^.next:=p;
        didSubstitution:=true;
      end else begin
        //dispose everything up to :
        while (first<>nil) and (bracketLevel>=0) and not((first^.tokType=tt_iifElse) and (bracketLevel=0)) do begin
          if first^.tokType in      C_openingBrackets then inc(bracketLevel)
          else if first^.tokType in C_closingBrackets then dec(bracketLevel);
          first:=recycler^.disposeToken(first);
        end;
        if (first<>nil) and (first^.tokType=tt_iifElse) and (bracketLevel=0) then begin
          first:=recycler^.disposeToken(first);
          didSubstitution:=true;
        end else
          context^.raiseError('Could not resolve inline-if; :-token could not be found',initialLocation);
      end;
    end;

  PROCEDURE startOrPushParameterList;
    begin
      stack.push(first);
      if first^.tokType=tt_braceOpen then begin
        first^.tokType:=tt_parList_constructor;
        first^.data:=recycler^.newListLiteral;
      end;
      stack.push(first);
      didSubstitution:=true;
    end;

  PROCEDURE applyMutation;
    VAR newValue:P_literal;
    begin
      if not(context^.checkSideEffects('<mutation>',first^.location,[se_alterPackageState])) then exit;
      newValue:=first^.next^.data;
      {$ifdef fullVersion}
      if tco_stackTrace in context^.threadOptions then context^.callStackPush(first^.location,'mutate global',first^.next^.location,nil);
      {$endif}
      P_variable(first^.data)^.setMutableValue(newValue,false,recycler);
      {$ifdef fullVersion}
      if tco_stackTrace in context^.threadOptions then context^.callStackPop(nil);
      {$endif}
      first:=recycler^.disposeToken(first);
      didSubstitution:=true;
    end;

  PROCEDURE applyLocalAssignment(CONST kind:T_tokenType);
    VAR newValue:P_literal;
    begin
      newValue:=first^.next^.data;
      case kind of
        tt_assignNewBlockLocal: begin
          context^.valueScope^.createVariable(first^.txt,newValue,false);
          first:=recycler^.disposeToken(first);
        end;
        tt_assignExistingBlockLocal: begin
          context^.valueScope^.setVariableValue(recycler,first^.txt,newValue,first^.location,context);
          first:=recycler^.disposeToken(first);
        end;
        tt_mut_nested_assign..tt_mut_nestedDrop: if first^.data=nil then begin
          {$ifdef fullVersion}
          if tco_stackTrace in context^.threadOptions then context^.callStackPush(first^.location,'mutate local',first^.next^.location,nil);
          {$endif}
          newValue:=context^.valueScope^.mutateVariableValue(recycler,first^.txt,kind,newValue,first^.location,context,recycler);
          {$ifdef fullVersion}
          if tco_stackTrace in context^.threadOptions then context^.callStackPop(nil);
          {$endif}
          if context^.continueEvaluation then begin
            first:=recycler^.disposeToken(first);
            recycler^.disposeLiteral(first^.data);
            first^.data:=newValue;
          end;
        end else begin
          if not(context^.checkSideEffects('<mutation>',first^.location,[se_alterPackageState])) then exit;
          {$ifdef fullVersion}
          if tco_stackTrace in context^.threadOptions then context^.callStackPush(first^.location,'mutate global',first^.next^.location,nil);
          {$endif}
          newValue:=P_variable(first^.data)^.mutateInline(kind,newValue,first^.location,context,recycler);
          {$ifdef fullVersion}
          if tco_stackTrace in context^.threadOptions then context^.callStackPop(nil);
          {$endif}
          if context^.continueEvaluation then begin
            first:=recycler^.disposeToken(first);
            recycler^.disposeLiteral(first^.data);
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
      newFunctionToken:=recycler^.newToken(first^.next^.next);

      if cTokType[2]=tt_EOL then cTokType3:=tt_EOL else begin
        oldSecond:=first^.next^.next^.next;
        if oldSecond=nil then cTokType3:=tt_EOL
        else cTokType3:=oldSecond^.tokType;
      end;

      with newFunctionToken^ do begin
        ruleIdResolved:=not(tokType in [tt_identifier,tt_blockLocalVariable]);
        if tokType=tt_blockLocalVariable then begin
          expression:=context^.valueScope^.getVariableValue(newFunctionToken^.txt);
          if (expression<>nil) then begin
            if expression^.literalType<>lt_expression then expression^.unreference
            else begin
              newFunctionToken^.data:=expression;
              newFunctionToken^.tokType:=tt_literal;
              ruleIdResolved:=true;
            end;
          end;
        end else if tokType in C_operators then begin
          txt:=C_tokenDefaultId[tokType];
          data:=intFuncForOperator[tokType];
          tokType:=tt_intrinsicRule;
          ruleIdResolved:=true;
        end;
      end;
      if ruleIdResolved then begin
        //resolved rule ID mutate x . y -> y(x)
        newParameterListToken:=recycler^.newToken(first^.next^.next^.location,'',tt_parList,recycler^.newListLiteral(1)^.append(recycler,first^.data,false));

        first^.data:=nil; first^.tokType:=tt_identifier;
                               //Disposing from:   <Lit> . func ...
        oldSecond:=recycler^.disposeToken(         //|   | | ^^^^
                   recycler^.disposeToken(         //|   | ^
                   recycler^.disposeToken(first)));//^^^^^

        first:=newFunctionToken; newFunctionToken^.next:=newParameterListToken; newParameterListToken^.next:=oldSecond;

        if cTokType3=tt_braceOpen then begin
          newParameterListToken^.next:=recycler^.disposeToken(oldSecond);
          newParameterListToken^.tokType:=tt_parList_constructor;
        end;
        didSubstitution:=true;
        exit;
      end else begin
        context^.raiseError('Unresolved identifier: '+newFunctionToken^.txt,newFunctionToken^.location);
        recycler^.disposeToken(newFunctionToken);
        exit;
      end;
    end;

  PROCEDURE process_op_lit_cascading;
    begin
      // x < y < z -> [x < y] and y < z
      newLit:=resolveOperator(stack.top^.next^.data,
                              stack.top^.tokType,
                              first^.data,
                              stack.top^.location,
                              context,recycler);
      //LHS literal is now result of first comparison (still a literal)
      recycler^.disposeLiteral(stack.top^.next^.data);
      stack.top^.next^.data:=newLit;
      stack.top^.next^.location:=stack.top^.location;
      //applied comparator is replaced by operator 'and'
      stack.top^.tokType:=tt_operatorAnd;
      didSubstitution:=true;
    end;

  PROCEDURE process_pow2_pow3;
    VAR newLit:P_literal;
        power:P_abstractIntLiteral;
    begin
      assert(cTokType[0]=tt_literal);
      assert(cTokType[1] in [tt_pow2,tt_pow3]);
      if cTokType[1]=tt_pow2
      then power:=recycler^.newIntLiteral(2)
      else power:=recycler^.newIntLiteral(3);
      newLit:=resolveOperator(first^.data,tt_operatorPot,power,first^.next^.location,context,recycler);
      recycler^.disposeLiteral(power);
      first:=recycler^.disposeToken(first);
      first^.data:=newLit;
      first^.tokType:=tt_literal;
      didSubstitution:=true;
    end;

  PROCEDURE process_op_lit;
    begin
      case cTokType[1] of
        tt_pow2,tt_pow3: process_pow2_pow3;
        tt_separatorMapItem..tt_operatorConcatAlt:
          if C_opPrecedence[cTokType[1],0]>=C_opPrecedence[cTokType[-1],1] then begin
            case cTokType[-1] of
              tt_unaryOpMinus ,
              tt_unaryOpNegate,
              tt_unaryOpPlus  : newLit:=resolveUnaryOperator(cTokType[-1],first^.data,stack.top^.location,context,recycler)
              else newLit:=resolveOperator(stack.top^.next^.data,
                                           cTokType[-1],
                                           first^.data,
                                           stack.top^.location,
                                           context,recycler);
            end;
            recycler^.disposeLiteral(first^.data);
            first^.data:=newLit; //store new literal in head
            first^.location:=stack.top^.location;
            stack.top:=recycler^.disposeToken(stack.top); //pop operator from stack
            if not(cTokType[-1] in C_unaryOperators) then stack.top:=recycler^.disposeToken(stack.top); //pop LHS-Literal from stack
            didSubstitution:=true;
          end else begin
            stack.push(first);
            stack.push(first);
            didSubstitution:=true;
          end;
        tt_braceClose,tt_listBraceClose,tt_EOL,tt_separatorComma,tt_semicolon, tt_separatorCnt, tt_iifCheck, tt_iifElse:
          begin
            case cTokType[-1] of
              tt_unaryOpMinus ,
              tt_unaryOpNegate,
              tt_unaryOpPlus  : newLit:=resolveUnaryOperator(cTokType[-1],first^.data,stack.top^.location,context,recycler)
              else newLit:=resolveOperator(stack.top^.next^.data,
                                           cTokType[-1],
                                           first^.data,
                                           stack.top^.location,
                                           context,recycler);
            end;
            recycler^.disposeLiteral(first^.data);
            first^.data:=newLit; //store new literal in head
            first^.location:=stack.top^.location;
            stack.top:=recycler^.disposeToken(stack.top); //pop operator from stack
            if not(cTokType[-1] in C_unaryOperators) then stack.top:=recycler^.disposeToken(stack.top); //pop LHS-Literal from stack
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
        else context^.raiseError('Unexpected token after literal: '+safeTokenToString(first^.next){$ifdef fullVersion}+' ('+C_tokenDoc[cTokType[1]].helpText+')'{$endif},errorLocation);
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
            first:=recycler^.disposeToken(first); //drop true
            first:=recycler^.disposeToken(first); //drop AND
            didSubstitution:=true;
          end else begin
            //false AND ... -> false
            p:=first^.next;
            while not((p=nil) or (p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorMapItem,tt_separatorComma,tt_semicolon,tt_iifCheck,tt_iifElse,tt_operatorLazyOr,tt_operatorOr]) and (bracketLevel=0)) do begin
              if      p^.tokType in C_openingBrackets then inc(bracketLevel)
              else if p^.tokType in C_closingBrackets then dec(bracketLevel);
              p:=recycler^.disposeToken(p);
            end;
            first^.next:=p;
            didSubstitution:=true;
          end;
        end else raiseLazyBooleanError(first^.next^.location,P_literal(first^.data));
        tt_operatorLazyOr:if (cTokType[0]=tt_literal) and (P_literal(first^.data)^.literalType=lt_boolean) then begin
          if (P_boolLiteral(first^.data)^.value) then begin
            //true OR ... -> true
            p:=first^.next;
            while not((p=nil) or (p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorMapItem,tt_separatorComma,tt_semicolon,tt_iifCheck,tt_iifElse]) and (bracketLevel=0)) do begin
              if      p^.tokType in C_openingBrackets then inc(bracketLevel)
              else if p^.tokType in C_closingBrackets then dec(bracketLevel);
              p:=recycler^.disposeToken(p);
            end;
            first^.next:=p;
            didSubstitution:=true;
          end else begin
            //false OR ... -> ...
            first:=recycler^.disposeToken(first); //drop false
            first:=recycler^.disposeToken(first); //drop OR
            didSubstitution:=true;
          end;
        end else raiseLazyBooleanError(first^.next^.location,P_literal(first^.data));
        tt_operatorOrElse: if (cTokType[0]=tt_literal) then begin
          if (P_literal(first^.data)^.literalType = lt_void) then begin
            //void orElse ... -> ...
            first:=recycler^.disposeToken(first); //drop void
            first:=recycler^.disposeToken(first); //drop orElse
            didSubstitution:=true;
          end else begin
            //<Lit> orElse ... -> <Lit>
            p:=first^.next;
            while not((p=nil) or (p^.tokType in [tt_braceClose,tt_listBraceClose,tt_separatorCnt,tt_separatorComma,tt_semicolon,tt_iifCheck,tt_iifElse,tt_operatorLazyOr,tt_operatorOr,tt_separatorMapItem]) and (bracketLevel=0)) do begin
              if      p^.tokType in C_openingBrackets then inc(bracketLevel)
              else if p^.tokType in C_closingBrackets then dec(bracketLevel);
              p:=recycler^.disposeToken(p);
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

  PROCEDURE constructList;
    begin
      repeat
        P_listLiteral(stack.top^.data)^.appendConstructing(recycler,first^.data,first^.next^.location,context,
                      stack.top^.tokType=tt_list_constructor_ranging);
        if first^.next^.tokType=tt_separatorCnt
        then stack.top^.tokType:=tt_list_constructor_ranging
        else stack.top^.tokType:=tt_list_constructor;
        first:=recycler^.disposeToken(first);
        first:=recycler^.disposeToken(first);
      until (first=nil) or (first^.tokType<>tt_literal) or
            (first^.next=nil) or not(first^.next^.tokType in [tt_separatorComma,tt_separatorCnt]);
      didSubstitution:=true;
    end;

  PROCEDURE resolveElementAccess;
    begin
      newLit:=recycler^.newListLiteral;
      P_listLiteral(newLit)^
      .append   (recycler,first^.data,false)^
      .appendAll(recycler,first^.next^.data);
      recycler^.disposeLiteral(first^.next^.data);
      first^.tokType:=tt_intrinsicRule;
      first^.data:=BUILTIN_GET;
      first^.txt:='get';
      first^.next^.tokType:=tt_parList;
      first^.next^.data:=newLit;
      applyRule(first^.next,first^.next^.next);
    end;

  PROCEDURE handleSquareBrackets;
    begin
      P_listLiteral(stack.top^.data)^.appendConstructing(recycler,first^.data,first^.next^.location,context,
                    stack.top^.tokType=tt_list_constructor_ranging);
      first:=recycler^.disposeToken(first);
      first:=recycler^.disposeToken(first);
      stack.popLink(first);   // -> ? | [ ...
      first^.tokType:=tt_literal; // -> ? | <NewList>
      didSubstitution:=true;
      if (stack.top<>nil) and (stack.top^.tokType in [tt_blockLocalVariable,tt_globalVariable]) then begin
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
          then newLit:=context^.valueScope^.getVariableValue(first^.txt)
          else newLit:=P_variable(first^.data)^.getValue(context,recycler);
          if newLit<>nil then begin
            first^.data:=newLit;
            first^.tokType:=tt_literal;
            resolveElementAccess;
          end else begin
            context^.raiseError('Cannot resolve variable '+first^.txt{$ifdef fullVersion}+' ('+C_tokenDoc[first^.tokType].helpText+')'{$endif},first^.location);
            didSubstitution:=false;
          end;
        end;
      end else if (stack.top<>nil) and (stack.top^.tokType=tt_literal) then begin
        // <Lit> | <NewList> ...
        stack.popLink(first); // -> | <Lit> <NewList> ...
        resolveElementAccess;
      end;
    end;

  PROCEDURE resolvePseudoFuncPointer;
    VAR exRule:P_expressionLiteral;
        ruleToken:P_token;
        temp:P_token;
        location:T_tokenLocation;
    begin
      //state @pre: ::f ...
      location:=first^.location;;
      ruleToken:=recycler^.disposeToken(first); //dispose ::, store f
      temp:=ruleToken^.next; //store ...
      if (ruleToken^.tokType=tt_userRule)
      then begin
        ruleToken^.data:=P_rule(ruleToken^.data)^.getFunctionPointer(context,recycler,ruleToken^.location);
        ruleToken^.tokType:=tt_literal;
        first:=ruleToken;
      end else begin
        exRule:=builtinFunctionMap.getIntrinsicRuleAsExpression(P_intFuncCallback(ruleToken^.data),true);
        recycler^.disposeToken(ruleToken);
        first:=recycler^.newToken(location,'',tt_literal,exRule); // {f@$params}
      end;
      first^.next:=temp; //-> {f@$params} ...
      didSubstitution:=true;
    end;

  VAR initialScope:P_valueScope;
  PROCEDURE cleanupStackAndExpression;
    begin
      while context^.valueScope<>initialScope do recycler^.scopePop(context^.valueScope);
      recycler^.cascadeDisposeToken(stack.top);
      recycler^.cascadeDisposeToken(first);
    end;

  PROCEDURE processEntryConstructor;
    begin
      stack.top:=recycler^.disposeToken(stack.top);
      stack.popLink(first);
      first^.data:=recycler^.newListLiteral(2)^
        .append(recycler,first^.data,false)^
        .append(recycler,first^.next^.data,true);
      first^.next:=recycler^.disposeToken(first^.next);
      didSubstitution:=true;
    end;

  PROCEDURE resolveFormatString;
    VAR r:P_token;
        par:P_listLiteral;
    begin
      r:=recycler^.newToken(first^.location,'format',tt_intrinsicRule,FORMAT_FUNCTION);
      par:=recycler^.newListLiteral(1);
      par^.append(recycler,P_stringLiteral(first^.data),false);
      first^.tokType:=tt_parList;
      first^.data:=par;
      r^.next:=first;
      first:=r;
      didSubstitution:=true;
    end;

{$MACRO ON}
{$define COMMON_CASES:=
tt_pow2,tt_pow3: process_pow2_pow3;
tt_listBraceOpen: begin
  first^.next^.data:=recycler^.newListLiteral;
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
tt_separatorMapItem..tt_operatorConcatAlt: operator_and_literal_push;
tt_iifCheck: begin stack.push(first); didSubstitution:=true; end;
tt_each,tt_parallelEach: resolveEach(cTokType[1])}

{$define FORBIDDEN_SEPARATORS:=
tt_separatorCnt:   context^.raiseError('Token .. is only allowed in list constructors.',first^.next^.location);
tt_separatorComma: context^.raiseError('Token , is only allowed in parameter lists and list constructors.',first^.next^.location)}

{$WARN 2005 OFF}
//COMMON_SEMICOLON_HANDLING is defined for cTokType[0]=tt_literal; case C_tokType[1] of ...
{$define COMMON_SEMICOLON_HANDLING:=tt_semicolon:
if (cTokType[-1] in [tt_beginBlock,tt_beginRule,tt_beginExpression]) then begin
  if (cTokType[2]=C_compatibleEnd[cTokType[-1]]) then begin
    if (cTokType[-1] in [tt_beginRule,tt_beginExpression]) then begin
      {$ifdef fullVersion}
      context^.callStackPop(first);
      {$endif}
    end;
    stack.top:=recycler^.disposeToken(stack.top);
    context^.valueScope^.checkVariablesOnPop(recycler,first^.location,context);
    first^.next:=recycler^.disposeToken(first^.next);
    first^.next:=recycler^.disposeToken(first^.next);
    recycler^.scopePop(context^.valueScope);
    didSubstitution:=true;
  end else begin
    first:=recycler^.disposeToken(first);
    first:=recycler^.disposeToken(first);
    didSubstitution:=true;
  end;
end else begin
  first:=recycler^.disposeToken(first);
  first:=recycler^.disposeToken(first);
  didSubstitution:=true;
end}

  VAR cleanupCount:longint=0;
  {$ifdef fullVersion}
  debugRun:boolean=true;
  {$endif}
  begin
    result:=rr_ok;
    inc(context^.callDepth);
    if context^.callDepth>STACK_DEPTH_LIMIT then context^.raiseError('Stack depth limit exceeded',errorLocation(first),mt_el4_systemError);
    stack.create;
    initialScope:=context^.valueScope;
    if context^.continueEvaluation then
    {$ifdef useTryCatchBlocks}try{$endif}
    repeat
      didSubstitution:=false;
      initTokTypes;

      //writeln(cTokType[-1],' # ',cTokType[0],' ',cTokType[1],' ',cTokType[2]);
      //writeln(stack.toString(first,50));

      {$ifdef fullVersion}
      debugRun:=debugRun and context^.stepping(first,@stack);
      {$endif}
      case cTokType[0] of
        tt_assignBlockConstant: begin
          context^.valueScope^.createVariable(first^.txt,first^.data,true);
          first:=recycler^.disposeToken(first);
          didSubstitution:=true;
        end;
        tt_formatString: resolveFormatString;
{cT[0]=}tt_literal,tt_aggregatorExpressionLiteral: case cTokType[-1] of
          tt_for: if cTokType[1]=tt_do then begin
            stack.popLink(first);
            resolveFor;
          end;
 {cT[-1]=}tt_separatorMapItem: case cTokType[1] of
            tt_braceClose,tt_separatorCnt,tt_separatorComma,tt_EOL,tt_semicolon,tt_expBraceClose,tt_listBraceClose: processEntryConstructor;
            COMMON_CASES;
          end;
 {cT[-1]=}tt_ponFlipper: if (P_literal(first^.data)^.literalType=lt_expression)
            and (stack.top<>nil) and (stack.top^.next<>nil)  and (stack.top^.next^.tokType=tt_literal) then begin
            // <Lit> . # {$x}
            stack.popLink(first);
            stack.popLink(first);
            // -> # <Lit> {$x}
            initTokTypes;
            pon_flip;
          end;
 {cT[-1]=}tt_listToParameterList: if P_literal(first^.data)^.literalType in C_listTypes then begin
            stack.top:=recycler^.disposeToken(stack.top);
            first^.tokType:=tt_parList;
            stack.popLink(first);
            while first^.tokType=tt_parList do begin
              P_listLiteral(first^.data)^.appendAll(recycler,P_listLiteral(first^.next^.data));
              first^.next:=recycler^.disposeToken(first^.next);
              stack.popLink(first);
            end;
            didSubstitution:=true;
          end;
 {cT[-1]=}tt_comparatorEq..tt_comparatorListEq:
            if (cTokType[1] in [tt_comparatorEq..tt_comparatorListEq])
            then process_op_lit_cascading  //For cases like "1 < x < 6"
            else process_op_lit;
 {cT[-1]=}tt_operatorIn..tt_operatorConcatAlt,
          tt_unaryOpPlus,tt_unaryOpMinus,tt_unaryOpNegate: process_op_lit;
 {cT[-1]=}tt_braceOpen : case cTokType[1] of // ( | <Lit>
            tt_braceClose: begin  // ( | <Lit> )
              stack.top:=recycler^.disposeToken(stack.top);
              first^.next:=recycler^.disposeToken(first^.next);
              didSubstitution:=true;
            end;
            COMMON_SEMICOLON_HANDLING;
            COMMON_CASES;
            FORBIDDEN_SEPARATORS;
            else context^.raiseError('Unable to resolve paranthesis!',stack.top^.location);
          end;
 {cT[-1]=}tt_list_constructor,tt_list_constructor_ranging: case cTokType[1] of
            tt_separatorComma, tt_separatorCnt: constructList; // [ | <Lit> ,
            tt_listBraceClose: begin // [ | <Lit> ] ...
              handleSquareBrackets;
            end;
            COMMON_SEMICOLON_HANDLING;
            COMMON_CASES;
          end;
 {cT[-1]=}tt_parList_constructor: case cTokType[1] of
            tt_braceClose: begin // <F> <par(> | <Lit> ) -> <F> <par>
              P_listLiteral(stack.top^.data)^.append(recycler,first^.data,true);
              stack.top^.tokType:=tt_parList; //mutate <tt_parList_constructor> -> <tt_parList>
              first:=recycler^.disposeToken(first); //dispose literal
              first:=recycler^.disposeToken(first); //dispose closing bracket
              stack.popLink(first); //pop parameter list
              stack.popLink(first); //pop FUNCTION
              didSubstitution:=true;
            end;
            tt_separatorComma: begin // <F> <par(> | <Lit> , -> <F> <par(> |
              P_listLiteral(stack.top^.data)^.append(recycler,first^.data,true);
              first:=recycler^.disposeToken(first);
              first:=recycler^.disposeToken(first);
              didSubstitution:=true;
            end;
            tt_separatorCnt:   context^.raiseError('Token .. is only allowed in list constructors.',first^.next^.location);
            COMMON_SEMICOLON_HANDLING;
            COMMON_CASES;
          end;
 {cT[-1]=}tt_mutate: case cTokType[1] of
            tt_semicolon: if (cTokType[-1] in [tt_beginBlock,tt_beginRule,tt_beginExpression]) and (cTokType[2]=C_compatibleEnd[cTokType[-1]])  then begin
              stack.top:=recycler^.disposeToken(stack.top);
              context^.valueScope^.checkVariablesOnPop(recycler,first^.location,context);
              first^.next:=recycler^.disposeToken(first^.next);
              first^.next:=recycler^.disposeToken(first^.next);
              recycler^.scopePop(context^.valueScope);
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
 {cT[-1]=}tt_assignNewBlockLocal, tt_assignExistingBlockLocal,tt_mut_nested_assign..tt_mut_nestedDrop: case cTokType[1] of
            tt_semicolon: if (cTokType[-1] in [tt_beginBlock,tt_beginRule,tt_beginExpression]) and (cTokType[2]=C_compatibleEnd[cTokType[-1]]) then begin
              first:=recycler^.disposeToken(first);
              if (cTokType[-1] in [tt_beginRule,tt_beginExpression]) then begin
                {$ifdef fullVersion}
                context^.callStackPop(first);
                {$endif}
              end;
              context^.valueScope^.checkVariablesOnPop(recycler,first^.location,context);
              first^.next:=recycler^.disposeToken(first^.next);
              first^.next:=recycler^.disposeToken(first^.next);
              recycler^.scopePop(context^.valueScope);
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
 {cT[-1]=}tt_return: case cTokType[1] of
            tt_semicolon: begin
              stack.top:=recycler^.disposeToken(stack.top); //pop "return" from stack
              processReturnStatement;
            end;
            COMMON_CASES;
          end;
 {cT[-1]=}tt_nameOf: begin
            stack.top:=recycler^.disposeToken(stack.top);
            newLit:=recycler^.newStringLiteral(P_literal(first^.data)^.getId);
            recycler^.disposeLiteral(first^.data);
            first^.data:=newLit;
            didSubstitution:=true;
          end;
          else begin
            case cTokType[1] of
              COMMON_SEMICOLON_HANDLING;
              COMMON_CASES;
              FORBIDDEN_SEPARATORS;
            end;
          end;
        end;
{cT[0]=}tt_beginRule,tt_beginBlock,tt_beginExpression: begin
          recycler^.scopePush(context^.valueScope);
          stack.push(first);
          didSubstitution:=true;
        end;
{cT[0]=}tt_assignNewBlockLocal, tt_assignExistingBlockLocal,tt_mut_nested_assign..tt_mut_nestedDrop: begin
          stack.push(first);
          didSubstitution:=true;
        end;
{cT[0]=}tt_globalVariable:
        if cTokType[-1]=tt_nameOf then begin
          first^.data:=recycler^.newStringLiteral(first^.txt);
          first^.tokType:=tt_literal;
          stack.top:=recycler^.disposeToken(stack.top);
          didSubstitution:=true;
        end else if cTokType[1]=tt_listBraceOpen then begin
          stack.push(first);
          didSubstitution:=true;
        end else begin
          first^.data:=P_variable(first^.data)^.getValue(context,recycler);
          if first^.data<>nil then begin
            first^.tokType:=tt_literal;
            didSubstitution:=true;
          end else begin
            context^.raiseError('Cannot find value for global variable "'+first^.txt+'"',errorLocation);
          end;
        end;
{cT[0]=}tt_blockLocalVariable:
        if cTokType[-1]=tt_nameOf then begin
          first^.data:=recycler^.newStringLiteral(first^.txt);
          first^.tokType:=tt_literal;
          stack.top:=recycler^.disposeToken(stack.top);
          didSubstitution:=true;
        end else if cTokType[1]=tt_listBraceOpen then begin
          stack.push(first);
          didSubstitution:=true;
        end else begin
          if context^.valueScope<>nil
          then first^.data:=context^.valueScope^.getVariableValue(first^.txt)
          else first^.data:=nil;
          if first^.data<>nil then begin
            first^.tokType:=tt_literal;
            didSubstitution:=true;
          end else begin
            context^.raiseError('Cannot find value for local id "'+first^.txt+'"',errorLocation);
          end;
        end;
{cT[0]=}tt_operatorPlus:     begin first^.tokType:=tt_unaryOpPlus;  stack.push(first); didSubstitution:=true; end;
{cT[0]=}tt_operatorMinus:    begin first^.tokType:=tt_unaryOpMinus; stack.push(first); didSubstitution:=true; end;
{cT[0]=}tt_unaryOpPlus,
        tt_unaryOpMinus,
        tt_unaryOpNegate:    begin                                  stack.push(first); didSubstitution:=true; end;
{cT[0]=}tt_comparatorEq..tt_operatorLazyOr,
        tt_operatorMult..tt_operatorConcatAlt:
          context^.raiseError('Undefined prefix operator '+first^.singleTokenToString,errorLocation);
{cT[0]=}tt_braceOpen: begin stack.push(first); didSubstitution:=true; end;
{cT[0]=}tt_expBraceOpen,tt_functionPattern: begin
          digestInlineExpression(first,context,recycler);
          didSubstitution:=true;
        end;
{cT[0]=}tt_braceClose: if cTokType[-1]=tt_parList_constructor then begin
          first:=recycler^.disposeToken(first);
          stack.popLink(first);
          first^.tokType:=tt_parList;
          stack.popLink(first);
          didSubstitution:=true;
        end;
{cT[0]=}tt_listBraceOpen: if cTokType[1]=tt_listBraceClose then begin
          //empty list
          first^.data:=recycler^.newListLiteral;
          first^.tokType:=tt_literal;
          first^.next:=recycler^.disposeToken(first^.next);
          didSubstitution:=true;
        end else begin
          first^.data:=recycler^.newListLiteral;
          first^.tokType:=tt_list_constructor;
          stack.push(first);
          didSubstitution:=true;
        end;
{cT[0]=}tt_list_constructor, tt_list_constructor_ranging: begin stack.push(first); didSubstitution:=true; end;
{cT[0]=}tt_identifier: begin
          P_abstractPackage(first^.location.package)^.resolveId(first^,context^.messages);
          didSubstitution:=true;
        end;
        tt_parameterIdentifier: begin
          context^.getGlobals^.resolveMainParameter(first,context,recycler);
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
              first^.next:=recycler^.disposeToken(first^.next); //drop (
              first^.next:=recycler^.disposeToken(first^.next); //drop +
              first^.next:=recycler^.disposeToken(first^.next); //drop )
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

{cT[0]=}tt_userRule, tt_intrinsicRule, tt_rulePutCacheValue: case cTokType[1] of
          tt_braceOpen, tt_parList_constructor, tt_listToParameterList: startOrPushParameterList;
          tt_listBraceOpen: begin
            if (cTokType[0]=tt_globalVariable) then begin
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
          tt_braceClose,tt_listBraceClose,tt_separatorMapItem..tt_operatorConcatAlt,tt_EOL,tt_iifCheck,tt_iifElse,tt_separatorCnt,tt_separatorComma,tt_semicolon,
          tt_ponFlipper, tt_each,tt_parallelEach,tt_pow2,tt_pow3: applyRule(nil,first^.next);
        end;
{cT[0]=}tt_while: resolveWhile;
{cT[0]=}tt_repeat: resolveRepeat;
        tt_for: if (cTokType[1]=tt_literal) and (cTokType[2]=tt_do) then resolveFor else begin
          stack.push(first);
          didSubstitution:=true;
        end;
{cT[0]=}tt_iifCheck: if (cTokType[-1]=tt_literal) then begin
          if (P_literal(stack.top^.data)^.literalType=lt_boolean)
          then resolveInlineIf(P_boolLiteral(stack.top^.data)^.value)
          else context^.raiseError('Invalid syntax for inline-if; first operand is expected to be a boolean. Instead I found a '+P_literal(stack.top^.data)^.typeString+': '+stack.top^.singleTokenToString,errorLocation);
        end else context^.raiseError('Invalid syntax for inline-if; first operand is expected to be a boolean. Here, the first operand is not even a literal.',errorLocation);
{cT[0]=}tt_pseudoFuncPointer: case cTokType[1] of
          tt_userRule, tt_intrinsicRule: resolvePseudoFuncPointer;
          low(intFuncForOperator)..high(intFuncForOperator): begin
            first^.data:=createPrimitiveAggregatorLiteral(first^.next,context);
            first^.tokType:=tt_literal;
            first^.next:=recycler^.disposeToken(first^.next);
            didSubstitution:=true;
          end;
          tt_blockLocalVariable: begin
            first^.data:=recycler^.newStringLiteral(first^.next^.txt);
            first^.tokType:=tt_literal;
            first^.next:=recycler^.disposeToken(first^.next);
            didSubstitution:=true;
          end;
        end;
{cT[0]=}tt_save: if cTokType[1]=tt_semicolon then begin
          first:=recycler^.disposeToken(first);
          first:=recycler^.disposeToken(first);
          didSubstitution:=true;
        end;
{cT[0]=}tt_return,tt_nameOf: begin
          stack.push(first);
          didSubstitution:=true;
        end;
      end;
      inc(cleanupCount);
      if cleanupCount>8196 then begin
        recycler^.cleanupIfPosted;
        cleanupCount:=0;
      end;
    until not(didSubstitution) or not(context^.continueEvaluation);
    {$ifdef useTryCatchBlocks}
    except
      on e:Exception do begin
        context^.raiseError('An unhandled, exception was caught in reduceExpression on callDepth='+intToStr(context^.callDepth)+C_lineBreakChar+e.message,errorLocation(first),mt_el4_systemError);
      end;
    end;
    {$endif}
    dec(context^.callDepth);
    if context^.continueEvaluation then begin
      if (stack.top<>nil) or (first<>nil) and (first^.next<>nil) then begin
        context^.raiseError('Irreducible expression: '+stack.toString(first,100),errorLocation);
        cleanupStackAndExpression;
        result:=rr_fail;
      end;
    end else begin
      result:=rr_fail;
      cleanupStackAndExpression;
    end;
    stack.destroy;
    recycler^.cleanupIfPosted;
  end;

TYPE
  T_asyncTask=class(T_basicThread)
    private
      recycler:P_recycler;
      payload:P_futureLiteral;
      myContext:P_context;
    protected
      PROCEDURE execute; override;
    public
      CONSTRUCTOR create(CONST payload_:P_futureLiteral; CONST context_:P_context);
      DESTRUCTOR destroy; override;
  end;

PROCEDURE T_asyncTask.execute;
  begin
    if not(Terminated) then payload^.executeInContext(myContext,recycler);
    Terminate;
  end;

CONSTRUCTOR T_asyncTask.create(CONST payload_: P_futureLiteral; CONST context_: P_context);
  begin
    payload:=payload_;
    myContext:=context_;
    recycler:=newRecycler;
    inherited create(tpNormal,true);
    FreeOnTerminate:=true;
  end;

DESTRUCTOR T_asyncTask.destroy;
  begin
    inherited destroy;
    recycler^.disposeLiteral(payload);
    myContext^.finalizeTaskAndDetachFromParent(recycler);
    contextPool.disposeContext(myContext);
    freeRecycler(recycler);
  end;

{$i func_defines.inc}
FUNCTION localOrGlobalAsync(CONST local:boolean; CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  VAR payload     :P_futureLiteral=nil;
      childContext:P_context      =nil;
      parameters  :P_listLiteral  =nil;
      timeout     :double         =0;
      task        :T_asyncTask    =nil;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_expression) and
       ((params^.size=1) or (params^.size=2) and (arg1^.literalType in C_listTypes)) and
        context^.checkSideEffects('async',tokenLocation,[se_detaching]) then begin

      if getGlobalThreads>=GLOBAL_THREAD_LIMIT
      then begin
        threadStartsSleeping;
        context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,'Cannot create any more threads - retrying for one second...');
        timeout:=context^.wallclockTime+60;
        while (context^.wallclockTime<timeout) and (getGlobalThreads>=GLOBAL_THREAD_LIMIT) do sleep(1);
        threadStopsSleeping;
      end;

      if getGlobalThreads>=GLOBAL_THREAD_LIMIT
      then context^.raiseError('Cannot create any more threads.',tokenLocation)
      else try
        childContext:=context^.getNewAsyncContext(recycler,local);
        if childContext<>nil then begin
          if params^.size=2 then parameters:=list1;
          new(payload,create(P_expressionLiteral(arg0),parameters,tokenLocation,{blocking=}false));
          result:=payload^.rereferenced;
          task:=T_asyncTask.create(payload,childContext);
          if task.FatalException=nil
          then begin
            try
              task.start;
            except
              on e:Exception do begin
                context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,'Error on thread start: '+e.message+'; cleaning memory and retrying once...');
                memoryCleaner.callCleanupMethods(3);
                recycler^.cleanupIfPosted;
                task.start;
              end;
            end;
          end
          else begin
            context^.raiseError('Thread creation failed: '+task.FatalException.toString,tokenLocation);
            task.free;
            task:=nil;
          end;
        end else begin
          context^.raiseError('Creation of asynchronous/future tasks is forbidden for the current context',tokenLocation);
        end;
      except
        on e:EOutOfMemory do begin
          context^.raiseError(e.message,tokenLocation,mt_el4_systemError);
          memoryCleaner.callCleanupMethods(high(T_cleanupLevel));
          recycler^.cleanupIfPosted;
        end;
        on e:Exception do begin
          context^.raiseError(e.message,tokenLocation,mt_el4_systemError);
          if task<>nil then task.free;
          if result<>nil then recycler^.disposeLiteral(result);
        end;
      end;
    end;
  end;

FUNCTION async_imp      intFuncSignature; begin result:=localOrGlobalAsync(false,params,tokenLocation,context,recycler); end;
FUNCTION localAsync_imp intFuncSignature; begin result:=localOrGlobalAsync(true ,params,tokenLocation,context,recycler); end;

FUNCTION future_imp intFuncSignature;
  VAR future:P_futureLiteral;
      parameters:P_listLiteral=nil;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_expression) and
       ((params^.size=1) or (params^.size=2) and (arg1^.literalType in C_listTypes)) then begin
      if params^.size=2 then parameters:=list1;
      new(future,create(P_expressionLiteral(arg0),parameters,tokenLocation,{blocking=}true));
      if (tco_spawnWorker in context^.threadOptions) then begin
        future^.rereference;
        enqueueFutureTask(future,context,recycler);
      end;
      result:=future;
    end;
  end;

FUNCTION peekFuture_imp intFuncSignature;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and
       (arg0^.literalType=lt_expression) and
       (P_expressionLiteral(arg0)^.typ=et_builtinAsyncOrFuture) and
       (P_futureLiteral(arg0)^.isFuture) then begin
      result:=newBoolLiteral(P_futureLiteral(arg0)^.isDone);
    end;
  end;

INITIALIZATION
  reduceExpressionCallback:=@reduceExpression;
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'async',@async_imp,ak_variadic_1);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'localAsync',@localAsync_imp,ak_variadic_1);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'future',@future_imp,ak_variadic_1);
  builtinFunctionMap.registerRule(SYSTEM_BUILTIN_NAMESPACE,'peekFuture',@peekFuture_imp,ak_unary);
end.
