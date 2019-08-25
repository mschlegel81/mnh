UNIT rules;
INTERFACE
USES sysutils,
     myGenerics, myStringUtil,
     mnh_constants,basicTypes,
     out_adapters,
     litVar, valueStore,
     funcs,
     tokens,
     tokenArray,
     mnh_messages,
     contexts,
     patterns,
     recyclers,
     datastores,
     subrules,
     caches;
CONST C_varTypeText:array [T_variableType] of string=('mutable','datastore','plain datastore');

TYPE
  T_subruleArray=array of P_subruleExpression;

  P_rule=^T_rule;
  T_ruleList=array of P_rule;
  T_rule=object(T_abstractRule)
    private
       hiddenRule:P_intFuncCallback;
    public
    allowCurrying:boolean;
    FUNCTION getFunctionPointer(VAR context:T_context; VAR recycler:T_recycler; CONST location:T_tokenLocation):P_expressionLiteral; virtual; abstract;
    FUNCTION inspect({$WARN 5024 OFF}CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler):P_mapLiteral; virtual;
    PROCEDURE resolveIds(CONST adapters:P_messages; CONST resolveIdContext:T_resolveIdContext{$ifdef fullVersion}; CONST functionCallInfos:P_functionCallInfos{$endif}); virtual;
    {$ifdef fullVersion}
    PROCEDURE checkParameters(VAR context:T_context); virtual;
    PROCEDURE setIdResolved; virtual;
    {$endif}
    FUNCTION isFallbackPossible(CONST callLocation:T_tokenLocation; CONST givenParameters:P_listLiteral; OUT firstRep,lastRep:P_token; VAR context:T_context; VAR recycler:T_recycler):boolean;
    FUNCTION evaluateToLiteral(CONST callLocation:T_tokenLocation; CONST p1,p2:P_literal;       VAR recycler:T_recycler; CONST context:P_abstractContext):P_literal; virtual;
    FUNCTION evaluateToLiteral(CONST callLocation:T_tokenLocation; CONST parList:P_listLiteral; VAR recycler:T_recycler; CONST context:P_abstractContext):P_literal; virtual;
    FUNCTION arity:T_arityInfo; virtual;
  end;

  P_ruleWithSubrules=^T_ruleWithSubrules;
  T_ruleWithSubrules=object(T_rule)
    private
      subrules:T_subruleArray;
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ruleTyp:T_ruleType=rt_normal);
      DESTRUCTOR destroy; virtual;
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subruleExpression; VAR context:T_context); virtual;
      PROCEDURE resolveIds(CONST adapters:P_messages; CONST resolveIdContext:T_resolveIdContext{$ifdef fullVersion}; CONST functionCallInfos:P_functionCallInfos{$endif}); virtual;
      FUNCTION hasPublicSubrule:boolean; virtual;
      FUNCTION getCmdLineHelpText:T_arrayOfString; virtual;
      FUNCTION isReportable(OUT value:P_literal):boolean; virtual;
      FUNCTION getInlineValue:P_literal; virtual;
      PROPERTY getSubrules:T_subruleArray read subrules;
      FUNCTION replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean; virtual;
      FUNCTION inspect(CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler):P_mapLiteral; virtual;
      FUNCTION getFunctionPointer(VAR context:T_context; VAR recycler:T_recycler; CONST location:T_tokenLocation):P_expressionLiteral; virtual;
      {$ifdef fullVersion}
      FUNCTION getDocTxt: ansistring; virtual;
      PROCEDURE checkParameters(VAR context:T_context); virtual;
      {$endif}
      {Returns the common arity of all subrules or -1 if arity differs or any subrule has optional parameters}
      FUNCTION arity:T_arityInfo; virtual;
  end;

  P_delegatorRule=^T_delegatorRule;
  T_delegatorRule=object(T_rule)
    private
      intOperator:T_tokenType;
      isUnary:boolean;
      localRule:P_ruleWithSubrules;
      imported:array of P_ruleWithSubrules;
    public
      CONSTRUCTOR create(CONST id:T_idString; CONST declaredInPackage:P_abstractPackage);
      DESTRUCTOR destroy; virtual;
      PROCEDURE resolveIds(CONST adapters:P_messages; CONST resolveIdContext:T_resolveIdContext{$ifdef fullVersion}; CONST functionCallInfos:P_functionCallInfos{$endif}); virtual;
      FUNCTION hasPublicSubrule:boolean; virtual;
      FUNCTION isReportable(OUT value:P_literal):boolean; virtual;
      FUNCTION replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean; virtual;
      FUNCTION inspect(CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler):P_mapLiteral; virtual;
      FUNCTION getFunctionPointer(VAR context:T_context; VAR recycler:T_recycler; CONST location:T_tokenLocation):P_expressionLiteral; virtual;
      FUNCTION innerRuleType:T_ruleType; virtual;
      {$ifdef fullVersion}
      FUNCTION getDocTxt: ansistring; virtual;
      PROCEDURE checkParameters(VAR context:T_context); virtual;
      PROCEDURE setIdResolved; virtual;
      {$endif}
      {Returns the common arity of all subrules or -1 if arity differs or any subrule has optional parameters}
      FUNCTION arity:T_arityInfo; virtual;

      PROCEDURE clearImported;
      PROCEDURE addRule(CONST ruleOrDelegateToAdd:P_abstractRule);
      PROPERTY getLocalRule:P_ruleWithSubrules read localRule;
  end;

  P_protectedRuleWithSubrules=^T_protectedRuleWithSubrules;
  T_protectedRuleWithSubrules=object(T_ruleWithSubrules)
    private
      rule_cs:system.TRTLCriticalSection;
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ruleTyp:T_ruleType=rt_synchronized);
      DESTRUCTOR destroy; virtual;
      FUNCTION replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean; virtual;
      FUNCTION getFunctionPointer(VAR context:T_context; VAR recycler:T_recycler; CONST location:T_tokenLocation):P_expressionLiteral; virtual;
  end;

  P_memoizedRule=^T_memoizedRule;
  T_memoizedRule=object(T_protectedRuleWithSubrules)
    private
      cache:T_cache;
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ruleType:T_ruleType=rt_memoized);
      DESTRUCTOR destroy; virtual;
      PROCEDURE clearCache; virtual;
      FUNCTION doPutCache(CONST param:P_listLiteral):P_literal;
      FUNCTION replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean; virtual;
  end;

  P_typeCheckRule=^T_typeCheckRule;
  P_typeCastRule=^T_typeCastRule;
  T_typeCastRule=object(T_ruleWithSubrules)
    private
      typedef:P_typedef;
      related:P_typeCheckRule;
    public
      CONSTRUCTOR create(CONST def:P_typedef; CONST relatedCheckRule:P_typeCheckRule);
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subruleExpression; VAR context:T_context); virtual;
      FUNCTION replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean; virtual;
      FUNCTION getFunctionPointer(VAR context:T_context; VAR recycler:T_recycler; CONST location:T_tokenLocation):P_expressionLiteral; virtual;
      FUNCTION hasPublicSubrule:boolean; virtual;
      FUNCTION getRootId:T_idString; virtual;
      {$ifdef fullVersion}
      PROCEDURE setIdResolved; virtual;
      {$endif}
      FUNCTION inspect(CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler):P_mapLiteral; virtual;
      FUNCTION arity:T_arityInfo; virtual;
  end;

  T_typeCheckRule=object(T_ruleWithSubrules)
    private
      typedef:P_typedef;
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ducktyping:boolean);
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subruleExpression; VAR context:T_context); virtual;
      FUNCTION replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean; virtual;
      FUNCTION getFirstParameterTypeWhitelist:T_literalTypeSet; virtual;
      FUNCTION getRootId:T_idString; virtual;
      FUNCTION hasPublicSubrule:boolean; virtual;
      FUNCTION castRuleIsValid:boolean;
      DESTRUCTOR destroy; virtual;
      FUNCTION arity:T_arityInfo; virtual;
  end;

  P_variable=^T_variable;
  T_variable=object(T_abstractRule)
    private
      varType:T_variableType;
      rule_cs:system.TRTLCriticalSection;
      privateRule:boolean;
      called,
      valueChangedAfterDeclaration:boolean;
      namedValue:T_namedVariable;
      meta:T_ruleMetaData;
    public
      PROPERTY metaData:T_ruleMetaData read meta;
      PROPERTY getVariableType:T_variableType read varType;

      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; VAR meta_:T_ruleMetaData; CONST isPrivate:boolean; CONST variableType:T_variableType);
      DESTRUCTOR destroy; virtual;
      FUNCTION hasPublicSubrule:boolean; virtual;
      PROCEDURE setMutableValue(CONST value:P_literal; CONST onDeclaration:boolean); virtual;
      FUNCTION mutateInline(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):P_literal; virtual;
      FUNCTION isReportable(OUT value:P_literal):boolean; virtual;
      FUNCTION inspect(CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler):P_mapLiteral; virtual;
      {$ifdef fullVersion}
      FUNCTION getDocTxt: ansistring; virtual;
      {$endif}
      FUNCTION getValueOrElseVoid(VAR context:T_context; VAR recycler:T_recycler):P_literal;
      FUNCTION getValue(VAR context:T_context; VAR recycler:T_recycler):P_literal; virtual;

      {Part of T_abstractRule, but should not be called an throws exception}
      FUNCTION evaluateToLiteral(CONST callLocation:T_tokenLocation; CONST p1,p2:P_literal;       VAR recycler:T_recycler; CONST context:P_abstractContext):P_literal; virtual;
      {Part of T_abstractRule, but should not be called an throws exception}
      FUNCTION evaluateToLiteral(CONST callLocation:T_tokenLocation; CONST parList:P_listLiteral; VAR recycler:T_recycler; CONST context:P_abstractContext):P_literal; virtual;
      {Part of T_abstractRule, but should not be called an throws exception}
      FUNCTION replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean; virtual;
  end;

  P_datastore=^T_datastore;
  T_datastore=object(T_variable)
    private
      dataStoreMeta:T_datastoreMeta;
      PROCEDURE readDataStore(VAR context:T_context; VAR recycler:T_recycler);
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; VAR meta_:T_ruleMetaData; CONST datastorePackage:P_objectWithPath; CONST isPrivate:boolean; CONST variableType:T_variableType);
      DESTRUCTOR destroy; virtual;
      FUNCTION mutateInline(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):P_literal; virtual;
      PROCEDURE writeBack(CONST adapters:P_messages);
      PROCEDURE memoryCleanup;
      FUNCTION isInitialized:boolean;
      FUNCTION getValue(VAR context:T_context; VAR recycler:T_recycler):P_literal; virtual;
  end;

  T_ruleMapEntry=object
    //one of: tt_userRule, tt_globalVariable, tt_customType
    entryType:T_tokenType;
    isImported:boolean;
    value:P_objectWithIdAndLocation;
    FUNCTION isImportedOrDelegateWithoutLocal:boolean;
    FUNCTION hasPublicSubrule:boolean;
  end;

  T_ruleMapEntries=array of T_ruleMapEntry;
  P_ruleMap=^T_ruleMap;
  T_ruleMap=object(specialize G_stringKeyMap<T_ruleMapEntry>)
    private
      {$ifdef fullVersion}
      suppressAllUnusedWarnings,
      {$endif}
      merging:boolean;
      afterRules:array of P_subruleExpression;
      localPackage:P_abstractPackage;
      FUNCTION mergeEntry(CONST id:T_idString; entry:T_ruleMapEntry):boolean;
    protected
      PROCEDURE disposeValue(VAR v:VALUE_TYPE); virtual;
    public
      CONSTRUCTOR create(CONST package:P_abstractPackage);
      PROCEDURE clear;
      FUNCTION addImports(CONST other:P_ruleMap):boolean;
      FUNCTION getOperators:T_customOperatorArray;
      PROCEDURE clearImports;
      PROCEDURE declare(CONST ruleId: T_idString;
                        CONST modifiers: T_modifierSet;
                        CONST ruleDeclarationStart: T_tokenLocation;
                        VAR context:T_context;
                        VAR metaData:T_ruleMetaData;
                        subRule:P_subruleExpression
                        {$ifdef fullVersion}; CONST functionCallInfos:P_functionCallInfos{$endif});
      FUNCTION getLocalMain:P_rule;
      FUNCTION getAllLocalRules:T_ruleList;
      PROCEDURE executeAfterRules(VAR context:T_context; VAR recycler:T_recycler);
      PROCEDURE writeBackDatastores(CONST messages:P_messages);
      FUNCTION getTypeMap:T_typeMap;
      PROCEDURE resolveRuleIds(CONST messages:P_messages; CONST resolveIdContext:T_resolveIdContext{$ifdef fullVersion}; CONST functionCallInfos:P_functionCallInfos{$endif});
      FUNCTION inspect(VAR context:T_context; VAR recycler:T_recycler; CONST includeFunctionPointer:boolean):P_mapLiteral;
      {$ifdef fullVersion}
      PROCEDURE updateLists(VAR userDefinedRules:T_setOfString; CONST forCompletion:boolean);
      PROCEDURE complainAboutUnused(CONST messages:P_messages);
      {$endif}
  end;

FUNCTION createPrimitiveAggregatorLiteral(CONST tok:P_token; VAR context:T_context):P_expressionLiteral;
IMPLEMENTATION
USES mySys, operators,fileWrappers;

FUNCTION createPrimitiveAggregatorLiteral(CONST tok:P_token; VAR context:T_context):P_expressionLiteral;
  begin
    if      tok^.tokType in C_operators   then result:=getIntrinsicRuleAsExpression(intFuncForOperator[tok^.tokType])
    else if tok^.tokType=tt_intrinsicRule then result:=getIntrinsicRuleAsExpression(                   tok^.data    )
    else begin
      result:=nil;
      raise Exception.create('Invalid argument for createPrimitiveAggregatorLiteral('+safeTokenToString(tok)+')');
    end;
  end;

FUNCTION T_ruleMapEntry.isImportedOrDelegateWithoutLocal: boolean;
  begin
    result:=isImported or
            (entryType = tt_userRule) and
            (P_abstractRule(value)^.getRuleType=rt_delegate) and
            (P_delegatorRule(value)^.getLocalRule=nil);
  end;

FUNCTION T_ruleMapEntry.hasPublicSubrule:boolean;
  begin
    case entryType of
      tt_userRule:       result:=P_rule(value)^.hasPublicSubrule;
      tt_globalVariable: result:=P_variable(value)^.hasPublicSubrule;
      else               result:=true;
    end;
  end;

FUNCTION T_ruleMap.mergeEntry(CONST id: T_idString; entry: T_ruleMapEntry): boolean;
  VAR earlierEntry:T_ruleMapEntry;
  FUNCTION wrapRuleInDelegator(CONST rule:P_ruleWithSubrules):P_delegatorRule;
    begin
      new(result,create(id,localPackage));
      result^.addRule(rule);
    end;

  begin
    result:=false;
    if containsKey(id,earlierEntry) then begin
      if (entry       .entryType = tt_userRule) and
         (earlierEntry.entryType = tt_userRule) then begin
       {$ifdef debugMode}
       if P_rule(entry.value)^.getRuleType=rt_delegate then raise Exception.create('This should not happen; entry must not be a delegate at this point');
       {$endif}
       if P_rule(earlierEntry.value)^.getRuleType=rt_delegate then begin
         {$ifdef debugMode}
         if earlierEntry.isImported then raise Exception.create('This should not happen; earlierEntry must not be imported at this point');
         {$endif}
       end else begin
         earlierEntry.value:=wrapRuleInDelegator(P_ruleWithSubrules(earlierEntry.value));
         earlierEntry.isImported:=false;
         merging:=true;
         put(id,earlierEntry);
         merging:=false;
       end;
       P_delegatorRule(earlierEntry.value)^.addRule(P_rule(entry.value));
      end else begin
        //Entries cannot be merged
        // -> overwrite unless earlier entry is local
        if earlierEntry.isImportedOrDelegateWithoutLocal then put(id,entry);
      end;
    end else put(id,entry);
  end;

PROCEDURE T_ruleMap.disposeValue(VAR v: VALUE_TYPE);
  begin
    if not(v.isImported or merging) then begin
      case v.entryType of
        tt_globalVariable: dispose(P_variable(v.value),destroy);
        tt_customType    : begin end
        else dispose(P_rule(v.value),destroy);
      end;
    end;
  end;

CONSTRUCTOR T_ruleMap.create(CONST package: P_abstractPackage);
  begin
    inherited create;
    localPackage:=package;
    setLength(afterRules,0);
    merging:=false;
    {$ifdef fullVersion}
    suppressAllUnusedWarnings:=false;
    {$endif}
  end;

PROCEDURE T_ruleMap.clear;
  VAR i:longint;
  begin
    inherited clear;
    for i:=0 to length(afterRules)-1 do disposeLiteral(afterRules[i]);
    setLength(afterRules,0);
    {$ifdef fullVersion}
    suppressAllUnusedWarnings:=false;
    {$endif}
  end;

FUNCTION T_ruleMap.addImports(CONST other: P_ruleMap): boolean;
  FUNCTION qualifiedId(CONST o:P_objectWithIdAndLocation):T_idString;
    begin
      result:=o^.getId;
      if not(isQualified(result)) then result:=other^.localPackage^.getId + ID_QUALIFY_CHARACTER + result;
    end;

  VAR entryToMerge:T_ruleMapEntry;
      newEntry:T_ruleMapEntry;
  begin
    result:=false;
    for entryToMerge in other^.valueSet do if not(entryToMerge.isImported) and entryToMerge.hasPublicSubrule then begin
      newEntry.isImported:=true;
      newEntry.entryType :=entryToMerge.entryType;
      newEntry.value     :=entryToMerge.value;
      if (newEntry.entryType=tt_userRule) and (P_abstractRule(newEntry.value)^.getRuleType=rt_delegate)
      then begin
        newEntry.value:=P_delegatorRule(newEntry.value)^.localRule;
        if (newEntry.value<>nil) and not(P_rule(newEntry.value)^.hasPublicSubrule)
        then newEntry.value:=nil;
      end;
      if newEntry.value<>nil then begin
        //Qualified entries are always added
        put(qualifiedId(newEntry.value),newEntry);

        if not(isQualified(entryToMerge.value^.getId)) and
          (entryToMerge.value^.getId<>MAIN_RULE_ID) then //"main" of imported packages can only be accessed using its qualified id "<package>.main"
        begin
          if mergeEntry(entryToMerge.value^.getId,newEntry) then result:=true;
        end;
      end;
    end;
  end;

FUNCTION T_ruleMap.getOperators: T_customOperatorArray;
  VAR op:T_tokenType;
      entry:T_ruleMapEntry;
  begin
    for op:=low(T_customOperatorArray) to high(T_customOperatorArray) do begin
      if containsKey(operatorName[op],entry)
      then begin
        result[op]:=P_abstractRule(entry.value);
        {$ifdef fullVersion}
        P_abstractRule(entry.value)^.setIdResolved;
        {$endif}
      end
      else result[op]:=nil;
    end;
  end;

PROCEDURE T_ruleMap.clearImports;
  VAR entries:KEY_VALUE_LIST;
      entry  :KEY_VALUE_PAIR;
  begin
    entries:=entrySet;
    for entry in entries do if (T_ruleMapEntry(entry.value).isImportedOrDelegateWithoutLocal)
    then dropKey(entry.key)
    else if (T_ruleMapEntry(entry.value).entryType=tt_userRule) and
            (P_rule         (T_ruleMapEntry(entry.value).value)^.getRuleType=rt_delegate) then
             P_delegatorRule(T_ruleMapEntry(entry.value).value)^.clearImported;
  end;

PROCEDURE T_ruleMap.declare(CONST ruleId: T_idString;
                            CONST modifiers: T_modifierSet;
                            CONST ruleDeclarationStart: T_tokenLocation;
                            VAR context: T_context;
                            VAR metaData: T_ruleMetaData;
                            subRule: P_subruleExpression
                            {$ifdef fullVersion}; CONST functionCallInfos:P_functionCallInfos{$endif});
  VAR metaType    :T_tokenType;
      ruleType    :T_ruleType;
      variableType:T_variableType;
      operatorBeingOverridden:T_tokenType;
      builtinRuleBeingOverridden:P_intFuncCallback=nil;
  FUNCTION checkModifiers:boolean;
    VAR i:longint;
        m:T_modifier;
        s:string='';
        op:T_tokenType;
    begin
      if (ruleId=MAIN_RULE_ID) and (modifiers<>[]) then begin
        context.raiseError('main rules must not have any modifiers',ruleDeclarationStart);
        exit(false);
      end;

      //Check valid modifier combinations:
      result:=false;
      for i:=0 to length(C_validModifierCombinations)-1 do
      if not(result) and (C_validModifierCombinations[i].modifiers=modifiers) then begin
        metaType    :=C_validModifierCombinations[i].metaType    ;
        ruleType    :=C_validModifierCombinations[i].ruleType    ;
        variableType:=C_validModifierCombinations[i].variableType;
        result:=true;
      end;
      if not(result) then begin
        //Complain about modifiers:
        for m in modifiers do s:=s+C_modifierInfo[m].name+' ';
        context.raiseError('Invalid combination of modifiers: '+s,ruleDeclarationStart);
        exit(false);
      end;

      if intrinsicRuleMap.containsKey(ruleId,builtinRuleBeingOverridden) and (metaType<>tt_userRule) then begin
        //You can only overload builtin rules, not hide them altogether
        context.raiseError('Declaration of '+BoolToStr(metaType=tt_globalVariable,'variable','type')+' would hide a builtin rule of the same name',ruleDeclarationStart);
        exit(false);
      end;

      if (metaType<>tt_globalVariable) and (subRule=nil) then begin
        context.raiseError('Missing rule body',ruleDeclarationStart);
        exit(false);
      end;

      for op in allOperators do if operatorName[op]=ruleId then begin
        ruleType:=rt_customOperator;
        operatorBeingOverridden:=op;
      end;
      if ruleType=rt_customOperator then begin
        if not(operatorBeingOverridden in overridableOperators) then begin
          context.raiseError('Operator '+C_tokenDefaultId[operatorBeingOverridden]+' cannot be overridden',ruleDeclarationStart);
          exit(false);
        end;
        for m in [modifier_mutable,
                  modifier_datastore,
                  modifier_plain,
                  modifier_curry,
                  modifier_synchronized,
                  modifier_customType,
                  modifier_customDuckType] do if m in modifiers then context.raiseError('modifier '+C_modifierInfo[m].name+' is not allowed when overriding operators',ruleDeclarationStart);
        if modifier_private  in modifiers then context.messages^.postTextMessage(mt_el2_warning  ,ruleDeclarationStart,'private modifier is ignored when overloading operators' );
        if modifier_memoized in modifiers then context.messages^.postTextMessage(mt_el2_warning  ,ruleDeclarationStart,'memoized modifier is ignored when overloading operators');
      end;
      result:=context.continueEvaluation;
    end;

  PROCEDURE declareRule;
    VAR entryForId:T_ruleMapEntry;
        rule:P_rule=nil;
        newEntry:boolean=false;
    begin
      if containsKey(ruleId,entryForId) then begin
        case entryForId.entryType of
          tt_globalVariable, tt_customType: begin
            if entryForId.isImported
            then context.messages^.postTextMessage(mt_el1_note,ruleDeclarationStart,'Rule '+ruleId+' hides imported '+BoolToStr(entryForId.entryType=tt_globalVariable,'variable','type')+' of same name')
            else begin
              context.raiseError('Cannot declare rule '+ruleId+' because there is a '+BoolToStr(entryForId.entryType=tt_globalVariable,'variable','type')+' of the same name',ruleDeclarationStart);
              exit;
            end;
          end;
          tt_userRule: begin
            if entryForId.isImported then begin
              //no problem; this can be merged
            end else
            if P_rule(entryForId.value)^.getRuleType=rt_delegate
            then rule:=P_delegatorRule(entryForId.value)^.localRule
            else rule:=P_rule         (entryForId.value);

            if rule<>nil then begin
              if (rule^.getRuleType<>ruleType) and (ruleType<>rt_normal)
              then begin
                context.raiseError('Colliding modifiers! Rule '+ruleId+' is '+C_ruleTypeText[rule^.getRuleType]+', redeclared as '+C_ruleTypeText[ruleType],ruleDeclarationStart);
                exit;
              end else if (rule^.getRuleType in C_ruleTypesWithOnlyOneSubrule) then begin
                context.raiseError(C_ruleTypeText[rule^.getRuleType]+'rules must have exactly one subrule',ruleDeclarationStart);
                exit;
              end;
              if not(rule^.allowCurrying) and (modifier_curry in modifiers)
              then begin
                context.messages^.postTextMessage(mt_el1_note,ruleDeclarationStart,'All functions of name '+ruleId+' will support (un-)currying');
                rule^.allowCurrying:=true;
              end;
            end;
          end;
        end;
      end else rule:=nil;

      if rule=nil then begin
        newEntry:=true;
        case ruleType of
          rt_memoized      : new(P_memoizedRule             (rule),create(ruleId,ruleDeclarationStart));
          rt_synchronized  : new(P_protectedRuleWithSubrules(rule),create(ruleId,ruleDeclarationStart));
          rt_normal        ,
          rt_customOperator: new(P_ruleWithSubrules         (rule),create(ruleId,ruleDeclarationStart,ruleType));
          {$ifdef debugMode}
          else raise Exception.create('Unexpected rule type '+C_ruleTypeText[ruleType]);
          {$endif}
        end;
      end;
      P_ruleWithSubrules(rule)^.allowCurrying:=modifier_curry in modifiers;
      P_ruleWithSubrules(rule)^.addOrReplaceSubRule(subRule,context);

      if newEntry then begin
        entryForId.isImported:=false;
        entryForId.entryType:=tt_userRule;
        entryForId.value:=rule;

        if mergeEntry(ruleId,entryForId) then resolveRuleIds(nil,ON_DELEGATION{$ifdef fullVersion},functionCallInfos{$endif});
      end;

      {$ifdef debugMode}
      if containsKey(ruleId,entryForId) then begin
        if entryForId.entryType<>tt_userRule then raise Exception.create('Rule '+ruleId+' is not present as user rule after declaration');
        if entryForId.isImported             then raise Exception.create('Rule '+ruleId+' is not present as local user rule after declaration');
      end else raise Exception.create('Rule '+ruleId +' is not present in map after declaration!');
      {$endif}

      if subRule^.metaData.hasAttribute(EXECUTE_AFTER_ATTRIBUTE) then begin
        if subRule^.canApplyToNumberOfParameters(0) then begin
          setLength(afterRules,length(afterRules)+1);
          afterRules[length(afterRules)-1]:=P_subruleExpression(subRule^.rereferenced);
        end else context.messages^.postTextMessage(mt_el2_warning,ruleDeclarationStart,'Only nullary functions can be executed automatically on package finalization');
      end;
    end;

  PROCEDURE declareVariable;
    VAR entryForId:T_ruleMapEntry;
        newVar:P_variable;
        variableValue:P_literal;
    begin
      if containsKey(ruleId,entryForId) then begin
        if entryForId.isImportedOrDelegateWithoutLocal
        then context.messages^.postTextMessage(mt_el1_note,ruleDeclarationStart,ruleId+' hides an imported rule of same name')
        else begin
          context.raiseError('Variable '+ruleId+' cannot be declared because a local rule of the same name already exists',ruleDeclarationStart);
          exit;
        end;
      end;
      if variableType=vt_mutable
      then new(newVar             ,create(ruleId,ruleDeclarationStart,metaData,             modifier_private in modifiers,variableType))
      else new(P_datastore(newVar),create(ruleId,ruleDeclarationStart,metaData,localPackage,modifier_private in modifiers,variableType));
      entryForId.isImported:=false;
      entryForId.entryType :=tt_globalVariable;
      entryForId.value     :=newVar;
      if subRule<>nil then begin
        variableValue:=subRule^.getInlineValue;
        disposeLiteral(subRule);
        if variableValue<>nil then begin
          newVar^.setMutableValue(variableValue,true);
          disposeLiteral(variableValue);
        end;
      end;
      put(ruleId,entryForId);
    end;

  PROCEDURE declareType;
    VAR castable:boolean;
        checkEntry,
        castEntry,
        entryForId:T_ruleMapEntry;

        needIdRefresh:boolean=false;
        checkRule:P_typeCheckRule=nil;
        castRule :P_typeCastRule =nil;
        typedef  :P_typedef      =nil;
    begin
      if not((subRule<>nil) and (subRule^.hasValidValidCustomTypeCheckPattern(ruleType=rt_duckTypeCheck))) then begin
        if (ruleType=rt_customTypeCheck)
        then context.raiseError('Invalid pattern/signature for custom type check! Must accept exactly one List, Set, Map or Expression parameter.',ruleDeclarationStart)
        else context.raiseError('Invalid pattern/signature for custom type check! Must accept exactly one parameter.',ruleDeclarationStart);
        exit;
      end;
      castable :=subRule^.hasValidValidCustomTypeCheckPattern(false);
      if containsKey('is'+ruleId,checkEntry) and not(checkEntry.isImported) then begin
        context.raiseError('You cannot declare type '+ruleId+' because the related type check rule is'+ruleId+' already exists',ruleDeclarationStart);
        exit;
      end;
      if castable and containsKey('to'+ruleId,castEntry) and not(castEntry.isImported) then begin
        context.raiseError('You cannot declare type '+ruleId+' because the related type cast rule to'+ruleId+' already exists',ruleDeclarationStart);
        exit;
      end;
      if containsKey(ruleId,entryForId) and not(entryForId.isImported) then begin
        context.raiseError('You cannot declare type '+ruleId+' because a rule of the same name already exists',ruleDeclarationStart);
        exit;
      end;
      if not(ruleId[1] in ['A'..'Z'])
      then context.messages^.postTextMessage(mt_el2_warning,ruleDeclarationStart,'Type names should begin with an uppercase letter');

      new(checkRule,create(ruleId,ruleDeclarationStart,ruleType=rt_duckTypeCheck));
      checkRule^.addOrReplaceSubRule(subRule,context);
      if not(context.continueEvaluation) then begin
        dispose(checkRule,destroy);
        exit;
      end;

      typedef:=checkRule^.typedef;
      entryForId.isImported:=false;
      entryForId.entryType:=tt_customType;
      entryForId.value:=typedef;
      put(ruleId,entryForId);

      entryForId.entryType:=tt_userRule;
      entryForId.value:=checkRule;
      needIdRefresh:=mergeEntry('is'+ruleId,entryForId);

      if castable then begin
        new(castRule,create(typedef,checkRule));
        entryForId.entryType:=tt_userRule;
        entryForId.value:=castRule;
        if mergeEntry('to'+ruleId,entryForId) then needIdRefresh:=true;
      end;
      if needIdRefresh then resolveRuleIds(nil,ON_DELEGATION{$ifdef fullVersion},functionCallInfos{$endif});
    end;

  begin
    {$ifdef fullVersion}
    suppressAllUnusedWarnings:=suppressAllUnusedWarnings or (metaData.getAttribute(SUPPRESS_UNUSED_WARNING_ATTRIBUTE).value=SUPPRESS_ALL_UNUSED_VALUE);
    {$endif}
    if checkModifiers then case metaType of
      tt_userRule:       declareRule;
      tt_globalVariable: declareVariable;
      tt_customType:     declareType;
    end else if subRule<>nil then disposeLiteral(subRule);
  end;

FUNCTION T_ruleMap.getLocalMain: P_rule;
  VAR entry:T_ruleMapEntry;
  begin
    if containsKey(MAIN_RULE_ID,entry) then begin
      if (entry.entryType<>tt_userRule) or
         (entry.isImported) or
         (P_rule(entry.value)^.getRuleType<>rt_normal)
      then raise Exception.create('main in '+localPackage^.getPath+' has unexpected type')
      else result:=P_rule(entry.value);
    end else result:=nil;
  end;

FUNCTION T_ruleMap.getAllLocalRules: T_ruleList;
  VAR entry:T_ruleMapEntry;
      k:longint=0;
  begin
    setLength(result,size);
    for entry in valueSet do if (entry.entryType=tt_userRule) and not(entry.isImportedOrDelegateWithoutLocal) then begin
      result[k]:=P_rule(entry.value);
      if result[k]^.getRuleType=rt_delegate then result[k]:=P_delegatorRule(result[k])^.localRule;
      if result[k]<>nil then inc(k);
    end;
    setLength(result,k);
  end;

PROCEDURE T_ruleMap.executeAfterRules(VAR context: T_context;
  VAR recycler: T_recycler);
  VAR s:P_subruleExpression;
  begin
    for s in afterRules do s^.evaluate(packageTokenLocation(localPackage),@context,@recycler,nil);
  end;

PROCEDURE T_ruleMap.writeBackDatastores(CONST messages: P_messages);
  VAR entry:T_ruleMapEntry;
  begin
    for entry in valueSet do
    if not(entry.isImported) and (entry.entryType=tt_globalVariable) and (P_variable(entry.value)^.varType in [vt_datastore,vt_plainDatastore])
    then P_datastore(entry.value)^.writeBack(messages);
  end;

FUNCTION T_ruleMap.getTypeMap: T_typeMap;
  VAR entry:T_ruleMapEntry;
  begin
    result.create();
    for entry in valueSet do
    if not(entry.isImported) and (entry.entryType=tt_customType) then
    result.put(entry.value^.getId,P_typedef(entry.value));
  end;

PROCEDURE T_ruleMap.resolveRuleIds(CONST messages: P_messages; CONST resolveIdContext: T_resolveIdContext{$ifdef fullVersion}; CONST functionCallInfos:P_functionCallInfos{$endif});
  VAR entry:T_ruleMapEntry;
  begin
    for entry in valueSet do
    if not(entry.isImported) and (entry.entryType=tt_userRule)
    then P_rule(entry.value)^.resolveIds(messages,resolveIdContext{$ifdef fullVersion}, functionCallInfos{$endif});
  end;

FUNCTION T_ruleMap.inspect(VAR context:T_context; VAR recycler:T_recycler; CONST includeFunctionPointer:boolean) : P_mapLiteral;
  VAR entry:T_ruleMapEntry;
  begin
    result:=newMapLiteral;
    for entry in valueSet do if not(entry.isImportedOrDelegateWithoutLocal) then case entry.entryType of
      tt_userRule:
        result^.put(entry.value^.getId,P_rule(entry.value)^.inspect(includeFunctionPointer,context,recycler),false);
      tt_globalVariable:
        result^.put(entry.value^.getId,P_variable(entry.value)^.inspect(includeFunctionPointer,context,recycler),false);
    end;
  end;

{$ifdef fullVersion}
PROCEDURE T_ruleMap.updateLists(VAR userDefinedRules:T_setOfString; CONST forCompletion:boolean);
  VAR entry:KEY_VALUE_PAIR;
  begin
    for entry in entrySet do if not(T_ruleMapEntry(entry.value).isImported) or T_ruleMapEntry(entry.value).hasPublicSubrule then begin
      if forCompletion then begin
        userDefinedRules.put(entry.key);
        if not(isQualified(entry.key)) and (T_ruleMapEntry(entry.value).entryType<>tt_customType)
        then userDefinedRules.put(ID_QUALIFY_CHARACTER+entry.key);
      end else begin
        if not(isQualified(entry.key))
        then userDefinedRules.put(entry.key);
        userDefinedRules.put(T_ruleMapEntry(entry.value).value^.getLocation.package^.getId);
      end;
    end;
  end;

PROCEDURE T_ruleMap.complainAboutUnused(CONST messages: P_messages);
  VAR entry:T_ruleMapEntry;
  begin
    if suppressAllUnusedWarnings then exit;
    for entry in valueSet do if not(entry.isImportedOrDelegateWithoutLocal) then case entry.entryType of
      tt_userRule:
        P_rule(entry.value)^.complainAboutUnused(messages);
      tt_globalVariable:  begin
        if not(P_variable(entry.value)^.idResolved) then
        messages^.postTextMessage(mt_el2_warning,entry.value^.getLocation,
        'Unused rule '+entry.value^.getId+
        '; you can suppress this warning with '+
        ATTRIBUTE_PREFIX+SUPPRESS_UNUSED_WARNING_ATTRIBUTE);
      end;
    end;
  end;
{$endif}

DESTRUCTOR T_delegatorRule.destroy;
  begin
    if (localRule<>nil)
    then dispose(localRule,destroy);
    inherited destroy;
  end;

PROCEDURE T_delegatorRule.clearImported;
  begin
    setLength(imported,0);
  end;

PROCEDURE T_delegatorRule.addRule(CONST ruleOrDelegateToAdd:P_abstractRule);
  VAR ruleToAdd:P_ruleWithSubrules;
      r:P_ruleWithSubrules;
      i:longint;
  begin
    if ruleOrDelegateToAdd=nil then exit;

    if ruleOrDelegateToAdd^.getRuleType=rt_delegate
    then ruleToAdd:=P_delegatorRule(ruleOrDelegateToAdd)^.localRule
    else ruleToAdd:=P_ruleWithSubrules(ruleOrDelegateToAdd);

    if ruleToAdd=nil then exit;

    if ruleToAdd^.getLocation.package=declarationStart.package then begin
      if localRule=nil
      then localRule:=ruleToAdd
      else raise Exception.create('T_delegatorRule.mergeImported : duplicate declaration of local rule; id='+getId);
      declarationStart:=localRule^.declarationStart;
      exit;
    end;

    for r in imported do if r=ruleToAdd then exit;
    setLength(imported,length(imported)+1);
    for i:=length(imported)-1 downto 1 do imported[i]:=imported[i-1];
    imported[0]:=ruleToAdd;
  end;

FUNCTION T_delegatorRule.isReportable(OUT value: P_literal): boolean;
  begin
    value:=nil;
    result:=false;
    if localRule<>nil then result:=localRule^.isReportable(value);
  end;

FUNCTION T_rule.isFallbackPossible(CONST callLocation: T_tokenLocation;
  CONST givenParameters: P_listLiteral; OUT firstRep, lastRep: P_token;
  VAR context: T_context; VAR recycler: T_recycler): boolean;
  VAR tempToken:P_token;
      tempInline:P_inlineExpression;
      parHead,parTail:P_listLiteral;
      tempLiteral:P_literal;
  begin
    if hiddenRule<>nil then begin
      inc(context.callDepth);
      tempLiteral:=hiddenRule(givenParameters,callLocation,context,recycler);
      dec(context.callDepth);
      if tempLiteral<>nil then begin
        firstRep:=recycler.newToken(callLocation,'',tt_literal,tempLiteral);
        lastRep:=firstRep;
        exit(true);
      end;
    end;
    if not(allowCurrying) or (givenParameters=nil) or (arity.maxPatternLength<>arity.minPatternLength) then exit(false);
    result:=false;
    if (givenParameters^.size<arity.minPatternLength) then begin
      //CURRY
      //  rule : f(x,y)->...
      //  input: f(x)
      //  out  : {f(x,$y)}
      tempToken      :=recycler.newToken(callLocation,getId,tt_userRule,@self);
      tempToken^.next:=getParametersForUncurrying(givenParameters,arity.minPatternLength,callLocation,context,recycler);
      new(tempInline,createFromInline(tempToken,context,recycler,C_tokenDefaultId[tt_pseudoFuncPointer]+getId));
      firstRep:=recycler.newToken(callLocation,'',tt_literal,tempInline);
      lastRep:=firstRep;
      result:=true;
    end else if (givenParameters^.size>arity.minPatternLength) then begin
      //UNCURRY
      //  rule : f(x)->...
      //  input: f(x,y)
      //  out  : f(x)(y)
      parHead:=givenParameters^.head(arity.minPatternLength);
      if replaces(callLocation,parHead,firstRep,lastRep,@context,recycler) then begin
        parTail:=givenParameters^.tail(arity.minPatternLength);
        tempToken:=recycler.newToken(firstRep^.location,'',tt_braceOpen);
        tempToken^.next:=firstRep;
        firstRep:=tempToken;

        lastRep^.next:=recycler.newToken(firstRep^.location,'',tt_braceClose);
        lastRep:=lastRep^.next;
        lastRep^.next:=recycler.newToken(firstRep^.location,'',tt_parList,parTail);
        lastRep:=lastRep^.next;
        result:=true;
      end;
      disposeLiteral(parHead);
    end;
  end;

FUNCTION T_rule.evaluateToLiteral(CONST callLocation:T_tokenLocation; CONST p1,p2:P_literal; VAR recycler:T_recycler; CONST context:P_abstractContext):P_literal;
  VAR parList:P_listLiteral;
      firstRep,lastRep:P_token;
  begin
    if P_context(context)^.callDepth>=STACK_DEPTH_LIMIT then begin
      P_context(context)^.raiseError('Stack overflow in rule '+getId+' ('+string(getLocation)+')',callLocation,mt_el4_systemError);
      exit(nil);
    end;
    inc(P_context(context)^.callDepth);
    parList:=P_listLiteral(newListLiteral(2)^.append(p1,true)^.append(p2,true));
    if replaces(callLocation,parList,firstRep,lastRep,context,recycler,false)
    then result:=P_context(context)^.reduceToLiteral(firstRep,recycler).literal
    else result:=nil;
    dec(P_context(context)^.callDepth);
    disposeLiteral(parList);
  end;

FUNCTION T_rule.evaluateToLiteral(CONST callLocation:T_tokenLocation; CONST parList:P_listLiteral; VAR recycler:T_recycler; CONST context:P_abstractContext):P_literal;
  VAR firstRep,lastRep:P_token;
  begin
    if P_context(context)^.callDepth>=STACK_DEPTH_LIMIT then begin
      P_context(context)^.raiseError('Stack overflow in rule '+getId+' ('+string(getLocation)+')',callLocation,mt_el4_systemError);
      exit(nil);
    end;
    inc(P_context(context)^.callDepth);
    if replaces(callLocation,parList,firstRep,lastRep,context,recycler,false)
    then result:=P_context(context)^.reduceToLiteral(firstRep,recycler).literal
    else result:=nil;
    dec(P_context(context)^.callDepth);
  end;

FUNCTION T_rule.arity:T_arityInfo;
  begin
    result:=NO_ARITY_INFO;
  end;

FUNCTION T_delegatorRule.arity:T_arityInfo;
  VAR r:P_ruleWithSubrules;
  begin
    if localRule<>nil then result:=localRule^.arity
                      else result:=NO_ARITY_INFO;
    for r in imported do result:=commonArity(result,r^.arity);
  end;

FUNCTION T_ruleWithSubrules.arity:T_arityInfo;
  VAR s:P_subruleExpression;
  begin
    result:=NO_ARITY_INFO;
    for s in subrules do result:=commonArity(result,s^.arity);
  end;

FUNCTION T_typeCheckRule.arity:T_arityInfo;
  begin
    result.minPatternLength:=1;
    result.maxPatternLength:=1;
  end;

FUNCTION T_typeCastRule.arity:T_arityInfo;
  begin
    result.minPatternLength:=1;
    result.maxPatternLength:=1;
  end;

{$ifdef fullVersion}
PROCEDURE T_rule.checkParameters(VAR context:T_context);
  begin end;

PROCEDURE T_ruleWithSubrules.checkParameters(VAR context:T_context);
  VAR s:P_subruleExpression;
  begin
    if length(subrules)=1 then
    for s in subrules do s^.checkParameters(context);
  end;

PROCEDURE T_delegatorRule.checkParameters(VAR context: T_context);
  VAR r:P_rule;
  begin
    if localRule<>nil then localRule^.checkParameters(context);
    for r in imported do           r^.checkParameters(context);
  end;
{$endif}

CONSTRUCTOR T_ruleWithSubrules.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation; CONST ruleTyp: T_ruleType);
  begin
    inherited create(ruleId,startAt,ruleTyp);
    if (ruleTyp=rt_customOperator) or not(intrinsicRuleMap.containsKey(ruleId,hiddenRule)) then begin
      hiddenRule:=nil;
    end;
    allowCurrying:=false;
    setLength(subrules,0);
  end;

CONSTRUCTOR T_delegatorRule.create(CONST id:T_idString; CONST declaredInPackage:P_abstractPackage);
  VAR op:T_tokenType;
  begin
    inherited create(id,packageTokenLocation(declaredInPackage),rt_delegate);
    localRule:=nil;
    setLength(imported,0);
    intOperator:=tt_userRule;
    for op in overridableOperators do if id=C_tokenDefaultId[op] then intOperator:=op;
    isUnary:=intOperator in unaryOperators;
  end;

CONSTRUCTOR T_protectedRuleWithSubrules.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation; CONST ruleTyp: T_ruleType);
  begin
    initCriticalSection(rule_cs);
    inherited create(ruleId,startAt,ruleTyp);
  end;

CONSTRUCTOR T_memoizedRule.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation; CONST ruleType:T_ruleType=rt_memoized);
  begin
    inherited create(ruleId,startAt,ruleType);
    cache.create(rule_cs);
  end;

CONSTRUCTOR T_typeCastRule.create(CONST def:P_typedef; CONST relatedCheckRule:P_typeCheckRule);
  begin
    inherited create('to'+def^.getName,relatedCheckRule^.getLocation,rt_customTypeCast);
    typedef:=def;
    allowCurrying:=false;
    related:=relatedCheckRule;
    {$ifdef fullVersion}
    if def^.isDucktyping then setIdResolved;
    {$endif}
  end;

CONSTRUCTOR T_typeCheckRule.create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ducktyping:boolean);
  begin
    if ducktyping
    then inherited create(ruleId,startAt,rt_duckTypeCheck)
    else inherited create(ruleId,startAt,rt_customTypeCheck);
    typedef:=nil;
    allowCurrying:=false;
  end;

CONSTRUCTOR T_variable.create(CONST ruleId: T_idString;
  CONST startAt: T_tokenLocation; VAR meta_: T_ruleMetaData;
  CONST isPrivate: boolean; CONST variableType: T_variableType);
  begin
    inherited create(ruleId,startAt,rt_normal);
    varType:=variableType;
    privateRule:=isPrivate;
    namedValue.create(ruleId,newVoidLiteral,false);
    initCriticalSection(rule_cs);
    meta:=meta_;
    {$ifdef fullVersion}
    idResolved:=false;
    {$endif}
  end;

CONSTRUCTOR T_datastore.create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; VAR meta_:T_ruleMetaData; CONST datastorePackage:P_objectWithPath; CONST isPrivate:boolean; CONST variableType:T_variableType);
  begin
    inherited create(ruleId,startAt,meta_,isPrivate,variableType);
    dataStoreMeta.create(datastorePackage^.getPath,ruleId);
    memoryCleaner.registerObjectForCleanup(@memoryCleanup);
  end;

DESTRUCTOR T_ruleWithSubrules.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(subrules)-1 do disposeLiteral(subrules[i]);
    setLength(subrules,0);
    inherited destroy;
  end;

DESTRUCTOR T_protectedRuleWithSubrules.destroy;
  begin
    enterCriticalSection(rule_cs);
    inherited destroy;
    leaveCriticalSection(rule_cs);
    doneCriticalSection(rule_cs);
  end;

DESTRUCTOR T_memoizedRule.destroy;
  begin
    cache.destroy;
    inherited destroy;
  end;

DESTRUCTOR T_variable.destroy;
  begin
    enterCriticalSection(rule_cs);
    namedValue.destroy;
    leaveCriticalSection(rule_cs);
    doneCriticalSection(rule_cs);
  end;

DESTRUCTOR T_datastore.destroy;
  begin
    dataStoreMeta.destroy;
    memoryCleaner.unregisterObjectForCleanup(@memoryCleanup);
    inherited destroy;
  end;

FUNCTION T_typeCastRule .getRootId:T_idString; begin result:=typedef^.getName; end;
FUNCTION T_typeCheckRule.getRootId:T_idString; begin result:=typedef^.getName; end;

PROCEDURE T_ruleWithSubrules.addOrReplaceSubRule(CONST rule: P_subruleExpression; VAR context: T_context);
  VAR i,j:longint;
  begin
    rule^.parent:=@self;
    if (getId=MAIN_RULE_ID) and not(rule^.hasValidMainPattern) then context.messages^.raiseSimpleError('Invalid pattern/signature for main rule! Must accept strings.',rule^.getLocation);
    if (getRuleType=rt_customOperator) then begin
      if isUnaryOperatorId(getId) then begin
        if not(rule^.canApplyToNumberOfParameters(1)) then context.messages^.raiseSimpleError('Overloaded operator must accept one parameter',rule^.getLocation);
      end else begin
        if not(rule^.canApplyToNumberOfParameters(2)) then context.messages^.raiseSimpleError('Overloaded operator must accept two parameter',rule^.getLocation);
      end;
      if not(rule^.getPattern.usesStrictCustomTyping) then context.messages^.postTextMessage(mt_el2_warning,rule^.getLocation,'Overloading operators based on ducktype is discouraged! Use explicit types instead.');
    end;
    i:=0;
    while (i<length(subrules)) and not(rule^.hasEquivalentPattern(subrules[i])) do inc(i);
    if i>=length(subrules) then begin
      setLength(subrules,i+1);
      for j:=0 to i-1 do if subrules[j]^.hidesSubrule(rule) then context.messages^.postTextMessage(mt_el2_warning,rule^.getLocation,'Rule '+rule^.getId+' seems to be hidden by '+subrules[j]^.getId+' @'+ansistring(subrules[j]^.getLocation));
    end else begin
      disposeLiteral(subrules[i]);
      if not(rule^.metaData.hasAttribute(OVERRIDE_ATTRIBUTE))
      then context.messages^.postTextMessage(mt_el2_warning,rule^.getLocation,'Overriding rule '+rule^.getId+'; you can suppress this warning with '+ATTRIBUTE_PREFIX+OVERRIDE_ATTRIBUTE);
    end;
    subrules[i]:=rule;
    if (length(subrules)>1) and (getRuleType in C_ruleTypesWithOnlyOneSubrule) then context.messages^.raiseSimpleError('Cannot add a subrule to a '+C_ruleTypeText[getRuleType]+'rule!',rule^.getLocation);
    {$ifdef fullVersion}
    if rule^.metaData.hasAttribute(SUPPRESS_UNUSED_WARNING_ATTRIBUTE) or
       rule^.metaData.hasAttribute(EXECUTE_AFTER_ATTRIBUTE) then setIdResolved;
    {$endif}
    clearCache;
  end;

PROCEDURE T_typeCastRule.addOrReplaceSubRule(CONST rule:P_subruleExpression; VAR context:T_context);
  begin
    inherited addOrReplaceSubRule(rule,context);
    if not(rule^.metaData.hasAttribute(OVERRIDE_ATTRIBUTE)) then context.messages^.postTextMessage(mt_el2_warning,rule^.getLocation,'Overloading implicit typecast rule');
  end;

PROCEDURE T_typeCheckRule.addOrReplaceSubRule(CONST rule:P_subruleExpression; VAR context:T_context);
  VAR rulePattern:T_patternElement;
      inlineValue:P_literal;
      alwaysTrue:boolean=false;
  begin
    if length(subrules)>0 then begin
      context.messages^.raiseSimpleError('Type definitions must have only one subrule and may not be overridden',rule^.getLocation);
      exit;
    end;
    setLength(subrules,1);
    subrules[0]:=rule;
    rule^.parent:=@self;
    {$ifdef fullVersion}
    if rule^.metaData.hasAttribute(SUPPRESS_UNUSED_WARNING_ATTRIBUTE) or
       rule^.metaData.hasAttribute(EXECUTE_AFTER_ATTRIBUTE) then setIdResolved;
    {$endif}
    if typedef=nil then begin
      rulePattern:=rule^.getPattern.getFirst;
      if rulePattern.isTypeCheckOnly then begin
        inlineValue:=rule^.getInlineValue;
        if inlineValue<>nil then begin
          alwaysTrue:=boolLit[true].equals(inlineValue);
          disposeLiteral(inlineValue);
        end;
      end else alwaysTrue:=false;
      new(typedef,create(getId,
                         rulePattern.getBuiltinTypeCheck,
                         rulePattern.getBuiltinCheckParameter,
                         rulePattern.getCustomTypeCheck,
                         P_expressionLiteral(rule^.rereferenced),
                         getRuleType=rt_duckTypeCheck,
                         alwaysTrue));
    end;
  end;

FUNCTION T_typeCheckRule.castRuleIsValid:boolean;
  begin
    result:=(getRuleType=rt_customTypeCheck) or subrules[0]^.hasValidValidCustomTypeCheckPattern(false);
  end;

PROCEDURE T_rule.resolveIds(CONST adapters:P_messages; CONST resolveIdContext:T_resolveIdContext{$ifdef fullVersion}; CONST functionCallInfos:P_functionCallInfos{$endif});
  begin
    raise Exception.create('Really? I mean, this should not be called!');
  end;

PROCEDURE T_delegatorRule.resolveIds(CONST adapters:P_messages; CONST resolveIdContext:T_resolveIdContext{$ifdef fullVersion}; CONST functionCallInfos:P_functionCallInfos{$endif});
  begin
    if localRule<>nil then localRule^.resolveIds(adapters,resolveIdContext{$ifdef fullVersion}, functionCallInfos{$endif});
  end;

PROCEDURE T_ruleWithSubrules.resolveIds(CONST adapters: P_messages; CONST resolveIdContext:T_resolveIdContext{$ifdef fullVersion}; CONST functionCallInfos:P_functionCallInfos{$endif});
  VAR s:P_subruleExpression;
  begin
    for s in subrules do s^.resolveIds(adapters,resolveIdContext{$ifdef fullVersion}, functionCallInfos{$endif});
  end;

FUNCTION T_delegatorRule.hasPublicSubrule: boolean;
  begin
    result:=(localRule<>nil) and localRule^.hasPublicSubrule;
  end;

FUNCTION T_variable.hasPublicSubrule: boolean;
  begin
    result:=not(privateRule);
  end;

FUNCTION T_ruleWithSubrules.hasPublicSubrule: boolean;
  VAR s:P_subruleExpression;
  begin
    for s in subrules do if s^.isPublic then exit(true);
    result:=false;
  end;

FUNCTION T_typeCastRule .hasPublicSubrule:boolean; begin result:=true; end;
FUNCTION T_typeCheckRule.hasPublicSubrule:boolean; begin result:=true; end;

{$ifdef fullVersion}
PROCEDURE T_rule.setIdResolved;
  begin
    if declarationStart.package<>nil then P_abstractPackage(declarationStart.package)^.anyCalled:=true;
    idResolved:=true;
  end;

PROCEDURE T_delegatorRule.setIdResolved;
  VAR r:P_ruleWithSubrules;
  begin
    if localRule<>nil then localRule^.setIdResolved;
    for r in imported do r^.setIdResolved;
    idResolved:=true;
  end;

PROCEDURE T_typeCastRule.setIdResolved;
  begin
    inherited setIdResolved;
    related^.setIdResolved;
  end;

{$endif}

FUNCTION T_ruleWithSubrules.getCmdLineHelpText: T_arrayOfString;
  VAR i:longint;
      txt:string;
  begin
    txt:=inherited getCmdLineHelpText[0];
    for i:=0 to length(subrules)-1 do txt:=txt+C_lineBreakChar+subrules[i]^.getCmdLineHelpText();
    result:=formatTabs(split(txt));
  end;

FUNCTION T_ruleWithSubrules.isReportable(OUT value: P_literal): boolean;
  begin
    value:=getInlineValue;
    if value=nil then exit(false);
    value^.unreference;
    result:=true;
  end;

FUNCTION T_ruleWithSubrules.getInlineValue: P_literal;
  begin
    if length(subrules)=1 then result:=subrules[0]^.getInlineValue
                          else result:=nil;
  end;

FUNCTION T_delegatorRule.replaces(CONST callLocation: T_tokenLocation; CONST param: P_listLiteral; OUT firstRep, lastRep: P_token; CONST context: P_abstractContext; VAR recycler: T_recycler; CONST calledFromDelegator: boolean): boolean;
  VAR r:P_rule;
  begin
    if (localRule<>nil) and localRule^.replaces(callLocation,param,firstRep,lastRep,context,recycler,true) then exit(true);
    for r in imported do if         r^.replaces(callLocation,param,firstRep,lastRep,context,recycler,true) then exit(true);
    if intOperator=tt_userRule then begin
      if (localRule<>nil) and localRule^.isFallbackPossible(callLocation,param,firstRep,lastRep,P_context(context)^,recycler) then exit(true);
      for r in imported do if         r^.isFallbackPossible(callLocation,param,firstRep,lastRep,P_context(context)^,recycler) then exit(true);
    end;
    result:=false;
  end;

FUNCTION T_ruleWithSubrules.replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean;
  VAR sub:P_subruleExpression;
  begin
    result:=false;
    for sub in subrules do if ((sub^.isPublic) or (sub^.getLocation.package=callLocation.package)) and sub^.replaces(param,callLocation,firstRep,lastRep,P_context(context)^,recycler) then exit(true);
    if (getId<>MAIN_RULE_ID) and not(calledFromDelegator)
    then result:=isFallbackPossible(callLocation,param,firstRep,lastRep,P_context(context)^,recycler);
  end;

FUNCTION T_protectedRuleWithSubrules.replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean;
  begin
    result:=false;
    if P_context(context)^.callDepth>=STACK_DEPTH_LIMIT then P_context(context)^.raiseError('Stack depth limit exceeded calling '+getId+'.',getLocation,mt_el4_systemError)
    else if inherited replaces(callLocation,param,firstRep,lastRep,context,recycler,calledFromDelegator) then begin
      system.enterCriticalSection(rule_cs);
      try
        result:=true;
        P_context(context)^.reduceExpression(firstRep,recycler);
        if firstRep<>nil then lastRep:=firstRep^.last else begin
          lastRep:=nil;
          result:=false;
        end;
      finally
        system.leaveCriticalSection(rule_cs);
      end;
    end;
  end;

FUNCTION T_memoizedRule.replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean;
{$MACRO ON}
{$define CLEAN_EXIT:=
if param=nil then disposeLiteral(useParam);
system.leaveCriticalSection(rule_cs);
exit}
  VAR lit:P_literal;
      useParam:P_listLiteral;
      sub:P_subruleExpression;
  PROCEDURE wrapResultInPutCacheRule;
    VAR newFirst,t:P_token;
    begin
      newFirst      :=recycler.newToken(firstRep^.location, getId+'.put.cache',tt_rulePutCacheValue,@self);
      newFirst^.next:=recycler.newToken(firstRep^.location, '', tt_parList_constructor,newListLiteral(1)^.append(useParam,true)); t:=newFirst^.next;
      t       ^.next:=firstRep;
      firstRep:=newFirst;
      lastRep^.next:=recycler.newToken(firstRep^.location, '', tt_braceClose);
      lastRep:=lastRep^.next;
    end;

  FUNCTION enterCriticalSectionWithDeadlockDetection:boolean; inline;
    CONST millesecondsBeforeRetry=10;
    begin
      while (tryEnterCriticalsection(rule_cs)=0) do begin
        if not(P_context(context)^.messages^.continueEvaluation) then exit(false);
        ThreadSwitch;
        sleep(millesecondsBeforeRetry);
      end;
      result:=true;
    end;

  begin
    result:=false;
    if param=nil then useParam:=newListLiteral
                 else useParam:=param;

    if not(enterCriticalSectionWithDeadlockDetection) then begin
      P_context(context)^.raiseError('Deadlock detected, trying to access memoized rule '+getId,callLocation);
      exit(false);
    end;
    lit:=cache.get(useParam);
    if lit<>nil then begin
      lit^.rereference;
      firstRep:=recycler.newToken(getLocation,'',tt_literal,lit);
      lastRep:=firstRep;
      CLEAN_EXIT(true);
    end else for sub in subrules do if ((sub^.isPublic) or (sub^.getLocation.package=callLocation.package)) and sub^.replaces(useParam,callLocation,firstRep,lastRep,P_context(context)^,recycler) then begin
      if (P_context(context)^.callDepth>=STACK_DEPTH_LIMIT) then begin wrapResultInPutCacheRule; CLEAN_EXIT(true); end;
      if (P_context(context)^.messages^.continueEvaluation) then P_context(context)^.reduceExpression(firstRep,recycler);
      if (P_context(context)^.messages^.continueEvaluation) and (firstRep^.next=nil) and (firstRep^.tokType=tt_literal) then begin
        lit:=firstRep^.data;
        cache.put(useParam,lit);
        lastRep:=firstRep;
      end else begin
        recycler.cascadeDisposeToken(firstRep);
        firstRep:=recycler.newToken(getLocation,'',tt_literal,newVoidLiteral);
        lastRep:=firstRep;
      end;
      CLEAN_EXIT(true);
    end;
    if not(calledFromDelegator) and isFallbackPossible(callLocation,param,firstRep,lastRep,P_context(context)^,recycler)
    then begin CLEAN_EXIT(true); end
    else begin CLEAN_EXIT(false); end;
  end;

FUNCTION T_typeCastRule.replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean;
  VAR cast:P_typableLiteral;
      raw :P_literal;
  begin
    if inherited replaces(callLocation,param,firstRep,lastRep,context,recycler) then begin
      if P_context(context)^.callDepth>STACK_DEPTH_LIMIT then begin
        P_context(context)^.raiseError('Stack overflow in typecast rule',callLocation,mt_el4_systemError);
        exit(false);
      end;
      inc(P_context(context)^.callDepth);
      raw:=P_context(context)^.reduceToLiteral(firstRep,recycler).literal;
      dec(P_context(context)^.callDepth);
      if not(P_context(context)^.messages^.continueEvaluation) then begin
        if raw<>nil then disposeLiteral(raw);
        exit(false);
      end;
    end else if (param<>nil) and (param^.size=1)
    then raw:=param^.value[0]^.rereferenced
    else exit(false);
    cast:=typedef^.cast(raw,callLocation,context,@recycler);
    disposeLiteral(raw);
    if cast=nil then exit(false)
    else begin
      result:=true;
      firstRep:=recycler.newToken(callLocation,'',tt_literal,cast);
      lastRep:=firstRep;
    end;
  end;

FUNCTION T_typeCheckRule.replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean;
  VAR boolResult:boolean=false;
  begin
    boolResult:=(param<>nil)
            and (param^.size=1)
            and (typedef<>nil)
            and (typedef^.matchesLiteral(param^.value[0],callLocation,context,@recycler));
    firstRep:=recycler.newToken(callLocation,'',tt_literal,newBoolLiteral(boolResult));
    lastRep:=firstRep;
    result:=true;
  end;

//FUNCTION T_variable.replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean;
//  begin
//    result:=(not(privateRule) or (getLocation.package=callLocation.package)) and ((param=nil) or (param^.size=0));
//    if result then begin
//      {$ifdef fullVersion}
//      if tco_stackTrace in P_context(context)^.threadOptions
//      then P_context(context)^.callStackPush(callLocation,@self,newCallParametersNode(param));
//      {$endif}
//      system.enterCriticalSection(rule_cs);
//      try
//        firstRep:=recycler.newToken(getLocation,'',tt_literal,namedValue.getValue);
//      finally
//        system.leaveCriticalSection(rule_cs);
//      end;
//      lastRep:=firstRep;
//      called:=true;
//      {$ifdef fullVersion}
//      if tco_stackTrace in P_context(context)^.threadOptions
//      then P_context(context)^.callStackPop(firstRep);
//      {$endif}
//    end else result:=isFallbackPossible(callLocation,param,firstRep,lastRep,P_context(context)^,recycler);
//  end;

//FUNCTION T_datastore.replaces(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler; CONST calledFromDelegator:boolean=false):boolean;
//  begin
//    result:=(not(privateRule) or (getLocation.package=callLocation.package)) and ((param=nil) or (param^.size=0));
//    if result then begin
//      {$ifdef fullVersion}
//      if tco_stackTrace in P_context(context)^.threadOptions
//      then P_context(context)^.callStackPush(callLocation,@self,newCallParametersNode(param));
//      {$endif}
//      system.enterCriticalSection(rule_cs);
//      try
//        readDataStore(P_context(context)^,recycler);
//        firstRep:=recycler.newToken(getLocation,'',tt_literal,namedValue.getValue);
//        lastRep:=firstRep;
//      finally
//        system.leaveCriticalSection(rule_cs);
//      end;
//      {$ifdef fullVersion}
//      if tco_stackTrace in P_context(context)^.threadOptions
//      then P_context(context)^.callStackPop(firstRep);
//      {$endif}
//    end else result:=isFallbackPossible(callLocation,param,firstRep,lastRep,P_context(context)^,recycler);
//  end;

FUNCTION T_typeCheckRule.getFirstParameterTypeWhitelist: T_literalTypeSet;
  begin
    result:=subrules[0]^.getPattern.getFirstParameterTypeWhitelist;
  end;

DESTRUCTOR T_typeCheckRule.destroy;
  begin
    if typedef<>nil then dispose(typedef,destroy);
    inherited destroy;
  end;

FUNCTION T_variable.getValueOrElseVoid(VAR context: T_context;
  VAR recycler: T_recycler): P_literal;
  begin
    result:=getValue(context,recycler);
    if result=nil then result:=newVoidLiteral;
  end;

FUNCTION T_variable.getValue(VAR context: T_context; VAR recycler: T_recycler
  ): P_literal;
  begin
    system.enterCriticalSection(rule_cs);
    try
      result:=namedValue.getValue;
    finally
      system.leaveCriticalSection(rule_cs);
    end;
  end;

FUNCTION T_variable.evaluateToLiteral(CONST callLocation: T_tokenLocation;
  CONST p1, p2: P_literal; VAR recycler: T_recycler;
  CONST context: P_abstractContext): P_literal;
begin
  raise Exception.create('T_variable.evaluateToLiteral must not be called');
end;

FUNCTION T_variable.evaluateToLiteral(CONST callLocation: T_tokenLocation;
  CONST parList: P_listLiteral; VAR recycler: T_recycler;
  CONST context: P_abstractContext): P_literal;
begin
  raise Exception.create('T_variable.evaluateToLiteral must not be called');
end;

FUNCTION T_variable.replaces(CONST callLocation: T_tokenLocation;
  CONST param: P_listLiteral; OUT firstRep, lastRep: P_token;
  CONST context: P_abstractContext; VAR recycler: T_recycler;
  CONST calledFromDelegator: boolean): boolean;
begin
  raise Exception.create('T_variable.replaces must not be called');
end;

FUNCTION T_datastore.getValue(VAR context:T_context; VAR recycler:T_recycler):P_literal;
  begin
    system.enterCriticalSection(rule_cs);
    try
      readDataStore(context,recycler);
      result:=namedValue.getValue;
    finally
      system.leaveCriticalSection(rule_cs);
    end;
  end;

FUNCTION T_rule.inspect(CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler):P_mapLiteral;
  begin
    result:=newMapLiteral^
      .put('type'    ,newStringLiteral(C_ruleTypeText[getRuleType]),false)^
      .put('location',newStringLiteral(getLocation                ),false)
      {$ifdef fullVersion}
      ^.put('used',newBoolLiteral(isIdResolved),false)
      {$endif};
  end;

FUNCTION T_delegatorRule.inspect(CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler):P_mapLiteral;
  begin
    if localRule=nil
    then result:=nil
    else result:=localRule^.inspect(includeFunctionPointer,context,recycler);
  end;

FUNCTION T_ruleWithSubrules.inspect(CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler): P_mapLiteral;
  FUNCTION subrulesList:P_listLiteral;
    VAR sub:P_subruleExpression;
    begin
      result:=newListLiteral(length(subrules));
      for sub in subrules do result^.append(sub^.inspect,false);
    end;

  begin
    result:=inherited inspect(includeFunctionPointer,context,recycler)^
            .put('subrules',subrulesList,false);
    if includeFunctionPointer then
    result^.put('function',getFunctionPointer(context,recycler,getLocation),false);
  end;

FUNCTION T_typeCastRule.inspect(CONST includeFunctionPointer:boolean; VAR context:T_context; VAR recycler:T_recycler):P_mapLiteral;
  FUNCTION privateOrPublic:string;
    begin
      if related^.hasPublicSubrule
      then result:=PUBLIC_TEXT
      else result:=PRIVATE_TEXT;
    end;

  FUNCTION subrulesList:P_listLiteral;
    VAR sub:P_subruleExpression;
    begin
      result:=newListLiteral(length(subrules)+1);
      result^.append(newMapLiteral^
        .put('pattern'   ,'()'           )^
        .put('location'  ,getLocation    )^
        .put('type'      ,privateOrPublic)^
        .put('comment'   ,related^.subrules[0]^.metaData.comment)^
        .put('attributes',related^.subrules[0]^.metaData.getAttributesLiteral,false),false);
      for sub in subrules do result^.append(sub^.inspect,false);
    end;

  begin
    result:=inherited inspect(includeFunctionPointer,context,recycler)^
            .put('subrules',subrulesList,false);
    if includeFunctionPointer then
    result^.put('function',getFunctionPointer(context,recycler,getLocation),false);
  end;

FUNCTION T_variable.inspect(CONST includeFunctionPointer: boolean;
  VAR context: T_context; VAR recycler: T_recycler): P_mapLiteral;
  FUNCTION privateOrPublic:string;
    begin
      if privateRule then result:=PRIVATE_TEXT
                     else result:=PUBLIC_TEXT;
    end;

  begin
    result:=newMapLiteral^
      .put('type'      ,newStringLiteral(privateOrPublic+' '+C_varTypeText[varType]),false)^
      .put('location'  ,newStringLiteral(getLocation ),false)^
      .put('comment'   ,newStringLiteral(meta.comment),false)^
      .put('attributes',meta.getAttributesLiteral,false);
  end;

FUNCTION T_delegatorRule.innerRuleType:T_ruleType;
  begin
    if localRule<>nil then result:=localRule^.getRuleType else result:=getRuleType;
  end;

FUNCTION T_ruleWithSubrules.getFunctionPointer(VAR context:T_context; VAR recycler:T_recycler; CONST location:T_tokenLocation):P_expressionLiteral;
  VAR tempToken:P_token=nil;
      arityInfo:T_arityInfo;
  begin
    if (getRuleType=rt_normal) and (length(subrules)=1) then exit(P_expressionLiteral(subrules[0]^.rereferenced));
    arityInfo:=arity;
    tempToken      :=recycler.newToken(location,getId,tt_userRule,@self);
    tempToken^.next:=getParametersForPseudoFuncPtr(arityInfo.minPatternLength,arityInfo.maxPatternLength<>arityInfo.minPatternLength,location,context,recycler);
    new(P_inlineExpression(result),createFromInline(tempToken,context,recycler,C_tokenDefaultId[tt_pseudoFuncPointer]+getId));
  end;

FUNCTION T_delegatorRule.getFunctionPointer(VAR context: T_context; VAR recycler: T_recycler; CONST location: T_tokenLocation): P_expressionLiteral;
  VAR tempToken:P_token=nil;
      arityInfo:T_arityInfo;
  begin
    if intOperator=tt_userRule then begin
      arityInfo:=arity;
      tempToken            :=recycler.newToken(location,getId,tt_userRule,@self);
      tempToken^.next      :=getParametersForPseudoFuncPtr(arityInfo.minPatternLength,arityInfo.maxPatternLength<>arityInfo.minPatternLength,location,context,recycler);
      new(P_inlineExpression(result),createFromInline(tempToken,context,recycler,C_tokenDefaultId[tt_pseudoFuncPointer]+getId));
    end else result:=getIntrinsicRuleAsExpression(intFuncForOperator[intOperator]);
  end;

FUNCTION T_protectedRuleWithSubrules.getFunctionPointer(VAR context:T_context; VAR recycler:T_recycler; CONST location:T_tokenLocation):P_expressionLiteral;
  VAR tempToken:P_token=nil;
      arityInfo:T_arityInfo;
  begin
    arityInfo:=arity;
    tempToken            :=recycler.newToken(location,getId,tt_userRule,@self);
    tempToken^.next      :=getParametersForPseudoFuncPtr(arityInfo.minPatternLength,arityInfo.maxPatternLength<>arityInfo.minPatternLength,location,context,recycler);
    new(P_inlineExpression(result),createFromInline(tempToken,context,recycler,C_tokenDefaultId[tt_pseudoFuncPointer]+getId));
  end;

FUNCTION T_typeCastRule.getFunctionPointer(VAR context:T_context; VAR recycler:T_recycler; CONST location:T_tokenLocation):P_expressionLiteral;
  VAR tempToken:P_token=nil;
  begin
    tempToken            :=recycler.newToken(location,getId,tt_userRule,@self);
    tempToken^.next      :=getParametersForPseudoFuncPtr(1,false,location,context,recycler);
    new(P_inlineExpression(result),createFromInline(tempToken,context,recycler,C_tokenDefaultId[tt_pseudoFuncPointer]+getId));
  end;

{$ifdef fullVersion}
FUNCTION T_delegatorRule.getDocTxt: string;
  VAR r:P_rule;
  begin
    if localRule<>nil then result:=localRule^.getDocTxt
                      else result:='';
    for r in imported do   result:=result+BoolToStr(result='','',C_lineBreakChar)+r^.getDocTxt;
  end;

FUNCTION T_ruleWithSubrules.getDocTxt: ansistring;
  VAR s:P_subruleExpression;
  begin
    result:='';
    for s in subrules do result:=result+C_lineBreakChar+s^.getDocTxt();
    result:=join(formatTabs(split(result)),LineEnding);
    result:=ECHO_MARKER+C_ruleTypeText[getRuleType]+'rule '+getId+' '+ansistring(getLocation)+result;
  end;

FUNCTION T_variable.getDocTxt: ansistring;
  begin
    result:=meta.getDocTxt+
            ECHO_MARKER+C_varTypeText[varType]+' variable '+getId+' '+ansistring(getLocation);
  end;
{$endif}

PROCEDURE T_variable.setMutableValue(CONST value: P_literal;
  CONST onDeclaration: boolean);
  begin
    system.enterCriticalSection(rule_cs);
    try
      namedValue.setValue(value);
      if not(onDeclaration) then begin
        valueChangedAfterDeclaration:=true;
        called:=true;
      end;
    finally
      system.leaveCriticalSection(rule_cs);
    end;
  end;

FUNCTION T_variable.isReportable(OUT value: P_literal): boolean;
  begin
    value:=namedValue.getValue;
    if value=nil then exit(false);
    value^.unreference;
    result:=true;
  end;

PROCEDURE T_datastore.readDataStore(VAR context:T_context; VAR recycler:T_recycler);
  VAR lit:P_literal;
  begin
    if not(called) or (not(valueChangedAfterDeclaration) and dataStoreMeta.fileChangedSinceRead) then begin
      lit:=dataStoreMeta.readValue(getLocation,context,recycler);
      if lit<>nil then begin
        namedValue.setValue(lit);
        lit^.unreference;
        called:=true;
      end;
    end;
  end;

FUNCTION T_variable.mutateInline(CONST mutation: T_tokenType; CONST RHS: P_literal; CONST location: T_tokenLocation; VAR context: T_context; VAR recycler: T_recycler): P_literal;
  begin
    system.enterCriticalSection(rule_cs);
    try
      result:=namedValue.mutate(mutation,RHS,location,@context,@recycler);
      valueChangedAfterDeclaration:=true;
      called:=true;
    finally
      system.leaveCriticalSection(rule_cs);
    end;
  end;

FUNCTION T_datastore.mutateInline(CONST mutation: T_tokenType; CONST RHS: P_literal; CONST location: T_tokenLocation; VAR context: T_context; VAR recycler:T_recycler): P_literal;
  begin
    system.enterCriticalSection(rule_cs);
    try
      if not(called) and not(valueChangedAfterDeclaration) then readDataStore(context,recycler);
      result:=inherited mutateInline(mutation,RHS,location,context,recycler);
    finally
      system.leaveCriticalSection(rule_cs);
    end;
  end;

PROCEDURE T_datastore.writeBack(CONST adapters:P_messages);
  VAR L:P_literal;
  begin
    if adapters^.continueEvaluation and valueChangedAfterDeclaration then begin
      L:=namedValue.getValue;
      dataStoreMeta.writeValue(L,getLocation,adapters,varType=vt_plainDatastore);
      disposeLiteral(L);
    end;
  end;

PROCEDURE T_datastore.memoryCleanup;
  begin
    enterCriticalSection(rule_cs);
    try
      if called and not(valueChangedAfterDeclaration) //The store has been read but not modified
         and (dataStoreMeta.fileExists)               //and the store can be re-read
      then begin
        namedValue.setValue(newVoidLiteral);
        called:=false;
      end;
    finally
      leaveCriticalSection(rule_cs);
    end;
  end;

FUNCTION T_datastore.isInitialized:boolean;
  begin
    result:=called or valueChangedAfterDeclaration;
  end;

PROCEDURE T_memoizedRule.clearCache;
  begin
    enterCriticalSection(rule_cs);
    try
      cache.clear;
    finally
      leaveCriticalSection(rule_cs);
    end;
  end;

FUNCTION T_memoizedRule.doPutCache(CONST param: P_listLiteral): P_literal;
  begin
    enterCriticalSection(rule_cs);
    try
      cache.put(P_listLiteral(param^.value[0]),
                              param^.value[1] );
      result:=param^.value[1]^.rereferenced;
    finally
      leaveCriticalSection(rule_cs);
    end;
  end;

end.
