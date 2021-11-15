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
    public
    FUNCTION getFunctionPointer(CONST context:P_context; CONST recycler:P_recycler; CONST location:T_tokenLocation):P_expressionLiteral; virtual; abstract;
    FUNCTION inspect({$WARN 5024 OFF}CONST includeFunctionPointer:boolean; CONST context:P_context; CONST recycler:P_recycler):P_mapLiteral; virtual;
    PROCEDURE resolveIds(CONST adapters:P_messages; CONST resolveIdContext:T_resolveIdContext); virtual;
    {$ifdef fullVersion}
    PROCEDURE fillCallInfos(CONST callInfos:P_callAndIdInfos); virtual; abstract;
    PROCEDURE checkParameters(CONST context:P_context); virtual;
    {$endif}
    FUNCTION evaluateToLiteral(CONST callLocation:T_tokenLocation; CONST p1,p2:P_literal;       CONST recycler:P_recycler; CONST context:P_abstractContext):P_literal; virtual;
    FUNCTION evaluateToLiteral(CONST callLocation:T_tokenLocation; CONST parList:P_listLiteral; CONST recycler:P_recycler; CONST context:P_abstractContext):P_literal; virtual;
    FUNCTION arity:T_arityInfo; virtual;
  end;

  P_ruleWithSubrules=^T_ruleWithSubrules;
  T_ruleWithSubrules=object(T_rule)
    private
      subrules:T_subruleArray;
      FUNCTION canCurry(CONST callLocation:T_tokenLocation; CONST givenParameters:P_listLiteral; OUT output:T_tokenRange; CONST context:P_context; CONST recycler:P_recycler):boolean;
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ruleTyp:T_ruleType=rt_normal);
      DESTRUCTOR destroy; virtual;
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subruleExpression; CONST context:P_context; CONST recycler:P_recycler); virtual;
      PROCEDURE resolveIds(CONST adapters:P_messages; CONST resolveIdContext:T_resolveIdContext); virtual;
      FUNCTION hasPublicSubrule:boolean; virtual;
      FUNCTION getCmdLineHelpText:T_arrayOfString; virtual;
      FUNCTION isReportable(OUT value:P_literal):boolean; virtual;
      FUNCTION getInlineValue:P_literal; virtual;
      PROPERTY getSubrules:T_subruleArray read subrules;
      FUNCTION canBeApplied(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT output:T_tokenRange; CONST context:P_abstractContext; CONST recycler:P_recycler):boolean; virtual;
      FUNCTION inspect(CONST includeFunctionPointer:boolean; CONST context:P_context; CONST recycler:P_recycler):P_mapLiteral; virtual;
      FUNCTION getFunctionPointer(CONST context:P_context; CONST recycler:P_recycler; CONST location:T_tokenLocation):P_expressionLiteral; virtual;
      {$ifdef fullVersion}
      PROCEDURE fillCallInfos(CONST callInfos:P_callAndIdInfos); virtual;
      FUNCTION hasAnnotationMarkingAsUsed:boolean; virtual;
      FUNCTION getStructuredInfo:T_structuredRuleInfoList; virtual;
      PROCEDURE checkParameters(CONST context:P_context); virtual;
      {$endif}
      {Returns the common arity of all subrules or -1 if arity differs or any subrule has optional parameters}
      FUNCTION arity:T_arityInfo; virtual;
      FUNCTION isPure:boolean; virtual;
  end;

  P_delegatorRule=^T_delegatorRule;
  T_delegatorRule=object(T_rule)
    private
      hiddenRule:P_intFuncCallback;
      intOperator:T_tokenType;
      isUnary:boolean;
      localRule:P_ruleWithSubrules;
      imported:array of P_ruleWithSubrules;
    public
      CONSTRUCTOR create(CONST id:T_idString; CONST declaredInPackage:P_abstractPackage);
      DESTRUCTOR destroy; virtual;
      PROCEDURE resolveIds(CONST adapters:P_messages; CONST resolveIdContext:T_resolveIdContext); virtual;
      FUNCTION hasPublicSubrule:boolean; virtual;
      FUNCTION isReportable(OUT value:P_literal):boolean; virtual;
      FUNCTION canBeApplied(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT output:T_tokenRange; CONST context:P_abstractContext; CONST recycler:P_recycler):boolean; virtual;
      FUNCTION inspect(CONST includeFunctionPointer:boolean; CONST context:P_context; CONST recycler:P_recycler):P_mapLiteral; virtual;
      FUNCTION getFunctionPointer(CONST context:P_context; CONST recycler:P_recycler; CONST location:T_tokenLocation):P_expressionLiteral; virtual;
      FUNCTION innerRuleType:T_ruleType; virtual;
      {$ifdef fullVersion}
      PROCEDURE fillCallInfos(CONST callInfos:P_callAndIdInfos); virtual;
      FUNCTION hasAnnotationMarkingAsUsed:boolean; virtual;
      FUNCTION getStructuredInfo:T_structuredRuleInfoList; virtual;
      PROCEDURE checkParameters(CONST context:P_context); virtual;
      {$endif}
      {Returns the common arity of all subrules or -1 if arity differs or any subrule has optional parameters}
      FUNCTION arity:T_arityInfo; virtual;

      PROCEDURE clearImported;
      PROCEDURE addRule(CONST ruleOrDelegateToAdd:P_abstractRule);
      PROPERTY getLocalRule:P_ruleWithSubrules read localRule;
      FUNCTION isPure:boolean; virtual;
  end;

  P_variable=^T_variable;
  P_protectedRuleWithSubrules=^T_protectedRuleWithSubrules;
  T_protectedRuleWithSubrules=object(T_ruleWithSubrules)
    private
      rule_cs:system.TRTLCriticalSection;
      usedGlobalVariables:T_arrayOfPointer;
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ruleTyp:T_ruleType=rt_synchronized);
      DESTRUCTOR destroy; virtual;
      PROCEDURE resolveIds(CONST adapters:P_messages; CONST resolveIdContext:T_resolveIdContext); virtual;
      FUNCTION canBeApplied(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT output:T_tokenRange; CONST context:P_abstractContext; CONST recycler:P_recycler):boolean; virtual;
      FUNCTION getFunctionPointer(CONST context:P_context; CONST recycler:P_recycler; CONST location:T_tokenLocation):P_expressionLiteral; virtual;
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
      FUNCTION canBeApplied(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT output:T_tokenRange; CONST context:P_abstractContext; CONST recycler:P_recycler):boolean; virtual;
      FUNCTION isPure:boolean; virtual;
  end;

  P_typeCheckRule=^T_typeCheckRule;
  P_typeCastRule=^T_typeCastRule;
  T_typeCastRule=object(T_ruleWithSubrules)
    private
      typedef:P_typedef;
      related:P_typeCheckRule;
    public
      CONSTRUCTOR create(CONST def:P_typedef; CONST relatedCheckRule:P_typeCheckRule);
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subruleExpression; CONST context:P_context; CONST recycler:P_recycler); virtual;
      FUNCTION canBeApplied(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT output:T_tokenRange; CONST context:P_abstractContext; CONST recycler:P_recycler):boolean; virtual;
      FUNCTION getFunctionPointer(CONST context:P_context; CONST recycler:P_recycler; CONST location:T_tokenLocation):P_expressionLiteral; virtual;
      FUNCTION hasPublicSubrule:boolean; virtual;
      FUNCTION getRootId:T_idString; virtual;
      FUNCTION inspect(CONST includeFunctionPointer:boolean; CONST context:P_context; CONST recycler:P_recycler):P_mapLiteral; virtual;
      FUNCTION arity:T_arityInfo; virtual;
  end;

  T_typeCheckRule=object(T_ruleWithSubrules)
    private
      typedef:P_typedef;
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ducktyping:boolean);
      PROCEDURE addOrReplaceSubRule(CONST rule:P_subruleExpression; CONST context:P_context; CONST recycler:P_recycler); virtual;
      FUNCTION canBeApplied(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT output:T_tokenRange; CONST context:P_abstractContext; CONST recycler:P_recycler):boolean; virtual;
      FUNCTION getFirstParameterTypeWhitelist:T_literalTypeSet; virtual;
      FUNCTION getRootId:T_idString; virtual;
      FUNCTION hasPublicSubrule:boolean; virtual;
      FUNCTION castRuleIsValid:boolean;
      DESTRUCTOR destroy; virtual;
      FUNCTION arity:T_arityInfo; virtual;
  end;

  T_variableState=(vs_uninitialized,  //set on construction
                   vs_initialized,    //set on declaration
                   vs_modified,       //set on mutate
                   vs_readFromFile,   //set on datastore read
                   vs_inSyncWithFile);//set on datastore write

  T_variable=object(T_abstractRule)
    private
      varType:T_variableType;
      privateRule:boolean;
      state:T_variableState;
      namedValue:T_namedVariable;
      meta:T_ruleMetaData;
    public
      PROPERTY metaData:T_ruleMetaData read meta;
      PROPERTY getVariableType:T_variableType read varType;

      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; VAR meta_:T_ruleMetaData; CONST isPrivate:boolean; CONST variableType:T_variableType);
      DESTRUCTOR destroy; virtual;
      FUNCTION hasPublicSubrule:boolean; virtual;
      PROCEDURE setMutableValue(CONST value:P_literal; CONST onDeclaration:boolean; CONST literalRecycler:P_literalRecycler); virtual;
      FUNCTION mutateInline(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal; virtual;
      FUNCTION isReportable(OUT value:P_literal):boolean; virtual;
      FUNCTION inspect(CONST includeFunctionPointer:boolean; CONST context:P_context; CONST recycler:P_recycler):P_mapLiteral; virtual;
      {$ifdef fullVersion}
      FUNCTION hasAnnotationMarkingAsUsed:boolean; virtual;
      FUNCTION getStructuredInfo:T_structuredRuleInfoList; virtual;
      {$endif}
      FUNCTION getValueOrElseVoid(CONST context:P_context; CONST recycler:P_recycler):P_literal;
      FUNCTION getValue(CONST context:P_context; CONST recycler:P_recycler):P_literal; virtual;

      {Part of T_abstractRule, but should not be called and throws exception}
      FUNCTION evaluateToLiteral(CONST callLocation:T_tokenLocation; CONST p1,p2:P_literal;       CONST recycler:P_recycler; CONST context:P_abstractContext):P_literal; virtual;
      {Part of T_abstractRule, but should not be called and throws exception}
      FUNCTION evaluateToLiteral(CONST callLocation:T_tokenLocation; CONST parList:P_listLiteral; CONST recycler:P_recycler; CONST context:P_abstractContext):P_literal; virtual;
      {Part of T_abstractRule, but should not be called and throws exception}
      FUNCTION canBeApplied(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT output:T_tokenRange; CONST context:P_abstractContext; CONST recycler:P_recycler):boolean; virtual;
      FUNCTION isPure:boolean; virtual;
  end;

  P_datastore=^T_datastore;
  T_datastore=object(T_variable)
    private
      dataStoreMeta:T_datastoreMeta;
      PROCEDURE readDataStore(CONST context:P_context; CONST recycler:P_recycler);
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; VAR meta_:T_ruleMetaData; CONST datastorePackage:P_objectWithPath; CONST isPrivate:boolean; CONST variableType:T_variableType);
      DESTRUCTOR destroy; virtual;
      FUNCTION mutateInline(CONST mutation:T_tokenType; CONST RHS:P_literal; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal; virtual;
      FUNCTION writeBackToTemp(CONST adapters:P_messages; VAR flush:T_datastoreFlush):boolean;
      FUNCTION getValue(CONST context:P_context; CONST recycler:P_recycler):P_literal; virtual;
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
      PROCEDURE disposeValue(VAR v:MY_VALUE_TYPE); virtual;
    public
      CONSTRUCTOR create(CONST package:P_abstractPackage);
      PROCEDURE clear;
      FUNCTION addImports(CONST other:P_ruleMap):boolean;
      FUNCTION getOperators:T_customOperatorArray;
      PROCEDURE clearImports;
      PROCEDURE declare(CONST ruleId: T_idString;
                        CONST modifiers: T_modifierSet;
                        CONST ruleDeclarationStart: T_tokenLocation;
                        CONST context:P_context;
                        CONST recycler:P_recycler;
                        VAR metaData:T_ruleMetaData;
                        subRule:P_subruleExpression);
      FUNCTION getLocalMain:P_rule;
      FUNCTION getAllLocalRules:T_ruleList;
      PROCEDURE executeAfterRules(CONST context:P_context; CONST recycler:P_recycler);
      FUNCTION writeBackDatastores(CONST messages:P_messages; CONST literalRecycler:P_literalRecycler; VAR flush:T_datastoreFlush):boolean;
      FUNCTION getTypeMap:T_typeMap;
      PROCEDURE resolveRuleIds(CONST messages:P_messages; CONST resolveIdContext:T_resolveIdContext);
      FUNCTION inspect(CONST context:P_context; CONST recycler:P_recycler; CONST includeFunctionPointer:boolean):P_mapLiteral;
      {$ifdef fullVersion}
      PROCEDURE updateLists(VAR userDefinedRules:T_setOfString; CONST forCompletion:boolean);
      PROCEDURE complainAboutUnused(CONST messages:P_messages; CONST functionCallInfos:P_callAndIdInfos);
      PROCEDURE fillCallInfos(CONST functionCallInfos:P_callAndIdInfos);
      {$endif}
  end;

FUNCTION isIdOfAnyOverloadableOperator(CONST id:T_idString):boolean;
FUNCTION createPrimitiveAggregatorLiteral(CONST tok:P_token; CONST context:P_context):P_expressionLiteral;
IMPLEMENTATION
USES mySys, operators,fileWrappers;

FUNCTION createPrimitiveAggregatorLiteral(CONST tok:P_token; CONST context:P_context):P_expressionLiteral;
  begin
    if      tok^.tokType in C_operators   then result:=builtinFunctionMap.getIntrinsicRuleAsExpression(intFuncForOperator[tok^.tokType],true)
    else if tok^.tokType=tt_intrinsicRule then result:=builtinFunctionMap.getIntrinsicRuleAsExpression(P_intFuncCallback (tok^.data)   ,true)
    else begin
      result:=nil;
      assert(false);
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
      delegator:P_delegatorRule;
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
       assert(P_rule(entry.value)^.getRuleType<>rt_delegate,'This should not happen; entry must not be a delegate at this point');
       if P_rule(earlierEntry.value)^.getRuleType=rt_delegate then begin
         assert(not(earlierEntry.isImported),'This should not happen; earlierEntry must not be imported at this point');
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
    end else begin
      if builtinFunctionMap.containsKey(id) and not(isOperatorName(id)) then begin
        new(delegator,create(id,localPackage));
        if entry.isImported then begin
          //An imported rule is converted to a locally declared delegator
          setLength(delegator^.imported,1);
          delegator^.imported[0]:=P_ruleWithSubrules(entry.value);
          entry.isImported:=false;
        end else begin
          delegator^.localRule:=P_ruleWithSubrules(entry.value);
        end;
        entry.value:=delegator;
      end;
      put(id,entry);
    end;
  end;

PROCEDURE T_ruleMap.disposeValue(VAR v: MY_VALUE_TYPE);
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
      recycler:P_recycler;
  begin
    inherited clear;
    recycler:=newRecycler;
    for i:=0 to length(afterRules)-1 do recycler^.disposeLiteral(afterRules[i]);
    setLength(afterRules,0);
    {$ifdef fullVersion}
    suppressAllUnusedWarnings:=false;
    {$endif}
    freeRecycler(recycler);
  end;

FUNCTION T_ruleMap.addImports(CONST other: P_ruleMap): boolean;
  FUNCTION qualifiedId(CONST o:P_objectWithIdAndLocation):T_idString;
    begin
      result:=o^.getId;
      if not(isQualified(result)) then result:=other^.localPackage^.getId + ID_QUALIFY_CHARACTER + result;
    end;

  VAR entryToMerge:T_ruleMap.KEY_VALUE_PAIR;
      newEntry:T_ruleMapEntry;
  begin
    result:=false;
    for entryToMerge in other^.entrySet do if not(entryToMerge.value.isImported) and entryToMerge.value.hasPublicSubrule then begin
      newEntry.isImported:=true;
      newEntry.entryType :=entryToMerge.value.entryType;
      newEntry.value     :=entryToMerge.value.value;
      if (newEntry.entryType=tt_userRule) and (P_abstractRule(newEntry.value)^.getRuleType=rt_delegate)
      then begin
        newEntry.value:=P_delegatorRule(newEntry.value)^.localRule;
        if (newEntry.value<>nil) and not(P_rule(newEntry.value)^.hasPublicSubrule)
        then newEntry.value:=nil;
      end;
      if newEntry.value<>nil then begin
        //Qualified entries are always added
        put(qualifiedId(newEntry.value),newEntry);

        if not(isQualified(entryToMerge.key)) and
          (entryToMerge.key<>MAIN_RULE_ID) then //"main" of imported packages can only be accessed using its qualified id "<package>.main"
        begin
          if mergeEntry(entryToMerge.key,newEntry) then result:=true;
        end;
      end;
    end;
  end;

FUNCTION isIdOfAnyOverloadableOperator(CONST id:T_idString):boolean;
  var op: T_tokenType;
  begin
    for op:=low(T_customOperatorArray) to high(T_customOperatorArray) do
    if id=operatorName[op] then exit(true);
    result:=false;
  end;

FUNCTION T_ruleMap.getOperators: T_customOperatorArray;
  VAR op:T_tokenType;
      entry:T_ruleMapEntry;
  begin
    for op:=low(T_customOperatorArray) to high(T_customOperatorArray) do begin
      if containsKey(operatorName[op],entry)
      then result[op]:=P_abstractRule(entry.value)
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
                            CONST context:P_context;
                            CONST recycler:P_recycler;
                            VAR metaData: T_ruleMetaData;
                            subRule: P_subruleExpression);
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
        context^.raiseError('main rules must not have any modifiers',ruleDeclarationStart);
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
        context^.raiseError('Invalid combination of modifiers: '+s,ruleDeclarationStart);
        exit(false);
      end;

      if builtinFunctionMap.containsFunctionForId(ruleId,builtinRuleBeingOverridden) and (metaType<>tt_userRule) then begin
        //You can only overload builtin rules, not hide them altogether
        context^.raiseError('Declaration of '+BoolToStr(metaType=tt_globalVariable,'variable','type')+' would hide a builtin rule of the same name',ruleDeclarationStart);
        exit(false);
      end;

      if (metaType<>tt_globalVariable) and (subRule=nil) then begin
        context^.raiseError('Missing rule body',ruleDeclarationStart);
        exit(false);
      end;

      for op in allOperators do if operatorName[op]=ruleId then begin
        ruleType:=rt_customOperator;
        operatorBeingOverridden:=op;
      end;
      if ruleType=rt_customOperator then begin
        if not(operatorBeingOverridden in overridableOperators) then begin
          context^.raiseError('Operator '+C_tokenDefaultId[operatorBeingOverridden]+' cannot be overridden',ruleDeclarationStart);
          exit(false);
        end;
        for m in [modifier_mutable,
                  modifier_datastore,
                  modifier_plain,
                  modifier_curry,
                  modifier_synchronized,
                  modifier_customType,
                  modifier_customDuckType] do if m in modifiers then context^.raiseError('modifier '+C_modifierInfo[m].name+' is not allowed when overriding operators',ruleDeclarationStart);
        if modifier_private  in modifiers then context^.messages^.postTextMessage(mt_el2_warning  ,ruleDeclarationStart,'private modifier is ignored when overloading operators' );
        if modifier_memoized in modifiers then context^.messages^.postTextMessage(mt_el2_warning  ,ruleDeclarationStart,'memoized modifier is ignored when overloading operators');
      end;
      result:=context^.continueEvaluation;
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
            then context^.messages^.postTextMessage(mt_el1_note,ruleDeclarationStart,'Rule '+ruleId+' hides imported '+BoolToStr(entryForId.entryType=tt_globalVariable,'variable','type')+' of same name')
            else begin
              context^.raiseError('Cannot declare rule '+ruleId+' because there is a '+BoolToStr(entryForId.entryType=tt_globalVariable,'variable','type')+' of the same name',ruleDeclarationStart);
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
                context^.raiseError('Colliding modifiers! Rule '+ruleId+' is '+C_ruleTypeText[rule^.getRuleType]+', redeclared as '+C_ruleTypeText[ruleType],ruleDeclarationStart);
                exit;
              end else if (rule^.getRuleType in C_ruleTypesWithOnlyOneSubrule) then begin
                context^.raiseError(C_ruleTypeText[rule^.getRuleType]+'rules must have exactly one subrule',ruleDeclarationStart);
                exit;
              end;
            end;
          end;
        end;
      end else rule:=nil;

      if rule=nil then begin
        newEntry:=true;
        case ruleType of
          rt_memoized    ,rt_memoized_curry    : new(P_memoizedRule             (rule),create(ruleId,ruleDeclarationStart,ruleType));
          rt_synchronized,rt_synchronized_curry: new(P_protectedRuleWithSubrules(rule),create(ruleId,ruleDeclarationStart,ruleType));
          rt_normal      ,rt_normal_curry,
          rt_customOperator                    : new(P_ruleWithSubrules         (rule),create(ruleId,ruleDeclarationStart,ruleType));
          else assert(false,'Unexpected rule type '+C_ruleTypeText[ruleType]);
        end;
      end;
      assert(rule<>nil);
      assert(rule^.getRuleType in [rt_memoized,rt_memoized_curry,rt_synchronized,rt_synchronized_curry,rt_normal,rt_normal_curry,rt_customOperator,rt_customTypeCast,rt_customTypeCheck]);

      P_ruleWithSubrules(rule)^.addOrReplaceSubRule(subRule,context,recycler);

      if newEntry then begin
        entryForId.isImported:=false;
        entryForId.entryType:=tt_userRule;
        entryForId.value:=rule;
        if mergeEntry(ruleId,entryForId) then resolveRuleIds(nil,ON_DELEGATION);
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
        end else context^.messages^.postTextMessage(mt_el2_warning,ruleDeclarationStart,'Only nullary functions can be executed automatically on package finalization');
      end;
    end;

  PROCEDURE declareVariable;
    VAR entryForId:T_ruleMapEntry;
        newVar:P_variable;
        variableValue:P_literal;
    begin
      if containsKey(ruleId,entryForId) then begin
        if entryForId.isImportedOrDelegateWithoutLocal
        then context^.messages^.postTextMessage(mt_el1_note,ruleDeclarationStart,ruleId+' hides an imported rule of same name')
        else begin
          context^.raiseError('Variable '+ruleId+' cannot be declared because a local rule of the same name already exists',ruleDeclarationStart);
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
        recycler^.disposeLiteral(subRule);
        if variableValue<>nil then begin
          newVar^.setMutableValue(variableValue,true,recycler);
          recycler^.disposeLiteral(variableValue);
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
        then context^.raiseError('Invalid pattern/signature for custom type check! Must accept exactly one List, Set, Map or Expression parameter.',ruleDeclarationStart)
        else context^.raiseError('Invalid pattern/signature for custom type check! Must accept exactly one parameter.',ruleDeclarationStart);
        exit;
      end;
      castable :=subRule^.hasValidValidCustomTypeCheckPattern(false);
      if containsKey('is'+ruleId,checkEntry) and not(checkEntry.isImported) then begin
        context^.raiseError('You cannot declare type '+ruleId+' because the related type check rule is'+ruleId+' already exists',ruleDeclarationStart);
        exit;
      end;
      if castable and containsKey('to'+ruleId,castEntry) and not(castEntry.isImported) then begin
        context^.raiseError('You cannot declare type '+ruleId+' because the related type cast rule to'+ruleId+' already exists',ruleDeclarationStart);
        exit;
      end;
      if containsKey(ruleId,entryForId) and not(entryForId.isImported) then begin
        context^.raiseError('You cannot declare type '+ruleId+' because a rule of the same name already exists',ruleDeclarationStart);
        exit;
      end;
      if not(ruleId[1] in ['A'..'Z'])
      then context^.messages^.postTextMessage(mt_el2_warning,ruleDeclarationStart,'Type names should begin with an uppercase letter');

      new(checkRule,create(ruleId,ruleDeclarationStart,ruleType=rt_duckTypeCheck));
      checkRule^.addOrReplaceSubRule(subRule,context,recycler);
      if not(context^.continueEvaluation) then begin
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
      if needIdRefresh then resolveRuleIds(nil,ON_DELEGATION);
    end;

  begin
    {$ifdef fullVersion}
    suppressAllUnusedWarnings:=suppressAllUnusedWarnings or (metaData.getAttribute(SUPPRESS_UNUSED_WARNING_ATTRIBUTE).value=SUPPRESS_ALL_UNUSED_VALUE);
    {$endif}
    if checkModifiers then case metaType of
      tt_userRule:       declareRule;
      tt_globalVariable: declareVariable;
      tt_customType:     declareType;
    end else if subRule<>nil then recycler^.disposeLiteral(subRule);
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

PROCEDURE T_ruleMap.executeAfterRules(CONST context:P_context;
  CONST recycler:P_recycler);
  VAR s:P_subruleExpression;
  begin
    for s in afterRules do s^.evaluate(packageTokenLocation(localPackage),context,recycler,nil);
  end;

FUNCTION T_ruleMap.writeBackDatastores(CONST messages: P_messages; CONST literalRecycler:P_literalRecycler; VAR flush:T_datastoreFlush):boolean;
  VAR entry:T_ruleMapEntry;
  begin
    result:=false;
    for entry in valueSet do
    if not(entry.isImported) and (entry.entryType=tt_globalVariable) and (P_variable(entry.value)^.varType in [vt_datastore,vt_plainDatastore])
    then begin
      if P_datastore(entry.value)^.writeBackToTemp(messages,flush) then result:=true;
    end;
  end;

FUNCTION T_ruleMap.getTypeMap: T_typeMap;
  VAR entry:T_ruleMapEntry;
  begin
    result.create();
    for entry in valueSet do
    if not(entry.isImported) and (entry.entryType=tt_customType) then
    result.put(entry.value^.getId,P_typedef(entry.value));
  end;

PROCEDURE T_ruleMap.resolveRuleIds(CONST messages: P_messages; CONST resolveIdContext: T_resolveIdContext);
  VAR entry:T_ruleMapEntry;
  begin
    for entry in valueSet do
    if not(entry.isImported) and (entry.entryType=tt_userRule)
    then P_rule(entry.value)^.resolveIds(messages,resolveIdContext);
  end;

FUNCTION T_ruleMap.inspect(CONST context:P_context; CONST recycler:P_recycler; CONST includeFunctionPointer:boolean) : P_mapLiteral;
  VAR entry:T_ruleMapEntry;
  begin
    result:=newMapLiteral(size);
    for entry in valueSet do if not(entry.isImportedOrDelegateWithoutLocal) then case entry.entryType of
      tt_userRule:
        result^.put(recycler,entry.value^.getId,P_rule(entry.value)^.inspect(includeFunctionPointer,context,recycler),false);
      tt_globalVariable:
        result^.put(recycler,entry.value^.getId,P_variable(entry.value)^.inspect(includeFunctionPointer,context,recycler),false);
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

PROCEDURE T_ruleMap.fillCallInfos(CONST functionCallInfos:P_callAndIdInfos);
  VAR entry:KEY_VALUE_PAIR;
  begin
    try
      resolveRuleIds(nil,ON_DELEGATION);
      for entry in entrySet do if entry.value.entryType=tt_userRule then
        P_rule(entry.value.value)^.fillCallInfos(functionCallInfos);
    except
      functionCallInfos^.clear;
    end;
  end;

PROCEDURE T_ruleMap.complainAboutUnused(CONST messages: P_messages; CONST functionCallInfos:P_callAndIdInfos);
  VAR entry:T_ruleMapEntry;
      rule:P_abstractRule;
  begin
    if suppressAllUnusedWarnings or (functionCallInfos=nil) then exit;
    fillCallInfos(functionCallInfos);
    for entry in valueSet do if not(entry.isImportedOrDelegateWithoutLocal) and (entry.entryType in [tt_userRule,tt_globalVariable]) then begin
      rule:=P_abstractRule(entry.value);
      if not(rule^.hasAnnotationMarkingAsUsed) and
         not(functionCallInfos^.isLocationReferenced(rule^.getLocation))
      then begin
        messages^.postTextMessage(mt_el2_warning,P_objectWithIdAndLocation(entry.value)^.getLocation,
        'Unused rule '+P_objectWithIdAndLocation(entry.value)^.getId+
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
      assert(localRule=nil);
      localRule:=ruleToAdd;
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

FUNCTION T_ruleWithSubrules.canCurry(CONST callLocation: T_tokenLocation; CONST givenParameters: P_listLiteral; OUT output:T_tokenRange; CONST context:P_context; CONST recycler:P_recycler): boolean;
  VAR tempToken:P_token;
      tempInline:P_inlineExpression;
      parHead,parTail:P_listLiteral;
  begin
    if not(getRuleType in [rt_normal_curry,rt_memoized_curry,rt_synchronized_curry]) or
       (givenParameters=nil) or
       (arity.maxPatternLength<>arity.minPatternLength) then exit(false);
    result:=false;
    if (givenParameters^.size<arity.minPatternLength) then begin
      //CURRY
      //  rule : f(x,y)->...
      //  input: f(x)
      //  out  : {f(x,$y)}
      tempToken      :=recycler^.newToken(callLocation,getId,tt_userRule,@self);
      tempToken^.next:=getParametersForUncurrying(givenParameters,arity.minPatternLength,callLocation,context,recycler);
      new(tempInline,createFromInline(tempToken,context,recycler,C_tokenDefaultId[tt_pseudoFuncPointer]+getId));
      output.first:=recycler^.newToken(callLocation,'',tt_literal,tempInline);
      output.last :=output.first;
      result:=true;
    end else if (givenParameters^.size>arity.minPatternLength) then begin
      //UNCURRY
      //  rule : f(x)->...
      //  input: f(x,y)
      //  out  : f(x)(y)
      parHead:=givenParameters^.head(recycler,arity.minPatternLength);
      if canBeApplied(callLocation,parHead,output,context,recycler) then begin
        parTail:=givenParameters^.tail(recycler,arity.minPatternLength);
        tempToken:=recycler^.newToken(output.first^.location,'',tt_braceOpen);
        tempToken^.next:=output.first;
        output.first:=tempToken;

        output.last^.next:=recycler^.newToken(output.first^.location,'',tt_braceClose);
        output.last:=output.last^.next;
        output.last^.next:=recycler^.newToken(output.first^.location,'',tt_parList,parTail);
        output.last:=output.last^.next;
        result:=true;
      end;
      recycler^.disposeLiteral(parHead);
    end;
  end;

FUNCTION T_rule.evaluateToLiteral(CONST callLocation:T_tokenLocation; CONST p1,p2:P_literal; CONST recycler:P_recycler; CONST context:P_abstractContext):P_literal;
  VAR parList:P_listLiteral;
      rep:T_tokenRange;
  begin
    if P_context(context)^.callDepth>=STACK_DEPTH_LIMIT then begin
      P_context(context)^.raiseError('Stack overflow in rule '+getId+' ('+string(getLocation)+')',callLocation,mt_el4_systemError);
      exit(nil);
    end;
    inc(P_context(context)^.callDepth);
    parList:=P_listLiteral(recycler^.newListLiteral(2)
      ^.append(recycler,p1,true)
      ^.append(recycler,p2,true));
    if canBeApplied(callLocation,parList,rep,context,recycler)
    then result:=P_context(context)^.reduceToLiteral(rep.first,recycler).literal
    else result:=nil;
    dec(P_context(context)^.callDepth);
    recycler^.disposeLiteral(parList);
  end;

FUNCTION T_rule.evaluateToLiteral(CONST callLocation:T_tokenLocation; CONST parList:P_listLiteral; CONST recycler:P_recycler; CONST context:P_abstractContext):P_literal;
  VAR rep:T_tokenRange;
  begin
    if P_context(context)^.callDepth>=STACK_DEPTH_LIMIT then begin
      P_context(context)^.raiseError('Stack overflow in rule '+getId+' ('+string(getLocation)+')',callLocation,mt_el4_systemError);
      exit(nil);
    end;
    inc(P_context(context)^.callDepth);
    if canBeApplied(callLocation,parList,rep,context,recycler)
    then result:=P_context(context)^.reduceToLiteral(rep.first,recycler).literal
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

FUNCTION T_ruleWithSubrules.isPure:boolean;
  VAR i:longint;
  begin
    for i:=0 to length(subrules)-1 do if subrules[i]^.getSideEffects<>[] then exit(false);
    result:=true;
  end;

FUNCTION T_delegatorRule.isPure:boolean;
  VAR i:longint;
  begin
    result:=((hiddenRule=nil) or (builtinFunctionMap.getMeta(hiddenRule)^.sideEffects=[])
        and  (localRule=nil)  or (localRule^.isPure));
    if result then for i:=0 to length(imported)-1 do if not(imported[i]^.isPure) then exit(false);
  end;

FUNCTION T_memoizedRule.isPure:boolean;
  begin
    result:=true;
  end;

FUNCTION T_variable.isPure:boolean;
  begin
    result:=false;
  end;

{$ifdef fullVersion}
PROCEDURE T_rule.checkParameters(CONST context:P_context);
  begin end;

PROCEDURE T_ruleWithSubrules.checkParameters(CONST context:P_context);
  VAR s:P_subruleExpression;
      patterns:T_arrayOfPpattern;
      distinction:T_arrayOfLongint;
      i:longint;
  begin
    if length(subrules)>1 then begin
      setLength(patterns,length(subrules));
      for i:=0 to length(subrules)-1 do patterns[i]:=@(subrules[i]^.getPattern);
      distinction:=extractIdsForCaseDistinction(patterns);
      for s in subrules do append(distinction,s^.getUsedParameters);
    end else distinction:=C_EMPTY_LONGINT_ARRAY;
    sortUnique(distinction);
    for s in subrules do s^.checkParameters(distinction,context);
  end;

PROCEDURE T_delegatorRule.checkParameters(CONST context:P_context);
  VAR r:P_rule;
  begin
    if localRule<>nil then localRule^.checkParameters(context);
    for r in imported do           r^.checkParameters(context);
  end;
{$endif}

CONSTRUCTOR T_ruleWithSubrules.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation; CONST ruleTyp: T_ruleType);
  begin
    inherited create(ruleId,startAt,ruleTyp);
    setLength(subrules,0);
  end;

CONSTRUCTOR T_delegatorRule.create(CONST id:T_idString; CONST declaredInPackage:P_abstractPackage);
  VAR op:T_tokenType;
  begin
    inherited create(id,packageTokenLocation(declaredInPackage),rt_delegate);
    localRule:=nil;
    setLength(imported,0);
    intOperator:=tt_userRule;
    for op in overridableOperators do if id=operatorName[op] then intOperator:=op;
    isUnary:=intOperator in unaryOperators;
    if (intOperator=tt_userRule) and not(builtinFunctionMap.containsFunctionForId(id,hiddenRule)) then hiddenRule:=nil;
  end;

CONSTRUCTOR T_protectedRuleWithSubrules.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation; CONST ruleTyp: T_ruleType);
  begin
    initCriticalSection(rule_cs);
    setLength(usedGlobalVariables,0);
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
    related:=relatedCheckRule;
  end;

CONSTRUCTOR T_typeCheckRule.create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ducktyping:boolean);
  begin
    if ducktyping
    then inherited create(ruleId,startAt,rt_duckTypeCheck)
    else inherited create(ruleId,startAt,rt_customTypeCheck);
    typedef:=nil;
  end;

CONSTRUCTOR T_variable.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation; VAR meta_: T_ruleMetaData; CONST isPrivate: boolean; CONST variableType: T_variableType);
  begin
    inherited create(ruleId,startAt,rt_normal);
    varType:=variableType;
    privateRule:=isPrivate;
    namedValue.create(ruleId,newVoidLiteral,false);
    state:=vs_uninitialized;
    meta:=meta_;
  end;

CONSTRUCTOR T_datastore.create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; VAR meta_:T_ruleMetaData; CONST datastorePackage:P_objectWithPath; CONST isPrivate:boolean; CONST variableType:T_variableType);
  begin
    inherited create(ruleId,startAt,meta_,isPrivate,variableType);
    dataStoreMeta.create(datastorePackage^.getPath,ruleId);
  end;

DESTRUCTOR T_ruleWithSubrules.destroy;
  VAR i:longint;
      recycler:P_recycler;
  begin
    recycler:=newRecycler;
    for i:=0 to length(subrules)-1 do recycler^.disposeLiteral(subrules[i]);
    freeRecycler(recycler);
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
  VAR recycler:P_recycler;
  begin
    recycler:=newRecycler;
    namedValue.cleanup(recycler);
    namedValue.destroy;
    freeRecycler(recycler);
  end;

DESTRUCTOR T_datastore.destroy;
  begin
    dataStoreMeta.destroy;
    inherited destroy;
  end;

FUNCTION T_typeCastRule .getRootId:T_idString; begin result:=typedef^.getName; end;
FUNCTION T_typeCheckRule.getRootId:T_idString; begin result:=typedef^.getName; end;

PROCEDURE T_ruleWithSubrules.addOrReplaceSubRule(CONST rule: P_subruleExpression; CONST context:P_context; CONST recycler:P_recycler);
  VAR i,j:longint;
  begin
    rule^.parent:=@self;
    if (getId=MAIN_RULE_ID) and not(rule^.hasValidMainPattern) then context^.messages^.raiseSimpleError('Invalid pattern/signature for main rule! Must accept strings.',rule^.getLocation);
    if (getRuleType=rt_customOperator) then begin
      if isUnaryOperatorId(getId) then begin
        if not(rule^.canApplyToNumberOfParameters(1)) then context^.messages^.raiseSimpleError('Overloaded operator must accept one parameter',rule^.getLocation);
      end else begin
        if not(rule^.canApplyToNumberOfParameters(2)) then context^.messages^.raiseSimpleError('Overloaded operator must accept two parameter',rule^.getLocation);
      end;
      if not(rule^.getPattern.usesStrictCustomTyping) then context^.messages^.postTextMessage(mt_el2_warning,rule^.getLocation,'Overloading operators based on ducktype is discouraged! Use explicit types instead.');
    end;
    i:=0;
    while (i<length(subrules)) and not(rule^.hasEquivalentPattern(subrules[i])) do inc(i);
    if i>=length(subrules) then begin
      setLength(subrules,i+1);
      for j:=0 to i-1 do if subrules[j]^.hidesSubrule(rule) then context^.messages^.postTextMessage(mt_el2_warning,rule^.getLocation,'Rule '+rule^.getId+' seems to be hidden by '+subrules[j]^.getId+' @'+ansistring(subrules[j]^.getLocation));
    end else begin
      recycler^.disposeLiteral(subrules[i]);
      if not(rule^.metaData.hasAttribute(OVERRIDE_ATTRIBUTE))
      then context^.messages^.postTextMessage(mt_el2_warning,rule^.getLocation,'Overriding rule '+rule^.getId+'; you can suppress this warning with '+ATTRIBUTE_PREFIX+OVERRIDE_ATTRIBUTE);
    end;
    subrules[i]:=rule;
    if (length(subrules)>1) and (getRuleType in C_ruleTypesWithOnlyOneSubrule) then context^.messages^.raiseSimpleError('Cannot add a subrule to a '+C_ruleTypeText[getRuleType]+'rule!',rule^.getLocation);
    clearCache;
  end;

PROCEDURE T_typeCastRule.addOrReplaceSubRule(CONST rule:P_subruleExpression; CONST context:P_context; CONST recycler:P_recycler);
  begin
    inherited addOrReplaceSubRule(rule,context,recycler);
    if not(rule^.metaData.hasAttribute(OVERRIDE_ATTRIBUTE)) and
       not(rule^.metaData.hasAttribute(OVERLOAD_ATTRIBUTE)) then context^.messages^.postTextMessage(mt_el2_warning,rule^.getLocation,'Overloading implicit typecast rule');
  end;

PROCEDURE T_typeCheckRule.addOrReplaceSubRule(CONST rule:P_subruleExpression; CONST context:P_context; CONST recycler:P_recycler);
  VAR rulePattern:T_patternElement;
      inlineValue:P_literal;
      alwaysTrue:boolean=false;
  begin
    if length(subrules)>0 then begin
      context^.messages^.raiseSimpleError('Type definitions must have only one subrule and may not be overridden',rule^.getLocation);
      exit;
    end;
    setLength(subrules,1);
    subrules[0]:=rule;
    rule^.parent:=@self;
    if typedef=nil then begin
      rulePattern:=rule^.getPattern.getFirst;
      if rulePattern.isTypeCheckOnly then begin
        inlineValue:=rule^.getInlineValue;
        if inlineValue<>nil then begin
          alwaysTrue:=boolLit[true].equals(inlineValue);
          recycler^.disposeLiteral(inlineValue);
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

PROCEDURE T_rule.resolveIds(CONST adapters:P_messages; CONST resolveIdContext:T_resolveIdContext);
  begin
    raise Exception.create('This should not be called!');
  end;

PROCEDURE T_delegatorRule.resolveIds(CONST adapters:P_messages; CONST resolveIdContext:T_resolveIdContext);
  begin
    if localRule<>nil then localRule^.resolveIds(adapters,resolveIdContext);
  end;

PROCEDURE T_ruleWithSubrules.resolveIds(CONST adapters: P_messages; CONST resolveIdContext:T_resolveIdContext);
  VAR s:P_subruleExpression;
  begin
    for s in subrules do s^.resolveIds(adapters,resolveIdContext);
  end;

PROCEDURE T_protectedRuleWithSubrules.resolveIds(CONST adapters:P_messages; CONST resolveIdContext:T_resolveIdContext);
  VAR s:P_subruleExpression;
      p:pointer;
  begin
    for s in subrules do begin
      s^.resolveIds(adapters,resolveIdContext);
      for p in s^.usedGlobalVariables do appendIfNew(usedGlobalVariables,p);
    end;
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

FUNCTION T_ruleWithSubrules.getCmdLineHelpText: T_arrayOfString;
  VAR sub:P_subruleExpression;
      txt:string='';
  begin
    for sub in subrules do txt+=sub^.getCmdLineHelpText()+C_lineBreakChar;
    result:=formatTabs(split(txt));
    prepend(result,inherited getCmdLineHelpText[0]);
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

FUNCTION T_delegatorRule.canBeApplied(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT output:T_tokenRange; CONST context:P_abstractContext; CONST recycler:P_recycler):boolean;
  VAR r:P_ruleWithSubrules;
      tempLiteral:P_literal;
  begin
    if (localRule<>nil) and localRule^.canBeApplied(callLocation,param,output,context,recycler) then exit(true);
    for r in imported do if         r^.canBeApplied(callLocation,param,output,context,recycler) then exit(true);
    if hiddenRule<>nil then begin
      inc(P_context(context)^.callDepth);
      tempLiteral:=hiddenRule(param,callLocation,P_context(context),recycler);
      dec(P_context(context)^.callDepth);
      if tempLiteral<>nil then begin
        output.first:=recycler^.newToken(callLocation,'',tt_literal,tempLiteral);
        output.last :=output.first;
        result:=true;
      end else result:=false;
    end else result:=false;
  end;

FUNCTION T_ruleWithSubrules.canBeApplied(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT output:T_tokenRange; CONST context:P_abstractContext; CONST recycler:P_recycler):boolean;
  VAR sub:P_subruleExpression;
  begin
    result:=false;
    for sub in subrules do if ((sub^.isPublic) or (sub^.getLocation.package=callLocation.package)) and sub^.matchesPatternAndReplaces(param,callLocation,output,P_context(context),recycler) then exit(true);
    if (getRuleType in [rt_normal_curry,rt_memoized_curry,rt_synchronized_curry]) and canCurry(callLocation,param,output,P_context(context),recycler) then exit(true);
  end;

FUNCTION T_protectedRuleWithSubrules.canBeApplied(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT output:T_tokenRange; CONST context:P_abstractContext; CONST recycler:P_recycler):boolean;
  VAR p:pointer;
  begin
    result:=false;
    if P_context(context)^.callDepth>=STACK_DEPTH_LIMIT then P_context(context)^.raiseError('Stack depth limit exceeded calling '+getId+'.',getLocation,mt_el4_systemError)
    else if inherited canBeApplied(callLocation,param,output,context,recycler) then begin
      system.enterCriticalSection(rule_cs);
      for p in usedGlobalVariables do enterCriticalSection(P_variable(p)^.namedValue.varCs);
      try
        result:=true;
        P_context(context)^.reduceExpression(output.first,recycler);
        if output.first<>nil then output.last:=output.first^.last else begin
          output.last:=nil;
          result:=false;
        end;
      finally
        for p in usedGlobalVariables do leaveCriticalSection(P_variable(p)^.namedValue.varCs);
        system.leaveCriticalSection(rule_cs);
      end;
    end;
  end;

FUNCTION T_memoizedRule.canBeApplied(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT output:T_tokenRange; CONST context:P_abstractContext; CONST recycler:P_recycler):boolean;
{$MACRO ON}
{$define CLEAN_EXIT:=
if param=nil then recycler^.disposeLiteral(useParam);
exit}
  VAR lit:P_literal;
      useParam:P_listLiteral;
      p:pointer;

  FUNCTION canBeAppliedNaively:boolean;
    VAR sub:P_subruleExpression;
    begin
      result:=false;
      for sub in subrules do if ((sub^.isPublic) or (sub^.getLocation.package=callLocation.package)) and sub^.matchesPatternAndReplaces(param,callLocation,output,P_context(context),recycler) then exit(true);
      if (getRuleType in [rt_normal_curry,rt_memoized_curry,rt_synchronized_curry]) and canCurry(callLocation,param,output,P_context(context),recycler) then exit(true);
    end;

  PROCEDURE wrapResultInPutCacheRule;
    VAR newFirst,t:P_token;
    begin
      newFirst      :=recycler^.newToken(output.first^.location, getId+'.put.cache',tt_rulePutCacheValue,@self);
      newFirst^.next:=recycler^.newToken(output.first^.location, '', tt_parList_constructor,recycler^.newListLiteral(1)^.append(recycler,useParam,true)); t:=newFirst^.next;
      t       ^.next:=output.first;
      output.first:=newFirst;
      output.last^.next:=recycler^.newToken(output.first^.location, '', tt_braceClose);
      output.last:=output.last^.next;
    end;

  begin
    initialize(output);
    result:=false;
    if param=nil then useParam:=recycler^.newListLiteral
                 else useParam:=param;
    lit:=cache.get(useParam);
    if lit<>nil then begin
      lit^.rereference;
      output.first:=recycler^.newToken(getLocation,'',tt_literal,lit);
      output.last:=output.first;
      CLEAN_EXIT(true);
    end else if canBeAppliedNaively then begin
      if (P_context(context)^.callDepth>=STACK_DEPTH_LIMIT) then begin wrapResultInPutCacheRule; CLEAN_EXIT(true); end;
      if (P_context(context)^.messages^.continueEvaluation) then begin
        enterCriticalSection(rule_cs);
        for p in usedGlobalVariables do enterCriticalSection(P_variable(p)^.namedValue.varCs);
        P_context(context)^.reduceExpression(output.first,recycler);
        for p in usedGlobalVariables do leaveCriticalSection(P_variable(p)^.namedValue.varCs);
        leaveCriticalSection(rule_cs);
      end;
      if (P_context(context)^.messages^.continueEvaluation) and (output.first^.next=nil) and (output.first^.tokType=tt_literal) then begin
        lit:=output.first^.data;
        cache.put(useParam,lit);
        output.last:=output.first;
      end else begin
        recycler^.cascadeDisposeToken(output.first);
        output.first:=recycler^.newToken(getLocation,'',tt_literal,newVoidLiteral);
        output.last:=output.first;
      end;
      CLEAN_EXIT(true);
    end;
    CLEAN_EXIT(false);
  end;

FUNCTION T_typeCastRule.canBeApplied(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT output:T_tokenRange; CONST context:P_abstractContext; CONST recycler:P_recycler):boolean;
  VAR cast:P_typableLiteral;
      raw :P_literal;
  begin
    if inherited canBeApplied(callLocation,param,output,context,recycler) then begin
      if P_context(context)^.callDepth>STACK_DEPTH_LIMIT then begin
        P_context(context)^.raiseError('Stack overflow in typecast rule',callLocation,mt_el4_systemError);
        exit(false);
      end;
      inc(P_context(context)^.callDepth);
      raw:=P_context(context)^.reduceToLiteral(output.first,recycler).literal;
      dec(P_context(context)^.callDepth);
      if not(P_context(context)^.messages^.continueEvaluation) then begin
        if raw<>nil then recycler^.disposeLiteral(raw);
        exit(false);
      end;
    end else if (param<>nil) and (param^.size=1)
    then raw:=param^.value[0]^.rereferenced
    else exit(false);
    cast:=typedef^.cast(recycler,raw,callLocation,context,recycler);
    recycler^.disposeLiteral(raw);
    if cast=nil then exit(false)
    else begin
      result:=true;
      output.first:=recycler^.newToken(callLocation,'',tt_literal,cast);
      output.last:=output.first;
    end;
  end;

FUNCTION T_typeCheckRule.canBeApplied(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT output:T_tokenRange; CONST context:P_abstractContext; CONST recycler:P_recycler):boolean;
  VAR boolResult:boolean=false;
  begin
    boolResult:=(param<>nil)
            and (param^.size=1)
            and (typedef<>nil)
            and (typedef^.matchesLiteral(param^.value[0],callLocation,context,recycler));
    output.first:=recycler^.newToken(callLocation,'',tt_literal,newBoolLiteral(boolResult));
    output.last:=output.first;
    result:=true;
  end;

FUNCTION T_typeCheckRule.getFirstParameterTypeWhitelist: T_literalTypeSet;
  begin
    result:=subrules[0]^.getPattern.getFirstParameterTypeWhitelist;
  end;

DESTRUCTOR T_typeCheckRule.destroy;
  VAR recycler:P_recycler;
  begin
    if typedef<>nil then begin
      recycler:=newRecycler;
      typedef^.cleanup(recycler);
      dispose(typedef,destroy);
      freeRecycler(recycler);
    end;
    inherited destroy;
  end;

FUNCTION T_variable.getValueOrElseVoid(CONST context:P_context; CONST recycler:P_recycler): P_literal;
  begin
    result:=getValue(context,recycler);
    if result=nil then result:=newVoidLiteral;
  end;

FUNCTION T_variable.getValue(CONST context:P_context; CONST recycler:P_recycler): P_literal;
  begin
    result:=namedValue.getValue;
  end;

FUNCTION T_variable.evaluateToLiteral(CONST callLocation: T_tokenLocation; CONST p1, p2: P_literal; CONST recycler:P_recycler; CONST context: P_abstractContext): P_literal;
  begin
    raise Exception.create('T_variable.evaluateToLiteral must not be called');
    result:=nil;
  end;

FUNCTION T_variable.evaluateToLiteral(CONST callLocation: T_tokenLocation; CONST parList: P_listLiteral; CONST recycler:P_recycler; CONST context: P_abstractContext): P_literal;
  begin
    raise Exception.create('T_variable.evaluateToLiteral must not be called');
    result:=nil;
  end;

FUNCTION T_variable.canBeApplied(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT output:T_tokenRange; CONST context:P_abstractContext; CONST recycler:P_recycler):boolean;
  begin
    raise Exception.create('T_variable.replaces must not be called');
    result:=false;
  end;

FUNCTION T_datastore.getValue(CONST context:P_context; CONST recycler:P_recycler):P_literal;
  begin
    readDataStore(context,recycler);
    result:=namedValue.getValue;
  end;

FUNCTION T_rule.inspect(CONST includeFunctionPointer:boolean; CONST context:P_context; CONST recycler:P_recycler):P_mapLiteral;
  begin
    result:=newMapLiteral(3)^
      .put(recycler,'type'    ,recycler^.newStringLiteral(C_ruleTypeText[getRuleType]),false)^
      .put(recycler,'location',recycler^.newStringLiteral(getLocation                ),false);
  end;

FUNCTION T_delegatorRule.inspect(CONST includeFunctionPointer:boolean; CONST context:P_context; CONST recycler:P_recycler):P_mapLiteral;
  begin
    if localRule=nil
    then result:=nil
    else result:=localRule^.inspect(includeFunctionPointer,context,recycler);
  end;

FUNCTION T_ruleWithSubrules.inspect(CONST includeFunctionPointer:boolean; CONST context:P_context; CONST recycler:P_recycler): P_mapLiteral;
  FUNCTION subrulesList:P_listLiteral;
    VAR sub:P_subruleExpression;
    begin
      result:=recycler^.newListLiteral(length(subrules));
      for sub in subrules do result^.append(recycler,sub^.inspect(recycler),false);
    end;

  begin
    result:=inherited inspect(includeFunctionPointer,context,recycler)^
            .put(recycler,'subrules',subrulesList,false);
    if includeFunctionPointer then
    result^.put(recycler,'function',getFunctionPointer(context,recycler,getLocation),false);
  end;

FUNCTION T_typeCastRule.inspect(CONST includeFunctionPointer:boolean; CONST context:P_context; CONST recycler:P_recycler):P_mapLiteral;
  FUNCTION privateOrPublic:string;
    begin
      if related^.hasPublicSubrule
      then result:=PUBLIC_TEXT
      else result:=PRIVATE_TEXT;
    end;

  FUNCTION subrulesList:P_listLiteral;
    VAR sub:P_subruleExpression;
    begin
      result:=recycler^.newListLiteral(length(subrules)+1);
      result^.append(recycler,newMapLiteral(5)^
        .put(recycler,'pattern'   ,'()'           )^
        .put(recycler,'location'  ,getLocation    )^
        .put(recycler,'type'      ,privateOrPublic)^
        .put(recycler,'comment'   ,related^.subrules[0]^.metaData.comment)^
        .put(recycler,'attributes',related^.subrules[0]^.metaData.getAttributesLiteral(recycler),false),false);
      for sub in subrules do result^.append(recycler,sub^.inspect(recycler),false);
    end;

  begin
    result:=inherited inspect(includeFunctionPointer,context,recycler)^
            .put(recycler,'subrules',subrulesList,false);
    if includeFunctionPointer then
    result^.put(recycler,'function',getFunctionPointer(context,recycler,getLocation),false);
  end;

FUNCTION T_variable.inspect(CONST includeFunctionPointer: boolean; CONST context:P_context; CONST recycler:P_recycler): P_mapLiteral;
  FUNCTION privateOrPublic:string;
    begin
      if privateRule then result:=PRIVATE_TEXT
                     else result:=PUBLIC_TEXT;
    end;

  begin
    result:=newMapLiteral(4)^
      .put(recycler,'type'      ,recycler^.newStringLiteral(privateOrPublic+' '+C_varTypeText[varType]),false)^
      .put(recycler,'location'  ,recycler^.newStringLiteral(getLocation ),false)^
      .put(recycler,'comment'   ,recycler^.newStringLiteral(meta.comment),false)^
      .put(recycler,'attributes',meta.getAttributesLiteral(recycler),false);
  end;

FUNCTION T_delegatorRule.innerRuleType:T_ruleType;
  begin
    if localRule<>nil then result:=localRule^.getRuleType else result:=getRuleType;
  end;

FUNCTION T_ruleWithSubrules.getFunctionPointer(CONST context:P_context; CONST recycler:P_recycler; CONST location:T_tokenLocation):P_expressionLiteral;
  VAR tempToken:P_token=nil;
      arityInfo:T_arityInfo;
  begin
    if (getRuleType=rt_normal) and (length(subrules)=1) then exit(P_expressionLiteral(subrules[0]^.rereferenced));
    arityInfo:=arity;
    tempToken      :=recycler^.newToken(location,getId,tt_userRule,@self);
    tempToken^.next:=getParametersForPseudoFuncPtr(arityInfo.minPatternLength,arityInfo.maxPatternLength<>arityInfo.minPatternLength,location,context,recycler);
    new(P_inlineExpression(result),createFromInline(tempToken,context,recycler,C_tokenDefaultId[tt_pseudoFuncPointer]+getId));
  end;

FUNCTION T_delegatorRule.getFunctionPointer(CONST context:P_context; CONST recycler:P_recycler; CONST location: T_tokenLocation): P_expressionLiteral;
  VAR tempToken:P_token=nil;
      arityInfo:T_arityInfo;
  begin
    if intOperator=tt_userRule then begin
      arityInfo:=arity;
      tempToken            :=recycler^.newToken(location,getId,tt_userRule,@self);
      tempToken^.next      :=getParametersForPseudoFuncPtr(arityInfo.minPatternLength,arityInfo.maxPatternLength<>arityInfo.minPatternLength,location,context,recycler);
      new(P_inlineExpression(result),createFromInline(tempToken,context,recycler,C_tokenDefaultId[tt_pseudoFuncPointer]+getId));
    end else result:=builtinFunctionMap.getIntrinsicRuleAsExpression(intFuncForOperator[intOperator],true);
  end;

FUNCTION T_protectedRuleWithSubrules.getFunctionPointer(CONST context:P_context; CONST recycler:P_recycler; CONST location:T_tokenLocation):P_expressionLiteral;
  VAR tempToken:P_token=nil;
      arityInfo:T_arityInfo;
  begin
    arityInfo:=arity;
    tempToken            :=recycler^.newToken(location,getId,tt_userRule,@self);
    tempToken^.next      :=getParametersForPseudoFuncPtr(arityInfo.minPatternLength,arityInfo.maxPatternLength<>arityInfo.minPatternLength,location,context,recycler);
    new(P_inlineExpression(result),createFromInline(tempToken,context,recycler,C_tokenDefaultId[tt_pseudoFuncPointer]+getId));
  end;

FUNCTION T_typeCastRule.getFunctionPointer(CONST context:P_context; CONST recycler:P_recycler; CONST location:T_tokenLocation):P_expressionLiteral;
  VAR tempToken:P_token=nil;
  begin
    tempToken            :=recycler^.newToken(location,getId,tt_userRule,@self);
    tempToken^.next      :=getParametersForPseudoFuncPtr(1,false,location,context,recycler);
    new(P_inlineExpression(result),createFromInline(tempToken,context,recycler,C_tokenDefaultId[tt_pseudoFuncPointer]+getId));
  end;

{$ifdef fullVersion}
PROCEDURE T_ruleWithSubrules.fillCallInfos(CONST callInfos:P_callAndIdInfos);
  VAR s:P_subruleExpression;
  begin
    for s in subrules do s^.fillCallInfos(callInfos);
  end;

PROCEDURE T_delegatorRule.fillCallInfos(CONST callInfos:P_callAndIdInfos);
  VAR r:P_rule;
  begin
    for r in imported do           r^.fillCallInfos(callInfos);
    if localRule<>nil then localRule^.fillCallInfos(callInfos);
  end;

FUNCTION T_delegatorRule.hasAnnotationMarkingAsUsed:boolean;
  begin
    result:=(localRule<>nil) and (localRule^.hasAnnotationMarkingAsUsed);
  end;

FUNCTION T_ruleWithSubrules.hasAnnotationMarkingAsUsed:boolean;
  VAR s:P_subruleExpression;
      x:string;
  begin
    if getId=MAIN_RULE_ID then exit(true);
    if getId=TO_STRING_RULE_ID then exit(true);
    for x in operatorName do if getId=x then exit(true);
    for s in subrules do
      if s^.metaData.hasAttribute(EXECUTE_AFTER_ATTRIBUTE) or
         s^.metaData.hasAttribute(SUPPRESS_UNUSED_WARNING_ATTRIBUTE)
      then exit(true);
    result:=false;
  end;

FUNCTION T_variable.hasAnnotationMarkingAsUsed:boolean;
  begin
    result:=metaData.hasAttribute(EXECUTE_AFTER_ATTRIBUTE) or
            metaData.hasAttribute(SUPPRESS_UNUSED_WARNING_ATTRIBUTE);
  end;

FUNCTION T_delegatorRule.getStructuredInfo:T_structuredRuleInfoList;
  VAR i,i0:longint;
      fromDelegate:T_structuredRuleInfoList;
      r:P_ruleWithSubrules;
  begin
    if localRule<>nil
    then result:=localRule^.getStructuredInfo
    else setLength(result,0);

    for r in imported do begin
      fromDelegate:=r^.getStructuredInfo;
      i0:=length(result);
      setLength(result,i0+length(fromDelegate));
      for i:=0 to length(fromDelegate)-1 do result[i+i0]:=fromDelegate[i];
      setLength(fromDelegate,0);
    end;
  end;

FUNCTION T_ruleWithSubrules.getStructuredInfo:T_structuredRuleInfoList;
  VAR i:longint=0;
  begin
    setLength(result,length(subrules));
    for i:=0 to length(subrules)-1 do result[i]:=subrules[i]^.getStructuredInfo;
  end;

FUNCTION T_variable.getStructuredInfo:T_structuredRuleInfoList;
  begin
    setLength(result,1);
    result[0].location:=getLocation;
    result[0].idAndSignature:=C_varTypeText[varType]+' '+getId;
    result[0].comment:=meta.getDocTxt;
  end;
{$endif}

PROCEDURE T_variable.setMutableValue(CONST value: P_literal; CONST onDeclaration: boolean; CONST literalRecycler:P_literalRecycler);
  begin
    namedValue.setValue(literalRecycler,value);
    system.enterCriticalSection(namedValue.varCs);
    if onDeclaration then state:=vs_initialized
                     else state:=vs_modified;
    system.leaveCriticalSection(namedValue.varCs);
  end;

FUNCTION T_variable.isReportable(OUT value: P_literal): boolean;
  begin
    value:=namedValue.getValue;
    if value=nil then exit(false);
    value^.unreference;
    result:=true;
  end;

PROCEDURE T_datastore.readDataStore(CONST context:P_context; CONST recycler:P_recycler);
  VAR lit:P_literal;
  begin
    if (state in [vs_uninitialized,vs_initialized]) or (state=vs_readFromFile) and dataStoreMeta.fileChangedSinceRead then begin
      system.enterCriticalSection(namedValue.varCs);
      lit:=dataStoreMeta.readValue(getLocation,context,recycler);
      if lit<>nil then begin
        namedValue.setValue(recycler,lit);
        lit^.unreference;
      end;
      state:=vs_readFromFile;
      system.leaveCriticalSection(namedValue.varCs);
    end;
  end;

FUNCTION T_variable.mutateInline(CONST mutation: T_tokenType; CONST RHS: P_literal; CONST location: T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler): P_literal;
  begin
    result:=namedValue.mutate(recycler,mutation,RHS,location,context,recycler);
    state:=vs_modified;
  end;

FUNCTION T_datastore.mutateInline(CONST mutation: T_tokenType; CONST RHS: P_literal; CONST location: T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler): P_literal;
  begin
    readDataStore(context,recycler);
    result:=inherited mutateInline(mutation,RHS,location,context,recycler);
  end;

FUNCTION T_datastore.writeBackToTemp(CONST adapters:P_messages; VAR flush:T_datastoreFlush):boolean;
  VAR L:P_literal;
  begin
    if adapters^.continueEvaluation and (state=vs_modified) then begin
      L:=namedValue.getValue;
      flush.addStoreToFlush(@dataStoreMeta,L,getLocation,varType=vt_plainDatastore);
      state:=vs_inSyncWithFile;
      result:=true;
    end else result:=false;
  end;

PROCEDURE T_memoizedRule.clearCache;
  begin
    enterCriticalSection(rule_cs);
    cache.clear;
    leaveCriticalSection(rule_cs);
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
