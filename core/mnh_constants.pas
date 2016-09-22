UNIT mnh_constants;
INTERFACE
USES sysutils;
{$ifdef CPU32}
//This directive seems to be beneficial only for 32bit targets
{$PACKENUM 1}
{$endif}
TYPE
  T_hashInt=dword;
  T_idString =shortString;
  P_idString =^T_idString;
CONST maxIdLength=255;
TYPE
  T_commentString=ansistring;
  P_commentString=^T_commentString;
  T_namespace=(DEFAULT_BUILTIN_NAMESPACE,
               MATH_NAMESPACE           ,
               STRINGS_NAMESPACE        ,
               LIST_NAMESPACE           ,
               REGEX_NAMESPACE          ,
               SYSTEM_BUILTIN_NAMESPACE ,
               FILES_BUILTIN_NAMESPACE  ,
               TYPECAST_NAMESPACE
               {$ifdef fullVersion},
               PLOT_NAMESPACE
               {$endif});
  T_reservedWordClass=(rwc_not_reserved,
                       rwc_specialLiteral,
                       rwc_specialConstruct,
                       rwc_operator,
                       rwc_typeCheck,
                       rwc_modifier);
  P_abstractPackage=^T_abstractPackage;
  T_abstractPackage=object
    FUNCTION getPath:ansistring; virtual; abstract;
  end;

CONST
  UTF8_ZERO_WIDTH_SPACE=#226#128#139;
  C_appName='MNH';
  C_appTitle='MNH5';
  C_voidText= 'void';
  C_nanText = 'Nan';
  C_infText = 'Inf';
  C_boolText: array[false..true] of string = ('false', 'true');
  {$ifdef fullVersion}
  C_forceGuiPseudoPackage='GUI';
  {$endif}
  ONE_SECOND=1/(24*60*60);
  ONE_MINUTE=1/(24*60);
  SCRIPT_EXTENSION='.mnh';
  C_namespaceString:array[T_namespace] of string=('mnh','math','strings','lists','regex','system','files','typecast'{$ifdef fullVersion},'plot'{$endif});
  C_ID_QUALIFY_CHARACTER='.';

  C_eachIndexIdentifier='index';
  C_tryMessageIdentifier='messages';
  C_mainRuleId='main';
  C_builtinPseudolocationPrefix='builtin';

FUNCTION isQualified(CONST s:string):boolean;
FUNCTION configDir:string;
CONST
  LOGO:array[0..6] of string=(' ___      ___ ___   ___ ___   ___ ',
                              '|   \    /   |   \ |   |   | |   |  ______',
                              '|    \  /    |    \|   |   |_|   | |   ___|',
                              '|     \/     |     \   |         | |  |__',
                              '|   \    /   |   \     |    _    | |___  \',
                              '|   |\  /|   |   |\    |   | |   |  ___)  |',
                              '|___| \/ |___|___| \___|___| |___| |_____/');

TYPE
  T_myFloat = extended;

  T_tokenType = (tt_literal, tt_aggregatorExpressionLiteral,
    //identifier and resolved identifiers
    tt_identifier, tt_parameterIdentifier, tt_localUserRule,
    tt_importedUserRule, tt_intrinsicRule, tt_rulePutCacheValue,
    tt_customTypeRule,
    tt_blockLocalVariable,
    tt_ponFlipper,
    tt_aggregatorConstructor,
    //special operators
    tt_each, tt_parallelEach, tt_forcedParallelEach, tt_agg, tt_while, tt_beginBlock, tt_beginFunc, tt_endBlock, tt_endFunc, tt_try, tt_toId, tt_pseudoFuncPointer,
    //lists and list constructors
    tt_braceOpen, tt_braceClose, tt_parList_constructor, tt_parList,
    tt_listBraceOpen, tt_listBraceClose, tt_list_constructor,
    tt_expBraceOpen, tt_expBraceClose,
    //separators
    tt_separatorComma, tt_separatorCnt,
    //comparators
    tt_comparatorEq, tt_comparatorNeq, tt_comparatorLeq,
    tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq,
    //logical operators
    tt_operatorAnd, tt_operatorOr, tt_operatorXor,
    tt_operatorLazyAnd, tt_operatorLazyOr,
    //arthmetical operators
    tt_operatorPlus, tt_operatorMinus, tt_operatorMult,
    tt_operatorDivReal, tt_operatorDivInt, tt_operatorMod, tt_operatorPot,
    //partially evaluated operators
    tt_unaryOpPlus, tt_unaryOpMinus,
    //special: string concatenation
    tt_operatorStrConcat, tt_operatorOrElse,
    //list operators:
    tt_operatorConcat, tt_operatorIn,
    //inline if: (<condition>?<then>:<else>)
    tt_iifCheck, tt_iifElse,
    tt_listToParameterList,
    //assignment operators:
    tt_declare, tt_assign, tt_mutate, tt_assignNewBlockLocal, tt_assignExistingBlockLocal,
    tt_cso_assignPlus,tt_cso_assignMinus,tt_cso_assignMult,tt_cso_assignDiv,tt_cso_assignStrConcat,tt_cso_assignAppend, //+= -= *= /= &= |=
    //type checks:
    tt_typeCheckScalar, tt_typeCheckList,
    tt_typeCheckBoolean, tt_typeCheckBoolList,
    tt_typeCheckInt, tt_typeCheckIntList,
    tt_typeCheckReal, tt_typeCheckRealList,
    tt_typeCheckString, tt_typeCheckStringList,
    tt_typeCheckNumeric, tt_typeCheckNumList,
    tt_typeCheckExpression,
    tt_typeCheckKeyValueList,
    tt_customTypeCheck,
    tt_semicolon,
    tt_optionalParameters,
    //modifiers:
    tt_modifier_private,
    tt_modifier_memoized,
    tt_modifier_mutable,
    tt_modifier_persistent,
    tt_modifier_datastore,
    tt_modifier_synchronized,
    tt_modifier_local,
    tt_modifier_customType,
    //special: [E]nd [O]f [L]ine
    tt_EOL,
    tt_blank);

  T_tokenPayloadType=(tpt_none,tpt_identifier,tpt_comment,tpt_idWithPointerPayload,tpt_literal,tpt_userRule,tpt_builtinRule,tpt_eachPayload);

  P_tokenPayload=^T_tokenPayload;
  T_tokenPayload=object
    payloadType:T_tokenPayloadType;
    CONSTRUCTOR create(CONST typ:T_tokenPayloadType); //pro forma
    DESTRUCTOR destroy; virtual;
    FUNCTION hash:T_hashInt; virtual; abstract;
    FUNCTION toString(CONST tokType:T_tokenType=tt_EOL; CONST lengthLimit:longint=maxLongint):ansistring; virtual; abstract;
    FUNCTION cloneOrCopy(CONST tokType:T_tokenType=tt_EOL):P_tokenPayload; virtual; abstract;
  end;

  P_idPayload=^T_idPayload;
  T_idPayload=object(T_tokenPayload)
    id:T_idString;
    CONSTRUCTOR create(CONST id_:T_idString; CONST typ:T_tokenPayloadType);
    DESTRUCTOR destroy; virtual;
    FUNCTION hash:T_hashInt; virtual;
    FUNCTION toString(CONST tokType:T_tokenType=tt_EOL; CONST lengthLimit:longint=maxLongint):ansistring; virtual;
    FUNCTION cloneOrCopy(CONST tokType:T_tokenType=tt_EOL):P_tokenPayload; virtual;
  end;

  P_commentPayload=^T_commentPayload;

  { T_commentPayload }

  T_commentPayload=object(T_tokenPayload)
    txt:T_commentString;
    CONSTRUCTOR create(CONST txt_:T_commentString);
    DESTRUCTOR destroy; virtual;
    FUNCTION hash:T_hashInt; virtual;
    FUNCTION toString(CONST tokType:T_tokenType=tt_EOL; CONST lengthLimit:longint=maxLongint):ansistring; virtual;
    FUNCTION cloneOrCopy(CONST tokType:T_tokenType=tt_EOL):P_tokenPayload; virtual;
  end;

  P_idWithPointerPayload=^T_idWithPointerPayload;
  T_idWithPointerPayload=object(T_idPayload)
    ptr:pointer;
    CONSTRUCTOR create(CONST id_:T_idString; CONST ptr_:pointer);
    FUNCTION cloneOrCopy(CONST tokType:T_tokenType=tt_EOL):P_tokenPayload; virtual;
  end;

  T_tokenTypeSet=set of T_tokenType;
  T_modifier=                         tt_modifier_private..tt_modifier_customType;
  T_cStyleOperator=tt_cso_assignPlus..tt_cso_assignAppend;
CONST C_ruleModifiers:T_tokenTypeSet=[tt_modifier_private..tt_modifier_synchronized,tt_modifier_customType];
TYPE
  T_modifierSet=set of T_modifier;

  T_literalType = (
    lt_error,
    lt_boolean,
    lt_int,
    lt_real,
    lt_string,
    lt_expression,
    lt_list,
    lt_booleanList,
    lt_intList,
    lt_realList,
    lt_numList,
    lt_stringList,
    lt_emptyList,
    lt_keyValueList,
    lt_flatList,
    lt_listWithError,
    lt_void);

  T_literalTypeSet=set of T_literalType;

  T_tokenTypeInfo=record
    tokenType:T_tokenType;
    reservedWordClass:T_reservedWordClass;
    info:ansistring;
  end;

CONST
  C_forbiddenTokenTypes: T_tokenTypeSet=[tt_rulePutCacheValue, tt_agg, tt_parList_constructor, tt_parList,
    tt_declare,
    //type checks:
    tt_typeCheckScalar, tt_typeCheckList,
    tt_typeCheckBoolean, tt_typeCheckBoolList,
    tt_typeCheckInt, tt_typeCheckIntList,
    tt_typeCheckReal, tt_typeCheckRealList,
    tt_typeCheckString, tt_typeCheckStringList,
    tt_typeCheckNumeric, tt_typeCheckNumList,
    tt_typeCheckExpression,
    tt_typeCheckKeyValueList,
    tt_customTypeCheck,
    //modifiers:
    tt_modifier_private,
    tt_modifier_memoized,
    tt_modifier_mutable,
    tt_modifier_persistent,
    tt_modifier_datastore,
    tt_modifier_synchronized,
    tt_modifier_customType,
    //special: [E]nd [O]f [L]ine
    tt_EOL,
    tt_blank];
  C_validNonVoidTypes: T_literalTypeSet=[lt_boolean..lt_flatList];
  C_validListTypes: T_literalTypeSet=[lt_list..lt_flatList];
  C_validScalarTypes: T_literalTypeSet=[lt_boolean..lt_expression,lt_void];
  C_operatorsForAggregators: T_tokenTypeSet=[tt_operatorAnd..tt_operatorPot,tt_operatorStrConcat,tt_operatorOrElse,tt_operatorConcat];
  C_operatorsAndComparators: T_tokenTypeSet=[tt_comparatorEq..tt_operatorIn];
  C_typeChecks: T_tokenTypeSet=[tt_typeCheckScalar..tt_typeCheckKeyValueList];
  C_openingBrackets:T_tokenTypeSet=[tt_beginBlock,tt_beginFunc,tt_each,tt_parallelEach,tt_forcedParallelEach,tt_agg,tt_braceOpen,tt_parList_constructor,tt_listBraceOpen,tt_list_constructor,tt_expBraceOpen,tt_iifCheck];
  C_closingBrackets:T_tokenTypeSet=[tt_endBlock,tt_endFunc,tt_braceClose,tt_listBraceClose,tt_expBraceClose,tt_iifElse];
  C_matchingClosingBracket:array[tt_each..tt_iifCheck] of T_tokenType=
    {tt_each}              (tt_braceClose,
    {tt_parallelEach}       tt_braceClose,
    {tt_forcedParallelEach} tt_braceClose,
    {tt_agg}                tt_braceClose,
    {tt_while}              tt_EOL,
    {tt_beginBlock}         tt_endBlock,
    {tt_beginFunc}          tt_endFunc,
    {tt_endBlock}           tt_EOL,
    {tt_endFunc}            tt_EOL,
    {tt_try}                tt_EOL,
    {tt_toId}               tt_EOL,
                            tt_EOL,
    {tt_braceOpen}          tt_braceClose,
    {tt_braceClose}         tt_EOL,
    {tt_parList_constructor}tt_braceClose,
    {tt_parList}            tt_EOL,
    {tt_listBraceOpen}      tt_listBraceClose,
    {tt_listBraceClose}     tt_EOL,
    {tt_list_constructor}   tt_listBraceClose,
    {tt_expBraceOpen}       tt_expBraceClose,
    {tt_expBraceClose..tt_operatorIn} tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,
    {tt_iifCheck}           tt_iifElse);

  C_opPrecedence: array[tt_comparatorEq..tt_operatorIn] of byte =
   (6, 6, 6, 6, 6, 6, 6, //comparators
    8, 9, 9,             //logical operators
    8, 9,                //lazy logical operators
    4, 4, 3, 3, 3, 3, 2, //arthmetical operators
    8, 8,                //unaries
    5, 9,                //special: string concatenation
    1, 7);   //list operators

  C_matchingTypes: array[tt_typeCheckScalar..tt_typeCheckKeyValueList] of T_literalTypeSet =
    {tt_typeCheckScalar}      ([lt_boolean, lt_int, lt_real, lt_string],
    {tt_typeCheckList}         [lt_list..lt_flatList],
    {tt_typeCheckBoolean}      [lt_boolean],
    {tt_typeCheckBoolList}     [lt_booleanList, lt_emptyList],
    {tt_typeCheckInt}          [lt_int],
    {tt_typeCheckIntList}      [lt_intList, lt_emptyList],
    {tt_typeCheckReal}         [lt_real],
    {tt_typeCheckRealList}     [lt_realList, lt_emptyList],
    {tt_typeCheckString}       [lt_string],
    {tt_typeCheckStringList}   [lt_stringList, lt_emptyList],
    {tt_typeCheckNumeric}      [lt_int, lt_real],
    {tt_typeCheckNumList}      [lt_intList, lt_realList, lt_numList, lt_emptyList],
    {tt_typeCheckExpression}   [lt_expression],
    {tt_typeCheckKeyValueList} [lt_emptyList, lt_keyValueList]);
  C_modifieableTypeChecks: T_tokenTypeSet=[tt_typeCheckList,tt_typeCheckBoolList,tt_typeCheckIntList,tt_typeCheckRealList,tt_typeCheckStringList,tt_typeCheckNumList,tt_typeCheckExpression,tt_typeCheckKeyValueList];

  C_compatibleEnd:array[tt_beginBlock..tt_beginFunc] of T_tokenType=(tt_endBlock,tt_endFunc);
  C_tokenInfo:array[T_tokenType] of record
    defaultId:string;
    reservedWordClass:T_reservedWordClass;
    payloadType:T_tokenPayloadType;
    helpText:string;
  end=((defaultId:''; reservedWordClass:rwc_not_reserved; payloadType:tpt_literal; helpText:'A literal'),
       (defaultId:''; reservedWordClass:rwc_not_reserved; payloadType:tpt_literal; helpText:'An aggregator expression literal'),
    //identifier and resolved identifiers
      (defaultId:'';           reservedWordClass:rwc_not_reserved;     payloadType:tpt_identifier;  helpText:'An identifier (unresolved)'),
      (defaultId:'';           reservedWordClass:rwc_not_reserved;     payloadType:tpt_identifier;  helpText:'A parameter identifier'),
      (defaultId:'';           reservedWordClass:rwc_not_reserved;     payloadType:tpt_userRule;    helpText:'A local user rule'),
      (defaultId:'';           reservedWordClass:rwc_not_reserved;     payloadType:tpt_userRule;    helpText:'An imported user rule'),
      (defaultId:'';           reservedWordClass:rwc_not_reserved;     payloadType:tpt_builtinRule; helpText:'A built in rule'),
      (defaultId:'';           reservedWordClass:rwc_not_reserved;     payloadType:tpt_userRule;    helpText:'A put-cache-value call'),
      (defaultId:'';           reservedWordClass:rwc_not_reserved;     payloadType:tpt_userRule;    helpText:'A custom type check rule'),
      (defaultId:'';           reservedWordClass:rwc_not_reserved;     payloadType:tpt_identifier;  helpText:'A block-local variable'),
      (defaultId:'.';          reservedWordClass:rwc_not_reserved;     payloadType:tpt_none;        helpText:'A pseudo-object-notation flipper'),
      (defaultId:'aggregator'; reservedWordClass:rwc_specialConstruct; payloadType:tpt_none;        helpText:'Special construct: aggregator#The aggregator constructor'),
    //special operators
      (defaultId:'.each';  reservedWordClass:rwc_specialConstruct; payloadType:tpt_eachPayload; helpText:'Special construct: each#Used for (serial) list operations.#Syntax: <list>.each(<id>,<body>,<aggregator>)#<body> is an arbitrary expression which may use <id> to refer to the current list element or "index" for the current index#<aggregator> is optional and may be a simple operator'),
      (defaultId:'.pEach'; reservedWordClass:rwc_specialConstruct; payloadType:tpt_eachPayload; helpText:'Special construct: pEach (parallel each)#Used for parallel list operations.#Parallelized depending on the systen settings.#Syntax: <list>.pEach(<id>,<body>,<aggregator>)#<body> is an arbitrary expression which may use <id> to refer to the current list element or "index" for the current index#<aggregator> is optional and may be a simple operator'),
      (defaultId:'.PEach'; reservedWordClass:rwc_specialConstruct; payloadType:tpt_eachPayload; helpText:'Special construct: PEach (forced parallel each)#Used for parallel list operations.#Parallelized independent from systen settings.#Syntax: <list>.PEach(<id>,<body>,<aggregator>)#<body> is an arbitrary expression which may use <id> to refer to the current list element or "index" for the current index#<aggregator> is optional and may be a simple operator'),
      (defaultId:'.agg';   reservedWordClass:rwc_specialConstruct; payloadType:tpt_eachPayload; helpText:'Special construct: agg#Used for list aggregation#Syntax: <list>.agg(<aggregator>) - where <aggregator> may be an expression or a simple operator as +'),
      (defaultId:'while';  reservedWordClass:rwc_specialConstruct; payloadType:tpt_none;        helpText:'Special construct: while#Used for loops#Syntax: while(<entry condition>,<body>) - where <entry condition> must return a scalar boolean'),
      (defaultId:'begin';  reservedWordClass:rwc_specialConstruct; payloadType:tpt_none;        helpText:'Special construct: begin#Opening delimiter for procedural blocks'),
      (defaultId:'';       reservedWordClass:rwc_specialConstruct; payloadType:tpt_none;        helpText:''), {No default ID, because tokenizer shall not produce this token}
      (defaultId:'end';    reservedWordClass:rwc_specialConstruct; payloadType:tpt_none;        helpText:'Special construct: end#Closing delimiter for procedural blocks'),
      (defaultId:'';       reservedWordClass:rwc_specialConstruct; payloadType:tpt_none;        helpText:''), {No default ID, because tokenizer shall not produce this token}
      (defaultId:'try';    reservedWordClass:rwc_specialConstruct; payloadType:tpt_none;        helpText:'Special construct: try#Used for local exception handling#Syntax: try(<body>,<catch body>) - where <catch body> is executed only if evaluation of <body> fails'),
      (defaultId:'toId';   reservedWordClass:rwc_specialConstruct; payloadType:tpt_none;        helpText:'Special construct: toId#Returns the string argument as an identifier'),
      (defaultId:'::';     reservedWordClass:rwc_specialConstruct; payloadType:tpt_none;        helpText:'Special construct: ::# Returns reference to a function#::f -> {f@$params}'),
      //lists and list constructor
      (defaultId:'('; reservedWordClass:rwc_not_reserved; payloadType:tpt_none;    helpText:'Opening round bracket#Used as in default mathematical syntax.'),
      (defaultId:')'; reservedWordClass:rwc_not_reserved; payloadType:tpt_none;    helpText:'Closing round bracket#Used as in default mathematical syntax.'),
      (defaultId:'';  reservedWordClass:rwc_not_reserved; payloadType:tpt_literal; helpText:'A parameter list constructor'),
      (defaultId:'';  reservedWordClass:rwc_not_reserved; payloadType:tpt_literal; helpText:'A parameter list'),
      (defaultId:'['; reservedWordClass:rwc_not_reserved; payloadType:tpt_none;    helpText:'Square opening bracket#Used for list construction and list access'),
      (defaultId:']'; reservedWordClass:rwc_not_reserved; payloadType:tpt_none;    helpText:'Square closing bracket#Used for list construction and list access'),
      (defaultId:'';  reservedWordClass:rwc_not_reserved; payloadType:tpt_literal; helpText:'A list constructor'),
      (defaultId:'{'; reservedWordClass:rwc_not_reserved; payloadType:tpt_none;    helpText:'Curly opening bracket#Delimits an expression'),
      (defaultId:'}'; reservedWordClass:rwc_not_reserved; payloadType:tpt_none;    helpText:'Curly closing bracket#Delimits an expression'),
      //separators
      (defaultId:','; reservedWordClass:rwc_not_reserved; payloadType:tpt_none; helpText:'Separator comma'),
      (defaultId:'..';reservedWordClass:rwc_not_reserved; payloadType:tpt_none; helpText:'Separator ..#Used for constructing ranges and only allowed in that context'),
      //comparators
      (defaultId:'=';  reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Equals operator#Returns true if the scalar comparands are type-compatible#and equal#For list operands a list of booleans is returned'),
      (defaultId:'<>'; reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Not-equals operator#Returns true if the scalar comparands are type-compatible#and not equal#For list operands a list of booleans is returned'),
      (defaultId:'<='; reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Lesser-or-equals operator#Returns true if the scalar comparands are type-compatible#and the left hand side is lesser or equal to the right hand side#For list operands a list of booleans is returned'),
      (defaultId:'>='; reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Greater-or-equals operator#Returns true if the scalar comparands are type-compatible#and the left hand side is greater or equal to the right hand side#For list operands a list of booleans is returned'),
      (defaultId:'<';  reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Lesser operator#Returns true if the scalar comparands are type-compatible#and the left hand side is lesser than the right hand side#For list operands a list of booleans is returned'),
      (defaultId:'>';  reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Greater operator#Returns true if the scalar comparands are type-compatible#and the left hand side is greater than the right hand side#For list operands a list of booleans is returned'),
      (defaultId:'=='; reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Strict-equals operator#Returns true if the comparands are strictly equal.#Always returns a scalar boolean'),
      //logical operators
      (defaultId:'and'; reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Bit-wise and operator#Applies to (lists of) integers and (lists of) booleans'),
      (defaultId:'or';  reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Bit-wise or operator#Applies to (lists of) integers and (lists of) booleans'),
      (defaultId:'xor'; reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Bit-wise xor operator#Applies to (lists of) integers and (lists of) booleans'),
      (defaultId:'AND'; reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Lazy and operator#Applies to scalar booleans only'),
      (defaultId:'OR';  reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Lazy or operator#Applies to scalar booleans only'),
      //arthmetical operators
      (defaultId:'+';   reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Plus operator#Applies to (lists of) numbers and (lists of) strings'),
      (defaultId:'-';   reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Minus operator#Applies to (lists of) numbers'),
      (defaultId:'*';   reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Multiplication operator#Applies to (lists of) numbers'),
      (defaultId:'/';   reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Division operator#Applies to (lists of) numbers'),
      (defaultId:'div'; reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Integer division operator#Applies to (lists of) integers'),
      (defaultId:'mod'; reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Integer modulo operator#Applies to (lists of) integers'),
      (defaultId:'^';   reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Potentiation operator#Applies to (lists of) numbers'),
      //partially evaluated operators
      (defaultId:'+'; reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Unary plus operator#Neutral'),
      (defaultId:'-'; reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Unary minus operator#Negates the operand'),
      //special: string concatenation
      (defaultId:'&';      reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'String concatenation operator#Applies to all literals'),
      (defaultId:'orElse'; reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Or-Else operator#Employed to provide a fallback to void literals'),
      //list operators:
      (defaultId:'|';  reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'List concatenation operator#Applies to all literals'),
      (defaultId:'in'; reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'In operator#Applies to all literals on the left hand side and lists on the right hand side.#Returns true if the RHS contains the LHS'),
      //inline if: (<condition>?<then>:<else>)
      (defaultId:'?';reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Inline-if-operator'),
      (defaultId:':';reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'Inline-if-operator'),
      (defaultId:'@';reservedWordClass:rwc_operator; payloadType:tpt_none; helpText:'List-to-parameter-list operator'),
      //assignment operators:
      (defaultId:'->';reservedWordClass:rwc_operator; payloadType:tpt_none;                 helpText:'Declaration operator'),
      (defaultId:':=';reservedWordClass:rwc_operator; payloadType:tpt_none;                 helpText:'Assignment operator'),
      (defaultId:':=';reservedWordClass:rwc_operator; payloadType:tpt_idWithPointerPayload; helpText:'Mutate-assign operator'),
      (defaultId:':=';reservedWordClass:rwc_operator; payloadType:tpt_idWithPointerPayload; helpText:'Assign new block local operator'),
      (defaultId:':=';reservedWordClass:rwc_operator; payloadType:tpt_idWithPointerPayload; helpText:'Assign existing block local operator'),
      (defaultId:'+=';reservedWordClass:rwc_operator; payloadType:tpt_idWithPointerPayload; helpText:'C-Style assign-increment operator'),
      (defaultId:'-=';reservedWordClass:rwc_operator; payloadType:tpt_idWithPointerPayload; helpText:'C-Style assign-decrement operator'),
      (defaultId:'*=';reservedWordClass:rwc_operator; payloadType:tpt_idWithPointerPayload; helpText:'C-Style assign-multiply operator'),
      (defaultId:'/=';reservedWordClass:rwc_operator; payloadType:tpt_idWithPointerPayload; helpText:'C-Style assign-divide operator'),
      (defaultId:'&=';reservedWordClass:rwc_operator; payloadType:tpt_idWithPointerPayload; helpText:'C-Style assign-(string-)concatenate operator'),
      (defaultId:'|=';reservedWordClass:rwc_operator; payloadType:tpt_idWithPointerPayload; helpText:'C-Style assign-(list-)concatenate operator'),
      //type checks:
      (defaultId:':scalar';      reservedWordClass:rwc_typeCheck;    payloadType:tpt_none;     helpText:'Type check scalar;#Matches on all non-lists'),
      (defaultId:':list';        reservedWordClass:rwc_typeCheck;    payloadType:tpt_none;     helpText:'Type check list;#Matches on all lists#In patterns it can be modified to match only lists of a given size'),
      (defaultId:':boolean';     reservedWordClass:rwc_typeCheck;    payloadType:tpt_none;     helpText:'Type check boolean;#Matches on scalar booleans'),
      (defaultId:':booleanList'; reservedWordClass:rwc_typeCheck;    payloadType:tpt_none;     helpText:'Type check boolean list;#Matches on lists of booleans and empty lists'),
      (defaultId:':int';         reservedWordClass:rwc_typeCheck;    payloadType:tpt_none;     helpText:'Type check integer;#Matches on scalar integers'),
      (defaultId:':intList';     reservedWordClass:rwc_typeCheck;    payloadType:tpt_none;     helpText:'Type check integer list;#Matches on lists of integers and empty lists'),
      (defaultId:':real';        reservedWordClass:rwc_typeCheck;    payloadType:tpt_none;     helpText:'Type check real;#Matches on scalar reals'),
      (defaultId:':realList';    reservedWordClass:rwc_typeCheck;    payloadType:tpt_none;     helpText:'Type check real list;#Matches on lists of reals and empty lists'),
      (defaultId:':string';      reservedWordClass:rwc_typeCheck;    payloadType:tpt_none;     helpText:'Type check string;#Matches on scalar strings'),
      (defaultId:':stringList';  reservedWordClass:rwc_typeCheck;    payloadType:tpt_none;     helpText:'Type check string list;#Matches on lists of strings and empty lists'),
      (defaultId:':numeric';     reservedWordClass:rwc_typeCheck;    payloadType:tpt_none;     helpText:'Type check numeric;#Matches on scalar integers and reals'),
      (defaultId:':numericList'; reservedWordClass:rwc_typeCheck;    payloadType:tpt_none;     helpText:'Type check numeric list;#Matches on lists of integers and reals (mixing is allowed) and empty lists'),
      (defaultId:':expression';  reservedWordClass:rwc_typeCheck;    payloadType:tpt_none;     helpText:'Type check expression;#Matches on expressions#In patterns it can be modified to match only on expressions with a given arity'),
      (defaultId:':keyValueList';reservedWordClass:rwc_typeCheck;    payloadType:tpt_none;     helpText:'Type check key-value-list;#Matches on key-value-lists and empty lists#A key-value list only consists of sub-lists of size 2 whose first element is a string'),
      (defaultId:'';             reservedWordClass:rwc_not_reserved; payloadType:tpt_userRule; helpText:'Custom type check'),
      (defaultId:';';            reservedWordClass:rwc_not_reserved; payloadType:tpt_none;     helpText:'Semicolon#Ends a statement'),
      (defaultId:'...';          reservedWordClass:rwc_not_reserved; payloadType:tpt_none;     helpText:'Remaining arguments#Allowes access to anonymous furhter parameters#Returns a list'),
      (defaultId:'private';      reservedWordClass:rwc_modifier;     payloadType:tpt_none;     helpText:'Modifier private#Limits visiblity of the declaration to the package it is declared in'),
      (defaultId:'memoized';     reservedWordClass:rwc_modifier;     payloadType:tpt_none;     helpText:'Modifier memoized#Makes the rule memoized, caching previously computed results'),
      (defaultId:'mutable';      reservedWordClass:rwc_modifier;     payloadType:tpt_none;     helpText:'Modifier mutable#Makes the rule mutable, de facto changing the rule to a variable'),
      (defaultId:'persistent';   reservedWordClass:rwc_modifier;     payloadType:tpt_none;     helpText:'Modifier persistent#Makes the rule persistent.#Persistent rules also are mutable'),
      (defaultId:'datastore';    reservedWordClass:rwc_modifier;     payloadType:tpt_none;     helpText:'Modifier datastore#Makes the rule persistent in a separate file.#Persistent rules also are mutable'),
      (defaultId:'synchronized'; reservedWordClass:rwc_modifier;     payloadType:tpt_none;     helpText:'Modifier synchronized#Protects the rule from concurrent execution.'),
      (defaultId:'local';        reservedWordClass:rwc_modifier;     payloadType:tpt_none;     helpText:'Modifier local#Used for declaring block-local variables'),
      (defaultId:'type';         reservedWordClass:rwc_modifier;     payloadType:tpt_none;     helpText:'Modifier type#Used for declaring custom type checks'),
      (defaultId:'';             reservedWordClass:rwc_not_reserved; payloadType:tpt_comment;  helpText:'End-Of-Input#Helper token; May also indicate a comment'),
      (defaultId:'';             reservedWordClass:rwc_not_reserved; payloadType:tpt_comment;  helpText:'Blank#Helper token; May indicate a comment or whitespace'));

  C_specialWordInfo:array[0..6] of record
    txt:string;
    reservedWordClass:T_reservedWordClass;
    helpText:string;
  end=((txt:'USE'; reservedWordClass:rwc_modifier; helpText:'Marker: USE#As first token in a package it denotes the use clause#Followed by package paths or package ids'),
       (txt:C_voidText; reservedWordClass:rwc_specialLiteral; helpText:'void literal#Denotes a literal "which is not there"#Intended use: list construction and blank branches of inline if clauses'),
       (txt:C_nanText; reservedWordClass:rwc_specialLiteral; helpText:'not-a-number real literal'),
       (txt:C_infText; reservedWordClass:rwc_specialLiteral; helpText:'infinity real literal'),
       (txt:'false'; reservedWordClass:rwc_specialLiteral; helpText:'false literal'),
       (txt:'true'; reservedWordClass:rwc_specialLiteral; helpText:'true literal'),
       (txt:'main'; reservedWordClass:rwc_not_reserved; helpText:'main rule#Called when the script is executed from the command line (or via "call main" in the GUI)'));

  C_typeString: array[T_literalType] of string = (
    'error',
    'boolean',
    'int',
    'real',
    'string',
    'expression',
    'list',
    'booleanList',
    'intList',
    'realList',
    'numericList',
    'stringList',
    'emptyList',
    'keyValueList',
    'flatList',
    'list(containing error)',
    C_voidText);

  C_ruleTypeString: array[tt_localUserRule..tt_intrinsicRule] of string = (
    'user function (local)',
    'user function (imported)',
    'built in function');

TYPE
  T_ruleType=(rt_normal,
          rt_memoized,
          rt_mutable_public,
          rt_mutable_private,
          rt_persistent_public,
          rt_persistent_private,
          rt_datastore_public,
          rt_datastore_private,
          rt_synchronized,
          rt_customTypeCheck);
CONST C_mutableRuleTypes:set of T_ruleType=[rt_mutable_public,rt_mutable_private,rt_persistent_public,rt_persistent_private,rt_datastore_public,rt_datastore_private];
      C_ruleTypesWithOnlyOneSubrule:set of T_ruleType=[rt_mutable_public,rt_mutable_private,rt_persistent_public,rt_persistent_private,rt_datastore_public,rt_datastore_private,rt_customTypeCheck];
      C_csProtectedRuleTypes:set of T_ruleType=[rt_memoized,rt_mutable_public,rt_mutable_private,rt_persistent_public,rt_persistent_private,rt_synchronized,rt_datastore_public,rt_datastore_private];
      C_publicRuleTypes:set of T_ruleType=[rt_mutable_public,rt_persistent_public,rt_datastore_public,rt_customTypeCheck];
      C_ruleTypeText:array[T_ruleType] of string=(
      '','memoized ',
         'mutable ',
         'private mutable ',
         'persistent ',
         'private persistent ',
         'datastore ',
         'private datastore ',
         'synchronized ',
         'type ');
      C_validModifierCombinations:array[0..14] of record
        modifiers:T_modifierSet;
        ruleType:T_ruleType;
      end=((modifiers:[];ruleType:rt_normal),
           (modifiers:[tt_modifier_private];ruleType:rt_normal),
           (modifiers:[tt_modifier_memoized];ruleType:rt_memoized),
           (modifiers:[tt_modifier_memoized,tt_modifier_private];ruleType:rt_memoized),
           (modifiers:[tt_modifier_mutable];ruleType:rt_mutable_public),
           (modifiers:[tt_modifier_mutable,tt_modifier_private];ruleType:rt_mutable_private),
           (modifiers:[tt_modifier_persistent];ruleType:rt_persistent_public),
           (modifiers:[tt_modifier_persistent,tt_modifier_private];ruleType:rt_persistent_private),
           (modifiers:[tt_modifier_datastore];ruleType:rt_datastore_public),
           (modifiers:[tt_modifier_datastore,tt_modifier_private];ruleType:rt_datastore_private),
           (modifiers:[tt_modifier_memoized];ruleType:rt_memoized),
           (modifiers:[tt_modifier_memoized,tt_modifier_private];ruleType:rt_memoized),
           (modifiers:[tt_modifier_synchronized];ruleType:rt_synchronized),
           (modifiers:[tt_modifier_synchronized,tt_modifier_private];ruleType:rt_synchronized),
           (modifiers:[tt_modifier_customType];ruleType:rt_customTypeCheck));

TYPE
  T_messageType = (
    mt_clearConsole,
    mt_printline,
    mt_echo_input,
    mt_echo_declaration,
    mt_echo_output,
    mt_echo_continued,
    mt_el1_note,
    mt_el2_warning,
    mt_el3_evalError,
    mt_el3_noMatchingMain,
    mt_el3_stackTrace,
    mt_el3_userDefined,
    mt_el4_parsingError,
    mt_el5_systemError,
    mt_el5_haltMessageReceived,
    mt_el5_haltMessageQuiet,
    mt_endOfEvaluation,
    mt_reloadRequired,
    mt_timing_info
    {$ifdef fullVersion},
    mt_plotFileCreated,
    mt_plotCreatedWithDeferredDisplay,
    mt_plotCreatedWithInstantDisplay,
    mt_plotSettingsChanged,
    mt_evaluatedStatementInInteractiveMode,
    mt_displayTable,
    mt_guiPseudoPackageFound
    {$endif});

  T_messageTypeSet=set of T_messageType;

CONST
  C_messageTypeMeta:array[T_messageType] of record
    level:shortint;
    prefix:string;
    includeLocation,ignoredBySandbox,triggersGuiStartup,textOut:boolean;
  end = (
{mt_clearConsole           }             (level:-2; prefix: ''                     ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup:false; textOut:true),
{mt_printline              }             (level:-2; prefix: ''                     ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup:false; textOut:true),
{mt_echo_input             }             (level:-1; prefix: ' in>'                 ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup:false; textOut:true),
{mt_echo_declaration       }             (level:-1; prefix: ' in>'                 ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup:false; textOut:true),
{mt_echo_output            }             (level:-1; prefix: 'out>'                 ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup:false; textOut:true),
{mt_echo_continued         }             (level:-1; prefix: '...>'                 ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup:false; textOut:true),
{mt_el1_note               }             (level: 1; prefix: 'Note '                ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; textOut:true),
{mt_el2_warning            }             (level: 2; prefix: 'Warning '             ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; textOut:true),
{mt_el3_evalError          }             (level: 3; prefix: 'Error '               ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; textOut:true),
{mt_el3_noMatchingMain     }             (level: 3; prefix: 'Error '               ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; textOut:true),
{mt_el3_stackTrace         }             (level: 3; prefix: 'Error [stack trace]'  ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; textOut:true),
{mt_el3_userDefined        }             (level: 3; prefix: 'User-Error '          ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; textOut:true),
{mt_el4_parsingError       }             (level: 4; prefix: 'Parsing Error '       ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; textOut:true),
{mt_el5_systemError        }             (level: 5; prefix: 'Sys. Error '          ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; textOut:true),
{mt_el5_haltMessageReceived}             (level: 5; prefix: 'Evaluation haltet'    ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; textOut:true),
{mt_el5_haltMessageQuiet   }             (level: 5; prefix: ''                     ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup:false; textOut:false),
{mt_endOfEvaluation        }             (level:-1; prefix: ''                     ; includeLocation: false; ignoredBySandbox:  true; triggersGuiStartup:false; textOut:false),
{mt_reloadRequired         }             (level:-1; prefix: ''                     ; includeLocation: false; ignoredBySandbox:  true; triggersGuiStartup:false; textOut:false),
{mt_timing_info            }             (level:-1; prefix: UTF8_ZERO_WIDTH_SPACE  ; includeLocation: false; ignoredBySandbox:  true; triggersGuiStartup:false; textOut:true)
{$ifdef fullVersion},
{mt_plotFileCreated                    } (level:-1; prefix: 'Image:'               ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup:false; textOut:false),
{mt_plotCreatedWithDeferredDisplay     } (level:-1; prefix: 'Deferred plot request'; includeLocation: false; ignoredBySandbox:  true; triggersGuiStartup:false; textOut:false),
{mt_plotCreatedWithInstantDisplay      } (level:-1; prefix: 'Instant plot request' ; includeLocation: false; ignoredBySandbox:  true; triggersGuiStartup: true; textOut:false),
{mt_plotSettingsChanged                } (level:-1; prefix: 'Plot settings changed'; includeLocation: false; ignoredBySandbox:  true; triggersGuiStartup:false; textOut:false),
{mt_evaluatedStatementInInteractiveMode} (level:-1; prefix: 'Statement No.'        ; includeLocation: false; ignoredBySandbox:  true; triggersGuiStartup:false; textOut:false),
{mt_displayTable                       } (level:-1; prefix: ''                     ; includeLocation: false; ignoredBySandbox:  true; triggersGuiStartup: true; textOut:false),
{mt_guiPseudoPackageFound              } (level:-1; prefix: ''                     ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup: true; textOut:false)
{$endif});


  SELF_TOKEN_TEXT='$self';
  SELF_TOKEN_PAR_IDX=maxLongint;
  ALL_PARAMETERS_TOKEN_TEXT='$params';
  ALL_PARAMETERS_PAR_IDX=SELF_TOKEN_PAR_IDX-1;
  REMAINING_PARAMETERS_IDX=ALL_PARAMETERS_PAR_IDX-1;
  DOC_COMMENT_PREFIX='//*';
  SPECIAL_COMMENT_BLOB_BEGIN='//!';

FUNCTION isReservedNamespace(CONST id:ansistring):boolean;
FUNCTION isReservedWord(CONST wordText:ansistring):T_reservedWordClass;
FUNCTION new_P_idString(CONST id:T_idString):P_idString;
FUNCTION new_P_idString(CONST id:P_idString):P_idString;
FUNCTION new_P_commentString(CONST id:T_commentString):P_commentString;
FUNCTION new_P_commentString(CONST id:P_commentString):P_commentString;

IMPLEMENTATION
FUNCTION isQualified(CONST s:string):boolean;
  begin
    result:=pos(C_ID_QUALIFY_CHARACTER,s)>0;
  end;

FUNCTION isReservedNamespace(CONST id:ansistring):boolean;
  VAR n:T_namespace;
  begin
    for n:=low(T_namespace) to high(T_namespace) do if id=C_namespaceString[n] then exit(true);
    result:=false;
  end;

FUNCTION isReservedWord(CONST wordText:ansistring):T_reservedWordClass;
  VAR tt:T_tokenType;
  begin
    for tt:=low(T_tokenType) to high(T_tokenType) do if C_tokenInfo[tt].defaultId=wordText then exit(C_tokenInfo[tt].reservedWordClass);
    result:=rwc_not_reserved;
  end;

FUNCTION new_P_idString(CONST id: T_idString): P_idString;
  begin
    getMem(result,sizeOf(T_idString));
    result^:=id;
  end;

FUNCTION new_P_idString(CONST id: P_idString): P_idString;
  begin
    getMem(result,sizeOf(T_idString));
    result^:=id^;
  end;

FUNCTION new_P_commentString(CONST id: T_commentString): P_commentString;
  begin
    getMem(result,sizeOf(T_commentString));
    result^:=id;
  end;

FUNCTION new_P_commentString(CONST id: P_commentString): P_commentString;
  begin
    getMem(result,sizeOf(T_commentString));
    result^:=id^;
  end;

FUNCTION getAppName: string;
  begin
    result:=C_appName;
  end;

FUNCTION configDir:string;
  begin
    OnGetApplicationName:=@getAppName;
    {$ifdef WINDOWS}
    result:=GetAppConfigDir(true);
    {$else}
    result:=GetAppConfigDir(false);
    {$endif}
  end;

{ T_commentPayload }

CONSTRUCTOR T_commentPayload.create(CONST txt_: T_commentString);
  begin
    inherited create(tpt_comment);
    txt:=txt_;
  end;

DESTRUCTOR T_commentPayload.destroy;
  begin
    inherited destroy;
    txt:='';
  end;

FUNCTION T_commentPayload.hash: T_hashInt;
  VAR i:longint;
  begin
    {$Q-}{$R-}
    result:=length(txt);
    for i:=1 to length(txt) do result:=result*31+ord(txt[i]);
    {$Q+}{$R+}
  end;

FUNCTION T_commentPayload.toString(CONST tokType: T_tokenType; CONST lengthLimit: longint): ansistring;
  begin
    result:=txt;
  end;

FUNCTION T_commentPayload.cloneOrCopy(CONST tokType: T_tokenType): P_tokenPayload;
  begin
    new(P_commentPayload(result),create(txt));
  end;

{ T_tokenPayload }

CONSTRUCTOR T_tokenPayload.create(CONST typ:T_tokenPayloadType); begin payloadType:=typ; end;
DESTRUCTOR T_tokenPayload.destroy; begin end;
{ T_idPayload }
CONSTRUCTOR T_idPayload.create(CONST id_: T_idString; CONST typ:T_tokenPayloadType);
  begin
    inherited create(typ);
    id:=id_;
  end;

DESTRUCTOR T_idPayload.destroy;
  begin
    inherited destroy;
  end;

FUNCTION T_idPayload.hash: T_hashInt;
  VAR i:longint;
  begin
    {$Q-}{$R-}
    result:=length(id);
    for i:=1 to length(id) do result:=result*31+ord(id[i]);
    {$Q+}{$R+}
  end;

FUNCTION T_idPayload.toString(CONST tokType: T_tokenType; CONST lengthLimit: longint): ansistring;
  begin
    result:=id;
  end;

FUNCTION T_idPayload.cloneOrCopy(CONST tokType: T_tokenType): P_tokenPayload;
  VAR typedResult:P_idPayload;
  begin
    new(typedResult,create(id,tpt_identifier));
    result:=typedResult;
  end;

{ T_idWithPointerPayload }
CONSTRUCTOR T_idWithPointerPayload.create(CONST id_: T_idString; CONST ptr_: pointer);
  begin
    inherited create(id_,tpt_idWithPointerPayload);
    ptr:=ptr_;
  end;

FUNCTION T_idWithPointerPayload.cloneOrCopy(CONST tokType: T_tokenType): P_tokenPayload;
  begin
    result:=@self;
  end;


INITIALIZATION
  OnGetApplicationName:=@getAppName;
  ForceDirectories(configDir);
end.
