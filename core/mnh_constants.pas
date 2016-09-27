UNIT mnh_constants;
INTERFACE
USES sysutils;
{$ifdef CPU32}
//This directive seems to be beneficial only for 32bit targets
{$PACKENUM 1}
{$endif}
CONST
  LOGO:array[0..6] of string=(
  ' ___      ___ ___   ___ ___   ___',
  '|   \    /   |   \ |   |   | |   |  ______',
  '|    \  /    |    \|   |   |_|   | |   ___|',
  '|     \/     |     \   |         | |  |__',
  '|   \    /   |   \     |    _    | |___  \',
  '|   |\  /|   |   |\    |   | |   |  ___)  |',
  '|___| \/ |___|___| \___|___| |___| |_____/');

  UTF8_ZERO_WIDTH_SPACE=#226#128#139;
  APP_NAME             ='MNH';
  APP_TITLE            ='MNH5';
  SCRIPT_EXTENSION     ='.mnh';

  LITERAL_TEXT_VOID    = 'void';
  LITERAL_NAN_TEXT     = 'Nan';
  LITERAL_INF_TEXT     = 'Inf';
  LITERAL_BOOL_TEXT: array[boolean] of string = ('false', 'true');

  ONE_SECOND                    =1/(24*60*60);
  ONE_MINUTE                    =1/(24*60);

  ID_QUALIFY_CHARACTER          ='.';
  EACH_INDEX_IDENTIFIER         ='index';
  MAIN_RULE_ID                  ='main';
  BUILTIN_PSEUDO_LOCATION_PREFIX='builtin';
  SELF_TOKEN_TEXT               ='$self';
  SELF_TOKEN_PAR_IDX            =maxLongint;
  ALL_PARAMETERS_TOKEN_TEXT     ='$params';
  ALL_PARAMETERS_PAR_IDX        =SELF_TOKEN_PAR_IDX-1;
  REMAINING_PARAMETERS_IDX      =ALL_PARAMETERS_PAR_IDX-1;
  DOC_COMMENT_PREFIX            ='//*';
  SPECIAL_COMMENT_BLOB_BEGIN    ='//!';
  {$ifdef fullVersion}
  FORCE_GUI_PSEUDO_PACKAGE      ='GUI';
  {$endif}
TYPE
  T_hashInt  =dword;
  T_idString =string[100];
  T_myFloat = extended;
  P_abstractPackage=^T_abstractPackage;
  T_abstractPackage=object
    FUNCTION getPath:ansistring; virtual; abstract;
  end;
  T_reservedWordClass=(rwc_not_reserved,
                       rwc_specialLiteral,
                       rwc_specialConstruct,
                       rwc_operator,
                       rwc_typeCheck,
                       rwc_modifier);

  T_namespace=(DEFAULT_BUILTIN_NAMESPACE,
               MATH_NAMESPACE           ,
               STRINGS_NAMESPACE        ,
               LIST_NAMESPACE           ,
               REGEX_NAMESPACE          ,
               SYSTEM_BUILTIN_NAMESPACE ,
               FILES_BUILTIN_NAMESPACE  ,
               TYPECAST_NAMESPACE
               {$ifdef fullVersion},PLOT_NAMESPACE{$endif}
               );
CONST
  C_namespaceString:array[T_namespace] of string=(
    'mnh',
    'math',
    'strings',
    'lists',
    'regex',
    'system',
    'files',
    'typecast'
    {$ifdef fullVersion},'plot'{$endif}
    );
TYPE
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

  T_tokenTypeSet  =set of T_tokenType;
  T_modifier      =tt_modifier_private..tt_modifier_customType;
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
CONST
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
    LITERAL_TEXT_VOID);
TYPE
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
                                 defaultId:string;         reservedWordClass:T_reservedWordClass;  helpText:string; end=(
{tt_literal}                    (defaultId:'';             reservedWordClass:rwc_not_reserved;     helpText:'A literal'),
{tt_aggregatorExpressionLiteral}(defaultId:'';             reservedWordClass:rwc_not_reserved;     helpText:'An aggregator expression literal'),
{tt_identifier}                 (defaultId:'';             reservedWordClass:rwc_not_reserved;     helpText:'An identifier (unresolved)'),
{tt_parameterIdentifier}        (defaultId:'';             reservedWordClass:rwc_not_reserved;     helpText:'A parameter identifier'),
{tt_localUserRule}              (defaultId:'';             reservedWordClass:rwc_not_reserved;     helpText:'A local user rule'),
{tt_importedUserRule}           (defaultId:'';             reservedWordClass:rwc_not_reserved;     helpText:'An imported user rule'),
{tt_intrinsicRule}              (defaultId:'';             reservedWordClass:rwc_not_reserved;     helpText:'A built in rule'),
{tt_rulePutCacheValue}          (defaultId:'';             reservedWordClass:rwc_not_reserved;     helpText:'A put-cache-value call'),
{tt_customTypeRule}             (defaultId:'';             reservedWordClass:rwc_not_reserved;     helpText:'A custom type check rule'),
{tt_blockLocalVariable}         (defaultId:'';             reservedWordClass:rwc_not_reserved;     helpText:'A block-local variable'),
{tt_ponFlipper}                 (defaultId:'.';            reservedWordClass:rwc_not_reserved;     helpText:'A pseudo-object-notation flipper'),
{tt_aggregatorConstructor}      (defaultId:'aggregator';   reservedWordClass:rwc_specialConstruct; helpText:'Special construct: aggregator#The aggregator constructor'),
{tt_each}                       (defaultId:'.each';        reservedWordClass:rwc_specialConstruct; helpText:'Special construct: each#Used for (serial) list operations.#Syntax: <list>.each(<id>,<body>,<aggregator>)#<body> is an arbitrary expression which may use <id> to refer to the current list element or "index" for the current index#<aggregator> is optional and may be a simple operator'),
{tt_parallelEach}               (defaultId:'.pEach';       reservedWordClass:rwc_specialConstruct; helpText:'Special construct: pEach (parallel each)#Used for parallel list operations.#Parallelized depending on the systen settings.#Syntax: <list>.pEach(<id>,<body>,<aggregator>)#<body> is an arbitrary expression which may use <id> to refer to the current list element or "index" for the current index#<aggregator> is optional and may be a simple operator'),
{tt_forcedParallelEach}         (defaultId:'.PEach';       reservedWordClass:rwc_specialConstruct; helpText:'Special construct: PEach (forced parallel each)#Used for parallel list operations.#Parallelized independent from systen settings.#Syntax: <list>.PEach(<id>,<body>,<aggregator>)#<body> is an arbitrary expression which may use <id> to refer to the current list element or "index" for the current index#<aggregator> is optional and may be a simple operator'),
{tt_agg}                        (defaultId:'.agg';         reservedWordClass:rwc_specialConstruct; helpText:'Special construct: agg#Used for list aggregation#Syntax: <list>.agg(<aggregator>) - where <aggregator> may be an expression or a simple operator as +'),
{tt_while}                      (defaultId:'while';        reservedWordClass:rwc_specialConstruct; helpText:'Special construct: while#Used for loops#Syntax: while(<entry condition>,<body>) - where <entry condition> must return a scalar boolean'),
{tt_beginBlock}                 (defaultId:'begin';        reservedWordClass:rwc_specialConstruct; helpText:'Special construct: begin#Opening delimiter for procedural blocks'),
{tt_beginFunc}                  (defaultId:'';             reservedWordClass:rwc_specialConstruct; helpText:''), {No default ID, because tokenizer shall not produce this token}
{tt_endBlock}                   (defaultId:'end';          reservedWordClass:rwc_specialConstruct; helpText:'Special construct: end#Closing delimiter for procedural blocks'),
{tt_endFunc}                    (defaultId:'';             reservedWordClass:rwc_specialConstruct; helpText:''), {No default ID, because tokenizer shall not produce this token}
{tt_try}                        (defaultId:'try';          reservedWordClass:rwc_specialConstruct; helpText:'Special construct: try#Used for local exception handling#Syntax: try(<body>,<catch body>) - where <catch body> is executed only if evaluation of <body> fails'),
{tt_toId}                       (defaultId:'toId';         reservedWordClass:rwc_specialConstruct; helpText:'Special construct: toId#Returns the string argument as an identifier'),
{tt_pseudoFuncPointer}          (defaultId:'::';           reservedWordClass:rwc_specialConstruct; helpText:'Special construct: ::# Returns reference to a function#::f -> {f@$params}'),
{tt_braceOpen}                  (defaultId:'(';            reservedWordClass:rwc_not_reserved;     helpText:'Opening round bracket#Used as in default mathematical syntax.'),
{tt_braceClose}                 (defaultId:')';            reservedWordClass:rwc_not_reserved;     helpText:'Closing round bracket#Used as in default mathematical syntax.'),
{tt_parList_constructor}        (defaultId:'';             reservedWordClass:rwc_not_reserved;     helpText:'A parameter list constructor'),
{tt_parList}                    (defaultId:'';             reservedWordClass:rwc_not_reserved;     helpText:'A parameter list'),
{tt_listBraceOpen}              (defaultId:'[';            reservedWordClass:rwc_not_reserved;     helpText:'Square opening bracket#Used for list construction and list access'),
{tt_listBraceClose}             (defaultId:']';            reservedWordClass:rwc_not_reserved;     helpText:'Square closing bracket#Used for list construction and list access'),
{tt_list_constructor}           (defaultId:'';             reservedWordClass:rwc_not_reserved;     helpText:'A list constructor'),
{tt_expBraceOpen}               (defaultId:'{';            reservedWordClass:rwc_not_reserved;     helpText:'Curly opening bracket#Delimits an expression'),
{tt_expBraceClose}              (defaultId:'}';            reservedWordClass:rwc_not_reserved;     helpText:'Curly closing bracket#Delimits an expression'),
{tt_separatorComma}             (defaultId:',';            reservedWordClass:rwc_not_reserved;     helpText:'Separator comma'),
{tt_separatorCnt}               (defaultId:'..';           reservedWordClass:rwc_not_reserved;     helpText:'Separator ..#Used for constructing ranges and only allowed in that context'),
{tt_comparatorEq}               (defaultId:'=';            reservedWordClass:rwc_operator;         helpText:'Equals operator#Returns true if the scalar comparands are type-compatible#and equal#For list operands a list of booleans is returned'),
{tt_comparatorNeq}              (defaultId:'<>';           reservedWordClass:rwc_operator;         helpText:'Not-equals operator#Returns true if the scalar comparands are type-compatible#and not equal#For list operands a list of booleans is returned'),
{tt_comparatorLeq}              (defaultId:'<=';           reservedWordClass:rwc_operator;         helpText:'Lesser-or-equals operator#Returns true if the scalar comparands are type-compatible#and the left hand side is lesser or equal to the right hand side#For list operands a list of booleans is returned'),
{tt_comparatorGeq}              (defaultId:'>=';           reservedWordClass:rwc_operator;         helpText:'Greater-or-equals operator#Returns true if the scalar comparands are type-compatible#and the left hand side is greater or equal to the right hand side#For list operands a list of booleans is returned'),
{tt_comparatorLss}              (defaultId:'<';            reservedWordClass:rwc_operator;         helpText:'Lesser operator#Returns true if the scalar comparands are type-compatible#and the left hand side is lesser than the right hand side#For list operands a list of booleans is returned'),
{tt_comparatorGrt}              (defaultId:'>';            reservedWordClass:rwc_operator;         helpText:'Greater operator#Returns true if the scalar comparands are type-compatible#and the left hand side is greater than the right hand side#For list operands a list of booleans is returned'),
{tt_comparatorListEq}           (defaultId:'==';           reservedWordClass:rwc_operator;         helpText:'Strict-equals operator#Returns true if the comparands are strictly equal.#Always returns a scalar boolean'),
{tt_operatorAnd}                (defaultId:'and';          reservedWordClass:rwc_operator;         helpText:'Bit-wise and operator#Applies to (lists of) integers and (lists of) booleans'),
{tt_operatorOr}                 (defaultId:'or';           reservedWordClass:rwc_operator;         helpText:'Bit-wise or operator#Applies to (lists of) integers and (lists of) booleans'),
{tt_operatorXor}                (defaultId:'xor';          reservedWordClass:rwc_operator;         helpText:'Bit-wise xor operator#Applies to (lists of) integers and (lists of) booleans'),
{tt_operatorLazyAnd}            (defaultId:'AND';          reservedWordClass:rwc_operator;         helpText:'Lazy and operator#Applies to scalar booleans only'),
{tt_operatorLazyOr}             (defaultId:'OR';           reservedWordClass:rwc_operator;         helpText:'Lazy or operator#Applies to scalar booleans only'),
{tt_operatorPlus}               (defaultId:'+';            reservedWordClass:rwc_operator;         helpText:'Plus operator#Applies to (lists of) numbers and (lists of) strings'),
{tt_operatorMinus}              (defaultId:'-';            reservedWordClass:rwc_operator;         helpText:'Minus operator#Applies to (lists of) numbers'),
{tt_operatorMult}               (defaultId:'*';            reservedWordClass:rwc_operator;         helpText:'Multiplication operator#Applies to (lists of) numbers'),
{tt_operatorDivReal}            (defaultId:'/';            reservedWordClass:rwc_operator;         helpText:'Division operator#Applies to (lists of) numbers'),
{tt_operatorDivInt}             (defaultId:'div';          reservedWordClass:rwc_operator;         helpText:'Integer division operator#Applies to (lists of) integers'),
{tt_operatorMod}                (defaultId:'mod';          reservedWordClass:rwc_operator;         helpText:'Integer modulo operator#Applies to (lists of) integers'),
{tt_operatorPot}                (defaultId:'^';            reservedWordClass:rwc_operator;         helpText:'Potentiation operator#Applies to (lists of) numbers'),
{tt_unaryOpPlus}                (defaultId:'+';            reservedWordClass:rwc_operator;         helpText:'Unary plus operator#Neutral'),
{tt_unaryOpMinus}               (defaultId:'-';            reservedWordClass:rwc_operator;         helpText:'Unary minus operator#Negates the operand'),
{tt_operatorStrConcat}          (defaultId:'&';            reservedWordClass:rwc_operator;         helpText:'String concatenation operator#Applies to all literals'),
{tt_operatorOrElse}             (defaultId:'orElse';       reservedWordClass:rwc_operator;         helpText:'Or-Else operator#Employed to provide a fallback to void literals'),
{tt_operatorConcat}             (defaultId:'|';            reservedWordClass:rwc_operator;         helpText:'List concatenation operator#Applies to all literals'),
{tt_operatorIn}                 (defaultId:'in';           reservedWordClass:rwc_operator;         helpText:'In operator#Applies to all literals on the left hand side and lists on the right hand side.#Returns true if the RHS contains the LHS'),
{tt_iifCheck}                   (defaultId:'?';            reservedWordClass:rwc_operator;         helpText:'Inline-if-operator'),
{tt_iifElse}                    (defaultId:':';            reservedWordClass:rwc_operator;         helpText:'Inline-if-operator'),
{tt_listToParameterList}        (defaultId:'@';            reservedWordClass:rwc_operator;         helpText:'List-to-parameter-list operator'),
{tt_declare}                    (defaultId:'->';           reservedWordClass:rwc_operator;         helpText:'Declaration operator'),
{tt_assign}                     (defaultId:':=';           reservedWordClass:rwc_operator;         helpText:'Assignment operator'),
{tt_mutate}                     (defaultId:':=';           reservedWordClass:rwc_operator;         helpText:'Mutate-assign operator'),
{tt_assignNewBlockLocal}        (defaultId:':=';           reservedWordClass:rwc_operator;         helpText:'Assign new block local operator'),
{tt_assignExistingBlockLocal}   (defaultId:':=';           reservedWordClass:rwc_operator;         helpText:'Assign existing block local operator'),
{tt_cso_assignPlus}             (defaultId:'+=';           reservedWordClass:rwc_operator;         helpText:'C-Style assign-increment operator'),
{tt_cso_assignMinus}            (defaultId:'-=';           reservedWordClass:rwc_operator;         helpText:'C-Style assign-decrement operator'),
{tt_cso_assignMult}             (defaultId:'*=';           reservedWordClass:rwc_operator;         helpText:'C-Style assign-multiply operator'),
{tt_cso_assignDiv}              (defaultId:'/=';           reservedWordClass:rwc_operator;         helpText:'C-Style assign-divide operator'),
{tt_cso_assignStrConcat}        (defaultId:'&=';           reservedWordClass:rwc_operator;         helpText:'C-Style assign-(string-)concatenate operator'),
{tt_cso_assignAppend}           (defaultId:'|=';           reservedWordClass:rwc_operator;         helpText:'C-Style assign-(list-)concatenate operator'),
{tt_typeCheckScalar}            (defaultId:':scalar';      reservedWordClass:rwc_typeCheck;        helpText:'Type check scalar;#Matches on all non-lists'),
{tt_typeCheckList}              (defaultId:':list';        reservedWordClass:rwc_typeCheck;        helpText:'Type check list;#Matches on all lists#In patterns it can be modified to match only lists of a given size'),
{tt_typeCheckBoolean}           (defaultId:':boolean';     reservedWordClass:rwc_typeCheck;        helpText:'Type check boolean;#Matches on scalar booleans'),
{tt_typeCheckBoolList}          (defaultId:':booleanList'; reservedWordClass:rwc_typeCheck;        helpText:'Type check boolean list;#Matches on lists of booleans and empty lists'),
{tt_typeCheckInt}               (defaultId:':int';         reservedWordClass:rwc_typeCheck;        helpText:'Type check integer;#Matches on scalar integers'),
{tt_typeCheckIntList}           (defaultId:':intList';     reservedWordClass:rwc_typeCheck;        helpText:'Type check integer list;#Matches on lists of integers and empty lists'),
{tt_typeCheckReal}              (defaultId:':real';        reservedWordClass:rwc_typeCheck;        helpText:'Type check real;#Matches on scalar reals'),
{tt_typeCheckRealList}          (defaultId:':realList';    reservedWordClass:rwc_typeCheck;        helpText:'Type check real list;#Matches on lists of reals and empty lists'),
{tt_typeCheckString}            (defaultId:':string';      reservedWordClass:rwc_typeCheck;        helpText:'Type check string;#Matches on scalar strings'),
{tt_typeCheckStringList}        (defaultId:':stringList';  reservedWordClass:rwc_typeCheck;        helpText:'Type check string list;#Matches on lists of strings and empty lists'),
{tt_typeCheckNumeric}           (defaultId:':numeric';     reservedWordClass:rwc_typeCheck;        helpText:'Type check numeric;#Matches on scalar integers and reals'),
{tt_typeCheckNumList}           (defaultId:':numericList'; reservedWordClass:rwc_typeCheck;        helpText:'Type check numeric list;#Matches on lists of integers and reals (mixing is allowed) and empty lists'),
{tt_typeCheckExpression}        (defaultId:':expression';  reservedWordClass:rwc_typeCheck;        helpText:'Type check expression;#Matches on expressions#In patterns it can be modified to match only on expressions with a given arity'),
{tt_typeCheckKeyValueList}      (defaultId:':keyValueList';reservedWordClass:rwc_typeCheck;        helpText:'Type check key-value-list;#Matches on key-value-lists and empty lists#A key-value list only consists of sub-lists of size 2 whose first element is a string'),
{tt_customTypeCheck}            (defaultId:'';             reservedWordClass:rwc_not_reserved;     helpText:'Custom type check'),
{tt_semicolon}                  (defaultId:';';            reservedWordClass:rwc_not_reserved;     helpText:'Semicolon#Ends a statement'),
{tt_optionalParameters}         (defaultId:'...';          reservedWordClass:rwc_not_reserved;     helpText:'Remaining arguments#Allowes access to anonymous furhter parameters#Returns a list'),
{tt_modifier_private}           (defaultId:'private';      reservedWordClass:rwc_modifier;         helpText:'Modifier private#Limits visiblity of the declaration to the package it is declared in'),
{tt_modifier_memoized}          (defaultId:'memoized';     reservedWordClass:rwc_modifier;         helpText:'Modifier memoized#Makes the rule memoized, caching previously computed results'),
{tt_modifier_mutable}           (defaultId:'mutable';      reservedWordClass:rwc_modifier;         helpText:'Modifier mutable#Makes the rule mutable, de facto changing the rule to a variable'),
{tt_modifier_persistent}        (defaultId:'persistent';   reservedWordClass:rwc_modifier;         helpText:'Modifier persistent#Makes the rule persistent.#Persistent rules also are mutable'),
{tt_modifier_datastore}         (defaultId:'datastore';    reservedWordClass:rwc_modifier;         helpText:'Modifier datastore#Makes the rule persistent in a separate file.#Persistent rules also are mutable'),
{tt_modifier_synchronized}      (defaultId:'synchronized'; reservedWordClass:rwc_modifier;         helpText:'Modifier synchronized#Protects the rule from concurrent execution.'),
{tt_modifier_local}             (defaultId:'local';        reservedWordClass:rwc_modifier;         helpText:'Modifier local#Used for declaring block-local variables'),
{tt_modifier_customType}        (defaultId:'type';         reservedWordClass:rwc_modifier;         helpText:'Modifier type#Used for declaring custom type checks'),
{tt_EOL}                        (defaultId:'';             reservedWordClass:rwc_not_reserved;     helpText:'End-Of-Input#Helper token; May also indicate a comment'),
{tt_blank}                      (defaultId:'';             reservedWordClass:rwc_not_reserved;     helpText:'Blank#Helper token; May indicate a comment or whitespace'));

  C_specialWordInfo:array[0..6] of record
    txt:string;
    reservedWordClass:T_reservedWordClass;
    helpText:string;
  end=((txt:'USE';      reservedWordClass:rwc_modifier      ; helpText:'Marker: USE#As first token in a package it denotes the use clause#Followed by package paths or package ids'),
       (txt:LITERAL_TEXT_VOID; reservedWordClass:rwc_specialLiteral; helpText:'void literal#Denotes a literal "which is not there"#Intended use: list construction and blank branches of inline if clauses'),
       (txt:LITERAL_NAN_TEXT;  reservedWordClass:rwc_specialLiteral; helpText:'not-a-number real literal'),
       (txt:LITERAL_INF_TEXT;  reservedWordClass:rwc_specialLiteral; helpText:'infinity real literal'),
       (txt:'false';    reservedWordClass:rwc_specialLiteral; helpText:'false literal'),
       (txt:'true';     reservedWordClass:rwc_specialLiteral; helpText:'true literal'),
       (txt:'main';     reservedWordClass:rwc_not_reserved  ; helpText:'main rule#Called when the script is executed from the command line (or via "call main" in the GUI)'));

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
CONST C_mutableRuleTypes:           set of T_ruleType=[rt_mutable_public,rt_mutable_private,rt_persistent_public,rt_persistent_private,rt_datastore_public,rt_datastore_private];
      C_ruleTypesWithOnlyOneSubrule:set of T_ruleType=[rt_mutable_public,rt_mutable_private,rt_persistent_public,rt_persistent_private,rt_datastore_public,rt_datastore_private,rt_customTypeCheck];
      C_csProtectedRuleTypes:       set of T_ruleType=[rt_memoized,rt_mutable_public,rt_mutable_private,rt_persistent_public,rt_persistent_private,rt_synchronized,rt_datastore_public,rt_datastore_private];
      C_publicRuleTypes:            set of T_ruleType=[rt_mutable_public,rt_persistent_public,rt_datastore_public,rt_customTypeCheck];
      C_ruleTypeText:array[T_ruleType] of string=(
      '',
      'memoized ',
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
      end=((modifiers:[];                                             ruleType:rt_normal),
           (modifiers:[                         tt_modifier_private]; ruleType:rt_normal),
           (modifiers:[tt_modifier_memoized];                         ruleType:rt_memoized),
           (modifiers:[tt_modifier_memoized    ,tt_modifier_private]; ruleType:rt_memoized),
           (modifiers:[tt_modifier_mutable];                          ruleType:rt_mutable_public),
           (modifiers:[tt_modifier_mutable     ,tt_modifier_private]; ruleType:rt_mutable_private),
           (modifiers:[tt_modifier_persistent];                       ruleType:rt_persistent_public),
           (modifiers:[tt_modifier_persistent  ,tt_modifier_private]; ruleType:rt_persistent_private),
           (modifiers:[tt_modifier_datastore];                        ruleType:rt_datastore_public),
           (modifiers:[tt_modifier_datastore   ,tt_modifier_private]; ruleType:rt_datastore_private),
           (modifiers:[tt_modifier_memoized];                         ruleType:rt_memoized),
           (modifiers:[tt_modifier_memoized    ,tt_modifier_private]; ruleType:rt_memoized),
           (modifiers:[tt_modifier_synchronized];                     ruleType:rt_synchronized),
           (modifiers:[tt_modifier_synchronized,tt_modifier_private]; ruleType:rt_synchronized),
           (modifiers:[tt_modifier_customType];                       ruleType:rt_customTypeCheck));

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
  C_nonTextMessageTypes:T_messageTypeSet=[
    mt_el5_haltMessageQuiet,
    mt_endOfEvaluation,
    mt_reloadRequired
    {$ifdef fullVersion},
    mt_plotFileCreated,
    mt_plotCreatedWithDeferredDisplay,
    mt_plotCreatedWithInstantDisplay,
    mt_plotSettingsChanged,
    mt_evaluatedStatementInInteractiveMode,
    mt_displayTable,
    mt_guiPseudoPackageFound
    {$endif}
    {$ifdef IMIG},
    mt_displayImage
    {$endif}
    ];
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

FUNCTION isQualified(CONST s:string):boolean;
FUNCTION configDir:string;
FUNCTION isReservedNamespace(CONST id:ansistring):boolean;
FUNCTION isReservedWord(CONST wordText:ansistring):T_reservedWordClass;
//FUNCTION reservedWordsByClass(CONST clazz:T_reservedWordClass):T_listOfString;
IMPLEMENTATION
FUNCTION isQualified(CONST s:string):boolean;
  begin
    result:=pos(ID_QUALIFY_CHARACTER,s)>0;
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

FUNCTION getAppName: string;
  begin
    result:=APP_NAME;
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

INITIALIZATION
  OnGetApplicationName:=@getAppName;
  ForceDirectories(configDir);
end.
