UNIT mnh_constants;
INTERFACE
USES sysutils;
CONST
  STACK_DEPTH_LIMIT={$ifdef Windows}63000{$else}{$ifdef debugMode}2000{$else}4100{$endif}{$endif};
  {$i code_hash.inc}
  {$i built_number.inc}
  LOGO:array[0..20] of string=(
  ' ___      ___ ___   ___ ___   ___',
  '|   \    /   |   \ |   |   | |   |  ______',
  '|    \  /    |    \|   |   |_|   | |   ___|',
  '|     \/     |     \   |         | |  |__',
  '|   \    /   |   \     |    _    | |___  \',
  '|   |\  /|   |   |\    |   | |   |  ___)  |',
  '|___| \/ |___|___| \___|___| |___| |_____/',
  '             (c) Martin Schlegel, 2010-2017',
  '',
  'This program is distributed in the hope that it will be useful,',
  'but WITHOUT ANY WARRANTY; without even the implied warranty of',
  'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.',
  '',
  'You may freely redistribute this software.',
  '',
  'Includes functionality provided by the following projects:',
  '  Free Pascal    (c) 1993-2015 by Florian Klaempfl and others  http://www.freepascal.org/  ',
  '  Ararat Synapse (c) 1999-2003 by Lukas Gebauer                http://www.ararat.cz/synapse/',
  '  EpikTimer      (c) 2003-2014 by Tom Lisjac                   <netdxr@gmail.com>',
  '  TRegExpr lib   (c) 1999-2004 by Andrey V. Sorokin            http://RegExpStudio.com',
  '  Lazarus Component Library (LCL) ');

  //UTF-8 zero width and invisible characters
  ECHO_MARKER   =#226#128#139;
  NOTE_MARKER   =#226#128#140;
//UNUSED_MARKER =#226#128#141;
  ERROR_MARKER  =#226#129#162;
  WARNING_MARKER=#226#129#163;
  TIMING_MARKER =#226#129#164;

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
  ALL_PARAMETERS_TOKEN_TEXT     ='$params';
  PUBLIC_TEXT                   ='public';
  PRIVATE_TEXT                  ='private';
  ALL_PARAMETERS_PAR_IDX        =maxLongint;
  REMAINING_PARAMETERS_IDX      =ALL_PARAMETERS_PAR_IDX-1;
  COMMENT_PREFIX                ='//';
  DOC_COMMENT_PREFIX            =COMMENT_PREFIX+'*';
  ATTRIBUTE_COMMENT_PREFIX      =COMMENT_PREFIX+'@';
  SPECIAL_COMMENT_BLOB_BEGIN    =COMMENT_PREFIX+'!';
  {$ifdef fullVersion}
  FORCE_GUI_PSEUDO_PACKAGE      ='GUI';
  {$endif}
TYPE
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
               TYPECAST_NAMESPACE       ,
               HTTP_NAMESPACE           ,
               IPC_NAMESPACE
               {$ifdef fullVersion},PLOT_NAMESPACE,GUI_NAMESPACE{$endif}
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
    'typecast',
    'http',
    'ipc'
    {$ifdef fullVersion},'plot','gui'{$endif}
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
    tt_each, tt_parallelEach, tt_agg, tt_while, tt_beginBlock, tt_beginRule, tt_beginExpression, tt_endBlock, tt_endRule, tt_endExpression, tt_save, tt_toId, tt_pseudoFuncPointer,
    //lists and list constructors
    tt_braceOpen, tt_braceClose, tt_parList_constructor, tt_parList,
    tt_listBraceOpen, tt_listBraceClose, tt_list_constructor, tt_list_constructor_ranging,
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
    tt_cso_mapPut,tt_cso_mapDrop, //<< >>
    //type checks:
    tt_typeCheckScalar,  tt_typeCheckList,       tt_typeCheckSet,       tt_typeCheckCollection,
    tt_typeCheckBoolean, tt_typeCheckBoolList,   tt_typeCheckBoolSet,   tt_typeCheckBoolCollection,
    tt_typeCheckInt,     tt_typeCheckIntList,    tt_typeCheckIntSet,    tt_typeCheckIntCollection,
    tt_typeCheckReal,    tt_typeCheckRealList,   tt_typeCheckRealSet,   tt_typeCheckRealCollection,
    tt_typeCheckString,  tt_typeCheckStringList, tt_typeCheckStringSet, tt_typeCheckStringCollection,
    tt_typeCheckNumeric, tt_typeCheckNumList,    tt_typeCheckNumSet,    tt_typeCheckNumCollection,
    tt_typeCheckMap,
    tt_typeCheckExpression,
    tt_customTypeCheck,
    tt_semicolon,
    tt_optionalParameters,
    //modifiers:
    tt_modifier_private,
    tt_modifier_memoized,
    tt_modifier_mutable,
    tt_modifier_datastore,
    tt_modifier_plain,
    tt_modifier_synchronized,
    tt_modifier_local,
    tt_modifier_customType,
    //special: [E]nd [O]f [L]ine
    tt_EOL,
    tt_docComment,
    tt_attributeComment,
    tt_blank);

  T_tokenTypeSet  =set of T_tokenType;
  T_modifier      =tt_modifier_private..tt_modifier_customType;
  T_cStyleOperator=tt_cso_assignPlus..tt_cso_mapDrop;
CONST C_ruleModifiers:T_tokenTypeSet=[tt_modifier_private..tt_modifier_synchronized,tt_modifier_customType];
TYPE
  T_modifierSet=set of T_modifier;
  T_literalType = (
    lt_error,
             lt_boolean,     lt_int,     lt_real,                 lt_string,                   lt_expression,
    lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList, lt_emptyList,
    lt_set,  lt_booleanSet,  lt_intSet,  lt_realSet,  lt_numSet,  lt_stringSet,  lt_emptySet,
    lt_map,                                                                      lt_emptyMap,
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
    'set',
    'booleanSet',
    'intSet',
    'realSet',
    'numSet',
    'stringSet',
    'emptySet',
    'map',
    'emptyMap',
    LITERAL_TEXT_VOID);
  C_compatibleSet: array[lt_list..lt_emptyList] of T_literalType=(
    lt_set,
    lt_booleanSet,
    lt_intSet,
    lt_realSet,
    lt_numSet,
    lt_stringSet,
    lt_emptySet);
TYPE
  T_literalTypeSet=set of T_literalType;
  T_tokenTypeInfo=record
    tokenType:T_tokenType;
    reservedWordClass:T_reservedWordClass;
    info:ansistring;
  end;

CONST
  C_containingTypes: array [T_literalType] of T_literalTypeSet=
    {lt_error}      ([],
    {lt_boolean}     [lt_list,lt_booleanList,lt_set,lt_booleanSet],
    {lt_int}         [lt_list,lt_intList,lt_numList,lt_set,lt_intSet,lt_numSet],
    {lt_real}        [lt_list,lt_realList,lt_numList,lt_set,lt_realSet,lt_numSet],
    {lt_string}      [lt_list,lt_stringList,lt_set,lt_stringSet],
    {lt_expression}  [lt_list,lt_set],
    {lt_list}        [lt_list,lt_set,lt_map],
    {lt_booleanList} [lt_list,lt_set,lt_map],
    {lt_intList}     [lt_list,lt_set,lt_map],
    {lt_realList}    [lt_list,lt_set,lt_map],
    {lt_numList}     [lt_list,lt_set,lt_map],
    {lt_stringList}  [lt_list,lt_set,lt_map],
    {lt_emptyList}   [lt_list,lt_set],
    {lt_set}         [lt_list,lt_set],
    {lt_booleanSet}  [lt_list,lt_set],
    {lt_intSet}      [lt_list,lt_set],
    {lt_realSet}     [lt_list,lt_set],
    {lt_numSet}      [lt_list,lt_set],
    {lt_stringSet}   [lt_list,lt_set],
    {lt_emptySet}    [lt_list,lt_set],
    {lt_map}         [lt_list,lt_set],
    {lt_emptyMap}    [lt_list,lt_set],
    {lt_void}        []);
  C_comparableTypes: array [T_literalType] of T_literalTypeSet=
    {lt_error}      ([],
    {lt_boolean}     [lt_expression,lt_boolean,                         lt_list,lt_booleanList,                                                lt_emptyList,lt_set,lt_booleanSet,                                            lt_emptySet],
    {lt_int}         [lt_expression,           lt_int,lt_real,          lt_list,               lt_intList,lt_realList,lt_numList,              lt_emptyList,lt_set,              lt_intSet,lt_realSet,lt_numSet,             lt_emptySet],
    {lt_real}        [lt_expression,           lt_int,lt_real,          lt_list,               lt_intList,lt_realList,lt_numList,              lt_emptyList,lt_set,              lt_intSet,lt_realSet,lt_numSet,             lt_emptySet],
    {lt_string}      [lt_expression,                          lt_string,lt_list,                                                 lt_stringList,lt_emptyList,lt_set,                                             lt_stringSet,lt_emptySet],
    {lt_expression}  [lt_boolean..lt_emptySet],
    {lt_list}        [lt_expression,lt_boolean,lt_int,lt_real,lt_string,lt_list,lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList],
    {lt_booleanList} [lt_expression,lt_boolean,                         lt_list,lt_booleanList                                                ],
    {lt_intList}     [lt_expression,           lt_int,lt_real,          lt_list,               lt_intList,lt_realList,lt_numList              ],
    {lt_realList}    [lt_expression,           lt_int,lt_real,          lt_list,               lt_intList,lt_realList,lt_numList              ],
    {lt_numList}     [lt_expression,           lt_int,lt_real,          lt_list,               lt_intList,lt_realList,lt_numList              ],
    {lt_stringList}  [lt_expression,                          lt_string,lt_list,                                                 lt_stringList],
    {lt_emptyList}   [lt_expression,lt_boolean,lt_int,lt_real,lt_string,                                                                       lt_emptyList],
    {lt_set}         [lt_expression,lt_boolean,lt_int,lt_real,lt_string,lt_set ,lt_booleanSet ,lt_intSet ,lt_realSet ,lt_numSet ,lt_stringSet ,lt_emptySet],
    {lt_booleanSet}  [lt_expression,lt_boolean,                         lt_set ,lt_booleanSet ,                                                lt_emptySet],
    {lt_intSet}      [lt_expression,           lt_int,lt_real,          lt_set ,               lt_intSet ,lt_realSet ,lt_numSet ,              lt_emptySet],
    {lt_realSet}     [lt_expression,           lt_int,lt_real,          lt_set ,               lt_intSet ,lt_realSet ,lt_numSet ,              lt_emptySet],
    {lt_numSet}      [lt_expression,           lt_int,lt_real,          lt_set ,               lt_intSet ,lt_realSet ,lt_numSet ,              lt_emptySet],
    {lt_stringSet}   [lt_expression,                          lt_string,lt_set ,                                                 lt_stringSet ,lt_emptySet],
    {lt_emptySet}    [lt_expression,lt_boolean,lt_int,lt_real,lt_string,lt_set ,lt_booleanSet ,lt_intSet ,lt_realSet ,lt_numSet ,lt_stringSet ,lt_emptySet],
    {lt_map}         [],
    {lt_emptyMap}    [],
    {lt_void}        []);

  C_forbiddenTokenTypes: T_tokenTypeSet=[tt_rulePutCacheValue, tt_agg, tt_parList_constructor, tt_parList,
    tt_declare,
    //type checks:
    tt_typeCheckScalar..tt_customTypeCheck,
    //modifiers:
    tt_modifier_private,
    tt_modifier_memoized,
    tt_modifier_mutable,
    tt_modifier_datastore,
    tt_modifier_plain,
    tt_modifier_synchronized,
    tt_modifier_customType,
    //special: [E]nd [O]f [L]ine
    tt_EOL,
    tt_blank];
  C_nonVoidTypes: T_literalTypeSet=[lt_boolean..lt_emptyMap];
  C_compoundTypes:T_literalTypeSet=[lt_list..lt_emptyMap];
  C_listTypes:    T_literalTypeSet=[lt_list..lt_emptyList];
  C_setTypes :    T_literalTypeSet=[lt_set..lt_emptySet];
  C_mapTypes :    T_literalTypeSet=[lt_map..lt_emptyMap];
  C_emptyCompoundTypes:T_literalTypeSet=[lt_emptyMap,lt_emptySet,lt_emptyList];
  C_scalarTypes:  T_literalTypeSet=[lt_boolean..lt_expression,lt_void];
  C_operatorsForAggregators: T_tokenTypeSet=[tt_operatorAnd..tt_operatorPot,tt_operatorStrConcat,tt_operatorOrElse,tt_operatorConcat];
  C_operatorsAndComparators: T_tokenTypeSet=[tt_comparatorEq..tt_operatorIn];
  C_patternElementComparators: T_tokenTypeSet=[tt_comparatorEq..tt_comparatorListEq,tt_operatorIn];
  C_typeChecks: T_tokenTypeSet=[tt_typeCheckScalar..tt_typeCheckExpression];
  C_openingBrackets:T_tokenTypeSet=[tt_beginBlock,tt_beginRule,tt_beginExpression,tt_each,tt_parallelEach,tt_agg,tt_braceOpen,tt_parList_constructor,tt_listBraceOpen,tt_list_constructor,tt_expBraceOpen,tt_iifCheck];
  C_closingBrackets:T_tokenTypeSet=[tt_endBlock,tt_endRule,tt_endExpression,tt_braceClose,tt_listBraceClose,tt_expBraceClose,tt_iifElse];
  C_unpureTokens:T_tokenTypeSet=[tt_declare..tt_cso_mapDrop,tt_pseudoFuncPointer,tt_toId];
  C_matchingClosingBracket:array[tt_each..tt_iifCheck] of T_tokenType=
    {tt_each}              (tt_braceClose,
    {tt_parallelEach}       tt_braceClose,
    {tt_agg}                tt_braceClose,
    {tt_while}              tt_EOL,
    {tt_beginBlock}         tt_endBlock,
    {tt_beginRule}          tt_endRule,
    {tt_beginExpression}    tt_endExpression,
    {tt_endBlock}           tt_EOL,
    {tt_endRule}            tt_EOL,
    {tt_endExpression}      tt_EOL,
    {tt_save}               tt_EOL,
    {tt_toId}               tt_EOL,
                            tt_EOL,
    {tt_braceOpen}          tt_braceClose,
    {tt_braceClose}         tt_EOL,
    {tt_parList_constructor}tt_braceClose,
    {tt_parList}            tt_EOL,
    {tt_listBraceOpen}      tt_listBraceClose,
    {tt_listBraceClose}     tt_EOL,
    {tt_list_constructor}   tt_listBraceClose,
                            tt_listBraceClose,
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

  C_matchingTypes: array[tt_typeCheckScalar..tt_typeCheckExpression] of T_literalTypeSet =
    {tt_typeCheckScalar}          ([lt_boolean, lt_int, lt_real, lt_string],
    {tt_typeCheckList}             [lt_list..lt_emptyList],
    {tt_typeCheckSet}              [lt_set..lt_emptySet],
    {tt_typeCheckCollection}       [lt_list..lt_emptyList,lt_set..lt_emptySet],
    {tt_typeCheckBoolean}          [lt_boolean],
    {tt_typeCheckBoolList}         [lt_booleanList, lt_emptyList],
    {tt_typeCheckBoolSet}          [lt_booleanSet, lt_emptySet],
    {tt_typeCheckBoolCollection}   [lt_booleanList, lt_emptyList, lt_booleanSet, lt_emptySet],
    {tt_typeCheckInt}              [lt_int],
    {tt_typeCheckIntList}          [lt_intList, lt_emptyList],
    {tt_typeCheckIntSet}           [lt_intSet, lt_emptySet],
    {tt_typeCheckIntCollection}    [lt_intList, lt_emptyList, lt_intSet, lt_emptySet],
    {tt_typeCheckReal}             [lt_real],
    {tt_typeCheckRealList}         [lt_realList, lt_emptyList],
    {tt_typeCheckRealSet}          [lt_realSet, lt_emptySet],
    {tt_typeCheckRealCollection}   [lt_realList, lt_emptyList, lt_realSet, lt_emptySet],
    {tt_typeCheckString}           [lt_string],
    {tt_typeCheckStringList}       [lt_stringList, lt_emptyList],
    {tt_typeCheckStringSet}        [lt_stringSet, lt_emptySet],
    {tt_typeCheckStringCollection} [lt_stringList, lt_emptyList, lt_stringSet, lt_emptySet],
    {tt_typeCheckNumeric}          [lt_int, lt_real],
    {tt_typeCheckNumList}          [lt_intList, lt_realList, lt_numList, lt_emptyList],
    {tt_typeCheckNumSet}           [lt_intSet, lt_realSet, lt_numSet, lt_emptySet],
    {tt_typeCheckNumCollection}    [lt_intList, lt_realList, lt_numList, lt_emptyList, lt_intSet, lt_realSet, lt_numSet, lt_emptySet],
    {tt_typeCheckMap}              [lt_emptyMap,lt_map],
    {tt_typeCheckExpression}       [lt_expression]);
  C_modifieableTypeChecks: T_tokenTypeSet=[
    tt_typeCheckList,       tt_typeCheckSet,       tt_typeCheckCollection,
    tt_typeCheckBoolList,   tt_typeCheckBoolSet,   tt_typeCheckBoolCollection,
    tt_typeCheckIntList,    tt_typeCheckIntSet,    tt_typeCheckIntCollection,
    tt_typeCheckRealList,   tt_typeCheckRealSet,   tt_typeCheckRealCollection,
    tt_typeCheckStringList, tt_typeCheckStringSet, tt_typeCheckStringCollection,
    tt_typeCheckNumList,    tt_typeCheckNumSet,    tt_typeCheckNumCollection,
    tt_typeCheckMap,
    tt_typeCheckExpression];

  C_compatibleEnd:array[tt_beginBlock..tt_beginExpression] of T_tokenType=(tt_endBlock,tt_endRule,tt_endExpression);
  C_tokenInfo:array[T_tokenType] of record
                                 defaultId:string;          defaultHtmlSpan:string; reservedWordClass:T_reservedWordClass;  helpText:string; end=(
{tt_literal}                    (defaultId:'';              defaultHtmlSpan:'literal';    reservedWordClass:rwc_not_reserved;     helpText:'A literal'),
{tt_aggregatorExpressionLiteral}(defaultId:'';              defaultHtmlSpan:'literal';    reservedWordClass:rwc_not_reserved;     helpText:'An aggregator expression literal'),
{tt_identifier}                 (defaultId:'';              defaultHtmlSpan:'identifier'; reservedWordClass:rwc_not_reserved;     helpText:'An identifier (unresolved)'),
{tt_parameterIdentifier}        (defaultId:'';              defaultHtmlSpan:'identifier'; reservedWordClass:rwc_not_reserved;     helpText:'A parameter identifier'),
{tt_localUserRule}              (defaultId:'';              defaultHtmlSpan:'identifier'; reservedWordClass:rwc_not_reserved;     helpText:''),
{tt_importedUserRule}           (defaultId:'';              defaultHtmlSpan:'identifier'; reservedWordClass:rwc_not_reserved;     helpText:''),
{tt_intrinsicRule}              (defaultId:'';              defaultHtmlSpan:'builtin';    reservedWordClass:rwc_not_reserved;     helpText:'A built in rule'),
{tt_rulePutCacheValue}          (defaultId:'';              defaultHtmlSpan:'builtin';    reservedWordClass:rwc_not_reserved;     helpText:'A put-cache-value call'),
{tt_customTypeRule}             (defaultId:'';              defaultHtmlSpan:'identifier'; reservedWordClass:rwc_not_reserved;     helpText:''),
{tt_blockLocalVariable}         (defaultId:'';              defaultHtmlSpan:'identifier'; reservedWordClass:rwc_not_reserved;     helpText:'A block-local variable'),
{tt_ponFlipper}                 (defaultId:'.';             defaultHtmlSpan:'identifier'; reservedWordClass:rwc_not_reserved;     helpText:'A pseudo-object-notation flipper'),
{tt_aggregatorConstructor}      (defaultId:'aggregator';    defaultHtmlSpan:'builtin';    reservedWordClass:rwc_specialConstruct; helpText:'Special construct: aggregator#The aggregator constructor'),
{tt_each}                       (defaultId:'.each';         defaultHtmlSpan:'builtin';    reservedWordClass:rwc_specialConstruct; helpText:'Special construct: each#Used for (serial) list operations.#Syntax: <input>.each(<id>,<body>,<aggregator>)#<body> is an arbitrary expression which may use <id> to refer to the current list element or "index" for the current index#<aggregator> is optional and may be a simple operator'),
{tt_parallelEach}               (defaultId:'.pEach';        defaultHtmlSpan:'builtin';    reservedWordClass:rwc_specialConstruct; helpText:'Special construct: pEach (parallel each)#Used for parallel list operations.#Parallelized depending on the systen settings.#Syntax: <input>.pEach(<id>,<body>,<aggregator>)#<body> is an arbitrary expression which may use <id> to refer to the current list element or "index" for the current index#<aggregator> is optional and may be a simple operator'),
{tt_agg}                        (defaultId:'.agg';          defaultHtmlSpan:'builtin';    reservedWordClass:rwc_specialConstruct; helpText:'Special construct: agg#Used for list aggregation#Syntax: <input>.agg(<aggregator>) - where <aggregator> may be an expression or a simple operator as +'),
{tt_while}                      (defaultId:'while';         defaultHtmlSpan:'builtin';    reservedWordClass:rwc_specialConstruct; helpText:'Special construct: while#Used for loops#Syntax: while(<entry condition>,<body>) - where <entry condition> must return a scalar boolean'),
{tt_beginBlock}                 (defaultId:'begin';         defaultHtmlSpan:'builtin';    reservedWordClass:rwc_specialConstruct; helpText:'Special construct: begin#Opening delimiter for procedural blocks'),
{tt_beginRule}                  (defaultId:'';              defaultHtmlSpan:'builtin';    reservedWordClass:rwc_specialConstruct; helpText:''), {No default ID, because tokenizer shall not produce this token}
{tt_beginExpression}            (defaultId:'';              defaultHtmlSpan:'builtin';    reservedWordClass:rwc_specialConstruct; helpText:''), {No default ID, because tokenizer shall not produce this token}
{tt_endBlock}                   (defaultId:'end';           defaultHtmlSpan:'builtin';    reservedWordClass:rwc_specialConstruct; helpText:'Special construct: end#Closing delimiter for procedural blocks'),
{tt_endRule}                    (defaultId:'';              defaultHtmlSpan:'builtin';    reservedWordClass:rwc_specialConstruct; helpText:''), {No default ID, because tokenizer shall not produce this token}
{tt_endExpression}              (defaultId:'';              defaultHtmlSpan:'builtin';    reservedWordClass:rwc_specialConstruct; helpText:''), {No default ID, because tokenizer shall not produce this token}
{tt_save}                       (defaultId:'save';          defaultHtmlSpan:'builtin';    reservedWordClass:rwc_specialConstruct; helpText:'Special construct: save#Saves the current value store for future function calls'),
{tt_toId}                       (defaultId:'toId';          defaultHtmlSpan:'builtin';    reservedWordClass:rwc_specialConstruct; helpText:'Special construct: toId#Returns the string argument as an identifier'),
{tt_pseudoFuncPointer}          (defaultId:'::';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_specialConstruct; helpText:'Special construct: ::# Returns reference to a function#::f -> {f@$params}'),
{tt_braceOpen}                  (defaultId:'(';             defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'Opening round bracket#Used as in default mathematical syntax.'),
{tt_braceClose}                 (defaultId:')';             defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'Closing round bracket#Used as in default mathematical syntax.'),
{tt_parList_constructor}        (defaultId:'';              defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'A parameter list constructor'),
{tt_parList}                    (defaultId:'';              defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'A parameter list'),
{tt_listBraceOpen}              (defaultId:'[';             defaultHtmlSpan:'literal';    reservedWordClass:rwc_not_reserved;     helpText:'Square opening bracket#Used for list construction and list access'),
{tt_listBraceClose}             (defaultId:']';             defaultHtmlSpan:'literal';    reservedWordClass:rwc_not_reserved;     helpText:'Square closing bracket#Used for list construction and list access'),
{tt_list_constructor}           (defaultId:'';              defaultHtmlSpan:'literal';    reservedWordClass:rwc_not_reserved;     helpText:'A list constructor'),
{tt_list_Constructor_ranging}   (defaultId:'';              defaultHtmlSpan:'literal';    reservedWordClass:rwc_not_reserved;     helpText:'A list constructor'),
{tt_expBraceOpen}               (defaultId:'{';             defaultHtmlSpan:'literal';    reservedWordClass:rwc_not_reserved;     helpText:'Curly opening bracket#Delimits an expression'),
{tt_expBraceClose}              (defaultId:'}';             defaultHtmlSpan:'literal';    reservedWordClass:rwc_not_reserved;     helpText:'Curly closing bracket#Delimits an expression'),
{tt_separatorComma}             (defaultId:',';             defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'Separator comma'),
{tt_separatorCnt}               (defaultId:'..';            defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'Separator ..#Used for constructing ranges and only allowed in that context'),
{tt_comparatorEq}               (defaultId:'=';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Equals operator#Returns true if the scalar comparands are type-compatible#and equal#For list operands a list of booleans is returned'),
{tt_comparatorNeq}              (defaultId:'<>';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Not-equals operator#Returns true if the scalar comparands are type-compatible#and not equal#For list operands a list of booleans is returned'),
{tt_comparatorLeq}              (defaultId:'<=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Lesser-or-equals operator#Returns true if the scalar comparands are type-compatible#and the left hand side is lesser or equal to the right hand side#For list operands a list of booleans is returned'),
{tt_comparatorGeq}              (defaultId:'>=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Greater-or-equals operator#Returns true if the scalar comparands are type-compatible#and the left hand side is greater or equal to the right hand side#For list operands a list of booleans is returned'),
{tt_comparatorLss}              (defaultId:'<';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Lesser operator#Returns true if the scalar comparands are type-compatible#and the left hand side is lesser than the right hand side#For list operands a list of booleans is returned'),
{tt_comparatorGrt}              (defaultId:'>';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Greater operator#Returns true if the scalar comparands are type-compatible#and the left hand side is greater than the right hand side#For list operands a list of booleans is returned'),
{tt_comparatorListEq}           (defaultId:'==';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Strict-equals operator#Returns true if the comparands are strictly equal.#Always returns a scalar boolean'),
{tt_operatorAnd}                (defaultId:'and';           defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Bit-wise and operator#Applies to (lists of) integers and (lists of) booleans'),
{tt_operatorOr}                 (defaultId:'or';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Bit-wise or operator#Applies to (lists of) integers and (lists of) booleans'),
{tt_operatorXor}                (defaultId:'xor';           defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Bit-wise xor operator#Applies to (lists of) integers and (lists of) booleans'),
{tt_operatorLazyAnd}            (defaultId:'AND';           defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Lazy and operator#Applies to scalar booleans only'),
{tt_operatorLazyOr}             (defaultId:'OR';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Lazy or operator#Applies to scalar booleans only'),
{tt_operatorPlus}               (defaultId:'+';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Plus operator#Applies to (lists of) numbers and (lists of) strings'),
{tt_operatorMinus}              (defaultId:'-';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Minus operator#Applies to (lists of) numbers'),
{tt_operatorMult}               (defaultId:'*';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Multiplication operator#Applies to (lists of) numbers'),
{tt_operatorDivReal}            (defaultId:'/';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Division operator#Applies to (lists of) numbers'),
{tt_operatorDivInt}             (defaultId:'div';           defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Integer division operator#Applies to (lists of) integers'),
{tt_operatorMod}                (defaultId:'mod';           defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Integer modulo operator#Applies to (lists of) integers'),
{tt_operatorPot}                (defaultId:'^';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Potentiation operator#Applies to (lists of) numbers'),
{tt_unaryOpPlus}                (defaultId:'+';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Unary plus operator#Neutral'),
{tt_unaryOpMinus}               (defaultId:'-';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Unary minus operator#Negates the operand'),
{tt_operatorStrConcat}          (defaultId:'&';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'String concatenation operator#Applies to all literals'),
{tt_operatorOrElse}             (defaultId:'orElse';        defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Or-Else operator#Employed to provide a fallback to void literals'),
{tt_operatorConcat}             (defaultId:'|';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'List concatenation operator#Applies to all literals'),
{tt_operatorIn}                 (defaultId:'in';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'In operator#Applies to all literals on the left hand side and lists on the right hand side.#Returns true if the RHS contains the LHS'),
{tt_iifCheck}                   (defaultId:'?';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Inline-if-operator'),
{tt_iifElse}                    (defaultId:':';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Inline-if-operator'),
{tt_listToParameterList}        (defaultId:'@';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'List-to-parameter-list operator'),
{tt_declare}                    (defaultId:'->';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Declaration operator'),
{tt_assign}                     (defaultId:':=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Assignment operator'),
{tt_mutate}                     (defaultId:':=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Mutate-assign operator'),
{tt_assignNewBlockLocal}        (defaultId:':=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Assign new block local operator'),
{tt_assignExistingBlockLocal}   (defaultId:':=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Assign existing block local operator'),
{tt_cso_assignPlus}             (defaultId:'+=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'C-Style assign-increment operator'),
{tt_cso_assignMinus}            (defaultId:'-=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'C-Style assign-decrement operator'),
{tt_cso_assignMult}             (defaultId:'*=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'C-Style assign-multiply operator'),
{tt_cso_assignDiv}              (defaultId:'/=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'C-Style assign-divide operator'),
{tt_cso_assignStrConcat}        (defaultId:'&=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'C-Style assign-(string-)concatenate operator'),
{tt_cso_assignAppend}           (defaultId:'|=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'C-Style assign-(list-)concatenate operator'),
{tt_cso_mapPut}                 (defaultId:'<<';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Map put-assign operator'),
{tt_cso_mapDrop}                (defaultId:'>>';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Map drop-assign operator'),
{tt_typeCheckScalar}            (defaultId:':scalar';       defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check scalar;#Matches on all non-lists'),
{tt_typeCheckList}              (defaultId:':list';         defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check list;#Matches on all lists#Can be modified to match only lists of a given size'),
{tt_typeCheckSet}               (defaultId:':set';          defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check set;#Matches on all sets#Can be modified to match only sets of a given size'),
{tt_typeCheckCollection}        (defaultId:':collection';   defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check collection;#Matches on all lists and sets#Can be modified to match only colections of a given size'),
{tt_typeCheckBoolean}           (defaultId:':boolean';      defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check boolean;#Matches on scalar booleans'),
{tt_typeCheckBoolList}          (defaultId:':booleanList';  defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check boolean list;#Matches on lists of booleans and empty lists'),
{tt_typeCheckBoolSet}           (defaultId:':booleanSet';   defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check boolean set;#Matches on sets of booleans and empty sets'),
{tt_typeCheckBoolCollection}    (defaultId:':booleanCollection';defaultHtmlSpan:'builtin';reservedWordClass:rwc_typeCheck;        helpText:'Type check boolean collection;#Matches on collections of booleans and empty collections'),
{tt_typeCheckInt}               (defaultId:':int';          defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check integer;#Matches on scalar integers'),
{tt_typeCheckIntList}           (defaultId:':intList';      defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check integer list;#Matches on lists of integers and empty lists'),
{tt_typeCheckIntSet}            (defaultId:':intSet';       defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check integer set;#Matches on sets of integers and empty sets'),
{tt_typeCheckIntCollection}     (defaultId:':intCollection';defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check integer collection;#Matches on collections of integers and empty collections'),
{tt_typeCheckReal}              (defaultId:':real';         defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check real;#Matches on scalar reals'),
{tt_typeCheckRealList}          (defaultId:':realList';     defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check real list;#Matches on lists of reals and empty lists'),
{tt_typeCheckRealSet}           (defaultId:':realSet';      defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check real set;#Matches on sets of reals and empty sets'),
{tt_typeCheckRealCollection}    (defaultId:':realCollection';defaultHtmlSpan:'builtin';   reservedWordClass:rwc_typeCheck;        helpText:'Type check real collection;#Matches on collections of reals and empty collections'),
{tt_typeCheckString}            (defaultId:':string';       defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check string;#Matches on scalar strings'),
{tt_typeCheckStringList}        (defaultId:':stringList';   defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check string list;#Matches on lists of strings and empty lists'),
{tt_typeCheckStringSet}         (defaultId:':stringSet';    defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check string set;#Matches on sets of strings and empty sets'),
{tt_typeCheckStringCollection}  (defaultId:':stringCollection';defaultHtmlSpan:'builtin'; reservedWordClass:rwc_typeCheck;        helpText:'Type check string collection;#Matches on collections of strings and empty collections'),
{tt_typeCheckNumeric}           (defaultId:':numeric';      defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check numeric;#Matches on scalar integers and reals'),
{tt_typeCheckNumList}           (defaultId:':numericList';  defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check numeric list;#Matches on lists of integers and reals (mixing is allowed) and empty lists'),
{tt_typeCheckNumSet}            (defaultId:':numericSet';   defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check numeric set;#Matches on sets of integers and reals (mixing is allowed) and empty sets'),
{tt_typeCheckNumCollection}     (defaultId:':numericCollection';defaultHtmlSpan:'builtin';reservedWordClass:rwc_typeCheck;        helpText:'Type check numeric collection;#Matches on collections of integers and reals (mixing is allowed) and empty collections'),
{tt_typeCheckMap}               (defaultId:':map';          defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check map;#Matches on maps'),
{tt_typeCheckExpression}        (defaultId:':expression';   defaultHtmlSpan:'builtin';    reservedWordClass:rwc_typeCheck;        helpText:'Type check expression;#Matches on expressions#Can be modified to match only on expressions with a given arity'),
{tt_customTypeCheck}            (defaultId:'';              defaultHtmlSpan:'identifier'; reservedWordClass:rwc_not_reserved;     helpText:'Custom type check'),
{tt_semicolon}                  (defaultId:';';             defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'Semicolon#Ends a statement'),
{tt_optionalParameters}         (defaultId:'...';           defaultHtmlSpan:'identifier'; reservedWordClass:rwc_not_reserved;     helpText:'Remaining arguments#Allowes access to anonymous furhter parameters#Returns a list'),
{tt_modifier_private}           (defaultId:PRIVATE_TEXT;    defaultHtmlSpan:'modifier';   reservedWordClass:rwc_modifier;         helpText:'Modifier private#Limits visiblity of the declaration to the package it is declared in'),
{tt_modifier_memoized}          (defaultId:'memoized';      defaultHtmlSpan:'modifier';   reservedWordClass:rwc_modifier;         helpText:'Modifier memoized#Makes the rule memoized, caching previously computed results'),
{tt_modifier_mutable}           (defaultId:'mutable';       defaultHtmlSpan:'modifier';   reservedWordClass:rwc_modifier;         helpText:'Modifier mutable#Makes the rule mutable, de facto changing the rule to a variable'),
{tt_modifier_datastore}         (defaultId:'datastore';     defaultHtmlSpan:'modifier';   reservedWordClass:rwc_modifier;         helpText:'Modifier datastore#Makes the rule persistent in a separate file.#Persistent rules also are mutable'),
{tt_modifier_plain}             (defaultId:'plain';         defaultHtmlSpan:'modifier';   reservedWordClass:rwc_modifier;         helpText:'Modifier plain#Modifies a datastore to use plain text instead of default binary format'),
{tt_modifier_synchronized}      (defaultId:'synchronized';  defaultHtmlSpan:'modifier';   reservedWordClass:rwc_modifier;         helpText:'Modifier synchronized#Protects the rule from concurrent execution.'),
{tt_modifier_local}             (defaultId:'local';         defaultHtmlSpan:'modifier';   reservedWordClass:rwc_modifier;         helpText:'Modifier local#Used for declaring block-local variables'),
{tt_modifier_customType}        (defaultId:'type';          defaultHtmlSpan:'modifier';   reservedWordClass:rwc_modifier;         helpText:'Modifier type#Used for declaring custom type checks'),
{tt_EOL}                        (defaultId:'';              defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'End-Of-Input#Helper token; May also indicate a comment'),
{tt_docComment}                 (defaultId:'';              defaultHtmlSpan:'comment';    reservedWordClass:rwc_not_reserved;     helpText:'Documentation comment'),
{tt_attributeComment}           (defaultId:'';              defaultHtmlSpan:'comment';    reservedWordClass:rwc_not_reserved;     helpText:'Attribute comment'),
{tt_blank}                      (defaultId:'';              defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'Blank#Helper token; May indicate a comment or whitespace'));

  C_use='USE';
  C_include='INCLUDE';

  C_specialWordInfo:array[0..7] of record
    txt:string;
    reservedWordClass:T_reservedWordClass;
    helpText:string;
  end=((txt:C_use;             reservedWordClass:rwc_modifier      ; helpText:'Marker: USE#Denotes the use clause#Followed by package paths (as string) or package ids'),
       (txt:C_include;         reservedWordClass:rwc_modifier      ; helpText:'Marker: INCLUDE#Denotes the include clause#Followed by one package path (as string) or one package id'),
       (txt:LITERAL_TEXT_VOID; reservedWordClass:rwc_specialLiteral; helpText:'void literal#Denotes a literal "which is not there"#Intended use: list construction and blank branches of inline if clauses'),
       (txt:LITERAL_NAN_TEXT;  reservedWordClass:rwc_specialLiteral; helpText:'not-a-number real literal'),
       (txt:LITERAL_INF_TEXT;  reservedWordClass:rwc_specialLiteral; helpText:'infinity real literal'),
       (txt:'false';           reservedWordClass:rwc_specialLiteral; helpText:'false literal'),
       (txt:'true';            reservedWordClass:rwc_specialLiteral; helpText:'true literal'),
       (txt:'main';            reservedWordClass:rwc_not_reserved  ; helpText:'main rule#Called when the script is executed from the command line (or via "call main" in the GUI)'));

  C_ruleTypeString: array[tt_localUserRule..tt_customTypeRule] of string = (
    'user function (local)',
    'user function (imported)',
    'built in function',
    'put-cache rule',
    'custom type');

TYPE
  T_ruleType=(rt_normal,
              rt_memoized,
              rt_mutable,
              rt_datastore,
              rt_synchronized,
              rt_customTypeCheck);
CONST C_mutableRuleTypes:           set of T_ruleType=[rt_mutable,rt_datastore];
      C_ruleTypesWithOnlyOneSubrule:set of T_ruleType=[rt_mutable,rt_datastore,rt_customTypeCheck];
      C_ruleTypeText:array[T_ruleType] of string=(
      '',
      'memoized ',
      'mutable ',
      'datastore ',
      'synchronized ',
      'type ');
      C_validModifierCombinations:array[0..14] of record
        modifiers:T_modifierSet;
        ruleType:T_ruleType;
      end=((modifiers:[];                                             ruleType:rt_normal),
           (modifiers:[                         tt_modifier_private]; ruleType:rt_normal),
           (modifiers:[tt_modifier_memoized];                         ruleType:rt_memoized),
           (modifiers:[tt_modifier_memoized    ,tt_modifier_private]; ruleType:rt_memoized),
           (modifiers:[tt_modifier_mutable];                          ruleType:rt_mutable),
           (modifiers:[tt_modifier_mutable     ,tt_modifier_private]; ruleType:rt_mutable),
           (modifiers:[tt_modifier_datastore];                        ruleType:rt_datastore),
           (modifiers:[tt_modifier_datastore   ,tt_modifier_private]; ruleType:rt_datastore),
           (modifiers:[tt_modifier_plain,tt_modifier_datastore];                        ruleType:rt_datastore),
           (modifiers:[tt_modifier_plain,tt_modifier_datastore   ,tt_modifier_private]; ruleType:rt_datastore),
           (modifiers:[tt_modifier_memoized];                         ruleType:rt_memoized),
           (modifiers:[tt_modifier_memoized    ,tt_modifier_private]; ruleType:rt_memoized),
           (modifiers:[tt_modifier_synchronized];                     ruleType:rt_synchronized),
           (modifiers:[tt_modifier_synchronized,tt_modifier_private]; ruleType:rt_synchronized),
           (modifiers:[tt_modifier_customType];                       ruleType:rt_customTypeCheck));

TYPE
  T_messageClass=(mc_echo   ,
                  mc_print  ,
                  mc_timing ,
                  mc_note   ,
                  mc_warning,
                  mc_error  );
CONST
  C_messageClassMeta:array[T_messageClass] of record guiMarker:string; htmlSpan:string; end=
    {mc_echo   }((guiMarker: ECHO_MARKER   ; htmlSpan:''),
    {mc_print  } (guiMarker: ''            ; htmlSpan:''),
    {mc_timing } (guiMarker: TIMING_MARKER ; htmlSpan:''),
    {mc_note   } (guiMarker: NOTE_MARKER   ; htmlSpan:''),
    {mc_warning} (guiMarker: WARNING_MARKER; htmlSpan:''),
    {mc_error  } (guiMarker: ERROR_MARKER  ; htmlSpan:'error'));

TYPE
  T_messageType = (
    mt_clearConsole,
    mt_printline,
    mt_printdirect,
    mt_echo_input,
    mt_echo_declaration,
    mt_echo_output,
    mt_echo_continued,
    mt_el1_note,
    mt_el1_userNote,
    mt_el2_warning,
    mt_el2_userWarning,
    mt_el3_evalError,
    mt_el3_noMatchingMain,
    mt_el3_stackTrace,
    mt_el3_userDefined,
    mt_el4_systemError,
    mt_el4_haltMessageReceived,
    mt_el4_haltMessageQuiet,
    mt_endOfEvaluation,
    mt_timing_info
    {$ifdef fullVersion},
    mt_plotFileCreated,
    mt_plotCreatedWithDeferredDisplay,
    mt_plotCreatedWithInstantDisplay,
    mt_plotSettingsChanged,
    mt_displayTable,
    mt_gui_editScriptSucceeded,
    mt_gui_editScriptFailed,
    mt_gui_breakpointEncountered,
    mt_guiPseudoPackageFound
    {$endif});

  T_messageTypeSet=set of T_messageType;

CONST
  {$ifdef fullVersion}
  C_plotMessages:T_messageTypeSet=[mt_plotFileCreated..mt_plotSettingsChanged];
  C_guiOnlyMessages:T_messageTypeSet=[mt_plotFileCreated..mt_gui_breakpointEncountered];
  {$endif}
  C_messageTypeMeta:array[T_messageType] of record
    level:shortint;
    mClass:T_messageClass;
    prefix:string;
    includeLocation,ignoredBySandbox,triggersGuiStartup:boolean;
    systemErrorLevel:byte;
  end = (
{mt_clearConsole           }             (level:-2; mClass:mc_print;   prefix: ''                     ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_printline              }             (level:-2; mClass:mc_print;   prefix: ''                     ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_print                  }             (level:-2; mClass:mc_print;   prefix: ''                     ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_echo_input             }             (level:-1; mClass:mc_echo;    prefix: ' in>'                 ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_echo_declaration       }             (level:-1; mClass:mc_echo;    prefix: ' in>'                 ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_echo_output            }             (level:-1; mClass:mc_echo;    prefix: 'out>'                 ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_echo_continued         }             (level:-1; mClass:mc_echo;    prefix: '...>'                 ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_el1_note               }             (level: 1; mClass:mc_note;    prefix: 'Note '                ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_el1_userNote           }             (level: 1; mClass:mc_note;    prefix: 'Note '                ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_el2_warning            }             (level: 2; mClass:mc_warning; prefix: 'Warning '             ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_el2_userWarning        }             (level: 2; mClass:mc_warning; prefix: 'Warning '             ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_el3_evalError          }             (level: 3; mClass:mc_error;   prefix: 'Error '               ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:3),
{mt_el3_noMatchingMain     }             (level: 3; mClass:mc_error;   prefix: 'Error '               ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:1),
{mt_el3_stackTrace         }             (level: 3; mClass:mc_error;   prefix: 'Stack trace '         ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_el3_userDefined        }             (level: 3; mClass:mc_error;   prefix: 'User-Error '          ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:2),
{mt_el4_systemError        }             (level: 4; mClass:mc_error;   prefix: 'Sys. Error '          ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:5),
{mt_el4_haltMessageReceived}             (level: 4; mClass:mc_error;   prefix: 'Evaluation haltet'    ; includeLocation:  true; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_el4_haltMessageQuiet   }             (level: 4; mClass:mc_error;   prefix: ''                     ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_endOfEvaluation        }             (level:-1; mClass:mc_note;    prefix: ''                     ; includeLocation: false; ignoredBySandbox:  true; triggersGuiStartup:false; systemErrorLevel:0),
{mt_timing_info            }             (level:-1; mClass:mc_timing;  prefix: ''                     ; includeLocation: false; ignoredBySandbox:  true; triggersGuiStartup:false; systemErrorLevel:0)
{$ifdef fullVersion},
{mt_plotFileCreated                    } (level:-1; mClass:mc_note;    prefix: 'Image:'               ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup:false; systemErrorLevel:0),
{mt_plotCreatedWithDeferredDisplay     } (level:-1; mClass:mc_note;    prefix: 'Deferred plot request'; includeLocation: false; ignoredBySandbox:  true; triggersGuiStartup:false; systemErrorLevel:0),
{mt_plotCreatedWithInstantDisplay      } (level:-1; mClass:mc_note;    prefix: 'Instant plot request' ; includeLocation: false; ignoredBySandbox:  true; triggersGuiStartup: true; systemErrorLevel:0),
{mt_plotSettingsChanged                } (level:-1; mClass:mc_note;    prefix: 'Plot settings changed'; includeLocation: false; ignoredBySandbox:  true; triggersGuiStartup:false; systemErrorLevel:0),
{mt_displayTable                       } (level:-1; mClass:mc_note;    prefix: ''                     ; includeLocation: false; ignoredBySandbox:  true; triggersGuiStartup: true; systemErrorLevel:0),
{mt_gui_editScriptSucceeded            } (level:-1; mClass:mc_note;    prefix: ''                     ; includeLocation: false; ignoredBySandbox:  true; triggersGuiStartup:false; systemErrorLevel:0),
{mt_gui_editScriptFailed               } (level:-1; mClass:mc_note;    prefix: ''                     ; includeLocation: false; ignoredBySandbox:  true; triggersGuiStartup:false; systemErrorLevel:0),
{mt_gui_breakpointEncountered          } (level:-1; mClass:mc_note;    prefix: ''                     ; includeLocation: false; ignoredBySandbox:  true; triggersGuiStartup:false; systemErrorLevel:0),
{mt_guiPseudoPackageFound              } (level:-1; mClass:mc_note;    prefix: ''                     ; includeLocation: false; ignoredBySandbox: false; triggersGuiStartup: true; systemErrorLevel:0)
{$endif});
  C_errorMessageTypes:array[1..4] of T_messageTypeSet=(
    [mt_el1_note,mt_el1_userNote],
    [mt_el2_warning,mt_el2_userWarning],
    [mt_el3_evalError,mt_el3_noMatchingMain,mt_el3_stackTrace,mt_el3_userDefined],
    [mt_el4_systemError,mt_el4_haltMessageReceived,mt_el4_haltMessageQuiet]);

FUNCTION isQualified(CONST s:string):boolean;
FUNCTION configDir:string;

OPERATOR :=(CONST x:T_messageTypeSet):qword;
OPERATOR :=(x:qword):T_messageTypeSet;
IMPLEMENTATION

FUNCTION isQualified(CONST s:string):boolean;
  begin
    result:=pos(ID_QUALIFY_CHARACTER,s)>0;
  end;

FUNCTION getAppName: string;
  begin
    result:=APP_NAME;
  end;

FUNCTION configDir:string;
  begin
    OnGetApplicationName:=@getAppName;
    {$ifdef Windows}
    result:=GetAppConfigDir(true);
    {$else}
    result:=GetAppConfigDir(false);
    {$endif}
  end;

OPERATOR :=(CONST x:T_messageTypeSet):qword;
  VAR mt:T_messageType;
      mask:bitpacked array [0..sizeOf(qword)*8-1] of boolean;
      i:longint;
  begin
    for i:=0 to length(mask)-1 do mask[i]:=false;
    i:=length(mask)-1;
    for mt:=low(T_messageType) to high(T_messageType) do if i>0 then begin
      mask[i]:=mt in x;
      dec(i);
    end;
    result:=0;
    move(mask,result,sizeOf(qword));
  end;

OPERATOR :=(x:qword):T_messageTypeSet;
  VAR mt:T_messageType;
      mask:bitpacked array [0..sizeOf(qword)*8-1] of boolean;
      i:longint;
  begin
    initialize(mask);
    move(x,mask,sizeOf(qword));
    i:=length(mask)-1;
    result:=[];
    for mt:=low(T_messageType) to high(T_messageType) do begin
      if (i>0) and mask[i] then include(result,mt);
      dec(i);
    end;
  end;

INITIALIZATION
  OnGetApplicationName:=@getAppName;
  ForceDirectories(configDir);
end.
