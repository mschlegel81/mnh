UNIT mnh_constants;
INTERFACE
USES sysutils;
CONST
  STACK_DEPTH_LIMIT={$ifdef Windows}63000{$else}{$ifdef debugMode}2000{$else}4100{$endif}{$endif};
  {$i code_hash.inc}
  {$i build_number.inc}
  FLAVOUR_STRING={$ifdef fullVersion}'F'{$else}'L'{$endif}+
                 {$ifdef profilingFlavour}'P'{$else}
                   {$ifdef debugMode}'D'{$else}'O'{$endif}
                 {$endif}+{$I %FPCTargetOS%};
  LOGO:array[0..20] of string=(
  ' ___      ___ ___   ___ ___   ___',
  '|   \    /   |   \ |   |   | |   |  ______',
  '|    \  /    |    \|   |   |_|   | |   ___|',
  '|     \/     |     \   |         | |  |__',
  '|   \    /   |   \     |    _    | |___  \',
  '|   |\  /|   |   |\    |   | |   |  ___)  |',
  '|___| \/ |___|___| \___|___| |___| |_____/',
  '             (c) Martin Schlegel, 2010-2018',
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
  BLOCK_COMMENT_DELIMITER       ='#';
  DOC_COMMENT_INFIX             ='*';
  ATTRIBUTE_COMMENT_INFIX       ='@';
  SPECIAL_COMMENT_BLOB_BEGIN_INFIX='!';
  {$ifdef fullVersion}
  FORCE_GUI_PSEUDO_PACKAGE      ='GUI';
  SUPPRESS_UNUSED_WARNING_ATTRIBUTE='SuppressUnusedWarning';
  SUPPRESS_UNUSED_PARAMETER_WARNING_ATTRIBUTE='SuppressUnusedParameterWarning';
  {$endif}
  ALLOW_SIDE_EFFECT_ATTRIBUTE='AllowOnly';
  ALLOW_NO_SIDE_EFFECTS_ATTRIBUTE_VALUE='pure';
  EXECUTE_AFTER_ATTRIBUTE='after';

TYPE
  T_reservedWordClass=(rwc_not_reserved,
                       rwc_specialLiteral,
                       rwc_specialConstruct,
                       rwc_operator,
                       rwc_type,
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
    tt_each, tt_parallelEach, tt_agg, tt_while, tt_beginBlock, tt_beginRule, tt_beginExpression, tt_endBlock, tt_endRule, tt_endExpression, tt_save, tt_return, tt_pseudoFuncPointer,
    //lists and list constructors
    tt_braceOpen, tt_braceClose, tt_parList_constructor, tt_parList,
    tt_listBraceOpen, tt_listBraceClose, tt_list_constructor, tt_list_constructor_ranging,
    tt_expBraceOpen, tt_expBraceClose,
    //separators
    tt_separatorComma, tt_separatorCnt,
    //comparators
    tt_comparatorEq, tt_comparatorNeq, tt_comparatorLeq,
    tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq,
    tt_operatorIn,
    //logical operators
    tt_operatorAnd, tt_operatorOr, tt_operatorXor,
    tt_operatorLazyAnd, tt_operatorLazyOr,
    //arthmetical operators
    tt_operatorPlus, tt_operatorMinus, tt_operatorMult,
    tt_operatorDivReal, tt_operatorDivInt, tt_operatorMod, tt_operatorPot,
    //special: string concatenation
    tt_operatorStrConcat, tt_operatorOrElse,
    //list operators:
    tt_operatorConcat, tt_operatorConcatAlt,
    //inline if: (<condition>?<then>:<else>)
    tt_iifCheck, tt_iifElse,
    tt_listToParameterList,
    //partially evaluated operators
    tt_unaryOpPlus, tt_unaryOpMinus,
    //assignment operators:
    tt_declare, tt_assign, tt_mutate, tt_assignNewBlockLocal, tt_assignExistingBlockLocal,
                            tt_mut_nested_assign,
    tt_mut_assignPlus,      tt_mut_nestedPlus,      //+=
    tt_mut_assignMinus,     tt_mut_nestedMinus,     //-=
    tt_mut_assignMult,      tt_mut_nestedMult,      //*=
    tt_mut_assignDiv,       tt_mut_nestedDiv,       ///=
    tt_mut_assignStrConcat, tt_mut_nestedStrConcat, //&=
    tt_mut_assignAppend,    tt_mut_nestedAppend,    //|=
    tt_mut_assignAppendAlt, tt_mut_nestedAppendAlt, //||=
    tt_mut_assignDrop,      tt_mut_nestedDrop,      //>>
    //type checks:
    tt_type,
    tt_typeCheck,
    tt_customTypeCheck,
    tt_semicolon,
    tt_optionalParameters,
    //modifiers:
    tt_modifier,
    //special: [E]nd [O]f [L]ine
    tt_EOL,
    tt_docComment,
    tt_attributeComment,
    tt_use,tt_include,
    tt_blank);

  T_tokenTypeSet  =set of T_tokenType;
  T_tokenTypeInfo=record
    tokenType:T_tokenType;
    reservedWordClass:T_reservedWordClass;
    info:ansistring;
  end;

CONST
  C_forbiddenTokenTypes: T_tokenTypeSet=[tt_rulePutCacheValue, tt_agg, tt_parList_constructor, tt_parList,
    tt_declare,
    //type checks:
    tt_typeCheck..tt_customTypeCheck,
    //special: [E]nd [O]f [L]ine
    tt_EOL,
    tt_blank];

  C_operatorsForAggregators: T_tokenTypeSet=[tt_operatorAnd..tt_operatorPot,tt_operatorStrConcat,tt_operatorOrElse,tt_operatorConcat,tt_operatorConcatAlt];
  C_operators: T_tokenTypeSet=[tt_comparatorEq..tt_operatorConcatAlt];
  C_comparators: T_tokenTypeSet=[tt_comparatorEq..tt_operatorIn];
  C_openingBrackets:T_tokenTypeSet=[tt_beginBlock,tt_beginRule,tt_beginExpression,tt_each,tt_parallelEach,tt_agg,tt_braceOpen,tt_parList_constructor,tt_listBraceOpen,tt_list_constructor,tt_expBraceOpen,tt_iifCheck];
  C_closingBrackets:T_tokenTypeSet=[tt_endBlock,tt_endRule,tt_endExpression,tt_braceClose,tt_listBraceClose,tt_expBraceClose,tt_iifElse];
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
    {tt_return}             tt_EOL,
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
    {tt_expBraceClose..tt_operatorIn} tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,tt_EOL,
    {tt_iifCheck}           tt_iifElse);

  C_opPrecedence: array[tt_comparatorEq..tt_operatorConcatAlt] of byte =
   (6, 6, 6, 6, 6, 6, 6, //comparators
    7,                   //in
    8, 9, 9,             //logical operators
    8, 9,                //lazy logical operators
    4, 4, 3, 3, 3, 3, 2, //arthmetical operators
    5, 9,                //special: string concatenation
   10,11);            //list operators

  C_compatibleEnd:array[tt_beginBlock..tt_beginExpression] of T_tokenType=(tt_endBlock,tt_endRule,tt_endExpression);
  C_tokenInfo:array[T_tokenType] of record
                                 defaultId:string;          defaultHtmlSpan:string; reservedWordClass:T_reservedWordClass;  helpText:string; end=(
{tt_literal}                    (defaultId:'';              defaultHtmlSpan:'literal';    reservedWordClass:rwc_not_reserved;     helpText:'A literal'),
{tt_aggregatorExpressionLiteral}(defaultId:'';              defaultHtmlSpan:'literal';    reservedWordClass:rwc_not_reserved;     helpText:'An aggregator expression literal'),
{tt_identifier}                 (defaultId:'';              defaultHtmlSpan:'identifier'; reservedWordClass:rwc_not_reserved;     helpText:'An identifier (unresolved)'),
{tt_parameterIdentifier}        (defaultId:'';              defaultHtmlSpan:'identifier'; reservedWordClass:rwc_not_reserved;     helpText:'A parameter identifier'),
{tt_localUserRule}              (defaultId:'';              defaultHtmlSpan:'identifier'; reservedWordClass:rwc_not_reserved;     helpText:'A local user rule'),
{tt_importedUserRule}           (defaultId:'';              defaultHtmlSpan:'identifier'; reservedWordClass:rwc_not_reserved;     helpText:'An imported user rule'),
{tt_intrinsicRule}              (defaultId:'';              defaultHtmlSpan:'builtin';    reservedWordClass:rwc_not_reserved;     helpText:'A built in rule'),
{tt_rulePutCacheValue}          (defaultId:'';              defaultHtmlSpan:'builtin';    reservedWordClass:rwc_not_reserved;     helpText:'A put-cache-value call'),
{tt_customTypeRule}             (defaultId:'';              defaultHtmlSpan:'identifier'; reservedWordClass:rwc_not_reserved;     helpText:'A custom type rule'),
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
{tt_return}                     (defaultId:'return';        defaultHtmlSpan:'builtin';    reservedWordClass:rwc_specialConstruct; helpText:'Special construct: return#Returns from the current function with the given result value'),
{tt_pseudoFuncPointer}          (defaultId:'::';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_specialConstruct; helpText:'Special construct: ::# Returns reference to a function#::f -> {f@$params}'),
{tt_braceOpen}                  (defaultId:'(';             defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'Opening round bracket#Used as in default mathematical syntax.'),
{tt_braceClose}                 (defaultId:')';             defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'Closing round bracket#Used as in default mathematical syntax.'),
{tt_parList_constructor}        (defaultId:'';              defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'A parameter list constructor'),
{tt_parList}                    (defaultId:'';              defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'A parameter list'),
{tt_listBraceOpen}              (defaultId:'[';             defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'Square opening bracket#Used for list construction and list access'),
{tt_listBraceClose}             (defaultId:']';             defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'Square closing bracket#Used for list construction and list access'),
{tt_list_constructor}           (defaultId:'';              defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'A list constructor'),
{tt_list_Constructor_ranging}   (defaultId:'';              defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'A list constructor'),
{tt_expBraceOpen}               (defaultId:'{';             defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'Curly opening bracket#Delimits an expression'),
{tt_expBraceClose}              (defaultId:'}';             defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'Curly closing bracket#Delimits an expression'),
{tt_separatorComma}             (defaultId:',';             defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'Separator comma'),
{tt_separatorCnt}               (defaultId:'..';            defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'Separator ..#Used for constructing ranges and only allowed in that context'),
{tt_comparatorEq}               (defaultId:'=';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Equals operator#Returns true if the scalar comparands are type-compatible#and equal#For list operands a list of booleans is returned'),
{tt_comparatorNeq}              (defaultId:'<>';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Not-equals operator#Returns true if the scalar comparands are type-compatible#and not equal#For list operands a list of booleans is returned'),
{tt_comparatorLeq}              (defaultId:'<=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Lesser-or-equals operator#Returns true if the scalar comparands are type-compatible#and the left hand side is lesser or equal to the right hand side#For list operands a list of booleans is returned'),
{tt_comparatorGeq}              (defaultId:'>=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Greater-or-equals operator#Returns true if the scalar comparands are type-compatible#and the left hand side is greater or equal to the right hand side#For list operands a list of booleans is returned'),
{tt_comparatorLss}              (defaultId:'<';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Lesser operator#Returns true if the scalar comparands are type-compatible#and the left hand side is lesser than the right hand side#For list operands a list of booleans is returned'),
{tt_comparatorGrt}              (defaultId:'>';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Greater operator#Returns true if the scalar comparands are type-compatible#and the left hand side is greater than the right hand side#For list operands a list of booleans is returned'),
{tt_comparatorListEq}           (defaultId:'==';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Strict-equals operator#Returns true if the comparands are strictly equal.#Always returns a scalar boolean'),
{tt_operatorIn}                 (defaultId:'in';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'In operator#Applies to all literals on the left hand side and lists on the right hand side.#Returns true if the RHS contains the LHS'),
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
{tt_operatorStrConcat}          (defaultId:'&';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'String concatenation operator#Applies to all literals'),
{tt_operatorOrElse}             (defaultId:'orElse';        defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Or-Else operator#Employed to provide a fallback to void literals'),
{tt_operatorConcat}             (defaultId:'|';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'List concatenation operator#Applies to all literals'),
{tt_operatorConcatAlt}          (defaultId:'||';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'List concatenation operator#Applies to all literals'),
{tt_iifCheck}                   (defaultId:'?';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Inline-if-operator'),
{tt_iifElse}                    (defaultId:':';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Inline-if-operator'),
{tt_listToParameterList}        (defaultId:'@';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'List-to-parameter-list operator'),
{tt_unaryOpPlus}                (defaultId:'+';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Unary plus operator#Neutral'),
{tt_unaryOpMinus}               (defaultId:'-';             defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Unary minus operator#Negates the operand'),
{tt_declare}                    (defaultId:'->';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Declaration operator'),
{tt_assign}                     (defaultId:':=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Assignment operator'),
{tt_mutate}                     (defaultId:':=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Mutate-assign operator'),
{tt_assignNewBlockLocal}        (defaultId:':=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Assign new block local operator'),
{tt_assignExistingBlockLocal}   (defaultId:':=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Assign existing block local operator'),
{tt_mut_nested_assign}          (defaultId:'';              defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:''),
{tt_mut_assignPlus}             (defaultId:'+=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Incrementation operator'),
{tt_mut_nestedPlus}             (defaultId:'';              defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:''),
{tt_mut_assignMinus}            (defaultId:'-=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Decrementation operator'),
{tt_mut_nestedMinus}            (defaultId:'';              defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:''),
{tt_mut_assignMult}             (defaultId:'*=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Multipliy/assign operator'),
{tt_mut_nestedMult}             (defaultId:'';              defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:''),
{tt_mut_assignDiv}              (defaultId:'/=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Divide/assign operator'),
{tt_mut_nestedDiv}              (defaultId:'';              defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:''),
{tt_mut_assignStrConcat}        (defaultId:'&=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'String-concatenate/assign operator'),
{tt_mut_nestedStrConcat}        (defaultId:'';              defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:''),
{tt_mut_assignAppend}           (defaultId:'|=';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'List-concatenate/assign operator'),
{tt_mut_nestedAppend}           (defaultId:'';              defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:''),
{tt_mut_assignAppendAlt}        (defaultId:'||=';           defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Alt-List-concatenate/assign operator'),
{tt_mut_nestedAppendAlt}        (defaultId:'';              defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:''),
{tt_mut_assignDrop}             (defaultId:'>>';            defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:'Drop operator'),
{tt_mut_nestedDrop}             (defaultId:'';              defaultHtmlSpan:'operator';   reservedWordClass:rwc_operator;         helpText:''),
{tt_type}                       (defaultId:'';              defaultHtmlSpan:'builtin';    reservedWordClass:rwc_not_reserved;     helpText:'Type'),
{tt_typeCheck}                  (defaultId:'';              defaultHtmlSpan:'builtin';    reservedWordClass:rwc_not_reserved;     helpText:'Type check'),
{tt_customTypeCheck}            (defaultId:'';              defaultHtmlSpan:'identifier'; reservedWordClass:rwc_not_reserved;     helpText:'Custom type check'),
{tt_semicolon}                  (defaultId:';';             defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'Semicolon#Ends a statement'),
{tt_optionalParameters}         (defaultId:'...';           defaultHtmlSpan:'identifier'; reservedWordClass:rwc_not_reserved;     helpText:'Remaining arguments#Allowes access to anonymous furhter parameters#Returns a list'),
{tt_modifier}                   (defaultId:'';              defaultHtmlSpan:'modifier';   reservedWordClass:rwc_modifier;         helpText:'Modifier'),
{tt_EOL}                        (defaultId:'';              defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'End-Of-Input#Helper token; May also indicate a comment'),
{tt_docComment}                 (defaultId:'';              defaultHtmlSpan:'comment';    reservedWordClass:rwc_not_reserved;     helpText:'Documentation comment'),
{tt_attributeComment}           (defaultId:'';              defaultHtmlSpan:'comment';    reservedWordClass:rwc_not_reserved;     helpText:'Attribute comment'),
{tt_use}                        (defaultId:'USE';           defaultHtmlSpan:'modifier';   reservedWordClass:rwc_modifier;         helpText:'Marker: USE#Denotes the use clause#Followed by package paths (as string) or package ids'),
{tt_include}                    (defaultId:'INCLUDE';       defaultHtmlSpan:'modifier';   reservedWordClass:rwc_modifier;         helpText:'Marker: INCLUDE#Denotes the include clause#Followed by one package path (as string) or one package id'),
{tt_blank}                      (defaultId:'';              defaultHtmlSpan:'';           reservedWordClass:rwc_not_reserved;     helpText:'Blank#Helper token; May indicate a comment or whitespace'));
TYPE
  T_literalType = (
    lt_error,
             lt_boolean,     lt_int,     lt_real,                 lt_string,                   lt_expression,
    lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList, lt_emptyList,
    lt_set,  lt_booleanSet,  lt_intSet,  lt_realSet,  lt_numSet,  lt_stringSet,  lt_emptySet,
    lt_map,                                                                      lt_emptyMap,
    lt_void);
  T_literalTypeSet=set of T_literalType;

CONST
  C_typeInfo:array[T_literalType] of record
    name:string;
    containedIn,
    comparableTo:T_literalTypeSet
  end=(
  {lt_error      }(name:'Error'          ; containedIn:[]                                                          ;comparableTo:[]),
  {lt_boolean    }(name:'Boolean'        ; containedIn:[lt_list,lt_booleanList,lt_set,lt_booleanSet]               ;comparableTo:[lt_expression,lt_boolean,                         lt_list,lt_booleanList,                                                lt_emptyList,lt_set,lt_booleanSet,                                            lt_emptySet]),
  {lt_int        }(name:'Int'            ; containedIn:[lt_list,lt_intList,lt_numList,lt_set,lt_intSet,lt_numSet]  ;comparableTo:[lt_expression,           lt_int,lt_real,          lt_list,               lt_intList,lt_realList,lt_numList,              lt_emptyList,lt_set,              lt_intSet,lt_realSet,lt_numSet,             lt_emptySet]),
  {lt_real       }(name:'Real'           ; containedIn:[lt_list,lt_realList,lt_numList,lt_set,lt_realSet,lt_numSet];comparableTo:[lt_expression,           lt_int,lt_real,          lt_list,               lt_intList,lt_realList,lt_numList,              lt_emptyList,lt_set,              lt_intSet,lt_realSet,lt_numSet,             lt_emptySet]),
  {lt_string     }(name:'String'         ; containedIn:[lt_list,lt_stringList,lt_set,lt_stringSet]                 ;comparableTo:[lt_expression,                          lt_string,lt_list,                                                 lt_stringList,lt_emptyList,lt_set,                                             lt_stringSet,lt_emptySet]),
  {lt_expression }(name:'Expression'     ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[lt_boolean..lt_emptySet]),
  {lt_list       }(name:'List'           ; containedIn:[lt_list,lt_set,lt_map]                                     ;comparableTo:[lt_expression,lt_boolean,lt_int,lt_real,lt_string,lt_list,lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList]),
  {lt_booleanList}(name:'BooleanList'    ; containedIn:[lt_list,lt_set,lt_map]                                     ;comparableTo:[lt_expression,lt_boolean,                         lt_list,lt_booleanList                                                ]),
  {lt_intList    }(name:'IntList'        ; containedIn:[lt_list,lt_set,lt_map]                                     ;comparableTo:[lt_expression,           lt_int,lt_real,          lt_list,               lt_intList,lt_realList,lt_numList              ]),
  {lt_realList   }(name:'RealList'       ; containedIn:[lt_list,lt_set,lt_map]                                     ;comparableTo:[lt_expression,           lt_int,lt_real,          lt_list,               lt_intList,lt_realList,lt_numList              ]),
  {lt_numList    }(name:'NumericList'    ; containedIn:[lt_list,lt_set,lt_map]                                     ;comparableTo:[lt_expression,           lt_int,lt_real,          lt_list,               lt_intList,lt_realList,lt_numList              ]),
  {lt_stringList }(name:'StringList'     ; containedIn:[lt_list,lt_set,lt_map]                                     ;comparableTo:[lt_expression,                          lt_string,lt_list,                                                 lt_stringList]),
  {lt_emptyList  }(name:'EmptyList'      ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[lt_expression,lt_boolean,lt_int,lt_real,lt_string,                                                                       lt_emptyList]),
  {lt_set        }(name:'Set'            ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[lt_expression,lt_boolean,lt_int,lt_real,lt_string,lt_set ,lt_booleanSet ,lt_intSet ,lt_realSet ,lt_numSet ,lt_stringSet ,lt_emptySet]),
  {lt_booleanSet }(name:'BooleanSet'     ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[lt_expression,lt_boolean,                         lt_set ,lt_booleanSet ,                                                lt_emptySet]),
  {lt_intSet     }(name:'IntSet'         ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[lt_expression,           lt_int,lt_real,          lt_set ,               lt_intSet ,lt_realSet ,lt_numSet ,              lt_emptySet]),
  {lt_realSet    }(name:'RealSet'        ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[lt_expression,           lt_int,lt_real,          lt_set ,               lt_intSet ,lt_realSet ,lt_numSet ,              lt_emptySet]),
  {lt_numSet     }(name:'NumSet'         ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[lt_expression,           lt_int,lt_real,          lt_set ,               lt_intSet ,lt_realSet ,lt_numSet ,              lt_emptySet]),
  {lt_stringSet  }(name:'StringSet'      ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[lt_expression,                          lt_string,lt_set ,                                                 lt_stringSet ,lt_emptySet]),
  {lt_emptySet   }(name:'EmptySet'       ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[lt_expression,lt_boolean,lt_int,lt_real,lt_string,lt_set ,lt_booleanSet ,lt_intSet ,lt_realSet ,lt_numSet ,lt_stringSet ,lt_emptySet]),
  {lt_map        }(name:'Map'            ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[]),
  {lt_emptyMap   }(name:'EmptyMap'       ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[]),
  {lt_void       }(name:LITERAL_TEXT_VOID; containedIn:[]                                                          ;comparableTo:[]));

  C_compoundTypes     : T_literalTypeSet=[lt_list..lt_emptyMap];
  C_listTypes         : T_literalTypeSet=[lt_list..lt_emptyList];
  C_setTypes          : T_literalTypeSet=[lt_set..lt_emptySet];
  C_mapTypes          : T_literalTypeSet=[lt_map..lt_emptyMap];
  C_emptyCompoundTypes: T_literalTypeSet=[lt_emptyMap,lt_emptySet,lt_emptyList];
  C_scalarTypes       : T_literalTypeSet=[lt_boolean..lt_expression,lt_void];

TYPE
  T_typeCheck=(tc_typeCheckScalar,  tc_typeCheckList,       tc_typeCheckSet,       tc_typeCheckCollection,
               tc_typeCheckBoolean, tc_typeCheckBoolList,   tc_typeCheckBoolSet,   tc_typeCheckBoolCollection,
               tc_typeCheckInt,     tc_typeCheckIntList,    tc_typeCheckIntSet,    tc_typeCheckIntCollection,
               tc_typeCheckReal,    tc_typeCheckRealList,   tc_typeCheckRealSet,   tc_typeCheckRealCollection,
               tc_typeCheckString,  tc_typeCheckStringList, tc_typeCheckStringSet, tc_typeCheckStringCollection,
               tc_typeCheckNumeric, tc_typeCheckNumList,    tc_typeCheckNumSet,    tc_typeCheckNumCollection,
               tc_typeCheckMap,
               tc_typeCheckExpression,
               tc_typeCheckStatelessExpression,
               tc_typeCheckStatefulExpression,
               tc_typeCheckIteratableExpression,
               tc_typeCheckIteratable);

CONST
  C_typeCheckInfo:array[T_typeCheck] of record
    name:string;
    helpText:string;
    modifiable:boolean;
    matching:T_literalTypeSet;
  end=(
  {tc_typeCheckScalar}            (name:'Scalar';            helpText:'Matches all scalar types except expressions';
                                   modifiable:false; matching:[lt_boolean, lt_int, lt_real, lt_string]),
  {tc_typeCheckList}              (name:'List';              helpText:'Matches all list types#Can be modified to only match lists of a given size';
                                   modifiable:true;  matching:[lt_list..lt_emptyList]),
  {tc_typeCheckSet}               (name:'Set';               helpText:'Matches all set types#Can be modified to only match sets of a given size';
                                   modifiable:true;  matching:[lt_set..lt_emptySet]),
  {tc_typeCheckCollection}        (name:'Collection';        helpText:'Matches all list and set types#Can be modified to only match collections of a given size';
                                   modifiable:true;  matching:[lt_list..lt_emptyList,lt_set..lt_emptySet]),
  {tc_typeCheckBoolean}           (name:'Boolean';           helpText:'Matches scalar booleans';
                                   modifiable:false; matching:[lt_boolean]),
  {tc_typeCheckBoolList}          (name:'BooleanList';       helpText:'Matches lists, empty or containing only booleans#Can be modified to only match lists of a given size';
                                   modifiable:true;  matching:[lt_booleanList, lt_emptyList]),
  {tc_typeCheckBoolSet}           (name:'BooleanSet';        helpText:'Matches sets, empty or containing only booleans#Can be modified to only match sets of a given size';
                                   modifiable:true;  matching:[lt_booleanSet, lt_emptySet]),
  {tc_typeCheckBoolCollection}    (name:'BooleanCollection'; helpText:'Matches lists and sets, empty or containing only booleans#Can be modified to only match collections of a given size';
                                   modifiable:true;  matching:[lt_booleanList, lt_emptyList, lt_booleanSet, lt_emptySet]),
  {tc_typeCheckInt}               (name:'Int';               helpText:'Matches scalar integers';
                                   modifiable:false; matching:[lt_int]),
  {tc_typeCheckIntList}           (name:'IntList';           helpText:'Matches lists, empty or containing only integers#Can be modified to only match lists of a given size';
                                   modifiable:true;  matching:[lt_intList, lt_emptyList]),
  {tc_typeCheckIntSet}            (name:'IntSet';            helpText:'Matches sets, empty or containing only integers#Can be modified to only match sets of a given size';
                                   modifiable:true;  matching:[lt_intSet, lt_emptySet]),
  {tc_typeCheckIntCollection}     (name:'IntCollection';     helpText:'Matches lists and sets, empty or containing only integers#Can be modified to only match collections of a given size';
                                   modifiable:true;  matching:[lt_intList, lt_emptyList, lt_intSet, lt_emptySet]),
  {tc_typeCheckReal}              (name:'Real';              helpText:'Matches scalar real values';
                                   modifiable:false; matching:[lt_real]),
  {tc_typeCheckRealList}          (name:'RealList';          helpText:'Matches lists, empty or containing only reals#Can be modified to only match lists of a given size';
                                   modifiable:true;  matching:[lt_realList, lt_emptyList]),
  {tc_typeCheckRealSet}           (name:'RealSet';           helpText:'Matches sets, empty or containing only reals#Can be modified to only match sets of a given size';
                                   modifiable:true;  matching:[lt_realSet, lt_emptySet]),
  {tc_typeCheckRealCollection}    (name:'RealCollection';    helpText:'Matches lists and sets, empty or containing only reals#Can be modified to only match collections of a given size';
                                   modifiable:true;  matching:[lt_realList, lt_emptyList, lt_realSet, lt_emptySet]),
  {tc_typeCheckString}            (name:'String';            helpText:'Matches scalar strings';
                                   modifiable:false; matching:[lt_string]),
  {tc_typeCheckStringList}        (name:'StringList';        helpText:'Matches lists, empty or containing only strings#Can be modified to only match lists of a given size';
                                   modifiable:true;  matching:[lt_stringList, lt_emptyList]),
  {tc_typeCheckStringSet}         (name:'StringSet';         helpText:'Matches sets, empty or containing only strings#Can be modified to only match sets of a given size';
                                   modifiable:true;  matching:[lt_stringSet, lt_emptySet]),
  {tc_typeCheckStringCollection}  (name:'StringCollection';  helpText:'Matches lists and sets, empty or containing only strings#Can be modified to only match collections of a given size';
                                   modifiable:true;  matching:[lt_stringList, lt_emptyList, lt_stringSet, lt_emptySet]),
  {tc_typeCheckNumeric}           (name:'Numeric';           helpText:'Matches scalar integers or reals';
                                   modifiable:false; matching:[lt_int, lt_real]),
  {tc_typeCheckNumList}           (name:'NumericList';       helpText:'Matches lists, empty or containing only integers or reals#Can be modified to only match lists of a given size';
                                   modifiable:true;  matching:[lt_intList, lt_realList, lt_numList, lt_emptyList]),
  {tc_typeCheckNumSet}            (name:'NumericSet';        helpText:'Matches sets, empty or containing only integers or reals#Can be modified to only match sets of a given size';
                                   modifiable:true;  matching:[lt_intSet, lt_realSet, lt_numSet, lt_emptySet]),
  {tc_typeCheckNumCollection}     (name:'NumericCollection'; helpText:'Matches lists and sets, empty or containing only integers or reals#Can be modified to only match collections of a given size';
                                   modifiable:true;  matching:[lt_intList, lt_realList, lt_numList, lt_emptyList, lt_intSet, lt_realSet, lt_numSet, lt_emptySet]),
  {tc_typeCheckMap}               (name:'Map';               helpText:'Matches maps#Can be modified to only match maps of a given size';
                                   modifiable:true;  matching:[lt_emptyMap,lt_map]),
  {tc_typeCheckExpression}        (name:'Expression';        helpText:'Matches expressions#Can be modified to only match expressions accepting a given number of parameters';
                                   modifiable:true;  matching:[lt_expression]),
  {tc_typeCheckStatelessExpression}
                                  (name:'StatelessExpression'; helpText:'Matches stateless expressions#Can be modified to only match expressions accepting a given number of parameters';
                                   modifiable:true;  matching:[lt_expression]),
  {tc_typeCheckStatefulExpression}(name:'StatefulExpression'; helpText:'Matches stateful expressions#Can be modified to only match expressions accepting a given number of parameters';
                                   modifiable:true;  matching:[lt_expression]),
  {tc_typeCheckIteratableExpression}
                                  (name:'IteratableExpression'; helpText:'Matches iteratable expressions';
                                   modifiable:false;  matching:[lt_expression]),
  {tc_typeCheckIteratable}        (name:'Iteratable'; helpText:'Matches iteratable expressions, collections and maps';
                                   modifiable:false;  matching:[lt_expression..lt_emptyMap]));

TYPE
  T_modifier=(
    modifier_private,
    modifier_memoized,
    modifier_mutable,
    modifier_datastore,
    modifier_plain,
    modifier_synchronized,
    modifier_local,
    modifier_customType);
  T_modifierSet=set of T_modifier;
CONST
  C_modifierInfo:array[T_modifier] of record
    name,helpText:string;
    isRuleModifier:boolean;
  end=((name:PRIVATE_TEXT;    helpText:'Limits visiblity of the declaration to the package it is declared in'           ; isRuleModifier:true ),
       (name:'memoized';      helpText:'Makes the rule memoized, caching previously computed results'                   ; isRuleModifier:true ),
       (name:'mutable';       helpText:'Makes the rule mutable, de facto changing the rule to a variable'               ; isRuleModifier:true ),
       (name:'datastore';     helpText:'Makes the rule persistent in a separate file.#Persistent rules also are mutable'; isRuleModifier:true ),
       (name:'plain';         helpText:'Modifies a datastore to use plain text instead of default binary format'        ; isRuleModifier:true ),
       (name:'synchronized';  helpText:'Protects the rule from concurrent execution.'                                   ; isRuleModifier:true ),
       (name:'local';         helpText:'Used for declaring block-local variables'                                       ; isRuleModifier:false),
       (name:'type';          helpText:'Used for declaring custom type checks'                                          ; isRuleModifier:true ));

  C_specialWordInfo:array[0..5] of record
    txt:string;
    reservedWordClass:T_reservedWordClass;
    helpText:string;
  end=((txt:LITERAL_TEXT_VOID; reservedWordClass:rwc_specialLiteral; helpText:'void literal#Denotes a literal "which is not there"#Intended use: list construction and blank branches of inline if clauses'),
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
      C_validModifierCombinations:array[0..15] of record
        modifiers:T_modifierSet;
        ruleType:T_ruleType;
      end=((modifiers:[];                                                   ruleType:rt_normal),
           (modifiers:[modifier_private];                                   ruleType:rt_normal),
           (modifiers:[modifier_memoized];                                  ruleType:rt_memoized),
           (modifiers:[modifier_memoized,modifier_private];                 ruleType:rt_memoized),
           (modifiers:[modifier_mutable];                                   ruleType:rt_mutable),
           (modifiers:[modifier_mutable,modifier_private];                  ruleType:rt_mutable),
           (modifiers:[modifier_datastore];                                 ruleType:rt_datastore),
           (modifiers:[modifier_datastore   ,modifier_private];             ruleType:rt_datastore),
           (modifiers:[modifier_plain,modifier_datastore];                  ruleType:rt_datastore),
           (modifiers:[modifier_plain,modifier_datastore,modifier_private]; ruleType:rt_datastore),
           (modifiers:[modifier_memoized];                                  ruleType:rt_memoized),
           (modifiers:[modifier_memoized,modifier_private];                 ruleType:rt_memoized),
           (modifiers:[modifier_synchronized];                              ruleType:rt_synchronized),
           (modifiers:[modifier_synchronized,modifier_private];             ruleType:rt_synchronized),
           (modifiers:[modifier_customType];                                ruleType:rt_customTypeCheck),
           (modifiers:[modifier_customType,modifier_private];               ruleType:rt_customTypeCheck));

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
  C_textMessages:T_messageTypeSet=[mt_clearConsole..mt_el4_systemError,mt_timing_info];
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
{mt_el4_haltMessageQuiet   }             (level: 4; mClass:mc_error;   prefix: ''                     ; includeLocation: false; ignoredBySandbox:  true; triggersGuiStartup:false; systemErrorLevel:0),
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
TYPE
  T_sideEffect=(se_inputViaAsk,
                se_output,
                se_sound,
                se_sleep,
                se_detaching,
                se_server,
                se_readPackageState,
                se_alterPackageState,
                se_alterContextState,
                se_alterPlotState,
                se_readFile,
                se_writeFile,
                se_accessHttp,
                se_accessIpc,
                se_executingExternal);
  T_sideEffects=set of T_sideEffect;

CONST
  C_sideEffectName:array[T_sideEffect] of string=(
                'input',
                'output',
                'sound',
                'sleep',
                'detaching',
                'server',
                'read package state',
                'alter package state',
                'alter context state',
                'alter plot state',
                'read file',
                'write file',
                'http',
                'ipc',
                'executing external');
  C_allSideEffects:T_sideEffects=[low(T_sideEffect)..high(T_sideEffect)];
FUNCTION ALLOW_ALL_SIDE_EFFECTS_ATTRIBUTE_VALUE:string;
FUNCTION isQualified(CONST s:string):boolean;
FUNCTION unqualifiedId(CONST qualifiedId:string):string;
FUNCTION configDir:string;
FUNCTION isSideEffectName(CONST s:string; OUT sideEffect:T_sideEffect):boolean;
OPERATOR :=(CONST x:T_messageTypeSet):qword;
OPERATOR :=(x:qword):T_messageTypeSet;
IMPLEMENTATION
FUNCTION isSideEffectName(CONST s:string; OUT sideEffect:T_sideEffect):boolean;
  VAR se:T_sideEffect;
  begin
    result:=false;
    for se in C_allSideEffects do if s=C_sideEffectName[se] then begin
      sideEffect:=se;
      exit(true);
    end;
  end;

FUNCTION ALLOW_ALL_SIDE_EFFECTS_ATTRIBUTE_VALUE:string;
  VAR se:T_sideEffect;
  begin
    result:='';
    for se in C_allSideEffects do begin
      if result<>'' then result:=result+', ';
      result+=C_sideEffectName[se];
    end;
  end;

FUNCTION isQualified(CONST s:string):boolean;
  begin
    result:=pos(ID_QUALIFY_CHARACTER,s)>0;
  end;

FUNCTION unqualifiedId(CONST qualifiedId:string):string;
  VAR idx:longint;
  begin
    idx:=pos(ID_QUALIFY_CHARACTER,qualifiedId);
    if idx<=0 then result:=qualifiedId
              else result:=copy(qualifiedId,idx+1,length(qualifiedId));
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
