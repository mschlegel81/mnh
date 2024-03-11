UNIT mnh_constants;
INTERFACE
USES sysutils,FileUtil,Classes;
CONST
  STACK_DEPTH_LIMIT={$ifdef debugMode}
                      {$ifdef Windows} 2500
                      {$else}          1000
                      {$endif}
                    {$else}
                      {$ifdef Windows}
                         {$ifdef CPU64}   6100      //Win64
                           {$else}        8300      //Win32
                           {$endif}
                      {$else}
                        {$ifdef profilingFlavour}
                                          1500
                        {$else}           3000     //Linux
                        {$endif}
                      {$endif}
                    {$endif};
  {$i res_version.inc}
  FLAVOUR_STRING={$ifdef fullVersion}'F'{$else}'L'{$endif}+
                 {$ifdef profilingFlavour}'P'{$else}
                   {$ifdef debugMode}'D'{$else}'O'{$endif}
                 {$endif}+{$I %FPCTargetOS%};
  LOGO:array[0..21] of string=(
  ' ___      ___ ___   ___ ___   ___  '+VERSION,
  '|   \    /   |   \ |   |   | |   |  ______',
  '|    \  /    |    \|   |   |_|   | |   ___|',
  '|     \/     |     \   |         | |  |__',
  '|   \    /   |   \     |    _    | |___  \',
  '|   |\  /|   |   |\    |   | |   |  ___)  |',
  '|___| \/ |___|___| \___|___| |___| |_____/',
  '             (c) Martin Schlegel, 2010-2023',
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
  '  BGRABitmap                                                   https://github.com/bgrabitmap/',
  '  Lazarus Component Library (LCL) ');

  APP_NAME             ='MNH';
  APP_TITLE            ='MNH5';
  SCRIPT_EXTENSION     ='.mnh';
  SETTINGS_FILE_NAME   ='mnh.settings';
  {$ifdef Windows}
  VAR APP_STYLE:(APP_STYLE_BLANK,APP_STYLE_PORTABLE,APP_STYLE_NORMAL)=APP_STYLE_BLANK;
  CONST
  {$endif}
  LITERAL_TEXT_VOID    = 'void';
  LITERAL_TEXT_END_OF_GENERATOR = 'generatorClosed';
  LITERAL_NAN_TEXT     = 'Nan';
  LITERAL_INF_TEXT     = 'Inf';
  LITERAL_BOOL_TEXT: array[boolean] of string = ('false', 'true');

  ONE_SECOND                    =1/(24*60*60);
  ONE_MINUTE                    =1/(24*60);
  ONE_HOUR                      =1/ 24;

  ID_QUALIFY_CHARACTER          ='.';
  EACH_INDEX_IDENTIFIER         ='index';
  MAIN_RULE_ID                  ='main';
  ALL_PARAMETERS_TOKEN_TEXT     ='$params';
  PUBLIC_TEXT                   ='public';
  PRIVATE_TEXT                  ='private';
  NO_PARAMETERS_IDX             =255;
  ALL_PARAMETERS_PAR_IDX        =254;
  REMAINING_PARAMETERS_IDX      =ALL_PARAMETERS_PAR_IDX-1;
  COMMENT_PREFIX                ='//';
  BLOCK_COMMENT_DELIMITER       ='#';
  DOC_COMMENT_INFIX             ='*';
  ATTRIBUTE_PREFIX              ='@';
  SPECIAL_COMMENT_BLOB_BEGIN_INFIX='!';
  FORCE_GUI_PSEUDO_PACKAGE      ='GUI';

  SUPPRESS_WARNING_ATTRIBUTE='SuppressWarning';
  SUPPRESS_UNUSED_WARNING_ATTRIBUTE='SuppressUnusedWarning';
  SUPPRESS_ALL_UNUSED_VALUE='all';
  {$ifdef fullVersion}
  SUPPRESS_UNUSED_PARAMETER_WARNING_ATTRIBUTE='SuppressUnusedParameterWarning';
  {$endif}
  OVERRIDE_ATTRIBUTE='Override';
  OVERLOAD_ATTRIBUTE='Overload';
  EXECUTE_AFTER_ATTRIBUTE='after';
  MAX_AGE_ATTRIBUTE='MaxAge';
  TO_STRING_RULE_ID='toString';

TYPE
  T_ruleSorting=(rs_none,rs_byNameCaseSensitive,rs_byNameCaseInsensitive,rs_byLocation);

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
               FTP_NAMESPACE            ,
               IPC_NAMESPACE            ,
               PLOT_NAMESPACE           ,
               GUI_NAMESPACE, IMIG_NAMESPACE);

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
    'ftp',
    'ipc',
    'plot',
    'gui','imig');
TYPE
  T_tokenType = (tt_literal, tt_aggregatorExpressionLiteral,
    //identifier and resolved identifiers
    tt_identifier, tt_parameterIdentifier, tt_userRule,
    tt_intrinsicRule, tt_rulePutCacheValue,
    tt_customType,
    tt_globalVariable,
    tt_blockLocalVariable,
    tt_eachParameter,
    tt_eachIndex,
    tt_ponFlipper,
    tt_aggregatorConstructor,
    //special operators
    tt_each, tt_parallelEach, tt_agg, tt_while, tt_beginBlock, tt_beginRule, tt_beginExpression, tt_endBlock, tt_endRule, tt_endExpression, tt_save, tt_return, tt_pseudoFuncPointer,
    tt_pow2,tt_pow3,
    //lists and list constructors
    tt_braceOpen, tt_braceClose, tt_parList_constructor, tt_parList,
    tt_listBraceOpen, tt_listBraceClose, tt_list_constructor, tt_list_constructor_ranging,
    tt_expBraceOpen, tt_expBraceClose,
    //inline if: (<condition>?<then>:<else>)
    tt_iifCheck, tt_iifElse,
    //separators
    tt_separatorComma, tt_separatorCnt, tt_separatorMapItem,
    //comparators
    tt_comparatorEq, tt_comparatorNeq, tt_comparatorLeq,
    tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq,
    tt_operatorIn, tt_operatorNotIn,
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
    //unary operators
    tt_unaryOpNegate,
    tt_unaryOpPlus, tt_unaryOpMinus,
    tt_listToParameterList,
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
    tt_assignBlockConstant,
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
    tt_use,tt_include,tt_nameOf,
    tt_startOfPattern,     //lexer interal
    tt_endOfPatternDeclare,//lexer interal
    tt_endOfPatternAssign, //lexer interal
    tt_functionPattern,
    tt_formatString,
    tt_blank);

  T_tokenTypeSet  =set of T_tokenType;
  T_tokenTypeInfo=record
    tokenType:T_tokenType;
    reservedWordClass:T_reservedWordClass;
    info:ansistring;
  end;

CONST
  C_operatorsForAggregators: T_tokenTypeSet=[tt_operatorAnd..tt_operatorPot,tt_operatorStrConcat,tt_operatorOrElse,tt_operatorConcat,tt_operatorConcatAlt];
  C_operators: T_tokenTypeSet=[tt_comparatorEq..tt_operatorConcatAlt];
  C_unaryOperators: T_tokenTypeSet=[tt_unaryOpNegate,tt_unaryOpPlus,tt_unaryOpMinus];
  C_comparators: T_tokenTypeSet=[tt_comparatorEq..tt_operatorNotIn];
  C_openingBrackets:T_tokenTypeSet=[tt_beginBlock,tt_beginRule,tt_beginExpression,tt_each,tt_parallelEach,tt_agg,tt_braceOpen,tt_parList_constructor,tt_listBraceOpen,tt_list_constructor,tt_expBraceOpen,tt_iifCheck,tt_startOfPattern];
  C_closingBrackets:T_tokenTypeSet=[tt_endBlock,tt_endRule,tt_endExpression,tt_braceClose,tt_listBraceClose,tt_expBraceClose,tt_iifElse,tt_endOfPatternAssign,tt_endOfPatternDeclare];
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
    {tt_pseudoFuncPointer}  tt_EOL,
    {tt_pow2}               tt_EOL,
    {tt_pow3}               tt_EOL,
    {tt_braceOpen}          tt_braceClose,
    {tt_braceClose}         tt_EOL,
    {tt_parList_constructor}tt_braceClose,
    {tt_parList}            tt_EOL,
    {tt_listBraceOpen}      tt_listBraceClose,
    {tt_listBraceClose}     tt_EOL,
    {tt_list_constructor}   tt_listBraceClose,
                            tt_listBraceClose,
    {tt_expBraceOpen}       tt_expBraceClose,
    {tt_expBraceClose}      tt_EOL,
    {tt_iifCheck}           tt_iifElse);

  C_opPrecedence: array[tt_separatorMapItem..tt_unaryOpMinus,0..1] of byte =
   ((2,2),
    (6,6),(6,6),(6,6),(6,6),(6,6),(6,6),(6,6), //comparators
    (7,7),(7,7),                //in, not in
    (8,8),(9,9),(9,9),          //logical operators
    (8,8),(9,9),                //lazy logical operators
    (4,4),(4,4),(3,3),(3,3),(3,3),(3,3),(2,3), //arthmetical operators
    (5,5),(9,9),                   //special: string concatenation
    (10,10),(11,11),
    (1,1),(1,1),(1,1));            //unary (prefix) operators

  allOperators:T_tokenTypeSet=[tt_comparatorEq..tt_unaryOpMinus];
  unaryOperators:T_tokenTypeSet=[tt_unaryOpNegate..tt_unaryOpMinus];
  overridableOperators:T_tokenTypeSet=[
  tt_comparatorEq     ,
  tt_comparatorNeq    ,
  tt_comparatorLeq    ,
  tt_comparatorGeq    ,
  tt_comparatorLss    ,
  tt_comparatorGrt    ,
  tt_operatorAnd      ,
  tt_operatorOr       ,
  tt_operatorXor      ,
  tt_operatorPlus     ,
  tt_operatorMinus    ,
  tt_operatorMult     ,
  tt_operatorDivReal  ,
  tt_operatorDivInt   ,
  tt_operatorMod      ,
  tt_operatorPot      ,
  tt_operatorStrConcat,
  tt_unaryOpNegate,
  tt_unaryOpMinus];

  C_compatibleEnd:array[tt_beginBlock..tt_beginExpression] of T_tokenType=(tt_endBlock,tt_endRule,tt_endExpression);
  C_compatibleBegin:array[tt_endBlock..tt_endExpression] of T_tokenType=(tt_beginBlock,tt_beginRule,tt_beginExpression);
  C_tokenDefaultId:array[T_tokenType] of string=(
  {tt_literal}                    '',
  {tt_aggregatorExpressionLiteral}'',
  {tt_identifier}                 '',
  {tt_parameterIdentifier}        '',
  {tt_userRule}                   '',
  {tt_intrinsicRule}              '',
  {tt_rulePutCacheValue}          '',
  {tt_customType}                 '',
  {tt_globalVariable}             '',
  {tt_blockLocalVariable}         '',
  {tt_eachParameter}              '',
  {tt_eachIndex}                  '',
  {tt_ponFlipper}                 '.',
  {tt_aggregatorConstructor}      'aggregator',
  {tt_each}                       '.each',
  {tt_parallelEach}               '.pEach',
  {tt_agg}                        '.agg',
  {tt_while}                      'while',
  {tt_beginBlock}                 'begin',
  {tt_beginRule}                  '',
  {tt_beginExpression}            '',
  {tt_endBlock}                   'end',
  {tt_endRule}                    '',
  {tt_endExpression}              '',
  {tt_save}                       'save',
  {tt_return}                     'return',
  {tt_pseudoFuncPointer}          '::',
  {tt_pow2}                       '²',
  {tt_pow3}                       '³',
  {tt_braceOpen}                  '(',
  {tt_braceClose}                 ')',
  {tt_parList_constructor}        '',
  {tt_parList}                    '',
  {tt_listBraceOpen}              '[',
  {tt_listBraceClose}             ']',
  {tt_list_constructor}           '',
  {tt_list_Constructor_ranging}   '',
  {tt_expBraceOpen}               '{',
  {tt_expBraceClose}              '}',
  {tt_iifCheck}                   '?',
  {tt_iifElse}                    ':',
  {tt_separatorComma}             ',',
  {tt_separatorCnt}               '..',
  {tt_separatorMapItem}           '=>',
  {tt_comparatorEq}               '=',
  {tt_comparatorNeq}              '<>',
  {tt_comparatorLeq}              '<=',
  {tt_comparatorGeq}              '>=',
  {tt_comparatorLss}              '<',
  {tt_comparatorGrt}              '>',
  {tt_comparatorListEq}           '==',
  {tt_operatorIn}                 'in',
  {tt_operatorNotIn}              'not in',
  {tt_operatorAnd}                'and',
  {tt_operatorOr}                 'or',
  {tt_operatorXor}                'xor',
  {tt_operatorLazyAnd}            'AND',
  {tt_operatorLazyOr}             'OR',
  {tt_operatorPlus}               '+',
  {tt_operatorMinus}              '-',
  {tt_operatorMult}               '*',
  {tt_operatorDivReal}            '/',
  {tt_operatorDivInt}             'div',
  {tt_operatorMod}                'mod',
  {tt_operatorPot}                '^',
  {tt_operatorStrConcat}          '&',
  {tt_operatorOrElse}             'orElse',
  {tt_operatorConcat}             '|',
  {tt_operatorConcatAlt}          '||',
  {tt_unaryOpNegate}              '!',
  {tt_unaryOpPlus}                '+',
  {tt_unaryOpMinus}               '-',
  {tt_listToParameterList}        '@',
  {tt_declare}                    '->',
  {tt_assign}                     ':=',
  {tt_mutate}                     ':=',
  {tt_assignNewBlockLocal}        ':=',
  {tt_assignExistingBlockLocal}   ':=',
  {tt_mut_nested_assign}          '',
  {tt_mut_assignPlus}             '+=',
  {tt_mut_nestedPlus}             '',
  {tt_mut_assignMinus}            '-=',
  {tt_mut_nestedMinus}            '',
  {tt_mut_assignMult}             '*=',
  {tt_mut_nestedMult}             '',
  {tt_mut_assignDiv}              '/=',
  {tt_mut_nestedDiv}              '',
  {tt_mut_assignStrConcat}        '&=',
  {tt_mut_nestedStrConcat}        '',
  {tt_mut_assignAppend}           '|=',
  {tt_mut_nestedAppend}           '',
  {tt_mut_assignAppendAlt}        '||=',
  {tt_mut_nestedAppendAlt}        '',
  {tt_mut_assignDrop}             '>>',
  {tt_mut_nestedDrop}             '',
  {tt_assignBlockConstant}        '',
  {tt_type}                       '',
  {tt_typeCheck}                  '',
  {tt_customTypeCheck}            '',
  {tt_semicolon}                  ';',
  {tt_optionalParameters}         '...',
  {tt_modifier}                   '',
  {tt_EOL}                        '',
  {tt_docComment}                 '',
  {tt_attributeComment}           '',
  {tt_use}                        'USE',
  {tt_include}                    'INCLUDE',
  {tt_nameOf}                     'nameOf',
  {tt_startOfPattern}             '(',
  {tt_endOfPatternDeclare}        ')->',
  {tt_endOfPatternAssign}         '):=',
  {tt_functionPattern}            '',
  {tt_formatString}               '',
  {tt_blank}                      '');
{$ifdef fullVersion}
  C_tokenDoc:array[T_tokenType] of record
                                 reservedWordClass:T_reservedWordClass;  helpText,helpLink:string; end=(
{tt_literal}                    (reservedWordClass:rwc_not_reserved;     helpText:'A literal'; helpLink:''),
{tt_aggregatorExpressionLiteral}(reservedWordClass:rwc_not_reserved;     helpText:'An aggregator expression literal'; helpLink:''),
{tt_identifier}                 (reservedWordClass:rwc_not_reserved;     helpText:'An identifier (unresolved)'; helpLink:''),
{tt_parameterIdentifier}        (reservedWordClass:rwc_not_reserved;     helpText:'A parameter identifier'; helpLink:'/types.html#expressions'),
{tt_userRule}                   (reservedWordClass:rwc_not_reserved;     helpText:'A local user rule'; helpLink:'/functions.html#declarations'),
{tt_intrinsicRule}              (reservedWordClass:rwc_not_reserved;     helpText:'A built in rule'; helpLink:''),
{tt_rulePutCacheValue}          (reservedWordClass:rwc_not_reserved;     helpText:'A put-cache-value call'; helpLink:''),
{tt_customType}                 (reservedWordClass:rwc_not_reserved;     helpText:'A custom type'; helpLink:'/functions.html#typeMod'),
{tt_globalVariable}             (reservedWordClass:rwc_not_reserved;     helpText:'A global variable'; helpLink:'/functions.html#mutableMod'),
{tt_blockLocalVariable}         (reservedWordClass:rwc_not_reserved;     helpText:'A block-local variable'; helpLink:'/functions.html#localMod'),
{tt_eachParameter}              (reservedWordClass:rwc_not_reserved;     helpText:'each parameter'; helpLink:'/specials.html#each'),
{tt_eachIndex}                  (reservedWordClass:rwc_not_reserved;     helpText:'each index'; helpLink:'/specials.html#each'),
{tt_ponFlipper}                 (reservedWordClass:rwc_not_reserved;     helpText:'A pseudo-object-notation flipper'; helpLink:'/functions.html#calls'),
{tt_aggregatorConstructor}      (reservedWordClass:rwc_specialConstruct; helpText:'Special construct: aggregator#The aggregator constructor'; helpLink:'/specials.html#aggregator'),
{tt_each}                       (reservedWordClass:rwc_specialConstruct; helpText:'Special construct: each#Used for (serial) list operations.#Syntax: <input>.each(<id>,<body>,<aggregator>)#<body> is an arbitrary expression which may use <id> to refer to the current list element or "index" for the current index#<aggregator> is optional and may be a simple operator#A shorthand for <list>.each(_,_,<op>) is: <list>.agg(<op>)'; helpLink:'/specials.html#each'),
{tt_parallelEach}               (reservedWordClass:rwc_specialConstruct; helpText:'Special construct: pEach (parallel each)#Used for parallel list operations.#Parallelized depending on the systen settings.#Syntax: <input>.pEach(<id>,<body>,<aggregator>)#<body> is an arbitrary expression which may use <id> to refer to the current list element or "index" for the current index#<aggregator> is optional and may be a simple operator'; helpLink:'/specials.html#each'),
{tt_agg}                        (reservedWordClass:rwc_specialConstruct; helpText:'Special construct: agg#Used for list aggregation#Syntax: <input>.agg(<aggregator>) - where <aggregator> may be an expression or a simple operator as +'; helpLink:'/specials.html#aggregation'),
{tt_while}                      (reservedWordClass:rwc_specialConstruct; helpText:'Special construct: while#Used for loops#Syntax: while(<entry condition>,<body>) - where <entry condition> must return a scalar boolean'; helpLink:'/specials.html#while'),
{tt_beginBlock}                 (reservedWordClass:rwc_specialConstruct; helpText:'Special construct: begin#Opening delimiter for procedural blocks'; helpLink:'/functions.html#beginEnd'),
{tt_beginRule}                  (reservedWordClass:rwc_specialConstruct; helpText:''; helpLink:''), {No default ID, because tokenizer shall not produce this token}
{tt_beginExpression}            (reservedWordClass:rwc_specialConstruct; helpText:''; helpLink:''), {No default ID, because tokenizer shall not produce this token}
{tt_endBlock}                   (reservedWordClass:rwc_specialConstruct; helpText:'Special construct: end#Closing delimiter for procedural blocks'; helpLink:'/functions.html#beginEnd'),
{tt_endRule}                    (reservedWordClass:rwc_specialConstruct; helpText:''; helpLink:''), {No default ID, because tokenizer shall not produce this token}
{tt_endExpression}              (reservedWordClass:rwc_specialConstruct; helpText:''; helpLink:''), {No default ID, because tokenizer shall not produce this token}
{tt_save}                       (reservedWordClass:rwc_specialConstruct; helpText:'Special construct: save#Saves the current value store for future function calls'; helpLink:'/functions.html#beginSaveEnd'),
{tt_return}                     (reservedWordClass:rwc_specialConstruct; helpText:'Special construct: return#Returns from the current function with the given result value'; helpLink:'/specials.html#return'),
{tt_pseudoFuncPointer}          (reservedWordClass:rwc_specialConstruct; helpText:'Special construct: ::# Returns reference to a function#::f -> {f@$params}'; helpLink:'/operators.html#funcToExp'),
{tt_pow2}                       (reservedWordClass:rwc_specialConstruct; helpText:'Pseudo-Operator: shorthand for ^2'; helpLink:''),
{tt_pow3}                       (reservedWordClass:rwc_specialConstruct; helpText:'Pseudo-Operator: shorthand for ^3'; helpLink:''),
{tt_braceOpen}                  (reservedWordClass:rwc_not_reserved;     helpText:'Opening round bracket#Used as in default mathematical syntax.'; helpLink:''),
{tt_braceClose}                 (reservedWordClass:rwc_not_reserved;     helpText:'Closing round bracket#Used as in default mathematical syntax.'; helpLink:''),
{tt_parList_constructor}        (reservedWordClass:rwc_not_reserved;     helpText:'A parameter list constructor'; helpLink:''),
{tt_parList}                    (reservedWordClass:rwc_not_reserved;     helpText:'A parameter list'; helpLink:''),
{tt_listBraceOpen}              (reservedWordClass:rwc_not_reserved;     helpText:'Square opening bracket#Used for list construction and list access'; helpLink:'/types.html#lists'),
{tt_listBraceClose}             (reservedWordClass:rwc_not_reserved;     helpText:'Square closing bracket#Used for list construction and list access'; helpLink:'/types.html#lists'),
{tt_list_constructor}           (reservedWordClass:rwc_not_reserved;     helpText:'A list constructor'; helpLink:'/types.html#lists'),
{tt_list_Constructor_ranging}   (reservedWordClass:rwc_not_reserved;     helpText:'A list constructor'; helpLink:'/types.html#lists'),
{tt_expBraceOpen}               (reservedWordClass:rwc_not_reserved;     helpText:'Curly opening bracket#Delimits an expression'; helpLink:'/types.html#expressions'),
{tt_expBraceClose}              (reservedWordClass:rwc_not_reserved;     helpText:'Curly closing bracket#Delimits an expression'; helpLink:'/types.html#expressions'),
{tt_iifCheck}                   (reservedWordClass:rwc_operator;         helpText:'Inline-if-operator'; helpLink:'/operators.html#iifOps'),
{tt_iifElse}                    (reservedWordClass:rwc_operator;         helpText:'Inline-if-operator'; helpLink:'/operators.html#iifOps'),
{tt_separatorComma}             (reservedWordClass:rwc_not_reserved;     helpText:'Separator comma'; helpLink:''),
{tt_separatorCnt}               (reservedWordClass:rwc_not_reserved;     helpText:'Separator ..#Used for constructing ranges and only allowed in that context'; helpLink:'/types.html#lists'),
{tt_separatorMapItem}           (reservedWordClass:rwc_not_reserved;     helpText:'Separator =>#Used for constructing key-value-pairs'; helpLink:'/types.html#maps'),
{tt_comparatorEq}               (reservedWordClass:rwc_operator;         helpText:'Equals operator#Returns true if the scalar comparands are type-compatible#and equal#For list operands a list of booleans is returned'; helpLink:'/operators.html#compOps'),
{tt_comparatorNeq}              (reservedWordClass:rwc_operator;         helpText:'Not-equals operator#Returns true if the scalar comparands are type-compatible#and not equal#For list operands a list of booleans is returned'; helpLink:'/operators.html#compOps'),
{tt_comparatorLeq}              (reservedWordClass:rwc_operator;         helpText:'Lesser-or-equals operator#Returns true if the scalar comparands are type-compatible#and the left hand side is lesser or equal to the right hand side#For list operands a list of booleans is returned'; helpLink:'/operators.html#compOps'),
{tt_comparatorGeq}              (reservedWordClass:rwc_operator;         helpText:'Greater-or-equals operator#Returns true if the scalar comparands are type-compatible#and the left hand side is greater or equal to the right hand side#For list operands a list of booleans is returned'; helpLink:'/operators.html#compOps'),
{tt_comparatorLss}              (reservedWordClass:rwc_operator;         helpText:'Lesser operator#Returns true if the scalar comparands are type-compatible#and the left hand side is lesser than the right hand side#For list operands a list of booleans is returned'; helpLink:'/operators.html#compOps'),
{tt_comparatorGrt}              (reservedWordClass:rwc_operator;         helpText:'Greater operator#Returns true if the scalar comparands are type-compatible#and the left hand side is greater than the right hand side#For list operands a list of booleans is returned'; helpLink:'/operators.html#compOps'),
{tt_comparatorListEq}           (reservedWordClass:rwc_operator;         helpText:'Strict-equals operator#Returns true if the comparands are strictly equal.#Always returns a scalar boolean'; helpLink:'/operators.html#compOps'),
{tt_operatorIn}                 (reservedWordClass:rwc_operator;         helpText:'In operator#Applies to all literals on the left hand side and lists on the right hand side.#Returns true if the RHS contains the LHS'; helpLink:'/operators.html#listOps'),
{tt_operatorNotIn}              (reservedWordClass:rwc_operator;         helpText:'Not-In operator#Applies to all literals on the left hand side and lists on the right hand side.#Returns true if the RHS does not contain the LHS'; helpLink:'/operators.html#listOps'),
{tt_operatorAnd}                (reservedWordClass:rwc_operator;         helpText:'Bit-wise and operator#Applies to (lists of) integers and (lists of) booleans'; helpLink:'/operators.html#logOps'),
{tt_operatorOr}                 (reservedWordClass:rwc_operator;         helpText:'Bit-wise or operator#Applies to (lists of) integers and (lists of) booleans'; helpLink:'/operators.html#logOps'),
{tt_operatorXor}                (reservedWordClass:rwc_operator;         helpText:'Bit-wise xor operator#Applies to (lists of) integers and (lists of) booleans'; helpLink:'/operators.html#logOps'),
{tt_operatorLazyAnd}            (reservedWordClass:rwc_operator;         helpText:'Lazy and operator#Applies to scalar booleans only'; helpLink:'/operators.html#logOps'),
{tt_operatorLazyOr}             (reservedWordClass:rwc_operator;         helpText:'Lazy or operator#Applies to scalar booleans only'; helpLink:'/operators.html#logOps'),
{tt_operatorPlus}               (reservedWordClass:rwc_operator;         helpText:'Plus operator#Applies to (lists of) numbers and (lists of) strings'; helpLink:'/operators.html#arithOps'),
{tt_operatorMinus}              (reservedWordClass:rwc_operator;         helpText:'Minus operator#Applies to (lists of) numbers'; helpLink:'/operators.html#arithOps'),
{tt_operatorMult}               (reservedWordClass:rwc_operator;         helpText:'Multiplication operator#Applies to (lists of) numbers'; helpLink:'/operators.html#arithOps'),
{tt_operatorDivReal}            (reservedWordClass:rwc_operator;         helpText:'Division operator#Applies to (lists of) numbers'; helpLink:'/operators.html#arithOps'),
{tt_operatorDivInt}             (reservedWordClass:rwc_operator;         helpText:'Integer division operator#Applies to (lists of) integers'; helpLink:'/operators.html#arithOps'),
{tt_operatorMod}                (reservedWordClass:rwc_operator;         helpText:'Integer modulo operator#Applies to (lists of) integers'; helpLink:'/operators.html#arithOps'),
{tt_operatorPot}                (reservedWordClass:rwc_operator;         helpText:'Potentiation operator#Applies to (lists of) numbers'; helpLink:'/operators.html#arithOps'),
{tt_operatorStrConcat}          (reservedWordClass:rwc_operator;         helpText:'String concatenation operator#Applies to all literals'; helpLink:'/operators.html#stringOps'),
{tt_operatorOrElse}             (reservedWordClass:rwc_operator;         helpText:'Or-Else operator#Employed to provide a fallback to void literals'; helpLink:'/operators.html#orElseOp'),
{tt_operatorConcat}             (reservedWordClass:rwc_operator;         helpText:'List concatenation operator#Applies to all literals'; helpLink:'/operators.html#listOps'),
{tt_operatorConcatAlt}          (reservedWordClass:rwc_operator;         helpText:'List concatenation operator#Applies to all literals'; helpLink:'/operators.html#listOps'),
{tt_unaryOpNegate}              (reservedWordClass:rwc_operator;         helpText:'Logical negation'; helpLink:'/operators.html#logOps'),
{tt_unaryOpPlus}                (reservedWordClass:rwc_operator;         helpText:'Unary plus operator#Neutral'; helpLink:'/operators.html#arithOps'),
{tt_unaryOpMinus}               (reservedWordClass:rwc_operator;         helpText:'Unary minus operator#Negates the operand'; helpLink:'/operators.html#arithOps'),
{tt_listToParameterList}        (reservedWordClass:rwc_operator;         helpText:'List-to-parameter-list operator'; helpLink:'/operators.html#listOps'),
{tt_declare}                    (reservedWordClass:rwc_operator;         helpText:'Declaration operator'; helpLink:'/functions.html#declarations'),
{tt_assign}                     (reservedWordClass:rwc_operator;         helpText:'Assignment operator'; helpLink:'/functions.html#assignments'),
{tt_mutate}                     (reservedWordClass:rwc_operator;         helpText:'Mutate-assign operator'; helpLink:'/functions.html#assignments'),
{tt_assignNewBlockLocal}        (reservedWordClass:rwc_operator;         helpText:'Assign new block local operator'; helpLink:'/functions.html#assignments'),
{tt_assignExistingBlockLocal}   (reservedWordClass:rwc_operator;         helpText:'Assign existing block local operator'; helpLink:'/functions.html#assignments'),
{tt_mut_nested_assign}          (reservedWordClass:rwc_operator;         helpText:''; helpLink:''),
{tt_mut_assignPlus}             (reservedWordClass:rwc_operator;         helpText:'Incrementation operator'; helpLink:'/operators.html#cStyleOps'),
{tt_mut_nestedPlus}             (reservedWordClass:rwc_operator;         helpText:''; helpLink:''),
{tt_mut_assignMinus}            (reservedWordClass:rwc_operator;         helpText:'Decrementation operator'; helpLink:'/operators.html#cStyleOps'),
{tt_mut_nestedMinus}            (reservedWordClass:rwc_operator;         helpText:''; helpLink:''),
{tt_mut_assignMult}             (reservedWordClass:rwc_operator;         helpText:'Multipliy/assign operator'; helpLink:'/operators.html#cStyleOps'),
{tt_mut_nestedMult}             (reservedWordClass:rwc_operator;         helpText:''; helpLink:''),
{tt_mut_assignDiv}              (reservedWordClass:rwc_operator;         helpText:'Divide/assign operator'; helpLink:'/operators.html#cStyleOps'),
{tt_mut_nestedDiv}              (reservedWordClass:rwc_operator;         helpText:''; helpLink:''),
{tt_mut_assignStrConcat}        (reservedWordClass:rwc_operator;         helpText:'String-concatenate/assign operator'; helpLink:'/operators.html#cStyleOps'),
{tt_mut_nestedStrConcat}        (reservedWordClass:rwc_operator;         helpText:''; helpLink:''),
{tt_mut_assignAppend}           (reservedWordClass:rwc_operator;         helpText:'List-concatenate/assign operator'; helpLink:'/operators.html#cStyleOps'),
{tt_mut_nestedAppend}           (reservedWordClass:rwc_operator;         helpText:''; helpLink:''),
{tt_mut_assignAppendAlt}        (reservedWordClass:rwc_operator;         helpText:'Alt-List-concatenate/assign operator'; helpLink:'/operators.html#cStyleOps'),
{tt_mut_nestedAppendAlt}        (reservedWordClass:rwc_operator;         helpText:''; helpLink:''),
{tt_mut_assignDrop}             (reservedWordClass:rwc_operator;         helpText:'Drop operator. Drops element from a set (by value) or from a map (by key)'; helpLink:'/operators.html#cStyleOps'),
{tt_mut_nestedDrop}             (reservedWordClass:rwc_operator;         helpText:''; helpLink:''),
{tt_assignBlockConstant}        (reservedWordClass:rwc_operator;         helpText:''; helpLink:''),
{tt_type}                       (reservedWordClass:rwc_not_reserved;     helpText:'Type'; helpLink:'/functions.html#restrictions'),
{tt_typeCheck}                  (reservedWordClass:rwc_not_reserved;     helpText:'Type check'; helpLink:'/functions.html#restrictions'),
{tt_customTypeCheck}            (reservedWordClass:rwc_not_reserved;     helpText:'Custom type check'; helpLink:'/functions.html#restrictions'),
{tt_semicolon}                  (reservedWordClass:rwc_not_reserved;     helpText:'Semicolon#Ends a statement'; helpLink:''),
{tt_optionalParameters}         (reservedWordClass:rwc_not_reserved;     helpText:'Remaining arguments#Allowes access to anonymous furhter parameters#Returns a list'; helpLink:'/functions.html#variadic'),
{tt_modifier}                   (reservedWordClass:rwc_modifier;         helpText:'Modifier'; helpLink:'/functions.html#modifiers'),
{tt_EOL}                        (reservedWordClass:rwc_not_reserved;     helpText:'End-Of-Input#Helper token; May also indicate a comment'; helpLink:''),
{tt_docComment}                 (reservedWordClass:rwc_not_reserved;     helpText:'Documentation comment'; helpLink:'/packages.html#comments'),
{tt_attributeComment}           (reservedWordClass:rwc_not_reserved;     helpText:'Attribute comment'; helpLink:'/packages.html#attributes'),
{tt_use}                        (reservedWordClass:rwc_modifier;         helpText:'Marker: USE#Denotes the use clause#Followed by package paths (as string) or package ids'; helpLink:'/packages.html#importing'),
{tt_include}                    (reservedWordClass:rwc_modifier;         helpText:'Marker: INCLUDE#Denotes the include clause#Followed by one package path (as string) or one package id'; helpLink:'/packages.html#including'),
{tt_nameOf}                     (reservedWordClass:rwc_operator;         helpText:'Returns the name of the argument or the blank string if there is none'; helpLink:'/specials.html#nameof'),
{tt_startOfPattern}             (reservedWordClass:rwc_not_reserved;     helpText:'Start-of-pattern'; helpLink:''),
{tt_endOfPatternDeclare}        (reservedWordClass:rwc_not_reserved;     helpText:'End-of-pattern/declare'; helpLink:''),
{tt_endOfPatternAssign}         (reservedWordClass:rwc_not_reserved;     helpText:'End-of-pattern/assign'; helpLink:''),
{tt_functionPattern}            (reservedWordClass:rwc_not_reserved;     helpText:'Function pattern'; helpLink:''),
{tt_formatString}               (reservedWordClass:rwc_not_reserved;     helpText:'Format string'; helpLink:''),
{tt_blank}                      (reservedWordClass:rwc_not_reserved;     helpText:'Blank#Helper token; May indicate a comment or whitespace'; helpLink:''));
{$endif}
TYPE
  T_literalType = (
    lt_error,
             lt_boolean,     lt_smallint,
                             lt_bigint,
                                         lt_real,                 lt_string,                   lt_expression,
    lt_list, lt_booleanList, lt_intList, lt_realList, lt_numList, lt_stringList, lt_emptyList,
    lt_set,  lt_booleanSet,  lt_intSet,  lt_realSet,  lt_numSet,  lt_stringSet,  lt_emptySet,
    lt_map,                                                                      lt_emptyMap,
    lt_void, lt_generatorClosed);
  T_literalTypeSet=set of T_literalType;

CONST
  C_typables:T_literalTypeSet=[lt_expression..lt_emptyMap];
  C_typeInfo:array[T_literalType] of record
    name:string;
    containedIn,
    comparableTo:T_literalTypeSet
  end=(
  {lt_error      }(name:'Error'          ; containedIn:[]                                                          ;comparableTo:[]),
  {lt_boolean    }(name:'Boolean'        ; containedIn:[lt_list,lt_booleanList,lt_set,lt_booleanSet]               ;comparableTo:[lt_expression,lt_boolean,                                        lt_list,lt_booleanList,                                                lt_emptyList,lt_set,lt_booleanSet,                                            lt_emptySet]),
  {lt_smallInt   }(name:'Int'            ; containedIn:[lt_list,lt_intList,lt_numList,lt_set,lt_intSet,lt_numSet]  ;comparableTo:[lt_expression,           lt_smallint,lt_bigint,lt_real,          lt_list,               lt_intList,lt_realList,lt_numList,              lt_emptyList,lt_set,              lt_intSet,lt_realSet,lt_numSet,             lt_emptySet]),
  {lt_bigInt     }(name:'Int'            ; containedIn:[lt_list,lt_intList,lt_numList,lt_set,lt_intSet,lt_numSet]  ;comparableTo:[lt_expression,           lt_smallint,lt_bigint,lt_real,          lt_list,               lt_intList,lt_realList,lt_numList,              lt_emptyList,lt_set,              lt_intSet,lt_realSet,lt_numSet,             lt_emptySet]),
  {lt_real       }(name:'Real'           ; containedIn:[lt_list,lt_realList,lt_numList,lt_set,lt_realSet,lt_numSet];comparableTo:[lt_expression,           lt_smallint,lt_bigint,lt_real,          lt_list,               lt_intList,lt_realList,lt_numList,              lt_emptyList,lt_set,              lt_intSet,lt_realSet,lt_numSet,             lt_emptySet]),
  {lt_string     }(name:'String'         ; containedIn:[lt_list,lt_stringList,lt_set,lt_stringSet]                 ;comparableTo:[lt_expression,                                         lt_string,lt_list,                                                 lt_stringList,lt_emptyList,lt_set,                                             lt_stringSet,lt_emptySet]),
  {lt_expression }(name:'Expression'     ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[lt_boolean..lt_emptySet]),
  {lt_list       }(name:'List'           ; containedIn:[lt_list,lt_set,lt_map]                                     ;comparableTo:[lt_expression,lt_boolean,lt_smallint,lt_bigint,lt_real,lt_string,lt_list,lt_booleanList,lt_intList,lt_realList,lt_numList,lt_stringList]),
  {lt_booleanList}(name:'BooleanList'    ; containedIn:[lt_list,lt_set,lt_map]                                     ;comparableTo:[lt_expression,lt_boolean,                                        lt_list,lt_booleanList                                                ]),
  {lt_intList    }(name:'IntList'        ; containedIn:[lt_list,lt_set,lt_map]                                     ;comparableTo:[lt_expression,           lt_smallint,lt_bigint,lt_real,          lt_list,               lt_intList,lt_realList,lt_numList              ]),
  {lt_realList   }(name:'RealList'       ; containedIn:[lt_list,lt_set,lt_map]                                     ;comparableTo:[lt_expression,           lt_smallint,lt_bigint,lt_real,          lt_list,               lt_intList,lt_realList,lt_numList              ]),
  {lt_numList    }(name:'NumericList'    ; containedIn:[lt_list,lt_set,lt_map]                                     ;comparableTo:[lt_expression,           lt_smallint,lt_bigint,lt_real,          lt_list,               lt_intList,lt_realList,lt_numList              ]),
  {lt_stringList }(name:'StringList'     ; containedIn:[lt_list,lt_set,lt_map]                                     ;comparableTo:[lt_expression,                                         lt_string,lt_list,                                                 lt_stringList]),
  {lt_emptyList  }(name:'EmptyList'      ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[lt_expression,lt_boolean,lt_smallint,lt_bigint,lt_real,lt_string,                                                                       lt_emptyList]),
  {lt_set        }(name:'Set'            ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[lt_expression,lt_boolean,lt_smallint,lt_bigint,lt_real,lt_string,lt_set ,lt_booleanSet ,lt_intSet ,lt_realSet ,lt_numSet ,lt_stringSet ,lt_emptySet]),
  {lt_booleanSet }(name:'BooleanSet'     ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[lt_expression,lt_boolean,                                        lt_set ,lt_booleanSet ,                                                lt_emptySet]),
  {lt_intSet     }(name:'IntSet'         ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[lt_expression,           lt_smallint,lt_bigint,lt_real,          lt_set ,               lt_intSet ,lt_realSet ,lt_numSet ,              lt_emptySet]),
  {lt_realSet    }(name:'RealSet'        ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[lt_expression,           lt_smallint,lt_bigint,lt_real,          lt_set ,               lt_intSet ,lt_realSet ,lt_numSet ,              lt_emptySet]),
  {lt_numSet     }(name:'NumSet'         ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[lt_expression,           lt_smallint,lt_bigint,lt_real,          lt_set ,               lt_intSet ,lt_realSet ,lt_numSet ,              lt_emptySet]),
  {lt_stringSet  }(name:'StringSet'      ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[lt_expression,                                         lt_string,lt_set ,                                                 lt_stringSet ,lt_emptySet]),
  {lt_emptySet   }(name:'EmptySet'       ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[lt_expression,lt_boolean,lt_smallint,lt_bigint,lt_real,lt_string,lt_set ,lt_booleanSet ,lt_intSet ,lt_realSet ,lt_numSet ,lt_stringSet ,lt_emptySet]),
  {lt_map        }(name:'Map'            ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[]),
  {lt_emptyMap   }(name:'EmptyMap'       ; containedIn:[lt_list,lt_set]                                            ;comparableTo:[]),
  {lt_void       }(name:LITERAL_TEXT_VOID; containedIn:[]                                                          ;comparableTo:[]),
  {lt_generato...}(name:LITERAL_TEXT_END_OF_GENERATOR; containedIn:[]                                              ;comparableTo:[]));

  C_compoundTypes     : T_literalTypeSet=[lt_list..lt_emptyMap];
  C_collectionTypes   : T_literalTypeSet=[lt_list..lt_emptySet];
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
               tc_typeCheckIteratable,
               tc_any,
               tc_byTypeString);

CONST
  C_typeCheckInfo:array[T_typeCheck] of record
    name:string;
    helpText:string;
    modifiable:boolean;
    matching:T_literalTypeSet;
  end=(
  {tc_typeCheckScalar}            (name:'Scalar';            helpText:'Matches all scalar types except expressions';
                                   modifiable:false; matching:[lt_boolean, lt_smallint,lt_bigint, lt_real, lt_string]),
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
                                   modifiable:false; matching:[lt_smallint,lt_bigint]),
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
                                   modifiable:false; matching:[lt_smallint,lt_bigint, lt_real]),
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
                                   modifiable:false;  matching:[lt_expression..lt_emptyMap]),
  {tc_any}                        (name:''; helpText:'';
                                   modifiable:false;  matching:[lt_boolean..lt_emptyMap]),
  {tc_byTypeString}               (name:''; helpText:'';
                                   modifiable:false;  matching:[lt_boolean..lt_emptyMap]));
TYPE
  T_modifier=(
    modifier_private,
    modifier_memoized,
    modifier_mutable,
    modifier_datastore,
    modifier_plain,
    modifier_synchronized,
    modifier_local,
    modifier_customType,
    modifier_customDuckType,
    modifier_curry);
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
       (name:'type';          helpText:'Used for declaring custom types'                                                ; isRuleModifier:true ),
       (name:'ducktype';      helpText:'Used for declaring custom duck type checks'                                     ; isRuleModifier:true ),
       (name:'curry';         helpText:'Used to enable currying/uncurrying'                                             ; isRuleModifier:true ));

  {$ifdef fullVersion}
  C_specialWordInfo:array[0..6] of record
    txt:string;
    reservedWordClass:T_reservedWordClass;
    helpText:string;
  end=((txt:LITERAL_TEXT_VOID; reservedWordClass:rwc_specialLiteral; helpText:'void literal#Denotes a literal "which is not there"#Intended use: list construction and blank branches of inline if clauses'),
       (txt:LITERAL_TEXT_END_OF_GENERATOR; reservedWordClass:rwc_specialLiteral; helpText:'special modification of a void literal to indicate that no more values will be returned from a generator'),
       (txt:LITERAL_NAN_TEXT;  reservedWordClass:rwc_specialLiteral; helpText:'not-a-number real literal'),
       (txt:LITERAL_INF_TEXT;  reservedWordClass:rwc_specialLiteral; helpText:'infinity real literal'),
       (txt:'false';           reservedWordClass:rwc_specialLiteral; helpText:'false literal'),
       (txt:'true';            reservedWordClass:rwc_specialLiteral; helpText:'true literal'),
       (txt:'main';            reservedWordClass:rwc_not_reserved  ; helpText:'main rule#Called when the script is executed from the command line (or via "call main" in the GUI)'));
  {$endif}
  C_ruleTypeString: array[tt_userRule..tt_rulePutCacheValue] of string = (
    'user function',
    'built in function',
    'put-cache rule');

TYPE
  T_ruleType=(rt_normal, rt_normal_curry,
              rt_memoized, rt_memoized_curry,
              rt_synchronized, rt_synchronized_curry,
              rt_customTypeCheck,
              rt_duckTypeCheck,
              rt_customTypeCast,
              rt_customOperator,
              rt_delegate);
  T_variableType=(vt_mutable,vt_datastore,vt_plainDatastore);

CONST C_ruleTypesWithOnlyOneSubrule:set of T_ruleType=[rt_customTypeCheck,rt_duckTypeCheck,rt_normal_curry,rt_memoized_curry,rt_synchronized_curry];
      C_ruleTypeText:array[T_ruleType] of string=(
      '',
      'curry ',
      'memoized ',
      'memoized curry ',
      'synchronized ',
      'synchronized curry ',
      'type ',
      'ducktype ',
      'typecast ',
      'custom operator ',
      'delegate ');
      C_validModifierCombinations:array[0..19] of record
        modifiers:T_modifierSet;
        metaType:T_tokenType;
        ruleType:T_ruleType;
        variableType:T_variableType;
      end=((modifiers:[];                                                       metaType: tt_userRule;       ruleType:rt_normal               ; variableType:vt_mutable       ),
           (modifiers:[modifier_private];                                       metaType: tt_userRule;       ruleType:rt_normal               ; variableType:vt_mutable       ),
           (modifiers:[modifier_curry];                                         metaType: tt_userRule;       ruleType:rt_normal_curry         ; variableType:vt_mutable       ),
           (modifiers:[modifier_curry,modifier_private];                        metaType: tt_userRule;       ruleType:rt_normal_curry         ; variableType:vt_mutable       ),
           (modifiers:[modifier_memoized];                                      metaType: tt_userRule;       ruleType:rt_memoized             ; variableType:vt_mutable       ),
           (modifiers:[modifier_memoized,modifier_private];                     metaType: tt_userRule;       ruleType:rt_memoized             ; variableType:vt_mutable       ),
           (modifiers:[modifier_curry,modifier_memoized];                       metaType: tt_userRule;       ruleType:rt_memoized_curry       ; variableType:vt_mutable       ),
           (modifiers:[modifier_curry,modifier_memoized,modifier_private];      metaType: tt_userRule;       ruleType:rt_memoized_curry       ; variableType:vt_mutable       ),
           (modifiers:[modifier_synchronized];                                  metaType: tt_userRule;       ruleType:rt_synchronized         ; variableType:vt_mutable       ),
           (modifiers:[modifier_synchronized,modifier_private];                 metaType: tt_userRule;       ruleType:rt_synchronized         ; variableType:vt_mutable       ),
           (modifiers:[modifier_curry,modifier_synchronized];                   metaType: tt_userRule;       ruleType:rt_synchronized_curry   ; variableType:vt_mutable       ),
           (modifiers:[modifier_curry,modifier_synchronized,modifier_private];  metaType: tt_userRule;       ruleType:rt_synchronized_curry   ; variableType:vt_mutable       ),
           (modifiers:[modifier_mutable];                                       metaType: tt_globalVariable; ruleType:rt_normal               ; variableType:vt_mutable       ),
           (modifiers:[modifier_mutable,modifier_private];                      metaType: tt_globalVariable; ruleType:rt_normal               ; variableType:vt_mutable       ),
           (modifiers:[modifier_datastore];                                     metaType: tt_globalVariable; ruleType:rt_normal               ; variableType:vt_datastore    ),
           (modifiers:[modifier_datastore   ,modifier_private];                 metaType: tt_globalVariable; ruleType:rt_normal               ; variableType:vt_datastore    ),
           (modifiers:[modifier_plain,modifier_datastore];                      metaType: tt_globalVariable; ruleType:rt_normal               ; variableType:vt_plainDatastore),
           (modifiers:[modifier_plain,modifier_datastore,modifier_private];     metaType: tt_globalVariable; ruleType:rt_normal               ; variableType:vt_plainDatastore),
           (modifiers:[modifier_customType];                                    metaType: tt_customType;     ruleType:rt_customTypeCheck      ; variableType:vt_mutable       ),
           (modifiers:[modifier_customDuckType];                                metaType: tt_customType;     ruleType:rt_duckTypeCheck        ; variableType:vt_mutable       ));

TYPE
  T_sideEffect=(se_input,
                se_output,
                se_sound,
                se_sleep,
                se_detaching,
                se_server,
                se_readPackageState,
                se_alterPackageState,
                se_readContextState,
                se_alterContextState,
                se_readGuiState,
                se_alterGuiState,
                se_readIdeState,
                se_alterIdeState,
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
                'read context state',
                'alter context state',
                'read GUI state',
                'alter GUI state',
                'read IDE state',
                'alter IDE state',
                'read file',
                'write file',
                'http',
                'ipc',
                'executing external');
  C_allSideEffects:T_sideEffects=[low(T_sideEffect)..high(T_sideEffect)];
  C_sideEffectsForCodeAssistance=[se_readPackageState,se_alterPackageState,se_output];
  C_sideEffectsForFormatting=[se_input,se_readPackageState,se_alterPackageState,se_readContextState,se_alterContextState,se_readGuiState,se_alterGuiState,se_readIdeState,se_alterIdeState,se_readFile,se_accessHttp,se_accessIpc,se_executingExternal];
  C_sideEffectsRequiringGui=[se_readGuiState,se_alterGuiState,se_readIdeState,se_alterIdeState];
  C_sideEffectProfile:array [0..3] of record
    name:string;
    allowed:T_sideEffects;
  end=((name:''; allowed:[low(T_sideEffect)..high(T_sideEffect)]),
       (name:'readonly'; allowed:[se_input,se_output,se_sound,se_sleep,se_detaching,se_server,se_readPackageState,se_alterPackageState,se_readContextState,se_alterContextState,se_readGuiState,se_alterGuiState,se_readIdeState,se_alterIdeState,se_readFile,se_accessHttp]),
       (name:'isolated'; allowed:[         se_output,se_sound,se_sleep,                       se_readPackageState,se_alterPackageState,se_readContextState,se_alterContextState,se_readGuiState,se_alterGuiState,se_readIdeState,se_alterIdeState                          ]),
       (name:'noexe';    allowed:[se_input..se_accessIpc]));

FUNCTION isQualified(CONST s:string):boolean;
FUNCTION unqualifiedId(CONST qualifiedId:string):string;
FUNCTION configDir:string;
FUNCTION collapseMnhDir(CONST fileName:string):string;
FUNCTION expandMnhDir(CONST fileName:string):string;
FUNCTION getTempFileName:string;
IMPLEMENTATION

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

{$ifdef Windows}
PROCEDURE detectAppStyle;
  begin
    if fileExists(ExtractFileDir(paramStr(0))+DirectorySeparator+SETTINGS_FILE_NAME) then APP_STYLE:=APP_STYLE_PORTABLE
    else if                    fileExists(GetAppConfigDir(false)+SETTINGS_FILE_NAME) then APP_STYLE:=APP_STYLE_NORMAL
  end;
{$endif}

FUNCTION configDir:string;
  begin
    {$ifdef Windows}
    if APP_STYLE=APP_STYLE_NORMAL
    then result:=GetAppConfigDir(false)
    else result:=ExtractFileDir(paramStr(0))+DirectorySeparator;
    {$else}
    result:=GetAppConfigDir(false);
    {$endif}
  end;

FUNCTION collapseMnhDir(CONST fileName:string):string;
  begin
    result:=StringReplace(fileName,configDir,'[mnhDir]'+DirectorySeparator,[rfIgnoreCase]);
  end;

FUNCTION expandMnhDir(CONST fileName:string):string;
  begin
    result:=StringReplace(fileName,'[mnhDir]'+DirectorySeparator,configDir,[rfIgnoreCase]);
  end;

VAR tempFilesCs:TRTLCriticalSection;
FUNCTION getTempFileName:string;
  VAR tempFolder:string;
      handle:file of byte;
  begin
    tempFolder:=configDir+'temp';
    ForceDirectories(tempFolder);
    tempFolder+=DirectorySeparator;
    enterCriticalSection(tempFilesCs);
    try
      repeat result:=tempFolder+IntToHex(random(maxLongint),8) until not(fileExists(result));
      //touch file:
      assign(handle,result);
      rewrite(handle);
      close(handle);
    finally
      leaveCriticalSection(tempFilesCs);
    end;
  end;

PROCEDURE cleanupTemp;
  VAR fileName:string;
      allFiles:TStringList;
  begin
    enterCriticalSection(tempFilesCs);
    try
      allFiles:=FindAllFiles(configDir+'temp');
      for fileName in allFiles do try
        DeleteFile(fileName);
      except end;
      allFiles.free;
    finally
      leaveCriticalSection(tempFilesCs);
    end;
  end;

INITIALIZATION
  OnGetApplicationName:=@getAppName;
  {$ifdef Windows}
  detectAppStyle;
  {$endif}
  {$ifdef fullVersion}
  ForceDirectories(configDir);
  initialize(tempFilesCs);
  initCriticalSection(tempFilesCs);
  cleanupTemp;
  {$endif}

FINALIZATION
  {$ifdef fullVersion}
  cleanupTemp;
  doneCriticalSection(tempFilesCs);
  {$endif}
end.
