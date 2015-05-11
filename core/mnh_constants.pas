UNIT mnh_constants;

INTERFACE
CONST
  MAX_NUMBER_OF_SECONDARY_WORKER_THREADS=3;
  SCRIPT_EXTENSION='.MNH';
  DEFAULT_BUILTIN_NAMESPACE='mnh';
  SYSTEM_BUILTIN_NAMESPACE='system';
  C_ID_QUALIFY_CHARACTER='.';
  FUNCTION isQualified(CONST s:string):boolean;
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
    tt_identifier, tt_parameterIdentifier, tt_localUserRulePointer,
    tt_importedUserRulePointer, tt_intrinsicRulePointer, tt_rulePutCacheValue,
    tt_blockLocalVariable,
    tt_aggregatorConstructor,
    //special operators
    tt_each, tt_parallelEach,
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
    //arthmetical operators
    tt_operatorPlus, tt_operatorMinus, tt_operatorMult,
    tt_operatorDivReal, tt_operatorDivInt, tt_operatorMod, tt_operatorPot,
    //partially evaluated operators
    tt_unaryOpPlus, tt_unaryOpMinus,
    //special: string concatenation
    tt_operatorStrConcat,
    //list operators:
    tt_operatorExtractL0, tt_operatorExtractL1, tt_operatorExtractL2,
    tt_operatorExtractL3,
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
    tt_typeCheckNonemptyList,
    tt_typeCheckEmptyList,
    tt_typeCheckKeyValueList,
    tt_semicolon,
    tt_optionalParameters,
    //modifiers:
    tt_modifier_private,
    tt_modifier_memoized,
    tt_modifier_mutable,
    tt_modifier_synchronized,
    tt_modifier_local,
    //procedure block:
    tt_procedureBlockBegin,
    tt_procedureBlockEnd,
    tt_procedureBlockWhile,
    //special: [E]nd [O]f [L]ine
    tt_eol);

  T_literalType = (lt_error,
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
    lt_flatList,
    lt_uncheckedList,
    lt_listWithError,
    lt_void);

CONST
  C_validListTypes: set of T_literalType=[lt_list..lt_flatList];

  C_bracketPrecedence: byte = 8; //must be one higher than highest operator precedence
  C_opPrecedence: array[tt_comparatorEq..tt_operatorIn] of byte =
    (6, 6, 6, 6, 6, 6, 6, //comparators
    8, 9, 9,             //logical operators
    4, 4, 3, 3, 3, 3, 2, //arthmetical operators
    8, 8,                //unaries
    5,                   //special: string concatenation
    0, 0, 0, 0, 1, 7);   //list operators

  C_matchingTypes: array[tt_typeCheckScalar..tt_typeCheckKeyValueList] of set of T_literalType =
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
    {tt_typeCheckNonemptyList} [lt_list..lt_stringList, lt_flatList],
    {tt_typeCheckEmptyList}    [lt_emptyList],
    {tt_typeCheckKeyValueList} [lt_list, lt_emptyList]);

  C_tokenString: array[T_tokenType] of ansistring = ('','',
    //identifier and resolved identifiers
    '', '', '', '', '',
    '', '', 'aggregator',
    //special operators
    'each', 'pEach',
    //lists and list constructors
    '(', ')', '', '',
    '[', ']', '',
    '{', '}',
    //separators
    ',', '..',
    //comparators
    '=', '<>', '<=', '>=', '<', '>', '==',
    //logical operators
    'and', 'or', 'xor',
    //arthmetical operators
    '+', '-', '*', '/', 'div', 'mod', '^',
    //partially evaluated operators
    '+', '-',
    //special: string concatenation
    '&',
    //list operators:
    '%', '%%', '%%%', '%%%%', '|', 'in',
    //inline if: (<condition>?<then>:<else>)
    '?', ':',
    '@',
    //assignment operators:
    '->', ':=', ':=', ':=', ':=',
    '+=', '-=', '*=', '/=', '&=', '|=',
    //type checks:
    ':scalar', ':list',
    ':boolean', ':booleanList',
    ':int', ':intList',
    ':real', ':realList',
    ':string', ':stringList',
    ':numeric', ':numericList',
    ':expression',
    '<>[]',
    '=[]',
    ':keyValueList',
    //special: [E]nd [O]f [L]ine
    ';',
    '...',
    'private',
    'memoized',
    'mutable',
    'synchronized',
    'local',
    'begin',
    'end',
    'while',
    '');

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
    'flatList',
    'UNCHECKED LIST!',
    'list(containing error)',
    'void-type');

  C_tokenInfoString: array[T_tokenType] of string = (
    'literal',
    '',  //'aggregator',
    'identifier',
    'parameter identifier',
    'user function (local)',
    'user function (imported)',
    'built in function',
    '',  //'put cache value',
    '',  //'local variable reference',
    'aggregator-construcor: aggregator#can be used for constructing aggregators to be used in conjunction with each or pEach',
    'special built in function: each#can be used for constructing and/or aggregating lists',
    'special built in function: pEach#can be used for constructing and/or aggregating lists#parallel equivalent to each',
    'round opening bracket',
    'round closing bracket',
    '',  //'parameter list constructor',
    '',  //'parameter list',
    'square opening bracket#for list construction',
    'square closing bracket#for list construction',
    '',  //'list constructor',
    'curly opening bracket#for inline expressions',
    'curly closing bracket#for inline expressions',
    'colon#separator',
    'counter#separator; only valid when constructing lists',
    'comparator equals',
    'comparator not-equal',
    'comparator lesser-or-equal',
    'comparator greater-or-equal',
    'comparator lesser',
    'comparator greater',
    'comparator list-equal',
    'logical operator and#may be applied to integers or booleans',
    'logical operator or#may be applied to integers or booleans',
    'locical operator xor#exclusive or; may be applied to integers or booleans',
    'operator plus#binary or unary operator',
    'operator minus#binary or unary operator',
    'operator mult',
    'operator division#always yields a real number',
    'operator division#applies only to integers; returns integers',
    'operator modulo#applies only to integers; returns integers',
    'operator potentiation',
    '',  //'unaryOpPlus',
    '',  //'unaryOpMinus',
    'operator string concatenation#applies to all types; returns strings',
    'operator extract (Level 0)',
    'operator extract (Level 1)',
    'operator extract (Level 2)',
    'operator extract (Level 3)',
    'concatenation operator#applies to all types; returns lists',
    'in operator#used for determining whether a list contains a specific element',
    'ternary inline-if-operator#Completed by corresponding :',
    'ternary inline-if-operator#Completed by corresponding ?',
    'list-to-parameter-list prefix operator#Mutates a list to a parameter list',
    'declaration operator',
    'assignment operator',
    '',  //'mutate/assign operator',
    '',  //'mutate/new local operator',
    '',  //'mutate/ex. local operator',
    'add-mutate/ex. local operator',
    'subtract-mutate/ex. local operator',
    'multiply-mutate/ex. local operator',
    'divie-mutate/ex. local operator',
    'concat-mutate/ex. local operator',
    'append-mutate/ex. local operator',
    'type check: scalar#matches to all primitive (i.e. non-list) types',
    'type check: list#matches to all list types',
    'type check: boolean#matches to primitive booleans',
    'type check: boolean list#matches to flat lists of booleans',
    'type check: integer#matches to primitive integers',
    'type check: integer list#matches to flat lists of integers',
    'type check: real#matches to primitive reals',
    'type check: real list#matches to flat lists of reals',
    'type check: string#matches to primitive strings',
    'type check: string list#matches to flat lists of strings',
    'type check: numeric#matches to scalar integers and reals',
    'type check: numeric list#matches to flat lists containing only integers and reals',
    'type check: expression#matches to expressions',
    'type check: nonempty list#matches to all non-empty lists',
    'type check: empty list#matches only to the empty list',
    'type check: key-value-list#matches (nonempty) key-value lists',
    'semicolon#marks the end of a statement, assignment or declaration',
    'optional parameters#can be used inside a pattern declaration to denote further optional parameters',
    'private modifier#hides the subrule from all importing packages',
    'memoized modifier#enables caching for the rule#Note: caching affects all rules with the same id in the same package',
    'mutable modifier#makes the rule mutable, i.e. the value may be redefined at evaluation-time',
    'synchronized modifier#makes the rule synchronized, i.e. only one instance of the rule is evaluated at any time',
    'local modifier#allows for the declaration of local variables inside of procedure blocks',
    'marks the begin of a procedure block',
    'marks the end of a procedure block',
    'denotes the beginning of a (head controlled) loop',
    'There is nothing plausible to parse, e.g. a comment');

TYPE
  T_errorLevel = (el0_allOkay,
    el1_note,
    el2_warning,
    el3_evalError,
    el4_parsingError,
    el5_systemError,
    elX_stateInfo);

CONST
  SELF_TOKEN_TEXT='$self';
  SELF_TOKEN_PAR_IDX=maxLongint;
  ALL_PARAMETERS_TOKEN_TEXT='$params';
  ALL_PARAMETERS_PAR_IDX=SELF_TOKEN_PAR_IDX-1;
  REMAINING_PARAMETERS_IDX=ALL_PARAMETERS_PAR_IDX-1;
  C_errorLevelTxt: array[T_errorLevel] of string = ('          ',
    'Note    : ',
    'Warning : ',
    'Ev.Error: ',
    'Ps.Error: ',
    'Sys.Err.: ',
    'AbortState: ');

  DOC_COMMENT_PREFIX='//*';
  SPECIAL_COMMENT_BATCH_STYLE_ON ='//!BATCH ON';
  SPECIAL_COMMENT_BATCH_STYLE_OFF='//!BATCH OFF';
  SPECIAL_COMMENT_BLOB_BEGIN='//!BLOB START';
  SPECIAL_COMMENT_BLOB_END='//!BLOB END';

IMPLEMENTATION
FUNCTION isQualified(CONST s:string):boolean;
  begin
    result:=pos(C_ID_QUALIFY_CHARACTER,s)>0;
  end;

end.
