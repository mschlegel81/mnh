UNIT mnh_constants;

INTERFACE

TYPE
  T_tokenType = (tt_literal,
    //identifier and resolved identifiers
    tt_identifier, tt_parameterIdentifier, tt_localUserRulePointer,
    tt_importedUserRulePointer, tt_intrinsicRulePointer,
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
    //assignment operators:
    tt_declare, tt_assign,
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
    tt_semicolon,
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
    lt_flatList,
    lt_uncheckedList,
    lt_listWithError);

CONST
  C_bracketPrecedence: byte = 8; //must be one higher than highest operator precedence
  C_opPrecedence: array[tt_comparatorEq..tt_operatorIn] of byte =
    (6, 6, 6, 6, 6, 6, 6, //comparators
    8, 7, 7,             //logical operators
    4, 4, 3, 3, 3, 3, 2, //arthmetical operators
    8, 8,                //unaries
    5,                   //special: string concatenation
    0, 0, 0, 0, 1, 1);   //list operators

  C_matchingTypes: array[tt_typeCheckScalar..tt_typeCheckEmptyList] of
    SET of T_literalType =
    {tt_typeCheckScalar}     ([lt_boolean, lt_int, lt_real, lt_string],
    {tt_typeCheckList}        [lt_booleanList, lt_intList, lt_realList,
    lt_stringList, lt_list, lt_numList, lt_flatList],
    {tt_typeCheckBoolean}     [lt_boolean],
    {tt_typeCheckBoolList}    [lt_booleanList],
    {tt_typeCheckInt}         [lt_int],
    {tt_typeCheckIntList}     [lt_intList],
    {tt_typeCheckReal}        [lt_real],
    {tt_typeCheckRealList}    [lt_realList],
    {tt_typeCheckString}      [lt_string],
    {tt_typeCheckStringList}  [lt_stringList],
    {tt_typeCheckNumeric}     [lt_int, lt_real],
    {tt_typeCheckNumList}     [lt_intList, lt_realList, lt_numList],
    {tt_typeCheckExpression}  [lt_expression],
    {tt_typeCheckNonemptyList}[lt_booleanList, lt_intList, lt_realList,
    lt_stringList, lt_list, lt_numList, lt_flatList],
    {tt_typeCheckEmptyList}   [lt_list, lt_flatList]);

  C_tokenString: array[T_tokenType] of ansistring = ('',
    //identifier and resolved identifiers
    '', '', '', '', '',
    //special operators
    'each', 'Peach',
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
    //assignment operators:
    '->', ':=',
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
    //special: [E]nd [O]f [L]ine
    ';',
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
    'flatList',
    'UNCHECKED LIST!',
    'list(containing error)');

  C_tokenInfoString: array[T_tokenType] of string = (
    'literal',
    'identifier',
    'parameter identifier',
    'user function (local)',
    'user function (imported)',
    'built in function',
    'special built in function: each#can be used for constructing and/or aggregating lists',
    'special built in function: Peach#can be used for constructing and/or aggregating lists#Is evaluated parallel',
    'round opening bracket',
    'round closing bracket',
    'parameter list constructor',
    'parameter list',
    'square opening bracket#for list construction',
    'square closing bracket#for list construction',
    'list constructor',
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
    'unaryOpPlus',
    'unaryOpMinus',
    'operator string concatenation#applies to all types; returns strings',
    'operator extract (Level 0)',
    'operator extract (Level 1)',
    'operator extract (Level 2)',
    'operator extract (Level 3)',
    'concatenation operator#returns lists',
    'in operator#used for determining whether a list contains a specific element',
    'ternary inline-if-operator#Completed by corresponding :',
    'ternary inline-if-operator#Completed by corresponding ?',
    'declaration operator',
    'assignment operator',
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
    'semicolon#marks the end of a statement, assignment or declaration',
    '<void>#this means that there is nothing plausible to parse, e.g. a comment');

TYPE
  T_errorLevel = (el0_allOkay,
    el1_note,
    el2_warning,
    el3_evalError,
    el4_parsingError,
    el5_systemError);

CONST
  C_errorLevelTxt: array[T_errorLevel] of string = ('          ',
    'Note    : ',
    'Warning : ',
    'Ev.Error: ',
    'Ps.Error: ',
    'Sys.Err.: ');

IMPLEMENTATION

end.
