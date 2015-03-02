UNIT mnh_constants;
INTERFACE
TYPE
  T_tokenType=(tt_literal,
               //identifier and resolved identifiers
               tt_identifier, tt_parameterIdentifier, tt_userRulePointer, tt_intrinsicRulePointer,
               //special operators
               tt_set,
               tt_each,
               //lists and list constructors
               tt_braceOpen    ,tt_braceClose    ,tt_parList_constructor  ,tt_parList,
               tt_listBraceOpen,tt_listBraceClose,tt_list_constructor     ,
               tt_expBraceOpen ,tt_expBraceClose ,
               //separators
               tt_separatorComma, tt_separatorCnt,
               //comparators
               tt_comparatorEq,tt_comparatorNeq, tt_comparatorLeq, tt_comparatorGeq, tt_comparatorLss, tt_comparatorGrt, tt_comparatorListEq,
               //logical operators
               tt_operatorAnd, tt_operatorOr, tt_operatorXor,
               //arthmetical operators
               tt_operatorPlus, tt_operatorMinus, tt_operatorMult, tt_operatorDivReal, tt_operatorDivInt, tt_operatorMod, tt_operatorPot,
               //partially evaluated operators
               tt_unaryOpPlus, tt_unaryOpMinus,
               //special: string concatenation
               tt_operatorStrConcat,
               //list operators:
               tt_operatorExtractL0, tt_operatorExtractL1, tt_operatorExtractL2, tt_operatorExtractL3,
               tt_operatorConcat, tt_operatorIn,
               //inline if: (<condition>?<then>:<else>)
               tt_iifCheck, tt_iifElse,
               //assignment operators:
               tt_declare, tt_assign,
               //type checks:
               tt_typeCheckScalar,  tt_typeCheckList,
               tt_typeCheckBoolean, tt_typeCheckBoolList,
               tt_typeCheckInt,     tt_typeCheckIntList,
               tt_typeCheckReal,    tt_typeCheckRealList,
               tt_typeCheckString,  tt_typeCheckStringList,
               tt_typeCheckNumeric, tt_typeCheckNumList,
               tt_typeCheckExpression,
               tt_typeCheckNonemptyList,
               tt_typeCheckEmptyList,
               //special: [E]nd [O]f [L]ine
               tt_eol);

  T_literalType=(lt_error,
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
                 lt_uncheckedList,
                 lt_listWithError);
CONST
  C_bracketPrecedence:byte=8; //must be one higher than highest operator precedence
  C_opPrecedence:array[tt_comparatorEq..tt_operatorIn] of byte=
    (6, 6, 6, 6, 6, 6, 6, //comparators
     8, 7, 7,             //logical operators
     4, 4, 3, 3, 3, 3, 2, //arthmetical operators
     8, 8,                //unaries
     5,                   //special: string concatenation
     1, 1, 1, 1, 0, 0);   //list operators

  C_matchingTypes:array[tt_typeCheckScalar..tt_typeCheckEmptyList] of set of T_literalType=
  {tt_typeCheckScalar}     ([lt_boolean,lt_int,lt_real,lt_string],
  {tt_typeCheckList}        [lt_booleanList,lt_intList,lt_realList,lt_stringList,lt_list,lt_numList],
  {tt_typeCheckBoolean}     [lt_boolean],
  {tt_typeCheckBoolList}    [lt_booleanList],
  {tt_typeCheckInt}         [lt_int],
  {tt_typeCheckIntList}     [lt_intList],
  {tt_typeCheckReal}        [lt_real],
  {tt_typeCheckRealList}    [lt_realList],
  {tt_typeCheckString}      [lt_string],
  {tt_typeCheckStringList}  [lt_stringList],
  {tt_typeCheckNumeric}     [lt_int,lt_real],
  {tt_typeCheckNumList}     [lt_intList,lt_realList,lt_numList],
  {tt_typeCheckExpression}  [lt_expression],
  {tt_typeCheckNonemptyList}[lt_booleanList,lt_intList,lt_realList,lt_stringList,lt_list,lt_numList],
  {tt_typeCheckEmptyList}   [lt_list]);

  C_tokenString:array[T_tokenType] of ansistring=('',
    //identifier and resolved identifiers
    '','','','',
    //special operators
    'set', 'each',
    //lists and list constructors
    '(', ')', ''  ,'' ,
    '[', ']', '' ,
    '{', '}',
    //separators
    ',', '..',
    //comparators
    '=','<>','<=','>=','<','>','==',
    //logical operators
    ' and ',' or ',' xor ',
    //arthmetical operators
    '+','-','*','/',' div ',' mod ','^',
    //partially evaluated operators
    '+','-',
    //special: string concatenation
    '&',
    //list operators:
    '%','%%','%%%','%%%%','|',' in ',
    //inline if: (<condition>?<then>:<else>)
    '?',':',
    //assignment operators:
    '->', ':=',
    //type checks:
    ':scalar',':list',
    ':boolean',':booleanList',
    ':int',':intList',
    ':real',':realList',
    ':string',':stringList',
    ':numeric',':numericList',
    ':expression',
    '<>[]',
    '=[]',
    //special: [E]nd [O]f [L]ine
    '');

  C_typeString:array[T_literalType] of string=(
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
    'UNCHECKED LIST!',
    'list(containing error)');

TYPE
  T_errorLevel=(el0_allOkay,
                el1_note,
                el2_warning,
                el3_evalError,
                el4_parsingError,
                el5_systemError);
CONST
  C_errorLevelTxt:array[T_errorLevel] of string=('          ',
                                                 'Note    : ',
                                                 'Warning : ',
                                                 'Ev.Error: ',
                                                 'Ps.Error: ',
                                                 'Sys.Err.: ');
IMPLEMENTATION
end.