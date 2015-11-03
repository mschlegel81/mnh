UNIT mnh_constants;
INTERFACE
USES myGenerics,myStringUtil;
TYPE
  T_namespace=(DEFAULT_BUILTIN_NAMESPACE,
               MATH_NAMESPACE           ,
               STRINGS_NAMESPACE        ,
               LIST_NAMESPACE           ,
               REGEX_NAMESPACE          ,
               SYSTEM_BUILTIN_NAMESPACE ,
               PLOT_NAMESPACE);
  T_reservedWordClass=(rwc_not_reserved,
                       rwc_specialLiteral,
                       rwc_specialConstruct,
                       rwc_operator,
                       rwc_modifier);

CONST
  C_voidText= 'void';
  C_nanText = 'Nan';
  C_infText = 'Inf';
  C_boolText: array[false..true] of string = ('false', 'true');

  ONE_SECOND=1/(24*60*60);
  ONE_MINUTE=1/(24*60);
  SCRIPT_EXTENSION='.MNH';
  C_namespaceString:array[T_namespace] of string=('mnh','math','strings','lists','regex','system','plot');
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
    tt_identifier_pon,
    tt_localUserRulePointer_pon,
    tt_importedUserRulePointer_pon, tt_intrinsicRulePointer_pon,
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
    tt_operatorLazyAnd, tt_operatorLazyOr,
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
    tt_modifier_persistent,
    tt_modifier_synchronized,
    tt_modifier_local,
    //procedure block:
    tt_procedureBlockBegin,
    tt_procedureBlockEnd,
    tt_procedureBlockWhile,
    //special: [E]nd [O]f [L]ine
    tt_EOL);

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
  C_validListTypes: set of T_literalType=[lt_list..lt_flatList];

  C_bracketPrecedence: byte = 8; //must be one higher than highest operator precedence
  C_opPrecedence: array[tt_comparatorEq..tt_operatorIn] of byte =
   (6, 6, 6, 6, 6, 6, 6, //comparators
    8, 9, 9,             //logical operators
    8, 9,                //lazy logical operators
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
    {tt_typeCheckNonemptyList} [lt_list..lt_stringList, lt_keyValueList, lt_flatList],
    {tt_typeCheckEmptyList}    [lt_emptyList],
    {tt_typeCheckKeyValueList} [lt_emptyList, lt_keyValueList]);

  C_tokenString: array[T_tokenType] of ansistring = ('','',
    //identifier and resolved identifiers
    '', '', '', '', '','', '', '','',
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
    'AND', 'OR',
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
    'persistent',
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
    'keyValueList',
    'flatList',
    'list(containing error)',
    C_voidText);

  C_ruleTypeString: array[tt_localUserRulePointer..tt_intrinsicRulePointer] of string = (
    'user function (local)',
    'user function (imported)',
    'built in function');

TYPE
  T_messageType = (
    mt_clearConsole,
    mt_printline,
    mt_echo_input,
    mt_echo_declaration,
    mt_echo_output,
    mt_debug_step,
    mt_el1_note,
    mt_el2_warning,
    mt_el3_evalError,
    mt_el3_noMatchingMain,
    mt_el3_stackTrace,
    mt_el4_parsingError,
    mt_el5_systemError,
    mt_el5_haltMessageReceived,
    mt_endOfEvaluation,
    mt_reloadRequired,
    mt_timing_info,
    mt_imageCreated);

CONST
  C_errorLevelForMessageType:array[T_messageType] of shortint=(
   -2,//mt_clearConsole,
   -2,//mt_printline,
   -1,//mt_echo_input,
   -1,//mt_echo_declaration,
   -1,//mt_echo_output,
   -1,//mt_debug_step,
    1,//mt_el1_note,
    2,//mt_el2_warning,
    3,//mt_el3_evalError,
    3,//mt_el3_noMatchingMain
    3,
    4,//mt_el4_parsingError,
    5,//mt_el5_systemError,
    5,//mt_el5_haltMessageReceived
   -1,//mt_endOfEvaluation
   -1,//mt_reloadRequired
   -1,//mt_timing_info
   -1);

  SELF_TOKEN_TEXT='$self';
  SELF_TOKEN_PAR_IDX=maxLongint;
  ALL_PARAMETERS_TOKEN_TEXT='$params';
  ALL_PARAMETERS_PAR_IDX=SELF_TOKEN_PAR_IDX-1;
  REMAINING_PARAMETERS_IDX=ALL_PARAMETERS_PAR_IDX-1;
  C_errorLevelTxt: array[T_messageType] of string = (
    '',//cls
    '',//out
    ' in>',//echo input
    ' in>',//echo declaration
    'out>',//echo output
    'STEP:',

    'Note ',
    'Warning ',
    'Error ',
    'Error ',
    'Error [stack trace]',
    'Parsing Error ',
    'Sys. Error ',
    'Evaluation haltet (most probably by user).',
    '',
    '',
    '',
    'Image:');

  DOC_COMMENT_PREFIX='//*';
  SPECIAL_COMMENT_BLOB_BEGIN='//!BLOB START';
  SPECIAL_COMMENT_BLOB_END='//!BLOB END';

FUNCTION isReservedNamespace(CONST id:ansistring):boolean;
FUNCTION isReservedWord(CONST wordText:ansistring):T_reservedWordClass;
FUNCTION reservedWordsByClass(CONST clazz:T_reservedWordClass):T_listOfString;
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
    result:=rwc_not_reserved;
    if (wordText=C_voidText) or
       (wordText=C_nanText) or
       (wordText=C_infText) or
       (wordText=C_boolText[true]) or
       (wordText=C_boolText[false]) then exit(rwc_specialLiteral);
    if (wordText=C_tokenString[tt_each]) or
       (wordText=C_tokenString[tt_parallelEach]) or
       (wordText=C_tokenString[tt_aggregatorConstructor]) or
       (wordText=C_tokenString[tt_procedureBlockWhile]) or
       (wordText=C_tokenString[tt_procedureBlockBegin]) or
       (wordText=C_tokenString[tt_procedureBlockEnd]) then exit(rwc_specialConstruct);
    for tt:=tt_comparatorEq to tt_listToParameterList do
      if wordText=C_tokenString[tt] then exit(rwc_operator);
    for tt:=tt_modifier_private to tt_modifier_local do
      if wordText=C_tokenString[tt] then exit(rwc_modifier);
  end;

FUNCTION reservedWordsByClass(CONST clazz:T_reservedWordClass):T_listOfString;
  VAR tt:T_tokenType;
      rwc:T_reservedWordClass;
      subList:T_listOfString;
  begin
    result.create;
    case clazz of
      rwc_specialLiteral: begin
        result.add(C_voidText);
        result.add(C_nanText);
        result.add(C_infText);
        result.add(C_boolText[true]);
        result.add(C_boolText[false]);
      end;
      rwc_specialConstruct: begin
        result.add(C_tokenString[tt_each]);
        result.add(C_tokenString[tt_parallelEach]);
        result.add(C_tokenString[tt_aggregatorConstructor]);
        result.add(C_tokenString[tt_procedureBlockWhile]);
        result.add(C_tokenString[tt_procedureBlockBegin]);
        result.add(C_tokenString[tt_procedureBlockEnd]);
      end;
      rwc_operator: for tt:=tt_comparatorEq to tt_listToParameterList do if isIdentifier(C_tokenString[tt],false) then result.add(C_tokenString[tt]);
      rwc_modifier: for tt:=tt_modifier_private to tt_modifier_local do result.add(C_tokenString[tt]);
      else for rwc:=rwc_specialLiteral to rwc_modifier do begin
        subList:=reservedWordsByClass(rwc);
        result.addAll(subList.elementArray);
        subList.destroy;
      end;
    end;
    result.unique;
  end;

end.
