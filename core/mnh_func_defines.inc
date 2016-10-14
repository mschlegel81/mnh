{$MACRO ON}
{$define intFuncSignature:=(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal}
{$define arg0:=params^.value(0)}
{$define arg1:=params^.value(1)}
{$define arg2:=params^.value(2)}
{$define arg3:=params^.value(3)}
{$define list0:=P_listLiteral(params^.value(0))}
{$define list1:=P_listLiteral(params^.value(1))}
{$define list2:=P_listLiteral(params^.value(2))}
{$define list3:=P_listLiteral(params^.value(3))}
{$define str0:=P_stringLiteral(params^.value(0))}
{$define str1:=P_stringLiteral(params^.value(1))}
{$define str2:=P_stringLiteral(params^.value(2))}
{$define str3:=P_stringLiteral(params^.value(3))}
{$define int0:=P_intLiteral(params^.value(0))}
{$define int1:=P_intLiteral(params^.value(1))}
{$define int2:=P_intLiteral(params^.value(2))}
{$define int3:=P_intLiteral(params^.value(3))}
{$define real0:=P_realLiteral(params^.value(0))}
{$define real1:=P_realLiteral(params^.value(1))}
{$define real2:=P_realLiteral(params^.value(2))}
{$define real3:=P_realLiteral(params^.value(3))}
{$define bool0:=P_boolLiteral(params^.value(0))}
{$define bool1:=P_boolLiteral(params^.value(1))}
{$define bool2:=P_boolLiteral(params^.value(2))}
{$define bool3:=P_boolLiteral(params^.value(3))}
{$define lResult:=P_listLiteral(result)}

