PROGRAM testCompare;
USES math;




begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  writeln('0.0= Nan : ',0.0= Nan,' ',Nan= 0.0);
  writeln('0.0<>Nan : ',0.0<>Nan,' ',Nan<>0.0);
  writeln('0.0< Nan : ',0.0< Nan,' ',Nan> 0.0);
  writeln('0.0<=Nan : ',0.0<=Nan,' ',Nan>=0.0);
  writeln('0.0>=Nan : ',0.0>=Nan,' ',Nan<=0.0);
  writeln('0.0> Nan : ',0.0> Nan,' ',Nan< 0.0);
  writeln('CompareValue(0.0,Nan) : ',byte(CompareValue(0.0,Nan)));
  writeln('CompareValue(Nan,0.0) : ',CompareValue(Nan,0.0));

  writeln('0.0= +Inf : ',0.0= Infinity,' ',Infinity= 0.0);
  writeln('0.0<>+Inf : ',0.0<>Infinity,' ',Infinity<>0.0);
  writeln('0.0< +Inf : ',0.0< Infinity,' ',Infinity> 0.0);
  writeln('0.0<=+Inf : ',0.0<=Infinity,' ',Infinity>=0.0);
  writeln('0.0>=+Inf : ',0.0>=Infinity,' ',Infinity<=0.0);
  writeln('0.0> +Inf : ',0.0> Infinity,' ',Infinity< 0.0);

  writeln('0.0= -Inf : ',0.0= -Infinity,' ',-Infinity= 0.0);
  writeln('0.0<>-Inf : ',0.0<>-Infinity,' ',-Infinity<>0.0);
  writeln('0.0< -Inf : ',0.0< -Infinity,' ',-Infinity> 0.0);
  writeln('0.0<=-Inf : ',0.0<=-Infinity,' ',-Infinity>=0.0);
  writeln('0.0>=-Inf : ',0.0>=-Infinity,' ',-Infinity<=0.0);
  writeln('0.0> -Inf : ',0.0> -Infinity,' ',-Infinity< 0.0);

  writeln('+Inf= +Inf : ',Infinity= Infinity,' ',Infinity= Infinity);
  writeln('+Inf<>+Inf : ',Infinity<>Infinity,' ',Infinity<>Infinity);
  writeln('+Inf< +Inf : ',Infinity< Infinity,' ',Infinity> Infinity);
  writeln('+Inf<=+Inf : ',Infinity<=Infinity,' ',Infinity>=Infinity);
  writeln('+Inf>=+Inf : ',Infinity>=Infinity,' ',Infinity<=Infinity);
  writeln('+Inf> +Inf : ',Infinity> Infinity,' ',Infinity< Infinity);



end.


