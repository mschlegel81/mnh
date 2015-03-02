UNIT myFloat;
INTERFACE
USES sysutils;
TYPE
  T_float=record
    value:int64;
    scale:longint;
  end;
  
FUNCTION newFloat(CONST value,scale:longint):T_float;
OPERATOR :=(CONST x:T_float):extended;
OPERATOR :=(CONST x:T_float):string;
FUNCTION canonized(CONST x:T_float):T_float;
FUNCTION withScale(CONST x:T_float; CONST destScale:longint):T_float;
FUNCTION roundness(x:T_float):longint;
IMPLEMENTATION

FUNCTION newFloat(CONST value,scale:longint):T_float;
  begin
    result.value:=value;
    result.scale:=scale;
  end;

FUNCTION pot10(y:int64):extended; inline;
  VAR p10:extended;
  begin
    if y>=0 then begin
      p10:=10;
    end else begin
      p10:=0.1;
      y:=-y;
    end;
    result:=1;
    while y>0 do begin
      if odd(y) then result:=result*p10;
      p10:=p10*p10;
      y:=y shr 1;
    end;
  end;

OPERATOR :=(CONST x:T_float):extended;
  begin
    result:=pot10(x.scale)*x.value;
  end;
  
OPERATOR :=(CONST x:T_float):string;
  CONST suf:array[0..3] of string=('','0','00','000');
  begin
    with x do begin
      if value=0 then exit('0');
      if (scale >=-3) and (scale<0) then begin
        result:=IntToStr(value);
        while length(result)<2 do result:='0'+result;
        exit(copy(result,1,length(result)  +scale)+'.'+
             copy(result,  length(result)+1+scale,-scale));
      end else if (scale>=0) and (scale<3) then exit(IntToStr(value)+suf[scale])
      else exit(IntToStr(value)+'E'+IntToStr(scale));
    end;
  end;

FUNCTION canonized(CONST x: T_float): T_float;
  begin
    result:=x;
    with result do while (value mod 10)=0 do begin
      inc(scale);
      value:=value div 10;
    end;
  end;

FUNCTION roundness(x:T_float):longint;
  begin
    with x do while (value mod 10)=0 do begin
      inc(scale);
      value:=value div 10;
      result:=scale*3;
      if      value mod 5=0 then inc(result,2)
      else if value mod 2=0 then inc(result);
    end;
  end;

FUNCTION withScale(CONST x:T_float; CONST destScale:longint):T_float;
  begin
    result.value:=round(x.value*pot10(x.scale-destScale));
    result.scale:=destScale;
  end;

end.
