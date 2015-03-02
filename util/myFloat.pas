UNIT myFloat;
INTERFACE
TYPE
  T_float=record
    value:int64;
    scale:longint;
  end;
  
FUNCTION newFloat(CONST value,scale:longint):T_float;
OPERATOR :=(CONST x:T_float):extended;
OPERATOR :=(CONST x:T_float):string;
IMPLEMENTATION

FUNCTION newFloat(CONST value,scale:longint):T_float;
  begin
    result.value:=value;
    result.scale:=scale;
  end;
  
OPERATOR :=(CONST x:T_float):extended;
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
  begin
    result:=pot10(scale)*value;
  end;
  
OPERATOR :=(CONST x:T_float):string;
  begin
    
  end;

end.
