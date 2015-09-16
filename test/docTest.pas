PROGRAM docTest;
USES mnh_funcs,myGenerics;

PROCEDURE infoOnIntrinsics;
  VAR ids:T_arrayOfString;
      i:longint;
  begin
    ids:=intrinsicRuleMap.keySet;
    for i:=0 to length(ids)-1 do begin
      //writeln('=====================================================================');
      writeln(ids[i]);
      //writeln('---------------------------------------------------------------------');
      writeln(intrinsicRuleExplanationMap.get(ids[i]));
    end;

  end;

begin
  infoOnIntrinsics;

end.
