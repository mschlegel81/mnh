UNIT regexCache;
INTERFACE
USES myGenerics,
     RegExpr;
TYPE P_regexCache=^T_regexCache;

     { T_regexCache }

     T_regexCache=object
       private
         lock:TRTLCriticalSection;
         map:specialize G_stringKeyMap<TRegExpr>;
       public
         CONSTRUCTOR create;
         DESTRUCTOR destroy;
         FUNCTION regexForExpression(CONST expression:ansistring):TRegExpr;
     end;

IMPLEMENTATION
PROCEDURE disposeRegex(VAR r:TRegExpr);
  begin
    r.free;
  end;

CONSTRUCTOR T_regexCache.create;
  begin
    initCriticalSection(lock);
    enterCriticalSection(lock);
    map.create(@disposeRegex);
    leaveCriticalSection(lock);
  end;

DESTRUCTOR T_regexCache.destroy;
  begin
    enterCriticalSection(lock);
    map.destroy;
    leaveCriticalSection(lock);
  end;

FUNCTION T_regexCache.regexForExpression(CONST expression: ansistring): TRegExpr;
  begin
    enterCriticalSection(lock);
    if not(map.containsKey(expression,result)) then begin
      result:=TRegExpr.create(expression);
      map.put(expression,result);
    end;
    leaveCriticalSection(lock);
  end;

end.
