UNIT recyclers;
INTERFACE
USES sysutils,
     myGenerics, myStringUtil,
     basicTypes, mnh_constants,
     out_adapters,
     valueStore,
     litVar,
     {$ifdef fullVersion}
     mnh_messages,
     {$endif}
     tokens;
TYPE
  T_preparedToken=record
    parIdx:byte;
    token:T_token;
  end;

  T_preparedTokenArray = array of T_preparedToken;

  P_recycler=^T_recycler;
  T_recycler=object(T_localLiteralRecycler)
    private
      isFree:boolean;
      tokens:record
        dat:array[0..1023] of P_token;
        fill:longint;
      end;
      scopeRecycler:record
        fill:longint;
        dat:array[0..63] of P_valueScope;
      end;
      cleanupPosted:boolean;
      hardCleanupPosted:boolean;
      PROCEDURE cleanup(CONST hard:boolean);

      CONSTRUCTOR create;
    public
      DESTRUCTOR destroy; virtual;
      PROCEDURE cleanupIfPosted;
      FUNCTION disposeToken(p:P_token):P_token; inline;
      PROCEDURE cascadeDisposeToken(VAR p:P_token);
      FUNCTION newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer=nil):P_token; inline;
      FUNCTION newToken(CONST original:T_token):P_token; inline;
      FUNCTION newToken(CONST original:P_token):P_token; inline;
      FUNCTION getRawTokenRange(CONST templateLength:longint; CONST add3:boolean):T_tokenRange;

      PROCEDURE disposeScope(VAR scope:P_valueScope); inline;
      FUNCTION  newValueScopeAsChildOf(CONST scope:P_valueScope):P_valueScope; inline;
      PROCEDURE scopePush(VAR scope:P_valueScope); inline;
      PROCEDURE scopePop(VAR scope:P_valueScope); inline;
      FUNCTION  cloneSafeValueStore(CONST oldStore:P_valueScope):P_valueScope;
  end;

  P_abstractRule=^T_abstractRule;
  T_abstractRule=object(T_objectWithIdAndLocation)
    private
      id:T_idString;
      ruleType:T_ruleType;
    protected
      declarationStart:T_tokenLocation;
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ruleTyp:T_ruleType);
      DESTRUCTOR destroy; virtual;
      FUNCTION getId:T_idString; virtual;
      FUNCTION getRootId:T_idString; virtual;
      FUNCTION getLocation:T_tokenLocation; virtual;
      PROPERTY getRuleType:T_ruleType read ruleType;
      FUNCTION innerRuleType:T_ruleType; virtual;

      FUNCTION hasPublicSubrule:boolean; virtual; abstract;

      FUNCTION getCmdLineHelpText:T_arrayOfString; virtual;
      {$ifdef fullVersion}
      FUNCTION hasAnnotationMarkingAsUsed:boolean; virtual; abstract;
      FUNCTION getStructuredInfo:T_structuredRuleInfoList; virtual; abstract;
      {$endif}
      PROCEDURE clearCache; virtual;
      FUNCTION isReportable(OUT value:P_literal):boolean; virtual; abstract;
      FUNCTION canBeApplied(CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT output:T_tokenRange; CONST context:P_abstractContext; CONST recycler:P_recycler):boolean; virtual; abstract;
      FUNCTION evaluateToBoolean(CONST callLocation:T_tokenLocation; CONST singleParameter:P_literal; CONST recycler:P_recycler; CONST context:P_abstractContext):boolean;
      FUNCTION evaluateToLiteral(CONST callLocation:T_tokenLocation; CONST p1,p2:P_literal;       CONST recycler:P_recycler; CONST context:P_abstractContext):P_literal; virtual; abstract;
      FUNCTION evaluateToLiteral(CONST callLocation:T_tokenLocation; CONST parList:P_listLiteral; CONST recycler:P_recycler; CONST context:P_abstractContext):P_literal; virtual; abstract;
      FUNCTION getTypedef:P_typedef; virtual;
      FUNCTION getInlineValue:P_literal; virtual;
      {Return "pure" for the sake of constant inlining, i.e. memoized rules are pure by definition}
      FUNCTION isPure:boolean; virtual; abstract;
  end;

FUNCTION newRecycler:P_recycler;
PROCEDURE freeRecycler(VAR recycler:P_recycler);
PROCEDURE noRecycler_disposeScope(VAR scope: P_valueScope);
PROCEDURE cleanupRecyclerPools;
PROCEDURE cleanupRecyclerPoolsHard;
IMPLEMENTATION
USES mySys,mnh_settings;
VAR recyclerPool:array of P_recycler;
    recyclerPoolCs:TRTLCriticalSection;
    recyclerPoolsFinalized:boolean=false;

FUNCTION newRecycler:P_recycler;
  VAR r:P_recycler;
  begin
    result:=nil;
    enterCriticalSection(recyclerPoolCs);
    try
      for r in recyclerPool do if (result=nil) and r^.isFree then begin
        r^.isFree:=false;
        result:=r;
      end;
      if result=nil then begin
        setLength(recyclerPool,length(recyclerPool)+1);
        new(result,create);
        recyclerPool[length(recyclerPool)-1]:=result;
      end;
    finally
      leaveCriticalSection(recyclerPoolCs);
    end;
  end;

PROCEDURE freeRecycler(VAR recycler:P_recycler);
  VAR i:longint;
      j:longint=0;
  begin
    recycler^.cleanupIfPosted;
    recycler^.isFree:=true;
    recycler:=nil;

    if length(recyclerPool)*2>settings.cpuCount then begin
      enterCriticalSection(recyclerPoolCs);
      try
        for i:=0 to length(recyclerPool)-1 do
          if recyclerPool[i]^.isFree
          then dispose(recyclerPool[i],destroy)
          else begin
            recyclerPool[j]:=recyclerPool[i];
            inc(j);
          end;
        setLength(recyclerPool,j);
      finally
        leaveCriticalSection(recyclerPoolCs);
      end;
    end;
  end;

PROCEDURE cleanupRecyclerPools;
  VAR i:longint;
  begin
    if recyclerPoolsFinalized then exit;
    enterCriticalSection(recyclerPoolCs);
    try
      for i:=0 to length(recyclerPool)-1 do recyclerPool[i]^.cleanupPosted:=true;
    finally
      leaveCriticalSection(recyclerPoolCs);
    end;
    globalLiteralRecycler.freeMemory(false);
  end;

PROCEDURE cleanupRecyclerPoolsHard;
  VAR i:longint;
      j:longint=0;
  begin
    if recyclerPoolsFinalized then exit;
    enterCriticalSection(recyclerPoolCs);
    try
      for i:=0 to length(recyclerPool)-1 do
        if recyclerPool[i]^.isFree
        then dispose(recyclerPool[i],destroy)
        else begin
          recyclerPool[j]:=recyclerPool[i];
          recyclerPool[i]^.hardCleanupPosted:=true;
          inc(j);
        end;
      setLength(recyclerPool,j);
    finally
      leaveCriticalSection(recyclerPoolCs);
    end;
    globalLiteralRecycler.freeMemory(true);
  end;

PROCEDURE finalizeRecyclerPools;
  VAR tEnd: qword;
  begin
    assert(not(recyclerPoolsFinalized));
    enterCriticalSection(recyclerPoolCs);
    cleanupRecyclerPoolsHard;
    tEnd:=GetTickCount64+1000;
    while (length(recyclerPool)>0) and (GetTickCount64<tEnd) do begin
      leaveCriticalSection(recyclerPoolCs);
      ThreadSwitch; sleep(1);
      enterCriticalSection(recyclerPoolCs);
    end;
    assert(length(recyclerPool)=0);
    recyclerPoolsFinalized:=true;
    leaveCriticalSection(recyclerPoolCs);
    doneCriticalSection(recyclerPoolCs);
  end;

PROCEDURE T_recycler.cleanup(CONST hard:boolean);
  begin
    if hard then with tokens do while fill>0 do begin
      dec(fill);
      try
        dispose(dat[fill],destroy);
      except
        dat[fill]:=nil;
      end;
    end;
    with scopeRecycler do begin
      while fill>0 do begin
        dec(fill);
        try
          dispose(dat[fill],destroy);
        except
          dat[fill]:=nil;
        end;
      end;
    end;
    inherited freeMemory(hard);
  end;

CONSTRUCTOR T_recycler.create;
  begin
    isFree:=false;
    cleanupPosted:=false;
    hardCleanupPosted:=false;
    tokens.fill:=0;
    scopeRecycler.fill:=0;
    inherited create;
    exchangeLiterals();
  end;

DESTRUCTOR T_recycler.destroy;
  begin
    cleanup(true);
    inherited destroy;
  end;

PROCEDURE T_recycler.cleanupIfPosted;
  begin
    if cleanupPosted or hardCleanupPosted
    then cleanup(hardCleanupPosted)
    else exchangeLiterals();
    cleanupPosted:=false;
    hardCleanupPosted:=false;
  end;

FUNCTION T_recycler.disposeToken(p: P_token): P_token;
  {$ifdef debugMode}VAR i:longint; {$endif}
  begin
    if p=nil then result:=nil
    else begin
      {$ifdef debugMode}
      for i:=0 to tokens.fill-1 do if (pointer(tokens.dat[i])=pointer(p)) then raise Exception.create('Disposing already disposed token $'+IntToHex(ptrint(pointer(p)),16));
      {$endif}

      result:=p^.next;
      p^.undefine(@self);
      with tokens do if (fill>=length(dat))
      then dispose(p,destroy)
      else begin
        dat[fill]:=p;
        inc(fill);
      end;
    end;
  end;

PROCEDURE T_recycler.cascadeDisposeToken(VAR p: P_token);
  begin
    while p<>nil do p:=disposeToken(p);
  end;

FUNCTION T_recycler.newToken(CONST tokenLocation: T_tokenLocation; CONST tokenText: ansistring; CONST tokenType: T_tokenType; CONST ptr: pointer): P_token;
  begin
    with tokens do if (fill>0) then begin dec(fill); result:=dat[fill]; end else new(result,create);
    result^.define(tokenLocation,tokenText,tokenType,ptr);
    result^.next:=nil;
  end;

FUNCTION T_recycler.newToken(CONST original: T_token): P_token;
  begin
    with tokens do if (fill>0) then begin dec(fill); result:=dat[fill]; end else new(result,create);
    result^.define(original,@self);
    result^.next:=nil;
  end;

FUNCTION T_recycler.newToken(CONST original: P_token): P_token;
  begin
    with tokens do if (fill>0) then begin dec(fill); result:=dat[fill]; end else new(result,create);
    result^.define(original^,@self);
    result^.next:=nil;
  end;

FUNCTION T_recycler.getRawTokenRange(CONST templateLength:longint; CONST add3:boolean):T_tokenRange;
  VAR i:longint;
  begin
    if add3
    then i:=templateLength+3
    else i:=templateLength;
    with tokens do begin
      if (fill>0) then begin dec(fill); result.first:=dat[fill]; end else new(result.first,create);
      dec(i);
      result.last:=result.first;
      while (i>0) and (fill>0) do begin
        dec(fill);
        result.last^.next:=dat[fill];
        result.last:=result.last^.next;
        dec(i);
      end;
    end;
    while (i>0) do begin
      new(result.last^.next,create);
      result.last:=result.last^.next;
      dec(i);
    end;
    result.last^.next:=nil;
  end;

PROCEDURE noRecycler_disposeScope(VAR scope: P_valueScope);
  VAR recycler:P_recycler;
  begin
    recycler:=newRecycler;
    recycler^.disposeScope(scope);
    freeRecycler(recycler);
  end;

PROCEDURE T_recycler.disposeScope(VAR scope: P_valueScope);
  VAR parent:P_valueScope;
  begin
    if scope=nil then exit;
    if interlockedDecrement(scope^.refCount)>0 then begin
      scope:=nil;
      exit;
    end;
    scope^.cleanup(@self);
    parent:=scope^.parentScope;
    with scopeRecycler do begin
      if (fill>=length(dat))
      then dispose(scope,destroy)
      else begin
        scope^.insteadOfDestroy;
        dat[fill]:=scope;
        inc(fill);
      end;
    end;
    disposeScope(parent);
    scope:=nil;
  end;

FUNCTION T_recycler.newValueScopeAsChildOf(CONST scope: P_valueScope): P_valueScope;
  begin
    with scopeRecycler do begin
      if (fill>0) then begin
        dec(fill);
        result:=dat[fill];
        result^.insteadOfCreate(scope);
      end else new(result,create(scope));
    end;
  end;

PROCEDURE T_recycler.scopePush(VAR scope: P_valueScope);
  VAR newScope:P_valueScope;
  begin
    with scopeRecycler do begin
      if (fill>0) then begin
        dec(fill);
        newScope:=dat[fill];
        newScope^.insteadOfCreate(scope);
      end else new(newScope,create(scope));
    end;
    scope:=newScope;
  end;

PROCEDURE T_recycler.scopePop(VAR scope: P_valueScope);
  VAR newScope:P_valueScope;
  begin
    if scope=nil then exit;
    newScope:=scope^.parentScope;
    if interlockedDecrement(scope^.refCount)>0 then begin
      scope:=newScope;
      exit;
    end;
    scope^.cleanup(@self);
    with scopeRecycler do begin
      if (fill>=length(dat))
      then dispose(scope,destroy)
      else begin
        scope^.insteadOfDestroy;
        dat[fill]:=scope;
        inc(fill);
      end;
    end;
    if newScope<>nil then interlockedDecrement(newScope^.refCount);
    scope:=newScope;
  end;

FUNCTION T_recycler.cloneSafeValueStore(CONST oldStore: P_valueScope): P_valueScope;
  VAR k:longint;
  begin
    result:=newValueScopeAsChildOf(oldStore^.parentScope);
    setLength(result^.variables,oldStore^.varFill);
    result^.varFill:=oldStore^.varFill;
    for k:=0 to oldStore^.varFill-1 do result^.variables[k]:=oldStore^.variables[k]^.clone;
  end;

CONSTRUCTOR T_abstractRule.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation; CONST ruleTyp: T_ruleType);
  begin
    id              :=ruleId;
    declarationStart:=startAt;
    ruleType        :=ruleTyp;
  end;

DESTRUCTOR T_abstractRule.destroy;                    begin id:='';                   end;
FUNCTION T_abstractRule.getId: T_idString;            begin result:=id; end;
FUNCTION T_abstractRule.getRootId: T_idString;        begin result:=id; end;
FUNCTION T_abstractRule.getLocation: T_tokenLocation; begin result:=declarationStart; end;

FUNCTION T_abstractRule.innerRuleType: T_ruleType;
  begin
    result:=ruleType;
  end;

FUNCTION T_abstractRule.getCmdLineHelpText: T_arrayOfString;
  begin
    result:=(C_ruleTypeText[getRuleType]+'rule '+getId+C_lineBreakChar+'in '+getLocation.package^.getPath);
  end;

PROCEDURE T_abstractRule.clearCache; begin end;
FUNCTION T_abstractRule.evaluateToBoolean(CONST callLocation:T_tokenLocation; CONST singleParameter:P_literal; CONST recycler:P_recycler; CONST context:P_abstractContext):boolean;
  VAR parList:P_listLiteral;
      rep:T_tokenRange;
  begin
    parList:=recycler^.newListLiteral(1);
    parList^.append(recycler,singleParameter,true);
    if canBeApplied(callLocation,parList,rep,context,recycler)
    then begin
      result:=(rep.first<>     nil          ) and
              (rep.first^.next=nil          ) and
              (rep.first^.tokType=tt_literal) and
              (rep.first^.data=@boolLit[true]);
      recycler^.cascadeDisposeToken(rep.first);
    end else result:=false;
    recycler^.disposeLiteral(parList);
  end;

FUNCTION T_abstractRule.getTypedef: P_typedef;
  begin
    raise Exception.create('getTypeDef is not implemented for this rule type');
    result:=nil;
  end;

FUNCTION T_abstractRule.getInlineValue: P_literal;
  begin result:=nil; end;

INITIALIZATION
  initCriticalSection(recyclerPoolCs);
  setLength(recyclerPool,0);
  memoryCleaner.registerCleanupMethod(0,@cleanupRecyclerPools);
  memoryCleaner.registerCleanupMethod(1,@cleanupRecyclerPoolsHard);

FINALIZATION
  finalizeRecyclerPools;

end.
