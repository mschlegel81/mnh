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
  P_recycler=^T_recycler;
  T_recycler=object
    private
      tokens:record
        dat:array[0..1023] of P_token;
        fill:longint;
      end;
      scopeRecycler:record
        fill:longint;
        dat:array[0..63] of P_valueScope;
      end;
    public
      PROCEDURE initRecycler;
      PROCEDURE cleanup;

      FUNCTION disposeToken(p:P_token):P_token; inline;
      PROCEDURE cascadeDisposeToken(VAR p:P_token);
      FUNCTION newToken(CONST tokenLocation:T_tokenLocation; CONST tokenText:ansistring; CONST tokenType:T_tokenType; CONST ptr:pointer=nil):P_token; inline;
      FUNCTION newToken(CONST original:T_token):P_token; inline;
      FUNCTION newToken(CONST original:P_token):P_token; inline;

      PROCEDURE disposeScope(VAR scope:P_valueScope); inline;
      FUNCTION  newValueScopeAsChildOf(CONST scope:P_valueScope; CONST parentAccess:AccessLevel):P_valueScope; inline;
      PROCEDURE scopePush(VAR scope:P_valueScope; CONST parentAccess:AccessLevel); inline;
      PROCEDURE scopePop(VAR scope:P_valueScope); inline;
      FUNCTION  cloneSafeValueStore(CONST oldStore:P_valueScope):P_valueScope;
  end;

  P_abstractRule=^T_abstractRule;
  T_abstractRule=object(T_objectWithIdAndLocation)
    private
      {$ifdef fullVersion}
      idResolved:boolean;
      {$endif}
      id:T_idString;
      declarationStart:T_tokenLocation;
      ruleType:T_ruleType;
    public
      CONSTRUCTOR create(CONST ruleId: T_idString; CONST startAt:T_tokenLocation; CONST ruleTyp:T_ruleType);
      DESTRUCTOR destroy; virtual;
      FUNCTION getId:T_idString; virtual;
      FUNCTION getRootId:T_idString; virtual;
      FUNCTION getLocation:T_tokenLocation; virtual;
      PROPERTY getRuleType:T_ruleType read ruleType;

      FUNCTION hasPublicSubrule:boolean; virtual; abstract;

      FUNCTION getCmdLineHelpText:T_arrayOfString; virtual;
      {$ifdef fullVersion}
      PROCEDURE setIdResolved; virtual;
      PROPERTY isIdResolved:boolean read idResolved;
      FUNCTION complainAboutUnused(CONST adapters:P_messages):boolean;
      FUNCTION getDocTxt:string; virtual; abstract;
      {$endif}
      PROCEDURE clearCache; virtual;
      PROCEDURE resolveIds({$WARN 5024 OFF}CONST adapters:P_messages); virtual;
      FUNCTION isReportable(OUT value:P_literal):boolean; virtual; abstract;
      FUNCTION replaces(CONST ruleTokenType:T_tokenType; CONST callLocation:T_tokenLocation; CONST param:P_listLiteral; OUT firstRep,lastRep:P_token; CONST context:P_abstractContext; VAR recycler:T_recycler):boolean; virtual; abstract;
      FUNCTION evaluateToBoolean(CONST ruleTokenType:T_tokenType; CONST callLocation:T_tokenLocation; CONST singleParameter:P_literal; VAR recycler:T_recycler; CONST context:P_abstractContext):boolean;
      FUNCTION getTypedef:P_typedef; virtual;
      FUNCTION getInlineValue:P_literal; virtual;
  end;

PROCEDURE noRecycler_disposeScope(VAR scope: P_valueScope);
IMPLEMENTATION
PROCEDURE T_recycler.initRecycler;
  begin
    tokens.fill:=0;
    scopeRecycler.fill:=0;
  end;

PROCEDURE T_recycler.cleanup;
  begin
    with tokens do while fill>0 do begin
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
  end;

FUNCTION T_recycler.disposeToken(p: P_token): P_token;
  begin
    if p=nil then result:=nil
    else begin
      result:=p^.next;
      with tokens do if (fill>=length(dat))
      then dispose(p,destroy)
      else begin
        p^.undefine;
        dat[fill]:=p;
        inc(fill);
      end;
    end;
  end;

PROCEDURE T_recycler.cascadeDisposeToken(VAR p: P_token);
  begin
    while p<>nil do p:=disposeToken(p);
  end;

FUNCTION T_recycler.newToken(CONST tokenLocation: T_tokenLocation;
  CONST tokenText: ansistring; CONST tokenType: T_tokenType; CONST ptr: pointer
  ): P_token;
  begin
    with tokens do if (fill>0) then begin dec(fill); result:=dat[fill]; end else new(result,create);
    result^.define(tokenLocation,tokenText,tokenType,ptr);
    result^.next:=nil;
  end;

FUNCTION T_recycler.newToken(CONST original: T_token): P_token;
  begin
    with tokens do if (fill>0) then begin dec(fill); result:=dat[fill]; end else new(result,create);
    result^.define(original);
    result^.next:=nil;
  end;

FUNCTION T_recycler.newToken(CONST original: P_token): P_token;
  begin
    with tokens do if (fill>0) then begin dec(fill); result:=dat[fill]; end else new(result,create);
    result^.define(original^);
    result^.next:=nil;
  end;

PROCEDURE noRecycler_disposeScope(VAR scope: P_valueScope);
  VAR parent:P_valueScope;
  begin
    if scope=nil then exit;
    if interlockedDecrement(scope^.refCount)>0 then begin
      scope:=nil;
      exit;
    end;
    parent:=scope^.parentScope;
    dispose(scope,destroy);
    noRecycler_disposeScope(parent);
    scope:=nil;
  end;

PROCEDURE T_recycler.disposeScope(VAR scope: P_valueScope);
  VAR parent:P_valueScope;
  begin
    if scope=nil then exit;
    if interlockedDecrement(scope^.refCount)>0 then begin
      scope:=nil;
      exit;
    end;
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

FUNCTION T_recycler.newValueScopeAsChildOf(CONST scope: P_valueScope; CONST parentAccess: AccessLevel): P_valueScope;
  begin
    with scopeRecycler do begin
      if (fill>0) then begin
        dec(fill);
        result:=dat[fill];
        result^.insteadOfCreate(scope,parentAccess);
      end else new(result,create(scope,parentAccess));
    end;
  end;

PROCEDURE T_recycler.scopePush(VAR scope: P_valueScope; CONST parentAccess: AccessLevel);
  VAR newScope:P_valueScope;
  begin
    with scopeRecycler do begin
      if (fill>0) then begin
        dec(fill);
        newScope:=dat[fill];
        newScope^.insteadOfCreate(scope,parentAccess);
      end else new(newScope,create(scope,parentAccess));
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
    result:=newValueScopeAsChildOf(oldStore^.parentScope,oldStore^.parentAccess);
    setLength(result^.variables,oldStore^.varFill);
    result^.varFill:=oldStore^.varFill;
    for k:=0 to oldStore^.varFill-1 do result^.variables[k]:=oldStore^.variables[k]^.clone;
  end;

CONSTRUCTOR T_abstractRule.create(CONST ruleId: T_idString; CONST startAt: T_tokenLocation; CONST ruleTyp: T_ruleType);
  begin
    {$ifdef fullVersion}
    idResolved:=false;
    {$endif}
    id              :=ruleId;
    declarationStart:=startAt;
    ruleType        :=ruleTyp;
  end;

DESTRUCTOR T_abstractRule.destroy;                    begin id:='';                   end;
FUNCTION T_abstractRule.getId: T_idString;            begin result:=id; end;
FUNCTION T_abstractRule.getRootId:T_idString;         begin result:=id; end;
FUNCTION T_abstractRule.getLocation: T_tokenLocation; begin result:=declarationStart; end;

FUNCTION T_abstractRule.getCmdLineHelpText: T_arrayOfString;
  begin
    result:=(C_ruleTypeText[getRuleType]+'rule '+getId+C_lineBreakChar+'in '+getLocation.package^.getPath);
  end;

{$ifdef fullVersion}
PROCEDURE T_abstractRule.setIdResolved;
  begin
    idResolved:=true;
  end;

FUNCTION T_abstractRule.complainAboutUnused(CONST adapters: P_messages): boolean;
  begin
    result:=(id<>MAIN_RULE_ID) and not(idResolved);
    if result then adapters^.postTextMessage(mt_el2_warning,lineLocation(declarationStart),
    'Unused rule '+id+
    '; you can suppress this warning with '+
    ATTRIBUTE_PREFIX+SUPPRESS_UNUSED_WARNING_ATTRIBUTE);
  end;
{$endif}

PROCEDURE T_abstractRule.clearCache; begin end;
PROCEDURE T_abstractRule.resolveIds(CONST adapters: P_messages); begin end;
FUNCTION T_abstractRule.evaluateToBoolean(CONST ruleTokenType:T_tokenType; CONST callLocation:T_tokenLocation; CONST singleParameter:P_literal; VAR recycler:T_recycler; CONST context:P_abstractContext):boolean;
  VAR parList:P_listLiteral;
      firstRep,lastRep:P_token;
  begin
    new(parList,create(1));
    parList^.append(singleParameter,true);
    if replaces(ruleTokenType,callLocation,parList,firstRep,lastRep,context,recycler)
    then begin
      result:=(firstRep<>     nil          ) and
              (firstRep^.next=nil          ) and
              (firstRep^.tokType=tt_literal) and
              (firstRep^.data=@boolLit[true]);
      recycler.cascadeDisposeToken(firstRep);
    end else result:=false;
    disposeLiteral(parList);
  end;

FUNCTION T_abstractRule.getTypedef:P_typedef;
  begin
    raise Exception.create('getTypeDef is not implemented for this rule type');
    result:=nil;
  end;

FUNCTION T_abstractRule.getInlineValue:P_literal;
  begin result:=nil; end;

end.
