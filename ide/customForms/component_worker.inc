TYPE
  P_workerMeta=^T_workerMeta;
  T_workerMeta=object(T_guiElementMeta)
    nextInvocation:double;
    interval:double;
    CONSTRUCTOR create(CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST recycler:P_literalRecycler);
    PROCEDURE update; virtual;
    FUNCTION evaluate(CONST location: T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler): boolean; virtual;
    FUNCTION getControl:TControl; virtual;
    FUNCTION getName:string; virtual;
  end;

CONSTRUCTOR T_workerMeta.create(CONST def: P_mapLiteral; CONST location: T_tokenLocation; VAR context: T_context; CONST recycler:P_literalRecycler);
  VAR tmp:P_literal;
  begin
    inherited create(def,location,context,recycler,[dmk_type,dmk_action,dmk_interval]);
    tmp:=mapGet(def,key[dmk_interval],recycler);
    interval:=0;
    if tmp<>nil then begin
      case tmp^.literalType of
        lt_smallint,lt_bigint,lt_real: interval:=P_numericLiteral(tmp)^.floatValue/(24*60*60);
        else context.raiseError('interval is: '+tmp^.typeString+'; must be boolean or expression',location);
      end;
    end;
    nextInvocation:=now+interval;
  end;

PROCEDURE T_workerMeta.update;
  begin
  end;

FUNCTION T_workerMeta.evaluate(CONST location: T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler): boolean;
  VAR tmp:P_literal;
  begin
    if (config.action<>nil) and (now>=nextInvocation) then begin
      tmp:=config.action^.evaluate(location,context,recycler).literal;
      if tmp<>nil then recycler^.disposeLiteral(tmp);
      nextInvocation:=now+interval;
      result:=true;
    end else result:=false;
  end;

FUNCTION T_workerMeta.getControl: TControl;
  begin
    result:=nil;
  end;

FUNCTION T_workerMeta.getName:string;
  begin
    result:='T_workerMeta';
  end;
