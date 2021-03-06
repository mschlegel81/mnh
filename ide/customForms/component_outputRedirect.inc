TYPE
  P_secondaryOutputMemoMeta=^T_secondaryOutputMemoMeta;
  T_secondaryOutputMemoMeta=object(T_outputMemoMeta)
    adaptersHoldingCollector:P_messagesDistributor;
    synOutAdapter:P_eagerInitializedOutAdapter;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST recycler:P_literalRecycler);
    FUNCTION evaluate(CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):boolean; virtual;
    PROCEDURE update; virtual;
    PROCEDURE connect; virtual;
    PROCEDURE disconnect; virtual;
    FUNCTION getName:string; virtual;
    DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_secondaryOutputMemoMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral; CONST location: T_tokenLocation; VAR context: T_context; CONST recycler:P_literalRecycler);
  begin
    inherited create(parent,def,location,context,recycler,[dmk_type]);
    state_hlLang:=fileTypeMeta[LANG_OUTPUT].extensions[0];
    synMeta.setLanguageWithFallback(state_hlLang,synMeta.language);
    adaptersHoldingCollector:=P_messagesDistributor(context.messages);
    new(synOutAdapter,create(synMeta.editor,parent^.containedInForm));
  end;

FUNCTION T_secondaryOutputMemoMeta.evaluate(CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):boolean;
  begin
    result:=synOutAdapter^.fill>0;
  end;

PROCEDURE T_secondaryOutputMemoMeta.update;
  begin
  end;

PROCEDURE T_secondaryOutputMemoMeta.connect;
  begin
    adaptersHoldingCollector^.addOutAdapter(synOutAdapter,false);
    registerRedirector(synOutAdapter);
  end;

PROCEDURE T_secondaryOutputMemoMeta.disconnect;
  begin
    adaptersHoldingCollector^.removeOutAdapter(synOutAdapter);
    unregisterRedirector(synOutAdapter);
  end;

DESTRUCTOR T_secondaryOutputMemoMeta.destroy;
  begin
    inherited destroy;
    disconnect;
    dispose(synOutAdapter,destroy);
  end;

FUNCTION T_secondaryOutputMemoMeta.getName:string;
  begin
    result:='T_secondaryOutputMemoMeta';
  end;
