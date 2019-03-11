TYPE
  P_secondaryOutputMemoMeta=^T_secondaryOutputMemoMeta;
  T_secondaryOutputMemoMeta=object(T_outputMemoMeta)
    adaptersHoldingCollector:P_messagesDistributor;
    collector:P_synOutAdapter;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context);
    FUNCTION evaluate(CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):boolean; virtual;
    PROCEDURE update; virtual;
    PROCEDURE connect; virtual;
    PROCEDURE disconnect; virtual;
    FUNCTION getName:string; virtual;
    DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_secondaryOutputMemoMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral; CONST location: T_tokenLocation;
  VAR context: T_context);
  begin
    inherited create(parent,def,location,context,[dmk_type]);
    state_hlLang:=fileTypeMeta[LANG_OUTPUT].extensions[0];
    synMeta.setLanguage(state_hlLang,synMeta.language);
    adaptersHoldingCollector:=P_messagesDistributor(context.messages);
    new(collector,create(parent^.containedInForm,synMeta.editor));
    connect;
  end;

FUNCTION T_secondaryOutputMemoMeta.evaluate(CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):boolean;
  begin
    result:=length(collector^.storedMessages)>0;
  end;

PROCEDURE T_secondaryOutputMemoMeta.update;
  begin
    collector^.flushToGui(true);
  end;

PROCEDURE T_secondaryOutputMemoMeta.connect;
  begin
    adaptersHoldingCollector^.addOutAdapter(collector,false);
    registerRedirector(collector);
  end;

PROCEDURE T_secondaryOutputMemoMeta.disconnect;
  begin
    adaptersHoldingCollector^.removeOutAdapter(collector);
    unregisterRedirector(collector);
  end;

DESTRUCTOR T_secondaryOutputMemoMeta.destroy;
  begin
    inherited destroy;
    disconnect;
    dispose(collector,destroy);
  end;

FUNCTION T_secondaryOutputMemoMeta.getName:string;
  begin
    result:='T_secondaryOutputMemoMeta';
  end;
