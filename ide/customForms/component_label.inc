TYPE
  P_labelMeta=^T_labelMeta;
  T_labelMeta=object(T_guiElementMeta)
    mylabel:TLabel;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context);
    PROCEDURE update; virtual;
    FUNCTION getControl:TControl; virtual;
    FUNCTION getName:string; virtual;
  end;

CONSTRUCTOR T_labelMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral;
                               CONST location: T_tokenLocation; VAR context: T_context);
  begin
    inherited create(def,location,context,[dmk_type,dmk_caption]);
    mylabel:=TLabel.create(parent^.winControl);
    mylabel.parent:=parent^.winControl;
    mylabel.Align:=alTop;
    parent^.add(@self);
  end;

PROCEDURE T_labelMeta.update;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit;
    mylabel.caption:=state.caption;
    mylabel.enabled:=state.enabled;
    leaveCriticalSection(elementCs);
  end;

FUNCTION T_labelMeta.getControl: TControl;
  begin
    result:=mylabel;
  end;

FUNCTION T_labelMeta.getName:string;
  begin
    result:='T_labelMeta "'+mylabel.caption+'"';
  end;
