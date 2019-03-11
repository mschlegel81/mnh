TYPE
  P_buttonMeta=^T_buttonMeta;
  T_buttonMeta=object(T_guiElementMeta)
    button:TButton;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context);
    PROCEDURE update; virtual;
    PROCEDURE OnClick(Sender: TObject);
    FUNCTION getControl:TControl; virtual;
    FUNCTION getName:string; virtual;
  end;

CONSTRUCTOR T_buttonMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral;
                                CONST location: T_tokenLocation; VAR context: T_context);
  begin
    inherited create(def,location,context,[dmk_type,dmk_action,dmk_caption,dmk_enabled]);
    button:=TButton.create(parent^.winControl);
    button.parent:=parent^.winControl;
    update;
    button.Align:=alTop;

    if config.action<>nil then button.OnClick:=@OnClick;
    parent^.add(@self);
  end;

PROCEDURE T_buttonMeta.update;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit;
    button.caption:=state.caption;
    button.enabled:=state.enabled;
    leaveCriticalSection(elementCs);
  end;

PROCEDURE T_buttonMeta.OnClick(Sender: TObject);
  begin
    postAction(nil);
  end;

FUNCTION T_buttonMeta.getControl: TControl;
  begin
    result:=button;
  end;

FUNCTION T_buttonMeta.getName:string;
  begin
    result:='T_buttonMeta "'+button.caption+'"';
  end;

