TYPE
  P_buttonMeta=^T_buttonMeta;
  T_buttonMeta=object(T_guiElementMeta)
    button:TButton;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext);
    PROCEDURE update; virtual;
    PROCEDURE OnClick(Sender: TObject);
    FUNCTION getControl:TControl; virtual;
  end;

CONSTRUCTOR T_buttonMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral;
                                CONST location: T_tokenLocation; VAR context: T_threadContext);
  begin
    inherited create(def,location,context,[dmk_type,dmk_action,dmk_caption,dmk_enabled]);
    button:=TButton.create(parent^.winControl);
    button.parent:=parent^.winControl;
    update;
    button.Align:=alTop;

    if config.action<>nil then button.OnClick:=@OnClick;
    button.OnKeyUp:=@OnKeyUp;
    parent^.add(@self);
  end;

PROCEDURE T_buttonMeta.update;
  begin
    if state.evaluating then exit;
    button.caption:=state.caption;
    button.enabled:=state.enabled;
  end;

PROCEDURE T_buttonMeta.OnClick(Sender: TObject);
  begin
    if state.evaluating then exit;
    postAction(nil);
  end;

FUNCTION T_buttonMeta.getControl: TControl;
  begin
    result:=button;
  end;

