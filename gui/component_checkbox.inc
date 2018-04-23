TYPE
  P_checkboxMeta=^T_checkboxMeta;
  T_checkboxMeta=object(T_guiElementMeta)
    checkbox:TCheckBox;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext);
    PROCEDURE update; virtual;
    PROCEDURE OnClick(Sender: TObject);
    FUNCTION getControl:TControl; virtual;
  end;

CONSTRUCTOR T_checkboxMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral;
                                  CONST location: T_tokenLocation; VAR context: T_threadContext);
  begin
    inherited create(def,location,context,[dmk_type,dmk_action,dmk_caption,dmk_enabled,dmk_bind]);
    checkbox:=TCheckBox.create(parent^.winControl);
    checkbox.parent:=parent^.winControl;
    checkbox.Align:=alTop;

    if (config.action<>nil) or (config.bindingTo<>'') then checkbox.OnClick:=@OnClick;
    checkbox.checked:=state.bindingValue = @boolLit[true];
    checkbox.OnKeyUp:=@OnKeyUp;
    checkbox.onEnter:=@onEnter;
    checkbox.onExit:=@onExit;
    parent^.add(@self);
  end;

PROCEDURE T_checkboxMeta.update;
  begin
    if state.evaluating then exit;
    checkbox.caption:=state.caption;
    checkbox.enabled:=state.enabled;
    if (config.bindingTo<>'') and (state.bindingValue<>nil) and (state.bindingValue^.literalType=lt_boolean) then begin
      checkbox.checked:=P_boolLiteral(state.bindingValue)^.value;
    end;
  end;

PROCEDURE T_checkboxMeta.OnClick(Sender: TObject);
  begin
    if state.evaluating then exit;
    if (config.bindingTo<>'') then begin
      if state.bindingValue<>nil then disposeLiteral(state.bindingValue);
      state.bindingValue:=newBoolLiteral(checkbox.checked);
      propagateCursor(TWinControl(getControl.GetTopParent),crHourGlass);
    end;
    postAction(newBoolLiteral(checkbox.checked));
  end;

FUNCTION T_checkboxMeta.getControl: TControl;
  begin
    result:=checkbox;
  end;

