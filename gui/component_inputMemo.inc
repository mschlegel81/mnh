TYPE
  P_inputEditorMeta=^T_inputEditorMeta;
  T_inputEditorMeta=object(T_outputMemoMeta)
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext);
    PROCEDURE update; virtual;
    PROCEDURE OnChange(Sender: TObject);
    PROCEDURE OnKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  end;

CONSTRUCTOR T_inputEditorMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral; CONST location: T_tokenLocation; VAR context: T_threadContext);
  begin
    inherited create(parent,def,location,context,[dmk_type,dmk_highlight,dmk_bind,dmk_action]);
    synMeta.editor.readonly:=false;
    if (config.bindingTo<>'') or (config_onChange<>nil) then begin
      synMeta.editor.OnChange:=@OnChange;
      if state.bindingValue<>nil then begin
        if state.bindingValue^.literalType=lt_string
        then synMeta.editor.text:=P_stringLiteral(state.bindingValue)^.value
        else synMeta.editor.text:=                state.bindingValue^.toString();
      end;
    end;
    synMeta.editor.onEnter:=@onEnter;
    synMeta.editor.onExit:=@onExit;
    if (config.action<>nil) then synMeta.editor.OnKeyDown:=@OnKeyDown;
  end;

PROCEDURE T_inputEditorMeta.OnChange(Sender: TObject);
  begin
    if state.evaluating then exit;
    triggerChange(synMeta.editor.text);
  end;

PROCEDURE T_inputEditorMeta.OnKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if state.evaluating then exit;
    if (key<>13) or not(ssShift in Shift) then exit;
    postAction(newStringLiteral(synMeta.editor.text));
  end;

PROCEDURE T_inputEditorMeta.update;
  begin
    if state.evaluating then exit;
    synMeta.setLanguage(state_hlLang,synMeta.language);
    synMeta.editor.enabled:=state.enabled;
    synMeta.activate;
  end;

