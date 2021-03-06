TYPE
  P_inputEditorMeta=^T_inputEditorMeta;
  T_inputEditorMeta=object(T_outputMemoMeta)
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST recycler:P_literalRecycler);
    PROCEDURE update; virtual;
    PROCEDURE OnChange(Sender: TObject);
    PROCEDURE OnKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    FUNCTION getName:string; virtual;
  end;

CONSTRUCTOR T_inputEditorMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral; CONST location: T_tokenLocation; VAR context: T_context; CONST recycler:P_literalRecycler);
  begin
    inherited create(parent,def,location,context,recycler,[dmk_type,dmk_highlight,dmk_bind,dmk_action]);
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
    triggerChange(synMeta.editor.text);
  end;

PROCEDURE T_inputEditorMeta.OnKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  VAR recycler:P_recycler;
  begin
    if (key<>13) or not(ssShift in Shift) then exit;
    recycler:=newRecycler;
    if config.action<>nil then postAction(recycler^.newStringLiteral(synMeta.editor.text));
    freeRecycler(recycler);
  end;

FUNCTION T_inputEditorMeta.getName:string;
  begin
    result:='T_inputEditorMeta';
  end;

PROCEDURE T_inputEditorMeta.update;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit;
    try
      synMeta.setLanguageWithFallback(state_hlLang,synMeta.language);
      synMeta.editor.enabled:=state.enabled;
    finally
      leaveCriticalSection(elementCs);
    end;
  end;

