TYPE
  P_editMeta=^T_editMeta;
  T_editMeta=object(T_changeListeningMeta)
    editPanel:TPanel;
    editLabel:TLabel;
    edit:TEdit;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext);
    PROCEDURE update; virtual;
    FUNCTION leftLabelOrNil:TLabel; virtual;
    PROCEDURE OnKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE OnChange(Sender: TObject);
    FUNCTION getControl:TControl; virtual;
    FUNCTION getName:string; virtual;
  end;

CONSTRUCTOR T_editMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral;
                              CONST location: T_tokenLocation; VAR context: T_threadContext);
  begin
    inherited create(def,location,context,[dmk_type,dmk_action,dmk_caption,dmk_enabled,dmk_bind]);
    editPanel:=TPanel.create(parent^.winControl);
    editPanel.parent:=parent^.winControl;
    editPanel.Align:=alTop;
    editPanel.AutoSize:=true;

    editPanel.BorderWidth:=3;
    editPanel.BorderStyle:=bsNone;
    editPanel.BevelInner:=bvNone;
    editPanel.BevelOuter:=bvNone;
    editPanel.caption:='';

    editLabel:=TLabel.create(parent^.winControl);
    editLabel.parent:=editPanel;
    editLabel.Align:=alLeft;
    editLabel.caption:=state.caption;
    editLabel.AutoSize:=true;

    edit:=TEdit.create(parent^.winControl);
    edit.parent:=editPanel;
    edit.AnchorToNeighbour(akLeft,10,editLabel);
    edit.Align:=alClient;
    edit.AutoSize:=true;
    edit.enabled:=state.enabled;
    edit.text:='';
    if (config.action<>nil) then edit.OnKeyDown:=@OnKeyDown;
    if (config.bindingTo<>'') or (config_onChange<>nil) then begin
      edit.OnChange:=@OnChange;
      if state.bindingValue<>nil then begin
        if state.bindingValue^.literalType=lt_string
        then edit.text:=P_stringLiteral(state.bindingValue)^.value
        else edit.text:=                state.bindingValue^.toString();
      end;
    end;
    edit.OnKeyUp:=@OnKeyUp;
    edit.onEnter:=@onEnter;
    edit.onExit:=@onExit;
    parent^.add(@self);
  end;

PROCEDURE T_editMeta.update;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit;
    editLabel.caption:=state.caption;
    edit.enabled:=state.enabled;
    if (config.bindingTo<>'') and (state.bindingValue<>nil) and (state.bindingValue^.literalType=lt_string) then begin
      edit.caption:=P_stringLiteral(state.bindingValue)^.value;
    end;
    leaveCriticalSection(elementCs);
  end;

FUNCTION T_editMeta.leftLabelOrNil:TLabel;
  begin
    result:=editLabel;
  end;

PROCEDURE T_editMeta.OnKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key<>13) then exit;
    if config.action<>nil then postAction(newStringLiteral(TEdit(Sender).text));
  end;

PROCEDURE T_editMeta.OnChange(Sender: TObject);
  begin
    triggerChange(edit.text);
  end;

FUNCTION T_editMeta.getControl: TControl;
  begin
    result:=editPanel;
  end;

FUNCTION T_editMeta.getName:string;
  begin
    result:='T_editMeta "'+editLabel.caption+'"';
  end;

