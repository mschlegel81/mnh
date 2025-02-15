TYPE
  P_comboboxMeta=^T_comboboxMeta;
  T_comboboxMeta=object(T_changeListeningMeta)
    comboboxPanel:TPanel;
    comboboxLabel:TLabel;
    combobox:TComboBox;
    config_items:P_expressionLiteral;
    state_items :T_arrayOfString;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST recycler:P_literalRecycler);
    FUNCTION leftLabelOrNil:TLabel; virtual;
    PROCEDURE update; virtual;
    PROCEDURE OnSelect(Sender: TObject);
    PROCEDURE OnChange(Sender: TObject);
    FUNCTION getControl:TControl; virtual;
    FUNCTION evaluate(CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):boolean; virtual;
    FUNCTION getName:string; virtual;
    DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_comboboxMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral;
                                  CONST location: T_tokenLocation; VAR context: T_context; CONST recycler:P_literalRecycler);
  VAR tmp:P_literal;
  begin
    inherited create(def,location,context,[dmk_type,dmk_action,dmk_caption,dmk_enabled,dmk_bind,dmk_items],recycler);
    config_items:=nil;
    tmp:=mapGet(def,key[dmk_items],recycler);
    if tmp<>nil then begin
      case tmp^.literalType of
        lt_expression: config_items:=P_expressionLiteral(tmp);
        lt_stringList,
        lt_emptyList: begin
          state_items :=P_listLiteral(tmp)^;
          recycler^.disposeLiteral(tmp);
        end
        else context.raiseError('items is: '+tmp^.typeString+'; must be stringList or expression',location);
      end;
    end;

    comboboxPanel:=TPanel.create(parent^.winControl);
    comboboxPanel.parent:=parent^.winControl;
    comboboxPanel.Align:=alTop;
    comboboxPanel.AutoSize:=true;

    comboboxPanel.BorderWidth:=3;
    comboboxPanel.BorderStyle:=bsNone;
    comboboxPanel.BevelInner:=bvNone;
    comboboxPanel.BevelOuter:=bvNone;
    comboboxPanel.caption:='';

    comboboxLabel:=TLabel.create(parent^.winControl);
    comboboxLabel.parent:=comboboxPanel;
    comboboxLabel.Align:=alLeft;
    comboboxLabel.caption:=state.caption;
    comboboxLabel.AutoSize:=true;

    combobox:=TComboBox.create(parent^.winControl);
    combobox.parent:=comboboxPanel;
    combobox.AnchorToNeighbour(akLeft,10,comboboxLabel);
    combobox.Align:=alClient;
    combobox.AutoSize:=true;
    combobox.enabled:=state.enabled;
    combobox.AutoSelect:=true;
    combobox.text:='';
    if (config.action<>nil) then combobox.OnSelect:=@OnSelect;
    if (config.bindingTo<>'') or (config_onChange<>nil) then begin
      combobox.OnChange:=@OnChange;
      if state.bindingValue<>nil then begin
        if state.bindingValue^.literalType=lt_string
        then combobox.text:=P_stringLiteral(state.bindingValue)^.value
        else combobox.text:=                state.bindingValue^.toString();
      end;
    end;
    combobox.onEnter:=@onEnter;
    combobox.onExit:=@onExit;
    parent^.add(@self);
  end;
FUNCTION T_comboboxMeta.leftLabelOrNil:TLabel;
  begin
    result:=comboboxLabel;
  end;

PROCEDURE T_comboboxMeta.update;
  VAR i:longint;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit;
    try
      combobox.enabled:=state.enabled;
      comboboxLabel.caption:=state.caption;

      while (combobox.items.count>length(state_items)) do combobox.items.delete(length(state_items));
      for i:=0 to combobox.items.count-1 do combobox.items[i]:=state_items[i];
      for i:=combobox.items.count to length(state_items)-1 do combobox.items.add(state_items[i]);

      if (config.bindingTo<>'') and (state.bindingValue<>nil) and (state.bindingValue^.literalType=lt_string) then begin
        combobox.text:=P_stringLiteral(state.bindingValue)^.value;
      end;
    finally
      leaveCriticalSection(elementCs);
    end;
  end;

PROCEDURE T_comboboxMeta.OnSelect(Sender: TObject);
  VAR recycler:P_recycler;
  begin
    recycler:=newRecycler;
    if config.action<>nil then postAction(recycler^.newStringLiteral(combobox.text));
    freeRecycler(recycler);
  end;

PROCEDURE T_comboboxMeta.OnChange(Sender: TObject);
  begin
    triggerChange(combobox.text);
  end;

FUNCTION T_comboboxMeta.getControl: TControl;
  begin
    result:=comboboxPanel;
  end;

FUNCTION T_comboboxMeta.evaluate(CONST location: T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler): boolean;
  VAR oldItems:T_arrayOfString;
      tmp:P_literal;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit(false);
    try
      result:=inherited evaluate(location,context,recycler);
      if config_items<>nil then begin
        oldItems:=state_items;
        tmp:=config_items^.evaluate(location,context,recycler).literal;
        if (tmp<>nil) then begin
          if tmp^.literalType in [lt_stringList,lt_emptyList]
          then state_items:=P_listLiteral(tmp)^
          else context^.raiseError('Expression returned '+tmp^.typeString+' as items; must be stringList',config_items^.getLocation);
          recycler^.disposeLiteral(tmp);
        end;
        result:=result or not(arrEquals(oldItems,state_items));
      end;
    finally
      leaveCriticalSection(elementCs);
    end;
  end;

FUNCTION T_comboboxMeta.getName:string;
  begin
    result:='T_comboboxMeta "'+comboboxLabel.caption+'"';
  end;

DESTRUCTOR T_comboboxMeta.destroy;
  VAR recycler:P_recycler;
  begin
    inherited destroy;
    recycler:=newRecycler;
    if config_items<>nil then recycler^.disposeLiteral(config_items);
    setLength(state_items,0);
    freeRecycler(recycler);
  end;

