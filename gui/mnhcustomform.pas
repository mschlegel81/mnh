UNIT mnhCustomForm;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, mnh_constants, mnh_basicTypes, mnh_contexts,
  mnh_litVar, mnhFormHandler,myGenerics,mnh_funcs,mnh_out_adapters,editorMetaBase;

TYPE
  T_definingMapKey=(dmk_type,dmk_action,dmk_caption,dmk_enabled,dmk_bind,dmk_items,dmk_parts,dmk_left,dmk_right,dmk_highlight);
  T_definingMapKeys=set of T_definingMapKey;

CONST
  key:array[T_definingMapKey] of string=(
    'type',
    'action',
    'caption',
    'enabled',
    'bind',
    'items',
    'parts',
    'left',
    'right',
    'highlight');

TYPE
  P_guiElementMeta=^T_guiElementMeta;
  P_panelMeta=^T_panelMeta;
  T_guiElementMeta=object
    config:record
      action :P_expressionLiteral;
      caption:P_expressionLiteral;
      enabled:P_expressionLiteral;
      bindingTo:T_idString;
    end;
    state:record
      actionTriggered:boolean;
      actionParameter:P_literal;
      caption:string;
      enabled:boolean;
      bindUpdateTriggered:boolean;
      bindingValue:P_literal;
      evaluating:boolean;
    end;

    CONSTRUCTOR create(CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext; CONST consideredKeys:T_definingMapKeys);
    PROCEDURE postAction(CONST param:P_literal);
    FUNCTION evaluate(CONST location:T_tokenLocation; VAR context:T_threadContext):boolean; virtual;
    PROCEDURE update; virtual; abstract;
    DESTRUCTOR destroy; virtual;
    FUNCTION getControl:TControl; virtual; abstract;
    FUNCTION preferClientAlignment:boolean; virtual;
    PROCEDURE OnKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  end;

  P_editMeta=^T_editMeta;
  T_editMeta=object(T_guiElementMeta)
    editPanel:TPanel;
    editLabel:TLabel;
    edit:TEdit;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext);
    PROCEDURE update; virtual;
    PROCEDURE OnKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE OnChange(Sender: TObject);
    FUNCTION getControl:TControl; virtual;
  end;

  P_labelMeta=^T_labelMeta;
  T_labelMeta=object(T_guiElementMeta)
    mylabel:TLabel;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext);
    PROCEDURE update; virtual;
    FUNCTION getControl:TControl; virtual;
  end;

  P_checkboxMeta=^T_checkboxMeta;
  T_checkboxMeta=object(T_guiElementMeta)
    checkbox:TCheckBox;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext);
    PROCEDURE update; virtual;
    PROCEDURE OnClick(Sender: TObject);
    FUNCTION getControl:TControl; virtual;
  end;

  P_buttonMeta=^T_buttonMeta;
  T_buttonMeta=object(T_guiElementMeta)
    button:TButton;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext);
    PROCEDURE update; virtual;
    PROCEDURE OnClick(Sender: TObject);
    FUNCTION getControl:TControl; virtual;
  end;

  P_outputMemoMeta=^T_outputMemoMeta;
  T_outputMemoMeta=object(T_guiElementMeta)
    synMeta:T_basicEditorMeta;
    config_hlLang:P_expressionLiteral;
    state_hlLang :string;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext; CONST consideredKeys:T_definingMapKeys=[dmk_type,dmk_caption,dmk_highlight]);
    PROCEDURE update; virtual;
    FUNCTION evaluate(CONST location:T_tokenLocation; VAR context:T_threadContext):boolean; virtual;
    FUNCTION getControl:TControl; virtual;
    FUNCTION preferClientAlignment:boolean; virtual;
    DESTRUCTOR destroy; virtual;
  end;

  P_inputEditorMeta=^T_inputEditorMeta;
  T_inputEditorMeta=object(T_outputMemoMeta)
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext);
    PROCEDURE update; virtual;
    PROCEDURE OnChange(Sender: TObject);
    PROCEDURE OnKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  end;

  P_comboboxMeta=^T_comboboxMeta;
  T_comboboxMeta=object(T_guiElementMeta)
    comboboxPanel:TPanel;
    comboboxLabel:TLabel;
    combobox:TComboBox;
    config_items:P_expressionLiteral;
    state_items :T_arrayOfString;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext);
    PROCEDURE update; virtual;
    PROCEDURE OnChange(Sender: TObject);
    FUNCTION getControl:TControl; virtual;
    FUNCTION evaluate(CONST location:T_tokenLocation; VAR context:T_threadContext):boolean; virtual;
    DESTRUCTOR destroy; virtual;
  end;

  T_panelMeta=object(T_guiElementMeta)
    elements:array of P_guiElementMeta;
    lastControl:TControl;
    winControl:TWinControl;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext; CONST consideredKeys:T_definingMapKeys=[dmk_type,dmk_parts]);
    CONSTRUCTOR createForExistingForm(CONST form:TForm);
    PROCEDURE add(CONST meta:P_guiElementMeta);
    PROCEDURE update; virtual;
    FUNCTION getControl:TControl; virtual;
    PROCEDURE alignContents; virtual;
    DESTRUCTOR destroy; virtual;
  end;

  P_splitPanelMeta=^T_splitPanelMeta;
  T_splitPanelMeta=object(T_panelMeta)
    Left,Right:T_panelMeta;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext);
    PROCEDURE update; virtual;
    PROCEDURE alignContents; virtual;
    DESTRUCTOR destroy; virtual;
    PROCEDURE OnResize(Sender:TObject);
  end;

  TscriptedForm = class(TForm)
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  private
    setupParam:P_literal;
    setupLocation:T_tokenLocation;
    setupContext:P_threadContext;
    meta:array of P_guiElementMeta;
    displayPending:boolean;
    lock:TRTLCriticalSection;
    processingEvents:boolean;
    PROCEDURE initialize();
  public
    PROCEDURE processPendingEvents(CONST location: T_tokenLocation; VAR context: T_threadContext);
    PROCEDURE updateComponents;
    PROCEDURE conditionalShow(VAR adapters: T_adapters);
  end;

PROCEDURE freeScriptedForms;
PROCEDURE conditionalShowCustomForms(VAR adapters:T_adapters);
IMPLEMENTATION
VAR scriptedFormCs:TRTLCriticalSection;
    scriptedForm: TscriptedForm;
{$R *.lfm}

PROCEDURE propagateCursor(CONST c:TWinControl; CONST Cursor:TCursor);
  VAR i:longint;
  begin
    for i:=0 to c.ControlCount-1 do begin
      c.Controls[i].Cursor:=Cursor;
      if c.Controls[i] is TWinControl then propagateCursor(TWinControl(c.Controls[i]),Cursor);
    end;
  end;

PROCEDURE conditionalShowCustomForms(VAR adapters:T_adapters);
  begin
    enterCriticalSection(scriptedFormCs);
    if scriptedForm<>nil then scriptedForm.conditionalShow(adapters);
    leaveCriticalSection(scriptedFormCs);
  end;

FUNCTION createScriptedForm(CONST title:string; CONST definition:P_literal; CONST context:P_threadContext; CONST errorLocation:T_tokenLocation):TscriptedForm;
  begin
    enterCriticalSection(scriptedFormCs);
    if scriptedForm<>nil then begin
      unregisterForm(scriptedForm);
      FreeAndNil(scriptedForm);
    end;
    scriptedForm:=TscriptedForm.create(nil);
    registerForm(scriptedForm,false,true);
    scriptedForm.setupLocation:=errorLocation;
    scriptedForm.setupParam:=definition;
    scriptedForm.setupContext:=context;
    scriptedForm.caption:=title;
    definition^.rereferenced;
    scriptedForm.displayPending:=true;
    result:=scriptedForm;
    leaveCriticalSection(scriptedFormCs);
  end;

PROCEDURE freeScriptedForms;
  begin
    enterCriticalSection(scriptedFormCs);
    if scriptedForm<>nil then begin
      unregisterForm(scriptedForm);
      FreeAndNil(scriptedForm);
    end;
    leaveCriticalSection(scriptedFormCs);
  end;

FUNCTION mapGet(CONST map:P_mapLiteral; CONST key:string):P_literal;
  VAR keyLit:P_literal;
  begin
    keyLit:=newSingletonString(key);
    result:=map^.get(keyLit);
    disposeLiteral(keyLit);
    if result^.literalType=lt_void then disposeLiteral(result);
  end;

CONSTRUCTOR T_splitPanelMeta.create(CONST parent: P_panelMeta;
  CONST def: P_mapLiteral; CONST location: T_tokenLocation;
  VAR context: T_threadContext);
  begin
    inherited create(parent,def,location,context,[dmk_type,dmk_left,dmk_right]);
    Left .create(@self,def,location,context,[dmk_type,dmk_left,dmk_right]); Left.winControl.Align:=alLeft;
    Right.create(@self,def,location,context,[dmk_type,dmk_left,dmk_right]); Right.winControl.Align:=alClient;
    getControl.OnResize:=@OnResize;
  end;

PROCEDURE T_splitPanelMeta.update; begin end;

PROCEDURE T_splitPanelMeta.alignContents;
  begin
    Left.alignContents;
    Right.alignContents;
  end;

DESTRUCTOR T_splitPanelMeta.destroy;
  begin
    Left .destroy;
    Right.destroy;
    inherited destroy;
  end;

PROCEDURE T_splitPanelMeta.OnResize(Sender: TObject);
  VAR leftW :longint=0;
      rightW:longint=0;
      totalW:longint=0;
      dummy :longint=0;
  begin
    if state.evaluating then exit;
    Left .getControl.AutoSize:=true;
    Left .getControl.GetPreferredSize(leftW ,dummy);
    Right.getControl.GetPreferredSize(rightW,dummy);

    totalW:=winControl.width;
    //leftWidth / totalW = leftW /(leftW+rightW)
    //leftWidth          = leftW /(leftW+rightW) * totalW
    Left .getControl.AutoSize:=false;
    Left .getControl.width:=round(leftW/(leftW+rightW)*totalW);
  end;

CONSTRUCTOR T_guiElementMeta.create(CONST def: P_mapLiteral;
                                    CONST location: T_tokenLocation; VAR context: T_threadContext;
                                    CONST consideredKeys: T_definingMapKeys);
  PROCEDURE raiseUnusedKeyWarning;
    FUNCTION isConsidered(CONST k:P_literal):boolean;
      VAR c:T_definingMapKey;
      begin
        if k^.literalType<>lt_string then exit(false);
        result:=false;
        for c in consideredKeys do if key[c]=P_stringLiteral(k)^.value then exit(true);
      end;

    VAR keys:T_arrayOfLiteral;
        k:P_literal;
    begin
      if not(context.adapters^.noErrors) then exit;
      keys:=def^.keyIteratableList;
      for k in keys do if not(isConsidered(k)) then begin
        context.adapters^.raiseWarning('Key '+k^.toString()+' is ignored in '+def^.toString(),location);
      end;
      disposeLiteral(keys);
    end;

  VAR tmp:P_literal;
  begin
    with config do begin
      action :=nil;
      caption:=nil;
      enabled:=nil;
      bindingTo:='';
    end;
    with state do begin
      actionTriggered:=false;
      actionParameter:=nil;
      caption        :='';
      enabled        :=true;
      bindUpdateTriggered:=false;
      evaluating:=false;
    end;

    tmp:=mapGet(def,key[dmk_action]);
    if tmp<>nil then begin
      if (tmp^.literalType=lt_expression)
      then config.action:=P_expressionLiteral(tmp)
      else context.adapters^.raiseError('action is: '+tmp^.typeString+'; must be expression',location);
    end;

    tmp:=mapGet(def,key[dmk_caption]);
    if tmp<>nil then begin
      case tmp^.literalType of
        lt_expression: config.caption:=P_expressionLiteral(tmp);
        lt_string    : state .caption:=P_stringLiteral(tmp)^.value;
        else context.adapters^.raiseError('caption is: '+tmp^.typeString+'; must be string or expression',location);
      end;
    end;

    tmp:=mapGet(def,key[dmk_enabled]);
    if tmp<>nil then begin
      case tmp^.literalType of
        lt_expression: config.enabled:=P_expressionLiteral(tmp);
        lt_boolean   : state .enabled:=P_boolLiteral(tmp)^.value;
        else context.adapters^.raiseError('enabled is: '+tmp^.typeString+'; must be boolean or expression',location);
      end;
    end;

    tmp:=mapGet(def,key[dmk_bind]);
    if tmp<>nil then begin
      if tmp^.literalType=lt_string then begin
        config.bindingTo:=P_stringLiteral(tmp)^.value;
        state.bindingValue:=context.valueStore^.getVariableValue(config.bindingTo);
      end else context.adapters^.raiseError('bind is: '+tmp^.typeString+'; must the identifier of a local variable as string',location);
      disposeLiteral(tmp);
    end;

    raiseUnusedKeyWarning;
  end;

PROCEDURE T_guiElementMeta.postAction(CONST param: P_literal);
  begin
    if state.evaluating then exit;
    if config.action=nil then exit;
    if state.actionParameter<>nil then disposeLiteral(state.actionParameter);
    state.actionParameter:=param;
    state.actionTriggered:=true;
    propagateCursor(TWinControl(getControl.GetTopParent),crHourGlass);
  end;

FUNCTION T_guiElementMeta.evaluate(CONST location: T_tokenLocation;
  VAR context: T_threadContext): boolean;
  VAR tmp:P_literal;
      oldEnabled:boolean;
      oldCaption:string;
  begin
    state.evaluating:=true;
    result:=false;
    if state.actionTriggered then begin
      tmp:=config.action^.evaluateToLiteral(location,@context,state.actionParameter);
      if state.actionParameter<>nil then disposeLiteral(state.actionParameter);
      if tmp                  <>nil then disposeLiteral(tmp);
      state.actionTriggered:=false;
      result:=true;
    end;

    if config.enabled<>nil then begin
      oldEnabled:=state.enabled;
      state.enabled:=config.enabled^.evaluateToBoolean(location,@context);
      result:=result or (oldEnabled<>state.enabled);
    end;

    if config.caption<>nil then begin
      oldCaption:=state.caption;
      tmp:=config.caption^.evaluateToLiteral(location,@context);
      if tmp<>nil then begin
        if tmp^.literalType=lt_string
        then state.caption:=P_stringLiteral(tmp)^.value
        else state.caption:=tmp^.toString();
        disposeLiteral(tmp);
      end;
      result:=result or (oldCaption<>state.caption);
    end;

    if (config.bindingTo<>'') then begin
      if (state.bindingValue<>nil) and (state.bindUpdateTriggered) then begin
        tmp:=context.valueStore^.getVariableValue(config.bindingTo);
        if tmp=nil then begin
          result:=true;
          context.valueStore^.setVariableValue(config.bindingTo,state.bindingValue,location,context.adapters);
        end else begin
          if tmp^.equals(state.bindingValue) then begin
            disposeLiteral(tmp);
          end else begin
            result:=true;
            disposeLiteral(tmp);
            context.valueStore^.setVariableValue(config.bindingTo,state.bindingValue,location,context.adapters);
          end;
        end;
        state.bindUpdateTriggered:=false;
      end else if not(state.bindUpdateTriggered) then begin
        tmp:=context.valueStore^.getVariableValue(config.bindingTo);
        if tmp<>nil then begin
          if tmp^.equals(state.bindingValue)
          then  disposeLiteral(tmp)
          else begin
            disposeLiteral(state.bindingValue);
            state.bindingValue:=tmp;
            result:=true;
          end;
        end;
      end;
    end;
    state.evaluating:=false;
  end;

DESTRUCTOR T_guiElementMeta.destroy;
  begin
    if config.action  <>nil then disposeLiteral(config.action );
    if config.caption <>nil then disposeLiteral(config.caption);
    if config.enabled <>nil then disposeLiteral(config.enabled);
    state.actionTriggered:=false;
    if state.actionParameter<>nil then disposeLiteral(state.actionParameter);
    if state.bindingValue   <>nil then disposeLiteral(state.bindingValue);
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
    edit.enabled:=state.enabled;
    edit.AutoSize:=true;
    edit.text:='';
    if (config.action<>nil) then edit.OnKeyDown:=@OnKeyDown;
    if (config.bindingTo<>'') then begin
      edit.OnChange:=@OnChange;
      if state.bindingValue<>nil then begin
        if state.bindingValue^.literalType=lt_string
        then edit.text:=P_stringLiteral(state.bindingValue)^.value
        else edit.text:=                state.bindingValue^.toString();
      end;
    end;
    edit.OnKeyUp:=@OnKeyUp;
    parent^.add(@self);
  end;

PROCEDURE T_editMeta.OnKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if state.evaluating then exit;
    if (key<>13) then exit;
    postAction(newStringLiteral(TEdit(Sender).text));
  end;

PROCEDURE T_editMeta.OnChange(Sender: TObject);
  begin
    if state.evaluating then exit;
    if (config.bindingTo<>'') then begin
      state.bindUpdateTriggered:=true;
      if state.bindingValue<>nil then disposeLiteral(state.bindingValue);
      state.bindingValue:=newStringLiteral(edit.text);
      propagateCursor(TWinControl(getControl.GetTopParent),crHourGlass);
    end;
  end;

FUNCTION T_editMeta.getControl: TControl;
  begin
    result:=editPanel;
  end;

FUNCTION T_guiElementMeta.preferClientAlignment: boolean;
  begin
    result:=false;
  end;

PROCEDURE T_guiElementMeta.OnKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if state.evaluating then exit;
    if (key=9) and (Sender is TControl) and (ssCtrl in Shift) then formCycle(TForm(TControl(Sender).GetTopParent),ssShift in Shift);
  end;

PROCEDURE T_editMeta.update;
  begin
    if state.evaluating then exit;
    editLabel.caption:=state.caption;
    edit.enabled:=state.enabled;
    if (config.bindingTo<>'') and (state.bindingValue<>nil) and (state.bindingValue^.literalType=lt_string) then begin
      edit.caption:=P_stringLiteral(state.bindingValue)^.value;
    end;
  end;

CONSTRUCTOR T_labelMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral;
                               CONST location: T_tokenLocation; VAR context: T_threadContext);
  begin
    inherited create(def,location,context,[dmk_type,dmk_caption]);
    mylabel:=TLabel.create(parent^.winControl);
    mylabel.parent:=parent^.winControl;
    mylabel.Align:=alTop;
    parent^.add(@self);
  end;

PROCEDURE T_labelMeta.update;
  begin
    if state.evaluating then exit;
    mylabel.caption:=state.caption;
    mylabel.enabled:=state.enabled;
  end;

FUNCTION T_labelMeta.getControl: TControl;
  begin
    result:=mylabel;
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
      state.bindUpdateTriggered:=true;
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

CONSTRUCTOR T_inputEditorMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral; CONST location: T_tokenLocation; VAR context: T_threadContext);
  begin
    inherited create(parent,def,location,context,[dmk_type,dmk_highlight,dmk_bind,dmk_action]);
    synMeta.editor.readonly:=false;
    if (config.bindingTo<>'') then begin
      synMeta.editor.OnChange:=@OnChange;
      if state.bindingValue<>nil then begin
        if state.bindingValue^.literalType=lt_string
        then synMeta.editor.text:=P_stringLiteral(state.bindingValue)^.value
        else synMeta.editor.text:=                state.bindingValue^.toString();
      end;
    end;
    if (config.action<>nil) then synMeta.editor.OnKeyDown:=@OnKeyDown;
  end;

PROCEDURE T_inputEditorMeta.OnChange(Sender: TObject);
  begin
    if state.evaluating then exit;
    if (config.bindingTo<>'') then begin
      state.bindUpdateTriggered:=true;
      if state.bindingValue<>nil then disposeLiteral(state.bindingValue);
      state.bindingValue:=newStringLiteral(synMeta.editor.text);
      propagateCursor(TWinControl(getControl.GetTopParent),crHourGlass);
    end;
  end;

PROCEDURE T_inputEditorMeta.OnKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if state.evaluating then exit;
    if (key<>13) or not(ssShift in Shift) then exit;
    postAction(newStringLiteral(synMeta.editor.text));
  end;

CONSTRUCTOR T_outputMemoMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral;
                                    CONST location: T_tokenLocation; VAR context: T_threadContext;
                                    CONST consideredKeys:T_definingMapKeys=[dmk_type,dmk_caption,dmk_highlight]);
  VAR langLit:P_literal;
  begin
    inherited create(def,location,context,consideredKeys);
    synMeta.createWithParent(parent^.winControl);
    synMeta.editor.Align:=alTop;
    synMeta.editor.readonly:=true;
    synMeta.editor.OnKeyUp:=@OnKeyUp;
    synMeta.activate;
    state_hlLang:='';
    config_hlLang:=nil;

    langLit:=mapGet(def,key[dmk_highlight]);
    if langLit<>nil then begin
      case langLit^.literalType of
        lt_string: begin
          state_hlLang:=P_stringLiteral(langLit)^.value;
          synMeta.setLanguage(state_hlLang,LANG_TXT);
        end;
        lt_expression: config_hlLang:=P_expressionLiteral(langLit^.rereferenced);
        else context.adapters^.raiseError('highlighter is: '+langLit^.typeString+'; must be string or expression',location);
      end;
      disposeLiteral(langLit);
    end else synMeta.language:=LANG_TXT;
    parent^.add(@self);
  end;

PROCEDURE T_outputMemoMeta.update;
  begin
    synMeta.editor.text:=state.caption;
    synMeta.editor.enabled:=state.enabled;
    synMeta.setLanguage(state_hlLang,synMeta.language);
    synMeta.activate;
  end;

PROCEDURE T_inputEditorMeta.update;
  begin
    if state.evaluating then exit;
    synMeta.setLanguage(state_hlLang,synMeta.language);
    synMeta.editor.enabled:=state.enabled;
    synMeta.activate;
  end;

FUNCTION T_outputMemoMeta.getControl: TControl;
  begin
    result:=synMeta.editor;
  end;

FUNCTION T_outputMemoMeta.preferClientAlignment: boolean;
  begin
    result:=true;
  end;

DESTRUCTOR T_outputMemoMeta.destroy;
  begin
    synMeta.destroy;
    if config_hlLang<>nil then disposeLiteral(config_hlLang);
  end;

OPERATOR:=(x:T_listLiteral):T_arrayOfString;
  VAR i:longint;
  begin
    setLength(result,x.size);
    for i:=0 to length(result)-1 do begin
      if x.value[i]^.literalType=lt_string
      then result[i]:=P_stringLiteral(x.value[i])^.value
      else result[i]:=                x.value[i]^.toString();
    end;
  end;

CONSTRUCTOR T_comboboxMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral;
                                  CONST location: T_tokenLocation; VAR context: T_threadContext);
  VAR tmp:P_literal;
  begin
    inherited create(def,location,context,[dmk_type,dmk_action,dmk_caption,dmk_enabled,dmk_bind,dmk_items]);
    config_items:=nil;
    tmp:=mapGet(def,key[dmk_items]);
    if tmp<>nil then begin
      case tmp^.literalType of
        lt_expression: config_items:=P_expressionLiteral(tmp);
        lt_stringList,
        lt_emptyList: state_items :=P_listLiteral(tmp)^;
        else context.adapters^.raiseError('items is: '+tmp^.typeString+'; must be stringList or expression',location);
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
    combobox.Align:=alClient;
    combobox.AutoSize:=true;
    combobox.AnchorToNeighbour(akLeft,10,comboboxLabel);
    combobox.enabled:=state.enabled;
    combobox.text:='';
    if (config.action<>nil) then combobox.OnSelect:=@OnChange;
    if (config.bindingTo<>'') then begin
      combobox.OnSelect:=@OnChange;
      if state.bindingValue<>nil then begin
        if state.bindingValue^.literalType=lt_string
        then combobox.text:=P_stringLiteral(state.bindingValue)^.value
        else combobox.text:=                state.bindingValue^.toString();
      end;
    end;
    combobox.OnKeyUp:=@OnKeyUp;
    parent^.add(@self);
  end;

PROCEDURE T_comboboxMeta.update;
  VAR i:longint;
  begin
    if state.evaluating then exit;
    combobox.enabled:=state.enabled;
    comboboxLabel.caption:=state.caption;

    while (combobox.items.count>length(state_items)) do combobox.items.delete(length(state_items));
    for i:=0 to combobox.items.count-1 do combobox.items[i]:=state_items[i];
    for i:=combobox.items.count to length(state_items)-1 do combobox.items.add(state_items[i]);

    if (config.bindingTo<>'') and (state.bindingValue<>nil) and (state.bindingValue^.literalType=lt_string) then begin
      combobox.text:=P_stringLiteral(state.bindingValue)^.value;
    end;
  end;

PROCEDURE T_comboboxMeta.OnChange(Sender: TObject);
  begin
    if state.evaluating then exit;
    if (config.bindingTo<>'') then begin
      state.bindUpdateTriggered:=true;
      if state.bindingValue<>nil then disposeLiteral(state.bindingValue);
      state.bindingValue:=newStringLiteral(combobox.text);
      propagateCursor(TWinControl(getControl.GetTopParent),crHourGlass);
    end;
    postAction(newStringLiteral(combobox.text));
  end;

FUNCTION T_comboboxMeta.getControl: TControl;
  begin
    result:=comboboxPanel;
  end;

FUNCTION T_outputMemoMeta.evaluate(CONST location: T_tokenLocation; VAR context: T_threadContext): boolean;
  VAR tmp:P_literal;
      oldLang:string;
  begin
    result:=inherited evaluate(location,context);
    state.evaluating:=true;
    if config_hlLang<>nil then begin
      tmp:=config_hlLang^.evaluateToLiteral(location,@context);
      if tmp<>nil then begin
        if tmp^.literalType=lt_string then begin
          oldLang:=state_hlLang;
          state_hlLang:=P_stringLiteral(tmp)^.value;
          result:=result or (oldLang<>state_hlLang);
        end else context.adapters^.raiseError( 'Cannot evaluate expression to String '+config_hlLang^.toString(50)+'; returned type is '+tmp^.typeString,config_hlLang^.getLocation);
        disposeLiteral(tmp);
      end;
    end;
    state.evaluating:=false;
  end;

FUNCTION T_comboboxMeta.evaluate(CONST location: T_tokenLocation; VAR context: T_threadContext): boolean;
  VAR oldItems:T_arrayOfString;
      tmp:P_literal;
  begin
    result:=inherited evaluate(location,context);
    state.evaluating:=true;
    if config_items<>nil then begin
      oldItems:=state_items;
      tmp:=config.caption^.evaluateToLiteral(location,@context);
      if (tmp<>nil) then begin
        if tmp^.literalType in [lt_stringList,lt_emptyList] then begin
          state_items:=P_listLiteral(tmp)^;
        end else context.adapters^.raiseError('Expression returned '+tmp^.typeString+' as items; must be stringList',location);
        disposeLiteral(tmp);
      end;
      result:=result or not(arrEquals(oldItems,state_items));
    end;
    state.evaluating:=false;
  end;

DESTRUCTOR T_comboboxMeta.destroy;
  begin
    inherited destroy;
    if config_items<>nil then disposeLiteral(config_items);
    setLength(state_items,0);
  end;

CONSTRUCTOR T_panelMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral;
                               CONST location: T_tokenLocation; VAR context: T_threadContext;
                               CONST consideredKeys:T_definingMapKeys);
  VAR newPanel:TPanel;
  begin
    inherited create(def,location,context,consideredKeys);
    newPanel:=TPanel.create(parent^.winControl);
    newPanel.parent:=parent^.winControl;
    newPanel.Align:=alTop;
    newPanel.AutoSize:=true;
    newPanel.BorderWidth:=3;
    newPanel.BorderStyle:=bsNone;
    newPanel.BevelInner:=bvNone;
    newPanel.BevelOuter:=bvNone;
    newPanel.caption:='';
    setLength(elements,0);
    lastControl:=nil;
    winControl:=newPanel;
    parent^.add(@self);
  end;

CONSTRUCTOR T_panelMeta.createForExistingForm(CONST form: TForm);
  begin
    setLength(elements,0);
    lastControl:=nil;
    winControl:=form;
  end;

PROCEDURE T_panelMeta.add(CONST meta: P_guiElementMeta);
  begin
    if lastControl<>nil then begin
      meta^.getControl.top:=lastControl.top+lastControl.height+10;
    end;
    setLength(elements,length(elements)+1);
    elements[length(elements)-1]:=meta;
    lastControl:=meta^.getControl;
  end;

PROCEDURE T_panelMeta.update; begin end;

FUNCTION T_panelMeta.getControl: TControl;
  begin
    result:=winControl;
  end;

PROCEDURE T_panelMeta.alignContents;
  VAR lastClient:longint;
      k:longint;
  begin
    lastClient:=length(elements)-1;
    while (lastClient>=0) and not(elements[lastClient]^.preferClientAlignment) do dec(lastClient);
    if lastClient<0 then exit;
    for k:=length(elements)-1 downto lastClient+1 do elements[k]^.getControl.Align:=alBottom;
    elements[lastClient]^.getControl.Align:=alClient;
    elements[lastClient]^.getControl.AutoSize:=true;
  end;

DESTRUCTOR T_panelMeta.destroy;
  begin
    inherited destroy;
    setLength(elements,0);
  end;

PROCEDURE TscriptedForm.FormCreate(Sender: TObject);
  begin
    initCriticalSection(lock);
    enterCriticalSection(lock);
    setLength(meta,0);
    displayPending:=false;
    leaveCriticalSection(lock);
  end;

PROCEDURE TscriptedForm.FormDestroy(Sender: TObject);
  VAR k:longint;
  begin
    enterCriticalSection(lock);
    for k:=0 to length(meta)-1 do dispose(meta[k],destroy);
    setLength(meta,0);
    displayPending:=false;
    leaveCriticalSection(lock);
    doneCriticalSection(lock);
  end;

PROCEDURE TscriptedForm.FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=9) and (ssCtrl in Shift) then formCycle(self,ssShift in Shift);
  end;

PROCEDURE TscriptedForm.initialize();
  TYPE  T_componentType=(tc_error,tc_button,tc_label,tc_checkbox,tc_textBox,tc_panel,tc_splitPanel,tc_inputEditor,tc_outputEditor,tc_comboBox);
  CONST C_componentType:array[T_componentType] of string=('','button','label','checkbox','edit','panel','splitPanel','inputEditor','outputEditor','comboBox');

  FUNCTION componentTypeOf(CONST def:P_mapLiteral):T_componentType;
    VAR tc:T_componentType;
        componentTypeLiteral:P_literal;
    begin
      componentTypeLiteral:=mapGet(def,key[dmk_type]);
      if (componentTypeLiteral=nil) or (componentTypeLiteral^.literalType<>lt_string) then begin
        setupContext^.adapters^.raiseError('Missing "type" entry in '+def^.toString(100),setupLocation);
        if componentTypeLiteral<>nil then disposeLiteral(componentTypeLiteral);
        exit(tc_error);
      end;
      result:=tc_error;
      for tc in T_componentType do if C_componentType[tc]=P_stringLiteral(componentTypeLiteral)^.value then result:=tc;
      if result=tc_error then
        setupContext^.adapters^.raiseError('Invalid type: '+componentTypeLiteral^.toString()+'; must be one of ["panel","button","edit","comboBox","label","inputEditor","outputEditor","checkbox","splitPanel"]',setupLocation);
      disposeLiteral(componentTypeLiteral);
    end;

  PROCEDURE initComponent(CONST container:T_panelMeta; CONST def:P_literal);
    VAR labelMeta   :P_labelMeta;
        buttonMeta  :P_buttonMeta;
        checkboxMeta:P_checkboxMeta;
        editMeta    :P_editMeta;
        inputEditM  :P_inputEditorMeta;
        outputEditM :P_outputMemoMeta;
        comboMeta   :P_comboboxMeta;
        newPanel    :P_panelMeta;
        splitPanel  :P_splitPanelMeta;

    PROCEDURE addPanelContents(VAR targetPanel:T_panelMeta; panelContents:P_literal);
      VAR iter:T_arrayOfLiteral;
      begin
        if panelContents=nil then begin end
        else if panelContents^.literalType=lt_list then begin
          iter:=P_listLiteral(panelContents)^.iteratableList;
          disposeLiteral(panelContents);
          for panelContents in iter do if setupContext^.adapters^.noErrors then initComponent(targetPanel,panelContents);
          disposeLiteral(iter);
        end else begin
          setupContext^.adapters^.raiseError('Invalid panel parts type: '+panelContents^.typeString+'; must be a list; context='+panelContents^.toString(100),setupLocation);
          disposeLiteral(panelContents);
        end;
      end;

    PROCEDURE addMeta(CONST m:P_guiElementMeta);
      begin
        setLength(meta,length(meta)+1);
        meta[length(meta)-1]:=m;
      end;

    begin
      if def^.literalType in C_mapTypes then case componentTypeOf(P_mapLiteral(def)) of
        tc_label:        begin new(labelMeta   ,create(@container,P_mapLiteral(def),setupLocation,setupContext^)); addMeta(labelMeta   ); end;
        tc_button:       begin new(buttonMeta  ,create(@container,P_mapLiteral(def),setupLocation,setupContext^)); addMeta(buttonMeta  ); end;
        tc_checkbox:     begin new(checkboxMeta,create(@container,P_mapLiteral(def),setupLocation,setupContext^)); addMeta(checkboxMeta); end;
        tc_textBox:      begin new(editMeta    ,create(@container,P_mapLiteral(def),setupLocation,setupContext^)); addMeta(editMeta    ); end;
        tc_inputEditor : begin new(inputEditM  ,create(@container,P_mapLiteral(def),setupLocation,setupContext^)); addMeta(inputEditM  ); end;
        tc_outputEditor: begin new(outputEditM ,create(@container,P_mapLiteral(def),setupLocation,setupContext^)); addMeta(outputEditM ); end;
        tc_comboBox:     begin new(comboMeta   ,create(@container,P_mapLiteral(def),setupLocation,setupContext^)); addMeta(comboMeta   ); end;

        tc_panel:begin
          new(newPanel,create(@container,P_mapLiteral(def),setupLocation,setupContext^));
          addMeta(newPanel);
          addPanelContents(newPanel^,mapGet(P_mapLiteral(def),key[dmk_parts]));
          newPanel^.alignContents;
        end;
        tc_splitPanel:begin
          new(splitPanel,create(@container,P_mapLiteral(def),setupLocation,setupContext^));
          addMeta(splitPanel);
          addPanelContents(splitPanel^.Left ,mapGet(P_mapLiteral(def),key[dmk_left ]));
          addPanelContents(splitPanel^.Right,mapGet(P_mapLiteral(def),key[dmk_right]));
          splitPanel^.alignContents;
        end;
      end else setupContext^.adapters^.raiseError('Invalid component definition type: '+def^.typeString+'; must be a map',setupLocation);
    end;

  VAR formMeta:T_panelMeta;
      k:longint;
  begin
    enterCriticalSection(lock);
    formMeta.createForExistingForm(self);
    if setupParam^.literalType in C_listTypes
    then for k:=0 to P_listLiteral(setupParam)^.size-1 do initComponent(formMeta,P_listLiteral(setupParam)^.value[k])
    else                                                  initComponent(formMeta,              setupParam           );
    formMeta.alignContents;
    formMeta.destroy;
    displayPending:=true;
    leaveCriticalSection(lock);
  end;

PROCEDURE TscriptedForm.processPendingEvents(CONST location: T_tokenLocation;
  VAR context: T_threadContext);
  VAR m:P_guiElementMeta;
      somethingDone:boolean=false;
  begin
    enterCriticalSection(lock);
    processingEvents:=true;
    for m in meta do if context.adapters^.noErrors then begin
      if m^.evaluate(location,context) then somethingDone:=true;
    end;
    processingEvents:=false;
    leaveCriticalSection(lock);
    if somethingDone then context.adapters^.logDisplayCustomForm;
  end;

PROCEDURE TscriptedForm.updateComponents;
  VAR m:P_guiElementMeta;
  begin
    enterCriticalSection(lock);
    for m in meta do m^.update;
    leaveCriticalSection(lock);
    if processingEvents then propagateCursor(self,crHourGlass)
                        else propagateCursor(self,crDefault);
  end;

PROCEDURE TscriptedForm.conditionalShow(VAR adapters: T_adapters);
  begin
    enterCriticalSection(lock);
    if displayPending and adapters.noErrors then begin
      if (setupParam<>nil) and (setupContext<>nil) then begin
        initialize();
        disposeLiteral(setupParam);
        setupContext:=nil;
      end;
      Show;
      displayPending:=false;
    end;
    updateComponents;
    leaveCriticalSection(lock);
  end;

FUNCTION showDialog_impl(CONST params:P_listLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext):P_literal;
  VAR form:TscriptedForm;
  begin
    result:=nil;
    if not(gui_started) then begin
      context.adapters^.logDisplayCustomForm;
      exit(nil);
    end;
    if (params<>nil) and (params^.size=2) and (params^.value[0]^.literalType=lt_string) and (params^.value[1]^.literalType in C_mapTypes+C_listTypes) then begin
      form:=
      createScriptedForm(P_stringLiteral(params^.value[0])^.value,
                         P_mapLiteral(params^.value[1]),
                         @context,
                         location);
      context.adapters^.logDisplayCustomForm;

      while ((form.displayPending) or (form.showing)) and (context.adapters^.noErrors) do begin
        form.processPendingEvents(location,context);
        sleep(10);
      end;
      result:=newVoidLiteral;
    end;
  end;

INITIALIZATION
  initCriticalSection(scriptedFormCs);
  scriptedForm:=nil;
  registerRule(GUI_NAMESPACE,'showDialog',@showDialog_impl,ak_binary,'showDialog(title:String,contents);//Shows a custom dialog defined by the given contents (Map or List)#//returns void when the form is closed');

FINALIZATION
  freeScriptedForms;
  doneCriticalSection(scriptedFormCs);
end.

