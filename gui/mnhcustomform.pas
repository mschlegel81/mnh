UNIT mnhCustomForm;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, mnh_constants, mnh_basicTypes, mnh_contexts,
  mnh_litVar, mnhFormHandler,myGenerics,mnh_funcs,mnh_out_adapters;

CONST
  TYPE_KEY   ='type';
  ALIGN_KEY  ='align';
  ACTION_KEY ='action';
  CAPTION_KEY='caption';
  ENABLED_KEY='enabled';
  BINDING_KEY='bind';

TYPE
  P_guiElementMeta=^T_guiElementMeta;
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
      bindingValue:P_literal;
    end;

    CONSTRUCTOR create(CONST def:P_mapLiteral; CONST location:T_tokenLocation; CONST adapters:P_adapters);
    PROCEDURE postAction(CONST param:P_literal);
    FUNCTION evaluate(CONST location:T_tokenLocation; VAR context:T_threadContext):boolean;
    PROCEDURE update; virtual; abstract;
    DESTRUCTOR destroy; virtual;
  end;

  P_editMeta=^T_editMeta;
  T_editMeta=object(T_guiElementMeta)
    editPanel:TPanel;
    editLabel:TLabel;
    edit:TEdit;
    CONSTRUCTOR create(CONST parent:TWinControl; CONST def:P_mapLiteral; CONST location:T_tokenLocation; CONST adapters:P_adapters);
    PROCEDURE update; virtual;
    PROCEDURE OnKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE OnChange(Sender: TObject);
  end;

  P_labelMeta=^T_labelMeta;
  T_labelMeta=object(T_guiElementMeta)
    mylabel:TLabel;
    CONSTRUCTOR create(CONST parent:TWinControl; CONST def:P_mapLiteral; CONST location:T_tokenLocation; CONST adapters:P_adapters);
    PROCEDURE update; virtual;
  end;

  P_checkboxMeta=^T_checkboxMeta;
  T_checkboxMeta=object(T_guiElementMeta)
    checkbox:TCheckBox;
    CONSTRUCTOR create(CONST parent:TWinControl; CONST def:P_mapLiteral; CONST location:T_tokenLocation; CONST adapters:P_adapters);
    PROCEDURE update; virtual;
    PROCEDURE OnClick(Sender: TObject);
  end;

  P_buttonMeta=^T_buttonMeta;
  T_buttonMeta=object(T_guiElementMeta)
    button:TButton;
    CONSTRUCTOR create(CONST parent:TWinControl; CONST def:P_mapLiteral; CONST location:T_tokenLocation; CONST adapters:P_adapters);
    PROCEDURE update; virtual;
    PROCEDURE OnClick(Sender: TObject);
  end;

  TscriptedForm = class(TForm)
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
  private
    setupParam:P_mapLiteral;
    setupLocation:T_tokenLocation;
    meta:array of P_guiElementMeta;
    displayPending:boolean;
    lock:TRTLCriticalSection;
    PROCEDURE initialize(VAR adapters:T_adapters);
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
PROCEDURE conditionalShowCustomForms(VAR adapters:T_adapters);
  begin
    enterCriticalSection(scriptedFormCs);
    if scriptedForm<>nil then scriptedForm.conditionalShow(adapters);
    leaveCriticalSection(scriptedFormCs);
  end;

FUNCTION createScriptedForm(CONST title:string; CONST definition:P_mapLiteral; CONST errorLocation:T_tokenLocation):TscriptedForm;
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

CONSTRUCTOR T_guiElementMeta.create(CONST def: P_mapLiteral; CONST location: T_tokenLocation; CONST adapters: P_adapters);
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
    end;

    tmp:=mapGet(def,ACTION_KEY);
    if tmp<>nil then begin
      if (tmp^.literalType=lt_expression)
      then config.action:=P_expressionLiteral(tmp)
      else adapters^.raiseError('action is: '+tmp^.typeString+'; must be expression',location);
    end;

    tmp:=mapGet(def,CAPTION_KEY);
    if tmp<>nil then begin
      case tmp^.literalType of
        lt_expression: config.caption:=P_expressionLiteral(tmp);
        lt_string    : state .caption:=P_stringLiteral(tmp)^.value;
        else adapters^.raiseError('caption is: '+tmp^.typeString+'; must be string or expression',location);
      end;
    end;

    tmp:=mapGet(def,ENABLED_KEY);
    if tmp<>nil then begin
      case tmp^.literalType of
        lt_expression: config.enabled:=P_expressionLiteral(tmp);
        lt_boolean   : state .enabled:=P_boolLiteral(tmp)^.value;
        else adapters^.raiseError('enabled is: '+tmp^.typeString+'; must be boolean or expression',location);
      end;
    end;

    tmp:=mapGet(def,BINDING_KEY);
    if tmp<>nil then begin
      if tmp^.literalType=lt_string then begin
        config.bindingTo:=P_stringLiteral(tmp)^.value;
      end else adapters^.raiseError('bind is: '+tmp^.typeString+'; must the identifier of a local variable as string',location);
      disposeLiteral(tmp);
    end;
  end;

PROCEDURE T_guiElementMeta.postAction(CONST param: P_literal);
  begin
    if config.action=nil then exit;
    if state.actionParameter<>nil then disposeLiteral(state.actionParameter);
    state.actionParameter:=param;
    state.actionTriggered:=true;
  end;

FUNCTION T_guiElementMeta.evaluate(CONST location: T_tokenLocation; VAR context: T_threadContext):boolean;
  VAR tmp:P_literal;
      oldEnabled:boolean;
      oldCaption:string;
  begin
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

    if (config.bindingTo<>'') and (state.bindingValue<>nil) then begin
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
    end;
  end;

PROCEDURE doAlign(component:TControl; CONST Align:TAlign; CONST setAutosize:boolean);
  begin
    component.Align:=alNone;
    component.Align:=Align;
    if setAutosize then begin
      component.AutoSize:=Align<>alClient;
    end;
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

FUNCTION getAlignment(CONST def:P_mapLiteral; CONST location:T_tokenLocation; CONST adapters:P_adapters):TAlign;
  VAR alignLit:P_literal;
  begin
    alignLit:=mapGet(def,ALIGN_KEY);
    if alignLit=nil then exit(alClient);
    if alignLit^.literalType<>lt_string then begin
      adapters^.raiseError('align is: '+alignLit^.typeString+'; must be string',location);
      disposeLiteral(alignLit);
      exit;
    end;
    result:=alNone;
    if P_stringLiteral(alignLit)^.value='left'   then result:=alLeft   else
    if P_stringLiteral(alignLit)^.value='top'    then result:=alTop    else
    if P_stringLiteral(alignLit)^.value='right'  then result:=alRight  else
    if P_stringLiteral(alignLit)^.value='bottom' then result:=alBottom else
    if P_stringLiteral(alignLit)^.value='client' then result:=alClient else
    adapters^.raiseError('Invalid alignment '+alignLit^.toString()+'; must be one of ["left","top","right","bottom","client"]',location);
    disposeLiteral(alignLit);
  end;

CONSTRUCTOR T_editMeta.create(CONST parent:TWinControl; CONST def: P_mapLiteral; CONST location: T_tokenLocation; CONST adapters: P_adapters);
  begin
    inherited create(def,location,adapters);
    editPanel:=TPanel.create(parent);
    editPanel.parent:=parent;
    editPanel.Align:=getAlignment(def,location,adapters);
    editPanel.AutoSize:=editPanel.Align<>alClient;

    editPanel.BorderWidth:=3;
    editPanel.BorderStyle:=bsNone;
    editPanel.BevelInner:=bvNone;
    editPanel.BevelOuter:=bvNone;
    editPanel.caption:='';

    editLabel:=TLabel.create(parent);
    editLabel.parent:=editPanel;
    editLabel.Align:=alLeft;
    editLabel.caption:=state.caption;
    editLabel.AutoSize:=true;

    edit:=TEdit.create(parent);
    edit.parent:=editPanel;
    edit.Align:=alClient;
    edit.enabled:=state.enabled;
    edit.text:='';
    if (config.action<>nil) then edit.OnKeyDown:=@OnKeyDown;
    if (config.bindingTo<>'') then edit.OnChange:=@OnChange;
  end;

PROCEDURE T_editMeta.OnKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key<>13) then exit;
    postAction(newStringLiteral(TEdit(Sender).text));
  end;

PROCEDURE T_editMeta.OnChange(Sender: TObject);
  begin
    if (config.bindingTo<>'') then begin
      if state.bindingValue<>nil then disposeLiteral(state.bindingValue);
      state.bindingValue:=newStringLiteral(edit.text);
    end;
  end;

PROCEDURE T_editMeta.update;
  begin
    editLabel.caption:=state.caption;
    edit.enabled:=state.enabled;
  end;

CONSTRUCTOR T_labelMeta.create(CONST parent: TWinControl; CONST def: P_mapLiteral; CONST location: T_tokenLocation; CONST adapters: P_adapters);
  begin
    inherited create(def,location,adapters);
    mylabel:=TLabel.create(parent);
    mylabel.parent:=parent;
    mylabel.Align:=getAlignment(def,location,adapters);

    mylabel.AutoSize:=mylabel.Align<>alClient;
  end;

PROCEDURE T_labelMeta.update;
  begin
    mylabel.caption:=state.caption;
    mylabel.enabled:=state.enabled;
  end;

CONSTRUCTOR T_checkboxMeta.create(CONST parent: TWinControl; CONST def: P_mapLiteral; CONST location: T_tokenLocation; CONST adapters: P_adapters);
  begin
    inherited create(def,location,adapters);
    checkbox:=TCheckBox.create(parent);
    checkbox.parent:=parent;
    checkbox.Align:=getAlignment(def,location,adapters);

    if (config.action<>nil) or (config.bindingTo<>'') then checkbox.OnClick:=@OnClick;
  end;

PROCEDURE T_checkboxMeta.update;
  begin
    checkbox.caption:=state.caption;
    checkbox.enabled:=state.enabled;
  end;

PROCEDURE T_checkboxMeta.OnClick(Sender: TObject);
  begin
    if (config.bindingTo<>'') then begin
      if state.bindingValue<>nil then disposeLiteral(state.bindingValue);
      state.bindingValue:=newBoolLiteral(checkbox.checked);
    end;
    postAction(newBoolLiteral(checkbox.checked));
  end;

CONSTRUCTOR T_buttonMeta.create(CONST parent: TWinControl; CONST def: P_mapLiteral; CONST location: T_tokenLocation; CONST adapters: P_adapters);
  begin
    inherited create(def,location,adapters);
    button:=TButton.create(parent);
    button.parent:=parent;
    update;
    button.Align:=getAlignment(def,location,adapters);

    if config.action<>nil then button.OnClick:=@OnClick;
  end;

PROCEDURE T_buttonMeta.update;
  begin
    button.caption:=state.caption;
    button.enabled:=state.enabled;
  end;

PROCEDURE T_buttonMeta.OnClick(Sender: TObject);
  begin
    postAction(nil);
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

PROCEDURE TscriptedForm.initialize(VAR adapters: T_adapters);
  TYPE  T_componentType=(tc_error,tc_button,tc_label,tc_checkbox,tc_textBox,tc_panel);//,tc_output);
  CONST C_componentType:array[T_componentType] of string=('','button','label','checkbox','edit','panel');//,'output');

  FUNCTION componentTypeOf(CONST def:P_mapLiteral):T_componentType;
    VAR tc:T_componentType;
        componentTypeLiteral:P_literal;
    begin
      componentTypeLiteral:=mapGet(def,TYPE_KEY);
      if (componentTypeLiteral=nil) or (componentTypeLiteral^.literalType<>lt_string) then begin
        adapters.raiseError('Missing "type" entry in '+def^.toString(100),setupLocation);
        if componentTypeLiteral<>nil then disposeLiteral(componentTypeLiteral);
        exit(tc_error);
      end;
      result:=tc_error;
      for tc in T_componentType do if C_componentType[tc]=P_stringLiteral(componentTypeLiteral)^.value then result:=tc;
      if result=tc_error then
        adapters.raiseError('Invalid type: '+componentTypeLiteral^.toString()+'; must be one of ["panel","button","edit","label","checkbox"]',setupLocation);
      disposeLiteral(componentTypeLiteral);
    end;

  PROCEDURE addMeta(CONST m:P_guiElementMeta);
    begin
      setLength(meta,length(meta)+1);
      meta[length(meta)-1]:=m;
    end;

  PROCEDURE initComponent(CONST component:TWinControl; CONST def:P_literal);
    VAR labelMeta   :P_labelMeta;
        buttonMeta  :P_buttonMeta;
        checkboxMeta:P_checkboxMeta;
        editMeta    :P_editMeta;

        newPanel    :TPanel;
        panelContents:P_literal;
        iter:T_arrayOfLiteral;
    begin
      writeln('Adding custom component(s) ',def^.toString());
      if def^.literalType in C_mapTypes then case componentTypeOf(P_mapLiteral(def)) of
        tc_label: begin
          new(labelMeta,create(component,P_mapLiteral(def),setupLocation,@adapters));
          addMeta(labelMeta);
        end;
        tc_button: begin
          new(buttonMeta,create(component,P_mapLiteral(def),setupLocation,@adapters));
          addMeta(buttonMeta);
        end;
        tc_checkbox:begin
          new(checkboxMeta,create(component,P_mapLiteral(def),setupLocation,@adapters));
          addMeta(checkboxMeta);
        end;
        tc_textBox:begin
          new(editMeta,create(component,P_mapLiteral(def),setupLocation,@adapters));
          addMeta(editMeta);
        end;
        tc_panel:begin
          newPanel:=TPanel.create(component);
          newPanel.parent:=component;
          newPanel.Align:=getAlignment(P_mapLiteral(def),setupLocation,@adapters);
          newPanel.AutoSize:=newPanel.Align<>alClient;
          newPanel.BorderWidth:=3;
          newPanel.BorderStyle:=bsNone;
          newPanel.BevelInner:=bvNone;
          newPanel.BevelOuter:=bvNone;
          newPanel.caption:='';
          panelContents:=mapGet(P_mapLiteral(def),'parts');
          if panelContents=nil then begin end
          else if panelContents^.literalType=lt_list then begin
            iter:=P_listLiteral(panelContents)^.iteratableList;
            disposeLiteral(panelContents);
            for panelContents in iter do if adapters.noErrors then initComponent(newPanel,panelContents);
            disposeLiteral(iter);
          end else begin
            adapters.raiseError('Invalid panel parts type: '+panelContents^.typeString+'; must be a list; context='+panelContents^.toString(100),setupLocation);
            disposeLiteral(panelContents);
          end;
        end;
      end else adapters.raiseError('Invalid component definition type: '+def^.typeString+'; must be a map',setupLocation);
    end;

  begin
    enterCriticalSection(lock);
    initComponent(self,setupParam);
    displayPending:=true;
    leaveCriticalSection(lock);
  end;

PROCEDURE TscriptedForm.processPendingEvents(CONST location: T_tokenLocation; VAR context: T_threadContext);
  VAR m:P_guiElementMeta;
      somethingDone:boolean=false;
  begin
    enterCriticalSection(lock);
    for m in meta do if context.adapters^.noErrors then begin
      if m^.evaluate(location,context) then somethingDone:=true;
    end;
    leaveCriticalSection(lock);
    if somethingDone then context.adapters^.logDisplayCustomForm;
  end;

PROCEDURE TscriptedForm.updateComponents;
  VAR m:P_guiElementMeta;
  begin
    enterCriticalSection(lock);
    for m in meta do m^.update;
    leaveCriticalSection(lock);
  end;

PROCEDURE TscriptedForm.conditionalShow(VAR adapters: T_adapters);
  begin
    enterCriticalSection(lock);
    if displayPending and adapters.noErrors then begin
      if setupParam<>nil then begin
        initialize(adapters);
        disposeLiteral(setupParam);
        setupParam:=nil;
      end;
      Show;
      displayPending:=false;
    end;
    writeln('Updating components');
    updateComponents;
    leaveCriticalSection(lock);
  end;

FUNCTION showDialog_impl(CONST params:P_listLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext):P_literal;
  VAR form:TscriptedForm;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (params^.value[0]^.literalType=lt_string) and (params^.value[1]^.literalType in C_mapTypes) then begin
      form:=
      createScriptedForm(P_stringLiteral(params^.value[0])^.value,
                         P_mapLiteral(params^.value[1]),
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
  registerRule(GUI_NAMESPACE,'showDialog',@showDialog_impl,ak_binary,'');
FINALIZATION
  freeScriptedForms;
  doneCriticalSection(scriptedFormCs);
end.

