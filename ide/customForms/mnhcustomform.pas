UNIT mnhCustomForm;

{$mode objfpc}{$H+}

INTERFACE
{$ifdef debugMode}
  {define debug_mnhCustomForm}
{$endif}

USES
  Classes, sysutils, Forms, Controls,
  ExtCtrls, StdCtrls,
  myGenerics,
  mnh_constants, basicTypes, contexts,
  mnh_messages,
  recyclers,
  out_adapters,
  litVar,
  funcs,
  editorMetaBase,
  mnh_plotForm,
  plotMath,
  synOutAdapter,
  ideLayoutUtil;

TYPE
  T_definingMapKey=(dmk_type,dmk_action,dmk_onChange,dmk_caption,dmk_enabled,dmk_bind,dmk_items,dmk_parts,dmk_left,dmk_right,dmk_highlight,
                    dmk_mouseMoved,dmk_mouseClicked,dmk_interval,dmk_colCount);
  T_definingMapKeys=set of T_definingMapKey;

CONST
  key:array[T_definingMapKey] of string=(
    'type',
    'action',
    'actionOnChange',
    'caption',
    'enabled',
    'bind',
    'items',
    'parts',
    'left',
    'right',
    'highlight',
    'mouseMoved',
    'mouseClicked',
    'interval',
    'colCount');

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
      isFocused:boolean;
      bindingValue:P_literal;
    end;
    elementCs:TRTLCriticalSection;

    CONSTRUCTOR create(CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST consideredKeys:T_definingMapKeys);
    PROCEDURE postAction(CONST param:P_literal);
    FUNCTION evaluate(CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):boolean; virtual;
    PROCEDURE update; virtual; abstract;
    DESTRUCTOR destroy; virtual;
    FUNCTION leftLabelOrNil:TLabel; virtual;
    FUNCTION getControl:TControl; virtual; abstract;
    FUNCTION preferClientAlignment:boolean; virtual;
    PROCEDURE onEnter(Sender: TObject);
    PROCEDURE onExit(Sender: TObject);
    PROCEDURE connect; virtual;
    PROCEDURE disconnect; virtual;
    FUNCTION getName:string; virtual; abstract;
  end;

  { TscriptedForm }

  TscriptedForm = class(T_mnhComponentForm)
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
  private
    markedForCleanup:boolean;
    //setupParam:P_literal;
    //setupLocation:T_tokenLocation;
    //setupContext:P_context;
    meta:array of P_guiElementMeta;
    metaForUpdate:T_setOfPointer;
    plotLink:P_guiElementMeta;
    displayPending:boolean;
    lock:TRTLCriticalSection;
    processingEvents:boolean;
    PROCEDURE initialize(CONST setupParam: P_literal; CONST setupLocation: T_tokenLocation; CONST setupContext: P_context; CONST relatedPlotAdapter:P_guiPlotSystem);
    PROCEDURE showAndConnectAll;
    PROCEDURE hideAndDisconnectAll;
  public
    FUNCTION processPendingEvents(CONST location: T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):boolean;
    PROCEDURE conditionalShow(CONST messages:P_messages);
  end;

  P_customFormRequest=^T_customFormRequest;

  { T_customFormRequest }

  T_customFormRequest=object(T_payloadMessage)
    private
      setupTitle:string;
      setupDef  :P_literal;
      setupContext:P_context;
      setupLocation:T_tokenLocation;

      createdForm: TscriptedForm;
    protected
      FUNCTION internalType:shortstring; virtual;
    public
      CONSTRUCTOR create(CONST title:string; CONST definition:P_literal; CONST context:P_context; CONST errorLocation:T_tokenLocation);
      FUNCTION getCreatedForm(CONST messages:P_messages):TscriptedForm;
      PROCEDURE setCreatedForm(form:TscriptedForm);
      DESTRUCTOR destroy; virtual;
  end;

  P_customFormAdapter = ^T_customFormAdapter;
  T_customFormAdapter = object(T_abstractGuiOutAdapter)
    scriptedForms: array of TscriptedForm;
    relatedPlotAdapter:P_guiPlotSystem;
    CONSTRUCTOR createCustomFormAdapter(CONST plot:P_guiPlotSystem);
    FUNCTION flushToGui:T_messageTypeSet; virtual;
    DESTRUCTOR destroy; virtual;
  end;

IMPLEMENTATION
{$R *.lfm}

PROCEDURE propagateCursor(CONST c:TWinControl; CONST Cursor:TCursor);
  VAR i:longint;
  begin
    for i:=0 to c.ControlCount-1 do begin
      c.Controls[i].Cursor:=Cursor;
      if c.Controls[i] is TWinControl then propagateCursor(TWinControl(c.Controls[i]),Cursor);
    end;
  end;

FUNCTION mapGet(CONST map:P_mapLiteral; CONST key:string):P_literal;
  VAR keyLit:P_literal;
  begin
    keyLit:=newStringLiteral(key);
    result:=map^.get(keyLit);
    disposeLiteral(keyLit);
    if result^.literalType=lt_void then disposeLiteral(result);
  end;

OPERATOR:=(x:T_listLiteral):T_arrayOfString;
  VAR i:longint;
  begin
    {$WARN 5058 OFF}
    setLength(result,x.size);
    for i:=0 to length(result)-1 do begin
      if x.value[i]^.literalType=lt_string
      then result[i]:=P_stringLiteral(x.value[i])^.value
      else result[i]:=                x.value[i]^.toString();
    end;
  end;

//indentation signifies inheritance
{$I component_panel.inc}
  {$I component_splitPanel.inc}
  {$I component_grid.inc}
{$I component_label.inc}
{$I component_checkbox.inc}
{$I component_button.inc}
{$I component_changeListener.inc}
  {$I component_edit.inc}
  {$I component_combobox.inc}
  {$I component_outputMemo.inc}
    {$I component_inputMemo.inc}
    {$I component_outputRedirect.inc}
{$I component_plotConnector.inc}
{$I component_worker.inc}

CONSTRUCTOR T_guiElementMeta.create(CONST def: P_mapLiteral;
                                    CONST location: T_tokenLocation; VAR context: T_context;
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
      if not(context.messages^.continueEvaluation) then exit;
      keys:=def^.keyIteratableList;
      for k in keys do if not(isConsidered(k)) then begin
        context.messages^.postTextMessage(mt_el2_warning,location,'Key '+k^.toString()+' is ignored in '+def^.toString());
      end;
      disposeLiteral(keys);
    end;

  VAR tmp:P_literal;
  begin
    initCriticalSection(elementCs);
    enterCriticalSection(elementCs);
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
      isFocused:=false;
    end;

    tmp:=mapGet(def,key[dmk_action]);
    if tmp<>nil then begin
      if (tmp^.literalType=lt_expression)
      then config.action:=P_expressionLiteral(tmp)
      else context.raiseError('action is: '+tmp^.typeString+'; must be expression',location);
    end;

    tmp:=mapGet(def,key[dmk_caption]);
    if tmp<>nil then begin
      case tmp^.literalType of
        lt_expression: config.caption:=P_expressionLiteral(tmp);
        lt_string    : begin
          state .caption:=P_stringLiteral(tmp)^.value;
          disposeLiteral(tmp);
        end
        else context.raiseError('caption is: '+tmp^.typeString+'; must be string or expression',location);
      end;
    end;

    tmp:=mapGet(def,key[dmk_enabled]);
    if tmp<>nil then begin
      case tmp^.literalType of
        lt_expression: config.enabled:=P_expressionLiteral(tmp);
        lt_boolean   : state .enabled:=P_boolLiteral(tmp)^.value;
        else context.raiseError('enabled is: '+tmp^.typeString+'; must be boolean or expression',location);
      end;
    end;

    tmp:=mapGet(def,key[dmk_bind]);
    if tmp<>nil then begin
      if tmp^.literalType=lt_string then begin
        config.bindingTo:=P_stringLiteral(tmp)^.value;
        state.bindingValue:=context.valueScope^.getVariableValue(config.bindingTo);
      end else context.raiseError('bind is: '+tmp^.typeString+'; must the identifier of a local variable as string',location);
      disposeLiteral(tmp);
    end;

    raiseUnusedKeyWarning;
    leaveCriticalSection(elementCs);
  end;

PROCEDURE T_guiElementMeta.postAction(CONST param: P_literal);
  begin
    if (config.action=nil) or (tryEnterCriticalsection(elementCs)=0) then exit;
    if state.actionParameter<>nil then disposeLiteral(state.actionParameter);
    state.actionParameter:=param;
    state.actionTriggered:=true;
    propagateCursor(TWinControl(getControl.GetTopParent),crHourGlass);
    leaveCriticalSection(elementCs);
  end;

FUNCTION T_guiElementMeta.evaluate(CONST location: T_tokenLocation; VAR context: T_context; VAR recycler:T_recycler): boolean;
  VAR tmp:P_literal;
      oldEnabled:boolean;
      oldCaption:string;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit(false);
    result:=false;
    if (config.bindingTo<>'') then begin
      if (state.bindingValue<>nil) and (state.isFocused) then begin
        tmp:=context.valueScope^.getVariableValue(config.bindingTo);
        if tmp=nil then begin
          {$ifdef debug_mnhCustomForm}
          writeln(stdErr,'        DEBUG: evaluating binding (gui initializing) for ',getName);
          {$endif}
          result:=true;
          context.valueScope^.setVariableValue(config.bindingTo,state.bindingValue,location,@context);
        end else begin
          if tmp^.equals(state.bindingValue) then begin
            disposeLiteral(tmp);
          end else begin
            {$ifdef debug_mnhCustomForm}
            writeln(stdErr,'        DEBUG: evaluating binding (gui leading) for ',getName);
            {$endif}
            result:=true;
            disposeLiteral(tmp);
            context.valueScope^.setVariableValue(config.bindingTo,state.bindingValue,location,@context);
          end;
        end;
      end else if not(state.isFocused) then begin
        tmp:=context.valueScope^.getVariableValue(config.bindingTo);
        if tmp<>nil then begin
          if tmp^.equals(state.bindingValue)
          then  disposeLiteral(tmp)
          else begin
            {$ifdef debug_mnhCustomForm}
            writeln(stdErr,'        DEBUG: evaluating binding (script leading) for ',getName);
            {$endif}
            disposeLiteral(state.bindingValue);
            state.bindingValue:=tmp;
            result:=true;
          end;
        end;
      end;
    end;

    if state.actionTriggered then begin
      {$ifdef debug_mnhCustomForm}
      writeln(stdErr,'        DEBUG: evaluating action for ',getName);
      {$endif}
      if config.action^.canApplyToNumberOfParameters(1) and (state.actionParameter<>nil)
      then tmp:=config.action^.evaluateToLiteral(location,@context,@recycler,state.actionParameter,nil).literal
      else tmp:=config.action^.evaluateToLiteral(location,@context,@recycler,nil                  ,nil).literal;
      if state.actionParameter<>nil then disposeLiteral(state.actionParameter);
      if tmp                  <>nil then disposeLiteral(tmp);
      state.actionTriggered:=false;
      result:=true;
    end;

    if config.enabled<>nil then begin
      {$ifdef debug_mnhCustomForm}
      writeln(stdErr,'        DEBUG: evaluating enabled for ',getName);
      {$endif}
      oldEnabled:=state.enabled;
      state.enabled:=config.enabled^.evaluateToBoolean(location,@context,@recycler,true,nil,nil);
      result:=result or (oldEnabled<>state.enabled);
    end;

    if config.caption<>nil then begin
      {$ifdef debug_mnhCustomForm}
      writeln(stdErr,'        DEBUG: evaluating caption for ',getName);
      {$endif}
      oldCaption:=state.caption;
      tmp:=config.caption^.evaluateToLiteral(location,@context,@recycler,nil,nil).literal;
      if tmp<>nil then begin
        if tmp^.literalType=lt_string
        then state.caption:=P_stringLiteral(tmp)^.value
        else state.caption:=tmp^.toString();
        disposeLiteral(tmp);
      end;
      result:=result or (oldCaption<>state.caption);
    end;

    leaveCriticalSection(elementCs);
  end;

DESTRUCTOR T_guiElementMeta.destroy;
  begin
    enterCriticalSection(elementCs);
    if config.action  <>nil then disposeLiteral(config.action );
    if config.caption <>nil then disposeLiteral(config.caption);
    if config.enabled <>nil then disposeLiteral(config.enabled);
    state.actionTriggered:=false;
    if state.actionParameter<>nil then disposeLiteral(state.actionParameter);
    if state.bindingValue   <>nil then disposeLiteral(state.bindingValue);
    leaveCriticalSection(elementCs);
    doneCriticalSection(elementCs);
  end;

FUNCTION T_guiElementMeta.leftLabelOrNil:TLabel;
  begin
    result:=nil;
  end;

FUNCTION T_guiElementMeta.preferClientAlignment: boolean;
  begin
    result:=false;
  end;

PROCEDURE T_guiElementMeta.onEnter(Sender: TObject);
  begin
    state.isFocused:=true;
  end;

PROCEDURE T_guiElementMeta.onExit(Sender: TObject);
  begin
    state.isFocused:=false;
  end;

PROCEDURE T_guiElementMeta.connect; begin end;
PROCEDURE T_guiElementMeta.disconnect; begin end;

PROCEDURE TscriptedForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    if tryEnterCriticalsection(lock)=0
    then CloseAction:=caNone
    else begin
      if processingEvents then CloseAction:=caNone
                          else markedForCleanup:=(CloseAction in [caFree,caHide]);
      leaveCriticalSection(lock);
      CloseAction:=caFree;
    end;
  end;

PROCEDURE TscriptedForm.FormCreate(Sender: TObject);
  begin
    initCriticalSection(lock);
    enterCriticalSection(lock);
    setLength(meta,0);
    metaForUpdate.create;
    displayPending:=false;
    leaveCriticalSection(lock);
    markedForCleanup:=false;
  end;

PROCEDURE TscriptedForm.FormDestroy(Sender: TObject);
  VAR k:longint;
  begin
    enterCriticalSection(lock);
    for k:=0 to length(meta)-1 do dispose(meta[k],destroy);
    setLength(meta,0);
    displayPending:=false;
    metaForUpdate.destroy;
    leaveCriticalSection(lock);
    doneCriticalSection(lock);
  end;

FUNCTION TscriptedForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icCustomForm;
  end;

PROCEDURE TscriptedForm.performSlowUpdate;
  begin

  end;

PROCEDURE TscriptedForm.performFastUpdate;
  begin

  end;

PROCEDURE TscriptedForm.initialize(CONST setupParam: P_literal; CONST setupLocation: T_tokenLocation; CONST setupContext: P_context; CONST relatedPlotAdapter:P_guiPlotSystem);
  TYPE  T_componentType=(tc_error,tc_button,tc_label,tc_checkbox,tc_textBox,tc_panel,tc_splitPanel,tc_inputEditor,tc_outputEditor,tc_console,tc_comboBox,tc_plot,tc_worker,tc_grid);
  CONST C_componentType:array[T_componentType] of string=('','button','label','checkbox','edit','panel','splitPanel','inputEditor','outputEditor','console','comboBox','plot','worker','grid');

  FUNCTION componentTypeOf(CONST def:P_mapLiteral):T_componentType;
    VAR tc:T_componentType;
        componentTypeLiteral:P_literal;
    begin
      componentTypeLiteral:=mapGet(def,key[dmk_type]);
      if (componentTypeLiteral=nil) or (componentTypeLiteral^.literalType<>lt_string) then begin
        setupContext^.raiseError('Missing "type" entry in '+def^.toString(100),setupLocation);
        if componentTypeLiteral<>nil then disposeLiteral(componentTypeLiteral);
        exit(tc_error);
      end;
      result:=tc_error;
      for tc in T_componentType do if C_componentType[tc]=P_stringLiteral(componentTypeLiteral)^.value then result:=tc;
      if result=tc_error then
        setupContext^.raiseError('Invalid type: '+componentTypeLiteral^.toString()+'; must be one of ["panel","button","edit","comboBox","label","inputEditor","outputEditor","checkbox","splitPanel","grid"]',setupLocation);
      disposeLiteral(componentTypeLiteral);
    end;

  PROCEDURE addMeta(CONST m:P_guiElementMeta);
    begin
      setLength(meta,length(meta)+1);
      meta[length(meta)-1]:=m;
    end;

  PROCEDURE initComponent(CONST container:P_panelMeta; CONST def:P_literal);
    VAR labelMeta   :P_labelMeta;
        buttonMeta  :P_buttonMeta;
        checkboxMeta:P_checkboxMeta;
        editMeta    :P_editMeta;
        inputEditM  :P_inputEditorMeta;
        outputEditM :P_outputMemoMeta;
        comboMeta   :P_comboboxMeta;
        newPanel    :P_panelMeta;
        splitPanel  :P_splitPanelMeta;
        consoleMeta :P_secondaryOutputMemoMeta;
        workerMeta  :P_workerMeta;
        gridMeta    :P_gridMeta;

    PROCEDURE addPanelContents(CONST targetPanel:P_panelMeta; panelContents:P_literal);
      VAR iter:T_arrayOfLiteral;
      begin
        if panelContents=nil then begin end
        else if panelContents^.literalType=lt_list then begin
          iter:=P_listLiteral(panelContents)^.iteratableList;
          disposeLiteral(panelContents);
          for panelContents in iter do if setupContext^.messages^.continueEvaluation then initComponent(targetPanel,panelContents);
          disposeLiteral(iter);
        end else begin
          setupContext^.raiseError('Invalid panel parts type: '+panelContents^.typeString+'; must be a list; context='+panelContents^.toString(100),setupLocation);
          disposeLiteral(panelContents);
        end;
      end;

    begin
      if def^.literalType in C_mapTypes then case componentTypeOf(P_mapLiteral(def)) of
        tc_label:        begin new(labelMeta   ,create(container,P_mapLiteral(def),setupLocation,setupContext^)); addMeta(labelMeta   ); end;
        tc_button:       begin new(buttonMeta  ,create(container,P_mapLiteral(def),setupLocation,setupContext^)); addMeta(buttonMeta  ); end;
        tc_checkbox:     begin new(checkboxMeta,create(container,P_mapLiteral(def),setupLocation,setupContext^)); addMeta(checkboxMeta); end;
        tc_textBox:      begin new(editMeta    ,create(container,P_mapLiteral(def),setupLocation,setupContext^)); addMeta(editMeta    ); end;
        tc_inputEditor : begin new(inputEditM  ,create(container,P_mapLiteral(def),setupLocation,setupContext^)); addMeta(inputEditM  ); end;
        tc_outputEditor: begin new(outputEditM ,create(container,P_mapLiteral(def),setupLocation,setupContext^)); addMeta(outputEditM ); end;
        tc_comboBox:     begin new(comboMeta   ,create(container,P_mapLiteral(def),setupLocation,setupContext^)); addMeta(comboMeta   ); end;
        tc_console:      begin new(consoleMeta ,create(container,P_mapLiteral(def),setupLocation,setupContext^)); addMeta(consoleMeta ); end;
        tc_worker:       begin new(workerMeta  ,create(          P_mapLiteral(def),setupLocation,setupContext^)); addMeta(workerMeta); end;
        tc_plot:         if plotLink=nil then begin;
          new(P_plotConnectorMeta(plotLink),create(P_mapLiteral(def),setupLocation,setupContext^,relatedPlotAdapter));
          addMeta(plotLink);
        end else setupContext^.raiseError('Only one plot link is allowed per custom form',setupLocation);
        tc_panel:begin
          new(newPanel,create(container,P_mapLiteral(def),setupLocation,setupContext^));
          addPanelContents(newPanel,mapGet(P_mapLiteral(def),key[dmk_parts]));
          addMeta(newPanel);
          newPanel^.alignContents;
        end;
        tc_splitPanel:begin
          new(splitPanel,create(container,P_mapLiteral(def),setupLocation,setupContext^));
          addPanelContents(@splitPanel^.Left ,mapGet(P_mapLiteral(def),key[dmk_left ]));
          addPanelContents(@splitPanel^.Right,mapGet(P_mapLiteral(def),key[dmk_right]));
          addMeta(splitPanel);
          splitPanel^.alignContents;
        end;
        tc_grid:begin
          new(gridMeta,create(container,P_mapLiteral(def),setupLocation,setupContext^));
          addPanelContents(gridMeta,mapGet(P_mapLiteral(def),key[dmk_parts]));
          gridMeta^.doneAdding;
          addMeta(gridMeta);
        end;
      end else setupContext^.raiseError('Invalid component definition type: '+def^.typeString+'; must be a map',setupLocation);
    end;

  VAR formMeta:P_panelMeta;
      k:longint;
      m:P_guiElementMeta;
  begin
    enterCriticalSection(lock);
    new(formMeta,createForExistingForm(self));
    if setupParam^.literalType in C_listTypes
    then for k:=0 to P_listLiteral(setupParam)^.size-1 do initComponent(formMeta,P_listLiteral(setupParam)^.value[k])
    else                                                  initComponent(formMeta,              setupParam           );
    formMeta^.alignContents;
    for k:=length(meta)-1 downto 0 do begin
      if meta[k]^.getControl<>nil then begin
        {$ifdef debug_mnhCustomForm}
        writeln(stdErr,'        DEBUG: Custom form height by ',meta[k]^.getName);
        {$endif}
        height:=meta[k]^.getControl.top+meta[k]^.getControl.height;
        break;
      end;
    end;
    addMeta(formMeta);
    for m in meta do metaForUpdate.put(m);
    displayPending:=true;
    leaveCriticalSection(lock);
  end;

PROCEDURE TscriptedForm.showAndConnectAll;
  VAR m:P_guiElementMeta;
  begin
    Show;
    for m in meta do m^.connect;
  end;

PROCEDURE TscriptedForm.hideAndDisconnectAll;
  VAR m:P_guiElementMeta;
  begin
    Hide;
    for m in meta do m^.disconnect;
  end;

FUNCTION TscriptedForm.processPendingEvents(CONST location: T_tokenLocation;
  VAR context: T_context; VAR recycler: T_recycler): boolean;
  VAR m:P_guiElementMeta;
      customFormBefore:pointer;
  begin
    {$ifdef debug_mnhCustomForm} writeln(stdErr,'        DEBUG: start processing events'); {$endif}
    result:=false;
    if tryEnterCriticalsection(lock)=0 then begin
      {$ifdef debug_mnhCustomForm} writeln(stdErr,'        DEBUG: Cannot obtain lock in TscriptedForm.processPendingEvents'); {$endif}
      exit(false);
    end;
    customFormBefore:=context.parentCustomForm;
    context.parentCustomForm:=self;
    processingEvents:=true;
    for m in meta do if context.messages^.continueEvaluation then begin
      if m^.evaluate(location,context,recycler) then begin
        result:=true;
        metaForUpdate.put(m);
      end;
    end;
    processingEvents:=false;
    context.parentCustomForm:=customFormBefore;
    leaveCriticalSection(lock);
    if result then context.messages^.postSingal(mt_displayCustomForm,C_nilTokenLocation);
    {$ifdef debug_mnhCustomForm} writeln(stdErr,'        DEBUG: done processing events'); {$endif}
  end;

PROCEDURE TscriptedForm.conditionalShow(CONST messages: P_messages);
  PROCEDURE updateComponents;
    VAR metaPointer:pointer;
    begin
      if processingEvents
      then propagateCursor(self,crHourGlass)
      else propagateCursor(self,crDefault);
      for metaPointer in metaForUpdate.values do P_guiElementMeta(metaPointer)^.update;
      metaForUpdate.clear;
    end;

  begin
    if tryEnterCriticalsection(lock)=0 then begin
      messages^.postSingal(mt_displayCustomForm,C_nilTokenLocation);
      exit;
    end;
    //if displayPending and messages^.continueEvaluation then begin
    //  if (setupParam<>nil) and (setupContext<>nil) then begin
    //    initialize();
    //    disposeLiteral(setupParam);
    //    setupContext:=nil;
    //  end;
    //  Show;
    //  displayPending:=false;
    //end;
    updateComponents;
    leaveCriticalSection(lock);
  end;

FUNCTION showDialog_impl(CONST params:P_listLiteral; CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):P_literal;
  VAR form:TscriptedForm;
      sleepTime:longint=0;
      formRequest:P_customFormRequest;
  begin
    result:=nil;
    if not(gui_started) then begin
      context.messages^.logGuiNeeded;
      exit(nil);
    end;
    if (params<>nil) and (params^.size=2) and (params^.value[0]^.literalType=lt_string) and (params^.value[1]^.literalType in C_mapTypes+C_listTypes) then begin
      new(formRequest,create(P_stringLiteral(params^.value[0])^.value,
                         P_mapLiteral(params^.value[1]),
                         @context,
                         location));
      context.messages^.postCustomMessage(@formRequest,false);
      form:=formRequest^.getCreatedForm(context.messages);
      disposeMessage(formRequest);
      if context.parentCustomForm<>nil then TscriptedForm(context.parentCustomForm).hideAndDisconnectAll;
      while not(form.markedForCleanup) and (context.messages^.continueEvaluation) do begin
        if form.processPendingEvents(location,context,recycler)
        then sleepTime:=0
        else begin
          if sleepTime<100 then sleepTime:=100;
          if sleepTime<1000 then inc(sleepTime);
          sleep(sleepTime);
        end;
      end;
      if context.parentCustomForm<>nil then TscriptedForm(context.parentCustomForm).showAndConnectAll;
      result:=newVoidLiteral;
    end;
  end;

CONSTRUCTOR T_customFormAdapter.createCustomFormAdapter(CONST plot:P_guiPlotSystem);
  begin
    inherited create(at_customForm,[mt_displayCustomForm,mt_endOfEvaluation]);
    setLength(scriptedForms,0);
    relatedPlotAdapter:=plot;
  end;

FUNCTION T_customFormAdapter.flushToGui: T_messageTypeSet;
  VAR m:P_storedMessage;
      k:longint;
      newForm:TscriptedForm;
  begin
    result:=[];
    enterCriticalSection(cs);
    for m in storedMessages do case m^.messageType of
      mt_endOfEvaluation: begin
        for k:=0 to length(scriptedForms)-1 do FreeAndNil(scriptedForms[k]);
        setLength(scriptedForms,0);
        include(result,m^.messageType);
      end;
      mt_displayCustomForm: with P_customFormRequest(m)^ do begin
        newForm:=TscriptedForm.create(Application);
        newForm.displayPending:=true;
        newForm.caption:=setupTitle;
        setLength(scriptedForms,length(scriptedForms)+1);
        scriptedForms[length(scriptedForms)-1]:=newForm;
        newForm.initialize(setupDef,setupLocation,setupContext,relatedPlotAdapter);
        setCreatedForm(newForm);
        include(result,m^.messageType);
      end;
    end;
    clear;
    leaveCriticalSection(cs);
  end;

DESTRUCTOR T_customFormAdapter.destroy;
  VAR k:longint;
  begin
    enterCriticalSection(cs);
    for k:=0 to length(scriptedForms)-1 do FreeAndNil(scriptedForms[k]);
    setLength(scriptedForms,0);
    leaveCriticalSection(cs);
  end;

INITIALIZATION
  registerRule(GUI_NAMESPACE,'showDialog',@showDialog_impl,ak_binary,'showDialog(title:String,contents);//Shows a custom dialog defined by the given contents (Map or List)#//returns void when the form is closed');

end.

