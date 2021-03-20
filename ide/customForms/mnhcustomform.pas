UNIT mnhCustomForm;

{$mode objfpc}{$H+}

INTERFACE
{$ifdef debugMode}
  {define debug_mnhCustomForm}
{$endif}

USES
  Classes, sysutils, Forms, Controls,
  ExtCtrls, StdCtrls, Menus,
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

    CONSTRUCTOR create(CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST recycler:P_literalRecycler; CONST consideredKeys:T_definingMapKeys);
    PROCEDURE postAction(CONST param:P_literal);
    FUNCTION getCustomForm:TWinControl;
    FUNCTION evaluate(CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):boolean; virtual;
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

  P_customFormAdapter = ^T_customFormAdapter;
  TscriptedForm = class(T_mnhComponentForm)
    CloseButton: TButton;
    closeButtonPanel: TPanel;
    MainMenu1: TMainMenu;
    PopupMenu1: TPopupMenu;
    PROCEDURE closeButtonClick(Sender: TObject);
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate(CONST isEvaluationRunning:boolean); override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE dockChanged; override;
  private
    markedForCleanup:boolean;
    meta:array of P_guiElementMeta;
    metaForUpdate:T_setOfPointer;
    plotLink,plotDock:P_guiElementMeta;
    displayPending:boolean;
    lock:TRTLCriticalSection;
    processingEvents:boolean;
    adapter:P_customFormAdapter;
    PROCEDURE initialize(CONST setupParam: P_literal; CONST setupLocation: T_tokenLocation; CONST setupContext: P_context; CONST recycler:P_literalRecycler; CONST relatedPlotAdapter:P_guiPlotSystem);
    PROCEDURE showAndConnectAll;
    PROCEDURE hideAndDisconnectAll;
  public
    FUNCTION processPendingEvents(CONST location: T_tokenLocation; CONST context: P_context; CONST recycler: P_recycler):boolean;
  end;

  P_customFormRequest=^T_customFormRequest;
  T_customFormRequest=object(T_payloadMessage)
    private
      setupTitle:string;
      setupDef  :P_literal;
      setupContext:P_context;
      setupLocation:T_tokenLocation;

      createdForm: TscriptedForm;
    public
      FUNCTION internalType:shortstring; virtual;
      CONSTRUCTOR create(CONST title:string; CONST definition:P_literal; CONST context:P_context; CONST errorLocation:T_tokenLocation);
      FUNCTION getCreatedForm(CONST messages:P_messages):TscriptedForm;
      PROCEDURE setCreatedForm(form:TscriptedForm);
      DESTRUCTOR destroy; virtual;
  end;

  T_customFormAdapter = object(T_abstractGuiOutAdapter)
    scriptedForms: array of TscriptedForm;
    relatedPlotAdapter:P_guiPlotSystem;
    CONSTRUCTOR createCustomFormAdapter(CONST plot:P_guiPlotSystem);
    FUNCTION flushToGui(CONST forceFlush:boolean):T_messageTypeSet; virtual;
    DESTRUCTOR destroy; virtual;
  end;

IMPLEMENTATION
USES synOutAdapter,mnh_settings,ComCtrls;
{$R *.lfm}

PROCEDURE propagateCursor(CONST c:TWinControl; CONST Cursor:TCursor);
  VAR i:longint;
  begin
    for i:=0 to c.ControlCount-1 do begin
      c.Controls[i].Cursor:=Cursor;
      if c.Controls[i] is TWinControl then propagateCursor(TWinControl(c.Controls[i]),Cursor);
    end;
  end;

FUNCTION mapGet(CONST map:P_mapLiteral; CONST key:string; CONST recycler:P_literalRecycler):P_literal;
  VAR keyLit:P_literal;
  begin
    keyLit:=recycler^.newStringLiteral(key);
    result:=map^.get(recycler,keyLit);
    recycler^.disposeLiteral(keyLit);
    if result^.literalType=lt_void then recycler^.disposeLiteral(result);
  end;

OPERATOR:=(x:T_listLiteral):T_arrayOfString;
  VAR i:longint;
  begin
    {$WARN 5058 OFF}
    initialize(result);
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
  {$I component_plotDock.inc}
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
                                    CONST location: T_tokenLocation; VAR context: T_context; CONST recycler:P_literalRecycler;
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
      if not(context.continueEvaluation) then exit;
      keys:=def^.keyIteratableList;
      for k in keys do if not(isConsidered(k)) then begin
        context.messages^.postTextMessage(mt_el2_warning,location,'Key '+k^.toString()+' is ignored in '+def^.toString());
      end;
      recycler^.disposeLiteral(keys);
    end;

  VAR tmp:P_literal;
  begin
    initCriticalSection(elementCs);
    enterCriticalSection(elementCs);
    try
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

      tmp:=mapGet(def,key[dmk_action],recycler);
      if tmp<>nil then begin
        if (tmp^.literalType=lt_expression)
        then config.action:=P_expressionLiteral(tmp)
        else context.raiseError('action is: '+tmp^.typeString+'; must be expression',location);
      end;

      tmp:=mapGet(def,key[dmk_caption],recycler);
      if tmp<>nil then begin
        case tmp^.literalType of
          lt_expression: config.caption:=P_expressionLiteral(tmp);
          lt_string    : begin
            state .caption:=P_stringLiteral(tmp)^.value;
            recycler^.disposeLiteral(tmp);
          end
          else context.raiseError('caption is: '+tmp^.typeString+'; must be string or expression',location);
        end;
      end;

      tmp:=mapGet(def,key[dmk_enabled],recycler);
      if tmp<>nil then begin
        case tmp^.literalType of
          lt_expression: config.enabled:=P_expressionLiteral(tmp);
          lt_boolean   : state .enabled:=P_boolLiteral(tmp)^.value;
          else context.raiseError('enabled is: '+tmp^.typeString+'; must be boolean or expression',location);
        end;
      end;

      tmp:=mapGet(def,key[dmk_bind],recycler);
      if tmp<>nil then begin
        if tmp^.literalType=lt_string then begin
          config.bindingTo:=P_stringLiteral(tmp)^.value;
          state.bindingValue:=context.valueScope^.getVariableValue(config.bindingTo);
        end else context.raiseError('bind is: '+tmp^.typeString+'; must the identifier of a local variable as string',location);
        recycler^.disposeLiteral(tmp);
      end;

      raiseUnusedKeyWarning;
    finally
      leaveCriticalSection(elementCs);
    end;
  end;

FUNCTION T_guiElementMeta.getCustomForm:TWinControl;
  begin
    result:=getControl.parent;
    while (result.parent<>nil) and (result.ClassType<>TscriptedForm.ClassType) do result:=result.parent;
  end;

PROCEDURE T_guiElementMeta.postAction(CONST param: P_literal);
  VAR recycler:P_recycler;
  begin
    if (config.action=nil) or (tryEnterCriticalsection(elementCs)=0) then exit;
    try
      if state.actionParameter<>nil then begin
        recycler:=newRecycler;
        recycler^.disposeLiteral(state.actionParameter);
        freeRecycler(recycler);
      end;
      state.actionParameter:=param;
      state.actionTriggered:=true;
      propagateCursor(getCustomForm,crHourGlass);
    finally
      leaveCriticalSection(elementCs);
    end;
  end;

FUNCTION T_guiElementMeta.evaluate(CONST location: T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler): boolean;
  VAR tmp:P_literal;
      oldEnabled:boolean;
      oldCaption:string;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit(false);
    try
      result:=false;
      if (config.bindingTo<>'') then begin
        if (state.bindingValue<>nil) and (state.isFocused) then begin
          tmp:=context^.valueScope^.getVariableValue(config.bindingTo);
          if tmp=nil then begin
            {$ifdef debug_mnhCustomForm}
            writeln(stdErr,'        DEBUG: evaluating binding (gui initializing) for ',getName);
            {$endif}
            result:=true;
            context^.valueScope^.setVariableValue(recycler,config.bindingTo,state.bindingValue,location,context);
          end else begin
            if tmp^.equals(state.bindingValue) then begin
              recycler^.disposeLiteral(tmp);
            end else begin
              {$ifdef debug_mnhCustomForm}
              writeln(stdErr,'        DEBUG: evaluating binding (gui leading) for ',getName);
              {$endif}
              result:=true;
              recycler^.disposeLiteral(tmp);
              context^.valueScope^.setVariableValue(recycler,config.bindingTo,state.bindingValue,location,context);
            end;
          end;
        end else if not(state.isFocused) then begin
          tmp:=context^.valueScope^.getVariableValue(config.bindingTo);
          if tmp<>nil then begin
            if tmp^.equals(state.bindingValue)
            then  recycler^.disposeLiteral(tmp)
            else begin
              {$ifdef debug_mnhCustomForm}
              writeln(stdErr,'        DEBUG: evaluating binding (script leading) for ',getName);
              {$endif}
              recycler^.disposeLiteral(state.bindingValue);
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
        then tmp:=config.action^.evaluateToLiteral(location,context,recycler,state.actionParameter,nil).literal
        else tmp:=config.action^.evaluateToLiteral(location,context,recycler,nil                  ,nil).literal;
        if state.actionParameter<>nil then recycler^.disposeLiteral(state.actionParameter);
        if tmp                  <>nil then recycler^.disposeLiteral(tmp);
        state.actionTriggered:=false;
        result:=true;
      end;

      if config.enabled<>nil then begin
        {$ifdef debug_mnhCustomForm}
        writeln(stdErr,'        DEBUG: evaluating enabled for ',getName);
        {$endif}
        oldEnabled:=state.enabled;
        state.enabled:=config.enabled^.evaluateToBoolean(location,context,recycler,true,nil,nil);
        result:=result or (oldEnabled<>state.enabled);
      end;

      if config.caption<>nil then begin
        {$ifdef debug_mnhCustomForm}
        writeln(stdErr,'        DEBUG: evaluating caption for ',getName);
        {$endif}
        oldCaption:=state.caption;
        tmp:=config.caption^.evaluateToLiteral(location,context,recycler,nil,nil).literal;
        if tmp<>nil then begin
          if tmp^.literalType=lt_string
          then state.caption:=P_stringLiteral(tmp)^.value
          else state.caption:=tmp^.toString();
          recycler^.disposeLiteral(tmp);
        end;
        result:=result or (oldCaption<>state.caption);
      end;
    finally
      leaveCriticalSection(elementCs);
    end;
  end;

DESTRUCTOR T_guiElementMeta.destroy;
  VAR recycler:P_recycler;
  begin
    enterCriticalSection(elementCs);
    recycler:=newRecycler;
    try
      if config.action  <>nil then recycler^.disposeLiteral(config.action );
      if config.caption <>nil then recycler^.disposeLiteral(config.caption);
      if config.enabled <>nil then recycler^.disposeLiteral(config.enabled);
      state.actionTriggered:=false;
      if state.actionParameter<>nil then recycler^.disposeLiteral(state.actionParameter);
      if state.bindingValue   <>nil then recycler^.disposeLiteral(state.bindingValue);
    finally
      leaveCriticalSection(elementCs);
      doneCriticalSection(elementCs);
      freeRecycler(recycler);
    end;
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
  VAR m:P_guiElementMeta;

  begin
    if tryEnterCriticalsection(lock)=0
    then CloseAction:=caNone
    else begin
      try
        if processingEvents then CloseAction:=caNone;
        markedForCleanup:=(CloseAction in [caFree,caHide]);
        if markedForCleanup then for m in meta do m^.disconnect;
      finally
        leaveCriticalSection(lock);
      end;
    end;
  end;

PROCEDURE TscriptedForm.closeButtonClick(Sender: TObject);
  begin
    close;
  end;

PROCEDURE TscriptedForm.FormCreate(Sender: TObject);
  begin
    initCriticalSection(lock);
    enterCriticalSection(lock);
    try
      setLength(meta,0);
      metaForUpdate.create;
      displayPending:=false;
      plotLink:=nil;
      plotDock:=nil;
    finally
      leaveCriticalSection(lock);
    end;
    markedForCleanup:=false;
    initDockMenuItems(MainMenu1,nil);
    initDockMenuItems(PopupMenu1,PopupMenu1.items);
  end;

PROCEDURE TscriptedForm.FormDestroy(Sender: TObject);
  VAR k:longint;
  begin
    enterCriticalSection(lock);
    try
      for k:=0 to length(meta)-1 do dispose(meta[k],destroy);
      setLength(meta,0);
      displayPending:=false;
      metaForUpdate.destroy;
      if adapter<>nil then with adapter^ do begin
        k:=0;
        while (k<length(scriptedForms)) and (scriptedForms[k]<>self) do inc(k);
        if k<length(scriptedForms) then begin
          scriptedForms[k]:=scriptedForms[length(scriptedForms)-1];
          setLength(        scriptedForms,length(scriptedForms)-1);
        end;
      end;
    finally
      leaveCriticalSection(lock);
      doneCriticalSection(lock);
    end;
  end;

FUNCTION TscriptedForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icCustomForm;
  end;

PROCEDURE TscriptedForm.performSlowUpdate(CONST isEvaluationRunning:boolean);
  begin
  end;

PROCEDURE TscriptedForm.performFastUpdate;
  VAR metaPointer:pointer;
  begin
    if markedForCleanup or (tryEnterCriticalsection(lock)=0) then exit;
    try
      if processingEvents
      then propagateCursor(self,crHourGlass)
      else propagateCursor(self,crDefault);
      for metaPointer in metaForUpdate.values do P_guiElementMeta(metaPointer)^.update;
      metaForUpdate.clear;
    finally
      leaveCriticalSection(lock);
    end;
  end;

PROCEDURE TscriptedForm.initialize(CONST setupParam: P_literal; CONST setupLocation: T_tokenLocation; CONST setupContext: P_context; CONST recycler:P_literalRecycler; CONST relatedPlotAdapter: P_guiPlotSystem);
  TYPE  T_componentType=(tc_error,tc_button,tc_label,tc_checkbox,tc_textBox,tc_panel,tc_splitPanel,tc_inputEditor,tc_outputEditor,tc_console,tc_comboBox,tc_plot,tc_worker,tc_grid,tc_plotDock);
  CONST C_componentType:array[T_componentType] of string=('','button','label','checkbox','edit','panel','splitPanel','inputEditor','outputEditor','console','comboBox','plot','worker','grid','plotDock');

  FUNCTION componentTypeOf(CONST def:P_mapLiteral):T_componentType;
    VAR tc:T_componentType;
        componentTypeLiteral:P_literal;
    begin
      componentTypeLiteral:=mapGet(def,key[dmk_type],recycler);
      if (componentTypeLiteral=nil) or (componentTypeLiteral^.literalType<>lt_string) then begin
        setupContext^.raiseError('Missing "type" entry in '+def^.toString(100),setupLocation);
        if componentTypeLiteral<>nil then recycler^.disposeLiteral(componentTypeLiteral);
        exit(tc_error);
      end;
      result:=tc_error;
      for tc in T_componentType do if C_componentType[tc]=P_stringLiteral(componentTypeLiteral)^.value then result:=tc;
      if result=tc_error then
        setupContext^.raiseError('Invalid type: '+componentTypeLiteral^.toString()+'; must be one of ["panel","button","edit","comboBox","label","inputEditor","outputEditor","checkbox","splitPanel","grid"]',setupLocation);
      recycler^.disposeLiteral(componentTypeLiteral);
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
          iter:=P_listLiteral(panelContents)^.tempIteratableList;
          recycler^.disposeLiteral(panelContents);
          for panelContents in iter do if setupContext^.messages^.continueEvaluation then initComponent(targetPanel,panelContents);
        end else begin
          setupContext^.raiseError('Invalid panel parts type: '+panelContents^.typeString+'; must be a list; context='+panelContents^.toString(100),setupLocation);
          recycler^.disposeLiteral(panelContents);
        end;
      end;

    begin
      if def^.literalType in C_mapTypes then case componentTypeOf(P_mapLiteral(def)) of
        tc_label:        begin new(labelMeta   ,create(container,P_mapLiteral(def),setupLocation,setupContext^,recycler)); addMeta(labelMeta   ); end;
        tc_button:       begin new(buttonMeta  ,create(container,P_mapLiteral(def),setupLocation,setupContext^,recycler)); addMeta(buttonMeta  ); end;
        tc_checkbox:     begin new(checkboxMeta,create(container,P_mapLiteral(def),setupLocation,setupContext^,recycler)); addMeta(checkboxMeta); end;
        tc_textBox:      begin new(editMeta    ,create(container,P_mapLiteral(def),setupLocation,setupContext^,recycler)); addMeta(editMeta    ); end;
        tc_inputEditor : begin new(inputEditM  ,create(container,P_mapLiteral(def),setupLocation,setupContext^,recycler)); addMeta(inputEditM  ); end;
        tc_outputEditor: begin new(outputEditM ,create(container,P_mapLiteral(def),setupLocation,setupContext^,recycler)); addMeta(outputEditM ); end;
        tc_comboBox:     begin new(comboMeta   ,create(container,P_mapLiteral(def),setupLocation,setupContext^,recycler)); addMeta(comboMeta   ); end;
        tc_console:      begin new(consoleMeta ,create(container,P_mapLiteral(def),setupLocation,setupContext^,recycler)); addMeta(consoleMeta ); end;
        tc_worker:       begin new(workerMeta  ,create(          P_mapLiteral(def),setupLocation,setupContext^,recycler)); addMeta(workerMeta); end;
        tc_plot:         if plotLink=nil then begin;
          new(P_plotConnectorMeta(plotLink),create(P_mapLiteral(def),setupLocation,setupContext^,recycler,relatedPlotAdapter));
          addMeta(plotLink);
        end else setupContext^.raiseError('Only one plot link is allowed per custom form',setupLocation);
        tc_plotDock:     if plotDock=nil then begin;
          new(P_plotDockMeta(plotDock),create(container,P_mapLiteral(def),setupLocation,setupContext^,recycler,relatedPlotAdapter));
          addMeta(plotDock);
        end else setupContext^.raiseError('Only one plot dock is allowed per custom form',setupLocation);
        tc_panel:begin
          new(newPanel,create(container,P_mapLiteral(def),setupLocation,setupContext^,recycler));
          addPanelContents(newPanel,mapGet(P_mapLiteral(def),key[dmk_parts],recycler));
          addMeta(newPanel);
          newPanel^.alignContents;
        end;
        tc_splitPanel:begin
          new(splitPanel,create(container,P_mapLiteral(def),setupLocation,setupContext^,recycler));
          addPanelContents(@splitPanel^.Left ,mapGet(P_mapLiteral(def),key[dmk_left ],recycler));
          addPanelContents(@splitPanel^.Right,mapGet(P_mapLiteral(def),key[dmk_right],recycler));
          addMeta(splitPanel);
          splitPanel^.alignContents;
        end;
        tc_grid:begin
          new(gridMeta,create(container,P_mapLiteral(def),setupLocation,setupContext^,recycler));
          addPanelContents(gridMeta,mapGet(P_mapLiteral(def),key[dmk_parts],recycler));
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
    try
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
    finally
      leaveCriticalSection(lock);
    end;
  end;

PROCEDURE TscriptedForm.showAndConnectAll;
  VAR m:P_guiElementMeta;
      page:TTabSheet;
      PageControl:TPageControl;
  begin
    Show;
    getParents(page,PageControl);
    if page<>nil then page.visible:=true;
    for m in meta do m^.connect;
    dockChanged;
  end;

PROCEDURE TscriptedForm.hideAndDisconnectAll;
  VAR m:P_guiElementMeta;
      page:TTabSheet;
      PageControl:TPageControl;
  begin
    Hide;
    getParents(page,PageControl);
    if page<>nil then page.visible:=false;
    for m in meta do m^.disconnect;
  end;

PROCEDURE TscriptedForm.dockChanged;
  begin
    closeButtonPanel.visible:=myComponentParent<>cpNone;
  end;

FUNCTION TscriptedForm.processPendingEvents(CONST location: T_tokenLocation; CONST context: P_context; CONST recycler: P_recycler): boolean;
  VAR m:P_guiElementMeta;
      customFormBefore:pointer;
  begin
    {$ifdef debug_mnhCustomForm} writeln(stdErr,'        DEBUG: start processing events'); {$endif}
    result:=false;
    if tryEnterCriticalsection(lock)=0 then begin
      {$ifdef debug_mnhCustomForm} writeln(stdErr,'        DEBUG: Cannot obtain lock in TscriptedForm.processPendingEvents'); {$endif}
      exit(false);
    end;
    try
      customFormBefore:=context^.parentCustomForm;
      context^.parentCustomForm:=self;
      processingEvents:=true;
      for m in meta do if context^.continueEvaluation then begin
        if m^.evaluate(location,context,recycler) then begin
          result:=true;
          metaForUpdate.put(m);
        end;
      end;
      processingEvents:=false;
      context^.parentCustomForm:=customFormBefore;
    finally
      leaveCriticalSection(lock);
    end;
    {$ifdef debug_mnhCustomForm} writeln(stdErr,'        DEBUG: done processing events'); {$endif}
  end;

FUNCTION showDialog_impl(CONST params:P_listLiteral; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):P_literal;
  VAR form:TscriptedForm;
      sleepTime:longint=0;
      formRequest:P_customFormRequest;
  begin
    result:=nil;
    if (gui_started=NO) then begin
      context^.messages^.logGuiNeeded;
      exit(nil);
    end;
    if not(context^.checkSideEffects('showDialog',location,[se_alterGuiState,se_input])) then exit(nil);
    if (params<>nil) and (params^.size=2) and (params^.value[0]^.literalType=lt_string) and (params^.value[1]^.literalType in C_mapTypes+C_listTypes) then begin
      new(formRequest,create(P_stringLiteral(params^.value[0])^.value,
                         P_mapLiteral(params^.value[1]),
                         context,
                         location));
      context^.messages^.postCustomMessage(formRequest,false);
      form:=formRequest^.getCreatedForm(context^.messages);
      disposeMessage(formRequest);
      if context^.parentCustomForm<>nil then TscriptedForm(context^.parentCustomForm).hideAndDisconnectAll;
      while not(form.markedForCleanup) and (context^.continueEvaluation) do begin
        if form.processPendingEvents(location,context,recycler)
        then sleepTime:=0
        else begin
          if sleepTime<100 then sleepTime:=100;
          if sleepTime<1000 then inc(sleepTime);
          sleep(sleepTime);
        end;
      end;
      if context^.parentCustomForm<>nil then TscriptedForm(context^.parentCustomForm).showAndConnectAll;
      result:=newVoidLiteral;
    end;
  end;

CONSTRUCTOR T_customFormAdapter.createCustomFormAdapter(CONST plot:P_guiPlotSystem);
  begin
    inherited create(at_customForm,[mt_displayCustomForm,mt_endOfEvaluation]);
    setLength(scriptedForms,0);
    relatedPlotAdapter:=plot;
  end;

FUNCTION T_customFormAdapter.flushToGui(CONST forceFlush:boolean): T_messageTypeSet;
  VAR i,k:longint;
      newForm:TscriptedForm;
      recycler:P_recycler;
  begin
    result:=[];
    enterCriticalSection(adapterCs);
    recycler:=newRecycler;
    try
      for i:=0 to collectedFill-1 do begin
        case collected[i]^.messageType of
          mt_endOfEvaluation: begin
            for k:=0 to length(scriptedForms)-1 do begin
              scriptedForms[k].adapter:=nil;
              FreeAndNil(scriptedForms[k]);
            end;
            setLength(scriptedForms,0);
            include(result,collected[i]^.messageType);
          end;
          mt_displayCustomForm: with P_customFormRequest(collected[i])^ do begin
            newForm:=TscriptedForm.create(nil);
            newForm.displayPending:=true;
            newForm.caption:=setupTitle;
            setLength(scriptedForms,length(scriptedForms)+1);
            scriptedForms[length(scriptedForms)-1]:=newForm;
            newForm.initialize(setupDef,setupLocation,setupContext,recycler,relatedPlotAdapter);
            newForm.adapter:=@self;
            setCreatedForm(newForm);
            dockNewForm(newForm);
            newForm.showAndConnectAll;
            include(result,collected[i]^.messageType);
          end;
        end;
      end;
      clear;
    finally
      leaveCriticalSection(adapterCs);
      freeRecycler(recycler);
    end;
  end;

DESTRUCTOR T_customFormAdapter.destroy;
  VAR k:longint;
  begin
    enterCriticalSection(adapterCs);
    try
      for k:=0 to length(scriptedForms)-1 do FreeAndNil(scriptedForms[k]);
      setLength(scriptedForms,0);
    finally
      leaveCriticalSection(adapterCs);
    end;
    inherited destroy;
  end;

INITIALIZATION
  builtinFunctionMap.registerRule(GUI_NAMESPACE,'showDialog',@showDialog_impl,ak_binary,'showDialog(title:String,contents);//Shows a custom dialog defined by the given contents (Map or List)#//returns void when the form is closed',[se_alterGuiState,se_input]);

end.

