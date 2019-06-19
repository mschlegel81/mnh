TYPE
  P_panelMeta=^T_panelMeta;
  T_panelMeta=object(T_guiElementMeta)
    elements:array of P_guiElementMeta;
    lastControl:TControl;
    winControl:TWinControl;
    containedInForm:TForm;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST consideredKeys:T_definingMapKeys=[dmk_type,dmk_parts]);
    CONSTRUCTOR createForExistingForm(CONST form:TscriptedForm);
    PROCEDURE add(CONST meta:P_guiElementMeta); virtual;
    FUNCTION evaluate(CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):boolean; virtual;
    PROCEDURE update; virtual;
    FUNCTION getControl:TControl; virtual;
    PROCEDURE alignContents; virtual;
    DESTRUCTOR destroy; virtual;
    FUNCTION getName:string; virtual;
  end;

FUNCTION T_customFormRequest.internalType: shortstring;
  begin
    result:='T_customFormRequest';
  end;

CONSTRUCTOR T_customFormRequest.create(CONST title: string;
  CONST definition: P_literal; CONST context: P_context;
  CONST errorLocation: T_tokenLocation);
  begin
    inherited create(mt_displayCustomForm);
    setupTitle:=title;
    setupDef:=definition^.rereferenced;
    setupContext:=context;
    setupLocation:=errorLocation;
    createdForm:=nil;
  end;

FUNCTION T_customFormRequest.getCreatedForm(CONST messages:P_messages): TscriptedForm;
  begin
    repeat
      enterCriticalSection(messageCs);
      result:=createdForm;
      leaveCriticalSection(messageCs);
      ThreadSwitch;
    until (result<>nil) or not(messages^.continueEvaluation);
  end;

PROCEDURE T_customFormRequest.setCreatedForm(form:TscriptedForm);
  begin
    enterCriticalSection(messageCs);
    createdForm:=form;
    leaveCriticalSection(messageCs);
  end;

DESTRUCTOR T_customFormRequest.destroy;
  begin
    inherited destroy;
    disposeLiteral(setupDef);
  end;

CONSTRUCTOR T_panelMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral;
                               CONST location: T_tokenLocation; VAR context: T_context;
                               CONST consideredKeys:T_definingMapKeys);
  VAR newPanel:TPanel;
  begin
    inherited create(def,location,context,consideredKeys);
    newPanel:=TPanel.create(parent^.winControl);
    newPanel.parent:=parent^.winControl;
    newPanel.ParentFont:=true;
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
    containedInForm:=parent^.containedInForm;
    parent^.add(@self);
  end;

CONSTRUCTOR T_panelMeta.createForExistingForm(CONST form: TscriptedForm);
  begin
    initCriticalSection(elementCs);
    setLength(elements,0);
    lastControl:=form.closeButtonPanel;
    winControl:=form;
    containedInForm:=form;
    registerFontControl(form,ctGeneral);
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

FUNCTION T_panelMeta.evaluate(CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):boolean; begin result:=false; end;

PROCEDURE T_panelMeta.update;
  VAR k:longint;
      maxLabelWidth:longint=0;
      l:TLabel;
      pw:longint=0;
      ph:longint=0;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit;
    try
      for k:=0 to length(elements)-1 do begin
        l:=elements[k]^.leftLabelOrNil;
        if (l<>nil) then begin
          l.AutoSize:=false;
          l.GetPreferredSize(pw,ph);
          if pw>maxLabelWidth then maxLabelWidth:=pw;
        end;
      end;
      if maxLabelWidth>0 then for k:=0 to length(elements)-1 do begin
        l:=elements[k]^.leftLabelOrNil;
        if (l<>nil) then l.width:=maxLabelWidth;
      end;
    finally
      leaveCriticalSection(elementCs);
    end;
  end;

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
    unregisterFontControl(winControl);
  end;

FUNCTION T_panelMeta.getName:string;
  begin
    result:='T_panelMeta holding '+intToStr(length(elements));
  end;
