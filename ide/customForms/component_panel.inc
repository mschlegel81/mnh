TYPE
  P_panelMeta=^T_panelMeta;
  T_panelMeta=object(T_guiElementMeta)
    elements:array of P_guiElementMeta;
    lastControl:TControl;
    winControl:TWinControl;
    containedInForm:TForm;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST recycler:P_literalRecycler; CONST consideredKeys:T_definingMapKeys=[dmk_type,dmk_parts]);
    CONSTRUCTOR createForExistingForm(CONST form:TscriptedForm);
    PROCEDURE add(CONST meta:P_guiElementMeta); virtual;
    FUNCTION evaluate(CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):boolean; virtual;
    PROCEDURE update; virtual;
    FUNCTION getControl:TControl; virtual;
    FUNCTION preferClientAlignment: boolean; virtual;
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
  VAR recyler:P_recycler;
  begin
    inherited destroy;
    recyler:=newRecycler;
    recyler^.disposeLiteral(setupDef);
    freeRecycler(recyler);
  end;

CONSTRUCTOR T_panelMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral;
                               CONST location: T_tokenLocation; VAR context: T_context; CONST recycler:P_literalRecycler;
                               CONST consideredKeys:T_definingMapKeys);
  VAR newPanel:TPanel;
  begin
    inherited create(def,location,context,recycler,consideredKeys);
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
    lastControl:=nil;
    winControl:=form;
    containedInForm:=form;
    registerFontControl(form,ctGeneral);
  end;

PROCEDURE T_panelMeta.add(CONST meta: P_guiElementMeta);
  begin
    if lastControl<>nil then begin
      meta^.getControl.top:=lastControl.top+lastControl.height+10;
    end;
    meta^.getControl.AnchorToNeighbour(akLeft,0,nil);
    meta^.getControl.AnchorToNeighbour(akRight,0,nil);

    setLength(elements,length(elements)+1);
    elements[length(elements)-1]:=meta;
    lastControl:=meta^.getControl;
    TscriptedForm(containedInForm).closeButtonPanel.BringToFront;
  end;

FUNCTION T_panelMeta.evaluate(CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):boolean; begin result:=false; end;

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

FUNCTION T_panelMeta.preferClientAlignment: boolean;
  VAR e:P_guiElementMeta;
  begin
    for e in  elements do if e^.preferClientAlignment then exit(true);
    result:=false;
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
