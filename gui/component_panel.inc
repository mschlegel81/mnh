TYPE
  P_panelMeta=^T_panelMeta;
  T_panelMeta=object(T_guiElementMeta)
    elements:array of P_guiElementMeta;
    lastControl:TControl;
    winControl:TWinControl;
    containedInForm:TForm;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext; CONST consideredKeys:T_definingMapKeys=[dmk_type,dmk_parts]);
    CONSTRUCTOR createForExistingForm(CONST form:TForm);
    PROCEDURE add(CONST meta:P_guiElementMeta);
    FUNCTION evaluate(CONST location:T_tokenLocation; VAR context:T_threadContext):boolean; virtual;
    PROCEDURE update; virtual;
    FUNCTION getControl:TControl; virtual;
    PROCEDURE alignContents; virtual;
    DESTRUCTOR destroy; virtual;
    FUNCTION getName:string; virtual;
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
    containedInForm:=parent^.containedInForm;
    parent^.add(@self);
  end;

CONSTRUCTOR T_panelMeta.createForExistingForm(CONST form: TForm);
  begin
    setLength(elements,0);
    lastControl:=nil;
    winControl:=form;
    containedInForm:=form;
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

FUNCTION T_panelMeta.evaluate(CONST location:T_tokenLocation; VAR context:T_threadContext):boolean; begin result:=false; end;

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

FUNCTION T_panelMeta.getName:string;
  begin
    result:='T_panelMeta holding '+intToStr(length(elements));
  end;
