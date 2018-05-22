UNIT mnhFormHandler;
INTERFACE
USES
  sysutils,Forms,
  mnh_constants,
  mnh_basicTypes,
  mnh_funcs,
  mnh_litVar,
  mnh_contexts;
TYPE
  T_formType=(ft_main,ft_plot,ft_table,ft_variableView,ft_customForm,ft_askDialog);

  P_formMeta=^T_formMeta;
  T_formMeta=object
    form:TForm;
    formType:T_formType;

    CONSTRUCTOR create(CONST form_:TForm; CONST t:T_formType);
    DESTRUCTOR destroy;
    FUNCTION visible: boolean;
    FUNCTION cyclable: boolean;
  end;

PROCEDURE registerForm(CONST f:TForm; CONST formType:T_formType);
PROCEDURE unregisterForm(CONST f:TForm);
PROCEDURE formCycle(CONST caller:TForm; CONST cycleForward:boolean);
FUNCTION anyFormShowing:boolean;
FUNCTION anyFormShowing(CONST formType:T_formType):boolean;
IMPLEMENTATION
VAR formMeta:array of P_formMeta;

PROCEDURE finalizeUnit;
  VAR i:longint;
  begin
    for i:=0 to length(formMeta)-1 do dispose(formMeta[i],destroy);
    setLength(formMeta,0);
  end;

PROCEDURE formCycle(CONST caller: TForm; CONST cycleForward: boolean);
  VAR i,j,dj:longint;
  begin
    if cycleForward then dj:=1 else dj:=length(formMeta)-1;
    for i:=0 to length(formMeta)-1 do if formMeta[i]^.form=caller then begin
      j:=i;
      repeat j:=(j+dj) mod length(formMeta) until (j=i) or (formMeta[j]^.cyclable);
      if i<>j then with formMeta[j]^.form do begin
        Show;
        BringToFront;
        SetFocus;
      end;
      exit;
    end;
  end;

FUNCTION anyFormShowing: boolean;
VAR m:P_formMeta;
  begin
    for m in formMeta do if m^.visible then exit(true);
    result:=false;
  end;

FUNCTION anyFormShowing(CONST formType:T_formType):boolean;
  VAR m:P_formMeta;
  begin
    for m in formMeta do if m^.visible and (m^.formType=formType) then exit(true);
    result:=false;
  end;

FUNCTION indexOfForm(CONST f:TForm):longint;
  VAR i:longint;
  begin
    for i:=0 to length(formMeta)-1 do if formMeta[i]^.form=f then exit(i);
    result:=-1;
  end;

PROCEDURE registerForm(CONST f: TForm; CONST formType:T_formType);
  VAR i:longint;
  begin
    i:=indexOfForm(f);
    if i<0 then begin
      i:=length(formMeta);
      setLength(formMeta,i+1);
      new(formMeta[i],create(f,formType));
    end;
  end;

PROCEDURE unregisterForm(CONST f: TForm);
  VAR i:longint;
  begin
    i:=indexOfForm(f);
    if i<0 then exit;
    dispose(formMeta[i],destroy);
    while i<length(formMeta)-1 do begin
      formMeta[i]:=formMeta[i+1];
      inc(i);
    end;
    setLength(formMeta,length(formMeta)-1);
  end;

CONSTRUCTOR T_formMeta.create(CONST form_: TForm; CONST t:T_formType);
  begin
    form:=form_;
    formType:=t;
  end;

DESTRUCTOR T_formMeta.destroy;
  begin
  end;

FUNCTION T_formMeta.visible: boolean;
  begin
    result:=(form<>nil) and (form.visible);
  end;

FUNCTION T_formMeta.cyclable: boolean;
  begin
    if formType in [ft_main,ft_plot,ft_table,ft_variableView] then exit(true);
    result:=visible;
  end;

{$i mnh_func_defines.inc}
FUNCTION anyFormShowing_imp intFuncSignature;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then result:=newBoolLiteral(anyFormShowing);
  end;

INITIALIZATION
  registerRule(GUI_NAMESPACE,'anyFormShowing',@anyFormShowing_imp,ak_nullary,'anyFormShowing();//returns true if any form is showing');

FINALIZATION
  finalizeUnit;
end.

