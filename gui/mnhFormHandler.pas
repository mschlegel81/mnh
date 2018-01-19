UNIT mnhFormHandler;
INTERFACE
USES
  Classes, sysutils,Forms,
  mnh_constants,
  mnh_basicTypes,
  mnh_funcs,
  mnh_litVar,
  mnh_contexts;
TYPE
  P_formMeta=^T_formMeta;
  T_formMeta=object
    form:TForm;
    isMain:boolean;
    cyclable:boolean;
    name:string;

    CONSTRUCTOR create(CONST form_:TForm; CONST name_:string; CONST isMain_,cyclable_:boolean);
    DESTRUCTOR destroy;
    FUNCTION visible: boolean;
  end;

PROCEDURE registerForm(CONST f:TForm; CONST name:string; CONST isMainForm,includeInCycle:boolean);
PROCEDURE unregisterForm(CONST f:TForm);
PROCEDURE formCycle(CONST caller:TForm; CONST cycleForward:boolean);
FUNCTION anyFormShowing:boolean;
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
  VAR i:longint;
  begin
    for i:=0 to length(formMeta)-1 do if formMeta[i]^.visible then exit(true);
    result:=false;
  end;

FUNCTION indexOfForm(CONST f:TForm):longint;
  VAR i:longint;
  begin
    for i:=0 to length(formMeta)-1 do if formMeta[i]^.form=f then exit(i);
    result:=-1;
  end;

PROCEDURE registerForm(CONST f: TForm; CONST name:string; CONST isMainForm, includeInCycle: boolean);
  VAR i:longint;
  begin
    i:=indexOfForm(f);
    if i<0 then begin
      i:=length(formMeta);
      setLength(formMeta,i+1);
      new(formMeta[i],create(f, name, isMainForm,includeInCycle));
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

CONSTRUCTOR T_formMeta.create(CONST form_: TForm; CONST name_:string; CONST isMain_,cyclable_: boolean);
  begin
    form:=form_;
    isMain:=isMain_;
    cyclable:=cyclable_;
    name:=name_;
  end;

DESTRUCTOR T_formMeta.destroy;
  begin
  end;

FUNCTION T_formMeta.visible: boolean;
  begin
    result:=(form<>nil) and (form.visible);
    {$ifdef debugMode}
    writeln(stdErr,'        DEBUG: Form ',name,' visible= ',result);
    {$endif}
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

