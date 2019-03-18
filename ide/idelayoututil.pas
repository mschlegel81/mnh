UNIT ideLayoutUtil;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms,Controls,ComCtrls,Graphics,Menus,SynEdit,evalThread,mnh_settings,serializationUtil;

TYPE
  T_ideComponent=(icOutline,
                  icHelp,
                  icAssistance,
                  icOutput,
                  icQuickEval,
                  icDebugger,
                  icDebuggerVariables,
                  icDebuggerBreakpoints,
                  icPlot,
                  icCustomForm,
                  icTable,
                  icVariableView);
  T_ideComponentSet=set of T_ideComponent;

  T_componentParent=(cpNone,
                     cpPageControl1,
                     cpPageControl2,
                     cpPageControl3,
                     cpPageControl4);

  T_splitterPositions=array[1..4] of longint;

  T_mnhComponentForm=class(TForm)
    private
      myComponentParent:T_componentParent;
    published
      CONSTRUCTOR create(TheOwner: TComponent); override;
      PROCEDURE defaultEndDock(Sender, target: TObject; X,Y: integer);
      FUNCTION getIdeComponentType:T_ideComponent; virtual; abstract;
      PROCEDURE performSlowUpdate; virtual; abstract;
      PROCEDURE performFastUpdate; virtual; abstract;
    public
      DESTRUCTOR destroy; override;
  end;

  T_mnhIdeForm=class(T_abstractMnhForm)
    PROCEDURE attachNewForm(CONST form:T_mnhComponentForm); virtual; abstract;
  end;

VAR lastDockLocationFor:array[T_ideComponent] of T_componentParent
    {icOutline}   =(cpPageControl3,
    {icHelp}        cpPageControl2,
    {icAssistance}  cpPageControl2,
    {icOutput}      cpPageControl2,
    {icQuickEval}   cpPageControl2,
    {icDebugger}    cpPageControl4,
    {icDebuggerVari}cpPageControl4,
    {icDebuggerBrea}cpPageControl4,
    {icPlot}        cpNone,
    {icCustomForm}  cpNone,
    {icTable}       cpNone,
    {icVariableView}cpNone);

    mainForm:T_mnhIdeForm=nil;

PROCEDURE dockNewForm(newForm:T_mnhComponentForm);
FUNCTION hasFormOfType(CONST ideComponent:T_ideComponent; CONST BringToFront:boolean=false):boolean;
FUNCTION getFormOfType(CONST ideComponent:T_ideComponent):T_mnhComponentForm;
PROCEDURE registerSynEdit(VAR edit:TSynEdit);
PROCEDURE unregisterSynEdit(VAR edit:TSynEdit);
PROCEDURE propagateEditorFont(newFont:TFont);
PROCEDURE performSlowUpdates;
PROCEDURE performFastUpdates;
FUNCTION focusedEditor:TSynEdit;

PROCEDURE saveMainFormLayout(VAR stream:T_bufferedOutputStreamWrapper; VAR splitters:T_splitterPositions);
FUNCTION loadMainFormLayout(VAR stream: T_bufferedInputStreamWrapper; VAR splitters: T_splitterPositions; OUT activeComponents:T_ideComponentSet):boolean;

IMPLEMENTATION
VAR activeForms:array of T_mnhComponentForm;
    activeSynEdits:array of TSynEdit;

PROCEDURE dockNewForm(newForm: T_mnhComponentForm);
  begin
    if mainForm<>nil then mainForm.attachNewForm(newForm);
  end;

FUNCTION hasFormOfType(CONST ideComponent:T_ideComponent; CONST BringToFront:boolean=false):boolean;
  VAR f:T_mnhComponentForm;
  begin
    result:=false;
    for f in activeForms do if f.getIdeComponentType=ideComponent then begin
      if BringToFront then begin
        if f.parent=nil
        then f.BringToFront
        else begin
          if (f.parent       .ClassName='TTabSheet') and
             (f.parent.parent.ClassName='TPageControl') then
            TPageControl(f.parent.parent).activePage:=TTabSheet(f.parent);
        end;
        if mainForm<>nil then mainForm.ActiveControl:=f;
      end;
      exit(true);
    end;
  end;

FUNCTION getFormOfType(CONST ideComponent:T_ideComponent):T_mnhComponentForm;
  VAR f:T_mnhComponentForm;
  begin
    result:=nil;
    for f in activeForms do if f.getIdeComponentType=ideComponent then exit(f);
  end;

PROCEDURE registerSynEdit(VAR edit:TSynEdit);
  begin
    setLength(activeSynEdits,length(activeSynEdits)+1);
    activeSynEdits[length(activeSynEdits)-1]:=edit;
    if length(activeSynEdits)=1 then begin
      edit.Font.name:=settings.editor.fontName;
      edit.Font.size:=settings.editor.fontSize;
      edit.Font.quality:=fqCleartypeNatural;
    end else edit.Font:=activeSynEdits[0].Font;
  end;

PROCEDURE unregisterSynEdit(VAR edit:TSynEdit);
  VAR k:longint=0;
  begin
    while (k<length(activeSynEdits)) and (activeSynEdits[k]<>edit) do inc(k);
    if k<length(activeSynEdits) then begin
      activeSynEdits[k]:=activeSynEdits[length(activeSynEdits)-1];
      setLength(activeSynEdits,length(activeSynEdits)-1);
    end;
  end;

PROCEDURE propagateEditorFont(newFont:TFont);
  VAR e:TSynEdit;
  begin
    for e in activeSynEdits do e.Font:=newFont;
  end;

CONSTRUCTOR T_mnhComponentForm.create(TheOwner: TComponent);
  VAR k:longint;
  begin
    inherited create(TheOwner);
    OnEndDock:=@defaultEndDock;
    k:=length(activeForms);
    setLength(activeForms,k+1);
    activeForms[k]:=self;
  end;

DESTRUCTOR T_mnhComponentForm.destroy;
  VAR k:longint=0;
  begin
    inherited destroy;
    while (k<length(activeForms)) and (activeForms[k]<>self) do inc(k);
    if k<length(activeForms) then begin
      activeForms[k]:=activeForms[length(activeForms)-1];
      setLength(activeForms,length(activeForms)-1);
    end;
  end;

PROCEDURE T_mnhComponentForm.defaultEndDock(Sender, target: TObject; X, Y: integer);
  VAR n:string;
  begin
    if (target<>nil) then begin
            writeln('Dock @',TComponent(target).name);

    if target.ClassNameIs('TPageControl') then begin
      n:=TPageControl(target).name;
      if (n.endsWith('1')) then myComponentParent:=cpPageControl1;
      if (n.endsWith('2')) then myComponentParent:=cpPageControl2;
      if (n.endsWith('3')) then myComponentParent:=cpPageControl3;
      if (n.endsWith('4')) then myComponentParent:=cpPageControl4;
    end else writeln('Unexpected dock at component of type ',target.ClassName);
    end else myComponentParent:=cpNone;
    lastDockLocationFor[getIdeComponentType]:=myComponentParent;
  end;

PROCEDURE performSlowUpdates;
  VAR f:T_mnhComponentForm;
  begin
    for f in activeForms do f.performSlowUpdate;
  end;

PROCEDURE performFastUpdates;
  VAR f:T_mnhComponentForm;
  begin
    for f in activeForms do f.performFastUpdate;
  end;

FUNCTION focusedEditor: TSynEdit;
  VAR e:TSynEdit;
  begin
    result:=nil;
    for e in activeSynEdits do if e.Focused then exit(e);
  end;

PROCEDURE saveMainFormLayout(VAR stream: T_bufferedOutputStreamWrapper; VAR splitters: T_splitterPositions);
  VAR k:longint;
      ic:T_ideComponent;
  begin
    stream.writeLongint(mainForm.top);
    stream.writeLongint(mainForm.Left);
    stream.writeLongint(mainForm.height);
    stream.writeLongint(mainForm.width);
    stream.writeByte(byte(mainForm.WindowState));

    for k:=1 to 4 do stream.writeLongint(splitters[k]);

    for ic in T_ideComponent do begin
      stream.writeByte(byte(lastDockLocationFor[ic]));
      stream.writeBoolean(hasFormOfType(ic));
    end;
  end;

FUNCTION loadMainFormLayout(VAR stream: T_bufferedInputStreamWrapper; VAR splitters: T_splitterPositions; OUT activeComponents:T_ideComponentSet):boolean;
  VAR k:longint;
      ic:T_ideComponent;
  begin
    mainForm.top   :=stream.readLongint;
    mainForm.Left  :=stream.readLongint;
    mainForm.height:=stream.readLongint;
    mainForm.width :=stream.readLongint;
    mainForm.WindowState:=TWindowState(stream.readByte);

    for k:=1 to 4 do splitters[k]:=stream.readLongint;
    result:=true;
    activeComponents:=[];
    for ic in T_ideComponent do begin
      lastDockLocationFor[ic]:=T_componentParent(stream.readByte);
      if stream.readBoolean then include(activeComponents,ic);
    end;

    result:=result and stream.allOkay;
  end;

INITIALIZATION
  initialize(lastDockLocationFor);
  setLength(activeForms,0);
end.

