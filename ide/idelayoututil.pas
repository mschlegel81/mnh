UNIT ideLayoutUtil;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms,Controls,ComCtrls,Graphics,myGenerics,Menus,SynEdit,evalThread,mnh_settings,debugging,debuggingVar;

TYPE
  T_ideComponent=(icOutline,
                  icHelp,
                  icAssistance,
                  icOutput,
                  icQuickEval,
                  icPlot,
                  icCustomForm,
                  icTable,
                  icVariableView,
                  icDebugger,
                  icDebuggerVariables,
                  icDebuggerBreakpoints);

  T_componentParent=(cpNone,
                     cpPageControl1,
                     cpPageControl2,
                     cpPageControl3,
                     cpPageControl4);

  T_splitterPositions=array[1..4] of longint;

  T_componentPosition=record
    parent:T_componentParent;
    top,Left,width,height:longint;
  end;

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
    {icPlot}        cpNone,
    {icCustomForm}  cpNone,
    {icTable}       cpNone,
    {icVariableView}cpNone,
    {icDebugger}    cpPageControl4,
    {icDebuggerVari}cpPageControl4,
    {icDebuggerBrea}cpPageControl4);

    mainForm:T_mnhIdeForm=nil;
    debuggerData:record
      currentSnapshot:P_debuggingSnapshot;
      globalVariableReport,
      localVariableReport,
      inlineVariableReport:P_variableTreeEntryCategoryNode;
    end=(currentSnapshot     :nil;
         globalVariableReport:nil;
         localVariableReport :nil;
         inlineVariableReport:nil);


PROCEDURE dockNewForm(newForm:T_mnhComponentForm);
FUNCTION hasFormOfType(CONST ideComponent:T_ideComponent):boolean;
PROCEDURE registerSynEdit(VAR edit:TSynEdit);
PROCEDURE unregisterSynEdit(VAR edit:TSynEdit);
PROCEDURE propagateEditorFont(newFont:TFont);
PROCEDURE performSlowUpdates;
PROCEDURE performFastUpdates;
FUNCTION focusedEditor:TSynEdit;
IMPLEMENTATION
VAR activeForms:array of T_mnhComponentForm;
    activeSynEdits:array of TSynEdit;

PROCEDURE dockNewForm(newForm: T_mnhComponentForm);
  begin
    if mainForm<>nil then mainForm.attachNewForm(newForm);
  end;

FUNCTION hasFormOfType(CONST ideComponent:T_ideComponent):boolean;
  VAR f:T_mnhComponentForm;
  begin
    result:=false;
    for f in activeForms do if f.getIdeComponentType=ideComponent then exit(true);
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

INITIALIZATION
  initialize(lastDockLocationFor);
  setLength(activeForms,0);
end.

