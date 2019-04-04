UNIT ideLayoutUtil;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms,Controls,ComCtrls,Graphics,Menus,SynEdit,mnh_settings,serializationUtil,mnh_doc,mnh_constants,debugging,editScripts,mnh_messages;

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
    published
      CONSTRUCTOR create(TheOwner: TComponent); override;
      PROCEDURE defaultEndDock(Sender, target: TObject; X,Y: integer);
      FUNCTION getIdeComponentType:T_ideComponent; virtual; abstract;
      PROCEDURE performSlowUpdate; virtual; abstract;
      PROCEDURE performFastUpdate; virtual; abstract;
      PROCEDURE getParents(OUT page:TTabSheet; OUT PageControl:TPageControl);
      PROCEDURE tabNextKeyHandling(Sender: TObject; VAR key: word; Shift: TShiftState);
      PROCEDURE showComponent;
    public
      myComponentParent:T_componentParent;
      DESTRUCTOR destroy; override;
  end;

  T_mnhIdeForm=class(TForm)
    PROCEDURE attachNewForm(CONST form:T_mnhComponentForm);   virtual; abstract;
    PROCEDURE onEditFinished(CONST data:P_storedMessage    ); virtual; abstract;
    PROCEDURE onBreakpoint  (CONST data:P_debuggingSnapshot); virtual; abstract;
    PROCEDURE onDebuggerEvent;                                virtual; abstract;
    PROCEDURE onEndOfEvaluation;                              virtual; abstract;
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

PROCEDURE registerFontControl(control:TWinControl; CONST controlType:T_controlType);
PROCEDURE unregisterFontControl(control:TWinControl);
PROCEDURE propagateFont(newFont:TFont; CONST controlType:T_controlType);
FUNCTION getFontSize(CONST c:T_controlType): longint;
PROCEDURE setFontSize (CONST c:T_controlType; CONST value: longint);

PROCEDURE performSlowUpdates;
PROCEDURE performFastUpdates;
FUNCTION  focusedEditor:TSynEdit;
FUNCTION  typeOfFocusedControl:T_controlType;

PROCEDURE saveMainFormLayout(VAR stream:T_bufferedOutputStreamWrapper; VAR splitters:T_splitterPositions);
FUNCTION loadMainFormLayout(VAR stream: T_bufferedInputStreamWrapper; VAR splitters: T_splitterPositions; OUT activeComponents:T_ideComponentSet):boolean;

OPERATOR :=(x:byte):TFontStyles;
OPERATOR :=(x:TFontStyles):byte;

TYPE F_getFontSize= FUNCTION (CONST c:T_controlType): longint of object;
     F_setFontSize= PROCEDURE (CONST c:T_controlType; CONST value: longint) of object;
VAR getFontSize_callback:F_getFontSize=nil;
    setFontSize_callback:F_setFontSize=nil;

VAR doShowSplashScreen:boolean;
IMPLEMENTATION
USES math;
VAR activeForms:array of T_mnhComponentForm;
    fontControls:array[T_controlType] of array of TWinControl;

PROCEDURE dockNewForm(newForm: T_mnhComponentForm);
  begin
    if mainForm<>nil then mainForm.attachNewForm(newForm)
    else begin
      newForm.DragKind:=dkDrag;
      newForm.DragMode:=dmManual;
      newForm.Show;
    end;
  end;

PROCEDURE T_mnhComponentForm.getParents(OUT page:TTabSheet; OUT PageControl:TPageControl);
  begin
    page:=nil;
    PageControl:=nil;;
    if (parent<>nil) and (parent.ClassName='TTabSheet') then begin
      page:=TTabSheet(parent);
      if (page.parent<>nil) and (page.parent.ClassName='TPageControl')
      then PageControl:=TPageControl(page.parent);
    end;
  end;

PROCEDURE T_mnhComponentForm.tabNextKeyHandling(Sender: TObject; VAR key: word; Shift: TShiftState);
  VAR page:TTabSheet;
      PageControl:TPageControl;
  begin
    if ((key=33) or (key=34)) and (ssCtrl in Shift) then begin
      getParents(page,PageControl);
      if PageControl<>nil then begin
        if key=33
        then PageControl.activePageIndex:=(PageControl.activePageIndex+1                      ) mod PageControl.PageCount
        else PageControl.activePageIndex:=(PageControl.activePageIndex+PageControl.PageCount-1) mod PageControl.PageCount;
        if mainForm<>nil then mainForm.ActiveControl:=PageControl.activePage;
      end;
    end;
  end;

PROCEDURE T_mnhComponentForm.showComponent;
  VAR page:TTabSheet;
      PageControl:TPageControl;
  begin
    getParents(page,PageControl);
    if PageControl=nil then begin
      Show;
      BringToFront;
    end else begin
      PageControl.activePage:=page;
      if mainForm<>nil then mainForm.ActiveControl:=self;
    end;
  end;

FUNCTION hasFormOfType(CONST ideComponent:T_ideComponent; CONST BringToFront:boolean=false):boolean;
  VAR f:T_mnhComponentForm;

  begin
    result:=false;
    for f in activeForms do if f.getIdeComponentType=ideComponent then begin
      if BringToFront then f.showComponent;
      exit(true);
    end;
  end;

FUNCTION getFormOfType(CONST ideComponent:T_ideComponent):T_mnhComponentForm;
  VAR f:T_mnhComponentForm;
  begin
    result:=nil;
    for f in activeForms do if f.getIdeComponentType=ideComponent then exit(f);
  end;

PROCEDURE registerFontControl(control:TWinControl; CONST controlType:T_controlType);
  begin
    if (controlType=ctEditor) and (control.ClassName<>'TSynEdit') then raise Exception.create('Invalid control for type ctEditor');

    setLength(fontControls[controlType],length(fontControls[controlType])+1);
    fontControls[controlType][length(fontControls[controlType])-1]:=control;
    if length(fontControls[controlType])=1 then begin
      control.Font.name:=settings.Font[controlType].fontName;
      control.Font.size:=settings.Font[controlType].fontSize;
      control.Font.style:=settings.Font[controlType].style;
      control.Font.quality:=fqCleartypeNatural;
    end else control.Font:=fontControls[controlType][0].Font;
  end;

PROCEDURE unregisterFontControl(control:TWinControl);
  VAR k:longint=0;
      c:T_controlType;
  begin
    for c in T_controlType do begin
      k:=0;
      while (k<length(fontControls[c])) and (fontControls[c][k]<>control) do inc(k);
      if k<length(fontControls[c]) then begin
        fontControls[c][k]:=fontControls[c][length(fontControls[c])-1];
        setLength(fontControls[c],length(fontControls[c])-1);
      end;
    end;
  end;

PROCEDURE propagateFont(newFont:TFont; CONST controlType:T_controlType);
  VAR e:TControl;
  begin
    for e in fontControls[controlType] do e.Font:=newFont;
    settings.Font[controlType].fontName:=newFont.name;
    settings.Font[controlType].fontSize:=newFont.size;
    settings.Font[controlType].style:=newFont.style;
  end;

FUNCTION getFontSize(CONST c: T_controlType): longint;
  begin
    if getFontSize_callback<>nil
    then exit(getFontSize_callback(c))
    else result:=settings.Font[c].fontSize;
  end;

PROCEDURE setFontSize(CONST c: T_controlType; CONST value: longint);
  VAR e:TControl;
  begin
    if setFontSize_callback<>nil
    then begin setFontSize_callback(c,value); exit; end
    else begin
      for e in fontControls[c] do e.Font.size:=value;
      settings.Font[c].fontSize:=value;
    end;
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
    if target.ClassNameIs('TPageControl') then begin
      n:=TPageControl(target).name;
      if (n.endsWith('1')) then myComponentParent:=cpPageControl1;
      if (n.endsWith('2')) then myComponentParent:=cpPageControl2;
      if (n.endsWith('3')) then myComponentParent:=cpPageControl3;
      if (n.endsWith('4')) then myComponentParent:=cpPageControl4;
    end else writeln('Unexpected dock at component of type ',target.ClassName);
    end else myComponentParent:=cpNone;
    lastDockLocationFor[getIdeComponentType]:=myComponentParent;

    if myComponentParent=cpNone then begin
      if width <100 then width :=100;
      if height<100 then height:=100;
    end;
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
  VAR e:TWinControl;
  begin
    result:=nil;
    for e in fontControls[ctEditor] do if e.Focused then exit(TSynEdit(e));
  end;

FUNCTION typeOfFocusedControl:T_controlType;
  VAR e:TWinControl;
      c:T_controlType;
      active:TWinControl;
  begin
    result:=ctNoneOrUnknown;
    if mainForm=nil then exit(ctNoneOrUnknown);
    active:=mainForm.ActiveControl;

    if active.ClassName='TSynEdit' then exit(ctEditor);
    if active.ClassName='TTreeView' then exit(ctGeneral);
    if active.ClassName='TListBox' then exit(ctGeneral);
    if active.ClassName='TplotForm' then exit(ctPlot);
    if active.ClassName='TStringGrid' then exit(ctTable);

    {$ifdef debugMode}
    writeln('Unknown control class ',active.ClassName);
    {$endif}

    for c in T_controlType do
    for e in fontControls[ctEditor] do if e=mainForm.ActiveControl then exit(c);
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

    for k:=1 to 4 do stream.writeWord(max(0,min(65535,splitters[k])));

    for ic in T_ideComponent do begin
      stream.writeByte(byte(lastDockLocationFor[ic]));
      stream.writeBoolean(hasFormOfType(ic));
    end;

    stream.writeBoolean(doShowSplashScreen);
    stream.writeAnsiString(htmlDocGeneratedForCodeHash);
  end;

FUNCTION loadMainFormLayout(VAR stream: T_bufferedInputStreamWrapper; VAR splitters: T_splitterPositions; OUT activeComponents:T_ideComponentSet):boolean;
  VAR k:longint;
      ic:T_ideComponent;
      intendedWindowState:TWindowState;
  begin
    mainForm.top   :=min(max(stream.readLongint,0  ),screen.height-100);
    mainForm.Left  :=min(max(stream.readLongint,0  ),screen.width-100);
    mainForm.height:=min(max(stream.readLongint,100),screen.height);
    mainForm.width :=min(max(stream.readLongint,100),screen.width);
    intendedWindowState:=TWindowState(stream.readByte([byte(wsFullScreen),byte(wsMaximized),byte(wsNormal)]));
    if not(stream.allOkay) then exit(false);
    if intendedWindowState=wsFullScreen
    then mainForm.BorderStyle:=bsNone
    else mainForm.BorderStyle:=bsSizeable;
    mainForm.WindowState:=intendedWindowState;

    for k:=1 to 4 do splitters[k]:=stream.readWord;
    result:=true;
    activeComponents:=[];
    for ic in T_ideComponent do begin
      lastDockLocationFor[ic]:=T_componentParent(stream.readByte);
      if stream.readBoolean then include(activeComponents,ic);
    end;

    doShowSplashScreen:=stream.readBoolean;
    htmlDocGeneratedForCodeHash:=stream.readAnsiString;

    result:=result and stream.allOkay and (length(htmlDocGeneratedForCodeHash)=length(CODE_HASH));
    if not(result) then begin
      mainForm.BorderStyle:=bsSizeable;
      mainForm.WindowState:=wsMaximized;
      for k:=1 to 4 do splitters[k]:=0;
      doShowSplashScreen:=true;
      htmlDocGeneratedForCodeHash:='';
    end;
  end;

OPERATOR:=(x: byte): TFontStyles;
  begin
    result:=[];
    if (x and FONT_STYLE_BOLD  >0) then include(result,fsBold);
    if (x and FONT_STYLE_ITALIC>0) then include(result,fsItalic);
  end;

OPERATOR:=(x: TFontStyles): byte;
  begin
    result:=0;
    if fsBold   in x then result+=FONT_STYLE_BOLD;
    if fsItalic in x then result+=FONT_STYLE_ITALIC;
  end;

INITIALIZATION
  initialize(lastDockLocationFor);
  setLength(activeForms,0);
  setLength(fontControls[ctEditor ],0);
  setLength(fontControls[ctTable  ],0);
  setLength(fontControls[ctGeneral],0);
end.

