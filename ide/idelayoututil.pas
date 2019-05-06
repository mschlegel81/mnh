UNIT ideLayoutUtil;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms,Controls,ComCtrls,Graphics,Menus,SynEdit,mnh_settings,serializationUtil,mnh_doc,mnh_constants,debugging,mnh_messages,
  SynEditTypes,SynExportHTML,SynEditHighlighter;

CONST
  CL_INACTIVE_GREY=TColor($E0E0E0);

TYPE
  T_windowStateForUpdate=(wsfuNone,wsfuNormal,wsfuMaximized,wsfuFullscreen);
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
CONST
  PAGES:set of T_componentParent=[cpPageControl1..cpPageControl4];
TYPE
  T_mnhComponentForm=class(TForm)
    published
      CONSTRUCTOR create(TheOwner: TComponent); override;
      PROCEDURE defaultEndDock(Sender, target: TObject; X,Y: integer);
      FUNCTION getIdeComponentType:T_ideComponent; virtual; abstract;
      PROCEDURE performSlowUpdate; virtual; abstract;
      PROCEDURE performFastUpdate; virtual; abstract;
      PROCEDURE getParents(OUT page:TTabSheet; OUT PageControl:TPageControl);
      PROCEDURE tabNextKeyHandling(Sender: TObject; VAR key: word; Shift: TShiftState);
      PROCEDURE showComponent(CONST retainOriginalFocus:boolean);
    public
      myComponentParent:T_componentParent;
      DESTRUCTOR destroy; override;
  end;

  P_mnhDockSiteModel=^T_mnhDockSiteModel;
  T_mnhDockSiteModel=object
    private
      PageControl:TPageControl;
      undockMi,closeMi:TMenuItem;
      undockMenu:TPopupMenu;
      canScaleWidth:boolean;
      dockId:T_componentParent;
      relativeSize:word;
      FUNCTION getAbsSize:longint;
      PROCEDURE setAbsSize(CONST value:longint);
      FUNCTION getFormSize:longint;

      FUNCTION canCloseActivePage:boolean;
      PROCEDURE closeActivePage;
    public
      CONSTRUCTOR create(CONST dockId_:T_componentParent;
                         CONST pageControl_:TPageControl;
                         CONST undockMi_,closeMi_:TMenuItem;
                         CONST undockMenu_:TPopupMenu);
      DESTRUCTOR destroy;
      PROPERTY absSize:longint read getAbsSize write setAbsSize;
      PROCEDURE updateAbsSizeByRelSize;
      PROCEDURE updateRelSizeByAbsSize;
      PROCEDURE fixSize;

      PROCEDURE menuPopup(Sender:TObject);
      PROCEDURE closeClick(Sender:TObject);
      FUNCTION  undockCurrent:boolean;
      PROCEDURE undockClick(Sender:TObject);
      PROCEDURE undockAll;
  end;

  T_mnhIdeForm=class(TForm)
    PROCEDURE attachNewForm(CONST form:T_mnhComponentForm);   virtual; abstract;
    PROCEDURE onEditFinished(CONST data:P_storedMessage    ); virtual; abstract;
    PROCEDURE onBreakpoint  (CONST data:P_debuggingSnapshot); virtual; abstract;
    PROCEDURE onDebuggerEvent;                                virtual; abstract;
    PROCEDURE onEndOfEvaluation;                              virtual; abstract;
    protected
      windowStateForUpdate:T_windowStateForUpdate;
      dockSites:array[T_componentParent] of P_mnhDockSiteModel;
  end;

  T_htmlExporter=object
    FUNCTION textToHtml(CONST title:string; CONST content:TStrings; CONST highlighter:TSynCustomHighlighter):string;
    PROCEDURE OutputSynEditCutCopy(Sender: TObject; VAR AText: string;
      VAR AMode: TSynSelectionMode; ALogStartPos: TPoint;
      VAR AnAction: TSynCopyPasteAction);
  end;

TYPE T_dockSetup=array[T_ideComponent] of T_componentParent;
CONST C_dockSetupDockAll:T_dockSetup
    {icOutline}   =(cpPageControl3,
    {icHelp}        cpPageControl2,
    {icAssistance}  cpPageControl2,
    {icOutput}      cpPageControl2,
    {icQuickEval}   cpPageControl2,
    {icDebugger}    cpPageControl2,
    {icDebuggerVari}cpPageControl1,
    {icDebuggerBrea}cpPageControl1,
    {icPlot}        cpPageControl1,
    {icCustomForm}  cpPageControl1,
    {icTable}       cpPageControl1,
    {icVariableView}cpPageControl1);
  C_dockSetupUnDockAll:T_dockSetup
     {icOutline}   =(cpNone,
     {icHelp}        cpNone,
     {icAssistance}  cpNone,
     {icOutput}      cpNone,
     {icQuickEval}   cpNone,
     {icDebugger}    cpNone,
     {icDebuggerVari}cpNone,
     {icDebuggerBrea}cpNone,
     {icPlot}        cpNone,
     {icCustomForm}  cpNone,
     {icTable}       cpNone,
     {icVariableView}cpNone);

VAR lastDockLocationFor:T_dockSetup
    {icOutline}   =(cpPageControl3,
    {icHelp}        cpPageControl2,
    {icAssistance}  cpPageControl2,
    {icOutput}      cpPageControl2,
    {icQuickEval}   cpPageControl2,
    {icDebugger}    cpPageControl2,
    {icDebuggerVari}cpPageControl1,
    {icDebuggerBrea}cpPageControl1,
    {icPlot}        cpPageControl1,
    {icCustomForm}  cpPageControl1,
    {icTable}       cpPageControl1,
    {icVariableView}cpPageControl1);

    mainForm:T_mnhIdeForm=nil;

PROCEDURE dockNewForm(newForm:T_mnhComponentForm);
FUNCTION hasAnyForm:boolean;
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

PROCEDURE saveMainFormLayout(VAR stream:T_bufferedOutputStreamWrapper);
FUNCTION loadMainFormLayout(VAR stream: T_bufferedInputStreamWrapper; OUT activeComponents:T_ideComponentSet):boolean;
PROCEDURE dockAllForms;

OPERATOR :=(x:byte):TFontStyles;
OPERATOR :=(x:TFontStyles):byte;

TYPE F_getFontSize= FUNCTION (CONST c:T_controlType): longint of object;
     F_setFontSize= PROCEDURE (CONST c:T_controlType; CONST value: longint) of object;
VAR getFontSize_callback:F_getFontSize=nil;
    setFontSize_callback:F_setFontSize=nil;
    htmlExporter:T_htmlExporter;
VAR doShowSplashScreen:boolean;
IMPLEMENTATION
USES math,litVar,recyclers,basicTypes,contexts,funcs,Clipbrd;
VAR activeForms:array of T_mnhComponentForm;
    fontControls:array[T_controlType] of array of TWinControl;

PROCEDURE dockNewForm(newForm: T_mnhComponentForm);
  begin
    if mainForm<>nil then mainForm.attachNewForm(newForm)
    else begin
      newForm.DragKind:=dkDrag;
      newForm.DragMode:=dmManual;
      newForm.ShowInTaskBar:=stAlways;
      newForm.Show;
    end;
  end;

PROCEDURE dockAllForms;
  VAR f:T_mnhComponentForm;
  begin
    lastDockLocationFor:=C_dockSetupDockAll;
    for f in activeForms do dockNewForm(f);
    lastDockLocationFor:=C_dockSetupDockAll;
  end;

FUNCTION T_htmlExporter.textToHtml(CONST title:string; CONST content:TStrings; CONST highlighter:TSynCustomHighlighter): string;
  VAR SynExporterHTML: TSynExporterHTML;
      outputStream:TMemoryStream;
      size:longint;
  begin
    SynExporterHTML:=TSynExporterHTML.create(nil);
    SynExporterHTML.title:=title;
    SynExporterHTML.highlighter:=highlighter;
    SynExporterHTML.ExportAll(content);
    outputStream:=TMemoryStream.create();
    SynExporterHTML.saveToStream(outputStream);
    SynExporterHTML.free;
    size:=outputStream.size;
    outputStream.Seek(0,soFromBeginning);
    setLength(result,size);
    outputStream.ReadBuffer(result[1],size);
    outputStream.free;
  end;

PROCEDURE T_htmlExporter.OutputSynEditCutCopy(Sender: TObject;
  VAR AText: string; VAR AMode: TSynSelectionMode; ALogStartPos: TPoint;
  VAR AnAction: TSynCopyPasteAction);
  VAR content:TStringList;
begin
  if (Sender.ClassName<>'TSynEdit') or
     (AnAction<>scaPlainText) or
     (TSynEdit(Sender).highlighter=nil) or
     (Clipboard=nil)
  then exit;
  content:=TStringList.create;
  content.text:=AText;
  Clipboard.SetAsHtml(textToHtml('',content,TSynEdit(Sender).highlighter), AText);
  AnAction:=scaAbort;
  content.free;
end;

FUNCTION T_mnhDockSiteModel.getAbsSize: longint;
  begin
    if PageControl=nil then exit(0);
    if canScaleWidth
    then result:=PageControl.width
    else result:=PageControl.height;
  end;

PROCEDURE T_mnhDockSiteModel.setAbsSize(CONST value: longint);
  begin
    if PageControl=nil then exit;
    if canScaleWidth
    then PageControl.width :=value
    else PageControl.height:=value;
  end;

FUNCTION T_mnhDockSiteModel.getFormSize: longint;
  begin
    if canScaleWidth
    then result:=mainForm.width
    else result:=mainForm.height;
  end;

FUNCTION T_mnhDockSiteModel.canCloseActivePage: boolean;
  begin
    result:=(PageControl.activePage.ControlCount=1)
      and (PageControl.activePage.Controls[0].InheritsFrom(T_mnhComponentForm.ClassType))
      and T_mnhComponentForm(PageControl.activePage.Controls[0]).CloseQuery;
  end;

PROCEDURE T_mnhDockSiteModel.closeActivePage;
  VAR active:T_mnhComponentForm;
      CloseAction:TCloseAction=caFree;
  begin
    if (PageControl.activePage.ControlCount=1) and (PageControl.activePage.Controls[0].InheritsFrom(T_mnhComponentForm.ClassType))
    then begin
      active:=T_mnhComponentForm(PageControl.activePage.Controls[0]);
      if not(active.CloseQuery) then exit;
      if active.OnClose<>nil then active.OnClose(PageControl,CloseAction);
      if CloseAction=caFree then FreeAndNil(active);
    end;
  end;

CONSTRUCTOR T_mnhDockSiteModel.create(CONST dockId_: T_componentParent;
                                      CONST pageControl_: TPageControl;
                                      CONST undockMi_, closeMi_: TMenuItem;
                                      CONST undockMenu_: TPopupMenu);
  begin
    dockId       :=dockId_;
    canScaleWidth:=dockId<>cpPageControl2;
    PageControl  :=pageControl_;
    undockMenu   :=undockMenu_;
    undockMi     :=undockMi_;
    closeMi      :=closeMi_;
    if dockId=cpNone then exit;
    undockMenu.OnPopup:=@menuPopup;
    closeMi.OnClick:=@closeClick;
    undockMi.OnClick:=@undockClick;
  end;

DESTRUCTOR T_mnhDockSiteModel.destroy; begin end;

PROCEDURE T_mnhDockSiteModel.updateAbsSizeByRelSize;
  begin
    absSize:=relativeSize*getFormSize div 65535;
    fixSize;
  end;

PROCEDURE T_mnhDockSiteModel.updateRelSizeByAbsSize;
  begin
    relativeSize:=absSize*65535 div getFormSize;
  end;

PROCEDURE T_mnhDockSiteModel.fixSize;
  begin
    if PageControl=nil then exit;
    if PageControl.PageCount=0 then begin
      absSize     :=0;
      relativeSize:=0;
    end else if absSize<0.05*getFormSize then begin
      absSize     :=round(0.2*getFormSize);
      relativeSize:=13107; //=0.2*65535
    end;
  end;

PROCEDURE T_mnhDockSiteModel.menuPopup(Sender: TObject);
  begin
    undockMi.enabled:=(PageControl.activePage<>nil);
    closeMi .enabled:=canCloseActivePage;
  end;

PROCEDURE T_mnhDockSiteModel.closeClick(Sender: TObject);
  begin
    closeActivePage;
    fixSize;
  end;

FUNCTION T_mnhDockSiteModel.undockCurrent: boolean;
  VAR control:TControl;
      newForm:T_mnhComponentForm;
  begin
    if PageControl.PageCount<=0 then exit(false);
    //Only handle pages with one control
    if PageControl.activePage.ControlCount<>1 then exit(false);
    control:=PageControl.activePage.Controls[0];
    //If the sheet is a TForm return it directly
    if control.ClassType.InheritsFrom(T_mnhComponentForm.ClassType)
    then newForm:=T_mnhComponentForm(control)
    else raise Exception.create('Not an mnhComponent form!');
    newForm.ManualDock(nil);
    newForm.BringToFront;
    newForm.myComponentParent:=cpNone;
    newForm.ShowInTaskBar:=stAlways;
    result:=true;
  end;

PROCEDURE T_mnhDockSiteModel.undockClick(Sender: TObject);
  begin
    if undockCurrent then fixSize;
  end;

PROCEDURE T_mnhDockSiteModel.undockAll;
  VAR needSizeFix:boolean=false;
  begin
    while undockCurrent do needSizeFix:=true;
    if needSizeFix then fixSize;
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

PROCEDURE T_mnhComponentForm.showComponent(CONST retainOriginalFocus:boolean);
  VAR page:TTabSheet;
      PageControl:TPageControl;
      oldActive:TWinControl;
  begin
    getParents(page,PageControl);
    if PageControl=nil then begin
      if not(showing) then begin
        Show;
        BringToFront;
      end;
    end else begin
      oldActive:=mainForm.ActiveControl;
      Show;
      PageControl.activePage:=page;
      if (mainForm<>nil) then begin
        if retainOriginalFocus and (oldActive<>nil)
        then mainForm.ActiveControl:=oldActive
        else mainForm.ActiveControl:=self;
      end;
    end;
  end;

FUNCTION hasAnyForm:boolean;
  {$ifdef debugMode}
  VAR f:T_mnhComponentForm;
  {$endif}
  begin
    result:=length(activeForms)>0;
    {$ifdef debugMode}
    write('Active Forms:');
    for f in activeForms do write(f.getIdeComponentType,' ');
    writeln;
    {$endif}
  end;

FUNCTION hasFormOfType(CONST ideComponent:T_ideComponent; CONST BringToFront:boolean=false):boolean;
  VAR f:T_mnhComponentForm;
  begin
    result:=false;
    for f in activeForms do if f.getIdeComponentType=ideComponent then begin
      if BringToFront then f.showComponent(false);
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

    if control.ClassName='TSynEdit' then TSynEdit(control).OnCutCopy:=@htmlExporter.OutputSynEditCutCopy;
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

PROCEDURE saveMainFormLayout(VAR stream: T_bufferedOutputStreamWrapper);
  VAR ic:T_ideComponent;
      cp:T_componentParent;
  begin
    stream.writeLongint(mainForm.top);
    stream.writeLongint(mainForm.Left);
    stream.writeLongint(mainForm.height);
    stream.writeLongint(mainForm.width);
    case mainForm.WindowState of
      wsMaximized : stream.writeByte(byte(wsfuMaximized));
      wsFullScreen: stream.writeByte(byte(wsfuFullscreen));
      else          stream.writeByte(byte(wsfuNormal));
    end;

    for cp in PAGES do stream.writeWord(mainForm.dockSites[cp]^.relativeSize);

    for ic in T_ideComponent do begin
      stream.writeByte(byte(lastDockLocationFor[ic]));
      stream.writeBoolean(hasFormOfType(ic));
    end;

    stream.writeBoolean(doShowSplashScreen);
    stream.writeAnsiString(htmlDocGeneratedForCodeHash);
  end;

FUNCTION loadMainFormLayout(VAR stream: T_bufferedInputStreamWrapper; OUT activeComponents:T_ideComponentSet):boolean;
  VAR cp:T_componentParent;
      ic:T_ideComponent;
  begin
    mainForm.top   :=min(max(stream.readLongint,0  ),screen.height-100);
    mainForm.Left  :=min(max(stream.readLongint,0  ),screen.width-100);
    mainForm.height:=min(max(stream.readLongint,100),screen.height);
    mainForm.width :=min(max(stream.readLongint,100),screen.width);
    mainForm.windowStateForUpdate:=T_windowStateForUpdate(stream.readByte([byte(wsfuFullscreen),byte(wsfuMaximized),byte(wsfuNormal)]));

    for cp in PAGES do mainForm.dockSites[cp]^.relativeSize:=stream.readWord;
    result:=true;
    activeComponents:=[];
    for ic in T_ideComponent do begin
      lastDockLocationFor[ic]:=T_componentParent(stream.readByte);
      if stream.readBoolean then include(activeComponents,ic);
    end;

    doShowSplashScreen:=stream.readBoolean;
    htmlDocGeneratedForCodeHash:=stream.readAnsiString;
    result:=stream.allOkay;
    if not(result) then begin
      mainForm.windowStateForUpdate:=wsfuNone;
      for cp in PAGES do mainForm.dockSites[cp]^.relativeSize:=0;
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

{$i func_defines.inc}
FUNCTION anyFormShowing_imp intFuncSignature;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then result:=newBoolLiteral(hasAnyForm);
  end;

INITIALIZATION
  initialize(lastDockLocationFor);
  setLength(activeForms,0);
  setLength(fontControls[ctEditor ],0);
  setLength(fontControls[ctTable  ],0);
  setLength(fontControls[ctGeneral],0);
  registerRule(GUI_NAMESPACE,'anyFormShowing',@anyFormShowing_imp,ak_nullary,'anyFormShowing();//returns true if any form is showing');
end.

