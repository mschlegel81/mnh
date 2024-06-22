UNIT ideLayoutUtil;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms,Controls,ComCtrls,Graphics,Menus,SynEdit,mnh_settings,serializationUtil,mnh_constants,debugging,mnh_messages,
  SynEditTypes,SynExportHTML,SynEditHighlighter,myGenerics;

TYPE
  T_registeredAssociation=(raNone,raFullVersion);
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
                  icVariableView,
                  icProfilingOutput,
                  icIdeEvents);
  T_ideComponentSet=set of T_ideComponent;

  T_componentParent=(cpNone,
                     cpPageControl1,
                     cpPageControl2,
                     cpPageControl3,
                     cpPageControl4);
VAR COMPONENT_SHORTCUT:array [T_ideComponent] of string;
CONST
  COMPONENT_CAPTION :array [T_ideComponent] of string=('Outline','Help','Assistance','Output','Quick evaluation','Debugger','Debugger - Variables','Breakpoints','Plot','Custom Form',
                                                       'Table','Variable','Profiling output','Events');
  PAGES:set of T_componentParent=[cpPageControl1..cpPageControl4];
TYPE
  T_windowPosition=record
    Left,top,height,width:longint;
  end;

  T_mnhComponentForm=class(TForm)
    published
      CONSTRUCTOR create(TheOwner: TComponent); override;
      PROCEDURE defaultEndDock(Sender, target: TObject; X,Y: integer);
      FUNCTION getCaption:string; virtual;
      FUNCTION getIdeComponentType:T_ideComponent; virtual; abstract;
      PROCEDURE performSlowUpdate(CONST isEvaluationRunning:boolean); virtual; abstract;
      PROCEDURE performFastUpdate; virtual; abstract;
      FUNCTION getDefaultControl:TWinControl; virtual;
      PROCEDURE getParents(OUT page:TTabSheet; OUT PageControl:TPageControl);
      PROCEDURE tabNextKeyHandling(Sender: TObject; VAR key: word; Shift: TShiftState);
      PROCEDURE showComponent(CONST retainOriginalFocus:boolean);
      PROCEDURE dockChanged; virtual; abstract;
      PROCEDURE changeDock(CONST newSite:T_componentParent);
      PROCEDURE defaultReattachClick(Sender:TObject);
      PROCEDURE defaultUndockClick(Sender:TObject);
      PROCEDURE defaultCloseClick(Sender:TObject);
      PROCEDURE defaultDockSite1Click(Sender:TObject);
      PROCEDURE defaultDockSite2Click(Sender:TObject);
      PROCEDURE defaultDockSite3Click(Sender:TObject);
      PROCEDURE defaultDockSite4Click(Sender:TObject);
      PROCEDURE initDockMenuItems(CONST menuToInit:TMenu; CONST dockRoot:TMenuItem);
      PROCEDURE setComponentFormVisible(CONST visible_:boolean);
      FUNCTION isDocked:boolean;
    public
      lastDock,
      myComponentParent:T_componentParent;
      DESTRUCTOR destroy; override;
  end;

  P_mnhDockSiteModel=^T_mnhDockSiteModel;
  T_mnhDockSiteModel=object
    private
      PageControl:TPageControl;
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
                         CONST pageControl_:TPageControl);
      DESTRUCTOR destroy;
      PROPERTY absSize:longint read getAbsSize write setAbsSize;
      PROCEDURE updateAbsSizeByRelSize;
      PROCEDURE updateRelSizeByAbsSize;
      PROCEDURE fixSize;
      PROCEDURE tabNextKeyHandling(Sender: TObject; VAR key: word; Shift: TShiftState);
      FUNCTION  undockCurrent:boolean;
      PROCEDURE undockAll;
  end;

  T_mnhIdeForm=class(TForm)
    dockImages: TImageList;
    PROCEDURE attachNewForm(CONST form:T_mnhComponentForm);   virtual; abstract;
    PROCEDURE onEditFinished(CONST data:P_storedMessage    ); virtual; abstract;
    PROCEDURE onBreakpoint  (CONST data:P_debuggingSnapshot); virtual; abstract;
    PROCEDURE onDebuggerEvent;                                virtual; abstract;
    PROCEDURE onEndOfEvaluation;                              virtual; abstract;
    protected
      dockSites:array[T_componentParent] of P_mnhDockSiteModel;
  end;

  T_htmlExporter=object
    FUNCTION textToHtml(CONST title:string; CONST content:TStrings; CONST highlighter:TSynCustomHighlighter):string;
    PROCEDURE OutputSynEditCutCopy(Sender: TObject; VAR AText: string; VAR AMode: TSynSelectionMode; ALogStartPos: TPoint; VAR AnAction: TSynCopyPasteAction);
  end;

  T_outlineSettings=object(T_serializable)
    showPrivate,
    showImported:boolean;
    ruleSorting:T_ruleSorting;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  end;

  T_ideSettings=object(T_serializable)
    private
      currentWorkspace:string;
      workspaceHistory:T_arrayOfString;
    public
    activeComponents:T_ideComponentSet;
    windowStateForUpdate:T_windowStateForUpdate;
    ideComponentSize:array[T_ideComponent] of T_windowPosition;

    doShowSplashScreen:boolean;
    copyTextAsHtml:boolean;

    registeredAssociation:T_registeredAssociation;
    //IDE:
    Font:array[ctEditor..ctGeneral] of record
      fontName :string;
      style    :byte;
      fontSize :longint;
    end;
    doResetPlotOnEvaluation: boolean;
    cacheAnimationFrames: boolean;

    outputBehavior,
    quickOutputBehavior: T_ideMessageConfig;
    outputLinesLimit:longint;
    outputLinesLimitPerLiteral:longint;
    forceFullLiterals:boolean;

    outlineSettings:T_outlineSettings;

    CONSTRUCTOR create;
    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
    PROCEDURE initDefaults;
    FUNCTION workspaceFilename: string;
    PROPERTY allWorkspaceFiles:T_arrayOfString read workspaceHistory;
  end;

VAR
  mainForm:T_mnhIdeForm=nil;
  ideSettings:T_ideSettings;

PROCEDURE dockNewForm(newForm:T_mnhComponentForm);
FUNCTION hasAnyForm:boolean;
FUNCTION hasFormOfType(CONST ideComponent:T_ideComponent; CONST BringToFront:boolean=false):boolean;
FUNCTION getFormOfType(CONST ideComponent:T_ideComponent):T_mnhComponentForm;

PROCEDURE registerFontControl(control:TWinControl; CONST controlType:T_controlType);
PROCEDURE unregisterFontControl(control:TWinControl);
PROCEDURE propagateFont(newFont:TFont; CONST controlType:T_controlType);
FUNCTION getFontSize(CONST c:T_controlType): longint;
PROCEDURE setFontSize (CONST c:T_controlType; CONST value: longint);

PROCEDURE performSlowUpdates(CONST isEvaluationRunning:boolean);
PROCEDURE performFastUpdates;
FUNCTION  focusedEditor:TSynEdit;
FUNCTION  typeOfFocusedControl:T_controlType;

PROCEDURE dockAllForms;
PROCEDURE closeAllForms;

PROCEDURE moveAllItems(CONST sourceMenu,destMenu:TMenuItem);

OPERATOR :=(x:byte):TFontStyles;
OPERATOR :=(x:TFontStyles):byte;

TYPE F_getFontSize= FUNCTION (CONST c:T_controlType): longint of object;
     F_setFontSize= PROCEDURE (CONST c:T_controlType; CONST value: longint) of object;
VAR getFontSize_callback:F_getFontSize=nil;
    setFontSize_callback:F_setFontSize=nil;
    htmlExporter:T_htmlExporter;
FUNCTION loadWindowPositionFromStream(VAR stream:T_bufferedInputStreamWrapper):T_windowPosition;
PROCEDURE saveWindowPositionToSream(VAR stream:T_bufferedOutputStreamWrapper; CONST pos:T_windowPosition);
FUNCTION getWindowPosition(CONST form:TForm):T_windowPosition;
PROCEDURE applyWindowPosition(CONST form:TForm; CONST pos:T_windowPosition);

IMPLEMENTATION
USES math,litVar,recyclers,basicTypes,contexts,funcs,Clipbrd,
     editorMetaBase,myStringUtil,SynHighlighterMnh,codeAssistance,fileWrappers,strutils,mnh_doc;
VAR activeForms:array of T_mnhComponentForm;
    fontControls:array[T_controlType] of array of TWinControl;
TYPE T_dockSetup=array[T_ideComponent] of T_componentParent;
CONST C_defaultDock:T_dockSetup
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
    {icVariableView}cpPageControl1,
    {icProfiling...}cpPageControl2,
                    cpPageControl2);

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
    {icVariableView}cpPageControl1,
                    cpPageControl1,
                    cpPageControl2);

PROCEDURE dockNewForm(newForm: T_mnhComponentForm);
  begin
    if mainForm<>nil then mainForm.attachNewForm(newForm)
    else begin
      newForm.DragKind:=dkDrag;
      newForm.DragMode:=dmManual;
      newForm.ShowInTaskBar:=stAlways;
      newForm.Show;
    end;
    newForm.dockChanged;
    if newForm.myComponentParent=cpNone then begin
      newForm.ShowInTaskBar:=stAlways;
      newForm.Show;
    end;
  end;

PROCEDURE closeAllForms;
  begin
    while length(activeForms)>0 do begin
      {$ifdef debugMode}
      writeln('Destroying form of type ',activeForms[0].getIdeComponentType);
      {$endif}
      activeForms[0].destroy;
    end;
  end;

PROCEDURE dockAllForms;
  VAR f:T_mnhComponentForm;
  begin
    for f in activeForms do f.defaultReattachClick(nil);
  end;

FUNCTION T_htmlExporter.textToHtml(CONST title:string; CONST content:TStrings; CONST highlighter:TSynCustomHighlighter): string;
  VAR SynExporterHTML: TSynExporterHTML;
      outputStream:TMemoryStream;
      size:longint;
      i:longint;
  begin
    if highlighter=nil then begin
      result:='<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd"><html><head><meta http-equiv="content-type" content="text/html; charset=utf-8"><title>'
              +title+
              '</title></head><body text="black" bgcolor="#EEEEEE"><pre><code><font  size=3 face="Courier New">';
      for i:=0 to content.count-1 do begin
        if i>0 then result+=C_carriageReturnChar+C_lineBreakChar;
        result+=ansiReplaceStr(ansiReplaceStr(ansiReplaceStr(content[i],'&','&amp;'),'<','&lt;'),'>','&gt;');
      end;
      result+='</font></code></pre></body></html>';
      exit(result);
    end;
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

VAR mnhOutputSyn:TMnhOutputSyn=nil;

FUNCTION ioTextToHtml(CONST line:string; CONST forceSyntaxHighlighting:boolean):string;
  VAR SynExporterHTML: TSynExporterHTML;
      outputStream:TMemoryStream;
      size:longint;
      content:TStringList;
  begin
    if mnhOutputSyn=nil then mnhOutputSyn:=TMnhOutputSyn.create(nil);

    SynExporterHTML:=TSynExporterHTML.create(nil);
    SynExporterHTML.highlighter:=mnhOutputSyn;
    SynExporterHTML.options:=[heoFragmentOnly];

    content:=TStringList.create;
    if forceSyntaxHighlighting and not(startsWith(line,#226)) then begin
      if startsWith(trimLeft(line),'out>')
      then content.add(ECHO_MARKER+' '+trimLeft(line))
      else if startsWith(trimLeft(line),'in>')
      then content.add(ECHO_MARKER+'  '+trimLeft(line))
      else content.add(ECHO_MARKER+line);
    end else content.add(line);
    SynExporterHTML.ExportAll(content);
    FreeAndNil(content);

    outputStream:=TMemoryStream.create();
    SynExporterHTML.saveToStream(outputStream);
    SynExporterHTML.free;

    size:=outputStream.size;
    outputStream.Seek(0,soFromBeginning);
    if size=0
    then result:=''
    else begin
      setLength(result,size);
      outputStream.ReadBuffer(result[1],size);
      result:=StringReplace(result,#$0D#$0A,'',[rfReplaceAll]);
      result:=StringReplace(result,'</code></pre>','</font></code></pre>',[]);
      //result:=StringReplace(result,#$0D#$0A'</font>'#$0D#$0A'</code></pre>'           ,'</font></font></code></pre>',[]);
      //result:=StringReplace(result,#$0D#$0A'</font></font>'#$0D#$0A'</code></pre>'    ,'</font></font></font></code></pre>',[]);
      //result:=StringReplace(result,#$0D#$0A'</i></font></font>'#$0D#$0A'</code></pre>','</i></font></font></font></code></pre>',[]);
      //result:=StringReplace(result,#$0D#$0A'</b></font></font>'#$0D#$0A'</code></pre>','</b></font></font></font></code></pre>',[]);
      result:=StringReplace(result,'<font  size=3 face="Courier New">','<font  size=3 face="Hack, Courier New">',[]);
    end;
    outputStream.free;
  end;

PROCEDURE T_htmlExporter.OutputSynEditCutCopy(Sender: TObject; VAR AText: string; VAR AMode: TSynSelectionMode; ALogStartPos: TPoint; VAR AnAction: TSynCopyPasteAction);
  VAR content:TStringList;
  begin
    if not(ideSettings.copyTextAsHtml) or
       (Sender.ClassName<>'TSynEdit') or
       (AnAction<>scaPlainText) or
       (TSynEdit(Sender).highlighter=nil) or
       (AMode=smColumn) or
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
                                      CONST pageControl_: TPageControl);
  begin
    dockId       :=dockId_;
    canScaleWidth:=dockId<>cpPageControl2;
    PageControl  :=pageControl_;
    if PageControl<>nil then
    PageControl.OnKeyUp:=@tabNextKeyHandling;
    if dockId=cpNone then exit;
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
  VAR anyVisiblePage:boolean=false;
      i:longint;
  begin
    if PageControl=nil then exit;
    for i:=0 to PageControl.PageCount-1 do anyVisiblePage:=anyVisiblePage or PageControl.PAGES[i].visible;
    if not(anyVisiblePage) then begin
      if canScaleWidth
      then PageControl.width :=0
      else PageControl.height:=0;
    end else begin
      if absSize=0 then absSize:=relativeSize*getFormSize div 65535;
      if absSize<0.05*getFormSize then begin
        absSize     :=round(0.2*getFormSize);
        relativeSize:=13107; //=0.2*65535
      end;
    end;
  end;

PROCEDURE T_mnhDockSiteModel.tabNextKeyHandling(Sender: TObject; VAR key: word; Shift: TShiftState);
  VAR step:longint;
      prevPageIndex,PageCount:longint;
      activePage: TTabSheet;
      ActiveControl: T_mnhComponentForm;
  begin
    if ((key=33) or (key=34)) and (ssCtrl in Shift) then begin
      prevPageIndex:=PageControl.activePageIndex;
      if prevPageIndex<0 then begin
        prevPageIndex:=0;
        PageControl.activePageIndex:=0;
      end;
      PageCount:=PageControl.PageCount;
      if key=34
      then step:=1
      else step:=PageCount-1;
      if (step<0) or (PageCount=0) then exit;

      repeat
        PageControl.activePageIndex:=(PageControl.activePageIndex+step) mod PageCount;
        activePage:=PageControl.activePage;
        if activePage=nil
        then ActiveControl:=nil
        else ActiveControl:=T_mnhComponentForm(activePage.Controls[0]);
      until (prevPageIndex=PageControl.activePageIndex) or (ActiveControl<>nil) and ActiveControl.visible;
      if mainForm<>nil then mainForm.ActiveControl:=ActiveControl.getDefaultControl;
      key:=0;
    end;
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
    if newForm.myComponentParent<>cpNone then newForm.lastDock:=newForm.myComponentParent;
    lastDockLocationFor[newForm.getIdeComponentType]:=cpNone;
    newForm.ManualDock(nil);
    newForm.BringToFront;
    newForm.myComponentParent:=cpNone;
    newForm.ShowInTaskBar:=stAlways;
    newForm.dockChanged;
    result:=true;
  end;

PROCEDURE T_mnhDockSiteModel.undockAll;
  VAR needSizeFix:boolean=false;
  begin
    while undockCurrent do needSizeFix:=true;
    if needSizeFix then fixSize;
  end;

PROCEDURE T_mnhComponentForm.getParents(OUT page: TTabSheet; OUT
  PageControl: TPageControl);
  begin
    page:=nil;
    PageControl:=nil;;
    if (parent<>nil) and (parent.ClassName='TTabSheet') then begin
      page:=TTabSheet(parent);
      if (page.parent<>nil) and (page.parent.ClassName='TPageControl')
      then PageControl:=TPageControl(page.parent);
    end;
  end;

PROCEDURE T_mnhComponentForm.tabNextKeyHandling(Sender: TObject; VAR key: word;
  Shift: TShiftState);
  begin
    if (mainForm<>nil) and (mainForm.dockSites[myComponentParent]<>nil) then mainForm.dockSites[myComponentParent]^.tabNextKeyHandling(Sender,key,Shift);
  end;

PROCEDURE T_mnhComponentForm.showComponent(CONST retainOriginalFocus: boolean);
  VAR page:TTabSheet;
      PageControl:TPageControl;
      oldActive:TWinControl;
  begin
    getParents(page,PageControl);
    if PageControl=nil then begin
      Show;
      applyWindowPosition(self,ideSettings.ideComponentSize[getIdeComponentType]);
      BringToFront;
    end else begin
      oldActive:=mainForm.ActiveControl;
      Show;
      PageControl.activePage:=page;
      if (mainForm<>nil) then try
        if (mainForm.dockSites[myComponentParent]<>nil) then mainForm.dockSites[myComponentParent]^.fixSize;
        if retainOriginalFocus and (oldActive<>nil)
        then mainForm.ActiveControl:=oldActive
        else mainForm.ActiveControl:=getDefaultControl;
      except
      end;
    end;
    dockChanged;
  end;

PROCEDURE T_mnhComponentForm.changeDock(CONST newSite: T_componentParent);
  VAR prevSite:T_componentParent;
  begin
    if myComponentParent=newSite then exit;
    prevSite:=myComponentParent;
    if newSite=cpNone
    then begin
      ManualDock(nil);
      applyWindowPosition(self,ideSettings.ideComponentSize[getIdeComponentType]);
      ShowInTaskBar:=stAlways;
    end else begin
      ManualDock(mainForm.dockSites[newSite]^.PageControl);
      lastDock:=newSite;
    end;
    myComponentParent                       :=newSite;
    lastDockLocationFor[getIdeComponentType]:=newSite;
    dockChanged;
    mainForm.dockSites[prevSite]^.fixSize;
    mainForm.dockSites[newSite ]^.fixSize;
    showComponent(false);
  end;

FUNCTION T_mnhComponentForm.isDocked:boolean;
  begin
    result:=myComponentParent<>cpNone;
  end;

PROCEDURE T_mnhComponentForm.defaultCloseClick(Sender: TObject);
  VAR page:TTabSheet;
      PageControl:TPageControl;
      CloseAction:TCloseAction=caFree;
      cp:T_componentParent;
  begin
    if not(CloseQuery) then exit;
    cp:=myComponentParent;
    getParents(page,PageControl);
    if page    <>nil then FreeAndNil(page);
    if OnClose <>nil then OnClose(Sender,CloseAction);
    if mainForm<>nil then mainForm.dockSites[cp]^.fixSize;
    if CloseAction=caFree then FreeAndNil(self);
  end;

PROCEDURE T_mnhComponentForm.defaultUndockClick(Sender: TObject); begin changeDock(cpNone);         end;
PROCEDURE T_mnhComponentForm.defaultDockSite1Click(Sender: TObject); begin changeDock(cpPageControl1); end;
PROCEDURE T_mnhComponentForm.defaultDockSite2Click(Sender: TObject); begin changeDock(cpPageControl2); end;
PROCEDURE T_mnhComponentForm.defaultDockSite3Click(Sender: TObject); begin changeDock(cpPageControl3); end;
PROCEDURE T_mnhComponentForm.defaultDockSite4Click(Sender: TObject); begin changeDock(cpPageControl4); end;
PROCEDURE T_mnhComponentForm.defaultReattachClick(Sender: TObject);
  begin
    if lastDock=cpNone
    then changeDock(C_defaultDock[getIdeComponentType])
    else changeDock(lastDock);
  end;

PROCEDURE T_mnhComponentForm.initDockMenuItems(CONST menuToInit: TMenu;
  CONST dockRoot: TMenuItem);
  VAR useRoot,item:TMenuItem;
  begin
    if mainForm=nil then exit;
    if dockRoot=nil then begin
      useRoot:=TMenuItem.create(menuToInit);
      useRoot.caption:='&Dock';
      menuToInit.items.add(useRoot);
    end else useRoot:=dockRoot;
    useRoot.Tag:=99;
    menuToInit.Images:=mainForm.dockImages;
    item:=TMenuItem.create(menuToInit); item.OnClick:=@defaultUndockClick;    item.caption:='&Undock';                     useRoot.add(item);
    item:=TMenuItem.create(menuToInit); item.OnClick:=@defaultReattachClick;  item.caption:='&Attach';                     useRoot.add(item);
    item:=TMenuItem.create(menuToInit); item.OnClick:=@defaultDockSite1Click; item.caption:='Dock &1'; item.ImageIndex:=0; useRoot.add(item);
    item:=TMenuItem.create(menuToInit); item.OnClick:=@defaultDockSite2Click; item.caption:='Dock &2'; item.ImageIndex:=1; useRoot.add(item);
    item:=TMenuItem.create(menuToInit); item.OnClick:=@defaultDockSite3Click; item.caption:='Dock &3'; item.ImageIndex:=2; useRoot.add(item);
    item:=TMenuItem.create(menuToInit); item.OnClick:=@defaultDockSite4Click; item.caption:='Dock &4'; item.ImageIndex:=3; useRoot.add(item);
    item:=TMenuItem.create(menuToInit); item.OnClick:=@defaultCloseClick;     item.caption:='&Close';                      useRoot.add(item);
  end;

PROCEDURE T_mnhComponentForm.setComponentFormVisible(CONST visible_: boolean);
  VAR page:TTabSheet;
      PageControl:TPageControl;
  begin
    if visible=visible_ then exit;
    visible:=visible_;
    getParents(page,PageControl);
    if page<>nil then begin
      page.visible:=visible_;
      page.TabVisible:=visible_;
    end;
    if (mainForm<>nil) and (mainForm.dockSites[myComponentParent]<>nil) then mainForm.dockSites[myComponentParent]^.fixSize;
  end;

FUNCTION hasAnyForm:boolean;
  begin
    result:=length(activeForms)>0;
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
      control.Font.name:=ideSettings.Font[controlType].fontName;
      control.Font.size:=ideSettings.Font[controlType].fontSize;
      control.Font.style:=ideSettings.Font[controlType].style;
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
    ideSettings.Font[controlType].fontName:=newFont.name;
    ideSettings.Font[controlType].fontSize:=newFont.size;
    ideSettings.Font[controlType].style:=newFont.style;
  end;

FUNCTION getFontSize(CONST c: T_controlType): longint;
  begin
    if getFontSize_callback<>nil
    then exit(getFontSize_callback(c))
    else result:=ideSettings.Font[c].fontSize;
  end;

PROCEDURE setFontSize(CONST c: T_controlType; CONST value: longint);
  VAR e:TControl;
  begin
    if setFontSize_callback<>nil
    then begin setFontSize_callback(c,value); exit; end
    else begin
      for e in fontControls[c] do e.Font.size:=value;
      ideSettings.Font[c].fontSize:=value;
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
    lastDock:=lastDockLocationFor[getIdeComponentType];
    OnKeyUp:=@tabNextKeyHandling;
  end;

DESTRUCTOR T_mnhComponentForm.destroy;
  VAR k:longint=0;
      cp:T_componentParent;
  begin
    cp:=myComponentParent;
    while (k<length(activeForms)) and (activeForms[k]<>self) do inc(k);
    if k<length(activeForms) then begin
      activeForms[k]:=activeForms[length(activeForms)-1];
      setLength(activeForms,length(activeForms)-1);
    end;
    inherited destroy;
    if mainForm<>nil then mainForm.dockSites[cp]^.fixSize;
  end;

PROCEDURE T_mnhComponentForm.defaultEndDock(Sender, target: TObject; X,Y: integer);
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
    if myComponentParent<>cpNone then lastDock:=myComponentParent;

    if myComponentParent=cpNone then begin
      if width <200 then width :=200;
      if height<200 then height:=200;
    end;
    dockChanged;
  end;

FUNCTION T_mnhComponentForm.getCaption: string;
  VAR i:T_ideComponent;
  begin
    i:=getIdeComponentType;
    result:=COMPONENT_CAPTION[i];
    if COMPONENT_SHORTCUT[i]<>'' then result+=' ('+COMPONENT_SHORTCUT[i]+')';
  end;

FUNCTION T_mnhComponentForm.getDefaultControl: TWinControl;
  begin
    result:=self;
  end;

PROCEDURE performSlowUpdates(CONST isEvaluationRunning:boolean);
  VAR f:T_mnhComponentForm;
  begin
    for f in activeForms do begin
      f.performSlowUpdate(isEvaluationRunning);
      if f.myComponentParent=cpNone then
      ideSettings.ideComponentSize[f.getIdeComponentType]:=getWindowPosition(f);
    end;
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

    if active.ClassName='TSynEdit'    then exit(ctEditor );
    if active.ClassName='TTreeView'   then exit(ctGeneral);
    if active.ClassName='TListBox'    then exit(ctGeneral);
    if active.ClassName='TplotForm'   then exit(ctPlot   );
    if active.ClassName='TStringGrid' then exit(ctTable  );

    {$ifdef debugMode}
    writeln('Unknown control class ',active.ClassName);
    {$endif}

    for c in T_controlType do
    for e in fontControls[c] do if e=mainForm.ActiveControl then exit(c);
  end;

PROCEDURE moveAllItems(CONST sourceMenu, destMenu: TMenuItem);
  VAR mi:TMenuItem;
      i:longint=0;
  begin
    while sourceMenu.count>i do begin
      mi:=sourceMenu[i];
      if mi.Tag=99 then inc(i)
      else begin
        sourceMenu.remove(mi);
        destMenu.add(mi);
      end;
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

FUNCTION loadWindowPositionFromStream(VAR stream: T_bufferedInputStreamWrapper): T_windowPosition;
  begin
    result.top   :=min(max(stream.readLongint,0  ),screen.height-100);
    result.Left  :=min(max(stream.readLongint,0  ),screen.width-100);
    result.height:=min(max(stream.readLongint,100),screen.height);
    result.width :=min(max(stream.readLongint,100),screen.width);
  end;

PROCEDURE saveWindowPositionToSream(VAR stream: T_bufferedOutputStreamWrapper; CONST pos: T_windowPosition);
  begin
    stream.writeLongint(pos.top);
    stream.writeLongint(pos.Left);
    stream.writeLongint(pos.height);
    stream.writeLongint(pos.width);
  end;

FUNCTION getWindowPosition(CONST form: TForm): T_windowPosition;
  begin
    result.top   :=form.top   ;
    result.Left  :=form.Left  ;
    result.height:=form.height;
    result.width :=form.width ;
  end;

PROCEDURE applyWindowPosition(CONST form: TForm; CONST pos: T_windowPosition);
  begin
    form.top   :=pos.top   ;
    form.Left  :=pos.Left  ;
    form.height:=pos.height;
    form.width :=pos.width ;
  end;

{$i func_defines.inc}
{$define FUNC_ID:='anyFormShowing'}
{$define SIDE_EFFECTS:=[se_readGuiState]}
FUNCTION anyFormShowing_imp intFuncSignature;
  begin
    CHECK_SIDE;
    result:=nil;
    if (params=nil) or (params^.size=0) then result:=newBoolLiteral(hasAnyForm);
  end;

FUNCTION formatHtmlPage_imp intFuncSignature;
  VAR name:string;
      language:T_language;
      highlighter:TSynCustomHighlighter;
      content:TStrings;
      i:longint;
  PROCEDURE initMnhHighlighting;
    VAR lineData:T_arrayOfString=();
        k:longint;
        provider:P_virtualFileCodeProvider;
        codeAssistanceData:P_codeAssistanceResponse;

    begin
      setLength(lineData,list0^.size);
      for k:=0 to length(lineData)-1 do lineData[k]:=P_stringLiteral(list0^.value[k])^.value;

      provider:=newVirtualFileCodeProvider(name,lineData);
      codeAssistanceData:=getAssistanceResponseSync(provider);
      highlighter:=TMnhInputSyn.create(nil);
      codeAssistanceData^.updateHighlightingData(TMnhInputSyn(highlighter).highlightingData);
      disposeMessage(codeAssistanceData);
    end;

  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and (arg0^.literalType in [lt_emptyList,lt_stringList]) and (arg1^.literalType=lt_string) then begin
      name:=str1^.value;
      language:=languageFromExtension(extractFileExt(name));
      if language=LANG_MNH then initMnhHighlighting
                           else highlighter:=fileTypeMeta[language].highlighter;
      content:=TStringList.create;
      for i:=0 to list0^.size-1 do content.append(P_stringLiteral(list0^.value[i])^.value);
      result:=recycler^.newStringLiteral(htmlExporter.textToHtml(name,content,highlighter));
      FreeAndNil(content);
      if language=LANG_MNH then FreeAndNil(highlighter);
    end;
  end;

CONSTRUCTOR T_outlineSettings.create;
  begin
    showPrivate:=true;
    showImported:=false;
    ruleSorting:=rs_byLocation;
  end;

DESTRUCTOR T_outlineSettings.destroy;
  begin
  end;

FUNCTION T_outlineSettings.getSerialVersion: dword;
  begin
    result:=1;
  end;

FUNCTION T_outlineSettings.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  begin
    try
      showPrivate:=stream.readBoolean;
      showImported:=stream.readBoolean;
      ruleSorting:=T_ruleSorting(stream.readByte([byte(low(T_ruleSorting))..byte(high(T_ruleSorting))]));
      result:=stream.allOkay;
    except
      result:=false;
    end;
    if not(result) then begin
      showPrivate:=true;
      showImported:=false;
      ruleSorting:=rs_byLocation;
    end;
  end;

PROCEDURE T_outlineSettings.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    stream.writeBoolean(showPrivate);
    stream.writeBoolean(showImported);
    stream.writeByte(byte(ruleSorting));
  end;

CONSTRUCTOR T_ideSettings.create;
  begin
    initDefaults;
    outputBehavior.create;
    quickOutputBehavior.create;
    outlineSettings.create;
    initialize(workspaceHistory);
  end;

FUNCTION T_ideSettings.getSerialVersion: dword;
  begin
    result:=24823585;
  end;

FUNCTION T_ideSettings.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  {$MACRO ON}
  {$define cleanExit:=begin initDefaults; exit(false) end}
  VAR c:T_controlType;
      cp:T_componentParent;
      ic:T_ideComponent;
      i:longint;
      serial: dword;

  begin
    serial:=stream.readDWord;
    //TODO: Remove code for backwards compatibility after Version 3.2.0
    if (serial<>24823584) and (serial<>getSerialVersion) then begin
      stream.logWrongTypeError;
      cleanExit;
    end;
    result:=true;
    for c:=low(Font) to high(Font) do with Font[c] do begin
      fontSize:=stream.readLongint;
      style   :=stream.readByte([0..3]);
      fontName:=stream.readAnsiString;
    end;
    doResetPlotOnEvaluation := stream.readBoolean;
    cacheAnimationFrames    := stream.readBoolean;

    outputBehavior     .loadFromStream(stream);
    quickOutputBehavior.loadFromStream(stream);
    outputLinesLimit:=stream.readLongint;
    if outputLinesLimit<0 then stream.logWrongTypeError;
    if (serial<>24823584) then begin
      outputLinesLimitPerLiteral:=stream.readLongint;
      if outputLinesLimitPerLiteral<0 then stream.logWrongTypeError;
      forceFullLiterals:=stream.readBoolean;
    end;
    outlineSettings.loadFromStream(stream);
    currentWorkspace:=stream.readAnsiString;
    i:=stream.readNaturalNumber;
    if (i>MAX_WORKSPACE_HISTORY_SIZE) then stream.logWrongTypeError else begin
      setLength(workspaceHistory,i);
      for i:=0 to length(workspaceHistory)-1 do workspaceHistory[i]:=stream.readAnsiString;
    end;

    if mainForm<>nil then begin
      applyWindowPosition(mainForm,loadWindowPositionFromStream(stream));
      windowStateForUpdate:=T_windowStateForUpdate(stream.readByte([byte(wsfuFullscreen),byte(wsfuMaximized),byte(wsfuNormal)]));
      for cp in PAGES do mainForm.dockSites[cp]^.relativeSize:=stream.readWord;
    end else begin
      loadWindowPositionFromStream(stream);
      windowStateForUpdate:=T_windowStateForUpdate(stream.readByte([byte(wsfuFullscreen),byte(wsfuMaximized),byte(wsfuNormal)]));
      for cp in PAGES do stream.readWord;
    end;
    activeComponents:=[];
    for ic in T_ideComponent do begin
      lastDockLocationFor[ic]:=T_componentParent(stream.readByte);
      ideComponentSize   [ic]:=loadWindowPositionFromStream(stream);
      if stream.readBoolean then include(activeComponents,ic);
    end;

    doShowSplashScreen:=stream.readBoolean;
    htmlDocGeneratedForCodeHash:=stream.readAnsiString;
    copyTextAsHtml:=stream.readBoolean;
    registeredAssociation:=T_registeredAssociation(stream.readByte([byte(raNone),byte(raFullVersion)]));
    result:=result and stream.allOkay;
    if not(result) then begin
      if mainForm<>nil then begin
        windowStateForUpdate:=wsfuNone;
        for cp in PAGES do mainForm.dockSites[cp]^.relativeSize:=0;
      end;
      doShowSplashScreen:=true;
      htmlDocGeneratedForCodeHash:='';
      cleanExit;
    end;
    if not(stream.allOkay) then cleanExit else result:=true;
  end;

PROCEDURE T_ideSettings.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR c:T_controlType;
      cp:T_componentParent;
      ic:T_ideComponent;
      i:longint;
  begin
    inherited saveToStream(stream);
    for c:=low(Font) to high(Font) do with Font[c] do begin
      stream.writeLongint(fontSize);
      stream.writeByte(style);
      stream.writeAnsiString(fontName);
    end;
    stream.writeBoolean(doResetPlotOnEvaluation);
    stream.writeBoolean(cacheAnimationFrames);
    outputBehavior.saveToStream(stream);
    quickOutputBehavior.saveToStream(stream);
    stream.writeLongint(outputLinesLimit);
    stream.writeLongint(outputLinesLimitPerLiteral);
    stream.writeBoolean(forceFullLiterals);
    outlineSettings.saveToStream(stream);
    stream.writeAnsiString(currentWorkspace);
    if length(workspaceHistory)>MAX_WORKSPACE_HISTORY_SIZE then setLength(workspaceHistory,MAX_WORKSPACE_HISTORY_SIZE);
    stream.writeNaturalNumber(length(workspaceHistory));

    saveWindowPositionToSream(stream,getWindowPosition(mainForm));
    case mainForm.WindowState of
      wsMaximized : stream.writeByte(byte(wsfuMaximized));
      wsFullScreen: stream.writeByte(byte(wsfuFullscreen));
      else          stream.writeByte(byte(wsfuNormal));
    end;

    for cp in PAGES do stream.writeWord(mainForm.dockSites[cp]^.relativeSize);

    for ic in T_ideComponent do begin
      stream.writeByte(byte(lastDockLocationFor[ic]));
      saveWindowPositionToSream(stream,ideComponentSize[ic]);
      stream.writeBoolean(hasFormOfType(ic));
    end;

    stream.writeBoolean(doShowSplashScreen);
    stream.writeAnsiString(htmlDocGeneratedForCodeHash);
    stream.writeBoolean(copyTextAsHtml);
    stream.writeByte(byte(registeredAssociation));
    for i:=0 to length(workspaceHistory)-1 do stream.writeAnsiString(workspaceHistory[i]);
  end;

PROCEDURE T_ideSettings.initDefaults;
  VAR c:T_controlType;
  begin
    registeredAssociation:=raNone;
    for c:=low(Font) to high(Font) do with Font[c] do begin
      fontName:='Courier New';
      style   :=0;
      fontSize:=10;
    end;
    doResetPlotOnEvaluation:=true;
    cacheAnimationFrames:=true;
    outputBehavior.reset;
    quickOutputBehavior.reset;
    outputLinesLimit:=maxLongint;
    currentWorkspace:='';
    forceFullLiterals:=false;
    outputLinesLimitPerLiteral:=maxLongint;
    setLength(workspaceHistory,0);
  end;

FUNCTION T_ideSettings.workspaceFilename: string;
  begin
    if currentWorkspace=''
    then result:=defaultWorkspaceFilename
    else result:=currentWorkspace;
  end;

PROCEDURE clearComponentShortcuts;
  VAR i:T_ideComponent;
  begin
    for i in T_ideComponent do COMPONENT_SHORTCUT[i]:='';
  end;

INITIALIZATION
  mnh_doc.lineToHtml:=@ioTextToHtml;
  clearComponentShortcuts;
  ideSettings.create;
  initialize(lastDockLocationFor);
  setLength(activeForms,0);
  setLength(fontControls[ctEditor ],0);
  setLength(fontControls[ctTable  ],0);
  setLength(fontControls[ctGeneral],0);
  builtinFunctionMap.registerRule(GUI_NAMESPACE,'anyFormShowing',@anyFormShowing_imp,ak_nullary,[se_readGuiState]);
  builtinFunctionMap.registerRule(HTTP_NAMESPACE,'formatHtmlPage',@formatHtmlPage_imp,ak_binary);
FINALIZATION
  if mnhOutputSyn<>nil then FreeAndNil(mnhOutputSyn);
end.

