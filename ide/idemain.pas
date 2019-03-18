UNIT ideMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Dialogs, Menus, ExtCtrls,
  ComCtrls, StdCtrls, ideLayoutUtil, mnh_gui_settings,
  editorMeta,editorMetaBase,evalThread,guiOutAdapters,codeAssistance,
  outputFormUnit,debugging,assistanceFormUnit,debuggerForms,breakpointsForms,searchModel,outlineFormUnit;

TYPE

  { TIdeMainForm }

  TIdeMainForm = class(T_mnhIdeForm)
    bookmarkImages: TImageList;
    breakpointImages: TImageList;
    MainMenu: TMainMenu;
    miAbout: TMenuItem;
    miHelp: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    miKeepStackTrace: TMenuItem;
    miDebug: TMenuItem;
    miProfile: TMenuItem;
    miRunScriptExternally: TMenuItem;
    miRunScript: TMenuItem;
    miRunDirect: TMenuItem;
    miHaltEvaluation: TMenuItem;
    miEditScriptFile: TMenuItem;
    miToggleFullscreen: TMenuItem;
    miDecFontSize: TMenuItem;
    miIncFontSize: TMenuItem;
    miRename: TMenuItem;
    miGotoLine: TMenuItem;
    miFindPrevious: TMenuItem;
    miFindNext: TMenuItem;
    miFind: TMenuItem;
    miReplace: TMenuItem;
    smEdit: TMenuItem;
    smRecent: TMenuItem;
    smHistory: TMenuItem;
    miRestore: TMenuItem;
    miExportToHtml: TMenuItem;
    miSaveAs: TMenuItem;
    miSave: TMenuItem;
    miClose: TMenuItem;
    miOpenClassical: TMenuItem;
    miOpen: TMenuItem;
    miBreakpoints: TMenuItem;
    miDebugger: TMenuItem;
    miQuickEval: TMenuItem;
    miOutline: TMenuItem;
    miAssistant: TMenuItem;
    smAppearance: TMenuItem;
    miLanguage: TMenuItem;
    miNew: TMenuItem;
    miSettings: TMenuItem;
    smFile: TMenuItem;
    smShow: TMenuItem;

    PageControl1: TPageControl;
    PageControl2: TPageControl;
    PageControl3: TPageControl;
    EditorsPageControl: TPageControl;
    PageControl4: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    StatusBar1: TStatusBar;
    timer: TTimer;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDropFiles(Sender: TObject; CONST FileNames: array of string);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE miAboutClick(Sender: TObject);
    PROCEDURE miAssistantClick(Sender: TObject);
    PROCEDURE miBreakpointsClick(Sender: TObject);
    PROCEDURE miCloseClick(Sender: TObject);
    PROCEDURE miDebugClick(Sender: TObject);
    PROCEDURE miDebuggerClick(Sender: TObject);
    PROCEDURE miDecFontSizeClick(Sender: TObject);
    PROCEDURE miEditScriptFileClick(Sender: TObject);
    PROCEDURE miExportToHtmlClick(Sender: TObject);
    PROCEDURE miFindClick(Sender: TObject);
    PROCEDURE miFindNextClick(Sender: TObject);
    PROCEDURE miFindPreviousClick(Sender: TObject);
    PROCEDURE miGotoLineClick(Sender: TObject);
    PROCEDURE miHelpClick(Sender: TObject);
    PROCEDURE miIncFontSizeClick(Sender: TObject);
    PROCEDURE miKeepStackTraceClick(Sender: TObject);
    PROCEDURE miNewClick(Sender: TObject);
    PROCEDURE miOpenClassicalClick(Sender: TObject);
    PROCEDURE miOpenClick(Sender: TObject);
    PROCEDURE miOutlineClick(Sender: TObject);
    PROCEDURE miProfileClick(Sender: TObject);
    PROCEDURE miQuickEvalClick(Sender: TObject);
    PROCEDURE miRenameClick(Sender: TObject);
    PROCEDURE miReplaceClick(Sender: TObject);
    PROCEDURE miRestoreClick(Sender: TObject);
    PROCEDURE miRunDirectClick(Sender: TObject);
    PROCEDURE miRunScriptClick(Sender: TObject);
    PROCEDURE miRunScriptExternallyClick(Sender: TObject);
    PROCEDURE miSaveAsClick(Sender: TObject);
    PROCEDURE miSaveClick(Sender: TObject);
    PROCEDURE miSettingsClick(Sender: TObject);
    PROCEDURE miToggleFullscreenClick(Sender: TObject);
    PROCEDURE PageControl1StartDock(Sender: TObject; VAR DragObject: TDragDockObject);
    PROCEDURE PageControl2StartDock(Sender: TObject; VAR DragObject: TDragDockObject);
    PROCEDURE PageControl3StartDock(Sender: TObject; VAR DragObject: TDragDockObject);
    PROCEDURE PageControl4StartDock(Sender: TObject; VAR DragObject: TDragDockObject);
    PROCEDURE Splitter1Moved(Sender: TObject);
    PROCEDURE attachNewForm(CONST form:T_mnhComponentForm); override;

    PROCEDURE onEditFinished(CONST data:P_editScriptTask   ); override;
    PROCEDURE onBreakpoint  (CONST data:P_debuggingSnapshot); override;
    PROCEDURE onDebuggerEvent;                                override;
    PROCEDURE onEndOfEvaluation;                              override;
    PROCEDURE TimerTimer(Sender: TObject);
  private
    subTimerCounter:longint;
    splitterPositions:T_splitterPositions;
    FUNCTION startDock(CONST PageControl:TPageControl):TDragDockObject;
  public
    { public declarations }
  end;

VAR
  IdeMainForm: TIdeMainForm;

IMPLEMENTATION
USES mnh_splash;
{$R ideMain.lfm}

PROCEDURE TIdeMainForm.FormDropFiles(Sender: TObject; CONST FileNames: array of string);
  begin
    editorMeta.workspace.addOrGetEditorMetaForFiles(FileNames,true);
  end;

PROCEDURE TIdeMainForm.FormCreate(Sender: TObject);
  VAR outputForm:TOutputForm;
  begin
    subTimerCounter:=0;
    splitterPositions[1]:=16384;
    splitterPositions[2]:=10000;
    splitterPositions[3]:=16384;
    splitterPositions[4]:=16384;
    ideLayoutUtil.mainForm:=self;

    outputForm:=TOutputForm.create(self);
    attachNewForm(outputForm);
    attachNewForm(TAssistanceForm.create(self));

    setupEditorMetaBase(miLanguage);
    runnerModel.create;
    initGuiOutAdapters(self,outputForm.OutputSynEdit);

    initUnit(@guiAdapters);

    workspace.create(self,
                     EditorsPageControl,
                     breakpointImages,
                     bookmarkImages);

    splashOnStartup;
  end;

PROCEDURE TIdeMainForm.FormResize(Sender: TObject);
  begin
    PageControl1.width := width*splitterPositions[1] div 65535;
    PageControl2.height:=height*splitterPositions[2] div 65535;
    PageControl3.width := width*splitterPositions[3] div 65535;
    PageControl4.width := width*splitterPositions[4] div 65535;
  end;

PROCEDURE TIdeMainForm.miAboutClick(Sender: TObject);
  begin
    splashForAbout;
  end;

PROCEDURE TIdeMainForm.miAssistantClick(Sender: TObject);
  begin
    ensureAssistanceForm;
  end;

PROCEDURE TIdeMainForm.miBreakpointsClick(Sender: TObject);
  begin
    ensureBreakpointsForm;
  end;

PROCEDURE TIdeMainForm.miCloseClick(Sender: TObject);
  begin
    workspace.closeCurrentFile;
  end;

PROCEDURE TIdeMainForm.miDebugClick(Sender: TObject);
begin
  runnerModel.debugMode:=miDebug.checked;
end;

PROCEDURE TIdeMainForm.miDebuggerClick(Sender: TObject);
  begin
    ensureDebuggerForm;
  end;

PROCEDURE TIdeMainForm.miDecFontSizeClick(Sender: TObject);
  begin
    //TODO: Implement me
  end;

PROCEDURE TIdeMainForm.miEditScriptFileClick(Sender: TObject);
  begin
    workspace.addOrGetEditorMetaForFiles(utilityScriptFileName,true);
  end;

PROCEDURE TIdeMainForm.miExportToHtmlClick(Sender: TObject);
  begin
    workspace.exportCurrentFileToHtml;
  end;

PROCEDURE TIdeMainForm.miFindClick(Sender: TObject);
  begin
    searchReplaceModel.beginFindOrReplace(focusedEditor,true);
  end;

PROCEDURE TIdeMainForm.miFindNextClick(Sender: TObject);
  begin
    searchReplaceModel.doFindNext(focusedEditor);
  end;

PROCEDURE TIdeMainForm.miFindPreviousClick(Sender: TObject);
  begin
    searchReplaceModel.doFindPrevious(focusedEditor);
  end;

PROCEDURE TIdeMainForm.miGotoLineClick(Sender: TObject);
  begin
    //TODO: Implement me
  end;

PROCEDURE TIdeMainForm.miHelpClick(Sender: TObject);
  begin
    //TODO: Implement me
  end;

PROCEDURE TIdeMainForm.miIncFontSizeClick(Sender: TObject);
  begin
    //TODO: Implement me
  end;

PROCEDURE TIdeMainForm.miKeepStackTraceClick(Sender: TObject);
  begin
    runnerModel.stackTracing:=miKeepStackTrace.checked;
  end;

PROCEDURE TIdeMainForm.miNewClick(Sender: TObject);
  begin
    workspace.createNewFile;
  end;

PROCEDURE TIdeMainForm.miOpenClassicalClick(Sender: TObject);
  begin
    //TODO: Implement me
  end;

PROCEDURE TIdeMainForm.miOpenClick(Sender: TObject);
  begin
    //TODO: Implement me
  end;

PROCEDURE TIdeMainForm.miOutlineClick(Sender: TObject);
  begin
    ensureOutlineForm;
  end;

PROCEDURE TIdeMainForm.miProfileClick(Sender: TObject);
begin
  runnerModel.profiling:=miProfile.checked;
end;

PROCEDURE TIdeMainForm.miQuickEvalClick(Sender: TObject);
  begin
    //TODO: Implement me
  end;

PROCEDURE TIdeMainForm.miRenameClick(Sender: TObject);
  begin
    //TODO: Implement me like:
      //VAR meta:P_editorMeta;
      //    id:string;
      //    idType:T_tokenType;
      //    renameLocation:T_searchTokenLocation;
      //    scanOther:boolean;
      //begin
      //  meta:=getEditor;
      //  if (meta<>nil) and
      //     meta^.canRenameUnderCursor(id,idType,renameLocation,scanOther) and
      //     (renameForm.showModalFor(id,idType,scanOther)=mrOk) then begin
      //    meta^.doRename(renameLocation,id,renameForm.newId,renameForm.checkAllEditorsCheckBox.checked and scanOther);
      //  end;
      //end;
  end;

PROCEDURE TIdeMainForm.miReplaceClick(Sender: TObject);
  begin
    searchReplaceModel.beginFindOrReplace(focusedEditor,false);
  end;

PROCEDURE TIdeMainForm.miRestoreClick(Sender: TObject);
  begin
    //TODO: Implement me
  end;

PROCEDURE TIdeMainForm.miRunDirectClick(Sender: TObject);
  begin
    runnerModel.customRun(false);
  end;

PROCEDURE TIdeMainForm.miRunScriptClick(Sender: TObject);
  begin
    runnerModel.customRun(false);
  end;

PROCEDURE TIdeMainForm.miRunScriptExternallyClick(Sender: TObject);
  begin
    //TODO: Implement me
  end;

PROCEDURE TIdeMainForm.miSaveAsClick(Sender: TObject);
  begin
    workspace.saveCurrentFile(true);
  end;

PROCEDURE TIdeMainForm.miSaveClick(Sender: TObject);
  begin
    workspace.saveCurrentFile();
  end;

PROCEDURE TIdeMainForm.miSettingsClick(Sender: TObject);
  begin
    SettingsForm.ShowModal;
  end;

PROCEDURE TIdeMainForm.miToggleFullscreenClick(Sender: TObject);
  begin
    if WindowState=wsFullScreen
    then WindowState:=wsMaximized
    else WindowState:=wsFullScreen;
  end;

PROCEDURE TIdeMainForm.PageControl1StartDock(Sender: TObject;
  VAR DragObject: TDragDockObject); begin DragObject:=startDock(PageControl1); end;
PROCEDURE TIdeMainForm.PageControl2StartDock(Sender: TObject;
  VAR DragObject: TDragDockObject); begin DragObject:=startDock(PageControl2); end;
PROCEDURE TIdeMainForm.PageControl3StartDock(Sender: TObject;
  VAR DragObject: TDragDockObject); begin DragObject:=startDock(PageControl3); end;
PROCEDURE TIdeMainForm.PageControl4StartDock(Sender: TObject;
  VAR DragObject: TDragDockObject); begin DragObject:=startDock(PageControl4); end;

PROCEDURE TIdeMainForm.Splitter1Moved(Sender: TObject);
  begin
    splitterPositions[1]:=PageControl1.width *65535 div  width;
    splitterPositions[2]:=PageControl2.height*65535 div height;
    splitterPositions[3]:=PageControl3.width *65535 div  width;
    splitterPositions[4]:=PageControl4.width *65535 div  width;
  end;

FUNCTION TIdeMainForm.startDock(CONST PageControl: TPageControl): TDragDockObject;
  VAR control:TControl;
      newForm:T_mnhComponentForm;
  begin
    //Only handle pages with one control
    if PageControl.activePage.ControlCount<>1 then exit(nil);
    control:=PageControl.activePage.Controls[0];
    //If the sheet is a TForm return it directly
    if control.ClassType.InheritsFrom(T_mnhComponentForm.ClassType) then newForm:=T_mnhComponentForm(control)
    else begin
      raise Exception.create('Not an mnhComponent form!');
    end;
    writeln('StartDock');
    result:=TDragDockObject.AutoCreate(newForm);
  end;

PROCEDURE TIdeMainForm.attachNewForm(CONST form: T_mnhComponentForm);
  VAR dockMeta:TDragDockObject=nil;
      componentParent:T_componentParent;
  begin
    componentParent:=lastDockLocationFor[form.getIdeComponentType];
    if componentParent in [cpPageControl1..cpPageControl4] then dockMeta:=TDragDockObject.create(form);
    case componentParent of
      cpPageControl1: PageControl1.DockDrop(dockMeta,0,0);
      cpPageControl2: PageControl2.DockDrop(dockMeta,0,0);
      cpPageControl3: PageControl3.DockDrop(dockMeta,0,0);
      cpPageControl4: PageControl4.DockDrop(dockMeta,0,0);
      else begin
        form.top :=top +(height-form.height) div 2;
        form.Left:=Left+(width -form.width ) div 2;
      end;
    end;
    form.Show;
    if dockMeta<>nil then FreeAndNil(dockMeta);
  end;

PROCEDURE TIdeMainForm.onEditFinished(CONST data: P_editScriptTask);
  begin
    //TODO: Implement me
  end;

PROCEDURE TIdeMainForm.onBreakpoint(CONST data: P_debuggingSnapshot);
  begin
    currentSnapshot:=data;
    ensureDebuggerForm;
  end;

PROCEDURE TIdeMainForm.onDebuggerEvent;
  begin
    //TODO: Implement me
  end;

PROCEDURE TIdeMainForm.onEndOfEvaluation;
  begin
    //TODO: Implement me
  end;

PROCEDURE TIdeMainForm.TimerTimer(Sender: TObject);
  PROCEDURE slowUpdates; inline;
    VAR edit:P_editorMeta;
    begin
      edit:=workspace.currentEditor;
      if (edit<>nil) then begin
        edit^.pollAssistanceResult;
      end;
      performSlowUpdates;
    end;

  PROCEDURE fastUpdates; inline;
    begin
      performFastUpdates;
    end;

  begin
    fastUpdates;
    inc(subTimerCounter);
    if subTimerCounter>50 then begin
      slowUpdates;
      subTimerCounter:=0;
    end;
  end;

end.

