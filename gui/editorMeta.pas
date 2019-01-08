UNIT editorMeta;
INTERFACE
USES  //basic classes
  Classes, sysutils, LazUTF8, LCLType, types, LazFileUtils,
  //my utilities:
  serializationUtil,
  myGenerics,
  myStringUtil,
  //GUI: LCL components
  Controls, Graphics, Dialogs, Menus, ComCtrls, StdCtrls,
  //GUI: SynEdit
  SynEdit, SynEditMiscClasses, SynEditMarks,
  //GUI: highlighters
  SynHighlighterMnh,
  SynExportHTML,
  closeDialog,
  mnh_tables,
  variableTreeViews,
  mnh_plotForm,
  //MNH:
  mnh_doc,
  outlines,
  mnh_constants, basicTypes, fileWrappers,mnh_settings,
  tokenArray,
  contexts,
  litVar,
  funcs,
  debugging,
  cmdLineInterpretation,
  evalThread,
  packages,
  guiOutAdapters,
  datastores,
  editorMetaBase,
  recyclers,
  codeAssistance;

TYPE
P_editorMeta=^T_editorMeta;
T_editorMeta=object(T_basicEditorMeta)
  private
    index:longint;
    paintedWithStateHash:T_hashInt;
    fileInfo:record
      filePath:ansistring;
      fileAccessAge:double;
      isChanged:boolean;
      ignoreDeleted:boolean;
    end;
    strictlyReadOnly:boolean;

    latestAssistanceReponse:P_codeAssistanceResponse;
    tabsheet    : TTabSheet;
    PROCEDURE guessLanguage(CONST fallback:T_language);
    CONSTRUCTOR create(CONST idx:longint);
    CONSTRUCTOR create(CONST idx:longint; VAR stream:T_bufferedInputStreamWrapper);

    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper);
  public
    PROPERTY getCodeAssistanceData:P_codeAssistanceResponse read latestAssistanceReponse;
    FUNCTION enabled:boolean;
    DESTRUCTOR destroy; virtual;
    FUNCTION getPath:ansistring; virtual;
    FUNCTION isPseudoFile: boolean; virtual;

    PROCEDURE activate; virtual;
    FUNCTION caretInMainFormCoordinates:TPoint;

    PROCEDURE setUnderCursor(CONST updateMarker,forHelpOrJump: boolean; CONST caret:TPoint);
    PROCEDURE setUnderCursor(CONST updateMarker,forHelpOrJump: boolean);
    FUNCTION canRenameUnderCursor(OUT orignalId:string; OUT tokTyp:T_tokenType; OUT ref:T_searchTokenLocation; OUT mightBeUsedElsewhere:boolean):boolean;
    PROCEDURE doRename(CONST ref:T_searchTokenLocation; CONST oldId,newId:string; CONST renameInOtherEditors:boolean=false);

    PROCEDURE onClearBookmark(Sender: TObject; VAR mark: TSynEditMark);
    PROCEDURE onPlaceBookmark(Sender: TObject; VAR mark: TSynEditMark);
    PROCEDURE clearBookmark(markIndex:longint);
    PROCEDURE toggleBreakpoint;
    PROCEDURE setWorkingDir;
    PROCEDURE closeEditorWithDialogs;

    FUNCTION saveAsWithDialog:boolean;
    FUNCTION saveWithDialog:boolean;
    PROCEDURE reloadFile(CONST fileName:string);
    PROCEDURE exportToHtml;
    FUNCTION pseudoName(CONST short:boolean=false):ansistring;
    FUNCTION defaultExtensionByLanguage:ansistring;
    PROCEDURE updateContentAfterEditScript(CONST stringListLiteral:P_listLiteral);
    FUNCTION resolveImport(CONST text:string):string;
    PROCEDURE updateAssistanceResponse(CONST response:P_codeAssistanceResponse);
    PROCEDURE pollAssistanceResult;
  private
    PROCEDURE triggerCheck;

    PROCEDURE closeEditorQuietly;
    PROCEDURE InputEditChange(Sender: TObject);
    PROCEDURE languageMenuItemClick(Sender: TObject);
    FUNCTION isFile:boolean;
    PROCEDURE setFile(CONST fileName:string);
    PROCEDURE initForNewFile;
    PROCEDURE setStepperBreakpoints;
    PROCEDURE _add_breakpoint_(CONST lineIndex:longint);
    FUNCTION updateSheetCaption:ansistring;
    FUNCTION changed:boolean;
    FUNCTION saveFile(CONST fileName:string=''):string;
    FUNCTION fileIsDeleted:boolean;
    FUNCTION fileIsModifiedOnFileSystem:boolean;
end;

T_runnerModel=object
  private
    lastStart:record
      mainCall:boolean;
      parameters:string;
    end;
    debugLine:record
      editor:TSynEdit;
      line:longint;
    end;
    debugMode_:boolean;
    PROCEDURE setDebugMode(CONST value:boolean);
  public
    stackTracing:boolean;
    firstCallAfterActivation:boolean;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION areEditorsLocked:boolean;
    PROPERTY debugMode:boolean read debugMode_ write setDebugMode;
    FUNCTION canRun(CONST quickMode:boolean=false):boolean;
    PROCEDURE customRun(CONST mainCall,profiling:boolean; CONST mainParameters:string='');
    PROCEDURE runExternally(CONST mainParameters:string='');
    PROCEDURE rerun(CONST profiling:boolean);
    PROCEDURE InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);
    PROCEDURE doDebuggerAction(CONST newState:T_debuggerState);
    PROCEDURE markDebugLine(CONST editor:TSynEdit; CONST line:longint);
    PROCEDURE haltEvaluation;
  end;

PROCEDURE setupUnit(CONST p_mainForm              :T_abstractMnhForm;
                    CONST p_inputPageControl      :TPageControl;
                    CONST p_breakpointsImagesList :TImageList;
                    CONST p_bookmarkImagesList    :TImageList;
                    CONST p_assistanceSynEdit     :TSynEdit;
                    CONST p_assistanceTabSheet    :TTabSheet;
                    CONST outputHighlighter       :TSynMnhSyn;
                    CONST languageMenuRoot        :TMenuItem;
                    CONST p_restoreMenuItem       :TMenuItem;
                    CONST p_EditKeyUp             :TKeyEvent;
                    CONST p_EditMouseDown         :TMouseEvent;
                    CONST p_EditProcessUserCommand:TProcessCommandEvent;
                    CONST p_outlineGroupBox       :TGroupBox;
                    CONST p_outlineModel          :P_outlineTreeModel);
FUNCTION hasEditor:boolean;
FUNCTION getEditor:P_editorMeta;
FUNCTION addEditorMetaForNewFile:longint;
FUNCTION addOrGetEditorMetaForFiles(CONST FileNames: array of string; CONST useCurrentPageAsFallback:boolean):longint;
PROCEDURE updateFonts(CONST Font:TFont);
FUNCTION allPseudoNames:T_arrayOfString;
FUNCTION getMeta(CONST nameOrPseudoName:string):P_editorMeta;
FUNCTION getHelpPopupText:string;
FUNCTION getHelpLocation:T_searchTokenLocation;
PROCEDURE cycleEditors(CONST cycleForward:boolean);
PROCEDURE updateEditorsByGuiStatus;
PROCEDURE closeAllEditorsButCurrent;
PROCEDURE closeAllUnmodifiedEditors;
PROCEDURE checkForFileChanges;
PROCEDURE finalizeEditorMeta;
PROCEDURE saveWorkspace;
PROCEDURE gotoMarker(markerIndex:longint);
FUNCTION currentlyOpenFiles:T_arrayOfString;
FUNCTION workspaceFilename:string;
TYPE F_safeCallback=FUNCTION(CONST path,name,ext:string):string;
VAR safeCallback:F_safeCallback;
    runnerModel:T_runnerModel;
    recentlyActivated:T_fileHistory;
    folderHistory    :T_fileHistory;
    fileHistory      :T_fileHistory;
IMPLEMENTATION
VAR mainForm              :T_abstractMnhForm;
    inputPageControl      :TPageControl;
    breakpointsImagesList :TImageList;
    bookmarkImagesList    :TImageList;
    EditKeyUp             :TKeyEvent;
    EditMouseDown         :TMouseEvent;
    EditProcessUserCommand:TProcessCommandEvent;
    assistanceSynEdit     :TSynEdit;
    assistanceTabSheet    :TTabSheet;
    restoreMenuItem       :TMenuItem;
    outlineModel          :P_outlineTreeModel=nil;
    outlineGroupBox       :TGroupBox;
    globalBookmarks       :array[0..9] of record
                             editorIndex,lineIndex,columnIndex:longint;
                           end;

VAR editorMetaData:array of P_editorMeta;
    underCursor:T_tokenInfo;

FUNCTION currentlyOpenFiles:T_arrayOfString;
  VAR meta:P_editorMeta;
  begin
    result:='';
    for meta in editorMetaData do
    if meta^.enabled and meta^.isFile then append(result,meta^.fileInfo.filePath);
  end;

FUNCTION workspaceFilename:string;
  begin
    result:=configDir+'workspace.0';
  end;

CONST workspaceSerialVersion=2661226502;
FUNCTION loadWorkspace:boolean;
  VAR stream:T_bufferedInputStreamWrapper;
      i:longint;
      validMetaCount:longint=0;
  begin
    stream.createToReadFromFile(workspaceFilename);
    fileHistory.create(false);
    folderHistory.create(true);
    if not(stream.readDWord=workspaceSerialVersion) then begin
      stream.destroy;
      exit(false);
    end;
    if not(fileHistory  .loadFromStream(stream)) then exit(false);
    if not(folderHistory.loadFromStream(stream)) then exit(false);
    setLength(editorMetaData,stream.readNaturalNumber);
    if not(stream.allOkay) then begin
      setLength(editorMetaData,0);
      stream.destroy;
      exit(false);
    end;
    result:=true;
    for i:=0 to length(editorMetaData)-1 do begin
      new(editorMetaData[i],create(i,stream));
      result:=result and stream.allOkay;
      if result then validMetaCount:=i+1;
    end;
    result:=result and stream.allOkay;
    if not(result) then setLength(editorMetaData,validMetaCount) else begin
      if length(filesToOpenInEditor)=0
      then inputPageControl.activePageIndex:=stream.readLongint
      else inputPageControl.activePageIndex:=addOrGetEditorMetaForFiles(filesToOpenInEditor,true);
    end;
    for i:=0 to 9 do begin
      globalBookmarks[i].editorIndex:=stream.readInteger;
      globalBookmarks[i].lineIndex  :=stream.readInteger;
      globalBookmarks[i].columnIndex:=stream.readInteger;
      if (globalBookmarks[i].editorIndex>=0) and (globalBookmarks[i].editorIndex<length(editorMetaData)) and
         (globalBookmarks[i].lineIndex  >=0) then begin
        editorMetaData[globalBookmarks[i].editorIndex]^.editor.SetBookMark(i,globalBookmarks[i].columnIndex,globalBookmarks[i].lineIndex);
      end else globalBookmarks[i].editorIndex:=-1;
    end;
    stream.destroy;
  end;

PROCEDURE saveWorkspace;
  VAR stream:T_bufferedOutputStreamWrapper;
      i,k:longint;
      visibleEditorCount:longint=0;
      pageIndex:longint=0;
      editorIndexAfterLoading:T_arrayOfLongint;
  begin
    stream.createToWriteToFile(workspaceFilename);
    stream.writeDWord(workspaceSerialVersion);
    fileHistory.saveToStream(stream);
    folderHistory.saveToStream(stream);
    pageIndex:=inputPageControl.activePageIndex;
    for i:=0 to length(editorMetaData)-1 do if editorMetaData[i]^.enabled
    then inc(visibleEditorCount);
    stream.writeNaturalNumber(visibleEditorCount);
    setLength(editorIndexAfterLoading,length(editorMetaData));
    k:=0;
    for i:=0 to length(editorMetaData)-1 do if editorMetaData[i]^.enabled then begin
      editorIndexAfterLoading[i]:=k; inc(k);
      editorMetaData[i]^.saveToStream(stream);
    end else editorIndexAfterLoading[i]:=-1;
    stream.writeLongint(editorIndexAfterLoading[pageIndex]);
    for i:=0 to 9 do if globalBookmarks[i].editorIndex<0 then begin
      stream.writeInteger(-1);
      stream.writeInteger(-1);
      stream.writeInteger(-1);
    end else begin
      stream.writeInteger(editorIndexAfterLoading[globalBookmarks[i].editorIndex]);
      stream.writeInteger(globalBookmarks[i].lineIndex  );
      stream.writeInteger(globalBookmarks[i].columnIndex);
    end;
    stream.destroy;
  end;

PROCEDURE initNewWorkspace;
  VAR i:longint;
  begin
    for i:=0 to length(editorMetaData)-1 do dispose(editorMetaData[i],destroy);
    setLength(editorMetaData,1);
    new(editorMetaData[0],create(0));
    inputPageControl.activePageIndex:=0;
    setLength(fileHistory.items,0);
    setLength(folderHistory.items,0);
    for i:=0 to 9 do globalBookmarks[i].editorIndex:=-1;
  end;

PROCEDURE setupUnit(CONST p_mainForm              :T_abstractMnhForm;
                    CONST p_inputPageControl      :TPageControl;
                    CONST p_breakpointsImagesList :TImageList;
                    CONST p_bookmarkImagesList    :TImageList;
                    CONST p_assistanceSynEdit     :TSynEdit;
                    CONST p_assistanceTabSheet    :TTabSheet;
                    CONST outputHighlighter       :TSynMnhSyn;
                    CONST languageMenuRoot        :TMenuItem;
                    CONST p_restoreMenuItem       :TMenuItem;
                    CONST p_EditKeyUp             :TKeyEvent;
                    CONST p_EditMouseDown         :TMouseEvent;
                    CONST p_EditProcessUserCommand:TProcessCommandEvent;
                    CONST p_outlineGroupBox       :TGroupBox;
                    CONST p_outlineModel          :P_outlineTreeModel);

  VAR i:longint;
  begin
    for i:=0 to 9 do globalBookmarks[i].editorIndex:=-1;
    editorFont:=p_assistanceSynEdit.Font;
    setupEditorMetaBase(outputHighlighter,languageMenuRoot);

    mainForm              :=p_mainForm              ;
    inputPageControl      :=p_inputPageControl      ;
    breakpointsImagesList :=p_breakpointsImagesList ;
    bookmarkImagesList    :=p_bookmarkImagesList    ;
    EditKeyUp             :=p_EditKeyUp             ;
    EditMouseDown         :=p_EditMouseDown         ;
    EditProcessUserCommand:=p_EditProcessUserCommand;
    restoreMenuItem       :=p_restoreMenuItem;
    assistanceSynEdit     :=p_assistanceSynEdit     ;
    assistanceTabSheet    :=p_assistanceTabSheet    ;
    outlineGroupBox       :=p_outlineGroupBox       ;
    outlineModel          :=p_outlineModel;

    if not(loadWorkspace) then initNewWorkspace;
  end;

PROCEDURE gotoMarker(markerIndex:longint);
  VAR currentEdit:P_editorMeta;
      i:longint;
  begin
    if (markerIndex<0) or (markerIndex>=length(globalBookmarks)) then exit;
    if globalBookmarks[markerIndex].editorIndex<0 then exit;
    currentEdit:=getEditor;
    if currentEdit^.index=globalBookmarks[markerIndex].editorIndex then exit;
    for i:=0 to length(editorMetaData)-1 do if (editorMetaData[i]^.enabled) and (editorMetaData[i]^.index=globalBookmarks[markerIndex].editorIndex) then begin
      inputPageControl.activePageIndex:=i;
      editorMetaData[i]^.editor.CaretY:=globalBookmarks[markerIndex].lineIndex;
      editorMetaData[i]^.editor.CaretX:=globalBookmarks[markerIndex].columnIndex;
      mainForm.ActiveControl:=editorMetaData[i]^.editor;
    end;
  end;

CONSTRUCTOR T_editorMeta.create(CONST idx: longint);
  begin
    latestAssistanceReponse:=nil;
    paintedWithStateHash:=0;
    index:=idx;
    tabsheet:=TTabSheet.create(inputPageControl);
    tabsheet.PageControl:=inputPageControl;
    createWithParent(tabsheet,bookmarkImagesList);
    paintedWithStateHash:=0;
    index:=idx;
    editor_.Gutter.MarksPart.width:=breakpointsImagesList.width+bookmarkImagesList.width+10;
    editor_.OnChange            :=@InputEditChange;
    editor_.OnKeyUp             :=EditKeyUp;
    editor_.OnMouseDown         :=EditMouseDown;
    editor_.OnProcessCommand    :=EditProcessUserCommand;
    editor_.OnSpecialLineMarkup :=@(runnerModel.InputEditSpecialLineMarkup);
    editor_.onPlaceBookmark     :=@onPlaceBookmark;
    editor_.onClearBookmark     :=@onClearBookmark;
    initForNewFile;
  end;

CONST editorMetaSerial=1417366168;

CONSTRUCTOR T_editorMeta.create(CONST idx: longint; VAR stream:T_bufferedInputStreamWrapper);
  VAR lineCount,markCount:longint;
      i:longint;
  begin
    create(idx);
    if not(stream.readDWord=editorMetaSerial) then begin //#0
      stream.logWrongTypeError;
      exit;
    end;
    tabsheet.tabVisible:=true;
    with fileInfo do begin
      filePath     :=stream.readAnsiString; //#1
      isChanged    :=stream.readBoolean;    //#2
      strictlyReadOnly:=stream.readBoolean; //#3
      if isChanged then fileAccessAge:=stream.readDouble;  //#4
      ignoreDeleted:=false;
    end;
    editor.clearAll;
    if fileInfo.isChanged
    then begin
      editor.lines.clear;
      lineCount:=stream.readNaturalNumber; //#5
      for i:=1 to lineCount do editor.lines.append(stream.readAnsiString); //#6
    end else setFile(fileInfo.filePath);
    markCount:=stream.readNaturalNumber; //#7
    for i:=1 to markCount do _add_breakpoint_(stream.readNaturalNumber); //#8
    editor.CaretX:=stream.readNaturalNumber; //#9
    editor.CaretY:=stream.readNaturalNumber; //#10
    language_:=T_language(stream.readByte);  //#11
    editor.modified:=fileInfo.isChanged;
    updateSheetCaption;
  end;

PROCEDURE T_editorMeta.saveToStream(VAR stream:T_bufferedOutputStreamWrapper);
  VAR i:longint;
      editorLine:string;
      saveChanged:boolean;
      k:longint;
  begin
    stream.writeDWord(editorMetaSerial); //#0
    if fileInfo.filePath=''
    then stream.writeAnsiString(               fileInfo.filePath )  //#1
    else stream.writeAnsiString(expandFileName(fileInfo.filePath)); //#1
    saveChanged:=changed;
    stream.writeBoolean(saveChanged); //#2
    stream.writeBoolean(strictlyReadOnly);   //#3
    if saveChanged then begin
      stream.writeDouble(fileInfo.fileAccessAge);  //#4
      stream.writeNaturalNumber(editor.lines.count); //#5
      for editorLine in editor.lines do stream.writeAnsiString(editorLine); //#6
    end;
    k:=0;
    for i:=0 to editor.Marks.count-1 do if not(editor.Marks[i].IsBookmark) then inc(k);
    stream.writeNaturalNumber(k); //#7
    for i:=0 to editor.Marks.count-1 do if not(editor.Marks[i].IsBookmark) then stream.writeNaturalNumber(editor.Marks[i].line); //#8

    stream.writeNaturalNumber(editor.CaretX); //#9
    stream.writeNaturalNumber(editor.CaretY); //#10
    stream.writeByte(ord(language));          //#11
  end;

DESTRUCTOR T_editorMeta.destroy;
  begin
    inherited destroy;
    disposeCodeAssistanceResponse(latestAssistanceReponse);
  end;

FUNCTION T_editorMeta.getPath: ansistring;
  begin
    result:=pseudoName(false);
  end;

FUNCTION T_editorMeta.isPseudoFile: boolean;
  begin
    result:=fileInfo.filePath='';
  end;

PROCEDURE T_editorMeta.activate;
  VAR l:T_language;
  begin
    if not(enabled) then exit;
    inherited activate;
    mainForm.activeFileChanged(updateSheetCaption,language_=LANG_MNH,fileInfo.filePath='');

    for l in T_language do if Assigned(fileTypeMeta[l].menuItem) then begin
      fileTypeMeta[l].menuItem.OnClick:=@languageMenuItemClick;
      fileTypeMeta[l].menuItem.checked:=(l=language);
    end;
    try
      restoreMenuItem.enabled:=isRestorable(getPath)>=0;
      recentlyActivated.fileClosed(getPath);
      folderHistory.fileClosed(ExtractFileDir(getPath));
      if language_=LANG_MNH then begin
        outlineGroupBox.visible:=true;
        editor.highlighter:=highlighter;
        paintedWithStateHash:=0;
        assistanceTabSheet.tabVisible:=true;
        triggerCheck;
        completionLogic.assignEditor(editor_,nil);
        runnerModel.firstCallAfterActivation:=true;
      end else begin
        outlineGroupBox.visible:=false;
        editor.highlighter:=fileTypeMeta[language_].highlighter;
        assistanceSynEdit.clearAll;
        assistanceTabSheet.caption:='';
        assistanceTabSheet.tabVisible:=false;
        disposeCodeAssistanceResponse(latestAssistanceReponse);
        completionLogic.assignEditor(editor_,nil);
      end;
      editor.Gutter.MarksPart.visible:=true;
      editor.readonly                :=runnerModel.areEditorsLocked;
      mainForm.onDebuggerEvent;
      mainForm.ActiveControl:=editor_;
    except end; //catch and ignore all exceptions
  end;

PROCEDURE T_editorMeta.InputEditChange(Sender: TObject);
  PROCEDURE invalidateWordUnderCursor;
    begin
      underCursor.location.line:=-1;
    end;

  begin
    if not(enabled) then exit;
    if language_=LANG_MNH then begin
      triggerCheck;
      invalidateWordUnderCursor;
    end;
    mainForm.activeFileChanged(updateSheetCaption,language_=LANG_MNH,fileInfo.filePath='');
  end;

PROCEDURE T_editorMeta.languageMenuItemClick(Sender: TObject);
  begin
    setLanguage(T_language(TMenuItem(Sender).Tag));
  end;

FUNCTION T_editorMeta.saveAsWithDialog: boolean;
  VAR path,name,ext:string;
  begin
    if isFile then begin
      path:=ExtractFileDir(fileInfo.filePath);
      name:=ExtractFileNameOnly(fileInfo.filePath);
      ext :=extractFileExt(fileInfo.filePath);
    end else begin
      path:=GetCurrentDir;
      name:='';
      if length(fileTypeMeta[language].extensions)>0
      then ext:='.'+lowercase(fileTypeMeta[language].extensions[0])
      else ext:='';
    end;
    name:=safeCallback(path,name,ext);
    if name<>'' then begin
      mainForm.activeFileChanged(saveFile(name),language_=LANG_MNH,fileInfo.filePath='');
      result:=true;
    end else result:=false;
  end;

FUNCTION T_editorMeta.saveWithDialog: boolean;
  begin
    if isFile then begin
      mainForm.activeFileChanged(saveFile(),language_=LANG_MNH,fileInfo.filePath='');
      result:=true;
    end else result:=saveAsWithDialog;
  end;

PROCEDURE T_editorMeta.closeEditorQuietly;
  VAR k:longint;
  begin
    tabsheet.tabVisible:=false;
    editor.clearAll;
    with fileInfo do begin
      filePath:='';
      isChanged:=false;
      ignoreDeleted:=false;
    end;
    for k:=0 to length(globalBookmarks)-1 do if globalBookmarks[k].editorIndex=index then begin
      globalBookmarks[k].editorIndex:=-1;
      globalBookmarks[k].lineIndex:=-1;
      globalBookmarks[k].columnIndex:=-1;
    end;
    editor.modified:=false;
    strictlyReadOnly:=false;
    getEditor^.activate;
  end;

PROCEDURE T_editorMeta.closeEditorWithDialogs;
  VAR mr:longint;
  begin
    if not(enabled) then exit;
    if changed then begin
      mr:=closeDialogForm.showOnClose(pseudoName(true));
      if mr=mrOk then if not(saveWithDialog) then exit;
      if mr=mrCancel then exit;
    end;
    if isFile then begin
      fileHistory.fileClosed(fileInfo.filePath);
      folderHistory.fileClosed(ExtractFileDir(fileInfo.filePath));
    end;
    closeEditorQuietly;
  end;

FUNCTION T_editorMeta.enabled:boolean;
  begin
    result:=tabsheet.tabVisible;
  end;

PROCEDURE T_editorMeta.guessLanguage(CONST fallback: T_language);
  begin
    setLanguage(copy(extractFileExt(fileInfo.filePath),2,10),fallback);
  end;

PROCEDURE T_editorMeta.setFile(CONST fileName: string);
  FUNCTION isDatastore:boolean;
    begin
      result:=uppercase(copy(extractFileExt(fileName),2,9))='DATASTORE';
    end;
  VAR datastore:boolean=false;
      defaultLoad:boolean=true;
      tempLines:T_arrayOfString;
      l:string;
  begin
    tabsheet.tabVisible:=true;
    fileInfo.filePath:=fileName;
    fileInfo.ignoreDeleted:=false;
    editor.clearAll;
    if fileName<>'' then try
      strictlyReadOnly:=false;
      if isDatastore then begin
        datastore:=true;
        if isBinaryDatastore(fileName,tempLines) then begin
          editor.lines.clear;
          for l in tempLines do editor.lines.add(l);
          strictlyReadOnly:=true;
          editor.readonly:=true;
          defaultLoad:=false;
        end;
        language:=LANG_MNH;
      end;
      if defaultLoad then editor.lines.loadFromFile(fileInfo.filePath);
      fileAge(fileInfo.filePath,fileInfo.fileAccessAge);
      fileInfo.isChanged:=false;
      editor.modified:=false;
    except
      editor.lines.clear;
      fileInfo.isChanged:=true;
      fileInfo.fileAccessAge:=0;
    end;
    if not(datastore) then guessLanguage(LANG_TXT);
    updateSheetCaption;
  end;

PROCEDURE T_editorMeta.initForNewFile;
  begin
    tabsheet.tabVisible:=true;
    with fileInfo do begin
      isChanged       :=false;
      fileAccessAge:=0;
      filePath     :='';
      ignoreDeleted:=false;
    end;
    editor.clearAll;
    editor.modified:=false;
    setLanguage(LANG_MNH);
    updateSheetCaption;
  end;

PROCEDURE T_editorMeta.reloadFile(CONST fileName: string);
  begin
    if enabled and (fileInfo.filePath=SysToUTF8(fileName)) and (fileExists(fileName)) then begin
      editor.lines.loadFromFile(fileInfo.filePath);
      fileAge(fileInfo.filePath,fileInfo.fileAccessAge);
      editor.modified:=false;
      fileInfo.isChanged:=false;
      mainForm.activeFileChanged(updateSheetCaption,language_=LANG_MNH,fileInfo.filePath='');
      if language_=LANG_MNH then triggerCheck;
    end;
  end;

FUNCTION T_editorMeta.caretInMainFormCoordinates: TPoint;
  begin
    result.x:=editor.CaretXPix;
    result.y:=editor.CaretYPix+editor.LineHeight;
    result:=editor.ClientToParent(result,mainForm);
  end;

PROCEDURE T_editorMeta.setUnderCursor(CONST updateMarker,forHelpOrJump: boolean; CONST caret: TPoint);
  VAR m:P_editorMeta;
      wordUnderCursor:string;
  begin
    if (language_<>LANG_MNH) or not(updateMarker or forHelpOrJump) then exit;
    wordUnderCursor:=editor.GetWordAtRowCol(caret);
    if updateMarker then begin
      for m in editorMetaData do if m^.enabled then m^.setMarkedWord(wordUnderCursor);
      editor.Repaint;
    end;
    if forHelpOrJump and (latestAssistanceReponse<>nil) then with editor do
      latestAssistanceReponse^.explainIdentifier(lines[caret.y-1],caret.y,caret.x,underCursor);
  end;

PROCEDURE T_editorMeta.setUnderCursor(CONST updateMarker, forHelpOrJump: boolean);
  begin
    setUnderCursor(updateMarker,forHelpOrJump,editor.CaretXY);
  end;

FUNCTION T_editorMeta.canRenameUnderCursor(OUT orignalId:string; OUT tokTyp:T_tokenType; OUT ref:T_searchTokenLocation; OUT mightBeUsedElsewhere:boolean):boolean;
  begin
    if language<>LANG_MNH then exit(false);
    setUnderCursor(false,true);
    result   :=underCursor.canRename;
    orignalId:=underCursor.idWithoutIsPrefix;
    tokTyp   :=underCursor.tokenType;
    if tokTyp in [tt_each,tt_parallelEach] then tokTyp:=tt_eachParameter;
    ref      :=underCursor.location;
    mightBeUsedElsewhere:=underCursor.mightBeUsedInOtherPackages and (fileInfo.filePath<>'');
  end;

PROCEDURE T_editorMeta.doRename(CONST ref:T_searchTokenLocation; CONST oldId,newId:string; CONST renameInOtherEditors:boolean=false);
  VAR meta:P_editorMeta;
      lineIndex:longint;
      lineTxt:string;
      recycler:T_recycler;
  PROCEDURE updateLine;
    VAR lineStart,lineEnd:TPoint;
    begin
      lineStart.y:=lineIndex+1; lineStart.x:=0;
      lineEnd  .y:=lineIndex+1; lineEnd  .x:=length(editor.lines[lineIndex])+1;
      editor.SetTextBetweenPoints(lineStart,lineEnd,lineTxt);
    end;

  begin
    if not(enabled) or (language<>LANG_MNH) then exit;
    if renameInOtherEditors then saveFile();
    recycler.initRecycler;
    updateAssistanceResponse(doCodeAssistanceSynchronously(@self,recycler));
    recycler.cleanup;

    editor.BeginUpdate(true);
    with editor do for lineIndex:=0 to lines.count-1 do begin
      lineTxt:=lines[lineIndex];
      if latestAssistanceReponse^.renameIdentifierInLine(ref,oldId,newId,lineTxt,lineIndex+1) then updateLine;
    end;
    editor.EndUpdate;

    if renameInOtherEditors then for meta in editorMetaData do if meta<>@self then meta^.doRename(ref,oldId,newId);
  end;

PROCEDURE T_editorMeta.setWorkingDir;
  begin
    if fileInfo.filePath='' then SetCurrentDir(ExtractFileDir(paramStr(0)))
                            else SetCurrentDir(ExtractFileDir(fileInfo.filePath));
  end;

PROCEDURE T_editorMeta.onClearBookmark(Sender: TObject; VAR mark: TSynEditMark);
  begin
    globalBookmarks[mark.BookmarkNumber].editorIndex:=-1;
  end;

PROCEDURE T_editorMeta.onPlaceBookmark(Sender: TObject; VAR mark: TSynEditMark);
  VAR other:P_editorMeta;
  begin
    if not(Assigned(mark)) then exit;
    for other in editorMetaData do if (other<>@self) and (other^.enabled) then other^.clearBookmark(mark.BookmarkNumber);
    globalBookmarks[mark.BookmarkNumber].editorIndex:=index;
    globalBookmarks[mark.BookmarkNumber].lineIndex  :=mark.line;
    globalBookmarks[mark.BookmarkNumber].columnIndex:=mark.column;
  end;

PROCEDURE T_editorMeta.clearBookmark(markIndex:longint);
  VAR x:longint=0;
      y:longint=0;
  begin
    if editor_.GetBookMark(markIndex,x,y) then editor_.clearBookmark(markIndex);
  end;

PROCEDURE T_editorMeta.toggleBreakpoint;
  VAR i:longint;
  begin
    for i:=0 to editor_.Marks.count-1 do if (editor_.Marks[i].line=editor_.CaretY) and not(editor_.Marks[i].IsBookmark) then begin
      editor_.Marks.remove(editor.Marks[i]);
      runEvaluator.globals.stepper^.removeBreakpoint(pseudoName,editor_.CaretY);
      exit;
    end;
    runEvaluator.globals.stepper^.addBreakpoint(pseudoName,editor_.CaretY);
    _add_breakpoint_(editor_.CaretY);
  end;

FUNCTION T_editorMeta.isFile: boolean;
  begin
    result:=fileInfo.filePath<>'';
  end;

FUNCTION T_editorMeta.pseudoName(CONST short: boolean): ansistring;
  begin
    if fileInfo.filePath<>'' then begin
      if short then result:=extractFileName(fileInfo.filePath)
               else result:=fileInfo.filePath;
    end else result:='<new '+intToStr(index)+'>';
  end;

FUNCTION T_editorMeta.defaultExtensionByLanguage: ansistring;
  begin
    result:=fileTypeMeta[language_].extensions[0];
  end;

PROCEDURE T_editorMeta.setStepperBreakpoints;
  VAR i:longint;
  begin
    for i:=0 to editor.Marks.count-1 do
      if not(editor.Marks[i].IsBookmark)
      then runEvaluator.globals.stepper^.addBreakpoint(pseudoName,editor.Marks[i].line);
  end;

PROCEDURE T_editorMeta._add_breakpoint_(CONST lineIndex: longint);
  VAR m:TSynEditMark;
  begin
    m:=TSynEditMark.create(editor);
    m.line:=lineIndex;
    m.ImageList:=breakpointsImagesList;
    m.ImageIndex:=0;
    m.visible:=true;
    editor.Marks.add(m);
  end;

FUNCTION T_editorMeta.updateSheetCaption: ansistring;
  begin
    if changed then result:=' *'
               else result:='';
    tabsheet.caption:=pseudoName(true)+result;
    result:=APP_TITLE+' '+pseudoName(false)+result{$ifdef debugMode}+' [debug]'{$endif};
  end;

PROCEDURE T_editorMeta.triggerCheck;
  begin
    postCodeAssistanceRequest(@self);
  end;

PROCEDURE T_editorMeta.updateAssistanceResponse(CONST response:P_codeAssistanceResponse);
  CONST SHORTCUT_SUFFIX=' (F2)';
  VAR  hasErrors,hasWarnings:boolean;
  begin
    if (response=nil) or (language_<>LANG_MNH) then exit;
    disposeCodeAssistanceResponse(latestAssistanceReponse);
    latestAssistanceReponse:=response;
    completionLogic.assignEditor(editor_,latestAssistanceReponse);
    if (paintedWithStateHash<>latestAssistanceReponse^.stateHash) then begin
      paintedWithStateHash:=latestAssistanceReponse^.stateHash;
      latestAssistanceReponse^.updateHighlightingData(highlighter.highlightingData);
      editor.highlighter:=highlighter;
      editor.Repaint;
      assistanceSynEdit.clearAll;
      latestAssistanceReponse^.getErrorHints(assistanceSynEdit,hasErrors,hasWarnings);
      if hasErrors then begin if hasWarnings then assistanceTabSheet.caption:='Errors + Warnings'+SHORTCUT_SUFFIX
                                             else assistanceTabSheet.caption:='Errors'+SHORTCUT_SUFFIX; end
                   else begin if hasWarnings then assistanceTabSheet.caption:='Warnings'+SHORTCUT_SUFFIX
                                             else assistanceTabSheet.caption:='(no warnings)'+SHORTCUT_SUFFIX; end;
      outlineModel^.update(latestAssistanceReponse^.package);
    end;
  end;

PROCEDURE T_editorMeta.pollAssistanceResult;
  VAR response:P_codeAssistanceResponse;
  begin
    if language_<>LANG_MNH then exit;
    response:=getLatestAssistanceResponse(@self);
    if response<>nil then updateAssistanceResponse(response);
  end;

FUNCTION T_editorMeta.changed: boolean;
  begin
    result:=fileInfo.isChanged or editor.modified;
  end;

FUNCTION T_editorMeta.saveFile(CONST fileName: string): string;
  VAR arr:T_arrayOfString;
      i:longint;
      previousName:string;
      lineEndingSetting:byte;
  begin
    previousName:=fileInfo.filePath;
    if fileName<>'' then fileInfo.filePath:=expandFileName(fileName);
    if (previousName<>'') and (previousName<>fileInfo.filePath) then begin
      fileHistory.fileClosed(previousName);
      folderHistory.fileClosed(ExtractFileDir(previousName));
    end;
    if previousName<>fileInfo.filePath
    then lineEndingSetting:=settings.newFileLineEnding
    else lineEndingSetting:=settings.overwriteLineEnding;
    setLength(arr,editor.lines.count);
    for i:=0 to length(arr)-1 do arr[i]:=editor.lines[i];
    with fileInfo do begin
      writeFileLines(filePath,arr,LINE_ENDING[lineEndingSetting],false);
      fileAge(filePath,fileAccessAge);
      isChanged:=false;
      editor.modified:=false;
      editor.MarkTextAsSaved;
      if (filePath=utilityScriptFileName) then runEvaluator.ensureEditScripts();
    end;
    result:=updateSheetCaption;
  end;

FUNCTION T_editorMeta.fileIsDeleted: boolean;
  begin
    result:=enabled and isFile and not(fileExists(fileInfo.filePath));
  end;

FUNCTION T_editorMeta.fileIsModifiedOnFileSystem: boolean;
  VAR currentFileAge:double;
  begin
    if not(enabled and isFile) or changed then exit(false);
    fileAge(fileInfo.filePath,currentFileAge);
    result:=currentFileAge<>fileInfo.fileAccessAge;
  end;

PROCEDURE T_editorMeta.updateContentAfterEditScript(
  CONST stringListLiteral: P_listLiteral);
  VAR concatenatedText:ansistring='';
      i:longint;
  begin
    if stringListLiteral^.literalType<>lt_stringList then exit;
    for i:=0 to stringListLiteral^.size-1 do begin
      if i>0 then concatenatedText:=concatenatedText+LineEnding;
      concatenatedText:=concatenatedText+P_stringLiteral(stringListLiteral^.value[i])^.value;
    end;
    editor.BeginUndoBlock;
    editor.SelectAll;
    editor.SelText:=concatenatedText;
    editor.EndUndoBlock;
  end;

FUNCTION T_editorMeta.resolveImport(CONST text: string): string;
  begin
    if latestAssistanceReponse=nil then result:='' else result:=latestAssistanceReponse^.resolveImport(text);
  end;

PROCEDURE T_editorMeta.exportToHtml;
  VAR SynExporterHTML: TSynExporterHTML;
      name:string;
  begin
    name:=safeCallback(GetCurrentDir,'','.html');
    if name='' then exit;
    SynExporterHTML:=TSynExporterHTML.create(nil);
    SynExporterHTML.title:=pseudoName();
    SynExporterHTML.highlighter:=editor.highlighter;
    SynExporterHTML.ExportAll(editor.lines);
    SynExporterHTML.saveToFile(name);
    SynExporterHTML.free;
  end;

//==================================================================
FUNCTION hasEditor:boolean;
  VAR i:longint;
  begin
    i:=inputPageControl.activePageIndex;
    result:=(i>=0) and (i<length(editorMetaData));
  end;

FUNCTION getEditor:P_editorMeta;
  VAR i:longint;
  begin
    i:=inputPageControl.activePageIndex;
    if (i>=0) and (i<length(editorMetaData))
    then result:=editorMetaData[i]
    else result:=nil;

    if result=nil then result:=editorMetaData[addEditorMetaForNewFile];
  end;

FUNCTION addEditorMetaForNewFile:longint;
  VAR i:longint;
  begin
    i:=length(editorMetaData)-1;
    //decrease i until a visible meta is encountered
    while (i>=0) and not(editorMetaData[i]^.enabled) do dec(i);
    inc(i);
    //i now is the index of the last visible editor meta +1
    if (i>=0) and (i<length(editorMetaData)) then begin
      editorMetaData[i]^.initForNewFile;
      editorMetaData[i]^.activate;
      exit(i);
    end;

    i:=length(editorMetaData);
    setLength(editorMetaData,i+1);
    new(editorMetaData[i],create(i));
    editorMetaData[i]^.editor.Font:=editorFont;

    result:=i;
    editorMetaData[i]^.editor.Gutter.MarksPart.visible:=true;
    editorMetaData[i]^.editor.readonly                :=runnerModel.areEditorsLocked or editorMetaData[i]^.strictlyReadOnly;
    editorMetaData[i]^.activate;
  end;

FUNCTION addOrGetEditorMetaForFiles(CONST FileNames: array of string; CONST useCurrentPageAsFallback:boolean):longint;

  PROCEDURE openSingleFile(CONST fileName:ansistring);
    FUNCTION isPseudoName:boolean;
      begin
        result:=(length(fileName)>1)
            and (fileName[1]='<')
            and (fileName[length(fileName)]='>');
      end;

    VAR filePath:ansistring;
        i:longint;
    begin
      if isPseudoName then begin
        for i:=0 to length(editorMetaData)-1 do if (editorMetaData[i]^.enabled) and (editorMetaData[i]^.pseudoName=fileName) then begin
          result:=i;
          exit;
        end;
      end else begin
        filePath:=expandFileName(fileName);
        for i:=0 to length(editorMetaData)-1 do if (editorMetaData[i]^.enabled) and (editorMetaData[i]^.fileInfo.filePath=filePath) then begin
          result:=i;
          exit;
        end;
        result:=addEditorMetaForNewFile();
        editorMetaData[result]^.setFile(filePath);
        editorMetaData[result]^.editor.Font:=editorFont;
      end;
    end;

  VAR f:string;
  begin
    if useCurrentPageAsFallback then result:=inputPageControl.activePageIndex
                                else result:=-1;
    for f in FileNames do openSingleFile(f);
    if result<>-1 then editorMetaData[result]^.activate;
  end;

PROCEDURE updateFonts(CONST Font:TFont);
  VAR m:P_editorMeta;
  begin
    editorFont:=Font;
    for m in editorMetaData do m^.editor.Font:=editorFont;
  end;

FUNCTION allPseudoNames:T_arrayOfString;
  VAR m:P_editorMeta;
  begin
    setLength(result,0);
    for m in editorMetaData do if m^.enabled then append(result,m^.pseudoName);
  end;

FUNCTION getMeta(CONST nameOrPseudoName:string):P_editorMeta;
  VAR m:P_editorMeta;
  begin
    result:=nil;
    for m in editorMetaData do if (m^.enabled) and (m^.pseudoName()=nameOrPseudoName) then exit(m);
  end;

FUNCTION getHelpPopupText:string;
  begin
    result:=underCursor.infoText;
  end;

FUNCTION getHelpLocation:T_searchTokenLocation;
  begin
    {$ifdef debugMode} writeln(stdErr,'        DEBUG: getHelpLocation filename="',underCursor.location.fileName,'"; line=',underCursor.location.line,'; column=',underCursor.location.column); {$endif}
    result:=underCursor.location;
  end;

PROCEDURE cycleEditors(CONST cycleForward:boolean);
  VAR i,k:longint;
      delta:longint;
  begin
    i:=inputPageControl.activePageIndex;
    if cycleForward then delta:=1 else delta:=length(editorMetaData)-1;
    for k:=0 to length(editorMetaData)-1 do begin
      i:=(i+delta) mod (length(editorMetaData));
      if editorMetaData[i]^.enabled then begin
        inputPageControl.activePageIndex:=i;
        editorMetaData[i]^.activate;
        exit;
      end;
    end;
    inputPageControl.activePageIndex:=addEditorMetaForNewFile;
  end;

PROCEDURE updateEditorsByGuiStatus;
  VAR m:P_editorMeta;
  begin
    for m in editorMetaData do begin
      m^.editor.Gutter.MarksPart.visible:=true;
      m^.editor.readonly                :=runnerModel.areEditorsLocked or m^.strictlyReadOnly;
    end;
  end;

PROCEDURE closeAllEditorsButCurrent;
  VAR m:P_editorMeta;
  begin
    if not(hasEditor and getEditor^.enabled) then exit;
    for m in editorMetaData do if (m^.index<>inputPageControl.activePageIndex) and (m^.enabled) then m^.closeEditorWithDialogs;
  end;

PROCEDURE closeAllUnmodifiedEditors;
  VAR m:P_editorMeta;
  begin
    for m in editorMetaData do if not(m^.changed) then m^.closeEditorWithDialogs;
    if not(hasEditor and getEditor^.enabled) then cycleEditors(true);
  end;

VAR doNotCheckFileBefore:double;
PROCEDURE checkForFileChanges;
  VAR m:P_editorMeta;
      modalRes:longint;
  begin
    if now<doNotCheckFileBefore then exit;
    doNotCheckFileBefore:=now+1;
    for m in editorMetaData do with m^ do
    if fileIsDeleted and not(fileInfo.ignoreDeleted) then begin
      modalRes:=closeDialogForm.showOnDeleted(fileInfo.filePath);
      if modalRes=mrOk then closeEditorQuietly;
      if modalRes=mrClose then begin if not(saveWithDialog) then fileInfo.isChanged:=true; end else begin
        fileInfo.ignoreDeleted:=true;
        fileInfo.isChanged:=true;
        updateSheetCaption;
      end;
      continue;
    end else if fileIsModifiedOnFileSystem then begin
      modalRes:=closeDialogForm.showOnOutOfSync(fileInfo.filePath);
      if modalRes=mrOk then reloadFile(fileInfo.filePath);
      if modalRes=mrClose then begin if not(saveWithDialog) then fileInfo.isChanged:=true; end else
      fileInfo.isChanged:=true;
    end;
    doNotCheckFileBefore:=now+ONE_SECOND;
  end;

PROCEDURE finalizeEditorMeta;
  VAR i:longint;
  begin
    finalizeCodeAssistance;
    if outlineModel<>nil then begin
      dispose(outlineModel,destroy);
      outlineModel:=nil;
    end;
    for i:=0 to length(editorMetaData)-1 do dispose(editorMetaData[i],destroy);
    setLength(editorMetaData,0);
  end;

PROCEDURE T_runnerModel.setDebugMode(CONST value: boolean);
  begin
    if value=debugMode_ then exit;
    debugMode_:=value;
    if (runEvaluator.evaluationRunning) and not(runEvaluator.getRunnerStateInfo.state=es_editRunning) then runEvaluator.haltEvaluation;
  end;

CONSTRUCTOR T_runnerModel.create;
  begin
    debugMode_:=false;
    stackTracing:=false;
    with lastStart do begin mainCall:=false; parameters:=''; end;
  end;

DESTRUCTOR T_runnerModel.destroy;
  begin

  end;

FUNCTION T_runnerModel.areEditorsLocked: boolean;
  begin
    result:=(debugMode_ and runEvaluator.evaluationRunning) or (runEvaluator.getRunnerStateInfo.state=es_editRunning);
  end;

FUNCTION T_runnerModel.canRun(CONST quickMode:boolean=false): boolean;
  begin
    result:=not(runEvaluator.evaluationRunningOrPending) and
            (quickMode or hasEditor and (getEditor^.language=LANG_MNH));
  end;

PROCEDURE T_runnerModel.customRun(CONST mainCall, profiling: boolean; CONST mainParameters: string);
  VAR m:P_editorMeta;
      contextType:T_evaluationContextType;
  begin
    if not(canRun) then exit;
    //adapter reset
    guiAdapters.clear;
    guiOutAdapter.flushClear;
    plotSystem.resetOnEvaluationStart(false);
    guiOutAdapter.flushToGui;
    //dynamic forms reset
    resetPlot(mainCall);
    resetTableForms;
    resetTreeForms;
    //environment and options
    getEditor^.setWorkingDir;
    if debugMode then begin
      updateEditorsByGuiStatus;
      runEvaluator.globals.stepper^.clearBreakpoints;
      for m in editorMetaData do m^.setStepperBreakpoints;
    end;
    if profiling then begin
      if debugMode_ then contextType:=ect_debuggingAndProfiling
                    else contextType:=ect_profiling;
    end else begin
      if debugMode_        then contextType:=ect_debugging
      else if stackTracing then contextType:=ect_stackTracing
                           else contextType:=ect_normal;
    end;

    if mainCall then runEvaluator.callMain(getEditor,mainParameters,contextType,firstCallAfterActivation)
                else runEvaluator.evaluate(getEditor,               contextType,firstCallAfterActivation);
    firstCallAfterActivation:=false;
    lastStart.mainCall:=mainCall;
    lastStart.parameters:=mainParameters;
  end;

PROCEDURE T_runnerModel.runExternally(CONST mainParameters:string='');
  VAR callParameters:T_arrayOfString;
      flag:T_cmdLineFlag;
      executor:string='';
  begin
    callParameters:=FLAG_PAUSE_ALWAYS;
    with settings.externalRunOptions do begin
      for flag in flags do
        append(callParameters,FLAG_TEXT[flag]);
      if verbosity<>'' then append(callParameters,'-v'+verbosity);
      if customFolder=''
      then getEditor^.setWorkingDir
      else SetCurrentDirUTF8(customFolder);
      if callLightFlavour then executor:=settings.lightFlavourLocation
                          else executor:=paramStr(0);
    end;

    append(callParameters,getEditor^.getPath);
    append(callParameters,splitCommandLine(trim(mainParameters)));
    runCommandAsyncOrPipeless(executor,callParameters,true);
  end;

PROCEDURE T_runnerModel.rerun(CONST profiling:boolean);
  begin
    customRun(lastStart.mainCall,profiling,lastStart.parameters);
  end;

PROCEDURE T_runnerModel.InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);
  begin
    Special:=runEvaluator.globals.isPaused and runEvaluator.evaluationRunning and (Sender=debugLine.editor) and (line=debugLine.line);
  end;

PROCEDURE T_runnerModel.doDebuggerAction(CONST newState: T_debuggerState);
  begin
    runEvaluator.globals.stepper^.setState(newState);
    mainForm.onDebuggerEvent;
    if hasEditor then with getEditor^ do begin
      editor.Gutter.MarksPart.visible:=true;
      editor.readonly:=areEditorsLocked or strictlyReadOnly;
    end;
  end;

PROCEDURE T_runnerModel.markDebugLine(CONST editor:TSynEdit; CONST line:longint);
  begin
    debugLine.editor:=editor;
    debugLine.line  :=line;
  end;

PROCEDURE T_runnerModel.haltEvaluation;
  begin
    runEvaluator.haltEvaluation;
    if debugMode_ then runEvaluator.globals.stepper^.haltEvaluation;
  end;

INITIALIZATION
  setLength(editorMetaData,0);
  doNotCheckFileBefore:=now;
  recentlyActivated.create(false);
FINALIZATION
  recentlyActivated.destroy;
  folderHistory    .destroy;
  fileHistory      .destroy;
end.
