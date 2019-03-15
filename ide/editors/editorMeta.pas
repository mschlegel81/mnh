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
  SynEdit, SynEditMiscClasses, SynEditMarks, SynEditKeyCmds,
  //GUI: highlighters
  SynHighlighterMnh,
  closeDialog,
  mnh_tables,
  mnh_plotForm,
  //MNH:
  mnh_doc,
  //outlines,
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
  codeAssistance,
  ideLayoutUtil;

TYPE
P_editorMeta=^T_editorMeta;
T_editorMeta=object(T_basicEditorMeta)
  private
    metaIndex:longint;

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
    PROCEDURE languageMenuItemClick(Sender: TObject);

    CONSTRUCTOR create(CONST mIdx:longint);
    CONSTRUCTOR create(CONST mIdx:longint; VAR stream:T_bufferedInputStreamWrapper);
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper);

    //Bookmark related
    FUNCTION markLocation(CONST line,column:longint):T_searchTokenLocation;
    PROCEDURE _add_breakpoint_(CONST lineIndex:longint);
    PROCEDURE clearBookmark(markIndex:longint);
    PROCEDURE toggleBreakpoint;

    //Assistant related
    PROCEDURE triggerCheck;
    PROCEDURE updateAssistanceResponse(CONST response:P_codeAssistanceResponse);

    PROCEDURE setFile(CONST fileName:string);
    FUNCTION updateSheetCaption:ansistring;
  public
    //Editor events
    PROCEDURE InputEditChange(Sender: TObject);
    PROCEDURE processUserCommand(Sender: TObject; VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer); virtual;
    PROCEDURE onClearBookmark(Sender: TObject; VAR mark: TSynEditMark);
    PROCEDURE onPlaceBookmark(Sender: TObject; VAR mark: TSynEditMark);
    //Inherited overrides
    DESTRUCTOR destroy; virtual;
    FUNCTION getPath:ansistring; virtual;
    FUNCTION isPseudoFile: boolean; virtual;
    PROCEDURE activate; virtual;

    //Misc. queries
    FUNCTION pseudoName(CONST short:boolean=false):ansistring;

    //Externally triggered actions
    PROPERTY getCodeAssistanceData:P_codeAssistanceResponse read latestAssistanceReponse;
    PROCEDURE pollAssistanceResult;

    //PROCEDURE closeEditorWithDialogs;
    //FUNCTION saveAsWithDialog:boolean;
    //FUNCTION saveWithDialog:boolean;
    //PROCEDURE reloadFile(CONST fileName:string);
    //PROCEDURE exportToHtml;
    //PROPERTY getCodeAssistanceData:P_codeAssistanceResponse read latestAssistanceReponse;
    //FUNCTION caretInMainFormCoordinates:TPoint;
    //PROCEDURE setUnderCursor(CONST updateMarker,forHelpOrJump: boolean; CONST caret:TPoint);
    //PROCEDURE setUnderCursor(CONST updateMarker,forHelpOrJump: boolean);
    //FUNCTION canRenameUnderCursor(OUT orignalId:string; OUT tokTyp:T_tokenType; OUT ref:T_searchTokenLocation; OUT mightBeUsedElsewhere:boolean):boolean;
    //PROCEDURE doRename(CONST ref:T_searchTokenLocation; CONST oldId,newId:string; CONST renameInOtherEditors:boolean=false);
    //FUNCTION defaultExtensionByLanguage:ansistring;
    //PROCEDURE updateContentAfterEditScript(CONST stringListLiteral:P_listLiteral);
    //FUNCTION resolveImport(CONST text:string):string;
    //PROCEDURE closeEditorQuietly;
    //FUNCTION isFile:boolean;
    //PROCEDURE initForNewFile;
    //FUNCTION changed:boolean;
    //FUNCTION saveFile(CONST fileName:string=''):string;
    //FUNCTION fileIsDeleted:boolean;
    //FUNCTION fileIsModifiedOnFileSystem:boolean;
end;
T_bookmarkIndex=0..9;

{$define includeInterface}
{$i runnermodel.inc}
{$i workspace.inc}
{$undef includeInterface}

//PROCEDURE setupUnit(CONST p_mainForm              :T_abstractMnhForm;
//                    CONST p_breakpointsImagesList :TImageList;
//                    CONST p_bookmarkImagesList    :TImageList;
//                    CONST outputHighlighter       :TSynMnhSyn;
//                    CONST languageMenuRoot        :TMenuItem;
//                    CONST p_EditKeyUp             :TKeyEvent;
//                    CONST p_EditMouseDown         :TMouseEvent;
//                    CONST p_EditProcessUserCommand:TProcessCommandEvent);
//FUNCTION hasEditor:boolean;
//FUNCTION getEditor:P_editorMeta;
//FUNCTION addEditorMetaForNewFile:P_editorMeta;
//FUNCTION addOrGetEditorMetaForFiles(CONST FileNames: array of string; CONST useCurrentPageAsFallback:boolean):P_editorMeta;
//FUNCTION openLocation(CONST location:T_searchTokenLocation):boolean;
//
//FUNCTION getHelpPopupText:string;
//FUNCTION getHelpLocation:T_searchTokenLocation;
//PROCEDURE updateEditorsByGuiStatus;
//PROCEDURE closeAllEditorsButCurrent;
//PROCEDURE closeAllUnmodifiedEditors;
//PROCEDURE checkForFileChanges;
//PROCEDURE finalizeEditorMeta;
//PROCEDURE saveWorkspace;
//FUNCTION currentlyOpenFiles:T_arrayOfString;
//FUNCTION workspaceFilename:string;
//
//FUNCTION getAllBreakpoints:T_searchTokenLocations;

TYPE F_safeCallback=FUNCTION(CONST path,name,ext:string):string;
VAR safeCallback:F_safeCallback;
    runnerModel:T_runnerModel;
    workspace  :T_workspace;
IMPLEMENTATION
USES variableTreeViews;
VAR underCursor:T_tokenInfo;
{$define includeImplementation}
{$i runnermodel.inc}
{$i workspace.inc}
{$undef includeImplementation}

PROCEDURE T_editorMeta.guessLanguage(CONST fallback: T_language);
  begin
    setLanguage(copy(extractFileExt(fileInfo.filePath),2,10),fallback);
  end;

PROCEDURE T_editorMeta.languageMenuItemClick(Sender: TObject);
  begin
    setLanguage(T_language(TMenuItem(Sender).Tag));
  end;

CONSTRUCTOR T_editorMeta.create(CONST mIdx:longint);
  begin
    metaIndex:=mIdx;
    tabsheet:=TTabSheet.create(workspace.inputPageControl);
    tabsheet.PageControl:=workspace.inputPageControl;
    createWithParent(tabsheet,workspace.bookmarkImagesList);
    latestAssistanceReponse:=nil;
    paintedWithStateHash:=0;
    paintedWithStateHash:=0;
    editor_.Gutter.MarksPart.width:=workspace.breakpointsImagesList.width+workspace.bookmarkImagesList.width+10;
    editor_.OnChange            :=@InputEditChange;
    editor_.OnKeyUp             :=@(workspace.keyUpForJumpToLocation);
    editor_.OnMouseDown         :=@(workspace.mouseDownForJumpToLocation);
    editor_.OnProcessCommand    :=@processUserCommand;
    editor_.OnSpecialLineMarkup :=@(runnerModel.InputEditSpecialLineMarkup);
    editor_.onPlaceBookmark     :=@onPlaceBookmark;
    editor_.onClearBookmark     :=@onClearBookmark;

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

CONSTRUCTOR T_editorMeta.create(CONST mIdx:longint; VAR stream: T_bufferedInputStreamWrapper);
  begin
    create(mIdx);
    //if not(stream.readDWord=editorMetaSerial) then begin //#0
    //  stream.logWrongTypeError;
    //  exit;
    //end;
    //with fileInfo do begin
    //  filePath     :=stream.readAnsiString; //#1
    //  isChanged    :=stream.readBoolean;    //#2
    //  strictlyReadOnly:=stream.readBoolean; //#3
    //  if isChanged then fileAccessAge:=stream.readDouble;  //#4
    //  ignoreDeleted:=false;
    //end;
    //editor.clearAll;
    //if fileInfo.isChanged
    //then begin
    //  editor.lines.clear;
    //  lineCount:=stream.readNaturalNumber; //#5
    //  for i:=1 to lineCount do editor.lines.append(stream.readAnsiString); //#6
    //end else setFile(fileInfo.filePath);
    //markCount:=stream.readNaturalNumber; //#7
    //for i:=1 to markCount do _add_breakpoint_(stream.readNaturalNumber); //#8
    //editor.CaretX:=stream.readNaturalNumber; //#9
    //editor.CaretY:=stream.readNaturalNumber; //#10
    //language_:=T_language(stream.readByte);  //#11
    //editor.modified:=fileInfo.isChanged;
    //updateSheetCaption;
  end;

PROCEDURE T_editorMeta.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  //VAR i:longint;
  //    editorLine:string;
  //    saveChanged:boolean;
  //    k:longint;
  begin
    //stream.writeDWord(editorMetaSerial); //#0
    //if fileInfo.filePath=''
    //then stream.writeAnsiString(               fileInfo.filePath )  //#1
    //else stream.writeAnsiString(expandFileName(fileInfo.filePath)); //#1
    //saveChanged:=changed;
    //stream.writeBoolean(saveChanged); //#2
    //stream.writeBoolean(strictlyReadOnly);   //#3
    //if saveChanged then begin
    //  stream.writeDouble(fileInfo.fileAccessAge);  //#4
    //  stream.writeNaturalNumber(editor.lines.count); //#5
    //  for editorLine in editor.lines do stream.writeAnsiString(editorLine); //#6
    //end;
    //k:=0;
    //for i:=0 to editor.Marks.count-1 do if not(editor.Marks[i].IsBookmark) then inc(k);
    //stream.writeNaturalNumber(k); //#7
    //for i:=0 to editor.Marks.count-1 do if not(editor.Marks[i].IsBookmark) then stream.writeNaturalNumber(editor.Marks[i].line); //#8
    //
    //stream.writeNaturalNumber(editor.CaretX); //#9
    //stream.writeNaturalNumber(editor.CaretY); //#10
    //stream.writeByte(ord(language));          //#11
  end;

FUNCTION T_editorMeta.markLocation(CONST line,column:longint):T_searchTokenLocation;
  begin
    result.fileName:=getPath;
    result.line:=line;
    result.column:=column;
  end;

PROCEDURE T_editorMeta._add_breakpoint_(CONST lineIndex: longint);
  VAR m:TSynEditMark;
  begin
    m:=TSynEditMark.create(editor);
    m.line:=lineIndex;
    m.ImageList:=workspace.breakpointsImagesList;
    m.ImageIndex:=0;
    m.visible:=true;
    editor.Marks.add(m);
  end;

PROCEDURE T_editorMeta.clearBookmark(markIndex: longint);
  VAR x:longint=0;
      y:longint=0;
  begin
    if editor_.GetBookMark(markIndex,x,y) then editor_.clearBookmark(markIndex);
  end;

PROCEDURE T_editorMeta.toggleBreakpoint;
  VAR i:longint;
      mark:TSynEditMark;
  begin
    for i:=0 to editor_.Marks.count-1 do if (editor_.Marks[i].line=editor_.CaretY) and not(editor_.Marks[i].IsBookmark) then begin
      mark:=editor.Marks[i];
      editor_.Marks.remove(editor.Marks[i]);
      mark.free;
      exit;
    end;
    _add_breakpoint_(editor_.CaretY);
  end;

PROCEDURE T_editorMeta.triggerCheck;
  begin
    postCodeAssistanceRequest(@self);
  end;

PROCEDURE T_editorMeta.updateAssistanceResponse(CONST response: P_codeAssistanceResponse);
  begin
    disposeCodeAssistanceResponse(latestAssistanceReponse);
    latestAssistanceReponse:=response;
    completionLogic.assignEditor(editor_,latestAssistanceReponse);
    if (paintedWithStateHash<>latestAssistanceReponse^.stateHash) then begin
      paintedWithStateHash:=latestAssistanceReponse^.stateHash;
      latestAssistanceReponse^.updateHighlightingData(highlighter.highlightingData);
      editor.highlighter:=highlighter;
      editor.Repaint;
    end;
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
      editor.modified:=true;
      fileInfo.fileAccessAge:=0;
    end;
    if not(datastore) then guessLanguage(LANG_TXT);
    updateSheetCaption;
  end;

FUNCTION T_editorMeta.updateSheetCaption: ansistring;
  begin
    if editor.modified then result:=' *'
                       else result:='';
    tabsheet.caption:=pseudoName(true)+result;
    result:=APP_TITLE+' '+pseudoName(false)+result{$ifdef debugMode}+' [debug]'{$endif};
  end;

PROCEDURE T_editorMeta.InputEditChange(Sender: TObject);
  PROCEDURE invalidateWordUnderCursor;
    begin
      underCursor.location.line:=-1;
    end;

  begin
    if language_=LANG_MNH then begin
      triggerCheck;
      invalidateWordUnderCursor;
    end;
    //mainForm.activeFileChanged(updateSheetCaption,language_=LANG_MNH,fileInfo.filePath='');
  end;

PROCEDURE T_editorMeta.processUserCommand(Sender: TObject; VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
begin
  if command=editCommandToggleBookmark then begin
    toggleBreakpoint;
    command:=ecNone;
  end else if (command>=ecGotoMarker0) and (command<=ecGotoMarker9)
  then workspace.openBookmarkLocation(command-ecGotoMarker0)
  else inherited processUserCommand(Sender,command,AChar,data);
end;

PROCEDURE T_editorMeta.onClearBookmark(Sender: TObject; VAR mark: TSynEditMark);
  begin
  end;

PROCEDURE T_editorMeta.onPlaceBookmark(Sender: TObject; VAR mark: TSynEditMark);
  VAR other:P_editorMeta;
  begin
    if not(Assigned(mark)) then exit;
    for other in workspace.metas do if (other<>@self) then other^.clearBookmark(mark.BookmarkNumber);
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
    inherited activate;
    for l in T_language do if Assigned(fileTypeMeta[l].menuItem) then begin
      fileTypeMeta[l].menuItem.OnClick:=@languageMenuItemClick;
      fileTypeMeta[l].menuItem.checked:=(l=language);
    end;
    try
      if language_=LANG_MNH then begin
        editor.highlighter:=highlighter;
        paintedWithStateHash:=0;
        triggerCheck;
        completionLogic.assignEditor(editor_,nil);
        runnerModel.firstCallAfterActivation:=true;
      end else begin
        editor.highlighter:=fileTypeMeta[language_].highlighter;
        disposeCodeAssistanceResponse(latestAssistanceReponse);
        completionLogic.assignEditor(editor_,nil);
      end;
      editor.Gutter.MarksPart.visible:=true;
      editor.readonly                :=runnerModel.areEditorsLocked;
      mainForm.onDebuggerEvent;
      mainForm.ActiveControl:=editor_;
    except end; //catch and ignore all exceptions
  end;

FUNCTION T_editorMeta.pseudoName(CONST short: boolean): ansistring;
  begin
    if fileInfo.filePath<>'' then begin
      if short then result:=extractFileName(fileInfo.filePath)
               else result:=fileInfo.filePath;
    end else result:='<new '+intToStr(metaIndex)+'>';
  end;

PROCEDURE T_editorMeta.pollAssistanceResult;
  VAR response:P_codeAssistanceResponse;
  begin
    if language_<>LANG_MNH then exit;
    response:=getLatestAssistanceResponse(@self);
    if response<>nil then updateAssistanceResponse(response);
  end;

//CONST workspaceSerialVersion=612461341;
//FUNCTION loadWorkspace:boolean;
//  VAR stream:T_bufferedInputStreamWrapper;
//      i:longint;
//  begin
//    stream.createToReadFromFile(workspaceFilename);
//    fileHistory.create(false);
//    folderHistory.create(true);
//    if not(stream.readDWord=workspaceSerialVersion) then begin
//      stream.destroy;
//      exit(false);
//    end;
//    if not(fileHistory  .loadFromStream(stream)) then exit(false);
//    if not(folderHistory.loadFromStream(stream)) then exit(false);
//    setLength(editorMetaData,stream.readNaturalNumber);
//    if not(stream.allOkay) then begin
//      setLength(editorMetaData,0);
//      stream.destroy;
//      exit(false);
//    end;
//    result:=true;
//    for i:=0 to length(editorMetaData)-1 do begin
//      new(editorMetaData[i],create(stream));
//      result:=result and stream.allOkay;
//    end;
//    result:=result and stream.allOkay;
//    //if not(result) then setLength(editorMetaData,validMetaCount) else begin
//    //  if length(filesToOpenInEditor)=0
//    //  then inputPageControl.activePageIndex:=stream.readLongint
//    //  else inputPageControl.activePageIndex:=addOrGetEditorMetaForFiles(filesToOpenInEditor,true);
//    //end;
//    //for i:=0 to 9 do begin
//    //  globalBookmarks[i].editorIndex:=stream.readInteger;
//    //  globalBookmarks[i].lineIndex  :=stream.readInteger;
//    //  globalBookmarks[i].columnIndex:=stream.readInteger;
//    //  if (globalBookmarks[i].editorIndex>=0) and (globalBookmarks[i].editorIndex<length(editorMetaData)) and
//    //     (globalBookmarks[i].lineIndex  >=0) then begin
//    //    editorMetaData[globalBookmarks[i].editorIndex]^.editor.SetBookMark(i,globalBookmarks[i].columnIndex,globalBookmarks[i].lineIndex);
//    //  end else globalBookmarks[i].editorIndex:=-1;
//    //end;
//    stream.destroy;
//  end;

//PROCEDURE saveWorkspace;
//  VAR stream:T_bufferedOutputStreamWrapper;
//      i,k:longint;
//      editorIndexAfterLoading:T_arrayOfLongint;
//  begin
//    stream.createToWriteToFile(workspaceFilename);
//    stream.writeDWord(workspaceSerialVersion);
//    fileHistory.saveToStream(stream);
//    folderHistory.saveToStream(stream);
//    stream.writeNaturalNumber(length(editorMetaData));
//    setLength(editorIndexAfterLoading,length(editorMetaData));
//    k:=0;
//    for i:=0 to length(editorMetaData)-1 do begin
//      editorIndexAfterLoading[i]:=k; inc(k);
//      editorMetaData[i]^.saveToStream(stream);
//    end;
//    //for i:=0 to 9 do if globalBookmarks[i].editorIndex<0 then begin
//    //  stream.writeInteger(-1);
//    //  stream.writeInteger(-1);
//    //  stream.writeInteger(-1);
//    //end else begin
//    //  stream.writeInteger(editorIndexAfterLoading[globalBookmarks[i].editorIndex]);
//    //  stream.writeInteger(globalBookmarks[i].lineIndex  );
//    //  stream.writeInteger(globalBookmarks[i].columnIndex);
//    //end;
//    stream.destroy;
//  end;

//PROCEDURE initNewWorkspace;
//  VAR i:longint;
//  begin
//    for i:=0 to length(editorMetaData)-1 do dispose(editorMetaData[i],destroy);
//    setLength(fileHistory.items,0);
//    setLength(folderHistory.items,0);
//  end;
//
//PROCEDURE setupUnit(CONST p_mainForm              :T_abstractMnhForm;
//                    CONST p_breakpointsImagesList :TImageList;
//                    CONST p_bookmarkImagesList    :TImageList;
//                    CONST outputHighlighter       :TSynMnhSyn;
//                    CONST languageMenuRoot        :TMenuItem;
//                    CONST p_EditKeyUp             :TKeyEvent;
//                    CONST p_EditMouseDown         :TMouseEvent;
//                    CONST p_EditProcessUserCommand:TProcessCommandEvent);
//
//  begin
//    setupEditorMetaBase(outputHighlighter,languageMenuRoot);
//
//    mainForm              :=p_mainForm              ;
//    breakpointsImagesList :=p_breakpointsImagesList ;
//    bookmarkImagesList    :=p_bookmarkImagesList    ;
//    EditMouseDown         :=p_EditMouseDown         ;
//    EditProcessUserCommand:=p_EditProcessUserCommand;
//
//    if not(loadWorkspace) then initNewWorkspace;
//  end;

//PROCEDURE gotoMarker(markerIndex:longint);
//  VAR currentEdit:P_editorMeta;
//      i:longint;
//  begin
//    if (markerIndex<0) or (markerIndex>=length(globalBookmarks)) then exit;
//    if globalBookmarks[markerIndex].editorIndex<0 then exit;
//    currentEdit:=getEditor;
//    if currentEdit^.index=globalBookmarks[markerIndex].editorIndex then exit;
//    for i:=0 to length(editorMetaData)-1 do if (editorMetaData[i]^.enabled) and (editorMetaData[i]^.index=globalBookmarks[markerIndex].editorIndex) then begin
//      inputPageControl.activePageIndex:=i;
//      editorMetaData[i]^.editor.CaretY:=globalBookmarks[markerIndex].lineIndex;
//      editorMetaData[i]^.editor.CaretX:=globalBookmarks[markerIndex].columnIndex;
//      mainForm.ActiveControl:=editorMetaData[i]^.editor;
//    end;
//  end;

//function T_editorMeta.saveAsWithDialog: boolean;
//  VAR path,name,ext:string;
//  begin
//    if isFile then begin
//      path:=ExtractFileDir(fileInfo.filePath);
//      name:=ExtractFileNameOnly(fileInfo.filePath);
//      ext :=extractFileExt(fileInfo.filePath);
//    end else begin
//      path:=GetCurrentDir;
//      name:='';
//      if length(fileTypeMeta[language].extensions)>0
//      then ext:='.'+lowercase(fileTypeMeta[language].extensions[0])
//      else ext:='';
//    end;
//    name:=safeCallback(path,name,ext);
//    if name<>'' then begin
//      mainForm.activeFileChanged(saveFile(name),language_=LANG_MNH,fileInfo.filePath='');
//      result:=true;
//    end else result:=false;
//  end;

//function T_editorMeta.saveWithDialog: boolean;
//  begin
//    if isFile then begin
//      mainForm.activeFileChanged(saveFile(),language_=LANG_MNH,fileInfo.filePath='');
//      result:=true;
//    end else result:=saveAsWithDialog;
//  end;

//procedure T_editorMeta.closeEditorQuietly;
//  begin
//    editor.clearAll;
//    with fileInfo do begin
//      filePath:='';
//      isChanged:=false;
//      ignoreDeleted:=false;
//    end;
//    editor.modified:=false;
//    strictlyReadOnly:=false;
//  end;

//procedure T_editorMeta.closeEditorWithDialogs;
//  VAR mr:longint;
//  begin
//    if changed then begin
//      mr:=closeDialogForm.showOnClose(pseudoName(true));
//      if mr=mrOk then if not(saveWithDialog) then exit;
//      if mr=mrCancel then exit;
//    end;
//    if isFile then begin
//      fileHistory.fileClosed(fileInfo.filePath);
//      folderHistory.fileClosed(ExtractFileDir(fileInfo.filePath));
//    end;
//    closeEditorQuietly;
//  end;

//procedure T_editorMeta.reloadFile(const fileName: string);
//  begin
//    if (fileInfo.filePath=SysToUTF8(fileName)) and (fileExists(fileName)) then begin
//      editor.lines.loadFromFile(fileInfo.filePath);
//      fileAge(fileInfo.filePath,fileInfo.fileAccessAge);
//      editor.modified:=false;
//      fileInfo.isChanged:=false;
//      mainForm.activeFileChanged('Unimplemented sheet caption',language_=LANG_MNH,fileInfo.filePath='');
//      if language_=LANG_MNH then triggerCheck;
//    end;
//  end;

//function T_editorMeta.caretInMainFormCoordinates: TPoint;
//  begin
//    result.x:=editor.CaretXPix;
//    result.y:=editor.CaretYPix+editor.LineHeight;
//    result:=editor.ClientToParent(result,mainForm);
//  end;

//procedure T_editorMeta.setUnderCursor(const updateMarker,
//  forHelpOrJump: boolean; const caret: TPoint);
//  VAR m:P_editorMeta;
//      wordUnderCursor:string;
//  begin
//    if (language_<>LANG_MNH) or not(updateMarker or forHelpOrJump) then exit;
//    wordUnderCursor:=editor.GetWordAtRowCol(caret);
//    if updateMarker then begin
//      for m in editorMetaData do m^.setMarkedWord(wordUnderCursor);
//      editor.Repaint;
//    end;
//    if forHelpOrJump and (latestAssistanceReponse<>nil) then with editor do
//      latestAssistanceReponse^.explainIdentifier(lines[caret.y-1],caret.y,caret.x,underCursor);
//  end;

//procedure T_editorMeta.setUnderCursor(const updateMarker, forHelpOrJump: boolean
//  );
//  begin
//    setUnderCursor(updateMarker,forHelpOrJump,editor.CaretXY);
//  end;

//function T_editorMeta.canRenameUnderCursor(out orignalId: string; out
//  tokTyp: T_tokenType; out ref: T_searchTokenLocation; out
//  mightBeUsedElsewhere: boolean): boolean;
//  begin
//    if language<>LANG_MNH then exit(false);
//    setUnderCursor(false,true);
//    result   :=underCursor.canRename;
//    orignalId:=underCursor.idWithoutIsPrefix;
//    tokTyp   :=underCursor.tokenType;
//    if tokTyp in [tt_each,tt_parallelEach] then tokTyp:=tt_eachParameter;
//    ref      :=underCursor.location;
//    mightBeUsedElsewhere:=underCursor.mightBeUsedInOtherPackages and (fileInfo.filePath<>'');
//  end;

//procedure T_editorMeta.doRename(const ref: T_searchTokenLocation; const oldId,
//  newId: string; const renameInOtherEditors: boolean);
//  VAR meta:P_editorMeta;
//      lineIndex:longint;
//      lineTxt:string;
//      recycler:T_recycler;
//  PROCEDURE updateLine;
//    VAR lineStart,lineEnd:TPoint;
//    begin
//      lineStart.y:=lineIndex+1; lineStart.x:=0;
//      lineEnd  .y:=lineIndex+1; lineEnd  .x:=length(editor.lines[lineIndex])+1;
//      editor.SetTextBetweenPoints(lineStart,lineEnd,lineTxt);
//    end;
//
//  begin
//    if (language<>LANG_MNH) then exit;
//    if renameInOtherEditors then saveFile();
//    recycler.initRecycler;
//    updateAssistanceResponse(doCodeAssistanceSynchronously(@self,recycler));
//    recycler.cleanup;
//
//    editor.BeginUpdate(true);
//    with editor do for lineIndex:=0 to lines.count-1 do begin
//      lineTxt:=lines[lineIndex];
//      if latestAssistanceReponse^.renameIdentifierInLine(ref,oldId,newId,lineTxt,lineIndex+1) then updateLine;
//    end;
//    editor.EndUpdate;
//
//    if renameInOtherEditors then for meta in editorMetaData do if meta<>@self then meta^.doRename(ref,oldId,newId);
//  end;

//function T_editorMeta.isFile: boolean;
//  begin
//    result:=fileInfo.filePath<>'';
//  end;

//function T_editorMeta.defaultExtensionByLanguage: ansistring;
//  begin
//    result:=fileTypeMeta[language_].extensions[0];
//  end;

//function T_editorMeta.changed: boolean;
//  begin
//    result:=fileInfo.isChanged or editor.modified;
//  end;

//function T_editorMeta.saveFile(const fileName: string): string;
//  VAR arr:T_arrayOfString;
//      i:longint;
//      previousName:string;
//      lineEndingSetting:byte;
//  begin
//    previousName:=fileInfo.filePath;
//    if fileName<>'' then fileInfo.filePath:=expandFileName(fileName);
//    if (previousName<>'') and (previousName<>fileInfo.filePath) then begin
//      fileHistory.fileClosed(previousName);
//      folderHistory.fileClosed(ExtractFileDir(previousName));
//    end;
//    if previousName<>fileInfo.filePath
//    then lineEndingSetting:=settings.newFileLineEnding
//    else lineEndingSetting:=settings.overwriteLineEnding;
//    setLength(arr,editor.lines.count);
//    for i:=0 to length(arr)-1 do arr[i]:=editor.lines[i];
//    with fileInfo do begin
//      writeFileLines(filePath,arr,LINE_ENDING[lineEndingSetting],false);
//      fileAge(filePath,fileAccessAge);
//      isChanged:=false;
//      editor.modified:=false;
//      editor.MarkTextAsSaved;
//      if (filePath=utilityScriptFileName) then runEvaluator.ensureEditScripts();
//    end;
//    //result:=updateSheetCaption;
//  end;

//function T_editorMeta.fileIsDeleted: boolean;
//  begin
//    result:=isFile and not(fileExists(fileInfo.filePath));
//  end;

//function T_editorMeta.fileIsModifiedOnFileSystem: boolean;
//  VAR currentFileAge:double;
//  begin
//    if not(isFile) or changed then exit(false);
//    fileAge(fileInfo.filePath,currentFileAge);
//    result:=currentFileAge<>fileInfo.fileAccessAge;
//  end;

//procedure T_editorMeta.updateContentAfterEditScript(
//  const stringListLiteral: P_listLiteral);
//  VAR concatenatedText:ansistring='';
//      i:longint;
//  begin
//    if stringListLiteral^.literalType<>lt_stringList then exit;
//    for i:=0 to stringListLiteral^.size-1 do begin
//      if i>0 then concatenatedText:=concatenatedText+LineEnding;
//      concatenatedText:=concatenatedText+P_stringLiteral(stringListLiteral^.value[i])^.value;
//    end;
//    editor.BeginUndoBlock;
//    editor.SelectAll;
//    editor.SelText:=concatenatedText;
//    editor.EndUndoBlock;
//  end;

//function T_editorMeta.resolveImport(const text: string): string;
//  begin
//    if latestAssistanceReponse=nil then result:='' else result:=latestAssistanceReponse^.resolveImport(text);
//  end;

//procedure T_editorMeta.exportToHtml;
//  VAR SynExporterHTML: TSynExporterHTML;
//      name:string;
//  begin
//    name:=safeCallback(GetCurrentDir,'','.html');
//    if name='' then exit;
//    SynExporterHTML:=TSynExporterHTML.create(nil);
//    SynExporterHTML.title:=pseudoName();
//    SynExporterHTML.highlighter:=editor.highlighter;
//    SynExporterHTML.ExportAll(editor.lines);
//    SynExporterHTML.saveToFile(name);
//    SynExporterHTML.free;
//  end;

//==================================================================
//FUNCTION hasEditor:boolean;
//  begin
//    result:=length(editorMetaData)>0;
//  end;

//FUNCTION allPseudoNames:T_arrayOfString;
//  VAR m:P_editorMeta;
//  begin
//    setLength(result,0);
//    for m in editorMetaData do if m^.enabled then append(result,m^.pseudoName);
//  end;

//FUNCTION getMeta(CONST nameOrPseudoName:string):P_editorMeta;
//  VAR m:P_editorMeta;
//  begin
//    result:=nil;
//    for m in editorMetaData do if (m^.pseudoName()=nameOrPseudoName) then exit(m);
//  end;

//FUNCTION getHelpPopupText:string;
//  begin
//    result:=underCursor.infoText;
//  end;
//
//FUNCTION getHelpLocation:T_searchTokenLocation;
//  begin
//    {$ifdef debugMode} writeln(stdErr,'        DEBUG: getHelpLocation filename="',underCursor.location.fileName,'"; line=',underCursor.location.line,'; column=',underCursor.location.column); {$endif}
//    result:=underCursor.location;
//  end;

//PROCEDURE closeAllEditorsButCurrent;
//  VAR m:P_editorMeta;
//  begin
//    if not(hasEditor) then exit;
//    for m in editorMetaData do if (m^.tabsheet<>inputPageControl.activePage) then m^.closeEditorWithDialogs;
//  end;

//PROCEDURE closeAllUnmodifiedEditors;
//  VAR m:P_editorMeta;
//  begin
//    for m in editorMetaData do if not(m^.changed) then m^.closeEditorWithDialogs;
//  end;

//VAR doNotCheckFileBefore:double;
//PROCEDURE checkForFileChanges;
//  VAR m:P_editorMeta;
//      modalRes:longint;
//  begin
//    if now<doNotCheckFileBefore then exit;
//    doNotCheckFileBefore:=now+1;
//    for m in editorMetaData do with m^ do
//    if fileIsDeleted and not(fileInfo.ignoreDeleted) then begin
//      modalRes:=closeDialogForm.showOnDeleted(fileInfo.filePath);
//      if modalRes=mrOk then closeEditorQuietly;
//      if modalRes=mrClose then begin if not(saveWithDialog) then fileInfo.isChanged:=true; end else begin
//        fileInfo.ignoreDeleted:=true;
//        fileInfo.isChanged:=true;
//        updateSheetCaption;
//      end;
//      continue;
//    end else if fileIsModifiedOnFileSystem then begin
//      modalRes:=closeDialogForm.showOnOutOfSync(fileInfo.filePath);
//      if modalRes=mrOk then reloadFile(fileInfo.filePath);
//      if modalRes=mrClose then begin if not(saveWithDialog) then fileInfo.isChanged:=true; end else
//      fileInfo.isChanged:=true;
//    end;
//    doNotCheckFileBefore:=now+ONE_SECOND;
//  end;

//PROCEDURE finalizeEditorMeta;
//  VAR i:longint;
//  begin
//    finalizeCodeAssistance;
//    //if outlineModel<>nil then begin
//    //  dispose(outlineModel,destroy);
//    //  outlineModel:=nil;
//    //end;
//    for i:=0 to length(editorMetaData)-1 do dispose(editorMetaData[i],destroy);
//    setLength(editorMetaData,0);
//  end;

end.
