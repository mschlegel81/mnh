UNIT editorMeta;
INTERFACE
USES  //basic classes
  Classes, sysutils, LazUTF8, LCLType, types, LazFileUtils,FileUtil,
  //my utilities:
  serializationUtil,
  myGenerics,
  myStringUtil,
  //GUI: LCL components
  Controls, Graphics, Dialogs, Menus, ComCtrls,
  //GUI: SynEdit
  SynEdit, SynEditMiscClasses, SynEditMarks, SynEditKeyCmds,
  closeDialog,
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
  guiOutAdapters,
  datastores,
  editorMetaBase,
  codeAssistance,
  ideLayoutUtil,
  editScripts,
  synOutAdapter,
  menuUtil,
  mnh_messages;

TYPE
P_editorMeta=^T_editorMeta;
P_editorMetaProxy=^T_editorMetaProxy;
T_editorMetaProxy=object(T_codeProvider)
  private
    fixated:boolean;
    lines:T_arrayOfString;
    pseudo:boolean;
    filePath: ansistring;
  public
    CONSTRUCTOR create(CONST path:ansistring);
    DESTRUCTOR destroy;                 virtual;
    FUNCTION getLines: T_arrayOfString; virtual;
    FUNCTION getPath: ansistring;       virtual;
    FUNCTION isPseudoFile:boolean;      virtual;
    FUNCTION fixate:P_editorMetaProxy;
end;

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
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper);

    //Bookmark related
    FUNCTION markLocation(CONST line,column:longint):T_searchTokenLocation;
    PROCEDURE _add_breakpoint_or_bookmark_(CONST lineIndex:longint; CONST columnIndex:longint=1; CONST bookmarkIndex:byte=10);
    PROCEDURE clearBookmark(markIndex:longint);
    PROCEDURE deleteBreakpointIfExistent(CONST lineIndex:longint);
    PROCEDURE toggleBreakpoint;

    //Assistant related
    PROCEDURE triggerCheck;
    PROCEDURE updateAssistanceResponse(CONST response:P_codeAssistanceResponse);
    FUNCTION canRenameUnderCursor(OUT orignalId:string; OUT tokTyp:T_tokenType; OUT ref:T_searchTokenLocation; OUT mightBeUsedElsewhere:boolean):boolean;
  public
    FUNCTION setUnderCursor(CONST updateMarker,forHelpOrJump: boolean):boolean;
  private
    FUNCTION setUnderCursor(CONST updateMarker,forHelpOrJump: boolean; CONST caret:TPoint):boolean;
    FUNCTION doRename(CONST ref:T_searchTokenLocation; CONST oldId,newId:string; CONST renameInOtherEditors:boolean=false):boolean;

    PROCEDURE setFile(CONST fileName:string);
    PROCEDURE saveFile(CONST fileName:string='');
    PROCEDURE updateSheetCaption;
  public
    PROCEDURE activate;

    //Editor events
    PROCEDURE InputEditChange(Sender: TObject);
    PROCEDURE processUserCommand(Sender: TObject; VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer); virtual;
    PROCEDURE onClearBookmark(Sender: TObject; VAR mark: TSynEditMark);
    PROCEDURE onPlaceBookmark(Sender: TObject; VAR mark: TSynEditMark);
    PROCEDURE editorMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    //Inherited overrides
    DESTRUCTOR destroy; virtual;
    FUNCTION getPath:ansistring; virtual;
    FUNCTION getFolder:ansistring;
    FUNCTION isPseudoFile: boolean; virtual;

    //Misc. queries
    FUNCTION pseudoName(CONST short:boolean=false):ansistring;
    PROCEDURE reloadFile();

    //Externally triggered actions
    PROCEDURE pollAssistanceResult;

    FUNCTION getCodeAssistanceDataRereferenced:P_codeAssistanceResponse;
    PROCEDURE updateContentAfterEditScript(CONST stringListLiteral:P_listLiteral);
end;
T_bookmarkIndex=0..9;

{$define includeInterface}
{$i runnermodel.inc}
{$i fileHistory.inc}
{$i workspace.inc}
{$undef includeInterface}

FUNCTION getHelpText(OUT helpLink:string):string;
TYPE F_safeCallback=FUNCTION(CONST path,name,ext:string):string;
VAR safeCallback:F_safeCallback=nil;
    runnerModel:T_runnerModel;
    workspace  :T_workspace;
IMPLEMENTATION
USES renameDialog,
     recyclers,
     packages;
VAR underCursor:T_tokenInfo;
CONSTRUCTOR T_editorMetaProxy.create(CONST path: ansistring);
  begin
    filePath:=path;
    fixated:=false;
    setLength(lines,0);
  end;

DESTRUCTOR T_editorMetaProxy.destroy;
  begin
    setLength(lines,0);
  end;

FUNCTION T_editorMetaProxy.getLines: T_arrayOfString;
  VAR meta:P_editorMeta;
      accessed:boolean;
  begin
    if fixated then exit(lines);
    enterCriticalSection(workspace.workspaceCs);
    try
      meta:=workspace.getExistingEditorForPath(filePath);
      if meta=nil then begin
        result:=fileLines(filePath,accessed);
        if not(accessed) then result:=C_EMPTY_STRING_ARRAY;
      end else result:=meta^.getLines;
    finally
      leaveCriticalSection(workspace.workspaceCs);
    end;
  end;

FUNCTION T_editorMetaProxy.getPath: ansistring;
  begin
    result:=filePath;
  end;

FUNCTION T_editorMetaProxy.isPseudoFile: boolean;
  VAR meta:P_editorMeta;
  begin
    if fixated then exit(pseudo);
    enterCriticalSection(workspace.workspaceCs);
    try
      meta:=workspace.getExistingEditorForPath(filePath);
      if meta=nil then begin
        result:=not(fileExists(filePath))
      end else begin
        result:=meta^.isPseudoFile;
      end;
    finally
      leaveCriticalSection(workspace.workspaceCs);
    end;
  end;

FUNCTION T_editorMetaProxy.fixate: P_editorMetaProxy;
  begin
    if not(fixated) then begin
      lines:=getLines;
      pseudo:=isPseudoFile;
      fixated:=true;
    end;
    result:=@self;
  end;

{$define includeImplementation}
{$i runnermodel.inc}
{$i fileHistory.inc}
{$i workspace.inc}
{$undef includeImplementation}

PROCEDURE T_editorMeta.guessLanguage(CONST fallback: T_language);
  begin
    setLanguage(copy(extractFileExt(fileInfo.filePath),2,10),fallback);
  end;

PROCEDURE T_editorMeta.languageMenuItemClick(Sender: TObject);
  begin
    setLanguage(T_language(TMenuItem(Sender).Tag));
    activate;
  end;

CONSTRUCTOR T_editorMeta.create(CONST mIdx: longint);
  begin
    metaIndex:=mIdx;
    tabsheet:=TTabSheet.create(workspace.inputPageControl);
    tabsheet.PageControl:=workspace.inputPageControl;
    createWithParent(tabsheet,workspace.bookmarkImagesList);
    latestAssistanceReponse:=nil;
    paintedWithStateHash:=0;
    editor_.Gutter.MarksPart.width:=workspace.breakpointsImagesList.width+workspace.bookmarkImagesList.width+10;
    editor_.OnChange            :=@InputEditChange;
    editor_.OnMouseDown         :=@editorMouseDown;
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

CONST editorMetaSerial=2344515;
FUNCTION T_editorMeta.loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean;
  VAR lineCount:longint=0;
      markCount:longint=0;
      i:longint;
      line,column:longint;
  begin
    if not(stream.readDWord=editorMetaSerial) then begin //#0
      stream.logWrongTypeError;
      exit(false);
    end;

    metaIndex:=stream.readLongint;
    with fileInfo do begin
      filePath     :=stream.readAnsiString; //#1
      isChanged    :=stream.readBoolean;    //#2
      strictlyReadOnly:=stream.readBoolean; //#3
      if isChanged then fileAccessAge:=stream.readDouble;  //#4
      ignoreDeleted:=false;
    end;
    {$ifdef debugMode}
    writeln('Read file info. ok=',stream.allOkay);
    {$endif}

    editor.clearAll;
    if fileInfo.isChanged
    then begin
      editor.lines.clear;
      lineCount:=stream.readNaturalNumber; //#5
      for i:=1 to lineCount do editor.lines.append(stream.readAnsiString); //#6
    end else setFile(fileInfo.filePath);
    {$ifdef debugMode}
    writeln('Read file lines. ok=',stream.allOkay);
    {$endif}

    markCount:=stream.readNaturalNumber; //#7
    for i:=1 to markCount do begin
      line:=stream.readNaturalNumber;    //#8a
      column:=stream.readNaturalNumber;  //#8b
      _add_breakpoint_or_bookmark_(line,column,stream.readByte([0..10])); //#8c
    end;
    {$ifdef debugMode}
    writeln('Read marks. ok=',stream.allOkay);
    {$endif}

    editor.CaretX:=stream.readNaturalNumber; //#9
    editor.CaretY:=stream.readNaturalNumber; //#10
    language_:=T_language(stream.readByte([byte(low(T_language))..byte(high(T_language))]));  //#11
    {$ifdef debugMode}
    writeln('Read language. ok=',stream.allOkay);
    {$endif}

    result:=stream.allOkay;
    if result then begin
      editor.modified:=fileInfo.isChanged;
      updateSheetCaption;
      {$ifdef debugMode}
      writeln('Succressfully restored editor: ',pseudoName());
      {$endif}
    end
    {$ifdef debugMode}
    else writeln('Failed to restore editor: ',pseudoName())
    {$endif};
  end;

PROCEDURE T_editorMeta.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i:longint;
      editorLine:string;
      saveChanged:boolean;
      mark:TSynEditMark;
  begin
    stream.writeDWord(editorMetaSerial); //#0
    stream.writeLongint(metaIndex);
    if fileInfo.filePath=''
    then stream.writeAnsiString(               fileInfo.filePath )  //#1
    else stream.writeAnsiString(expandFileName(fileInfo.filePath)); //#1
    saveChanged:=editor.modified or fileInfo.isChanged;
    stream.writeBoolean(saveChanged); //#2
    stream.writeBoolean(strictlyReadOnly);   //#3
    if saveChanged then begin
      stream.writeDouble(fileInfo.fileAccessAge);  //#4
      stream.writeNaturalNumber(editor.lines.count); //#5
      for editorLine in editor.lines do stream.writeAnsiString(editorLine); //#6
    end;

    stream.writeNaturalNumber(editor.Marks.count); //#7
    for i:=0 to editor.Marks.count-1 do begin
      mark:=editor.Marks[i];
      stream.writeNaturalNumber(mark.line);   //#8a
      stream.writeNaturalNumber(mark.column); //#8b
      if mark.IsBookmark
      then stream.writeByte(mark.BookmarkNumber) //#8c
      else stream.writeByte(10);
    end;

    stream.writeNaturalNumber(editor.CaretX); //#9
    stream.writeNaturalNumber(editor.CaretY); //#10
    stream.writeByte(ord(language));          //#11
  end;

FUNCTION T_editorMeta.markLocation(CONST line,column:longint):T_searchTokenLocation;
  begin
    result.fileName:=getPath;
    result.line:=line;
    result.column:=column;
  end;

PROCEDURE T_editorMeta._add_breakpoint_or_bookmark_(CONST lineIndex: longint; CONST columnIndex:longint=1; CONST bookmarkIndex:byte=10);
  VAR m:TSynEditMark;
  begin
    m:=TSynEditMark.create(editor);
    m.line:=lineIndex;
    m.column:=columnIndex;
    if bookmarkIndex>=10 then begin
      m.ImageList:=workspace.breakpointsImagesList;
      m.ImageIndex:=0;
    end else begin
      m.ImageList:=workspace.bookmarkImagesList;
      m.ImageIndex:=bookmarkIndex;
      m.BookmarkNumber:=bookmarkIndex;
    end;
    m.visible:=true;
    editor.Marks.add(m);
  end;

PROCEDURE T_editorMeta.clearBookmark(markIndex: longint);
  VAR x:longint=0;
      y:longint=0;
  begin
    if editor_.GetBookMark(markIndex,x,y) then editor_.clearBookmark(markIndex);
  end;

PROCEDURE T_editorMeta.deleteBreakpointIfExistent(CONST lineIndex:longint);
  VAR i:longint;
      mark:TSynEditMark;
  begin
    for i:=0 to editor_.Marks.count-1 do if (editor_.Marks[i].line=lineIndex) and not(editor_.Marks[i].IsBookmark) then begin
      mark:=editor.Marks[i];
      editor_.Marks.remove(editor.Marks[i]);
      mark.free;
      exit;
    end;
  end;

PROCEDURE T_editorMeta.toggleBreakpoint;
  VAR i:longint;
      mark:TSynEditMark;
  begin
    for i:=0 to editor_.Marks.count-1 do if (editor_.Marks[i].line=editor_.CaretY) and not(editor_.Marks[i].IsBookmark) then begin
      mark:=editor.Marks[i];
      editor_.Marks.remove(editor.Marks[i]);
      mark.free;
      runnerModel.evaluation.stepper^.setBreakpoints(workspace.getAllBreakpoints);
      exit;
    end;
    _add_breakpoint_or_bookmark_(editor_.CaretY);
    runnerModel.evaluation.stepper^.setBreakpoints(workspace.getAllBreakpoints);
  end;

PROCEDURE T_editorMeta.triggerCheck;
  FUNCTION optionalAdditionals:T_arrayOfString;
    begin
      if (language_=LANG_MNH) and workspace.checkUsingScripts and not(isPseudoFile)
      then result:=workspace.fileHistory.findScriptsUsing(fileInfo.filePath)
      else result:=C_EMPTY_STRING_ARRAY;
    end;

  begin
    postCodeAssistanceRequest(newFixatedFileProxy(pseudoName()),optionalAdditionals);
  end;

PROCEDURE T_editorMeta.updateAssistanceResponse(CONST response: P_codeAssistanceResponse);
  begin
    if response=nil then exit;
    disposeCodeAssistanceResponse(latestAssistanceReponse);
    latestAssistanceReponse:=response;
    completionLogic.assignEditor(editor_,latestAssistanceReponse);
    if (paintedWithStateHash<>latestAssistanceReponse^.stateHash) then begin
      paintedWithStateHash:=latestAssistanceReponse^.stateHash;
      latestAssistanceReponse^.updateHighlightingData(highlighter.highlightingData);
      editor.highlighter:=highlighter;
      editor.Repaint;
    end;
    if not(isPseudoFile) then workspace.fileHistory.updateScriptUsage(fileInfo.filePath,latestAssistanceReponse^.package^.usedAndExtendedPackages);
  end;

FUNCTION T_editorMeta.canRenameUnderCursor(OUT orignalId: string;
                                           OUT tokTyp: T_tokenType;
                                           OUT ref: T_searchTokenLocation;
                                           OUT mightBeUsedElsewhere: boolean): boolean;
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

FUNCTION T_editorMeta.setUnderCursor(CONST updateMarker, forHelpOrJump: boolean):boolean;
  begin
    result:=setUnderCursor(updateMarker,forHelpOrJump,editor.CaretXY);
  end;

FUNCTION T_editorMeta.setUnderCursor(CONST updateMarker, forHelpOrJump: boolean; CONST caret: TPoint):boolean;
  VAR m:P_editorMeta;
      wordUnderCursor:string;
  begin
    if (language_<>LANG_MNH) or not(updateMarker or forHelpOrJump) then exit(false);
    wordUnderCursor:=editor.GetWordAtRowCol(caret);
    if updateMarker then begin
      for m in workspace.metas do m^.setMarkedWord(wordUnderCursor);
      editor.Repaint;
    end;
    if forHelpOrJump and (latestAssistanceReponse<>nil)
    then with editor do result:=latestAssistanceReponse^.explainIdentifier(lines[caret.y-1],caret.y,caret.x,underCursor)
    else result:=false;
  end;

FUNCTION T_editorMeta.doRename(CONST ref: T_searchTokenLocation; CONST oldId, newId: string; CONST renameInOtherEditors: boolean):boolean;
  VAR meta:P_editorMeta;
      lineIndex:longint;
      lineTxt:string;
      recycler:T_recycler;
      fileName:string;

  PROCEDURE updateLine;
    VAR lineStart,lineEnd:TPoint;
    begin
      lineStart.y:=lineIndex+1; lineStart.x:=0;
      lineEnd  .y:=lineIndex+1; lineEnd  .x:=length(editor.lines[lineIndex])+1;
      editor.SetTextBetweenPoints(lineStart,lineEnd,lineTxt);
    end;

  VAR tempAssistanceResponse:P_codeAssistanceResponse;
  FUNCTION usedInLines:T_arrayOfLongint;
    VAR location:T_searchTokenLocation;
        localName:string;
    begin
      localName:=pseudoName();
      if ref.fileName=localName
      then result:=ref.line-1
      else result:=C_EMPTY_LONGINT_ARRAY;
      for location in tempAssistanceResponse^.findUsagesOf(ref) do if location.fileName=localName then appendIfNew(result,location.line-1);
    end;

  begin
    result:=false;
    if (language<>LANG_MNH) then exit(false);
    if renameInOtherEditors then begin
      for meta in workspace.metas do if meta<>@self then meta^.doRename(ref,oldId,newId);
      if not(isPseudoFile) then for fileName in workspace.fileHistory.findScriptsUsing(fileInfo.filePath) do begin
        if not(workspace.hasEditorForFile(fileName)) then begin
          meta:=workspace.addOrGetEditorMetaForFiles(fileName,false);
          if meta<>nil then begin
            if not(meta^.doRename(ref,oldId,newId,false)) then workspace.closeQuietly(meta);
          end;
        end;
      end;
    end;

    recycler.initRecycler;
    tempAssistanceResponse:=doCodeAssistanceSynchronously(newFixatedFileProxy(pseudoName()),C_EMPTY_STRING_ARRAY,recycler);
    recycler.cleanup;

    editor.BeginUpdate(true);
    with editor do for lineIndex:=0 to editor.lines.count-1 do begin
      lineTxt:=lines[lineIndex];
      if tempAssistanceResponse^.renameIdentifierInLine(ref,oldId,newId,lineTxt,lineIndex+1) then updateLine;
      result:=true;
    end;
    editor.EndUpdate;
    disposeCodeAssistanceResponse(tempAssistanceResponse);
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

PROCEDURE T_editorMeta.saveFile(CONST fileName:string='');
  VAR arr:T_arrayOfString;
      i:longint;
      previousName:string;
      lineEndingSetting:byte;
  begin
    previousName:=fileInfo.filePath;
    if fileName<>'' then fileInfo.filePath:=expandFileName(fileName);
    if (previousName<>'') and (previousName<>fileInfo.filePath) then workspace.fileHistory.fileClosed(previousName);
    if previousName<>fileInfo.filePath
    then lineEndingSetting:=workspace.newFileLineEnding
    else lineEndingSetting:=workspace.overwriteLineEnding;
    setLength(arr,editor.lines.count);
    for i:=0 to length(arr)-1 do arr[i]:=editor.lines[i];
    with fileInfo do begin
      writeFileLines(filePath,arr,LINE_ENDING[lineEndingSetting],false);
      fileAge(filePath,fileAccessAge);
      isChanged:=false;
      editor.modified:=false;
      editor.MarkTextAsSaved;
        if SameFileName(filePath,utilityScriptFileName) then runnerModel.ensureEditScripts();
      updateSheetCaption;
    end;
  end;

PROCEDURE T_editorMeta.updateSheetCaption;
  begin
    tabsheet.caption:=pseudoName(true)+BoolToStr(editor.modified,' *','');
  end;

PROCEDURE T_editorMeta.InputEditChange(Sender: TObject);
  begin
    if language_=LANG_MNH then triggerCheck;
    updateSheetCaption;
  end;

PROCEDURE T_editorMeta.processUserCommand(Sender: TObject; VAR command: TSynEditorCommand; VAR AChar: TUTF8Char; data: pointer);
  PROCEDURE cycleEditors(CONST tabForward:boolean);
    begin
      with workspace.inputPageControl do if tabForward
      then activePageIndex:=(activePageIndex+          1) mod PageCount
      else activePageIndex:=(activePageIndex+PageCount-1) mod PageCount;
      workspace.currentEditor^.activate;
    end;

  begin
    if command=editCommandPageRight         then begin command:=ecNone; cycleEditors(true ); end else
    if command=editCommandPageLeft          then begin command:=ecNone; cycleEditors(false); end else
    if command=editCommandToggleBookmark    then begin command:=ecNone; toggleBreakpoint;    end else
    if command=editCommandMarkWord          then begin command:=ecNone; setUnderCursor(true,false); end else
    if command=editCommandJumpToDeclaration then begin command:=ecNone; setUnderCursor(false,true); workspace.openLocation(underCursor.location); end else
    if (command>=ecGotoMarker0) and (command<=ecGotoMarker9)
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

PROCEDURE T_editorMeta.editorMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR jump,mark:boolean;
  begin
    jump:=ssCtrl in Shift;
    mark:=ssAlt  in Shift;
    if not(jump or mark) then exit;
    setUnderCursor(mark,jump,editor_.PixelsToRowColumn(point(x,y)));
    if jump then workspace.openLocation(underCursor.location);
  end;

DESTRUCTOR T_editorMeta.destroy;
  begin
    inherited destroy;
    fileInfo.filePath:='';
    disposeCodeAssistanceResponse(latestAssistanceReponse);
  end;

FUNCTION T_editorMeta.getPath: ansistring;
  begin
    result:=pseudoName(false);
  end;

FUNCTION T_editorMeta.getFolder:ansistring;
  begin
    if fileInfo.filePath='' then result:=ExtractFileDir(paramStr(0))
                            else result:=ExtractFileDir(fileInfo.filePath);
  end;

FUNCTION T_editorMeta.isPseudoFile: boolean;
  begin
    result:=fileInfo.filePath='';
  end;

PROCEDURE T_editorMeta.activate;
  VAR l:T_language;
  begin
    for l in T_language do if Assigned(fileTypeMeta[l].menuItem) then begin
      fileTypeMeta[l].menuItem.OnClick:=@languageMenuItemClick;
      fileTypeMeta[l].menuItem.checked:=(l=language_);
    end;
    try
      if language_=LANG_MNH then begin
        editor.highlighter:=highlighter;
        paintedWithStateHash:=0;
        triggerCheck;
      end else begin
        editor.highlighter:=fileTypeMeta[language_].highlighter;
        disposeCodeAssistanceResponse(latestAssistanceReponse);
      end;
      completionLogic.assignEditor(editor_,nil);
      editor.readonly       :=runnerModel.areEditorsLocked;
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

FUNCTION T_editorMeta.getCodeAssistanceDataRereferenced: P_codeAssistanceResponse;
  begin
    if latestAssistanceReponse=nil
    then exit(nil)
    else exit(latestAssistanceReponse^.rereferenced);
  end;

FUNCTION getHelpText(OUT helpLink:string):string;
  begin
    result:=underCursor.infoText;
    helpLink:=underCursor.linkToHelp;
  end;

PROCEDURE T_editorMeta.reloadFile;
  begin
    if not(isPseudoFile) and fileExists(fileInfo.filePath) then begin
      editor.lines.loadFromFile(fileInfo.filePath);
      fileAge(fileInfo.filePath,fileInfo.fileAccessAge);
      editor.modified:=false;
      fileInfo.isChanged:=false;
      InputEditChange(nil);
    end;
  end;

PROCEDURE T_editorMeta.updateContentAfterEditScript(CONST stringListLiteral: P_listLiteral);
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

INITIALIZATION
  underCursor.fullLine:='';
  underCursor.CaretX:=-1;

end.
