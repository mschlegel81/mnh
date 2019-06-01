{$ifdef includeInterface}
T_workspace=object(T_serializable)
  private
    savedAt:double; //nonpersistent
    scriptMenu:T_submenuModel; //nonpersistent

    mainForm:T_mnhIdeForm; //nonpersistent
    inputPageControl:TPageControl; //nonpersistent
    breakpointsImagesList :TImageList; //nonpersistent
    bookmarkImagesList    :TImageList; //nonpersistent

    nextMetaIndex:longint; //nonpersistent

    oldMarkers:array of record
      location:T_searchTokenLocation;
      markIndex:byte;
    end;

    metas:array of P_editorMeta;

  public
    saveIntervalIdx:byte;
    autosaveBeforeEachExecution:boolean;
    newFileLineEnding,overwriteLineEnding:byte;

    fileHistory:T_fileHistory;
    CONSTRUCTOR create(CONST ide:T_mnhIdeForm;
                       CONST pageControlForEditors:TPageControl;
                       CONST breakpointImages,bookmarkImages:TImageList;
                       CONST historyRootItem,scriptMenuRootItem:TMenuItem);
    DESTRUCTOR destroy;

    //From T_serializable:
    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;

    FUNCTION currentEditor:P_editorMeta;
    FUNCTION getExistingEditorForPath(CONST pathOrPseudoName:string):P_editorMeta;
    FUNCTION addEditorMetaForNewFile:P_editorMeta;
  public
    FUNCTION createNewFile:P_editorMeta;
    FUNCTION addOrGetEditorMetaForFiles(CONST FileNames: array of string; CONST useCurrentPageAsFallback:boolean):P_editorMeta;

    PROCEDURE openBookmarkLocation(CONST index:T_bookmarkIndex);
    FUNCTION openLocation(CONST location:T_searchTokenLocation):boolean;
    PROCEDURE keyUpForJumpToLocation(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE mouseDownForJumpToLocation(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);

    PROCEDURE updateEditorsByGuiStatus;

    FUNCTION getAllBreakpoints:T_searchTokenLocations;
    PROCEDURE removeBreakpoint(CONST loc:T_searchTokenLocation);
  private
    PROCEDURE closeQuietly(CONST toClose:P_editorMeta);
  public
    PROCEDURE closeCurrentFile;
    FUNCTION saveCurrentFile(CONST forceDialog:boolean=false; CONST meta:P_editorMeta=nil):boolean;
    PROCEDURE exportCurrentFileToHtml;
    PROCEDURE renameWordUnderCursor;
    PROCEDURE checkForFileChanges;

    FUNCTION getMainFormCaption:string;

    PROCEDURE postSaveRequest;
    FUNCTION savingRequested:boolean;
    FUNCTION getCurrentAssistanceResponse:P_codeAssistanceResponse;
    //Scripts:
    PROCEDURE miRunCustomUtilScript(Sender: TObject);
    PROCEDURE processEditScriptMessage(CONST message:P_storedMessage);
  end;

CONST
  C_SAVE_INTERVAL:array[0..6] of record text:string; interval:double; end=
  ((text:'off';        interval:1E6),
   (text:'1 minute';   interval: 1/(24*60)),
   (text:'2 minutes';  interval: 2/(24*60)),
   (text:'5 minutes';  interval: 5/(24*60)),
   (text:'10 minutes'; interval:10/(24*60)),
   (text:'30 minutes'; interval:30/(24*60)),
   (text:'1 hour';     interval: 1/24));
  LINE_ENDING_UNCHANGED=0;
  LINE_ENDING_DEFAULT=1;
  LINE_ENDING_LINUX=2;
  LINE_ENDING_WINDOWS=3;

  LINE_ENDING:array[0..3] of string=('',LineEnding,#10,#13#10);

FUNCTION newFileProxy(CONST fileName:string):P_codeProvider;
FUNCTION newFixatedFileProxy(CONST fileName:string):P_editorMetaProxy;
{$endif}
{$ifdef includeImplementation}
FUNCTION newFileProxy(CONST fileName:string):P_codeProvider;
  VAR proxy:P_editorMetaProxy;
  begin
    new(proxy,create(fileName));
    result:=proxy;
  end;

FUNCTION newFixatedFileProxy(CONST fileName:string):P_editorMetaProxy;
  begin
    new(result,create(fileName));
    result^.fixate;
  end;

{$i func_defines.inc}
FUNCTION editors_impl intFuncSignature;
  VAR meta:P_editorMeta;
  begin
    result:=newListLiteral();
    for meta in workspace.metas do listResult^.appendString(meta^.pseudoName());
  end;

FUNCTION editorContent_impl intFuncSignature;
  VAR meta:P_editorMeta=nil;
      i:longint;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      meta:=workspace.getExistingEditorForPath(str0^.value);
      if meta=nil then exit(newVoidLiteral);
      result:=newListLiteral(meta^.editor.lines.count);
      for i:=0 to meta^.editor.lines.count-1 do listResult^.appendString(meta^.editor.lines[i]);
    end;
  end;

//TODO: Implement this
//FUNCTION newEditor_impl intFuncSignature;
//  begin
//    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) and (arg1^.literalType=lt_stringList) then begin
//
//    end;
//  end;

CONSTRUCTOR T_workspace.create(CONST ide: T_mnhIdeForm; CONST pageControlForEditors: TPageControl; CONST breakpointImages,bookmarkImages: TImageList; CONST historyRootItem,scriptMenuRootItem:TMenuItem);
  begin
    savedAt:=now+1; //= now + 24h

    mainForm:=ide;
    inputPageControl:=pageControlForEditors;
    breakpointsImagesList:=breakpointImages;
    bookmarkImagesList   :=bookmarkImages;

    newFileLineEnding:=LINE_ENDING_DEFAULT;
    overwriteLineEnding:=LINE_ENDING_UNCHANGED;

    nextMetaIndex:=1;
    setLength(metas,0);
    fileHistory.create(historyRootItem);
    setLength(oldMarkers,0);

    scriptMenu.create(scriptMenuRootItem,@miRunCustomUtilScript);

    registerRule(GUI_NAMESPACE,'editors'       ,@editors_impl      ,ak_nullary,'editors(...);//Lists all editors');
    registerRule(GUI_NAMESPACE,'editorContent' ,@editorContent_impl,ak_unary,'editorContent(name:string);//Returns the content of the given editor as a string or void if no such editor was found.');
    packages.newCodeProvider:=@newFileProxy;
  end;

DESTRUCTOR T_workspace.destroy;
  VAR k:longint;
  begin
    fileHistory.destroy;
    for k:=0 to length(metas)-1 do dispose(metas[k],destroy);
    setLength(metas,0);
    scriptMenu.destroy;
  end;

FUNCTION T_workspace.getSerialVersion: dword;
  begin
    result:=2624114;
  end;

FUNCTION T_workspace.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  PROCEDURE resetWorkspace;
    begin
      nextMetaIndex:=1;
      setLength(metas,0);
      fileHistory  .clear;
      setLength(oldMarkers,0);
    end;

  VAR k,j:longint;
      meta:P_editorMeta;
      sheetToDestroy:TTabSheet;
      currentFile:string='';
  begin
    {$ifdef debugMode}
    writeln('Loading T_workspace @',stream.streamPos);
    {$endif}
    if not(inherited loadFromStream(stream)) then exit(false);

    nextMetaIndex:=1;
    setLength(oldMarkers,stream.readNaturalNumber);
    for k:=0 to length(oldMarkers)-1 do begin
      oldMarkers[k].location:=readSearchTokenLocation(stream);
      oldMarkers[k].markIndex:=stream.readByte([0..10]);
    end;
    if not(stream.allOkay) then begin
      setLength(oldMarkers,0);
      exit(false);
    end;

    setLength(metas,stream.readNaturalNumber);
    {$ifdef debugMode}
    writeln('T_workspace: Attempting to load ',length(metas),' editors @',stream.streamPos);
    {$endif}
    for k:=0 to length(metas)-1 do new(metas[k],create(k));

    result:=stream.allOkay;
    for k:=0 to length(metas)-1 do if result then begin
      result:=result and metas[k]^.loadFromStream(stream) and stream.allOkay;
      metas[k]^.metaIndex:=nextMetaIndex;
      inc(nextMetaIndex);
    end;
    if result then begin
      currentFile:=stream.readAnsiString;
      saveIntervalIdx:=stream.readByte([low(C_SAVE_INTERVAL)..high(C_SAVE_INTERVAL)]);
      autosaveBeforeEachExecution:=stream.readBoolean;
      newFileLineEnding  :=stream.readByte([LINE_ENDING_DEFAULT,LINE_ENDING_LINUX,LINE_ENDING_WINDOWS]);
      overwriteLineEnding:=stream.readByte([LINE_ENDING_DEFAULT,LINE_ENDING_LINUX,LINE_ENDING_WINDOWS,LINE_ENDING_UNCHANGED]);
      result:=stream.allOkay and
      fileHistory  .loadFromStream(stream);
    end;
    if result then begin
      for meta in metas do meta^.updateSheetCaption;
      savedAt:=now;
      if currentFile<>'' then begin
        meta:=getExistingEditorForPath(currentFile);
        if meta<>nil then begin
          inputPageControl.activePage:=meta^.tabsheet;
          meta^.activate;
        end;
      end else meta:=nil;
      if meta=nil then meta:=currentEditor;
      if meta<>nil then meta^.activate;
      {$ifdef debugMode}
      writeln('T_workspace loaded successfully @',stream.streamPos);
      {$endif}
    end else begin
      for j:=0 to length(metas)-1 do begin
        sheetToDestroy:=metas[j]^.tabsheet;
        dispose(metas[j],destroy);
        FreeAndNil(sheetToDestroy);
      end;
      setLength(metas,0);

      nextMetaIndex:=1;
      fileHistory.clear;
      setLength(oldMarkers,0);

      saveIntervalIdx:=0;
      autosaveBeforeEachExecution:=false;
      newFileLineEnding:=LINE_ENDING_DEFAULT;
      overwriteLineEnding:=LINE_ENDING_UNCHANGED;
      {$ifdef debugMode}
      writeln('Loading T_workspace failed @',stream.streamPos);
      {$endif}
    end;
  end;

PROCEDURE T_workspace.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR k:longint;
      active:P_editorMeta;
  begin
    {$ifdef debugMode}
    writeln('Saving T_workspace @',stream.streamPos);
    {$endif}
    inherited saveToStream(stream);

    stream.writeNaturalNumber(length(oldMarkers));
    for k:=0 to length(oldMarkers)-1 do begin
      writeSearchTokenLocation(stream,oldMarkers[k].location);
      stream.writeByte(oldMarkers[k].markIndex);
    end;

    stream.writeNaturalNumber(length(metas));
    for k:=0 to length(metas)-1 do metas[k]^.saveToStream(stream);
    active:=currentEditor;
    if active=nil then stream.writeAnsiString('') else stream.writeAnsiString(active^.pseudoName());

    stream.writeByte(saveIntervalIdx);
    stream.writeBoolean(autosaveBeforeEachExecution);
    stream.writeByte(newFileLineEnding);
    stream.writeByte(overwriteLineEnding);

    fileHistory  .saveToStream(stream);
    savedAt:=now;
  end;

FUNCTION T_workspace.currentEditor: P_editorMeta;
  VAR meta:P_editorMeta;
  begin
    result:=nil;
    for meta in metas do
      if meta^.tabsheet=inputPageControl.activePage
      then exit(meta);
  end;

FUNCTION T_workspace.addEditorMetaForNewFile: P_editorMeta;
  VAR i:longint;
  begin
    i:=length(metas);
    setLength(metas,i+1);
    new(result,create(nextMetaIndex));
    inc(nextMetaIndex);
    metas[i]:=result;
    result^.editor.readonly:=runnerModel.areEditorsLocked or result^.strictlyReadOnly;
  end;

FUNCTION T_workspace.createNewFile:P_editorMeta;
  begin
    result:=addEditorMetaForNewFile;
    inputPageControl.activePage:=result^.tabsheet;
    result^.activate;
  end;

FUNCTION T_workspace.getExistingEditorForPath(CONST pathOrPseudoName:string):P_editorMeta;
  VAR meta:P_editorMeta;
  begin
    try
      result:=nil;
      for meta in metas do if SameFileName(meta^.pseudoName,pathOrPseudoName) then exit(meta);
    except
      result:=nil;
    end;
  end;

FUNCTION T_workspace.addOrGetEditorMetaForFiles(CONST FileNames: array of string; CONST useCurrentPageAsFallback: boolean): P_editorMeta;
  PROCEDURE openSingleFile(CONST fileName:ansistring);
    FUNCTION isPseudoName:boolean;
      begin
        result:=(length(fileName)>1)
            and (fileName[1]='<')
            and (fileName[length(fileName)]='>');
      end;

    PROCEDURE updateMarkers(CONST meta:P_editorMeta);
      VAR k:longint=0;
          i:longint;
      begin
        while k<length(oldMarkers) do if oldMarkers[k].location.fileName=fileName then begin
          meta^._add_breakpoint_or_bookmark_(oldMarkers[k].location.line,oldMarkers[k].location.column,oldMarkers[k].markIndex);
          for i:=k to length(oldMarkers)-2 do oldMarkers[i]:=oldMarkers[i+1];
          setLength(oldMarkers,length(oldMarkers)-1);
        end else inc(k);
      end;

    VAR meta:P_editorMeta;
    begin
      if isPseudoName then begin
        for meta in metas do if meta^.pseudoName=fileName then begin result:=meta; exit; end;
      end else begin
        for meta in metas do if SameFileName(meta^.fileInfo.filePath,fileName) then begin result:=meta; exit; end;
        result:=addEditorMetaForNewFile();
        result^.setFile(fileName);
        updateMarkers(result);
        //Update history:
        fileHistory.fileClosed(fileName);
      end;
    end;

  VAR f:string;
  begin
    if useCurrentPageAsFallback
    then result:=currentEditor
    else result:=nil;

    for f in FileNames do openSingleFile(f);
    if result<>nil then begin
      inputPageControl.activePage:=result^.tabsheet;
      result^.activate;
    end;
  end;

PROCEDURE T_workspace.openBookmarkLocation(CONST index: T_bookmarkIndex);
  VAR meta:P_editorMeta;
      k:longint;
      mark:TSynEditMark;
      location:T_searchTokenLocation;
  begin
    for meta in metas do begin
      for k:=0 to meta^.editor.Marks.count-1 do begin
        mark:=meta^.editor.Marks[k];
        if mark.IsBookmark and (mark.BookmarkNumber=index) then begin
          inputPageControl.activePage:=meta^.tabsheet;
          meta^.activate;
          meta^.editor.CaretY:=mark.line;
          meta^.editor.CaretX:=mark.column;
          exit;
        end;
      end;
    end;
    for k:=0 to length(oldMarkers)-1 do if oldMarkers[k].markIndex=index then begin
      location:=oldMarkers[k].location;
      openLocation(location);
      exit;
    end;
  end;

FUNCTION T_workspace.openLocation(CONST location: T_searchTokenLocation): boolean;
  VAR meta:P_editorMeta;
  begin
    if (location.fileName='') or (location.fileName='?') or (location.fileName=MNH_PSEUDO_PACKAGE.getPath) then exit(false);
    meta:=addOrGetEditorMetaForFiles(location.fileName,false);
    if meta=nil then exit(false);
    inputPageControl.activePage:=meta^.tabsheet;
    meta^.setCaret(location);
    inputPageControl.activePage:=meta^.tabsheet;
    mainForm.ActiveControl:=meta^.editor;
    result:=true;
  end;

PROCEDURE T_workspace.keyUpForJumpToLocation(Sender: TObject; VAR key: word; Shift: TShiftState);
  VAR caller:TSynEdit;
  begin
    if (key=13) and (ssCtrl in Shift) then begin
      caller:=TSynEdit(Sender);
      openLocation(guessLocationFromString(caller.lines[caller.CaretY-1],false));
      key:=0;
    end;
  end;

PROCEDURE T_workspace.mouseDownForJumpToLocation(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR caller:TSynEdit;
  begin
    if (ssCtrl in Shift) and (button=mbLeft) then begin
      caller:=TSynEdit(Sender);
      openLocation(guessLocationFromString(caller.lines[caller.PixelsToRowColumn(point(x,y)).y-1],false));
    end;
  end;

PROCEDURE T_workspace.updateEditorsByGuiStatus;
  VAR m:P_editorMeta;
  begin
    for m in metas do m^.editor.readonly:=runnerModel.areEditorsLocked or m^.strictlyReadOnly;
  end;

FUNCTION T_workspace.getAllBreakpoints: T_searchTokenLocations;
  VAR m:P_editorMeta;
      mark:TSynEditMark;
      k:longint;
  begin
    setLength(result,0);
    for m in metas do begin
      for k:=0 to m^.editor.Marks.count-1 do begin
        mark:=m^.editor.Marks[k];
        if not(mark.IsBookmark) then begin
          setLength(result,length(result)+1);
          result[length(result)-1]:=m^.markLocation(mark.line,mark.column);
        end;
      end;
    end;
    for k:=0 to length(oldMarkers)-1 do if oldMarkers[k].markIndex>9 then begin
      setLength(result,length(result)+1);
      result[length(result)-1]:=oldMarkers[k].location;
    end;
  end;

PROCEDURE T_workspace.removeBreakpoint(CONST loc:T_searchTokenLocation);
  VAR m:P_editorMeta;
      k:longint=0;
      j:longint;
  begin
    for m in metas do if SameFileName(m^.getPath,loc.fileName) then m^.deleteBreakpointIfExistent(loc.line);
    while k<length(oldMarkers) do
    if (oldMarkers[k].location=loc) and (oldMarkers[k].markIndex>=10) then begin
      for j:=k to length(oldMarkers)-2 do oldMarkers[j]:=oldMarkers[j+1];
      setLength(oldMarkers,length(oldMarkers)-1);
    end else inc(k);
    runnerModel.evaluation.stepper^.setBreakpoints(getAllBreakpoints);
  end;

PROCEDURE T_workspace.closeQuietly(CONST toClose:P_editorMeta);
  VAR k:longint;
      sheetToDestroy:TTabSheet;
      newCurrent:P_editorMeta;
      mark:TSynEditMark;
  begin
    //Editor shall be closed
    if not(toClose^.isPseudoFile) then begin
      //Copy marks
      for k:=0 to toClose^.editor.Marks.count-1 do begin
        setLength(oldMarkers,length(oldMarkers)+1);
        mark:=toClose^.editor.Marks[k];
        oldMarkers[length(oldMarkers)-1].location:=toClose^.markLocation(mark.line,mark.column);
        if mark.IsBookmark
        then oldMarkers[length(oldMarkers)-1].markIndex:=mark.BookmarkNumber
        else oldMarkers[length(oldMarkers)-1].markIndex:=10;
      end;
    end;

    //actually close editor...
    k:=0;
    while (k<length(metas)) and (metas[k]<>toClose) do inc(k);
    if k>length(metas) then raise Exception.create('Invalid state; Found editor to close but cannot determine index');
    sheetToDestroy:=metas[k]^.tabsheet;
    dispose(metas[k],destroy);
    FreeAndNil(sheetToDestroy);
    metas[k]:=metas[length(metas)-1];
    setLength(metas,length(metas)-1);

    newCurrent:=currentEditor;
    if newCurrent<>nil then newCurrent^.activate;
  end;

PROCEDURE T_workspace.closeCurrentFile;
  VAR toClose:P_editorMeta;
  begin
    toClose:=currentEditor;
    if toClose=nil then exit;
    //Ask if editor shall be closed
    if toClose^.editor.modified then begin
      case closeDialogForm.showOnClose(toClose^.pseudoName(true)) of
        cda_saveAndClose: if not(saveCurrentFile(false,toClose)) then exit;
        cda_cancelAndStay:                                            exit;
      end;
    end;
    closeQuietly(toClose);
  end;

FUNCTION T_workspace.saveCurrentFile(CONST forceDialog:boolean=false; CONST meta:P_editorMeta=nil): boolean;
  VAR toSave:P_editorMeta;
      newName:string='';
      path,name,ext:string;
  begin
    if meta=nil
    then toSave:=currentEditor
    else toSave:=meta;
    //If nothing to save, then exit
    if toSave=nil then exit(false);

    //If file has no name or "saveAs" has been called, show save dialog
    if (toSave^.isPseudoFile) or forceDialog then begin
      if toSave^.isPseudoFile then begin
        path:=GetCurrentDir;
        name:='';
        if length(fileTypeMeta[toSave^.language].extensions)>0
        then ext:='.'+lowercase(fileTypeMeta[toSave^.language].extensions[0])
        else ext:='';
      end else begin
        path:=ExtractFileDir     (toSave^.fileInfo.filePath);
        name:=ExtractFileNameOnly(toSave^.fileInfo.filePath);
        ext :=extractFileExt     (toSave^.fileInfo.filePath);
      end;
      newName:=safeCallback(path,name,ext);
      if newName=''
      then exit(false)
      else toSave^.fileInfo.filePath:=expandFileName(newName);
    end;

    toSave^.saveFile(newName);
    fileHistory.fileClosed(newName);
    result:=true;
  end;

PROCEDURE T_workspace.exportCurrentFileToHtml;
  VAR meta:P_editorMeta;
      htmlMeta:P_editorMeta;
      htmlText:ansistring;
  begin
    meta:=currentEditor;
    if meta=nil then exit;
    htmlText:=htmlExporter.textToHtml(meta^.pseudoName,meta^.editor.lines,meta^.editor.highlighter);
    htmlMeta:=addEditorMetaForNewFile;
    htmlMeta^.language:=LANG_HTML;
    htmlMeta^.editor.text:=htmlText;
    inputPageControl.activePage:=htmlMeta^.tabsheet;
  end;

PROCEDURE T_workspace.renameWordUnderCursor;
  VAR meta:P_editorMeta;
      id:string;
      idType:T_tokenType;
      renameLocation:T_searchTokenLocation;
      scanOther:boolean;
  begin
    meta:=currentEditor;
    if (meta=nil) or (meta^.language<>LANG_MNH) then exit;
    if meta^.canRenameUnderCursor(id,idType,renameLocation,scanOther) and
       (renameForm.showModalFor(id,idType,scanOther)=mrOk)
    then meta^.doRename(renameLocation,id,renameForm.newId,renameForm.checkAllEditorsCheckBox.checked and scanOther);
  end;

PROCEDURE T_workspace.checkForFileChanges;
  VAR metaIndex:longint=0;
      m:P_editorMeta;
  FUNCTION fileIsModifiedOnFileSystem: boolean;
    VAR currentFileAge:double;
    begin
      if (m^.fileInfo.filePath='') or (m^.editor.modified) or (m^.fileInfo.isChanged) then exit(false);
      fileAge(m^.fileInfo.filePath,currentFileAge);
      result:=currentFileAge<>m^.fileInfo.fileAccessAge;
    end;

  begin
    while (metaIndex>=0) and (metaIndex<length(metas)) do begin
      m:=metas[metaIndex];
      with m^ do
      if (fileInfo.filePath<>'') and not(fileExists(fileInfo.filePath)) and not(fileInfo.ignoreDeleted) then begin
        case closeDialogForm.showOnDeleted(fileInfo.filePath) of
          cda_close        : begin
            closeQuietly(m);
            dec(metaIndex);
          end;
          cda_ignoreChanges: begin
            if not(saveCurrentFile(false,m))
            then fileInfo.isChanged:=true;
          end;
          cda_overwrite    : begin
            fileInfo.ignoreDeleted:=true;
            fileInfo.isChanged:=true;
            updateSheetCaption;
          end;
        end;
      end else if fileIsModifiedOnFileSystem then begin
        case closeDialogForm.showOnOutOfSync(fileInfo.filePath) of
          cda_reload:
            reloadFile();
          cda_ignoreChanges:
            fileInfo.isChanged:=true;
          cda_overwrite:
            if not(saveCurrentFile(false,m)) then fileInfo.isChanged:=true;
        end;
      end;
      inc(metaIndex);
    end;
  end;

FUNCTION T_workspace.getMainFormCaption:string;
  VAR meta:P_editorMeta;
  begin
    meta:=currentEditor;
    if meta=nil
    then result:=APP_TITLE                                                                     {$ifdef debugMode}+' [debug]'{$endif}
    else result:=APP_TITLE+' '+meta^.pseudoName(false)+BoolToStr(meta^.editor.modified,' *',''){$ifdef debugMode}+' [debug]'{$endif};
  end;

PROCEDURE T_workspace.postSaveRequest;
  begin
    savedAt:=0;
  end;

FUNCTION T_workspace.savingRequested:boolean;
  begin
    result:=(savedAt<=0) or ((now-savedAt)>C_SAVE_INTERVAL[saveIntervalIdx].interval);
  end;

FUNCTION T_workspace.getCurrentAssistanceResponse:P_codeAssistanceResponse;
  VAR meta:P_editorMeta;
  begin
    meta:=currentEditor;
    if (meta=nil) or (meta^.language<>LANG_MNH)
    then result:=nil
    else result:=meta^.getCodeAssistanceDataRereferenced;
  end;

PROCEDURE T_workspace.miRunCustomUtilScript(Sender: TObject);
  VAR meta:P_editorMeta;
  begin
    if not(runnerModel.canRun) then exit;
    meta:=currentEditor;
    if meta=nil then exit;
    runnerModel.scriptEval.runUtilScript(
      TMenuItem(Sender).Tag,
      meta^.getLines,
      fileTypeMeta[meta^.language].extensions[0],
      meta^.pseudoName());
    updateEditorsByGuiStatus;
  end;

PROCEDURE T_workspace.processEditScriptMessage(CONST message:P_storedMessage);
  PROCEDURE processEditScriptResponse(CONST data:P_editScriptTask);
    VAR target:P_editorMeta;
    begin
      {$ifdef debugMode} writeln('        DEBUG: TMnhForm.onEditFinished; data present: ',data<>nil,'; successful: ',(data<>nil) and (data^.successful)); {$endif}
      if data^.successful then begin
        if (data^.wantOutput) and (data^.getOutput<>nil) and (data^.getOutput^.literalType=lt_stringList) then begin
          if data^.wantNewEditor then target:=createNewFile
                                 else target:=addOrGetEditorMetaForFiles(data^.inputEditorPseudoName,false);
          if target<>nil then begin
            target^.setLanguage(data^.getOutputLanguage,LANG_TXT);
            target^.updateContentAfterEditScript(P_listLiteral(data^.getOutput));
          end;
        end else if (data^.wantInsert) and (data^.getOutput<>nil) and (data^.getOutput^.literalType=lt_string) then begin
          target:=addOrGetEditorMetaForFiles(data^.inputEditorPseudoName,false);
          if target<>nil then target^.insertText(P_stringLiteral(data^.getOutput)^.value);
        end;
      end;
      updateEditorsByGuiStatus;
      disposeMessage_(data);
    end;

  PROCEDURE updateScriptMenu;
    VAR i:longint;
        scriptList:T_scriptMetaArray;
    begin
      scriptMenu.clear;
      scriptList:=runnerModel.scriptEval.getScripts;
      for i:=0 to length(scriptList)-1 do scriptMenu.addItem(scriptList[i]^.getName,i);
    end;

  begin
    case message^.messageType of
      mt_guiEditScriptsLoaded: updateScriptMenu;
      mt_guiEdit_done: processEditScriptResponse(P_editScriptTask(message));
      else raise Exception.create('Invalid message type handed to workspace.processEditScriptMessage()');
    end;
  end;

{$endif}
