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

    FUNCTION addEditorMetaForNewFile:P_editorMeta;
  public
    FUNCTION createNewFile:P_editorMeta;
    FUNCTION addOrGetEditorMetaForFiles(CONST FileNames: array of string; CONST useCurrentPageAsFallback:boolean):P_editorMeta;

    PROCEDURE openBookmarkLocation(CONST index:T_bookmarkIndex);
    FUNCTION openLocation(CONST location:T_searchTokenLocation):boolean;
    PROCEDURE keyUpForJumpToLocation(Sender: TObject; VAR key: word; Shift: TShiftState);

    PROCEDURE updateEditorsByGuiStatus;

    FUNCTION getAllBreakpoints:T_searchTokenLocations;
    PROCEDURE removeBreakpoint(CONST loc:T_searchTokenLocation);

    PROCEDURE closeCurrentFile;
    FUNCTION saveCurrentFile(CONST forceDialog:boolean=false; CONST meta:P_editorMeta=nil):boolean;
    PROCEDURE exportCurrentFileToHtml;
    PROCEDURE renameWordUnderCursor;

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

{$endif}
{$ifdef includeImplementation}
CONSTRUCTOR T_workspace.create(CONST ide: T_mnhIdeForm; CONST pageControlForEditors: TPageControl; CONST breakpointImages,bookmarkImages: TImageList; CONST historyRootItem,scriptMenuRootItem:TMenuItem);
  begin
    savedAt:=now+1; //= now + 24h

    mainForm:=ide;
    inputPageControl:=pageControlForEditors;
    breakpointsImagesList:=breakpointImages;
    bookmarkImagesList   :=bookmarkImages;

    nextMetaIndex:=1;
    setLength(metas,0);
    fileHistory.create(historyRootItem);
    setLength(oldMarkers,0);

    scriptMenu.create(scriptMenuRootItem,@miRunCustomUtilScript);
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
  begin
    {$ifdef debugMode}
    writeln('Loading T_workspace @',stream.streamPos);
    {$endif}

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
    for k:=0 to length(metas)-1 do new(metas[k],create(k));

    result:=stream.allOkay;
    for k:=0 to length(metas)-1 do if result then begin
      result:=result and metas[k]^.loadFromStream(stream) and stream.allOkay;
      if nextMetaIndex<=metas[k]^.metaIndex then nextMetaIndex:=metas[k]^.metaIndex+1;
    end;
    if not(result) then begin
      for j:=0 to length(metas)-1 do dispose(metas[j],destroy);
      setLength(metas,0);
    end else begin
      saveIntervalIdx:=stream.readByte([low(C_SAVE_INTERVAL)..high(C_SAVE_INTERVAL)]);
      autosaveBeforeEachExecution:=stream.readBoolean;
      newFileLineEnding  :=stream.readByte([LINE_ENDING_DEFAULT,LINE_ENDING_LINUX,LINE_ENDING_WINDOWS]);
      overwriteLineEnding:=stream.readByte([LINE_ENDING_DEFAULT,LINE_ENDING_LINUX,LINE_ENDING_WINDOWS,LINE_ENDING_UNCHANGED]);
      result:=stream.allOkay and
      fileHistory  .loadFromStream(stream);
    end;
    if result then begin
      for meta in metas do meta^.updateSheetCaption;
      if currentEditor<>nil then currentEditor^.activate;
      savedAt:=now;
    end else begin
      nextMetaIndex:=1;
      for k:=0 to length(metas)-1 do dispose(metas[k],destroy);
      setLength(metas,0);
      fileHistory.clear;
      setLength(oldMarkers,0);

      saveIntervalIdx:=0;
      autosaveBeforeEachExecution:=false;
      newFileLineEnding:=0;
      overwriteLineEnding:=0;
    end;
  end;

PROCEDURE T_workspace.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR k:longint;
  begin
    {$ifdef debugMode}
    writeln('Saving T_workspace @',stream.streamPos);
    {$endif}

    stream.writeNaturalNumber(length(oldMarkers));
    for k:=0 to length(oldMarkers)-1 do begin
      writeSearchTokenLocation(stream,oldMarkers[k].location);
      stream.writeByte(oldMarkers[k].markIndex);
    end;

    stream.writeNaturalNumber(length(metas));
    for k:=0 to length(metas)-1 do begin
      metas[k]^.saveToStream(stream);
    end;

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

FUNCTION T_workspace.addOrGetEditorMetaForFiles(
  CONST FileNames: array of string; CONST useCurrentPageAsFallback: boolean): P_editorMeta;
  PROCEDURE openSingleFile(CONST fileName:ansistring);
    FUNCTION isPseudoName:boolean;
      begin
        result:=(length(fileName)>1)
            and (fileName[1]='<')
            and (fileName[length(fileName)]='>');
      end;

    VAR meta:P_editorMeta;
    begin
      if isPseudoName then begin
        for meta in metas do if meta^.pseudoName=fileName then begin result:=meta; exit; end;
      end else begin
        for meta in metas do if meta^.fileInfo.filePath=fileName then begin result:=meta; exit; end;
        result:=addEditorMetaForNewFile();
        result^.setFile(fileName);
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
    caller:=TSynEdit(Sender);
    if (key=13) and (ssCtrl in Shift) then begin
      openLocation(guessLocationFromString(caller.lines[caller.CaretY-1],false));
      key:=0;
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
    for m in metas do if m^.getPath=loc.fileName then m^.deleteBreakpointIfExistent(loc.line);
    while k<length(oldMarkers) do
    if (oldMarkers[k].location=loc) and (oldMarkers[k].markIndex>=10) then begin
      for j:=k to length(oldMarkers)-2 do oldMarkers[j]:=oldMarkers[j+1];
      setLength(oldMarkers,length(oldMarkers)-1);
    end else inc(k);
    runnerModel.evaluation.stepper^.setBreakpoints(getAllBreakpoints);
  end;

PROCEDURE T_workspace.closeCurrentFile;
  VAR toClose:P_editorMeta;
      k:longint;
      answer:integer;
      mark:TSynEditMark;
  begin
    toClose:=currentEditor;
    if toClose=nil then exit;

    //Ask if editor shall be closed
    if toClose^.editor.modified then begin
      answer:=closeDialogForm.showOnClose(toClose^.pseudoName(true));
      if answer=mrOk then begin
        if not(saveCurrentFile(false,toClose)) then exit
      end else if answer=mrCancel then exit;
    end;

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
      //Update history:
      fileHistory.fileClosed(toClose^.getPath);
    end;

    //actually close editor...
    k:=0;
    while (k<length(metas)) and (metas[k]<>toClose) do inc(k);
    if k>length(metas) then raise Exception.create('Invalid state; Found editor to close but cannot determine index');
    dispose(metas[k],destroy);
    metas[k]:=metas[length(metas)-1];
    setLength(metas,length(metas)-1);

    toClose:=currentEditor;
    if toClose<>nil then toClose^.activate;
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
  end;

PROCEDURE T_workspace.exportCurrentFileToHtml;
  VAR SynExporterHTML: TSynExporterHTML;
      name:string;
      meta:P_editorMeta;
  begin
    meta:=currentEditor;
    if meta=nil then exit;

    name:=safeCallback(GetCurrentDir,'','.html');
    if name='' then exit;

    SynExporterHTML:=TSynExporterHTML.create(nil);
    SynExporterHTML.title:=meta^.pseudoName();
    SynExporterHTML.highlighter:=meta^.editor.highlighter;
    SynExporterHTML.ExportAll(meta^.editor.lines);
    SynExporterHTML.saveToFile(name);
    SynExporterHTML.free;
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
    result:=(now-savedAt)>C_SAVE_INTERVAL[saveIntervalIdx].interval;
  end;

FUNCTION T_workspace.getCurrentAssistanceResponse:P_codeAssistanceResponse;
  VAR meta:P_editorMeta;
  begin
    meta:=currentEditor;
    if meta=nil
    then result:=nil
    else result:=meta^.latestAssistanceReponse;
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
      disposeMessage(data);
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
