{$ifdef includeInterface}
T_workspace=object(T_serializable)
  private
    mainForm:T_mnhIdeForm;
    inputPageControl:TPageControl;
    breakpointsImagesList :TImageList;
    bookmarkImagesList    :TImageList;

    nextMetaIndex:longint;

    oldMarkers:array of record
      location:T_searchTokenLocation;
      markIndex:byte;
    end;

    metas:array of P_editorMeta;
    fileHistory      :T_fileHistory;

  public
    folderHistory    :T_fileHistory;
    CONSTRUCTOR create(CONST ide:T_mnhIdeForm;
                       CONST pageControlForEditors:TPageControl;
                       CONST breakpointImages,bookmarkImages:TImageList);
    DESTRUCTOR destroy;

    //From T_serializable:
    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;

    FUNCTION currentEditor:P_editorMeta;

  private
    FUNCTION addEditorMetaForNewFile:P_editorMeta;
  public
    PROCEDURE createNewFile;
    FUNCTION addOrGetEditorMetaForFiles(CONST FileNames: array of string; CONST useCurrentPageAsFallback:boolean):P_editorMeta;

    PROCEDURE openBookmarkLocation(CONST index:T_bookmarkIndex);
    FUNCTION openLocation(CONST location:T_searchTokenLocation):boolean;
    PROCEDURE keyUpForJumpToLocation(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE mouseDownForJumpToLocation(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);

    PROCEDURE updateEditorsByGuiStatus;

    FUNCTION getAllBreakpoints:T_searchTokenLocations;

    PROCEDURE closeCurrentFile;
    FUNCTION saveCurrentFile(CONST forceDialog:boolean=false; CONST meta:P_editorMeta=nil):boolean;
    PROCEDURE exportCurrentFileToHtml;
    PROCEDURE renameWordUnderCursor;
end;
{$endif}
{$ifdef includeImplementation}
CONSTRUCTOR T_workspace.create(CONST ide: T_mnhIdeForm; CONST pageControlForEditors: TPageControl; CONST breakpointImages,
  bookmarkImages: TImageList);
  begin
    mainForm:=ide;
    inputPageControl:=pageControlForEditors;
    breakpointsImagesList:=breakpointImages;
    bookmarkImagesList   :=bookmarkImages;

    nextMetaIndex:=1;
    setLength(metas,0);
    folderHistory    .create(true);
    fileHistory      .create(false);
  end;

DESTRUCTOR T_workspace.destroy;
  VAR k:longint;
  begin
    folderHistory.destroy;
    fileHistory.destroy;
    for k:=0 to length(metas)-1 do dispose(metas[k],destroy);
    setLength(metas,0);
  end;

FUNCTION T_workspace.getSerialVersion: dword;
  begin
    result:=2624113;
  end;

FUNCTION T_workspace.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR k,j:longint;
  begin
    nextMetaIndex:=1;
    setLength(oldMarkers,stream.readNaturalNumber);
    for k:=0 to length(oldMarkers)-1 do begin
      oldMarkers[k].location:=readSearchTokenLocation(stream);
      oldMarkers[k].markIndex:=stream.readByte;
    end;
    if not(stream.allOkay) then begin
      setLength(oldMarkers,0);
      exit(false);
    end;

    setLength(metas,stream.readNaturalNumber);
    for k:=0 to length(metas)-1 do begin
      new(metas[k],create(-1,stream));
      if not(stream.allOkay) then begin
        for j:=0 to k do dispose(metas[j],destroy);
        setLength(metas,0);
        exit(false);
      end;
      if nextMetaIndex<=metas[k]^.metaIndex then nextMetaIndex:=metas[k]^.metaIndex+1;
    end;

    result:=stream.allOkay and
    fileHistory  .loadFromStream(stream) and
    folderHistory.loadFromStream(stream);
  end;

PROCEDURE T_workspace.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR k:longint;
  begin
    stream.writeNaturalNumber(length(oldMarkers));
    for k:=0 to length(oldMarkers)-1 do begin
      writeSearchTokenLocation(stream,oldMarkers[k].location);
      stream.writeByte(oldMarkers[k].markIndex);
    end;

    stream.writeNaturalNumber(length(metas));
    for k:=0 to length(metas)-1 do begin
      metas[k]^.saveToStream(stream);
    end;

    fileHistory  .saveToStream(stream);
    folderHistory.saveToStream(stream);
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

PROCEDURE T_workspace.createNewFile;
  VAR meta:P_editorMeta;
  begin
    meta:=addEditorMetaForNewFile;
    inputPageControl.activePage:=meta^.tabsheet;
    meta^.activate;
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

    VAR filePath:ansistring;
        meta:P_editorMeta;
    begin
      if isPseudoName then begin
        for meta in metas do if meta^.pseudoName=fileName then begin result:=meta; exit; end;
      end else begin
        for meta in metas do if meta^.fileInfo.filePath=filePath then begin result:=meta; exit; end;
        result:=addEditorMetaForNewFile();
        result^.setFile(filePath);
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
    if (key=13) and (ssCtrl in Shift) then openLocation(guessLocationFromString(caller.lines[caller.CaretY-1],false));
  end;

PROCEDURE T_workspace.mouseDownForJumpToLocation(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
      caller:TSynEdit;
  begin
    if (ssCtrl) in Shift then begin
      caller:=TSynEdit(Sender);
      point.x:=x;
      point.y:=y;
      point:=caller.PixelsToRowColumn(point);
      openLocation(guessLocationFromString(caller.lines[point.y-1],false));
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
    //Copy marks
    if not(toClose^.isPseudoFile) then
    for k:=0 to toClose^.editor.Marks.count-1 do begin
      setLength(oldMarkers,length(oldMarkers)+1);
      mark:=toClose^.editor.Marks[k];
      oldMarkers[length(oldMarkers)-1].location:=toClose^.markLocation(mark.line,mark.column);
      if mark.IsBookmark
      then oldMarkers[length(oldMarkers)-1].markIndex:=mark.BookmarkNumber
      else oldMarkers[length(oldMarkers)-1].markIndex:=10;
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

{$endif}
