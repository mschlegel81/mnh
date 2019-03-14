{$ifdef includeInterface}
T_workspace=object(T_serializable)
  private
    mainForm:T_mnhIdeForm;
    inputPageControl:TPageControl;
    breakpointsImagesList :TImageList;
    bookmarkImagesList    :TImageList;

    nextMetaIndex:longint;

    metas:array of P_editorMeta;
    recentlyActivated:T_fileHistory;
    folderHistory    :T_fileHistory;
    fileHistory      :T_fileHistory;

    FUNCTION getAllBreakpoints:T_searchTokenLocations;
  public
    CONSTRUCTOR create(CONST ide:T_mnhIdeForm;
                       CONST pageControlForEditors:TPageControl;
                       CONST breakpointImages,bookmarkImages:TImageList);
    DESTRUCTOR destroy;

    //From T_serializable:
    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;

    FUNCTION currentEditor:P_editorMeta;

    FUNCTION addEditorMetaForNewFile:P_editorMeta;
    FUNCTION addOrGetEditorMetaForFiles(CONST FileNames: array of string; CONST useCurrentPageAsFallback:boolean):P_editorMeta;

    PROCEDURE openBookmarkLocation(CONST index:T_bookmarkIndex);
    FUNCTION openLocation(CONST location:T_searchTokenLocation):boolean;
    PROCEDURE keyUpForJumpToLocation(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE mouseDownForJumpToLocation(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);

    PROCEDURE updateEditorsByGuiStatus;
end;
{$endif}
{$ifdef includeImplementation}
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
  end;

CONSTRUCTOR T_workspace.create(CONST ide: T_mnhIdeForm;
  CONST pageControlForEditors: TPageControl; CONST breakpointImages,
  bookmarkImages: TImageList);
  begin
    mainForm:=ide;
    inputPageControl:=pageControlForEditors;
    breakpointsImagesList:=breakpointImages;
    bookmarkImagesList   :=bookmarkImages;

    nextMetaIndex:=1;
    setLength(metas,0);
    recentlyActivated.create(false);
    folderHistory    .create(true);
    fileHistory      .create(false);
  end;

DESTRUCTOR T_workspace.destroy;
begin

end;

FUNCTION T_workspace.getSerialVersion: dword;
begin

end;

FUNCTION T_workspace.loadFromStream(VAR stream: T_bufferedInputStreamWrapper
  ): boolean;
begin

end;

PROCEDURE T_workspace.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
begin

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

FUNCTION T_workspace.addOrGetEditorMetaForFiles(CONST FileNames: array of string; CONST useCurrentPageAsFallback: boolean): P_editorMeta;
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
    if result<>nil then result^.activate;
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
    mainForm.ActiveControl:=meta^.editor;
    result:=true;
  end;

PROCEDURE T_workspace.keyUpForJumpToLocation(Sender: TObject; VAR key: word; Shift: TShiftState);
  VAR caller:TSynEdit;
  begin
    caller:=TSynEdit(Sender);
    if (key=13) and (ssCtrl in Shift) then openLocation(guessLocationFromString(caller.lines[caller.CaretY-1],false));
  end;

PROCEDURE T_workspace.mouseDownForJumpToLocation(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
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
{$endif}
