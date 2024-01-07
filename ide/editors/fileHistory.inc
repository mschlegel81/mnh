{$ifdef includeInterface}
CONST FILE_HISTORY_MAX_SIZE=1000;
TYPE

P_fileHistory=^T_fileHistory;

{ T_fileHistory }

T_fileHistory=object(T_serializable)
  private
    //nonpersistent:
    menuRoot:TMenuItem;
    historyMenuItems:array of TMenuItem;
    historyCs:TRTLCriticalSection;

    //persistent:
    fileItems: T_arrayOfString;
    FUNCTION polishHistory: boolean;
  public
    CONSTRUCTOR create(CONST historyRootItem:TMenuItem);
    DESTRUCTOR destroy;
    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
    PROCEDURE fileTouched(CONST fileName:ansistring; CONST addToHistory:boolean);
    FUNCTION historyItem(CONST index:longint):ansistring;
    PROCEDURE clear;
    PROCEDURE updateHistoryMenu;
    PROCEDURE fileHistoryClick(Sender: TObject);
    PROCEDURE getClonedFileItems(OUT fi:T_arrayOfString);
    FUNCTION allFolders:T_arrayOfString;
  end;
{$endif}
{$ifdef includeImplementation}
CONSTRUCTOR T_fileHistory.create(CONST historyRootItem: TMenuItem);
  begin
    menuRoot:=historyRootItem;
    setLength(historyMenuItems,0);
    fileItems:=C_EMPTY_STRING_ARRAY;
    initCriticalSection(historyCs);
  end;

DESTRUCTOR T_fileHistory.destroy;
  begin
    setLength(fileItems,0);
    doneCriticalSection(historyCs);
  end;

FUNCTION T_fileHistory.getSerialVersion: dword; begin result:=176494; end;

FUNCTION T_fileHistory.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  FUNCTION readArray(OUT items:T_arrayOfString):boolean;
    VAR i,count:longint;
    begin
      initialize(items);
      setLength(items,0);
      count:=stream.readNaturalNumber;
      if count>FILE_HISTORY_MAX_SIZE then exit(false);
      setLength(items,count);
      for i:=0 to count-1 do items[i]:=canonicalFileName(stream.readAnsiString);
      result:=stream.allOkay;
    end;
  VAR dummy_for_backwards_compatibility:T_arrayOfString;
  begin
    {$ifdef debugMode}
    writeln('Loading T_fileHistory @',stream.streamPos);
    {$endif}
    if not(inherited loadFromStream(stream)) then exit(false);
    if readArray(fileItems) and readArray(dummy_for_backwards_compatibility)
    then result:=true
    else begin
      setLength(fileItems,0);
      result:=false;
    end;
    result:=result and stream.allOkay;
  end;

PROCEDURE T_fileHistory.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  PROCEDURE writeArray(CONST items:T_arrayOfString);
    VAR i,count:longint;
    begin
      count:=length(items);
      if count>FILE_HISTORY_MAX_SIZE then count:=FILE_HISTORY_MAX_SIZE;
      stream.writeNaturalNumber(count);
      for i:=0 to count-1 do stream.writeAnsiString(items[i]);
    end;

  begin
    try
      enterCriticalSection(historyCs);
      {$ifdef debugMode}
      writeln('Saving T_fileHistory @',stream.streamPos);
      {$endif}
      inherited saveToStream(stream);
      writeArray(fileItems);
      stream.writeNaturalNumber(0);
    finally
      leaveCriticalSection(historyCs);
    end;
  end;

FUNCTION T_fileHistory.polishHistory: boolean;
  VAR i, j: longint;
      fileItemsWorkingCopy:T_arrayOfString;
  begin
    try
      enterCriticalSection(historyCs);
      setLength(fileItemsWorkingCopy,length(fileItems));
      for i:=0 to length(fileItemsWorkingCopy)-1 do fileItemsWorkingCopy[i]:=fileItems[i];
    finally
      leaveCriticalSection(historyCs);
    end;

    result := false;
    for i:=0 to length(fileItemsWorkingCopy)-1 do
    if not(fileExists (fileItemsWorkingCopy[i])) then begin
      fileItemsWorkingCopy[i]:='';
      result:=true;
    end;
    for i:=1 to length(fileItemsWorkingCopy)-1 do
    if (fileItemsWorkingCopy[i]<>'') then for j:=0 to i-1 do
    if SameFileName(fileItemsWorkingCopy[i],fileItemsWorkingCopy[j]) then begin
      fileItemsWorkingCopy[i]:='';
      result:=true;
    end;
    dropValues(fileItemsWorkingCopy,'');

    try
      enterCriticalSection(historyCs);
      setLength(fileItems,length(fileItemsWorkingCopy));
      for i:=0 to length(fileItemsWorkingCopy)-1 do fileItems[i]:=fileItemsWorkingCopy[i];
    finally
      leaveCriticalSection(historyCs);
    end;
  end;

PROCEDURE T_fileHistory.fileTouched(CONST fileName: ansistring; CONST addToHistory:boolean);
  begin
    try
      enterCriticalSection(historyCs);
      if addToHistory then begin
        prepend(fileItems,canonicalFileName(fileName));
        updateHistoryMenu;
      end;
      fileCache.postFolderToScan(ExtractFileDir(fileName));
    finally
      leaveCriticalSection(historyCs);
    end;
  end;

FUNCTION T_fileHistory.historyItem(CONST index: longint): ansistring;
  begin
    try
      enterCriticalSection(historyCs);
      if (index>=0) and (index<length(fileItems))
      then result:=fileItems[index]
      else result:='';
    finally
      leaveCriticalSection(historyCs);
    end;
  end;

PROCEDURE T_fileHistory.clear;
  begin
    try
      enterCriticalSection(historyCs);
      setLength(fileItems,0);
    finally
      leaveCriticalSection(historyCs);
    end;
  end;

PROCEDURE T_fileHistory.updateHistoryMenu;
  VAR i:longint;
  begin
    if menuRoot=nil then exit;
    try
      enterCriticalSection(historyCs);
      for i:=0 to length(historyMenuItems)-1 do begin
        menuRoot.remove(historyMenuItems[i]);
        FreeAndNil(historyMenuItems[i]);
      end;
      setLength(historyMenuItems,length(fileItems));
      for i:=0 to length(fileItems)-1 do begin
        historyMenuItems[i]:=TMenuItem.create(menuRoot);
        historyMenuItems[i].caption:=intToStr(i)+': '+fileItems[i];
        historyMenuItems[i].Tag:=i;
        historyMenuItems[i].OnClick:=@fileHistoryClick;
        menuRoot.add(historyMenuItems[i]);
      end;
    finally
      leaveCriticalSection(historyCs);
    end;
  end;

PROCEDURE T_fileHistory.fileHistoryClick(Sender: TObject);
  VAR historyIdx:longint;
  begin
    historyIdx:=TMenuItem(Sender).Tag;
    if fileExists(historyItem(historyIdx))
    then workspace.addOrGetEditorMetaForFiles(historyItem(historyIdx),true,false);
  end;

PROCEDURE T_fileHistory.getClonedFileItems(OUT fi: T_arrayOfString);
  VAR i:longint;
  begin
    enterCriticalSection(historyCs);
    setLength(fi,length(fileItems));
    for i:=0 to length(fileItems)-1 do fi[i]:=fileItems[i];
    leaveCriticalSection(historyCs);
  end;

FUNCTION T_fileHistory.allFolders:T_arrayOfString;
  VAR s:string;
  begin
    result:=C_EMPTY_STRING_ARRAY;
    EnterCriticalSection(historyCs);
    for s in fileItems do appendIfNew(result,ExtractFileDir(s));
    LeaveCriticalSection(historyCs);
  end;

{$endif}
