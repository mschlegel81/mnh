{$ifdef includeInterface}
CONST FILE_HISTORY_MAX_SIZE=100;
TYPE
P_fileHistory=^T_fileHistory;
T_fileHistory=object(T_serializable)
  private
    menuRoot:TMenuItem;
    historyMenuItems:array of TMenuItem;

    fileItems: T_arrayOfString;
    FUNCTION polishHistory: boolean;
  public
    folderItems: T_arrayOfString;
    CONSTRUCTOR create(CONST historyRootItem:TMenuItem);
    DESTRUCTOR destroy;
    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
    PROCEDURE fileClosed(CONST fileName:ansistring);
    FUNCTION historyItem(CONST index:longint):ansistring;
    FUNCTION findFiles(CONST rootPath:string):T_arrayOfString;
    PROCEDURE clear;
    PROCEDURE updateHistoryMenu;
    PROCEDURE fileHistoryClick(Sender: TObject);
  end;
{$endif}
{$ifdef includeImplementation}
CONSTRUCTOR T_fileHistory.create(CONST historyRootItem: TMenuItem);
  begin
    menuRoot:=historyRootItem;
    setLength(historyMenuItems,0);

    fileItems:=C_EMPTY_STRING_ARRAY;
    folderItems:=C_EMPTY_STRING_ARRAY;
  end;

DESTRUCTOR T_fileHistory.destroy;
  begin
    setLength(fileItems,0);
    setLength(folderItems,0);
  end;

FUNCTION T_fileHistory.getSerialVersion: dword; begin result:=176494; end;

FUNCTION T_fileHistory.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  FUNCTION readArray(OUT items:T_arrayOfString):boolean;
    VAR i,count:longint;
    begin
      setLength(items,0);
      count:=stream.readNaturalNumber;
      if count>FILE_HISTORY_MAX_SIZE then exit(false);
      setLength(items,count);
      for i:=0 to count-1 do items[i]:=stream.readAnsiString;
      result:=stream.allOkay;
    end;

  begin
    {$ifdef debugMode}
    writeln('Loading T_fileHistory @',stream.streamPos);
    {$endif}
    if not(inherited loadFromStream(stream)) then exit(false);
    if readArray(fileItems) and readArray(folderItems)
    then result:=true
    else begin
      setLength(fileItems,0);
      setLength(folderItems,0);
      result:=false;
    end;
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
    {$ifdef debugMode}
    writeln('Saving T_fileHistory @',stream.streamPos);
    {$endif}
    inherited saveToStream(stream);
    writeArray(fileItems);
    writeArray(folderItems);
  end;

FUNCTION T_fileHistory.polishHistory: boolean;
  VAR i, j: longint;
  begin
    result := false;
    for i:=0 to length(fileItems)-1 do
    if not(fileExists (fileItems[i])) then begin
      fileItems[i]:='';
      result:=true;
    end;
    for i:=1 to length(fileItems)-1 do
    if (fileItems[i]<>'') then for j:=0 to i-1 do
    if SameFileName(expandFileName(fileItems[i]),expandFileName(fileItems[j])) then begin
      fileItems[i]:='';
      result:=true;
    end;
    dropValues(fileItems,'');

    for i:=0 to length(folderItems)-1 do
    if not(DirectoryExists(folderItems[i])) then begin
      folderItems[i]:='';
      result:=true;
    end;
    for i:=1 to length(folderItems)-1 do
    if (folderItems[i]<>'') then for j:=0 to i-1 do
    if SameFileName(expandFileName(folderItems[i]),expandFileName(folderItems[j])) then begin
      folderItems[i]:='';
      result:=true;
    end;
    dropValues(folderItems,'');
  end;

PROCEDURE T_fileHistory.fileClosed(CONST fileName: ansistring);
  begin
    prepend(fileItems,fileName);
    prepend(folderItems,ExtractFileDir(fileName));
    polishHistory;
    updateHistoryMenu;
  end;

FUNCTION T_fileHistory.historyItem(CONST index: longint): ansistring;
  begin
    if (index>=0) and (index<length(fileItems))
    then result:=fileItems[index]
    else result:='';
  end;

FUNCTION T_fileHistory.findFiles(CONST rootPath: string): T_arrayOfString;
  VAR allPathsToScan:T_arrayOfString;
      fileName:string;
      pathToScan:string;
      list:TStringList;
  begin
    allPathsToScan:=folderItems;
    result:=listScriptFileNames(rootPath);
    for pathToScan in allPathsToScan do begin
      list:=FindAllFiles(pathToScan+DirectorySeparator,'*.mnh',false);
      for fileName in list do append(result,fileName);
      list.free;
    end;
    sortUnique(result);
  end;

PROCEDURE T_fileHistory.clear;
  begin
    setLength(fileItems,0);
    setLength(folderItems,0);
  end;

PROCEDURE T_fileHistory.updateHistoryMenu;
  VAR i:longint;
  begin
    if menuRoot=nil then exit;
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
  end;

PROCEDURE T_fileHistory.fileHistoryClick(Sender: TObject);
  VAR historyIdx:longint;
  begin
    historyIdx:=TMenuItem(Sender).Tag;
    if fileExists(historyItem(historyIdx))
    then workspace.addOrGetEditorMetaForFiles(historyItem(historyIdx),true)
    else if polishHistory then updateHistoryMenu;
  end;

{$endif}
