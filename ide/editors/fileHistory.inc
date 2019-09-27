{$ifdef includeInterface}
CONST FILE_HISTORY_MAX_SIZE=100;
TYPE

P_fileHistory=^T_fileHistory;
T_fileHistory=object(T_serializable)
  private
    //nonpersistent:
    menuRoot:TMenuItem;
    historyMenuItems:array of TMenuItem;

    scriptUsage:record
      cs:TRTLCriticalSection;
      destructionPending:boolean;
      dat:array of record usedScript,usingScript:string; end;
      foldersScanned:T_arrayOfString;
      scanning:boolean;
    end;

    //persistent:
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
    PROCEDURE logFolder(CONST folderName:string);
    FUNCTION historyItem(CONST index:longint):ansistring;
    FUNCTION findFiles(CONST rootPath:string):T_arrayOfString;
    FUNCTION findScriptsUsing(CONST scriptName:string):T_arrayOfString;
    PROCEDURE updateScriptUsage(CONST scriptName:string; CONST allUses:T_arrayOfString);
    PROCEDURE postUsageScan();
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
    fileWrappers.logFolderCallback:=@logFolder;

    initialize(scriptUsage);
    with scriptUsage do begin
      destructionPending:=false;
      initCriticalSection(cs);
      setLength(dat,0);
      setLength(foldersScanned,0);
      scanning:=false;
    end;
  end;

DESTRUCTOR T_fileHistory.destroy;
  VAR waitCount:longint=0;
  begin
    setLength(fileItems,0);
    setLength(folderItems,0);
    fileWrappers.logFolderCallback:=nil;
    with scriptUsage do begin
      destructionPending:=true;
      enterCriticalSection(cs);
      while scanning and (waitCount<200) do begin
        leaveCriticalSection(cs);
        sleep(10);
        inc(waitCount);
        enterCriticalSection(cs);
      end;
      setLength(dat,0);
      setLength(foldersScanned,0);
      leaveCriticalSection(cs);
      doneCriticalSection(cs);
    end;
  end;

FUNCTION T_fileHistory.getSerialVersion: dword; begin result:=176494; end;

FUNCTION canonicalFileName(CONST s:string):string;
  begin
    result:=replaceAll(replaceAll(replaceAll(expandFileName(s),'\','/'),'//','/'),'/',DirectorySeparator);
    if result[length(result)]=DirectorySeparator then result:=copy(result,1,length(result)-1);
  end;

FUNCTION T_fileHistory.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  FUNCTION readArray(OUT items:T_arrayOfString):boolean;
    VAR i,count:longint;
    begin
      setLength(items,0);
      count:=stream.readNaturalNumber;
      if count>FILE_HISTORY_MAX_SIZE then exit(false);
      setLength(items,count);
      for i:=0 to count-1 do items[i]:=canonicalFileName(stream.readAnsiString);
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
    if SameFileName(fileItems[i],fileItems[j]) then begin
      fileItems[i]:='';
      result:=true;
    end;
    dropValues(fileItems,'');

    for i:=0 to length(folderItems)-1 do
    if not(DirectoryExists(folderItems[i])) then begin
      folderItems[i]:='';
      result:=true;
    end;
    dropValues(folderItems,'');
    sortUnique(folderItems);
  end;

PROCEDURE T_fileHistory.fileClosed(CONST fileName: ansistring);
  begin
    prepend(fileItems,canonicalFileName(fileName));
    logFolder(ExtractFileDir(fileName));
    polishHistory;
    updateHistoryMenu;
  end;

PROCEDURE T_fileHistory.logFolder(CONST folderName:string);
  begin
    appendIfNew(folderItems,canonicalFileName(folderName));
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

FUNCTION T_fileHistory.findScriptsUsing(CONST scriptName:string):T_arrayOfString;
  VAR i:longint;
  begin
    with scriptUsage do if scanning then result:=C_EMPTY_STRING_ARRAY else begin
      enterCriticalSection(scriptUsage.cs);
      try
        setLength(result,0);
        for i:=0 to length(dat)-1 do if dat[i].usedScript=scriptName then appendIfNew(result,dat[i].usingScript);
        {$ifdef debugMode}
        writeln('T_fileHistory.findScriptsUsing found ',length(result),' scripts using ',scriptName);
        {$endif}
      finally
        leaveCriticalSection(scriptUsage.cs);
      end;
    end;
  end;

PROCEDURE T_fileHistory.updateScriptUsage(CONST scriptName:string; CONST allUses:T_arrayOfString);
  VAR i,j:longint;
  begin
    with scriptUsage do if not scanning then begin
      if tryEnterCriticalsection(scriptUsage.cs)<>0 then try
        j:=0;
        for i:=0 to length(dat)-1 do if dat[i].usingScript<>scriptName
        then begin
          if j<>i then dat[j]:=dat[i];
          inc(j);
        end;
        setLength(dat,j+length(allUses));
        for i:=0 to length(allUses)-1 do begin
          dat[j+i].usingScript:=scriptName;
          dat[j+i].usedScript:=allUses[i];
        end;
      finally
        leaveCriticalSection(scriptUsage.cs);
      end;
    end;
  end;

FUNCTION scanScripts(p:pointer):ptrint;
  VAR foldersToScan:T_arrayOfString;
      folder,script,usedScript:string;
      list:TStringList;
      scriptsToScan:T_arrayOfString;
  begin
    with P_fileHistory(p)^ do begin
      enterCriticalSection(scriptUsage.cs);
      try
        //Determine folders to scan:
        setLength(foldersToScan,0);
        append(foldersToScan,folderItems);
        for folder in scriptUsage.foldersScanned do dropValues(foldersToScan,folder);
        //Update scriptUsage.foldersScanned
        append(scriptUsage.foldersScanned,foldersToScan);

        {$ifdef debugMode}
        if length(foldersToScan)>0 then writeln(stdErr,'fileHistory/scanScripts: ',length(foldersToScan),' folders must be scanned [',scriptUsage.destructionPending,']');
        {$endif}
        //Determine scripts to scan:
        setLength(scriptsToScan,0);
        for folder in foldersToScan do if not(scriptUsage.destructionPending) then begin
          list:=FindAllFiles(folder+DirectorySeparator,'*.mnh',false);
          for script in list do append(scriptsToScan,script);
          list.free;
        end;
        sortUnique(scriptsToScan);
        {$ifdef debugMode}
        if length(foldersToScan)>0 then writeln(stdErr,'fileHistory/scanScripts: ',length(scriptsToScan),' scripts must be scanned [',scriptUsage.destructionPending,']');
        {$endif}

        //Load scripts
        for script in scriptsToScan do if not(scriptUsage.destructionPending) then
        for usedScript in sandbox^.usedAndExtendedPackages(script) do begin
          setLength(scriptUsage.dat,length(scriptUsage.dat)+1);
          scriptUsage.dat[length(scriptUsage.dat)-1].usingScript:=script;
          scriptUsage.dat[length(scriptUsage.dat)-1].usedScript :=usedScript;
          {$ifdef debugMode}
          writeln(stdErr,'fileHistory/scanScripts: ',script,' uses "',usedScript,'"');
          {$endif}
        end;
      finally
        scriptUsage.scanning:=false;
        leaveCriticalSection(scriptUsage.cs);
        setLength(foldersToScan,0);
        setLength(scriptsToScan,0);
      end;
    end;
    result:=1;
  end;

PROCEDURE T_fileHistory.postUsageScan();
  begin
    if scriptUsage.scanning or scriptUsage.destructionPending then exit;
    scriptUsage.scanning:=true;
    beginThread(@scanScripts,@self);
  end;

PROCEDURE T_fileHistory.clear;
  begin
    setLength(fileItems,0);
    setLength(folderItems,0);
    with scriptUsage do begin
      enterCriticalSection(cs);
      try
        setLength(dat,0);
        setLength(foldersScanned,0);
      finally
        leaveCriticalSection(cs);
      end;
    end;
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
