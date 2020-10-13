{$ifdef includeInterface}
CONST FILE_HISTORY_MAX_SIZE=100;
TYPE
T_usagePair=record usedScript,usingScript:string; end;

P_fileHistory=^T_fileHistory;
T_fileHistory=object(T_serializable)
  private
    //nonpersistent:
    menuRoot:TMenuItem;
    historyMenuItems:array of TMenuItem;
    historyCs:TRTLCriticalSection;

    scriptUsage:record
      destructionPending:boolean;
      dat:array of T_usagePair;
      foldersScanned:T_arrayOfString;
      scanning:boolean;
    end;

    //persistent:
    fileItems: T_arrayOfString;
    FUNCTION polishHistory: boolean;
  public
    //TODO: Extend history by "ignored folders", e.g. network drives...
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
    FUNCTION findRelatedScriptsTransitive(CONST scriptName:string):T_arrayOfString;
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

    initCriticalSection(historyCs);
    initialize(scriptUsage);
    with scriptUsage do begin
      destructionPending:=false;
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
      while scanning and (waitCount<200) do begin
        sleep(10);
        inc(waitCount);
      end;
      setLength(dat,0);
      setLength(foldersScanned,0);
      if not(scanning) then doneCriticalSection(historyCs);
    end;
  end;

FUNCTION T_fileHistory.getSerialVersion: dword; begin result:=176494; end;

FUNCTION canonicalFileName(CONST s:string):string;
  begin
    result:=ansiReplaceStr(ansiReplaceStr(ansiReplaceStr(expandFileName(s),'\','/'),'//','/'),'/',DirectorySeparator);
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
    try
      enterCriticalSection(historyCs);
      {$ifdef debugMode}
      writeln('Saving T_fileHistory @',stream.streamPos);
      {$endif}
      inherited saveToStream(stream);
      writeArray(fileItems);
      writeArray(folderItems);
    finally
      leaveCriticalSection(historyCs);
    end;
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
    try
      enterCriticalSection(historyCs);
      prepend(fileItems,canonicalFileName(fileName));
      logFolder(ExtractFileDir(fileName));
      polishHistory;
      updateHistoryMenu;
    finally
      leaveCriticalSection(historyCs);
    end;
  end;

PROCEDURE T_fileHistory.logFolder(CONST folderName:string);
  begin
    try
      enterCriticalSection(historyCs);
      appendIfNew(folderItems,canonicalFileName(folderName));
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

FUNCTION T_fileHistory.findFiles(CONST rootPath: string): T_arrayOfString;
  VAR allPathsToScan:T_arrayOfString;
      fileName:string;
      pathToScan:string;
      list:TStringList;
  begin
    try
      enterCriticalSection(historyCs);
      allPathsToScan:=folderItems;
      result:=listScriptFileNames(rootPath);
      for pathToScan in allPathsToScan do begin
        list:=FindAllFiles(pathToScan+DirectorySeparator,'*.*',false);
        for fileName in list do append(result,fileName);
        list.free;
      end;
      sortUnique(result);
    finally
      leaveCriticalSection(historyCs);
    end;
  end;

FUNCTION T_fileHistory.findScriptsUsing(CONST scriptName:string):T_arrayOfString;
  VAR i:longint;
  begin
    try
      enterCriticalSection(historyCs);
      with scriptUsage do begin
        setLength(result,0);
        for i:=0 to length(dat)-1 do if dat[i].usedScript=scriptName then appendIfNew(result,dat[i].usingScript);
      end;
    finally
      leaveCriticalSection(historyCs);
    end;
  end;

FUNCTION T_fileHistory.findRelatedScriptsTransitive(CONST scriptName:string):T_arrayOfString;
  VAR i:longint;
      countBefore:longint=0;
      script:string;
  begin
    try
      enterCriticalSection(historyCs);
      with scriptUsage do begin
        result:=scriptName;
        repeat
          countBefore:=length(result);
          for script in result do
          for i:=0 to length(dat)-1 do
            if dat[i].usedScript=script
            then appendIfNew(result,dat[i].usingScript)
            else if dat[i].usingScript=script
            then appendIfNew(result,dat[i].usedScript);
        until countBefore=length(result);
        dropValues(result,scriptName);
        sortUnique(result);
      end;
    finally
      leaveCriticalSection(historyCs);
    end;
  end;

PROCEDURE T_fileHistory.updateScriptUsage(CONST scriptName:string; CONST allUses:T_arrayOfString);
  VAR i,j:longint;
  begin
    try
      enterCriticalSection(historyCs);
      with scriptUsage do begin
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
      end;
    finally
      leaveCriticalSection(historyCs);
    end;
  end;

FUNCTION scanScripts(p:pointer):ptrint;
  CONST ONE_SECOND = 1/(24*60*60);
  VAR foldersToScan:T_arrayOfString=();
      folder,script,usedScript:string;
      scriptsInThisFolder:TStringList;
      foundUsages:array of T_usagePair=();
      i0,i:longint;
      scanStart:double;
      foldersScannedInThisRun:T_arrayOfString=();
      startFolderScan:double;

      //TODO: log time per script, warn about long running analyses

  FUNCTION isDestructionPending:boolean;
    begin
      result:=P_fileHistory(p)^.scriptUsage.destructionPending;
    end;

  begin
    scanStart:=now;
    with P_fileHistory(p)^ do begin
      try
        enterCriticalSection(historyCs);
        //Determine folders to scan:
        append(foldersToScan,folderItems);
        for folder in scriptUsage.foldersScanned do dropValues(foldersToScan,folder);
      finally
        leaveCriticalSection(historyCs);
      end;
    end;
    //Determine scripts to scan:
    for folder in foldersToScan do if not(isDestructionPending) then begin
      startFolderScan:=now;
      append(foldersScannedInThisRun,folder);
      scriptsInThisFolder:=FindAllFiles(folder+DirectorySeparator,'*.mnh',false);
      //Load scripts
      for script in scriptsInThisFolder do if not(isDestructionPending) then begin
        {$ifdef debugMode}
        writeln(stdErr,'fileHistory/scanScripts: scanning ',script);
        {$endif}
        for usedScript in sandbox^.usedAndExtendedPackages(script) do begin
          setLength(foundUsages,length(foundUsages)+1);
          foundUsages[length(foundUsages)-1].usingScript:=script;
          foundUsages[length(foundUsages)-1].usedScript :=usedScript;
        end;
      end;
      if now-startFolderScan>0.5/(24*60*60) then postIdeMessage('Background folder scan: finished "'+folder+'" in '+myTimeToStr(now-startFolderScan),true);
      scriptsInThisFolder.free;
    end;
    with P_fileHistory(p)^ do try
      enterCriticalSection(historyCs);
      i0:=length(scriptUsage.dat);
      setLength(scriptUsage.dat,i0+length(foundUsages));
      for i:=0 to length(foundUsages)-1 do scriptUsage.dat[i0+i]:=foundUsages[i];
      //Update scriptUsage.foldersScanned
      append(scriptUsage.foldersScanned,foldersScannedInThisRun);
    finally
      scriptUsage.scanning:=false;
      leaveCriticalSection(historyCs);
    end;
    setLength(foldersToScan,0);
    if length(foldersScannedInThisRun)>0 then postIdeMessage('Background folder scan for '+intToStr(length(foldersScannedInThisRun))+' folders finished in '+myTimeToStr(now-scanStart),false);
    if (length(foundUsages)>0) and not(isDestructionPending) then workspace.onFileScanFinished;
    setLength(foundUsages,0);
    result:=1;
  end;

PROCEDURE T_fileHistory.postUsageScan();
  begin
    try
      enterCriticalSection(historyCs);
      if scriptUsage.scanning or scriptUsage.destructionPending then exit;
      scriptUsage.scanning:=true;
      beginThread(@scanScripts,@self);
    finally
      leaveCriticalSection(historyCs);
    end;
  end;

PROCEDURE T_fileHistory.clear;
  begin
    try
      enterCriticalSection(historyCs);
      setLength(fileItems,0);
      setLength(folderItems,0);
      with scriptUsage do begin
        setLength(dat,0);
        setLength(foldersScanned,0);
      end;
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
    then workspace.addOrGetEditorMetaForFiles(historyItem(historyIdx),true,false)
    else if polishHistory then updateHistoryMenu;
  end;

{$endif}
