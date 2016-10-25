UNIT mnh_settings;
INTERFACE
USES myGenerics,dateutils,Classes,sysutils,mnh_fileWrappers,mnh_out_adapters,mySys,mnh_constants,serializationUtil,typinfo;
CONST
  C_SAVE_INTERVAL:array[0..6] of record text:string; interval:double; end=
  ((text:'off';        interval:1E6),
   (text:'1 minute';   interval:1/(24*60)),
   (text:'2 minutes';  interval:2/(24*60)),
   (text:'5 minutes';  interval:5/(24*60)),
   (text:'10 minutes'; interval:10/(24*60)),
   (text:'30 minutes'; interval:30/(24*60)),
   (text:'1 hour';     interval:1/24));
  FILE_HISTORY_MAX_SIZE=20;
TYPE
T_formPosition=object(T_serializable)
  top, Left, width, height: longint;
  isFullscreen: boolean;
  CONSTRUCTOR create;
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_streamWrapper):boolean; virtual;
  PROCEDURE saveToStream(VAR stream:T_streamWrapper); virtual;
end;

T_editorState=object(T_serializable)
  visible:boolean;
  filePath:ansistring;
  fileAccessAge:double;
  changed:boolean;
  lines:T_arrayOfString;
  markedLines:T_arrayOfLongint;
  caret:array['x'..'y'] of longint;
  language:byte;

  CONSTRUCTOR create;
  DESTRUCTOR destroy;
  PROCEDURE getLines(CONST dat: TStrings);
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_streamWrapper):boolean; virtual;
  PROCEDURE saveToStream(VAR stream:T_streamWrapper; CONST saveAll:boolean);
end;

T_fileHistory=object(T_serializable)
  items: T_listOfString;

  CONSTRUCTOR create;
  DESTRUCTOR destroy;
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_streamWrapper):boolean; virtual;
  PROCEDURE saveToStream(VAR stream:T_streamWrapper); virtual;
  FUNCTION polishHistory: boolean;
  PROCEDURE fileClosed(CONST fileName:ansistring);
  FUNCTION historyItem(CONST index:longint):ansistring;
end;

T_workspaceMeta=record
  fileName:string;
  name:string;
end;
T_workspaceMetaArray=array of T_workspaceMeta;

T_workspace=object(T_serializable)
  exporting:boolean;

  name:string;
  fileHistory:T_fileHistory;
  editorState: array of T_editorState;
  activePage:longint;

  CONSTRUCTOR create;
  DESTRUCTOR destroy;
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_streamWrapper):boolean; virtual;
  FUNCTION loadNameOnly(CONST fileName:string):boolean;
  PROCEDURE saveToStream(VAR stream:T_streamWrapper); virtual;
  PROCEDURE initDefaults(CONST withEditor:boolean);
end;

P_Settings=^T_settings;

{ T_settings }

T_settings=object(T_serializable)
  private
    //Nonpersistent:
    wasLoaded:boolean;
    savedAt:double;
    FUNCTION currentWorkspaceFilename:string;
  public
  //Global:
  memoryLimit:int64;
  cpuCount:longint;
  saveIntervalIdx:byte;
  editorFontname: string;
  fontSize:longint;
  antialiasedFonts:boolean;
  mainForm:T_formPosition;
  outputBehaviour: T_messageTypeSet;
  wordWrapEcho:boolean;
  outputLinesLimit:longint;
  doResetPlotOnEvaluation: boolean;
  workspaceFileName:string;

  //Workspace:
  workspace:T_workspace;

  CONSTRUCTOR create;
  DESTRUCTOR destroy;
  FUNCTION getSerialVersion:dword; virtual;
  FUNCTION loadFromStream(VAR stream:T_streamWrapper):boolean; virtual;
  PROCEDURE saveToStream(VAR stream:T_streamWrapper); virtual;
  PROCEDURE initDefaults;

  FUNCTION savingRequested:boolean;
  PROPERTY loaded:boolean read wasLoaded;

  PROCEDURE createNewWorkspace;
  FUNCTION importWorkspace(CONST fileName:string):boolean;
  PROCEDURE exportWorkspace(CONST fileName:string);
  PROCEDURE switchWorkspace(CONST fileName:string);
  FUNCTION deleteWorkspace:boolean;
end;

PROCEDURE saveSettings;
FUNCTION workerThreadCount:longint;
VAR settings:specialize G_lazyVar<P_Settings>;
IMPLEMENTATION

FUNCTION settingsFileName: string;
  begin
    result:=configDir+'mnh.settings';
  end;

FUNCTION defaultWorkspaceFileName: string;
  begin
    result:=configDir+'workspace.0';
  end;

FUNCTION newWorkspaceFileName: string;
  VAR i:longint;
  begin
    for i:=1 to maxLongint do begin
      result:=ChangeFileExt(defaultWorkspaceFileName,'.'+intToStr(i));
      if not(fileExists(result)) then exit(result);
    end;
    result:='';
  end;

FUNCTION avaliableWorkspaces:T_workspaceMetaArray;
  VAR files:T_arrayOfString;
      w:T_workspace;
      i:longint;
  begin
    files:=find(configDir+DirectorySeparator+'workspace.*',true,false);
    w.create;
    setLength(result,0);
    for i:=0 to length(result)-1 do if w.loadNameOnly(files[i]) then begin
      setLength(result,length(result)+1);
      with result[length(result)-1] do begin
        fileName:=files[i];
        name:=w.name;
      end;
    end;
    w.destroy;
  end;

PROCEDURE saveSettings;
  begin
    settings.value^.saveToFile(settingsFileName);
  end;

CONSTRUCTOR T_workspace.create;
  begin
    fileHistory.create;
    setLength(editorState,0);
    initDefaults(false);
  end;

DESTRUCTOR T_workspace.destroy;
  begin
    initDefaults(false);
    fileHistory.destroy;
    setLength(editorState,0);
  end;

FUNCTION T_workspace.getSerialVersion: dword;
  begin
    result:=2661226500;
  end;

FUNCTION T_workspace.loadFromStream(VAR stream: T_streamWrapper): boolean;
  VAR i:longint;
  begin
    if not(inherited loadFromStream(stream)) then exit(false);
    initDefaults(false);
    name:=stream.readAnsiString;
    if not(fileHistory.loadFromStream(stream)) then exit(false);
    setLength(editorState,stream.readNaturalNumber);
    result:=stream.allOkay;
    for i:=0 to length(editorState)-1 do begin
      editorState[i].create;
      result:=result and editorState[i].loadFromStream(stream);
    end;
    activePage:=stream.readLongint;
    result:=result and stream.allOkay;
    if not(result) then initDefaults(true);
  end;

FUNCTION T_workspace.loadNameOnly(CONST fileName:string): boolean;
  VAR stream:T_streamWrapper;
  begin
    stream.createToReadFromFile(fileName);
    result:=stream.allOkay and inherited loadFromStream(stream);
    if result then name:=stream.readAnsiString;
    result:=stream.allOkay;
    stream.destroy;
  end;

PROCEDURE T_workspace.saveToStream(VAR stream: T_streamWrapper);
  VAR i:longint;
      visibleEditorCount:longint=0;
  begin
    inherited saveToStream(stream);
    stream.writeAnsiString(name);
    fileHistory.saveToStream(stream);
    for i:=0 to length(editorState)-1 do if editorState[i].visible then inc(visibleEditorCount);
    stream.writeNaturalNumber(visibleEditorCount);
    for i:=0 to length(editorState)-1 do if editorState[i].visible then editorState[i].saveToStream(stream,exporting);
    stream.writeLongint(activePage);
  end;

PROCEDURE T_workspace.initDefaults(CONST withEditor:boolean);
  VAR i:longint;
  begin
    name:='';
    fileHistory.items.clear;
    for i:=0 to length(editorState)-1 do editorState[i].destroy;
    if withEditor then begin
      setLength(editorState,1);
      editorState[0].create;
      activePage:=0;
    end else begin
      setLength(editorState,0);
      activePage:=-1;
    end;
    exporting:=false;
  end;

FUNCTION T_settings.currentWorkspaceFilename: string;
  begin
    if workspaceFileName=''
    then result:=defaultWorkspaceFileName
    else result:=workspaceFileName;
  end;

CONSTRUCTOR T_settings.create;
  begin
    cpuCount:=getNumberOfCPUs;
    mainForm.create;
    workspace.create;
    wasLoaded:=false;
  end;

DESTRUCTOR T_settings.destroy;
  begin
    workspace.destroy;
  end;

FUNCTION workerThreadCount:longint;
  begin
    result:=settings.value^.cpuCount-1;
    if result>=0 then exit(result);
    result:=getNumberOfCPUs-1;
    if result<0 then result:=0;
    settings.value^.cpuCount:=result+1;
  end;

FUNCTION T_settings.getSerialVersion: dword; begin result:=1644235075; end;
FUNCTION T_settings.loadFromStream(VAR stream: T_streamWrapper): boolean;
  {$MACRO ON}
  {$define cleanExit:=begin initDefaults; exit(false) end}
  begin
    if not inherited loadFromStream(stream) then cleanExit;

    cpuCount:=stream.readLongint;
    if cpuCount<=0 then begin
      cpuCount:=getNumberOfCPUs;
      if cpuCount<1 then cpuCount:=1;
    end;
    fontSize:=stream.readLongint;
    editorFontname := stream.readAnsiString;
    antialiasedFonts:=stream.readBoolean;
    mainForm.loadFromStream(stream);
    outputBehaviour:=stream.readNaturalNumber;
    outputBehaviour:=outputBehaviour+[mt_clearConsole,mt_printline];
    doResetPlotOnEvaluation := stream.readBoolean;
    saveIntervalIdx:=stream.readByte;
    wordWrapEcho:=stream.readBoolean;
    memoryLimit:=stream.readInt64;
    outputLinesLimit:=stream.readLongint;
    workspaceFileName:=stream.readAnsiString;
    if not(stream.allOkay) then cleanExit else result:=true;
    if result then begin
      if not(fileExists(currentWorkspaceFilename)) then workspaceFileName:='';
      if not(workspace.loadFromFile(currentWorkspaceFilename)) then workspace.initDefaults(true);
    end;
    savedAt:=now;
    wasLoaded:=result;
  end;

PROCEDURE T_settings.saveToStream(VAR stream: T_streamWrapper);
  begin
    inherited saveToStream(stream);
    stream.writeLongint(cpuCount);
    stream.writeLongint(fontSize);
    stream.writeAnsiString(editorFontname);
    stream.writeBoolean(antialiasedFonts);
    mainForm.saveToStream(stream);
    stream.writeNaturalNumber(outputBehaviour);
    stream.writeBoolean(doResetPlotOnEvaluation);
    stream.writeByte(saveIntervalIdx);
    stream.writeBoolean(wordWrapEcho);
    stream.writeInt64(memoryLimit);
    stream.writeLongint(outputLinesLimit);
    workspace.saveToFile(currentWorkspaceFilename);
    savedAt:=now;
  end;

PROCEDURE T_settings.initDefaults;
  begin
    wordWrapEcho:=false;
    cpuCount:=getNumberOfCPUs;
    editorFontname:='';
    fontSize:=0;
    antialiasedFonts:=false;
    with mainForm do begin
      top := 0;
      Left := 0;
      width := 480;
      height := 480;
      isFullscreen := false;
    end;
    outputBehaviour:=C_defaultOutputBehavior_interactive;
    doResetPlotOnEvaluation:=true;
    saveIntervalIdx:=0;
    wasLoaded:=false;
    savedAt:=now;
    workspace.initDefaults(true);
    memoryLimit:={$ifdef Windows}
                   {$ifdef CPU32}
                   1000000000;
                   {$else}
                   int64(maxLongint)*2;
                   {$endif}
                 {$else}
                 1000000000;
                 {$endif}
    outputLinesLimit:=maxLongint;
  end;

FUNCTION T_settings.savingRequested: boolean;
  begin
    result:=(now-savedAt)>C_SAVE_INTERVAL[saveIntervalIdx].interval;
  end;

PROCEDURE T_settings.createNewWorkspace;
  begin
    workspace.saveToFile(currentWorkspaceFilename);
    workspace.initDefaults(true);
    workspaceFileName:=newWorkspaceFileName;
  end;

FUNCTION T_settings.importWorkspace(CONST fileName: string):boolean;
  begin
    workspace.saveToFile(currentWorkspaceFilename);
    result:=workspace.loadFromFile(fileName);
    if result then workspaceFileName:=newWorkspaceFileName
              else workspace.loadFromFile(currentWorkspaceFilename);
  end;

PROCEDURE T_settings.exportWorkspace(CONST fileName: string);
  begin
    workspace.exporting:=true;
    workspace.saveToFile(fileName);
    workspace.exporting:=false;
  end;

PROCEDURE T_settings.switchWorkspace(CONST fileName: string);
  begin
    workspace.saveToFile(currentWorkspaceFilename);
    if fileName='' then begin
      if not(workspace.loadFromFile(defaultWorkspaceFileName)) then workspace.initDefaults(true);
    end else begin
      if fileExists(fileName) and workspace.loadFromFile(fileName)
      then workspaceFileName:=fileName
      else workspace.loadFromFile(currentWorkspaceFilename);
    end;
  end;

FUNCTION T_settings.deleteWorkspace: boolean;
  VAR toDelete:string;
  begin
    result:=(workspaceFileName<>'') and (fileExists(workspaceFileName));
    if result then begin
      toDelete:=workspaceFileName;
      switchWorkspace('');
      DeleteFile(toDelete);
    end;
  end;

CONSTRUCTOR T_fileHistory.create;
  begin
    items.create;
  end;

DESTRUCTOR T_fileHistory.destroy;
  begin
    items.destroy;
  end;

FUNCTION T_fileHistory.getSerialVersion:dword; begin result:=176454893; end;

FUNCTION T_fileHistory.loadFromStream(VAR stream:T_streamWrapper):boolean;
  VAR i,count:longint;
  begin
    if not(inherited loadFromStream(stream)) then exit(false);
    items.clear;
    count:=stream.readNaturalNumber;
    if count>FILE_HISTORY_MAX_SIZE then exit(false);
    for i:=0 to count-1 do items.add(stream.readAnsiString);
    result:=stream.allOkay;
  end;

PROCEDURE T_fileHistory.saveToStream(VAR stream:T_streamWrapper);
  VAR i,count:longint;
  begin
    inherited saveToStream(stream);
    count:=items.size;
    if count>FILE_HISTORY_MAX_SIZE then count:=FILE_HISTORY_MAX_SIZE;
    stream.writeNaturalNumber(count);
    for i:=0 to count-1 do stream.writeAnsiString(items[i]);
  end;

FUNCTION T_fileHistory.polishHistory: boolean;
  VAR i, j: longint;
  begin
    result := false;
    for i:=0 to items.size-1 do
      if not(fileExists(items[i])) then begin
        items[i]:='';
        result:=true;
      end;
    i:=0;
    for i:=1 to items.size-1 do
      if (items[i]<>'') then for j:=0 to i-1 do
        if (expandFileName(items[i])=expandFileName(items[j])) then begin
          items[j]:='';
          result:=true;
        end;
    items.remValue('');
  end;

PROCEDURE T_fileHistory.fileClosed(CONST fileName: ansistring);
  begin
    items.add(fileName,0);
    polishHistory;
  end;

FUNCTION T_fileHistory.historyItem(CONST index: longint): ansistring;
  begin
    if (index>=0) and (index<items.size)
    then result:=items[index]
    else result:='';
  end;

CONSTRUCTOR T_formPosition.create;
  begin
    top:=0;
    Left:=0;
    width:=500;
    height:=500;
    isFullscreen:=true;
  end;

FUNCTION T_formPosition.getSerialVersion:dword; begin result:=1529642236; end;
FUNCTION T_formPosition.loadFromStream(VAR stream:T_streamWrapper):boolean;
  begin
    if not inherited loadFromStream(stream) then exit(false);
    top   :=stream.readLongint;
    Left  :=stream.readLongint;
    width :=stream.readLongint;
    height:=stream.readLongint;
    isFullscreen:=stream.readBoolean;
    result:=true;
  end;

PROCEDURE T_formPosition.saveToStream(VAR stream:T_streamWrapper);
  begin
    inherited saveToStream(stream);
    stream.writeLongint(top);
    stream.writeLongint(Left);
    stream.writeLongint(width);
    stream.writeLongint(height);
    stream.writeBoolean(isFullscreen);
  end;

CONSTRUCTOR T_editorState.create;
  begin
    visible:=false;
    setLength(lines,0);
    filePath:='';
    fileAccessAge:=0;
    changed:=false;
    setLength(lines,0);
    setLength(markedLines,0);
  end;

DESTRUCTOR T_editorState.destroy;
  begin
    filePath:='';
    fileAccessAge:=0;
    setLength(lines,0);
    setLength(markedLines,0);
  end;

PROCEDURE T_editorState.getLines(CONST dat: TStrings);
  VAR i:longint;
  begin
    dat.clear;
    for i:=0 to length(lines)-1 do dat.append(lines[i]);
  end;

FUNCTION T_editorState.getSerialVersion:dword; begin result:=1417366166; end;

FUNCTION T_editorState.loadFromStream(VAR stream:T_streamWrapper):boolean;
  VAR i:longint;
  begin
    if not inherited loadFromStream(stream) then exit(false);
    visible:=true;
    filePath:=stream.readAnsiString;
    changed:=stream.readBoolean;
    if changed then begin
      fileAccessAge:=stream.readDouble;
      setLength(lines,stream.readNaturalNumber);
      for i:=0 to length(lines)-1 do lines[i]:=stream.readAnsiString;
     end else begin
      lines:=fileLines(filePath,result);
      if result then fileAge(filePath,fileAccessAge)
                else visible:=false;
    end;
    setLength(markedLines,stream.readNaturalNumber);
    for i:=0 to length(markedLines)-1 do markedLines[i]:=stream.readLongint;
    caret['x']:=stream.readNaturalNumber;
    caret['y']:=stream.readNaturalNumber;
    language:=stream.readByte;
    result:=stream.allOkay;
  end;

PROCEDURE T_editorState.saveToStream(VAR stream:T_streamWrapper; CONST saveAll:boolean);
  VAR i:longint;
  begin
    inherited saveToStream(stream);
    if filePath<>'' then filePath:=expandFileName(filePath);
    stream.writeAnsiString(filePath);
    stream.writeBoolean(changed);
    if changed or saveAll then begin
      stream.writeDouble(fileAccessAge);
      stream.writeNaturalNumber(length(lines));
      for i:=0 to length(lines)-1 do stream.writeAnsiString(lines[i]);
    end;
    stream.writeNaturalNumber(length(markedLines));
    for i:=0 to length(markedLines)-1 do stream.writeLongint(markedLines[i]);
    stream.writeNaturalNumber(caret['x']);
    stream.writeNaturalNumber(caret['y']);
    stream.writeByte(language);
  end;

FUNCTION obtainSettings:P_Settings;
  begin
    ensurePath(settingsFileName);
    new(result,create);
    if fileExists(settingsFileName)
    then result^.loadFromFile(settingsFileName)
    else result^.initDefaults;
  end;

PROCEDURE disposeSettings(settings:P_Settings);
  begin
    dispose(settings,destroy);
  end;

INITIALIZATION
  settings.create(@obtainSettings,@disposeSettings);

FINALIZATION
  settings.destroy;

end.
