UNIT codeAssistance;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, Controls, Graphics, Dialogs, SynEdit,
  myGenerics,
  basicTypes,
  mnh_constants,
  fileWrappers,
  tokenArray,
  tokenStack,
  mnh_messages,
  out_adapters,
  contexts,
  recyclers,
  packages;

TYPE
  T_highlightingData=object
    private
      highlightingCs:TRTLCriticalSection;
      warnLocations:array of record line,column:longint; isError:boolean; end;
      userRules    :T_setOfString;
      localIdInfos :T_localIdInfos;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE clear;
      FUNCTION isUserRule(CONST id: string): boolean;
      FUNCTION isErrorLocation(CONST lineIndex, tokenStart, tokenEnd: longint): byte;
      FUNCTION isLocalId(CONST id: string; CONST lineIndex, colIdx: longint): boolean;
      PROCEDURE clearLocalIdInfos;
  end;

  P_codeAssistanceResponse=^T_codeAssistanceResponse;
  T_codeAssistanceResponse=object
    private
      responseCs:TRTLCriticalSection;
      referenceCount:longint;
      localErrors,externalErrors:T_storedMessages;
      localIdInfos:P_localIdInfos;
      responseStateHash:T_hashInt;
      CONSTRUCTOR create(CONST package_:P_package; CONST messages:T_storedMessages; CONST stateHash_:T_hashInt; CONST localIdInfos_:P_localIdInfos);
      DESTRUCTOR destroy;
    public
      package:P_package;
      PROPERTY stateHash:T_hashInt read responseStateHash;
      FUNCTION  updateCompletionList(VAR wordsInEditor:T_setOfString; CONST lineIndex, colIdx: longint):boolean;
      PROCEDURE updateHighlightingData(VAR highlightingData:T_highlightingData);
      FUNCTION explainIdentifier(CONST fullLine: ansistring; CONST CaretY, CaretX: longint; VAR info: T_tokenInfo):boolean;
      FUNCTION  renameIdentifierInLine(CONST location:T_searchTokenLocation; CONST oldId,newId:string; VAR lineText:ansistring; CONST CaretY:longint):boolean;
      FUNCTION  getImportablePackages:T_arrayOfString;
      FUNCTION  resolveImport(CONST id:string):string;
      PROCEDURE getErrorHints(VAR edit:TSynEdit; OUT hasErrors, hasWarnings: boolean);
      FUNCTION  rereferenced:P_codeAssistanceResponse;
  end;

FUNCTION doCodeAssistanceSynchronously(CONST source:P_codeProvider; VAR recycler:T_recycler; CONST givenGlobals:P_evaluationGlobals=nil; CONST givenAdapters:P_messagesErrorHolder=nil):P_codeAssistanceResponse;
FUNCTION getLatestAssistanceResponse(CONST source:P_codeProvider):P_codeAssistanceResponse;
PROCEDURE postCodeAssistanceRequest(CONST source:P_codeProvider);
PROCEDURE disposeCodeAssistanceResponse(VAR r:P_codeAssistanceResponse);
PROCEDURE finalizeCodeAssistance;

IMPLEMENTATION
USES sysutils;
FUNCTION doCodeAssistanceSynchronously(CONST source:P_codeProvider; VAR recycler:T_recycler; CONST givenGlobals:P_evaluationGlobals=nil; CONST givenAdapters:P_messagesErrorHolder=nil):P_codeAssistanceResponse;
  VAR //temporary
      globals:P_evaluationGlobals;
      adapters:T_messagesErrorHolder;
      //output
      initialStateHash:T_hashInt;
      package:P_package;
      localIdInfos:P_localIdInfos;
      loadMessages:T_storedMessages;
  begin
    if givenGlobals=nil then begin
      adapters.createErrorHolder(nil,C_errorsAndWarnings+[mt_el1_note]);
      new(globals,create(@adapters));
    end else begin
      globals:=givenGlobals;
      givenAdapters^.clear;
    end;
    new(package,create(source,nil));
    {$Q-}{$R-}
    initialStateHash:=source^.stateHash xor hashOfAnsiString(source^.getPath);
    {$Q+}{$R+}
    globals^.resetForEvaluation(package,nil,ect_silent,C_EMPTY_STRING_ARRAY,recycler);
    new(localIdInfos,create);
    package^.load(lu_forCodeAssistance,globals^,recycler,C_EMPTY_STRING_ARRAY,localIdInfos);
    if givenGlobals<>nil then loadMessages:=givenAdapters^.storedMessages(false)
                         else loadMessages:=adapters      .storedMessages(false);
    new(result,create(package,loadMessages,initialStateHash,localIdInfos));
    globals^.afterEvaluation(recycler);
    if givenGlobals=nil then begin
      dispose(globals,destroy);
      {$WARN 5089 OFF}
      adapters.destroy;
    end;
  end;

VAR codeAssistanceResponse:P_codeAssistanceResponse=nil;
    codeAssistanceRequest :P_codeProvider=nil;
    codeAssistanceCs      :TRTLCriticalSection;
    codeAssistantIsRunning:boolean=false;
    shuttingDown          :boolean=false;

PROCEDURE disposeCodeAssistanceResponse(VAR r:P_codeAssistanceResponse);
  begin
    if (r<>nil) and (interlockedDecrement(r^.referenceCount)<=0) then dispose(r,destroy);
    r:=nil;
  end;

FUNCTION codeAssistanceThread(p:pointer):ptrint;
  VAR globals:P_evaluationGlobals;
      adapters:T_messagesErrorHolder;

      provider:P_codeProvider;
      response:P_codeAssistanceResponse;
      lastEvaluationEnd:double;
  FUNCTION isShutdownRequested:boolean;
    begin
      enterCriticalSection(codeAssistanceCs);
      result:=shuttingDown;
      leaveCriticalSection(codeAssistanceCs);
    end;

  FUNCTION timeForNextCheck:boolean;
    CONST ONE_SECOND=1/(24*60*60);
    begin
      if (now<lastEvaluationEnd+0.5*ONE_SECOND) then exit(false);
      enterCriticalSection(codeAssistanceCs);
      result:=shuttingDown or
              (codeAssistanceRequest<>nil) and
              ((codeAssistanceResponse^.package^.getCodeProvider^.getPath<>codeAssistanceRequest^.getPath) or
               (codeAssistanceResponse^.stateHash                        <>codeAssistanceRequest^.stateHash));
      leaveCriticalSection(codeAssistanceCs);
    end;
  VAR recycler:T_recycler;
  begin
    //setup:
    adapters.createErrorHolder(nil,C_errorsAndWarnings+[mt_el1_note]);
    new(globals,create(@adapters));
    recycler.initRecycler;
    //:setup
    repeat
      enterCriticalSection(codeAssistanceCs);
      provider:=codeAssistanceRequest;
      codeAssistanceRequest:=nil;
      leaveCriticalSection(codeAssistanceCs);
      if provider<>nil then begin;
        response:=doCodeAssistanceSynchronously(provider,recycler,globals,@adapters);
        lastEvaluationEnd:=now;
        enterCriticalSection(codeAssistanceCs);
        try
          disposeCodeAssistanceResponse(codeAssistanceResponse);
          codeAssistanceResponse:=response;
        finally
          leaveCriticalSection(codeAssistanceCs);
        end;
      end;
      repeat sleep(100) until timeForNextCheck;
    until isShutdownRequested;
    //shutdown:
    dispose(globals,destroy);
    adapters.destroy;
    enterCriticalSection(codeAssistanceCs);
    codeAssistantIsRunning:=false;
    leaveCriticalSection(codeAssistanceCs);
    result:=0;
    recycler.cleanup;
    //:shutdown
  end;

FUNCTION getLatestAssistanceResponse(CONST source:P_codeProvider): P_codeAssistanceResponse;
  begin
    enterCriticalSection(codeAssistanceCs);
    try
      if (codeAssistanceResponse<>nil) and (codeAssistanceResponse^.package^.getCodeProvider^.getPath=source^.getPath)
      then result:=codeAssistanceResponse^.rereferenced
      else result:=nil;
    finally
      leaveCriticalSection(codeAssistanceCs);
    end;
  end;

PROCEDURE postCodeAssistanceRequest(CONST source: P_codeProvider);
  begin
    enterCriticalSection(codeAssistanceCs);
    try
      if (codeAssistanceRequest<>nil) and codeAssistanceRequest^.disposeOnPackageDestruction
      then dispose(codeAssistanceRequest,destroy);
      codeAssistanceRequest:=source;
      if not(codeAssistantIsRunning) then begin
        codeAssistantIsRunning:=true;
        beginThread(@codeAssistanceThread);
      end;
    finally
      leaveCriticalSection(codeAssistanceCs);
    end;
  end;

PROCEDURE T_codeAssistanceResponse.updateHighlightingData(VAR highlightingData: T_highlightingData);
  VAR k:longint;
      e:P_storedMessage;
  begin
    enterCriticalSection(responseCs);
    enterCriticalSection(highlightingData.highlightingCs);
    try
      highlightingData.userRules.clear;
      package^.updateLists(highlightingData.userRules,false);
      highlightingData.localIdInfos.copyFrom(localIdInfos);
      setLength(highlightingData.warnLocations,length(localErrors));
      k:=0;
      for e in localErrors do begin
        if k>=length(highlightingData.warnLocations) then setLength(highlightingData.warnLocations,k+1);
        highlightingData.warnLocations[k].line   :=e^.getLocation.line;
        highlightingData.warnLocations[k].column :=e^.getLocation.column;
        highlightingData.warnLocations[k].isError:=C_messageTypeMeta[e^.messageType].level>2;
        inc(k);
      end;
      setLength(highlightingData.warnLocations,k);
    finally
      leaveCriticalSection(highlightingData.highlightingCs);
      leaveCriticalSection(responseCs);
    end;
  end;

CONSTRUCTOR T_highlightingData.create;
  begin
    initCriticalSection(highlightingCs);
    enterCriticalSection(highlightingCs);
    try
      userRules   .create;
      localIdInfos.create;
      setLength(warnLocations,0);
    finally
      leaveCriticalSection(highlightingCs);
    end;
  end;

PROCEDURE T_highlightingData.clear;
  begin
    enterCriticalSection(highlightingCs);
    try
      userRules   .clear;
      localIdInfos.clear;
      setLength(warnLocations,0);
    finally
      leaveCriticalSection(highlightingCs);
    end;
  end;

DESTRUCTOR T_highlightingData.destroy;
  begin
    enterCriticalSection(highlightingCs);
    try
      userRules   .destroy;
      localIdInfos.destroy;
      setLength(warnLocations,0);
    finally
      leaveCriticalSection(highlightingCs);
      doneCriticalSection(highlightingCs);
    end;
  end;

FUNCTION T_highlightingData.isUserRule(CONST id: string): boolean;
  begin
    enterCriticalSection(highlightingCs);
    try
      result:=userRules.contains(id);
    finally
      leaveCriticalSection(highlightingCs);
    end;
  end;

FUNCTION T_highlightingData.isErrorLocation(CONST lineIndex, tokenStart, tokenEnd: longint): byte;
  VAR k:longint;
  begin
    enterCriticalSection(highlightingCs);
    try
      result:=0;
      for k:=0 to length(warnLocations)-1 do with warnLocations[k] do
      if (result=0) and (lineIndex=line-1) and ((column<0) or (tokenStart<=column-1) and (tokenEnd>column-1)) then begin
        if isError then begin
          leaveCriticalSection(highlightingCs);
          exit(2);
        end
        else if result<1 then result:=1;
      end;
    finally
      leaveCriticalSection(highlightingCs);
    end;
  end;

FUNCTION T_highlightingData.isLocalId(CONST id: string; CONST lineIndex, colIdx: longint): boolean;
  VAR dummyLocation:T_tokenLocation;
  begin
    enterCriticalSection(highlightingCs);
    try
      result:=localIdInfos.localTypeOf(id,lineIndex,colIdx,dummyLocation)=tt_blockLocalVariable;
    finally
      leaveCriticalSection(highlightingCs);
    end;
  end;

PROCEDURE T_highlightingData.clearLocalIdInfos;
  begin
    enterCriticalSection(highlightingCs);
    try
      localIdInfos.clear;
    finally
      leaveCriticalSection(highlightingCs);
    end;
  end;

CONSTRUCTOR T_codeAssistanceResponse.create(CONST package_:P_package; CONST messages:T_storedMessages; CONST stateHash_:T_hashInt; CONST localIdInfos_:P_localIdInfos);
  VAR m:P_storedMessage;
      level:longint;
  begin
    initCriticalSection(responseCs);
    enterCriticalSection(responseCs);
    try
      referenceCount:=1;
      package:=package_;
      responseStateHash:=stateHash_;
      localIdInfos:=localIdInfos_;

      setLength(localErrors,0);
      setLength(externalErrors,0);
      for level:=4 downto 1 do for m in messages do
      if C_messageTypeMeta[m^.messageType].level=level then begin
        if m^.getLocation.fileName=package_^.getPath
        then begin
          setLength(localErrors,length(localErrors)+1);
          localErrors[length(localErrors)-1]:=m^.rereferenced;
        end else begin
          setLength(externalErrors,length(externalErrors)+1);
          externalErrors[length(externalErrors)-1]:=m^.rereferenced;
        end;
      end;
    finally
      leaveCriticalSection(responseCs);
    end;
  end;

FUNCTION T_codeAssistanceResponse.updateCompletionList(VAR wordsInEditor: T_setOfString; CONST lineIndex, colIdx: longint): boolean;
  VAR s:string;
  begin
    enterCriticalSection(responseCs);
    try
      package^.updateLists(wordsInEditor,true);
      for s in localIdInfos^.allLocalIdsAt(lineIndex,colIdx) do wordsInEditor.put(s);
      result:=(package^.packageRules .size>0) or (package^.importedRules.size>0) or not(localIdInfos^.isEmpty);
    finally
      leaveCriticalSection(responseCs);
    end;
  end;

FUNCTION T_codeAssistanceResponse.explainIdentifier(CONST fullLine: ansistring; CONST CaretY, CaretX: longint; VAR info: T_tokenInfo):boolean;
  VAR lexer:T_lexer;
      loc:T_tokenLocation;
      enhanced:T_enhancedTokens;
  begin
    enterCriticalSection(responseCs);
    try
      loc.line:=CaretY;
      loc.column:=1;
      loc.package:=package;
      result:=(fullLine<>info.fullLine) or (CaretX<>info.CaretX);
      if result then begin
        lexer.create(fullLine,loc,package);
        enhanced:=lexer.getEnhancedTokens(localIdInfos);
        info:=enhanced.getTokenAtIndex(CaretX).toInfo;
        info.fullLine:=fullLine;
        info.CaretX:=CaretX;
        enhanced.destroy;
        lexer.destroy;
      end;
    finally
      leaveCriticalSection(responseCs);
    end;
  end;

FUNCTION T_codeAssistanceResponse.renameIdentifierInLine(CONST location: T_searchTokenLocation; CONST oldId,newId: string; VAR lineText: ansistring; CONST CaretY: longint): boolean;
  VAR lexer:T_lexer;
      loc:T_tokenLocation;
      enhanced:T_enhancedTokens;
  begin
    enterCriticalSection(responseCs);
    try
      loc.line:=CaretY;
      loc.column:=1;
      loc.package:=package;
      lexer.create(lineText,loc,package);
      enhanced:=lexer.getEnhancedTokens(localIdInfos);
      result:=enhanced.renameInLine(lineText,location,oldId,newId);
      enhanced.destroy;
      lexer.destroy;
    finally
      leaveCriticalSection(responseCs);
    end;
  end;

FUNCTION T_codeAssistanceResponse.resolveImport(CONST id:string):string;
  begin
    enterCriticalSection(responseCs);
    try
      if package=nil then result:=''
                     else result:=package^.getSecondaryPackageById(id);
    finally
      leaveCriticalSection(responseCs);
    end;
  end;

FUNCTION T_codeAssistanceResponse.getImportablePackages: T_arrayOfString;
  begin
    enterCriticalSection(responseCs);
    try
      result:=listScriptIds(extractFilePath(package^.getPath));
    finally
      leaveCriticalSection(responseCs);
    end;
  end;

DESTRUCTOR T_codeAssistanceResponse.destroy;
  VAR m:P_storedMessage;
  begin
    enterCriticalSection(responseCs);
    try
      for m in localErrors do disposeMessage_(m);
      setLength(localErrors,0);
      for m in externalErrors do disposeMessage_(m);
      setLength(externalErrors,0);
      dispose(localIdInfos,destroy);
      dispose(package,destroy);
    finally
      leaveCriticalSection(responseCs);
      doneCriticalSection(responseCs);
    end;
  end;

PROCEDURE T_codeAssistanceResponse.getErrorHints(VAR edit:TSynEdit; OUT hasErrors, hasWarnings: boolean);
  VAR lengthLimit:longint;

  PROCEDURE splitAtSpace(VAR headOrAll:string; OUT tail:string; CONST dontSplitBefore,lengthLimit:longint);
    VAR splitIndex:longint;
    begin
      if length(headOrAll)<lengthLimit then begin
        tail:='';
        exit;
      end;
      splitIndex:=lengthLimit;
      while (splitIndex>dontSplitBefore) and (headOrAll[splitIndex]<>' ') do dec(splitIndex);
      if splitIndex<=dontSplitBefore then begin
        splitIndex:=lengthLimit;
        while (splitIndex<=length(headOrAll)) and (headOrAll[splitIndex]<>' ') do inc(splitIndex);
      end;
      tail:=copy(headOrAll,splitIndex,length(headOrAll));
      headOrAll:=copy(headOrAll,1,splitIndex-1);
    end;

  PROCEDURE addErrors(CONST list:T_storedMessages);
    VAR s,head,rest:string;
        m:P_storedMessage;
    begin
      for m in list do begin
        hasErrors  :=hasErrors   or (C_messageTypeMeta[m^.messageType].level> 2);
        hasWarnings:=hasWarnings or (C_messageTypeMeta[m^.messageType].level<=2);
        for s in m^.messageText do begin
          head:=ansistring(m^.getLocation);
          if length(head)>=lengthLimit-3 then begin
            edit.lines.append(C_messageTypeMeta[m^.messageType].guiMarker+head);
            head:='. '+s;
          end else head:=head+' '+s;
          repeat
            splitAtSpace(head,rest,3,lengthLimit);
            edit.lines.append(C_messageTypeMeta[m^.messageType].guiMarker+head);
            head:='. '+rest;
          until rest='';
        end;
      end;
    end;

  begin
    enterCriticalSection(responseCs);
    try
      edit.clearAll;
      edit.lines.clear;
      lengthLimit:=edit.charsInWindow;
      if lengthLimit<20 then lengthLimit:=20;
      hasErrors:=false;
      hasWarnings:=false;
      addErrors(localErrors);
      addErrors(externalErrors);
    finally
      leaveCriticalSection(responseCs);
    end;
  end;

FUNCTION T_codeAssistanceResponse.rereferenced:P_codeAssistanceResponse;
  begin
    enterCriticalSection(responseCs);
    try
      interLockedIncrement(referenceCount);
      result:=@self;
    finally
      leaveCriticalSection(responseCs);
    end;
  end;

VAR isFinalized:boolean=false;
PROCEDURE finalizeCodeAssistance;
  begin
    enterCriticalSection(codeAssistanceCs);
    shuttingDown:=true;
    while codeAssistantIsRunning do begin
      leaveCriticalSection(codeAssistanceCs);
      sleep(1); ThreadSwitch;
      enterCriticalSection(codeAssistanceCs);
    end;
    doneCriticalSection(codeAssistanceCs);
    disposeCodeAssistanceResponse(codeAssistanceResponse);
    if (codeAssistanceRequest<>nil) and codeAssistanceRequest^.disposeOnPackageDestruction
    then dispose(codeAssistanceRequest,destroy);
    codeAssistanceRequest:=nil;
    isFinalized:=true;
  end;

INITIALIZATION
  initialize(codeAssistanceCs);
  initCriticalSection(codeAssistanceCs);
FINALIZATION
  if not(isFinalized) then finalizeCodeAssistance;

end.

