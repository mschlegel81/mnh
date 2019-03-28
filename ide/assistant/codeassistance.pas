UNIT codeAssistance;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, SynEdit,ideLayoutUtil,
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
    warnLocations:array of record line,column:longint; isError:boolean; end;
    userRules    :T_setOfString;
    localIdInfos :T_localIdInfos;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE clear;
    FUNCTION isUserRule(CONST id: string): boolean;
    FUNCTION isErrorLocation(CONST lineIndex, tokenStart, tokenEnd: longint): byte;
    FUNCTION isLocalId(CONST id: string; CONST lineIndex, colIdx: longint): boolean;
  end;

  P_codeAssistanceResponse=^T_codeAssistanceResponse;
  T_codeAssistanceResponse=object
    private
      referenceCount:longint;
      localErrors,externalErrors:T_storedMessages;
      localIdInfos:P_localIdInfos;
      CONSTRUCTOR create(CONST package_:P_package; CONST messages:T_storedMessages; CONST stateHash_:T_hashInt; CONST localIdInfos_:P_localIdInfos);
      DESTRUCTOR destroy;
    public
      package:P_package;
      stateHash:T_hashInt;
      FUNCTION  updateCompletionList(VAR wordsInEditor:T_setOfString; CONST lineIndex, colIdx: longint):boolean;
      PROCEDURE updateHighlightingData(VAR highlightingData:T_highlightingData);
      PROCEDURE explainIdentifier(CONST fullLine: ansistring; CONST CaretY, CaretX: longint; VAR info: T_tokenInfo);
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
      new(globals,create(@adapters));
      adapters.createErrorHolder(nil,C_errorsAndWarnings);
    end else begin
      globals:=givenGlobals;
      givenAdapters^.clear;
    end;
    new(package,create(source,nil));
    initialStateHash:=source^.stateHash;
    globals^.resetForEvaluation(package,ect_silent,C_EMPTY_STRING_ARRAY,recycler);
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
      lastEvaluationStart:double;
  FUNCTION isShutdownRequested:boolean;
    begin
      enterCriticalSection(codeAssistanceCs);
      result:=shuttingDown;
      leaveCriticalSection(codeAssistanceCs);
    end;

  FUNCTION timeForNextCheck:boolean;
    CONST ONE_SECOND=1/(24*60*60);
    begin
      if (now<lastEvaluationStart+ONE_SECOND) then exit(false);
      enterCriticalSection(codeAssistanceCs);
      result:=shuttingDown or
              (codeAssistanceRequest<>nil) and
              ((codeAssistanceResponse=nil) or
               (codeAssistanceResponse^.package^.getCodeProvider<>codeAssistanceRequest) or
               (codeAssistanceResponse^.stateHash<>codeAssistanceRequest^.stateHash));
      leaveCriticalSection(codeAssistanceCs);
    end;
  VAR recycler:T_recycler;
  begin
    //TODO: Can this be simplified? We just need all messages...

    //setup:
    adapters.createErrorHolder(nil,C_errorsAndWarnings);
    new(globals,create(@adapters));
    recycler.initRecycler;
    //:setup
    repeat
      enterCriticalSection(codeAssistanceCs);
      provider:=codeAssistanceRequest;
      codeAssistanceRequest:=nil;
      leaveCriticalSection(codeAssistanceCs);
      if provider<>nil then begin;
        lastEvaluationStart:=now;
        response:=doCodeAssistanceSynchronously(provider,recycler,globals,@adapters);
        enterCriticalSection(codeAssistanceCs);
        disposeCodeAssistanceResponse(codeAssistanceResponse);
        codeAssistanceResponse:=response;
        leaveCriticalSection(codeAssistanceCs);
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
    if (codeAssistanceResponse<>nil) and (codeAssistanceResponse^.package^.getCodeProvider=source) then begin
      result:=codeAssistanceResponse;
      interLockedIncrement(result^.referenceCount);
    end else result:=nil;
    leaveCriticalSection(codeAssistanceCs);
  end;

PROCEDURE postCodeAssistanceRequest(CONST source: P_codeProvider);
  begin
    enterCriticalSection(codeAssistanceCs);
    codeAssistanceRequest:=source;
    if not(codeAssistantIsRunning) then begin
      beginThread(@codeAssistanceThread);
      codeAssistantIsRunning:=true;
    end;
    leaveCriticalSection(codeAssistanceCs);
  end;

PROCEDURE T_codeAssistanceResponse.updateHighlightingData(VAR highlightingData: T_highlightingData);
  VAR k:longint;
      e:P_storedMessage;
  begin
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
  end;

CONSTRUCTOR T_highlightingData.create;
  begin
    userRules   .create;
    localIdInfos.create;
    setLength(warnLocations,0);
  end;

PROCEDURE T_highlightingData.clear;
  begin
    userRules   .clear;
    localIdInfos.clear;
    setLength(warnLocations,0);
  end;

DESTRUCTOR T_highlightingData.destroy;
  begin
    userRules   .destroy;
    localIdInfos.destroy;
    setLength(warnLocations,0);
  end;

FUNCTION T_highlightingData.isUserRule(CONST id: string): boolean;
  begin
    result:=userRules.contains(id);
  end;

FUNCTION T_highlightingData.isErrorLocation(CONST lineIndex, tokenStart, tokenEnd: longint): byte;
  VAR k:longint;
  begin
    result:=0;
    for k:=0 to length(warnLocations)-1 do with warnLocations[k] do
    if (result=0) and (lineIndex=line-1) and ((column<0) or (tokenStart<=column-1) and (tokenEnd>column-1)) then begin
      if isError then exit(2)
      else if result<1 then result:=1;
    end;
  end;

FUNCTION T_highlightingData.isLocalId(CONST id: string; CONST lineIndex, colIdx: longint): boolean;
  VAR dummyLocation:T_tokenLocation;
  begin
    result:=localIdInfos.localTypeOf(id,lineIndex,colIdx,dummyLocation)=tt_blockLocalVariable;
  end;

CONSTRUCTOR T_codeAssistanceResponse.create(CONST package_:P_package; CONST messages:T_storedMessages; CONST stateHash_:T_hashInt; CONST localIdInfos_:P_localIdInfos);
  VAR m:P_storedMessage;
      level:longint;
  begin
    referenceCount:=1;
    package:=package_;
    stateHash:=stateHash_;
    localIdInfos:=localIdInfos_;

    setLength(localErrors,0);
    setLength(externalErrors,0);
    for level:=4 downto 2 do for m in messages do
    if C_messageTypeMeta[m^.messageType].level=level then begin
      if m^.getLocation.fileName=package_^.getPath
      then begin
        setLength(localErrors,length(localErrors)+1);
        localErrors[length(localErrors)-1]:=m;
      end else begin
        setLength(externalErrors,length(externalErrors)+1);
        externalErrors[length(externalErrors)-1]:=m;
      end;
    end;
  end;

FUNCTION T_codeAssistanceResponse.updateCompletionList(VAR wordsInEditor: T_setOfString; CONST lineIndex, colIdx: longint): boolean;
  VAR s:string;
  begin
    package^.updateLists(wordsInEditor,true);
    for s in localIdInfos^.allLocalIdsAt(lineIndex,colIdx) do wordsInEditor.put(s);
    result:=(package^.packageRules .size>0) or (package^.importedRules.size>0) or not(localIdInfos^.isEmpty);
  end;

PROCEDURE T_codeAssistanceResponse.explainIdentifier(
  CONST fullLine: ansistring; CONST CaretY, CaretX: longint;
  VAR info: T_tokenInfo);
  VAR lexer:T_lexer;
      loc:T_tokenLocation;
      enhanced:T_enhancedTokens;
  begin
    loc.line:=CaretY;
    loc.column:=1;
    loc.package:=package;

    lexer.create(fullLine,loc,package);
    enhanced:=lexer.getEnhancedTokens(localIdInfos);
    info:=enhanced.getTokenAtIndex(CaretX).toInfo;
    enhanced.destroy;
    lexer.destroy;
  end;

FUNCTION T_codeAssistanceResponse.renameIdentifierInLine(CONST location: T_searchTokenLocation; CONST oldId,newId: string; VAR lineText: ansistring; CONST CaretY: longint): boolean;
  VAR lexer:T_lexer;
      loc:T_tokenLocation;
      enhanced:T_enhancedTokens;
  begin
    loc.line:=CaretY;
    loc.column:=1;
    loc.package:=package;
    lexer.create(lineText,loc,package);
    enhanced:=lexer.getEnhancedTokens(localIdInfos);
    result:=enhanced.renameInLine(lineText,location,oldId,newId);
    enhanced.destroy;
    lexer.destroy;
  end;

FUNCTION T_codeAssistanceResponse.resolveImport(CONST id:string):string;
  begin
    if package=nil then result:=''
                   else result:=package^.getSecondaryPackageById(id);
  end;

FUNCTION T_codeAssistanceResponse.getImportablePackages: T_arrayOfString;
  begin
    result:=listScriptIds(extractFilePath(package^.getPath));
  end;

DESTRUCTOR T_codeAssistanceResponse.destroy;
  begin
    setLength(localErrors,0);
    setLength(externalErrors,0);
    dispose(localIdInfos,destroy);
    dispose(package,destroy);
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
    edit.clearAll;
    edit.lines.clear;
    lengthLimit:=edit.charsInWindow;
    if lengthLimit<20 then lengthLimit:=20;
    hasErrors:=false;
    hasWarnings:=false;
    addErrors(localErrors);
    addErrors(externalErrors);
  end;

FUNCTION T_codeAssistanceResponse.rereferenced:P_codeAssistanceResponse;
  begin
    interLockedIncrement(referenceCount);
    result:=@self;
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
    isFinalized:=true;
  end;

INITIALIZATION
  initialize(codeAssistanceCs);
  initCriticalSection(codeAssistanceCs);
FINALIZATION
  if not(isFinalized) then finalizeCodeAssistance;

end.

