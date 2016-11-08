{$ifdef includeInterface}
PROCEDURE FormDropFiles(Sender: TObject; CONST FileNames: array of string);
PROCEDURE miClearClick(Sender: TObject);
PROCEDURE miCloseClick(Sender: TObject);
PROCEDURE miCloseAllButCurrentClick(Sender: TObject);
PROCEDURE miCloseAllUnmodifiedClick(Sender: TObject);
PROCEDURE openFromHistory(CONST historyIdx:byte);
PROCEDURE miFileHistory0Click(Sender: TObject);
PROCEDURE miFileHistory1Click(Sender: TObject);
PROCEDURE miFileHistory2Click(Sender: TObject);
PROCEDURE miFileHistory3Click(Sender: TObject);
PROCEDURE miFileHistory4Click(Sender: TObject);
PROCEDURE miFileHistory5Click(Sender: TObject);
PROCEDURE miFileHistory6Click(Sender: TObject);
PROCEDURE miFileHistory7Click(Sender: TObject);
PROCEDURE miFileHistory8Click(Sender: TObject);
PROCEDURE miFileHistory9Click(Sender: TObject);
PROCEDURE miFileHistory10Click(Sender: TObject);
PROCEDURE miFileHistory11Click(Sender: TObject);
PROCEDURE miFileHistory12Click(Sender: TObject);
PROCEDURE miFileHistory13Click(Sender: TObject);
PROCEDURE miFileHistory14Click(Sender: TObject);
PROCEDURE miFileHistory15Click(Sender: TObject);
PROCEDURE miFileHistory16Click(Sender: TObject);
PROCEDURE miFileHistory17Click(Sender: TObject);
PROCEDURE miFileHistory18Click(Sender: TObject);
PROCEDURE miFileHistory19Click(Sender: TObject);
PROCEDURE miOpenClick(Sender: TObject);
PROCEDURE miOpenImportedClick(Sender: TObject);
PROCEDURE miSaveAsClick(Sender: TObject);
PROCEDURE miSaveClick(Sender: TObject);

PROCEDURE inputEditReposition(CONST caret:TPoint; CONST doJump,updateMarker:boolean);
PROCEDURE outputEditReposition(CONST caret:TPoint; CONST doJump:boolean);
PROCEDURE assistanceEditReposition(CONST caret:TPoint; CONST doJump:boolean);
FUNCTION _doSaveAs_(CONST index:longint):boolean;
FUNCTION _doSave_(CONST index:longint):boolean;
FUNCTION addEditorMetaForNewFile(CONST newFileName: ansistring=''):longint;
FUNCTION addOrGetEditorMetaForFile(CONST fileName: ansistring):longint;
FUNCTION editForSearch(CONST replacing:boolean):TSynEdit;
PROCEDURE processFileHistory;
PROCEDURE pmiOpenFile(CONST idOrName:string);
PROCEDURE pmiOpenFile1Click(Sender: TObject);
PROCEDURE pmiOpenFile2Click(Sender: TObject);

PROCEDURE InputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
PROCEDURE assistanceSynEditKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
PROCEDURE OutputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);

PROCEDURE InputEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
PROCEDURE assistanceSynEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
PROCEDURE OutputEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);

PROCEDURE assistanceSynEditMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
PROCEDURE OutputEditMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
PROCEDURE miEditScriptFileClick(Sender: TObject);
{$endif}
{$ifdef includeImplementation}
PROCEDURE TMnhForm.FormDropFiles(Sender: TObject; CONST FileNames: array of string);
  VAR i:longint;
  begin
    for i:=0 to length(FileNames)-1 do inputPageControl.activePageIndex:=addOrGetEditorMetaForFile(FileNames[i]);
  end;

PROCEDURE TMnhForm.miClearClick(Sender: TObject);
  begin
    inputPageControl.activePageIndex:=addEditorMetaForNewFile();
  end;

PROCEDURE TMnhForm.miCloseClick(Sender: TObject);
  VAR i,mr:longint;
  begin
    if (inputPageControl.activePageIndex<0) or (inputPageControl.activePageIndex>=length(editorMeta)) then exit;
    with editorMeta[inputPageControl.activePageIndex] do begin
      if changed then begin
        mr:=closeDialogForm.showOnLoad;
        if mr=mrOk then if not(_doSave_(inputPageControl.activePageIndex)) then exit;
        if mr=mrCancel then exit;
      end;
      if isFile then begin
        settings.value^.workspace.fileHistory.fileClosed(fileInfo.filePath);
        processFileHistory;
      end;
      closeEditor;
    end;

    mr:=-1;
    for i:=0 to length(editorMeta)-1 do if editorMeta[i].sheet.tabVisible then mr:=i;
    if mr=-1 then inputPageControl.activePageIndex:=addEditorMetaForNewFile()
             else inputPageControl.activePageIndex:=mr;
  end;

PROCEDURE TMnhForm.miCloseAllButCurrentClick(Sender: TObject);
  VAR mr,pageIdx:longint;
  begin
    if (inputPageControl.activePageIndex<0) or (inputPageControl.activePageIndex>=length(editorMeta)) then exit;
    for pageIdx:=0 to length(editorMeta)-1 do if (pageIdx<>inputPageControl.activePageIndex) and (editorMeta[pageIdx].sheet.tabVisible) then
    with editorMeta[pageIdx] do begin
      if changed then begin
        mr:=closeDialogForm.showOnLoad;
        if mr=mrOk then if not(_doSave_(pageIdx)) then exit;
        if mr=mrCancel then exit;
      end;
      if isFile then begin
        settings.value^.workspace.fileHistory.fileClosed(fileInfo.filePath);
        processFileHistory;
      end;
      closeEditor;
    end;
  end;

PROCEDURE TMnhForm.miCloseAllUnmodifiedClick(Sender: TObject);
  VAR i,pageIdx:longint;
      activePageClosed:boolean=false;
  begin
    if (inputPageControl.activePageIndex<0) or (inputPageControl.activePageIndex>=length(editorMeta)) then exit;
    for pageIdx:=0 to length(editorMeta)-1 do if (pageIdx<>inputPageControl.activePageIndex) and (editorMeta[pageIdx].sheet.tabVisible) and not(editorMeta[pageIdx].changed) then
    with editorMeta[pageIdx] do begin
      if isFile then begin
        settings.value^.workspace.fileHistory.fileClosed(fileInfo.filePath);
        processFileHistory;
      end;
      closeEditor;
      activePageClosed:=activePageClosed or (pageIdx=inputPageControl.activePageIndex);
    end;
    if activePageClosed then begin
      pageIdx:=-1;
      for i:=0 to length(editorMeta)-1 do if editorMeta[i].sheet.tabVisible then pageIdx:=i;
      if pageIdx=-1 then inputPageControl.activePageIndex:=addEditorMetaForNewFile()
                    else inputPageControl.activePageIndex:=pageIdx;
    end;
  end;

PROCEDURE TMnhForm.openFromHistory(CONST historyIdx: byte);
  begin
    with settings.value^.workspace.fileHistory do begin
      if fileExists(historyItem(historyIdx))
      then inputPageControl.activePageIndex:=addOrGetEditorMetaForFile(historyItem(historyIdx))
      else if polishHistory then processFileHistory;
    end;
  end;

PROCEDURE TMnhForm.miFileHistory0Click(Sender: TObject); begin openFromHistory(0); end;
PROCEDURE TMnhForm.miFileHistory1Click(Sender: TObject); begin openFromHistory(1); end;
PROCEDURE TMnhForm.miFileHistory2Click(Sender: TObject); begin openFromHistory(2); end;
PROCEDURE TMnhForm.miFileHistory3Click(Sender: TObject); begin openFromHistory(3); end;
PROCEDURE TMnhForm.miFileHistory4Click(Sender: TObject); begin openFromHistory(4); end;
PROCEDURE TMnhForm.miFileHistory5Click(Sender: TObject); begin openFromHistory(5); end;
PROCEDURE TMnhForm.miFileHistory6Click(Sender: TObject); begin openFromHistory(6); end;
PROCEDURE TMnhForm.miFileHistory7Click(Sender: TObject); begin openFromHistory(7); end;
PROCEDURE TMnhForm.miFileHistory8Click(Sender: TObject); begin openFromHistory(8); end;
PROCEDURE TMnhForm.miFileHistory9Click(Sender: TObject); begin openFromHistory(9); end;
PROCEDURE TMnhForm.miFileHistory10Click(Sender: TObject); begin openFromHistory(10); end;
PROCEDURE TMnhForm.miFileHistory11Click(Sender: TObject); begin openFromHistory(11); end;
PROCEDURE TMnhForm.miFileHistory12Click(Sender: TObject); begin openFromHistory(12); end;
PROCEDURE TMnhForm.miFileHistory13Click(Sender: TObject); begin openFromHistory(13); end;
PROCEDURE TMnhForm.miFileHistory14Click(Sender: TObject); begin openFromHistory(14); end;
PROCEDURE TMnhForm.miFileHistory15Click(Sender: TObject); begin openFromHistory(15); end;
PROCEDURE TMnhForm.miFileHistory16Click(Sender: TObject); begin openFromHistory(16); end;
PROCEDURE TMnhForm.miFileHistory17Click(Sender: TObject); begin openFromHistory(17); end;
PROCEDURE TMnhForm.miFileHistory18Click(Sender: TObject); begin openFromHistory(18); end;
PROCEDURE TMnhForm.miFileHistory19Click(Sender: TObject); begin openFromHistory(19); end;

PROCEDURE TMnhForm.inputEditReposition(CONST caret: TPoint; CONST doJump,updateMarker: boolean);
  VAR wordUnderCursor:string;
      newCaret:TPoint;
      pageIdx:longint;
  begin
    with editorMeta[inputPageControl.activePageIndex] do begin
      wordUnderCursor:=editor.GetWordAtRowCol(caret);
      setUnderCursor(wordUnderCursor,updateMarker,doJump);
      if not(doJump) then exit;
      if (underCursor.tokenText<>wordUnderCursor) or
         (underCursor.location.column<=0) then exit;
      if (underCursor.location.fileName='') or (underCursor.location.fileName='?') then exit;
      pageIdx:=addOrGetEditorMetaForFile(underCursor.location.fileName);
      if pageIdx>=0 then begin
        inputPageControl.activePageIndex:=pageIdx;
        newCaret.x:=underCursor.location.column;
        newCaret.y:=underCursor.location.line;
        editorMeta[pageIdx].editor.CaretXY:=newCaret;
      end;
    end;
  end;

PROCEDURE TMnhForm.outputEditReposition(CONST caret: TPoint;
  CONST doJump: boolean);
  VAR loc:T_searchTokenLocation;
      newCaret:TPoint;
      pageIdx:longint;
  begin
    forceInputEditFocusOnOutputEditMouseUp:=false;
    setUnderCursor(OutputEdit.GetWordAtRowCol(caret),true,doJump);
    loc:=guessLocationFromString(OutputEdit.lines[caret.y-1],false);
    if not(doJump) then exit;
    if reEvaluationWithGUIrequired then begin
      if runEvaluator.evaluationRunning
      then exit
      else setEditorMode(true);
    end;
    if (loc.fileName='') or (loc.fileName='?') then exit;
    pageIdx:=addOrGetEditorMetaForFile(loc.fileName);
    if pageIdx<0 then exit;
    inputPageControl.activePageIndex:=pageIdx;
    with editorMeta[pageIdx] do begin
      editor.SetFocus;
      highlighter.setMarkedToken(loc.line-1,loc.column-1);
      newCaret.x:=loc.column;
      newCaret.y:=loc.line;
      editor.CaretXY:=newCaret;
      forceInputEditFocusOnOutputEditMouseUp:=true;
    end;
  end;

PROCEDURE TMnhForm.assistanceEditReposition(CONST caret: TPoint; CONST doJump: boolean);
  VAR loc:T_searchTokenLocation;
      newCaret:TPoint;
      pageIdx:longint;
  begin
    forceInputEditFocusOnOutputEditMouseUp:=false;
    setUnderCursor(assistanceSynEdit.GetWordAtRowCol(caret),true,doJump);
    loc:=guessLocationFromString(assistanceSynEdit.lines[caret.y-1],false);
    if not(doJump) then exit;
    if (loc.fileName='') or (loc.fileName='?') then exit;
    pageIdx:=addOrGetEditorMetaForFile(loc.fileName);
    if pageIdx<0 then exit;
    inputPageControl.activePageIndex:=pageIdx;
    with editorMeta[pageIdx] do begin
      editor.SetFocus;
      highlighter.setMarkedToken(loc.line-1,loc.column-1);
      newCaret.x:=loc.column;
      newCaret.y:=loc.line;
      editor.CaretXY:=newCaret;
      forceInputEditFocusOnOutputEditMouseUp:=true;
    end;
  end;

FUNCTION TMnhForm._doSaveAs_(CONST index: longint): boolean;
  begin
    if index<0 then exit(false);
    if SaveDialog.execute then with editorMeta[index] do begin
      caption:=saveFile(SaveDialog.fileName);
      result:=true;
    end else result:=false;
  end;

FUNCTION TMnhForm._doSave_(CONST index: longint): boolean;
  begin
    if index<0 then exit(false);
    with editorMeta[index] do if not(isFile) then result:=_doSaveAs_(index)
    else begin
      caption:=saveFile();
      result:=true;
    end;
  end;

FUNCTION TMnhForm.addEditorMetaForNewFile(CONST newFileName: ansistring): longint;
  VAR i:longint;
  begin
    i:=length(editorMeta)-1;
    //decrease i until a visible meta is encountered
    while (i>=0) and not(editorMeta[i].sheet.tabVisible) do dec(i);
    inc(i);
    //i now is the index of the last visible editor meta +1
    if (i>=0) and (i<length(editorMeta)) then begin
      editorMeta[i].initForNewFile;
      exit(i);
    end;

    i:=length(editorMeta)-1;
    while (i>=0) and (editorMeta[i].sheet.visible) do dec(i);
    i:=length(editorMeta);
    setLength(editorMeta,i+1);
    editorMeta[i].create(i);
    editorMeta[i].editor.Font:=OutputEdit.Font;
    if newFileName<>'' then _doSave_(i);
    result:=i;
    if miDebug.Checked then editorMeta[i].editor.Gutter.MarksPart.visible:=true;
  end;

FUNCTION TMnhForm.addOrGetEditorMetaForFile(CONST fileName: ansistring): longint;
  FUNCTION isPseudoName:boolean;
    begin
      result:=(length(fileName)>1)
          and (fileName[1]='<')
          and (fileName[length(fileName)]='>');
    end;

  VAR filePath:ansistring;
      i:longint;
  begin
    if isPseudoName then begin
      for i:=0 to length(editorMeta)-1 do if (editorMeta[i].sheet.tabVisible) and (editorMeta[i].pseudoName=fileName) then exit(i);
      result:=-1;
    end else begin
      filePath:=expandFileName(fileName);
      for i:=0 to length(editorMeta)-1 do if (editorMeta[i].sheet.tabVisible) and (editorMeta[i].fileInfo.filePath=filePath) then exit(i);
      result:=addEditorMetaForNewFile();
      editorMeta[result].setFile(filePath);
      editorMeta[result].editor.Font:=OutputEdit.Font;
      enableMenuForLanguage(editorMeta[result].language);
    end;
  end;

FUNCTION TMnhForm.editForSearch(CONST replacing: boolean): TSynEdit;
  begin
    if outputFocusedOnFind and not(replacing) then exit(OutputEdit);
    if (inputPageControl.activePageIndex>=0) and (inputPageControl.activePageIndex<length(editorMeta))
    then result:=editorMeta[inputPageControl.activePageIndex].editor
    else exit(OutputEdit); //not nice, but a valid fallback
  end;

PROCEDURE TMnhForm.miOpenClick(Sender: TObject);
  begin
    OpenDialog.FilterIndex:=1;
    OpenDialog.options:=OpenDialog.options+[ofPathMustExist,ofFileMustExist];
    OpenDialog.title:='Open file';
    if OpenDialog.execute and fileExists(OpenDialog.fileName)
    then inputPageControl.activePageIndex:=addOrGetEditorMetaForFile(OpenDialog.fileName);
  end;

PROCEDURE TMnhForm.miOpenImportedClick(Sender: TObject);
  begin
    if (inputPageControl.PageIndex>=0) and
       (inputPageControl.PageIndex<length(editorMeta)) and
       (editorMeta[inputPageControl.PageIndex].language=LANG_MNH) then
    FormDropFiles(Sender,assistancEvaluator.getAllUsedFiles);
  end;


PROCEDURE TMnhForm.miSaveAsClick(Sender: TObject);
  begin
    _doSaveAs_(inputPageControl.activePageIndex);
  end;

PROCEDURE TMnhForm.miSaveClick(Sender: TObject);
  begin
    _doSave_(inputPageControl.activePageIndex);
  end;

PROCEDURE TMnhForm.processFileHistory;
  FUNCTION historyMenuItem(index:byte):TMenuItem;
    begin
      case index of
        0: result:=miFileHistory0;
        1: result:=miFileHistory1;
        2: result:=miFileHistory2;
        3: result:=miFileHistory3;
        4: result:=miFileHistory4;
        5: result:=miFileHistory5;
        6: result:=miFileHistory6;
        7: result:=miFileHistory7;
        8: result:=miFileHistory8;
        9: result:=miFileHistory9;
        10: result:=miFileHistory10;
        11: result:=miFileHistory11;
        12: result:=miFileHistory12;
        13: result:=miFileHistory13;
        14: result:=miFileHistory14;
        15: result:=miFileHistory15;
        16: result:=miFileHistory16;
        17: result:=miFileHistory17;
        18: result:=miFileHistory18;
        19: result:=miFileHistory19;
      else result:=nil;
      end;
    end;
  VAR i:longint;
  begin
    for i:=0 to 19 do if settings.value^.workspace.fileHistory.historyItem(i)='' then begin
      historyMenuItem(i).enabled:=false;
      historyMenuItem(i).visible:=false;
    end else begin
      historyMenuItem(i).enabled:=true;
      historyMenuItem(i).visible:=true;
      historyMenuItem(i).caption:=intToStr(i)+': '+settings.value^.workspace.fileHistory.historyItem(i);
    end;
  end;

PROCEDURE TMnhForm.pmiOpenFile(CONST idOrName:string);
  VAR fileName:string;
  begin
    with settings.value^ do begin
      if fileExists(idOrName)
      then begin
        inputPageControl.activePageIndex:=addOrGetEditorMetaForFile(idOrName);
        exit;
      end;
      if workspace.fileHistory.polishHistory then processFileHistory;
      fileName:=assistancEvaluator.resolveImport(idOrName);
      if (fileName<>'') and fileExists(fileName) then inputPageControl.activePageIndex:=addOrGetEditorMetaForFile(fileName);
    end;
  end;

PROCEDURE TMnhForm.pmiOpenFile1Click(Sender: TObject);
  begin
    pmiOpenFile(popupFile[1]);
  end;

PROCEDURE TMnhForm.pmiOpenFile2Click(Sender: TObject);
  begin
    pmiOpenFile(popupFile[2]);
  end;

PROCEDURE TMnhForm.InputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=13) and ((ssCtrl in Shift) or (ssAlt in Shift))
    then inputEditReposition(editorMeta[inputPageControl.activePageIndex].editor.CaretXY,ssCtrl in Shift,true)
    else inputEditReposition(editorMeta[inputPageControl.activePageIndex].editor.CaretXY,false,false);
    if runEvaluator.context.hasOption(cp_debug) and runEvaluator.evaluationRunning then begin
      if (key=116) and tbRun      .enabled then tbRunClick(Sender);
      if (key=117) and tbStepIn   .enabled then tbStepInClick(Sender);
      if (key=118) and tbStep     .enabled then tbStepClick(Sender);
      if (key=119) and tbStepOut  .enabled then tbStepOutClick(Sender);
      if (key=121) and tbMicroStep.enabled then tbMicroStepClick(Sender);
    end;
  end;

PROCEDURE TMnhForm.assistanceSynEditKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if ((key=13) and (ssCtrl in Shift)) then assistanceEditReposition(assistanceSynEdit.CaretXY,true);
    if forceInputEditFocusOnOutputEditMouseUp and (inputPageControl.activePageIndex>=0) then ActiveControl:=editorMeta[inputPageControl.activePageIndex].editor;
    forceInputEditFocusOnOutputEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.OutputEditKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if ((key=13) and (ssCtrl in Shift)) then outputEditReposition(OutputEdit.CaretXY,true);
    if forceInputEditFocusOnOutputEditMouseUp and (inputPageControl.activePageIndex>=0) then ActiveControl:=editorMeta[inputPageControl.activePageIndex].editor;
    forceInputEditFocusOnOutputEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.InputEditMouseDown(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    inputEditReposition(editorMeta[inputPageControl.activePageIndex].editor.PixelsToRowColumn(point),ssCtrl in Shift,true);
  end;

PROCEDURE TMnhForm.assistanceSynEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    assistanceEditReposition(assistanceSynEdit.PixelsToRowColumn(point),ssCtrl in Shift);
  end;

PROCEDURE TMnhForm.OutputEditMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    outputEditReposition(OutputEdit.PixelsToRowColumn(point),ssCtrl in Shift);
  end;

PROCEDURE TMnhForm.assistanceSynEditMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if forceInputEditFocusOnOutputEditMouseUp then ActiveControl:=editorMeta[inputPageControl.activePageIndex].editor;
    forceInputEditFocusOnOutputEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.OutputEditMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if forceInputEditFocusOnOutputEditMouseUp then ActiveControl:=editorMeta[inputPageControl.activePageIndex].editor;
    forceInputEditFocusOnOutputEditMouseUp :=false;
  end;

PROCEDURE TMnhForm.miEditScriptFileClick(Sender: TObject);
  begin
    FormDropFiles(Sender,editScriptFileName);
  end;
{$endif}
