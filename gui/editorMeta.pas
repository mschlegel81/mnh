UNIT editorMeta;
INTERFACE
USES  //basic classes
  Classes, sysutils, LazUTF8, LCLType, types,
  //my utilities:
  myGenerics,
  //GUI: LCL components
  Controls, Graphics, Dialogs, Menus, ComCtrls, StdCtrls,
  //GUI: SynEdit
  SynEdit, SynEditMiscClasses, SynEditMarks,
  //GUI: highlighters
  SynHighlighterMnh,
  SynExportHTML,
  closeDialog,
  mnh_tables,
  mnh_plotForm,
  //MNH:
  outlines,
  mnh_constants, mnh_basicTypes, mnh_fileWrappers,mnh_settings,
  mnh_tokenArray,
  mnh_contexts,
  mnh_litVar,
  mnh_funcs,
  mnh_debugging,
  mnh_cmdLineInterpretation,
  mnh_evalThread,
  mnh_packages,
  guiOutAdapters,
  mnh_datastores,
  editorMetaBase;

TYPE
P_editorMeta=^T_editorMeta;
T_editorMeta=object(T_basicEditorMeta)
  private
    index:longint;
    paintedWithStateHash:T_hashInt;
    fileInfo:record
      filePath:ansistring;
      fileAccessAge:double;
      isChanged:boolean;
      ignoreDeleted:boolean;
    end;
    strictlyReadOnly:boolean;
    assistant   : P_codeAssistanceData;
    tabsheet    : TTabSheet;
    PROCEDURE guessLanguage(CONST fallback:T_language);
    CONSTRUCTOR create(CONST idx:longint);
    CONSTRUCTOR create(CONST idx:longint; VAR state:T_editorState);
  public
    FUNCTION enabled:boolean;
    DESTRUCTOR destroy; virtual;
    FUNCTION getPath:ansistring; virtual;
    FUNCTION isPseudoFile: boolean; virtual;

    PROCEDURE activate; virtual;
    FUNCTION caretInMainFormCoordinates:TPoint;

    PROCEDURE setUnderCursor(CONST updateMarker,forHelpOrJump: boolean; CONST caret:TPoint);
    PROCEDURE setUnderCursor(CONST updateMarker,forHelpOrJump: boolean);
    FUNCTION canRenameUnderCursor(OUT orignalId:string; OUT tokTyp:T_tokenType; OUT ref:T_searchTokenLocation; OUT mightBeUsedElsewhere:boolean):boolean;
    PROCEDURE doRename(CONST ref:T_searchTokenLocation; CONST newId:string; CONST renameInOtherEditors:boolean=false);

    PROCEDURE toggleBreakpoint;
    PROCEDURE setWorkingDir;
    PROCEDURE closeEditorWithDialogs;

    FUNCTION saveAsWithDialog:boolean;
    FUNCTION saveWithDialog:boolean;
    PROCEDURE reloadFile(CONST fileName:string);
    PROCEDURE exportToHtml;
    FUNCTION pseudoName(CONST short:boolean=false):ansistring;
    FUNCTION defaultExtensionByLanguage:ansistring;
    PROCEDURE updateContentAfterEditScript(CONST stringListLiteral:P_listLiteral);
    FUNCTION resolveImport(CONST text:string):string;
    PROCEDURE assignAdditionalHighlighter(CONST additionalHighlighter:TSynMnhSyn);
    PROCEDURE pollAssistanceResult;
  private
    PROCEDURE ensureAssistant;
    PROCEDURE dropAssistant;
    PROCEDURE triggerCheck;

    PROCEDURE initWithState(VAR state:T_editorState);
    PROCEDURE closeEditorQuietly;
    PROCEDURE InputEditChange(Sender: TObject);
    PROCEDURE languageMenuItemClick(Sender: TObject);
    FUNCTION isFile:boolean;
    PROCEDURE setFile(CONST fileName:string);
    PROCEDURE initForNewFile;
    PROCEDURE writeToEditorState(CONST settings:P_Settings);
    PROCEDURE setStepperBreakpoints;
    PROCEDURE _add_breakpoint_(CONST lineIndex:longint);
    FUNCTION updateSheetCaption:ansistring;
    FUNCTION changed:boolean;
    FUNCTION saveFile(CONST fileName:string=''):string;
    FUNCTION fileIsDeleted:boolean;
    FUNCTION fileIsModifiedOnFileSystem:boolean;
end;

T_runnerModel=object
  private
    lastStart:record
      mainCall:boolean;
      parameters:string;
    end;
    debugLine:record
      editor:TSynEdit;
      line:longint;
    end;
    debugMode_:boolean;
    PROCEDURE setDebugMode(CONST value:boolean);
  public
    stackTracing:boolean;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION areEditorsLocked:boolean;
    PROPERTY debugMode:boolean read debugMode_ write setDebugMode;
    FUNCTION canRun(CONST quickMode:boolean=false):boolean;
    PROCEDURE customRun(CONST mainCall,profiling:boolean; CONST mainParameters:string='');
    PROCEDURE rerun(CONST profiling:boolean);
    PROCEDURE InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);
    PROCEDURE doDebuggerAction(CONST newState:T_debuggerState);
    PROCEDURE markDebugLine(CONST editor:TSynEdit; CONST line:longint);
    PROCEDURE haltEvaluation;
  end;

PROCEDURE setupUnit(CONST p_mainForm              :T_abstractMnhForm;
                    CONST p_inputPageControl      :TPageControl;
                    CONST p_SaveDialog            :TSaveDialog;
                    CONST p_breakpointsImagesList :TImageList;
                    CONST p_assistanceSynEdit     :TSynEdit;
                    CONST p_assistanceTabSheet    :TTabSheet;
                    CONST outputHighlighter       :TSynMnhSyn;
                    CONST languageMenuRoot        :TMenuItem;
                    CONST p_EditKeyUp             :TKeyEvent;
                    CONST p_EditMouseDown         :TMouseEvent;
                    CONST p_EditProcessUserCommand:TProcessCommandEvent;
                    CONST p_outlineGroupBox       :TGroupBox;
                    CONST p_outlineTreeView       :TTreeView;
                    CONST p_outlineFilterPrivateCb,p_outlineFilterImportedCb:TCheckBox;
                    CONST p_openlocation          :T_openLocationCallback);
FUNCTION hasEditor:boolean;
FUNCTION getEditor:P_editorMeta;
FUNCTION addEditorMetaForNewFile:longint;
FUNCTION addOrGetEditorMetaForFiles(CONST FileNames: array of string; CONST useCurrentPageAsFallback:boolean):longint;
PROCEDURE updateFonts(CONST Font:TFont);
FUNCTION allPseudoNames:T_arrayOfString;
FUNCTION getMeta(CONST nameOrPseudoName:string):P_editorMeta;
PROCEDURE storeEditorsToSettings;
FUNCTION getHelpPopupText:string;
FUNCTION getHelpLocation:T_searchTokenLocation;
PROCEDURE cycleEditors(CONST cycleForward:boolean);
PROCEDURE updateEditorsByGuiStatus;
PROCEDURE closeAllEditorsButCurrent;
PROCEDURE closeAllUnmodifiedEditors;
PROCEDURE checkForFileChanges;
PROCEDURE finalizeEditorMeta;

FUNCTION getSafeAssistant(CONST editor:P_editorMeta):P_codeAssistanceData;
VAR runnerModel:T_runnerModel;
    recentlyActivated:T_fileHistory;
IMPLEMENTATION
VAR mainForm              :T_abstractMnhForm;
    inputPageControl      :TPageControl;
    SaveDialog            :TSaveDialog;
    breakpointsImagesList :TImageList;
    EditKeyUp             :TKeyEvent;
    EditMouseDown         :TMouseEvent;
    EditProcessUserCommand:TProcessCommandEvent;
    assistanceSynEdit     :TSynEdit;
    assistanceTabSheet    :TTabSheet;

    outlineModel          :P_outlineTreeModel=nil;
    outlineGroupBox       :TGroupBox;
    fallbackCodeAssistant :P_blankCodeAssistanceData=nil;

VAR editorMetaData:array of P_editorMeta;
    underCursor:T_tokenInfo;

PROCEDURE setupUnit(CONST p_mainForm              :T_abstractMnhForm;
                    CONST p_inputPageControl      :TPageControl;
                    CONST p_SaveDialog            :TSaveDialog;
                    CONST p_breakpointsImagesList :TImageList;
                    CONST p_assistanceSynEdit     :TSynEdit;
                    CONST p_assistanceTabSheet    :TTabSheet;
                    CONST outputHighlighter       :TSynMnhSyn;
                    CONST languageMenuRoot        :TMenuItem;
                    CONST p_EditKeyUp             :TKeyEvent;
                    CONST p_EditMouseDown         :TMouseEvent;
                    CONST p_EditProcessUserCommand:TProcessCommandEvent;
                    CONST p_outlineGroupBox       :TGroupBox;
                    CONST p_outlineTreeView       :TTreeView;
                    CONST p_outlineFilterPrivateCb,p_outlineFilterImportedCb:TCheckBox;
                    CONST p_openlocation          :T_openLocationCallback);

  PROCEDURE restoreEditors;
    VAR i:longint;
    begin
      setLength(editorMetaData,length(settings.value^.workspace.editorState));
      for i:=0 to length(editorMetaData)-1 do new(editorMetaData[i],create(i,settings.value^.workspace.editorState[i]));
      i:=settings.value^.workspace.activePage;
      inputPageControl.activePageIndex:=i;
      inputPageControl.activePageIndex:=addOrGetEditorMetaForFiles(filesToOpenInEditor,true);
    end;

  begin
    editorFont:=p_assistanceSynEdit.Font;
    setupEditorMetaBase(p_mainForm,outputHighlighter,languageMenuRoot);

    mainForm              :=p_mainForm              ;
    inputPageControl      :=p_inputPageControl      ;
    SaveDialog            :=p_SaveDialog            ;
    breakpointsImagesList :=p_breakpointsImagesList ;
    EditKeyUp             :=p_EditKeyUp             ;
    EditMouseDown         :=p_EditMouseDown         ;
    EditProcessUserCommand:=p_EditProcessUserCommand;
    assistanceSynEdit     :=p_assistanceSynEdit     ;
    assistanceTabSheet    :=p_assistanceTabSheet    ;
    outlineGroupBox       :=p_outlineGroupBox       ;
    new(outlineModel,create(p_outlineTreeView,p_outlineFilterPrivateCb,p_outlineFilterImportedCb,p_openlocation));

    restoreEditors;
  end;

CONSTRUCTOR T_editorMeta.create(CONST idx: longint);
  begin
    paintedWithStateHash:=0;
    index:=idx;
    tabsheet:=TTabSheet.create(inputPageControl);
    tabsheet.PageControl:=inputPageControl;
    createWithParent(tabsheet);
    paintedWithStateHash:=0;
    index:=idx;
    editor_.Gutter.MarksPart.width:=breakpointsImagesList.width;
    editor_.OnChange            :=@InputEditChange;
    editor_.OnKeyUp             :=EditKeyUp;
    editor_.OnMouseDown         :=EditMouseDown;
    editor_.OnProcessCommand    :=EditProcessUserCommand;
    editor_.OnProcessUserCommand:=EditProcessUserCommand;
    editor_.OnSpecialLineMarkup :=@(runnerModel.InputEditSpecialLineMarkup);
    initForNewFile;
  end;

CONSTRUCTOR T_editorMeta.create(CONST idx: longint; VAR state: T_editorState);
  begin
    create(idx);
    initWithState(state);
  end;

PROCEDURE T_editorMeta.initWithState(VAR state: T_editorState);
  VAR i:longint;
  begin
    tabsheet.tabVisible:=true;
    with fileInfo do begin
      isChanged    :=state.changed;
      fileAccessAge:=state.fileAccessAge;
      filePath     :=state.filePath;
      ignoreDeleted:=false;
      strictlyReadOnly:=state.strictReadOnly;
    end;
    if fileInfo.isChanged
    then state.getLines(editor.lines)
    else setFile(state.filePath);
    for i:=0 to length(state.markedLines)-1 do _add_breakpoint_(state.markedLines[i]);
    editor.CaretX:=state.caret['x'];
    editor.CaretY:=state.caret['y'];
    language_:=T_language(state.language);
    updateSheetCaption;
  end;

DESTRUCTOR T_editorMeta.destroy;
  begin
    inherited destroy;
    dropAssistant;
  end;

FUNCTION T_editorMeta.getPath: ansistring;
  begin
    result:=pseudoName(false);
  end;

FUNCTION T_editorMeta.isPseudoFile: boolean;
  begin
    result:=fileInfo.filePath='';
  end;

PROCEDURE T_editorMeta.activate;
  VAR l:T_language;
  begin
    if not(enabled) then exit;
    inherited activate;
    mainForm.caption:=updateSheetCaption;

    for l in T_language do if Assigned(fileTypeMeta[l].menuItem) then begin
      fileTypeMeta[l].menuItem.OnClick:=@languageMenuItemClick;
      fileTypeMeta[l].menuItem.checked:=(l=language);
    end;
    try
      recentlyActivated.fileClosed(getPath);
      if language_=LANG_MNH then begin
        outlineGroupBox.visible:=true;
        editor.highlighter:=highlighter;
        paintedWithStateHash:=0;
        assistanceTabSheet.tabVisible:=true;
        triggerCheck;
        completionLogic.assignEditor(editor_,assistant);
      end else begin
        outlineGroupBox.visible:=false;
        editor.highlighter:=fileTypeMeta[language_].highlighter;
        assistanceSynEdit.clearAll;
        assistanceTabSheet.caption:='';
        assistanceTabSheet.tabVisible:=false;
        dropAssistant;
        completionLogic.assignEditor(editor_,nil);
      end;
      editor.Gutter.MarksPart.visible:=runnerModel.debugMode and (language_=LANG_MNH);
      editor.readonly                :=runnerModel.areEditorsLocked;
      settings.value^.workspace.activePage:=index;
      mainForm.onDebuggerEvent;
    except end; //catch and ignore all exceptions
  end;

PROCEDURE T_editorMeta.InputEditChange(Sender: TObject);
  PROCEDURE invalidateWordUnderCursor;
    begin
      underCursor.location.line:=-1;
    end;

  begin
    {$ifdef debugMode} writeln(stdErr,'        DEBUG: T_editorMeta.InputEditChange for ',pseudoName(),'; language: ',language_); {$endif}
    if not(enabled) then exit;
    if language_=LANG_MNH then begin
      triggerCheck;
      invalidateWordUnderCursor;
    end;
    mainForm.caption:=updateSheetCaption;
  end;

PROCEDURE T_editorMeta.languageMenuItemClick(Sender: TObject);
  begin
    setLanguage(T_language(TMenuItem(Sender).Tag));
  end;

FUNCTION T_editorMeta.saveAsWithDialog: boolean;
  begin
    if language=LANG_MNH
    then begin
      SaveDialog.FilterIndex:=0;
      SaveDialog.options:=SaveDialog.options+[ofExtensionDifferent];
    end else begin
      SaveDialog.FilterIndex:=1;
      SaveDialog.options:=SaveDialog.options-[ofExtensionDifferent];
    end;
    if SaveDialog.execute then begin
      mainForm.caption:=saveFile(SaveDialog.fileName);
      result:=true;
    end else result:=false;
  end;

FUNCTION T_editorMeta.saveWithDialog: boolean;
  begin
    if isFile then begin
      mainForm.caption:=saveFile();
      result:=true;
    end else result:=saveAsWithDialog;
  end;

PROCEDURE T_editorMeta.closeEditorQuietly;
  begin
    tabsheet.tabVisible:=false;
    editor.clearAll;
    with fileInfo do begin
      filePath:='';
      isChanged:=false;
      ignoreDeleted:=false;
    end;
    editor.modified:=false;
    getEditor^.activate;
  end;

PROCEDURE T_editorMeta.closeEditorWithDialogs;
  VAR mr:longint;
  begin
    if not(enabled) then exit;
    if changed then begin
      mr:=closeDialogForm.showOnClose(pseudoName(true));
      if mr=mrOk then if not(saveWithDialog) then exit;
      if mr=mrCancel then exit;
    end;
    if isFile then settings.value^.workspace.fileHistory.fileClosed(fileInfo.filePath);
    closeEditorQuietly;
  end;

FUNCTION T_editorMeta.enabled:boolean;
  begin
    result:=tabsheet.tabVisible;
  end;

PROCEDURE T_editorMeta.guessLanguage(CONST fallback: T_language);
  begin
    setLanguage(copy(extractFileExt(fileInfo.filePath),2,10),fallback);
  end;

PROCEDURE T_editorMeta.setFile(CONST fileName: string);
  FUNCTION isDatastore:boolean;
    begin
      result:=uppercase(copy(extractFileExt(fileName),2,9))='DATASTORE';
    end;
  VAR datastore:boolean=false;
      defaultLoad:boolean=true;
      tempLines:T_arrayOfString;
      l:string;
  begin
    tabsheet.tabVisible:=true;
    fileInfo.filePath:=fileName;
    fileInfo.ignoreDeleted:=false;
    editor.clearAll;
    try
      strictlyReadOnly:=false;
      if isDatastore then begin
        datastore:=true;
        if isBinaryDatastore(fileName,tempLines) then begin
          editor.lines.clear;
          for l in tempLines do editor.lines.add(l);
          strictlyReadOnly:=true;
          editor.readonly:=true;
          defaultLoad:=false;
        end;
        language:=LANG_MNH;
      end;
      if defaultLoad then editor.lines.loadFromFile(fileInfo.filePath);
      fileAge(fileInfo.filePath,fileInfo.fileAccessAge);
      fileInfo.isChanged:=false;
      editor.modified:=false;
    except
      editor.lines.clear;
      fileInfo.isChanged:=true;
      fileInfo.fileAccessAge:=0;
    end;
    if not(datastore) then guessLanguage(LANG_TXT);
    updateSheetCaption;
  end;

PROCEDURE T_editorMeta.initForNewFile;
  begin
    tabsheet.tabVisible:=true;
    with fileInfo do begin
      isChanged       :=false;
      fileAccessAge:=0;
      filePath     :='';
      ignoreDeleted:=false;
    end;
    editor.clearAll;
    editor.modified:=false;
    setLanguage(LANG_MNH);
    updateSheetCaption;
  end;

PROCEDURE T_editorMeta.reloadFile(CONST fileName: string);
  begin
    if enabled and (fileInfo.filePath=SysToUTF8(fileName)) and (fileExists(fileName)) then begin
      editor.lines.loadFromFile(fileInfo.filePath);
      fileAge(fileInfo.filePath,fileInfo.fileAccessAge);
      editor.modified:=false;
      fileInfo.isChanged:=false;
      mainForm.caption:=updateSheetCaption;
      if language_=LANG_MNH then triggerCheck;
    end;
  end;

FUNCTION T_editorMeta.caretInMainFormCoordinates: TPoint;
  begin
    result.x:=editor.CaretXPix;
    result.y:=editor.CaretYPix+editor.LineHeight;
    result:=editor.ClientToParent(result,mainForm);
  end;

PROCEDURE T_editorMeta.setUnderCursor(CONST updateMarker,forHelpOrJump: boolean; CONST caret: TPoint);
  VAR m:P_editorMeta;
      wordUnderCursor:string;
  begin
    if (language_<>LANG_MNH) or not(updateMarker or forHelpOrJump) then exit;
    wordUnderCursor:=editor.GetWordAtRowCol(caret);
    if updateMarker then begin
      for m in editorMetaData do m^.setMarkedWord(wordUnderCursor);
      editor.Repaint;
    end;
    if forHelpOrJump then with editor do begin
      ensureAssistant;
      assistant^.explainIdentifier(lines[caret.y-1],caret.y,caret.x,underCursor);
    end;
  end;

PROCEDURE T_editorMeta.setUnderCursor(CONST updateMarker, forHelpOrJump: boolean);
  begin
    setUnderCursor(updateMarker,forHelpOrJump,editor.CaretXY);
  end;

FUNCTION T_editorMeta.canRenameUnderCursor(OUT orignalId:string; OUT tokTyp:T_tokenType; OUT ref:T_searchTokenLocation; OUT mightBeUsedElsewhere:boolean):boolean;
  begin
    if language<>LANG_MNH then exit(false);
    setUnderCursor(false,true);
    result   :=underCursor.canRename;
    orignalId:=underCursor.idWithoutIsPrefix;
    tokTyp   :=underCursor.tokenType;
    ref      :=underCursor.location;
    mightBeUsedElsewhere:=underCursor.mightBeUsedInOtherPackages and (fileInfo.filePath<>'');
  end;

PROCEDURE T_editorMeta.doRename(CONST ref:T_searchTokenLocation; CONST newId:string; CONST renameInOtherEditors:boolean=false);
  VAR meta:P_editorMeta;
      lineIndex:longint;
      lineTxt:string;
  PROCEDURE updateLine;
    VAR lineStart,lineEnd:TPoint;
    begin
      lineStart.y:=lineIndex+1; lineStart.x:=0;
      lineEnd  .y:=lineIndex+1; lineEnd  .x:=length(editor.lines[lineIndex])+1;
      editor.SetTextBetweenPoints(lineStart,lineEnd,lineTxt);
    end;

  begin
    if not(enabled) or (language<>LANG_MNH) then exit;

    if renameInOtherEditors then saveFile();
    ensureAssistant;
    assistant^.synchronousUpdate(@self);

    editor.BeginUpdate(true);
    with editor do for lineIndex:=0 to lines.count-1 do begin
      lineTxt:=lines[lineIndex];
      if assistant^.renameIdentifierInLine(ref,newId,lineTxt,lineIndex+1) then updateLine;
    end;
    editor.EndUpdate;

    if renameInOtherEditors then for meta in editorMetaData do if meta<>@self then meta^.doRename(ref,newId);
  end;

PROCEDURE T_editorMeta.setWorkingDir;
  begin
    if fileInfo.filePath='' then SetCurrentDir(ExtractFileDir(paramStr(0)))
                            else SetCurrentDir(ExtractFileDir(fileInfo.filePath));
  end;

PROCEDURE T_editorMeta.writeToEditorState(CONST settings: P_Settings);
  VAR i:longint;
  begin
    i:=length(settings^.workspace.editorState);
    while i<=index do begin
      setLength(settings^.workspace.editorState,i+1);
      settings^.workspace.editorState[i].create;
      inc(i);
    end;
    settings^.workspace.editorState[index].visible:=enabled;
    if not(settings^.workspace.editorState[index].visible) then exit;
    setLength(settings^.workspace.editorState[index].markedLines,0);
    for i:=0 to editor.Marks.count-1 do appendIfNew(settings^.workspace.editorState[index].markedLines,editor.Marks[i].line);

    settings^.workspace.editorState[index].filePath:=fileInfo.filePath;
    settings^.workspace.editorState[index].fileAccessAge:=fileInfo.fileAccessAge;
    settings^.workspace.editorState[index].changed:=changed;
    settings^.workspace.editorState[index].strictReadOnly:=strictlyReadOnly;
    setLength(settings^.workspace.editorState[index].lines,editor.lines.count);
    for i:=0 to length(settings^.workspace.editorState[index].lines)-1 do settings^.workspace.editorState[index].lines[i]:=editor.lines[i];
    settings^.workspace.editorState[index].caret['x']:=editor.CaretX;
    settings^.workspace.editorState[index].caret['y']:=editor.CaretY;
    settings^.workspace.editorState[index].language:=ord(language);
  end;

PROCEDURE T_editorMeta.toggleBreakpoint;
  VAR i:longint;
  begin
    for i:=0 to editor_.Marks.count-1 do if editor_.Marks[i].line=editor_.CaretY then begin
      editor_.Marks.remove(editor.Marks[i]);
      runEvaluator.context.stepper^.removeBreakpoint(pseudoName,editor_.CaretY);
      exit;
    end;
    runEvaluator.context.stepper^.addBreakpoint(pseudoName,editor_.CaretY);
    _add_breakpoint_(editor_.CaretY);
  end;

FUNCTION T_editorMeta.isFile: boolean;
  begin
    result:=fileInfo.filePath<>'';
  end;

FUNCTION T_editorMeta.pseudoName(CONST short: boolean): ansistring;
  begin
    if fileInfo.filePath<>'' then begin
      if short then result:=extractFileName(fileInfo.filePath)
               else result:=fileInfo.filePath;
    end else result:='<new '+intToStr(index)+'>';
  end;

FUNCTION T_editorMeta.defaultExtensionByLanguage: ansistring;
  begin
    result:=fileTypeMeta[language_].extensions[0];
  end;

PROCEDURE T_editorMeta.setStepperBreakpoints;
  VAR i:longint;
  begin
    for i:=0 to editor.Marks.count-1 do runEvaluator.context.stepper^.addBreakpoint(pseudoName,editor.Marks[i].line);
  end;

PROCEDURE T_editorMeta._add_breakpoint_(CONST lineIndex: longint);
  VAR m:TSynEditMark;
  begin
    m:=TSynEditMark.create(editor);
    m.line:=lineIndex;
    m.ImageList:=breakpointsImagesList;
    m.ImageIndex:=0;
    m.visible:=true;
    editor.Marks.add(m);
  end;

FUNCTION T_editorMeta.updateSheetCaption: ansistring;
  begin
    if changed then result:=' *'
               else result:='';
    tabsheet.caption:=pseudoName(true)+result;
    result:=APP_TITLE+' '+pseudoName(false)+result;
  end;

PROCEDURE T_editorMeta.ensureAssistant;
  begin
    if assistant=nil then new(assistant,create);
    highlighter.codeAssistant:=assistant;
  end;

PROCEDURE T_editorMeta.assignAdditionalHighlighter(CONST additionalHighlighter:TSynMnhSyn);
  begin
    additionalHighlighter.codeAssistant:=assistant;
  end;

PROCEDURE T_editorMeta.dropAssistant;
  begin
    if (assistant<>nil) then dispose(assistant,destroy);
    highlighter.codeAssistant:=nil;
    assistant:=nil;
  end;

PROCEDURE T_editorMeta.triggerCheck;
  begin
    ensureAssistant;
    assistant^.triggerUpdate(@self);
  end;

PROCEDURE T_editorMeta.pollAssistanceResult;
  CONST SHORTCUT_SUFFIX=' (F2)';
  VAR s:string;
      hints:T_arrayOfString;
      hasErrors,hasWarnings:boolean;
  begin
    if language_<>LANG_MNH then exit;
    if (paintedWithStateHash<>assistant^.getStateHash) then begin
      paintedWithStateHash:=assistant^.getStateHash;
      highlighter.codeAssistant:=assistant;
      editor.highlighter:=highlighter;
      editor.Repaint;
      assistanceSynEdit.clearAll;
      assistanceSynEdit.lines.clear;
      hints:=assistant^.getErrorHints(hasErrors,hasWarnings,assistanceSynEdit.charsInWindow);
      if hasErrors then begin if hasWarnings then assistanceTabSheet.caption:='Errors + Warnings'+SHORTCUT_SUFFIX
                                             else assistanceTabSheet.caption:='Errors'+SHORTCUT_SUFFIX; end
                   else begin if hasWarnings then assistanceTabSheet.caption:='Warnings'+SHORTCUT_SUFFIX
                                             else assistanceTabSheet.caption:='(no warnings)'+SHORTCUT_SUFFIX; end;
      for s in hints do assistanceSynEdit.lines.add(s);
      outlineModel^.update(assistant);
    end;
    assistant^.triggerUpdate(nil);
  end;

FUNCTION T_editorMeta.changed: boolean;
  begin
    result:=fileInfo.isChanged or editor.modified;
  end;

FUNCTION T_editorMeta.saveFile(CONST fileName: string): string;
  VAR arr:T_arrayOfString;
      i:longint;
      previousName:string;
  begin
    previousName:=fileInfo.filePath;
    if fileName<>'' then fileInfo.filePath:=expandFileName(fileName);
    if (previousName<>'') and (previousName<>fileInfo.filePath) then settings.value^.workspace.fileHistory.fileClosed(previousName);
    setLength(arr,editor.lines.count);
    for i:=0 to length(arr)-1 do arr[i]:=editor.lines[i];
    with fileInfo do begin
      writeFileLines(filePath,arr,'',false);
      fileAge(filePath,fileAccessAge);
      isChanged:=false;
      editor.modified:=false;
      editor.MarkTextAsSaved;
      if (filePath=utilityScriptFileName) then runEvaluator.ensureEditScripts();
    end;
    result:=updateSheetCaption;
  end;

FUNCTION T_editorMeta.fileIsDeleted: boolean;
  begin
    result:=enabled and isFile and not(fileExists(fileInfo.filePath));
  end;

FUNCTION T_editorMeta.fileIsModifiedOnFileSystem: boolean;
  VAR currentFileAge:double;
  begin
    if not(enabled and isFile) or changed then exit(false);
    fileAge(fileInfo.filePath,currentFileAge);
    result:=currentFileAge<>fileInfo.fileAccessAge;
  end;

PROCEDURE T_editorMeta.updateContentAfterEditScript(
  CONST stringListLiteral: P_listLiteral);
  VAR concatenatedText:ansistring='';
      i:longint;
  begin
    if stringListLiteral^.literalType<>lt_stringList then exit;
    for i:=0 to stringListLiteral^.size-1 do begin
      if i>0 then concatenatedText:=concatenatedText+LineEnding;
      concatenatedText:=concatenatedText+P_stringLiteral(stringListLiteral^.value[i])^.value;
    end;
    editor.BeginUndoBlock;
    editor.SelectAll;
    editor.SelText:=concatenatedText;
    editor.EndUndoBlock;
  end;

FUNCTION T_editorMeta.resolveImport(CONST text: string): string;
  begin
    if assistant=nil then result:='' else result:=assistant^.resolveImport(text);
  end;

PROCEDURE T_editorMeta.exportToHtml;
  VAR SynExporterHTML: TSynExporterHTML;
  begin
    SaveDialog.FilterIndex:=2;
    if not(SaveDialog.execute) then exit;
    SynExporterHTML:=TSynExporterHTML.create(nil);
    SynExporterHTML.title:=pseudoName();
    SynExporterHTML.highlighter:=editor.highlighter;
    SynExporterHTML.ExportAll(editor.lines);
    SynExporterHTML.saveToFile(SaveDialog.fileName);
    SynExporterHTML.free;
  end;

//==================================================================
FUNCTION hasEditor:boolean;
  VAR i:longint;
  begin
    i:=inputPageControl.activePageIndex;
    result:=(i>=0) and (i<length(editorMetaData));
  end;

FUNCTION getEditor:P_editorMeta;
  VAR i:longint;
  begin
    i:=inputPageControl.activePageIndex;
    if (i>=0) and (i<length(editorMetaData))
    then result:=editorMetaData[i]
    else result:=nil;

    if result=nil then result:=editorMetaData[addEditorMetaForNewFile];
  end;

FUNCTION addEditorMetaForNewFile:longint;
  VAR i:longint;
  begin
    i:=length(editorMetaData)-1;
    //decrease i until a visible meta is encountered
    while (i>=0) and not(editorMetaData[i]^.enabled) do dec(i);
    inc(i);
    //i now is the index of the last visible editor meta +1
    if (i>=0) and (i<length(editorMetaData)) then begin
      editorMetaData[i]^.initForNewFile;
      editorMetaData[i]^.activate;
      exit(i);
    end;

    i:=length(editorMetaData);
    setLength(editorMetaData,i+1);
    new(editorMetaData[i],create(i));
    editorMetaData[i]^.editor.Font:=editorFont;

    result:=i;
    editorMetaData[i]^.editor.Gutter.MarksPart.visible:=runnerModel.debugMode and (editorMetaData[i]^.language=LANG_MNH);
    editorMetaData[i]^.editor.readonly                :=runnerModel.areEditorsLocked or editorMetaData[i]^.strictlyReadOnly;
    editorMetaData[i]^.activate;
  end;

FUNCTION addOrGetEditorMetaForFiles(CONST FileNames: array of string; CONST useCurrentPageAsFallback:boolean):longint;

  PROCEDURE openSingleFile(CONST fileName:ansistring);
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
        for i:=0 to length(editorMetaData)-1 do if (editorMetaData[i]^.enabled) and (editorMetaData[i]^.pseudoName=fileName) then begin
          result:=i;
          exit;
        end;
      end else begin
        filePath:=expandFileName(fileName);
        for i:=0 to length(editorMetaData)-1 do if (editorMetaData[i]^.enabled) and (editorMetaData[i]^.fileInfo.filePath=filePath) then begin
          result:=i;
          exit;
        end;
        result:=addEditorMetaForNewFile();
        editorMetaData[result]^.setFile(filePath);
        editorMetaData[result]^.editor.Font:=editorFont;
      end;
    end;

  VAR f:string;
  begin
    if useCurrentPageAsFallback then result:=inputPageControl.activePageIndex
                                else result:=-1;
    for f in FileNames do openSingleFile(f);
    if result<>-1 then editorMetaData[result]^.activate;
  end;

PROCEDURE updateFonts(CONST Font:TFont);
  VAR m:P_editorMeta;
  begin
    editorFont:=Font;
    for m in editorMetaData do m^.editor.Font:=editorFont;
  end;

FUNCTION allPseudoNames:T_arrayOfString;
  VAR m:P_editorMeta;
  begin
    setLength(result,0);
    for m in editorMetaData do if m^.enabled then append(result,m^.pseudoName);
  end;

FUNCTION getMeta(CONST nameOrPseudoName:string):P_editorMeta;
  VAR m:P_editorMeta;
  begin
    result:=nil;
    for m in editorMetaData do if (m^.enabled) and (m^.pseudoName()=nameOrPseudoName) then exit(m);
  end;

PROCEDURE storeEditorsToSettings;
  VAR e:P_editorMeta;
  begin
    for e in editorMetaData do e^.writeToEditorState(settings.value);
  end;

FUNCTION getHelpPopupText:string;
  begin
    result:=underCursor.infoText;
  end;

FUNCTION getHelpLocation:T_searchTokenLocation;
  begin
    {$ifdef debugMode} writeln(stdErr,'        DEBUG: getHelpLocation filename="',underCursor.location.fileName,'"; line=',underCursor.location.line,'; column=',underCursor.location.column); {$endif}
    result:=underCursor.location;
  end;

PROCEDURE cycleEditors(CONST cycleForward:boolean);
  VAR i,k:longint;
      delta:longint;
  begin
    i:=inputPageControl.activePageIndex;
    if cycleForward then delta:=1 else delta:=length(editorMetaData)-1;
    for k:=0 to length(editorMetaData)-1 do begin
      i:=(i+delta) mod (length(editorMetaData));
      if editorMetaData[i]^.enabled then begin
        inputPageControl.activePageIndex:=i;
        editorMetaData[i]^.activate;
        exit;
      end;
    end;
    inputPageControl.activePageIndex:=addEditorMetaForNewFile;
  end;

PROCEDURE updateEditorsByGuiStatus;
  VAR m:P_editorMeta;
  begin
    for m in editorMetaData do begin
      m^.editor.Gutter.MarksPart.visible:=runnerModel.debugMode and (m^.language=LANG_MNH);
      m^.editor.readonly                :=runnerModel.areEditorsLocked or m^.strictlyReadOnly;
    end;
  end;

PROCEDURE closeAllEditorsButCurrent;
  VAR m:P_editorMeta;
  begin
    if not(hasEditor and getEditor^.enabled) then exit;
    for m in editorMetaData do if (m^.index<>inputPageControl.activePageIndex) and (m^.enabled) then m^.closeEditorWithDialogs;
  end;

PROCEDURE closeAllUnmodifiedEditors;
  VAR m:P_editorMeta;
  begin
    for m in editorMetaData do if not(m^.changed) then m^.closeEditorWithDialogs;
    if not(hasEditor and getEditor^.enabled) then cycleEditors(true);
  end;

VAR doNotCheckFileBefore:double;
PROCEDURE checkForFileChanges;
  VAR m:P_editorMeta;
      modalRes:longint;
  begin
    if now<doNotCheckFileBefore then exit;
    doNotCheckFileBefore:=now+1;
    for m in editorMetaData do with m^ do
    if fileIsDeleted and not(fileInfo.ignoreDeleted) then begin
      modalRes:=closeDialogForm.showOnDeleted(fileInfo.filePath);
      if modalRes=mrOk then closeEditorQuietly;
      if modalRes=mrClose then begin if not(saveWithDialog) then fileInfo.isChanged:=true; end else begin
        fileInfo.ignoreDeleted:=true;
        fileInfo.isChanged:=true;
        updateSheetCaption;
      end;
      continue;
    end else if fileIsModifiedOnFileSystem then begin
      modalRes:=closeDialogForm.showOnOutOfSync(fileInfo.filePath);
      if modalRes=mrOk then reloadFile(fileInfo.filePath);
      if modalRes=mrClose then begin if not(saveWithDialog) then fileInfo.isChanged:=true; end else
      fileInfo.isChanged:=true;
    end;
    doNotCheckFileBefore:=now+ONE_SECOND;
  end;

PROCEDURE finalizeEditorMeta;
  VAR i:longint;
  begin
    if outlineModel<>nil then begin
      dispose(outlineModel,destroy);
      outlineModel:=nil;
    end;
    for i:=0 to length(editorMetaData)-1 do dispose(editorMetaData[i],destroy);
    setLength(editorMetaData,0);
  end;

PROCEDURE T_runnerModel.setDebugMode(CONST value: boolean);
  begin
    if value=debugMode_ then exit;
    debugMode_:=value;
    if (runEvaluator.evaluationRunning) and not(runEvaluator.getRunnerStateInfo.state=es_editRunning) then runEvaluator.haltEvaluation;
  end;

CONSTRUCTOR T_runnerModel.create;
  begin
    debugMode_:=false;
    stackTracing:=false;
    with lastStart do begin mainCall:=false; parameters:=''; end;
  end;

DESTRUCTOR T_runnerModel.destroy;
  begin

  end;

FUNCTION T_runnerModel.areEditorsLocked: boolean;
  begin
    result:=(debugMode_ and runEvaluator.evaluationRunning) or (runEvaluator.getRunnerStateInfo.state=es_editRunning);
  end;

FUNCTION T_runnerModel.canRun(CONST quickMode:boolean=false): boolean;
  begin
    result:=not(runEvaluator.evaluationRunningOrPending) and
            (quickMode or hasEditor and (getEditor^.language=LANG_MNH));
  end;

PROCEDURE T_runnerModel.customRun(CONST mainCall, profiling: boolean; CONST mainParameters: string);
  VAR m:P_editorMeta;
      contextType:T_evaluationContextType;
  begin
    if not(canRun) then exit;
    guiOutAdapter.flushClear;
    resetPlot;
    if settings.value^.doResetPlotOnEvaluation then begin
      guiAdapters.plot^.setDefaults;
      if plotFormIsInitialized then plotForm.pullPlotSettingsToGui();
    end;
    resetTableForms;
    getEditor^.setWorkingDir;
    if debugMode then begin
      updateEditorsByGuiStatus;
      runEvaluator.context.stepper^.clearBreakpoints;
      for m in editorMetaData do m^.setStepperBreakpoints;
    end;
    if profiling then begin
      if debugMode_ then contextType:=ect_debuggingAndProfiling
                    else contextType:=ect_profiling;
    end else begin
      if debugMode_        then contextType:=ect_debugging
      else if stackTracing then contextType:=ect_stackTracing
                           else contextType:=ect_normal;
    end;

    if mainCall then runEvaluator.callMain(getEditor,mainParameters,contextType)
                else runEvaluator.evaluate(getEditor,               contextType);
    lastStart.mainCall:=mainCall;
    lastStart.parameters:=mainParameters;
  end;

PROCEDURE T_runnerModel.rerun(CONST profiling:boolean);
  begin
    customRun(lastStart.mainCall,profiling,lastStart.parameters);
  end;

PROCEDURE T_runnerModel.InputEditSpecialLineMarkup(Sender: TObject; line: integer; VAR Special: boolean; Markup: TSynSelectedColor);
  begin
    Special:=runEvaluator.context.isPaused and runEvaluator.evaluationRunning and (Sender=debugLine.editor) and (line=debugLine.line);
  end;

PROCEDURE T_runnerModel.doDebuggerAction(CONST newState: T_debuggerState);
  begin
    runEvaluator.context.stepper^.setState(newState);
    mainForm.onDebuggerEvent;
    if hasEditor then with getEditor^ do begin
      editor.Gutter.MarksPart.visible:=debugMode_ and (language=LANG_MNH);
      editor.readonly:=areEditorsLocked or strictlyReadOnly;
    end;
  end;

PROCEDURE T_runnerModel.markDebugLine(CONST editor:TSynEdit; CONST line:longint);
  begin
    debugLine.editor:=editor;
    debugLine.line  :=line;
  end;

PROCEDURE T_runnerModel.haltEvaluation;
  begin
    runEvaluator.haltEvaluation;
    if debugMode_ then runEvaluator.context.stepper^.haltEvaluation;
  end;

FUNCTION getSafeAssistant(CONST editor:P_editorMeta):P_codeAssistanceData;
  begin
    if editor=nil then result:=nil else result:=editor^.assistant;
    if result=nil then begin
      if fallbackCodeAssistant=nil then new(fallbackCodeAssistant,createBlank);
      result:=fallbackCodeAssistant;
    end;
  end;

INITIALIZATION
  setLength(editorMetaData,0);
  doNotCheckFileBefore:=now;
  recentlyActivated.create;
FINALIZATION
  recentlyActivated.destroy;
  if fallbackCodeAssistant<>nil then dispose(fallbackCodeAssistant,destroy);
end.
