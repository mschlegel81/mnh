unit mnh_gui_main;

{$mode objfpc}{$H+}
{$define useAdapterCS}
interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, StdCtrls, ComCtrls, Grids, SynHighlighterMnh,
  mnh_fileWrappers, mnh_gui_settings, mnh_tokloc,
  mnh_out_adapters, mnh_stringutil, mnh_askForm, mnh_evalThread,mnh_constants;

type

  { TMnhForm }

  TMnhForm = class(TForm)
    MenuItem2: TMenuItem;
    miHaltEvalutaion: TMenuItem;
    miEvalModeDirect: TMenuItem;
    miEvaluateNow: TMenuItem;
    miEvalModeDirectOnKeypress: TMenuItem;
    miClear: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    OpenDialog: TOpenDialog;
    popMiOpenDeclaration: TMenuItem;
    myhl:TSynMnhSyn;
    ErrorGroupBox: TGroupBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem4: TMenuItem;
    miExpressionEcho: TMenuItem;
    miExpressionResult: TMenuItem;
    miDeclarationEcho: TMenuItem;
    miDecFontSize: TMenuItem;
    miIncFontSize: TMenuItem;
    mi_settings: TMenuItem;
    EditorPopup: TPopupMenu;
    SaveDialog: TSaveDialog;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    ErrorStringGrid: TStringGrid;
    InputEdit: TSynEdit;
    OutputEdit: TSynEdit;
    UpdateTimeTimer: TTimer;
    procedure EditorPopupPopup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InputEditChange(Sender: TObject);
    procedure InputEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure InputEditMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure miClearClick(Sender: TObject);
    procedure miDecFontSizeClick(Sender: TObject);
    procedure miDeclarationEchoClick(Sender: TObject);
    procedure miEvalModeDirectClick(Sender: TObject);
    procedure miEvalModeDirectOnKeypressClick(Sender: TObject);
    procedure miEvaluateNowClick(Sender: TObject);
    procedure miExpressionEchoClick(Sender: TObject);
    procedure miExpressionResultClick(Sender: TObject);
    procedure miHaltEvalutaionClick(Sender: TObject);
    procedure miIncFontSizeClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure mi_settingsClick(Sender: TObject);
    procedure OutputEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OutputEditMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure popMiOpenDeclarationClick(Sender: TObject);
    procedure UpdateTimeTimerTimer(Sender: TObject);

  private
    lastWordUnderCursor:string;
    underCursor:T_idInfo;

    settingsHaveBeenProcessed:boolean;
    procedure processSettings;
    { private declarations }
  public
    { public declarations }
  end;

var
  MnhForm: TMnhForm;

implementation
//var adapter:record
//      state:(as_directNoFile,
//             as_directFile,
//             as_watching);
//
//      currentlyEditingFile:specialize G_safeVar<string>;
//      fileWasEdited:boolean;
//      watchedFileAge:longint;
//
//      errorGridRequiresResize:boolean;
//
//      evaluationStarted        : double;
//      haltEvaluationLoopPosted : boolean;
//      evaluationRunning        : boolean;
//      evaluationLoopRunning    : boolean;
//    end;
{$R *.lfm}

PROCEDURE writeDeclEcho(CONST s:ansistring);
  begin
    MnhForm.OutputEdit.Lines.Append(C_DeclEchoHead+' '+s);
  end;

PROCEDURE writeExprEcho(CONST s:ansistring);
  begin
    MnhForm.OutputEdit.Lines.Append(C_ExprEchoHead+' '+s);
  end;

PROCEDURE writeExprOut (CONST s:ansistring);
  begin
    MnhForm.OutputEdit.Lines.Append(C_ExprOutHead+' '+s);
  end;

PROCEDURE writePrint   (CONST s:ansistring);
  begin
    MnhForm.OutputEdit.Lines.Append(s);
  end;

{ TMnhForm }
procedure TMnhForm.FormCreate(Sender: TObject);
  begin
    myhl:=TSynMnhSyn.Create(nil);
    InputEdit.Highlighter:=myhl;
    OutputEdit.Highlighter:=myhl;
    settingsHaveBeenProcessed:=false;
    StatusBar.SimpleText:=
      'compiled on: '+{$I %DATE%}+
      ' at: '+{$I %TIME%}+
      ' with FPC'+{$I %FPCVERSION%}+
      ' for '+{$I %FPCTARGET%};
  end;

procedure TMnhForm.EditorPopupPopup(Sender: TObject);
  begin
    underCursor:=ad_getIdInfo(lastWordUnderCursor);
    if underCursor.isBuiltIn then popMiOpenDeclaration.Caption:='"'+lastWordUnderCursor+'" is a built in function'
    else if underCursor.isUserDefined then popMiOpenDeclaration.Caption:='Open declaration of "'+lastWordUnderCursor+'" ('+underCursor.filename+')'
    else if underCursor.filename<>'' then popMiOpenDeclaration.Caption:='Open package "'+underCursor.filename+'"';
    popMiOpenDeclaration.Enabled:=underCursor.filename<>'';
  end;

//procedure ad_haltEvaluation;
//  begin with adapter do begin
//    haltEvaluation;
//    while evaluationRunning do sleep(1);
//  end; end;
//
//procedure ad_killEvaluationLoopSoftly;
//  begin with adapter do begin
//    haltEvaluation;
//    //this must happen outside of the critical section to avoid a deadlock
//    repeat
//      haltEvaluationLoopPosted:=true;
//      sleep(1);
//    until not(evaluationLoopRunning) and not(evaluationRunning);
//  end; end;
//
//procedure ad_initEvaluation;
//  begin with adapter do if not(evaluationLoopRunning) then begin
//    fileWrapper:=nil;
//    directInputWrapper:=nil;
//    if state=as_watching then begin
//      new(fileWrapper,create(currentlyEditingFile.value));
//      initMainPackage(fileWrapper);
//    end else begin
//      new(directInputWrapper,create);
//      directInputWrapper^.logCheck;
//      if state=as_directFile then addSourceScanPath(ExtractFilePath(currentlyEditingFile.value));
//      initMainPackage(directInputWrapper);
//    end;
//    BeginThread(@evaluationThread);
//  end; end;
//
//procedure ad_loadInputToWrapper;
//  VAR i:longint;
//      L:T_stringList;
//  begin with adapter do if (state<>as_watching) and (directInputWrapper<>nil) then begin
//    if state=as_directFile then fileWasEdited:=true;
//    setLength(L,MnhForm.InputEdit.Lines.Count);
//    for i:=0 to MnhForm.InputEdit.Lines.Count-1 do L[i]:=MnhForm.InputEdit.Lines[i];
//    directInputWrapper^.setInput(L);
//  end; end;
//
//procedure ad_updateForm;
//  VAR caption:string;
//  begin with adapter do begin
//    caption:='MNH5 '+currentlyEditingFile.value; if MnhForm.Caption<>caption then MnhForm.Caption:=caption;
//    MnhForm.miSave          .Enabled:=(currentlyEditingFile.value<>'') and (state<>as_watching);
//    MnhForm.miSaveAs        .Enabled:=                                     (state<>as_watching);
//    MnhForm.miEvaluateNow   .Enabled:=evaluationLoopRunning and not(evaluationRunning);
//    MnhForm.miHaltEvalutaion.Enabled:=evaluationRunning;
//    if evaluationRunning then MnhForm.StatusBar.SimpleText:='Evaluating '+FloatToStr(round((now-evaluationStarted)*24*60*60*100)*0.01)+' sec.';
//    if (state=as_watching) then begin
//      if not(MnhForm.miEvalModeWatch.Checked) then MnhForm.miEvalModeWatch.Checked:=true;
//      if not(MnhForm.InputEdit.ReadOnly)      then MnhForm.InputEdit.ReadOnly:=true;
//    end else begin
//      if     MnhForm.InputEdit.ReadOnly       then MnhForm.InputEdit.ReadOnly:=false;
//    end;
//    //MnhForm.ErrorStringGrid.AutoSizeColumns;
//    if (state=as_watching) and
//       (currentlyEditingFile.value<>'') and
//       (FileAge(currentlyEditingFile.value)<>watchedFileAge)
//    then try
//      MnhForm.InputEdit.Lines.LoadFromFile(currentlyEditingFile.value);
//      watchedFileAge:=FileAge(currentlyEditingFile.value);
//    except
//      watchedFileAge:=0;
//    end;
//    if errorGridRequiresResize then try
//      MnhForm.ErrorStringGrid.AutoSizeColumns;
//      errorGridRequiresResize:=false;
//    except
//      errorGridRequiresResize:=true;
//    end;
//  end; end;
//
//procedure ad_triggerEvaluation;
//  begin with adapter do if evaluationLoopRunning then begin
//    if directInputWrapper<>nil then directInputWrapper^.markAsDirty;
//    if fileWrapper       <>nil then fileWrapper       ^.markAsDirty;
//  end; end;
//
//procedure ad_clearFile;
//  begin with adapter do begin
//    currentlyEditingFile.value:='';
//    if state=as_watching then begin
//      ad_killEvaluationLoopSoftly;
//      state:=as_directNoFile;
//      ad_initEvaluation;
//    end else begin
//      directInputWrapper^.setInput('');
//      ad_haltEvaluation;
//      state:=as_directNoFile;
//      clearImportedPackages;
//    end;
//    MnhForm.InputEdit.ClearAll;
//    MnhForm.InputEdit.Lines.Clear;
//    ad_updateForm;
//  end; end;
//
//PROCEDURE ad_switchToDirectWithFile(CONST filename:string);
//  VAR mr:integer;
//  begin with adapter do begin
//    case state of
//      as_directNoFile: if filename<>'' then begin
//        ad_haltEvaluation;
//        currentlyEditingFile.value:=filename;
//        MnhForm.InputEdit.Lines.LoadFromFile(filename);
//        ad_updateForm;
//        ad_loadInputToWrapper;
//      end;
//      as_directFile: begin
//        if (filename<>'') and (filename<>currentlyEditingFile.value) and fileWasEdited then begin
//          mr:=AskForm.showModalOnFileChange();
//          if mr=mrYes then MnhForm.InputEdit.Lines.SaveToFile(currentlyEditingFile.value)
//          else if mr<>mrNo then exit;
//        end;
//        ad_haltEvaluation;
//        currentlyEditingFile:=filename;
//        MnhForm.InputEdit.Lines.LoadFromFile(filename);
//        ad_updateForm;
//        ad_loadInputToWrapper;
//      end;
//      as_watching: begin
//        ad_killEvaluationLoopSoftly;
//        state:=as_directFile;
//        currentlyEditingFile:=filename;
//        MnhForm.InputEdit.Lines.LoadFromFile(filename);
//        ad_updateForm;
//        ad_initEvaluation;
//      end;
//    end;
//  end; end;
//
//PROCEDURE ad_switchToWatchFile(CONST filename:string);
//  VAR mr:integer;
//  begin with adapter do begin
//    case state of
//      as_directNoFile: if filename<>'' then begin
//        ad_killEvaluationLoopSoftly;
//        state:=as_watching;
//        currentlyEditingFile:=filename;
//        MnhForm.InputEdit.Lines.LoadFromFile(filename);
//        ad_updateForm;
//        ad_initEvaluation;
//      end;
//      as_directFile: begin
//        if (filename<>'') and (filename<>currentlyEditingFile) and fileWasEdited then begin
//          mr:=AskForm.showModalOnFileChange();
//          if (mr<>mrYes) and (mr<>mrNo) then exit;
//        end else if fileWasEdited then mr:=mrYes;
//        if mr=mrYes then MnhForm.InputEdit.Lines.SaveToFile(currentlyEditingFile);
//
//        ad_killEvaluationLoopSoftly;
//        state:=as_watching;
//        currentlyEditingFile:=filename;
//        MnhForm.InputEdit.Lines.LoadFromFile(filename);
//        ad_updateForm;
//        ad_initEvaluation;
//      end;
//      as_watching: if (filename<>'') and (filename<>currentlyEditingFile) then begin
//        ad_killEvaluationLoopSoftly;
//        currentlyEditingFile:=filename;
//        MnhForm.InputEdit.Lines.LoadFromFile(filename);
//        ad_updateForm;
//        ad_initEvaluation;
//      end;
//    end;
//  end; end;


procedure TMnhForm.FormDestroy(Sender: TObject);
  begin
    ad_killEvaluationLoopSoftly;
  end;

procedure TMnhForm.FormResize(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      SettingsForm.mainForm.top   :=top;
      SettingsForm.mainForm.left  :=left;
      SettingsForm.mainForm.width :=width;
      SettingsForm.mainForm.height:=height;
    end;
  end;

procedure TMnhForm.FormShow(Sender: TObject);
  begin
    if not(settingsHaveBeenProcessed) then processSettings;
  end;

procedure TMnhForm.InputEditChange(Sender: TObject);
  begin
    if (miEvalModeDirectOnKeypress.Checked) then begin
      ad_copyLinesToWrapper(InputEdit.Lines);
      OutputEdit.Lines.Clear;
      ErrorStringGrid.RowCount:=0;
      ad_evaluate;
    end;
  end;

procedure TMnhForm.InputEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  VAR wordUnderCursor:string;
  begin
    if (key>=33) and (key<=40) then begin
      wordUnderCursor:=InputEdit.GetWordAtRowCol(InputEdit.CaretXY);
      if isIdentifier(wordUnderCursor,true) then lastWordUnderCursor:=wordUnderCursor;
    end;
  end;

procedure TMnhForm.InputEditMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  VAR point:TPoint;
      wordUnderCursor:string;
  begin
    point.x:=x;
    point.y:=y;
    wordUnderCursor:=InputEdit.GetWordAtRowCol(InputEdit.PixelsToRowColumn(point));
    if isIdentifier(wordUnderCursor,true) then lastWordUnderCursor:=wordUnderCursor;
  end;

procedure TMnhForm.miClearClick(Sender: TObject);
  begin
    ad_clearFile;
  end;

procedure TMnhForm.miDecFontSizeClick(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      SettingsForm.fontSize:=SettingsForm.fontSize-1;
      processSettings;
    end;
  end;

procedure TMnhForm.miDeclarationEchoClick(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      miDeclarationEcho.Checked:=not(miDeclarationEcho.Checked);
      with SettingsForm.outputBehaviour do begin
        doEchoDeclaration:=miDeclarationEcho.Checked;
        if doEchoDeclaration then mnh_out_adapters.inputDeclEcho:=@writeDeclEcho
                             else mnh_out_adapters.inputDeclEcho:=nil;
      end;
    end;
  end;

procedure TMnhForm.miEvalModeDirectClick(Sender: TObject);
  begin
    if miEvalModeDirect.Checked then exit;
    miEvalModeDirect.Checked:=true;
  end;

procedure TMnhForm.miEvalModeDirectOnKeypressClick(Sender: TObject);
  begin
    if miEvalModeDirectOnKeypress.Checked then exit;
    miEvalModeDirectOnKeypress.Checked:=true;
  end;

procedure TMnhForm.miEvaluateNowClick(Sender: TObject);
  begin
    ad_copyLinesToWrapper(InputEdit.Lines);
    OutputEdit.Lines.Clear;
    ErrorStringGrid.RowCount:=0;
    ad_evaluate;
  end;

procedure TMnhForm.miExpressionEchoClick(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      miExpressionEcho.Checked:=not(miExpressionEcho.Checked);
      with SettingsForm.outputBehaviour do begin
        doEchoInput:=miExpressionEcho.Checked;
        if doEchoInput then mnh_out_adapters.inputExprEcho:=@writeExprEcho
                       else mnh_out_adapters.inputExprEcho:=nil;
      end;
    end;
  end;

procedure TMnhForm.miExpressionResultClick(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      miExpressionResult.Checked:=not(miExpressionResult.Checked);
      with SettingsForm.outputBehaviour do begin
        doShowExpressionOut:=miExpressionResult.Checked;
        if doShowExpressionOut then mnh_out_adapters.exprOut:=@writeExprOut
                               else mnh_out_adapters.exprOut:=nil;
      end;
    end;
  end;

procedure TMnhForm.miHaltEvalutaionClick(Sender: TObject);
  begin
    ad_haltEvaluation;
  end;

procedure TMnhForm.miIncFontSizeClick(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      SettingsForm.fontSize:=SettingsForm.fontSize+1;
      processSettings;
    end;
  end;

procedure TMnhForm.miOpenClick(Sender: TObject);
  begin
    OpenDialog.Title:='Open file';
    if OpenDialog.Execute and FileExists(OpenDialog.FileName)
    then ad_setFile(OpenDialog.FileName,InputEdit.Lines);
  end;

procedure TMnhForm.miSaveAsClick(Sender: TObject);
  begin
    if SaveDialog.Execute then begin
      MnhForm.InputEdit.Lines.SaveToFile(SaveDialog.FileName);
      ad_setFile(SaveDialog.FileName,InputEdit.Lines);
    end;
  end;

procedure TMnhForm.miSaveClick(Sender: TObject);
  begin
    if ad_currentFile<>'' then MnhForm.InputEdit.Lines.SaveToFile(ad_currentFile);
  end;

procedure TMnhForm.mi_settingsClick(Sender: TObject);
  begin
    SettingsForm.ShowModal;
    processSettings;
  end;

procedure TMnhForm.OutputEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  VAR wordUnderCursor:string;
  begin
    if (key>=33) and (key<=40) then begin
      wordUnderCursor:=OutputEdit.GetWordAtRowCol(OutputEdit.CaretXY);
      if isIdentifier(wordUnderCursor,true) then lastWordUnderCursor:=wordUnderCursor;
    end;
  end;

procedure TMnhForm.OutputEditMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  VAR point:TPoint;
      wordUnderCursor:string;
  begin
    point.x:=x;
    point.y:=y;
    wordUnderCursor:=OutputEdit.GetWordAtRowCol(OutputEdit.PixelsToRowColumn(point));
    if isIdentifier(wordUnderCursor,true) then lastWordUnderCursor:=wordUnderCursor;
  end;

procedure TMnhForm.popMiOpenDeclarationClick(Sender: TObject);
  begin
    with underCursor do
    if filename<>''
    then SettingsForm.canOpenFile(filename,fileline);
  end;

procedure TMnhForm.UpdateTimeTimerTimer(Sender: TObject);
  VAR aid:string;
      flag:boolean;
  begin
    //Form caption:-------------------------------------------------------------
    aid:='MNH5 '+ad_currentFile;
    if aid<>Caption then begin Caption:=aid; UpdateTimeTimer.Interval:=0; end;
    //-------------------------------------------------------------:Form caption
    //Halt/Run enabled states:--------------------------------------------------
    flag:=ad_evaluationRunning;
    if flag<>miHaltEvalutaion.Enabled then begin miHaltEvalutaion.Enabled:=flag; UpdateTimeTimer.Interval:=0; end;
    flag:=not(flag);
    if flag<>miEvaluateNow.Enabled then begin miEvaluateNow.Enabled:=flag; UpdateTimeTimer.Interval:=0; end;
    //--------------------------------------------------:Halt/Run enabled states
    writeln('FORM UPDATE ',UpdateTimeTimer.Interval,'ms');
    if UpdateTimeTimer.Interval<1000 then UpdateTimeTimer.Interval:=UpdateTimeTimer.Interval+1;
  end;

procedure TMnhForm.processSettings;
  begin
    InputEdit.Font.name:=SettingsForm.getEditorFontName;
    InputEdit.Font.Size:=SettingsForm.fontSize;
    if SettingsForm.AntialiasCheckbox.Checked
    then InputEdit.Font.Quality:=fqCleartypeNatural
    else InputEdit.Font.Quality:=fqNonAntialiased;

    OutputEdit.Font     :=InputEdit.Font;
    //ErrorStringGrid.Font:=InputEdit.Font;

    top   :=SettingsForm.mainForm.top;
    left  :=SettingsForm.mainForm.left;
    width :=SettingsForm.mainForm.width;
    height:=SettingsForm.mainForm.height;

    with SettingsForm.outputBehaviour do begin
      miDeclarationEcho.Checked:=doEchoDeclaration;
      if doEchoDeclaration   then mnh_out_adapters.inputDeclEcho:=@writeDeclEcho
                             else mnh_out_adapters.inputDeclEcho:=nil;
      miExpressionEcho.Checked:=doEchoInput;
      if doEchoInput         then mnh_out_adapters.inputExprEcho:=@writeExprEcho
                             else mnh_out_adapters.inputExprEcho:=nil;
      miExpressionResult.Checked:=doShowExpressionOut;
      if doShowExpressionOut then mnh_out_adapters.exprOut:=@writeExprOut
                             else mnh_out_adapters.exprOut:=nil;
    end;
    settingsHaveBeenProcessed:=true;
  end;

PROCEDURE logError(CONST errorLevel:T_errorLevel; CONST errorMessage:ansistring; CONST errorLocation:T_tokenLocation);
  VAR row:longint;
  begin
    MnhForm.ErrorGroupBox.Visible:=true;
    row:=MnhForm.ErrorStringGrid.RowCount;
    MnhForm.ErrorStringGrid.RowCount:=row+1;
    MnhForm.ErrorStringGrid.Cells[0,row]:=C_errorLevelTxt[errorLevel];
    MnhForm.ErrorStringGrid.Cells[1,row]:=errorMessage;
    MnhForm.ErrorStringGrid.Cells[2,row]:=errorLocation;
    if (errorLocation.provider=nil) or (errorLocation.provider^.getPath='') then begin
      MnhForm.ErrorStringGrid.Cells[3,row]:='';
      MnhForm.ErrorStringGrid.Cells[4,row]:='';
    end else begin
      MnhForm.ErrorStringGrid.Cells[3,row]:=errorLocation.provider^.getPath;
      MnhForm.ErrorStringGrid.Cells[4,row]:=IntToStr(errorLocation.line);
    end;
  end;

INITIALIZATION
  mnh_out_adapters.inputDeclEcho:=@writeDeclEcho;
  mnh_out_adapters.inputExprEcho:=@writeExprEcho;
  mnh_out_adapters.exprOut      :=@writeExprOut;
  mnh_out_adapters.printOut     :=@writePrint;


end.

