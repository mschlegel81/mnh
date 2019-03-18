UNIT searchModel;
INTERFACE
USES Dialogs,SynEdit,SynEditTypes;
TYPE
  T_searchReplaceModel=object
    private
      FindDialog    :TFindDialog;
      ReplaceDialog :TReplaceDialog;
      assignedEditor:TSynEdit;
      hasEditor:boolean;
      PROCEDURE FindDialogFind(Sender: TObject);
      PROCEDURE ReplaceDialogFind(Sender: TObject);
      PROCEDURE ReplaceDialogReplace(Sender: TObject);
    public
      CONSTRUCTOR create(CONST FindDialog_   :TFindDialog;
                         CONST ReplaceDialog_:TReplaceDialog);
      DESTRUCTOR destroy;
      PROCEDURE beginFindOrReplace(CONST focusedEditor:TSynEdit; CONST findRequested:boolean);
      PROCEDURE doFindNext(CONST focusedEditor:TSynEdit);
      PROCEDURE doFindPrevious(CONST focusedEditor:TSynEdit);
  end;

VAR searchReplaceModel:T_searchReplaceModel;
IMPLEMENTATION
FUNCTION FindOptionsToSearchOptions (CONST FindOptions: TFindOptions): TSynSearchOptions;
  begin
    result:=[];
    if frMatchCase       in FindOptions then include(result,ssoMatchCase);
    if frWholeWord       in FindOptions then include(result,ssoWholeWord);
    if frReplace         in FindOptions then include(result,ssoReplace);
    if frReplaceAll      in FindOptions then include(result,ssoReplaceAll);
    if frHideEntireScope in FindOptions then include(result,ssoEntireScope);
    if frPromptOnReplace in FindOptions then include(result,ssoPrompt);
    if frFindNext        in FindOptions then include(result,ssoFindContinue);
    if not(frDown in FindOptions) then include(result,ssoBackwards);
  end;

PROCEDURE T_searchReplaceModel.FindDialogFind(Sender: TObject);
  begin
    if not(hasEditor) then exit;
    assignedEditor.SearchReplace(FindDialog.FindText,FindDialog.FindText,FindOptionsToSearchOptions(FindDialog.options));
  end;

PROCEDURE T_searchReplaceModel.ReplaceDialogFind(Sender: TObject);
  begin
    if not(hasEditor) then exit;
    assignedEditor.SearchReplace(ReplaceDialog.FindText,ReplaceDialog.FindText,FindOptionsToSearchOptions(ReplaceDialog.options));
  end;

PROCEDURE T_searchReplaceModel.ReplaceDialogReplace(Sender: TObject);
  begin
    if not(hasEditor) then exit;
    assignedEditor.SearchReplace(ReplaceDialog.FindText,ReplaceDialog.ReplaceText,FindOptionsToSearchOptions(ReplaceDialog.options));
  end;

CONSTRUCTOR T_searchReplaceModel.create(CONST FindDialog_: TFindDialog; CONST ReplaceDialog_: TReplaceDialog);
  begin
    hasEditor              :=false;
    FindDialog             :=FindDialog_   ;
    FindDialog   .OnFind   :=@FindDialogFind;
    ReplaceDialog          :=ReplaceDialog_;
    ReplaceDialog.OnFind   :=@ReplaceDialogFind;
    ReplaceDialog.OnReplace:=@ReplaceDialogReplace;
  end;

DESTRUCTOR T_searchReplaceModel.destroy;
begin
end;

PROCEDURE T_searchReplaceModel.beginFindOrReplace(CONST focusedEditor: TSynEdit; CONST findRequested: boolean);
  VAR wordUnderCursor:string;
  begin
    if focusedEditor=nil then exit;
    assignedEditor:=focusedEditor;
    hasEditor:=true;
    wordUnderCursor:=assignedEditor.TextBetweenPoints[assignedEditor.BlockBegin,assignedEditor.BlockEnd];
    if wordUnderCursor='' then wordUnderCursor:=assignedEditor.GetWordAtRowCol(assignedEditor.CaretXY);
    if wordUnderCursor<>'' then FindDialog.FindText:=wordUnderCursor;
    if findRequested then begin
      FindDialog.execute;
      ReplaceDialog.FindText:=FindDialog.FindText;
    end else begin
      ReplaceDialog.execute;
      FindDialog.FindText:=ReplaceDialog.FindText;
    end;
  end;

PROCEDURE T_searchReplaceModel.doFindNext(CONST focusedEditor: TSynEdit);
  begin
    if focusedEditor=nil then exit;
    focusedEditor.SearchReplace(FindDialog.FindText,FindDialog.FindText,FindOptionsToSearchOptions(FindDialog.options)-[ssoBackwards]);
  end;

PROCEDURE T_searchReplaceModel.doFindPrevious(CONST focusedEditor: TSynEdit);
  begin
    if focusedEditor=nil then exit;
    focusedEditor.SearchReplace(FindDialog.FindText,FindDialog.FindText,FindOptionsToSearchOptions(FindDialog.options)+[ssoBackwards]);
  end;

end.
