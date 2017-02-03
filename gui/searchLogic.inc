{$ifdef includeInterface}
PROCEDURE miFindClick(Sender: TObject);
PROCEDURE miReplaceClick(Sender: TObject);
PROCEDURE FindDialogFind(Sender: TObject);
PROCEDURE ReplaceDialogReplace(Sender: TObject);
PROCEDURE ReplaceDialogFind(Sender: TObject);
PROCEDURE miFindNextClick(Sender: TObject);
PROCEDURE miFindPreviousClick(Sender: TObject);
PROCEDURE miGotoClick(Sender: TObject);
{$endif}
{$ifdef includeImplementation}
PROCEDURE TMnhForm.miFindClick(Sender: TObject);
  VAR wordUnderCursor:string;
  begin
    if OutputEdit.Focused then begin
      OutputEdit.GetWordAtRowCol(OutputEdit.CaretXY);
      wordUnderCursor:=OutputEdit.TextBetweenPoints[OutputEdit.BlockBegin,OutputEdit.BlockEnd];
      if wordUnderCursor='' then wordUnderCursor:=OutputEdit.GetWordAtRowCol(OutputEdit.CaretXY);
      outputFocusedOnFind:=true;
    end else begin
      with editorMeta[inputPageControl.activePageIndex]^ do begin
        wordUnderCursor:=editor.TextBetweenPoints[editor.BlockBegin,editor.BlockEnd];
        if wordUnderCursor='' then wordUnderCursor:=editor.GetWordAtRowCol(editor.CaretXY);
      end;
      outputFocusedOnFind:=false;
    end;
    if wordUnderCursor<>'' then FindDialog.FindText:=wordUnderCursor;
    FindDialog.execute;
    ReplaceDialog.FindText:=FindDialog.FindText;
  end;

PROCEDURE TMnhForm.miReplaceClick(Sender: TObject);
  VAR wordUnderCursor:string;
  begin
    if OutputEdit.Focused then begin
      OutputEdit.GetWordAtRowCol(OutputEdit.CaretXY);
      wordUnderCursor:=OutputEdit.TextBetweenPoints[OutputEdit.BlockBegin,OutputEdit.BlockEnd];
      if wordUnderCursor='' then wordUnderCursor:=OutputEdit.GetWordAtRowCol(OutputEdit.CaretXY);
      outputFocusedOnFind:=true;
    end else begin
      with editorMeta[inputPageControl.activePageIndex]^ do begin
        wordUnderCursor:=editor.TextBetweenPoints[editor.BlockBegin,editor.BlockEnd];
        if wordUnderCursor='' then wordUnderCursor:=editor.GetWordAtRowCol(editor.CaretXY);
      end;
      outputFocusedOnFind:=false;
    end;
    if wordUnderCursor<>'' then ReplaceDialog.FindText:=wordUnderCursor;
    ReplaceDialog.execute;
    FindDialog.FindText:=ReplaceDialog.FindText;
  end;

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

PROCEDURE TMnhForm.FindDialogFind(Sender: TObject);
  begin
    editForSearch(false).SearchReplace(FindDialog.FindText,FindDialog.FindText,FindOptionsToSearchOptions(FindDialog.options));
  end;

PROCEDURE TMnhForm.ReplaceDialogReplace(Sender: TObject);
  begin
    editForSearch(true).SearchReplace(ReplaceDialog.FindText,ReplaceDialog.ReplaceText,FindOptionsToSearchOptions(ReplaceDialog.options));
  end;

PROCEDURE TMnhForm.ReplaceDialogFind(Sender: TObject);
  begin
    editForSearch(true).SearchReplace(ReplaceDialog.FindText,ReplaceDialog.FindText,FindOptionsToSearchOptions(ReplaceDialog.options));
  end;

PROCEDURE TMnhForm.miFindNextClick(Sender: TObject);
  begin
    outputFocusedOnFind:=OutputEdit.Focused;
    editForSearch(false).SearchReplace(FindDialog.FindText,FindDialog.FindText,FindOptionsToSearchOptions(FindDialog.options)-[ssoBackwards]);
  end;

PROCEDURE TMnhForm.miFindPreviousClick(Sender: TObject);
  begin
    outputFocusedOnFind:=OutputEdit.Focused;
    editForSearch(false).SearchReplace(FindDialog.FindText,FindDialog.FindText,FindOptionsToSearchOptions(FindDialog.options)+[ssoBackwards]);
  end;

PROCEDURE TMnhForm.miGotoClick(Sender: TObject);
  VAR i:longint;
  begin
    i:=inputPageControl.activePageIndex;
    if (i<0) or (i>=length(editorMeta)) then exit;
    with editorMeta[i]^ do begin
      askForm.initWithQuestion('Go to line');
      askForm.ShowModal;
      i:=strToIntDef(askForm.getLastAnswerReleasing(nil),-1);
      if i>=1 then begin
        editor.CaretX:=1;
        editor.CaretY:=i;
      end;
    end;
  end;
{$endif}
