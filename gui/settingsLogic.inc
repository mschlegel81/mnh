{$ifdef includeInterface}
PROCEDURE processSettings;
PROCEDURE miDecFontSizeClick(Sender: TObject);
PROCEDURE miIncFontSizeClick(Sender: TObject);
PROCEDURE miDeclarationEchoClick(Sender: TObject);
PROCEDURE miExpressionEchoClick(Sender: TObject);
PROCEDURE miExpressionResultClick(Sender: TObject);
PROCEDURE _setErrorlevel_(CONST i: byte);
PROCEDURE miMinErrorlevel1Click(Sender: TObject);
PROCEDURE miMinErrorlevel2Click(Sender: TObject);
PROCEDURE miMinErrorlevel3Click(Sender: TObject);
PROCEDURE miMinErrorlevel4Click(Sender: TObject);
PROCEDURE miTimingInfoClick(Sender: TObject);
PROCEDURE miWrapEchoClick(Sender: TObject);
PROCEDURE mi_settingsClick(Sender: TObject);

{$endif}
{$ifdef includeImplementation}
PROCEDURE TMnhForm._setErrorlevel_(CONST i: byte);
  VAR j:longint;
  begin
    if settingsReady then begin
      case i of
        1: miMinErrorlevel1.Checked:=true;
        2: miMinErrorlevel2.Checked:=true;
        3: miMinErrorlevel3.Checked:=true;
        4: miMinErrorlevel4.Checked:=true;
      end;
      for j:=1 to 4 do guiOutAdapter.enableMessageType(j>=i,C_errorMessageTypes[j]);
      settings.value^.outputBehaviour:=guiOutAdapter.outputBehavior;
    end;
  end;

PROCEDURE TMnhForm.miMinErrorlevel1Click(Sender: TObject); begin _setErrorlevel_(1); end;
PROCEDURE TMnhForm.miMinErrorlevel2Click(Sender: TObject); begin _setErrorlevel_(2); end;
PROCEDURE TMnhForm.miMinErrorlevel3Click(Sender: TObject); begin _setErrorlevel_(3); end;
PROCEDURE TMnhForm.miMinErrorlevel4Click(Sender: TObject); begin _setErrorlevel_(4); end;

PROCEDURE TMnhForm.processSettings;
  VAR formPosition:T_formPosition;
      i,j:longint;
      maxLevel:longint=5;
  begin
    if not(settingsReady) then begin
      if settings.value^.loaded then begin
        formPosition:=settings.value^.mainForm;
        top   :=formPosition.top;
        Left  :=formPosition.Left;
        width :=formPosition.width;
        height:=formPosition.height;
        if formPosition.isFullscreen then WindowState:=wsMaximized;
        outputPageControl.height:=round(ClientHeight*formPosition.relativeSplitterPosition);
        StatusBar.top:=outputPageControl.top+outputPageControl.height;
      end else settings.value^.mainForm.relativeSplitterPosition:=outputPageControl.height/ClientHeight;

      miDeclarationEcho .Checked:=mt_echo_declaration in settings.value^.outputBehaviour;
      miExpressionEcho  .Checked:=mt_echo_input       in settings.value^.outputBehaviour;
      miExpressionResult.Checked:=mt_echo_output      in settings.value^.outputBehaviour;
      miTimingInfo      .Checked:=mt_timing_info      in settings.value^.outputBehaviour;;
      miWrapEcho.Checked:=settings.value^.wordWrapEcho;
      for j:=4 downto 1 do if (C_errorMessageTypes[j] * settings.value^.outputBehaviour <> []) then maxLevel:=j;
      miMinErrorlevel1.Checked:=maxLevel=1;
      miMinErrorlevel2.Checked:=maxLevel=2;
      miMinErrorlevel3.Checked:=maxLevel=3;
      miMinErrorlevel4.Checked:=maxLevel>=4;
      guiOutAdapter.outputBehavior:=settings.value^.outputBehaviour;

      setupOutputBehaviourFromCommandLineOptions(guiAdapters,nil);

      processFileHistory;
      SettingsForm.ensureFont(OutputEdit.Font);

      setLength(editorMeta,length(settings.value^.workspace.editorState));
      runEvaluator.context.stepper^.clearBreakpoints;
      for i:=0 to length(editorMeta)-1 do begin
        new(editorMeta[i],create(i,settings.value^.workspace.editorState[i]));
        editorMeta[i]^.setStepperBreakpoints;
      end;
      i:=settings.value^.workspace.activePage;
      inputPageControl.activePageIndex:=i;

      for i:=0 to length(filesToOpenInEditor)-1 do FormDropFiles(nil,filesToOpenInEditor[i]);

      if (inputPageControl.activePageIndex>=0) and (inputPageControl.activePageIndex<length(editorMeta)) then SynCompletion.editor:=editorMeta[inputPageControl.activePageIndex]^.editor;
      {$ifdef UNIX}
      miIncFontSize.ShortCut:=16605;
      {$endif}

      settingsReady:=true;
      if (inputPageControl.activePageIndex>=0) and (inputPageControl.activePageIndex<length(editorMeta)) then assistancEvaluator.evaluate((editorMeta[inputPageControl.activePageIndex]));
    end;

    OutputEdit.Font.name:=settings.value^.editorFontname;
    OutputEdit.Font.size:=SettingsForm.fontSize;
    if SettingsForm.AntialiasCheckbox.Checked
    then OutputEdit.Font.quality:=fqCleartypeNatural
    else OutputEdit.Font.quality:=fqNonAntialiased;
    for i:=0 to length(editorMeta)-1 do editorMeta[i]^.editor.Font:=OutputEdit.Font;
    if settings.value^.wordWrapEcho
    then guiAdapters.preferredEchoLineLength:=OutputEdit.charsInWindow-6
    else guiAdapters.preferredEchoLineLength:=-1;

    currentExpressionMemo.Font:=OutputEdit.Font;
    assistanceSynEdit.Font:=OutputEdit.Font;
    helpPopupMemo.Font:=OutputEdit.Font;
    helpPopupMemo.Font.size:=helpPopupMemo.Font.size-2;
  end;

PROCEDURE TMnhForm.miDecFontSizeClick(Sender: TObject);
  begin
    if settingsReady then begin
      SettingsForm.fontSize:=SettingsForm.fontSize-1;
      processSettings;
    end;
  end;

PROCEDURE TMnhForm.miIncFontSizeClick(Sender: TObject);
  begin
    if settingsReady then begin
      SettingsForm.fontSize:=SettingsForm.fontSize+1;
      processSettings;
    end;
  end;

PROCEDURE TMnhForm.miDeclarationEchoClick(Sender: TObject);
  begin
    if settingsReady then begin
      miDeclarationEcho.Checked:=not(miDeclarationEcho.Checked);
      guiOutAdapter.enableMessageType(miDeclarationEcho.Checked,[mt_echo_declaration]);
      settings.value^.outputBehaviour:=guiOutAdapter.outputBehavior;
    end;
  end;

PROCEDURE TMnhForm.miExpressionEchoClick(Sender: TObject);
  begin
    if settingsReady then begin
      miExpressionEcho.Checked:=not(miExpressionEcho.Checked);
      guiOutAdapter.enableMessageType(miExpressionEcho.Checked,[mt_echo_input]);
      settings.value^.outputBehaviour:=guiOutAdapter.outputBehavior;
    end;
  end;

PROCEDURE TMnhForm.miExpressionResultClick(Sender: TObject);
  begin
    if settingsReady then begin
      miExpressionResult.Checked:=not(miExpressionResult.Checked);
      guiOutAdapter.enableMessageType(miExpressionResult.Checked,[mt_echo_output]);
      settings.value^.outputBehaviour:=guiOutAdapter.outputBehavior;
    end;
  end;

PROCEDURE TMnhForm.miTimingInfoClick(Sender: TObject);
  begin
    if settingsReady then begin
      miTimingInfo.Checked:=not(miTimingInfo.Checked);
      guiOutAdapter.enableMessageType(miTimingInfo.Checked,[mt_timing_info]);
      settings.value^.outputBehaviour:=guiOutAdapter.outputBehavior;
    end;
  end;

PROCEDURE TMnhForm.miWrapEchoClick(Sender: TObject);
  begin
    settings.value^.wordWrapEcho:=not(settings.value^.wordWrapEcho);
    miWrapEcho.Checked:=settings.value^.wordWrapEcho;
    processSettings;
  end;

PROCEDURE TMnhForm.mi_settingsClick(Sender: TObject);
  begin
    SettingsForm.ShowModal;
    processSettings;
  end;
{$endif}
