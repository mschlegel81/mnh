PROCEDURE initializeOutputSettings;
  VAR j:byte;
      maxLevel:byte=5;
  begin
    with MnhForm do begin
      miDeclarationEcho .Checked:=mt_echo_declaration in settings.value^.outputBehaviour;
      miExpressionEcho  .Checked:=mt_echo_input       in settings.value^.outputBehaviour;
      miExpressionResult.Checked:=mt_echo_output      in settings.value^.outputBehaviour;
      miTimingInfo      .Checked:=mt_timing_info      in settings.value^.outputBehaviour;;
      miWrapEcho        .Checked:=                       settings.value^.wordWrapEcho;
      for j:=4 downto 1 do if (C_errorMessageTypes[j] * settings.value^.outputBehaviour <> []) then maxLevel:=j;
      miMinErrorlevel1.Checked:=maxLevel= 1;
      miMinErrorlevel2.Checked:=maxLevel= 2;
      miMinErrorlevel3.Checked:=maxLevel= 3;
      miMinErrorlevel4.Checked:=maxLevel>=4;
      miUserErrors    .Checked:=(mt_el1_userNote    in settings.value^.outputBehaviour)
                            and (mt_el2_userWarning in settings.value^.outputBehaviour)
                            and (mt_el3_userDefined in settings.value^.outputBehaviour);
    end;
    guiOutAdapter.outputBehavior:=settings.value^.outputBehaviour+C_guiOnlyMessages;
    MnhForm.quick.adapters^.getAdapter(0)^.outputBehavior:=guiOutAdapter.outputBehavior;
    setupOutputBehaviourFromCommandLineOptions(guiAdapters,nil);
  end;

PROCEDURE setVerbosity(level:byte);
  VAR j:byte;
  begin
    case level of
      1: begin
           MnhForm.miMinErrorlevel1.Checked:=true;
           MnhForm.miUserErrors    .Checked:=true;
         end;
      2: MnhForm.miMinErrorlevel2.Checked:=true;
      3: MnhForm.miMinErrorlevel3.Checked:=true;
      4: MnhForm.miMinErrorlevel4.Checked:=true;
    255: MnhForm.miUserErrors    .Checked:=true;
    254: begin
           MnhForm.miUserErrors  .Checked:=MnhForm.miMinErrorlevel1.Checked;
           if MnhForm.miMinErrorlevel1.Checked then level:=1;
           if MnhForm.miMinErrorlevel2.Checked then level:=2;
           if MnhForm.miMinErrorlevel3.Checked then level:=3;
           if MnhForm.miMinErrorlevel4.Checked then level:=4;
         end;
    end;
    if level in [1..4] then for j:=1 to 4 do guiOutAdapter.enableMessageType(j>=level,C_errorMessageTypes[j]);
    if MnhForm.miUserErrors.Checked then guiOutAdapter.enableMessageType(true,[mt_el1_userNote,mt_el2_userWarning,mt_el3_userDefined]);
    settings.value^.outputBehaviour:=guiOutAdapter.outputBehavior;
    MnhForm.quick.adapters^.getAdapter(0)^.outputBehavior:=guiOutAdapter.outputBehavior;
  end;

PROCEDURE setSpecificMessageType(CONST enable:boolean; CONST messageType:T_messageType);
  begin
    guiOutAdapter.enableMessageType(enable,[messageType]);
    settings.value^.outputBehaviour:=guiOutAdapter.outputBehavior;
    MnhForm.quick.adapters^.getAdapter(0)^.outputBehavior:=guiOutAdapter.outputBehavior;
  end;

PROCEDURE updateWordWrap;
  begin
    if settings.value^.wordWrapEcho
    then guiAdapters.preferredEchoLineLength:=MnhForm.OutputEdit.charsInWindow-6
    else guiAdapters.preferredEchoLineLength:=-1;
    MnhForm.quick.adapters^.preferredEchoLineLength:=guiAdapters.preferredEchoLineLength;
  end;

PROCEDURE processFontSettings;
  begin
    with MnhForm do begin
      OutputEdit.Font.name:=settings.value^.editorFontname;
      OutputEdit.Font.size:=SettingsForm.fontSize;
      if SettingsForm.AntialiasCheckbox.Checked
      then OutputEdit.Font.quality:=fqCleartypeNatural
      else OutputEdit.Font.quality:=fqNonAntialiased;

      quick.meta.editor.Font:=OutputEdit.Font;
      QuickOutputEdit.Font:=OutputEdit.Font;
      currentExpressionMemo.Font:=OutputEdit.Font;
      editorMeta.updateFonts(OutputEdit.Font);
      helpPopupMemo.Font:=OutputEdit.Font;
      assistanceSynEdit.Font:=OutputEdit.Font;
      outlineSynEdit.Font:=OutputEdit.Font;
      outlineSynEdit.Font.size:=outlineSynEdit.Font.size-2;
      updateWordWrap;
    end;
  end;

PROCEDURE initializeSettings;
  VAR formPosition:T_formPosition;
  begin
    {$ifdef UNIX}
    MnhForm.miIncFontSize.ShortCut:=16605;
    {$endif}

    if settings.value^.loaded then begin
      formPosition:=settings.value^.mainForm;
      MnhForm.top   :=formPosition.top;
      MnhForm.Left  :=formPosition.Left;
      MnhForm.width :=formPosition.width;
      MnhForm.height:=formPosition.height;
      if formPosition.isFullscreen then MnhForm.WindowState:=wsMaximized;
      MnhForm.BottomPanel.height:=round(MnhForm.ClientHeight*formPosition.relativeSplitterPosition);
      MnhForm.StatusBar.top:=MnhForm.BottomPanel.top+MnhForm.BottomPanel.height;
    end else settings.value^.mainForm.relativeSplitterPosition:=MnhForm.BottomPanel.height/MnhForm.ClientHeight;
    SettingsForm.ensureFont(MnhForm.OutputEdit.Font);
    initializeOutputSettings;
    processFontSettings;
  end;

PROCEDURE updateFormPositionInSettings;
  begin
    with settings.value^ do begin
      mainForm.top   :=MnhForm.top;
      mainForm.Left  :=MnhForm.Left;
      mainForm.width :=MnhForm.width;
      mainForm.height:=MnhForm.height;
      mainForm.isFullscreen:=(MnhForm.WindowState=wsMaximized);
      MnhForm.BottomPanel.height:=round(MnhForm.ClientHeight*settings.value^.mainForm.relativeSplitterPosition);
    end;
  end;
