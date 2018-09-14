PROCEDURE initializeOutputSettings;
  VAR j:byte;
      maxLevel:byte=5;
  begin
    with MnhForm do begin
      miDeclarationEcho .checked:=mt_echo_declaration in settings.outputBehaviour;
      miExpressionEcho  .checked:=mt_echo_input       in settings.outputBehaviour;
      miExpressionResult.checked:=mt_echo_output      in settings.outputBehaviour;
      miTimingInfo      .checked:=mt_timing_info      in settings.outputBehaviour;;
      miWrapEcho        .checked:=                       settings.wordWrapEcho;
      for j:=4 downto 1 do if (C_errorMessageTypes[j] * settings.outputBehaviour <> []) then maxLevel:=j;
      miMinErrorlevel1.checked:=maxLevel= 1;
      miMinErrorlevel2.checked:=maxLevel= 2;
      miMinErrorlevel3.checked:=maxLevel= 3;
      miMinErrorlevel4.checked:=maxLevel>=4;
      miUserErrors    .checked:=(mt_el1_userNote    in settings.outputBehaviour)
                            and (mt_el2_userWarning in settings.outputBehaviour)
                            and (mt_el3_userDefined in settings.outputBehaviour);
    end;
    guiOutAdapter.outputBehavior:=settings.outputBehaviour+C_messagesAlwaysProcessedInGuiMode;
    MnhForm.quick.adapters^.getAdapter(0)^.outputBehavior:=guiOutAdapter.outputBehavior;
    setupOutputBehaviourFromCommandLineOptions(guiAdapters,nil);
  end;

PROCEDURE setVerbosity(level:byte);
  VAR j:byte;
  begin
    case level of
      1: begin
           MnhForm.miMinErrorlevel1.checked:=true;
           MnhForm.miUserErrors    .checked:=true;
         end;
      2: MnhForm.miMinErrorlevel2.checked:=true;
      3: MnhForm.miMinErrorlevel3.checked:=true;
      4: MnhForm.miMinErrorlevel4.checked:=true;
    255: MnhForm.miUserErrors    .checked:=true;
    254: begin
           MnhForm.miUserErrors  .checked:=MnhForm.miMinErrorlevel1.checked;
           if MnhForm.miMinErrorlevel1.checked then level:=1;
           if MnhForm.miMinErrorlevel2.checked then level:=2;
           if MnhForm.miMinErrorlevel3.checked then level:=3;
           if MnhForm.miMinErrorlevel4.checked then level:=4;
         end;
    end;
    if level in [1..4] then for j:=1 to 4 do guiOutAdapter.enableMessageType(j>=level,C_errorMessageTypes[j]);
    if MnhForm.miUserErrors.checked then guiOutAdapter.enableMessageType(true,[mt_el1_userNote,mt_el2_userWarning,mt_el3_userDefined]);
    settings.outputBehaviour:=guiOutAdapter.outputBehavior;
    MnhForm.quick.adapters^.getAdapter(0)^.outputBehavior:=guiOutAdapter.outputBehavior;
  end;

PROCEDURE setSpecificMessageType(CONST enable:boolean; CONST messageType:T_messageType);
  begin
    guiOutAdapter.enableMessageType(enable,[messageType]);
    settings.outputBehaviour:=guiOutAdapter.outputBehavior;
    MnhForm.quick.adapters^.getAdapter(0)^.outputBehavior:=guiOutAdapter.outputBehavior;
  end;

PROCEDURE updateWordWrap;
  begin
    if settings.wordWrapEcho
    then guiAdapters.preferredEchoLineLength:=MnhForm.outputEdit.charsInWindow-6
    else guiAdapters.preferredEchoLineLength:=-1;
    MnhForm.quick.adapters^.preferredEchoLineLength:=guiAdapters.preferredEchoLineLength;
  end;

PROCEDURE processFontSettings;
  begin
    with MnhForm do begin
      outputEdit.Font.name:=settings.editorFontname;
      outputEdit.Font.size:=SettingsForm.fontSize;
      if SettingsForm.AntialiasCheckbox.checked
      then outputEdit.Font.quality:=fqCleartypeNatural
      else outputEdit.Font.quality:=fqNonAntialiased;

      quick.meta.editor.Font:=outputEdit.Font;
      QuickOutputEdit.Font:=outputEdit.Font;
      currentExpressionMemo.Font:=outputEdit.Font;
      editorMeta.updateFonts(outputEdit.Font);
      helpPopupMemo.Font:=outputEdit.Font;
      assistanceSynEdit.Font:=outputEdit.Font;
      updateWordWrap;
    end;
    conditionalShowCustomForms(guiAdapters);
  end;

PROCEDURE initializeSettings;
  VAR formPosition:T_formPosition;
  begin
    {$ifdef UNIX}
    MnhForm.miIncFontSize.ShortCut:=16605;
    {$endif}

    if settings.loaded then begin
      formPosition:=settings.mainForm;
      MnhForm.top   :=formPosition.top;
      MnhForm.Left  :=formPosition.Left;
      MnhForm.width :=formPosition.width;
      MnhForm.height:=formPosition.height;
      if formPosition.isFullscreen then MnhForm.WindowState:=wsMaximized;
      MnhForm.BottomPanel.height:=round(MnhForm.ClientHeight*formPosition.relativeSplitterPosition);
      MnhForm.StatusBar.top:=MnhForm.BottomPanel.top+MnhForm.BottomPanel.height;
    end else settings.mainForm.relativeSplitterPosition:=MnhForm.BottomPanel.height/MnhForm.ClientHeight;
    SettingsForm.ensureFont(MnhForm.outputEdit.Font);
    initializeOutputSettings;
    processFontSettings;
  end;

PROCEDURE updateFormPositionInSettings;
  begin
    with settings do begin
      mainForm.top   :=MnhForm.top;
      mainForm.Left  :=MnhForm.Left;
      mainForm.width :=MnhForm.width;
      mainForm.height:=MnhForm.height;
      mainForm.isFullscreen:=(MnhForm.WindowState=wsMaximized);
      MnhForm.BottomPanel.height:=round(MnhForm.ClientHeight*settings.mainForm.relativeSplitterPosition);
    end;
  end;
