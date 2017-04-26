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
    end;
    guiOutAdapter.outputBehavior:=settings.value^.outputBehaviour;
    setupOutputBehaviourFromCommandLineOptions(guiAdapters,nil);
  end;

PROCEDURE setVerbosity(CONST level:byte);
  VAR j:byte;
  begin
    case level of
      1: MnhForm.miMinErrorlevel1.Checked:=true;
      2: MnhForm.miMinErrorlevel2.Checked:=true;
      3: MnhForm.miMinErrorlevel3.Checked:=true;
      4: MnhForm.miMinErrorlevel4.Checked:=true;
    end;
    for j:=1 to 4 do guiOutAdapter.enableMessageType(j>=level,C_errorMessageTypes[j]);
    settings.value^.outputBehaviour:=guiOutAdapter.outputBehavior;
  end;

PROCEDURE setSpecificMessageType(CONST enable:boolean; CONST messageType:T_messageType);
  begin
    guiOutAdapter.enableMessageType(enable,[messageType]);
    settings.value^.outputBehaviour:=guiOutAdapter.outputBehavior;
  end;

PROCEDURE updateWordWrap;
  begin
    if settings.value^.wordWrapEcho
    then guiAdapters.preferredEchoLineLength:=MnhForm.OutputEdit.charsInWindow-6
    else guiAdapters.preferredEchoLineLength:=-1;
  end;

PROCEDURE processFontSettings;
  VAR e:P_editorMeta;
  begin
    with MnhForm do begin
      OutputEdit.Font.name:=settings.value^.editorFontname;
      OutputEdit.Font.size:=SettingsForm.fontSize;
      if SettingsForm.AntialiasCheckbox.Checked
      then OutputEdit.Font.quality:=fqCleartypeNatural
      else OutputEdit.Font.quality:=fqNonAntialiased;
      for e in editorMetaData do e^.editor.Font:=OutputEdit.Font;
      currentExpressionMemo.Font:=OutputEdit.Font;
      assistanceSynEdit.Font:=OutputEdit.Font;
      helpPopupMemo.Font:=OutputEdit.Font;
      helpPopupMemo.Font.size:=helpPopupMemo.Font.size-2;
      updateWordWrap;
    end;
  end;
