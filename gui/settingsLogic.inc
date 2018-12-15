PROCEDURE initializeOutputSettings;
  begin
    with MnhForm do begin
      miEchoDecl         .checked:=settings.     outputBehavior.echo_declaration;
      miEchoDecl1        .checked:=settings.quickOutputBehavior.echo_declaration;
      miEchoInput        .checked:=settings.     outputBehavior.echo_input;
      miEchoInput1       .checked:=settings.quickOutputBehavior.echo_input;
      miEchoOutput       .checked:=settings.     outputBehavior.echo_output;
      miEchoOutput1      .checked:=settings.quickOutputBehavior.echo_output;
      miWrapEcho         .checked:=settings.     outputBehavior.echo_wrapping;
      miWrapEcho1        .checked:=settings.quickOutputBehavior.echo_wrapping;
      miShowTiming       .checked:=settings.     outputBehavior.show_timing;
      miShowTiming1      .checked:=settings.quickOutputBehavior.show_timing;
      miErrorUserDefined .checked:=settings.     outputBehavior.show_all_userMessages;
      miErrorUserDefined1.checked:=settings.quickOutputBehavior.show_all_userMessages;
      miErrorL1          .checked:=settings.     outputBehavior.suppressWarningsUnderLevel<=1;
      miErrorL5          .checked:=settings.quickOutputBehavior.suppressWarningsUnderLevel<=1;
      miErrorL2          .checked:=settings.     outputBehavior.suppressWarningsUnderLevel =2;
      miErrorL6          .checked:=settings.quickOutputBehavior.suppressWarningsUnderLevel =2;
      miErrorL3          .checked:=settings.     outputBehavior.suppressWarningsUnderLevel =3;
      miErrorL7          .checked:=settings.quickOutputBehavior.suppressWarningsUnderLevel =3;
      miErrorL4          .checked:=settings.     outputBehavior.suppressWarningsUnderLevel>=4;
      miErrorL8          .checked:=settings.quickOutputBehavior.suppressWarningsUnderLevel>=4;
    end;
    guiOutAdapter.outputBehavior                                                :=settings.outputBehavior;
    P_messagesDistributor(MnhForm.quick.adapters)^.getAdapter(0)^.outputBehavior:=settings.quickOutputBehavior;
    guiOutAdapter                                                                 .wrapEcho:=settings.outputBehavior     .echo_wrapping;
    P_synOutAdapter(P_messagesDistributor(MnhForm.quick.adapters)^.getAdapter(0))^.wrapEcho:=settings.quickOutputBehavior.echo_wrapping;
    setupOutputBehaviourFromCommandLineOptions(@guiAdapters,nil);
  end;

PROCEDURE updateWordWrap;
  begin
    if settings.outputBehavior.echo_wrapping
    then guiAdapters.preferredEchoLineLength:=MnhForm.outputEdit.charsInWindow-6
    else guiAdapters.preferredEchoLineLength:=-1;
    if settings.quickOutputBehavior.echo_wrapping
    then MnhForm.quick.adapters^.preferredEchoLineLength:=MnhForm.outputEdit.charsInWindow-6
    else MnhForm.quick.adapters^.preferredEchoLineLength:=-1;
  end;

PROCEDURE TMnhForm.stdOutputPopupClick(Sender:TObject);
  begin
    with settings.outputBehavior do begin
      echo_declaration     :=miEchoDecl         .checked;
      echo_input           :=miEchoInput        .checked;
      echo_output          :=miEchoOutput       .checked;
      echo_wrapping        :=miWrapEcho         .checked;
      show_timing          :=miShowTiming       .checked;
      show_all_userMessages:=miErrorUserDefined .checked;
      if miErrorL1.checked then suppressWarningsUnderLevel:=1;
      if miErrorL2.checked then suppressWarningsUnderLevel:=2;
      if miErrorL3.checked then suppressWarningsUnderLevel:=3;
      if miErrorL4.checked then suppressWarningsUnderLevel:=4;
    end;
    guiOutAdapter.outputBehavior:=settings.outputBehavior;
    guiOutAdapter.wrapEcho:=settings.outputBehavior.echo_wrapping;
    updateWordWrap;
  end;

PROCEDURE TMnhForm.quickOutputPopupClick(Sender:TObject);
  begin
    with settings.quickOutputBehavior do begin
      echo_declaration     :=miEchoDecl1        .checked;
      echo_input           :=miEchoInput1       .checked;
      echo_output          :=miEchoOutput1      .checked;
      echo_wrapping        :=miWrapEcho1        .checked;
      show_timing          :=miShowTiming1      .checked;
      show_all_userMessages:=miErrorUserDefined1.checked;
      if miErrorL5.checked then suppressWarningsUnderLevel:=1;
      if miErrorL6.checked then suppressWarningsUnderLevel:=2;
      if miErrorL7.checked then suppressWarningsUnderLevel:=3;
      if miErrorL8.checked then suppressWarningsUnderLevel:=4;
    end;
    P_messagesDistributor(MnhForm.quick.adapters)^.getAdapter(0)^.outputBehavior:=settings.quickOutputBehavior;
    P_synOutAdapter(P_messagesDistributor(MnhForm.quick.adapters)^.getAdapter(0))^.wrapEcho:=settings.quickOutputBehavior.echo_wrapping;
    updateWordWrap;
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
    conditionalShowCustomForms(@guiAdapters);
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
