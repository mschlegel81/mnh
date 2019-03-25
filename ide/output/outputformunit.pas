UNIT outputFormUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  SynEdit, SynHighlighterMnh, ideLayoutUtil, guiOutAdapters, mnh_messages,mnh_settings;

TYPE

  { TOutputForm }

  TOutputForm = class(T_mnhComponentForm)
    cbShowOnOutput: TCheckBox;
    cbFreezeOutput: TCheckBox;
    MenuItem1: TMenuItem;
    miErrorUser: TMenuItem;
    miErrorL4: TMenuItem;
    miErrorL3: TMenuItem;
    miErrorL2: TMenuItem;
    miErrorL1: TMenuItem;
    miShowTiming: TMenuItem;
    miWrapEcho: TMenuItem;
    miEchoDeclarations: TMenuItem;
    miEchoInput: TMenuItem;
    miEchoOutput: TMenuItem;
    OutputSynEdit: TSynEdit;
    outputHighlighter:TSynMnhSyn;
    OutputPopupMenu: TPopupMenu;
    PROCEDURE FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE miEchoDeclarationsClick(Sender: TObject);
    PROCEDURE performSlowUpdate; override;
    PROCEDURE performFastUpdate; override;
  private
    PROCEDURE updateWordWrap;
  public
  end;

PROCEDURE ensureOutputForm;
PROCEDURE unfreezeOutput;
IMPLEMENTATION

PROCEDURE ensureOutputForm;
  begin
    if not(hasFormOfType(icOutput,true))
    then dockNewForm(TOutputForm.create(Application));
  end;

PROCEDURE unfreezeOutput;
  begin
    TOutputForm(getFormOfType(icOutput)).
    cbFreezeOutput.checked:=false;
  end;

{$R *.lfm}

PROCEDURE TOutputForm.FormCreate(Sender: TObject);
  begin
    registerFontControl(OutputSynEdit,ctEditor);
    outputHighlighter:=TSynMnhSyn.create(self,msf_output);
    OutputSynEdit.highlighter:=outputHighlighter;
  end;

PROCEDURE TOutputForm.FormResize(Sender: TObject);
  begin
    updateWordWrap;
  end;

PROCEDURE TOutputForm.FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
  begin
    CanClose:=false;
  end;

FUNCTION TOutputForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icOutput;
  end;

PROCEDURE TOutputForm.updateWordWrap;
  begin
    if outputBehavior.echo_wrapping
    then guiAdapters.preferredEchoLineLength:=OutputSynEdit.charsInWindow-6
    else guiAdapters.preferredEchoLineLength:=-1;

    //TODO: Implement this in quick edit form
    //if settings.quickOutputBehavior.echo_wrapping
    //then MnhForm.quick.adapters^.preferredEchoLineLength:=MnhForm.outputEdit.charsInWindow-6
    //else MnhForm.quick.adapters^.preferredEchoLineLength:=-1;
  end;

PROCEDURE TOutputForm.miEchoDeclarationsClick(Sender: TObject);
  begin
    with outputBehavior do begin
      echo_declaration     :=miEchoDeclarations .checked;
      echo_input           :=miEchoInput        .checked;
      echo_output          :=miEchoOutput       .checked;
      echo_wrapping        :=miWrapEcho         .checked;
      show_timing          :=miShowTiming       .checked;
      show_all_userMessages:=miErrorUser .checked;
      if miErrorL1.checked then suppressWarningsUnderLevel:=1;
      if miErrorL2.checked then suppressWarningsUnderLevel:=2;
      if miErrorL3.checked then suppressWarningsUnderLevel:=3;
      if miErrorL4.checked then suppressWarningsUnderLevel:=4;
    end;
    guiOutAdapter.outputBehavior:=outputBehavior;
    guiOutAdapter.wrapEcho:=outputBehavior.echo_wrapping;
    updateWordWrap;
  end;

PROCEDURE TOutputForm.performSlowUpdate;
  begin
    if cbFreezeOutput.checked
    then OutputSynEdit.color:=clSilver
    else OutputSynEdit.color:=clWhite;
  end;

PROCEDURE TOutputForm.performFastUpdate;
  VAR oldActive:TWinControl;
  begin
    if not(cbFreezeOutput.checked) then begin
      if (guiOutAdapter.flushToGui(cbShowOnOutput.checked)<>[])
      and (cbShowOnOutput.checked)
      then begin
        if mainForm<>nil
        then oldActive:=mainForm.ActiveControl
        else oldActive:=nil;
        showComponent;
        if oldActive<>nil then mainForm.ActiveControl:=oldActive;
      end;
    end;
  end;

end.

