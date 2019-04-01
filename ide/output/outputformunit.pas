UNIT outputFormUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  sysutils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  SynEdit, SynHighlighterMnh, ideLayoutUtil, guiOutAdapters,mnh_settings,out_adapters,synOutAdapter,mnh_messages;

TYPE
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
    PROCEDURE cbShowOnOutputChange(Sender: TObject);
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
    adapter:T_synOutAdapter;
  end;

FUNCTION ensureStdOutAdapter:TOutputForm;
IMPLEMENTATION

FUNCTION ensureStdOutAdapter: TOutputForm;
  begin
    if not(hasFormOfType(icOutput,true)) then begin
      result:=TOutputForm.create(Application);
      dockNewForm(result);
    end else result:=TOutputForm(getFormOfType(icOutput));
  end;

{$R *.lfm}

PROCEDURE TOutputForm.FormCreate(Sender: TObject);
  begin
    registerFontControl(OutputSynEdit,ctEditor);
    outputHighlighter:=TSynMnhSyn.create(self,msf_output);
    OutputSynEdit.highlighter:=outputHighlighter;
    with outputBehavior do begin
      miEchoDeclarations .checked:=echo_declaration     ;
      miEchoInput        .checked:=echo_input           ;
      miEchoOutput       .checked:=echo_output          ;
      miWrapEcho         .checked:=echo_wrapping        ;
      miShowTiming       .checked:=show_timing          ;
      miErrorUser        .checked:=show_all_userMessages;
      miErrorL1.checked:=suppressWarningsUnderLevel=1;
      miErrorL2.checked:=suppressWarningsUnderLevel=2;
      miErrorL3.checked:=suppressWarningsUnderLevel=3;
      miErrorL4.checked:=suppressWarningsUnderLevel=4;
    end;
    adapter.create(self,OutputSynEdit,outputBehavior);
  end;

PROCEDURE TOutputForm.FormResize(Sender: TObject);
  begin
    updateWordWrap;
  end;

PROCEDURE TOutputForm.FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
  begin
    CanClose:=false;
  end;

PROCEDURE TOutputForm.cbShowOnOutputChange(Sender: TObject);
  begin
    adapter.jumpToEnd:=cbShowOnOutput.checked;
  end;

FUNCTION TOutputForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icOutput;
  end;

PROCEDURE TOutputForm.updateWordWrap;
  begin
    if adapter.parentMessages=nil then exit;
    if outputBehavior.echo_wrapping
    then adapter.parentMessages^.preferredEchoLineLength:=OutputSynEdit.charsInWindow-6
    else adapter.parentMessages^.preferredEchoLineLength:=-1;

    //TODO: Implement this in quick edit form
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
    adapter.outputBehavior:=outputBehavior;
    adapter.wrapEcho:=outputBehavior.echo_wrapping;
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
    adapter.jumpToEnd:=cbShowOnOutput.checked;
    if not(cbFreezeOutput.checked) then begin
      if (adapter.flushToGui<>[])
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

