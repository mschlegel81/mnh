UNIT outputFormUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  sysutils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  SynEdit, SynHighlighterMnh, ideLayoutUtil, guiOutAdapters,mnh_settings,out_adapters,synOutAdapter,mnh_messages;

TYPE
  P_ideStdOutAdapter=^T_ideStdOutAdapter;
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
    associatedAdapter:P_ideStdOutAdapter;
    PROCEDURE updateWordWrap;
  public
  end;

  T_ideStdOutAdapter=object(T_synOutAdapter)
    private
      parent:P_messages;
    public
      CONSTRUCTOR create(CONST parent_:P_messages);
      FUNCTION flushToGui:T_messageTypeSet; virtual;
      PROCEDURE ensureForm;
      PROCEDURE showForm;
  end;

IMPLEMENTATION
{$R *.lfm}

CONSTRUCTOR T_ideStdOutAdapter.create(CONST parent_: P_messages);
  begin
    parent:=parent_;
    inherited create(nil,nil);
  end;

FUNCTION T_ideStdOutAdapter.flushToGui: T_messageTypeSet;
  begin
    ensureForm;
    result:=inherited flushToGui;
  end;

PROCEDURE T_ideStdOutAdapter.ensureForm;
  VAR outputForm:TOutputForm;
  begin
    if synOwnerForm=nil then begin
      outputForm:=TOutputForm.create(Application);
      synOwnerForm:=outputForm;
      syn         :=outputForm.OutputSynEdit;
      outputForm.associatedAdapter:=@self;
      dockNewForm(outputForm);
    end;
  end;

PROCEDURE T_ideStdOutAdapter.showForm;
  begin
    ensureForm;
    TOutputForm(synOwnerForm).showComponent;
  end;

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

PROCEDURE TOutputForm.cbShowOnOutputChange(Sender: TObject);
  begin
    associatedAdapter^.jumpToEnd:=cbShowOnOutput.checked;
  end;

FUNCTION TOutputForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icOutput;
  end;

PROCEDURE TOutputForm.updateWordWrap;
  begin
    if outputBehavior.echo_wrapping
    then associatedAdapter^.parent^.preferredEchoLineLength:=OutputSynEdit.charsInWindow-6
    else associatedAdapter^.parent^.preferredEchoLineLength:=-1;

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
    associatedAdapter^.outputBehavior:=outputBehavior;
    associatedAdapter^.wrapEcho:=outputBehavior.echo_wrapping;
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
    associatedAdapter^.jumpToEnd:=cbShowOnOutput.checked;
    if not(cbFreezeOutput.checked) then begin
      if (associatedAdapter^.flushToGui<>[])
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

