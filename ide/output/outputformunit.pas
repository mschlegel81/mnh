UNIT outputFormUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  sysutils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  SynEdit, SynHighlighterMnh, ideLayoutUtil, guiOutAdapters,mnh_settings,out_adapters,synOutAdapter,mnh_messages, Classes;

TYPE
  TIsRunningFunc=FUNCTION:boolean of object;
  TOutputForm = class;
  P_lazyInitializedOutAdapter=^T_lazyInitializedOutAdapter;
  T_lazyInitializedOutAdapter=object(T_abstractSynOutAdapter)
    private
      outputForm:TOutputForm;
      formCaption:string;
      running:TIsRunningFunc;
    protected
      FUNCTION getSynEdit:TSynEdit; virtual;
      FUNCTION getOwnerForm:TForm;  virtual;
    public
      CONSTRUCTOR create(CONST isRunning:TIsRunningFunc;
                         CONST caption:string;
                         CONST messageTypesToInc:T_messageTypeSet=[mt_clearConsole,
                                                                   mt_printline,
                                                                   mt_printdirect,
                                                                   mt_el1_note,
                                                                   mt_el1_userNote,
                                                                   mt_el2_warning,
                                                                   mt_el2_userWarning,
                                                                   mt_echo_input,
                                                                   mt_echo_output,
                                                                   mt_echo_declaration,
                                                                   mt_echo_continued,
                                                                   mt_startOfEvaluation,
                                                                   mt_endOfEvaluation]);
      FUNCTION ensureOutputForm:TOutputForm;
  end;

  TOutputForm = class(T_mnhComponentForm)
    cbShowOnOutput: TCheckBox;
    cbFreezeOutput: TCheckBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    miDockMainRoot: TMenuItem;
    miOutputMainRoot: TMenuItem;
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
    outputHighlighter:TMnhOutputSyn;
    OutputPopupMenu: TPopupMenu;
    PROCEDURE cbShowOnOutputChange(Sender: TObject);
    PROCEDURE FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE miEchoDeclarationsClick(Sender: TObject);
    PROCEDURE performSlowUpdate(CONST isEvaluationRunning:boolean); override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE dockChanged; override;
  private
    PROCEDURE updateWordWrap;
    PROCEDURE updateAfterSettingsRestore;
  public
    adapter:P_lazyInitializedOutAdapter;
  end;

IMPLEMENTATION
USES editorMeta;

{$R *.lfm}

FUNCTION T_lazyInitializedOutAdapter.getSynEdit: TSynEdit;
  begin
    result:=ensureOutputForm.OutputSynEdit;
  end;

FUNCTION T_lazyInitializedOutAdapter.getOwnerForm: TForm;
  begin
    result:=ensureOutputForm;
  end;

CONSTRUCTOR T_lazyInitializedOutAdapter.create(CONST isRunning:TIsRunningFunc; CONST caption:string; CONST messageTypesToInc: T_messageTypeSet);
  begin
    inherited create(messageTypesToInc);
    formCaption:=caption;
    outputForm:=nil;
    running:=isRunning;
  end;

FUNCTION T_lazyInitializedOutAdapter.ensureOutputForm: TOutputForm;
  begin
    if outputForm=nil then begin
      outputForm:=TOutputForm.create(Application);
      outputForm.caption:=formCaption;
      outputForm.adapter:=@self;
      dockNewForm(outputForm);
      outputForm.updateAfterSettingsRestore;
      outputForm.showComponent(false);
    end;
    result:=outputForm;
  end;

PROCEDURE TOutputForm.updateAfterSettingsRestore;
  begin
    with guiOutAdapters.outputBehavior do begin
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
    adapter^.outputBehavior:=guiOutAdapters.outputBehavior;
    adapter^.autoflush:=false;
  end;

PROCEDURE TOutputForm.FormCreate(Sender: TObject);
  begin
    registerFontControl(OutputSynEdit,ctEditor);
    outputHighlighter:=TMnhOutputSyn.create(self);
    OutputSynEdit.highlighter:=outputHighlighter;
    OutputSynEdit.OnKeyUp:=@workspace.keyUpForJumpToLocation;
    OutputSynEdit.OnMouseDown:=@workspace.mouseDownForJumpToLocation;
    initDockMenuItems(MainMenu1,miDockMainRoot);
    initDockMenuItems(OutputPopupMenu,nil);
  end;

PROCEDURE TOutputForm.FormDestroy(Sender: TObject);
  begin
    unregisterFontControl(OutputSynEdit);
    adapter^.outputForm:=nil;
    adapter^.autoflush:=true;
    adapter^.jumpToEnd:=true;
  end;

PROCEDURE TOutputForm.FormResize(Sender: TObject);
  begin
    updateWordWrap;
  end;

PROCEDURE TOutputForm.FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
  begin
    CanClose:=not(adapter^.running());
  end;

PROCEDURE TOutputForm.cbShowOnOutputChange(Sender: TObject);
  begin
    adapter^.jumpToEnd:=cbShowOnOutput.checked;
  end;

FUNCTION TOutputForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icOutput;
  end;

PROCEDURE TOutputForm.updateWordWrap;
  begin
    if adapter^.parentMessages=nil then exit;
    if outputBehavior.echo_wrapping
    then adapter^.parentMessages^.preferredEchoLineLength:=OutputSynEdit.charsInWindow-6
    else adapter^.parentMessages^.preferredEchoLineLength:=-1;
  end;

PROCEDURE TOutputForm.miEchoDeclarationsClick(Sender: TObject);
  begin
    with guiOutAdapters.outputBehavior do begin
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
    adapter^.outputBehavior:=guiOutAdapters.outputBehavior;
    adapter^.wrapEcho:=outputBehavior.echo_wrapping;
    updateWordWrap;
  end;

PROCEDURE TOutputForm.performSlowUpdate(CONST isEvaluationRunning:boolean);
  begin
    if not(isEvaluationRunning) then cbFreezeOutput.checked:=false;
    if cbFreezeOutput.checked
    then OutputSynEdit.color:=CL_INACTIVE_GREY
    else OutputSynEdit.color:=clWhite;
  end;

PROCEDURE TOutputForm.performFastUpdate;
  begin
    adapter^.jumpToEnd:=cbShowOnOutput.checked;
    if not(cbFreezeOutput.checked) then begin
      if (adapter^.flushToGui(true)<>[])
      and (cbShowOnOutput.checked)
      then showComponent(true);
    end;
  end;

PROCEDURE TOutputForm.dockChanged;
  begin
    if myComponentParent=cpNone
    then moveAllItems(OutputPopupMenu.items,miOutputMainRoot)
    else moveAllItems(miOutputMainRoot,OutputPopupMenu.items);
  end;

end.

