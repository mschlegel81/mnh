UNIT mnh_gui_settings;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, mnh_funcs, myGenerics, mySys, mnh_out_adapters,mnh_constants,
  mnh_packages,myStringUtil,mnh_settings;

CONST
  STATE_SAVE_INTERVAL=ONE_MINUTE;

TYPE

  { TSettingsForm }

  TSettingsForm = class(TForm)
    FontButton: TButton;
    AntialiasCheckbox: TCheckBox;
    FontSizeEdit: TEdit;
    EditorFontDialog: TFontDialog;
    Label2: TLabel;
    Label3: TLabel;
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    Label8: TLabel;
    TabSheet1: TTabSheet;
    Button1: TButton;
    Button2: TButton;
    uninstallToggleBox: TToggleBox;
    TabSheet3: TTabSheet;
    Label1: TLabel;
    Label4: TLabel;
    workerThreadCountEdit: TEdit;
    PROCEDURE FontButtonClick(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE Button1Click(Sender: TObject);
    PROCEDURE Button2Click(Sender: TObject);
    PROCEDURE workerThreadCountEditEditingDone(Sender: TObject);
  private
    { private declarations }
    //nonpersistent:
    savedAt:double;
    FUNCTION getFontSize: longint;
    PROCEDURE setFontSize(CONST value: longint);
    FUNCTION getMainFormPosition:T_formPosition;
    PROCEDURE setMainFormPosition(CONST value:T_formPosition);
    FUNCTION getInstantEvaluation:boolean;
    PROCEDURE setInstantEvaluation(CONST value:boolean);
    FUNCTION getOutputBehaviour:T_outputBehaviour;
    PROCEDURE setOutputBehaviour(CONST value:T_outputBehaviour);
    FUNCTION getResetPlotOnEvaluation:boolean;
    PROCEDURE setResetPlotOnEvaluation(CONST value:boolean);
    FUNCTION getPageIndex:longint;
    PROCEDURE setPageIndex(CONST value:longint);
  public
    { public declarations }
    PROPERTY fontSize: longint read getFontSize write setFontSize;
    PROPERTY mainFormPosition: T_formPosition read getMainFormPosition write setMainFormPosition;
    PROPERTY wantInstantEvaluation: boolean read getInstantEvaluation write setInstantEvaluation;
    PROPERTY behaviour:T_outputBehaviour read getOutputBehaviour write setOutputBehaviour;
    PROPERTY resetPlotOnEvaluation:boolean read getResetPlotOnEvaluation write setResetPlotOnEvaluation;
    PROPERTY pageIndex:longint read getPageIndex write setPageIndex;
    FUNCTION getEditorFontName: string;
    PROCEDURE saveSettings;
    FUNCTION timeForSaving:boolean;
    PROCEDURE setEditorState(CONST index:longint; CONST dat:T_editorState);
    FUNCTION  getEditorState(CONST index:longint):T_editorState;
    FUNCTION polishHistory: boolean;
    PROCEDURE fileClosed(CONST fileName:ansistring);
    FUNCTION historyItem(CONST index:longint):ansistring;
  end;

VAR
  SettingsForm: TSettingsForm;

IMPLEMENTATION

{$R *.lfm}

PROCEDURE TSettingsForm.FormCreate(Sender: TObject);
  PROCEDURE ensurePackages;
    {$i res_ensurePackages.inc}
    VAR code:T_arrayOfString;
        i:longint;
    begin
      setLength(code,length(ensurePackages_mnh));
      for i:=0 to length(code)-1 do code[i]:=ensurePackages_mnh[i];
      append(code,'('+escapeString(GetAppConfigDir(true))+')');
      runAlone(code);
    end;

  begin
    EditorFontDialog.Font.name := settings.value^.editorFontname;
    AntialiasCheckbox.Checked := settings.value^.antialiasedFonts;
    setFontSize(settings.value^.fontSize);
    if not(settings.value^.wasLoaded) then begin
      settings.value^.activePage:=0;
      ensurePackages;
    end;
    workerThreadCountEdit.text:=intToStr(settings.value^.workerThreadCount);
    FontButton.Font.name := settings.value^.editorFontname;
    FontButton.Font.size := getFontSize;
    FontButton.Caption := settings.value^.editorFontname;
    with settings.value^.mainForm do begin
      if top<0 then
        top := 0;
      if Left<0 then
        Left := 0;
      if height>screen.height-top then
        height := screen.height-top;
      if width>screen.width-Left then
        width := screen.width-Left;
      if (height<0) or (width<0) then
        begin
        top := 0;
        Left := 0;
        width := 480;
        height := 480;
        end;
    end;
    polishHistory;
    savedAt:=now;
  end;

PROCEDURE TSettingsForm.FontButtonClick(Sender: TObject);
  begin
    if EditorFontDialog.execute then begin
      setFontSize(EditorFontDialog.Font.size);
      settings.value^.editorFontname := EditorFontDialog.Font.name;

      FontButton.Font.name := settings.value^.editorFontname;
      FontButton.Font.size := getFontSize;
      FontButton.Caption := settings.value^.editorFontname;
    end;
  end;

PROCEDURE TSettingsForm.FormDestroy(Sender: TObject);
  begin
    saveSettings;
  end;

PROCEDURE TSettingsForm.Button1Click(Sender: TObject);
  {$i res_ensureNppHighlighting.inc}
  begin
    runAlone(ensureNotepad__Highlighting_mnh);
  end;

PROCEDURE TSettingsForm.Button2Click(Sender: TObject);
  {$i res_ensureMnhFileAssociations.inc}
  begin
    runAlone(ensureMnhFileAssociations_mnh);
  end;

PROCEDURE TSettingsForm.workerThreadCountEditEditingDone(Sender: TObject);
  VAR newValue:longint;
  begin
    newValue:=strToIntDef(workerThreadCountEdit.text,0);
    if newValue<=0 then workerThreadCountEdit.text:=intToStr(settings.value^.workerThreadCount)
                   else settings.value^.workerThreadCount:=newValue;
  end;

FUNCTION TSettingsForm.getFontSize: longint;
  begin
    result := StrToInt64Def(trim(FontSizeEdit.text), 12);
  end;

PROCEDURE TSettingsForm.setFontSize(CONST value: longint);
  begin
    FontSizeEdit.text := intToStr(value);
    EditorFontDialog.Font.size := value;
    settings.value^.fontSize:=value;
  end;

FUNCTION TSettingsForm.getMainFormPosition: T_formPosition;
  begin
    result:=settings.value^.mainForm;
  end;

PROCEDURE TSettingsForm.setMainFormPosition(CONST value: T_formPosition);
  begin
    settings.value^.mainForm:=value;
  end;

FUNCTION TSettingsForm.getInstantEvaluation: boolean;
  begin
    result:=settings.value^.instantEvaluation;
  end;

PROCEDURE TSettingsForm.setInstantEvaluation(CONST value: boolean);
  begin
    settings.value^.instantEvaluation:=value;
  end;

FUNCTION TSettingsForm.getOutputBehaviour: T_outputBehaviour;
  begin
    result:=settings.value^.outputBehaviour;
  end;

PROCEDURE TSettingsForm.setOutputBehaviour(CONST value: T_outputBehaviour);
  begin
    settings.value^.outputBehaviour:=value;
  end;

FUNCTION TSettingsForm.getResetPlotOnEvaluation: boolean;
  begin
    result:=settings.value^.doResetPlotOnEvaluation;
  end;

PROCEDURE TSettingsForm.setResetPlotOnEvaluation(CONST value: boolean);
  begin
    settings.value^.doResetPlotOnEvaluation:=value;
  end;

FUNCTION TSettingsForm.getPageIndex: longint;
  begin
    result:=settings.value^.activePage;
  end;

PROCEDURE TSettingsForm.setPageIndex(CONST value: longint);
  begin
    if (value>=0) and (value<=9) then settings.value^.activePage:=value;
  end;

FUNCTION TSettingsForm.getEditorFontName: string;
  begin
    result := settings.value^.editorFontname;
  end;

PROCEDURE TSettingsForm.saveSettings;
  begin
    mnh_settings.saveSettings;
    savedAt:=now;
  end;

FUNCTION TSettingsForm.timeForSaving: boolean;
  begin
    result:=(now-savedAt>STATE_SAVE_INTERVAL);
  end;

PROCEDURE TSettingsForm.setEditorState(CONST index: longint; CONST dat: T_editorState);
  begin
    settings.value^.editorState[index]:=dat;
  end;

FUNCTION TSettingsForm.getEditorState(CONST index: longint): T_editorState;
  begin
    result:=settings.value^.editorState[index];
  end;

FUNCTION TSettingsForm.polishHistory: boolean;
  VAR i, j: longint;
  begin
    with settings.value^ do begin
      result := false;
      for i:=0 to length(fileHistory)-1 do
        if not(fileExists(fileHistory[i])) then begin
          fileHistory[i]:='';
          result:=true;
        end;
      for i:=1 to length(fileHistory)-1 do
        if (fileHistory[i]<>'') then for j:=0 to i-1 do
          if (expandFileName(fileHistory[i])=expandFileName(fileHistory[j])) then begin
            fileHistory[i]:='';
            result:=true;
          end;
      for i := 0 to length(fileHistory)-1 do if (fileHistory [i]='') then begin
        for j := i to length(fileHistory)-2 do fileHistory[j] := fileHistory [j+1];
        fileHistory[length(fileHistory)-1] := '';
        result := true;
      end;
    end;
  end;

PROCEDURE TSettingsForm.fileClosed(CONST fileName:ansistring);
  VAR i:longint;
  begin
    with settings.value^ do begin
      for i:=0 to length(fileHistory)-1 do if fileHistory[i]='' then begin
        fileHistory[i]:=fileName;
        polishHistory;
        exit;
      end;
      for i:=0 to length(fileHistory)-2 do fileHistory[i]:=fileHistory[i+1];
      fileHistory[length(fileHistory)-1]:=fileName;
      polishHistory;
    end;
  end;

FUNCTION TSettingsForm.historyItem(CONST index: longint): ansistring;
  begin
    with settings.value^ do
    if (index>=0) and (index<length(fileHistory))
    then result:=fileHistory[index]
    else result:='';
  end;

end.
