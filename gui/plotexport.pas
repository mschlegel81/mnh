UNIT plotExport;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, ComCtrls;

TYPE
  TExportPlotForm = class(TForm)
    OKButton: TButton;
    Button2: TButton;
    widthEdit: TEdit;
    heightEdit: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    OutputFileNameEdit: TFileNameEdit;
    Label1: TLabel;
    Panel1: TPanel;
    rbExportAll: TRadioButton;
    rbExportCurrentOnly: TRadioButton;
    QualityTrackbar: TTrackBar;
    PROCEDURE OutputFileNameEditEditingDone(Sender: TObject);
    PROCEDURE QualityTrackbarChange(Sender: TObject);
    PROCEDURE widthEditChange(Sender: TObject);
  private
    PROCEDURE enableOkButton;
  public
    renderWidth:longint;
    renderHeight:longint;
    FUNCTION showModalFor(CONST isAnimation:boolean):integer;
    FUNCTION animationFileName(CONST frameIndex,framesTotal:longint):string;
  end;

FUNCTION exportPlotForm:TExportPlotForm;
IMPLEMENTATION
VAR myExportPlotForm: TExportPlotForm=nil;

FUNCTION exportPlotForm: TExportPlotForm;
  begin
    if myExportPlotForm=nil then myExportPlotForm:=TExportPlotForm.create(nil);
    result:=myExportPlotForm;
  end;

{$R *.lfm}

PROCEDURE TExportPlotForm.OutputFileNameEditEditingDone(Sender: TObject);
  begin
    OutputFileNameEdit.text:=ChangeFileExt(OutputFileNameEdit.text,'.png');
    enableOkButton;
  end;

PROCEDURE TExportPlotForm.QualityTrackbarChange(Sender: TObject);
  begin
    enableOkButton;
  end;

PROCEDURE TExportPlotForm.widthEditChange(Sender: TObject);
  begin
    enableOkButton;
  end;

PROCEDURE TExportPlotForm.enableOkButton;
  FUNCTION validSize(CONST s:string):boolean;
    VAR i:longint;
    begin
      i:=strToIntDef(s,10000);
      if i<=0 then exit(false);
      if i>9999 then exit(false);
      if i>4999 then exit((QualityTrackbar.position=0) or
                          (QualityTrackbar.position=2));
      result:=true;
    end;

  begin
    OKButton.enabled:=(OutputFileNameEdit.text<>'') and
                      (OutputFileNameEdit.text<>'.png') and
                      DirectoryExists(ExtractFileDir(expandFileName(OutputFileNameEdit.text))) and
                      validSize(widthEdit.text) and
                      validSize(heightEdit.text);
  end;

FUNCTION TExportPlotForm.showModalFor(CONST isAnimation: boolean): integer;
  begin
    rbExportCurrentOnly.enabled:=isAnimation;
    rbExportAll.enabled:=isAnimation;
    if not(isAnimation) then rbExportCurrentOnly.checked:=true;
    result:=ShowModal;
    renderHeight:=strToIntDef(heightEdit.text,10000);
    renderWidth:=strToIntDef(widthEdit.text,10000);
  end;

FUNCTION TExportPlotForm.animationFileName(CONST frameIndex, framesTotal: longint): string;
  VAR digits:longint;
      idx:string;
  begin
    digits:=length(intToStr(framesTotal-1));
    idx:=intToStr(frameIndex);
    while length(idx)<digits do idx:='0'+idx;
    result:=ChangeFileExt(OutputFileNameEdit.caption,'_'+idx+'.png');
  end;

FINALIZATION
  if myExportPlotForm<>nil then FreeAndNil(myExportPlotForm);

end.

