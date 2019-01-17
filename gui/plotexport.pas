UNIT plotExport;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, ComCtrls,
  mnh_plotData;

TYPE
  TExportPlotForm = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
    ProgressBar1: TProgressBar;
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
    PROCEDURE cancelButtonClick(Sender: TObject);
    PROCEDURE okButtonClick(Sender: TObject);
    PROCEDURE OutputFileNameEditEditingDone(Sender: TObject);
    PROCEDURE QualityTrackbarChange(Sender: TObject);
    PROCEDURE widthEditChange(Sender: TObject);
  private
    exporting,exportHalted:boolean;
    psys:P_plotSystem;
    animationFrameIndex:longint;
    PROCEDURE enableOkButton;
    PROCEDURE enableAll;
  public
    FUNCTION showModalFor(CONST plotSystem:P_plotSystem; CONST selectedFrame:longint):integer;
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

PROCEDURE TExportPlotForm.cancelButtonClick(Sender: TObject);
  begin
    if exporting then exportHalted:=true;
  end;

PROCEDURE TExportPlotForm.okButtonClick(Sender: TObject);
  VAR renderHeight,renderWidth,frameIndex:longint;
  begin
    if exporting then exit;

    OKButton          .enabled:=false;
    OutputFileNameEdit.enabled:=false;
    widthEdit         .enabled:=false;
    heightEdit        .enabled:=false;
    CancelButton.ModalResult:=mrNone;
    CancelButton.Cancel     :=false;
    exporting:=true;
    exportHalted:=false;

    renderHeight:=strToIntDef(heightEdit.text,10000);
    renderWidth :=strToIntDef(widthEdit.text,10000);

    ProgressBar1.position:=0;
    if (rbExportAll.checked) and (psys^.animation.frameCount>0) then begin
      ProgressBar1.max:=psys^.animation.frameCount;
      for frameIndex:=0 to psys^.animation.frameCount-1 do if not(exportHalted) then begin
        ProgressBar1.position:=frameIndex;
        Application.ProcessMessages;
        psys^.animation.renderFrame(frameIndex,
                              animationFileName(
                                   frameIndex,
                                   psys^.animation.frameCount),
                              renderWidth,
                              renderHeight,
                              QualityTrackbar.position,true);
      end;
    end else begin
      ProgressBar1.max:=1;
      if (psys^.animation.frameCount>0)
      then psys^.animation.renderFrame(
             animationFrameIndex,
             OutputFileNameEdit.caption,
             renderWidth,
             renderHeight,
             QualityTrackbar.position,false)
      else psys^.currentPlot.renderToFile(
             OutputFileNameEdit.caption,
             renderWidth,
             renderHeight,
             QualityTrackbar.position);
      ProgressBar1.position:=1;
      Application.ProcessMessages;
    end;
    if exportHalted then ProgressBar1.position:=0 else ProgressBar1.position:=ProgressBar1.max;
    exporting:=false;
    enableAll;
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
                      validSize(heightEdit.text) and not(exporting);
  end;

PROCEDURE TExportPlotForm.enableAll;
  VAR isAnimation:boolean;
  begin
    OutputFileNameEdit.enabled:=true;
    widthEdit         .enabled:=true;
    heightEdit        .enabled:=true;
    CancelButton.ModalResult:=mrCancel;
    CancelButton.Cancel     :=true;
    OKButton.enabled        :=true;
    isAnimation:=psys^.animation.frameCount>0;
    rbExportCurrentOnly.enabled:=isAnimation;
    rbExportAll.enabled:=isAnimation;
    if not(isAnimation) then rbExportCurrentOnly.checked:=true;
    enableOkButton;
  end;

FUNCTION TExportPlotForm.showModalFor(CONST plotSystem: P_plotSystem; CONST selectedFrame: longint): integer;
  begin
    psys:=plotSystem;
    animationFrameIndex:=selectedFrame;
    exporting          :=false;
    exportHalted       :=false;
    enableAll;
    ProgressBar1.position:=0;
    result:=ShowModal;
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

