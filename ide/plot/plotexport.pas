UNIT plotExport;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, ComCtrls,
  mnh_plotData;

TYPE
  TExportPlotForm = class(TForm)
    GroupBox3: TGroupBox;
    OKButton: TButton;
    CancelButton: TButton;
    ProgressBar1: TProgressBar;
    rbExportToMnh: TRadioButton;
    rbExportToPng: TRadioButton;
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
    PROCEDURE rbExportToPngChange(Sender: TObject);
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
USES ideLayoutUtil,
     myGenerics,
     fileWrappers,
     editScripts,
     editorMeta;
VAR myExportPlotForm: TExportPlotForm=nil;

FUNCTION exportPlotForm: TExportPlotForm;
  begin
    if myExportPlotForm=nil then myExportPlotForm:=TExportPlotForm.create(Application);
    result:=myExportPlotForm;
  end;

{$R *.lfm}

PROCEDURE TExportPlotForm.OutputFileNameEditEditingDone(Sender: TObject);
  begin
    if rbExportToPng.checked
    then OutputFileNameEdit.text:=ChangeFileExt(OutputFileNameEdit.text,'.png')
    else OutputFileNameEdit.text:=ChangeFileExt(OutputFileNameEdit.text,'.mnh');
    enableOkButton;
  end;

PROCEDURE TExportPlotForm.cancelButtonClick(Sender: TObject);
  begin
    if exporting then exportHalted:=true;
  end;

PROCEDURE TExportPlotForm.okButtonClick(Sender: TObject);
  PROCEDURE renderToPng;
    VAR renderHeight,renderWidth,frameIndex:longint;
    begin
      renderHeight:=strToIntDef(heightEdit.text,10000);
      renderWidth :=strToIntDef(widthEdit.text,10000);
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
    end;

  PROCEDURE createScript;
    VAR scriptLines:T_arrayOfString;
        task:P_editScriptTask;
    begin
      if (rbExportAll.checked) and (psys^.animation.frameCount>0) then begin
        scriptLines:=psys^.getPlotStatement(-1,@exportHalted,Application,ProgressBar1);
      end else begin
        scriptLines:=psys^.getPlotStatement(animationFrameIndex,@exportHalted,Application,ProgressBar1);
      end;
      if exportHalted then ProgressBar1.position:=0 else begin
        ProgressBar1.position:=ProgressBar1.max;
        if mainForm=nil
        then writeFileLines(OutputFileNameEdit.caption,scriptLines,LineEnding,false)
        else begin
          new(task,createForNewEditor(scriptLines));
          mainForm.onEditFinished(task);
          Application.ProcessMessages;
        end;
      end;
    end;

  begin
    if exporting then exit;

    OKButton           .enabled:=false;
    OutputFileNameEdit .enabled:=false;
    widthEdit          .enabled:=false;
    heightEdit         .enabled:=false;
    rbExportCurrentOnly.enabled:=false;
    rbExportAll        .enabled:=false;
    CancelButton.ModalResult:=mrNone;
    CancelButton.Cancel     :=false;
    exporting:=true;
    exportHalted:=false;
    if rbExportToPng.checked
    then renderToPng
    else createScript;
    Application.ProcessMessages;
    exporting:=false;
    enableAll;
  end;

PROCEDURE TExportPlotForm.QualityTrackbarChange(Sender: TObject);
  begin
    enableOkButton;
  end;

PROCEDURE TExportPlotForm.rbExportToPngChange(Sender: TObject);
  begin
    GroupBox1.visible:=rbExportToPng.checked;
    Panel1.visible:=rbExportToPng.checked or (mainForm=nil);
    if rbExportToPng.checked
    then OutputFileNameEdit.text:=ChangeFileExt(OutputFileNameEdit.text,'.png')
    else OutputFileNameEdit.text:=ChangeFileExt(OutputFileNameEdit.text,'.mnh');
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
      if rbExportToMnh.checked then exit(true);
      i:=strToIntDef(s,10000);
      if i<=0 then exit(false);
      if i>9999 then exit(false);
      if i>4999 then exit((QualityTrackbar.position=0) or
                          (QualityTrackbar.position=2));
      result:=true;
    end;

  begin
    OKButton.enabled:=((OutputFileNameEdit.text<>'') and
                      DirectoryExists(ExtractFileDir(expandFileName(OutputFileNameEdit.text)))
                      or not(rbExportToPng.checked or (mainForm=nil))) and
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

end.

