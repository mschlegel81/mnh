UNIT mnh_imig_form;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, mnh_out_adapters, mypics;

TYPE

  { TDisplayImageForm }

  TDisplayImageForm = class(TForm)
    displayImage: TImage;
    PROCEDURE FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    PROCEDURE displayCurrentImage;
  end;

VAR
  guiAdapters:P_adapters;
  formCycleCallback: PROCEDURE(CONST ownId:longint; CONST next:boolean) = nil;
FUNCTION DisplayImageForm: TDisplayImageForm;
IMPLEMENTATION
VAR myDisplayImageForm:TDisplayImageForm=nil;
FUNCTION DisplayImageForm: TDisplayImageForm;
  begin
    if myDisplayImageForm=nil then myDisplayImageForm:=TDisplayImageForm.create(nil);
    result:=myDisplayImageForm;
  end;

{$R *.lfm}

{ TDisplayImageForm }

PROCEDURE TDisplayImageForm.FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=9) and (ssCtrl in Shift) and (formCycleCallback<>nil) then formCycleCallback(3,ssShift in Shift);
  end;

PROCEDURE TDisplayImageForm.FormResize(Sender: TObject);
  begin
    displayImage.Align:=alClient;
    displayCurrentImage;
  end;

PROCEDURE TDisplayImageForm.FormShow(Sender: TObject);
  begin
    position:=poDefault;
  end;

PROCEDURE TDisplayImageForm.displayCurrentImage;
  VAR resizedPic:T_rawImage;
  begin
    with guiAdapters^.picture do begin
      lock;
      if value=nil then begin
        unlock;
        exit;
      end;
      if not(showing) then Show;
      if (value^.width<displayImage.width) and (value^.height<displayImage.height)
      then value^.copyToImage(displayImage)
      else begin
        resizedPic.create(value^);
        resizedPic.resize(displayImage.width,displayImage.height,res_fit);
        resizedPic.copyToImage(displayImage);
        resizedPic.destroy;
      end;
      unlock;
    end;
  end;

FINALIZATION
  if myDisplayImageForm<>nil then FreeAndNil(myDisplayImageForm);

end.

