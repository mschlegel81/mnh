UNIT mnh_imig_form;

{$mode objfpc}{$H+}

INTERFACE

USES
  sysutils,
  Classes, Forms, Controls, ExtCtrls,
  mypics,
  mnhFormHandler,
  mnh_out_adapters, mnh_imig;

TYPE

  TDisplayImageForm = class(TForm)
    displayImage: TImage;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
  private
    { private declarations }
    tryingToDisplayImage:boolean;
  public
    { public declarations }
    PROCEDURE displayCurrentImage;
  end;

VAR
  guiAdapters:P_adapters;

FUNCTION DisplayImageForm: TDisplayImageForm;
IMPLEMENTATION
VAR myDisplayImageForm:TDisplayImageForm=nil;
FUNCTION DisplayImageForm: TDisplayImageForm;
  begin
    if myDisplayImageForm=nil then begin
      writeln('Creating new instance of TDisplayImageForm');
      myDisplayImageForm:=TDisplayImageForm.create(nil);
      registerForm(myDisplayImageForm,false,true);
    end;
    result:=myDisplayImageForm;
  end;

{$R *.lfm}

{ TDisplayImageForm }

PROCEDURE TDisplayImageForm.FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
  begin
    if (key=9) and (ssCtrl in Shift) then formCycle(self,ssShift in Shift);
  end;

PROCEDURE TDisplayImageForm.FormCreate(Sender: TObject);
  begin
    tryingToDisplayImage:=false;
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
      if tryingToDisplayImage then exit;
      tryingToDisplayImage:=true;
      lock;
      if value=nil then begin
        unlock;
        tryingToDisplayImage:=false;
        guiAdapters^.raiseSystemError('There is no image loaded to display');
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
      tryingToDisplayImage:=false;
    end;
  end;

FINALIZATION
  if myDisplayImageForm<>nil then FreeAndNil(myDisplayImageForm);

end.

