UNIT mnh_imig_form;
{$mode objfpc}{$H+}
INTERFACE
USES
  sysutils,
  Classes, Forms, Controls, ExtCtrls,
  mnh_constants,basicTypes,
  mnh_litVar,contexts,
  funcs,
  mypics,
  mnhFormHandler,
  recyclers,
  mnh_out_adapters, mnh_imig;

TYPE
  TDisplayImageForm = class(TForm)
    displayImage: TImage;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormKeyUp(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
  private
    tryingToDisplayImage:boolean;
  public
    PROCEDURE displayCurrentImage;
  end;

VAR
  imigSystem:T_imageSystem;

FUNCTION DisplayImageForm: TDisplayImageForm;
IMPLEMENTATION
VAR myDisplayImageForm:TDisplayImageForm=nil;
FUNCTION DisplayImageForm: TDisplayImageForm;
  begin
    if myDisplayImageForm=nil then begin
      {$ifdef debugMode} writeln(stdErr,'        DEBUG: Creating new instance of TDisplayImageForm'); {$endif}
      myDisplayImageForm:=TDisplayImageForm.create(nil);
      registerForm(myDisplayImageForm,ft_imageForm);
    end;
    result:=myDisplayImageForm;
  end;

{$R *.lfm}

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
    if anyFormShowing(ft_main)
    then ShowInTaskBar:=stDefault
    else ShowInTaskBar:=stAlways;
  end;

PROCEDURE TDisplayImageForm.displayCurrentImage;
  VAR resizedPic:T_rawImage;
  begin
    with imigSystem do begin
      if tryingToDisplayImage or (currentImage=nil) then exit;
      tryingToDisplayImage:=true;
      if not(showing) then Show;
      if (currentImage^.dimensions.width<displayImage.width) and (currentImage^.dimensions.height<displayImage.height)
      then currentImage^.copyToImage(displayImage)
      else begin
        resizedPic.create(currentImage^);
        resizedPic.resize(displayImage.width,displayImage.height,res_fit);
        resizedPic.copyToImage(displayImage);
        resizedPic.destroy;
      end;
      tryingToDisplayImage:=false;
    end;
  end;

{$i func_defines.inc}
FUNCTION getScreenSize_imp intFuncSignature;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then result:=newListLiteral(2)^.appendInt(screen.width)^.appendInt(screen.height);
  end;

PROCEDURE renderImage;
  begin
    DisplayImageForm.displayCurrentImage;
  end;

INITIALIZATION
  imigSystem.create(@renderImage);
  registerRule(IMIG_NAMESPACE,'getScreenSize',@getScreenSize_imp,ak_nullary,'Returns the current screen size');

FINALIZATION
  if myDisplayImageForm<>nil then FreeAndNil(myDisplayImageForm);
  imigSystem.destroy;

end.
