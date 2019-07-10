UNIT mnh_imig_form;
{$mode objfpc}{$H+}
INTERFACE
USES
  sysutils,
  Forms, Controls, ExtCtrls, Menus,
  mnh_imig,   ideLayoutUtil;

TYPE
  TDisplayImageForm = class;
  P_guiImageSystem = ^T_guiImageSystem;
  T_guiImageSystem = object(T_imageSystem)
    private
      myImageForm:TDisplayImageForm;
      cap:string;
      PROCEDURE ensureForm;
    public
      CONSTRUCTOR create(CONST plotFormCaption:string='MNH image');
      DESTRUCTOR destroy; virtual;
      PROCEDURE displayImage;
      PROCEDURE formDestroyed;
      PROCEDURE render(VAR target:TImage);
  end;

  TDisplayImageForm = class(T_mnhComponentForm)
    displayImage: TImage;
    MainMenu1: TMainMenu;
    PopupMenu1: TPopupMenu;
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate(CONST isEvaluationRunning:boolean); override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE dockChanged; override;
  private
    relatedAdapters:P_guiImageSystem;
  public
    PROCEDURE displayCurrentImage;
  end;

IMPLEMENTATION
USES   mnh_constants,basicTypes,
  litVar,contexts,
  funcs,
  mypics,
  recyclers;

{$R *.lfm}
PROCEDURE T_guiImageSystem.ensureForm;
  begin
    if myImageForm=nil then begin
      myImageForm:=TDisplayImageForm.create(Application);
      myImageForm.caption:=cap;
      myImageForm.relatedAdapters:=@self;
      dockNewForm(myImageForm);
    end;
  end;

CONSTRUCTOR T_guiImageSystem.create(CONST plotFormCaption: string);
  begin
    inherited create(@displayImage);
    cap:=plotFormCaption;
    myImageForm:=nil;
  end;

DESTRUCTOR T_guiImageSystem.destroy;
  begin
    if myImageForm<>nil then begin
      myImageForm.relatedAdapters:=nil;
      FreeAndNil(myImageForm);
    end;
    inherited destroy;
  end;

PROCEDURE T_guiImageSystem.displayImage;
  begin
    ensureForm;
    myImageForm.displayCurrentImage;
  end;

PROCEDURE T_guiImageSystem.formDestroyed;
  begin
    myImageForm:=nil;
  end;

PROCEDURE T_guiImageSystem.render(VAR target: TImage);
  VAR resizedPic:T_rawImage;
  begin
    enterCriticalSection(adapterCs);
    try
      if (currentImage<>nil) then begin
        if (currentImage^.dimensions.width<target.width) and (currentImage^.dimensions.height<target.height)
        then currentImage^.copyToImage(target)
        else begin
          resizedPic.create(currentImage^);
          resizedPic.resize(target.width,target.height,res_fit);
          resizedPic.copyToImage(target);
          resizedPic.destroy;
        end;
      end;
    finally
      leaveCriticalSection(adapterCs);
    end;
  end;

PROCEDURE TDisplayImageForm.FormCreate(Sender: TObject);
  begin
    relatedAdapters:=nil;
    initDockMenuItems(MainMenu1,nil);
    initDockMenuItems(PopupMenu1,PopupMenu1.items);
  end;

PROCEDURE TDisplayImageForm.FormDestroy(Sender: TObject);
  begin
    if relatedAdapters<>nil then relatedAdapters^.formDestroyed;
    relatedAdapters:=nil;
  end;

PROCEDURE TDisplayImageForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    CloseAction:=caFree;
  end;

PROCEDURE TDisplayImageForm.FormResize(Sender: TObject);
  begin
    displayImage.Align:=alClient;
    displayCurrentImage;
  end;

FUNCTION TDisplayImageForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icImage;
  end;

PROCEDURE TDisplayImageForm.performSlowUpdate(CONST isEvaluationRunning:boolean);
  begin
  end;

PROCEDURE TDisplayImageForm.performFastUpdate;
  begin
  end;

PROCEDURE TDisplayImageForm.dockChanged;
  begin
  end;

PROCEDURE TDisplayImageForm.displayCurrentImage;
  begin
    if relatedAdapters<>nil then relatedAdapters^.render(displayImage);
  end;

{$i func_defines.inc}
FUNCTION getScreenSize_imp intFuncSignature;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then result:=newListLiteral(2)^.appendInt(screen.width)^.appendInt(screen.height);
  end;

INITIALIZATION
  registerRule(IMIG_NAMESPACE,'getScreenSize',@getScreenSize_imp,ak_nullary,'Returns the current screen size');

end.
