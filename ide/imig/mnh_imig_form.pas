UNIT mnh_imig_form;
{$mode objfpc}{$H+}
INTERFACE
USES
  sysutils,
  Forms, Controls, ExtCtrls, Menus,
  mypics,
  mnh_imig, ideLayoutUtil,mnh_messages, Classes;

TYPE
  TDisplayImageForm = class;
  P_guiImageSystem = ^T_guiImageSystem;

  T_guiImageSystem = object(T_imageSystem)
    private
      myImageForm:TDisplayImageForm;
      headless:boolean;
      closedByUser:boolean;
      cap:string;
      PROCEDURE ensureForm;
    protected
      PROCEDURE processMessage(CONST message:P_storedMessage); virtual;
    public
      CONSTRUCTOR create(CONST plotFormCaption:string='');
      DESTRUCTOR destroy; virtual;
      PROCEDURE displayImage;
      PROCEDURE formDestroyed;
      PROCEDURE render(VAR target:TImage; CONST enlargeSmall,shrinkBig:boolean);
  end;

  { TDisplayImageForm }

  TDisplayImageForm = class(T_mnhComponentForm)
    displayImage: TImage;
    MainMenu1: TMainMenu;
    miShrinkLarge: TMenuItem;
    miEnlargeSmall: TMenuItem;
    miDisplay: TMenuItem;
    PopupMenu1: TPopupMenu;
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE miShrinkLargeClick(Sender: TObject);
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
  recyclers,
  pixMaps,out_adapters;

{$R *.lfm}
PROCEDURE T_guiImageSystem.ensureForm;
  begin
    if myImageForm=nil then begin
      myImageForm:=TDisplayImageForm.create(Application);
      if cap=''
      then myImageForm.caption:=myImageForm.getCaption
      else myImageForm.caption:=cap;
      myImageForm.relatedAdapters:=@self;
      dockNewForm(myImageForm);
    end;
  end;

PROCEDURE T_guiImageSystem.processMessage(CONST message: P_storedMessage);
  VAR width :longint=-1;
      height:longint=-1;
  begin
    case message^.messageType of
      mt_image_queryClosedByUser: begin
        if myImageForm<>nil then begin
          width :=myImageForm.ClientWidth;
          height:=myImageForm.ClientHeight;
        end;
        P_queryImigClosedMessage(message)^.setResponse(closedByUser,width,height);
      end;
      mt_startOfEvaluation: begin
        closedByUser:=false;
      end
      else inherited processMessage(message);
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
    if headless then exit;
    ensureForm;
    myImageForm.displayCurrentImage;
  end;

PROCEDURE T_guiImageSystem.formDestroyed;
  begin
    myImageForm:=nil;
    closedByUser:=true;
  end;

PROCEDURE T_guiImageSystem.render(VAR target: TImage; CONST enlargeSmall,shrinkBig:boolean);
  VAR resizedPic:T_rawImage;
  begin
    enterCriticalSection(adapterCs);
    try
      if (currentImage<>nil) then begin
        if (currentImage^.dimensions.width<=target.width) and (currentImage^.dimensions.height<=target.height) and not(enlargeSmall)
        or (currentImage^.dimensions.width>=target.width) and (currentImage^.dimensions.height>=target.height) and not(shrinkBig)
        then currentImage^.copyToImage(target)
        else begin
          resizedPic.create(currentImage^);
          resizedPic.resize(imageDimensions(target.width,target.height),res_fit);
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
    initDockMenuItems(PopupMenu1,nil);
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

PROCEDURE TDisplayImageForm.miShrinkLargeClick(Sender: TObject);
begin
  displayCurrentImage;
end;

PROCEDURE TDisplayImageForm.performSlowUpdate(CONST isEvaluationRunning:boolean);
  begin
  end;

PROCEDURE TDisplayImageForm.performFastUpdate;
  begin
  end;

PROCEDURE TDisplayImageForm.dockChanged;
  begin
    if myComponentParent=cpNone
    then moveAllItems(PopupMenu1.items,MainMenu1.items)
    else moveAllItems(MainMenu1.items,PopupMenu1.items);
    resize;
  end;

PROCEDURE TDisplayImageForm.displayCurrentImage;
  begin
    if relatedAdapters<>nil then relatedAdapters^.render(displayImage,miEnlargeSmall.checked,miShrinkLarge.checked);
  end;

{$i func_defines.inc}
FUNCTION getScreenSize_imp intFuncSignature;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then result:=recycler^.listLiteralOf(
      recycler^.newIntLiteral(screen.width),
      recycler^.newIntLiteral(screen.height));
  end;

FUNCTION imigImageSize_imp intFuncSignature;
  VAR closedRequest:P_queryImigClosedMessage;
      plotWidth,
      plotHeight: longint;
  begin if (params=nil) or (params^.size=0) then begin
    if (gui_started=NO) then context^.messages^.logGuiNeeded;
    if not(se_readGuiState in context^.sideEffectWhitelist) then exit(newBoolLiteral(false));
    new(closedRequest,createRetrieveRequest);
    context^.messages^.postCustomMessage(closedRequest);
    closedRequest^.getResponseWaiting(context^.messages,plotWidth,plotHeight);
    result:=recycler^.newListLiteral()^.appendInt(recycler,plotWidth)^.appendInt(recycler,plotHeight);
    disposeMessage(closedRequest);
  end else result:=nil; end;

FUNCTION imigClosedByUser_impl intFuncSignature;
  VAR closedRequest:P_queryImigClosedMessage;
      dummyWidth,
      dummyHeight: longint;
  begin if (params=nil) or (params^.size=0) then begin
    if (gui_started=NO) then context^.messages^.logGuiNeeded;
    if not(se_readGuiState in context^.sideEffectWhitelist) then exit(newBoolLiteral(false));
    new(closedRequest,createRetrieveRequest);
    context^.messages^.postCustomMessage(closedRequest);
    result:=newBoolLiteral(closedRequest^.getResponseWaiting(context^.messages,dummyWidth,dummyHeight));
    disposeMessage(closedRequest);
  end else result:=nil; end;

INITIALIZATION
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'getScreenSize',@getScreenSize_imp,ak_nullary,'Returns the current screen size');
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'imageDisplaySize',@imigImageSize_imp,ak_nullary,'Returns the current image display size',[se_readGuiState]);
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'imageDisplayClosedByUser',@imigClosedByUser_impl,ak_nullary,'Returns true if the image display was closed by the user',[se_readGuiState]);

end.
