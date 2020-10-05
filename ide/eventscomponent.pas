UNIT eventsComponent;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, Menus, Grids,ideLayoutUtil,mnh_messages,out_adapters;

CONST TIME_COLUMN_INDEX=0;
      WARN_COLUMN_INDEX=1;
      TEXT_COLUMN_INDEX=2;
      RETAIN_MESSAGE_THRESHOLD:array[false..true] of double=(10/(24*60), //=10 minutes for notes
                                                              1/(24  )); //=1 hour for warnings
TYPE
  TeventsForm = class(T_mnhComponentForm)
    eventsGrid: TStringGrid;
    MainMenu1: TMainMenu;
    PopupMenu1: TPopupMenu;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    FUNCTION getIdeComponentType:T_ideComponent; override;
    PROCEDURE performSlowUpdate(CONST isEvaluationRunning:boolean); override;
    PROCEDURE performFastUpdate; override;
    PROCEDURE dockChanged; override;
    PROCEDURE eventsGridPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
  public

  end;

PROCEDURE setupEventsComponentOnIdeStartup;
PROCEDURE postIdeMessage(CONST messageText:string; CONST warn:boolean);
PROCEDURE ensureEventsForm;
IMPLEMENTATION
USES basicTypes,myGenerics,mySys,myStringUtil,mnh_settings;
TYPE
  P_ideMessage=^T_ideMessage;
  T_ideMessage=object
    protected
      time:double;
      isWarning:boolean;
      messageTxt:string;
    public
      CONSTRUCTOR create(CONST warn:boolean; CONST message:string);
      DESTRUCTOR destroy; virtual;
  end;

  P_eventsAdapter=^T_eventsAdapter;
  T_eventsAdapter=object
    private
      adapterCs:TRTLCriticalSection;
      changedSinceDraw:boolean;
      collectedEvents:array of P_ideMessage;
    public
      CONSTRUCTOR create;
      PROCEDURE append(CONST message:P_ideMessage);
      PROCEDURE drawEventsList;
      DESTRUCTOR destroy; virtual;
  end;

VAR eventsFormSingleton:TeventsForm=nil;
    eventsAdapterSingleton:P_eventsAdapter=nil;

CONSTRUCTOR T_ideMessage.create(CONST warn: boolean; CONST message: string);
  begin
    time:=now;
    isWarning:=warn;
    messageTxt:=message;
  end;

DESTRUCTOR T_ideMessage.destroy;
  begin
    messageTxt:='';
  end;

PROCEDURE ensureEventsForm;
  begin
    if eventsFormSingleton=nil then begin
      eventsFormSingleton:=TeventsForm.create(Application);
      dockNewForm(eventsFormSingleton);
    end;
    eventsFormSingleton.showComponent(false);
  end;

PROCEDURE memoryCleanerCallback;
  VAR dummy:double;
  begin
    postIdeMessage('Cleaning memory; '+mySys.getMemoryUsedAsString(dummy)+' used = '+intToStr(round(dummy*100))+'% of threshold value',dummy>1);
  end;

PROCEDURE setupEventsComponentOnIdeStartup;
  begin
    if eventsAdapterSingleton=nil then begin
      new(eventsAdapterSingleton,create);
      memoryCleaner.registerCleanupMethod(@memoryCleanerCallback);
    end;
  end;

PROCEDURE postIdeMessage(CONST messageText: string; CONST warn: boolean);
  VAR message:P_ideMessage;
  begin
    new(message,create(warn,messageText));
    if eventsAdapterSingleton=nil then setupEventsComponentOnIdeStartup;
    eventsAdapterSingleton^.append(message);
  end;

{$R *.lfm}

CONSTRUCTOR T_eventsAdapter.create;
  begin
    initCriticalSection(adapterCs);
    changedSinceDraw:=true;
    setLength(collectedEvents,0);
  end;

PROCEDURE T_eventsAdapter.append(CONST message:P_ideMessage);
  VAR i:longint;
  begin
    enterCriticalSection(adapterCs);
    i:=length(collectedEvents);
    setLength(collectedEvents,i+1);
    collectedEvents[i]:=message;
    changedSinceDraw:=true;
    leaveCriticalSection(adapterCs);
  end;

PROCEDURE T_eventsAdapter.drawEventsList;
  PROCEDURE cleanupEventsList;
    VAR i:longint;
        j:longint=0;
    begin
      for i:=0 to length(collectedEvents)-1 do begin
        if now<=collectedEvents[i]^.time+RETAIN_MESSAGE_THRESHOLD[collectedEvents[i]^.isWarning]
        then begin
          if j<>i then collectedEvents[j]:=collectedEvents[i];
          inc(j);
        end else begin
          dispose(collectedEvents[i],destroy);
          changedSinceDraw:=true;
        end;
      end;
      if j<length(collectedEvents) then setLength(collectedEvents,j);
    end;

  CONST WARN_OR_NOT:array[false..true] of string=('','WARN');
  VAR i,rowIdx:longint;
  begin
    enterCriticalSection(adapterCs);
    cleanupEventsList;
    if changedSinceDraw then begin
      eventsFormSingleton.eventsGrid.RowCount:=1+length(collectedEvents);
      rowIdx:=length(collectedEvents);
      for i:=0 to length(collectedEvents)-1 do with eventsFormSingleton.eventsGrid do begin
        Cells[TIME_COLUMN_INDEX,rowIdx]:=FormatDateTime('dd.mm.yyyy hh:nn:ss.zzz',collectedEvents[i]^.time);
        Cells[WARN_COLUMN_INDEX,rowIdx]:=WARN_OR_NOT[collectedEvents[i]^.isWarning];
        Cells[TEXT_COLUMN_INDEX,rowIdx]:=collectedEvents[i]^.messageTxt;
        rowIdx-=1;
      end;
      eventsFormSingleton.eventsGrid.AutoSizeColumns;
    end;
    changedSinceDraw:=false;
    leaveCriticalSection(adapterCs);
  end;

DESTRUCTOR T_eventsAdapter.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(collectedEvents)-1 do dispose(collectedEvents[i],destroy);
    setLength(collectedEvents,0);
    doneCriticalSection(adapterCs);
  end;

FUNCTION TeventsForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icIdeEvents;
  end;

PROCEDURE TeventsForm.FormCreate(Sender: TObject);
  begin
    caption:=getCaption;
    initDockMenuItems(MainMenu1,nil);
    initDockMenuItems(PopupMenu1,PopupMenu1.items);
    registerFontControl(eventsGrid,ctTable);
  end;

PROCEDURE TeventsForm.FormDestroy(Sender: TObject);
  begin
    eventsFormSingleton:=nil;
  end;

PROCEDURE TeventsForm.performSlowUpdate(CONST isEvaluationRunning: boolean);
  begin
  end;

PROCEDURE TeventsForm.performFastUpdate;
  begin
    eventsAdapterSingleton^.drawEventsList;
  end;

PROCEDURE TeventsForm.dockChanged;
  begin
  end;

PROCEDURE TeventsForm.eventsGridPrepareCanvas(Sender: TObject; aCol,aRow: integer; aState: TGridDrawState);
begin
  if (eventsGrid.colCount > 1) and (aCol>0) then
   begin
     if eventsGrid.Cells[WARN_COLUMN_INDEX, aRow] <>''
     then eventsGrid.Canvas.Font.color :=  clRed
     else eventsGrid.Canvas.Font.color :=  clBlack;
   end;
end;

FINALIZATION
  if eventsAdapterSingleton<>nil then dispose(eventsAdapterSingleton,destroy);

end.

