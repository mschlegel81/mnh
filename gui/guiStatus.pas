UNIT guiStatus;
INTERFACE
USES Classes;
TYPE T_guiStatus=record
       active       :boolean;
       debugMode    :boolean;
       running      :boolean;
       debugHalted  :boolean;
       scriptEditing:boolean;
     end;
     //T_statusChangeEvent=PROCEDURE(CONST oldStatus,newStatus:T_guiStatus) of object;

FUNCTION getStatus:T_guiStatus;
PROCEDURE registerOnGuiStatusChange(CONST e:TNotifyEvent);
//PROCEDURE registerOnGuiStatusChange(CONST e:T_statusChangeEvent);
PROCEDURE status_activate;
IMPLEMENTATION
VAR onChange1:array of TNotifyEvent;
    //onChange2:array of T_statusChangeEvent;
    status:T_guiStatus=(active:false; debugMode:false; running:false; debugHalted:false; scriptEditing:false);

FUNCTION getStatus: T_guiStatus; begin result:=status; end;

PROCEDURE registerOnGuiStatusChange(CONST e:TNotifyEvent);
  begin
    setLength(onChange1,length(onChange1)+1);
    onChange1[length(onChange1)-1]:=e;
  end;

//PROCEDURE registerOnGuiStatusChange(CONST e:T_statusChangeEvent);
//  begin
//    setLength(onChange2,length(onChange2)+1);
//    onChange2[length(onChange2)-1]:=e;
//  end;

PROCEDURE statusChanged;
  VAR e:TNotifyEvent;
  begin
    for e in onChange1 do e(nil);
  end;

PROCEDURE status_activate;
  begin
    if status.active then exit;
    status.active:=true;
    statusChanged;
  end;

end.
