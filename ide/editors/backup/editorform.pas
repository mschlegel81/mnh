UNIT editorForm;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, SynEdit,ideLayoutUtil;

TYPE

  { TeditForm }

  TeditForm = class(T_mnhComponentForm)
    editor: TSynEdit;
  public
    FUNCTION getIdeComponentType:T_ideComponent; virtual;
  end;

VAR
  editForm: TeditForm;

IMPLEMENTATION

{$R *.lfm}

FUNCTION TeditForm.getIdeComponentType: T_ideComponent;
  begin
    result:=icEditor;
  end;

end.

