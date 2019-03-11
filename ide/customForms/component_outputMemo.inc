TYPE
  P_outputMemoMeta=^T_outputMemoMeta;
  T_outputMemoMeta=object(T_changeListeningMeta)
    synMeta:T_basicEditorMeta;
    config_hlLang:P_expressionLiteral;
    state_hlLang :string;
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST consideredKeys:T_definingMapKeys=[dmk_type,dmk_caption,dmk_highlight]);
    PROCEDURE update; virtual;
    FUNCTION evaluate(CONST location:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):boolean; virtual;
    FUNCTION getControl:TControl; virtual;
    FUNCTION preferClientAlignment:boolean; virtual;
    FUNCTION getName:string; virtual;
    DESTRUCTOR destroy; virtual;
  end;

CONSTRUCTOR T_outputMemoMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral;
                                    CONST location: T_tokenLocation; VAR context: T_context;
                                    CONST consideredKeys:T_definingMapKeys=[dmk_type,dmk_caption,dmk_highlight]);
  VAR langLit:P_literal;
  begin
    inherited create(def,location,context,consideredKeys);
    synMeta.createWithParent(parent^.winControl,nil);
    synMeta.editor.Align:=alTop;
    synMeta.editor.readonly:=true;
    synMeta.activate;
    state_hlLang:='';
    config_hlLang:=nil;

    langLit:=mapGet(def,key[dmk_highlight]);
    if langLit<>nil then begin
      case langLit^.literalType of
        lt_string: begin
          state_hlLang:=P_stringLiteral(langLit)^.value;
          synMeta.setLanguage(state_hlLang,LANG_TXT);
        end;
        lt_expression: config_hlLang:=P_expressionLiteral(langLit^.rereferenced);
        else context.raiseError('highlighter is: '+langLit^.typeString+'; must be string or expression',location);
      end;
      disposeLiteral(langLit);
    end else synMeta.language:=LANG_TXT;
    parent^.add(@self);
  end;

PROCEDURE T_outputMemoMeta.update;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit;
    synMeta.editor.text:=state.caption;
    synMeta.editor.enabled:=state.enabled;
    synMeta.setLanguage(state_hlLang,synMeta.language);
    synMeta.activate;
    leaveCriticalSection(elementCs);
  end;

FUNCTION T_outputMemoMeta.getControl: TControl;
  begin
    result:=synMeta.editor;
  end;

FUNCTION T_outputMemoMeta.preferClientAlignment: boolean;
  begin
    result:=true;
  end;

FUNCTION T_outputMemoMeta.getName:string;
  begin
    result:='T_outputMemoMeta';
  end;

FUNCTION T_outputMemoMeta.evaluate(CONST location: T_tokenLocation; VAR context: T_context; VAR recycler:T_recycler): boolean;
  VAR tmp:P_literal;
      oldLang:string;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit(false);
    result:=inherited evaluate(location,context,recycler);
    if config_hlLang<>nil then begin
      tmp:=config_hlLang^.evaluateToLiteral(location,@context,@recycler,nil,nil).literal;
      if tmp<>nil then begin
        if tmp^.literalType=lt_string then begin
          oldLang:=state_hlLang;
          state_hlLang:=P_stringLiteral(tmp)^.value;
          result:=result or (oldLang<>state_hlLang);
        end else context.raiseError( 'Cannot evaluate expression to String '+config_hlLang^.toString(50)+'; returned type is '+tmp^.typeString,config_hlLang^.getLocation);
        disposeLiteral(tmp);
      end;
    end;
    leaveCriticalSection(elementCs);
  end;

DESTRUCTOR T_outputMemoMeta.destroy;
  begin
    inherited destroy;
    synMeta.destroy;
    if config_hlLang<>nil then disposeLiteral(config_hlLang);
  end;

