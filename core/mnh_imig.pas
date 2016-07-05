UNIT mnh_imig;
INTERFACE
USES workflows,mnh_funcs,mnh_litVar,mnh_contexts,mnh_constants,mnh_funcs_list,mnh_tokLoc,sysutils,
     ig_gradient,
     ig_perlin,
     ig_simples,
     ig_fractals,
     ig_epicycles,
     ig_ifs,
     ig_bifurcation,
     ig_funcTrees,
     ig_expoClouds;

IMPLEMENTATION
VAR imigCS:TRTLCriticalSection;
FUNCTION createWorkflow(CONST steps:P_listLiteral; CONST validating:boolean; OUT isValid:boolean; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):T_imageManipulationWorkflow;
  PROCEDURE warn(CONST message:string);
    begin
      isValid:=false;
      if validating then context.adapters^.raiseWarning(message,tokenLocation)
                    else context.adapters^.raiseError  (message,tokenLocation);
    end;

  VAR i:longint;
      cmd:string;
      tmpSteps:P_listLiteral;
  begin
    result.create;
    if steps^.literalType=lt_stringList
    then tmpSteps:=steps
    else tmpSteps:=P_listLiteral(flatten_imp(steps,tokenLocation,context));
    isValid:=(tmpSteps^.literalType=lt_stringList) and (tmpSteps^.size>=1);
    if isValid then begin
      for i:=0 to steps^.size-1 do begin
        cmd:=P_stringLiteral(steps^.value(i))^.value;
        if not(result.addStep(cmd))
        then warn('Invalid workflow step: '+cmd);
      end;
    end;
    if steps<>tmpSteps then disposeLiteral(tmpSteps);
  end;

FUNCTION validateWorkflow_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR wf:T_imageManipulationWorkflow;
      isValid:boolean;
  begin
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType in [lt_stringList,lt_list,lt_keyValueList]) then begin
      wf:=createWorkflow(P_listLiteral(params^.value(0)),true,isValid,tokenLocation,context);
      wf.destroy;
      result:=newBoolLiteral(isValid);
    end else result:=nil;
  end;

FUNCTION executeWorkflow_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR isValid:boolean=true;
      Source:string='';
      dest:string='';
      xRes:longint=0;
      yRes:longint=0;
      sizeLimit:longint=-1;
      i:longint;
      currentProgress:string;
      lastProgress:string='';
  begin
    if (params<>nil) and (params^.size>=2) and (params^.value(0)^.literalType in [lt_stringList,lt_list,lt_keyValueList]) then begin
      for i:=1 to params^.size-1 do begin
        case params^.value(i)^.literalType of
          lt_int: begin
            if P_intLiteral(params^.value(i))^.value<=0 then exit(nil);
            if      xRes=0 then xRes:=P_intLiteral(params^.value(i))^.value
            else if yRes=0 then yRes:=P_intLiteral(params^.value(i))^.value
            else if sizeLimit<=-1 then sizeLimit:=P_intLiteral(params^.value(i))^.value
            else exit(nil);
          end;
          lt_string: begin
            if Source='' then Source:=P_stringLiteral(params^.value(i))^.value
            else if dest='' then dest:=P_stringLiteral(params^.value(i))^.value
            else exit(nil);
          end;
          else exit(nil);
        end;
      end;
      if (xRes>0) and (yRes=0) then begin sizeLimit:=xRes; xRes:=0; end;
      if (dest='') and (Source<>'') then begin dest:=Source; Source:=''; end;

      if (Source='') and ((xRes=0) or (yRes=0)) then begin
        context.adapters^.raiseError('Either target resolution or input image must be provided',tokenLocation);
        isValid:=false;
      end;
      if (dest='') then begin
        context.adapters^.raiseError('No output file given',tokenLocation);
        isValid:=false;
      end;
      EnterCriticalsection(imigCS);
      workflow:=createWorkflow(P_listLiteral(params^.value(0)),false,isValid,tokenLocation,context);
      if isValid then begin
        if Source<>'' then workflow.executeForTarget(Source,sizeLimit,dest)
                      else workflow.executeForTarget(xRes,yRes,sizeLimit,dest);
        while progressQueue.calculating and (context.adapters^.noErrors) do begin
          currentProgress:=progressQueue.getProgressString;
          if currentProgress<>lastProgress then context.adapters^.raiseNote(currentProgress,tokenLocation);
          lastProgress:=currentProgress;
          ThreadSwitch;
          sleep(100);
        end;
        progressQueue.cancelCalculation(true);
      end;
      workflow.destroy;
      LeaveCriticalsection(imigCS);
      if isValid then exit(newVoidLiteral) else exit(nil);
    end else result:=nil;
  end;

INITIALIZATION
  InitCriticalSection(imigCS);
  registerRule(IMIG_NAMESPACE,'validateWorkflow',@validateWorkflow_imp,'validateWorkflow(wf:list);Validates the workflow returning a boolean flag indicating validity');
  registerRule(IMIG_NAMESPACE,'executeWorkflow',@executeWorkflow_imp,'executeWorkflow(wf:list,xRes>0,yRes>0,target:string);#'+
                                                                     'executeWorkflow(wf:list,source:string,target:string);#'+
                                                                     'executeWorkflow(wf:list,xRes>0,yRes>0,sizeLimitInBytes>0,target:string);#'+
                                                                     'executeWorkflow(wf:list,source:string,sizeLimitInBytes>0,target:string);#Executes the workflow with the given options.');
finalization
  DoneCriticalsection(imigCS);
end.
