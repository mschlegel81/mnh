UNIT mnh_imig;
INTERFACE
USES workflows,mnh_funcs,mnh_litVar,mnh_contexts,mnh_constants,mnh_funcs_list,mnh_tokLoc,sysutils,
     mypics,
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
      for i:=0 to tmpSteps^.size-1 do begin
        cmd:=P_stringLiteral(tmpSteps^.value(i))^.value;
        if not(result.addStep(cmd))
        then warn('Invalid workflow step: '+tmpSteps^.value(i)^.toString);
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

  FUNCTION newFromWorkflowImage:P_rawImage;
    begin
      new(result,create(workflowImage));
    end;

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
      enterCriticalSection(imigCS);
      workflow:=createWorkflow(P_listLiteral(params^.value(0)),false,isValid,tokenLocation,context);
      if isValid and (Source=C_nullSourceOrTargetFileName) then with context.adapters^.picture do begin
        lock;
        if value=nil then begin
          context.adapters^.raiseError('Current image ("-") given as input image but no current image loaded.',tokenLocation);
          isValid:=false;
        end else begin
          workflowImage.copyFromImage(value^);
          context.adapters^.raiseNote('Input for workflow copied from current image',tokenLocation);
        end;
        unlock;
      end;
      if isValid then begin
        if Source<>'' then workflow.executeForTarget(Source,sizeLimit,dest)
                      else workflow.executeForTarget(xRes,yRes,sizeLimit,dest);
        while progressQueue.calculating and (context.adapters^.noErrors) do begin
          currentProgress:=progressQueue.getProgressString;
          if currentProgress<>lastProgress then context.adapters^.raiseNote(currentProgress,tokenLocation);
          lastProgress:=currentProgress;
          ThreadSwitch;
          sleep(1000);
        end;
        progressQueue.cancelCalculation(true);
      end;
      workflow.destroy;
      if (context.adapters^.noErrors) and (dest=C_nullSourceOrTargetFileName) then with context.adapters^.picture do begin
        lock;
        if value=nil then value:=newFromWorkflowImage
                     else value^.copyFromImage(workflowImage);
        unlock;
        context.adapters^.raiseNote('Output of workflow copied to current image',tokenLocation);
      end;
      workflowImage.clear;

      leaveCriticalSection(imigCS);
      if isValid then exit(newVoidLiteral) else exit(nil);
    end else result:=nil;
  end;

PROCEDURE doCloseImage(VAR context:T_evaluationContext);
  begin
    with context.adapters^.picture do begin
      lock;
      if (value<>nil) then dispose(value,destroy);
      value:=nil;
      unlock;
    end;
  end;

FUNCTION loadImage_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR loadedImage:P_rawImage;
      ok:boolean=true;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string) then begin
      new(loadedImage,create(1,1));
      try
        loadedImage^.loadFromFile(P_stringLiteral(params^.value(0))^.value);
      except
        ok:=false;
        context.adapters^.raiseError('Error loading image '+params^.value(0)^.toString(),tokenLocation);
      end;
      if ok then with context.adapters^.picture do begin
        lock;
        if value<>nil then dispose(value,destroy);
        value:=loadedImage;
        unlock;
        result:=newVoidLiteral;
      end else dispose(loadedImage,destroy);
    end;
  end;

FUNCTION saveImage_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  VAR ok:boolean=true;
  begin
    result:=nil;
    with context.adapters^.picture do begin
      lock;
      if value=nil then begin
        unlock;
        context.adapters^.raiseNote('Cannot save image because no image is loaded.',tokenLocation);
        exit(newBoolLiteral(false));
      end;
      try
        if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_string)
        then value^.saveToFile(P_stringLiteral(params^.value(0))^.value)
        else if (params<>nil) and (params^.size=2) and (params^.value(0)^.literalType=lt_string) and (params^.value(1)^.literalType=lt_int)
        then value^.saveJpgWithSizeLimit(P_stringLiteral(params^.value(0))^.value,P_intLiteral(params^.value(1))^.value)
        else ok:=false;
      except
        on e:Exception do begin
          context.adapters^.raiseError(e.message,tokenLocation);
          ok:=false;
        end;
      end;
      unlock;
      if ok then result:=newVoidLiteral;
    end;
  end;

FUNCTION closeImage_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      doCloseImage(context);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION imageSize_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then with context.adapters^.picture do begin
      lock;
      if value<>nil then result:=newListLiteral(2)^.appendInt(value^.width)^.appendInt(value^.height)
                    else result:=newListLiteral();
      unlock;
    end;
  end;

FUNCTION resizeImage_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  CONST styleString:array[res_exact..res_fit] of string=('exact','fill','fit');
  VAR xRes:longint=0;
      yRes:longint=0;
      style:string='exact';
      s,r:T_resizeStyle;

  begin
    result:=nil;
    if (params<>nil) and (params^.size>=2) and (params^.value(0)^.literalType=lt_int) and (params^.value(1)^.literalType=lt_int)
      and ((params^.size=2) or (params^.size=3) and (params^.value(2)^.literalType=lt_string)) then begin
      xRes:=P_intLiteral(params^.value(0))^.value;
      yRes:=P_intLiteral(params^.value(1))^.value;
      if params^.size=3 then style:=P_stringLiteral(params^.value(2))^.value;
      r:=res_dataResize;
      for s:=res_exact to res_fit do if styleString[s]=style then r:=s;

      if (r=res_dataResize) or (xRes<=0) or (yRes<=0) then exit(nil);
      with context.adapters^.picture do begin
        lock;
        if value<>nil then begin
          value^.resize(xRes,yRes,r);
          result:=newVoidLiteral;
        end else context.adapters^.raiseError('Cannot resize image because no image is loaded',tokenLocation);
        unlock;
      end;
    end;
  end;

FUNCTION displayImage_imp(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; VAR context:T_evaluationContext):P_literal;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      if context.adapters^.picture.value<>nil then context.adapters^.raiseCustomMessage(mt_displayImage,'',tokenLocation)
                                              else context.adapters^.raiseNote('Cannot display image because no image is loaded',tokenLocation);
      result:=newVoidLiteral;
    end;
  end;

INITIALIZATION
  initCriticalSection(imigCS);
  registerRule(IMIG_NAMESPACE,'validateWorkflow',@validateWorkflow_imp,'validateWorkflow(wf:list);//Validates the workflow returning a boolean flag indicating validity');
  registerRule(IMIG_NAMESPACE,'executeWorkflow',@executeWorkflow_imp,'executeWorkflow(wf:list,xRes>0,yRes>0,target:string);#'+
                                                                     'executeWorkflow(wf:list,source:string,target:string);#'+
                                                                     'executeWorkflow(wf:list,xRes>0,yRes>0,sizeLimitInBytes>0,target:string);#'+
                                                                     'executeWorkflow(wf:list,source:string,sizeLimitInBytes>0,target:string);#Executes the workflow with the given options. Use "-" as source or target to read/write the current image.');
  registerRule(IMIG_NAMESPACE,'loadImage',@loadImage_imp,'loadImage(filename:string);//Loads image from the given file');
  registerRule(IMIG_NAMESPACE,'saveImage',@saveImage_imp,'saveImage(filename:string);//Saves the current image to the given file. Supported types: JPG, PNG, BMP, VRAW#saveImage(filename:string,sizeLimit:int);//Saves the current image to the given file limiting the output size (limit=0 for automatic limiting). JPG only.');
  registerRule(IMIG_NAMESPACE,'closeImage',@closeImage_imp,'closeImage;//Closes the current image, freeing associated memory');
  registerRule(IMIG_NAMESPACE,'imageSize',@imageSize_imp,'imageSize;//Returns the size as [width,height] of the current image.');
  registerRule(IMIG_NAMESPACE,'resizeImage',@resizeImage_imp,'resizeImage(xRes>0,yRes>0);//Resizes the current image#resizeImage(xRes>0,yRes>0,style in ["fit","fill"]);//Resizes the current image with non-default scaling options');

  registerRule(IMIG_NAMESPACE,'displayImage',@displayImage_imp,'displayImage;//Displays the current image.');
FINALIZATION
  doneCriticalSection(imigCS);
end.
