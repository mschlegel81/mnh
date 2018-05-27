UNIT mnh_imig;
INTERFACE
USES sysutils,   //system
     ExtCtrls,
     myGenerics,myTools, //common
     //mnh:
     mnh_constants, mnh_basicTypes,
     mnh_funcs,mnh_litVar,mnh_contexts,mnh_funcs_list,mnh_plotData,
     //imig:
     workflows, imageGeneration,mypics,
     ig_gradient,
     ig_perlin,
     ig_simples,
     ig_fractals,
     ig_epicycles,
     ig_ifs,
     ig_ifs2,
     ig_bifurcation,
     ig_funcTrees,
     ig_expoClouds;

{$i mnh_func_defines.inc}
IMPLEMENTATION
VAR imigCS:TRTLCriticalSection;
FUNCTION createWorkflow(CONST steps:P_listLiteral; CONST validating:boolean; OUT isValid:boolean; CONST tokenLocation:T_tokenLocation; VAR context:T_threadContext):T_imageManipulationWorkflow;
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
        cmd:=P_stringLiteral(tmpSteps^.value[i])^.value;
        if not(result.addStep(cmd))
        then warn('Invalid workflow step: '+tmpSteps^.value[i]^.toString);
      end;
    end;
    if steps<>tmpSteps then disposeLiteral(tmpSteps);
  end;

FUNCTION validateWorkflow_imp intFuncSignature;
  VAR wf:T_imageManipulationWorkflow;
      isValid:boolean;
  begin
    enterCriticalSection(imigCS);
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_stringList,lt_list]) then begin
      wf:=createWorkflow(list0,true,isValid,tokenLocation,context);
      wf.destroy;
      result:=newBoolLiteral(isValid);
    end else result:=nil;
    leaveCriticalSection(imigCS);
  end;

FUNCTION executeWorkflow_imp intFuncSignature;
  CONST aditionalOutputInterval=1/(24*60); //one minute
  VAR isValid:boolean=true;
      source:string='';
      dest:string='';
      xRes:longint=0;
      yRes:longint=0;
      sizeLimit:longint=-1;
      i:longint;
      sleepTime:longint=1;
      lastOutput:double;
      progressLog:T_progressLog;
      logLinesDisplayed:longint=0;
      outputMethod:P_expressionLiteral=nil;
      thisWorkflow:T_imageManipulationWorkflow;

  FUNCTION newFromWorkflowImage:P_rawImage;
    begin
      new(result,create(thisWorkflow.workflowImage));
    end;

  PROCEDURE doOutput(CONST s:string);
    VAR sLit:P_stringLiteral;
        outputLit:P_literal;
    begin
      if outputMethod<>nil then begin
        sLit:=newStringLiteral(s);
        outputLit:=outputMethod^.evaluateToLiteral(tokenLocation,@context,sLit).literal;
        disposeLiteral(sLit);
        if outputLit<>nil then disposeLiteral(outputLit);
      end else context.adapters^.raiseNote(s,tokenLocation);
    end;

  begin
    if (params<>nil) and (params^.size>=2) and (arg0^.literalType in [lt_stringList,lt_list]) and context.checkSideEffects('executeWorkflow',tokenLocation,[se_readFile,se_writeFile]) then begin
      for i:=1 to params^.size-1 do begin
        case params^.value[i]^.literalType of
          lt_bigint,lt_smallint: begin
            if not(P_abstractIntLiteral(params^.value[i])^.isBetween(1,maxLongint)) then exit(nil);
            if      xRes=0 then xRes:=P_abstractIntLiteral(params^.value[i])^.intValue
            else if yRes=0 then yRes:=P_abstractIntLiteral(params^.value[i])^.intValue
            else if sizeLimit<=-1 then sizeLimit:=P_abstractIntLiteral(params^.value[i])^.intValue
            else exit(nil);
          end;
          lt_string: begin
            if source=''  then source:=P_stringLiteral(params^.value[i])^.value
            else if dest='' then dest:=P_stringLiteral(params^.value[i])^.value
            else exit(nil);
          end;
          lt_expression: if outputMethod=nil then outputMethod:=P_expressionLiteral(params^.value[i]) else exit(nil);
          else exit(nil);
        end;
      end;
      if (xRes>0) and (yRes=0) then begin sizeLimit:=xRes; xRes:=0; end;
      if (dest='') and (source<>'') then begin dest:=source; source:=''; end;

      if (source='') and ((xRes=0) or (yRes=0)) then begin
        context.adapters^.raiseError('Either target resolution or input image must be provided',tokenLocation);
        isValid:=false;
      end;
      if (dest='') then begin
        context.adapters^.raiseError('No output file given',tokenLocation);
        isValid:=false;
      end;
      if (source=C_nullSourceOrTargetFileName) or (dest=C_nullSourceOrTargetFileName) then enterCriticalSection(imigCS);
      thisWorkflow:=createWorkflow(list0,false,isValid,tokenLocation,context);
      if isValid and (source=C_nullSourceOrTargetFileName) then with context.adapters^.picture do begin
        lock;
        if value=nil then begin
          context.adapters^.raiseError('Current image ("-") given as input image but no current image loaded.',tokenLocation);
          isValid:=false;
        end else begin
          thisWorkflow.workflowImage.copyFromPixMap(value^);
          doOutput('Input for workflow copied from current image');
        end;
        unlock;
      end;
      if isValid then begin
        if source<>'' then begin
                             doOutput('Executing workflow with input="'+source+'", output="'+dest+'"');
                             thisWorkflow.executeForTarget(source,sizeLimit,dest);
                           end
                      else begin
                             doOutput('Executing workflow with xRes='+intToStr(xRes)+', yRes='+intToStr(yRes)+' output="'+dest+'"');
                             thisWorkflow.executeForTarget(xRes,yRes,sizeLimit,dest);
                           end;
        lastOutput:=now;
        while thisWorkflow.progressQueue.calculating and (context.adapters^.noErrors) do begin
          progressLog:=thisWorkflow.progressQueue.log;
          for i:=logLinesDisplayed to length(progressLog)-1 do if progressLog[i].message<>'' then begin
            doOutput(intToStr(i)+'/'+intToStr(thisWorkflow.stepCount+1)+': '+progressLog[i].message);
            logLinesDisplayed:=i+1;
            lastOutput:=now;
          end;
          if (now-lastOutput>aditionalOutputInterval) then begin
            doOutput(thisWorkflow.progressQueue.getProgressString(true));
            lastOutput:=now;
          end;
          ThreadSwitch;
          sleep(sleepTime);
          if sleepTime<1000 then inc(sleepTime);
        end;
        progressLog:=thisWorkflow.progressQueue.log;
        for i:=logLinesDisplayed to length(progressLog)-1 do begin
          doOutput(intToStr(i)+'/'+intToStr(thisWorkflow.stepCount+1)+': '+progressLog[i].message);
          logLinesDisplayed:=i+1;
        end;
        if not(context.adapters^.noErrors) then context.adapters^.raiseWarning('Image calculation incomplete',tokenLocation);
        thisWorkflow.progressQueue.ensureStop;
      end;
      if (context.adapters^.noErrors) and (dest=C_nullSourceOrTargetFileName) then with context.adapters^.picture do begin
        lock;
        if value=nil then value:=newFromWorkflowImage
                     else value^.copyFromPixMap(thisWorkflow.workflowImage);
        unlock;
        doOutput('Output of workflow copied to current image');
      end;
      thisWorkflow.destroy;
      if (source=C_nullSourceOrTargetFileName) or (dest=C_nullSourceOrTargetFileName) then leaveCriticalSection(imigCS);
      if isValid then exit(newVoidLiteral) else exit(nil);
    end else result:=nil;
  end;

PROCEDURE doCloseImage(VAR context:T_threadContext);
  begin
    enterCriticalSection(imigCS);
    with context.adapters^.picture do begin
      lock;
      if (value<>nil) then dispose(value,destroy);
      value:=nil;
      unlock;
    end;
    leaveCriticalSection(imigCS);
  end;

FUNCTION loadImage_imp intFuncSignature;
  VAR loadedImage:P_rawImage;
      ok:boolean=true;
  begin
    enterCriticalSection(imigCS);
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) and context.checkSideEffects('loadImage',tokenLocation,[se_readFile]) then begin
      new(loadedImage,create(1,1));
      try
        loadedImage^.loadFromFile(P_stringLiteral(arg0)^.value);
      except
        ok:=false;
        context.adapters^.raiseError('Error loading image '+arg0^.toString(),tokenLocation);
      end;
      if ok then with context.adapters^.picture do begin
        lock;
        if value<>nil then dispose(value,destroy);
        value:=loadedImage;
        unlock;
        result:=newVoidLiteral;
      end else dispose(loadedImage,destroy);
    end;
    leaveCriticalSection(imigCS);
  end;

FUNCTION saveImage_imp intFuncSignature;
  VAR ok:boolean=true;
  begin
    if not(context.checkSideEffects('saveImage',tokenLocation,[se_writeFile])) then exit(nil);
    enterCriticalSection(imigCS);
    result:=nil;
    with context.adapters^.picture do begin
      lock;
      if value=nil then begin
        unlock;
        context.adapters^.raiseNote('Cannot save image because no image is loaded.',tokenLocation);
        exit(newBoolLiteral(false));
      end;
      try
        if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string)
        then value^.saveToFile(P_stringLiteral(arg0)^.value)
        else if (params<>nil) and (params^.size=2) and (arg0^.literalType=lt_string) and (arg1^.literalType in [lt_smallint,lt_bigint])
        then value^.saveJpgWithSizeLimit(str0^.value,int1^.intValue)
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
    leaveCriticalSection(imigCS);
  end;

FUNCTION closeImage_imp intFuncSignature;
  begin
    enterCriticalSection(imigCS);
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      doCloseImage(context);
      result:=newVoidLiteral;
    end;
    leaveCriticalSection(imigCS);
  end;

FUNCTION imageSize_imp intFuncSignature;
  VAR tempImage:T_rawImage;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then with context.adapters^.picture do begin
      enterCriticalSection(imigCS);
      lock;
      if value<>nil then result:=newListLiteral(2)^.appendInt(value^.dimensions.width)^.appendInt(value^.dimensions.height)
                    else result:=newListLiteral(0);
      unlock;
      leaveCriticalSection(imigCS);
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      if not(fileExists(str0^.value)) then begin
        context.adapters^.raiseError('File '+str0^.value+' does not exist',tokenLocation);
        exit(nil);
      end;
      try
        tempImage.create(str0^.value);
        result:=newListLiteral(2)^.appendInt(tempImage.dimensions.width)^.appendInt(tempImage.dimensions.height)
      except
        if result<>nil then disposeLiteral(result);
        result:=newListLiteral(0);
      end;
      tempImage.destroy;
    end;
  end;

FUNCTION resizeImage_imp intFuncSignature;
  CONST styleString:array[res_exact..res_fitRotate] of string=('exact','fill','rotFill','fit','fitExpand','rotFit');
  VAR xRes:longint=0;
      yRes:longint=0;
      style:string='exact';
      s,r:T_resizeStyle;

  begin
    enterCriticalSection(imigCS);
    result:=nil;
    if (params<>nil) and (params^.size>=2) and (arg0^.literalType in [lt_smallint,lt_bigint]) and (arg1^.literalType in [lt_smallint,lt_bigint])
      and ((params^.size=2) or (params^.size=3) and (arg2^.literalType=lt_string)) then begin
      xRes:=int0^.intValue;
      yRes:=int1^.intValue;
      if params^.size=3 then style:=str2^.value;
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
    leaveCriticalSection(imigCS);
  end;

FUNCTION displayImage_imp intFuncSignature;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      if context.adapters^.picture.value<>nil
      then context.adapters^.logDisplayImage
      else context.adapters^.raiseNote('Cannot display image because no image is loaded',tokenLocation);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION imageJpgRawData_imp intFuncSignature;
  begin
    enterCriticalSection(imigCS);
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      if context.adapters^.picture.value<>nil
      then result:=newStringLiteral(context.adapters^.picture.value^.getJpgFileData)
      else context.adapters^.raiseError('Cannot display image because no image is loaded',tokenLocation);
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint]) then begin
      if context.adapters^.picture.value<>nil
      then result:=newStringLiteral(context.adapters^.picture.value^.getJpgFileData(int0^.intValue))
      else context.adapters^.raiseError('Cannot display image because no image is loaded',tokenLocation);
    end;
    leaveCriticalSection(imigCS);
  end;

FUNCTION listManipulations_imp intFuncSignature;
  VAR imt:T_imageManipulationType;
      alg:P_algorithmMeta;
  begin
    result:=newListLiteral();
    for imt:=imt_loadImage to high(T_imageManipulationType) do
      listResult^.appendString(stepParamDescription[imt]^.getDefaultParameterString);
    for alg in algorithms do listResult^.appendString(alg^.prototype^.toString(false));
  end;

FUNCTION getThumbnail_imp intFuncSignature;
  VAR img:T_rawImage;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and
       (arg0^.literalType=lt_string) and
       (arg1^.literalType in [lt_smallint,lt_bigint]) and
       (arg2^.literalType in [lt_smallint,lt_bigint]) and context.checkSideEffects('getThumbnail',tokenLocation,[se_readFile]) then begin
      if not(fileExists(str0^.value)) then begin
        context.adapters^.raiseError('File '+str0^.value+' does not exist',tokenLocation);
        exit(nil);
      end;
      img.create(str0^.value);
      img.resize(int1^.intValue,int2^.intValue,res_fit);
      result:=newStringLiteral(img.getJpgFileData(80));
      img.destroy;
    end;
  end;

FUNCTION renderPlotToCurrentImage intFuncSignature;
  VAR width, height, quality: longint;
      plotImage:TImage;
      plotPic  :P_rawImage;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=2) and
      (arg0^.literalType in [lt_smallint,lt_bigint]) and
      (arg1^.literalType in [lt_smallint,lt_bigint]) and
      ((params^.size = 2) or (params^.size = 3) and
      (arg2^.literalType in [lt_smallint,lt_bigint])) then begin
      width:=int0^.intValue;
      height:=int1^.intValue;
      if params^.size>2 then quality:=int2^.intValue
                        else quality:=0;
      if  (width<1) or (height<1) or (quality<PLOT_QUALITY_LOW) or (quality>PLOT_QUALITY_HIGH) then exit(nil);

      plotImage:=context.adapters^.plot^.obtainPlot(width,height,quality);
      new(plotPic,create(1,1));
      plotPic^.copyFromImage(plotImage);
      FreeAndNil(plotImage);

      enterCriticalSection(imigCS);
      result:=nil;
      with context.adapters^.picture do begin
        lock;
        if value<>nil then dispose(value,destroy);
        value:=plotPic;
        unlock;
      end;
      leaveCriticalSection(imigCS);
      result:=newVoidLiteral;
    end;
  end;

INITIALIZATION
  initialize(imigCS);
  initCriticalSection(imigCS);
  registerRule(IMIG_NAMESPACE,'validateWorkflow',@validateWorkflow_imp,ak_unary,'validateWorkflow(wf:list);//Validates the workflow returning a boolean flag indicating validity');
  registerRule(IMIG_NAMESPACE,'executeWorkflow',@executeWorkflow_imp,ak_variadic_3,'executeWorkflow(wf:list,xRes>0,yRes>0,target:string);#'+
                                                                     'executeWorkflow(wf:list,source:string,target:string);#'+
                                                                     'executeWorkflow(wf:list,xRes>0,yRes>0,sizeLimitInBytes>0,target:string);#'+
                                                                     'executeWorkflow(wf:list,source:string,sizeLimitInBytes>0,target:string);#//Executes the workflow with the given options. Use "-" as source or target to read/write the current image.'+
                                                                     '#//Give an additional expression(1) parameter for progress output');
  registerRule(IMIG_NAMESPACE,'loadImage'      ,@loadImage_imp      ,ak_unary,'loadImage(filename:string);//Loads image from the given file');
  registerRule(IMIG_NAMESPACE,'saveImage'      ,@saveImage_imp      ,ak_unary,'saveImage(filename:string);//Saves the current image to the given file. Supported types: JPG, PNG, BMP, VRAW#saveImage(filename:string,sizeLimit:int);//Saves the current image to the given file limiting the output size (limit=0 for automatic limiting). JPG only.');
  registerRule(IMIG_NAMESPACE,'closeImage'     ,@closeImage_imp     ,ak_nullary,'closeImage;//Closes the current image, freeing associated memory');
  registerRule(IMIG_NAMESPACE,'imageSize'      ,@imageSize_imp      ,ak_variadic,'imageSize;//Returns the size as [width,height] of the current image.#imageSize(filename:String);//Returns the size of the given file');
  registerRule(IMIG_NAMESPACE,'resizeImage'    ,@resizeImage_imp    ,ak_variadic_2,'resizeImage(xRes>0,yRes>0);//Resizes the current image#resizeImage(xRes>0,yRes>0,style in ["exact","fill","rotFill","fit","fitExpand","rotFit"]);//Resizes the current image with non-default scaling options');
  registerRule(IMIG_NAMESPACE,'displayImage'   ,@displayImage_imp   ,ak_nullary,'displayImage;//Displays the current image.');
  registerRule(IMIG_NAMESPACE,'imageJpgRawData',@imageJpgRawData_imp,ak_nullary,'imageJpgRawData;//Returns the image raw data in JPG representation.');
  registerRule(IMIG_NAMESPACE,'listManipulations',@listManipulations_imp,ak_nullary,'listManipulations;//Returns a list of all possible image manipulation steps.');
  registerRule(IMIG_NAMESPACE,'calculateThumbnail',@getThumbnail_imp,ak_ternary,'calculateThumbnail(file:string,maxXRes:int,maxYRes:int);//Returns a JPG thumbnail data for given input file');
  registerRule(IMIG_NAMESPACE,'renderPlotToCurrentImage',@renderPlotToCurrentImage,ak_ternary,'renderPlotToCurrentImage(width,height,quality in [0..3]);//Renders the current plot to the current image');
FINALIZATION
  doneCriticalSection(imigCS);
end.
