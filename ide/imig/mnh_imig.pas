UNIT mnh_imig;
INTERFACE
USES sysutils,   //system
     ExtCtrls,
     Classes,
     myGenerics,myTools,mySys, //common
     //mnh:
     mnh_constants, basicTypes,
     funcs,litVar,contexts,funcs_list,mnh_plotData,
     out_adapters,mnh_messages,
     recyclers,
     //imig:
     workflows, imageGeneration,mypics,pixMaps,
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
CONST
  WORKFLOW_START_INTERVAL_MILLISECONDS=1100;

TYPE
  P_imageSystem=^T_imageSystem;
  T_imageSystem=object(T_abstractGuiOutAdapter)
    private
      renderCommand:F_execPlotCallback;
      PROCEDURE processMessage(CONST message:P_storedMessage);
    public
      currentImage :P_rawImage;
      CONSTRUCTOR create(CONST renderCallback:F_execPlotCallback);
      DESTRUCTOR destroy; virtual;
      FUNCTION append(CONST message:P_storedMessage):boolean; virtual;
      FUNCTION flushToGui(CONST forceFlush:boolean):T_messageTypeSet; virtual;
  end;

  P_replaceImageMessage=^T_replaceImageMessage;
  T_replaceImageMessage=object(T_payloadMessage)
    private
      newImage:P_rawImage;
      retrieved:boolean;
    public
      CONSTRUCTOR createReplaceImageRequest(CONST img:P_rawImage);
      CONSTRUCTOR createGetImageRequest;
      FUNCTION getImageWaiting(CONST errorFlagProvider:P_messages):P_rawImage;
      PROCEDURE setImage(CONST image:P_rawImage);
  end;

  P_imageDimensionsMessage=^T_imageDimensionsMessage;
  T_imageDimensionsMessage=object(T_payloadMessage)
    private
      newWidth,newHeight:longint;
      retrieved:boolean;
    public
      CONSTRUCTOR createGetSizeRequest;
      FUNCTION getSizeWaiting(CONST errorFlagProvider:P_messages):T_imageDimensions;
      PROCEDURE setSize(CONST width,height:longint);
  end;

FUNCTION newImigSystemWithoutDisplay:P_imageSystem;
{$i func_defines.inc}
IMPLEMENTATION
FUNCTION newImigSystemWithoutDisplay:P_imageSystem;
  begin new(result,create(nil)); end;

FUNCTION obtainCurrentImageViaAdapters(CONST messages:P_messages):P_rawImage;
  VAR m:P_replaceImageMessage;
  begin
    new(m,createGetImageRequest);
    messages^.postCustomMessage(m);
    result:=m^.getImageWaiting(messages);
    disposeMessage(m);
  end;

PROCEDURE postNewImage(CONST messages:P_messages; CONST image:P_rawImage);
  VAR m:P_replaceImageMessage;
  begin
    new(m,createReplaceImageRequest(image));
    messages^.postCustomMessage(m,true);
  end;

FUNCTION obtainDimensionsViaAdapters(CONST messages:P_messages):T_imageDimensions;
  VAR m:P_imageDimensionsMessage;
  begin
    new(m,createGetSizeRequest);
    messages^.postCustomMessage(m);
    result:=m^.getSizeWaiting(messages);
    disposeMessage(m);
  end;

FUNCTION createWorkflow(CONST steps:P_listLiteral; CONST validating:boolean; OUT isValid:boolean; CONST tokenLocation:T_tokenLocation; VAR context:T_context; VAR recycler:T_recycler):T_imageManipulationWorkflow;
  PROCEDURE warn(CONST message:string);
    begin
      isValid:=false;
      if validating then context.messages^.postTextMessage(mt_el2_warning,tokenLocation,message)
                    else context.raiseError(message,tokenLocation);
    end;

  VAR i:longint;
      cmd:string;
      tmpSteps:P_listLiteral;
  begin
    result.create;
    if steps^.literalType=lt_stringList
    then tmpSteps:=steps
    else tmpSteps:=P_listLiteral(flatten_imp(steps,tokenLocation,context,recycler));
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
    if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_stringList,lt_list]) then begin
      wf:=createWorkflow(list0,true,isValid,tokenLocation,context,recycler);
      wf.destroy;
      result:=newBoolLiteral(isValid);
    end else result:=nil;
  end;

VAR workflowsActive:longint=0;
    workflowCs:TRTLCriticalSection;
    lastWorkflowStart:double=0;
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
      hasDelayMessage:boolean=false;

      obtainedImage:P_rawImage;

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
        outputLit:=outputMethod^.evaluateToLiteral(tokenLocation,@context,@recycler,sLit,nil).literal;
        disposeLiteral(sLit);
        if outputLit<>nil then disposeLiteral(outputLit);
      end else context.messages^.postTextMessage(mt_el1_note,tokenLocation,s);
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
        context.raiseError('Either target resolution or input image must be provided',tokenLocation);
        isValid:=false;
      end;
      if (dest='') then begin
        context.raiseError('No output file given',tokenLocation);
        isValid:=false;
      end;
      thisWorkflow:=createWorkflow(list0,false,isValid,tokenLocation,context,recycler);
      if isValid and (source=C_nullSourceOrTargetFileName) then begin
        obtainedImage:=obtainCurrentImageViaAdapters(context.messages);
        if obtainedImage=nil then begin
          context.raiseError('Current image ("-") given as input image but no current image loaded.',tokenLocation);
          isValid:=false;
        end else begin
          thisWorkflow.workflowImage.copyFromPixMap(obtainedImage^);
          doOutput('Input for workflow copied from current image');
        end;
        dispose(obtainedImage,destroy);
      end;
      if isValid then begin
        enterCriticalSection(workflowCs);
        while (workflowsActive>0) and (not(isMemoryInComfortZone) or (lastWorkflowStart+WORKFLOW_START_INTERVAL_MILLISECONDS*1E-3*ONE_SECOND>now)) do begin
          leaveCriticalSection(workflowCs);
          if not(hasDelayMessage) then begin
            hasDelayMessage:=true;
            context.messages^.postTextMessage(mt_el1_note,tokenLocation,'Start of workflow delayed.');
          end;
          sleep(random(WORKFLOW_START_INTERVAL_MILLISECONDS));
          ThreadSwitch;
          enterCriticalSection(workflowCs);
        end;
        lastWorkflowStart:=now;
        inc(workflowsActive);
        leaveCriticalSection(workflowCs);

        if source<>'' then begin
                             doOutput('Executing workflow with input="'+source+'", output="'+dest+'"');
                             thisWorkflow.executeForTarget(source,sizeLimit,dest);
                           end
                      else begin
                             doOutput('Executing workflow with xRes='+intToStr(xRes)+', yRes='+intToStr(yRes)+' output="'+dest+'"');
                             thisWorkflow.executeForTarget(xRes,yRes,sizeLimit,dest);
                           end;
        lastOutput:=now;
        while thisWorkflow.progressQueue.calculating and (context.messages^.continueEvaluation) do begin
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
        if not(context.messages^.continueEvaluation) then context.messages^.postTextMessage(mt_el2_warning,tokenLocation,'Image calculation incomplete');
        thisWorkflow.progressQueue.ensureStop;
        enterCriticalSection(workflowCs);
        dec(workflowsActive);
        if isMemoryInComfortZone then lastWorkflowStart:=now-ONE_MINUTE;
        leaveCriticalSection(workflowCs);
      end;
      if (context.messages^.continueEvaluation) and (dest=C_nullSourceOrTargetFileName) then begin
        postNewImage(context.messages,newFromWorkflowImage);
        doOutput('Output of workflow copied to current image');
      end;
      thisWorkflow.destroy;
      if isValid then exit(newVoidLiteral) else exit(nil);
    end else result:=nil;
  end;

FUNCTION loadImage_imp intFuncSignature;
  VAR loadedImage:P_rawImage;
      ok:boolean=true;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) and context.checkSideEffects('loadImage',tokenLocation,[se_readFile]) then begin
      new(loadedImage,create(1,1));
      try
        loadedImage^.loadFromFile(P_stringLiteral(arg0)^.value);
      except
        ok:=false;
        context.raiseError('Error loading image '+arg0^.toString(),tokenLocation);
      end;
      if ok then begin
        postNewImage(context.messages,loadedImage);
        result:=newVoidLiteral;
      end else dispose(loadedImage,destroy);
    end;
  end;

FUNCTION saveImage_imp intFuncSignature;
  VAR obtainedImage:P_rawImage;
  begin
    if not(context.checkSideEffects('saveImage',tokenLocation,[se_writeFile])) then exit(nil);
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_string)
                     and ((params^.size =1) or (params^.size=2) and (arg1^.literalType=lt_smallint)) then begin
      obtainedImage:=obtainCurrentImageViaAdapters(context.messages);
      if obtainedImage=nil then begin
        context.messages^.postTextMessage(mt_el1_note,tokenLocation,'Cannot save image because no image is loaded.');
        exit(nil);
      end;
      try
      if params^.size=2 then obtainedImage^.saveJpgWithSizeLimit(str0^.value,int1^.intValue)
                        else obtainedImage^.saveToFile(str0^.value);
      except
        on e:Exception do context.raiseError(e.message,tokenLocation);
      end;
      dispose(obtainedImage,destroy);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION closeImage_imp intFuncSignature;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      context.messages^.postSingal(mt_image_close,C_nilTokenLocation);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION imageSize_imp intFuncSignature;
  VAR tempImage:T_rawImage;
      size:T_imageDimensions;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      size:=obtainDimensionsViaAdapters(context.messages);
      result:=newListLiteral(2)^.appendInt(size.width)^.appendInt(size.height);
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      if not(fileExists(str0^.value)) then begin
        context.raiseError('File '+str0^.value+' does not exist',tokenLocation);
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
      obtainedImage:P_rawImage;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=2) and (arg0^.literalType in [lt_smallint,lt_bigint]) and (arg1^.literalType in [lt_smallint,lt_bigint])
      and ((params^.size=2) or (params^.size=3) and (arg2^.literalType=lt_string)) then begin
      xRes:=int0^.intValue;
      yRes:=int1^.intValue;
      if params^.size=3 then style:=str2^.value;
      r:=res_dataResize;
      for s:=res_exact to res_fit do if styleString[s]=style then r:=s;
      if (r=res_dataResize) or (xRes<=0) or (yRes<=0) then exit(nil);
      obtainedImage:=obtainCurrentImageViaAdapters(context.messages);
      if obtainedImage=nil then context.raiseError('Cannot resize image because no image is loaded',tokenLocation)
      else begin
        obtainedImage^.resize(xRes,yRes,r);
        postNewImage(context.messages,obtainedImage);
        result:=newVoidLiteral;
      end;
    end;
  end;

FUNCTION displayImage_imp intFuncSignature;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      context.messages^.postSingal(mt_image_postDisplay,C_nilTokenLocation);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION imageJpgRawData_imp intFuncSignature;
  VAR obtainedImage:P_rawImage;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      obtainedImage:=obtainCurrentImageViaAdapters(context.messages);
      if obtainedImage=nil then context.raiseError('Cannot display image because no image is loaded',tokenLocation)
      else begin
        result:=newStringLiteral(obtainedImage^.getJpgFileData());
        dispose(obtainedImage,destroy);
      end;
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint]) then begin
      obtainedImage:=obtainCurrentImageViaAdapters(context.messages);
      if obtainedImage=nil then context.raiseError('Cannot display image because no image is loaded',tokenLocation)
      else begin
        result:=newStringLiteral(obtainedImage^.getJpgFileData(int0^.intValue));
        dispose(obtainedImage,destroy);
      end;
    end;
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
        context.raiseError('File '+str0^.value+' does not exist',tokenLocation);
        exit(nil);
      end;
      img.create(str0^.value);
      img.resize(int1^.intValue,int2^.intValue,res_fit);
      result:=newStringLiteral(img.getJpgFileData(80));
      img.destroy;
    end;
  end;

FUNCTION renderPlotToCurrentImage intFuncSignature;
  VAR width, height: longint;
      renderRequest:P_plotRenderRequest;

      imgStream:TStringStream;
      plotImage:TImage;
      plotPic  :P_rawImage;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and
      (arg0^.literalType in [lt_smallint,lt_bigint]) and
      (arg1^.literalType in [lt_smallint,lt_bigint]) and
      ((params^.size = 2) or (params^.size = 3) and
      (arg2^.literalType in [lt_smallint,lt_bigint])) then begin
      width:=int0^.intValue;
      height:=int1^.intValue;
      new(renderRequest,createRenderToStringRequest(width,height));
      context.messages^.postCustomMessage(renderRequest^.rereferenced);
      plotImage:=TImage.create(nil);
      plotImage.SetInitialBounds(0,0,width,height);
      imgStream:=TStringStream.create(renderRequest^.getStringWaiting(context.messages));
      disposeMessage(renderRequest);
      imgStream.position:=0;
      plotImage.picture.PNG.loadFromStream(imgStream);
      imgStream.free;
      new(plotPic,create(1,1));
      plotPic^.copyFromImage(plotImage);
      FreeAndNil(plotImage);
      postNewImage(context.messages,plotPic);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION randomIfs_impl intFuncSignature;
  VAR ifs:T_ifs;
  begin
    ifs.create;
    ifs.resetParameters(1);
    result:=newStringLiteral(ifs.getAlgorithmName+ifs.toString());
    ifs.destroy;
  end;

CONSTRUCTOR T_imageDimensionsMessage.createGetSizeRequest;
  begin
    inherited create(mt_image_obtainDimensions);
    newWidth:=0;
    newHeight:=0;
    retrieved:=false;
  end;

FUNCTION T_imageDimensionsMessage.getSizeWaiting(CONST errorFlagProvider:P_messages):T_imageDimensions;
  begin
    enterCriticalSection(messageCs);
    while not(retrieved) and (errorFlagProvider^.continueEvaluation) do begin
      leaveCriticalSection(messageCs);
      sleep(1); ThreadSwitch;
      enterCriticalSection(messageCs);
    end;
    result.width:=newWidth;
    result.height:=newHeight;
    leaveCriticalSection(messageCs);
  end;

PROCEDURE T_imageDimensionsMessage.setSize(CONST width, height: longint);
  begin
    enterCriticalSection(messageCs);
    newWidth:=width;
    newHeight:=height;
    retrieved:=true;
    leaveCriticalSection(messageCs);
  end;

CONSTRUCTOR T_replaceImageMessage.createReplaceImageRequest(CONST img: P_rawImage);
  begin
    inherited create(mt_image_replaceImage);
    newImage:=img;
    retrieved:=true;
  end;

CONSTRUCTOR T_replaceImageMessage.createGetImageRequest;
  begin
    inherited create(mt_image_obtainImageData);
    newImage:=nil;
    retrieved:=false;
  end;

FUNCTION T_replaceImageMessage.getImageWaiting(CONST errorFlagProvider:P_messages): P_rawImage;
  begin
    enterCriticalSection(messageCs);
    while not(retrieved) and (errorFlagProvider^.continueEvaluation) do begin
      leaveCriticalSection(messageCs);
      sleep(1); ThreadSwitch;
      enterCriticalSection(messageCs);
    end;
    result:=newImage;
    leaveCriticalSection(messageCs);
  end;

PROCEDURE T_replaceImageMessage.setImage(CONST image: P_rawImage);
  begin
    enterCriticalSection(messageCs);
    newImage:=image;
    retrieved:=true;
    leaveCriticalSection(messageCs);
  end;

PROCEDURE T_imageSystem.processMessage(CONST message: P_storedMessage);
  begin
    case message^.messageType of
      mt_image_postDisplay : if renderCommand<>nil then renderCommand;
      mt_image_replaceImage: begin
        if currentImage<>nil then dispose(currentImage,destroy);
        currentImage:=P_replaceImageMessage(message)^.newImage;
      end;
      mt_image_close: begin
        if currentImage<>nil then dispose(currentImage,destroy);
        currentImage:=nil;
      end;
      mt_image_obtainImageData:
        if currentImage<>nil then P_replaceImageMessage(message)^.setImage(P_rawImage(currentImage^.getClone))
                             else P_replaceImageMessage(message)^.setImage(nil);
      mt_image_obtainDimensions:
        if currentImage<>nil then P_imageDimensionsMessage(message)^.setSize(currentImage^.dimensions.width,currentImage^.dimensions.height)
                             else P_imageDimensionsMessage(message)^.setSize(0,0);
    end;
  end;

CONSTRUCTOR T_imageSystem.create(CONST renderCallback: F_execPlotCallback);
  begin
    inherited create(at_imig,C_includableMessages[at_imig]);
    renderCommand:=renderCallback;
    currentImage:=nil;
  end;

DESTRUCTOR T_imageSystem.destroy;
  begin
    if currentImage<>nil then dispose(currentImage,destroy);
    inherited destroy;
  end;

FUNCTION T_imageSystem.append(CONST message: P_storedMessage): boolean;
  begin
    enterCriticalSection(adapterCs);
    try
      case message^.messageType of
        mt_image_postDisplay : result:=inherited append(message);
        mt_image_replaceImage,
        mt_image_close,
        mt_image_obtainImageData,
        mt_image_obtainDimensions:begin
          if collectedFill=0 then processMessage(message)
                             else inherited append(message);
          result:=true;
        end;
        else result:=false;
      end;
    finally
      leaveCriticalSection(adapterCs);
    end;
  end;

FUNCTION T_imageSystem.flushToGui(CONST forceFlush:boolean):T_messageTypeSet;
  VAR m:P_storedMessage;
  begin
    enterCriticalSection(adapterCs);
    try
      result:=typesOfStoredMessages;
      for m in getStoredMessages do processMessage(m);
      clear;
    finally
      leaveCriticalSection(adapterCs);
    end;
  end;

INITIALIZATION
  initialize(workflowCs);
  initCriticalSection(workflowCs);
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
  registerRule(IMIG_NAMESPACE,'renderPlotToCurrentImage',@renderPlotToCurrentImage,ak_binary,'renderPlotToCurrentImage(width,height);//Renders the current plot to the current image');
  registerRule(IMIG_NAMESPACE,'randomIfs',@randomIfs_impl,ak_nullary,'randomIfs;//returns a random IFS to be fed to executeWorkflow');
  lastWorkflowStart:=now-ONE_MINUTE;
FINALIZATION
  doneCriticalSection(workflowCs);
end.
