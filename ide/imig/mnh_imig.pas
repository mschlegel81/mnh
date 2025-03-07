UNIT mnh_imig;
INTERFACE
USES sysutils,
     ExtCtrls,
     Classes,
     myGenerics,mySys,
     //mnh:
     mnh_constants, basicTypes,
     mnh_settings,
     funcs,litVar,contexts,funcs_list,mnh_plotData,
     out_adapters,mnh_messages,
     recyclers,
     //imig:
     workflows, imageGeneration,mypics,pixMaps,ig_ifs;
CONST
  WORKFLOW_START_INTERVAL_MILLISECONDS=1100;
  C_rawDataOutput='+';

TYPE
  P_queryImigClosedMessage=^T_queryImigClosedMessage;
  T_queryImigClosedMessage=object(T_payloadMessage)
    private
      response:boolean;
      retrieved:boolean;
      plotWidth,plotHeight:longint;
    public
      FUNCTION internalType:shortstring; virtual;
      CONSTRUCTOR createRetrieveRequest;
      PROCEDURE setResponse(CONST r:boolean; CONST plotWidth_,plotHeight_:longint);
      FUNCTION getResponseWaiting(CONST errorFlagProvider:P_messages; OUT plotWidth_,plotHeight_:longint):boolean;
  end;

  P_imageSystem=^T_imageSystem;
  T_imageSystem=object(T_abstractGuiOutAdapter)
    protected
      renderCommand:F_execPlotCallback;
      PROCEDURE processMessage(CONST message:P_storedMessage); virtual;
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
USES myParams,generationBasics,imageContexts,myColors;
FUNCTION T_queryImigClosedMessage.internalType: shortstring;
begin
  result:='T_queryImigClosedMessage';
end;

CONSTRUCTOR T_queryImigClosedMessage.createRetrieveRequest;
  begin
    inherited create(mt_image_queryClosedByUser);
    retrieved:=false;
  end;

PROCEDURE T_queryImigClosedMessage.setResponse(CONST r: boolean; CONST plotWidth_,plotHeight_:longint);
  begin
    enterCriticalSection(messageCs);
    retrieved:=true;
    response:=r;
    plotWidth:=plotWidth_;
    plotHeight:=plotHeight_;
    leaveCriticalSection(messageCs);
  end;

FUNCTION T_queryImigClosedMessage.getResponseWaiting(CONST errorFlagProvider: P_messages; OUT plotWidth_,plotHeight_:longint): boolean;
  begin
    enterCriticalSection(messageCs);
    while not(retrieved) and (errorFlagProvider^.continueEvaluation) do begin
      leaveCriticalSection(messageCs);
      sleep(1); ThreadSwitch;
      enterCriticalSection(messageCs);
    end;
    result:=response;
    plotWidth_:=plotWidth;
    plotHeight_:=plotHeight;
    leaveCriticalSection(messageCs);
  end;

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

FUNCTION createWorkflow(CONST steps:P_listLiteral; CONST validating:boolean; OUT isValid:boolean; CONST tokenLocation:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler):T_standaloneWorkflow;
  VAR errorRaised:boolean=false;
  PROCEDURE warn(CONST message:string);
    begin
      errorRaised:=true;
      isValid:=false;
      if validating then context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,message)
                    else context^.raiseError(message,tokenLocation);
    end;

  VAR i:longint;
      tmpSteps:P_listLiteral;
      stepText:T_arrayOfString;
      msg:P_structuredMessage;
  begin
    result.create;
    result.messageQueue^.messageStringLengthLimit:=maxLongint;
    if steps^.literalType=lt_stringList
    then tmpSteps:=steps
    else tmpSteps:=P_listLiteral(flatten_imp(steps,tokenLocation,context,recycler));
    isValid:=(tmpSteps^.literalType=lt_stringList) and (tmpSteps^.size>=1);
    if isValid then begin
      setLength(stepText,tmpSteps^.size);
      for i:=0 to tmpSteps^.size-1 do stepText[i]:=P_stringLiteral(tmpSteps^.value[i])^.value;
      isValid:=result.parseWorkflow(stepText,false);
      msg:=result.messageQueue^.get;
      while msg<>nil do begin
        if msg^.indicatesError then warn(msg^.toString(maxLongint));
        dispose(msg,destroy);
        msg:=result.messageQueue^.get;
      end;
      imageContexts.maxImageManipulationThreads:=settings.cpuCount;
    end;
    if steps<>tmpSteps then recycler^.disposeLiteral(tmpSteps);
    if not(isValid) and not(validating) and not(errorRaised) then context^.raiseError('Invalid workflow',tokenLocation);
  end;

FUNCTION validateWorkflow_imp intFuncSignature;
  VAR wf:T_standaloneWorkflow;
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

PROCEDURE doOutput(CONST s:string; CONST warning:boolean; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler; CONST outputMethod:P_expressionLiteral);
  VAR sLit:P_stringLiteral;
      outputLit:P_literal;
  begin
    if outputMethod<>nil then begin
      sLit:=recycler^.newStringLiteral(s);
      outputLit:=evaluteExpression(outputMethod,location,context,recycler,sLit).literal;
      recycler^.disposeLiteral(sLit);
      if outputLit<>nil then recycler^.disposeLiteral(outputLit);
    end else begin
      if warning
      then context^.messages^.postTextMessage(mt_el2_warning,location,s)
      else context^.messages^.postTextMessage(mt_el1_note   ,location,s);
    end;
  end;

PROCEDURE pollLog(VAR thisWorkflow:T_simpleWorkflow; CONST location:T_tokenLocation; CONST context:P_context; CONST recycler:P_recycler; CONST outputMethod:P_expressionLiteral);
  VAR msg:P_structuredMessage;
  begin
    msg:=thisWorkflow.messageQueue^.get;
    while msg<>nil do begin
      doOutput(msg^.toString(maxLongint),msg^.indicatesError,location,context,recycler,outputMethod);
      dispose(msg,destroy);
      msg:=thisWorkflow.messageQueue^.get;
    end;
  end;

FUNCTION image_to_mnh_representation(CONST image:P_rawImage; CONST recycler:P_recycler):P_collectionLiteral;
  VAR dimensions: T_imageDimensions;
      x,y:longint;
      colorData:P_listLiteral;
      ScanLine: P_floatColor;
  begin
    dimensions:=image^.dimensions;
    colorData:=recycler^.newListLiteral(dimensions.width*dimensions.height);
    for y:=0 to dimensions.height-1 do begin
      ScanLine:=image^.linePtr(y);
      for x:=0 to dimensions.width-1 do begin
        colorData^.append(recycler,
                          recycler^.newListLiteral(3)^.appendReal(recycler,ScanLine^[cc_red])
                                                     ^.appendReal(recycler,ScanLine^[cc_green])
                                                     ^.appendReal(recycler,ScanLine^[cc_blue]),
                          false);
        inc(ScanLine);
      end;
    end;
    result:=recycler^.newListLiteral(3)^.append(recycler,colorData,false)^.appendInt(recycler,dimensions.width)^.appendInt(recycler,dimensions.height);
  end;

FUNCTION executeWorkflow_imp intFuncSignature;
  VAR isValid:boolean=true;
      source:string='';
      dest:string='';
      xRes:longint=0;
      yRes:longint=0;
      sizeLimit:longint=-1;
      i:longint;
      sleepTime:longint=1;
      outputMethod:P_expressionLiteral=nil;
      thisWorkflow:T_standaloneWorkflow;
      hasDelayMessage:boolean=false;

      obtainedImage:P_rawImage;

  FUNCTION newFromWorkflowImage:P_rawImage;
    begin
      new(result,create(thisWorkflow.image));
    end;

  begin
    if (params<>nil) and (params^.size>=2) and (arg0^.literalType in [lt_stringList,lt_list]) and context^.checkSideEffects('executeWorkflow',tokenLocation,[se_readFile,se_writeFile]) then begin
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
        context^.raiseError('Either target resolution or input image must be provided',tokenLocation);
        isValid:=false;
      end;
      if (dest='') then begin
        context^.raiseError('No output file given',tokenLocation);
        isValid:=false;
      end;
      thisWorkflow:=createWorkflow(list0,false,isValid,tokenLocation,context,recycler);
      if isValid then begin
        thisWorkflow.config.sizeLimit:=C_maxImageDimensions;
        if (source=C_nullSourceOrTargetFileName) then begin
          obtainedImage:=obtainCurrentImageViaAdapters(context^.messages);
          if obtainedImage=nil then begin
            context^.raiseError('Current image ("-") given as input image but no current image loaded.',tokenLocation);
            isValid:=false;
          end else begin
            thisWorkflow.config.setInitialImage(obtainedImage^);
            doOutput('Input for workflow copied from current image',false,tokenLocation,context,recycler,outputMethod);
          end;
          dispose(obtainedImage,destroy);
        end else if source<>''
        then thisWorkflow.config.setInitialImage(source)
        else thisWorkflow.config.initialResolution:=imageDimensions(xRes,yRes);
      end;
      if isValid then begin
        enterCriticalSection(workflowCs);
        while (workflowsActive>0) and not(memoryCleaner.isMemoryInComfortZone) do begin
          leaveCriticalSection(workflowCs);
          if not(hasDelayMessage) then begin
            hasDelayMessage:=true;
            context^.messages^.postTextMessage(mt_el1_note,tokenLocation,'Start of workflow delayed.');
          end;
          threadSleepMillis(random(WORKFLOW_START_INTERVAL_MILLISECONDS));
          enterCriticalSection(workflowCs);
        end;
        inc(workflowsActive);
        leaveCriticalSection(workflowCs);
        if (dest<>C_nullSourceOrTargetFileName) and (dest<>C_rawDataOutput) then try
          thisWorkflow.appendSaveStep(dest,sizeLimit);
        except
          on e:Exception do begin
            context^.raiseError(e.message,tokenLocation);
            isValid:=false;
          end;
        end;
        if isValid and thisWorkflow.isValid then begin
          maxImageManipulationThreads:=settings.cpuCount;
          thisWorkflow.executeWorkflowInBackground(false);
          if source<>''
          then doOutput('Executing workflow with input="'+source+'", output="'+dest+'"',false,tokenLocation,context,recycler,outputMethod)
          else doOutput('Executing workflow with xRes='+intToStr(xRes)+', yRes='+intToStr(yRes)+' output="'+dest+'"',false,tokenLocation,context,recycler,outputMethod);
          while thisWorkflow.executing and (context^.messages^.continueEvaluation) do begin
            pollLog(thisWorkflow,tokenLocation,context,recycler,outputMethod);
            threadSleepMillis(sleepTime);
            if sleepTime<1000 then inc(sleepTime);
          end;
          if not(context^.messages^.continueEvaluation) then context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,'Image calculation incomplete');
          thisWorkflow.ensureStop;
        end;
        pollLog(thisWorkflow,tokenLocation,context,recycler,outputMethod);
        enterCriticalSection(workflowCs);
        dec(workflowsActive);
        leaveCriticalSection(workflowCs);
      end;
      if (context^.messages^.continueEvaluation) then begin
        if (dest=C_nullSourceOrTargetFileName) then begin
          postNewImage(context^.messages,newFromWorkflowImage);
          doOutput('Output of workflow copied to current image',false,tokenLocation,context,recycler,outputMethod);
          result:=newBoolLiteral(thisWorkflow.isDone);
        end else if dest=C_rawDataOutput then begin
          result:=image_to_mnh_representation(@thisWorkflow.image,recycler);
        end else result:=newBoolLiteral(thisWorkflow.isDone);
      end else result:=nil;
      thisWorkflow.destroy;
    end else result:=nil;
  end;

FUNCTION executeTodo_imp intFuncSignature;
  VAR thisWorkflow:T_standaloneWorkflow;
      outputMethod:P_expressionLiteral=nil;
      sleepTime:longint=1;
  begin
    if (params^.size>=1) and (params^.size<=2) and
       (arg0^.literalType=lt_string) and
       ((params^.size=1) or (arg1^.literalType=lt_expression)) then begin
      if not(fileExists(str0^.value)) then begin
        context^.messages^.postTextMessage(mt_el2_warning,tokenLocation, 'File "'+str0^.value+'" does not exist');
        exit(newBoolLiteral(false));
      end;
      if params^.size>1 then outputMethod:=P_expressionLiteral(arg1);
      thisWorkflow.create;
      thisWorkflow.messageQueue^.messageStringLengthLimit:=maxLongint;
      if thisWorkflow.readWorkflowOnlyFromFile(str0^.value,false) then begin
        enterCriticalSection(workflowCs);
        inc(workflowsActive);
        maxImageManipulationThreads:=settings.cpuCount;
        thisWorkflow.executeAsTodo;
        leaveCriticalSection(workflowCs);
        while thisWorkflow.executing and (context^.messages^.continueEvaluation) do begin
          pollLog(thisWorkflow,tokenLocation,context,recycler,outputMethod);
          threadSleepMillis(sleepTime);
          if sleepTime<1000 then inc(sleepTime);
        end;
        if not(context^.messages^.continueEvaluation) then context^.messages^.postTextMessage(mt_el2_warning,tokenLocation,'Image calculation incomplete');
        thisWorkflow.ensureStop;
        enterCriticalSection(workflowCs);
        dec(workflowsActive);
        leaveCriticalSection(workflowCs);
      end;
      pollLog(thisWorkflow,tokenLocation,context,recycler,outputMethod);
      result:=newBoolLiteral(thisWorkflow.isDone);
      thisWorkflow.destroy;
    end else result:=nil;
  end;

FUNCTION loadImage_imp intFuncSignature;
  VAR loadedImage:P_rawImage;
      ok:boolean=true;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) and context^.checkSideEffects('loadImage',tokenLocation,[se_readFile]) then begin
      new(loadedImage,create(1,1));
      try
        loadedImage^.loadFromFile(P_stringLiteral(arg0)^.value);
      except
        ok:=false;
        context^.raiseError('Error loading image '+arg0^.toString(),tokenLocation);
      end;
      if ok then begin
        postNewImage(context^.messages,loadedImage);
        result:=newVoidLiteral;
      end else dispose(loadedImage,destroy);
    end;
  end;

FUNCTION saveImage_imp intFuncSignature;
  VAR obtainedImage:P_rawImage;
  begin
    if not(context^.checkSideEffects('saveImage',tokenLocation,[se_writeFile])) then exit(nil);
    result:=nil;
    if (params<>nil) and (params^.size>=1) and (arg0^.literalType=lt_string)
                     and ((params^.size =1) or (params^.size=2) and (arg1^.literalType=lt_smallint)) then begin
      obtainedImage:=obtainCurrentImageViaAdapters(context^.messages);
      if obtainedImage=nil then begin
        context^.messages^.postTextMessage(mt_el1_note,tokenLocation,'Cannot save image because no image is loaded.');
        exit(nil);
      end;
      try
      if params^.size=2 then obtainedImage^.saveJpgWithSizeLimit(str0^.value,int1^.intValue)
                        else obtainedImage^.saveToFile(str0^.value);
      except
        on e:Exception do context^.raiseError(e.message,tokenLocation);
      end;
      dispose(obtainedImage,destroy);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION closeImage_imp intFuncSignature;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      context^.messages^.postSingal(mt_image_close,tokenLocation);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION imageSize_imp intFuncSignature;
  VAR tempImage:T_rawImage;
      size:T_imageDimensions;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      size:=obtainDimensionsViaAdapters(context^.messages);
      result:=recycler^.listLiteralOf(
                recycler^.newIntLiteral(size.width),
                recycler^.newIntLiteral(size.height));
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      if not(fileExists(str0^.value)) then begin
        context^.raiseError('File '+str0^.value+' does not exist',tokenLocation);
        exit(nil);
      end;
      try
        tempImage.create(str0^.value);
        result:=recycler^.listLiteralOf(
                  recycler^.newIntLiteral(tempImage.dimensions.width),
                  recycler^.newIntLiteral(tempImage.dimensions.height));
      except
        if result<>nil then recycler^.disposeLiteral(result);
        result:=recycler^.newListLiteral(0);
      end;
      tempImage.destroy;
    end;
  end;

FUNCTION resizeImage_imp intFuncSignature;
  CONST styleString:array[res_exact..res_fitRotate] of string=('exact','fill','rotFill','fit','fitExpand','rotFit');
  VAR res:T_imageDimensions;
      style:string='exact';
      s,r:T_resizeStyle;
      obtainedImage:P_rawImage;
  begin
    result:=nil;
    if (params<>nil) and (params^.size>=2) and (arg0^.literalType in [lt_smallint,lt_bigint]) and (arg1^.literalType in [lt_smallint,lt_bigint])
      and ((params^.size=2) or (params^.size=3) and (arg2^.literalType=lt_string)) then begin
      res:=imageDimensions(int0^.intValue,int1^.intValue);
      if params^.size=3 then style:=str2^.value;
      r:=res_dataResize;
      for s:=res_exact to res_fit do if styleString[s]=style then r:=s;
      if (r=res_dataResize) or (res.width<=0) or (res.height<=0) then exit(nil);
      obtainedImage:=obtainCurrentImageViaAdapters(context^.messages);
      if obtainedImage=nil then context^.raiseError('Cannot resize image because no image is loaded',tokenLocation)
      else begin
        obtainedImage^.resize(res,r,true);
        postNewImage(context^.messages,obtainedImage);
        result:=newVoidLiteral;
      end;
    end;
  end;

FUNCTION displayImage_imp intFuncSignature;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      context^.messages^.postSingal(mt_image_postDisplay,tokenLocation);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION imageJpgRawData_imp intFuncSignature;
  VAR obtainedImage:P_rawImage;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      obtainedImage:=obtainCurrentImageViaAdapters(context^.messages);
      if obtainedImage=nil then context^.raiseError('Cannot display image because no image is loaded',tokenLocation)
      else begin
        result:=recycler^.newStringLiteral(obtainedImage^.getJpgFileData());
        dispose(obtainedImage,destroy);
      end;
    end else if (params<>nil) and (params^.size=1) and (arg0^.literalType in [lt_smallint,lt_bigint]) then begin
      obtainedImage:=obtainCurrentImageViaAdapters(context^.messages);
      if obtainedImage=nil then context^.raiseError('Cannot display image because no image is loaded',tokenLocation)
      else begin
        result:=recycler^.newStringLiteral(obtainedImage^.getJpgFileData(int0^.intValue));
        dispose(obtainedImage,destroy);
      end;
    end;
  end;

FUNCTION imageRawData_imp intFuncSignature;
  VAR obtainedImage:P_rawImage;
  begin
    result:=nil;
    if (params=nil) or (params^.size=0) then begin
      obtainedImage:=obtainCurrentImageViaAdapters(context^.messages);
      if obtainedImage=nil
      then context^.raiseError('Cannot display image because no image is loaded',tokenLocation)
      else begin
        result:=image_to_mnh_representation(obtainedImage,recycler);
        dispose(obtainedImage,destroy);
      end;
    end;
  end;

FUNCTION listManipulations_imp intFuncSignature;
  VAR op:P_imageOperationMeta;
  begin
    result:=recycler^.newListLiteral();
    for op in allImageOperations do listResult^.appendString(recycler,op^.getDefaultParameterString);
  end;

FUNCTION expandImageGeneration_imp intFuncSignature;
  VAR meta:P_algorithmMeta;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=1) and (arg0^.literalType=lt_string) then begin
      meta:=getAlgorithmOrNil(str0^.value,true);
      if meta=nil
      then exit(str0^.rereferenced)
      else exit(recycler^.newStringLiteral(meta^.prototype^.toFullString()));
    end;
  end;

FUNCTION getThumbnail_imp intFuncSignature;
  VAR img:T_rawImage;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=3) and
       (arg0^.literalType=lt_string) and
       (arg1^.literalType in [lt_smallint,lt_bigint]) and
       (arg2^.literalType in [lt_smallint,lt_bigint]) and context^.checkSideEffects('getThumbnail',tokenLocation,[se_readFile]) then begin
      if not(fileExists(str0^.value)) then begin
        context^.raiseError('File '+str0^.value+' does not exist',tokenLocation);
        exit(nil);
      end;
      img.create(str0^.value);
      img.resize(imageDimensions(int1^.intValue,int2^.intValue),res_fit,false);
      result:=recycler^.newStringLiteral(img.getJpgFileData(80));
      img.destroy;
    end;
  end;

FUNCTION renderPlotToCurrentImage intFuncSignature;
  VAR width, height: longint;
      renderRequest:P_plotRenderRequest;

      imgStream:TStringStream;
      plotImage:TImage;
      plotPic  :P_rawImage;
      dataLiteral: P_literal;
  begin
    result:=nil;
    if (params<>nil) and (params^.size=2) and
      (arg0^.literalType in [lt_smallint,lt_bigint]) and
      (arg1^.literalType in [lt_smallint,lt_bigint]) and
      ((params^.size = 2) or (params^.size = 3) and
      (arg2^.literalType in [lt_smallint,lt_bigint])) then begin
      width:=int0^.intValue;
      height:=int1^.intValue;
      new(renderRequest,createRenderToStringRequest(width,height,tokenLocation,context^.messages,false));
      context^.messages^.postCustomMessage(renderRequest);
      plotImage:=TImage.create(nil);
      plotImage.SetInitialBounds(0,0,width,height);
      dataLiteral:=renderRequest^.getLiteralWaiting(context^.messages);
      imgStream:=TStringStream.create(P_stringLiteral(dataLiteral)^.value);
      recycler^.disposeLiteral(dataLiteral);
      disposeMessage(renderRequest);
      imgStream.position:=0;
      plotImage.picture.Bitmap.loadFromStream(imgStream);
      imgStream.free;
      new(plotPic,create(1,1));
      plotPic^.copyFromImage(plotImage);
      FreeAndNil(plotImage);
      postNewImage(context^.messages,plotPic);
      result:=newVoidLiteral;
    end;
  end;

FUNCTION randomIfs_impl intFuncSignature;
  VAR ifs:T_ifs;
  begin
    ifs.create;
    ifs.resetParameters(1);
    result:=recycler^.newStringLiteral(ifs.getAlgorithmName+ifs.toString(tsm_forSerialization));
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
    threadStartsSleeping;
    enterCriticalSection(messageCs);
    while not(retrieved) and (errorFlagProvider^.continueEvaluation) do begin
      leaveCriticalSection(messageCs);
      sleep(1); ThreadSwitch;
      enterCriticalSection(messageCs);
    end;
    threadStopsSleeping;
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
    threadStartsSleeping;
    enterCriticalSection(messageCs);
    while not(retrieved) and (errorFlagProvider^.continueEvaluation) do begin
      leaveCriticalSection(messageCs);
      sleep(1); ThreadSwitch;
      enterCriticalSection(messageCs);
    end;
    threadStopsSleeping;
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
        mt_image_queryClosedByUser,
        mt_image_postDisplay : result:=inherited append(message);
        mt_startOfEvaluation,
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
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'validateWorkflow',@validateWorkflow_imp,ak_unary);
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'executeWorkflow',@executeWorkflow_imp,ak_variadic_3,[se_readFile,se_writeFile]);
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'executeTodo',@executeTodo_imp,ak_variadic_1,[se_readFile,se_writeFile]);
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'loadImage'      ,@loadImage_imp      ,ak_unary,[se_readFile]);
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'saveImage'      ,@saveImage_imp      ,ak_unary,[se_writeFile]);
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'closeImage'     ,@closeImage_imp     ,ak_nullary);
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'imageSize'      ,@imageSize_imp      ,ak_variadic);
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'resizeImage'    ,@resizeImage_imp    ,ak_variadic_2);
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'displayImage'   ,@displayImage_imp   ,ak_nullary);
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'imageJpgRawData',@imageJpgRawData_imp,ak_nullary);
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'imageRawData'   ,@imageRawData_imp   ,ak_nullary);
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'listManipulations',@listManipulations_imp,ak_nullary);
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'calculateThumbnail',@getThumbnail_imp,ak_ternary,[se_readFile]);
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'renderPlotToCurrentImage',@renderPlotToCurrentImage,ak_binary);
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'randomIfs',@randomIfs_impl,ak_nullary);
  builtinFunctionMap.registerRule(IMIG_NAMESPACE,'expandImageGeneration',@expandImageGeneration_imp,ak_unary);
FINALIZATION
  doneCriticalSection(workflowCs);
end.
