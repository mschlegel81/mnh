UNIT mnh_plotData;
INTERFACE
USES sysutils,
     math,
     Interfaces, Classes, ExtCtrls, Graphics, types,Forms, ComCtrls,
     mySys,myGenerics,
     basicTypes, mnh_constants,
     mnh_settings,
     mnh_messages,
     out_adapters,
     plotstyles,plotMath,
     litVar,
     EpikTimer,
     BGRABitmap,BGRACanvas,BGRABitmapTypes;
CONST VOLATILE_FRAMES_MAX=50;
TYPE
  T_boundingBox = array['x'..'y', 0..1] of double;
  T_timedPlotExecution=object
    timer:TEpikTimer;
    timeout:double;
    PROCEDURE wait;
  end;

  P_addRowMessage=^T_addRowMessage;
  T_addRowMessage=object(T_payloadMessage)
    protected
      FUNCTION internalType:shortstring; virtual;
    public
      styleOptions: string;
      rowData:T_dataRow;
      used:boolean;
      CONSTRUCTOR create(CONST styleOptions_: string; CONST rowData_:T_dataRow);
  end;

  P_addTextMessage=^T_addTextMessage;
  T_addTextMessage=object(T_payloadMessage)
    protected
      FUNCTION internalType:shortstring; virtual;
    public
      customText:P_customText;
      CONSTRUCTOR create(CONST cText:P_customText);
  end;

  P_plotOptionsMessage=^T_plotOptionsMessage;
  T_plotOptionsMessage=object(T_payloadMessage)
    private
      options:T_scalingOptions;
      modified:T_scalingOptionElements;
      retrieved:boolean;
    protected
      FUNCTION internalType:shortstring; virtual;
    public
      CONSTRUCTOR createRetrieveRequest;
      CONSTRUCTOR createPostRequest(CONST o:T_scalingOptions; CONST m:T_scalingOptionElements);
      PROCEDURE setOptions(CONST o:T_scalingOptions);
      FUNCTION getOptionsWaiting(CONST errorFlagProvider:P_messages):T_scalingOptions;
  end;

  P_plotRenderRequest=^T_plotRenderRequest;
  T_plotRenderRequest=object(T_payloadMessage)
    private
      targetIsString:boolean;
      fileName:string;
      width,height:longint;
      retrieved:boolean;
      outputString:string;
    protected
      FUNCTION internalType:shortstring; virtual;
    public
      CONSTRUCTOR createRenderToFileRequest  (CONST filename_:string; CONST width_,height_:longint);
      CONSTRUCTOR createRenderToStringRequest(CONST width_,height_:longint);
      PROCEDURE setString(CONST s:string);
      FUNCTION getStringWaiting(CONST errorFlagProvider:P_messages):string;
      PROPERTY isRenderToStringRequest:boolean read targetIsString;
  end;

  P_plotDropRowRequest=^T_plotDropRowRequest;
  T_plotDropRowRequest=object(T_payloadMessage)
    private
      count:longint;
      dropRows:boolean;
    protected
      FUNCTION internalType:shortstring; virtual;
    public
      CONSTRUCTOR create(CONST numberOfRowsToDrop:longint; CONST dropRows_:boolean);
  end;

  P_plotDisplayRequest=^T_plotDisplayRequest;
  T_plotDisplayRequest=object(T_payloadMessage)
    private
      displayExecuted:boolean;
    protected
      FUNCTION internalType:shortstring; virtual;
    public
      CONSTRUCTOR create();
      PROCEDURE waitForExecution(CONST errorFlagProvider:P_messages);
      PROCEDURE markExecuted;
  end;

  P_plotAddAnimationFrameRequest=^T_plotAddAnimationFrameRequest;
  T_plotAddAnimationFrameRequest=object(T_payloadMessage)
    private
      propSleep:double;
    protected
      FUNCTION internalType:shortstring; virtual;
    public
      CONSTRUCTOR create();
      FUNCTION getProposedSleepTime(CONST errorFlagProvider:P_messages):double;
      PROCEDURE markExecuted(CONST proposedSleepTime:double);
  end;

  P_plot =^T_plot;
  T_plot = object
    private
      cs: TRTLCriticalSection;
      scalingOptions:T_scalingOptions;
      row: array of T_sampleRow;
      virtualRowIndex:longint;
      customText:array of P_customText;
      PROCEDURE setScalingOptions(CONST value:T_scalingOptions);
      FUNCTION  getScalingOptions:T_scalingOptions;
      PROCEDURE drawGridAndRows(CONST target: TBGRACanvas; VAR gridTic: T_ticInfos);
      PROCEDURE drawCoordSys(CONST target: TBGRACanvas; VAR gridTic: T_ticInfos);
      PROCEDURE drawCustomText(CONST target: TBGRACanvas);
      FUNCTION  obtainPlot(CONST width,height:longint):TImage;

      PROCEDURE addRow(CONST styleOptions: string; CONST rowData: T_dataRow);
      PROCEDURE removeRows(CONST numberOfRowsToRemove:longint);
      PROCEDURE addCustomText(CONST text:P_customText);
      PROCEDURE removeCustomText(CONST numberOfEntriesToRemove:longint);
    public
      PROPERTY options:T_scalingOptions read getScalingOptions write setScalingOptions;

      CONSTRUCTOR createWithDefaults;
      PROCEDURE setDefaults;
      DESTRUCTOR destroy;
      PROCEDURE clear;

      PROCEDURE zoomOnPoint(CONST pixelX, pixelY: longint; CONST factor: double; VAR plotImage: TImage);
      PROCEDURE panByPixels(CONST pixelDX, pixelDY: longint; VAR plotImage: TImage);

      PROCEDURE renderPlot(VAR plotImage: TImage);
      PROCEDURE renderToFile(CONST fileName:string; CONST width,height:longint);
      FUNCTION renderToString(CONST width,height:longint):ansistring;

      PROCEDURE copyFrom(VAR p:T_plot);
      FUNCTION getRowStatements(CONST prevOptions:T_scalingOptions; VAR globalRowData:T_listLiteral; CONST haltExport:PBoolean; CONST Application:Tapplication; CONST progress:TProgressBar):T_arrayOfString;
      FUNCTION getRowStatementCount:longint;
  end;

  T_frameCacheMode=(fcm_none,fcm_retainImage,fcm_inMemoryPng,fcm_tempFile);

  P_plotSeriesFrame=^T_plotSeriesFrame;
  T_plotSeriesFrame=object
    private
      backgroundPreparation:boolean;

      cachedImage:record
        image:TImage;
        dumpIsUpToDate:boolean;
        dumpName:string;
        inMemoryDump:TMemoryStream;
        renderedWidth  ,
        renderedHeight :longint;
      end;

      postedWidth  ,
      postedHeight :longint;
      plotData:T_plot;
      PROCEDURE performPostedPreparation;
    public
      CONSTRUCTOR create(VAR currentPlot:T_plot);
      DESTRUCTOR destroy;
      PROCEDURE doneImage(CONST cacheMode:T_frameCacheMode);
      PROCEDURE obtainImage(VAR target:TImage; CONST timing:T_timedPlotExecution);
      PROCEDURE prepareImage(CONST width,height:longint);
      PROCEDURE postPreparation(CONST width,height:longint);

      FUNCTION isImagePreparedForResolution(CONST width,height:longint):boolean;
      FUNCTION hasResolution(CONST width,height:longint):boolean;
  end;

  T_plotSeries=object
    private
      frame:array of P_plotSeriesFrame;
      framesWithImagesAllocated:array[0..7] of P_plotSeriesFrame;
      seriesCs:TRTLCriticalSection;
      weHadAMemoryPanic:boolean;
      volatile:boolean;
      FUNCTION getOptions(CONST index:longint):T_scalingOptions;
      PROCEDURE setOptions(CONST index:longint; CONST value:T_scalingOptions);
      PROCEDURE flushFramesToDisk;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE clear(CONST isVolatile:boolean=false);
      FUNCTION frameCount:longint;
      PROCEDURE getFrame(VAR target:TImage; CONST frameIndex:longint; CONST timing:T_timedPlotExecution);
      PROCEDURE renderFrame(CONST index:longint; CONST fileName:string; CONST width,height:longint; CONST exportingAll:boolean);
      PROCEDURE addFrame(VAR plot:T_plot);
      FUNCTION nextFrame(VAR frameIndex:longint; CONST cycle:boolean; CONST width,height:longint):boolean;
      PROPERTY options[index:longint]:T_scalingOptions read getOptions write setOptions;
      PROPERTY isSeriesVolatile:boolean read volatile;
      PROCEDURE resolutionChanged(CONST newWidth,newHeight:longint);
  end;

  F_execPlotCallback=PROCEDURE of object;
  F_pullSettingsToGuiCallback=PROCEDURE of object;
  P_plotSystem=^T_plotSystem;
  T_plotSystem=object(T_abstractGuiOutAdapter)
    protected
      pullSettingsToGui:F_pullSettingsToGuiCallback;
      plotChangedSinceLastDisplay:boolean;
      PROCEDURE processMessage(CONST message:P_storedMessage); virtual;
    private
      displayImmediate           :boolean;
      doPlot:F_execPlotCallback;
      sandboxed:boolean;
    public
      currentPlot:T_plot;
      animation:T_plotSeries;

      CONSTRUCTOR create(CONST executePlotCallback:F_execPlotCallback; CONST isSandboxSystem:boolean);
      DESTRUCTOR destroy; virtual;
      FUNCTION append(CONST message:P_storedMessage):boolean; virtual;
      FUNCTION flushToGui(CONST forceFlush:boolean):T_messageTypeSet; virtual;
      PROCEDURE logPlotDone;
      PROCEDURE startGuiInteraction;
      PROCEDURE doneGuiInteraction;
      FUNCTION getPlotStatement(CONST frameIndexOrNegativeIfAll:longint; CONST haltExport:PBoolean; CONST Application:Tapplication; CONST progress:TProgressBar):T_arrayOfString;
      PROPERTY isPlotChanged:boolean read plotChangedSinceLastDisplay;
      FUNCTION isEmpty:boolean;
  end;

FUNCTION newPlotSystemWithoutDisplay:P_plotSystem;
FUNCTION getOptionsViaAdapters(CONST messages:P_messages):T_scalingOptions;
FUNCTION timedPlotExecution(CONST timer:TEpikTimer; CONST timeout:double):T_timedPlotExecution;
IMPLEMENTATION
USES FPReadPNG,
     FPWritePNG,
     IntfGraphics,
     myStringUtil,
     commandLineParameters;
FUNCTION timedPlotExecution(CONST timer:TEpikTimer; CONST timeout:double):T_timedPlotExecution;
  begin
    result.timer:=timer;
    result.timeout:=timeout;
  end;

VAR MAJOR_TIC_STYLE, MINOR_TIC_STYLE:T_style;
FUNCTION boundingBoxOf(CONST x0,y0,x1,y1:double):T_boundingBox;
  begin
    result['x',0]:=min(x0,x1);
    result['x',1]:=max(x0,x1);
    result['y',0]:=min(y0,y1);
    result['y',1]:=max(y0,y1);
  end;

FUNCTION intersect(CONST b1,b2:T_boundingBox):boolean;
  begin
    result:=(max(b1['x',0],b2['x',0])<=min(b1['x',1],b2['x',1])) and
            (max(b1['y',0],b2['y',0])<=min(b1['y',1],b2['y',1]));
  end;

FUNCTION getOptionsViaAdapters(CONST messages:P_messages):T_scalingOptions;
  VAR request:P_plotOptionsMessage;
  begin
    new(request,createRetrieveRequest);
    messages^.postCustomMessage(request);
    result:=request^.getOptionsWaiting(messages);
    disposeMessage(request);
  end;

FUNCTION T_plotAddAnimationFrameRequest.internalType: shortstring;
  begin result:='P_plotAddAnimationFrameRequest'; end;

CONSTRUCTOR T_plotAddAnimationFrameRequest.create();
  begin
    inherited create(mt_plot_addAnimationFrame);
    propSleep:=-1;
  end;

FUNCTION T_plotAddAnimationFrameRequest.getProposedSleepTime(CONST errorFlagProvider:P_messages):double;
  begin
    enterCriticalSection(messageCs);
    while (propSleep<0) and (errorFlagProvider^.continueEvaluation) do begin
      leaveCriticalSection(messageCs);
      sleep(1); ThreadSwitch;
      enterCriticalSection(messageCs);
    end;
    result:=propSleep;
    leaveCriticalSection(messageCs);
  end;

PROCEDURE T_plotAddAnimationFrameRequest.markExecuted(CONST proposedSleepTime:double);
  begin
    enterCriticalSection(messageCs);
    if (proposedSleepTime<0) or (isNan(proposedSleepTime)) or (isInfinite(proposedSleepTime))
    then propSleep:=0
    else propSleep:=proposedSleepTime;
    leaveCriticalSection(messageCs);
  end;

PROCEDURE T_timedPlotExecution.wait;
  begin
    if timer=nil then exit;
    if timeout-timer.elapsed>0.1 then sleep(round(1000*(timeout-timer.elapsed))-100);
    if timeout-timer.elapsed>0   then sleep(round(1000*(timeout-timer.elapsed))    );
  end;

CONSTRUCTOR T_plotSeriesFrame.create(VAR currentPlot: T_plot);
  begin
    with cachedImage do begin
      image:=nil;
      dumpIsUpToDate:=false;
      dumpName:='';
      inMemoryDump:=nil;
      renderedWidth :=-1;
      renderedHeight:=-1;
    end;
    backgroundPreparation:=false;
    plotData.createWithDefaults;
    plotData.copyFrom(currentPlot);
  end;

DESTRUCTOR T_plotSeriesFrame.destroy;
  begin
    enterCriticalSection(plotData.cs);
    try
      doneImage(fcm_none);
      if cachedImage.dumpName<>'' then DeleteFile(cachedImage.dumpName);
    finally
      leaveCriticalSection(plotData.cs);
      plotData.destroy;
    end;
  end;

PROCEDURE T_plotSeriesFrame.doneImage(CONST cacheMode:T_frameCacheMode);
  begin
    enterCriticalSection(plotData.cs);
    try
      with cachedImage do case cacheMode of
        fcm_none       : begin
          //Don't retain; clear all data and mark as invalid
          renderedHeight:=-1;
          renderedWidth :=-1;
          if fileExists(dumpName) then DeleteFile(dumpName);
          dumpName:='';
          if inMemoryDump<>nil then FreeAndNil(inMemoryDump);
          if image<>nil then FreeAndNil(image);
        end;
        fcm_retainImage: begin
          if (inMemoryDump<>nil) and dumpIsUpToDate then begin
            //If we have an in memory dump then prefer this (compressed memory)
            if image<>nil then FreeAndNil(image);
          end else
          //Just retain the image; clear in MemoryDump is present
          if inMemoryDump<>nil then FreeAndNil(inMemoryDump);
        end;
        fcm_inMemoryPng: begin
          if (not(dumpIsUpToDate) or (inMemoryDump=nil)) and (image<>nil) then begin
            inMemoryDump:=TMemoryStream.create;
            image.picture.PNG.saveToStream(inMemoryDump);
          end;
          if fileExists(dumpName) then DeleteFile(dumpName);
          dumpName:='';
          if image<>nil then FreeAndNil(image);
        end;
        fcm_tempFile: begin
          //Offload all data to temp file
          if (not(dumpIsUpToDate) or not(fileExists(dumpName))) and (image<>nil) then begin
            if dumpName='' then dumpName:=getTempFileName;
            image.picture.PNG.saveToFile(dumpName);
          end else if (inMemoryDump<>nil) and dumpIsUpToDate and not(fileExists(dumpName)) then begin
            if dumpName='' then dumpName:=getTempFileName;
            inMemoryDump.Seek(0,soBeginning);
            inMemoryDump.saveToFile(dumpName);
          end;
          if inMemoryDump<>nil then FreeAndNil(inMemoryDump);
          if image<>nil then FreeAndNil(image);
        end;
      end;
    finally
      leaveCriticalSection(plotData.cs);
    end;
  end;

PROCEDURE T_plotSeriesFrame.prepareImage(CONST width,height:longint);
  VAR imageIsPrepared:boolean=false;
  begin
    enterCriticalSection(plotData.cs);
    try
      if (cachedImage.renderedWidth  =width  ) and
         (cachedImage.renderedHeight =height ) then begin
        if cachedImage.image<>nil then imageIsPrepared:=true
        else with cachedImage do if inMemoryDump<>nil then begin
          image:=TImage.create(nil);
          image.SetInitialBounds(0,0,width,height);
          inMemoryDump.Seek(0,soBeginning);
          image.picture.PNG.loadFromStream(inMemoryDump);
          dumpIsUpToDate:=true;
          imageIsPrepared:=true;
        end else if (dumpName<>'') and fileExists(dumpName) then begin
          image:=TImage.create(nil);
          image.SetInitialBounds(0,0,width,height);
          image.picture.PNG.loadFromFile(dumpName);
          dumpIsUpToDate:=true;
          imageIsPrepared:=true;
        end;
      end;
      if not(imageIsPrepared) then begin
        enterCriticalSection(globalTextRenderingCs);
        if cachedImage.image<>nil then FreeAndNil(cachedImage.image);
        leaveCriticalSection(globalTextRenderingCs);
        cachedImage.image:=plotData.obtainPlot(width,height);
        cachedImage.dumpIsUpToDate:=false;
        cachedImage.renderedHeight :=height;
        cachedImage.renderedWidth  :=width;
      end;
    finally
      leaveCriticalSection(plotData.cs);
    end;
  end;

PROCEDURE T_plotSeriesFrame.performPostedPreparation;
  begin
    enterCriticalSection(plotData.cs);
    try
      prepareImage(postedWidth,postedHeight);
    finally
      backgroundPreparation:=false;
      leaveCriticalSection(plotData.cs);
    end;
  end;

VAR preparationThreadsRunning:longint=0;
FUNCTION preparationThread(p:pointer):ptrint;
  begin
    P_plotSeriesFrame(p)^.performPostedPreparation;
    interlockedDecrement(preparationThreadsRunning);
    result:=0
  end;

PROCEDURE T_plotSeriesFrame.postPreparation(CONST width,height:longint);
  begin
    if backgroundPreparation or isImagePreparedForResolution(width,height) then exit;
    enterCriticalSection(plotData.cs);
    if backgroundPreparation or isImagePreparedForResolution(width,height) then begin
      leaveCriticalSection(plotData.cs);
      exit;
    end;
    try
      backgroundPreparation:=true;
      postedHeight:=height;
      postedWidth:=width;
      interLockedIncrement(preparationThreadsRunning);
      beginThread(@preparationThread,@self);
    finally
      leaveCriticalSection(plotData.cs);
    end;
  end;

FUNCTION T_plotSeriesFrame.isImagePreparedForResolution(CONST width,height:longint):boolean;
  begin
    enterCriticalSection(plotData.cs);
    result:=(cachedImage.renderedWidth=width) and
            (cachedImage.renderedHeight=height) and
            (cachedImage.image<>nil);
    leaveCriticalSection(plotData.cs);
  end;

FUNCTION T_plotSeriesFrame.hasResolution(CONST width,height:longint):boolean;
  begin
    enterCriticalSection(plotData.cs);
    result:=(cachedImage.renderedWidth=width) and
            (cachedImage.renderedHeight=height);
    leaveCriticalSection(plotData.cs);
  end;

PROCEDURE T_plotSeriesFrame.obtainImage(VAR target: TImage; CONST timing:T_timedPlotExecution);
  begin
    enterCriticalSection(plotData.cs);
    try
      prepareImage(target.width,target.height);
      timing.wait;
      target.Canvas.draw(0,0,cachedImage.image.picture.Bitmap);
    finally
      leaveCriticalSection(plotData.cs);
    end;
  end;

FUNCTION T_plotDisplayRequest.internalType: shortstring;
  begin
    result:='P_plotDisplayRequest';
  end;

CONSTRUCTOR T_plotDisplayRequest.create();
  begin
    inherited create(mt_plot_postDisplay);
    displayExecuted:=false;
  end;

PROCEDURE T_plotDisplayRequest.waitForExecution(CONST errorFlagProvider:P_messages);
  begin
    enterCriticalSection(messageCs);
    while not(displayExecuted) and (errorFlagProvider^.continueEvaluation) do begin
      leaveCriticalSection(messageCs);
      sleep(1); ThreadSwitch;
      enterCriticalSection(messageCs);
    end;
    leaveCriticalSection(messageCs);
  end;

PROCEDURE T_plotDisplayRequest.markExecuted;
  begin
    enterCriticalSection(messageCs);
    displayExecuted:=true;
    leaveCriticalSection(messageCs);
  end;

FUNCTION T_addTextMessage.internalType: shortstring;
  begin
    result:='T_addTextMessage';
  end;

CONSTRUCTOR T_addTextMessage.create(CONST cText: P_customText);
  begin
    inherited create(mt_plot_addText);
    customText:=cText;
  end;

FUNCTION T_plotDropRowRequest.internalType: shortstring;
  begin
    result:='T_plotDropRowRequest';
  end;

CONSTRUCTOR T_plotDropRowRequest.create(CONST numberOfRowsToDrop: longint; CONST dropRows_:boolean);
  begin
    inherited create(mt_plot_dropRow);
    count:=numberOfRowsToDrop;
    dropRows:=dropRows_;
  end;

FUNCTION T_plotRenderRequest.internalType: shortstring;
  begin
    result:='T_plotRenderRequest';
  end;

CONSTRUCTOR T_plotRenderRequest.createRenderToFileRequest(CONST filename_: string; CONST width_, height_: longint);
  begin
    inherited create(mt_plot_renderRequest);
    targetIsString:=false;
    fileName:=filename_;
    width:=width_;
    height:=height_;
    retrieved:=false;
    outputString:='';
  end;

CONSTRUCTOR T_plotRenderRequest.createRenderToStringRequest(CONST width_,height_: longint);
  begin
    inherited create(mt_plot_renderRequest);
    targetIsString:=true;
    fileName:='';
    width:=width_;
    height:=height_;
    retrieved:=false;
    outputString:='';
  end;

PROCEDURE T_plotRenderRequest.setString(CONST s: string);
  begin
    enterCriticalSection(messageCs);
    outputString:=s;
    retrieved:=true;
    leaveCriticalSection(messageCs);
  end;

FUNCTION T_plotRenderRequest.getStringWaiting(CONST errorFlagProvider:P_messages): string;
  begin
    enterCriticalSection(messageCs);
    while not(retrieved) and (errorFlagProvider^.continueEvaluation) do begin
      leaveCriticalSection(messageCs);
      sleep(1); ThreadSwitch;
      enterCriticalSection(messageCs);
    end;
    result:=outputString;
    leaveCriticalSection(messageCs);
  end;

FUNCTION T_plotOptionsMessage.internalType: shortstring;
  begin
    result:='T_plotOptionsMessage';
  end;

CONSTRUCTOR T_plotOptionsMessage.createRetrieveRequest;
  begin
    inherited create(mt_plot_retrieveOptions);
    options.setDefaults;
    retrieved:=false;
  end;

CONSTRUCTOR T_plotOptionsMessage.createPostRequest(CONST o: T_scalingOptions; CONST m:T_scalingOptionElements);
  begin
    inherited create(mt_plot_setOptions);
    retrieved:=true;
    options:=o;
    modified:=m;
  end;

PROCEDURE T_plotOptionsMessage.setOptions(CONST o: T_scalingOptions);
  begin
    enterCriticalSection(messageCs);
    options:=o;
    retrieved:=true;
    leaveCriticalSection(messageCs);
  end;

FUNCTION T_plotOptionsMessage.getOptionsWaiting(CONST errorFlagProvider:P_messages):T_scalingOptions;
  begin
    enterCriticalSection(messageCs);
    while not(retrieved) and (errorFlagProvider^.continueEvaluation) do begin
      leaveCriticalSection(messageCs);
      sleep(1); ThreadSwitch;
      enterCriticalSection(messageCs);
    end;
    result:=options;
    leaveCriticalSection(messageCs);
  end;

FUNCTION T_addRowMessage.internalType: shortstring;
  begin
    result:='T_addRowMessage';
  end;

CONSTRUCTOR T_addRowMessage.create(CONST styleOptions_: string; CONST rowData_: T_dataRow);
  begin
    inherited create(mt_plot_addRow);
    styleOptions:=styleOptions_;
    rowData     :=rowData_;
    used        :=false;
  end;

FUNCTION T_plotSeries.getOptions(CONST index: longint): T_scalingOptions;
  begin
    enterCriticalSection(seriesCs);
    result:=frame[index]^.plotData.scalingOptions;
    leaveCriticalSection(seriesCs);
  end;

PROCEDURE T_plotSeries.setOptions(CONST index: longint; CONST value: T_scalingOptions);
  begin
    enterCriticalSection(seriesCs);
    frame[index]^.plotData.scalingOptions:=value;
    frame[index]^.doneImage(fcm_none);
    leaveCriticalSection(seriesCs);
  end;

PROCEDURE T_plotSeries.flushFramesToDisk;
  VAR k:longint;
      cleanupTimeout:double;
  begin
    enterCriticalSection(seriesCs);
    cleanupTimeout:=now+1/(24*60*60);
    try
      if settings.cacheAnimationFrames then begin
        //First move cache from images to in-memory PNGs
        for k:=0 to length(frame)-1 do if (now<cleanupTimeout) and (frame[k]^.cachedImage.image<>nil)
        then frame[k]^.doneImage(fcm_inMemoryPng);
        //If there is time left, move cache from in-memory PNGs to temp files
        if (now<cleanupTimeout) then
        for k:=0 to length(frame)-1 do if (now<cleanupTimeout)
        then frame[k]^.doneImage(fcm_tempFile);
      end else begin
        //If caching is disabled, clear all caches (if there are any)
        for k:=0 to length(frame)-1 do if (now<cleanupTimeout)
        then frame[k]^.doneImage(fcm_none);
      end;
    finally
      leaveCriticalSection(seriesCs);
    end;
  end;

CONSTRUCTOR T_plotSeries.create;
  begin
    setLength(frame,0);
    initCriticalSection(seriesCs);
    clear;
    memoryCleaner.registerObjectForCleanup(@flushFramesToDisk);
  end;

DESTRUCTOR T_plotSeries.destroy;
  begin
    memoryCleaner.unregisterObjectForCleanup(@flushFramesToDisk);
    clear;
    doneCriticalSection(seriesCs);
  end;

PROCEDURE T_plotSeries.clear(CONST isVolatile:boolean=false);
  VAR k:longint;
  begin
    enterCriticalSection(seriesCs);
    try
      for k:=0 to length(frame)-1 do dispose(frame[k],destroy);
      setLength(frame,0);
      for k:=0 to length(framesWithImagesAllocated)-1 do framesWithImagesAllocated[k]:=nil;
      volatile:=isVolatile;
      weHadAMemoryPanic:=false;
    finally
      leaveCriticalSection(seriesCs);
    end;
  end;

FUNCTION T_plotSeries.frameCount: longint;
  begin
    enterCriticalSection(seriesCs);
    result:=length(frame);
    leaveCriticalSection(seriesCs);
  end;

PROCEDURE T_plotSeries.getFrame(VAR target: TImage; CONST frameIndex: longint; CONST timing:T_timedPlotExecution);
  VAR current:P_plotSeriesFrame;

  PROCEDURE handleImagesToFree;
    VAR k,j:longint;
        cacheMode:T_frameCacheMode;
    begin
      //remove current from list
      k:=0;
      while k<length(framesWithImagesAllocated)-1 do
      if framesWithImagesAllocated[k]=current then begin
        for j:=k to length(framesWithImagesAllocated)-2 do
        framesWithImagesAllocated[j]:=framesWithImagesAllocated[j+1];
        framesWithImagesAllocated[length(framesWithImagesAllocated)-1]:=nil;
      end else inc(k);
      //deallocate the last one
      k:=length(framesWithImagesAllocated)-1;
      if settings.cacheAnimationFrames then begin
        if not(isMemoryInComfortZone) then weHadAMemoryPanic:=true;
        if weHadAMemoryPanic
        then cacheMode:=fcm_inMemoryPng
        else cacheMode:=fcm_retainImage;
      end else cacheMode:=fcm_none;
      if (framesWithImagesAllocated[k]<>nil) then framesWithImagesAllocated[k]^.doneImage(cacheMode);
      //shift
      move(framesWithImagesAllocated[0],
           framesWithImagesAllocated[1],
           (length(framesWithImagesAllocated)-1)*sizeOf(P_plotSeriesFrame));
      //insert
      framesWithImagesAllocated[0]:=current;
    end;

  begin
    enterCriticalSection(seriesCs);
    if (frameIndex<0) or (frameIndex>=length(frame)) then begin
      leaveCriticalSection(seriesCs);
      exit;
    end;
    try
      current:=frame[frameIndex];
      if not(volatile) then handleImagesToFree;
      current^.obtainImage(target,timing);
    finally
      leaveCriticalSection(seriesCs);
    end;
  end;

PROCEDURE T_plotSeries.renderFrame(CONST index:longint; CONST fileName:string; CONST width,height:longint; CONST exportingAll:boolean);
  VAR storeImage:TImage;
  begin
    enterCriticalSection(seriesCs);
    if (index<0) or (index>=length(frame)) then begin
      leaveCriticalSection(seriesCs);
      exit;
    end;
    try
      {$ifndef unix}
      if exportingAll and (index+settings.cpuCount-1<length(frame)) then frame[index+settings.cpuCount-1]^.postPreparation(width,height);
      {$endif}

      storeImage:=TImage.create(nil);
      storeImage.SetInitialBounds(0,0,width,height);
      frame[index]^.obtainImage(storeImage,timedPlotExecution(nil,0));
      storeImage.picture.PNG.saveToFile(ChangeFileExt(fileName, '.png'));
      enterCriticalSection(globalTextRenderingCs);
      storeImage.destroy;
      leaveCriticalSection(globalTextRenderingCs);
      frame[index]^.doneImage(fcm_none);
    finally
      leaveCriticalSection(seriesCs);
    end;
  end;

PROCEDURE T_plotSeries.addFrame(VAR plot: T_plot);
  VAR k:longint=0;
  begin
    enterCriticalSection(seriesCs);
    try
      k:=length(frame);
      setLength(frame,k+1);
      new(frame[k],create(plot));
    finally
      leaveCriticalSection(seriesCs);
    end;
  end;

FUNCTION T_plotSeries.nextFrame(VAR frameIndex: longint; CONST cycle:boolean; CONST width,height:longint):boolean;
  {$ifndef unix}
  VAR toPrepare,
      lastToPrepare:longint;
  {$endif}
  VAR i,j,k:longint;
  begin
    enterCriticalSection(seriesCs);
    try
      if length(frame)=0 then begin
        frameIndex:=-1;
        result:=false;
      end else begin
        if volatile then begin
          j:=0;
          k:=0;
          for i:=0 to length(frame)-1 do if (i>=frameIndex) then begin
            frame[j]:=frame[i];
            inc(j);
          end else begin
            dispose(frame[i],destroy);
            inc(k);
          end;
          if k>0 then begin
            setLength(frame,j);
            dec(frameIndex,k);
          end;
        end;
        result:=true;
        inc(frameIndex);
        if frameIndex>=length(frame) then begin
          if cycle then frameIndex:=0
                   else begin
                     frameIndex:=length(frame)-1;
                     result:=false;
                   end;
        end;
        {$ifndef unix}
        if result then begin
          if not(weHadAMemoryPanic) and isMemoryInComfortZone and (settings.cacheAnimationFrames and not(volatile)) then begin
            if cycle then lastToPrepare:=frameIndex+length(frame)-1
                     else lastToPrepare:=           length(frame)-1;
          end else begin
            lastToPrepare:=frameIndex+settings.cpuCount;
            if (lastToPrepare>=length(frame)) and not(cycle) then lastToPrepare:=length(frame)-1;
          end;
          for toPrepare:=frameIndex+1 to lastToPrepare do
            if (preparationThreadsRunning<settings.cpuCount) then frame[toPrepare mod length(frame)]^.postPreparation(width,height);
        end;
        {$endif}
      end;
    finally
      leaveCriticalSection(seriesCs);
    end;
  end;

PROCEDURE T_plotSeries.resolutionChanged(CONST newWidth,newHeight:longint);
  VAR i:longint;
  begin
    enterCriticalSection(seriesCs);
    try
      for i:=0 to length(frame)-1 do if not(frame[i]^.hasResolution(newWidth,newHeight)) and
                                        not(frame[i]^.hasResolution(-1,-1)) then frame[i]^.doneImage(fcm_none);
    finally
      leaveCriticalSection(seriesCs);
    end;
  end;

CONSTRUCTOR T_plot.createWithDefaults;
  begin
    system.initCriticalSection(cs);
    setDefaults;
  end;

PROCEDURE T_plot.setDefaults;
  begin
    system.enterCriticalSection(cs);
    try
      scalingOptions.setDefaults;
      clear;
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

DESTRUCTOR T_plot.destroy;
  begin
    system.enterCriticalSection(cs);
    try
      clear;
    finally
      system.leaveCriticalSection(cs);
      doneCriticalSection(cs);
    end;
  end;

PROCEDURE T_plot.clear;
  VAR i: longint;
  begin
    system.enterCriticalSection(cs);
    try
      for i:=0 to length(row)-1 do row[i].destroy;
      setLength(row, 0);
      virtualRowIndex:=0;
      for i:=0 to length(customText)-1 do dispose(customText[i],destroy);
      setLength(customText,0);
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

PROCEDURE T_plot.addRow(CONST styleOptions: string; CONST rowData: T_dataRow);
  VAR index:longint;
      style:T_style;
      firstRow:boolean=true;
      clonedData:T_dataRow;
  begin
    system.enterCriticalSection(cs);
    try
      for style in getStyles(virtualRowIndex,styleOptions) do begin
        index:=length(row);
        setLength(row, index+1);
        if firstRow then row[index].create(rowData) else begin
          rowData.cloneTo(clonedData);
          row[index].create(clonedData);
        end;
        row[index].style:=style;
        row[index].pseudoIndex:=virtualRowIndex;
        firstRow:=false;
      end;
      virtualRowIndex+=1;
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

PROCEDURE T_plot.removeRows(CONST numberOfRowsToRemove: longint);
  VAR i0,i:longint;
  begin
    if numberOfRowsToRemove<=0 then exit;
    system.enterCriticalSection(cs);
    try
      virtualRowIndex-=numberOfRowsToRemove;
      i0:=-1;
      for i:=0 to length(row)-1 do begin
        if i0<0 then i0:=i;
        if row[i].pseudoIndex<=virtualRowIndex then row[i].destroy;
      end;
      setLength(row,i0);
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

PROCEDURE T_plot.addCustomText(CONST text: P_customText);
  begin
    system.enterCriticalSection(cs);
    try
      setLength(customText,length(customText)+1);
      customText[length(customText)-1]:=text;
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

PROCEDURE T_plot.removeCustomText(CONST numberOfEntriesToRemove:longint);
  VAR i0,i:longint;
  begin
    if numberOfEntriesToRemove<=0 then exit;
    system.enterCriticalSection(cs);
    try
      i0:=length(customText)-numberOfEntriesToRemove; if i0<0 then i0:=0;
      for i:=i0 to length(customText)-1 do dispose(customText[i],destroy);
      setLength(customText,i0);
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

PROCEDURE T_plot.setScalingOptions(CONST value: T_scalingOptions);
  begin
    system.enterCriticalSection(cs);
    scalingOptions:=value;
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_plot.getScalingOptions: T_scalingOptions;
  begin
    system.enterCriticalSection(cs);
    result:=scalingOptions;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_plot.zoomOnPoint(CONST pixelX, pixelY: longint;
  CONST factor: double; VAR plotImage: TImage);
  VAR rectA, rectB: TRect;
  begin with scalingOptions do begin
    system.enterCriticalSection(cs);
    try
      scalingOptions.axisTrafo['x'].zoom(pixelX,factor);
      scalingOptions.axisTrafo['y'].zoom(pixelY,factor);

      rectA.top:=0;
      rectA.Left:=0;
      rectA.Right:=plotImage.width;
      rectA.Bottom:=plotImage.height;

      rectB.top:=round((-pixelY)*factor+pixelY);
      rectB.Left:=round((-pixelX)*factor+pixelX);
      rectB.Right:=round((plotImage.width-pixelX)*factor+pixelX);
      rectB.Bottom:=round((plotImage.height-pixelY)*factor+pixelY);

      plotImage.Canvas.CopyRect(rectA, plotImage.Canvas, rectB);
    finally
      system.leaveCriticalSection(cs);
    end;
  end; end;

PROCEDURE T_plot.panByPixels(CONST pixelDX, pixelDY: longint;
  VAR plotImage: TImage);
  VAR rectA, rectB: TRect;
  begin with scalingOptions do begin
    system.enterCriticalSection(cs);
    try
      scalingOptions.axisTrafo['x'].pan(pixelDX);
      scalingOptions.axisTrafo['y'].pan(pixelDY);

      rectA.top:=0;
      rectA.Left:=0;
      rectA.Right:=plotImage.width;
      rectA.Bottom:=plotImage.height;
      rectB.top:=0+pixelDY;
      rectB.Left:=0+pixelDX;
      rectB.Right:=plotImage.width+pixelDX;
      rectB.Bottom:=plotImage.height+pixelDY;

      plotImage.Canvas.CopyRect(rectA, plotImage.Canvas, rectB);
    finally
      system.leaveCriticalSection(cs);
    end;
  end; end;

PROCEDURE T_plot.drawGridAndRows(CONST target: TBGRACanvas; VAR gridTic: T_ticInfos);
  VAR scaleAndColor:T_scaleAndColor;
      screenRow:T_rowToPaint;
      screenBox:T_boundingBox;

  PROCEDURE drawCustomQuad(CONST x0,y0,x1,y1,x2,y2,x3,y3:longint; CONST withBorder:boolean);
    VAR points:array[0..4] of TPoint;
    begin
      if (scaleAndColor.solidStyle=bsClear) and not(withBorder) then exit;
      target.Brush.BGRAColor:=scaleAndColor.solidColor;
      target.Brush.style:=scaleAndColor.solidStyle;
      points[0].x:=x0; points[0].y:=y0;
      points[1].x:=x1; points[1].y:=y1;
      points[2].x:=x2; points[2].y:=y2;
      points[3].x:=x3; points[3].y:=y3;
      points[4].x:=x0; points[4].y:=y0;
      if not(withBorder) then target.Pen.style:=psClear;
      target.Polygon(points);
      if not(withBorder) then target.Pen.style:=psSolid;
    end;

  VAR yBaseLine:longint;

  PROCEDURE drawPatternRect(CONST x0, y0, x1, y1: longint; CONST withBorder:boolean);
    begin
      drawCustomQuad(x0,y0,
                     x0,yBaseLine,
                     x1,yBaseLine,
                     x1,y1,withBorder);
    end;

  PROCEDURE drawStraightLines;
    VAR i:longint;
        last:TPoint=(x:0;y:0);
        lastWasValid:boolean;
    begin
      target.Pen.style:=psSolid;
      target.Pen.BGRAColor:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecRound;
      lastWasValid:=false;
      for i:=0 to length(screenRow)-1 do begin
        if screenRow[i].valid then begin
          if lastWasValid then begin
            target.LineTo  (screenRow[i].point);
            drawPatternRect(last.x, last.y, screenRow[i].point.x, screenRow[i].point.y,false);
          end else
            target.MoveTo(screenRow[i].point);
          last:=screenRow[i].point;
        end;
        lastWasValid:=screenRow[i].valid;
      end;
    end;

  PROCEDURE drawStepsLeft;
    VAR i:longint;
        last:TPoint=(x:0;y:0);
        lastWasValid:boolean;
    begin
      target.Pen.style:=psSolid;
      target.Pen.BGRAColor:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecRound;
      lastWasValid:=false;
      for i:=0 to length(screenRow)-1 do begin
        if screenRow[i].valid then begin
          if lastWasValid then begin
            target.LineTo(last.x, screenRow[i].point.y);
            target.LineTo(screenRow[i].point);
            drawPatternRect(last.x, screenRow[i].point.y, screenRow[i].point.x, screenRow[i].point.y,false);
          end else target.MoveTo(screenRow[i].point);
          last:=screenRow[i].point;
        end;
        lastWasValid:=screenRow[i].valid;
      end;
    end;

  PROCEDURE drawStepsRight;
    VAR i:longint;
        last:TPoint=(x:0;y:0);
        lastWasValid:boolean;
    begin
      target.Pen.style:=psSolid;
      target.Pen.BGRAColor:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecRound;
      lastWasValid:=false;
      for i:=0 to length(screenRow)-1 do begin
        if screenRow[i].valid then begin
          if lastWasValid then begin
            target.LineTo(screenRow[i].point.x, last.y);
            target.LineTo(screenRow[i].point);
            drawPatternRect(last.x, last.y, screenRow[i].point.x, last.y,false);
          end else target.MoveTo(screenRow[i].point);
          last:=screenRow[i].point;
        end;
        lastWasValid:=screenRow[i].valid;
      end;
    end;

  PROCEDURE drawBars;
    VAR i:longint;
        last:TPoint=(x:0;y:0);
        lastWasValid:boolean;
    begin
      target.Pen.style:=psSolid;
      target.Pen.BGRAColor:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecRound;
      lastWasValid:=false;
      for i:=0 to length(screenRow)-1 do begin
        if screenRow[i].valid then begin
          if lastWasValid then
            drawPatternRect(round(last.x*0.95+screenRow[i].point.x*0.05), last.Y,
                            round(last.x*0.05+screenRow[i].point.x*0.95), last.Y,scaleAndColor.lineWidth>0);
          last:=screenRow[i].point;
          lastWasValid:=screenRow[i].valid;
        end;
      end;
    end;

  PROCEDURE drawBoxes;
    VAR i:longint;
    begin
      if scaleAndColor.lineWidth<=0
      then target.Pen.style:=psClear
      else begin
        target.Pen.style:=psSolid;
        target.Pen.BGRAColor:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecRound;
      end;
      target.Brush.BGRAColor:=scaleAndColor.solidColor;
      target.Brush.style:=scaleAndColor.solidStyle;
      i:=0;
      while i+1<length(screenRow) do begin
        if screenRow[i  ].valid and
           screenRow[i+1].valid and
           intersect(screenBox,boundingBoxOf(screenRow[i].point.x, screenRow[i].point.y,screenRow[i+1].point.x, screenRow[i+1].point.y)) then begin
          if scaleAndColor.lineWidth<=0
          then target.FillRect (screenRow[i].point.x,screenRow[i].point.y,screenRow[i+1].point.x,screenRow[i+1].point.y)
          else target.Rectangle(screenRow[i].point.x,screenRow[i].point.y,screenRow[i+1].point.x,screenRow[i+1].point.y);
        end;
        inc(i, 2);
      end;
    end;

  PROCEDURE drawEllipses;
    PROCEDURE drawEllipse(CONST x0,y0,x1,y1:longint);
      VAR points:array[0..100] of TPoint;
          cx,cy,rx,ry:double;
          i:longint;
      begin
        if not(intersect(screenBox,boundingBoxOf(x0,y0,x1,y1))) or ((scaleAndColor.solidStyle=bsClear) and (scaleAndColor.lineWidth<1)) then exit;
        if (abs(x1-x0)>target.width) or (abs(y1-y0)>target.height) then begin
          cx:=(x0+x1)*0.5; rx:=(x1-x0)*0.5;
          cy:=(y0+y1)*0.5; ry:=(y1-y0)*0.5;
          for i:=0 to 100 do begin
            points[i].x:=round(cx+rx*cos(0.02*pi*i));
            points[i].y:=round(cy+ry*sin(0.02*pi*i));
          end;
          target.Polygon(points);
        end else begin
          target.Ellipse(x0,y0,x1,y1);
        end;
      end;
    VAR i:longint;
    begin
      target.Pen.style:=psSolid;
      target.Pen.BGRAColor:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecRound;
      if (scaleAndColor.lineWidth<=0) then target.Pen.style:=psClear;
      target.Brush.BGRAColor:=scaleAndColor.solidColor;
      target.Brush.style:=scaleAndColor.solidStyle;
      i:=0;
      while i+1<length(screenRow) do begin
        if screenRow[i  ].valid and
           screenRow[i+1].valid then
          drawEllipse(screenRow[i  ].point.x, screenRow[i  ].point.y,
                      screenRow[i+1].point.x, screenRow[i+1].point.y);
        inc(i, 2);
      end;
      if (scaleAndColor.lineWidth<=0) then target.Pen.style:=psSolid;
    end;

  PROCEDURE drawTubes;
    VAR i :longint=0;
        k :longint=0;
        polyBetween:array of TPoint;
        lastWasValid:boolean=false;
    begin
      target.Brush.BGRAColor:=scaleAndColor.solidColor;
      target.Brush.style:=scaleAndColor.solidStyle;
      target.Pen.style:=psClear;
      if scaleAndColor.solidStyle<>bsClear then begin
        setLength(polyBetween,length(screenRow));
        for i:=0 to length(screenRow)-1 do if screenRow[i].valid then begin
          polyBetween[k]:=screenRow[i].point;
          inc(k);
        end;
        setLength(polyBetween,k);
        target.Polygon(polyBetween);
      end;
      target.Pen.style:=psSolid;
      target.Pen.BGRAColor:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecRound;
      if scaleAndColor.lineWidth>0 then begin
        for i:=0 to length(screenRow)-1 do if screenRow[i].valid then begin
          if lastWasValid then target.LineTo(screenRow[i].point)
                          else target.MoveTo(screenRow[i].point);
          lastWasValid:=true;
        end else lastWasValid:=false;
      end;
    end;

  PROCEDURE drawPolygons;
    VAR points:array of TPoint;
    PROCEDURE screenRowPoly(CONST i0,i1:longint); inline;
      VAR i:longint;
      begin
        if (i0<0) or (i1<i0+1) then exit;
        if scaleAndColor.solidStyle=bsClear then begin
                             target.MoveTo(screenRow[i1].point);
          for i:=i0 to i1 do target.LineTo(screenRow[i].point);
        end else begin
          setLength(points,i1-i0+1);
          for i:=0 to i1-i0 do begin
            points[i].x:=screenRow[i0+i].point.x;
            points[i].y:=screenRow[i0+i].point.y;
          end;
          target.Polygon(points);
        end;
      end;

    VAR i,j:longint;
    begin
      if (scaleAndColor.solidStyle=bsClear) and (scaleAndColor.lineWidth<=0) then exit;
      target.Pen.style:=psSolid;
      target.Pen.BGRAColor:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecRound;
      if (scaleAndColor.lineWidth<=0) then target.Pen.style:=psClear;
      target.Brush.BGRAColor:=scaleAndColor.solidColor;
      target.Brush.style:=scaleAndColor.solidStyle;
      j:=-1;
      for i:=0 to length(screenRow)-1 do if not(screenRow[i].valid) then begin
        if j>=0 then screenRowPoly(j,i-1);
        j:=-1;
      end else if j<0 then j:=i;
      i:=length(screenRow)-1;
      if j>=0 then screenRowPoly(j,i);
      setLength(points,0);
      if (scaleAndColor.lineWidth<=0) then target.Pen.style:=psSolid;
    end;

  PROCEDURE drawPluses;
    VAR i:longint;
    begin
      target.Pen.style:=psSolid;
      target.Pen.BGRAColor:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecSquare;
      for i:=0 to length(screenRow)-1 do if screenRow[i].valid then begin
        target.MoveTo(screenRow[i].point.x-scaleAndColor.symbolWidth,
                      screenRow[i].point.y);
        target.LineTo(screenRow[i].point.x+scaleAndColor.symbolWidth,
                      screenRow[i].point.y);
        target.MoveTo(screenRow[i].point.x,
                      screenRow[i].point.y-scaleAndColor.symbolWidth);
        target.LineTo(screenRow[i].point.x,
                      screenRow[i].point.y+scaleAndColor.symbolWidth);
      end;
    end;

  PROCEDURE drawCrosses;
    VAR i:longint;
    begin
      target.Pen.style:=psSolid;
      target.Pen.BGRAColor:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecSquare;
      for i:=0 to length(screenRow)-1 do if screenRow[i].valid then begin
        target.MoveTo(screenRow[i].point.x-scaleAndColor.symbolRadius,
                      screenRow[i].point.y-scaleAndColor.symbolRadius);
        target.LineTo(screenRow[i].point.x+scaleAndColor.symbolRadius,
                      screenRow[i].point.y+scaleAndColor.symbolRadius);
        target.MoveTo(screenRow[i].point.x+scaleAndColor.symbolRadius,
                      screenRow[i].point.y-scaleAndColor.symbolRadius);
        target.LineTo(screenRow[i].point.x-scaleAndColor.symbolRadius,
                      screenRow[i].point.y+scaleAndColor.symbolRadius);
      end;
    end;

  PROCEDURE drawDots;
    VAR i:longint;
    begin
      target.Pen.style:=psClear;
      target.Brush.style:=bsSolid;
      target.Brush.BGRAColor:=scaleAndColor.solidColor;
      if scaleAndColor.symbolWidth>=1 then begin
        for i:=0 to length(screenRow)-1 do if screenRow[i].valid then
          target.Ellipse(screenRow[i].point.x-scaleAndColor.symbolWidth,
                         screenRow[i].point.y-scaleAndColor.symbolWidth,
                         screenRow[i].point.x+scaleAndColor.symbolWidth,
                         screenRow[i].point.y+scaleAndColor.symbolWidth);
      end else begin
        for i:=0 to length(screenRow)-1 do if screenRow[i].valid then
          target.Pixels[screenRow[i].point.x,
                        screenRow[i].point.y]:=scaleAndColor.lineColor;
      end;
    end;

  PROCEDURE drawImpulses;
    VAR i:longint;
    begin
      target.Pen.style     :=psSolid;
      target.Pen.BGRAColor:=scaleAndColor.lineColor;
      target.Pen.width    :=scaleAndColor.lineWidth;
      target.Pen.EndCap   :=pecSquare;
      for i:=0 to length(screenRow)-1 do if screenRow[i].valid then begin
        target.MoveTo(screenRow[i].point.x, yBaseLine     );
        target.LineTo(screenRow[i].point);
      end;
    end;

  VAR i,k:longint;
      currRow:T_sampleRow;
  begin
    screenBox:=boundingBoxOf(0,0,target.width,target.height);
    //Clear:------------------------------------------------------------------
    target.Brush.style:=bsSolid;
    target.Brush.color:=clWhite;
    target.Pen.style:=psClear;
    target.Pen.EndCap:=pecSquare;
    target.FillRect(0, 0, target.width, target.height);
    //------------------------------------------------------------------:Clear
    //coordinate grid:========================================================
    target.Pen.style:=psSolid;
    //minor grid:-------------------------------------------------------------
    scaleAndColor:=MINOR_TIC_STYLE.getLineScaleAndColor(target.width,target.height);
    target.Pen.BGRAColor:=scaleAndColor.lineColor;
    target.Pen.width:=scaleAndColor.lineWidth;
    if (gse_fineGrid in scalingOptions.axisStyle['y']) then
    for i:=0 to length(gridTic['y'])-1 do with gridTic['y'][i] do if not(major) then begin
      k:=round(pos);
      target.MoveTo(0           , k);
      target.LineTo(target.width, k);
    end;
    if (gse_fineGrid in scalingOptions.axisStyle['x']) then
    for i:=0 to length(gridTic['x'])-1 do with gridTic['x'][i] do if not(major) then begin
      k:=round(pos);
      target.MoveTo(k, 0);
      target.LineTo(k, target.height);
    end;
    //-------------------------------------------------------------:minor grid
    //major grid:-------------------------------------------------------------
    scaleAndColor:=MAJOR_TIC_STYLE.getLineScaleAndColor(target.width,target.height);
    target.Pen.BGRAColor:=scaleAndColor.lineColor;
    target.Pen.width:=scaleAndColor.lineWidth;
    if (gse_coarseGrid in scalingOptions.axisStyle['y']) then
    for i:=0 to length(gridTic['y'])-1 do with gridTic['y'][i] do if major then begin
      {$Q-}
      k:=round(pos);
      {$Q+}
      target.MoveTo(0, k);
      target.LineTo(target.width, k);
    end;
    if (gse_coarseGrid in scalingOptions.axisStyle['x']) then
    for i:=0 to length(gridTic['x'])-1 do with gridTic['x'][i] do if major then begin
      {$Q-}
      k:=round(pos);
      {$Q+}
      target.MoveTo(k, 0);
      target.LineTo(k, target.height);
    end;
    //-------------------------------------------------------------:major grid
    //========================================================:coordinate grid
    try
      if scalingOptions.axisTrafo['y'].logscale
      then yBaseLine:=scalingOptions.axisTrafo['y'].screenMin
      else yBaseLine:=round(scalingOptions.axisTrafo['y'].apply(0));
      if      yBaseLine<0 then yBaseLine:=0
      else if yBaseLine>=target.height then yBaseLine:=target.height-1;
    except
      yBaseLine:=0;
    end;
    //row data:===============================================================
    for currRow in row do begin
      screenRow:=scalingOptions.transformRow(currRow.sample,currRow.style.style);
      scaleAndColor:=currRow.style.getLineScaleAndColor(target.width,target.height);
      if ps_stepLeft  in currRow.style.style then drawStepsLeft;
      if ps_stepRight in currRow.style.style then drawStepsRight;
      if ps_bar       in currRow.style.style then drawBars;
      if ps_box       in currRow.style.style then drawBoxes;
      if ps_ellipse   in currRow.style.style then drawEllipses;
      if ps_tube      in currRow.style.style then drawTubes;
      if ps_polygon   in currRow.style.style then drawPolygons;
      if ps_dot       in currRow.style.style then drawDots;
      if ps_plus      in currRow.style.style then drawPluses;
      if ps_cross     in currRow.style.style then drawCrosses;
      if ps_impulse   in currRow.style.style then drawImpulses;
      if (ps_bspline   in currRow.style.style) or
         (ps_cosspline in currRow.style.style) or
         (ps_straight  in currRow.style.style) then drawStraightLines;
    end;
    //===============================================================:row data
  end;

PROCEDURE T_plot.drawCustomText(CONST target: TBGRACanvas);
  VAR txt:P_customText;
  begin
    for txt in customText do txt^.renderText(scalingOptions,target);
  end;

PROCEDURE T_plot.drawCoordSys(CONST target: TBGRACanvas; VAR gridTic: T_ticInfos);
  VAR i, x, y: longint;
      cSysX,cSysY:longint;
  begin
    enterCriticalSection(globalTextRenderingCs);
    try
      target.Font.height:=scalingOptions.absoluteFontSize(target.width,target.height);
      target.Font.color:=clBlack;
      cSysX:=scalingOptions.axisTrafo['x'].screenMin;
      cSysY:=scalingOptions.axisTrafo['y'].screenMin;
      //clear border:-----------------------------------------------------------
      target.Brush.style:=bsSolid;
      target.Brush.BGRAColor.FromRGB(255,255,255);
      target.Pen.style:=psClear;
      target.Pen.width:=1;
      target.Pen.EndCap:=pecSquare;
      target.FillRect(0,0,cSysX,target.height);
      target.FillRect(cSysX,cSysY,target.width,target.height);
      //-----------------------------------------------------------:clear border
      //coordinate system:======================================================
      //axis:-------------------------------------------------------------------
      target.Pen.style:=psSolid;
      target.Pen.BGRAColor.FromRGB(0,0,0);
      target.Pen.width:=1;
      if (scalingOptions.axisStyle['y']<>[]) then begin
        target.MoveTo(cSysX, 0);
        target.LineTo(cSysX, cSysY);
      end;
      if (scalingOptions.axisStyle['x']<>[]) then begin
        target.MoveTo(target.width, cSysY);
        target.LineTo(cSysX        , cSysY);
      end;
      //-------------------------------------------------------------------:axis
      //tics:-------------------------------------------------------------------
      if (gse_tics in scalingOptions.axisStyle['y']) then for i:=0 to length(gridTic['y'])-1 do with gridTic['y'][i] do if major then begin
        y:=round(pos);
        target.MoveTo(cSysX-5, y);
        target.LineTo(cSysX  , y);
        target.textOut(cSysX-5-target.textWidth(txt),
                            y-target.textHeight(txt) shr 1, txt);
      end;
      if (gse_tics in scalingOptions.axisStyle['x']) then for i:=0 to length(gridTic['x'])-1 do with gridTic['x'][i] do if major then begin
        x:=round(pos);
        target.MoveTo(x, cSysY+5);
        target.LineTo(x, cSysY);
        target.textOut(x-target.textWidth(txt) shr 1, cSysY+5, txt);
      end;
      //-------------------------------------------------------------------:tics
      //======================================================:coordinate system
    finally
      leaveCriticalSection(globalTextRenderingCs);
    end;
  end;

PROCEDURE T_plot.renderPlot(VAR plotImage: TImage);
  VAR gridTics:T_ticInfos;

  VAR bgrabmp:TBGRABitmap;
  begin
    if (plotImage.width<5) or (plotImage.height<5) then exit;
    system.enterCriticalSection(cs);
    try
      if (length(row)=0) and (length(customText)=0) then begin
        plotImage.Canvas.clear;
        plotImage.Canvas.Brush.style:=bsSolid;
        plotImage.Canvas.Brush.color:=clWhite;
        plotImage.Canvas.Rectangle(0,0,plotImage.width,plotImage.height);
        plotImage.Canvas.Pen.color:=clRed;
        plotImage.Canvas.line(0,0,plotImage.width,plotImage.height);
        plotImage.Canvas.line(0,plotImage.height,plotImage.width,0);
      end else begin
        bgrabmp:=TBGRABitmap.create(plotImage.width,plotImage.height,BGRAWhite);
        initialize(gridTics);
        scalingOptions.updateForPlot(bgrabmp.CanvasBGRA,row,gridTics);
        drawGridAndRows             (bgrabmp.CanvasBGRA,gridTics);
        drawCoordSys                (bgrabmp.CanvasBGRA,gridTics);
        drawCustomText              (bgrabmp.CanvasBGRA);
        plotImage.Canvas.clear;
        bgrabmp.draw(plotImage.Canvas,0,0,false);
        bgrabmp.free;
      end;
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_plot.obtainPlot(CONST width, height: longint): TImage;
  begin
    result:=TImage.create(nil);
    result.SetInitialBounds(0,0,width,height);
    renderPlot(result);
  end;

PROCEDURE T_plot.renderToFile(CONST fileName: string; CONST width, height:longint);
  VAR storeImage:TImage;
  begin
    storeImage:=obtainPlot(width,height);
    storeImage.picture.PNG.saveToFile(ChangeFileExt(fileName, '.png'));
    enterCriticalSection(globalTextRenderingCs);
    storeImage.destroy;
    leaveCriticalSection(globalTextRenderingCs);
  end;

FUNCTION T_plot.renderToString(CONST width, height: longint): ansistring;
  VAR storeImage: TImage;
      memStream: TStringStream;
  begin
    storeImage:=obtainPlot(width,height);
    memStream := TStringStream.create('');
    storeImage.picture.PNG.saveToStream(memStream);
    memStream.position:=0;
    result:=memStream.DataString;
    memStream.free;
    enterCriticalSection(globalTextRenderingCs);
    storeImage.destroy;
    leaveCriticalSection(globalTextRenderingCs);
  end;

PROCEDURE T_plot.copyFrom(VAR p: T_plot);
  VAR i:longint;
      clonedSample:T_dataRow;
  begin
    system.enterCriticalSection(cs);
    system.enterCriticalSection(p.cs);
    try
      scalingOptions:=p.scalingOptions;
      //copy rows:
      for i:=0 to length(row)-1 do row[i].destroy;
      setLength(row,length(p.row));
      for i:=0 to length(row)-1 do begin
        p.row[i].sample.cloneTo(clonedSample);
        row[i].create(clonedSample);
        row[i].style:=p.row[i].style;
      end;
      //:copy rows | copy custom text:
      for i:=0 to length(customText)-1 do dispose(customText[i],destroy);
      setLength(customText,length(p.customText));
      for i:=0 to length(customText)-1 do customText[i]:=p.customText[i]^.clone;
    finally
      system.leaveCriticalSection(p.cs);
      system.leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_plot.getRowStatements(CONST prevOptions:T_scalingOptions; VAR globalRowData:T_listLiteral; CONST haltExport:PBoolean; CONST Application:Tapplication; CONST progress:TProgressBar):T_arrayOfString;
  VAR opt:string;
      i:longint;
  begin
    if haltExport^ then exit;
    system.enterCriticalSection(cs);
    try
      opt:=scalingOptions.getOptionDiffString(prevOptions);
      Application.ProcessMessages;
      if opt='' then setLength(result,0) else result:=opt;
      for i:=0 to length(row)-1 do if not(haltExport^) then begin
        append(result,row[i].toPlotStatement(i=0,globalRowData));
        progress.position:=progress.position+1;
      end;
      Application.ProcessMessages;
      for i:=0 to length(customText)-1 do if not(haltExport^) then begin
        append(result,customText[i]^.toTextStatement);
        progress.position:=progress.position+1;
      end;
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_plot.getRowStatementCount:longint;
  begin
    result:=length(row)+length(customText);
  end;

PROCEDURE T_plotSystem.processMessage(CONST message: P_storedMessage);
  VAR clonedRow:T_dataRow;
  begin
    case message^.messageType of
      mt_startOfEvaluation: begin
        if settings.doResetPlotOnEvaluation or sandboxed
        then currentPlot.setDefaults
        else currentPlot.clear;
        animation.clear;
        if pullSettingsToGui<>nil then pullSettingsToGui();
        animation.weHadAMemoryPanic:=false;
      end;
      mt_plot_addText:
        currentPlot.addCustomText(P_addTextMessage(message)^.customText);
      mt_plot_addRow : with P_addRowMessage(message)^ do begin
        if used then begin
          rowData.cloneTo(clonedRow);
          currentPlot.addRow(styleOptions,clonedRow);
        end else currentPlot.addRow(styleOptions,rowData);
        used:=true;
        styleOptions:='';
      end;
      mt_plot_dropRow:
        if P_plotDropRowRequest(message)^.dropRows
        then currentPlot.removeRows      (P_plotDropRowRequest(message)^.count)
        else currentPlot.removeCustomText(P_plotDropRowRequest(message)^.count);
      mt_plot_renderRequest: begin
        with P_plotRenderRequest(message)^ do if isRenderToStringRequest
        then setString(currentPlot.renderToString(width,height))
        else currentPlot.renderToFile(fileName,   width,height);
        P_plotRenderRequest(message)^.fileName:='';
      end;
      mt_plot_retrieveOptions:
        P_plotOptionsMessage(message)^.setOptions(currentPlot.scalingOptions);
      mt_plot_setOptions:
        currentPlot.scalingOptions.modifyOptions(P_plotOptionsMessage(message)^.options,P_plotOptionsMessage(message)^.modified);
      mt_plot_clear:
        currentPlot.clear;
      mt_plot_clearAnimation,
      mt_plot_clearAnimationVolatile:
        animation.clear(message^.messageType=mt_plot_clearAnimationVolatile);
      mt_plot_addAnimationFrame: begin
        displayImmediate:=true;
        animation.addFrame(currentPlot);
        if animation.volatile
        then P_plotAddAnimationFrameRequest(message)^.markExecuted((animation.frameCount-VOLATILE_FRAMES_MAX)/20)
        else P_plotAddAnimationFrameRequest(message)^.markExecuted(0);
      end;
      mt_plot_postDisplay:       begin
        if doPlot<>nil then doPlot();
        displayImmediate:=true;
        P_plotDisplayRequest(message)^.markExecuted;
      end;
      mt_endOfEvaluation: begin
        if plotChangedSinceLastDisplay and (doPlot<>nil) then doPlot();
        displayImmediate:=false;
      end;
    end;
    plotChangedSinceLastDisplay:=plotChangedSinceLastDisplay or
      (message^.messageType in [mt_plot_addText,mt_plot_addRow,mt_plot_dropRow,mt_plot_setOptions,mt_plot_clear,mt_plot_addAnimationFrame]);
  end;

PROCEDURE T_plotSystem.startGuiInteraction;
  begin
    enterCriticalSection(adapterCs);
  end;

PROCEDURE T_plotSystem.doneGuiInteraction;
  begin
    leaveCriticalSection(adapterCs);
  end;

FUNCTION T_plotSystem.isEmpty:boolean;
  begin
    result:=(length(currentPlot.row)=0) and (length(currentPlot.customText)=0) and (animation.frameCount=0);
  end;

FUNCTION T_plotSystem.getPlotStatement(CONST frameIndexOrNegativeIfAll:longint; CONST haltExport:PBoolean; CONST Application:Tapplication; CONST progress:TProgressBar):T_arrayOfString;
  VAR prevOptions:T_scalingOptions;
      i,dsOffset:longint;
      stepsTotal:longint=0;
      globalRowData:P_listLiteral;
      dummyLocation:T_tokenLocation=(package:nil;line:0;column:0);
      commands:T_arrayOfString;
      DataString:string;
  begin
    enterCriticalSection(adapterCs);
    try
      globalRowData:=newListLiteral();
      result:='#!'+settings.fullFlavourLocation+' '+FLAG_GUI;
      myGenerics.append(result,'plain script;');

      commands:='resetOptions;';
      myGenerics.append(commands,'clearAnimation;');
      prevOptions.setDefaults;
      if animation.frameCount>0 then begin
        if frameIndexOrNegativeIfAll<0 then begin
          for i:=0 to length(animation.frame)-1 do stepsTotal+=animation.frame[i]^.plotData.getRowStatementCount;
          progress.max:=stepsTotal*2;
          progress.position:=0;
          Application.ProcessMessages;
          for i:=0 to length(animation.frame)-1 do begin
            myGenerics.append(commands,animation.frame[i]^.plotData.getRowStatements(prevOptions,globalRowData^,haltExport,Application,progress));
            prevOptions:=animation.frame[i]^.plotData.scalingOptions;
            myGenerics.append(commands,'addAnimationFrame;');
          end;
        end else begin
          progress.max:=animation.frame[frameIndexOrNegativeIfAll]^.plotData.getRowStatementCount*2;
          Application.ProcessMessages;
          myGenerics.append(commands,animation.frame[frameIndexOrNegativeIfAll]^.plotData.getRowStatements(prevOptions,globalRowData^,haltExport,Application,progress));
        end;
      end else begin
        progress.max:=currentPlot.getRowStatementCount*2;
        Application.ProcessMessages;
        myGenerics.append(commands,currentPlot.getRowStatements(prevOptions,globalRowData^,haltExport,Application,progress));
      end;
      DataString:=base92Encode(
                   compressString(
                     serialize(globalRowData,
                               dummyLocation,
                               nil),
                     [C_compression_gzip]));
      myGenerics.append(result,'ROW:=//!~'+copy(DataString,1,151));
      dsOffset:=152;
      progress.max:=(length(DataString) div 160)*2;
      progress.position:=progress.max shr 1;
      while (length(DataString)>dsOffset) and not(haltExport^) do begin
        Application.ProcessMessages;
        myGenerics.append(result,copy(DataString,dsOffset,160));
        inc(dsOffset,160);
        progress.position:=progress.position+1;
      end;
      result[length(result)-1]+='~';
      if length(result[length(result)-1])<151
      then result[length(result)-1]+='.base92decode'
      else myGenerics.append(result, '.base92decode');
      if length(result[length(result)-1])<153
      then result[length(result)-1]+='.decompress'
      else myGenerics.append(result, '.decompress');
      if length(result[length(result)-1])<151
      then result[length(result)-1]+='.deserialize;'
      else myGenerics.append(result, '.deserialize;');
      myGenerics.append(result,commands);
      myGenerics.append(result,'display;');
      setLength(commands,0);
    finally
      disposeLiteral(globalRowData);
      leaveCriticalSection(adapterCs);
    end;
  end;

CONSTRUCTOR T_plotSystem.create(CONST executePlotCallback:F_execPlotCallback; CONST isSandboxSystem:boolean);
  begin
    if executePlotCallback=nil
    then inherited create(at_plot,C_includableMessages[at_plot]-[mt_plot_queryClosedByUser,mt_plot_addAnimationFrame,mt_plot_clearAnimation,mt_plot_clearAnimationVolatile,mt_plot_postDisplay])
    else inherited create(at_plot,C_includableMessages[at_plot]);
    sandboxed:=isSandboxSystem;
    plotChangedSinceLastDisplay:=false;
    currentPlot.createWithDefaults;
    doPlot:=executePlotCallback;
    pullSettingsToGui:=nil;
    animation.create;
  end;

FUNCTION newPlotSystemWithoutDisplay:P_plotSystem;
  begin
    new(result,create(nil,true));
  end;

DESTRUCTOR T_plotSystem.destroy;
  begin
    inherited destroy;
    currentPlot.destroy;
    animation.destroy;
  end;

FUNCTION T_plotSystem.append(CONST message: P_storedMessage): boolean;
  begin
    if not(message^.messageType in messageTypesToInclude) then exit(false);
    enterCriticalSection(adapterCs);
    try
      case message^.messageType of
        mt_startOfEvaluation,
        mt_plot_addText,
        mt_plot_addRow,
        mt_plot_dropRow,
        mt_plot_setOptions,
        mt_plot_clear,
        mt_plot_clearAnimation,
        mt_plot_clearAnimationVolatile,
        mt_plot_retrieveOptions,
        mt_plot_renderRequest,
        mt_plot_queryClosedByUser: begin
          result:=true;
          //if there are pending tasks then store else process
          if (collectedFill>0)
          then inherited append(message)
          else processMessage(message);
        end;
        mt_plot_addAnimationFrame: begin
          if not(animation.volatile) then P_plotAddAnimationFrameRequest(message)^.markExecuted(0);
          result:=true;
          //if there are pending tasks then store else process
          if (collectedFill>0)
          then inherited append(message)
          else processMessage(message);
        end;
        mt_endOfEvaluation,
        mt_plot_postDisplay: begin
          //if we can't plot anyway, we can process the message right away
          result:=true;
          if doPlot=nil
          then processMessage(message)
          else inherited append(message);
        end;
        else result:=false;
      end;
    finally
      leaveCriticalSection(adapterCs);
    end;
  end;

FUNCTION T_plotSystem.flushToGui(CONST forceFlush:boolean):T_messageTypeSet;
  VAR lastDisplayIndex:longint;
      i:longint;
      m:P_storedMessage;
  begin
    enterCriticalSection(adapterCs);
    try
      result:=[];
      //it does not make sense to render multiple plots in one run
      //Lookup the last display request;
      lastDisplayIndex:=-1;
      for i:=0 to collectedFill-1 do if collected[i]^.messageType=mt_plot_postDisplay then lastDisplayIndex:=i;
      //process messages
      for i:=0 to collectedFill-1 do begin
        m:=collected[i];
        include(result,m^.messageType);
        if m^.messageType=mt_plot_postDisplay
        then begin
          if i=lastDisplayIndex
          then processMessage(m)
          else P_plotDisplayRequest(m)^.markExecuted;
        end else processMessage(m);
      end;
      clear;
    finally
      leaveCriticalSection(adapterCs);
    end;
  end;

PROCEDURE T_plotSystem.logPlotDone;
  begin
    enterCriticalSection(adapterCs);
    try
      plotChangedSinceLastDisplay:=false;
    finally
      leaveCriticalSection(adapterCs);
    end;
  end;

INITIALIZATION
  MAJOR_TIC_STYLE.init; MAJOR_TIC_STYLE.styleModifier:=0.2; MAJOR_TIC_STYLE.defaults:=[];
  MINOR_TIC_STYLE.init; MINOR_TIC_STYLE.styleModifier:=0.1; MAJOR_TIC_STYLE.defaults:=[];
end.
