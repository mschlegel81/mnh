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
     EpikTimer;
TYPE
  T_plotQuality=0..3;
CONST
  PLOT_QUALITY_LOW     =0;
  PLOT_QUALITY_MEDIUM_1=1;
  PLOT_QUALITY_MEDIUM_2=2;
  PLOT_QUALITY_HIGH    =3;
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
      width,height,quality:longint;

      retrieved:boolean;
      outputString:string;
    protected
      FUNCTION internalType:shortstring; virtual;
    public
      CONSTRUCTOR createRenderToFileRequest  (CONST filename_:string; CONST width_,height_,quality_:longint);
      CONSTRUCTOR createRenderToStringRequest(CONST width_,height_,quality_:longint);
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

  P_plot =^T_plot;
  T_plot = object
    private
      cs: TRTLCriticalSection;
      scalingOptions:T_scalingOptions;
      row: array of T_sampleRow;
      customText:array of P_customText;
      transparentCount:longint;
      PROCEDURE setScalingOptions(CONST value:T_scalingOptions);
      FUNCTION  getScalingOptions:T_scalingOptions;
      PROCEDURE drawGridAndRows(CONST target: TCanvas; CONST intendedWidth,intendedHeight,scalingFactor:longint; VAR gridTic: T_ticInfos; CONST sampleIndex:byte);
      PROCEDURE drawCoordSys(CONST target: TCanvas; CONST intendedWidth,intendedHeight:longint; VAR gridTic: T_ticInfos);
      PROCEDURE drawCustomText(CONST target: TCanvas; CONST intendedWidth,intendedHeight:longint);
      FUNCTION  obtainPlot(CONST width,height:longint; CONST quality:T_plotQuality):TImage;

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

      PROCEDURE renderPlot(VAR plotImage: TImage; CONST quality:T_plotQuality);
      PROCEDURE renderToFile(CONST fileName:string; CONST width,height,supersampling:longint);
      FUNCTION renderToString(CONST width,height,supersampling:longint):ansistring;

      PROCEDURE copyFrom(VAR p:T_plot);
      FUNCTION getRowStatements(CONST prevOptions:T_scalingOptions; VAR globalRowData:T_listLiteral; CONST haltExport:PBoolean; CONST Application:Tapplication; CONST progress:TProgressBar):T_arrayOfString;
      FUNCTION getRowStatementCount:longint;
  end;

  T_frameCacheMode=(fcm_none,fcm_retainImage,fcm_inMemoryPng,fcm_tempFile);

  P_plotSeriesFrame=^T_plotSeriesFrame;
  T_plotSeriesFrame=object
    private
      backgroundPreparation:boolean;
      frameCS:TRTLCriticalSection;

      cachedImage:record
        image:TImage;
        dumpIsUpToDate:boolean;
        dumpName:string;
        inMemoryDump:TMemoryStream;
        renderedWidth  ,
        renderedHeight :longint;
        renderedQuality:byte;
      end;

      postedWidth  ,
      postedHeight :longint;
      postedQuality:byte;
      plotData:T_plot;
      PROCEDURE performPostedPreparation;
    public
      CONSTRUCTOR create(VAR currentPlot:T_plot);
      DESTRUCTOR destroy;
      PROCEDURE doneImage(CONST cacheMode:T_frameCacheMode);
      PROCEDURE obtainImage(VAR target:TImage; CONST quality:byte; CONST timing:T_timedPlotExecution);
      PROCEDURE prepareImage(CONST width,height:longint; CONST quality:byte);
      PROCEDURE postPreparation(CONST width,height:longint; CONST quality:byte);
  end;

  T_plotSeries=object
    private
      frame:array of P_plotSeriesFrame;
      framesWithImagesAllocated:array[0..7] of P_plotSeriesFrame;
      seriesCs:TRTLCriticalSection;
      weHadAMemoryPanic:boolean;
      FUNCTION getOptions(CONST index:longint):T_scalingOptions;
      PROCEDURE setOptions(CONST index:longint; CONST value:T_scalingOptions);
      PROCEDURE flushFramesToDisk;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE clear;
      FUNCTION frameCount:longint;
      PROCEDURE getFrame(VAR target:TImage; CONST frameIndex:longint; CONST quality:byte; CONST timing:T_timedPlotExecution);
      PROCEDURE renderFrame(CONST index:longint; CONST fileName:string; CONST width,height,quality:longint; CONST exportingAll:boolean);
      PROCEDURE addFrame(VAR plot:T_plot);
      FUNCTION nextFrame(VAR frameIndex:longint; CONST cycle:boolean; CONST width,height,quality:longint):boolean;
      PROPERTY options[index:longint]:T_scalingOptions read getOptions write setOptions;
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
     myStringUtil;
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

PROCEDURE T_timedPlotExecution.wait;
  begin
    if timer=nil then exit;
    while timer.elapsed<timeout do sleep(round(900*(timeout-timer.elapsed)));
  end;

CONSTRUCTOR T_plotSeriesFrame.create(VAR currentPlot: T_plot);
  begin
    initCriticalSection(frameCS);
    enterCriticalSection(frameCS);
    with cachedImage do begin
      image:=nil;
      dumpIsUpToDate:=false;
      dumpName:='';
      inMemoryDump:=nil;
      renderedWidth :=-1;
      renderedHeight:=-1;
      renderedQuality:=255;
    end;
    backgroundPreparation:=false;
    plotData.createWithDefaults;
    plotData.copyFrom(currentPlot);
    leaveCriticalSection(frameCS);
  end;

DESTRUCTOR T_plotSeriesFrame.destroy;
  begin
    enterCriticalSection(frameCS);
    try
      doneImage(fcm_none);
      if cachedImage.dumpName<>'' then DeleteFile(cachedImage.dumpName);
      plotData.destroy;
    finally
      leaveCriticalSection(frameCS);
      doneCriticalSection(frameCS);
    end;
  end;

PROCEDURE T_plotSeriesFrame.doneImage(CONST cacheMode:T_frameCacheMode);
  begin
    enterCriticalSection(frameCS);
    try
      with cachedImage do case cacheMode of
        fcm_none       : begin
          //Don't retain; clear all data and mark as invalid
          renderedHeight:=-1;
          renderedWidth :=-1;
          renderedQuality:=255;
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
      leaveCriticalSection(frameCS);
    end;
  end;

PROCEDURE T_plotSeriesFrame.prepareImage(CONST width,height:longint; CONST quality:byte);
  VAR imageIsPrepared:boolean=false;
  begin
    enterCriticalSection(frameCS);
    try
      if (cachedImage.renderedQuality=quality) and
         (cachedImage.renderedWidth  =width  ) and
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
        cachedImage.image:=plotData.obtainPlot(width,height,quality);
        cachedImage.dumpIsUpToDate:=false;
        cachedImage.renderedQuality:=quality;
        cachedImage.renderedHeight :=height;
        cachedImage.renderedWidth  :=width;
      end;
    finally
      leaveCriticalSection(frameCS);
    end;
  end;

PROCEDURE T_plotSeriesFrame.performPostedPreparation;
  begin
    enterCriticalSection(frameCS);
    try
      prepareImage(postedWidth,postedHeight,postedQuality);
    finally
      backgroundPreparation:=false;
      leaveCriticalSection(frameCS);
    end;
  end;

FUNCTION preparationThread(p:pointer):ptrint;
  begin
    P_plotSeriesFrame(p)^.performPostedPreparation;
    result:=0
  end;

PROCEDURE T_plotSeriesFrame.postPreparation(CONST width,height:longint; CONST quality:byte);
  begin
    if backgroundPreparation then exit;
    enterCriticalSection(frameCS);
    if backgroundPreparation then begin
      leaveCriticalSection(frameCS);
      exit;
    end;
    try
      backgroundPreparation:=true;
      postedHeight:=height;
      postedWidth:=width;
      postedQuality:=quality;
      beginThread(@preparationThread,@self);
    finally
      leaveCriticalSection(frameCS);
    end;
  end;

PROCEDURE T_plotSeriesFrame.obtainImage(VAR target: TImage; CONST quality: byte; CONST timing:T_timedPlotExecution);
  begin
    enterCriticalSection(frameCS);
    try
      prepareImage(target.width,target.height,quality);
      timing.wait;
      target.Canvas.draw(0,0,cachedImage.image.picture.Bitmap);
    finally
      leaveCriticalSection(frameCS);
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

CONSTRUCTOR T_plotRenderRequest.createRenderToFileRequest(CONST filename_: string; CONST width_, height_, quality_: longint);
  begin
    inherited create(mt_plot_renderRequest);
    targetIsString:=false;
    fileName:=filename_;
    width:=width_;
    height:=height_;
    quality:=quality_;
    retrieved:=false;
    outputString:='';
  end;

CONSTRUCTOR T_plotRenderRequest.createRenderToStringRequest(CONST width_,height_, quality_: longint);
  begin
    inherited create(mt_plot_renderRequest);
    targetIsString:=true;
    fileName:='';
    width:=width_;
    height:=height_;
    quality:=quality_;
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

PROCEDURE T_plotSeries.clear;
  VAR k:longint;
  begin
    enterCriticalSection(seriesCs);
    try
      for k:=0 to length(frame)-1 do dispose(frame[k],destroy);
      setLength(frame,0);
      for k:=0 to length(framesWithImagesAllocated)-1 do framesWithImagesAllocated[k]:=nil;
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

PROCEDURE T_plotSeries.getFrame(VAR target: TImage; CONST frameIndex: longint; CONST quality: byte; CONST timing:T_timedPlotExecution);
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
      //deallocate the last one, dump to file
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
      handleImagesToFree;
      current^.obtainImage(target,quality,timing);
    finally
      leaveCriticalSection(seriesCs);
    end;
  end;

PROCEDURE T_plotSeries.renderFrame(CONST index:longint; CONST fileName:string; CONST width,height,quality:longint; CONST exportingAll:boolean);
  VAR storeImage:TImage;
  begin
    enterCriticalSection(seriesCs);
    if (index<0) or (index>=length(frame)) then begin
      leaveCriticalSection(seriesCs);
      exit;
    end;
    try
      {$ifndef unix}
      if exportingAll and (index+settings.cpuCount-1<length(frame)) then frame[index+settings.cpuCount-1]^.postPreparation(width,height,quality);
      {$endif}

      storeImage:=TImage.create(nil);
      storeImage.SetInitialBounds(0,0,width,height);
      frame[index]^.obtainImage(storeImage,quality,timedPlotExecution(nil,0));
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
  VAR newIdx:longint;
  begin
    enterCriticalSection(seriesCs);
    try
      newIdx:=length(frame);
      setLength(frame,newIdx+1);
      new(frame[newIdx],create(plot));
    finally
      leaveCriticalSection(seriesCs);
    end;
  end;

FUNCTION T_plotSeries.nextFrame(VAR frameIndex: longint; CONST cycle:boolean; CONST width,height,quality:longint):boolean;
  {$ifndef unix}VAR nextToPrepare:longint;{$endif}
  begin
    enterCriticalSection(seriesCs);
    try
      if length(frame)=0 then begin
        frameIndex:=-1;
        result:=false;
      end else begin
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
          nextToPrepare:=frameIndex+(settings.cpuCount div 2);
          if nextToPrepare=frameIndex then inc(nextToPrepare);
          if cycle then nextToPrepare:=nextToPrepare mod length(frame);
          if (nextToPrepare>=0) and (nextToPrepare<length(frame))
          then frame[nextToPrepare]^.postPreparation(width,height,quality);
        end;
        {$endif}
      end;
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
      for i:=0 to length(customText)-1 do dispose(customText[i],destroy);
      setLength(customText,0);
      transparentCount:=0;
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

PROCEDURE T_plot.addRow(CONST styleOptions: string; CONST rowData: T_dataRow);
  VAR index:longint;
  begin
    system.enterCriticalSection(cs);
    try
      index:=length(row);
      setLength(row, index+1);
      row[index].create(rowData);
      row[index].style:=getStyle(index,styleOptions,transparentCount);
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
      i0:=length(row)-numberOfRowsToRemove; if i0<0 then i0:=0;
      for i:=i0 to length(row)-1 do row[i].destroy;
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

PROCEDURE T_plot.drawGridAndRows(CONST target: TCanvas; CONST intendedWidth, intendedHeight, scalingFactor: longint; VAR gridTic: T_ticInfos; CONST sampleIndex: byte);
  CONST darts_delta:array[0..4,0..1] of single=(( 0.12, 0.24),
                                                (-0.12,-0.24),
                                                ( 0.24,-0.12),
                                                (-0.24, 0.12),
                                                (0,0));
  VAR rowId, i, yBaseLine:longint;
      lastX: longint = 0;
      lastY: longint = 0;
      lastWasValid: boolean;
      scaleAndColor:T_scaleAndColor;
      screenRow:T_rowToPaint;
      screenBox:T_boundingBox;

  PROCEDURE drawCustomQuad(CONST x0,y0,x1,y1,x2,y2,x3,y3:longint; CONST withBorder:boolean);
    VAR points:array[0..4] of TPoint;
    begin
      if (scaleAndColor.solidStyle=bsClear) and not(withBorder) then exit;
      points[0].x:=x0; points[0].y:=y0;
      points[1].x:=x1; points[1].y:=y1;
      points[2].x:=x2; points[2].y:=y2;
      points[3].x:=x3; points[3].y:=y3;
      points[4].x:=x0; points[4].y:=y0;
      enterCriticalSection(globalTextRenderingCs);
      if not(withBorder) then target.Pen.style:=psClear;
      target.Brush.color:=scaleAndColor.solidColor;
      target.Brush.style:=scaleAndColor.solidStyle;
      leaveCriticalSection(globalTextRenderingCs);
      target.Polygon(points);
      enterCriticalSection(globalTextRenderingCs);
      if not(withBorder) then target.Pen.style:=psSolid;
      leaveCriticalSection(globalTextRenderingCs);
    end;

  PROCEDURE drawPatternRect(CONST x0, y0, x1, y1: longint; CONST withBorder:boolean);
    begin
      drawCustomQuad(x0,y0,
                     x0,yBaseLine,
                     x1,yBaseLine,
                     x1,y1,withBorder);
    end;

  PROCEDURE drawStraightLines;
    VAR i:longint;
    begin
      enterCriticalSection(globalTextRenderingCs);
      target.Pen.style:=psSolid;
      target.Pen.color:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecRound;
      leaveCriticalSection(globalTextRenderingCs);
      lastWasValid:=false;
      for i:=0 to length(screenRow)-1 do begin
        if screenRow[i].valid then begin
          if lastWasValid then begin
            target.LineTo(screenRow[i].x, screenRow[i].y);
            drawPatternRect(lastX, lastY, screenRow[i].x, screenRow[i].y,false);
          end else
            target.MoveTo(screenRow[i].x, screenRow[i].y);
          lastX:=screenRow[i].x;
          lastY:=screenRow[i].y;
        end;
        lastWasValid:=screenRow[i].valid;
      end;
    end;

  PROCEDURE drawStepsLeft;
    VAR i:longint;
    begin
      enterCriticalSection(globalTextRenderingCs);
      target.Pen.style:=psSolid;
      target.Pen.color:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecRound;
      leaveCriticalSection(globalTextRenderingCs);
      lastWasValid:=false;
      for i:=0 to length(screenRow)-1 do begin
        if screenRow[i].valid then begin
          if lastWasValid then begin
            target.LineTo(lastX, screenRow[i].y);
            target.LineTo(screenRow[i].x, screenRow[i].y);
            drawPatternRect(lastX, screenRow[i].y, screenRow[i].x, screenRow[i].y,false);
          end else target.MoveTo(screenRow[i].x, screenRow[i].y);
          lastX:=screenRow[i].x;
          lastY:=screenRow[i].y;
        end;
        lastWasValid:=screenRow[i].valid;
      end;
    end;

  PROCEDURE drawStepsRight;
    VAR i:longint;
    begin
      enterCriticalSection(globalTextRenderingCs);
      target.Pen.style:=psSolid;
      target.Pen.color:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecRound;
      leaveCriticalSection(globalTextRenderingCs);
      lastWasValid:=false;
      for i:=0 to length(screenRow)-1 do begin
        if screenRow[i].valid then begin
          if lastWasValid then begin
            target.LineTo(screenRow[i].x, lastY);
            target.LineTo(screenRow[i].x, screenRow[i].y);
            drawPatternRect(lastX, lastY, screenRow[i].x, lastY,false);
          end else target.MoveTo(screenRow[i].x, screenRow[i].y);
          lastX:=screenRow[i].x;
          lastY:=screenRow[i].y;
        end;
        lastWasValid:=screenRow[i].valid;
      end;
    end;

  PROCEDURE drawBars;
    VAR i:longint;
    begin
      enterCriticalSection(globalTextRenderingCs);
      target.Pen.style:=psSolid;
      target.Pen.color:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecRound;
      leaveCriticalSection(globalTextRenderingCs);
      lastWasValid:=false;
      for i:=0 to length(screenRow)-1 do begin
        if screenRow[i].valid then begin
          if lastWasValid then
            drawPatternRect(round(lastX*0.95+screenRow[i].x*0.05), lastY,
                            round(lastX*0.05+screenRow[i].x*0.95), lastY,scaleAndColor.lineWidth>0);
          lastX:=screenRow[i].x;
          lastY:=screenRow[i].y;
          lastWasValid:=screenRow[i].valid;
        end;
      end;
    end;

  PROCEDURE drawBoxes;
    VAR i:longint;
    begin
      enterCriticalSection(globalTextRenderingCs);
      target.Pen.style:=psSolid;
      target.Pen.color:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecRound;
      leaveCriticalSection(globalTextRenderingCs);
      lastWasValid:=false;
      i:=0;
      while i+1<length(screenRow) do begin
        if screenRow[i  ].valid and
           screenRow[i+1].valid and
           intersect(screenBox,boundingBoxOf(screenRow[i].x, screenRow[i].y,screenRow[i+1].x, screenRow[i+1].y)) then begin
          drawCustomQuad(screenRow[i  ].x, screenRow[i  ].y,
                         screenRow[i  ].x, screenRow[i+1].y,
                         screenRow[i+1].x, screenRow[i+1].y,
                         screenRow[i+1].x, screenRow[i  ].y,
                         scaleAndColor.lineWidth>0);
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
        enterCriticalSection(globalTextRenderingCs);
        if (scaleAndColor.lineWidth<=0) then target.Pen.style:=psClear;
        target.Brush.color:=scaleAndColor.solidColor;
        target.Brush.style:=scaleAndColor.solidStyle;
        leaveCriticalSection(globalTextRenderingCs);
        if (abs(x1-x0)>intendedWidth*scalingFactor) or (abs(y1-y0)>intendedHeight*scalingFactor) then begin
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
        enterCriticalSection(globalTextRenderingCs);
        if (scaleAndColor.lineWidth<=0) then target.Pen.style:=psSolid;
        leaveCriticalSection(globalTextRenderingCs);
      end;
    begin
      enterCriticalSection(globalTextRenderingCs);
      target.Pen.style:=psSolid;
      target.Pen.color:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecRound;
      leaveCriticalSection(globalTextRenderingCs);
      lastWasValid:=false;
      i:=0;
      while i+1<length(screenRow) do begin
        if screenRow[i  ].valid and
           screenRow[i+1].valid then
          drawEllipse(screenRow[i  ].x, screenRow[i  ].y,
                      screenRow[i+1].x, screenRow[i+1].y);
        inc(i, 2);
      end;
    end;

  PROCEDURE drawTubes;
    begin
      i:=0;
      while i+3<length(screenRow) do begin
        enterCriticalSection(globalTextRenderingCs);
        target.Pen.style:=psSolid;
        target.Pen.color:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecRound;
        leaveCriticalSection(globalTextRenderingCs);
        if scaleAndColor.lineWidth>0 then begin
          if screenRow[i  ].valid and screenRow[i+2].valid then target.line(screenRow[i  ].x,screenRow[i  ].y,
                                                                            screenRow[i+2].x,screenRow[i+2].y);
          if screenRow[i+1].valid and screenRow[i+3].valid then target.line(screenRow[i+1].x,screenRow[i+1].y,
                                                                            screenRow[i+3].x,screenRow[i+3].y);
        end;
        if screenRow[i  ].valid and screenRow[i+2].valid and
           screenRow[i+1].valid and screenRow[i+3].valid then
        drawCustomQuad(screenRow[i  ].x,screenRow[i  ].y,
                       screenRow[i+2].x,screenRow[i+2].y,
                       screenRow[i+3].x,screenRow[i+3].y,
                       screenRow[i+1].x,screenRow[i+1].y,false);
        inc(i,2);
      end;
    end;

  PROCEDURE drawPolygons;
    VAR points:array of TPoint;
    PROCEDURE screenRowPoly(CONST i0,i1:longint); inline;
      VAR i:longint;
      begin
        if (i0<0) or (i1<i0+1) then exit;
        if scaleAndColor.solidStyle=bsClear then begin
          target.MoveTo(screenRow[i1].x, screenRow[i1].y);
          for i:=i0 to i1 do target.LineTo(screenRow[i].x, screenRow[i].y);
        end else begin
          setLength(points,i1-i0+1);
          for i:=0 to i1-i0 do begin
            points[i].x:=screenRow[i0+i].x;
            points[i].y:=screenRow[i0+i].y;
          end;
          target.Polygon(points);
        end;
      end;

    VAR i,j:longint;
    begin
      if (scaleAndColor.solidStyle=bsClear) and (scaleAndColor.lineWidth<=0) then exit;
      enterCriticalSection(globalTextRenderingCs);
      if (scaleAndColor.lineWidth<=0)
      then target.Pen.style:=psClear
      else target.Pen.style:=psSolid;
      target.Pen.color:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecRound;
      target.Brush.color:=scaleAndColor.solidColor;
      target.Brush.style:=scaleAndColor.solidStyle;
      leaveCriticalSection(globalTextRenderingCs);
      j:=-1;
      for i:=0 to length(screenRow)-1 do if not(screenRow[i].valid) then begin
        if j>=0 then screenRowPoly(j,i-1);
        j:=-1;
      end else if j<0 then j:=i;
      i:=length(screenRow)-1;
      if j>=0 then screenRowPoly(j,i);
      setLength(points,0);
    end;

  PROCEDURE prepareBSplines;
    CONST coeff:array[0..15,0..3] of double=((0.16666666666666666    ,0.6666666666666666 ,0.16666666666666666,0),
                                             (0.13550617283950619    ,0.66237037037037039,0.20207407407407407,0.00004938271604938271),
                                             (0.10849382716049384    ,0.6500740740740741 ,0.24103703703703705,0.0003950617283950617),
                                             (0.085333333333333358   ,0.6306666666666666 ,0.28266666666666668,0.0013333333333333337),
                                             (0.065728395061728409   ,0.605037037037037  ,0.32607407407407407,0.0031604938271604936),
                                             (0.049382716049382734   ,0.57407407407407407,0.3703703703703704 ,0.006172839506172839),
                                             (0.036                  ,0.5386666666666666 ,0.41466666666666674,0.01066666666666667),
                                             (0.025283950617283949   ,0.4997037037037037 ,0.45807407407407408,0.016938271604938274),
                                             (0.016938271604938274   ,0.45807407407407408,0.4997037037037037 ,0.025283950617283949),
                                             (0.01066666666666667    ,0.41466666666666674,0.5386666666666666 ,0.036),
                                             (0.0061728395061728418  ,0.3703703703703704 ,0.57407407407407407,0.04938271604938271),
                                             (0.0031604938271604954  ,0.3260740740740741 ,0.6050370370370369 ,0.06572839506172838),
                                             (0.0013333333333333324  ,0.2826666666666666 ,0.6306666666666667 ,0.085333333333333358),
                                             (0.00039506172839506149 ,0.24103703703703708,0.650074074074074  ,0.10849382716049384),
                                             (0.000049382716049382686,0.20207407407407407,0.66237037037037039,0.13550617283950619),
                                             (0                      ,0.16666666666666666,0.6666666666666666 ,0.16666666666666666));
    VAR input:T_rowToPaint;
    PROCEDURE screenRowSpline(CONST i0,i1:longint; VAR k:longint);

      VAR support:T_rowToPaint;
          i,j:longint;
      begin
        if (i0<0) or (i1<i0+1) then exit;
        if (scaleAndColor.solidStyle=bsClear) and (scaleAndColor.lineWidth<=0) then exit;
        j:=k+(i1-i0+4)*length(coeff)+2;
        if length(screenRow)<j then setLength(screenRow,j);
        setLength(support,4);
        for i:=i0-2 to i1-1 do begin
          if i  <i0 then support[0]:=input[i0] else support[0]:=input[i  ];
          if i+1<i0 then support[1]:=input[i0] else support[1]:=input[i+1];
          if i+2>i1 then support[2]:=input[i1] else support[2]:=input[i+2];
          if i+3>i1 then support[3]:=input[i1] else support[3]:=input[i+3];
          for j:=0 to length(coeff)-1 do begin
            screenRow[k].x:=round(support[0].x*coeff[j,0]+
                                  support[1].x*coeff[j,1]+
                                  support[2].x*coeff[j,2]+
                                  support[3].x*coeff[j,3]);
            screenRow[k].y:=round(support[0].y*coeff[j,0]+
                                  support[1].y*coeff[j,1]+
                                  support[2].y*coeff[j,2]+
                                  support[3].y*coeff[j,3]);
            screenRow[k].valid:=true;
            inc(k);
          end;
        end;
        screenRow[k]:=input[i1];
        inc(k);
        screenRow[k].valid:=false;
        inc(k);
      end;

    VAR i,j,k:longint;
    begin
      setLength(input,length(screenRow));
      for i:=0 to length(screenRow)-1 do input[i]:=screenRow[i];
      setLength(screenRow,0);
      k:=0;
      j:=-1;
      for i:=0 to length(input)-1 do if not(input[i].valid) then begin
        if j>=0 then screenRowSpline(j,i-1,k);
        j:=-1;
      end else if j<0 then j:=i;
      i:=length(input)-1;
      if j>=0 then screenRowSpline(j,i,k);
      setLength(screenRow,k);
      setLength(input,0);
    end;

  PROCEDURE prepareSplines;
    VAR input:T_rowToPaint;
    PROCEDURE Spline(CONST i0,i1:longint; VAR k:longint);
      CONST precision=16;
            dt=1/precision;
      VAR M:array of T_point;
          C:array of double;
          i,n,j:longint;
          t:double;
          sample,
          cub0,cub1, off,lin :T_point;
      begin
        if (i0<0) or (i1<i0+1) then exit;
        if (scaleAndColor.solidStyle=bsClear) and (scaleAndColor.lineWidth<=0) then exit;
        n:=i1-i0+1;
        setLength(M,n);
        setLength(C,n);
        dec(n);
        M[0]:=pointOf(input[i0].x,input[i0].y)*0.125;
        M[n]:=pointOf(input[i1].x,input[i1].y)*(-0.5);
        C[0]:=1/4;
        for i:=1 to n-1 do begin
          M[i]:=(pointOf(input[i0+i-1].x,
                         input[i0+i-1].y)
                -pointOf(input[i0+i  ].x,
                         input[i0+i  ].y)*2
                +pointOf(input[i0+i+1].x,
                         input[i0+i+1].y))*6;
          C[i]:=1/(4-C[i-1]);
        end;
        M[0]:=M[0]*0.25;
        for i:=1 to n       do M[i]:=(M[i]-M[i-1])*C[i];
        for i:=n-1 downto 0 do M[i]:=M[i]-M[i+1]*C[i];

        setLength(screenRow,k+precision*(n)+2);
        for i:=0 to n-1 do begin
          cub0:=M[i  ]*(1/6);
          cub1:=M[i+1]*(1/6);
          off :=pointOf(input[i0+i].x,input[i0+i].y)-M[i]*(1/6);
          lin :=pointOf(input[i0+i+1].x-input[i0+i].x,
                        input[i0+i+1].y-input[i0+i].y)-(M[i+1]-M[i])*(1/6);

          t:=0;
          for j:=0 to precision-1 do begin
            sample:=off+(lin+cub1*sqr(t))*t+cub0*sqr(1-t)*(1-t);
            screenRow[k].x:=round(sample[0]);
            screenRow[k].y:=round(sample[1]);
            screenRow[k].valid:=true;
            inc(k);
            t+=dt;
          end;
        end;
        screenRow[k]:=input[i1];
        inc(k);
        screenRow[k].valid:=false;
        inc(k);
      end;

    VAR i,j,k:longint;
    begin
      setLength(input,length(screenRow));
      for i:=0 to length(screenRow)-1 do input[i]:=screenRow[i];
      setLength(screenRow,0);
      k:=0;
      j:=-1;
      for i:=0 to length(input)-1 do if not(input[i].valid) then begin
        if j>=0 then Spline(j,i-1,k);
        j:=-1;
      end else if j<0 then j:=i;
      i:=length(input)-1;
      if j>=0 then Spline(j,i,k);
      setLength(input,0);
    end;

  PROCEDURE drawPluses;
    VAR i:longint;
    begin
      enterCriticalSection(globalTextRenderingCs);
      target.Pen.style:=psSolid;
      target.Pen.color:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecRound;
      leaveCriticalSection(globalTextRenderingCs);
      for i:=0 to length(screenRow)-1 do if screenRow[i].valid then begin
        target.line(screenRow[i].x-scaleAndColor.symbolWidth,
                    screenRow[i].y,
                    screenRow[i].x+scaleAndColor.symbolWidth,
                    screenRow[i].y);
        target.line(screenRow[i].x,
                    screenRow[i].y-scaleAndColor.symbolWidth,
                    screenRow[i].x,
                    screenRow[i].y+scaleAndColor.symbolWidth);
      end;
    end;

  PROCEDURE drawCrosses;
    VAR i:longint;
    begin
      enterCriticalSection(globalTextRenderingCs);
      target.Pen.style:=psSolid;
      target.Pen.color:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecRound;
      leaveCriticalSection(globalTextRenderingCs);
      for i:=0 to length(screenRow)-1 do if screenRow[i].valid then begin
        target.line(screenRow[i].x-scaleAndColor.symbolRadius,
                    screenRow[i].y-scaleAndColor.symbolRadius,
                    screenRow[i].x+scaleAndColor.symbolRadius,
                    screenRow[i].y+scaleAndColor.symbolRadius);
        target.line(screenRow[i].x+scaleAndColor.symbolRadius,
                    screenRow[i].y-scaleAndColor.symbolRadius,
                    screenRow[i].x-scaleAndColor.symbolRadius,
                    screenRow[i].y+scaleAndColor.symbolRadius);
      end;
    end;

  PROCEDURE drawDots;
    VAR i:longint;
    begin
      enterCriticalSection(globalTextRenderingCs);
      target.Pen.style:=psClear;
      target.Brush.style:=bsSolid;
      target.Brush.color:=scaleAndColor.solidColor;
      leaveCriticalSection(globalTextRenderingCs);
      if scaleAndColor.symbolWidth>=1 then begin
        for i:=0 to length(screenRow)-1 do if screenRow[i].valid then
          target.Ellipse(screenRow[i].x-scaleAndColor.symbolWidth,
                         screenRow[i].y-scaleAndColor.symbolWidth,
                         screenRow[i].x+scaleAndColor.symbolWidth,
                         screenRow[i].y+scaleAndColor.symbolWidth);
      end else begin
        for i:=0 to length(screenRow)-1 do if screenRow[i].valid then
          target.Pixels[screenRow[i].x,
                        screenRow[i].y]:=scaleAndColor.lineColor;
      end;
    end;

  PROCEDURE drawImpulses;
    VAR i:longint;
    begin
      enterCriticalSection(globalTextRenderingCs);
      target.Pen.style:=psSolid;
      target.Pen.color:=scaleAndColor.lineColor;
      target.Pen.width:=scaleAndColor.lineWidth;
      target.Pen.EndCap:=pecSquare;
      leaveCriticalSection(globalTextRenderingCs);
      for i:=0 to length(screenRow)-1 do if screenRow[i].valid then
        target.line(screenRow[i].x,
                    yBaseLine,
                    screenRow[i].x,
                    screenRow[i].y);
    end;

  begin
    screenBox:=boundingBoxOf(0,0,intendedWidth*scalingFactor,intendedHeight*scalingFactor);
    target.LockCanvas;
    //Clear:------------------------------------------------------------------
    enterCriticalSection(globalTextRenderingCs);
    target.Brush.style:=bsSolid;
    target.Brush.color:=clWhite;
    target.Pen.style:=psClear;
    target.Pen.EndCap:=pecSquare;
    target.Pen.style:=psSolid;
    leaveCriticalSection(globalTextRenderingCs);
    target.FillRect(0, 0, intendedWidth*scalingFactor, intendedHeight*scalingFactor);

    //------------------------------------------------------------------:Clear
    //coordinate grid:========================================================
    //minor grid:-------------------------------------------------------------
    scaleAndColor:=MINOR_TIC_STYLE.getLineScaleAndColor(intendedWidth*scalingFactor,intendedHeight*scalingFactor,SINGLE_SAMPLE_INDEX);
    enterCriticalSection(globalTextRenderingCs);
    target.Pen.color:=scaleAndColor.lineColor;
    target.Pen.width:=scaleAndColor.lineWidth;
    leaveCriticalSection(globalTextRenderingCs);
    if (gse_fineGrid in scalingOptions.axisStyle['y']) then
    for i:=0 to length(gridTic['y'])-1 do with gridTic['y'][i] do if not(major) then begin
      lastY:=round(pos*scalingFactor);
      target.line(0, lastY, intendedWidth*scalingFactor, lastY);
    end;
    if (gse_fineGrid in scalingOptions.axisStyle['x']) then
    for i:=0 to length(gridTic['x'])-1 do with gridTic['x'][i] do if not(major) then begin
      lastX:=round(pos*scalingFactor);
      target.line(lastX, 0, lastX, intendedHeight*scalingFactor);
    end;
    //-------------------------------------------------------------:minor grid
    //major grid:-------------------------------------------------------------
    scaleAndColor:=MAJOR_TIC_STYLE.getLineScaleAndColor(intendedWidth*scalingFactor,intendedHeight*scalingFactor,SINGLE_SAMPLE_INDEX);
    enterCriticalSection(globalTextRenderingCs);
    target.Pen.color:=scaleAndColor.lineColor;
    target.Pen.width:=scaleAndColor.lineWidth;
    leaveCriticalSection(globalTextRenderingCs);
    if (gse_coarseGrid in scalingOptions.axisStyle['y']) then
    for i:=0 to length(gridTic['y'])-1 do with gridTic['y'][i] do if major then begin
      {$Q-}
      lastY:=round(pos*scalingFactor);
      {$Q+}
      target.line(0, lastY, intendedWidth*scalingFactor, lastY);
    end;
    if (gse_coarseGrid in scalingOptions.axisStyle['x']) then
    for i:=0 to length(gridTic['x'])-1 do with gridTic['x'][i] do if major then begin
      {$Q-}
      lastX:=round(pos*scalingFactor);
      {$Q+}
      target.line(lastX, 0, lastX, intendedHeight*scalingFactor);
    end;
    //-------------------------------------------------------------:major grid
    //========================================================:coordinate grid
    try
      if scalingOptions.axisTrafo['y'].logscale
      then yBaseLine:=scalingOptions.axisTrafo['y'].screenMin*scalingFactor
      else yBaseLine:=round(scalingOptions.axisTrafo['y'].apply(0)*scalingFactor);
      if      yBaseLine<0 then yBaseLine:=0
      else if yBaseLine>=intendedHeight*scalingFactor then yBaseLine:=intendedHeight*scalingFactor-1;
    except
      yBaseLine:=0;
    end;
    //row data:===============================================================
    for rowId:=0 to length(row)-1 do begin
      screenRow:=scalingOptions.transformRow(row[rowId].sample,scalingFactor,darts_delta[sampleIndex mod 5,0],darts_delta[sampleIndex mod 5,1]);
      scaleAndColor:=row[rowId].style.getLineScaleAndColor(intendedWidth*scalingFactor,intendedHeight*scalingFactor,sampleIndex);
      if ps_stepLeft  in row[rowId].style.style then drawStepsLeft;
      if ps_stepRight in row[rowId].style.style then drawStepsRight;
      if ps_bar       in row[rowId].style.style then drawBars;
      if ps_box       in row[rowId].style.style then drawBoxes;
      if ps_ellipse   in row[rowId].style.style then drawEllipses;
      if ps_tube      in row[rowId].style.style then drawTubes;
      if ps_polygon   in row[rowId].style.style then drawPolygons;
      if ps_dot       in row[rowId].style.style then drawDots;
      if ps_plus      in row[rowId].style.style then drawPluses;
      if ps_cross     in row[rowId].style.style then drawCrosses;
      if ps_impulse   in row[rowId].style.style then drawImpulses;

      if (ps_bspline  in row[rowId].style.style) and
         (ps_straight in row[rowId].style.style) then drawStraightLines;
      if      ps_bspline   in row[rowId].style.style then prepareBSplines
      else if ps_cosspline in row[rowId].style.style then prepareSplines;

      if (ps_bspline   in row[rowId].style.style) or
         (ps_cosspline in row[rowId].style.style) or
         (ps_straight  in row[rowId].style.style) then drawStraightLines;
    end;
    //===============================================================:row data
    target.UnlockCanvas;
  end;

PROCEDURE T_plot.drawCustomText(CONST target: TCanvas; CONST intendedWidth,intendedHeight: longint);
  VAR txt:P_customText;
  begin
    for txt in customText do txt^.renderText(intendedWidth,intendedHeight,scalingOptions,target);
  end;

PROCEDURE T_plot.drawCoordSys(CONST target: TCanvas; CONST intendedWidth,intendedHeight: longint; VAR gridTic: T_ticInfos);
  VAR i, x, y: longint;
      cSysX,cSysY:longint;
  begin
    enterCriticalSection(globalTextRenderingCs);
    try
      target.Font.size:=scalingOptions.absoluteFontSize(intendedWidth,intendedHeight);
      target.Font.color:=clBlack;
      cSysX:=scalingOptions.axisTrafo['x'].screenMin;
      cSysY:=scalingOptions.axisTrafo['y'].screenMin;
      //clear border:-----------------------------------------------------------
      enterCriticalSection(globalTextRenderingCs);
      target.Brush.style:=bsSolid;
      target.Brush.color:=clWhite;
      target.Pen.style:=psClear;
      target.Pen.width:=1;
      target.Pen.EndCap:=pecSquare;
      leaveCriticalSection(globalTextRenderingCs);
      if (scalingOptions.axisStyle['y']<>[]) then target.FillRect(0,0,cSysX,intendedHeight);
      if (scalingOptions.axisStyle['x']<>[]) then target.FillRect(cSysX,cSysY,intendedWidth,intendedHeight);
      //-----------------------------------------------------------:clear border
      //coordinate system:======================================================
      //axis:-------------------------------------------------------------------
      enterCriticalSection(globalTextRenderingCs);
      target.Pen.style:=psSolid;
      target.Pen.color:=clBlack;
      target.Pen.width:=1;
      leaveCriticalSection(globalTextRenderingCs);
      if (scalingOptions.axisStyle['y']<>[]) then target.line(cSysX, 0, cSysX, cSysY);
      if (scalingOptions.axisStyle['x']<>[]) then target.line(intendedWidth, cSysY, cSysX, cSysY);
      //-------------------------------------------------------------------:axis
      //tics:-------------------------------------------------------------------
      if (gse_tics in scalingOptions.axisStyle['y']) then for i:=0 to length(gridTic['y'])-1 do with gridTic['y'][i] do if major then begin
        y:=round(pos);
        target.line(cSysX-5, y, cSysX, y);
        target.textOut(cSysX-5-target.textWidth(txt),
                            y-target.textHeight(txt) shr 1, txt);
      end;
      if (gse_tics in scalingOptions.axisStyle['x']) then for i:=0 to length(gridTic['x'])-1 do with gridTic['x'][i] do if major then begin
        x:=round(pos);
        target.line(x, cSysY+5, x, cSysY);
        target.textOut(x-target.textWidth(txt) shr 1, cSysY+5, txt);
      end;
      //-------------------------------------------------------------------:tics
      //======================================================:coordinate system
    finally
      leaveCriticalSection(globalTextRenderingCs);
    end;
  end;

PROCEDURE scale(source: TImage; VAR dest: TImage; CONST factor: double);
  VAR ARect: TRect;
      X, Y: integer;
  begin
    X:=round(source.width*factor);
    Y:=round(source.height*factor);
    ARect:=rect(0, 0, X, Y);
    dest.Canvas.AntialiasingMode:=amOn;
    dest.Canvas.StretchDraw(ARect, source.picture.Bitmap);
  end;

PROCEDURE T_plot.renderPlot(VAR plotImage: TImage; CONST quality: T_plotQuality);
  VAR renderImage:array[0..3] of TImage;
      temp:TImage;
      gridTics:T_ticInfos;
      k:byte;

  PROCEDURE avgRenderImages;
    VAR src:array[0..3] of TLazIntfImage;
        dest:TLazIntfImage;
        srcLine :array[0..3] of PByte;
        destLine:PByte;

        x,y,k:longint;
    begin
      dest:=plotImage.picture.Bitmap.CreateIntfImage;
      for k:=0 to 3 do src[k]:=renderImage[k].picture.Bitmap.CreateIntfImage;
      for y:=0 to renderImage[k].height-1 do begin
        for k:=0 to 3 do srcLine[k]:=src[k].GetDataLineStart(y);
        destLine                   :=dest  .GetDataLineStart(y);
        for x:=0 to {$ifdef UNIX}4{$else}3{$endif}*plotImage.width-1 do begin
          destLine^:=(longint(srcLine[0]^)+
                      longint(srcLine[1]^)+
                      longint(srcLine[2]^)+
                      longint(srcLine[3]^)) shr 2;
          for k:=0 to 3 do inc(srcLine[k]);
          inc(destLine);
        end;
      end;
      for k:=0 to 3 do src[k].destroy;
      plotImage.picture.Bitmap.LoadFromIntfImage(dest);
      dest.destroy;
    end;

  begin
    if (plotImage.width<5) or (plotImage.height<5) then exit;
    initialize(gridTics);
    system.enterCriticalSection(cs);
    try
      scalingOptions.updateForPlot(plotImage.Canvas,plotImage.width,plotImage.height,row,gridTics);
      case quality of
        PLOT_QUALITY_LOW:
          begin
            renderImage[0]:=TImage.create(nil);
            renderImage[0].SetInitialBounds(0,0,plotImage.width,plotImage.height);
            drawGridAndRows(renderImage[0].Canvas,plotImage.width,plotImage.height,1,gridTics,SINGLE_SAMPLE_INDEX);
            drawCoordSys(renderImage[0].Canvas,plotImage.width,plotImage.height,gridTics);
            drawCustomText(renderImage[0].Canvas,plotImage.width,plotImage.height);
            scale(renderImage[0],plotImage,1);
            enterCriticalSection(globalTextRenderingCs);
            renderImage[0].destroy;
            leaveCriticalSection(globalTextRenderingCs);
          end;
        PLOT_QUALITY_MEDIUM_1:
          begin
            renderImage[0]:=TImage.create(nil);
            renderImage[0].SetInitialBounds(0,0,plotImage.width*2,plotImage.height*2);
            drawGridAndRows(renderImage[0].Canvas,plotImage.width,plotImage.height,2,gridTics,SINGLE_SAMPLE_INDEX);
            scale(renderImage[0],plotImage,0.5);
            enterCriticalSection(globalTextRenderingCs);
            renderImage[0].destroy;
            leaveCriticalSection(globalTextRenderingCs);
            drawCoordSys(plotImage.Canvas,plotImage.width,plotImage.height,gridTics);
            drawCustomText(plotImage.Canvas,plotImage.width,plotImage.height);
          end;
        PLOT_QUALITY_MEDIUM_2:
          begin
            for k:=0 to 3 do begin
              renderImage[k]:=TImage.create(nil);
              renderImage[k].SetInitialBounds(0,0,plotImage.width,plotImage.height);
              drawGridAndRows(renderImage[k].Canvas,plotImage.width,plotImage.height,1,gridTics,k);
            end;
            avgRenderImages;
            enterCriticalSection(globalTextRenderingCs);
            for k:=0 to 3 do renderImage[k].destroy;
            leaveCriticalSection(globalTextRenderingCs);
            drawCoordSys(plotImage.Canvas,plotImage.width,plotImage.height,gridTics);
            drawCustomText(plotImage.Canvas,plotImage.width,plotImage.height);
          end;
        PLOT_QUALITY_HIGH:
          begin
            temp:=TImage.create(nil);
            temp.SetInitialBounds(0,0,plotImage.width*2,plotImage.height*2);
            for k:=0 to 3 do begin
              drawGridAndRows(temp.Canvas,plotImage.width,plotImage.height,2,gridTics,k);
              renderImage[k]:=TImage.create(nil);
              renderImage[k].SetInitialBounds(0,0,plotImage.width,plotImage.height);
              scale(temp,renderImage[k],0.5);
            end;
            enterCriticalSection(globalTextRenderingCs);
            temp.destroy;
            leaveCriticalSection(globalTextRenderingCs);
            avgRenderImages;
            enterCriticalSection(globalTextRenderingCs);
            for k:=0 to 3 do renderImage[k].destroy;
            leaveCriticalSection(globalTextRenderingCs);
            drawCoordSys(plotImage.Canvas,plotImage.width,plotImage.height,gridTics);
            drawCustomText(plotImage.Canvas,plotImage.width,plotImage.height);
          end;
      end;
    finally
      system.leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_plot.obtainPlot(CONST width, height: longint; CONST quality: T_plotQuality): TImage;
  begin
    result:=TImage.create(nil);
    result.SetInitialBounds(0,0,width,height);
    renderPlot(result,quality);
  end;

PROCEDURE T_plot.renderToFile(CONST fileName: string; CONST width, height,
  supersampling: longint);
  VAR storeImage:TImage;
  begin
    storeImage:=obtainPlot(width,height,supersampling);
    storeImage.picture.PNG.saveToFile(ChangeFileExt(fileName, '.png'));
    enterCriticalSection(globalTextRenderingCs);
    storeImage.destroy;
    leaveCriticalSection(globalTextRenderingCs);
  end;

FUNCTION T_plot.renderToString(CONST width, height, supersampling: longint): ansistring;
  VAR storeImage: TImage;
      memStream: TStringStream;
  begin
    storeImage:=obtainPlot(width,height,supersampling);
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
      transparentCount:=p.transparentCount;
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
        then setString(currentPlot.renderToString(width,height,quality))
        else currentPlot.renderToFile(fileName,   width,height,quality);
        P_plotRenderRequest(message)^.fileName:='';
      end;
      mt_plot_retrieveOptions:
        P_plotOptionsMessage(message)^.setOptions(currentPlot.scalingOptions);
      mt_plot_setOptions:
        currentPlot.scalingOptions.modifyOptions(P_plotOptionsMessage(message)^.options,P_plotOptionsMessage(message)^.modified);
      mt_plot_clear:
        currentPlot.clear;
      mt_plot_clearAnimation:
        animation.clear;
      mt_plot_addAnimationFrame:
        animation.addFrame(currentPlot);
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
      dummyLocation:T_tokenLocation;
      commands:T_arrayOfString;
      DataString:string;
  begin
    enterCriticalSection(adapterCs);
    try
      globalRowData:=newListLiteral();
      result:='#!'+settings.fullFlavourLocation+' -GUI';
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
    then inherited create(at_plot,C_includableMessages[at_plot]-[mt_plot_queryClosedByUser,mt_plot_addAnimationFrame,mt_plot_clearAnimation,mt_plot_postDisplay])
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
        mt_plot_retrieveOptions,
        mt_plot_renderRequest,
        mt_plot_queryClosedByUser,
        mt_plot_addAnimationFrame: begin
          result:=true;
          //if there are pending tasks then store else process
          if (length(storedMessages)>0)
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
      for i:=0 to length(storedMessages)-1 do if storedMessages[i]^.messageType=mt_plot_postDisplay then lastDisplayIndex:=i;
      //process messages
      for i:=0 to length(storedMessages)-1 do begin
        m:=storedMessages[i];
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
