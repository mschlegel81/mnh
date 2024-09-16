UNIT mnh_plotData;
INTERFACE
{$ifdef Windows}
  {$define enable_render_threads}
{$endif}
USES sysutils,
     math,
     Classes, ExtCtrls, Graphics, types,Forms, ComCtrls, LCLType,
     mySys,myGenerics,
     basicTypes, mnh_constants,
     mnh_settings,
     mnh_messages,
     out_adapters,
     plotstyles,plotMath,
     litVar,
     EpikTimer,
     BGRABitmap,BGRACanvas,BGRABitmapTypes,BGRADefaultBitmap,
     ideLayoutUtil;
CONST VOLATILE_FRAMES_MAX=100;
TYPE
  T_boundingBox = array['x'..'y', 0..1] of double;
  T_timedPlotExecution=object
    timer:TEpikTimer;
    timeout:double;
    PROCEDURE wait;
    FUNCTION secondsLeft:double;
  end;

  P_addRowMessage=^T_addRowMessage;
  T_addRowMessage=object(T_payloadMessage)
    public
      styleOptions: string;
      rowData:T_dataRow;
      used:boolean;
      FUNCTION internalType:shortstring; virtual;
      CONSTRUCTOR create(CONST styleOptions_: string; CONST rowData_:T_dataRow);
  end;

  P_rasterImageMessage=^T_rasterImageMessage;
  T_rasterImageMessage=object(T_payloadMessage)
    public
      colorFill:longint;
      colors:array of T_color;
      primitive:P_rasterImage;
      FUNCTION internalType:shortstring; virtual;
      CONSTRUCTOR create(CONST width_:longint; CONST scale,offsetX,offsetY:double);
      FUNCTION canAddColor(CONST literal:P_literal):boolean;
      PROCEDURE doneAddingColors;
  end;

  P_addTextMessage=^T_addTextMessage;
  T_addTextMessage=object(T_payloadMessage)
    customText:P_customText;
    FUNCTION internalType:shortstring; virtual;
    CONSTRUCTOR create(CONST cText:P_customText);
  end;

  P_plotOptionsMessage=^T_plotOptionsMessage;
  T_plotOptionsMessage=object(T_payloadMessage)
    private
      options:T_scalingOptions;
      modified:T_scalingOptionElements;
      retrieved:boolean;
    public
      FUNCTION internalType:shortstring; virtual;
      CONSTRUCTOR createRetrieveRequest;
      CONSTRUCTOR createPostRequest(CONST o:T_scalingOptions; CONST m:T_scalingOptionElements);
      PROCEDURE setOptions(CONST o:T_scalingOptions);
      FUNCTION getOptionsWaiting(CONST errorFlagProvider:P_messages):T_scalingOptions;
  end;

  T_plotRenderTarget=(prt_file,prt_fileInBackground,prt_pngString,prt_bmpString,prt_rawData);

  P_plotRenderRequest=^T_plotRenderRequest;
  T_plotRenderRequest=object(T_payloadMessage)
    private
      feedbackLocation:T_searchTokenLocation;
      feedbackMessages:P_messages;
      target:T_plotRenderTarget;
      fileName:string;
      width,height:longint;
      outputLiteral:P_literal;
    public
      FUNCTION internalType:shortstring; virtual;
      CONSTRUCTOR createRenderToFileRequest   (CONST filename_:string; CONST width_,height_:longint; CONST backgroundRendering:boolean; CONST feedbackLocation_:T_searchTokenLocation; CONST feedbackMessages_:P_messages);
      CONSTRUCTOR createRenderToStringRequest (CONST width_,height_:longint; CONST feedbackLocation_:T_searchTokenLocation; CONST feedbackMessages_:P_messages; CONST pngFormat:boolean);
      CONSTRUCTOR createRenderToRawDataRequest(CONST width_,height_:longint; CONST feedbackLocation_:T_searchTokenLocation; CONST feedbackMessages_:P_messages);
      PROCEDURE setLiteral(CONST L:P_literal);
      FUNCTION getLiteralWaiting(CONST errorFlagProvider:P_messages):P_literal;
  end;

  P_plotDropRowRequest=^T_plotDropRowRequest;
  T_plotDropRowRequest=object(T_payloadMessage)
    private
      count:longint;
    public
      FUNCTION internalType:shortstring; virtual;
      CONSTRUCTOR create(CONST numberOfRowsToDrop:longint);
  end;

  P_plotDisplayRequest=^T_plotDisplayRequest;
  T_plotDisplayRequest=object(T_payloadMessage)
    private
      displayExecuted:boolean;
    public
      FUNCTION internalType:shortstring; virtual;
      CONSTRUCTOR create();
      PROCEDURE waitForExecution(CONST errorFlagProvider:P_messages);
      PROCEDURE markExecuted;
  end;

  P_plotAddAnimationFrameRequest=^T_plotAddAnimationFrameRequest;
  T_plotAddAnimationFrameRequest=object(T_payloadMessage)
    private
      propSleep:double;
      framesInQueue:longint;
    public
      FUNCTION internalType:shortstring; virtual;
      CONSTRUCTOR create();
      FUNCTION getProposedSleepTime(CONST errorFlagProvider:P_messages; OUT framesQueued:longint):double;
      PROCEDURE markExecuted(CONST proposedSleepTime:double; CONST framesQueued:longint);
  end;

  T_frameCacheMode=(fcm_none,fcm_retainImage,fcm_inMemoryPng,fcm_tempFile);
  T_backgroundProcessingState=(bps_none,bps_pending,bps_running);

  P_plot =^T_plot;
  T_plot = object
    private
      plotCs: TRTLCriticalSection;
      scalingOptions:T_scalingOptions;
      row: T_plotPrimitiveList;
      virtualRowIndex:longint;

      cachedImage:record
        image:TImage;
        dumpIsUpToDate:boolean;
        dumpName:string;
        inMemoryDump:TMemoryStream;
        renderedWidth  ,
        renderedHeight :longint;
      end;

      backgroundProcessing:record
        state:T_backgroundProcessingState;
        postedWidth  ,
        postedHeight :longint;
        postedFileName:string;
        renderToFilePosted:boolean;
        destroyAfterwardsPosted:boolean;
      end;

      PROCEDURE setScalingOptions(CONST value:T_scalingOptions);
      FUNCTION  getScalingOptions:T_scalingOptions;
      PROCEDURE drawGridAndRows(CONST target: TBGRACanvas; VAR gridTic: T_ticInfos);
      PROCEDURE drawCoordSys(CONST target: TBGRACanvas; VAR gridTic: T_ticInfos);
      FUNCTION  obtainPlot(CONST width,height:longint):TImage;

      PROCEDURE addRow(CONST styleOptions: string; CONST rowData: T_dataRow);
      PROCEDURE removeRows(CONST numberOfRowsToRemove:longint);
      PROCEDURE addCustomText(CONST text:P_customText);
    public
      PROPERTY options:T_scalingOptions read getScalingOptions write setScalingOptions;

      CONSTRUCTOR createWithDefaults;
      PROCEDURE setDefaults;
      DESTRUCTOR destroy;
      PROCEDURE clear;

      PROCEDURE zoomOnPoint(CONST pixelX, pixelY: longint; CONST factor: double; VAR plotImage: TImage);
      PROCEDURE panByPixels(CONST pixelDX, pixelDY: longint; VAR plotImage: TImage);

      PROCEDURE renderPlot(VAR plotImage: TImage);
      PROCEDURE renderToFile(CONST fileName:string; CONST width,height:longint; CONST feedbackLocation:T_searchTokenLocation; CONST feedbackMessages:P_messages);
      FUNCTION renderToString(CONST width,height:longint; CONST pngFormat:boolean):ansistring;
      FUNCTION renderToRawData(CONST width,height:longint):P_collectionLiteral;

      PROCEDURE copyFrom(VAR p:T_plot);
      FUNCTION getRowStatements(CONST prevOptions:T_scalingOptions; CONST literalRecycler:P_literalRecycler; VAR globalRowData:T_listLiteral; CONST haltExport:PBoolean; CONST Application:Tapplication; CONST progress:TProgressBar):T_arrayOfString;
      FUNCTION getRowStatementCount:longint;

      PROCEDURE doneImage(CONST cacheMode:T_frameCacheMode);
      PROCEDURE obtainImage(VAR target:TImage);
      PROCEDURE prepareImage(CONST width,height:longint);

      {$ifdef enable_render_threads}
      FUNCTION canRenderToFileInBackground(CONST fileName:string; CONST width,height:longint; CONST allowThreadCreation,destroyImageInThread:boolean; CONST feedbackLocation:T_searchTokenLocation; CONST feedbackMessages:P_messages):boolean;
      FUNCTION postPreparation(CONST width,height:longint):boolean;
      {$endif}

      FUNCTION isImagePreparedForResolution(CONST width,height:longint):boolean;
      FUNCTION hasResolution(CONST width,height:longint):boolean;
  end;

  T_plotSeries=object
    private
      frame:array of P_plot;
      framesWithImagesAllocated:array[0..7] of P_plot;
      seriesCs:TRTLCriticalSection;
      volatile:boolean;
      FUNCTION getOptions(CONST index:longint):T_scalingOptions;
      PROCEDURE setOptions(CONST index:longint; CONST value:T_scalingOptions);
      PROCEDURE flushFramesToDisk;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE clear(CONST isVolatile:boolean=false);
      FUNCTION frameCount:longint;
      PROCEDURE getFrame(VAR target:TImage; CONST frameIndex:longint);
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
      displayImmediate           :boolean;
      PROCEDURE processMessage(CONST message:P_storedMessage); virtual;
    private
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
      FUNCTION canStartGuiInteraction:boolean;
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
USES myStringUtil,
     commandLineParameters,
     contexts,
     IntfGraphics,
     GraphType,
     recyclers;
{$ifdef enable_render_threads}
VAR preparationThreadsRunning:longint=0;
TYPE
T_plotPreparationThread=class(T_basicThread)
  protected
    plot:P_plot;
    feedbackLocation:T_searchTokenLocation;
    messages:P_messages;
    PROCEDURE execute; override;
  public
    CONSTRUCTOR create(CONST plot_:P_plot; CONST feedbackLocation_:T_searchTokenLocation; CONST messages_:P_messages);
    DESTRUCTOR destroy; override;
  end;

CONSTRUCTOR T_plotPreparationThread.create(CONST plot_:P_plot; CONST feedbackLocation_:T_searchTokenLocation; CONST messages_:P_messages);
  begin
    plot:=plot_;
    feedbackLocation:=feedbackLocation_;
    messages:=messages_;
    inherited create;
    interLockedIncrement(preparationThreadsRunning);
  end;

DESTRUCTOR T_plotPreparationThread.destroy;
  begin
    inherited destroy;
    interlockedDecrement(preparationThreadsRunning);
  end;
{$endif}

FUNCTION timedPlotExecution(CONST timer:TEpikTimer; CONST timeout:double):T_timedPlotExecution;
  begin
    result.timer:=timer;
    result.timeout:=timeout;
  end;

VAR MAJOR_TIC_STYLE, MINOR_TIC_STYLE:T_style;

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

FUNCTION T_plotAddAnimationFrameRequest.getProposedSleepTime(CONST errorFlagProvider:P_messages; OUT framesQueued:longint):double;
  begin
    enterCriticalSection(messageCs);
    while (propSleep<0) and (errorFlagProvider^.continueEvaluation) do begin
      leaveCriticalSection(messageCs);
      sleep(1); ThreadSwitch;
      enterCriticalSection(messageCs);
    end;
    framesQueued:=framesInQueue;
    result:=propSleep;
    leaveCriticalSection(messageCs);
  end;

PROCEDURE T_plotAddAnimationFrameRequest.markExecuted(CONST proposedSleepTime:double; CONST framesQueued:longint);
  begin
    enterCriticalSection(messageCs);
    if (proposedSleepTime<0) or (isNan(proposedSleepTime)) or (isInfinite(proposedSleepTime))
    then propSleep:=0
    else propSleep:=proposedSleepTime;
    framesInQueue:=framesQueued;
    leaveCriticalSection(messageCs);
  end;

PROCEDURE T_timedPlotExecution.wait;
  begin
    if timer=nil then exit;
    if timeout-timer.elapsed>0.1 then sleep(round(1000*(timeout-timer.elapsed))-100);
    if timeout-timer.elapsed>0   then sleep(round(1000*(timeout-timer.elapsed))    );
  end;

FUNCTION T_timedPlotExecution.secondsLeft:double;
  begin
    if timer=nil
    then result:=-1
    else result:=timeout-timer.elapsed;
  end;

PROCEDURE T_plot.doneImage(CONST cacheMode:T_frameCacheMode);
  begin
    enterCriticalSection(plotCs);
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
      leaveCriticalSection(plotCs);
    end;
  end;

PROCEDURE T_plot.prepareImage(CONST width,height:longint);
  VAR imageIsPrepared:boolean=false;
  begin
    enterCriticalSection(plotCs);
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
        if cachedImage.image<>nil then FreeAndNil(cachedImage.image);
        if cachedImage.inMemoryDump<>nil then FreeAndNil(cachedImage.inMemoryDump);
        cachedImage.image:=obtainPlot(width,height);
        cachedImage.dumpIsUpToDate:=false;
        cachedImage.renderedHeight:=height;
        cachedImage.renderedWidth :=width;
      end;
    finally
      leaveCriticalSection(plotCs);
    end;
  end;

{$ifdef enable_render_threads}
PROCEDURE T_plotPreparationThread.execute;
  VAR destroyPlot:boolean=false;
      errorText:string='';
  begin
    try
      enterCriticalSection(plot^.plotCs);
    except
      plot^.backgroundProcessing.state:=bps_none;
      Terminate;
      exit;
    end;
    try
      with plot^.backgroundProcessing do begin
        state:=bps_running;
        plot^.prepareImage(postedWidth,postedHeight);
        if renderToFilePosted then begin
          destroyPlot:=destroyAfterwardsPosted;
          plot^.renderToFile(postedFileName,postedWidth,postedHeight,feedbackLocation,messages);
          plot^.doneImage(fcm_none);
        end;
      end;
    except
      if messages<>nil then begin
        if plot^.backgroundProcessing.renderToFilePosted
        then errorText:='Rendering to file '+plot^.backgroundProcessing.postedFileName
        else errorText:='Background plot preparation';
        errorText+=' @'+intToStr(plot^.backgroundProcessing.postedWidth)+'x'+intToStr(plot^.backgroundProcessing.postedHeight)+' failed.';
        messages^.raiseSimpleError(errorText,feedbackLocation);
      end;
    end;
    plot^.backgroundProcessing.state:=bps_none;
    leaveCriticalSection(plot^.plotCs);

    if destroyPlot then dispose(plot,destroy);
    Terminate;
  end;

FUNCTION T_plot.postPreparation(CONST width,height:longint):boolean;
  begin
    if (backgroundProcessing.state<>bps_none) or isImagePreparedForResolution(width,height) then exit(false);
    enterCriticalSection(plotCs);
    if (backgroundProcessing.state<>bps_none) or isImagePreparedForResolution(width,height) then begin
      leaveCriticalSection(plotCs);
      exit(false);
    end;
    try
      with backgroundProcessing do begin
        state:=bps_pending;
        postedHeight:=height;
        postedWidth:=width;
        renderToFilePosted:=false;
        destroyAfterwardsPosted:=false;
      end;
      T_plotPreparationThread.create(@self,C_nilSearchTokenLocation,nil);
      result:=true;
    finally
      leaveCriticalSection(plotCs);
    end;
  end;
{$endif}

FUNCTION T_plot.isImagePreparedForResolution(CONST width,height:longint):boolean;
  begin
    result:=(cachedImage.renderedWidth=width) and
            (cachedImage.renderedHeight=height) and
            (cachedImage.image<>nil);
  end;

FUNCTION T_plot.hasResolution(CONST width,height:longint):boolean;
  begin
    enterCriticalSection(plotCs);
    result:=(cachedImage.renderedWidth=width) and
            (cachedImage.renderedHeight=height);
    leaveCriticalSection(plotCs);
  end;

PROCEDURE T_plot.obtainImage(VAR target: TImage);
  begin
    enterCriticalSection(plotCs);
    try
      if not isImagePreparedForResolution(target.width,target.height) then
      prepareImage(target.width,target.height);
      target.Canvas.draw(0,0,cachedImage.image.picture.Bitmap);
    finally
      leaveCriticalSection(plotCs);
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

FUNCTION T_rasterImageMessage.internalType: shortstring;
  begin
    result:='P_rasterImageMessage';
  end;

CONSTRUCTOR T_rasterImageMessage.create(CONST width_: longint; CONST scale,offsetX,offsetY:double);
  begin
    inherited create(mt_plot_rasterImage);
    new(primitive,create(scale,offsetX,offsetY));
    primitive^.width:=width_;

    setLength(colors,width_*width_);
    colorFill:=0;
  end;

FUNCTION T_rasterImageMessage.canAddColor(CONST literal: P_literal): boolean;
  begin
    if literal^.literalType in [lt_smallint,lt_bigint,lt_real] then begin
      if colorFill>=length(colors) then setLength(colors,colorFill+primitive^.width);
      colors[colorFill,cc_red  ]:=round(255*max(0, min(1,P_numericLiteral(literal)^.floatValue)));
      colors[colorFill,cc_green]:=colors[colorFill,cc_red  ];
      colors[colorFill,cc_blue ]:=colors[colorFill,cc_red  ];
      colors[colorFill,cc_alpha]:=255;
      inc(colorFill);
      result:=true;
    end else if (literal^.literalType in [lt_intList,lt_realList,lt_numList]) and (P_listLiteral(literal)^.size=3) then begin
      if colorFill>=length(colors) then setLength(colors,colorFill+primitive^.width);
      colors[colorFill,cc_red  ]:=round(255*max(0, min(1,P_numericLiteral(P_listLiteral(literal)^.value[0])^.floatValue)));
      colors[colorFill,cc_green]:=round(255*max(0, min(1,P_numericLiteral(P_listLiteral(literal)^.value[1])^.floatValue)));
      colors[colorFill,cc_blue ]:=round(255*max(0, min(1,P_numericLiteral(P_listLiteral(literal)^.value[2])^.floatValue)));
      colors[colorFill,cc_alpha]:=255;
      inc(colorFill);
      result:=true;
    end else if (literal^.literalType in [lt_intList,lt_realList,lt_numList]) and (P_listLiteral(literal)^.size=4) then begin
      if colorFill>=length(colors) then setLength(colors,colorFill+primitive^.width);
      colors[colorFill,cc_red  ]:=round(255*max(0, min(1,P_numericLiteral(P_listLiteral(literal)^.value[0])^.floatValue)));
      colors[colorFill,cc_green]:=round(255*max(0, min(1,P_numericLiteral(P_listLiteral(literal)^.value[1])^.floatValue)));
      colors[colorFill,cc_blue ]:=round(255*max(0, min(1,P_numericLiteral(P_listLiteral(literal)^.value[2])^.floatValue)));
      colors[colorFill,cc_alpha]:=round(255*max(0, min(1,P_numericLiteral(P_listLiteral(literal)^.value[3])^.floatValue)));
      inc(colorFill);
      result:=true;
    end else result:=false;
  end;

PROCEDURE T_rasterImageMessage.doneAddingColors;
  VAR height,k:longint;
  begin
    setLength(colors,colorFill);
    height:=length(colors) div primitive^.width;
    if height*primitive^.width<length(colors) then inc(height);
    primitive^.sourceMap:=TBGRADefaultBitmap.create(primitive^.width,height);
    for k:=0 to length(colors)-1 do begin
      primitive^.sourceMap.setPixel  (k mod primitive^.width,k div primitive^.width,RGBToColor(colors[k,cc_red],colors[k,cc_green],colors[k,cc_blue]));
      primitive^.sourceMap.AlphaPixel(k mod primitive^.width,k div primitive^.width,           colors[k,cc_alpha]);
    end;
    setLength(colors,0);
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

CONSTRUCTOR T_plotDropRowRequest.create(CONST numberOfRowsToDrop: longint);
  begin
    inherited create(mt_plot_dropRow);
    count:=numberOfRowsToDrop;
  end;

FUNCTION T_plotRenderRequest.internalType: shortstring;
  begin
    result:='T_plotRenderRequest';
  end;

CONSTRUCTOR T_plotRenderRequest.createRenderToFileRequest(CONST filename_: string; CONST width_, height_: longint; CONST backgroundRendering:boolean; CONST feedbackLocation_:T_searchTokenLocation; CONST feedbackMessages_:P_messages);
  begin
    inherited create(mt_plot_renderRequest);
    feedbackLocation:=feedbackLocation_;
    feedbackMessages:=feedbackMessages_;
    if backgroundRendering
    then target:=prt_fileInBackground
    else target:=prt_file;
    fileName:=filename_;
    width:=width_;
    height:=height_;
    outputLiteral:=nil;
  end;

CONSTRUCTOR T_plotRenderRequest.createRenderToStringRequest(CONST width_,height_: longint; CONST feedbackLocation_:T_searchTokenLocation; CONST feedbackMessages_:P_messages; CONST pngFormat:boolean);
  begin
    inherited create(mt_plot_renderRequest);
    feedbackLocation:=feedbackLocation_;
    feedbackMessages:=feedbackMessages_;
    if pngFormat
    then target:=prt_pngString
    else target:=prt_bmpString;
    fileName:='';
    width:=width_;
    height:=height_;
    outputLiteral:=nil;
  end;

CONSTRUCTOR T_plotRenderRequest.createRenderToRawDataRequest(CONST width_,height_:longint; CONST feedbackLocation_:T_searchTokenLocation; CONST feedbackMessages_:P_messages);
  begin
    inherited create(mt_plot_renderRequest);
    feedbackLocation:=feedbackLocation_;
    feedbackMessages:=feedbackMessages_;
    target:=prt_rawData;
    fileName:='';
    width:=width_;
    height:=height_;
    outputLiteral:=nil;
  end;

PROCEDURE T_plotRenderRequest.setLiteral(CONST L:P_literal);
  begin
    enterCriticalSection(messageCs);
    if L=nil
    then outputLiteral:=newVoidLiteral
    else outputLiteral:=L;
    leaveCriticalSection(messageCs);
  end;

FUNCTION T_plotRenderRequest.getLiteralWaiting(CONST errorFlagProvider:P_messages):P_literal;
  begin
    enterCriticalSection(messageCs);
    while (outputLiteral=nil) and (errorFlagProvider^.continueEvaluation) do begin
      leaveCriticalSection(messageCs);
      sleep(1); ThreadSwitch;
      enterCriticalSection(messageCs);
    end;
    result:=outputLiteral;
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
    result:=frame[index]^.scalingOptions;
    leaveCriticalSection(seriesCs);
  end;

PROCEDURE T_plotSeries.setOptions(CONST index: longint; CONST value: T_scalingOptions);
  begin
    enterCriticalSection(seriesCs);
    frame[index]^.setScalingOptions(value);
    leaveCriticalSection(seriesCs);
  end;

PROCEDURE T_plotSeries.flushFramesToDisk;
  VAR k:longint;
      cleanupTimeout:double;
  begin
    enterCriticalSection(seriesCs);
    cleanupTimeout:=now+1/(24*60*60);
    try
      if ideSettings.cacheAnimationFrames then begin
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
    memoryCleaner.registerObjectForCleanup(4,@flushFramesToDisk);
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

PROCEDURE T_plotSeries.getFrame(VAR target: TImage; CONST frameIndex: longint);
  VAR current:P_plot;

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
      if ideSettings.cacheAnimationFrames
      then cacheMode:=fcm_retainImage
      else cacheMode:=fcm_none;
      if (framesWithImagesAllocated[k]<>nil) then framesWithImagesAllocated[k]^.doneImage(cacheMode);
      //shift
      move(framesWithImagesAllocated[0],
           framesWithImagesAllocated[1],
           (length(framesWithImagesAllocated)-1)*sizeOf(P_plot));
      //insert
      framesWithImagesAllocated[0]:=current;
    end;

  begin
    if (frameIndex<0) or (frameIndex>=length(frame)) then exit;
    current:=frame[frameIndex];
    if not(volatile) then handleImagesToFree;
    current^.obtainImage(target);
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
      {$ifdef enable_render_threads}
      if exportingAll
      then frame[index]^.canRenderToFileInBackground(fileName,width,height,true,false,C_nilSearchTokenLocation,nil)
      else begin
      {$endif}
        storeImage:=TImage.create(nil);
        storeImage.SetInitialBounds(0,0,width,height);
        frame[index]^.obtainImage(storeImage);
        try
          if lowercase(extractFileExt(fileName))='.bmp'
          then storeImage.picture.Bitmap.saveToFile(fileName)
          else storeImage.picture.PNG.saveToFile(ChangeFileExt(fileName, '.png'));
        except
          postIdeMessage('Export to PNG failed for: '+fileName,true);
        end;
        storeImage.destroy;
        frame[index]^.doneImage(fcm_none);
      {$ifdef enable_render_threads}
      end;
      {$endif}
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
      new(frame[k],createWithDefaults);
      frame[k]^.copyFrom(plot);
    finally
      leaveCriticalSection(seriesCs);
    end;
  end;

FUNCTION T_plotSeries.nextFrame(VAR frameIndex: longint; CONST cycle:boolean; CONST width,height:longint):boolean;
  {$ifdef enable_render_threads}
  VAR toPrepare,
      lastToPrepare:longint;
      memoryRemaining,
      imageSizeEstimate:int64;
  {$endif}
  VAR i,j,k:longint;
  begin
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
      {$ifdef enable_render_threads}
      if result then begin
        lastToPrepare:=frameIndex+settings.cpuCount;
        if cycle
        then lastToPrepare:=lastToPrepare mod length(frame)
        else if lastToPrepare>=length(frame) then lastToPrepare:=length(frame)-1;

        toPrepare:=frameIndex+1;
        imageSizeEstimate:=(width+4)*height*5+8192; //conservative estimate

        memoryRemaining:=memoryCleaner.memoryComfortThreshold-memoryCleaner.getMemoryUsedInBytes-imageSizeEstimate*8; //assume, that those have not been taken into account yet

        while (toPrepare<length(frame)) and (toPrepare<>lastToPrepare) {and (preparationThreadsRunning<settings.cpuCount)} and (memoryRemaining>imageSizeEstimate) do begin
          if frame[toPrepare]^.postPreparation(width,height) then dec(memoryRemaining,imageSizeEstimate);
          inc(toPrepare);
          if cycle then toPrepare:=toPrepare mod length(frame);
        end;
      end;
      {$endif}
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
    system.initCriticalSection(plotCs);
    setDefaults;

    with cachedImage do begin
      image:=nil;
      dumpIsUpToDate:=false;
      dumpName:='';
      inMemoryDump:=nil;
      renderedWidth :=-1;
      renderedHeight:=-1;
    end;
    backgroundProcessing.state:=bps_none;
  end;

PROCEDURE T_plot.setDefaults;
  begin
    system.enterCriticalSection(plotCs);
    try
      scalingOptions.setDefaults;
      clear;
    finally
      system.leaveCriticalSection(plotCs);
    end;
  end;

DESTRUCTOR T_plot.destroy;
  begin
    system.enterCriticalSection(plotCs);
    try
      doneImage(fcm_none);
      if cachedImage.dumpName<>'' then DeleteFile(cachedImage.dumpName);
      clear;
    finally
      system.leaveCriticalSection(plotCs);
      doneCriticalSection(plotCs);
    end;
  end;

PROCEDURE T_plot.clear;
  VAR i: longint;
  begin
    system.enterCriticalSection(plotCs);
    try
      for i:=0 to length(row)-1 do dispose(row[i],destroy);
      setLength(row, 0);
      virtualRowIndex:=0;
    finally
      system.leaveCriticalSection(plotCs);
    end;
  end;

PROCEDURE T_plot.addRow(CONST styleOptions: string; CONST rowData: T_dataRow);
  VAR index:longint;
      style:T_style;
      firstRow:boolean=true;
      clonedData:T_dataRow;
  begin
    system.enterCriticalSection(plotCs);
    try
      for style in getStyles(virtualRowIndex,styleOptions) do begin
        index:=length(row);
        setLength(row, index+1);
        if firstRow then new(P_sampleRow(row[index]),create(rowData)) else begin
          rowData.cloneTo(clonedData);
          new(P_sampleRow(row[index]),create(clonedData));
        end;
        P_sampleRow(row[index])^.style:=style;
        P_sampleRow(row[index])^.pseudoIndex:=virtualRowIndex;
        firstRow:=false;
      end;
      virtualRowIndex+=1;
    finally
      system.leaveCriticalSection(plotCs);
    end;
  end;

PROCEDURE T_plot.removeRows(CONST numberOfRowsToRemove: longint);
  VAR i0,i:longint;
  begin
    if numberOfRowsToRemove<=0 then exit;
    system.enterCriticalSection(plotCs);
    try
      virtualRowIndex-=numberOfRowsToRemove;
      i0:=-1;
      for i:=0 to length(row)-1 do if row[i]^.pseudoIndex>=virtualRowIndex then begin
        if i0<0 then i0:=i;
        dispose(row[i],destroy);
      end;
      setLength(row,i0);
    finally
      system.leaveCriticalSection(plotCs);
    end;
  end;

PROCEDURE T_plot.addCustomText(CONST text: P_customText);
  begin
    system.enterCriticalSection(plotCs);
    try
      setLength(row,length(row)+1);
      row[length(row)-1]:=text;
      text^.pseudoIndex:=virtualRowIndex;
      inc(virtualRowIndex);
    finally
      system.leaveCriticalSection(plotCs);
    end;
  end;

PROCEDURE T_plot.setScalingOptions(CONST value: T_scalingOptions);
  begin
    system.enterCriticalSection(plotCs);
    if not(scalingOptions.equals(value)) then doneImage(fcm_none);
    scalingOptions:=value;
    system.leaveCriticalSection(plotCs);
  end;

FUNCTION T_plot.getScalingOptions: T_scalingOptions;
  begin
    system.enterCriticalSection(plotCs);
    result:=scalingOptions;
    system.leaveCriticalSection(plotCs);
  end;

PROCEDURE T_plot.zoomOnPoint(CONST pixelX, pixelY: longint;
  CONST factor: double; VAR plotImage: TImage);
  VAR rectA, rectB: TRect;
  begin with scalingOptions do begin
    system.enterCriticalSection(plotCs);
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
      system.leaveCriticalSection(plotCs);
    end;
  end; end;

PROCEDURE T_plot.panByPixels(CONST pixelDX, pixelDY: longint; VAR plotImage: TImage);
  VAR rectA, rectB: TRect;
  begin with scalingOptions do begin
    system.enterCriticalSection(plotCs);
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
      system.leaveCriticalSection(plotCs);
    end;
  end; end;

PROCEDURE T_plot.drawGridAndRows(CONST target: TBGRACanvas; VAR gridTic: T_ticInfos);
  VAR scaleAndColor:T_scaleAndColor;
      screenBox:T_boundingBox;
  VAR yBaseLine:longint;

  VAR i,k:longint;
      primitive:P_plotPrimitive;
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
    for primitive in row do primitive^.render(scalingOptions,screenBox,yBaseLine,target);
    //===============================================================:row data
  end;

PROCEDURE T_plot.drawCoordSys(CONST target: TBGRACanvas; VAR gridTic: T_ticInfos);
  VAR i, x, y: longint;
      cSysX,cSysY:longint;
  begin
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
    if (gse_tics in scalingOptions.axisStyle['y']) or (gse_tics in scalingOptions.axisStyle['x']) then begin
        target.Font.height:=scalingOptions.absoluteFontSize(target.width,target.height);
        target.Font.color:=clBlack;
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
    end;
    //-------------------------------------------------------------------:tics
    //======================================================:coordinate system
  end;

PROCEDURE T_plot.renderPlot(VAR plotImage: TImage);
  VAR gridTics:T_ticInfos;
      bgrabmp:TBGRABitmap;
  begin
    if (plotImage.width<5) or (plotImage.height<5) then exit;
    system.enterCriticalSection(plotCs);
    try
      if (length(row)=0) then begin
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
        plotImage.Canvas.clear;
        bgrabmp.draw(plotImage.Canvas,0,0,false);
        bgrabmp.free;
      end;
    finally
      system.leaveCriticalSection(plotCs);
    end;
  end;

FUNCTION T_plot.obtainPlot(CONST width, height: longint): TImage;
  begin
    result:=TImage.create(nil);
    result.SetInitialBounds(0,0,width,height);
    renderPlot(result);
  end;

PROCEDURE T_plot.renderToFile(CONST fileName: string; CONST width, height:longint; CONST feedbackLocation:T_searchTokenLocation; CONST feedbackMessages:P_messages);
  VAR storeImage:TImage;
      message:ansistring;
  begin
    storeImage:=obtainPlot(width,height);
    try
      if lowercase(extractFileExt(fileName))='.bmp'
      then storeImage.picture.Bitmap.saveToFile(fileName)
      else storeImage.picture.PNG.saveToFile(ChangeFileExt(fileName, '.png'));
    except
      on e:Exception do begin
        message:='Export to file failed for: '+fileName+C_lineBreakChar+e.message;
        if feedbackMessages<>nil
        then feedbackMessages^.raiseSimpleError(message,feedbackLocation)
        else postIdeMessage(message,true);
      end;
    end;
    storeImage.destroy;
  end;

{$ifdef enable_render_threads}
FUNCTION T_plot.canRenderToFileInBackground(CONST fileName:string; CONST width,height:longint; CONST allowThreadCreation,destroyImageInThread:boolean; CONST feedbackLocation:T_searchTokenLocation; CONST feedbackMessages:P_messages):boolean;
  begin
    enterCriticalSection(plotCs);
    if not(allowThreadCreation) or (backgroundProcessing.state<>bps_none) or (getGlobalRunningThreads>=settings.cpuCount) or isImagePreparedForResolution(width,height) then begin
      renderToFile(fileName,width,height,feedbackLocation,feedbackMessages);
      leaveCriticalSection(plotCs);
      exit(false);
    end;
    try
      with backgroundProcessing do begin
        state:=bps_pending;
        postedHeight:=height;
        postedWidth:=width;
        renderToFilePosted:=true;
        destroyAfterwardsPosted:=destroyImageInThread;
        postedFileName:=fileName;
      end;
      T_plotPreparationThread.create(@self,feedbackLocation,feedbackMessages);
    finally
      leaveCriticalSection(plotCs);
    end;
    result:=true;
  end;
{$endif}

FUNCTION T_plot.renderToString(CONST width, height: longint; CONST pngFormat:boolean): ansistring;
  VAR storeImage: TImage;
      memStream: TStringStream;
  begin
    storeImage:=obtainPlot(width,height);
    memStream := TStringStream.create('');
    if pngFormat
    then storeImage.picture.PNG   .saveToStream(memStream)
    else storeImage.picture.Bitmap.saveToStream(memStream);
    memStream.position:=0;
    result:=memStream.DataString;
    memStream.free;
    storeImage.destroy;
  end;

FUNCTION T_plot.renderToRawData(CONST width,height:longint):P_collectionLiteral;
  VAR storeImage: TImage;
      x,y:longint;
      tempIntfImage,
      ScanLineImage: TLazIntfImage;
      ImgFormatDescription: TRawImageDescription;
      pix:PByte;
      pc:array[0..2] of byte;
      literal_lookup:T_arrayOfLiteral;
      pixelList:P_listLiteral;
      recycler: P_recycler;
  begin
    recycler:=newRecycler;
    setLength(literal_lookup,256);
    for x:=0 to 255 do literal_lookup[x]:=recycler^.newRealLiteral(x/255);

    storeImage:=obtainPlot(width,height);
    ScanLineImage:=TLazIntfImage.create(storeImage.width,storeImage.height);
    ImgFormatDescription.Init_BPP24_B8G8R8_BIO_TTB(storeImage.width,storeImage.height);
    ImgFormatDescription.ByteOrder:=riboMSBFirst;
    ScanLineImage.DataDescription:=ImgFormatDescription;
    tempIntfImage:=storeImage.picture.Bitmap.CreateIntfImage;
    ScanLineImage.CopyPixels(tempIntfImage);
    pixelList:=recycler^.newListLiteral(width*height);
    for y:=0 to storeImage.height-1 do begin
      pix:=ScanLineImage.GetDataLineStart(y);
      for x:=0 to storeImage.width-1 do begin
        move((pix+3*x)^,pc,3);
        pixelList^.append(recycler,
          recycler^.newListLiteral(3)
                      ^.append(recycler,literal_lookup[pc[0]],true)
                      ^.append(recycler,literal_lookup[pc[1]],true)
                      ^.append(recycler,literal_lookup[pc[2]],true),
          false);
      end;
    end;
    result:=recycler^.newListLiteral(3)
      ^.append(recycler,pixelList,false)
      ^.appendInt(recycler,storeImage.width)
      ^.appendInt(recycler,storeImage.height);
    ScanLineImage.free;
    tempIntfImage.free;
    storeImage.destroy;
    recycler^.disposeLiterals(literal_lookup);
    freeRecycler(recycler);
  end;

PROCEDURE T_plot.copyFrom(VAR p: T_plot);
  VAR i:longint;
  begin
    system.enterCriticalSection(plotCs);
    system.enterCriticalSection(p.plotCs);
    try
      scalingOptions:=p.scalingOptions;
      //copy rows:
      for i:=0 to length(row)-1 do dispose(row[i],destroy);
      setLength(row,length(p.row));
      for i:=0 to length(row)-1 do row[i]:=p.row[i]^.getFullClone;
    finally
      system.leaveCriticalSection(p.plotCs);
      system.leaveCriticalSection(plotCs);
    end;
  end;

FUNCTION T_plot.getRowStatements(CONST prevOptions:T_scalingOptions; CONST literalRecycler:P_literalRecycler; VAR globalRowData:T_listLiteral; CONST haltExport:PBoolean; CONST Application:Tapplication; CONST progress:TProgressBar):T_arrayOfString;
  VAR opt:string;
      i,k0,k1:longint;
  begin
    if haltExport^ then exit;
    initialize(result);
    system.enterCriticalSection(plotCs);
    try
      opt:=scalingOptions.getOptionDiffString(prevOptions);
      Application.ProcessMessages;
      if opt='' then setLength(result,0) else result:=opt;
      if length(row)<4 then begin
        for i:=0 to length(row)-1 do if not(haltExport^) then begin
          append(result,row[i]^.toStatementForExport(i=0,literalRecycler,globalRowData));
          progress.position:=progress.position+1;
        end;
      end else begin
        append(result,row[0]^.toStatementForExport(true,literalRecycler,globalRowData));
        k0:=globalRowData.size;
        for i:=1 to length(row)-1 do row[i]^.toStatementForExport(false,literalRecycler,globalRowData);
        k1:=globalRowData.size-1;
        append(result,'map(ROW[['+intToStr(k0)+'..'+intToStr(k1)+']],{addPlot@$0});');
      end;
      Application.ProcessMessages;
    finally
      system.leaveCriticalSection(plotCs);
    end;
  end;

FUNCTION T_plot.getRowStatementCount:longint;
  begin
    result:=length(row);
  end;

PROCEDURE T_plotSystem.processMessage(CONST message: P_storedMessage);
  VAR clonedRow:T_dataRow;
      {$ifdef enable_render_threads}
      clonedPlot:P_plot;
      {$endif}
  begin
    case message^.messageType of
      mt_startOfEvaluation: begin
        if ideSettings.doResetPlotOnEvaluation or sandboxed
        then currentPlot.setDefaults
        else currentPlot.clear;
        animation.clear;
        if pullSettingsToGui<>nil then pullSettingsToGui();
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
        currentPlot.removeRows      (P_plotDropRowRequest(message)^.count);
      mt_plot_rasterImage: with P_rasterImageMessage(message)^ do begin
        enterCriticalSection(currentPlot.plotCs);
        try
          setLength(currentPlot.row,length(currentPlot.row)+1);
          currentPlot.row[length(currentPlot.row)-1]:=primitive;
          primitive^.pseudoIndex:=currentPlot.virtualRowIndex;
          inc(currentPlot.virtualRowIndex);
        finally
          leaveCriticalSection(currentPlot.plotCs);
        end;
      end;
      mt_plot_renderRequest: begin
        with P_plotRenderRequest(message)^ do case target of
          prt_pngString: setLiteral(globalLiteralRecycler.newStringLiteral(currentPlot.renderToString(width,height,true )));
          prt_bmpString: setLiteral(globalLiteralRecycler.newStringLiteral(currentPlot.renderToString(width,height,false)));
          prt_rawData  : setLiteral(currentPlot.renderToRawData(width,height));
          else begin
            {$ifdef enable_render_threads}
            if target=prt_fileInBackground then begin
              new(clonedPlot,createWithDefaults);
              clonedPlot^.copyFrom(currentPlot);
              if not(clonedPlot^.canRenderToFileInBackground(fileName,width,height,true,true,feedbackLocation,feedbackMessages))
              then dispose(clonedPlot,destroy);
            end else
            {$endif}
            currentPlot.renderToFile(fileName,   width,height,feedbackLocation,feedbackMessages);
            P_plotRenderRequest(message)^.setLiteral(newBoolLiteral(true));
          end;
        end;
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
    plotChangedSinceLastDisplay:=plotChangedSinceLastDisplay or (message^.messageType in [mt_plot_addText,mt_plot_addRow,mt_plot_dropRow,mt_plot_rasterImage,mt_plot_setOptions,mt_plot_clear,mt_plot_addAnimationFrame]);
    if (message^.messageType=mt_plot_renderRequest) and (gui_started<>ide) then plotChangedSinceLastDisplay:=false;
  end;

FUNCTION T_plotSystem.canStartGuiInteraction:boolean;
  begin
    if tryEnterCriticalsection(animation.seriesCs)<>0 then begin
      if tryEnterCriticalsection(currentPlot.plotCs)<>0 then begin
        result:=true;
        if length(currentPlot.row)=0 then begin
          leaveCriticalSection(currentPlot.plotCs);
          leaveCriticalSection(animation.seriesCs);
          exit(false);
        end;
      end else begin
        leaveCriticalSection(animation.seriesCs);
        result:=false;
      end;
    end else result:=false;
  end;

PROCEDURE T_plotSystem.startGuiInteraction;
  begin
    enterCriticalSection(animation.seriesCs);
    enterCriticalSection(currentPlot.plotCs);
  end;

PROCEDURE T_plotSystem.doneGuiInteraction;
  begin
    leaveCriticalSection(animation.seriesCs);
    leaveCriticalSection(currentPlot.plotCs);
  end;

FUNCTION T_plotSystem.isEmpty:boolean;
  begin
    result:=(length(currentPlot.row)=0) and (animation.frameCount=0);
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
      globalRowData:=globalLiteralRecycler.newListLiteral();
      setLength(result,2);
      result[0]:='#!'+paramStr(0)+' '+FLAG_GUI;
      result[1]:='plain script;';
      commands:='resetOptions;';
      myGenerics.append(commands,'clearAnimation;');
      prevOptions.setDefaults;
      if animation.frameCount>0 then begin
        if frameIndexOrNegativeIfAll<0 then begin
          for i:=0 to length(animation.frame)-1 do stepsTotal+=animation.frame[i]^.getRowStatementCount;
          progress.max:=stepsTotal*2;
          progress.position:=0;
          Application.ProcessMessages;
          for i:=0 to length(animation.frame)-1 do begin
            myGenerics.append(commands,animation.frame[i]^.getRowStatements(prevOptions,@globalLiteralRecycler,globalRowData^,haltExport,Application,progress));
            prevOptions:=animation.frame[i]^.scalingOptions;
            myGenerics.append(commands,'addAnimationFrame;');
          end;
        end else begin
          progress.max:=animation.frame[frameIndexOrNegativeIfAll]^.getRowStatementCount*2;
          Application.ProcessMessages;
          myGenerics.append(commands,animation.frame[frameIndexOrNegativeIfAll]^.getRowStatements(prevOptions,@globalLiteralRecycler,globalRowData^,haltExport,Application,progress));
        end;
      end else begin
        progress.max:=currentPlot.getRowStatementCount*2;
        Application.ProcessMessages;
        myGenerics.append(commands,currentPlot.getRowStatements(prevOptions,@globalLiteralRecycler,globalRowData^,haltExport,Application,progress));
      end;
      DataString:=base92Encode(
                     serialize(globalRowData,
                               dummyLocation,
                               nil,true,true));
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
      if length(result[length(result)-1])<151
      then result[length(result)-1]+='.deserialize;'
      else myGenerics.append(result, '.deserialize;');
      myGenerics.append(result,commands);
      myGenerics.append(result,'display;');
      setLength(commands,0);
    finally
      globalLiteralRecycler.disposeLiteral(globalRowData);
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
        mt_endOfEvaluation,
        mt_plot_postDisplay,
        mt_startOfEvaluation,
        mt_plot_addText,
        mt_plot_addRow,
        mt_plot_dropRow,
        mt_plot_rasterImage,
        mt_plot_setOptions,
        mt_plot_clear,
        mt_plot_clearAnimation,
        mt_plot_clearAnimationVolatile,
        mt_plot_retrieveOptions,
        mt_plot_renderRequest,
        mt_plot_queryClosedByUser: begin
          result:=true;
          //if there are pending tasks then store else process
          if sandboxed
          then processMessage(message)
          else inherited append(message);
        end;
        mt_plot_addAnimationFrame: begin
          if animation.volatile
          then P_plotAddAnimationFrameRequest(message)^.markExecuted((animation.frameCount-VOLATILE_FRAMES_MAX)/25,animation.frameCount)
          else P_plotAddAnimationFrameRequest(message)^.markExecuted(0,animation.frameCount);
          result:=true;
          //if there are pending tasks then store else process
          if sandboxed
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
      toProcessInThisRun:T_storedMessages=();
      m:P_storedMessage;
      start:double;
  begin
    if collectedFill=0 then exit([]);
    start:=now;

    if tryEnterCriticalsection(adapterCs)=0 then exit([]);
    setLength(toProcessInThisRun,collectedFill);
    for i:=0 to collectedFill-1 do toProcessInThisRun[i]:=collected[i];
    collectedFill:=0;
    clear;
    leaveCriticalSection(adapterCs);

    result:=[];
    //it does not make sense to render multiple plots in one run
    //Lookup the last display request;
    lastDisplayIndex:=-1;
    for i:=0 to length(toProcessInThisRun)-1 do if toProcessInThisRun[i]^.messageType=mt_plot_postDisplay then lastDisplayIndex:=i;
    //process messages
    for i:=0 to length(toProcessInThisRun)-1 do begin
      m:=toProcessInThisRun[i];
      include(result,m^.messageType);
      if m^.messageType=mt_plot_postDisplay
      then begin
        if i=lastDisplayIndex
        then processMessage(m)
        else P_plotDisplayRequest(m)^.markExecuted;
      end else processMessage(m);
      disposeMessage(m);
    end;
    if now-start>ONE_SECOND*0.1 then postIdeMessage('Flush of plot adapter form took a long time: '+myTimeToStr(now-start),true);
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
  MAJOR_TIC_STYLE.init; MAJOR_TIC_STYLE.styleModifier:=0.2;
  MINOR_TIC_STYLE.init; MINOR_TIC_STYLE.styleModifier:=0.1;
{$ifdef enable_render_threads}
FINALIZATION
  while preparationThreadsRunning>0 do sleep(100);
{$endif}

end.
