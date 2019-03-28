UNIT mnh_plotData;
INTERFACE
USES sysutils,
     math,
     Interfaces, Classes, ExtCtrls, Graphics, types,
     mySys,myGenerics,
     basicTypes, mnh_constants,
     mnh_settings,
     mnh_messages,
     out_adapters,
     plotstyles,plotMath;
TYPE
  T_plotQuality=0..3;
CONST
  PLOT_QUALITY_LOW     =0;
  PLOT_QUALITY_MEDIUM_1=1;
  PLOT_QUALITY_MEDIUM_2=2;
  PLOT_QUALITY_HIGH    =3;
TYPE
  T_boundingBox = array['x'..'y', 0..1] of double;

  P_addRowMessage=^T_addRowMessage;
  T_addRowMessage=object(T_payloadMessage)
    protected
      FUNCTION internalType:shortstring; virtual;
    public
      styleOptions: string;
      rowData:T_dataRow;
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
      retrieved:boolean;
    protected
      FUNCTION internalType:shortstring; virtual;
    public
      CONSTRUCTOR createRetrieveRequest;
      CONSTRUCTOR createPostRequest(CONST o:T_scalingOptions);
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
    protected
      FUNCTION internalType:shortstring; virtual;
    public
      CONSTRUCTOR create(CONST numberOfRowsToDrop:longint);
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
      FUNCTION getRowStatements(CONST prevOptions:T_scalingOptions):T_arrayOfString;
  end;

  P_plotSeriesFrame=^T_plotSeriesFrame;
  T_plotSeriesFrame=object
    private
      backgroundPreparation:boolean;
      frameCS:TRTLCriticalSection;
      image:TImage;
      dumpIsUpToDate:boolean;
      dumpName:string;
      postedWidth  ,renderedWidth  ,
      postedHeight ,renderedHeight :longint;
      postedQuality,renderedQuality:byte;
      plotData:T_plot;
      PROCEDURE performPostedPreparation;
    public
      CONSTRUCTOR create(VAR currentPlot:T_plot);
      DESTRUCTOR destroy;
      PROCEDURE invalidate;
      PROCEDURE clearImage(CONST doDump:boolean=false);
      PROCEDURE obtainImage(VAR target:TImage; CONST quality:byte);
      PROCEDURE prepareImage(CONST width,height:longint; CONST quality:byte);
      PROCEDURE postPreparation(CONST width,height:longint; CONST quality:byte);
  end;

  T_plotSeries=object
    private
      frame:array of P_plotSeriesFrame;
      framesWithImagesAllocated:array[0..7] of P_plotSeriesFrame;
      tryToKeepMemoryLow:boolean;
      seriesCs:TRTLCriticalSection;
      FUNCTION getOptions(CONST index:longint):T_scalingOptions;
      PROCEDURE setOptions(CONST index:longint; CONST value:T_scalingOptions);
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE clear;
      FUNCTION frameCount:longint;
      PROCEDURE getFrame(VAR target:TImage; CONST frameIndex:longint; CONST quality:byte);
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
    private
      plotChangedSinceLastDisplay:boolean;
      displayImmediate           :boolean;
      doPlot:F_execPlotCallback;
      sandboxed:boolean;
      PROCEDURE processMessage(CONST message:P_storedMessage);
    public
      currentPlot:T_plot;
      animation:T_plotSeries;

      CONSTRUCTOR create(CONST executePlotCallback:F_execPlotCallback; CONST isSandboxSystem:boolean);
      DESTRUCTOR destroy; virtual;
      FUNCTION append(CONST message:P_storedMessage):boolean; virtual;
      FUNCTION requiresFastPolling:boolean;
      FUNCTION flushToGui:T_messageTypeSet; virtual;
      PROCEDURE logPlotDone;
      PROCEDURE startGuiInteraction;
      PROCEDURE doneGuiInteraction;
      FUNCTION getPlotStatement(CONST frameIndexOrNegativeIfAll:longint):T_arrayOfString;
  end;

FUNCTION newPlotSystemWithoutDisplay:P_plotSystem;
FUNCTION getOptionsViaAdapters(CONST messages:P_messages):T_scalingOptions;
IMPLEMENTATION
USES FPReadBMP,FPWriteBMP,IntfGraphics;

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

CONSTRUCTOR T_plotSeriesFrame.create(VAR currentPlot: T_plot);
  begin
    initCriticalSection(frameCS);
    enterCriticalSection(frameCS);
    image:=nil;
    dumpIsUpToDate:=false;
    dumpName:='';
    renderedWidth :=-1;
    renderedHeight:=-1;
    renderedQuality:=255;
    backgroundPreparation:=false;
    plotData.createWithDefaults;
    plotData.copyFrom(currentPlot);
    leaveCriticalSection(frameCS);
  end;

DESTRUCTOR T_plotSeriesFrame.destroy;
  begin
    enterCriticalSection(frameCS);
    clearImage(false);
    if dumpName<>'' then DeleteFile(dumpName);
    dumpName:='';
    renderedWidth :=-1;
    renderedHeight:=-1;
    renderedQuality:=255;
    plotData.destroy;
    leaveCriticalSection(frameCS);
    doneCriticalSection(frameCS);
  end;

PROCEDURE T_plotSeriesFrame.invalidate;
  begin
    enterCriticalSection(frameCS);
    clearImage(false);
    renderedWidth :=-1;
    renderedHeight:=-1;
    renderedQuality:=255;
    leaveCriticalSection(frameCS);
  end;

PROCEDURE T_plotSeriesFrame.clearImage(CONST doDump:boolean=false);
  VAR tempIntfImage: TLazIntfImage;
      bmpWriter:TFPWriterBMP;
  begin
    enterCriticalSection(frameCS);
    if doDump and not(dumpIsUpToDate) then begin
      bmpWriter:=TFPWriterBMP.create;
      if dumpName='' then dumpName:=getTempFileName;
      tempIntfImage:=image.picture.Bitmap.CreateIntfImage;
      tempIntfImage.saveToFile(dumpName,bmpWriter);
      tempIntfImage.free;
      bmpWriter.free;
    end;
    if not(doDump) then dumpName:='';
    if image<>nil then FreeAndNil(image);
    leaveCriticalSection(frameCS);
  end;

PROCEDURE T_plotSeriesFrame.prepareImage(CONST width,height:longint; CONST quality:byte);
  VAR tempIntfImage: TLazIntfImage;
      bmpReader:TFPReaderBMP;
  begin
    enterCriticalSection(frameCS);
    if (renderedQuality=quality) and
       (renderedHeight =height) and
       (renderedWidth  =width) and
       ((image<>nil) or (dumpName<>'') and fileExists(dumpName)) then begin
      if image=nil then begin
        //load image
        image:=TImage.create(nil);
        image.SetInitialBounds(0,0,width,height);
        tempIntfImage:=image.picture.Bitmap.CreateIntfImage;
        bmpReader:=TFPReaderBMP.create;
        tempIntfImage.loadFromFile(dumpName,bmpReader);
        bmpReader.destroy;
        image.picture.Bitmap.LoadFromIntfImage(tempIntfImage);
        dumpIsUpToDate:=true;
        tempIntfImage.free;
      end;
    end else begin
      enterCriticalSection(globalTextRenderingCs);
      if image<>nil then FreeAndNil(image);
      leaveCriticalSection(globalTextRenderingCs);
      image:=plotData.obtainPlot(width,height,quality);
      dumpIsUpToDate:=false;
      renderedQuality:=quality;
      renderedHeight :=height;
      renderedWidth  :=width;
    end;
    leaveCriticalSection(frameCS);
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
    backgroundPreparation:=true;
    postedHeight:=height;
    postedWidth:=width;
    postedQuality:=quality;
    beginThread(@preparationThread,@self);
    leaveCriticalSection(frameCS);
  end;

PROCEDURE T_plotSeriesFrame.obtainImage(VAR target: TImage; CONST quality: byte);
  begin
    enterCriticalSection(frameCS);
    prepareImage(target.width,target.height,quality);
    target.Canvas.draw(0,0,image.picture.Bitmap);
    leaveCriticalSection(frameCS);
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

CONSTRUCTOR T_plotDropRowRequest.create(CONST numberOfRowsToDrop: longint);
  begin
    inherited create(mt_plot_dropRow);
    count:=numberOfRowsToDrop;
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

CONSTRUCTOR T_plotOptionsMessage.createPostRequest(CONST o: T_scalingOptions);
  begin
    inherited create(mt_plot_setOptions);
    retrieved:=true;
    options:=o;
  end;

PROCEDURE T_plotOptionsMessage.setOptions(CONST o: T_scalingOptions);
  begin
    enterCriticalSection(messageCs);
    options:=o;
    retrieved:=true;
    leaveCriticalSection(messageCs);
  end;

FUNCTION T_plotOptionsMessage.getOptionsWaiting(CONST errorFlagProvider:P_messages):T_scalingOptions;
  VAR timeout:double;
  begin
    timeout:=now+5/(24*60*60);
    enterCriticalSection(messageCs);
    while not(retrieved) and (errorFlagProvider^.continueEvaluation) and (now<timeout) do begin
      leaveCriticalSection(messageCs);
      sleep(1); ThreadSwitch;
      enterCriticalSection(messageCs);
    end;
    result:=options;
    leaveCriticalSection(messageCs);
    if now>=timeout then raise Exception.create('T_plotOptionsMessage.getOptionsWaiting timed out');
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
    frame[index]^.invalidate;
    leaveCriticalSection(seriesCs);
  end;

CONSTRUCTOR T_plotSeries.create;
  begin
    setLength(frame,0);
    initCriticalSection(seriesCs);
    clear;
  end;

DESTRUCTOR T_plotSeries.destroy;
  begin
    clear;
    doneCriticalSection(seriesCs);
  end;

PROCEDURE T_plotSeries.clear;
  VAR k:longint;
  begin
    enterCriticalSection(seriesCs);
    for k:=0 to length(frame)-1 do dispose(frame[k],destroy);
    setLength(frame,0);
    for k:=0 to length(framesWithImagesAllocated)-1 do framesWithImagesAllocated[k]:=nil;
    leaveCriticalSection(seriesCs);
    tryToKeepMemoryLow:=not(isMemoryInComfortZone);
  end;

FUNCTION T_plotSeries.frameCount: longint;
  begin
    enterCriticalSection(seriesCs);
    result:=length(frame);
    leaveCriticalSection(seriesCs);
  end;

PROCEDURE T_plotSeries.getFrame(VAR target: TImage; CONST frameIndex: longint; CONST quality: byte);
  VAR current:P_plotSeriesFrame;

  PROCEDURE handleImagesToFree;
    VAR k,j:longint;
    begin
      tryToKeepMemoryLow:=tryToKeepMemoryLow or not(isMemoryInComfortZone);
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
      if (framesWithImagesAllocated[k]<>nil) and (tryToKeepMemoryLow or not(settings.cacheAnimationFrames)) then framesWithImagesAllocated[k]^.clearImage(settings.cacheAnimationFrames);
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
      current^.obtainImage(target,quality);
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
    {$ifndef unix}
    if exportingAll and (index+settings.cpuCount-1<length(frame)) then frame[index+settings.cpuCount-1]^.postPreparation(width,height,quality);
    {$endif}

    storeImage:=TImage.create(nil);
    storeImage.SetInitialBounds(0,0,width,height);
    frame[index]^.obtainImage(storeImage,quality);
    storeImage.picture.PNG.saveToFile(ChangeFileExt(fileName, '.png'));
    enterCriticalSection(globalTextRenderingCs);
    storeImage.destroy;
    leaveCriticalSection(globalTextRenderingCs);
    frame[index]^.clearImage(false);
    leaveCriticalSection(seriesCs);
  end;

PROCEDURE T_plotSeries.addFrame(VAR plot: T_plot);
  VAR newIdx:longint;
  begin
    enterCriticalSection(seriesCs);
    newIdx:=length(frame);
    setLength(frame,newIdx+1);
    new(frame[newIdx],create(plot));
    leaveCriticalSection(seriesCs);
  end;

FUNCTION T_plotSeries.nextFrame(VAR frameIndex: longint; CONST cycle:boolean; CONST width,height,quality:longint):boolean;
  {$ifndef unix}VAR nextToPrepare:longint;{$endif}
  begin
    enterCriticalSection(seriesCs);
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
    leaveCriticalSection(seriesCs);
  end;

CONSTRUCTOR T_plot.createWithDefaults;
  begin
    system.initCriticalSection(cs);
    setDefaults;
  end;

PROCEDURE T_plot.setDefaults;
  begin
    system.enterCriticalSection(cs);
    scalingOptions.setDefaults;
    clear;
    system.leaveCriticalSection(cs);
  end;

DESTRUCTOR T_plot.destroy;
  begin
    system.enterCriticalSection(cs);
    clear;
    system.leaveCriticalSection(cs);
    doneCriticalSection(cs);
  end;

PROCEDURE T_plot.clear;
  VAR i: longint;
  begin
    system.enterCriticalSection(cs);
    for i:=0 to length(row)-1 do row[i].destroy;
    setLength(row, 0);
    for i:=0 to length(customText)-1 do dispose(customText[i],destroy);
    setLength(customText,0);
    transparentCount:=0;
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_plot.addRow(CONST styleOptions: string; CONST rowData: T_dataRow);
  VAR index:longint;
  begin
    system.enterCriticalSection(cs);
    index:=length(row);
    setLength(row, index+1);
    row[index].create(index,rowData);
    if trim(styleOptions)<>'' then row[index].style.parseStyle(styleOptions,transparentCount);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_plot.removeRows(CONST numberOfRowsToRemove: longint);
  VAR i0,i:longint;
  begin
    if numberOfRowsToRemove<=0 then exit;
    system.enterCriticalSection(cs);
    i0:=length(row)-numberOfRowsToRemove; if i0<0 then i0:=0;
    for i:=i0 to length(row)-1 do row[i].destroy;
    setLength(row,i0);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_plot.addCustomText(CONST text: P_customText);
  begin
    system.enterCriticalSection(cs);
    setLength(customText,length(customText)+1);
    customText[length(customText)-1]:=text;
    system.leaveCriticalSection(cs);
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
    system.leaveCriticalSection(cs);
  end; end;

PROCEDURE T_plot.panByPixels(CONST pixelDX, pixelDY: longint;
  VAR plotImage: TImage);
  VAR rectA, rectB: TRect;
  begin with scalingOptions do begin
    system.enterCriticalSection(cs);
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
    system.leaveCriticalSection(cs);
  end; end;

PROCEDURE T_plot.drawGridAndRows(CONST target: TCanvas; CONST intendedWidth, intendedHeight, scalingFactor: longint; VAR gridTic: T_ticInfos; CONST sampleIndex: byte);
  CONST darts_delta:array[0..4,0..1] of single=(( 0.12, 0.24),
                                                (-0.12,-0.24),
                                                ( 0.24,-0.12),
                                                (-0.24, 0.12),
                                                (0,0));
  VAR rowId, i,j, yBaseLine:longint;
      lastX: longint = 0;
      lastY: longint = 0;
      lastWasValid: boolean;
      scaleAndColor:T_scaleAndColor;
      screenRow:T_rowToPaint;
      screenBox:T_boundingBox;

  PROCEDURE screenRowPoly(CONST i0,i1:longint);
    VAR points:array of TPoint;
        i:longint;
    begin
      if (i0<0) or (i1<i0+1) then exit;
      if (scaleAndColor.solidStyle=bsClear) and (scaleAndColor.lineWidth<1) then exit;
      target.Brush.color:=scaleAndColor.solidColor;
      target.Brush.style:=scaleAndColor.solidStyle;
      setLength(points,i1-i0+1);
      for i:=0 to i1-i0 do begin
        points[i].x:=screenRow[i0+i].x;
        points[i].y:=screenRow[i0+i].y;
      end;
      if (scaleAndColor.lineWidth<0) then target.Pen.style:=psClear;
      target.Polygon(points);
      if (scaleAndColor.lineWidth<0) then target.Pen.style:=psSolid;
    end;

  PROCEDURE drawCustomQuad(CONST x0,y0,x1,y1,x2,y2,x3,y3:longint; CONST withBorder:boolean);
    VAR points:array[0..4] of TPoint;
    begin
      if (scaleAndColor.solidStyle=bsClear) and not(withBorder) then exit;
      target.Brush.color:=scaleAndColor.solidColor;
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

  PROCEDURE drawEllipse(CONST x0,y0,x1,y1:longint);
    begin
      if not(intersect(screenBox,boundingBoxOf(x0,y0,x1,y1))) or ((scaleAndColor.solidStyle=bsClear) and (scaleAndColor.lineWidth<1)) then exit;
      target.Brush.color:=scaleAndColor.solidColor;
      target.Brush.style:=scaleAndColor.solidStyle;
      if (scaleAndColor.lineWidth<1) then target.Pen.style:=psClear;
      target.Ellipse(x0,y0,x1,y1);
      if (scaleAndColor.lineWidth<1) then target.Pen.style:=psSolid;
    end;

  PROCEDURE drawPatternRect(CONST x0, y0, x1, y1: longint; CONST withBorder:boolean);
    begin
      drawCustomQuad(x0,y0,
                     x0,yBaseLine,
                     x1,yBaseLine,
                     x1,y1,withBorder);
    end;

  begin
    screenBox:=boundingBoxOf(0,0,intendedWidth*scalingFactor,intendedHeight*scalingFactor);
    target.LockCanvas;
    //Clear:------------------------------------------------------------------
    target.Brush.style:=bsSolid;
    target.Brush.color:=clWhite;
    target.Pen.style:=psClear;
    target.Pen.EndCap:=pecSquare;
    target.FillRect(0, 0, intendedWidth*scalingFactor, intendedHeight*scalingFactor);

    //------------------------------------------------------------------:Clear
    //coordinate grid:========================================================
    target.Pen.style:=psSolid;
    //minor grid:-------------------------------------------------------------
    scaleAndColor:=MINOR_TIC_STYLE.getLineScaleAndColor(intendedWidth*scalingFactor,intendedHeight*scalingFactor,SINGLE_SAMPLE_INDEX);
    target.Pen.color:=scaleAndColor.lineColor;
    target.Pen.width:=scaleAndColor.lineWidth;
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
    target.Pen.color:=scaleAndColor.lineColor;
    target.Pen.width:=scaleAndColor.lineWidth;
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
      if ps_straight in row[rowId].style.style then begin
        target.Pen.style:=psSolid;
        target.Pen.color:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecRound;
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
      end else if ps_stepLeft in row[rowId].style.style then begin
        target.Pen.style:=psSolid;
        target.Pen.color:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecRound;
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
      end else if ps_stepRight in row[rowId].style.style then begin
        target.Pen.style:=psSolid;
        target.Pen.color:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecRound;
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
      end else if ps_bar in row[rowId].style.style then begin
        target.Pen.style:=psSolid;
        target.Pen.color:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecRound;
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
      end else if ps_box in row[rowId].style.style then begin
        target.Pen.style:=psSolid;
        target.Pen.color:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecRound;
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
      end else if ps_ellipse in row[rowId].style.style then begin
        target.Pen.style:=psSolid;
        target.Pen.color:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecRound;
        lastWasValid:=false;
        i:=0;
        while i+1<length(screenRow) do begin
          if screenRow[i  ].valid and
             screenRow[i+1].valid then
            drawEllipse(screenRow[i  ].x, screenRow[i  ].y,
                        screenRow[i+1].x, screenRow[i+1].y);
          inc(i, 2);
        end;
      end else if ps_tube in row[rowId].style.style then begin
        i:=0;
        while i+3<length(screenRow) do begin
          target.Pen.style:=psSolid;
          target.Pen.color:=scaleAndColor.lineColor;
          target.Pen.width:=scaleAndColor.lineWidth;
          target.Pen.EndCap:=pecRound;
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
      if ps_polygon in row[rowId].style.style then begin
        target.Pen.style:=psSolid;
        target.Pen.color:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecRound;
        j:=-1;
        for i:=0 to length(screenRow)-1 do if not(screenRow[i].valid) then begin
          if j>=0 then screenRowPoly(j,i-1);
          j:=-1;
        end else if j<0 then j:=i;
        i:=length(screenRow)-1;
        if j>=0 then screenRowPoly(j,i);
      end;
      if ps_dot in row[rowId].style.style then begin
        target.Pen.style:=psClear;
        target.Brush.style:=bsSolid;
        target.Brush.color:=scaleAndColor.solidColor;
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
      if ps_plus in row[rowId].style.style then begin
        target.Pen.style:=psSolid;
        target.Pen.color:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecRound;
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
      if ps_cross in row[rowId].style.style then begin
        target.Pen.style:=psSolid;
        target.Pen.color:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecRound;
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
      if ps_impulse in row[rowId].style.style then begin
        target.Pen.style:=psSolid;
        target.Pen.color:=scaleAndColor.lineColor;
        target.Pen.width:=scaleAndColor.lineWidth;
        target.Pen.EndCap:=pecSquare;
        for i:=0 to length(screenRow)-1 do if screenRow[i].valid then
          target.line(screenRow[i].x,
                      yBaseLine,
                      screenRow[i].x,
                      screenRow[i].y);
      end;
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
      target.Brush.style:=bsSolid;
      target.Brush.color:=clWhite;
      target.Pen.style:=psClear;
      target.Pen.width:=1;
      target.Pen.EndCap:=pecSquare;
      if (scalingOptions.axisStyle['y']<>[]) then target.FillRect(0,0,cSysX,intendedHeight);
      if (scalingOptions.axisStyle['x']<>[]) then target.FillRect(cSysX,cSysY,intendedWidth,intendedHeight);
      //-----------------------------------------------------------:clear border
      //coordinate system:======================================================
      //axis:-------------------------------------------------------------------
      target.Pen.style:=psSolid;
      target.Pen.color:=clBlack;
      target.Pen.width:=1;
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
    transparentCount:=p.transparentCount;
    scalingOptions:=p.scalingOptions;
    //copy rows:
    for i:=0 to length(row)-1 do row[i].destroy;
    setLength(row,length(p.row));
    for i:=0 to length(row)-1 do begin
      p.row[i].sample.cloneTo(clonedSample);
      row[i].create(i,clonedSample);
      row[i].style:=p.row[i].style;
    end;
    //:copy rows | copy custom text:
    for i:=0 to length(customText)-1 do dispose(customText[i],destroy);
    setLength(customText,length(p.customText));
    for i:=0 to length(customText)-1 do customText[i]:=p.customText[i]^.clone;
    //:copy custom text
    system.leaveCriticalSection(p.cs);
    system.leaveCriticalSection(cs);
  end;

FUNCTION T_plot.getRowStatements(CONST prevOptions:T_scalingOptions):T_arrayOfString;
  VAR opt:string;
      i:longint;
  begin
    system.enterCriticalSection(cs);
    opt:=scalingOptions.getOptionDiffString(prevOptions);
    if opt='' then setLength(result,0) else result:=opt;
    for i:=0 to length(row)-1 do append(result,row[i].toPlotStatement(i=0));
    for i:=0 to length(customText)-1 do append(result,customText[i]^.toTextStatement);
    system.leaveCriticalSection(cs);
  end;

PROCEDURE T_plotSystem.processMessage(CONST message: P_storedMessage);
  begin
    case message^.messageType of
      mt_startOfEvaluation: begin
        if settings.doResetPlotOnEvaluation or sandboxed
        then currentPlot.setDefaults
        else currentPlot.clear;
        animation.clear;
        if pullSettingsToGui<>nil then pullSettingsToGui();
      end;
      mt_plot_addText:
        currentPlot.addCustomText(P_addTextMessage(message)^.customText);
      mt_plot_addRow : begin
        currentPlot.addRow(P_addRowMessage(message)^.styleOptions,P_addRowMessage(message)^.rowData);
        P_addRowMessage(message)^.styleOptions:='';
      end;
      mt_plot_dropRow:
        currentPlot.removeRows(P_plotDropRowRequest(message)^.count);
      mt_plot_renderRequest: begin
        with P_plotRenderRequest(message)^ do if isRenderToStringRequest
        then setString(currentPlot.renderToString(width,height,quality))
        else currentPlot.renderToFile(fileName,   width,height,quality);
        P_plotRenderRequest(message)^.fileName:='';
      end;
      mt_plot_retrieveOptions:
        P_plotOptionsMessage(message)^.setOptions(currentPlot.scalingOptions);
      mt_plot_setOptions:
        currentPlot.scalingOptions:=P_plotOptionsMessage(message)^.options;
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
    enterCriticalSection(cs);
  end;

PROCEDURE T_plotSystem.doneGuiInteraction;
  begin
    leaveCriticalSection(cs);
  end;

FUNCTION T_plotSystem.getPlotStatement(CONST frameIndexOrNegativeIfAll:longint):T_arrayOfString;
  VAR prevOptions:T_scalingOptions;
      i:longint;
  begin
    enterCriticalSection(cs);
    result:='plain script;';
    myGenerics.append(result,'resetOptions;');
    myGenerics.append(result,'clearAnimation;');
    prevOptions.setDefaults;
    if animation.frameCount>0 then begin
      if frameIndexOrNegativeIfAll<0 then for i:=0 to length(animation.frame)-1 do begin
        myGenerics.append(result,animation.frame[i]^.plotData.getRowStatements(prevOptions));
        prevOptions:=animation.frame[i]^.plotData.scalingOptions;
        myGenerics.append(result,'addAnimationFrame;');
      end else begin
        myGenerics.append(result,animation.frame[frameIndexOrNegativeIfAll]^.plotData.getRowStatements(prevOptions));
      end;
    end else begin
      myGenerics.append(result,currentPlot.getRowStatements(prevOptions));
    end;
    myGenerics.append(result,'display;');
    leaveCriticalSection(cs);
  end;

CONSTRUCTOR T_plotSystem.create(CONST executePlotCallback:F_execPlotCallback; CONST isSandboxSystem:boolean);
  begin
    inherited create(at_plot,C_includableMessages[at_plot]);
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
    enterCriticalSection(cs);
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
    leaveCriticalSection(cs);
  end;

FUNCTION T_plotSystem.requiresFastPolling:boolean;
  begin
    enterCriticalSection(cs);
    result:=displayImmediate or (animation.frameCount<>0);
    leaveCriticalSection(cs);
  end;

FUNCTION T_plotSystem.flushToGui:T_messageTypeSet;
  VAR lastDisplayIndex:longint;
      i:longint;
  begin
    enterCriticalSection(cs);
    result:=[];
    //it does not make sense to render multiple plots in one run
    //Lookup the last display request;
    lastDisplayIndex:=-1;
    for i:=0 to length(storedMessages)-1 do if storedMessages[i]^.messageType=mt_plot_postDisplay then lastDisplayIndex:=i;
    //process messages
    for i:=0 to length(storedMessages)-1 do begin
      include(result,storedMessages[i]^.messageType);
      if storedMessages[i]^.messageType=mt_plot_postDisplay
      then begin
        if i=lastDisplayIndex
        then processMessage(storedMessages[i])
        else P_plotDisplayRequest(storedMessages[i])^.markExecuted;
      end else processMessage(storedMessages[i]);
    end;
    clear;
    leaveCriticalSection(cs);
  end;

PROCEDURE T_plotSystem.logPlotDone;
  begin
    enterCriticalSection(cs);
    plotChangedSinceLastDisplay:=false;
    leaveCriticalSection(cs);
  end;

INITIALIZATION
  MAJOR_TIC_STYLE.create(0); MAJOR_TIC_STYLE.styleModifier:=0.2;
  MINOR_TIC_STYLE.create(0); MINOR_TIC_STYLE.styleModifier:=0.1;
FINALIZATION
  MAJOR_TIC_STYLE.destroy;
  MINOR_TIC_STYLE.destroy;
end.
