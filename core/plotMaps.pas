UNIT plotMaps;
INTERFACE
USES sysutils,math,
     Graphics,
     IntfGraphics,FPimage;
TYPE
  T_wordColMap=object
    private
      data:PFPColor;
      xRes,yRes:longint;
      sampleCount:longint;
      tempIntfImage: TLazIntfImage;
    public
      CONSTRUCTOR create(CONST width,height:longint);
      DESTRUCTOR destroy;
      PROCEDURE addSample(CONST Bitmap:TBitmap);
      PROCEDURE obtainAveragedResult(CONST picture:TPicture);
  end;
IMPLEMENTATION
CONSTRUCTOR T_wordColMap.create(CONST width, height: longint);
  VAR i:longint;
  begin
    xRes:=width;
    yRes:=height;
    sampleCount:=0;
    getMem(data,sizeOf(TFPColor)*xRes*yRes);
    tempIntfImage:=nil;
    for i:=0 to xRes*yRes-1 do with data[i] do begin
      RED:=0;
      GREEN:=0;
      BLUE:=0;
      alpha:=0;
    end;
  end;

DESTRUCTOR T_wordColMap.destroy;
  begin
    if tempIntfImage<>nil then FreeAndNil(tempIntfImage);
    freeMem(data,sizeOf(TFPColor)*xRes*yRes);
  end;

PROCEDURE T_wordColMap.addSample(CONST Bitmap:TBitmap);
  VAR X, Y: integer;
      p:PFPColor;
      b:PByte;
  begin
    try
      if tempIntfImage=nil then tempIntfImage:=Bitmap.CreateIntfImage
                           else tempIntfImage.LoadFromBitmap(Bitmap.handle,Bitmap.MaskHandle);
      for y:=0 to min(yRes,Bitmap.height)-1 do begin
        p:=data+y*xRes;
        b:=tempIntfImage.GetDataLineStart(y);
        for x:=0 to min(xRes,Bitmap.width )-1 do begin
          inc(p^.BLUE ,b^); inc(b);
          inc(p^.GREEN,b^); inc(b);
          inc(p^.RED  ,b^); inc(b);
          inc(p);
        end;
      end;
    finally
      inc(sampleCount);
    end;
  end;

PROCEDURE T_wordColMap.obtainAveragedResult(CONST picture:TPicture);
  VAR X, Y: integer;
      p:PFPColor;
      b:PByte;
  begin
    try
      if tempIntfImage=nil then tempIntfImage:=picture.Bitmap.CreateIntfImage;
      if sampleCount=4
      then for y:=0 to min(yRes,picture.Bitmap.height)-1 do begin
        p:=data+y*xRes;
        b:=tempIntfImage.GetDataLineStart(y);
        for x:=0 to min(xRes,picture.Bitmap.width )-1 do begin
          b^:=(p^.BLUE  shr 2); inc(b);
          b^:=(p^.GREEN shr 2); inc(b);
          b^:=(p^.RED   shr 2); inc(b);
          inc(p);
        end;
      end
      else for y:=0 to min(yRes,picture.Bitmap.height)-1 do begin
        p:=data+y*xRes;
        b:=tempIntfImage.GetDataLineStart(y);
        for x:=0 to min(xRes,picture.Bitmap.width )-1 do begin
          b^:=(p^.BLUE  div sampleCount); inc(b);
          b^:=(p^.GREEN div sampleCount); inc(b);
          b^:=(p^.RED   div sampleCount); inc(b);
          inc(p);
        end;
      end;
      picture.Bitmap.LoadFromIntfImage(tempIntfImage);
    finally
      inc(sampleCount);
    end;
  end;

end.
