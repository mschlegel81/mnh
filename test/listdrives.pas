PROGRAM listdevices;

{$ifdef fpc}{$mode delphi}{$endif}
{$apptype console}

USES
  windows;

FUNCTION getVolumeLabel(CONST DriveChar: char): string;
  VAR
    NotUsed:     dword;
    VolumeFlags: dword;
    VolumeInfo:  array[0..MAX_PATH] of char;
    VolumeSerialNumber: dword;
    Buf: array [0..MAX_PATH] of char;
  begin
      GetVolumeInformation(PChar(DriveChar + ':\'),
      Buf, SizeOf(VolumeInfo), @VolumeSerialNumber, NotUsed,
      VolumeFlags, nil, 0);

      writeln(VolumeSerialNumber,' ',VolumeFlags);
      SetString(result, Buf, StrLen(Buf));
  end;

FUNCTION getVolumeType(CONST drive:char):string;
  VAR DriveLetter: string;
  begin
    DriveLetter := drive + ':\';
    case GetDriveType(PChar(DriveLetter)) of
      DRIVE_REMOVABLE: result:='removable';
      DRIVE_FIXED:     result:='fixed';
      DRIVE_REMOTE:    result:='network';
      DRIVE_CDROM:     result:='CD_ROM';
      DRIVE_RAMDISK:   result:='RAM_disk';
      else result:='';
    end;
  end;


VAR
  drive: char;

  OldMode: word;
begin
  writeln(DRIVE_REMOVABLE);
  writeln(DRIVE_FIXED);
  writeln(DRIVE_REMOTE);
  writeln(DRIVE_CDROM);
  writeln(DRIVE_RAMDISK);


  writeln('The following drives were found in this computer:');
  writeln('');

  // Empty Floppy or Zip drives can generate a Windows error.
  // We disable system errors during the listing.
  // Note that another way to skip these errors would be to use DEVICE_IO_CONTROL.
  OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try

    // Search all drive letters
    for drive := 'A' to 'Z' do
    begin
      writeln(drive,' ',getVolumeType(drive),' "',getVolumeLabel(drive),'"');
//
//      DriveLetter := Drive + ':\';
//
//      case GetDriveType(PChar(DriveLetter)) of
//       DRIVE_REMOVABLE: WriteLn(DriveLetter, ' removable "', getVolumeLabel(Drive),'"');
//       DRIVE_FIXED:     WriteLn(DriveLetter, ' fixed     "', getVolumeLabel(Drive),'"');
//       DRIVE_REMOTE:    WriteLn(DriveLetter, ' network   "', getVolumeLabel(Drive),'"');
//       DRIVE_CDROM:     WriteLn(DriveLetter, ' CD-ROM    "', getVolumeLabel(Drive),'"');
//       DRIVE_RAMDISK:   WriteLn(DriveLetter, ' RAM disk  "', getVolumeLabel(Drive),'"');
//      end;
    end;

  finally
    // Restores previous Windows error mode.
    SetErrorMode(OldMode);
  end;
end.
