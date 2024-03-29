unit SIXControlUnit;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Crt, Streamex,
  Dialogs, StdCtrls, ExtCtrls, Spin, Buttons, ComCtrls, LazFileUtils,
  TAGraph, TASeries, TATools, TAChartUtils, TADrawerSVG, TAFuncSeries, Math,
  Types, TATextElements, TALegend, TACustomSeries, TAChartAxis, ceAxisFrame,
  TATypes, TAGeometry, TAChartLiveView, StrUtils, DateUtils, SynaSer,
  // custom forms
  JTDriverSensingMain, NameSetting, NoteEditing;

type

  TSIXControl = class
    constructor create;
    procedure SCScrollViewCBChange(Sender: TObject);
    procedure SCDataPointHintToolHint(ATool: TDataPointHintTool;
      const APoint{%H-}: TPoint; var AHint: String);
    procedure SCDataPointHintToolHintPosition(
       ATool: TDataPointHintTool; var APoint: TPoint);
    procedure SCSaveCSVResultBClick(Sender: TObject);
    procedure SCSaveScreenshotLiveBClick(Sender: TObject);
    procedure SCSaveScreenshotResultBClick(Sender: TObject);
    procedure SCStartFitBClick(Sender: TObject);
    procedure SCChannelXLEChange(Sender: TObject);
    procedure SCChannelXCBChange(Sender: TObject);
    procedure SCChannelXGBDblClick(Sender: TObject);
    procedure SCNoSubtractBlankCBChange(Sender: TObject);
    procedure SCRawCurrentCBChange(Sender: TObject);
    procedure SCReadTimerTimerFinished(Sender: TObject);
    procedure SCTitleFootClickToolClick(Sender: TChartTool;
      Title: TChartTitle);
    procedure SCZoomDragToolAfterMouseUp(ATool{%H-}: TChartTool;
      APoint{%H-}: TPoint);
    procedure SCLegendClickToolClick(Sender: TChartTool;
      Legend: TChartLegend);
    procedure SCAxisClickToolClick(Sender{%H-}: TChartTool;
      AnAxis: TChartAxis; HitInfo: TChartAxisHitTests);
    procedure SCAppearanceXBBClick(Sender: TObject);
    procedure SCChannelXOnOffCBChange(Sender: TObject);
    procedure SCShowTempCBChange(Sender: TObject);
    procedure SCAnOutConnectorXOnOffCBChange(Sender: TObject);
    procedure SCAnOutOfXLEChange(Sender: TObject);
    procedure SCAnOutOnOffTBChange(Sender: TObject);
    procedure SCCalibrateTBChange(Sender: TObject; aborted: Boolean = false);
    procedure SCPerformAutoCalib(CalibSubstance: Substance);
    procedure SCChangeBackColorMIClick(Sender: TObject);
    procedure SCNoTempCorrectionCBChange(Sender: TObject);
    procedure SCSaveAppearance(iniFile: string);
    procedure SCLoadAppearance(iniFile: string);
    procedure SCDataPointClickToolPointClick(ATool: TChartTool;
      APoint{%H-}: TPoint);
    procedure SCTimeDayMIClick(Sender: TObject);
    procedure SCTimeHourMIClick(Sender: TObject);
    procedure SCTimeMinuteMIClick(Sender: TObject);
    procedure SCSIXCHAxisList1GetMarkText(Sender: TObject; var AText: String;
      AMark: Double);
    procedure SCAutoscaleMIClick(Sender: TObject);
    procedure SCHideNotesMIClick(Sender: TObject);
    procedure TimeXMIClick(Sender: TObject; factor: integer);
    procedure SubstMeasureCLBItemClick(ASender: TObject; AIndex: Integer);

  private

  public
    function SaveCSV(Overwrite: Boolean; ChartName: string): Boolean;
    function SaveScreenshot(Overwrite: Boolean; ChartName: string): Boolean;
    function ParseLine(Line: string; channel: Byte): double;
    function ParseDefFile(InFile: string): Boolean;
    function Nonlinear(X: double): double;
    function Linear(X: double): double;
    function FontStylesToString(FontStyles: TFontStyles): string;
    function StringToFontStyles(s: string): TFontStyles;
    procedure ReadNotes;
    function CalcDaysHoursMins(x : double) : string;

    class var
     evalTimeChanged : Boolean; // true if user changed evaluation time
     DelayReadCounter : integer; // to check if the readout will stop
     HeaderStrings : array [1..6] of string; // string for the output file header
     isBlank : array [1..6] of Boolean; // if channel is a blank
     timeCounter : double; // counter of the overall SIX signal time in min
     signalCounter : integer; // counter of the overall SIX readouts
     NumChannels : integer; // number of channels
     wasZoomDragged : Boolean; // if the user previously zoomDragged

  end;

var
  SIXControl: TSIXControl;
  InNameParse: string = ''; // name of load file
  DropfileNameParse: string = ''; // name of dropped file
  intSlopeCounter: integer = 0; // counts how many time the current measurement exceeded the slope limit
  wasReset: Boolean = false; // true if Reset button was pressed
  ScreenOutName: string = '';
  CSVOutName: string = '';
  Gains : array [1..6] of single; // the channel gains
  GainsRaw : array [1..6] of single; // the default gains
  Subtracts : array [1..6] of integer; // the channel subtracts
  TemperGains : array [1..8] of single; // the used temperature gains
  TemperGainsSaved : array [1..8] of single; // the temperature gains from the definition file
  ErrorCount : integer = 0; // counts how many times we did not reeive a stop bit
  wasNoStopByte : Boolean = false; // to catch the case no stop byte was sent
  inCalibration : Boolean = false; // to prevent chart scrolling while calibrating
  LiveViewWasAuto : Boolean = false; // if the left axis was in auto range when LiveView was turned off
  ConnectionLost : Boolean = false; // if connection is lost
  ConnectionLostChecking : Boolean = false; // if a port scane is already running when connection loss

implementation

uses
  Fitting, Calibration,
  // ChartEditing units
  ceTitleFootDlg, ceLegendDlg, ceSeriesDlg, ceAxisDlg;

constructor TSIXControl.create;
var
 i : integer;
begin
 evalTimeChanged:= false;
 for i:= 1 to 8 do
  TemperGains[i]:= 0;
end;

procedure TSIXControl.SCReadTimerTimerFinished(Sender: TObject);
type intArray = array[1..4] of byte;
     PintArray = ^intArray;
     TDataArray = array[0..24] of byte;
var
 OutLine : string;
 dataString : AnsiString;
 temperature, lastInterval, ScrollInterval, X, OldMax, OldMin : double;
 i, StopPos, ItemIndex : integer;
 MousePointer : TPoint;
 dataArray : TDataArray;
 tempArray : packed array of byte;
 HiLowArray : array[0..1] of byte;
 Chan : array [1..6] of Int16;
 ChanDbl : array [0..8] of double; // start from zero purposely for non-existing subtracts
 ChanRawDbl : array [1..8] of double;
 checksum : integer;
 tempInt16: Int16;
 PintegerArray : PintArray;
 SingleByte : byte;
 BeginTime : TDateTime;
 ScanTime : longword;
begin
 // tell the OS the application is alive
 Application.ProcessMessages;

 // initialize
 MousePointer:= Mouse.CursorPos;
 lastInterval:= 0.0;
 for i:= 0 to 8 do
  ChanDbl[i]:= 0.0;

 if ConnectionLost then
 begin
  // We can into a timer issue: The time finished already a few times
  // while in the meantime we have reconnected.
  // In this case, when this was initially executed ConnectionLost was false
  // Since every COMPortScan needs time we must not start a new scan until
  // the current one is ready or we can have meanwhile a reconnection
  // and then cannot setup a new connection over the existing one.
  // So we need to check if ConnectionLost is meanwhile true and that
  // no scan is running.
  if ConnectionLostChecking or (not ConnectionLost) then
   exit;
  BeginTime:= Now;
  ConnectionLostChecking:= true;
  MainForm.COMPortScan('SIX');
  ConnectionLostChecking:= false;
  // search the COM list if the SIX is listed there
  i:= Pos(':', MainForm.ConnComPortSensM.Lines[1]);
  for i:= 0 to Length(COMListSIX) - 1 do
  begin
   if COMListSIX[i] = connectedSIX then
   begin
   // open new connection
   try
    try
     // same check as described above
     if not ConnectionLost then
      exit;
     serSensor:= TBlockSerial.Create;
     serSensor.DeadlockTimeout:= 1000; //set timeout to 1 s
     serSensor.Connect('COM' + IntToStr(i));
     // the config must be set after the connection
     serSensor.config(9600, 8, 'N', SB1, False, False);
    except
     begin
      ScanTime:= MilliSecondsBetween(Now, BeginTime);
      lastInterval:= lastInterval + ScanTime / 60000;
      timeCounter:= timeCounter + lastInterval;
      exit;
     end;
    end;

    ConnectionLost:= false;
    MainForm.HaveSerialSensorCB.Checked:= true;
    wasNoStopByte:= true; // to force a re-sync
    NoSound;

    // since the SIX might be replugged to another USB slot
    // update the port number
    MainForm.ConnComPortSensM.Lines[0]:= 'COM' + IntToStr(i);

    MainForm.ConnComPortSensM.Color:= clDefault;
    MainForm.IndicatorSensorP.Hint:= 'Connection to Six was lost for some time but'
     + LineEnding + 'automatic reconnection was successful';
    MainForm.IndicatorSensorP.Caption:= 'Measurement running';
    MainForm.IndicatorSensorP.Color:= clYellow;
    MainForm.InfoNote.Hide; // hide the note
    MainForm.InfoNote.Color:= clInfoBk;
    break;

   finally
    if serSensor.LastError <> 0 then // output the error
    begin
     MessageDlgPos(MainForm.ConnComPortSensM.Lines[0]
      + ' error: ' + serSensor.LastErrorDesc,
      mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
     MainForm.CloseLazSerialConn;
     MainForm.IndicatorSensorP.Caption:= 'Connection failiure';
     MainForm.IndicatorSensorP.Color:= clRed;
     MainForm.ConnComPortSensM.Color:= clRed;
     MainForm.LoadSensorDataMI.Enabled:= true;
     exit;
    end;
   end;

   end; // if COMListSIX[i] = SIXNumber
  end; // end for i:= 0 to Length(COMListSIX - 1)

  // we must pause here
  // otherwise we can run into timing issues blocking the
  // serial connection when it is read from it too quick after the reconnection
  // experimenting on different PCs shows that 4 seconds is sufficient
  Sleep(4000);

  ScanTime:= MilliSecondsBetween(Now, BeginTime);
  lastInterval:= lastInterval + ScanTime / 60000;
  timeCounter:= timeCounter + lastInterval;
  exit;

 end; // end if ConnectionLost

 // first check if we still have a filestream
 // for example when saved to a USB stick and the stick was removed
 if not HaveSensorFileStream then
 begin
  MainForm.ReadTimer.Enabled:= false;
  MessageDlgPos('The access to the data file was lost!' + LineEnding
    + 'To restart you must call again the menu Connection -> SIX bisensors',
   mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
  MainForm.CloseLazSerialConn;
  MainForm.ConnComPortSensM.Color:= clRed;
  MainForm.IndicatorSensorP.Caption:= 'Connection lost';
  MainForm.IndicatorSensorP.Color:= clRed;
  exit;
 end;

 try
  // in case we need to re-sync we must readout as many bytes until we get a
  // stop byte again
  if wasNoStopByte then
  begin
   SingleByte:= $1;
   i:= 0;
   // look for a stop byte in the next 100 bytes
   while (SingleByte <> $16) and (i < 101) do
   begin
    SingleByte:= serSensor.RecvByte(100);
    inc(i);
   end;
   wasNoStopByte:= false;
   if i > 100 then // no stop byte within 100 bytes, so there is a severe problem
   begin
    MainForm.ReadTimer.Enabled:= False;
    MessageDlgPos('The received last 100 bytes do not contain a stop bit.'
    + LineEnding + 'Try to reconnect to the SIX.',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    MainForm.CloseLazSerialConn;
    MainForm.ConnComPortSensM.Color:= clRed;
    MainForm.IndicatorSensorP.Caption:= 'SIX error';
    MainForm.IndicatorSensorP.Color:= clRed;
    exit;
   end;
  end;
 finally
  if serSensor.LastError <> 0 then // can occur if USB cable was removed
  begin
   // Skip further error messages when there was already an error and thus
   // the timer is already stopped.
   if MainForm.ReadTimer.Enabled = false then
    exit;

   MainForm.ConnComPortSensM.Color:= clRed;
   MainForm.IndicatorSensorP.Caption:= 'Check USB cable!';
   MainForm.IndicatorSensorP.Color:= clRed;
   if MainForm.HaveSerialSensorCB.Checked then
   begin
    // close connection
    serSensor.CloseSocket;
    serSensor.free;
   end;
   MainForm.HaveSerialSensorCB.Checked:= False;
   ConnectionLost:= true;
   lastInterval:= lastInterval + MainForm.ReadTimer.Interval / 60000;
   timeCounter:= timeCounter + lastInterval;

   // since the process will need more than 10 seconds, show a note
   // at the position where the initial info message was output
   MainForm.InfoNote.vNotifierForm.Height := 200;
   MainForm.InfoNote.vNotifierForm.Width := 500;
   MainForm.InfoNote.Color:= clRed;
   MainForm.InfoNote.Text:= 'The connection to the SIX device was lost!'
    + LineEnding + LineEnding + 'Please check the USB cable.' + LineEnding
    + 'If it was acidentally unplugged, just plug it back in and the'
    + LineEnding + 'measurement will continue automatically.' + LineEnding
    + LineEnding + 'NOTE: if you close this dialog, an automatic reconnection'
    + LineEnding + 'will no longer be possible!';
   MainForm.InfoNote.Title:= 'Connection to Sensor lost!';
   MainForm.InfoNote.ShowAtPos(MainForm.Left + 100, MainForm.Top + 100);

   // play an annoying sound to inform the user
   Sound(1000);

   exit;

  end;
 end;

 // will be the case if serSensor.LastError <> 0 since the exit there
 // only jumps out of the try finally block
 if not MainForm.HaveSerialSensorCB.Checked then
  exit;

 // the are 3 different serial buffers:
 // - the 25 bytes of the SIX
 // - the buffer of the OS
 // - the LineBuffer of the synaser library
 // since the serial USB connection plug delivers from time to time wrong info
 // about available date (says there are no data, despite there are), we cannot
 // rely on the LineBuffer but must reast everything that is in the OS buffer
 dataString:= '';
 dataString:= serSensor.RecvPacket(100);

 // in case the read failed or not 25 bytes received
 if (serSensor.LastError <> 0) or (Length(dataString) < 25) then
 begin
  inc(ErrorCount);
  // we wait then another timer run
  // if we get 3 times the same error, something is wrong and we must stop
  if ErrorCount < 4 then
  begin
   lastInterval:= lastInterval + MainForm.ReadTimer.Interval / 60000;
   timeCounter:= timeCounter + lastInterval;
   exit;
  end
  else
  begin
   MainForm.ReadTimer.Enabled:= False;
   if serSensor.LastError <> 0 then
    MessageDlgPos(MainForm.ConnComPortSensM.Lines[0] + ' error on reading signal data: '
     + serSensor.LastErrorDesc, mtError, [mbOK], 0, MousePointer.X, MousePointer.Y)
   else
    MessageDlgPos(
     'Error: Could not read 25 bytes. Got only ' + IntToStr(Length(dataString)) + ' bytes.',
     mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   MainForm.CloseLazSerialConn;
   MainForm.ConnComPortSensM.Color:= clRed;
   MainForm.IndicatorSensorP.Caption:= 'SIX error';
   MainForm.IndicatorSensorP.Color:= clRed;
   exit;
  end;
 end;

 // convert string to a byte array
 SetLength(tempArray{%H-}, Length(dataString));
 Move(dataString[1], tempArray[0], Length(dataString));

 // now search the byte array for the stop bit
 StopPos:= -1;
 // since a value byte can also have the value $16, we search backwards
 // and check that the byte 20 positions earlier has the value $4 (begin of
 // a data block)
 for i:= Length(tempArray) - 1 downto Length(tempArray) - 5 do
 begin
  if (tempArray[i] = $16) and (tempArray[i - 20] = $4) then
  begin
   StopPos:= i;
   break;
  end;
 end;
 if StopPos = -1 then
 begin
  inc(ErrorCount);
  // the read data block sometimes misses the stop bit
  // usually the stop bit appears again within the next readout, but not at the
  // expected position
  // to re-sync then with the SIX, wait one interval and read out byte by
  // byte until a stop byte appears
  wasNoStopByte:= true;
  // however, if we cannot re-sync after 3 attempts, something is wrong with the
  // SIX and we must stop
  if ErrorCount < 4 then
  begin
   lastInterval:= lastInterval + MainForm.ReadTimer.Interval / 60000;
   timeCounter:= timeCounter + lastInterval;
   exit;
  end
  else
  begin
   MainForm.ReadTimer.Enabled:= False;
   MessageDlgPos('The received last 300 bytes do not contain a stop bit.'
    + LineEnding + 'Try to reconnect to the SIX.',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   MainForm.CloseLazSerialConn;
   MainForm.ConnComPortSensM.Color:= clRed;
   MainForm.IndicatorSensorP.Caption:= 'SIX error';
   MainForm.IndicatorSensorP.Color:= clRed;
   exit;
  end;
 end;

 // reset counter since we got no error
 ErrorCount:= 0;

 // copy the relevant 25 bytes to the dataArray
 dataArray:= default(TDataArray); // initialize or clear array
 Move(tempArray[StopPos - 24], dataArray[0], 25);
 StopPos:= 24;

 // we have all relevant data before the stop bit
 // first calculate the checksum
 checksum:= dataArray[StopPos - 2];
 for i:= StopPos - 3 downto StopPos - 20 do
  checksum:= checksum + dataArray[i];
 PintegerArray:= PintArray(@checksum);
 if PintegerArray^[1] <> dataArray[StopPos - 1] then
 begin
  // the data are corrupted so wait for another timer run
  lastInterval:= lastInterval + MainForm.ReadTimer.Interval / 60000;
  timeCounter:= timeCounter + lastInterval;
  exit;
 end;
 // transform the dataArray so that zero array position gets the first value byte
 for i := 0 to 18 do
  dataArray[i]:= dataArray[StopPos - 19 + i];

 // create now a string with the line we will write to the file
 // first the time and counter
 inc(signalCounter);
 OutLine:= IntToStr(signalCounter) + #9;
 // take the time passed until the timer was triggered
 lastInterval:= lastInterval + MainForm.ReadTimer.Interval / 60000; // in min
 if signalCounter > 1 then
  timeCounter:= timeCounter + lastInterval // in min
 else
  timeCounter:= 0.0; // assures the first data point has time zero
 OutLine:= OutLine + FloatToStrF(timeCounter, ffFixed, 3, 3) + #9;
 // check if user meanwhile changed the time
 if evalTimeChanged then
  MainForm.ReadTimer.Interval:= Trunc(MainForm.EvalTimeFSE.Value * 1000); // in ms;

 // now the channels
 // first convert each 2 bytes to a signed 16 bit integer
 for i:= 1 to NumChannels do
 begin
  HiLowArray[0]:= dataArray[2*i-1];
  HiLowArray[1]:= dataArray[2*i-2];
  Chan[i]:= Int16(HiLowArray);
 end;

 // now the temperature
 HiLowArray[0]:= dataArray[13];
 HiLowArray[1]:= dataArray[12];
 tempInt16:= Int16(HiLowArray);
 // the temperature value must be divided by 16 to get the value in deg celsius
 temperature:= tempInt16 / 16;
 MainForm.SIXTempLE.Text:= FloatToStr(RoundTo(temperature, -2));

 // get the raw values in nA
 // purposely not with temperature correction
 for i:= 1 to NumChannels do
  ChanRawDbl[i]:= Chan[i] * GainsRaw[i] / 100;

 if MainForm.HaveDefFileCB.Checked then
 // convert to mM
 begin
  for i:= 1 to NumChannels do
   ChanDbl[i]:= Chan[i] * Gains[i] / 100
    / exp(TemperGains[i] / 100 * (temperature - TemperGains[8]));
  // blank handling
  if MainForm.NoSubtractBlankCB.Checked then
  begin
   // output all non-blank channels
   for i:= 1 to NumChannels do
   begin
    if not isBlank[i] then
     OutLine:= OutLine + FormatFloat('0.0000', ChanDbl[i]) + #9;
   end;
   OutLine:= OutLine + FormatFloat('0.00', temperature) + #9;
  end
  else
  begin
   // subtract blank values
   // since the blank has no temperature correction, we cannot just subtract
   for i:= 1 to NumChannels do
    if (not isBlank[i]) and (Chan[i] <> 0) then
     ChanDbl[i]:= ChanDbl[i] * (Chan[i] - Chan[Subtracts[i]]) / Chan[i];
   // output all non-blank channels
   for i:= 1 to NumChannels do
   begin
    if not isBlank[i] then
     OutLine:= OutLine + FormatFloat('0.0000', ChanDbl[i]) + #9;
   end;
   OutLine:= OutLine + FormatFloat('0.00', temperature) + #9;
  end;
 end;

 // store also the raw current values
 if MainForm.HaveDefFileCB.Checked
  and (not MainForm.NoSubtractBlankCB.Checked) then
  // subtract blank values
  for i:= 1 to NumChannels do
   if not isBlank[i] then
    ChanRawDbl[i]:= ChanRawDbl[i] - ChanRawDbl[Subtracts[i]];
 // output
 for i:= 1 to NumChannels do
  OutLine:= OutLine + FormatFloat('0.0000', ChanRawDbl[i]) + #9;

 // output temperature if not already
 if  not MainForm.HaveDefFileCB.Checked then
  OutLine:= OutLine + FormatFloat('0.00', temperature) + #9;
 OutLine:= OutLine + LineEnding;

 // write the line to the file
 SensorFileStream.Write(OutLine[1], Length(OutLine));

 // calculate mean channel 7 and 8 values
 if MainForm.Channel7CB.Text = 'mean(#2, #5)' then
 begin
  ChanDbl[7]:= (ChanDbl[2] + ChanDbl[5]) / 2;
  ChanRawDbl[7]:= (ChanRawDbl[2] + ChanRawDbl[5]) / 2;
 end;
 if MainForm.Channel8CB.Text = 'mean(#2, #5)' then
 begin
  ChanDbl[8]:= (ChanDbl[2] + ChanDbl[5]) / 2;
  ChanRawDbl[8]:= (ChanRawDbl[2] + ChanRawDbl[5]) / 2;
 end;
 if MainForm.Channel7CB.Text = 'mean(#3, #6)' then
 begin
  ChanDbl[7]:= (ChanDbl[3] + ChanDbl[6]) / 2;
  ChanRawDbl[7]:= (ChanRawDbl[3] + ChanRawDbl[6]) / 2;
 end;
 if MainForm.Channel8CB.Text = 'mean(#3, #6)' then
 begin
  ChanDbl[8]:= (ChanDbl[3] + ChanDbl[6]) / 2;
  ChanRawDbl[8]:= (ChanRawDbl[3] + ChanRawDbl[6]) / 2;
 end;
 if MainForm.Channel7CB.Text = 'mean(#1, #4)' then
 begin
  ChanDbl[7]:= (ChanDbl[1] + ChanDbl[4]) / 2;
  ChanRawDbl[7]:= (ChanRawDbl[1] + ChanRawDbl[4]) / 2;
 end;
 if MainForm.Channel8CB.Text = 'mean(#1, #4)' then
 begin
  ChanDbl[8]:= (ChanDbl[1] + ChanDbl[4]) / 2;
  ChanRawDbl[8]:= (ChanRawDbl[1] + ChanRawDbl[4]) / 2;
 end;

 // draw SIX data
 for i:= 1 to NumChannels do
 begin
  if MainForm.HaveDefFileCB.Checked
   and (not MainForm.RawCurrentCB.Checked) then
   (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
    as TLineSeries).AddXY(timeCounter, ChanDbl[i])
  else
   (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
    as TLineSeries).AddXY(timeCounter, ChanRawDbl[i]);
 end;
 for i:= 7 to 8 do
 begin
  if MainForm.HaveDefFileCB.Checked
   and (not MainForm.RawCurrentCB.Checked) then
   (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
    as TLineSeries).AddXY(timeCounter, ChanDbl[i])
  else
   (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
    as TLineSeries).AddXY(timeCounter, ChanRawDbl[i]);
 end;
 MainForm.SIXTempValues.AddXY(timeCounter, temperature);

 ScrollInterval:= MainForm.ScrollIntervalFSE.Value;
 // don't scroll when user zoomed in or when in calibration mode
 if ((wasZoomDragged) or (inCalibration))
  and MainForm.ChartLiveView.Active then
 begin
  // turning of LiveView restores the axis properties from the time it was
  // started. This would change the axis but when zoomed, we don't want this
  // Therefore first save the axis state
  OldMax:= MainForm.SIXCH.LeftAxis.Range.Max;
  OldMin:= MainForm.SIXCH.LeftAxis.Range.Min;
  // turn LiveView off
  MainForm.ChartLiveView.Active:= false;
  // we only need to act when the restored setting was auto range
  if not MainForm.SIXCH.LeftAxis.Range.UseMax then
  begin
   LiveViewWasAuto:= true;
   // set the axis to the last used range before LiveView was turned off
   MainForm.SIXCH.LeftAxis.Range.UseMax:= true;
   MainForm.SIXCH.LeftAxis.Range.UseMin:= true;
   MainForm.SIXCH.LeftAxis.Range.Max:= OldMax;
   MainForm.SIXCH.LeftAxis.Range.Min:= OldMin;
  end;
 end;
 // don't scroll when not enough data
 if (timeCounter <= ScrollInterval)
  and (not wasZoomDragged) and (not inCalibration) then
 begin
  MainForm.ChartLiveView.Active:= false;
  MainForm.SIXCH.ZoomFull;
 end;
 // activate scrolling if not already on
 if MainForm.ScrollViewCB.Checked
   and (not MainForm.ChartLiveView.Active)
   and (not wasZoomDragged) and (not inCalibration)
   and (timeCounter > ScrollInterval) then
 begin
  if LiveViewWasAuto then
  begin
   // set the axis back to auto range
   MainForm.SIXCH.LeftAxis.Range.UseMax:= false;
   MainForm.SIXCH.LeftAxis.Range.UseMin:= false;
   LiveViewWasAuto:= false;
  end;
  MainForm.ChartLiveView.Active:= true;
 end;

 // catch a case in which the pen width must be 1
 // see procedure SCScrollViewCBChange why this must be assured
 if (timeCounter > ScrollInterval)
  and (not MainForm.ScrollViewCB.Checked)
  and (MainForm.SIXTempValues.LinePen.Width > 1) then
 begin
  for i:= 1 to 8 do
   (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
    as TLineSeries).LinePen.Width:= 1;
  MainForm.SIXTempValues.LinePen.Width:= 1;
 end;

 if (not wasZoomDragged) and (not inCalibration) then
 begin
  // we scrolled so we can go back with the line pen to width 2
  // see procedure SCScrollViewCBChange why we might be at 1
  if (MainForm.SIXTempValues.LinePen.Width = 1)
   and MainForm.ScrollViewCB.Checked then
  begin
   for i:= 1 to 8 do
    (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
     as TLineSeries).LinePen.Width:= 2;
   MainForm.SIXTempValues.LinePen.Width:= 2;
  end;
 end;

 // output analog voltages
 if (not MainForm.UseAnOutCB.checked) then
  exit;
 // first calculate the values
 for i:= 1 to 4 do // we limit to output channels (4-way pump driver)
 begin
  if (not (MainForm.FindComponent('AnOutConnector' + IntToStr(i) + 'OnOffCB')
   as TCheckBox).Checked) then
   continue;

  if MainForm.RawCurrentCB.Checked then // use raw signals
  begin
   // if just a channel
   // we calculate the values in nA
   ItemIndex:= (MainForm.FindComponent('AnOutputOf' + IntToStr(i) + 'CB')
    as TComboBox).ItemIndex;
   if ItemIndex < 6 then
   begin
    // the voltage out of the current value
    X:= ChanRawDbl[ItemIndex + 1] / MainForm.AnOutMaxSignalFSE.Value * 3.3;
    // correction to linearize the result
    // Nonlinear(x) described the measured analog output voltage to measured
    // voltage dependency, Linear(x) the linear curve defined by the points
    // Nonlinear(3.3) and Nonlinear (0.1) (range of the possible analog output)
    X:= X - Nonlinear(X) + Linear(X);
    (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
    as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
   end
   // if mean
   else
   begin
    if ItemIndex = 6 then
    begin
     X:= (ChanRawDbl[1] + ChanRawDbl[2]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 7 then
    begin
     X:= (ChanRawDbl[1] + ChanRawDbl[3]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 8 then
    begin
     X:= (ChanRawDbl[1] + ChanRawDbl[4]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 9 then
    begin
     X:= (ChanRawDbl[1] + ChanRawDbl[5]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 10 then
    begin
     X:= (ChanRawDbl[1] + ChanRawDbl[6]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 11 then
    begin
     X:= (ChanRawDbl[2] + ChanRawDbl[3]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 12 then
    begin
     X:= (ChanRawDbl[2] + ChanRawDbl[4]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 13 then
    begin
     X:= (ChanRawDbl[2] + ChanRawDbl[5]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 14 then
    begin
     X:= (ChanRawDbl[2] + ChanRawDbl[6]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 15 then
    begin
     X:= (ChanRawDbl[3] + ChanRawDbl[4]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 16 then
    begin
     X:= (ChanRawDbl[3] + ChanRawDbl[5]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 17 then
    begin
     X:= (ChanRawDbl[3] + ChanRawDbl[6]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 18 then
    begin
     X:= (ChanRawDbl[4] + ChanRawDbl[5]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 19 then
    begin
     X:= (ChanRawDbl[4] + ChanRawDbl[6]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end;
    if ItemIndex = 20 then
    begin
     X:= (ChanRawDbl[5] + ChanRawDbl[6]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end;
   end; // end else if mean
  end
  // values in mmol/l according to .def file
  else
  begin
   // if just a channel
   ItemIndex:= (MainForm.FindComponent('AnOutputOf' + IntToStr(i) + 'CB')
    as TComboBox).ItemIndex;
   if ItemIndex < 6 then
   begin
    // the voltage out of the current value
    X:= ChanDbl[ItemIndex + 1] / MainForm.AnOutMaxSignalFSE.Value * 3.3;
    X:= X - Nonlinear(X) + Linear(X);
    (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
    as TLabeledEdit).Text:=  FloatToStrF(X, ffFixed, 3, 3);
   end
   // if mean
   else
   begin
    if ItemIndex = 6 then
    begin
     X:= (ChanDbl[1] + ChanDbl[2]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 7 then
    begin
     X:= (ChanDbl[1] + ChanDbl[3]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 8 then
    begin
     X:= (ChanDbl[1] + ChanDbl[4]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 9 then
    begin
     X:= (ChanDbl[1] + ChanDbl[5]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 10 then
    begin
     X:= (ChanDbl[1] + ChanDbl[6]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 11 then
    begin
     X:= (ChanDbl[2] + ChanDbl[3]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 12 then
    begin
     X:= (ChanDbl[2] + ChanDbl[4]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 13 then
    begin
     X:= (ChanDbl[2] + ChanDbl[5]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 14 then
    begin
     X:= (ChanDbl[2] + ChanDbl[6]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 15 then
    begin
     X:= (ChanDbl[3] + ChanDbl[4]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 16 then
    begin
     X:= (ChanDbl[3] + ChanDbl[5]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 17 then
    begin
     X:= (ChanDbl[3] + ChanDbl[6]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 18 then
    begin
     X:= (ChanDbl[4] + ChanDbl[5]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end
    else if ItemIndex = 19 then
    begin
     X:= (ChanDbl[4] + ChanDbl[6]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end;
    if ItemIndex = 20 then
    begin
     X:= (ChanDbl[5] + ChanDbl[6]) / 2
         / MainForm.AnOutMaxSignalFSE.Value * 3.3;
     X:= X - Nonlinear(X) + Linear(X);
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(X, ffFixed, 3, 3);
    end;
   end; // end else if mean
  end; // end values in mM
 end; // end for i:= 1 to 4

end;

function TSIXControl.Nonlinear(X: double): double;
// the measured analog output voltage to measured voltage dependency
begin
 // note that the offset can be neglected since the linearization is independent
 result:= -0.1678 * power(X, 2) + 1.7397 * X;
end;

function TSIXControl.Linear(X: double): double;
// linear curve defined by the points
// Nonlinear(3.3) and Nonlinear (0.1) (range of the possible analog output)
begin
 result:= (Nonlinear(3.3) - Nonlinear(0.1))/(3.3 - 0.1) * (X - 0.1)
          + Nonlinear(0.1);
end;

function TSIXControl.CalcDaysHoursMins(x : double) : string;
// reformats x to the format day:hour:minute
var
 days, hours, minutes : integer;
begin
 // only if the data format is actually used, we do the calculation
 if not MainForm.TimeDaysHoursMinMI.checked then
 begin
  result:= Format('%.4g', [x]);
  exit;
 end;

 days:= 0; hours:= 0; minutes:= 0;
 if MainForm.TimeMinuteMI.Checked then
 begin
  days:= floor(x / 1440);
  hours:= floor((x / 1440 - days) * 24);
  minutes:= round(((x / 1440 - days) * 24 - hours) * 60);
 end
 else if MainForm.TimeHourMI.Checked then
 begin
  days:= floor(x / 24);
  hours:= floor((x / 24 - days) * 24);
  minutes:= round(((x / 24 - days) * 24 - hours) * 60);
 end
 else if MainForm.TimeDayMI.Checked then
 begin
  days:= floor(x);
  hours:= floor((x - days) * 24);
  minutes:= round(((x - days) * 24 - hours) * 60);
 end;

 result:= Format('%.2d',[days]) + ':'
          + Format('%.2d',[hours]) + ':' + Format('%.2d',[minutes]);
end;

procedure TSIXControl.SCDataPointHintToolHint(
  ATool: TDataPointHintTool; const APoint: TPoint; var AHint: String);
var
 SeriesName : string;
 x : double;
 currentSeries, otherSeries : TBasicChartSeries;
 i : integer;
begin
 otherSeries:= nil;
 currentSeries:= ATool.Series;

 for i:= 0 to MainForm.SIXCH.SeriesCount - 1 do
 begin
  if (MainForm.SIXCH.Series[i] is TLineSeries)
   and (MainForm.SIXCH.Series[i].Active)
   then
  begin
   otherSeries:= MainForm.SIXCH.Series[i];
   if currentSeries <> otherSeries then
    otherSeries.ZPosition:= 0;
  end;
 end;
 // repaint chart if necessary
 if currentSeries.ZPosition < 1 then
 begin
  currentSeries.ZPosition:= 1;
  MainForm.SIXCH.Invalidate;
 end;

 SeriesName:= ATool.Series.Name;
 x:= MainForm.SIXCH.AxisList[1].GetTransform.GraphToAxis(
      ATool.NearestGraphPoint.X);

 // all series except of SIXTempValues are connected to the left axis
 if SeriesName <> 'SIXTempValues' then
  AHint:= 'time = ' + CalcDaysHoursMins(x) + LineEnding
          + Format('%.4g',
                   [MainForm.SIXCH.AxisList[0].GetTransform.GraphToAxis(
                    ATool.NearestGraphPoint.Y)])
 else
  AHint:= 'time = ' + CalcDaysHoursMins(x) + LineEnding
          + Format('%.4g',
                   [MainForm.SIXCH.AxisList[2].GetTransform.GraphToAxis(
                    ATool.NearestGraphPoint.Y)])
end;

procedure TSIXControl.SCDataPointHintToolHintPosition(
 ATool: TDataPointHintTool; var APoint: TPoint);
var
 series : TLineSeries;
 x, y, HintWidth, HintHeight : Integer;
 rect : TRect;
 HintWindow : THintWindow;
 HintText : string = '';
 SeriesName : string;
// moves the hint text above the cursor and center it horizontally to cursor
begin
 series:= ATool.Series as TLineSeries;
 SeriesName:= ATool.Series.Name;
 // get image coordinates of the point
 x:= MainForm{%H-}.SIXCH.XGraphToImage(series.ListSource[ATool.PointIndex]^.X);
 // all series except of SIXTempValues are connected to the left axis
 if SeriesName <> 'SIXTempValues' then
  y:= MainForm{%H-}.SIXCH.YGraphToImage(
       MainForm.SIXCH.AxisList[0].GetTransform.AxisToGraph(
        series.ListSource[ATool.PointIndex]^.Y))
 else
  y:= MainForm{%H-}.SIXCH.YGraphToImage(
       MainForm.SIXCH.AxisList[2].GetTransform.AxisToGraph(
        series.ListSource[ATool.PointIndex]^.Y));

 // get hint text - just call the event handler of OnHint
 MainForm.DataPointHintToolHint(ATool, APoint, HintText);

 HintWindow:= THintWindow.Create(nil);
 try
  rect:= HintWindow.CalcHintRect(MainForm.SIXCH.Width, HintText, nil);
  HintWidth:= rect.Right - rect.Left; // hint window width
  HintHeight:= rect.Bottom - rect.Top; // hint window height
 finally
  HintWindow.Free;
 end;

 // center hint horizontally relative to data point
 APoint.x:= x - HintWidth div 2;
 // move hint 10 pixels above the "High" data point
 APoint.y:= y - HintHeight - 10;
 // hint coordinates are relative to screen
 APoint:= MainForm.SIXCH.ClientToScreen(APoint);
end;

function TSIXControl.ParseLine(Line: string; channel: Byte): double;
// parses the input string to get the time and channels
var
 List: TStringList;
 resultString: string;
begin
 // use a stringList to easily parse the string
 List:= TStringList.Create;
 try
  List.Delimiter:= #9; // the values are separated by a tabulator
  List.DelimitedText:= Line;
  // the time is the second element in a line
  resultString:= List[channel + 1];
 finally
  List.Free;
 end;
 result:= strToFloat(resultString);
end;

function TSIXControl.SaveCSV(Overwrite: Boolean; ChartName: string) : Boolean;
var
 InNameCSV, line : string;
 stream : TStream;
 i: Integer;

begin
 Result:= false;
 // propose a file name
 InNameCSV:= 'SIXMeasurements';
 if Overwrite then
 begin
  // proposal according to currently active tab
  if MainForm.MainPC.ActivePage = MainForm.SIXValuesTS then
   InNameCSV:= InNameCSV + '-Live';
  if MainForm.MainPC.ActivePage = MainForm.ResultTS then
   InNameCSV:= InNameCSV + '-Result';
 end;
 CSVOutName:= MainForm.SaveHandling(InNameCSV, '.csv'); // opens file dialog

 if (CSVOutName <> '') and FileExists(CSVOutName) then
 begin
  try
    stream:= TFileStream.Create(CSVOutName, fmCreate);
   // output the flow rate and channel as header lines
   line:= 'used pump rate in µl/min:' + #9 + LineEnding;
   stream.WriteBuffer(line[1], Length(line));
   line:= 'used SIX channel:' + #9 + MainForm.Channel2LE.Text + LineEnding;
   stream.WriteBuffer(line[1], Length(line));
   // output the table header line
   line:= 'concentration in mmol/l' + #9 + 'measured signal in nA' + LineEnding;
   stream.WriteBuffer(line[1], Length(line));
   for i:= 0 to (MainForm.FindComponent(ChartName) as TLineSeries).Count - 1 do
   begin
    line:= Format('%.9g'#9'%.9g',
           [(MainForm.FindComponent(ChartName) as TLineSeries).XValue[i],
            (MainForm.FindComponent(ChartName) as TLineSeries).YValue[i]],
           DefaultFormatSettings);
    line:= line + LineEnding;
    stream.WriteBuffer(line[1], Length(line));
   end;
  finally
   stream.Free;
  end;

  Result:= true;
 end;
end;

procedure TSIXControl.SCScrollViewCBChange(Sender: TObject);
var
 i : integer;
begin
 // don't scroll when not enough data
 if (timeCounter > MainForm.ScrollIntervalFSE.Value) then
  MainForm.ChartLiveView.Active:= MainForm.ScrollViewCB.Checked;

 MainForm.ScrollIntervalFSE.Enabled:= MainForm.ScrollViewCB.Checked;

 if not MainForm.ScrollViewCB.Checked then
 begin
  // We might have many data points. And when now the line thickess is not 1
  // Windows will perform some calculations that slow down the display of the
  // chart a lot. Therefore go down to 1.
  // but only if there are yet enough values
  if timeCounter > MainForm.ScrollIntervalFSE.Value then
  begin
   for i:= 1 to 8 do
    (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
     as TLineSeries).LinePen.Width:= 1;
   MainForm.SIXTempValues.LinePen.Width:= 1;
  end;
  // zoom back to normal and enable auto scale
  MainForm.SIXCH.ZoomFull;
  MainForm.AutoscaleMI.Enabled:= true;
  MainForm.SIXCH.BottomAxis.Range.UseMax:= false;
  MainForm.SIXCH.BottomAxis.Range.UseMin:= false;
 end
 // if checked
 else
 begin
  // also in case it is zoomed, enable scrolling since the user turned this
  // on purposely
  wasZoomDragged:= false;
  // the user might have set a range and then turned on scrolling
  // therefore assure the range is not used
  MainForm.SIXCH.BottomAxis.Range.UseMax:= false;
  MainForm.SIXCH.BottomAxis.Range.UseMin:= false;
  // for the x-axis also te extent must be set
  MainForm.SIXCH.Extent.UseXMax:= false;
  MainForm.SIXCH.Extent.UseXMin:= false;
  // disable auto scale context menu item
  MainForm.AutoscaleMI.Enabled:= false;
  // Under ome unfreproducible circumstances the scall range is not taken
  // into account. Therefore assure this by setting it again.
  MainForm.ChartLiveView.ViewportSize:= MainForm.ScrollIntervalFSE.Value;

  // info: we cannot go back to LinePen width 2 here because this would have an
  // immediate effect. Thus first do it after the next scrolling occurs.
 end;
end;

procedure TSIXControl.SCSaveCSVResultBClick(Sender: TObject);
begin
 CSVOutName:= '';
 SaveCSV(true, MainForm.ResultCHValues.Name);
end;

function TSIXControl.SaveScreenshot(Overwrite: Boolean; ChartName: string) : Boolean;
var
 OutNameHelp : string;

begin
 Result:= false;
 // propose a file name
 OutNameHelp:= 'Screenshot-' + MainForm.LoadedFileSensM.Text;
 if Overwrite then
 begin
  // proposal according to currently active tab
  if MainForm.MainPC.ActivePage = MainForm.SIXValuesTS then
   OutNameHelp:= OutNameHelp + '-Live';
  if MainForm.MainPC.ActivePage = MainForm.ResultTS then
   OutNameHelp:= OutNameHelp + '-Result';
 end;
 ScreenOutName:= MainForm.SaveHandling(OutNameHelp, '.svg'); // opens file dialog

 if ScreenOutName <> '' then
 begin
  (MainForm.FindComponent(ChartName) as TChart).SaveToSVGFile(ScreenOutName);
  Result:= true;
 end
 else
  Result:= false;

end;

procedure TSIXControl.SCSaveScreenshotLiveBClick(Sender: TObject);
begin
 ScreenOutName:= '';
 SaveScreenshot(true, MainForm.SIXCH.Name);
end;

procedure TSIXControl.SCSaveScreenshotResultBClick(Sender: TObject);
begin
 ScreenOutName:= '';
 SaveScreenshot(true, MainForm.ResultCH.Name);
end;

procedure TSIXControl.SCStartFitBClick(Sender: TObject);
// start the fit widget
begin
 // we close the form if it is already opened since on showing
 // the maybe updated/new values will be loaded
 // and to trigger the .Show routine, the form must not already be visible
 if FitForm.Visible then
  FitForm.Close;
 // show the form
 FitForm.Show;
end;

procedure TSIXControl.SCChangeBackColorMIClick(Sender: TObject);
begin
 if MainForm.MainPC.ActivePage = MainForm.SIXValuesTS then
 begin
  // start with current color
  MainForm.ColorDialog.Color:= MainForm.SIXCH.BackColor;
  if MainForm.ColorDialog.Execute then
   MainForm.SIXCH.BackColor:= MainForm.ColorDialog.Color;
 end
 else if MainForm.MainPC.ActivePage = MainForm.ResultTS then
 begin
  MainForm.ColorDialog.Color:= MainForm.ResultCH.BackColor;
  if MainForm.ColorDialog.Execute then
   MainForm.ResultCH.BackColor:= MainForm.ColorDialog.Color;
 end;
end;

procedure TSIXControl.SCChannelXLEChange(Sender: TObject);
var
 SenderName, Channel : string;
begin
 SenderName:= (Sender as TComponent).Name;
 // SenderName is in the form 'ChannelxLE' and we need the x
 // so get the 8th character of the name
 Channel:= Copy(SenderName, 8, 1);

 // transfer content to testing tab
 (MainForm.FindComponent('ChannelTest' + Channel + 'LE') as TLabeledEdit).Text:=
  (MainForm.FindComponent(SenderName) as TLabeledEdit).Text;
end;

procedure TSIXControl.SCChannelXCBChange(Sender: TObject);
var
 SenderName, Channel : string;
 j : integer;
begin
 SenderName:= (Sender as TComponent).Name;
 // SenderName is in the form 'ChannelxCB' and we need the x
 // so get the 8th character of the name
 Channel:= Copy(SenderName, 8, 1);

 // change the Text according to current item
 (MainForm.FindComponent(SenderName) as TComboBox).Text:=
  (MainForm.FindComponent(SenderName) as TComboBox).Items[
      (MainForm.FindComponent(SenderName) as TComboBox).ItemIndex];

 // set legend name
 if (MainForm.FindComponent(SenderName) as TComboBox).Text = 'mean(#2, #5)' then
  (MainForm.FindComponent('SIXCh' + Channel + 'Values') as TLineSeries).Title:=
   'Mean (' + MainForm.Channel2GB.Caption + ', ' + MainForm.Channel5GB.Caption + ')'
 else if (MainForm.FindComponent(SenderName) as TComboBox).Text = 'mean(#3, #6)' then
  (MainForm.FindComponent('SIXCh' + Channel + 'Values') as TLineSeries).Title:=
   'Mean (' + MainForm.Channel3GB.Caption + ', ' + MainForm.Channel6GB.Caption + ')'
 else if (MainForm.FindComponent(SenderName) as TComboBox).Text = 'mean(#1, #4)' then
  (MainForm.FindComponent('SIXCh' + Channel + 'Values') as TLineSeries).Title:=
   'Mean (' + MainForm.Channel1GB.Caption + ', ' + MainForm.Channel4GB.Caption + ')';
 // we use the same legend name for Live and Result charts
 (MainForm.FindComponent('SIXCh' + Channel + 'Results') as TLineSeries).Title:=
  (MainForm.FindComponent('SIXCh' + Channel + 'Values') as TLineSeries).Title;

 // if we have a raw signal, then the unit is nA
 if (MainForm.FindComponent(SenderName) as TComboBox).ItemIndex > 2 then
  (MainForm.FindComponent('CurrChannel' + Channel + 'LE')
    as TLabeledEdit).EditLabel.Caption:= 'Actual Signal [nA]';
 if MainForm.HaveDefFileCB.Checked
  and ((MainForm.FindComponent(SenderName) as TComboBox).ItemIndex < 3) then
  (MainForm.FindComponent('CurrChannel' + Channel + 'LE')
    as TLabeledEdit).EditLabel.Caption:= 'Actual Signal [mM]';

 // recalculate the channel values
 if Channel = '7' then
 begin
  if MainForm.Channel7CB.Text = 'mean(#1, #4)' then
  begin
      for j:= 0 to MainForm.SIXCh7Values.LastValueIndex do
      MainForm.SIXCh7Values.YValue[j]:=
       (MainForm.SIXCh1Values.YValue[j] + MainForm.SIXCh4Values.YValue[j]) / 2;
  end;
  if MainForm.Channel7CB.Text = 'mean(#2, #5)' then
  begin
      for j:= 0 to MainForm.SIXCh7Values.LastValueIndex do
      MainForm.SIXCh7Values.YValue[j]:=
       (MainForm.SIXCh2Values.YValue[j] + MainForm.SIXCh5Values.YValue[j]) / 2;
  end;
  if MainForm.Channel7CB.Text = 'mean(#3, #6)' then
  begin
   for j:= 0 to MainForm.SIXCh7Values.LastValueIndex do
     MainForm.SIXCh7Values.YValue[j]:=
      (MainForm.SIXCh3Values.YValue[j] + MainForm.SIXCh6Values.YValue[j]) / 2;
  end;
 end
 else if Channel = '8' then
 begin
  if MainForm.Channel8CB.Text = 'mean(#1, #4)' then
  begin
      for j:= 0 to MainForm.SIXCh8Values.LastValueIndex do
      MainForm.SIXCh8Values.YValue[j]:=
       (MainForm.SIXCh1Values.YValue[j] + MainForm.SIXCh4Values.YValue[j]) / 2;
  end;
  if MainForm.Channel8CB.Text = 'mean(#2, #5)' then
  begin
      for j:= 0 to MainForm.SIXCh8Values.LastValueIndex do
      MainForm.SIXCh8Values.YValue[j]:=
       (MainForm.SIXCh2Values.YValue[j] + MainForm.SIXCh5Values.YValue[j]) / 2;
  end;
  if MainForm.Channel8CB.Text = 'mean(#3, #6)' then
  begin
   for j:= 0 to MainForm.SIXCh8Values.LastValueIndex do
     MainForm.SIXCh8Values.YValue[j]:=
      (MainForm.SIXCh3Values.YValue[j] + MainForm.SIXCh6Values.YValue[j]) / 2;
  end;
 end;
end;

procedure TSIXControl.SCChannelXGBDblClick(Sender: TObject);
var
 SenderName, Channel, HeaderLine : string;
 i : integer;
begin
 HeaderLine:= '';
 SenderName:= (Sender as TComponent).Name;
 // SenderName is in the form 'ChannelxBB' and we need the x
 // so get the 8th character of the name
 Channel:= Copy(SenderName, 8, 1);
 // show in dialog the current name
 NameSettingF.NameE.Text:=
  (MainForm.FindComponent(SenderName) as TGroupBox).Caption;
 // open connection dialog
 NameSettingF.Caption:= 'Channel name selection';
 NameSettingF.NameL.Caption:= 'Channel name:';
 NameSettingF.ShowModal;
 if NameSettingF.ModalResult = mrCancel then
  exit
 else
  (MainForm.FindComponent(SenderName)
   as TGroupBox).Caption:= NameSettingF.NameE.Text;

 // rename the chart legend accordingly if not channel 7 or 8
 i:= StrToInt(Channel);
 if i < 7 then
 begin
  // update header line channel name
  HeaderStrings[i]:= (MainForm.FindComponent(SenderName) as TGroupBox).Caption;
  (MainForm.FindComponent('SIXCh' + Channel + 'Values') as TLineSeries).Title:=
   HeaderStrings[i];
  (MainForm.FindComponent('SIXCh' + Channel + 'Results') as TLineSeries).Title:=
   'Measurement ' + HeaderStrings[i];
 end;

 // the channel operations might show the old channel name, thus update them
 for i:= 7 to 8 do
 begin
  if (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
     as TComboBox).Text = 'mean(#2, #5)' then
   (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values') as TLineSeries).Title:=
    'Mean (' + MainForm.Channel2GB.Caption + ', ' + MainForm.Channel5GB.Caption + ')'
  else if (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
     as TComboBox).Text = 'mean(#3, #6)' then
   (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values') as TLineSeries).Title:=
    'Mean (' + MainForm.Channel3GB.Caption + ', ' + MainForm.Channel6GB.Caption + ')'
  else if (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
     as TComboBox).Text = 'mean(#1, #4)' then
   (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values') as TLineSeries).Title:=
    'Mean (' + MainForm.Channel1GB.Caption + ', ' + MainForm.Channel4GB.Caption + ')';
  // we use the same legend name for Live and Result charts
  (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Results') as TLineSeries).Title:=
   (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values') as TLineSeries).Title;
 end;

 // write a new header line to the output file
 if HaveSensorFileStream then
 begin
  if MainForm.HaveDefFileCB.Checked then
  begin
   HeaderLine:= HeaderLine + 'Counter' + #9 + 'Time [min]' + #9;
   // output all non-blank channels
   for i:= 1 to SIXControl.NumChannels do
    if not SIXControl.isBlank[i] then
     HeaderLine:= HeaderLine + HeaderStrings[i] + ' [mM]' + #9;
   HeaderLine:= HeaderLine + 'Temp [deg C]' + #9;
   // for the raw values
   for i:= 1 to SIXControl.NumChannels do
    HeaderLine:= HeaderLine + HeaderStrings[i] + ' [nA]' + #9;
   HeaderLine:= HeaderLine + LineEnding;
  end
  else
  begin
   HeaderLine:= HeaderLine + 'Counter' + #9 + 'Time [min]' + #9;
   for i:= 1 to SIXControl.NumChannels do
   begin
    if i = StrToInt(Channel) then
     HeaderLine:= HeaderLine + HeaderStrings[i] + ' [nA]' + #9
    else
     HeaderLine:= HeaderLine + 'Ch' + IntToStr(i) + ' [nA]' + #9;
   end;
   HeaderLine:= HeaderLine + 'Temp [deg C]' + #9 + LineEnding;
  end;
  // write line
  SensorFileStream.Write(HeaderLine[1], Length(HeaderLine))
 end;

 // transfer content to testing tab
 (MainForm.FindComponent('Channel' + Channel + 'TestGB') as TGroupBox).Caption:=
  (MainForm.FindComponent(SenderName) as TGroupBox).Caption;
end;

procedure TSIXControl.SCNoSubtractBlankCBChange(Sender: TObject);
var
 HeaderLine : string = '';
begin
 if MainForm.NoSubtractBlankCB.Checked and HaveSensorFileStream then
 begin
  // write into the output that the blanks are no longer subtracted
  HeaderLine:= 'Note: From now on the blank values are not subtracted from the channel values!'
               + LineEnding;
  // write a new header line to the output file
  SensorFileStream.Write(HeaderLine[1], Length(HeaderLine));
 end
 else if (not MainForm.NoSubtractBlankCB.Checked) and HaveSensorFileStream then
 begin
  // write into the output that the blanks are again subtracted
  HeaderLine:= 'Note: The blank values are again subtracted from the channel values.'
               + LineEnding;
  SensorFileStream.Write(HeaderLine[1], Length(HeaderLine));
 end;
end;

procedure TSIXControl.SCRawCurrentCBChange(Sender: TObject);
var
 i, j : integer;
 LastIndex : integer = 0;
 hasLoadedSensorData : Boolean;
 AppendCounter : Int64;
 LastDefFile, LastSIXID : string;
 AppendMinute : double;
begin
 // if RawCurrentCB is disabled we must not recalculate
 if (not MainForm.RawCurrentCB.Enabled) then
  exit;

 // if we have something in the chart but no connection to the SIX,
 // we have a data file loaded
 hasLoadedSensorData:= ((MainForm.SIXCh1Values.LastValueIndex > 0)
                        and (not MainForm.HaveSerialSensorCB.Checked));

 if MainForm.RawCurrentCB.Checked then
 begin
  // rename the chart axis
  MainForm.SIXCH.AxisList[0].Title.Caption:= 'Sensor Value [nA]';
  MainForm.ResultCH.AxisList[0].Title.Caption:= 'Sensor Value [nA]';
  // in this case no definition file is needed and the SIX connection
  // can be enabled
  MainForm.SIXBiosensorsMI.Enabled:= true;
  MainForm.SIXConnectBB.Enabled:= true;
  MainForm.IndicatorSensorP.Color:= cldefault;
  // thus also clear the warning
  MainForm.IndicatorSensorP.Caption:= '';
  // enable analog output
  MainForm.UseAnOutCB.Enabled:= true;
  // change 3.3V output label
  MainForm.AnOutMaxLabel.Caption:= 'nA will become 3.3 V output';

  if hasLoadedSensorData then
   // we need to re-read the file and don't recalculate
   MainForm.ReadSensorData('none', AppendMinute, AppendCounter,
                           LastDefFile, LastSIXID)
  else
  begin
   // recalculate the mmol values in the plot to nA
   // Note: this will purposely not have any influence on the output .csv file
   for i:= 1 to NumChannels do
   begin
    for j:= 0 to (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
     as TLineSeries).LastValueIndex do
     (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
      as TLineSeries).YValue[j]:=
       (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
        as TLineSeries).YValue[j]
        * exp(TemperGains[i] / 100
              * (MainForm.SIXTempValues.YValue[j] - TemperGains[8]))
        * GainsRaw[i] / Gains[i];
   end;
  end;
  // for raw current the temperature correction can be recalculated
  if hasLoadedSensorData and FoundLoadedDefFile then
   MainForm.NoTempCorrectionCB.Enabled:= true;
 end
 else // not checked
 begin
  MainForm.SIXCH.AxisList[0].Title.Caption:= 'Sensor Value [mmol/l]';
  MainForm.ResultCH.AxisList[0].Title.Caption:= 'Sensor Value [mmol/l]';
  // change 3.3V output label
  MainForm.AnOutMaxLabel.Caption:= 'mmol/l will become 3.3 V output';

  if hasLoadedSensorData then
   // we need to re-read the file
   MainForm.ReadSensorData('none', AppendMinute, AppendCounter,
                           LastDefFile, LastSIXID);

  if not MainForm.HaveDefFileCB.Checked then
  begin
   // disable then also analog output
   MainForm.UseAnOutCB.Enabled:= false;
   MainForm.UseAnOutCB.Checked:= false;
  end
  else
  begin
   // recalculate the nA values in the plot to mmol
   // Note: this will purposely not have any influence on the output .csv file
   for i:= 1 to NumChannels do
   begin
    for j:= 0 to (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
     as TLineSeries).LastValueIndex do
     (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
      as TLineSeries).YValue[j]:=
       (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
        as TLineSeries).YValue[j]
        / exp(TemperGains[i] / 100 * (MainForm.SIXTempValues.YValue[j] - TemperGains[8]))
        * Gains[i] / GainsRaw[i];
   end;
  end;
  // temperature correction cannot be recalculated
  MainForm.NoTempCorrectionCB.Enabled:= false;
 end; // end not checked

 // calculate channel 7 and 8 values
 LastIndex:= MainForm.SIXCh7Values.LastValueIndex;
 // we can have the case that e.g. a two-channel .def file was unload
 // then we don't have values for channel 3 to 6
 if (MainForm.SIXCh2Values.LastValueIndex = LastIndex)
  and (MainForm.SIXCh5Values.LastValueIndex = LastIndex) then
 begin
  if MainForm.Channel7CB.Text = 'mean(#2, #5)' then
  begin
      for j:= 0 to MainForm.SIXCh7Values.LastValueIndex do
      MainForm.SIXCh7Values.YValue[j]:=
       (MainForm.SIXCh2Values.YValue[j] + MainForm.SIXCh5Values.YValue[j]) / 2;
  end;
  if MainForm.Channel8CB.Text = 'mean(#2, #5)' then
  begin
   for j:= 0 to MainForm.SIXCh8Values.LastValueIndex do
     MainForm.SIXCh8Values.YValue[j]:=
      (MainForm.SIXCh2Values.YValue[j] + MainForm.SIXCh5Values.YValue[j]) / 2;
  end;
 end;
 if (MainForm.SIXCh3Values.LastValueIndex = LastIndex)
  and (MainForm.SIXCh6Values.LastValueIndex = LastIndex) then
 begin
  if MainForm.Channel7CB.Text = 'mean(#3, #6)' then
  begin
   for j:= 0 to MainForm.SIXCh7Values.LastValueIndex do
     MainForm.SIXCh7Values.YValue[j]:=
      (MainForm.SIXCh3Values.YValue[j] + MainForm.SIXCh6Values.YValue[j]) / 2;
  end;
  if MainForm.Channel8CB.Text = 'mean(#3, #6)' then
  begin
   for j:= 0 to MainForm.SIXCh8Values.LastValueIndex do
     MainForm.SIXCh8Values.YValue[j]:=
      (MainForm.SIXCh3Values.YValue[j] + MainForm.SIXCh6Values.YValue[j]) / 2;
  end;
 end;
 if (MainForm.SIXCh1Values.LastValueIndex = LastIndex)
  and (MainForm.SIXCh4Values.LastValueIndex = LastIndex) then
 begin
  if MainForm.Channel7CB.Text = 'mean(#1, #4)' then
  begin
   for j:= 0 to MainForm.SIXCh7Values.LastValueIndex do
     MainForm.SIXCh7Values.YValue[j]:=
      (MainForm.SIXCh1Values.YValue[j] + MainForm.SIXCh4Values.YValue[j]) / 2;
  end;
  if MainForm.Channel8CB.Text = 'mean(#1, #4)' then
  begin
   for j:= 0 to MainForm.SIXCh8Values.LastValueIndex do
     MainForm.SIXCh8Values.YValue[j]:=
      (MainForm.SIXCh1Values.YValue[j] + MainForm.SIXCh4Values.YValue[j]) / 2;
  end;
 end;

end;

procedure TSIXControl.SCTitleFootClickToolClick(Sender: TChartTool;
  Title: TChartTitle);
var
 editor : TChartTitleFootEditor;
begin
 editor:= TChartTitleFootEditor.Create(nil);
 try
  editor.Prepare(Title, 'Edit chart title');
  editor.ShowModal;
 finally
  editor.Free;
 end;
end;

procedure TSIXControl.SCZoomDragToolAfterMouseUp(ATool: TChartTool;
  APoint: TPoint);
var
 i : integer;
begin
 // find out if the user zoomed in
 // we cannot use SIXCH.IsZoomed because the LiveView will permanently change
 // the extent so that SIXCH.IsZoomed is always true
 // Thus compare the x coordinate of the extent with the previous one instead
 if MainForm.SIXCH.LogicalExtent.a.X > MainForm.SIXCH.PrevLogicalExtent.a.X then
  wasZoomDragged:= true
 else
  wasZoomDragged:= false;

 // when it will be zoomed out then we must assure that the line pen is 1
 // otherwise we would slow down the program a lot when the chart has
 // > 20k points, see procedure SCScrollViewCBChange for the reason
 // To test of if the next tep is the zoom out, check the selection rect since
 // when it is directed to the left, it will be zoomed out.
 if (MainForm.ZoomDragTool.SelectionRect.TopLeft.X >
     MainForm.ZoomDragTool.SelectionRect.BottomRight.X)
  and (MainForm.SIXTempValues.LinePen.Width = 2)
  and MainForm.ScrollViewCB.Checked
  and (timeCounter > MainForm.ScrollIntervalFSE.Value) then
 begin
  for i:= 1 to 8 do
   (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
    as TLineSeries).LinePen.Width:= 1;
  MainForm.SIXTempValues.LinePen.Width:= 1;
 end;

 // if not in scrollong mode, zoom out when clicked
 if (not MainForm.ScrollViewCB.Checked) and (not wasZoomDragged) then
  MainForm.SIXCH.ZoomFull;
end;

procedure TSIXControl.SCLegendClickToolClick(Sender: TChartTool;
  Legend: TChartLegend);
var
 editor : TChartLegendEditor;
begin
 editor:= TChartLegendEditor.Create(nil);
 try
  editor.Prepare(Legend, 'Edit chart legend');
  editor.ShowModal;
 finally
  editor.Free;
 end;
end;

procedure TSIXControl.SCAxisClickToolClick(Sender: TChartTool;
  AnAxis: TChartAxis; HitInfo: TChartAxisHitTests);
var
  page : TChartAxisEditorPage;
  editor : TChartAxisEditor;
begin
 Unused(Sender);
 if (ahtTitle in HitInfo) then
  page:= aepTitle
 else if (ahtLabels in HitInfo) then
  page:= aepLabels
 else if (ahtLine in HitInfo) then
  page:= aepLine
 else if (ahtGrid in HitInfo) then
  page:= aepGrid
 else
  exit;
 editor:= TChartAxisEditor.Create(nil);
 try
  editor.Prepare(AnAxis, 'Edit chart axis "%s"');
  editor.Page:= page;
  editor.ShowModal; // shows the dialog

  // Info: calling the dialog triggers ZoomDragToolAfterMouseUp
  // keep that in mind

  // update the axis settings for the LiveView
  MainForm.ChartLiveView.StoreAxisRange(AnAxis);
 finally
  editor.Free;
 end;
end;

procedure TSIXControl.SCAppearanceXBBClick(Sender: TObject);
Var
 SenderName, GBCaption, ChannelNumber : string;
 editor : TChartSeriesEditor;
 Channel : integer;
begin
 SenderName:= (Sender as TComponent).Name;
 // SenderName is in the form 'AppearancexBB' and we need the x
 // so get the 11th character of the name
 ChannelNumber:= Copy(SenderName, 11, 1);
 Channel:= StrToInt(ChannelNumber);
 // we have this assignment:
 // 1 -> Series 0, 2 -> Series 2 and so on
 Channel:= 2 * Channel - 2;
 // change to the chart tab to see the changes immediately
 MainForm.MainPC.ActivePage:= MainForm.SIXValuesTS;
 // get name of GroupBox or ComboBox
 if StrToInt(ChannelNumber) < 7 then
  GBCaption:= (MainForm.FindComponent('Channel' + ChannelNumber + 'GB')
   as TGroupBox).Caption
 else
  GBCaption:= (MainForm.FindComponent('Channel' + ChannelNumber + 'CB')
  as TComboBox).Text;
 // now we can edit the desired series
 editor:= TChartSeriesEditor.Create(nil);
 try
  editor.Prepare(MainForm.SIXCH.Series[Channel],
   'Edit appearance for ' + GBCaption);
  editor.ShowModal;
 finally
  editor.Free;
 end;
 // change back to tab from where we started
 //MainForm.MainPC.ActivePage:= MainForm.GeneralTS;
end;

procedure TSIXControl.SCChannelXOnOffCBChange(Sender: TObject);
var
 SenderName, Channel : string;
begin
 SenderName:= (Sender as TComponent).Name;
 // SenderName is in the form 'ChannelxOnOffCB' and we need the x
 // so get the 8th character of the name
 Channel:= Copy(SenderName, 8, 1);
 // show/hide data
 (MainForm.FindComponent('SIXCh' + Channel + 'Values') as TLineSeries).Active:=
  (MainForm.FindComponent(SenderName) as TCheckBox).Checked;
 // for the channel operations, empty the LineEdit when turned off
 if StrToInt(Channel) > 6 then
  (MainForm.FindComponent('CurrChannel' + Channel + 'LE')
     as TLabeledEdit).Text:= ''
end;

procedure TSIXControl.SCAnOutConnectorXOnOffCBChange(Sender: TObject);
var
 SenderName, Channel : string;
begin
 SenderName:= (Sender as TComponent).Name;
 // SenderName is in the form 'AnOutConnectorXOnOffCB' and we need the x
 // so get the 15th character of the name
 Channel:= Copy(SenderName, 15, 1);
 // transfer settings to the pump on/off of step 1
 (MainForm.FindComponent('Pump' + Channel + 'OnOffCB1') as TCheckBox).Checked:=
  (MainForm.FindComponent(SenderName) as TCheckBox).Checked;
end;

procedure TSIXControl.SCAnOutOfXLEChange(Sender: TObject);
var
 SenderName, Channel : string;
begin
 SenderName:= (Sender as TComponent).Name;
 // SenderName is in the form 'AnOutOfXLE' and we need the x
 // so get the 8th character of the name
 Channel:= Copy(SenderName, 8, 1);
 // transfer value to the PumpXVoltageFS of step 1
 (MainForm.FindComponent('Pump' + Channel + 'VoltageFS1') as TFloatSpinEdit).Value:=
  StrToFloat((MainForm.FindComponent(SenderName) as TLabeledEdit).Text);
end;

procedure TSIXControl.SCAnOutOnOffTBChange(Sender: TObject);
begin
 if MainForm.AnOutOnOffTB.checked then
 begin
  // 'run' the pumps
  MainForm.RunBBClick(Sender);
  // change button appearance
  MainForm.AnOutOnOffTB.Caption:= 'Output Off';
  MainForm.IndicatorAnOutP.Caption:= 'Output is on';
  MainForm.IndicatorAnOutP.Color:= clRed;
 end
 else
 begin
  // 'stop' the pumps
  MainForm.StopBBClick(Sender);
  // change button appearance
  MainForm.AnOutOnOffTB.Caption:= 'Output On';
  MainForm.IndicatorAnOutP.Caption:= '';
  MainForm.IndicatorAnOutP.Color:= clDefault;
 end;
end;

procedure TSIXControl.SCShowTempCBChange(Sender: TObject);
begin
 // show/hide temperature values
 MainForm.SIXTempValues.Active:= MainForm.ShowTempCB.Checked;
 MainForm.SIXCH.AxisList[2].Visible:= MainForm.ShowTempCB.Checked;
end;

function TSIXControl.ParseDefFile(InFile: string): Boolean;
// parses the input sensor definition file
var
 OpenFileStream : TFileStream;
 LineReader : TStreamReader;
 ReadLine : string;
 gainFactor : single;
 i, oldItem : integer;
 StringArray : TStringArray;
 MousePointer : TPoint;
 ppp : PChar;
 Component : TComponent = nil;
begin
 // initialize
 result:= false;
 MousePointer:= Mouse.CursorPos; // store mouse position
 // enable maybe previously disabled GroupBoxes
 for i:= 1 to 8 do
  (MainForm.FindComponent('Channel' + IntToStr(i) + 'GB')
   as TGroupBox).Enabled:= true;

 // check the SIX type
 if MainForm.SIXTypeRG.ItemIndex = 1 then
  gainFactor:= 1
 else if MainForm.SIXTypeRG.ItemIndex = 0 then
  gainFactor:= 0.5
 else if MainForm.SIXTypeRG.ItemIndex = 2 then
  gainFactor:= 5
 else if MainForm.SIXTypeRG.ItemIndex = 3 then
  gainFactor:= 10;

 // open file stream
 try
  try
   // purposely open with write access to assure that no other program can modify it
   // while it is in use
   OpenFileStream:= TFileStream.Create(InFile, fmOpenReadWrite);
  except
   on EFOpenError do
   begin
    MessageDlgPos('Definition file is used by another program and cannot be opened.',
                  mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    exit;
   end;
  end;
  LineReader:= TStreamReader.Create(OpenFileStream);

  // read first line
  LineReader.ReadLine(ReadLine);
  // read until first comma
  StringArray:= ReadLine.Split(',');
  for i:= 0 to 5 do
  begin
   if not TryStrToFloat(StringArray[i], Gains[i+1]) then
   begin
    Result:= false;
    exit;
   end;
   Gains[i+1]:= Gains[i+1] * gainFactor;
  end;

  // next interesting line is line 4
  LineReader.ReadLine(ReadLine); // line 2
  LineReader.ReadLine(ReadLine); // line 3
  LineReader.ReadLine(ReadLine);
  StringArray:= ReadLine.Split(',');
  NumChannels:= 0;
  for i:= 0 to 5 do
  begin
   ppp:= PChar(StringArray[i]);
   HeaderStrings[i+1]:= AnsiExtractQuotedStr(ppp, '"');
   if HeaderStrings[i+1] <> '' then // we have a channel
   begin
    inc(NumChannels);
    (MainForm.FindComponent('Channel' + IntToStr(NumChannels) + 'LE')
     as TLabeledEdit).Text:= '#' + IntToStr(NumChannels);
   end;
   // take this as name for the group box
   Component:= (MainForm.FindComponent('Channel' + IntToStr(i+1) + 'GB')
       as TGroupBox);
   if (Component is TGroupBox) then
    (MainForm.FindComponent('Channel' + IntToStr(i+1) + 'GB')
     as TGroupBox).Caption:= HeaderStrings[i+1];
  end;
  // disable all channel GroupBoxes for non-existing channels
  if NumChannels < 6 then
  begin
   for i:= NumChannels + 1 to 6 do
   begin
    (MainForm.FindComponent('Channel' + IntToStr(i) + 'GB')
     as TGroupBox).Enabled:= false;
    (MainForm.FindComponent('Channel' + IntToStr(i) + 'OnOffCB')
     as TCheckBox).Checked:= false;
    (MainForm.FindComponent('Channel' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= '';
    (MainForm.FindComponent('CurrChannel' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= '';
   end;
  end;
  // update the possible operations
  // first delete, then refill
  if NumChannels < 4 then
  begin
   for i:= 7 to 8 do
   begin
    // no operations are possible thus disable these channels
    (MainForm.FindComponent('Channel' + IntToStr(i) + 'GB')
     as TGroupBox).Enabled:= false;
    (MainForm.FindComponent('Channel' + IntToStr(i) + 'OnOffCB')
     as TCheckBox).Checked:= false;
    (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
     as TComboBox).Items.Clear;
   end;
  end
  else
  begin
   for i:= 7 to 8 do
   begin
    // at first store item index
    oldItem:= (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
     as TComboBox).ItemIndex;
    (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
     as TComboBox).Items.Clear; // sets ItemIndex to -1
    if NumChannels >= 5 then
     (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
      as TComboBox).Items.Add('mean(#2, #5)');
    if NumChannels = 6 then
     (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
      as TComboBox).Items.Add('mean(#3, #6)');
    if NumChannels >= 4 then
     (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
      as TComboBox).Items.Add('mean(#1, #4)');
    // handle cases when indices do no longer exist
    if (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
               as TComboBox).Items.Count > oldItem then
     (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
      as TComboBox).ItemIndex:= oldItem
    else if (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
               as TComboBox).Items.Count = 0 then
     (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
      as TComboBox).ItemIndex:= -1
    else
     (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
      as TComboBox).ItemIndex:= 0;
   end;
  end;

  // read line 5
  LineReader.ReadLine(ReadLine);
  // read until first comma
  StringArray:= ReadLine.Split(',');
  for i:= 0 to 5 do
   if not TryStrToInt(StringArray[i], Subtracts[i+1]) then
   begin
    Result:= false;
    exit;
   end;

  // next interesting line is line 7
  LineReader.ReadLine(ReadLine); // line 6
  LineReader.ReadLine(ReadLine);
  StringArray:= ReadLine.Split(',');
  for i:= 0 to 7 do
   if not TryStrToFloat(StringArray[i], TemperGainsSaved[i+1]) then
   begin
    Result:= false;
    exit;
   end;
  // if temp correction is used, transfer the gains to the active ones
  if not MainForm.NoTempCorrectionCB.Checked then
   for i:= 0 to 7 do
    TemperGains[i+1]:= TemperGainsSaved[i+1];

 finally
  LineReader.Free;
  OpenFileStream.Free;
 end;

 result:= true;
end;

procedure TSIXControl.SCCalibrateTBChange(Sender: TObject; aborted: Boolean);
{the calibration is done the following way:
 - after clicking the calibrate button, the user can select data by clicking
   and dragging in the chart. This way a selection rectangle is created.
 - after the selection a dialog pops up listing all series in the chart
 - the user selects there what series to use and what conversion unit should be used
 - the mean of the selected series' datapoints within the rectangle is eventually
   used as calibration value}
var
 extent : TDoubleRect;
 height, width : double;
 center : TDoublePoint;
 OutName, DummyString, HeaderLine, EvaluateString : string;
 StringList : TStringList;
 StringArray : TStringArray;
 i : integer;
 Event : TNotifyEvent;
 FormatSetting: TFormatSettings;
begin
 // show/hide the lines and en/disable the rectangle tool
 MainForm.TopLine.Active:= MainForm.CalibrateTB.Checked;
 MainForm.BottomLine.Active:= MainForm.CalibrateTB.Checked;
 MainForm.LeftLine.Active:= MainForm.CalibrateTB.Checked;
 MainForm.RightLine.Active:= MainForm.CalibrateTB.Checked;
 // enable rectangle tool
 MainForm.RectangleSelectionTool.Enabled:= MainForm.CalibrateTB.Checked;
 // disable unused tools
 MainForm.ZoomDragTool.Enabled:= (not MainForm.CalibrateTB.Checked);
 MainForm.DataPointHintTool.Enabled:= (not MainForm.CalibrateTB.Checked);
 MainForm.DataPointCrosshairTool.Enabled:= (not MainForm.CalibrateTB.Checked);
 MainForm.PanDragTool.Enabled:= (not MainForm.CalibrateTB.Checked);
 MainForm.PanMouseWheelTool.Enabled:= (not MainForm.CalibrateTB.Checked);
 // prevent scrolling while in calibration selection mode
 inCalibration:= MainForm.CalibrateTB.Checked;

 if MainForm.CalibrateTB.Checked then
 begin
  if aborted then
  // toggle the button but without triggering this way an event
  begin
   Event:= MainForm.CalibrateTB.OnChange;
   try
    MainForm.CalibrateTB.OnChange:= nil;
    MainForm.CalibrateTB.Checked:= false;
    // call the event function to undo all calibration-specific settings
    SCCalibrateTBChange(Sender, true);
    exit;
   finally
    MainForm.CalibrateTB.OnChange:= Event;
   end;
  end;

  extent:= MainForm.SIXCH.LogicalExtent;
  width:= extent.b.x - extent.a.x; // horizontal range of data
  height:= extent.b.y - extent.a.y; // vertical range of data
  center:= DoublePoint((extent.a.x + extent.b.x)/2, (extent.a.y + extent.b.y)/2);
  // make a preset for the selection line positions
  if IsInfinite(MainForm.TopLine.Position) then
   MainForm.TopLine.Position:= center.y + height/4;
  if IsInfinite(-MainForm.BottomLine.Position) then
   MainForm.BottomLine.Position:= center.y - height/4;
  if IsInfinite(-MainForm.LeftLine.Position) then
   MainForm.LeftLine.Position:= center.x - width/4;
  if IsInfinite(MainForm.RightLine.Position) then
   MainForm.RightLine.Position:= center.x + width/4;

  // activate the rectangle selection
  MainForm.LineDragTool.Shift:= [ssLeft];
  MainForm.RectangleSelectionTool.Shift:= [ssLeft];

  // deactivate to be able to switch to other tab when calibration not finished
  // thus deactivate all tabs except of the current one
  for i:= 0 to MainForm.MainPC.PageCount-1 do
   MainForm.MainPC.Pages[i].enabled:= false;
  MainForm.MainPC.Pages[1].enabled:= true;

  // show menu entry to cancel the calibration
  MainForm.AbortCalibrationMI.Visible:= true;
  MainForm.Separator3MI.Visible:= true;
 end
 // if not checked
 else
 begin
  if (not aborted) then
   // show the calibration dialog
   CalibrationF.ShowModal;

  // if user pressed OK and there is a valid mean value, write a new .def file
  if (not aborted) and (CalibrationF.ModalResult = mrOK) then
  begin
   if calibChannelA = 0 then // something went wrong
   begin
    // move lines back to infinity
    MainForm.TopLine.Position:= Infinity;
    MainForm.BottomLine.Position:= -Infinity;
    MainForm.LeftLine.Position:= -Infinity;
    MainForm.RightLine.Position:= Infinity;
    exit;
   end;
   // open a file save dialog to save the changed .def file
   // use the folder of the InNameDef as default directory
   MainForm.SaveDialog.InitialDir:= ExtractFilePath(InNameDef);

   // propose as filename the current date
   // first setup the format
   FormatSetting:= DefaultFormatSettings;
   FormatSetting.DateSeparator:= '-';
   FormatSetting.TimeSeparator:= '-';
   // if the filename had already a date, remove it and add the current time
   DummyString:= MainForm.LoadedDefFileM.Text;
   if Length(DummyString) > 16 then
   begin
    // now get the last 16 chars of the file name containing the DateTime
    EvaluateString:= RightStr(DummyString, 16);
    try
     StrToDateTime(EvaluateString, FormatSetting);
     // if there was a date, cut it off plus the 3 characters ' - '
     SetLength(DummyString, Length(DummyString) - 19);
    except
     on Exception : EConvertError do
      // FIXME: on Hebrew Windows StrToDateTime() failes, thus ignore the
      // exception -> can only be fixed/evaluated on a Hebrew Windows
      SetLength(DummyString, Length(DummyString) - 19);
    end;
   end;

   // Note: to be later able to read the output datetime,
   // the date and time part must be separated by a space
   MainForm.SaveDialog.FileName:= DummyString + ' - '
                                  + FormatDateTime('dd-mm-yyyy hh-nn', now)
                                  + '.def';
   OutName:= MainForm.SaveHandling(InNameDef, '.def'); // opens file dialog
   if (OutName <> '') then
   begin
    // copy the loaded .def file into a StringList
    StringList:= TStringList.Create;
    try
     StringList.LoadFromFile(InNameDef);
     // the first line needs to be changed
     // we get the calibrated channel and the factor for the gain
     StringArray:= StringList[0].Split(',');
     if calibChannelB = 0 then // we can change the single channel
     begin
      // the new gain is the current one times the factor
      if MainForm.RawCurrentCB.Checked then
       // then we must take the temperature correction into account
       calibFactorA:= calibFactorA * GainsRaw[calibChannelA]
                     * exp(TemperGains[calibChannelA] / 100
                     * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]))
      else
       // the existing gain includes the temperature correction
       calibFactorA:= calibFactorA * Gains[calibChannelA];
      StringArray[calibChannelA-1]:= FloatToStr(RoundTo(calibFactorA, -4));
     end
     else // for channel operations
     begin
      if calibChannelA = 2 then
      begin
       if MainForm.RawCurrentCB.Checked then
       begin
        // then we must take the temperature correction into account
        calibFactorA:= calibFactorA * GainsRaw[2]
                      * exp(TemperGains[2] / 100
                      * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]));
        calibFactorB:= calibFactorB * GainsRaw[5]
                      * exp(TemperGains[5] / 100
                      * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]));
       end
       else
       begin
        // the existing gain includes the temperature correction
        calibFactorA:= calibFactorA * Gains[2];
        calibFactorB:= calibFactorB * Gains[5];
       end;
       StringArray[2-1]:= FloatToStr(RoundTo(calibFactorA, -4));
       StringArray[5-1]:= FloatToStr(RoundTo(calibFactorB, -4));
      end
      else if calibChannelA = 3 then
      begin
       if MainForm.RawCurrentCB.Checked then
       begin
        // then we must take the temperature correction into account
        calibFactorA:= calibFactorA * GainsRaw[3]
                      * exp(TemperGains[3] / 100
                      * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]));
        calibFactorB:= calibFactorB * GainsRaw[6]
                      * exp(TemperGains[6] / 100
                      * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]));
       end
       else
       begin
        // the existing gain includes the temperature correction
        calibFactorA:= calibFactorA * Gains[3];
        calibFactorB:= calibFactorB * Gains[6];
       end;
       StringArray[3-1]:= FloatToStr(RoundTo(calibFactorA, -4));
       StringArray[6-1]:= FloatToStr(RoundTo(calibFactorB, -4));
      end
      else if calibChannelA = 1 then
      begin
       if MainForm.RawCurrentCB.Checked then
       begin
        // then we must take the temperature correction into account
        calibFactorA:= calibFactorA * GainsRaw[1]
                      * exp(TemperGains[1] / 100
                      * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]));
        calibFactorB:= calibFactorB * GainsRaw[4]
                      * exp(TemperGains[4] / 100
                      * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]));
       end
       else
       begin
        // the existing gain includes the temperature correction
        calibFactorA:= calibFactorA * Gains[1];
        calibFactorB:= calibFactorB * Gains[4];
       end;
       StringArray[1-1]:= FloatToStr(RoundTo(calibFactorA, -4));
       StringArray[4-1]:= FloatToStr(RoundTo(calibFactorB, -4));
      end;
     end;
     // transform the array to a string and save it as new first line
     StringList[0]:= string.join(',', StringArray);
     // save the whole file
     StringList.SaveToFile(OutName);
    finally
     StringList.free;
    end;
    // immediately use the new .def file
    InNameDef:= OutName;
    for i:= 1 to NumChannels do
     Gains[i]:= StrToFloat(StringArray[i-1]);
    // display file name without suffix
    DummyString:= ExtractFileName(InNameDef);
    SetLength(DummyString, Length(DummyString) - 4);
    MainForm.LoadedDefFileM.Text:= DummyString;
    // display full path as tooltip
    MainForm.LoadedDefFileM.hint:= InNameDef;
    // write a new header line to the output file
    if HaveSensorFileStream then
    begin
     HeaderLine:= 'Used definition file: "' + MainForm.LoadedDefFileM.Text +
      '.def"' + LineEnding;
     HeaderLine:= HeaderLine + 'Counter' + #9 + 'Time [min]' + #9;
     // the blank channels have the unit nA
     for i:= 1 to 6 do
     begin
     if (Pos('Blank', HeaderStrings[i]) <> 0)
      or (Pos('blank', HeaderStrings[i]) <> 0) then
       isBlank[i]:= true
     else
      isBlank[i]:= false;
     end;
     // output all non-blank channels
     for i:= 1 to NumChannels do
      if not isBlank[i] then
     HeaderLine:= HeaderLine + HeaderStrings[i] + ' [mM]' + #9;
     HeaderLine:= HeaderLine + 'Temp [deg C]' + #9;
     // for the raw values
     for i:= 1 to NumChannels do
      HeaderLine:= HeaderLine + HeaderStrings[i] + ' [nA]' + #9;
     HeaderLine:= HeaderLine + LineEnding;
     // write line
     SensorFileStream.Write(HeaderLine[1], Length(HeaderLine));
    end;
   end; //end if OutName <> ''

  end; // end mrOK

  // deactivate the rectangle selection
  MainForm.LineDragTool.Shift:= [];
  MainForm.RectangleSelectionTool.Shift:= [];

  // move lines back to infinity
  MainForm.TopLine.Position:= Infinity;
  MainForm.BottomLine.Position:= -Infinity;
  MainForm.LeftLine.Position:= -Infinity;
  MainForm.RightLine.Position:= Infinity;
  // reset calibChannel
  calibChannelA:= 0;
  // The user might have zoomed in, then calibrated and is wondering why nothing
  // happens afterwards. Therefore jump out of the wasZoomDragged mode.
  wasZoomDragged:= false;
  // the data range is different so we need to refresh the y-axis range
  MainForm.SIXCH.LogicalExtent:= MainForm.SIXCH.GetFullExtent;
  // enable tab switching again
  for i:= 0 to MainForm.MainPC.PageCount-1 do
   MainForm.MainPC.Pages[i].enabled:= true;

 end; // end if not checked

end;

procedure TSIXControl.SCPerformAutoCalib(CalibSubstance: Substance);
{the calibration is done the following way:
 - after the specified step the last specified number of available measurements
   are taken to calculate its mean value
 - this value is taken as the measured value for the calibration substance}
var
 OutName, DummyString, HeaderLine, CLBName, EvaluateString : string;
 StringList : TStringList;
 StringArray : TStringArray;
 i : integer;
 FormatSetting: TFormatSettings;
 MousePointer : TPoint;
begin
 MousePointer:= Mouse.CursorPos; // store mouse position

 // first check of the substance should be calibrated
 // if the value is zero, we don't calibrate
 if CalibSubstance = Substance.Glucose then
  CLBName:= 'Glucose'
 else if CalibSubstance = Substance.Lactate then
  CLBName:= 'Lactate';
 if (MainForm.FindComponent(CLBName + 'CalibValueFSE')
     as TFloatSpinEdit).Value = 0.0 then
  exit;

 // calculate the calibration factors calibChannelA and calibChannelB
 CalibrationF.CalculateMeanStep(CalibSubstance);
 if calibChannelA = 0 then // something went wrong
 begin
  MessageDlgPos('Calibration could not be performed!' + LineEnding
    + 'Check the electrical AND fluidic connection to the sensor!',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
  exit;
 end;

 // propose as filename the current date
 // first setup the format
 FormatSetting:= DefaultFormatSettings;
 FormatSetting.DateSeparator:= '-';
 FormatSetting.TimeSeparator:= '-';
 // if the filename had already a date, remove it and add the current time
 DummyString:= MainForm.LoadedDefFileM.Text;
 if Length(DummyString) > 16 then
 begin
  // now get the last 16 chars of the file name containing the DateTime
  EvaluateString:= RightStr(DummyString, 16);
  try
   StrToDateTime(EvaluateString, FormatSetting);
   // if there was a date, cut it off plus the 3 characters ' - '
   SetLength(DummyString, Length(DummyString) - 19);
  except
   on Exception : EConvertError do
    // FIXME: on Hebrew Windows StrToDateTime() failes, thus ignore the
    // exception -> can only be fixed/evaluated on a Hebrew Windows
    SetLength(DummyString, Length(DummyString) - 19);
  end;
 end;

 // Note: to be later able to read the output datetime,
 // the date and time part must be separated by a space
 if RightStr(ExtractFilePath(InNameDef), 16) = 'DefinitionFiles\' then
  OutName:= ExtractFilePath(InNameDef) + DummyString
            + ' - ' + FormatDateTime('dd-mm-yyyy hh-nn', now) + '.def'
 else
  OutName:= ExtractFilePath(InNameDef) + 'DefinitionFiles\' + DummyString
            + ' - ' + FormatDateTime('dd-mm-yyyy hh-nn', now) + '.def';
 // copy the .def file into a StringList
 StringList:= TStringList.Create;
 try
  StringList.LoadFromFile(InNameDef);
  // the first line needs to be changed
  // we get the calibrated channel and the factor for the gain
  StringArray:= StringList[0].Split(',');
  if calibChannelB = 0 then // we can change the single channel
  begin
   // the new gain is the current one times the factor
   if MainForm.RawCurrentCB.Checked then
    // then we must take the temperature correction into account
    calibFactorA:= calibFactorA * GainsRaw[calibChannelA]
                  * exp(TemperGains[calibChannelA] / 100
                  * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]))
   else
    // the existing gain includes the temperature correction
    calibFactorA:= calibFactorA * Gains[calibChannelA];
   StringArray[calibChannelA-1]:= FloatToStr(RoundTo(calibFactorA, -4));
  end
  else // for channel operations
  begin
   if calibChannelA = 2 then
   begin
    if MainForm.RawCurrentCB.Checked then
    begin
     // then we must take the temperature correction into account
     calibFactorA:= calibFactorA * GainsRaw[2]
                   * exp(TemperGains[2] / 100
                   * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]));
     calibFactorB:= calibFactorB * GainsRaw[5]
                   * exp(TemperGains[5] / 100
                   * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]));
    end
    else
    begin
     // the existing gain includes the temperature correction
     calibFactorA:= calibFactorA * Gains[2];
     calibFactorB:= calibFactorB * Gains[5];
    end;
    StringArray[2-1]:= FloatToStr(RoundTo(calibFactorA, -4));
    StringArray[5-1]:= FloatToStr(RoundTo(calibFactorB, -4));
   end
   else if calibChannelA = 3 then
   begin
    if MainForm.RawCurrentCB.Checked then
    begin
     // then we must take the temperature correction into account
     calibFactorA:= calibFactorA * GainsRaw[3]
                    * exp(TemperGains[3] / 100
                    * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]));
     calibFactorB:= calibFactorB * GainsRaw[6]
                    * exp(TemperGains[6] / 100
                    * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]));
    end
    else
    begin
     // the existing gain includes the temperature correction
     calibFactorA:= calibFactorA * Gains[3];
     calibFactorB:= calibFactorB * Gains[6];
    end;
    StringArray[3-1]:= FloatToStr(RoundTo(calibFactorA, -4));
    StringArray[6-1]:= FloatToStr(RoundTo(calibFactorB, -4));
   end
   else if calibChannelA = 1 then
   begin
    if MainForm.RawCurrentCB.Checked then
    begin
     // then we must take the temperature correction into account
     calibFactorA:= calibFactorA * GainsRaw[1]
                   * exp(TemperGains[1] / 100
                   * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]));
     calibFactorB:= calibFactorB * GainsRaw[4]
                   * exp(TemperGains[4] / 100
                   * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]));
    end
    else
    begin
     // the existing gain includes the temperature correction
     calibFactorA:= calibFactorA * Gains[1];
     calibFactorB:= calibFactorB * Gains[4];
    end;
    StringArray[1-1]:= FloatToStr(RoundTo(calibFactorA, -4));
    StringArray[4-1]:= FloatToStr(RoundTo(calibFactorB, -4));
   end;
  end;
  // transform the array to a string and save it as new first line
  StringList[0]:= string.join(',', StringArray);
  // save the whole file
  StringList.SaveToFile(OutName);
 finally
  StringList.free;
 end;
 // immediately use the new .def file
 InNameDef:= OutName;
 for i:= 1 to NumChannels do
  Gains[i]:= StrToFloat(StringArray[i-1]);
 // display file name without suffix
 DummyString:= ExtractFileName(InNameDef);
 SetLength(DummyString, Length(DummyString) - 4);
 MainForm.LoadedDefFileM.Text:= DummyString;
 // display full path as tooltip
 MainForm.LoadedDefFileM.hint:= InNameDef;
 // write a new header line to the output file
 if HaveSensorFileStream then
 begin
  HeaderLine:= 'Used definition file: "' + MainForm.LoadedDefFileM.Text +
   '.def"' + LineEnding;
  HeaderLine:= HeaderLine + 'Counter' + #9 + 'Time [min]' + #9;
  // the blank channels have the unit nA
  for i:= 1 to 6 do
  begin
  if (Pos('Blank', HeaderStrings[i]) <> 0)
   or (Pos('blank', HeaderStrings[i]) <> 0) then
    isBlank[i]:= true
  else
   isBlank[i]:= false;
  end;
  // output all non-blank channels
  for i:= 1 to NumChannels do
   if not isBlank[i] then
  HeaderLine:= HeaderLine + HeaderStrings[i] + ' [mM]' + #9;
  HeaderLine:= HeaderLine + 'Temp [deg C]' + #9;
  // for the raw values
  for i:= 1 to NumChannels do
   HeaderLine:= HeaderLine + HeaderStrings[i] + ' [nA]' + #9;
  HeaderLine:= HeaderLine + LineEnding;
  // write line
  SensorFileStream.Write(HeaderLine[1], Length(HeaderLine));
 end;

  // reset calibChannel
 calibChannelA:= 0;
end;

procedure TSIXControl.SCNoTempCorrectionCBChange(Sender: TObject);
var
 i, j : integer;
 calcGains : array [1..6] of single;
 hasLoadedSensorData : Boolean;
begin
 // initialize
 for i:= 1 to 6 do
  calcGains[i]:= 1.0;

 // if we have something in the chart but no connection to the SIX,
 // we have a data file loaded
 hasLoadedSensorData:= ((MainForm.SIXCh1Values.LastValueIndex > 0)
                        and (not MainForm.HaveSerialSensorCB.Checked));

 // recalculate the values in the plot
 // Note: this will purposely not have any influence on the output .csv file

 // first we need to calculate the value without the temperature correction
 for i:= 1 to NumChannels do
 begin
  if not MainForm.RawCurrentCB.Checked then
  begin
   // if we have loaded data Gains could be zero
   if (Gains[i] = 0.0) or hasLoadedSensorData then
    exit;
   // values are in mmol
   calcGains[i]:= Gains[i]
  end
  else
   // values are in nA
   if (GainsRaw[i] = 0.0) or (not FoundLoadedDefFile) then // just a safe guard
    exit;
   calcGains[i]:= GainsRaw[i];

  for j:= 0 to (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
   as TLineSeries).LastValueIndex do
   (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
    as TLineSeries).YValue[j]:=
     (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
      as TLineSeries).YValue[j] * 100 / calcGains[i]
      * exp(TemperGains[i] / 100 * (MainForm.SIXTempValues.YValue[j] - TemperGains[8]))
 end;

 // now update the temperature gains
 for i:= 1 to 8 do
 begin
  if MainForm.NoTempCorrectionCB.Checked then
   TemperGains[i]:= 0
  else
   TemperGains[i]:= TemperGainsSaved[i];
 end;

 // finally recalculate the values in the plot using the new temperature gains
 for i:= 1 to NumChannels do
 begin
  for j:= 0 to (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
   as TLineSeries).LastValueIndex do
   (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
    as TLineSeries).YValue[j]:=
     (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
      as TLineSeries).YValue[j] / 100 * calcGains[i]
      / exp(TemperGains[i] / 100 * (MainForm.SIXTempValues.YValue[j] - TemperGains[8]))
 end;
end;

procedure TSIXControl.SCSaveAppearance(iniFile: string);
var
 i : integer;
 Chart : TChart;
 Axis : TChartAxis;
 Series : TLineSeries;
 tempStr : string;
 List : TStringList;
begin

try
 List:= TStringList.Create;

 // first store pump and valve setup
 List.Add('PumpNumberSE.Value ' + IntToStr(MainForm.PumpNumberSE.Value));
 List.Add('ValveNumberSE.Value ' + IntToStr(MainForm.ValveNumberSE.Value));

 // store last shown channels
 List.Add('Channel1OnOffCB.Checked ' + BoolToStr(MainForm.Channel1OnOffCB.Checked));
 List.Add('Channel2OnOffCB.Checked ' + BoolToStr(MainForm.Channel2OnOffCB.Checked));
 List.Add('Channel3OnOffCB.Checked ' + BoolToStr(MainForm.Channel3OnOffCB.Checked));
 List.Add('Channel4OnOffCB.Checked ' + BoolToStr(MainForm.Channel4OnOffCB.Checked));
 List.Add('Channel5OnOffCB.Checked ' + BoolToStr(MainForm.Channel5OnOffCB.Checked));
 List.Add('Channel6OnOffCB.Checked ' + BoolToStr(MainForm.Channel6OnOffCB.Checked));
 List.Add('Channel7OnOffCB.Checked ' + BoolToStr(MainForm.Channel7OnOffCB.Checked));
 List.Add('Channel8OnOffCB.Checked ' + BoolToStr(MainForm.Channel8OnOffCB.Checked));

 Chart:= MainForm.SIXCH;
 List.Add('Chart SIXCH');

 // Axes
 for i:= 0 to Chart.AxisList.Count-1 do
 begin
  Axis:= Chart.AxisList[i];
  List.Add('Axis ' + IntToStr(i));
  // purposely don't store the axis visibility and caption since
  // this can lead to confusion
  // Axis title
  WriteStr(tempStr, Axis.Title.Alignment);
  List.Add('Title.Alignment ' + tempStr);
  List.Add('Title.LabelFont.Name ' + Axis.Title.LabelFont.Name);
  List.Add('Title.LabelFont.Size ' + IntToStr(Axis.Title.LabelFont.Size));
  List.Add('Title.LabelFont.Color ' + ColorToString(Axis.Title.LabelFont.Color));
  List.Add('Title.LabelFont.Style ' + FontStylesToString(Axis.Title.LabelFont.Style));
  List.Add('Title.LabelFont.Orientation ' + IntToStr(Axis.Title.LabelFont.Orientation));
  List.Add('Title.Distance ' + IntToStr(Axis.Title.Distance));
  WriteStr(tempStr, Axis.Title.Shape);
  List.Add('Title.Shape ' + tempStr);
  List.Add('Title.LabelBrush.Color ' + ColorToString(Axis.Title.LabelBrush.Color));
  WriteStr(tempStr, Axis.Title.LabelBrush.Style);
  List.Add('Title.LabelBrush.Style ' + tempStr);
  List.Add('Title.Frame.Visible ' + BoolToStr(Axis.Title.Frame.Visible));
  List.Add('Title.Frame.Color ' + ColorToString(Axis.Title.Frame.Color));
  WriteStr(tempStr, Axis.Title.Frame.Style);
  List.Add('Title.Frame.Style ' + tempStr);
  List.Add('Title.Frame.Width ' + IntToStr(Axis.Title.Frame.Width));
  List.Add('Title.Margins.Left ' + IntToStr(Axis.Title.Margins.Left));
  List.Add('Title.Margins.Top ' + IntToStr(Axis.Title.Margins.Top));
  List.Add('Title.Margins.Right ' + IntToStr(Axis.Title.Margins.Right));
  List.Add('Title.Margins.Bottom ' + IntToStr(Axis.Title.Margins.Bottom));

  // Axis range
  // purposely don't store the range since this depends on the actual values
  List.Add('Inverted ' + BoolToStr(Axis.Inverted));

  // Tick labels
  List.Add('Marks.Visible ' + BoolToStr(Axis.Marks{%H-}.Visible));
  List.Add('Marks.Format ' + Axis.Marks{%H-}.Format);
  List.Add('Marks.Distance ' + IntToStr(Axis.Marks{%H-}.Distance));
  List.Add('TickLength ' + IntToStr(Axis.TickLength));
  List.Add('TickInnerLength ' + IntToStr(Axis.TickInnerLength));
  List.Add('TickColor ' + ColorToString(Axis.TickColor));
  List.Add('Marks.LabelFont.Name ' + Axis.Marks{%H-}.LabelFont.Name);
  List.Add('Marks.LabelFont.Size ' + IntToStr(Axis.Marks{%H-}.LabelFont.Size));
  List.Add('Marks.LabelFont.Color ' + ColorToString(Axis.Marks{%H-}.LabelFont.Color));
  List.Add('Marks.LabelFont.Style ' + FontStylesToString(Axis.Marks{%H-}.LabelFont.Style));
  WriteStr(tempStr, Axis.Marks{%H-}.Shape);
  List.Add('Marks.Shape ' + tempStr);
  List.Add('Marks.LabelBrush.Color ' + ColorToString(Axis.Marks{%H-}.LabelBrush.Color));
  WriteStr(tempStr, Axis.Marks{%H-}.LabelBrush.Style);
  List.Add('Marks.LabelBrush.Style ' + tempStr);
  List.Add('Marks.Frame.Visible ' + BoolToStr(Axis.Marks{%H-}.Frame.Visible));
  List.Add('Marks.Frame.Color ' + ColorToString(Axis.Marks{%H-}.Frame.Color));
  WriteStr(tempStr, Axis.Marks{%H-}.Frame.Style);
  List.Add('Marks.Frame.Style ' + tempStr);
  List.Add('Marks.Frame.Width ' + IntToStr(Axis.Marks{%H-}.Frame.Width));
  List.Add('Marks.Margins.Left ' + IntToStr(Axis.Marks{%H-}.Margins.Left));
  List.Add('Marks.Margins.Top ' + IntToStr(Axis.Marks{%H-}.Margins.Top));
  List.Add('Marks.Margins.Right ' + IntToStr(Axis.Marks{%H-}.Margins.Right));
  List.Add('Marks.Margins.Bottom ' + IntToStr(Axis.Marks{%H-}.Margins.Bottom));

  // Grid
  List.Add('Grid.Visible ' + BoolToStr(Axis.Grid.Visible));
  WriteStr(tempStr, Axis.Grid.Style);
  List.Add('Grid.Style ' + tempStr);
  List.Add('Grid.Width ' + IntToStr(Axis.Grid.Width));
  List.Add('Grid.Color ' + ColorToString(Axis.Grid.Color));

  // Frame
  List.Add('Frame.Visible ' + BoolToStr(Chart.Frame.Visible));
  WriteStr(tempStr, Chart.Frame.Style);
  List.Add('Frame.Style ' + tempStr);
  List.Add('Frame.Width ' + IntToStr(Chart.Frame.Width));
  List.Add('Frame.Color ' + ColorToString(Chart.Frame.Color));

  // Arrow
  List.Add('Arrow.Visible ' + BoolToStr(Axis.Arrow.Visible));
  List.Add('Arrow.BaseLength ' + IntToStr(Axis.Arrow.BaseLength));
  List.Add('Arrow.Length ' + IntToStr(Axis.Arrow.Length));
  List.Add('Arrow.Width ' + IntToStr(Axis.Arrow.Width));
 end;

 // Background
 List.Add('BackColor ' + ColorToString(Chart.BackColor));

 // Legend
 // purposely don't store the legend visibility
 WriteStr(tempStr, Chart.Legend.Alignment);
 List.Add('Legend.Alignment ' + tempStr);
 List.Add('Legend.ColumnCount ' + IntToStr(Chart.Legend.ColumnCount));
 List.Add('Legend.Inverted ' + BoolToStr(Chart.Legend.Inverted));
 WriteStr(tempStr, Chart.Legend.ItemFillOrder);
 List.Add('Legend.ItemFillOrder ' + tempStr);
 List.Add('Legend.MarginX ' + IntToStr(Chart.Legend.MarginX));
 List.Add('Legend.MarginY ' + IntToStr(Chart.Legend.MarginY));
 List.Add('Legend.Spacing ' + IntToStr(Chart.Legend.Spacing));
 List.Add('Legend.SymbolWidth ' + IntToStr(Chart.Legend.SymbolWidth));
 List.Add('Legend.UseSidebar ' + BoolToStr(Chart.Legend.UseSidebar));
 // Legend Brush
 List.Add('Legend.BackgroundBrush.Color '
  + ColorToString(Chart.Legend.BackgroundBrush.Color));
 WriteStr(tempStr, Chart.Legend.BackgroundBrush.Style);
 List.Add('Legend.BackgroundBrush.Style ' + tempStr);
 // Legend Font
 List.Add('Legend.Font.Color ' + ColorToString(Chart.Legend.Font.Color));
 List.Add('Legend.Font.Name ' + Chart.Legend.Font.Name);
 List.Add('Legend.Font.Orientation ' + IntToStr(Chart.Legend.Font.Orientation));
 List.Add('Legend.Font.Size ' + IntToStr(Chart.Legend.Font.Size));
 List.Add('Legend.Font.Style ' + FontStylesToString(Chart.Legend.Font.Style));
 // Legend Frame
 List.Add('Legend.Frame.Color ' + ColorToString(Chart.Legend.Frame.Color));
 WriteStr(tempStr, Chart.Legend.Frame.Style);
 List.Add('Legend.Frame.Style ' + tempStr);
 List.Add('Legend.Frame.Visible ' + BoolToStr(Chart.Legend.Frame.Visible));
 List.Add('Legend.Frame.Width ' + IntToStr(Chart.Legend.Frame.Width));

 // Series
 for i:= 0 to Chart.SeriesCount-5 do // omit the TConstantLines
 begin
  Series:= (Chart.Series[i]) as TLineSeries;
  List.Add('LineSeries ' + Series.Name);
  // we don't store the Title since this is set via the .def file
  // also don't show the Active state for the same reason

  // Legend
  List.Add('Legend.Visible ' + BoolToStr(Series.Legend.Visible));
  WriteStr(tempStr, Series.Legend.Multiplicity);
  List.Add('Legend.Multiplicity ' + tempStr);
  // Marks
  WriteStr(tempStr, Series.Marks.Style);
  List.Add('Marks.Style ' + tempStr);
  List.Add('Marks.Format ' + Series.Marks.Format);
  List.Add('Marks.LabelBrush.Color ' + ColorToString(Series.Marks.LabelBrush.Color));
  List.Add('Marks.LabelFont.Color ' + ColorToString(Series.Marks.LabelFont.Color));
  List.Add('Marks.Visible ' + BoolToStr(Series.Marks.Visible));
  // Lines
  List.Add('ShowLines ' + BoolToStr(Series.ShowLines));
  List.Add('SeriesColor ' + ColorToString(Series.SeriesColor));
  WriteStr(tempStr, Series.LinePen.Style);
  List.Add('LinePen.Style ' + tempStr);
  List.Add('LinePen.Width ' + IntToStr(Series.LinePen.Width));
  // Points
  List.Add('ShowPoints ' + BoolToStr(Series.ShowPoints));
  List.Add('Pointer.Brush.Color ' + ColorToString(Series.Pointer.Brush.Color));
  WriteStr(tempStr, Series.Pointer.Brush.Style);
  List.Add('Pointer.Brush.Style ' + tempStr);
  List.Add('Pointer.HorizSize ' + IntToStr(Series.Pointer.HorizSize));
  List.Add('Pointer.Pen.Color ' + ColorToString(Series.Pointer.Pen.Color));
  WriteStr(tempStr, Series.Pointer.Pen.Style);
  List.Add('Pointer.Pen.Style ' + tempStr);
  List.Add('Pointer.Pen.Width ' + IntToStr(Series.Pointer.Pen.Width));
  WriteStr(tempStr, Series.Pointer.Style);
  List.Add('Pointer.Style ' + tempStr);
 end;

 // Title
 // purposely don't store the title text since this can cause issues
 // when another user starts the program for another measurement
 WriteStr(tempStr, Chart.Title.Alignment);
 List.Add('Title.Alignment ' + tempStr);
 WriteStr(tempStr, Chart.Title.Shape);
 List.Add('Title.Shape ' + tempStr);
 List.Add('Title.Wordwrap ' + BoolToStr(Chart.Title.Wordwrap));
 // Title Brush
 List.Add('Title.Brush.Color '
  + ColorToString(Chart.Title.Brush.Color));
 WriteStr(tempStr, Chart.Title.Brush.Style);
 List.Add('Title.Brush.Style ' + tempStr);
 // Title Font
 List.Add('Title.Font.Color ' + ColorToString(Chart.Title.Font.Color));
 List.Add('Title.Font.Name ' + Chart.Title.Font.Name);
 List.Add('Title.Font.Orientation ' + IntToStr(Chart.Title.Font.Orientation));
 List.Add('Title.Font.Size ' + IntToStr(Chart.Title.Font.Size));
 List.Add('Title.Font.Style ' + FontStylesToString(Chart.Title.Font.Style));
 // Title Frame
 List.Add('Title.Frame.Color ' + ColorToString(Chart.Title.Frame.Color));
 WriteStr(tempStr, Chart.Title.Frame.Style);
 List.Add('Title.Frame.Style ' + tempStr);
 List.Add('Title.Frame.Visible ' + BoolToStr(Chart.Title.Frame.Visible));
 List.Add('Title.Frame.Width ' + IntToStr(Chart.Title.Frame.Width));
 // Title Margins
 List.Add('Title.Margins.Left ' + IntToStr(Chart.Title.Margins.Left));
 List.Add('Title.Margins.Top ' + IntToStr(Chart.Title.Margins.Top));
 List.Add('Title.Margins.Right ' + IntToStr(Chart.Title.Margins.Right));
 List.Add('Title.Margins.Bottom ' + IntToStr(Chart.Title.Margins.Bottom));

 // Scrolling View
 List.Add('ScrollIntervalFSE.Value ' + FloatToStr(MainForm.ScrollIntervalFSE.Value));

 // save the list
 List.SaveToFile(iniFile);

finally
 List.Free;
end;

end;

procedure TSIXControl.SCDataPointClickToolPointClick(ATool: TChartTool;
  APoint: TPoint);
var
 NoteText, NotesFile, OutputLine, helpString : string;
 tool : TDataPointTool;
 series : TChartSeries;
 NotesFileStream : TFileStream;
 MousePointer : TPoint;
 i : integer;
begin
 MousePointer:= Mouse.CursorPos;
 tool:= ATool as TDataPointTool;
 if tool.PointIndex < 0 then
  exit;

 series:= tool.Series as TChartSeries;
 NoteText:= series.Source[tool.PointIndex]^.Text;
 with NoteEditingF do
 begin
  NoteLabelL.Caption:= 'Note for ' + series.Title + ':';
  NoteTextM.Lines.Text:= NoteText;
  ShowModal;
  // if nothing was changed, do nothing
  if (CompareStr(series.Source[tool.PointIndex]^.Text,
                 NoteTextM.Lines.Text) = 0)
    or (ModalResult = mrCancel) then
   exit
  else
  begin
   // we must strip empty lines to avoid problems on reading
   for i:= NoteTextM.Lines.Count - 1 downto 0 do
    if NoteTextM.Lines[i].IsEmpty then
     NoteTextM.Lines.Delete(i)
    else
     break;
   // we must also strip a possible line ending of last line
   helpString:= NoteTextM.Lines.Text;
   if Pos(LineEnding, helpString) > 0 then
    helpString:= Copy(helpString, 0, Length(helpString) - Length(LineEnding));
   series.Source[tool.PointIndex]^.Text:= helpString;
  end;
 end;

 // force a redraw of the chart
 MainForm.SIXCH.Invalidate;

 // write mark to the .notes file
 // we write into the same folder than the program .exe
 if InNameSensor.IsEmpty then
  NotesFile:= 'unknown.notes'
 else
  NotesFile:= ExtractFileName(InNameSensor) + '.notes';
 NotesFile:= ExtractFilePath(Application.ExeName) + NotesFile;

 try
  try
   if FileExists(NotesFile) then
    NotesFileStream:= TFileStream.Create(NotesFile, fmOpenReadWrite or fmShareDenyNone)
   else
    NotesFileStream:= TFileStream.Create(NotesFile, fmCreate or fmShareDenyNone);
  except
   on EFOpenError do
   begin
    MessageDlgPos('Notes file is used by another program and cannot be opened.'
                  + LineEnding + 'New notes cannot be saved.',
                  mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    exit;
   end;
  end;
  // seek to end and append the mark
  NotesFileStream.Seek(0, soFromEnd);
  OutputLine:= series.Name + #9 + IntToStr(tool.PointIndex) + LineEnding
               + helpString + LineEnding + LineEnding;
  try
   NotesFileStream.Write(OutputLine[1], Length(OutputLine));
  except
   NotesFileStream.Free;
   MessageDlgPos('Notes could not be written to the .notes file.',
                 mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
  end;

 finally
  NotesFileStream.Free;
 end;

end;

procedure TSIXControl.ReadNotes;
// reads marks from .notes file ad adds them to the chart
var
 NotesFileStream : TFileStream;
 LineReader : TStreamReader;
 StringArray : TStringArray;
 series : TChartSeries;
 ReadLine, NotesFile, OutputLine : string;
 MousePointer : TPoint;
 rowCounter, PointIndex : integer;
 skipNote : Boolean;
begin
 // initialize
 rowCounter:= 0;
 series:= nil;
 MousePointer:= Mouse.CursorPos;

 NotesFile:= ExtractFileName(InNameSensor) + '.notes';
 NotesFile:= ExtractFilePath(InNameSensor) + NotesFile;
 if not FileExists(NotesFile) then
  // nothing to do
  exit;

try
 try
  NotesFileStream:= TFileStream.Create(NotesFile, fmOpenRead or fmShareDenyNone)
 except
  on EFOpenError do
  begin
   MessageDlgPos('Notes file is used by another program and cannot be opened.'
                 + LineEnding + 'New notes cannot be saved.',
                 mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   exit;
  end;
 end;
 LineReader:= TStreamReader.Create(NotesFileStream);

 // parse the file to the end
 while not LineReader.Eof do
 begin
  skipNote:= false;
  inc(rowCounter);
  LineReader.ReadLine(ReadLine);
  StringArray:= ReadLine.Split(#9);
  // the first part is the series name the second one the point index
  series:= (MainForm.FindComponent(StringArray[0]) as TChartSeries);
  if series = nil then
  begin
   MessageDlgPos('In line ' + IntToStr(rowCounter) + ' "' + StringArray[0]
                 + '" is no valid chart series name.'
                 + LineEnding + 'This note could not be read.',
                 mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   skipNote:= true;
  end;
  if not skipNote then
  begin
   if not TryStrToInt(StringArray[1], PointIndex) then
   begin
    MessageDlgPos('Point index in line ' + IntToStr(rowCounter)
                  + ' is no valid integer.' + LineEnding
                  + 'This note could not be read.',
                  mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    skipNote:= true;
   end;
  if PointIndex < 0 then
   begin
   MessageDlgPos('Point index in line ' + IntToStr(rowCounter)
                  + ' is negative.' + LineEnding
                  + 'This note could not be read.',
                 mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    skipNote:= true;
   end
   else if PointIndex > series.Count then
   begin
    MessageDlgPos('Point index in line ' + IntToStr(rowCounter)
                  + ' is "' + IntToStr(PointIndex)
                  + '" and thus larger than "' + IntToStr(series.Count)
                  + '", the number of points in the series.'
                  + LineEnding + 'This note could not be read.',
                  mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    skipNote:= true;
   end;
  end; // end if not skipNote

  // now read as many lines until there is an empty line (2 subsequent LineEndings)
  OutputLine:= '';
  repeat
   inc(rowCounter);
   LineReader.ReadLine(ReadLine);
   OutputLine:= OutputLine + ReadLine + LineEnding;
  // we can have an empty line for a deleted note, therefore until we
  // have at least 2 LineEndings
  until ReadLine.IsEmpty and (Length(OutputLine) >= 2 * Length(LineEnding));
  // remove the two LineEndings at the end
  OutputLine:= Copy(OutputLine, 0, Length(OutputLine) - 2 * Length(LineEnding));
  // set the mark
  if not skipNote then
   series.Source[PointIndex]^.Text:= OutputLine;
 end; // end of while not LineReader.Eof do

finally
 LineReader.Free;
 NotesFileStream.Free;
 // force a redraw of the chart
 MainForm.SIXCH.Invalidate;
end;

end;

function TSIXControl.FontStylesToString(FontStyles: TFontStyles): string;
begin
  result := '';
  if fsBold in FontStyles then
   result:= result + IntToStr(Ord(fsBold)) + ',';
  if fsItalic in FontStyles then
   result:= result + IntToStr(Ord(fsItalic)) + ',';
  if fsUnderline in FontStyles then
   result:= result + IntToStr(Ord(fsUnderline)) + ',';
  if fsStrikeOut in FontStyles then
   result:= result + IntToStr(Ord(fsStrikeOut)) + ',';
  RemoveTrailingChars(result, [',']);
end;

procedure TSIXControl.SCLoadAppearance(iniFile: string);
var
 i, m : integer;
 Chart : TChart;
 Axis : TChartAxis;
 Series : TLineSeries;
 List : TStringList;
 tempAlignment : TAlignment;
 tempShape : TChartLabelShape;
 tempBrushStyle : TBrushStyle;
 tempStyle : TPenStyle;
 tempLegendAlignment : TLegendAlignment;
 tempFillOrder : TLegendItemFillOrder;
 tempMultiplicity : TLegendMultiplicity;
 tempMarksStyle : TSeriesMarksStyle;
 tempPointerStyle : TSeriesPointerStyle;
 Abool : Boolean;
begin

 try
  List:= TStringList.Create;
  List.LoadFromFile(iniFile);
  m:= 0;
  // first readout the number of pumps
  // this line might not be there for older appearance files
  if TryStrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length), i) then
  begin
   MainForm.PumpNumberSE.Value:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
  end;
  // readout the number of valves
  // also the valve line might not be there for older appearance files
  if TryStrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length), i) then
  begin
   MainForm.ValveNumberSE.Value:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
  end;
  // readout the last shown channels
  // also the channel lines might not be there for older appearance files
  if TryStrToBool(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length), Abool) then
  begin
   MainForm.Channel1OnOffCB.Checked:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   // we know now that there are 7 more lines
   for i:= 2 to 8 do
   begin
    (MainForm.FindComponent('Channel' + IntToStr(i) + 'OnOffCB')
      as TCheckBox).Checked:= StrToBool(
          Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
    inc(m);
   end;
  end;

  // now read the chart properties
  Chart:= (MainForm.FindComponent(
           Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length))
           as TChart);
  // Axes
  for i:= 0 to Chart.AxisList.Count-1 do
  begin
   inc(m);
   Axis:= Chart.AxisList[StrToInt(
           Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length)
          )];
   // Axis title
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempAlignment);
   Axis.Title.Alignment:= tempAlignment;
   inc(m);
   Axis.Title.LabelFont.Name:=
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length);
   inc(m);
   Axis.Title.LabelFont.Size:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Title.LabelFont.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Title.LabelFont.Style:= StringToFontStyles(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Title.LabelFont.Orientation:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Title.Distance:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempShape);
   Axis.Title.Shape:= tempShape;
   inc(m);
   Axis.Title.LabelBrush.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempBrushStyle);
   Axis.Title.LabelBrush.Style:= tempBrushStyle;
   inc(m);
   Axis.Title.Frame.Visible:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Title.Frame.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempStyle);
   Axis.Title.Frame.Style:= tempStyle;
   inc(m);
   Axis.Title.Frame.Width:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Title.Margins.Left:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Title.Margins.Top:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Title.Margins.Right:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Title.Margins.Bottom:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   // Axis range
   inc(m);
   Axis.Inverted:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   // Tick labels
   inc(m);
   Axis.Marks{%H-}.Visible:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Marks{%H-}.Format:= Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length);
   inc(m);
   Axis.Marks{%H-}.Distance:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.TickLength:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.TickInnerLength:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.TickColor:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Marks{%H-}.LabelFont.Name:=
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length);
   inc(m);
   Axis.Marks{%H-}.LabelFont.Size:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Marks{%H-}.LabelFont.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Marks{%H-}.LabelFont.Style:= StringToFontStyles(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempShape);
   Axis.Marks{%H-}.Shape:= tempShape;
   inc(m);
   Axis.Marks{%H-}.LabelBrush.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempBrushStyle);
   Axis.Marks{%H-}.LabelBrush.Style:= tempBrushStyle;
   inc(m);
   Axis.Marks{%H-}.Frame.Visible:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Marks{%H-}.Frame.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempStyle);
   Axis.Marks{%H-}.Frame.Style:= tempStyle;
   inc(m);
   Axis.Marks{%H-}.Frame.Width:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Marks{%H-}.Margins.Left:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Marks{%H-}.Margins.Top:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Marks{%H-}.Margins.Right:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Marks{%H-}.Margins.Bottom:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   // Grid
   inc(m);
   Axis.Grid.Visible:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempStyle);
   Axis.Grid.Style:= tempStyle;
   inc(m);
   Axis.Grid.Width:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Grid.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   // Frame
   inc(m);
   Chart.Frame.Visible:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempStyle);
   Chart.Frame.Style:= tempStyle;
   inc(m);
   Chart.Frame.Width:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Chart.Frame.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   // Arrow
   inc(m);
   Axis.Arrow.Visible:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Arrow.BaseLength:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Arrow.Length:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Axis.Arrow.Width:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  end;

  // Background
  inc(m);
  Chart.BackColor:= StringToColor(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));

  // Legend
  inc(m);
  ReadStr(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
   tempLegendAlignment);
  Chart.Legend.Alignment:= tempLegendAlignment;
  inc(m);
  Chart.Legend.ColumnCount:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Legend.Inverted:= StrToBool(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  ReadStr(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
   tempFillOrder);
  Chart.Legend.ItemFillOrder:= tempFillOrder;
  inc(m);
  Chart.Legend.MarginX:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Legend.MarginY:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Legend.Spacing:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Legend.SymbolWidth:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Legend.UseSidebar:= StrToBool(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  // Legend Brush
  inc(m);
  Chart.Legend.BackgroundBrush.Color:= StringToColor(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  ReadStr(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
   tempBrushStyle);
  Chart.Legend.BackgroundBrush.Style:= tempBrushStyle;
  // Legend Font
  inc(m);
  Chart.Legend.Font.Color:= StringToColor(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Legend.Font.Name:= Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length);
  inc(m);
  Chart.Legend.Font.Orientation:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Legend.Font.Size:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Legend.Font.Style:= StringToFontStyles(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  // Legend Frame
  inc(m);
  Chart.Legend.Frame.Color:= StringToColor(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  ReadStr(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
   tempStyle);
  Chart.Legend.Frame.Style:= tempStyle;
  inc(m);
  Chart.Legend.Frame.Visible:= StrToBool(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Legend.Frame.Width:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));

  // Series
  for i:= 0 to Chart.SeriesCount-5 do // omit the TConstantLines
  begin
   inc(m);
   Series:= (MainForm.FindComponent(
             Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length))
             as TLineSeries);

   // Legend
   inc(m);
   Series.Legend.Visible:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempMultiplicity);
   Series.Legend.Multiplicity:= tempMultiplicity;
   // Marks
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempMarksStyle);
   Series.Marks.Style:= tempMarksStyle;
   inc(m);
   Series.Marks.Format:= Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length);
   inc(m);
   Series.Marks.LabelBrush.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Series.Marks.LabelFont.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Series.Marks.Visible:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   // Lines
   inc(m);
   Series.ShowLines:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Series.SeriesColor:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempStyle);
   Series.LinePen.Style:= tempStyle;
   inc(m);
   Series.LinePen.Width:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   // Points
   inc(m);
   Series.ShowPoints:= StrToBool(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Series.Pointer.Brush.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempBrushStyle);
   Series.Pointer.Brush.Style:= tempBrushStyle;
   inc(m);
   Series.Pointer.HorizSize:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   Series.Pointer.Pen.Color:= StringToColor(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempStyle);
   Series.Pointer.Pen.Style:= tempStyle;
   inc(m);
   Series.Pointer.Pen.Width:= StrToInt(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
   inc(m);
   ReadStr(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
    tempPointerStyle);
   Series.Pointer.Style:= tempPointerStyle;
  end;

  // Title
  inc(m);
  ReadStr(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
   tempAlignment);
  Chart.Title.Alignment:= tempAlignment;
  inc(m);
  ReadStr(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
   tempShape);
  Chart.Title.Shape:= tempShape;
  inc(m);
  Chart.Title.Wordwrap:= StrToBool(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  // Title Brush
  inc(m);
  Chart.Title.Brush.Color:= StringToColor(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  ReadStr(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
   tempBrushStyle);
  Chart.Title.Brush.Style:= tempBrushStyle;
  // Title Font
  inc(m);
  Chart.Title.Font.Color:= StringToColor(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Title.Font.Name:= Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length);
  inc(m);
  Chart.Title.Font.Orientation:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Title.Font.Size:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Title.Font.Style:= StringToFontStyles(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  // Title Frame
  inc(m);
  Chart.Title.Frame.Color:= StringToColor(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  ReadStr(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length),
   tempStyle);
  Chart.Title.Frame.Style:= tempStyle;
  inc(m);
  Chart.Title.Frame.Visible:= StrToBool(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Title.Frame.Width:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  // Title Margins
  inc(m);
  Chart.Title.Margins.Left:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Title.Margins.Top:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Title.Margins.Right:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  inc(m);
  Chart.Title.Margins.Bottom:= StrToInt(
   Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));

  // Scrolling View
  // this line might not be there for older appearance files
  if List.Count > m + 1 then
  begin
   inc(m);
   MainForm.ScrollIntervalFSE.Value:= StrToFloat(
    Copy(List[m], Pos(' ', List[m]) + 1, List[m].Length));
  end;

 finally
  List.Free;
 end;
end;

function TSIXControl.StringToFontStyles(s: string): TFontStyles;
var
 i : integer;
begin
 result:= [];
 for i:= 1 to WordCount(s, [',']) do
  result:= result + [TFontStyle(StrToInt(ExtractWord(i, s, [','])))];
end;

procedure TSIXControl.TimeXMIClick(Sender: TObject; factor: integer);
var
 extent: TDoubleRect;
 previousScale : double;
begin
 // store current zoom state because changing scale will zoom out
 extent:= MainForm.SIXCH.LogicalExtent;
 // save the scale to be able to recalculate the new range min/max
 previousScale:= MainForm.ValuesLinearTransform.Scale;
 MainForm.ValuesLinearTransform.Scale:= factor;
 // set back zoom state
 MainForm.SIXCH.Prepare;
 MainForm.SIXCH.LogicalExtent:= extent;
 MainForm.SIXCH.BottomAxis.Range.Min:= MainForm.SIXCH.BottomAxis.Range.Min
   * previousScale / MainForm.ValuesLinearTransform.Scale;
 MainForm.SIXCH.BottomAxis.Range.Max:= MainForm.SIXCH.BottomAxis.Range.Max
   * previousScale / MainForm.ValuesLinearTransform.Scale;
end;

procedure TSIXControl.SCTimeDayMIClick(Sender: TObject);
begin
 TimeXMIClick(Sender, 1440); // 24 * 60
 MainForm.SIXCH.BottomAxis.Title.Caption:= 'Time [day]';
 MainForm.TimeMinuteMI.Checked:= false;
 MainForm.TimeHourMI.Checked:= false;
 MainForm.TimeDayMI.Checked:= true;
end;

procedure TSIXControl.SCTimeHourMIClick(Sender: TObject);
begin
 TimeXMIClick(Sender, 60);
 MainForm.SIXCH.BottomAxis.Title.Caption:= 'Time [hour]';
 MainForm.TimeMinuteMI.Checked:= false;
 MainForm.TimeHourMI.Checked:= true;
 MainForm.TimeDayMI.Checked:= false;
end;

procedure TSIXControl.SCTimeMinuteMIClick(Sender: TObject);
begin
 TimeXMIClick(Sender, 1);
 MainForm.SIXCH.BottomAxis.Title.Caption:= 'Time [min]';
 MainForm.TimeMinuteMI.Checked:= true;
 MainForm.TimeHourMI.Checked:= false;
 MainForm.TimeDayMI.Checked:= false;
end;

procedure TSIXControl.SCSIXCHAxisList1GetMarkText(Sender: TObject;
var
 AText : String;
 AMark : Double);
begin
 if MainForm.TimeDaysHoursMinMI.Checked then
 begin
  AText:= SIXControl.CalcDaysHoursMins(AMark);
  MainForm.SIXCH.BottomAxis.Intervals.MaxLength:= 100;
  MainForm.SIXCH.BottomAxis.Title.Caption:= 'Time [dd:hh:mm]';
 end
 else
 begin
  // do nothing
  AText:= Format(MainForm.SIXCH.BottomAxis.Marks{%H-}.Format, [AMark]);
  // use the default width for labels
  MainForm.SIXCH.BottomAxis.Intervals.MaxLength:= 50;
  if MainForm.TimeMinuteMI.Checked then
   MainForm.SIXCH.BottomAxis.Title.Caption:= 'Time [min]'
  else if MainForm.TimeHourMI.Checked then
   MainForm.SIXCH.BottomAxis.Title.Caption:= 'Time [hour]'
  else if MainForm.TimeDayMI.Checked then
   MainForm.SIXCH.BottomAxis.Title.Caption:= 'Time [day]';
 end;
end;

procedure TSIXControl.SCAutoscaleMIClick(Sender: TObject);
begin
 // sensor data axis
 MainForm.SIXCH.AxisList[0].Range.UseMax:= False;
 MainForm.SIXCH.AxisList[0].Range.UseMin:= False;
 // temperature axis
 MainForm.SIXCH.AxisList[2].Range.UseMax:= False;
 MainForm.SIXCH.AxisList[2].Range.UseMin:= False;
 // time axis
 MainForm.SIXCH.BottomAxis.Range.UseMax:= false;
 MainForm.SIXCH.BottomAxis.Range.UseMin:= false;
 // for the x-axis also te extent must be set
 MainForm.SIXCH.Extent.UseXMax:= false;
 MainForm.SIXCH.Extent.UseXMin:= false;

 // when in scrolling mode the change to the extent zoomed out completely
 // and we must trigger SCScrollViewCBChange to bring it back to scrolling
 if MainForm.ScrollViewCB.Checked then
  SCScrollViewCBChange(Sender);
end;

procedure TSIXControl.SCHideNotesMIClick(Sender: TObject);
var
 i: integer;
 lineSeries : TLineSeries;
begin
 if MainForm.HideNotesMI.Checked then
 begin
  for i:= 0 to MainForm.SIXCH.SeriesCount - 1 do
  begin
   if MainForm.SIXCH.Series[i] is TLineSeries then
   begin
    lineSeries:= MainForm.SIXCH.Series[i] as TLineSeries;
    //series:= ATool.Series as TLineSeries;
    lineSeries.Marks.Visible:= false;
   end;
  end;
  MainForm.HideNotesMI.Checked:= false;
 end
 else
 begin
  for i:= 0 to MainForm.SIXCH.SeriesCount - 1 do
  begin
   if MainForm.SIXCH.Series[i] is TLineSeries then
   begin
    lineSeries:= MainForm.SIXCH.Series[i] as TLineSeries;
    lineSeries.Marks.Visible:= true;
   end;
  end;
  MainForm.HideNotesMI.Checked:= true;
 end;
end;

procedure TSIXControl.SubstMeasureCLBItemClick(ASender: TObject; AIndex: Integer);
var
 i, k, l : integer;
 SelectedName : string;
 SelectedSeries : TChartSeries;
 Average, StepConcentrationValue : array[1..7] of double;
begin
 // update average if there is a channel selected for the result diagram
 SelectedName:= '';
 SelectedSeries:= nil;
 if MainForm.MeasurementChannelsPC.ActivePage = MainForm.GlucoseMeasureTS then
 begin
  // don't do anything if nothing is selected
  if MainForm.GlucoseMeasureCLB.SelCount = 0 then
   exit;
  for i:= 0 to MainForm.GlucoseMeasureCLB.SeriesCount-1 do
  begin
   if not MainForm.GlucoseMeasureCLB.Selected[i] then
    continue;
   SelectedSeries:= MainForm.GlucoseCalibCLB.Series[i] as TChartSeries;
  end;
 end
 else if MainForm.MeasurementChannelsPC.ActivePage = MainForm.LactateMeasureTS then
 begin
  // don't do anything if nothing is selected
  if MainForm.LactateMeasureCLB.SelCount = 0 then
   exit;
  for i:= 0 to MainForm.LactateMeasureCLB.SeriesCount-1 do
  begin
   if not MainForm.LactateMeasureCLB.Selected[i] then
    continue;
   SelectedSeries:= MainForm.LactateMeasureCLB.Series[i] as TChartSeries;
  end;
 end;
 SelectedName:= SelectedSeries.Name;
 if SelectedName = SelectedName then
  exit;
 // replace 'Values' by 'Results'
 SelectedName:= Copy(SelectedName, 0, SelectedName.Length-1-6);
 SelectedName:= SelectedName + 'Results';
 k:= 0;
 for i:= 0 to SelectedSeries.Count - 1 do
 begin
  for l:= 1 to 7 do
  begin
   Average[l]:= 0;
   StepConcentrationValue[l]:= (MainForm.FindComponent('Step' + IntToStr(l) + 'MeasureValueFSE')
                                as TFloatSpinEdit).Value;
   if (StepConcentrationValue[l] > 0) and
     ((MainForm.FindComponent(SelectedName) as TLineSeries).XValue[i] =
      StepConcentrationValue[l]) then
   begin
    Average[l]:= Average[l] + (MainForm.FindComponent(SelectedName) as TLineSeries).YValue[i];
    inc(k);
   end;
  end;
  Average[l]:= Average[l] / k;
  MainForm.ResultCHAverages.YValue[l]:= Average[l];
 end;
end;


end. //unit

