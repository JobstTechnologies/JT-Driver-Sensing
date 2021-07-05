unit SIXControlUnit;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Crt, Streamex,
  Dialogs, StdCtrls, ExtCtrls, Spin, Buttons, ComCtrls, LazFileUtils,
  TAGraph, TASeries, TATools, TAChartUtils, TADrawerSVG, TAFuncSeries, Math,
  Types, TATextElements, TALegend, TACustomSeries, TAChartAxis, ceAxisFrame,
  TAGeometry,
  // custom forms
  JTDriverSensingMain, NameSetting;

type

  TSIXControl = class
    constructor create;
    procedure SCScrollViewCBChange(Sender: TObject);
    procedure SCChartToolsetDataPointHintToolHintPosition(
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
    procedure SCChartToolsetTitleFootClickTool1Click(Sender: TChartTool;
      Title: TChartTitle);
    procedure SCChartToolsetLegendClickTool1Click(Sender: TChartTool;
      Legend: TChartLegend);
    procedure SCChartToolsetAxisClickTool1Click(Sender: TChartTool;
      AnAxis: TChartAxis; HitInfo: TChartAxisHitTests);
    procedure SCAppearanceXBBClick(Sender: TObject);
    procedure SCChannelXOnOffCBChange(Sender: TObject);
    procedure SCShowTempCBChange(Sender: TObject);
    procedure SCAnOutConnectorXOnOffCBChange(Sender: TObject);
    procedure SCAnOutOfXLEChange(Sender: TObject);
    procedure SCAnOutOnOffTBChange(Sender: TObject);
    procedure SCCalibrateTBChange(Sender: TObject);
    procedure SCChartDblClick(Sender: TObject);

  private

  public
    function SaveCSV(Overwrite: Boolean; ChartName: string): Boolean;
    function SaveScreenshot(Overwrite: Boolean; ChartName: string): Boolean;
    function ParseLine(Line: string; channel: Byte): double;
    function ParseDefFile(InFile: string): Boolean;
    function AdjustExtent : TDoubleRect;

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
  Started: Boolean = false; // if Start was pressed or not
  Gains : array [1..6] of single; // the channel gains
  GainsRaw : array [1..6] of double; // the default gains
  Subtracts : array [1..6] of integer; // the channel subtracts
  TemperGains : array [1..8] of single; // the temperature gains
  ErrorCount : integer = 0; // counts how many times we did not reeive a stop bit
  wasNoStopByte : Boolean = false; // to catch the case no stop byte was sent
  inCalibration : Boolean = false; // to prevent chart scrolling while calibrating
  MinExtentY : double = Infinity; // store the chart's extent min Y value
  MaxExtentY : double = Infinity; // store the chart's extent max Y value

implementation

uses
  Fitting, Calibration,
  // ChartEditing units
  ceTitleFootDlg, ceLegendDlg, ceSeriesDlg, ceAxisDlg;

constructor TSIXControl.create;
begin
 evalTimeChanged:= false;
end;

procedure TSIXControl.SCReadTimerTimerFinished(Sender: TObject);
type intArray = array[1..4] of byte;
     PintArray = ^intArray;
var
 OutLine : string;
 slope, temperature, lastInterval, ScrollInterval : double;
 i, k, StopPos, ItemIndex: integer;
 Extent : TDoubleRect;
 MousePointer : TPoint;
 dataArray : array[0..24] of byte;
 HiLowArray : array[0..1] of byte;
 Chan : array [1..6] of Int16;
 ChanDbl : array [0..8] of double; // start from zero purposely for non-existing subtracts
 ChanRawDbl : array [1..8] of double;
 prevChan : array [1..8] of double;
 checksum : integer;
 tempInt16: Int16;
 PintegerArray: PintArray;
 wasRead : Boolean = false;
 SingleByte : byte;
begin
 // tell the OS the application is alive
 Application.ProcessMessages;

 // initialize
 MousePointer:= Mouse.CursorPos;
 lastInterval:= 0.0;
 for i:= 0 to 8 do
  ChanDbl[i]:= 0.0;

 // first check if we still have a filestream
 if not HaveSensorFileStream then
 begin
  MainForm.ReadTimer.Enabled:= false;
  MessageDlgPos('The connection to the data file was lost!' + LineEnding
    + 'To restart you must call again the menu Connection -> SIX bisensors',
   mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
  MainForm.ConnComPortSensLE.Color:= clRed;
  MainForm.IndicatorSensorP.Caption:= 'Connection lost';
  MainForm.IndicatorSensorP.Color:= clRed;
  // disable all buttons
  MainForm.StartTestBB.Enabled:= false;
  MainForm.StopTestBB.Enabled:= false;
  MainForm.CloseLazSerialConn(MousePointer);
  HaveSerialSensor:= False;
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
    MainForm.ConnComPortSensLE.Color:= clRed;
    MainForm.IndicatorSensorP.Caption:= 'SIX error';
    MainForm.IndicatorSensorP.Color:= clRed;
    MainForm.StartTestBB.Enabled:= false;
    MainForm.StopTestBB.Enabled:= false;
    MainForm.CloseLazSerialConn(MousePointer);
    HaveSerialSensor:= False;
    exit;
   end;
  end;

  // check if there are 25 bytes available to be read
  // if not wait another 100 ms until the timer finished the next time
  while serSensor.WaitingDataEx < 25 do
  begin
   delay(100);
   lastInterval:= lastInterval + 0.00166; // 100 ms of the delay in min
   inc(DelayReadCounter);
   if DelayReadCounter > 50 then
   // we reached 3 times the 1.7 s SIX output cycle, so there is something wrong
   // can also occur if USB cable was removed
   begin
    // often the SIX only stops telling it has not enough data
    // to try to read data
    try
     k:= serSensor.RecvBufferEx(@dataArray[0], 25, 100);
     wasRead:= true;
    finally
     if k <> 25 then
     begin
      MainForm.ReadTimer.Enabled:= false;
      MessageDlgPos('Error: ' + MainForm.ConnComPortSensLE.Text +
       ' did not deliver data within 5.1 s.',
       mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
      MainForm.ConnComPortSensLE.Color:= clRed;
      MainForm.IndicatorSensorP.Caption:= 'SIX error';
      MainForm.IndicatorSensorP.Color:= clRed;
      MainForm.StartTestBB.Enabled:= false;
      MainForm.StopTestBB.Enabled:= false;
      MainForm.CloseLazSerialConn(MousePointer);
      HaveSerialSensor:= False;
      MainForm.AnOutOnOffTB.Checked:= false;
      MainForm.AnOutOnOffTB.Enabled:= false;
      MainForm.AnOutOnOffTB.Hint:= 'Outputs the sensor signal' + LineEnding
                          + 'to the pump connectors.' + LineEnding
                          + 'Connect to a SIX and a pump driver'  + LineEnding
                          + 'to enable the button.';
      exit;
     end;
     DelayReadCounter:= 0;
    end;
   end;
   if wasRead then
    break;
  end;
 finally
  if serSensor.LastError <> 0 then // occurs if USB cable was removed
  begin
   MainForm.ReadTimer.Enabled:= False;
   MessageDlgPos(MainForm.ConnComPortSensLE.Text + ' error on connecting to SIX: '
    + serSensor.LastErrorDesc, mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   MainForm.ConnComPortSensLE.Color:= clRed;
   MainForm.IndicatorSensorP.Caption:= 'Check USB cable';
   MainForm.IndicatorSensorP.Color:= clRed;
   MainForm.StartTestBB.Enabled:= false;
   MainForm.StopTestBB.Enabled:= false;
   MainForm.CloseLazSerialConn(MousePointer);
   HaveSerialSensor:= False;
   MainForm.AnOutOnOffTB.Checked:= false;
   MainForm.AnOutOnOffTB.Enabled:= false;
   MainForm.AnOutOnOffTB.Hint:= 'Outputs the sensor signal' + LineEnding
                       + 'to the pump connectors.' + LineEnding
                       + 'Connect to a SIX and a pump driver'  + LineEnding
                       + 'to enable the button.';
   exit;
  end;
 end;

 // read the data
 if not wasRead then
  k:= serSensor.RecvBufferEx(@dataArray[0], 25, 100);

 // in case the read failed or not 25 bytes received
 if (serSensor.LastError <> 0) or (k <> 25) then
 begin
  inc(ErrorCount);
  // we wait then another timer run
  // if we get 3 times the same error, something is wrong and we must stop
  if ErrorCount < 4 then
  begin
   lastInterval:= lastInterval + 1700 / 60000; // in min, every 1700 ms we get new bytes
   timeCounter:= timeCounter + lastInterval;
   exit;
  end
  else
  begin
   MainForm.ReadTimer.Enabled:= False;
   if serSensor.LastError <> 0 then
    MessageDlgPos(MainForm.ConnComPortSensLE.Text + ' error on reading signal data: '
     + serSensor.LastErrorDesc, mtError, [mbOK], 0, MousePointer.X, MousePointer.Y)
   else
    MessageDlgPos('Error: Could not read 25 bytes. Got only ' + IntToStr(k) + ' bytes.',
     mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   MainForm.ConnComPortSensLE.Color:= clRed;
   MainForm.IndicatorSensorP.Caption:= 'SIX error';
   MainForm.IndicatorSensorP.Color:= clRed;
   // disable all buttons
   MainForm.StartTestBB.Enabled:= false;
   MainForm.StopTestBB.Enabled:= false;
   MainForm.CloseLazSerialConn(MousePointer);
   HaveSerialSensor:= False;
   exit;
  end;
 end;

 // now search the byte array for the stop bit
 StopPos:= -1;
 for i:= 0 to 24 do
 begin
  if dataArray[i] = $16 then
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
  // to re-sync then with the SIX, wait one interval and read out all byte by
  // byte until a stop byte appears
  wasNoStopByte:= true;
  // however, if we cannot re-sync after 3 attempts, something is wrong with the
  // SIX and we must stop
  if ErrorCount < 4 then
  begin
   lastInterval:= lastInterval + 1700 / 60000; // in min, every 1700 ms we get new bytes
   timeCounter:= timeCounter + lastInterval;
   exit;
  end
  else
  begin
   MainForm.ReadTimer.Enabled:= False;
   MessageDlgPos('The received last 300 bytes do not contain a stop bit.'
    + LineEnding + 'Try to reconnect to the SIX.',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   MainForm.ConnComPortSensLE.Color:= clRed;
   MainForm.IndicatorSensorP.Caption:= 'SIX error';
   MainForm.IndicatorSensorP.Color:= clRed;
   MainForm.StartTestBB.Enabled:= false;
   MainForm.StopTestBB.Enabled:= false;
   MainForm.CloseLazSerialConn(MousePointer);
   HaveSerialSensor:= False;
   exit;
  end;
 end;

 // reset counter since we got no error
 ErrorCount:= 0;

 // if StopPos > 19 we have all relevant data before
 if StopPos > 19 then
 begin
  // checksum
  checksum:= dataArray[StopPos-2];
  for i:= StopPos-3 downto StopPos-20 do
   checksum:= checksum + dataArray[i];
  PintegerArray:= PintArray(@checksum);
  if PintegerArray^[1] <> dataArray[StopPos-1] then
  begin
   // the data are corrupted so wait for another timer run
   lastInterval:= lastInterval + 1700 / 60000; // in min, every 1700 ms we get new bytes
   timeCounter:= timeCounter + lastInterval;
   exit;
  end;
  // transform the dataArray so that zero array position gets the first value byte
  for i := 0 to 18 do
   dataArray[i]:= dataArray[StopPos - 19 + i];
 end
 else // we must wait another timer run
 begin
  lastInterval:= lastInterval + 1700 / 60000; // in min, every 1700 ms we get new bytes
  timeCounter:= timeCounter + lastInterval;
  exit;
 end;

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
 i:= NumChannels;
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
 for i:= 1 to NumChannels do
  ChanRawDbl[i]:= Chan[i] * GainsRaw[i] / 100;

 if (MainForm.LoadedDefFileM.Text <> 'None') then
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
 if (MainForm.LoadedDefFileM.Text <> 'None')
  and (not MainForm.NoSubtractBlankCB.Checked) then
  // subtract blank values
  for i:= 1 to NumChannels do
   if not isBlank[i] then
    ChanRawDbl[i]:= ChanRawDbl[i] - ChanRawDbl[Subtracts[i]];
 // output
 for i:= 1 to NumChannels do
  OutLine:= OutLine + FormatFloat('0.0000', ChanRawDbl[i]) + #9;

 // output temperature if not already
 if (MainForm.LoadedDefFileM.Text = 'None') then
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
  if (MainForm.LoadedDefFileM.Text <> 'None')
   and (not MainForm.RawCurrentCB.Checked) then
   (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
    as TLineSeries).AddXY(timeCounter, ChanDbl[i])
  else
   (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
    as TLineSeries).AddXY(timeCounter, ChanRawDbl[i])
 end;
 for i:= 7 to 8 do
 begin
  if (MainForm.LoadedDefFileM.Text <> 'None')
   and (not MainForm.RawCurrentCB.Checked) then
   (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
    as TLineSeries).AddXY(timeCounter, ChanDbl[i])
  else
   (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
    as TLineSeries).AddXY(timeCounter, ChanRawDbl[i])
 end;
 MainForm.SIXTempValues.AddXY(timeCounter, temperature);

 // adjust the chart extent so that all new data fit in
 Extent:= AdjustExtent;

 // scroll axis if desired by the user
 ScrollInterval:= MainForm.ScrollIntervalSE.Value/60;
 // don't scroll when user zoomed in and when in calibration mode
 if (MainForm.ScrollViewCB.Checked = true)
    and (not wasZoomDragged) and (not inCalibration)
    and (timeCounter > ScrollInterval) then
 begin
  // We could scroll by setting the LogicalExtent to the x-range of the
  // ScrollInterval. But when the user zooms out, he will see the full
  // chart extent unless the next scrolling takes place.
  // To avoid this, we we need to restrict the extent to the
  // ScrollInterval and to the max range of the Y values of all series.
  // Then zooming out can then only jump back to this extent.

  // get the desired ScrollInterval x-range
  Extent.a.x:= Extent.b.x - ScrollInterval;
 end;

 if (not  wasZoomDragged) and (not inCalibration) then
  // fix the extent to the determined x and y-range
  MainForm.SIXCH.Extent.FixTo(Extent);

 // if we are in the first run there are no previous values
 // and no slopes can be calculated
 if signalCounter = 1 then
  exit;
 // get last channel values out of diagramm series
 for i:= 1 to NumChannels do
  prevChan[i]:= (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
   as TLineSeries).GetYValue(signalCounter - 2);
 for i:= 7 to 8 do
  prevChan[i]:= (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
   as TLineSeries).GetYValue(signalCounter - 2);

 // calculate slopes
 for i:= 1 to NumChannels do
 begin
  if (MainForm.FindComponent('Channel' + IntToStr(i) + 'OnOffCB')
   as TCheckBox).Checked then
  begin
   if MainForm.RawCurrentCB.Checked then
    slope:= (ChanRawDbl[i] - prevChan[i])
            / (lastInterval * 60) * 1000 // in pA/s
   else
    slope:= (ChanDbl[i] - prevChan[i])
            / (lastInterval * 60) * 1000; // in uM/s
   (MainForm.FindComponent('Slope' + IntToStr(i) + 'LE')
   as TLabeledEdit).Text:= FloatToStr(RoundTo(slope, -2));
   (MainForm.FindComponent('PrevChannel' + IntToStr(i) + 'LE')
   as TLabeledEdit).Text:= FloatToStr(RoundTo(prevChan[i], -4));
   if MainForm.RawCurrentCB.Checked then
    (MainForm.FindComponent('CurrChannel' + IntToStr(i) + 'LE')
    as TLabeledEdit).Text:= FloatToStr(RoundTo(ChanRawDbl[i], -4))
   else
    (MainForm.FindComponent('CurrChannel' + IntToStr(i) + 'LE')
    as TLabeledEdit).Text:= FloatToStr(RoundTo(ChanDbl[i], -4));
  end;
 end;
 for i:= 7 to 8 do
 begin
  if (MainForm.FindComponent('Channel' + IntToStr(i) + 'OnOffCB')
   as TCheckBox).Checked then
  begin
   if MainForm.RawCurrentCB.Checked then
    slope:= (ChanRawDbl[i] - prevChan[i])
            / (lastInterval * 60) * 1000 // in pA/s
   else
    slope:= (ChanDbl[i] - prevChan[i])
            / (lastInterval * 60) * 1000; // in uM/s
   (MainForm.FindComponent('Slope' + IntToStr(i) + 'LE')
   as TLabeledEdit).Text:= FloatToStr(RoundTo(slope, -2));
   (MainForm.FindComponent('PrevChannel' + IntToStr(i) + 'LE')
   as TLabeledEdit).Text:= FloatToStr(RoundTo(prevChan[i], -4));
   if MainForm.RawCurrentCB.Checked then
    (MainForm.FindComponent('CurrChannel' + IntToStr(i) + 'LE')
    as TLabeledEdit).Text:= FloatToStr(RoundTo(ChanRawDbl[i], -4))
   else
    (MainForm.FindComponent('CurrChannel' + IntToStr(i) + 'LE')
    as TLabeledEdit).Text:= FloatToStr(RoundTo(ChanDbl[i], -4));
  end;
 end;

 // output analog voltages
 if MainForm.UseAnOutCB.checked then
 begin
 // first calculate the values
 for i:= 1 to 4 do // we limit to output channels (4 times pump driver)
 begin
  if (MainForm.FindComponent('AnOutConnector' + IntToStr(i) + 'OnOffCB')
   as TCheckBox).Checked then
  begin
   if MainForm.RawCurrentCB.Checked then // use raw signals
   begin
    // if just a channel
    // we calculate the values in nA
    ItemIndex:= (MainForm.FindComponent('AnOutputOf' + IntToStr(i) + 'CB')
     as TComboBox).ItemIndex;
    if ItemIndex < 6 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(ChanRawDbl[ItemIndex + 1]
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3)
    else
     // if mean
    begin
     if ItemIndex = 6 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanRawDbl[1] + ChanRawDbl[2]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 7 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanRawDbl[1] + ChanRawDbl[3]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 8 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanRawDbl[1] + ChanRawDbl[4]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 9 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanRawDbl[1] + ChanRawDbl[5]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 10 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanRawDbl[1] + ChanRawDbl[6]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 11 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanRawDbl[2] + ChanRawDbl[3]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 12 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanRawDbl[2] + ChanRawDbl[4]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 13 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanRawDbl[2] + ChanRawDbl[5]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 14 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanRawDbl[2] + ChanRawDbl[6]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 15 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanRawDbl[3] + ChanRawDbl[4]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 16 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanRawDbl[3] + ChanRawDbl[5]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 17 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanRawDbl[3] + ChanRawDbl[6]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 18 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanRawDbl[4] + ChanRawDbl[5]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 19 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanRawDbl[4] + ChanRawDbl[6]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 20 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanRawDbl[5] + ChanRawDbl[6]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
    end;
   end
   else // values in mM according to .def file
   begin
    // if just a channel
    ItemIndex:= (MainForm.FindComponent('AnOutputOf' + IntToStr(i) + 'CB')
     as TComboBox).ItemIndex;
    if ItemIndex < 6 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF(ChanDbl[ItemIndex + 1]
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3)
    else
     // if mean
    begin
     if ItemIndex = 6 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanDbl[1] + ChanDbl[2]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 7 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanDbl[1] + ChanDbl[3]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 8 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanDbl[1] + ChanDbl[4]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 9 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanDbl[1] + ChanDbl[5]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 10 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanDbl[1] + ChanDbl[6]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 11 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanDbl[2] + ChanDbl[3]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 12 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanDbl[2] + ChanDbl[4]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 13 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanDbl[2] + ChanDbl[5]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 14 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanDbl[2] + ChanDbl[6]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 15 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanDbl[3] + ChanDbl[4]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 16 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanDbl[3] + ChanDbl[5]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 17 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanDbl[3] + ChanDbl[6]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 18 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanDbl[4] + ChanDbl[5]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 19 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanDbl[4] + ChanDbl[6]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
     if ItemIndex = 20 then
     (MainForm.FindComponent('AnOutOf' + IntToStr(i) + 'LE')
     as TLabeledEdit).Text:= FloatToStrF((ChanDbl[5] + ChanDbl[6]) / 2
      / MainForm.AnOutMaxSignalFSE.Value * 3.3, ffFixed, 3, 3);
    end;
   end;
  end;
 end; // end for i:= 1 to 4
 end; // end if MainForm.UseAnOutCB.checked

end;

function TSIXControl.AdjustExtent : TDoubleRect;
var
  i: Integer;
  dummyValue : double;
  Extent: TDoubleRect;
begin
  // since all series have the same time we can take a series of our choice
  Extent:= MainForm.SIXCh1Values.Extent;
  // determine the y-range
  for i:= 1 to 8 do
  begin
   dummyValue:= (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
                 as TLineSeries).MaxYValue;
   if dummyValue > Extent.b.y then
    Extent.b.y:= dummyValue;
   dummyValue:= (MainForm.FindComponent('SIXCh' + IntToStr(i) + 'Values')
                 as TLineSeries).MinYValue;
   if dummyValue < Extent.a.y then
    Extent.a.y:= dummyValue;
  end;
  // store the y-range since this is necessary when turning off scrolling
  MinExtentY:= Extent.a.y;
  MaxExtentY:= Extent.b.y;

  Result:= Extent;
end;

procedure TSIXControl.SCChartToolsetDataPointHintToolHintPosition(
 ATool: TDataPointHintTool; var APoint: TPoint);
var
 series : TLineSeries;
 x, y, HintWidth, HintHeight : Integer;
 rect : TRect;
 HintWindow : THintWindow;
 HintText : string = '';
// moves the hint text above the cursor and center it horizontally to cursor
begin
 series:= ATool.Series as TLineSeries;
 // get image coordinates of the point
 x:= MainForm.SIXCH.XGraphToImage(series.ListSource[ATool.PointIndex]^.X);
 y:= MainForm.SIXCH.YGraphToImage(series.ListSource[ATool.PointIndex]^.Y);

 // Get hint text - just call the event handler of OnHint
 MainForm.ChartToolsetDataPointHintToolHint(ATool, APoint, HintText);

 HintWindow:= THintWindow.Create(nil);
 try
  rect:= HintWindow.CalcHintRect(MainForm.SIXCH.Width, HintText, nil);
  HintWidth:= rect.Right - rect.Left; // hint window width
  HintHeight:= rect.Bottom - rect.Top; // hint window height
 finally
  HintWindow.Free;
 end;

 // Center hint horizontally relative to data point
 APoint.x:= x - HintWidth div 2;
 // Move hint 10 pixels above the "High" data point
 APoint.y:= y - HintHeight - 10;
 // Hint coordinates are relative to screen
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
 //dyp, dyn: Double;

begin
 Result:= false;
 // propose a file name
 InNameCSV:= 'SIXMeasurements';
 if Overwrite = true then
 begin
  // proposal according to currently active tab
  if MainForm.MainPC.ActivePage = MainForm.SIXValuesTS then
   InNameCSV:= InNameCSV + '-Live';
  if MainForm.MainPC.ActivePage = MainForm.ResultTS then
   InNameCSV:= InNameCSV + '-Result';
 end;
 CSVOutName:= MainForm.SaveHandling(InNameCSV, '.csv'); // opens file dialog

 if (CSVOutName <> '') and (FileExists(CSVOutName) = true) then
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
 Extent : TDoubleRect;
begin
 if MainForm.ScrollViewCB.Checked = false then
 begin
  MainForm.ScrollIntervalSE.Enabled:= false;
  // zoom back to normal
  // if no scrolling took place, we can just zoom out fully
  if MinExtentY = Infinity then
   MainForm.SIXCH.ZoomFull
  // if scrolled, the chart extent is limited to the scrolled chart content
  // to get back to the full range:
  // - get the extent of one series, this gixes us the X since all series have
  //   the same X value range
  // - Y is the max/min of all series contents that we already stored after
  //   every scrollong action
  else
  begin
   Extent:= MainForm.SIXCh1Values.Extent;
   Extent.a.y:= MinExtentY; // determined in procedure SCReadTimerTimerFinished
   Extent.b.y:= MaxExtentY;
   // override the extent from the scrolling action with the new extent
   MainForm.SIXCH.Extent.FixTo(Extent);
  end;
 end
 else
 begin
  MainForm.ScrollIntervalSE.Enabled:= true;
  // also in case it is zoomed, enable scrolling
  wasZoomDragged:= false;
 end;
end;

procedure TSIXControl.SCSaveCSVResultBClick(Sender: TObject);
begin
 CSVOutName:= '';
 SaveCSV(true, MainForm.ResultCHValues.Name);
 //SaveCSV(true, ResultCHAverages.Name);
end;

function TSIXControl.SaveScreenshot(Overwrite: Boolean; ChartName: string) : Boolean;
var
 OutNameHelp : string;

begin
 Result:= false;
 // propose a file name
 OutNameHelp:= 'Screenshot';
 if Overwrite = true then
 begin
  // proposal according to currently active tab
  if MainForm.MainPC.ActivePage = MainForm.SIXValuesTS then
   OutNameHelp:= OutNameHelp + '-Live';
  if MainForm.MainPC.ActivePage = MainForm.ResultTS then
   OutNameHelp:= OutNameHelp + '-Fit';
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

procedure TSIXControl.SCChartDblClick(Sender: TObject);
var
 SenderName : string;
begin
 SenderName:= (Sender as TComponent).Name;
 MainForm.ColorDialog.Title:= 'Select Chart Background Color';
 if SenderName = 'SIXCH' then
 begin
  // start with current color
  MainForm.ColorDialog.Color:= MainForm.SIXCH.BackColor;
  if MainForm.ColorDialog.Execute then
   MainForm.SIXCH.BackColor:= MainForm.ColorDialog.Color;
 end
 else if SenderName = 'ResultCH' then
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
begin
 SenderName:= (Sender as TComponent).Name;
 // SenderName is in the form 'ChannelxCB' and we need the x
 // so get the 8th character of the name
 Channel:= Copy(SenderName, 8, 1);

 // change the Text according to current item
 (MainForm.FindComponent(SenderName) as TComboBox).Text:=
  (MainForm.FindComponent(SenderName) as TComboBox).Items[
      (MainForm.FindComponent(SenderName) as TComboBox).ItemIndex];

 // transfer content to testing tab
 (MainForm.FindComponent('Channel' + Channel + 'TestCB') as TComboBox).Text:=
  (MainForm.FindComponent(SenderName) as TComboBox).Text;
 (MainForm.FindComponent('Channel' + Channel + 'TestCB') as TComboBox).ItemIndex:=
  (MainForm.FindComponent(SenderName) as TComboBox).ItemIndex;

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
    as TLabeledEdit).EditLabel.Caption:= 'Current Signal [nA]';
 if (MainForm.LoadedDefFileM.Text <> 'None')
  and ((MainForm.FindComponent(SenderName) as TComboBox).ItemIndex < 3) then
  (MainForm.FindComponent('CurrChannel' + Channel + 'LE')
    as TLabeledEdit).EditLabel.Caption:= 'Current Signal [mM]';
end;

procedure TSIXControl.SCChannelXGBDblClick(Sender: TObject);
var
 SenderName, Channel : string;
 i : integer;
begin
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
 if StrToInt(Channel) < 7 then
 begin
  (MainForm.FindComponent('SIXCh' + Channel + 'Values') as TLineSeries).Title:=
   (MainForm.FindComponent(SenderName) as TGroupBox).Caption;
  if Started then
   (MainForm.FindComponent('SIXCh' + Channel + 'Results') as TLineSeries).Title:=
    'Stable ' + (MainForm.FindComponent(SenderName) as TGroupBox).Caption;
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
 i : integer;
begin
 if MainForm.RawCurrentCB.Checked then
 begin
  // rename the chart axis
  MainForm.SIXCH.AxisList[0].Title.Caption:= 'Sensor Value [nA]';
  MainForm.ResultCH.AxisList[0].Title.Caption:= 'Sensor Value [nA]';
  for i:= 1 to 8 do
  begin
   if (i < 7) and isBlank[i] then // don't do this for blank channels
    continue;
   (MainForm.FindComponent('PrevChannel' + IntToStr(i) + 'LE')
    as TLabeledEdit).EditLabel.Caption:= 'Previous Signal [nA]';
   (MainForm.FindComponent('CurrChannel' + IntToStr(i) + 'LE')
    as TLabeledEdit).EditLabel.Caption:= 'Current Signal [nA]';
   (MainForm.FindComponent('Slope' + IntToStr(i) + 'LE')
    as TLabeledEdit).EditLabel.Caption:= 'Signal Slope [pA/s]';
   (MainForm.FindComponent('LabelSlope' + IntToStr(i))
    as TLabel).Caption:= 'Limit for Slope [pA/s]';
  end;
  // in this case no definition file is needed and the SIX connection
  // can be enabled
  MainForm.SIXBiosensorsMI.Enabled:= true;
  MainForm.IndicatorSensorP.Color:= cldefault;
  // thus also clear the warning
  MainForm.IndicatorSensorP.Caption:= '';
  // enable analog output
  MainForm.UseAnOutCB.Enabled:= true;
  // change 3.3V output label
  MainForm.AnOutMaxLabel.Caption:= 'nA will become 3.3 V output';
 end
 else // not checked
 begin
  MainForm.SIXCH.AxisList[0].Title.Caption:= 'Sensor Value [mmol/l]';
  MainForm.ResultCH.AxisList[0].Title.Caption:= 'Sensor Value [mmol/l]';
  for i:= 1 to 8 do
  begin
   if (i < 7) and isBlank[i] then // don't do this for blank channels
    continue;
   (MainForm.FindComponent('PrevChannel' + IntToStr(i) + 'LE')
    as TLabeledEdit).EditLabel.Caption:= 'Previous Signal [mM]';
   (MainForm.FindComponent('CurrChannel' + IntToStr(i) + 'LE')
    as TLabeledEdit).EditLabel.Caption:= 'Current Signal [mM]';
   (MainForm.FindComponent('Slope' + IntToStr(i) + 'LE')
    as TLabeledEdit).EditLabel.Caption:= 'Signal Slope [uM/s]';
   (MainForm.FindComponent('LabelSlope' + IntToStr(i))
    as TLabel).Caption:= 'Limit for Slope [uM/s]';
  end;
  // change 3.3V output label
  MainForm.AnOutMaxLabel.Caption:= 'mM will become 3.3 V output';
  // if there is no definition file loaded issue a warning
  if MainForm.LoadedDefFileM.Text = 'None' then
  begin
   MainForm.IndicatorSensorP.Color:= clRed;
   MainForm.IndicatorSensorP.Caption:= 'No definition file loaded';
   MainForm.SIXBiosensorsMI.Enabled:= false;
   // disable then also analog output
   MainForm.UseAnOutCB.Enabled:= false;
   MainForm.UseAnOutCB.Checked:= false;
  end;
 end;

end;

procedure TSIXControl.SCChartToolsetTitleFootClickTool1Click(Sender: TChartTool;
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

procedure TSIXControl.SCChartToolsetLegendClickTool1Click(Sender: TChartTool;
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

procedure TSIXControl.SCChartToolsetAxisClickTool1Click(Sender: TChartTool;
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
  editor.ShowModal;
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
 // 1 -> Series 0, 2 -> Series 2, 4 -> Series 4, 5 -> Series 6
 // 6 -> Series 8, 7 -> Series 10
 if Channel < 4 then
  Channel:= 2 * Channel - 2
 else
  Channel:= 2 * Channel - 4;
 // change to the chart tab to see the changes immediately
 MainForm.MainPC.ActivePage:= MainForm.SIXValuesTS;
 // get name of GroupBox or ComboBox
 if Channel < 8 then
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
 MainForm.MainPC.ActivePage:= MainForm.GeneralTS;
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
 if Started then
  (MainForm.FindComponent('SIXCh' + Channel + 'Results') as TLineSeries).Active:=
   (MainForm.FindComponent(SenderName) as TCheckBox).Checked;
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
 i, gainFactor : integer;
 StringArray : TStringArray;
 ppp : PChar;
 Component : TComponent = nil;
begin
 // initialize
 result:= false;
 // enable maybe previously disabled GroupBoxes
 for i:= 1 to 8 do
  (MainForm.FindComponent('Channel' + IntToStr(i) + 'GB')
   as TGroupBox).Enabled:= true;

 // check the SIX type
 if MainForm.SIXTypeRG.ItemIndex = 1 then
  gainFactor:= 1
 else
  gainFactor:= 2;

 // open file stream
 try
  OpenFileStream:= TFileStream.Create(InFile, fmOpenRead);
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
   Gains[i+1]:= Gains[i+1] / gainFactor;
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
   for i:= 7 to 8 do
   begin
    // no operations are possible thus disable these channels
    (MainForm.FindComponent('Channel' + IntToStr(i) + 'GB')
     as TGroupBox).Enabled:= false;
    (MainForm.FindComponent('Channel' + IntToStr(i) + 'OnOffCB')
     as TCheckBox).Checked:= false;
    (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
     as TComboBox).Items.Clear;
   end
  else
   for i:= 7 to 8 do
   begin
    (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
     as TComboBox).Items.Clear;
    if NumChannels >= 5 then
     (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
      as TComboBox).Items.Add('mean(#2, #5)');
    if NumChannels = 6 then
     (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
      as TComboBox).Items.Add('mean(#3, #6)');
    if NumChannels >= 4 then
     (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
      as TComboBox).Items.Add('mean(#1, #4)');
   end;
  // set new item index
  for i:= 7 to 8 do
   (MainForm.FindComponent('Channel' + IntToStr(i) + 'CB')
    as TComboBox).ItemIndex:= i - 7;

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
   if not TryStrToFloat(StringArray[i], TemperGains[i+1]) then
   begin
    Result:= false;
    exit;
   end;

 finally
  LineReader.Free;
  OpenFileStream.Free;
 end;

 result:= true;
end;

procedure TSIXControl.SCCalibrateTBChange(Sender: TObject);
{the calibration is done the following way:
 - after clicking the calibrate button, the user can select data by clicking
   and dragging in the chart. This way a selection rectangle is created.
 - after the selection a dialog pops up listing all series in the chart
 - the user selects there what series to use and what conversion unit should be used
 - the mean of the selected series' datapoints within the rectangle is eventually
   used as calibration value}
var
 extent : TDoubleRect;
 height, width, calibFactorA, calibFactorB : double;
 center : TDoublePoint;
 OutName, DummyString, HeaderLine : string;
 StringList : TStringList;
 StringArray : TStringArray;
 i : integer;
begin
 // show/hide the lines and en/disable the rectangle tool
 MainForm.TopLine.Active:= MainForm.CalibrateTB.Checked;
 MainForm.BottomLine.Active:= MainForm.CalibrateTB.Checked;
 MainForm.LeftLine.Active:= MainForm.CalibrateTB.Checked;
 MainForm.RightLine.Active:= MainForm.CalibrateTB.Checked;
 // enable rectangle tool
 MainForm.RectangleSelectionTool.Enabled:= MainForm.CalibrateTB.Checked;
 // disable unused tools
 MainForm.ChartToolsetZoomDragTool.Enabled:= (not MainForm.CalibrateTB.Checked);
 MainForm.ChartToolsetDataPointHintTool.Enabled:= (not MainForm.CalibrateTB.Checked);
 MainForm.ChartToolsetDataPointCrosshairTool.Enabled:= (not MainForm.CalibrateTB.Checked);
 MainForm.ChartToolsetPanDragTool.Enabled:= (not MainForm.CalibrateTB.Checked);
 MainForm.ChartToolsetPanMouseWheelTool.Enabled:= (not MainForm.CalibrateTB.Checked);
 // prevent scrolling while in calibration selection mode
 inCalibration:= MainForm.CalibrateTB.Checked;

 if MainForm.CalibrateTB.Checked then
 begin
  extent:= MainForm.SIXCH.LogicalExtent;
  width:= extent.b.x - extent.a.x; // horizontal range of data
  height:= extent.b.y - extent.a.y; // vertical range of data
  center:= DoublePoint((extent.a.x + extent.b.x)/2, (extent.a.y + extent.b.y)/2);
  // make a preset for the selection line positions
  if MainForm.TopLine.Position = Infinity then
   MainForm.TopLine.Position:= center.y + height/4;
  if MainForm.BottomLine.Position = -Infinity then
   MainForm.BottomLine.Position:= center.y - height/4;
  if MainForm.LeftLine.Position = -Infinity then
   MainForm.LeftLine.Position:= center.x - width/4;
  if MainForm.RightLine.Position = Infinity then
   MainForm.RightLine.Position:= center.x + width/4;

  // activate the rectangle selection
  MainForm.LineDragTool.Shift:= [ssLeft];
  MainForm.RectangleSelectionTool.Shift:= [ssLeft];
 end
 else
 begin
  // deactivate the rectangle selection
  MainForm.LineDragTool.Shift:= [];
  MainForm.RectangleSelectionTool.Shift:= [];

  // show the calibration dialog
  CalibrationF.ShowModal;

  // if user pressed OK and there is a valid mean value, write a new .def file
  if CalibrationF.ModalResult = mrOK then
  begin
   if calibChannel = 0 then // something went wrong
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
   MainForm.SaveDialog.FileName:= MainForm.LoadedDefFileM.Text + ' - '
                                  + FormatDateTime('dd-mm-yyyy-hh-nn', now);

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
     if calibChannel < 7 then // we can change the single channel
     begin
      // the new gain is the current one times the factor
      if MainForm.RawCurrentCB.Checked then
       // then we must take the temperature correction into account
       calibFactor:= calibFactor * GainsRaw[calibChannel]
                     * exp(TemperGains[calibChannel] / 100
                     * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]))
      else
       // the existing gain includes the temperature correction
       calibFactor:= calibFactor * Gains[calibChannel];
      StringArray[calibChannel-1]:= FloatToStr(RoundTo(calibFactor, -4));
     end
     else // for channel operations
     begin
      if (MainForm.FindComponent('Channel' + IntToStr(calibChannel) + 'CB')
        as TComboBox).Text = 'mean(#2, #5)' then
      begin
       if MainForm.RawCurrentCB.Checked then
       begin
        // then we must take the temperature correction into account
        calibFactorA:= calibFactor * GainsRaw[2]
                      * exp(TemperGains[2] / 100
                      * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]));
        calibFactorB:= calibFactor * GainsRaw[5]
                      * exp(TemperGains[5] / 100
                      * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]));
       end
       else
       begin
        // the existing gain includes the temperature correction
        calibFactorA:= calibFactor * Gains[2];
        calibFactorB:= calibFactor * Gains[5];
       end;
       StringArray[2-1]:= FloatToStr(RoundTo(calibFactorA, -4));
       StringArray[5-1]:= FloatToStr(RoundTo(calibFactorB, -4));
      end
      else if (MainForm.FindComponent('Channel' + IntToStr(calibChannel) + 'CB')
        as TComboBox).Text = 'mean(#3, #6)' then
      begin
       if MainForm.RawCurrentCB.Checked then
       begin
        // then we must take the temperature correction into account
        calibFactorA:= calibFactor * GainsRaw[3]
                      * exp(TemperGains[3] / 100
                      * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]));
        calibFactorB:= calibFactor * GainsRaw[6]
                      * exp(TemperGains[6] / 100
                      * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]));
       end
       else
       begin
        // the existing gain includes the temperature correction
        calibFactorA:= calibFactor * Gains[3];
        calibFactorB:= calibFactor * Gains[6];
       end;
       StringArray[3-1]:= FloatToStr(RoundTo(calibFactorA, -4));
       StringArray[6-1]:= FloatToStr(RoundTo(calibFactorB, -4));
      end
      else if (MainForm.FindComponent('Channel' + IntToStr(calibChannel) + 'CB')
        as TComboBox).Text = 'mean(#1, #4)' then
      begin
       if MainForm.RawCurrentCB.Checked then
       begin
        // then we must take the temperature correction into account
        calibFactorA:= calibFactor * GainsRaw[1]
                      * exp(TemperGains[1] / 100
                      * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]));
        calibFactorB:= calibFactor * GainsRaw[4]
                      * exp(TemperGains[4] / 100
                      * (StrToFloat(MainForm.SIXTempLE.Text) - TemperGains[8]));
       end
       else
       begin
        // the existing gain includes the temperature correction
        calibFactorA:= calibFactor * Gains[1];
        calibFactorB:= calibFactor * Gains[4];
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
    // write a new header line to the output file
    if HaveSensorFileStream and (not MainForm.RawCurrentCB.Checked) then
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
    end
    else if HaveSensorFileStream and MainForm.RawCurrentCB.Checked then
    begin
     HeaderLine:= 'Calibration was performed' + LineEnding;
     HeaderLine:= HeaderLine + 'Counter' + #9 + 'Time [min]' + #9;
     for i:= 1 to NumChannels do
      HeaderLine:= HeaderLine + 'Ch' + IntToStr(i) + ' [nA]' + #9;
     HeaderLine:= HeaderLine + 'Temp [deg C]' + LineEnding;
     SensorFileStream.Write(HeaderLine[1], Length(HeaderLine));
    end;
   end; //end if OutName <> ''

  end; // end mrOK

  // move lines back to infinity
  MainForm.TopLine.Position:= Infinity;
  MainForm.BottomLine.Position:= -Infinity;
  MainForm.LeftLine.Position:= -Infinity;
  MainForm.RightLine.Position:= Infinity;
  //reset calibChannel
  calibChannel:= 0;
  // The user might have zoomed in, then calibrated and is wondering why nothing
  // happens afterwards. Therefore jump out of the wasZoomDragged mode.
  wasZoomDragged:= false;

 end;

end;

end. //unit

