unit PumpControlUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus, Math,
  StdCtrls, ExtCtrls, Spin, Buttons, LCLType, SynaSer, Crt, Character,
  System.UITypes, Types, TAChartListbox, TASeries,
  // custom forms
  JTDriverSensingMain, NameSetting;

type

  TPumpControl = class
    procedure PCDutyCycleXFSEChange(Sender: TObject);
    procedure PCStepTimerXFinished(Sender: TObject);
    procedure PCStepTimerLastFinished(Sender: TObject);
    procedure PCGenerateCommandBBClick(Sender: TObject);
    procedure PCRunBBClick(Sender: TObject);
    procedure PCStopBBClick(Sender: TObject);
    procedure PCLiveModeCBChange(Sender: TObject);
    procedure PCPumpOnOffCBLoopChange(Sender: TObject);
    procedure PCSendRepeatToPump;
    procedure PCRepeatTimerFinished;
    procedure PCOverallTimerStartTimer(Sender: TObject);
    procedure PCOverallTimerFinished;
    procedure PCStepXUseCBChange(Sender: TObject);
    procedure PCPumpVoltageFSChange(Sender: TObject);
    procedure PCRepeatPCChange(Sender: TObject);
    procedure PCPumpGBDblClick(Sender: TObject);
    procedure PCValveRGDblClick(Sender: TObject);
    procedure PCHasNoPumpsCBChange(Sender: TObject);
    procedure PCHasNoValvesCBChange(Sender: TObject);
    procedure PCUseCalibCBChange(Sender: TObject);
    procedure PCPumpNumberSEChange(Sender: TObject);
    procedure PCValveNumberSEChange(Sender: TObject);
    procedure PCHavePumpSerialCBChange(Sender: TObject);
    procedure AnOutPumpXGBDblClick(Sender: TObject);
    procedure PCRunEndlessCBChange(Sender: TObject);
    procedure PCCalibValueFSEChange(Sender: TObject);
    procedure PCRepeatSEChange(Sender: TObject);

  private

  public
    function GenerateCommand(out command: string;
      withSensor : Boolean = false) : Boolean;
    function ParseCommand(command: string) : Boolean;
    procedure RunImmediate;

    class var
     CurrentRepeat : integer;
     GlobalTime : Double;
     GlobalRepeatTime : Double;
     RepeatTime : Double;
     StepNum : integer; // number of steps
     PumpNum : integer; // number of pumps
     PumpNumFile : integer; // number of pumps defined in a loaded action file
     ValveNum : integer; // number of valves
     PumpPrefix : string; // line prefix for action files
     ValvePrefix : string; // line prefix for action files
     commandForRepeat : string; // stores the command sent on every repeat
     oneDay : longint; // stores the time of one day in ms
  end;

var
  PumpControl: TPumpControl;

implementation

uses
  SIXControlUnit;

procedure TPumpControl.PCGenerateCommandBBClick(Sender: TObject);
// call function to collect data and generate command
var
 command, Substance : string;
 i, j : integer;
begin
 GenerateCommand(command);
 MainForm.CommandM.Text:= command;
 // the button re-enables editing after an action file was loaded
 // enable all setting possibilities
 MainForm.LiveModeCB.Enabled:= True;
 MainForm.RunSettingsGB.Enabled:= not MainForm.LiveModeCB.Checked;
 MainForm.PumpSetupGB.Enabled:= True;
 MainForm.UseCalibGB.Enabled:= True;
 MainForm.ValveSetupGB.Enabled:= True;
 MainForm.CalibrationGB.Enabled:= not MainForm.LiveModeCB.Checked;
 if not MainForm.HaveDefFileCB.Checked then
  MainForm.CalibrationGB.Enabled:= false;

 // check all possible steps
 for j:= 1 to StepNum do
 begin
  (MainForm.FindComponent('Step' + IntToStr(j) + 'UseCB')
   as TCheckBox).Enabled:= True;
  // don't enable action time when run endless and only one step
  if not (MainForm.RunEndlessCB.Checked) or MainForm.Step2UseCB.Checked then
   (MainForm.FindComponent('ActionTime' + IntToStr(j) + 'GB')
    as TGroupBox).Enabled:= True;
  (MainForm.FindComponent('DutyCycle' + IntToStr(j) + 'GB')
   as TGroupBox).Enabled:= True;
  (MainForm.FindComponent('S' + IntToStr(j) + 'P14')
   as TTabSheet).Enabled:= True;
  (MainForm.FindComponent('S' + IntToStr(j) + 'P58')
   as TTabSheet).Enabled:= True;
  (MainForm.FindComponent('S' + IntToStr(j) + 'Valves')
   as TTabSheet).Enabled:= True;
  if j = 1 then
   // enable tooltips for pump name
   for i:= 1 to PumpNum do
    (MainForm.FindComponent('Pump' + IntToStr(i) + 'GB' + IntToStr(j))
     as TGroupBox).ShowHint:= True;
 end;
 // view tab after last used step
 for j:= 2 to StepNum-1 do
 begin
  if (MainForm.FindComponent('Step' + IntToStr(j) + 'UseCB')
      as TCheckBox).Checked then
   (MainForm.FindComponent('Step' + IntToStr(j+1) + 'TS')
    as TTabSheet).TabVisible:= True
  else
   break;
 end;
 // tab 2 must always be visible except when in live mode
 if not MainForm.LiveModeCB.Checked then
  MainForm.Step2TS.TabVisible:= True;
 // loaded settings are no longer valid
 MainForm.LoadedActionFileM.Text:= 'None';
 MainForm.LoadedActionFileM.Color:= clDefault;
 MainForm.LoadedActionFileM.Hint:= 'No action file loaded';
 // enable the calibration settings
 MainForm.UseCalibCB.Enabled:= True;
 if MainForm.UseCalibCB.Checked then
 begin
  MainForm.CalibStepCB.Enabled:= True;
  MainForm.CalibStepL.Enabled:= True;
  MainForm.CalibEveryXStepsSE.Enabled:= True;
  MainForm.CalibEveryXStepsL1.Enabled:= True;
  MainForm.CalibEveryXStepsL2.Enabled:= True;
  MainForm.UsedCalibValueSE.Enabled:= True;
  MainForm.UsedCalibValueL.Enabled:= True;
 end;
 for j:= 1 to MainForm.CalibSubstancesPC.PageCount do
 begin
  // the user must be able to see the settings for all substances
  // therefore we cannot just disable the CalibSubstancesPC component but its
  // child components except of XTS
  Substance:= MainForm.CalibSubstancesPC.Pages[j-1].Caption;
  (MainForm.FindComponent(Substance + 'AvailChanL')
   as TLabel).Enabled:= True;
  (MainForm.FindComponent(Substance + 'CalibGB')
   as TGroupBox).Enabled:= True;
  (MainForm.FindComponent(Substance + 'CalibCLB')
    as TChartListbox).Enabled:= True;
 end;
 // enable saving
 MainForm.SaveActionMI.Enabled:= True;
end;

function TPumpControl.ParseCommand(command: string): Boolean;
// parses the input command
var
 address : string;
 SOrder : array of char;
 LastParsed : char = 'X';
 StepCounter, MCounter, ICounter, i, j, k, G1, p,
   posSfirst, posSlast : integer;
 MousePointer : TPoint;
 StepTime, M1, M2, DutyStepTime : Double;
 Have2M : Boolean;
begin
 MousePointer:= Mouse.CursorPos; // store mouse position
 StepCounter:= 0; MCounter:= 0; ICounter:= 0;
 M1:= 0; M2:= 0; G1:= 0;
 result:= false; Have2M:= false; StepTime:= 0;
 SOrder:= nil;
 setLength(SOrder, PumpNumFile);
 for k:= 0 to PumpNumFile-1 do
  SOrder[k]:= '0';

 // first check address
 address:= Copy(command, 0, 2);
 if address <> '/0' then
 begin
  MessageDlgPos('Error: Loaded file does not begin with ''/0''.',
   mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
  MainForm.LoadedActionFileM.Text:= 'None';
  MainForm.LoadedActionFileM.Color:= clDefault;
  MainForm.LoadedActionFileM.Hint:= 'No action file loaded';
  MainForm.CommandM.Text:= '';
  exit;
 end;

 // disable all steps, will be re-enabled while parsing
 for j:= 2 to StepNum do
  (MainForm.FindComponent('Step' + IntToStr(j) + 'UseCB')
   as TCheckBox).Checked:= false;

 // set default values
 MainForm.RepeatSE.Value:= 0;
 MainForm.RunEndlessCB.Checked:= false;
 // set all duty cylces to 100%
 for j:= 1 to StepNum do
  (MainForm.FindComponent('DutyCycle' + IntToStr(j) + 'FSE')
   as TFloatSpinEdit).Value:= 100;

 // before we can parse the command, we need to clean it up
 // there might be 10 ms long steps in it used to start many motors at once
 // we ignore these because they will automatically be re-added at the
 // next pump run
 i:= 2;
 posSfirst:= 0;
 posSlast:= 0;
 // parse from 'S' to the next 'S'
 while i < length(command) do
 begin
  if command[i] = 'S' then
  begin
   if posSfirst = 0 then
    posSfirst:= i
   else
    posSlast:= i;
  end;
  // check if the M command is 'M10' and only then cut out from the command
  if (posSfirst > 0) and (posSlast > 0)
   and (Copy(command, posSlast - 3, 3) = 'M10') then
  begin
   Delete(command, posSfirst, posSlast - posSfirst);
   i:= posSfirst;
   posSfirst:= 0;
   posSlast:= 0;
  end
  else if (posSfirst > 0) and (posSlast > 0) then
  begin
   posSfirst:= 0;
   posSlast:= 0;
  end
  else
   inc(i);
 end;

 // parse the command
 for i:= 2 to Length(command) do
 begin

  // parse step 'S'
  if command[i] = 'S' then
  begin
   // syntax is "Sxyyy" with yyy/1000 * 3.3 = voltage
   inc(StepCounter);
   MCounter:= 0; // there can be several occurrences of 'M' for every step
   ICounter:= 0; // there can be several occurrences of 'I' for every step
   // initialize
   for k:= 0 to PumpNumFile-1 do
    SOrder[k]:= '0';
   LastParsed:= 'S';
   // determine the length
   j:= i;
   repeat
    inc(j)
   until IsDigit(command[j]) = false;
   // set the direction according to the SOrder command[i+k+1]
   k:= 1;
   while k < j-i do
   begin
    for p:= 1 to PumpNumFile do
     if command[i+k] = IntToStr(p) then
     begin
     (MainForm.FindComponent('Pump' + IntToStr(p) + 'VoltageFS' + IntToStr(StepCounter))
      as TFloatSpinEdit).Text:= FloatToStr((StrToInt(Copy(command, i+k+1, 3))) / 999 * 3.3);
      SOrder[trunc((k-1)/4)]:= IntToStr(p)[1];
     end;
    k:= k + 4; // after number are 3 digits
   end; // end while
  end; // end parse 'S'

  // parse step 'D'
  if command[i] = 'D' then
  begin
   // D is always connected to 'S', thus use the same StepCounter
   // syntax is Dxxxx, with x = [0,1] and there might only be one x
   // this is also possible: S39991999D11, then the first 1 belongs to pump 3
   LastParsed:= 'D';
   // determine the length
   j:= i;
   repeat
    inc(j)
   until IsDigit(command[j]) = false;
   // set the direction according to the SOrder
   for k:= 1 to j-i-1 do
   begin
    if command[i+k] = '1' then
    (MainForm.FindComponent('Pump' + SOrder[k-1] + 'DirectionRG' + IntToStr(StepCounter))
     as TRadioGroup).ItemIndex:= 1
    else
    (MainForm.FindComponent('Pump' + SOrder[k-1] + 'DirectionRG' + IntToStr(StepCounter))
     as TRadioGroup).ItemIndex:= 0;
   end;
  end; // end parse 'D'

  // parse step 'V'
  if command[i] = 'V' then
  begin
   // syntax is Vxxxx, with x = [0,1] and there might only be one x
   // if all valves are off, 'V' can be omitted and might not appear

   // when no pump is run, 'V' is the start of a step
   if (StepCounter = 0)
    or ((LastParsed = 'G') and
        (command[i+ValveNum+1+PumpNumFile+2] <> 'R')) then // not if last 'V'
   begin
    inc(StepCounter);
    ICounter:= 0;
    MCounter:= 0;
   end;
   // if there is no pump 'V' always starts a new step
   if LastParsed = 'M' then
   begin
    inc(StepCounter);
    ICounter:= 0;
    MCounter:= 0;
   end;

   // set the valves but not if it the very last 'V'
   if command[i+ValveNum+1+PumpNumFile+2] <> 'R' then
   begin
    if command[i+1] = '1' then
     (MainForm.FindComponent('Valve1RG' + IntToStr(StepCounter))
      as TRadioGroup).ItemIndex:= 1
    else
     (MainForm.FindComponent('Valve1RG' + IntToStr(StepCounter))
      as TRadioGroup).ItemIndex:= 0;
    if ValveNum > 1 then
    begin
     for p:= 2 to ValveNum do
     begin
      if (command[i+p] = '0') or (command[i+p] = '1') then
       if command[i+p] = '1' then
        (MainForm.FindComponent('Valve' + IntToStr(p) + 'RG' + IntToStr(StepCounter))
         as TRadioGroup).ItemIndex:= 1
       else
        (MainForm.FindComponent('Valve' + IntToStr(p) + 'RG' + IntToStr(StepCounter))
         as TRadioGroup).ItemIndex:= 0;
      end;
    end;
   end;
   LastParsed:= 'V';
  end; // end parse 'V'

  // parse step 'I'
  if command[i] = 'I' then
  begin
   // syntax is Ixxxx, with x = [0,1] and there might only be one x
   // 'I' can occur twice in one step, thus use StepCounter but only
   // parse the first occurence
   // there might also be no 'S' before 'I', then we must increase StepCounter
   // this is the case if:
   //  - the last parsed command is 'G'
   //  - the last parsed command is 'M' >= 1s and the next 'M' is >= 1 s
   //  - StepCounter is -1
   if (LastParsed = 'M') and (StepTime >= 1) then
   begin
    // check if there is a next 'M' with 1s
    if (command[i+PumpNumFile+1] = 'M') then
    begin
     // determine the length
     j:= i + PumpNumFile + 1;
     repeat
      inc(j)
     until IsDigit(command[j]) = false;
     StepTime:= StrToFloat(Copy(command, i+PumpNumFile+2, j-i-(PumpNumFile+2))) / 1000;
     if (StepTime >= 1) then
     begin
      inc(StepCounter);
      ICounter:= 0;
      MCounter:= 0;
     end;
    end;
   end;
   if (StepCounter = 0)
    or ((LastParsed = 'G') and (command[i+PumpNumFile+2] <> 'R')) then // not if last 'I'
   begin
    inc(StepCounter);
    ICounter:= 0;
    MCounter:= 0;
   end;
   inc(ICounter);
   LastParsed:= 'I';
   // only output if it is the first occurence in a step
   if ICounter = 1 then
   begin
    // enable the step
    (MainForm.FindComponent('Step' + IntToStr(StepCounter) + 'UseCB')
     as TCheckBox).Checked:= true;
    // set the pumps
    if command[i+1] = '1' then
    (MainForm.FindComponent('Pump1OnOffCB' + IntToStr(StepCounter))
      as TCheckBox).Checked:= true
    else
     (MainForm.FindComponent('Pump1OnOffCB' + IntToStr(StepCounter))
      as TCheckBox).Checked:= false;
    for p:= 2 to PumpNumFile do
    begin
     if (command[i+p] = '0') or (command[i+p] = '1') then
      if command[i+p] = '1' then
       (MainForm.FindComponent('Pump' + IntToStr(p) + 'OnOffCB' + IntToStr(StepCounter))
        as TCheckBox).Checked:= true
      else
       (MainForm.FindComponent('Pump' + IntToStr(p) + 'OnOffCB' + IntToStr(StepCounter))
        as TCheckBox).Checked:= false;
    end;
   end;
  end; // end parse 'I'

  // parse step 'M'
  if command[i] = 'M' then
  begin
   // M can occur twice in one step, thus use StepCounter
   // but only parse the first occurence
   // syntax is Mxxxx, with x = time in milliseconds
   inc(MCounter);
   LastParsed:= 'M';
   // determine the length
   j:= i;
   repeat
    inc(j)
   until IsDigit(command[j]) = false;
   StepTime:= StrToFloat(Copy(command, i+1, j-i-1)) / 1000;
   // only output if it is the first occurence in a step
   if MCounter = 1 then
   begin
    if (StepTime >= 1000) and (StepTime < 60000) then
    begin
     (MainForm.FindComponent('RunTime' + IntToStr(StepCounter) + 'FSE')
      as TFloatSpinEdit).Value:= StepTime / 60;
     (MainForm.FindComponent('Unit' + IntToStr(StepCounter) + 'RBmin')
      as TRadioButton).Checked:= true;
     end;
    if (StepTime > 60000) then
    begin
     (MainForm.FindComponent('RunTime' + IntToStr(StepCounter) + 'FSE')
      as TFloatSpinEdit).Value:= StepTime / 3600;
     (MainForm.FindComponent('Unit' + IntToStr(StepCounter) + 'RBh')
      as TRadioButton).Checked:= true;
    end;
    if (StepTime < 1000) then
    begin
     (MainForm.FindComponent('RunTime' + IntToStr(StepCounter) + 'FSE')
      as TFloatSpinEdit).Value:= StepTime;
     (MainForm.FindComponent('Unit' + IntToStr(StepCounter) + 'RBs')
      as TRadioButton).Checked:= true;
    end;
    // store the time for a possible duty cycle calculation
    M1:= StepTime;
   end
   else if MCounter = 2 then
   begin
    // store the second time for the duty cycle
    M2:= StepTime;
    Have2M:= true;
   end;
  end; // end parse 'M'

  // parse step 'G'
  if command[i] = 'G' then
  // the frontend only supports maximal one loop nesting level e.g. ggXGAgXGBGC
  begin
   LastParsed:= 'G';
   // if we have 2 'M' statements and M1 < 1, then 'G' is for the duty cycle
   // if there is no digit, it is the overall loop run forever
   if not isDigit(command[i+1]) then
    MainForm.RunEndlessCB.Checked:= true
   else
   begin
    // determine the length
    j:= i;
    repeat
     inc(j)
    until IsDigit(command[j]) = false;
    if Have2M and (M1 < 1) then
     G1:= StrToInt(Copy(command, i+1, j-i-1))
    else
     MainForm.RepeatSE.Text:= Copy(command, i+1, j-i-1);
    MainForm.RunEndlessCB.Checked:= false;
   end;
   if Have2M and (M1 < 1) then
   begin
    // calculate duty cycle
    (MainForm.FindComponent('DutyCycle' + IntToStr(StepCounter) + 'FSE')
     as TFloatSpinEdit).Value:= M1 / (M1 + M2) * 100;
    // calculate step time
    DutyStepTime:= (M1 + M2) * (G1 + 1);
    if (DutyStepTime >= 1000) and (DutyStepTime < 60000) then
    begin
     (MainForm.FindComponent('RunTime' + IntToStr(StepCounter) + 'FSE')
      as TFloatSpinEdit).Value:= DutyStepTime / 60;
     (MainForm.FindComponent('Unit' + IntToStr(StepCounter) + 'RBmin')
      as TRadioButton).Checked:= true;
     end;
    if (DutyStepTime > 60000) then
    begin
     (MainForm.FindComponent('RunTime' + IntToStr(StepCounter) + 'FSE')
      as TFloatSpinEdit).Value:= DutyStepTime / 3600;
     (MainForm.FindComponent('Unit' + IntToStr(StepCounter) + 'RBh')
      as TRadioButton).Checked:= true;
    end;
    if (DutyStepTime < 1000) then
    begin
     (MainForm.FindComponent('RunTime' + IntToStr(StepCounter) + 'FSE')
      as TFloatSpinEdit).Value:= DutyStepTime;
     (MainForm.FindComponent('Unit' + IntToStr(StepCounter) + 'RBs')
      as TRadioButton).Checked:= true;
    end;
    Have2M:= false;
   end;
  end; // end parse 'G'

 end; // end parse the command

 result:= true;

end;

function TPumpControl.GenerateCommand(out command: string;
  withSensor : Boolean): Boolean;
// collect data an generate command to be sent
var
 voltage, jStr, commandSplit, commandSave, commandOriginal : string;
 SOrder : array of char;
 timeFactor, DutyRepeats, XTime, OnTime, OffTime, i, j, k, j2, k2,
  voltageCalc, countPump, countPumpNumber, posS : integer;
 timeCalc, timeOut, timeStep : Double;
 HaveS : Boolean = False;
begin
 timeFactor:= 1; timeCalc:= 0; voltageCalc:= 0;
 command:= ''; commandSplit:= ''; voltage:= '';
 if not MainForm.LiveModeCB.Checked then
 begin
  MainForm.IndicatorPumpP.Color:= clDefault;
  MainForm.IndicatorPumpP.Caption:= '';
  MainForm.IndicatorPumpPPaint;
 end;
 MainForm.IndicatorPumpP.Hint:= '';
 SOrder:= nil;
 setLength(SOrder, PumpNum);
 posS:= 1; // 1 and not 0 because we use it to access chars in strings

 // address
 command:= '/0';
 // turn on LED
 command:= command + 'L';

 // when the SIX is active, we cannot run the Arduino timer parallel to the CPU
 // timer for a long time. Therefore we must send the pump command again after
 // every repeat. Therefore omit the outer loop.
 if not withSensor then
  if (StrToInt(MainForm.RepeatSE.Text) > 0) or (MainForm.RunEndlessCB.Checked) then
   // begin loop flag
   command:= command + 'g';

 // step through all tabs
 for j:= 1 to StepNum do
 begin
  // initialize
  for k:= 0 to PumpNum-1 do
   SOrder[k]:= '0';
  jStr:= IntToStr(j);
  voltageCalc:= 0;
  if (MainForm.FindComponent('Step' + jStr + 'UseCB') as TCheckBox).Checked
   and (MainForm.FindComponent('Step' + jStr + 'TS') as TTabSheet).TabVisible then
  begin
   // speed and direction flag
   if (MainForm.FindComponent('Pump1OnOffCB' + jStr)
        as TCheckBox).Checked
    or (MainForm.FindComponent('Pump2OnOffCB' + jStr)
        as TCheckBox).Checked
    or (MainForm.FindComponent('Pump3OnOffCB' + jStr)
        as TCheckBox).Checked
    or (MainForm.FindComponent('Pump4OnOffCB' + jStr)
        as TCheckBox).Checked
    or (MainForm.FindComponent('Pump5OnOffCB' + jStr)
        as TCheckBox).Checked
    or (MainForm.FindComponent('Pump6OnOffCB' + jStr)
        as TCheckBox).Checked
    or (MainForm.FindComponent('Pump7OnOffCB' + jStr)
        as TCheckBox).Checked
    or (MainForm.FindComponent('Pump8OnOffCB' + jStr)
        as TCheckBox).Checked
    then
    begin
     // first check the duty cycle, if it is not 100 we need an on-off loop
     if (MainForm.FindComponent('DutyCycle' + jStr + 'FSE')
         as TFloatSpinEdit).Value < 100 then
      command:= command + 'g';
     // syntax ist: Sxyyy, x = pump number, y = speed
     command:= command + 'S';
     HaveS:= True;
    end
    else
     HaveS:= False;
    // voltage, only write if pump is active
   if (MainForm.FindComponent('Pump1OnOffCB' + jStr) as TCheckBox).Checked then
   begin
    SOrder[0]:= '1';
    command:= command + '1';
    // 3.3 V is the maximum
    voltage:= FloatToStr(ceil(
     (MainForm.FindComponent('Pump1VoltageFS' + jStr)
      as TFloatSpinEdit).Value / 3.3 * 999));
    // we need to write always 3 characters
    case length(voltage) of
     2 : voltage:= '0' + voltage;
     1 : voltage:= '00' + voltage;
    end;
    command:= command + voltage;
    voltageCalc:= voltageCalc + StrToInt(voltage);
   end;
   if (MainForm.FindComponent('Pump2OnOffCB' + jStr) as TCheckBox).Checked then
   begin
    if SOrder[0] = '0' then
     SOrder[0]:= '2'
    else
     SOrder[1]:= '2';
    command:= command + '2';
    // 3.3 V is the maximum
    voltage:= FloatToStr(ceil(
     (MainForm.FindComponent('Pump2VoltageFS' + jStr)
      as TFloatSpinEdit).Value / 3.3 * 999));
    // we need to write always 3 characters
    case length(voltage) of
     2 : voltage:= '0' + voltage;
     1 : voltage:= '00' + voltage;
    end;
    command:= command + voltage;
    voltageCalc:= voltageCalc + StrToInt(voltage);
   end;
   if (MainForm.FindComponent('Pump3OnOffCB' + jStr) as TCheckBox).Checked then
   begin
    if SOrder[0] = '0' then
     SOrder[0]:= '3'
    else if SOrder[1] = '0' then
     SOrder[1]:= '3'
    else
     SOrder[2]:= '3';
    command:= command + '3';
    // 3.3 V is the maximum
    voltage:= FloatToStr(ceil(
     (MainForm.FindComponent('Pump3VoltageFS' + jStr)
      as TFloatSpinEdit).Value / 3.3 * 999));
    // we need to write always 3 characters
    case length(voltage) of
     2 : voltage:= '0' + voltage;
     1 : voltage:= '00' + voltage;
    end;
    command:= command + voltage;
    voltageCalc:= voltageCalc + StrToInt(voltage);
   end;
   if (MainForm.FindComponent('Pump4OnOffCB' + jStr) as TCheckBox).Checked then
   begin
    if SOrder[0] = '0' then
     SOrder[0]:= '4'
    else if SOrder[1] = '0' then
     SOrder[1]:= '4'
    else if SOrder[2] = '0' then
     SOrder[2]:= '4'
    else
     SOrder[3]:= '4';
    command:= command + '4';
    // 3.3 V is the maximum
    voltage:= FloatToStr(ceil(
     (MainForm.FindComponent('Pump4VoltageFS' + jStr)
      as TFloatSpinEdit).Value / 3.3 * 999));
    // we need to write always 3 characters
    case length(voltage) of
     2 : voltage:= '0' + voltage;
     1 : voltage:= '00' + voltage;
    end;
    command:= command + voltage;
    voltageCalc:= voltageCalc + StrToInt(voltage);
   end;
   if (MainForm.FindComponent('Pump5OnOffCB' + jStr) as TCheckBox).Checked then
   begin
    if SOrder[0] = '0' then
     SOrder[0]:= '5'
    else if SOrder[1] = '0' then
     SOrder[1]:= '5'
    else if SOrder[2] = '0' then
     SOrder[2]:= '5'
    else if SOrder[3] = '0' then
     SOrder[3]:= '5'
    else
     SOrder[4]:= '5';
    command:= command + '5';
    // 3.3 V is the maximum
    voltage:= FloatToStr(ceil(
     (MainForm.FindComponent('Pump5VoltageFS' + jStr)
      as TFloatSpinEdit).Value / 3.3 * 999));
    // we need to write always 3 characters
    case length(voltage) of
     2 : voltage:= '0' + voltage;
     1 : voltage:= '00' + voltage;
    end;
    command:= command + voltage;
    voltageCalc:= voltageCalc + StrToInt(voltage);
   end;
   if (MainForm.FindComponent('Pump6OnOffCB' + jStr) as TCheckBox).Checked then
   begin
    if SOrder[0] = '0' then
     SOrder[0]:= '6'
    else if SOrder[1] = '0' then
     SOrder[1]:= '6'
    else if SOrder[2] = '0' then
     SOrder[2]:= '6'
    else if SOrder[3] = '0' then
     SOrder[3]:= '6'
    else if SOrder[4] = '0' then
     SOrder[4]:= '6'
    else
     SOrder[5]:= '6';
    command:= command + '6';
    // 3.3 V is the maximum
    voltage:= FloatToStr(ceil(
     (MainForm.FindComponent('Pump6VoltageFS' + jStr)
      as TFloatSpinEdit).Value / 3.3 * 999));
    // we need to write always 3 characters
    case length(voltage) of
     2 : voltage:= '0' + voltage;
     1 : voltage:= '00' + voltage;
    end;
    command:= command + voltage;
    voltageCalc:= voltageCalc + StrToInt(voltage);
   end;
   if (MainForm.FindComponent('Pump7OnOffCB' + jStr) as TCheckBox).Checked then
   begin
    if SOrder[0] = '0' then
     SOrder[0]:= '7'
    else if SOrder[1] = '0' then
     SOrder[1]:= '7'
    else if SOrder[2] = '0' then
     SOrder[2]:= '7'
    else if SOrder[3] = '0' then
     SOrder[3]:= '7'
    else if SOrder[4] = '0' then
     SOrder[4]:= '7'
    else if SOrder[5] = '0' then
     SOrder[5]:= '7'
    else
     SOrder[6]:= '7';
    command:= command + '7';
    // 3.3 V is the maximum
    voltage:= FloatToStr(ceil(
     (MainForm.FindComponent('Pump7VoltageFS' + jStr)
      as TFloatSpinEdit).Value / 3.3 * 999));
    // we need to write always 3 characters
    case length(voltage) of
     2 : voltage:= '0' + voltage;
     1 : voltage:= '00' + voltage;
    end;
    command:= command + voltage;
    voltageCalc:= voltageCalc + StrToInt(voltage);
   end;
   if (MainForm.FindComponent('Pump8OnOffCB' + jStr) as TCheckBox).Checked then
   begin
    if SOrder[0] = '0' then
     SOrder[0]:= '8'
    else if SOrder[1] = '0' then
     SOrder[1]:= '8'
    else if SOrder[2] = '0' then
     SOrder[2]:= '8'
    else if SOrder[3] = '0' then
     SOrder[3]:= '8'
    else if SOrder[4] = '0' then
     SOrder[4]:= '8'
    else if SOrder[5] = '0' then
     SOrder[5]:= '8'
    else if SOrder[6] = '0' then
     SOrder[6]:= '8'
    else
     SOrder[7]:= '8';
    command:= command + '8';
    // 3.3 V is the maximum
    voltage:= FloatToStr(ceil(
     (MainForm.FindComponent('Pump8VoltageFS' + jStr)
      as TFloatSpinEdit).Value / 3.3 * 999));
    // we need to write always 3 characters
    case length(voltage) of
     2 : voltage:= '0' + voltage;
     1 : voltage:= '00' + voltage;
    end;
    command:= command + voltage;
    voltageCalc:= voltageCalc + StrToInt(voltage);
   end;
   // direction
   if HaveS then // only if there is any pump running
   begin
    command:= command + 'D';
    for k:= 1 to PumpNum do
    begin
     if SOrder[k-1] <> '0' then
      command:= command +
       IntToStr((MainForm.FindComponent('Pump' + SOrder[k-1] + 'DirectionRG' + jStr)
        as TRadioGroup).ItemIndex);
    end;
   end;
   // valves
   // if there are no valves omit V
   if ValveNum > 0 then
   begin
    command:= command + 'V';
    for k:= 1 to ValveNum do
    begin
     command:= command +
      IntToStr((MainForm.FindComponent('Valve' + IntToStr(k) + 'RG' + jStr)
        as TRadioGroup).ItemIndex);
    end;
   end;
   // action
   // if there are no pumps omit I
   if PumpNum > 0 then
    command:= command + 'I';
   for k:= 1 to PumpNum do
    command:= command +
     BoolToStr((MainForm.FindComponent('Pump' + IntToStr(k) + 'OnOffCB' + jStr)
        as TCheckBox).Checked, '1', '0');
   // calculate action time in ms
   timeStep:= 0;
   if (MainForm.FindComponent('Unit' + jStr + 'RBs')
       as TRadioButton).Checked then
    timeFactor:= 1000
   else if (MainForm.FindComponent('Unit' + jStr + 'RBmin')
            as TRadioButton).Checked then
    timeFactor:= 1000 * 60
   else if (MainForm.FindComponent('Unit' + jStr + 'RBh')
            as TRadioButton).Checked then
    timeFactor:= 1000 * 3600;

   // if we have not 100% duty cycle we need 2 steps
   if ((MainForm.FindComponent('DutyCycle' + jStr + 'FSE')
      as TFloatSpinEdit).Value < 100) and HaveS then
   begin
    if ((MainForm.FindComponent('DutyCycle' + jStr + 'FSE')
      as TFloatSpinEdit).Value / 100) >= 0.05 then
     XTime:= 1000 // base time is 1s
    else // calculate a base time so that the OnTime is 50 ms
     XTime:= round(0.05 / ((MainForm.FindComponent('DutyCycle' + jStr + 'FSE')
      as TFloatSpinEdit).Value / 100) * 1000);
    OnTime:= round((MainForm.FindComponent('DutyCycle' + jStr + 'FSE')
      as TFloatSpinEdit).Value / 100 * XTime); // time in ms
    OffTime:= XTime - OnTime;
    DutyRepeats:= round((MainForm.FindComponent('RunTime' + jStr + 'FSE')
      as TFloatSpinEdit).Value * timeFactor / XTime) - 1;
    // DutyRepeats can now be -1 if time is smaller than necessary OffTime
    if DutyRepeats < 0 then
     DutyRepeats:= 0;
    command:= command + 'M' + FloatToStr(OnTime);
    command:= command + 'I';
    for k:= 1 to PumpNum do
     command:= command + '0';
    command:= command + 'M' + FloatToStr(OffTime);
    command:= command + 'G' + IntToStr(DutyRepeats);
    timeStep:= XTime * (DutyRepeats + 1);
   end
   else // output the time directly
   begin
    command:= command + 'M'
     + FloatToStr((MainForm.FindComponent('RunTime' + jStr + 'FSE')
                   as TFloatSpinEdit).Value * timeFactor);
    timeStep:= (MainForm.FindComponent('RunTime' + jStr + 'FSE')
                as TFloatSpinEdit).Value * timeFactor;
   end;
   timeCalc:= timeCalc + timeStep;

   // When the sum of the S values is larger than 3*999 we must
   // split the step into substeps because the pump driver cannot
   // deliver enough current to start all pumps at once
   // In this case we will first start up to 3 pumps, then 10 ms later
   // the next up to 3 and so on

   if voltageCalc > (3*999) then
   begin
    // we take blocks of pumps vith a voltage below 3*999 thus restart counting
    voltageCalc:= 0; k2:= 1;
    countPump:= 0; countPumpNumber:= 0;
    commandOriginal:= command;
    commandSave:= ''; commandSplit:= '';
    for i:= posS to Length(commandOriginal) do
    begin
     if commandOriginal[i] = 'S' then
     begin
      // store the position of the first 'S' for this step
      posS:= i;
      k2:= i + 1;
     end;
     // parse now until the first 'D' is found
     while isDigit(commandOriginal[k2]) and (posS > 1) do
     begin
     voltageCalc:= voltageCalc + StrToInt(Copy(commandOriginal, k2+1, 3));
      if voltageCalc > (3*999) then
      begin // we have our first commandSplit
       commandSave:= commandSave + commandSplit;
       commandSplit:= commandSave;
       // add the direction
       commandSplit:= 'S' + commandSplit + 'D';
       for j2:= 1 to countPump do
       begin
        if SOrder[j2-1] <> '0' then
         commandSplit:= commandSplit +
          IntToStr((MainForm.FindComponent('Pump' + SOrder[j2-1] + 'DirectionRG' + jStr)
           as TRadioGroup).ItemIndex);
       end;
       // add the action
       commandSplit:= commandSplit + 'I';
       for j2:= 1 to countPumpNumber do
        commandSplit:= commandSplit +
         BoolToStr((MainForm.FindComponent('Pump' + IntToStr(j2) + 'OnOffCB' + jStr)
          as TCheckBox).Checked, '1', '0');
       for j2:= countPumpNumber + 1 to PumpNum do
        commandSplit:= commandSplit + '0';
       // eventually add the 10 ms
       commandSplit:= commandSplit + 'M10';
       // insert commandSplit to command
       Insert(commandSplit, command, posS);
       // move position and start collecting again
       posS:= posS + length(commandSplit);
       commandSplit:= '';
       // we start with the voltage of the currently not yet added pump
       voltageCalc:= StrToInt(Copy(commandOriginal, k2+1, 3));
      end; // end if voltageCalc
      // we can add another pump
      commandSplit:= commandSplit + Copy(commandOriginal, k2, 4);
      inc(countPump);
      countPumpNumber:= StrToInt(commandOriginal[k2]);
      k2:= k2 + 4;

     end; // end while
     // when voltageCalc is here > 0 we already parsed enough
     if voltageCalc > 0 then
      break;
    end; // end for i
    // set position to end of command for next step
    posS:= length(command);
   end; // end if voltageCalc > (3*999)

   // if the direction changes, wait 999 ms to protect the pumps
   // only if the next step is actually used and we have 100% duty cylce
   // only necessary if DutyCycle = 100
   if (MainForm.FindComponent('DutyCycle' + jStr + 'FSE')
       as TFloatSpinEdit).Value = 100 then
   begin
    if (j < StepNum) and (MainForm.FindComponent('Step' + IntToStr(j+1) + 'UseCB')
        as TCheckBox).checked then
    begin
     if ((MainForm.FindComponent('Pump1OnOffCB' + jStr) as TCheckBox).Checked
       and (MainForm.FindComponent('Pump1OnOffCB' + IntToStr(j+1)) as TCheckBox).Checked
       and ((MainForm.FindComponent('Pump1DirectionRG' + jStr) as TRadioGroup).ItemIndex
        <> (MainForm.FindComponent('Pump1DirectionRG' + IntToStr(j+1)) as TRadioGroup).ItemIndex))
      or ((MainForm.FindComponent('Pump2OnOffCB' + jStr) as TCheckBox).Checked
       and (MainForm.FindComponent('Pump2OnOffCB' + IntToStr(j+1)) as TCheckBox).Checked
       and ((MainForm.FindComponent('Pump2DirectionRG' + jStr) as TRadioGroup).ItemIndex
        <> (MainForm.FindComponent('Pump2DirectionRG' + IntToStr(j+1)) as TRadioGroup).ItemIndex))
      or ((MainForm.FindComponent('Pump3OnOffCB' + jStr) as TCheckBox).Checked
       and (MainForm.FindComponent('Pump3OnOffCB' + IntToStr(j+1)) as TCheckBox).Checked
       and ((MainForm.FindComponent('Pump3DirectionRG' + jStr) as TRadioGroup).ItemIndex
        <> (MainForm.FindComponent('Pump3DirectionRG' + IntToStr(j+1)) as TRadioGroup).ItemIndex))
      or ((MainForm.FindComponent('Pump4OnOffCB' + jStr) as TCheckBox).Checked
       and (MainForm.FindComponent('Pump4OnOffCB' + IntToStr(j+1)) as TCheckBox).Checked
       and ((MainForm.FindComponent('Pump4DirectionRG' + jStr) as TRadioGroup).ItemIndex
        <> (MainForm.FindComponent('Pump4DirectionRG' + IntToStr(j+1)) as TRadioGroup).ItemIndex))
      or ((MainForm.FindComponent('Pump5OnOffCB' + jStr) as TCheckBox).Checked
       and (MainForm.FindComponent('Pump5OnOffCB' + IntToStr(j+1)) as TCheckBox).Checked
       and ((MainForm.FindComponent('Pump5DirectionRG' + jStr) as TRadioGroup).ItemIndex
        <> (MainForm.FindComponent('Pump5DirectionRG' + IntToStr(j+1)) as TRadioGroup).ItemIndex))
      or ((MainForm.FindComponent('Pump6OnOffCB' + jStr) as TCheckBox).Checked
       and (MainForm.FindComponent('Pump6OnOffCB' + IntToStr(j+1)) as TCheckBox).Checked
       and ((MainForm.FindComponent('Pump6DirectionRG' + jStr) as TRadioGroup).ItemIndex
        <> (MainForm.FindComponent('Pump6DirectionRG' + IntToStr(j+1)) as TRadioGroup).ItemIndex))
      or ((MainForm.FindComponent('Pump7OnOffCB' + jStr) as TCheckBox).Checked
       and (MainForm.FindComponent('Pump7OnOffCB' + IntToStr(j+1)) as TCheckBox).Checked
       and ((MainForm.FindComponent('Pump7DirectionRG' + jStr) as TRadioGroup).ItemIndex
        <> (MainForm.FindComponent('Pump7DirectionRG' + IntToStr(j+1)) as TRadioGroup).ItemIndex))
      or ((MainForm.FindComponent('Pump8OnOffCB' + jStr) as TCheckBox).Checked
       and (MainForm.FindComponent('Pump8OnOffCB' + IntToStr(j+1)) as TCheckBox).Checked
       and ((MainForm.FindComponent('Pump8DirectionRG' + jStr) as TRadioGroup).ItemIndex
        <> (MainForm.FindComponent('Pump8DirectionRG' + IntToStr(j+1)) as TRadioGroup).ItemIndex))
     then
     begin
      // stop for 999 ms
      command:= command + 'I';
      for k:= 1 to PumpNum do
       command:= command + '0';
      command:= command + 'M999';
      timeStep:= timeStep + 999;
      timeCalc:= timeCalc + 999;
     end;
    end
    // next step could be step 1
    else if ((MainForm.RepeatSE.Value > 0) or (MainForm.RunEndlessCB.Checked)) and (j > 1) then
    begin
     if ((MainForm.FindComponent('Pump1OnOffCB' + IntToStr(j-1)) as TCheckBox).Checked
       and (MainForm.FindComponent('Pump1OnOffCB' + jStr) as TCheckBox).Checked
       and ((MainForm.FindComponent('Pump1DirectionRG' + IntToStr(j-1)) as TRadioGroup).ItemIndex
        <> (MainForm.FindComponent('Pump1DirectionRG' + jStr) as TRadioGroup).ItemIndex))
      or ((MainForm.FindComponent('Pump2OnOffCB' + IntToStr(j-1)) as TCheckBox).Checked
       and (MainForm.FindComponent('Pump2OnOffCB' + jStr) as TCheckBox).Checked
       and ((MainForm.FindComponent('Pump2DirectionRG' + IntToStr(j-1)) as TRadioGroup).ItemIndex
        <> (MainForm.FindComponent('Pump2DirectionRG' + jStr) as TRadioGroup).ItemIndex))
      or ((MainForm.FindComponent('Pump3OnOffCB' + IntToStr(j-1)) as TCheckBox).Checked
       and (MainForm.FindComponent('Pump3OnOffCB' + jStr) as TCheckBox).Checked
       and ((MainForm.FindComponent('Pump3DirectionRG' + IntToStr(j-1)) as TRadioGroup).ItemIndex
        <> (MainForm.FindComponent('Pump3DirectionRG' + jStr) as TRadioGroup).ItemIndex))
      or ((MainForm.FindComponent('Pump4OnOffCB' + IntToStr(j-1)) as TCheckBox).Checked
       and (MainForm.FindComponent('Pump4OnOffCB' + jStr) as TCheckBox).Checked
       and ((MainForm.FindComponent('Pump4DirectionRG' + IntToStr(j-1)) as TRadioGroup).ItemIndex
        <> (MainForm.FindComponent('Pump4DirectionRG' + jStr) as TRadioGroup).ItemIndex))
      or ((MainForm.FindComponent('Pump5OnOffCB' + IntToStr(j-1)) as TCheckBox).Checked
       and (MainForm.FindComponent('Pump5OnOffCB' + jStr) as TCheckBox).Checked
       and ((MainForm.FindComponent('Pump5DirectionRG' + IntToStr(j-1)) as TRadioGroup).ItemIndex
        <> (MainForm.FindComponent('Pump5DirectionRG' + jStr) as TRadioGroup).ItemIndex))
      or ((MainForm.FindComponent('Pump6OnOffCB' + IntToStr(j-1)) as TCheckBox).Checked
       and (MainForm.FindComponent('Pump6OnOffCB' + jStr) as TCheckBox).Checked
       and ((MainForm.FindComponent('Pump6DirectionRG' + IntToStr(j-1)) as TRadioGroup).ItemIndex
        <> (MainForm.FindComponent('Pump6DirectionRG' + jStr) as TRadioGroup).ItemIndex))
      or ((MainForm.FindComponent('Pump7OnOffCB' + IntToStr(j-1)) as TCheckBox).Checked
       and (MainForm.FindComponent('Pump7OnOffCB' + jStr) as TCheckBox).Checked
       and ((MainForm.FindComponent('Pump7DirectionRG' + IntToStr(j-1)) as TRadioGroup).ItemIndex
        <> (MainForm.FindComponent('Pump7DirectionRG' + jStr) as TRadioGroup).ItemIndex))
      or ((MainForm.FindComponent('Pump8OnOffCB' + IntToStr(j-1)) as TCheckBox).Checked
       and (MainForm.FindComponent('Pump8OnOffCB' + jStr) as TCheckBox).Checked
       and ((MainForm.FindComponent('Pump8DirectionRG' + IntToStr(j-1)) as TRadioGroup).ItemIndex
        <> (MainForm.FindComponent('Pump8DirectionRG' + jStr) as TRadioGroup).ItemIndex))
     then
     begin
      // only output if there is no single run
      if (MainForm.RepeatSE.Value > 0) or (MainForm.RunEndlessCB.Checked) then
      begin
       // stop for 999 ms
       command:= command + 'I';
       for k:= 1 to PumpNum do
        command:= command + '0';
       command:= command + 'M999';
       timeStep:= timeStep + 999;
       timeCalc:= timeCalc + 999;
      end;
     end;
    end; // end if if ((RepeatSE.Value > 0)
   end; // end if DutyCycle(JStr)FSE.Value = 100

   // a timer in Lazarus or can only run for 2^31-1 milliseconds
   // the timer in the Arduino could in principle run the double time, but
   // allowing this is not worth the effort (that under all circumstances
   // unsigned 32bit int is used) and we can expect an action within 24 days
   if timeStep > 2147483646 then
   begin
    MainForm.IndicatorPumpP.Color:= clRed;
    MainForm.IndicatorPumpP.Caption:= 'Step time ' + jStr + 'too long!';
    MainForm.IndicatorPumpP.Hint:= 'time for one step must not exceed 596 h';
    MainForm.IndicatorPumpPPaint;
    result:= False;
    exit;
   end;
   // set timer interval
   (MainForm.FindComponent('StepTimer' + jStr) as TTimer).Interval:= trunc(timeStep);

  end; // end if (MainForm.FindComponent('Step' + jStr + 'UseCB') as TCheckBox).Checked
 end; // end for j:=1 to StepNum

 // end loop flag
 if MainForm.RunEndlessCB.Checked then
 begin
  if not withSensor then
   command:= command + 'G';
  timeCalc:= MAXINT; // set maximal possible int for infinite repeats
 end;
 // if repeated run
 if (StrToInt(MainForm.RepeatSE.Text) > 0) and (not MainForm.RunEndlessCB.Checked) then
 begin
  if not withSensor then
   command:= command + 'G' + IntToStr(MainForm.RepeatSE.Value);
  timeCalc:= timeCalc * (MainForm.RepeatSE.Value + 1);
 end;

  // explicitly turn off all valves, pumps, turn off LED and execute flag
  // the explicite turn off is important because the Arduino command
  // execution loop needs several 100 ms. But when an explicit stop is sent,
  // the loop will end immediately
  if ValveNum > 0 then
  begin
   command:= command + 'V';
   for k:= 1 to ValveNum do
    command:= command + '0';
  end;
  if PumpNum > 0 then
  begin
   command:= command + 'I';
   for k:= 1 to PumpNum do
    command:= command + '0';
  end;
  command:= command + 'lR';

  // calculate the total time
  if not MainForm.RunEndlessCB.Checked then
  begin
   // output time in sensible unit
   if timeCalc <= 1e6 then
   begin
    MainForm.TotalTimeLE.Text:= FloatToStr(RoundTo(timeCalc /1000, -1));
    MainForm.TotalTimeLE.EditLabel.Caption:= 'Total Time in s';
   end
   else if timeCalc <= 60e6 then
   begin
    timeOut:= timeCalc / 1000 / 60;
    MainForm.TotalTimeLE.Text:= FloatToStr(RoundTo(timeOut, -2));
    MainForm.TotalTimeLE.EditLabel.Caption:= 'Total Time in min';
   end
   else if timeCalc > 60e6 then
   begin
    timeOut:= timeCalc / 1000 / 60 / 60;
    MainForm.TotalTimeLE.Text:= FloatToStr(RoundTo(timeOut, -2));
    MainForm.TotalTimeLE.EditLabel.Caption:= 'Total Time in h';
   end;
  end
  else
   MainForm.TotalTimeLE.Text:= 'Until Stop pressed';

  // output the time and result
  GlobalTime:= trunc(timeCalc);
  result:= True;
end;

procedure TPumpControl.RunImmediate;
// execute generated command
var
  command : string;
  CommandResult : Boolean = False;
begin
 // generate command
 CommandResult:= GenerateCommand(command);
 // if GenerateCommand returns e.g. a too long time stop
 if not CommandResult then
 begin
  MainForm.StopBBClick(MainForm.StopBB);
  exit;
 end;
 MainForm.CommandM.Text:= command;
 commandForRepeat:= command;
 // The TinyZero has an input buffer of 512 characters, if it is full, the
 // COM connection will break (no communication posible).
 // There is a special case (in my opinion a bug) that if the input string has
 // modulo 64 characters, the TinyZero will not accept it directly. First with
 // the next command it will be executed (e.g. when pressing the Stop button).
 // The solution is to vary in this case the string termination since the
 // Arduino code checks only for the #10 char.
 if (Length(command) + 2) mod 64 = 0 then
  command:= command + #10
 else
  command:= command + LineEnding;
  // if we have an open serial connection, execute
 if MainForm.HavePumpSerialCB.Checked then
 begin
  // disable the connection menu that the user cannot close
  // the conenction while the pumps are running
  MainForm.PumpDriverMI.Enabled:= False;
  MainForm.DriverConnectBB.Enabled:= False;
  MainForm.FirmwareUpdateMI.Enabled:= False;
  MainForm.FirmwareResetMI.Enabled:= False;
  // disable menu to load and save action files
  MainForm.LoadActionMI.Enabled:= False;
  MainForm.SaveActionMI.Enabled:= False;
  // send the command
  serPump.SendString(command);
  if serPump.LastError <> 0 then
  begin
   with Application do
    MessageBox(PChar(connectedPumpName + ' error: ' + serPump.LastErrorDesc),
                     'Error', MB_ICONERROR+MB_OK);
   MainForm.ConnComPortPumpLE.Color:= clRed;
   MainForm.ConnComPortPumpLE.Text:= 'Try to reconnect';
   MainForm.IndicatorPumpP.Caption:= 'Connection failiure';
   MainForm.IndicatorPumpP.Color:= clRed;
   MainForm.IndicatorPumpPPaint;
   MainForm.PumpDriverMI.Enabled:= True;
   MainForm.DriverConnectBB.Enabled:= True;
   MainForm.RunBB.Enabled:= False;
   if serPump.LastError = 9997 then
   begin
    MainForm.StopBB.Enabled:= False;
    exit; // we cannot close socket or free if the connection timed out
   end;
   MainForm.ClosePumpSerialConn;
   exit;
  end;
 end
 else // no serial connection
 begin
  MainForm.RunBB.Enabled:= False;
  exit;
 end;

end;

procedure TPumpControl.PCLiveModeCBChange(Sender: TObject);
// changes the program to transmit every pump settings change immediately
var
j : integer;
begin
 if MainForm.LiveModeCB.Checked then
 begin
  // make all steps invisible and rename step 1
  for j:= 2 to StepNum do
   (MainForm.FindComponent('Step' + IntToStr(j) + 'TS')
    as TTabSheet).TabVisible:= false;
  // assure that step 2 is not used
  MainForm.Step2UseCB.Checked:= false;
  MainForm.RunSettingsGB.Enabled:= false;
  MainForm.CalibrationGB.Enabled:= false;
  MainForm.Step1TS.Caption:= 'Live';
  // set that run until stop pressed
  MainForm.RunEndlessCB.Checked:= true;
  // en/disable pump setting elements
  PCPumpOnOffCBLoopChange(Sender);
 end
 else
 begin
  MainForm.RunSettingsGB.Enabled:= true;
  MainForm.CalibrationGB.Enabled:= true;
  // rename step 1 back and show step 2
  MainForm.Step1TS.Caption:= 'Step 1';
  MainForm.Step2TS.TabVisible:= true;
  MainForm.RunEndlessCB.Checked:= false;
  // en/disable pump setting elements
  PCPumpOnOffCBLoopChange(Sender);
 end;

end;

procedure TPumpControl.PCPumpOnOffCBLoopChange(Sender: TObject);
var
  i, step : integer;
begin
for step:= 1 to StepNum do
  for i:= 1 to PumpNum do
  begin
   // when in live mode the pump direction must be kept disabled when pump
   // is on and all elements must be enabled when the pump if off
   if MainForm.LiveModeCB.Checked then
   begin
   (MainForm.FindComponent('Pump' + IntToStr(i) + 'DirectionRG' + IntToStr(step))
     as TRadioGroup).Enabled:= not (MainForm.FindComponent('Pump' + IntToStr(i) + 'OnOffCB' + IntToStr(step))
     as TCheckBox).Checked;
   (MainForm.FindComponent('Pump' + IntToStr(i) + 'VoltageFS' + IntToStr(step))
     as TFloatSpinEdit).Enabled:= True;
   end
   else // outside live mode, disable all elements when pump is off
   begin
   (MainForm.FindComponent('Pump' + IntToStr(i) + 'DirectionRG' + IntToStr(step))
     as TRadioGroup).Enabled:= (MainForm.FindComponent('Pump' + IntToStr(i) + 'OnOffCB' + IntToStr(step))
     as TCheckBox).Checked;
   (MainForm.FindComponent('Pump' + IntToStr(i) + 'VoltageFS' + IntToStr(step))
     as TFloatSpinEdit).Enabled:= (MainForm.FindComponent('Pump' + IntToStr(i) + 'OnOffCB' + IntToStr(step))
     as TCheckBox).Checked;
   end;
  end;
  // if in live mode send trigger command generation and sending
  if MainForm.LiveModeCB.Checked and MainForm.OverallTimer.Enabled then
   RunImmediate;
end;

procedure TPumpControl.PCDutyCycleXFSEChange(Sender: TObject);
var
 DutyTime, StepTime : Double;
 j, Step : integer;
 SenderName : string;
begin
 SenderName:= (Sender as TComponent).Name;
 // SenderName is in the form "DutyCyclexFSE" and we need the x
 // so get the 10th character of the name
 Step:= StrToInt(Copy(SenderName, 10, 1));
 // reset increment to 1. If this is not sufficent,
 // it will be reset later in this procedure
 (MainForm.FindComponent('RunTime' + IntToStr(Step) + 'FSE')
   as TFloatSpinEdit).Increment:= 1;
 // if the duty cycle is not 100% we must require 1.1 V for the pumps
 // otherwise the voltage would be too low to start a short movement
 if (MainForm.FindComponent('DutyCycle' + IntToStr(Step) + 'FSE')
        as TFloatSpinEdit).Value < 100 then
  for j:= 1 to PumpNum do
   (MainForm.FindComponent('Pump' + IntToStr(j) + 'VoltageFS' + IntToStr(Step))
      as TFloatSpinEdit).MinValue:= 1.1
 else
 begin
  for j:= 1 to PumpNum do
   (MainForm.FindComponent('Pump' + IntToStr(j) + 'VoltageFS' + IntToStr(Step))
     as TFloatSpinEdit).MinValue:= 0.1;
  // also allow 50 ms OnTime because this might have been changed previously
  (MainForm.FindComponent('RunTime' + IntToStr(Step) + 'FSE')
   as TFloatSpinEdit).MinValue:= 0.05;
 end;
 // calculate necessary time increment
 if ((MainForm.FindComponent('DutyCycle' + IntToStr(Step) + 'FSE')
       as TFloatSpinEdit).Value / 100) >= 0.05 then
  DutyTime:= 1 // base time is 1s
 else // calculate a base time so that the OnTime is 50 ms
  DutyTime:= 0.05 / ((MainForm.FindComponent('DutyCycle' + IntToStr(Step) + 'FSE')
        as TFloatSpinEdit).Value / 100);
 // if the unit is s, we can also set a new increment
 // and we must adjust the MinValue if duty is < 100 %
 if (MainForm.FindComponent('Unit' + IntToStr(Step) + 'RBs')
      as TRadioButton).Checked then
 begin
  (MainForm.FindComponent('RunTime' + IntToStr(Step) + 'FSE')
    as TFloatSpinEdit).Increment:= round(DutyTime);
  if (MainForm.FindComponent('DutyCycle' + IntToStr(Step) + 'FSE')
       as TFloatSpinEdit).Value < 100 then
   (MainForm.FindComponent('RunTime' + IntToStr(Step) + 'FSE')
     as TFloatSpinEdit).MinValue:= round(DutyTime);
 end;
 // the set time might be smaller than necessary
 StepTime:= 1; // 1s
 if (MainForm.FindComponent('Unit' + IntToStr(Step) + 'RBmin')
        as TRadioButton).Checked then
    StepTime:= 60
 else if (MainForm.FindComponent('Unit' + IntToStr(Step) + 'RBh')
        as TRadioButton).Checked then
    StepTime:= 3600;
 StepTime:= (MainForm.FindComponent('RunTime' + IntToStr(Step) + 'FSE')
        as TFloatSpinEdit).Value * StepTime; // time in s
 if StepTime < DutyTime then
  // the maximal DutyTime is 50 s, thus the unit is already s
  (MainForm.FindComponent('RunTime' + IntToStr(Step) + 'FSE')
        as TFloatSpinEdit).Value:= DutyTime;

  // update the resulting speed
  for j:= 1 to PumpNum do
   (MainForm.FindComponent('Pump' + IntToStr(j) + 'ResultLE' + IntToStr(Step))
     as TLabeledEdit).Text:= FloatToStr(RoundTo(
      (MainForm.FindComponent('DutyCycle' + IntToStr(Step) + 'FSE')
        as TFloatSpinEdit).Value *
      (MainForm.FindComponent('Pump' + IntToStr(j) + 'VoltageFS' + IntToStr(Step))
        as TFloatSpinEdit).Value / 3.3 , -2));

 // if in live mode send trigger command generation and sending
 if MainForm.LiveModeCB.Checked and MainForm.OverallTimer.Enabled then
  RunImmediate;
end;

procedure TPumpControl.PCStepXUseCBChange(Sender: TObject);
var
 Step, j : integer;
 SenderName : string;
begin
 SenderName:= (Sender as TComponent).Name;
 // SenderName is in the form "StepxTS" and we need the x
 // so get the 5th character of the name
 Step:= StrToInt(Copy(SenderName, 5, 1));
 if (MainForm.FindComponent('Step' + IntToStr(Step) + 'UseCB')
     as TCheckBox).Checked then
 begin
  if Step <> StepNum then
   (MainForm.FindComponent('Step' + IntToStr(Step+1) + 'TS')
    as TTabSheet).TabVisible:= True;
  (MainForm.FindComponent('ActionTime' + IntToStr(Step) + 'GB')
   as TGroupBox).Enabled:= True;
  (MainForm.FindComponent('DutyCycle' + IntToStr(Step) + 'GB')
   as TGroupBox).Enabled:= True;
  for j:= 1 to PumpNum do
   (MainForm.FindComponent('Pump' + IntToStr(j) + 'GB' + IntToStr(Step))
    as TGroupBox).Enabled:= True;
  for j:= 1 to ValveNum do
   (MainForm.FindComponent('Valve' + IntToStr(j) + 'RG' + IntToStr(Step))
    as TRadioGroup).Enabled:= True;
  // in case it was disabled on unchecking step 2
  if (Step = 2) and (not MainForm.ActionTime1GB.Enabled) then
   MainForm.ActionTime1GB.Enabled:= True;
 end
 else
 begin
  if Step <> StepNum then
   (MainForm.FindComponent('Step' + IntToStr(Step+1) + 'TS')
    as TTabSheet).TabVisible:= False;
  (MainForm.FindComponent('ActionTime' + IntToStr(Step) + 'GB')
   as TGroupBox).Enabled:= False;
  (MainForm.FindComponent('DutyCycle' + IntToStr(Step) + 'GB')
   as TGroupBox).Enabled:= False;
  for j:= 1 to PumpNum do
   (MainForm.FindComponent('Pump' + IntToStr(j) + 'GB' + IntToStr(Step))
    as TGroupBox).Enabled:= False;
  for j:= 1 to ValveNum do
   (MainForm.FindComponent('Valve' + IntToStr(j) + 'RG' + IntToStr(Step))
    as TRadioGroup).Enabled:= False;
  // if there is only one step and endless repeat disable time settings
  if (Step = 2) and (MainForm.RunEndlessCB.Checked) then
   MainForm.ActionTime1GB.Enabled:= False;
 end;
 PCRepeatPCChange(Sender);
end;

procedure TPumpControl.PCPumpVoltageFSChange(Sender: TObject);
var
 Step, PumpNumber : integer;
 SenderName : string;
begin
 SenderName:= (Sender as TComponent).Name;
 // SenderName is in the form "PumpXVoltageFSY" and we need the X and Y
 // so get the 5th and 15th character of the name
 Step:= StrToInt(Copy(SenderName, 15, 1));
 PumpNumber:= StrToInt(Copy(SenderName, 5, 1));
 // update the resulting speed
 (MainForm.FindComponent('Pump' + IntToStr(PumpNumber) + 'ResultLE' + IntToStr(Step))
  as TLabeledEdit).Text:= FloatToStr(RoundTo(
  (MainForm.FindComponent('DutyCycle' + IntToStr(Step) + 'FSE')
   as TFloatSpinEdit).Value *
  (MainForm.FindComponent('Pump' + IntToStr(PumpNumber) + 'VoltageFS' + IntToStr(Step))
   as TFloatSpinEdit).Value / 3.3 , -2));

 // if in live mode send trigger command generation and sending
 if MainForm.LiveModeCB.Checked and MainForm.OverallTimer.Enabled then
  PumpControl.RunImmediate;
end;

procedure TPumpControl.PCRepeatPCChange(Sender: TObject);
// set visibility of repeat tabs
var
 i : integer;
begin
 for i:= 2 to StepNum-1 do
  if (MainForm.FindComponent('Step' + IntToStr(i) + 'TS')
      as TTabSheet).TabVisible = False then
   (MainForm.FindComponent('Step' + IntToStr(i+1) + 'TS')
    as TTabSheet).TabVisible:= False;
  for i:= 2 to StepNum-1 do
   if ((MainForm.FindComponent('Step' + IntToStr(i) + 'UseCB')
        as TCheckBox).Checked and
      (MainForm.FindComponent('Step' + IntToStr(i) + 'TS')
       as TTabSheet).TabVisible) then
    (MainForm.FindComponent('Step' + IntToStr(i+1) + 'TS')
     as TTabSheet).TabVisible:= True;
end;

procedure TPumpControl.PCStepTimerXFinished(Sender: TObject);
var
 Step, i: integer;
 SenderName : string;
 Subst : Substance;
 Average : double = 0;
begin
 SenderName:= (Sender as TComponent).Name;
 // SenderName is in the form "StepTimerX" and we need the X
 // so get the 10th character of the name
 Step:= StrToInt(Copy(SenderName, 10, 1));

 // if there is a step+1, start its timer and show its tab
 if (MainForm.FindComponent('Step' + IntToStr(Step+1) + 'UseCB')
     as TCheckBox).checked then
 begin
  // the interval is calculated in GenerateCommand
  (MainForm.FindComponent('StepTimer' + IntToStr(Step+1))
   as TTimer).Enabled:= True;
  MainForm.RepeatPC.ActivePage:=
   (MainForm.FindComponent('Step' + IntToStr(Step+1) + 'TS') as TTabSheet);
  // highlight the new active step by adding an asterisk to the step name
  (MainForm.FindComponent('Step' + IntToStr(Step+1) + 'TS')
   as TTabSheet).Caption:= 'Step ' + IntToStr(Step+1) + ' *';
 end
 else // there might be a repeat
 begin
  // switch to step 1
  MainForm.StepTimer1.Enabled:= True;
  // when there are finite number of repeats PCRepeatTimerFinished
  // already just performed the calibation
  if MainForm.HavePumpSerialCB.Checked and
   (MainForm.RunEndlessCB.Checked or (MainForm.RepeatSE.Value > 0)) then
   // send repeat sequence to pump driver
   PCSendRepeatToPump;
  MainForm.RepeatPC.ActivePage:= MainForm.Step1TS;
  // highlight it as active by adding an asterisk to the step name
  MainForm.Step1TS.Caption:= 'Step 1 *';
 end;

 // only when there is only step 1 we must not stop the timer
 if not ((Step = 1) and (not (MainForm.FindComponent('Step' + IntToStr(Step+1) + 'UseCB')
     as TCheckBox).checked)) then
 begin
  (MainForm.FindComponent('StepTimer' + IntToStr(Step))
   as TTimer).Enabled:= False;
  // remove asterisk from current step caption
  (MainForm.FindComponent('Step' + IntToStr(Step) + 'TS')
   as TTabSheet).Caption:= 'Step ' + IntToStr(Step);
 end;

 // perform a calibration if necessary
 if MainForm.UseCalibCB.Checked
  and (MainForm.CalibStepCB.ItemIndex = Step - 1)
  and ((CurrentRepeat+1) mod MainForm.CalibEveryXStepsSE.Value = 0) then
 begin
  for Subst in Substance do
   SIXControl.SCPerformAutoCalib(Subst);
 end;
 // perform a measurement if necessary
 if MainForm.PerformLinearityCB.Checked then
 begin
  if (MainForm.FindComponent('Step' + IntToStr(Step) + 'MeasureValueFSE')
   as TFloatSpinEdit).Value > 0 then
  begin
   // determine measurement value as average of the given number of last values
   for i:= 0 to MainForm.MeasureAverageSE.Value - 1 do
   begin
    Average:= Average +
     (MainForm.FindComponent('SIXCh' + IntToStr(Step) + 'Values')
      as TLineSeries).YValue[
       (MainForm.FindComponent('SIXCh' + IntToStr(Step) + 'Values')
        as TLineSeries).Count - 1 - i];
   end;
   Average:= Average / MainForm.MeasureAverageSE.Value;
   // output to diagram
   (MainForm.FindComponent('SIXCh' + IntToStr(Step) + 'Results')
     as TLineSeries).AddXY(
    (MainForm.FindComponent('Step' + IntToStr(Step) + 'MeasureValueFSE')
     as TFloatSpinEdit).Value, Average);

   // update average if there is a channel selected for the result diagram
   SIXControl.SubstMeasureCLBItemClick(Sender, 0);
  end;
  //for Subst in Substance do
  // SIXControl.SCPerformAutoCalib(Subst);
 end;
end;

procedure TPumpControl.PCStepTimerLastFinished(Sender: TObject);
var
 Subst: Substance;
begin
 // switch to step 1
 MainForm.StepTimer1.Enabled:= True;
 // when there are finite number of repeats PCRepeatTimerFinished
 // already just performed the calibation

 // send repeat sequence to pump driver
 if MainForm.HavePumpSerialCB.Checked and
  (MainForm.RunEndlessCB.Checked or (MainForm.RepeatSE.Value > 0)) then
  PCSendRepeatToPump;

 // perform a calibration if necessary
 if MainForm.UseCalibCB.Checked and (MainForm.CalibStepCB.ItemIndex = 6) then
 begin
  for Subst in Substance do
   SIXControl.SCPerformAutoCalib(Subst);
 end;

 // remove asterisk from step caption
 MainForm.Step7TS.Caption:= 'Step 7';
 (MainForm.FindComponent('StepTimer' + IntToStr(PumpControl.StepNum))
        as TTimer).Enabled:= False;
 MainForm.RepeatPC.ActivePage:= MainForm.Step1TS;
end;

procedure TPumpControl.PCRunBBClick(Sender: TObject);
// execute generated command
var
  command, StartTime, Substance : string;
  i, j : integer;
  CommandResult : Boolean = False;
  MousePointer : TPoint;
begin
 MousePointer:= Mouse.CursorPos; // store mouse position
  // if there should be a calibration but no sensor is connected, stop
  if MainForm.UseCalibCB.Checked and (not MainForm.HaveSerialSensorCB.Checked) then
  begin
   MessageDlgPos('The run sequence contains a calibration'
    + LineEnding + 'but no sensor is connected.' + LineEnding
    + 'Start a SIX measurement first',
    mtError, [mbOK], 0 , MousePointer.X, MousePointer.Y);
   exit;
  end;
  // if there is a calibration but no calibration channel is selected, stop
  if MainForm.UseCalibCB.Checked and
   (MainForm.GlucoseCalibCLB.SelCount = 0) and
   (MainForm.LactateCalibCLB.SelCount = 0) then
  begin
   MessageDlgPos('No calibration channel selected in the calibration settings.'
    + LineEnding + 'Set it manually or reload your action file.',
    mtError, [mbOK], 0 , MousePointer.X, MousePointer.Y);
   exit;
  end;
  // if there is a calibration, we must create the subfolder
  // to store the new sensor definition files
  if MainForm.UseCalibCB.Checked then
  begin
  if not (RightStr(ExtractFilePath(InNameDef), 16) = 'DefinitionFiles\')
   and
   (not DirectoryExists(ExtractFilePath(InNameDef) + 'DefinitionFiles\')) then
   CreateDir(ExtractFilePath(InNameDef) + 'DefinitionFiles\');
  end;
  // generate command
  if MainForm.HaveSerialSensorCB.Checked then
   CommandResult:= GenerateCommand(command, true)
  else
   CommandResult:= GenerateCommand(command);
  // if GenerateCommand returns e.g. a too long time do nothing
  if not CommandResult then
   exit;
  MainForm.StopTimer.Enabled:= False;
  // whatever might happen, there must be a way to stop
  MainForm.StopBB.Enabled:= True;
  MainForm.CommandM.Text:= command;
  // The TinyZero has an input buffer of 512 characters, if it is full, the
  // COM connection will break (no communication posible).
  // There is a special case (a bug) that if the input string has
  // modulo 64 characters, the TinyZero will not accept it directly. First with
  // the next command it will be executed (e.g. when pressing the Stop button).
  // The solution is to vary in this case the string termination since the
  // Arduino code checks only for the #10 char.
  if (Length(command) + 2) mod 64 = 0 then
   command:= command + #10
  else
   command:= command + LineEnding;

  // save command to be resend on every repeat
  commandForRepeat:= command;

  // if no serial connection but pumps, we can stop here
  if (not MainForm.HavePumpSerialCB.Checked)
   and (not MainForm.HasNoPumpsCB.Checked) then
  begin
   MainForm.RunBB.Enabled:= False;
   exit;
  end;

  if MainForm.HavePumpSerialCB.Checked then
  begin
   // send the command
   serPump.SendString(command);
   if serPump.LastError <> 0 then
   begin
    with Application do
     MessageBox(PChar(connectedPumpName + ' error: ' + serPump.LastErrorDesc),
                      'Error', MB_ICONERROR + MB_OK);
    MainForm.ConnComPortPumpLE.Color:= clRed;
    MainForm.ConnComPortPumpLE.Text:= 'Try to reconnect';
    MainForm.IndicatorPumpP.Caption:= 'Connection failiure';
    MainForm.IndicatorPumpP.Color:= clRed;
    MainForm.IndicatorPumpPPaint;
    MainForm.PumpDriverMI.Enabled:= True;
    MainForm.DriverConnectBB.Enabled:= True;
    MainForm.RunBB.Enabled:= False;
    if serPump.LastError = 9997 then
    begin
     MainForm.StopBB.Enabled:= False;
     exit; // we cannot close socket or free if the connection timed out
    end;
    MainForm.ClosePumpSerialConn;
    exit;
   end;
  end;

  // pump settings when not in live mode
  if not MainForm.LiveModeCB.Checked then
  begin
   // the user must be able to see if the pumps 5 - 8 are set
   // therefore we cannot just disable the StepXTS component but its
   // child components except of SXPC
   for j:= 1 to StepNum do
   begin
    (MainForm.FindComponent('Step' + IntToStr(j) + 'UseCB')
     as TCheckBox).Enabled:= False;
    (MainForm.FindComponent('ActionTime' + IntToStr(j) + 'GB')
     as TGroupBox).Enabled:= False;
    (MainForm.FindComponent('DutyCycle' + IntToStr(j) + 'GB')
     as TGroupBox).Enabled:= False;
    (MainForm.FindComponent('S' + IntToStr(j) + 'P14')
     as TTabSheet).Enabled:= False;
    (MainForm.FindComponent('S' + IntToStr(j) + 'P58')
     as TTabSheet).Enabled:= False;
    (MainForm.FindComponent('S' + IntToStr(j) + 'Valves')
     as TTabSheet).Enabled:= False;
    if j = 1 then
     // disable tooltips for pump name
     for i:= 1 to PumpNum do
      (MainForm.FindComponent('Pump' + IntToStr(i) + 'GB' + IntToStr(j))
       as TGroupBox).ShowHint:= False;
   end;
   MainForm.CalibStepCB.Enabled:= False;
   MainForm.CalibStepL.Enabled:= False;
   MainForm.UseCalibCB.Enabled:= False;
   MainForm.CalibEveryXStepsSE.Enabled:= False;
   MainForm.CalibEveryXStepsL1.Enabled:= False;
   MainForm.CalibEveryXStepsL2.Enabled:= False;
   MainForm.UsedCalibValueSE.Enabled:= False;
   MainForm.UsedCalibValueL.Enabled:= False;
   for j:= 1 to MainForm.CalibSubstancesPC.PageCount do
   begin
    // the user must be able to see the settings for all substances
    // therefore we cannot just disable the CalibSubstancesPC component but its
    // child components except of XTS
    Substance:= MainForm.CalibSubstancesPC.Pages[j-1].Caption;
    (MainForm.FindComponent(Substance + 'AvailChanL')
     as TLabel).Enabled:= False;
    (MainForm.FindComponent(Substance + 'CalibGB')
     as TGroupBox).Enabled:= False;
    (MainForm.FindComponent(Substance + 'CalibCLB')
    as TChartListbox).Enabled:= False;
   end;
  end;
  // further UI settings
  MainForm.RepeatOutputLE.Visible:= False;
  MainForm.IndicatorPumpP.Caption:= 'Action is running';
  MainForm.IndicatorPumpP.Color:= clRed;
  MainForm.IndicatorPumpPPaint;
  MainForm.LinearityTestGB.Enabled:= False;
  // set timers
  if (StrToInt(MainForm.RepeatSE.Text) > 0)
   and (MainForm.RunEndlessCB.Checked = False) then
  begin
   MainForm.RepeatOutputLE.Visible:= True;
   RepeatTime:= trunc(GlobalTime / (StrToFloat(MainForm.RepeatSE.Text) + 1));
   if RepeatTime < oneDay then
    MainForm.RepeatTimer.Interval:= trunc(RepeatTime)
   else // to restart timer every day
    MainForm.RepeatTimer.Interval:= oneDay;
   MainForm.RepeatTimer.Enabled:= True;
   CurrentRepeat:= 0;
   MainForm.RepeatOutputLE.Text:= '0';
   // set time that will later be evaluated when the timer ends
  GlobalRepeatTime:= RepeatTime;
  end;
  // delete finish time
  MainForm.FinishTimePumpLE.Text:= '';
  // output start time
  startTime:= FormatDateTime('dd.mm.yyyy, hh:nn:ss', now);
  MainForm.StartTimePumpLE.Text:= startTime;

  // start OverallTimer to indicate running state
  if GlobalTime < oneDay then
   MainForm.OverallTimer.Interval:= trunc(GlobalTime)
  else // to restart timer every day
   MainForm.OverallTimer.Interval:= oneDay;
  MainForm.OverallTimer.Enabled:= True;
  MainForm.ChangeSensorDataFileMI.Enabled:= False;
  // show first tab and start its timer
  MainForm.RepeatPC.ActivePage:= MainForm.Step1TS;
  MainForm.StepTimer1.Enabled:= true;
  // if there are more steps, highlight the first one as being active
  // by adding an asterisk to the step name
  if MainForm.Step2UseCB.Checked then
   MainForm.Step1TS.Caption:= 'Step 1 *';
  // do not show unused steps
  for j:= 2 to StepNum do
  begin
   if (MainForm.FindComponent('Step' + IntToStr(j) + 'UseCB')
     as TCheckBox).Checked = False then
    (MainForm.FindComponent('Step' + IntToStr(j) + 'TS')
     as TTabSheet).TabVisible:= False;
  end;

 // if calibration is used, the .def file must not be unloaded or changed
 if MainForm.UseCalibCB.Checked then
 begin
  MainForm.LoadOtherDefBB.enabled:= False;
  MainForm.UnloadDefBB.enabled:= False;
 end;

end;

procedure TPumpControl.PCStopBBClick(Sender: TObject);
// stop all pumps
var
 command, stopTime, Substance : string;
 i, j, k : integer;
 MousePointer : TPoint;
begin
 // re-enable the connection menu in every case
 MainForm.PumpDriverMI.Enabled:= True;
 MainForm.DriverConnectBB.Enabled:= True;
 MainForm.FirmwareUpdateMI.Enabled:= True;
 MainForm.FirmwareResetMI.Enabled:= True;
 // re-enable menu to load and save action files
 MainForm.LoadActionMI.Enabled:= True;
 MainForm.SaveActionMI.Enabled:= True;
 // enable .def file loading
 MainForm.LoadOtherDefBB.enabled:= True;
 MainForm.UnloadDefBB.enabled:= True;
 // address
 command:= '/0';
 // disable all valves
 if ValveNum > 0 then
 begin
  command:= command + 'V';
  for k:= 1 to ValveNum do
   command:= command + '0';
 end;
 // disable all pumps
 command:= command + 'I';
 for k:= 1 to PumpNum do
  command:= command + '0';
 // execute flag and turn off LED
 command:= command + 'lR';
 // execute
 MainForm.CommandM.Text:= command;
 command:= command + LineEnding;
 if MainForm.HavePumpSerialCB.Checked then
 begin
  serPump.SendString(command);
  if serPump.LastError <> 0 then
  begin
   MousePointer:= Mouse.CursorPos;
   MessageDlgPos(connectedPumpName + ' error: ' + serPump.LastErrorDesc,
                 mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   MainForm.ConnComPortPumpLE.Color:= clRed;
   MainForm.ConnComPortPumpLE.Text:= 'Try to reconnect';
   MainForm.IndicatorPumpP.Caption:= 'Connection failiure';
   MainForm.IndicatorPumpP.Color:= clRed;
   MainForm.IndicatorPumpPPaint;
   MainForm.PumpDriverMI.Enabled:= True;
   MainForm.DriverConnectBB.Enabled:= True;
   if serPump.LastError = 9997 then
   begin
    MainForm.StopBB.Enabled:= False;
    exit; // we cannot close socket or free when the connection timed out
   end;
   MainForm.ClosePumpSerialConn;
   exit;
  end;
 end;
 // output stop time only when there was actually a run
 if MainForm.IndicatorPumpP.Caption = 'Action is running' then
 begin
  stopTime:= FormatDateTime('dd.mm.yyyy, hh:nn:ss', now);
  MainForm.FinishTimePumpLE.Text:= stopTime;
  MainForm.IndicatorPumpP.Caption:= 'Manually stopped';
  MainForm.IndicatorPumpP.Color:= clHighlight;
  MainForm.IndicatorPumpPPaint;
 end;
 MainForm.OverallTimer.Enabled:= False;
 MainForm.RepeatTimer.Enabled:= False;
 // we must prevent that too long commands overflow the Arduino command buffer
 // therefore block the enabling to start a new action for a second
 MainForm.StopTimer.Enabled:= True;
 MainForm.RunBB.Enabled:= False;
 MainForm.GenerateCommandBB.Enabled:= True;
 MainForm.ChangeSensorDataFileMI.Enabled:= True;
 // stop all timers and reset captions
 for j:= 1 to StepNum do
 begin
  (MainForm.FindComponent('StepTimer' + IntToStr(j))
   as TTimer).Enabled:= False;
  (MainForm.FindComponent('Step' + IntToStr(j) + 'TS')
   as TTabSheet).Caption:= 'Step ' + IntToStr(j);
 end;
 // enable all setting possibilities only if no file is loaded
 if (MainForm.LoadedActionFileM.Text = 'None')
  or (MainForm.LoadedActionFileM.Text = 'Free Pumps') then
 begin
  MainForm.LiveModeCB.Enabled:= True;
  MainForm.RunSettingsGB.Enabled:= not MainForm.LiveModeCB.Checked;
  MainForm.PumpSetupGB.Enabled:= True;
  MainForm.ValveSetupGB.Enabled:= True;
  MainForm.LinearityTestGB.Enabled:= True;
  for j:= 1 to StepNum do
  begin
   (MainForm.FindComponent('Step' + IntToStr(j) + 'UseCB')
    as TCheckBox).Enabled:= True;
   // don't enable action time when run endless and only one step
   if not (MainForm.RunEndlessCB.Checked) or MainForm.Step2UseCB.Checked then
    (MainForm.FindComponent('ActionTime' + IntToStr(j) + 'GB')
     as TGroupBox).Enabled:= True;
   (MainForm.FindComponent('DutyCycle' + IntToStr(j) + 'GB')
    as TGroupBox).Enabled:= True;
   (MainForm.FindComponent('S' + IntToStr(j) + 'P14')
    as TTabSheet).Enabled:= True;
   (MainForm.FindComponent('S' + IntToStr(j) + 'P58')
    as TTabSheet).Enabled:= True;
   (MainForm.FindComponent('S' + IntToStr(j) + 'Valves')
     as TTabSheet).Enabled:= True;
   if j = 1 then
    // enable tooltips for pump name
    for i:= 1 to PumpNum do
     (MainForm.FindComponent('Pump' + IntToStr(i) + 'GB' + IntToStr(j))
      as TGroupBox).ShowHint:= True;
  end;
  MainForm.CalibStepCB.Enabled:= True;
  MainForm.CalibStepL.Enabled:= True;
  MainForm.UseCalibCB.Enabled:= True;
  MainForm.CalibEveryXStepsSE.Enabled:= True;
  MainForm.CalibEveryXStepsL1.Enabled:= True;
  MainForm.CalibEveryXStepsL2.Enabled:= True;
  MainForm.UsedCalibValueSE.Enabled:= True;
  MainForm.UsedCalibValueL.Enabled:= True;
  for j:= 1 to MainForm.CalibSubstancesPC.PageCount do
  begin
   // the user must be able to see the settings for all substances
   // therefore we cannot just disable the CalibSubstancesPC component but its
   // child components except of XTS
   Substance:= MainForm.CalibSubstancesPC.Pages[j-1].Caption;
   (MainForm.FindComponent(Substance + 'AvailChanL')
    as TLabel).Enabled:= True;
   (MainForm.FindComponent(Substance + 'CalibGB')
    as TGroupBox).Enabled:= True;
   (MainForm.FindComponent(Substance + 'CalibCLB')
    as TChartListbox).Enabled:= True;
  end;
  // view tab after last used step
  for j:= 2 to StepNum-1 do
  begin
   if (MainForm.FindComponent('Step' + IntToStr(j) + 'UseCB')
       as TCheckBox).Checked then
    (MainForm.FindComponent('Step' + IntToStr(j+1) + 'TS')
     as TTabSheet).TabVisible:= True
   else
    break;
  end;
  // tab 2 must always be visible except when in live mode
  if not MainForm.LiveModeCB.Checked then
   MainForm.Step2TS.TabVisible:= True;
 end;
 // after a Free Pums run we must reset the LoadedActionFileM
 if MainForm.LoadedActionFileM.Text = 'Free Pumps' then
 begin
  MainForm.LoadedActionFileM.Text:= 'None';
  MainForm.LoadedActionFileM.Color:= clDefault;
  MainForm.LoadedActionFileM.Hint:= 'No action file loaded';
 end;
end;

procedure TPumpControl.PCSendRepeatToPump;
// sends command to pump driver
var
 errorMsg, savePumpName : string;
 MousePointer : TPoint;
begin
 if MainForm.HavePumpSerialCB.Checked then
 begin
  serPump.SendString(commandForRepeat);
  if serPump.LastError <> 0 then
  begin
   // store message and COMPort because CloseSerialConn will empty them
   errorMsg:= serPump.LastErrorDesc;
   savePumpName:= connectedPumpName;
   MainForm.ConnComPortPumpLE.Color:= clRed;
   MainForm.ConnComPortPumpLE.Text:= 'Try to reconnect';
   MainForm.IndicatorPumpP.Caption:= 'Connection failiure';
   MainForm.IndicatorPumpP.Color:= clRed;
   MainForm.IndicatorPumpPPaint;
   MainForm.PumpDriverMI.Enabled:= True;
   MainForm.DriverConnectBB.Enabled:= True;
   MainForm.RunBB.Enabled:= False;
   MainForm.ClosePumpSerialConn;
   MousePointer:= Mouse.CursorPos;
   // since the dialog will interrupt the code execution
   // it must be after ClosePumpSerialConn
   // Don't show this message if InfoNote is shown because it would hide
   // the note and that note is important
   if MainForm.InfoNote.Color = clInfoBk then // is shown color would be clRed
    MessageDlgPos(savePumpName + ' error: ' + errorMsg, mtError,
                  [mbOK], 0, MousePointer.X, MousePointer.Y);
   exit;
  end;
 end
 else // no serial connection
 begin
  if not MainForm.HasNoPumpsCB.Checked then
   MainForm.RunBB.Enabled:= False;
  exit;
 end;
end;

procedure TPumpControl.PCRepeatTimerFinished;
// Actions after repeat time interval ends
begin
 // if one day has passed but the pumps must run longer
 if GlobalRepeatTime > oneDay then
 begin
  GlobalRepeatTime:= GlobalRepeatTime - oneDay;
  if GlobalRepeatTime < oneDay then
   MainForm.RepeatTimer.Interval:= trunc(GlobalRepeatTime)
  else // to restart timer every day
   MainForm.RepeatTimer.Interval:= oneDay;
  MainForm.RepeatTimer.Enabled:= True;
  exit;
 end;

 inc(CurrentRepeat);

 if StrToInt(MainForm.RepeatSE.Text) > StrToInt(MainForm.RepeatOutputLE.Text) then
 begin
  GlobalRepeatTime:= RepeatTime;
  // restart timer
  MainForm.RepeatTimer.Enabled:= True;
  // only increase shown repeat if not already stopped
  if MainForm.IndicatorPumpP.Caption <> 'Manually stopped' then
   MainForm.RepeatOutputLE.Text:= IntToStr(CurrentRepeat);
 end
 else
  MainForm.RepeatTimer.Enabled:= False;

end;

procedure TPumpControl.PCOverallTimerStartTimer(Sender: TObject);
begin
 MainForm.RunBB.Caption:= 'Action running';
 MainForm.RunBB.Enabled:= False;
 MainForm.GenerateCommandBB.Enabled:= False;
 MainForm.IndicatorPumpP.Caption:= 'Action is running';
 MainForm.IndicatorPumpP.Color:= clLime;
 MainForm.IndicatorPumpPPaint;
 MainForm.GenerateCommandBB.Enabled:= False;
 // disable menu to load and save action files
 MainForm.LoadActionMI.Enabled:= False;
 MainForm.SaveActionMI.Enabled:= False;
 // disable all setting possibilities
 MainForm.RunSettingsGB.Enabled:= False;
 MainForm.LiveModeCB.Enabled:= False;
 MainForm.PumpSetupGB.Enabled:= False;
 MainForm.ValveSetupGB.Enabled:= False;
 // disable the connection menu that the user cannot close
 // the conenction while the pumps are running
 MainForm.PumpDriverMI.Enabled:= False;
 MainForm.DriverConnectBB.Enabled:= False;
 MainForm.FirmwareUpdateMI.Enabled:= False;
 MainForm.FirmwareResetMI.Enabled:= False;
end;

procedure TPumpControl.PCOverallTimerFinished;
// actions after time interval ends
var
 finishTime, SubstanceName, command : string;
 i, j : integer;
 Subst: Substance;
begin
 // if one day has passed but the pumps must run longer
 if GlobalTime > oneDay then
 begin
  GlobalTime:= GlobalTime - oneDay;
  if GlobalTime < oneDay then
   MainForm.OverallTimer.Interval:= trunc(GlobalTime)
  else // to restart timer every day
   MainForm.OverallTimer.Interval:= oneDay;
  MainForm.OverallTimer.Enabled:= True;
  exit;
 end;
 // if there were no repeats and calibration at the last step,
 // we must calibrate here
 i:= MainForm.RepeatPC.ActivePageIndex;
 if MainForm.UseCalibCB.Checked and (MainForm.CalibStepCB.ItemIndex = i) then
 begin
  for Subst in Substance do
   SIXControl.SCPerformAutoCalib(Subst);
 end;
 // output finish time
 finishTime := FormatDateTime('dd.mm.yyyy, hh:nn:ss', now);
 MainForm.FinishTimePumpLE.Text:= finishTime;
 MainForm.OverallTimer.Enabled:= False;
 MainForm.PumpDriverMI.Enabled:= True;
 MainForm.DriverConnectBB.Enabled:= True;
 MainForm.FirmwareUpdateMI.Enabled:= True;
 MainForm.FirmwareResetMI.Enabled:= True;
 MainForm.LoadActionMI.Enabled:= True;
 MainForm.SaveActionMI.Enabled:= True;
 MainForm.LoadOtherDefBB.enabled:= True;
 MainForm.UnloadDefBB.enabled:= True;
 MainForm.ChangeSensorDataFileMI.Enabled:= True;
 MainForm.RunBB.Caption:= 'Run Action';
 if not MainForm.HasNoPumpsCB.Checked then
  MainForm.RunBB.Enabled:= MainForm.HavePumpSerialCB.Checked
 else
  MainForm.RunBB.Enabled:= true;
 MainForm.GenerateCommandBB.Enabled:= True;
 MainForm.IndicatorPumpP.Caption:= 'Run finished';
 MainForm.IndicatorPumpP.Color:= clInfoBk;
 MainForm.IndicatorPumpPPaint;
 MainForm.RepeatOutputLE.Visible:= False;

 // stop all pumps and valves
 command:= '/0';
 // disable all valves
 if ValveNum > 0 then
 begin
  command:= command + 'V';
  for j:= 1 to ValveNum do
   command:= command + '0';
 end;
 // disable all pumps
 command:= command + 'I';
 for j:= 1 to PumpNum do
  command:= command + '0';
 // execute flag and turn off LED
 command:= command + 'lR';
 // execute
 command:= command + LineEnding;
 if MainForm.HavePumpSerialCB.Checked then
  serPump.SendString(command);

 // stop all timers and reset captions
 for j:= 1 to StepNum do
 begin
  (MainForm.FindComponent('StepTimer' + IntToStr(j))
   as TTimer).Enabled:= False;
  (MainForm.FindComponent('Step' + IntToStr(j) + 'TS')
   as TTabSheet).Caption:= 'Step ' + IntToStr(j);
 end;
 // enable all setting possibilities only if no file is loaded
 if (MainForm.LoadedActionFileM.Text = 'None')
  or (MainForm.LoadedActionFileM.Text = 'Free Pumps') then
 begin
  MainForm.LiveModeCB.Enabled:= True;
  MainForm.RunSettingsGB.Enabled:= not MainForm.LiveModeCB.Checked;
  MainForm.PumpSetupGB.Enabled:= True;
  MainForm.ValveSetupGB.Enabled:= True;
  MainForm.CalibrationGB.Enabled:= not MainForm.LiveModeCB.Checked;
  MainForm.LinearityTestGB.Enabled:= True;
  for j:= 1 to StepNum do
  begin
   (MainForm.FindComponent('Step' + IntToStr(j) + 'UseCB')
    as TCheckBox).Enabled:= True;
   (MainForm.FindComponent('ActionTime' + IntToStr(j) + 'GB')
    as TGroupBox).Enabled:= True;
   (MainForm.FindComponent('DutyCycle' + IntToStr(j) + 'GB')
    as TGroupBox).Enabled:= True;
   (MainForm.FindComponent('S' + IntToStr(j) + 'P14')
    as TTabSheet).Enabled:= True;
   (MainForm.FindComponent('S' + IntToStr(j) + 'P58')
    as TTabSheet).Enabled:= True;
   (MainForm.FindComponent('S' + IntToStr(j) + 'Valves')
     as TTabSheet).Enabled:= True;
   if j = 1 then
    // enable tooltips for pump name
    for i:= 1 to PumpNum do
    (MainForm.FindComponent('Pump' + IntToStr(i) + 'GB' + IntToStr(j))
     as TGroupBox).ShowHint:= True;
  end;
  MainForm.CalibStepCB.Enabled:= True;
  MainForm.CalibStepL.Enabled:= True;
  MainForm.UseCalibCB.Enabled:= True;
  MainForm.CalibEveryXStepsSE.Enabled:= True;
  MainForm.CalibEveryXStepsL1.Enabled:= True;
  MainForm.CalibEveryXStepsL2.Enabled:= True;
  MainForm.UsedCalibValueSE.Enabled:= True;
  MainForm.UsedCalibValueL.Enabled:= True;
  for j:= 1 to MainForm.CalibSubstancesPC.PageCount do
  begin
   // the user must be able to see the settings for all substances
   // therefore we cannot just disable the CalibSubstancesPC component but its
   // child components except of XTS
   SubstanceName:= MainForm.CalibSubstancesPC.Pages[j-1].Caption;
   (MainForm.FindComponent(SubstanceName + 'AvailChanL')
    as TLabel).Enabled:= True;
   (MainForm.FindComponent(SubstanceName + 'CalibGB')
    as TGroupBox).Enabled:= True;
   (MainForm.FindComponent(SubstanceName + 'CalibCLB')
    as TChartListbox).Enabled:= True;
  end;
  // view tab after last used step
  for j:= 2 to StepNum-1 do
  begin
   if (MainForm.FindComponent('Step' + IntToStr(j) + 'UseCB')
       as TCheckBox).Checked then
    (MainForm.FindComponent('Step' + IntToStr(j+1) + 'TS')
     as TTabSheet).TabVisible:= True
  else
   break;
  end;
  // tab 2 must always be visible except when in live mode
  if not MainForm.LiveModeCB.Checked then
   MainForm.Step2TS.TabVisible:= True;
 end;
 // after a Free Pums run we must reset the LoadedActionFileM
 if MainForm.LoadedActionFileM.Text = 'Free Pumps' then
 begin
  MainForm.LoadedActionFileM.Text:= 'None';
  MainForm.LoadedActionFileM.Color:= clDefault;
  MainForm.LoadedActionFileM.Hint:= 'No action file loaded';
 end;
end;

procedure TPumpControl.PCPumpGBDblClick(Sender: TObject);
var
 j, Pump : integer;
 SenderName : string;
begin
 SenderName:= (Sender as TComponent).Name;
 // SenderName is in the form "PumpxGBy" and we need the x
 // so get the 5th character of the name
 Pump:= StrToInt(Copy(SenderName, 5, 1));
 // show in dialog always the name from the first step
 NameSettingF.NameE.Text:=
   (MainForm.FindComponent('Pump' + IntToStr(Pump) + 'GB1') as TGroupBox).Caption;
 // open connection dialog
 NameSettingF.Caption:= 'Pump name selection';
 NameSettingF.NameL.Caption:= 'Pump name:';
 NameSettingF.ShowModal;
 if NameSettingF.ModalResult = mrCancel then
  exit
 else
  for j:= 1 to StepNum do
   (MainForm.FindComponent('Pump' + IntToStr(Pump) + 'GB' + IntToStr(j))
    as TGroupBox).Caption:= NameSettingF.NameE.Text;
end;

procedure TPumpControl.PCValveRGDblClick(Sender: TObject);
var
 j, Valve : integer;
 SenderName : string;
begin
 SenderName:= (Sender as TComponent).Name;
 // SenderName is in the form "ValvexRGy" and we need the x
 // so get the 6th character of the name
 Valve:= StrToInt(Copy(SenderName, 6, 1));
 // show in dialog always the name from the first step
 NameSettingF.NameE.Text:=
   (MainForm.FindComponent('Valve' + IntToStr(Valve) + 'RG1')
    as TRadioGroup).Caption;
 // open connection dialog
 NameSettingF.Caption:= 'Valve name selection';
 NameSettingF.NameL.Caption:= 'Valve name:';
 NameSettingF.ShowModal;
 if NameSettingF.ModalResult = mrCancel then
  exit
 else
  for j:= 1 to StepNum do
   (MainForm.FindComponent('Valve' + IntToStr(Valve) + 'RG' + IntToStr(j))
    as TRadioGroup).Caption:= NameSettingF.NameE.Text;
end;

procedure TPumpControl.PCHasNoPumpsCBChange(Sender: TObject);
begin
 MainForm.PumpNumberSE.Enabled:= (not MainForm.HasNoPumpsCB.Checked);
 if MainForm.HasNoPumpsCB.Checked then
 begin
  MainForm.PumpNumberSE.Value:= 0; // this will handle everything else
  // action can then be run without connection to a pump driver
  MainForm.RunBB.Enabled:= true;
 end
 else
 begin
  MainForm.PumpNumberSE.Value:= 1;
  if not MainForm.HavePumpSerialCB.Checked then
   MainForm.RunBB.Enabled:= false;
 end;
end;

procedure TPumpControl.PCHasNoValvesCBChange(Sender: TObject);
var
 j : integer;
begin
 // show/hide the valves tab
 for j:= 1 to StepNum do
  (MainForm.FindComponent('S' + IntToStr(j) + 'Valves')
   as TTabSheet).TabVisible:= (not MainForm.HasNoValvesCB.Checked);
 MainForm.ValveNumberSE.Enabled:= (not MainForm.HasNoValvesCB.Checked);
 if MainForm.HasNoValvesCB.Checked then
  MainForm.ValveNumberSE.Value:= 0
 else
  MainForm.ValveNumberSE.Value:= 1;
end;

procedure TPumpControl.PCUseCalibCBChange(Sender: TObject);
var
 AllowCalibEveryXSteps : Boolean = false;
begin
 MainForm.CalibrationGB.Visible:= MainForm.UseCalibCB.Checked;
 MainForm.GlucoseTS.Enabled:= MainForm.UseCalibCB.Checked;
 MainForm.LactateTS.Enabled:= MainForm.UseCalibCB.Checked;
 MainForm.CalibStepCB.Enabled:= MainForm.UseCalibCB.Checked;
 MainForm.CalibStepL.Enabled:= MainForm.UseCalibCB.Checked;
 MainForm.UsedCalibValueSE.Enabled:= MainForm.UseCalibCB.Checked;
 MainForm.UsedCalibValueL.Enabled:= MainForm.UseCalibCB.Checked;

 // calibration every X steps can only be set when there is more than one step
 if MainForm.UseCalibCB.Checked
  and (MainForm.RunEndlessCB.Checked or (MainForm.RepeatSE.Value > 0)) then
  AllowCalibEveryXSteps:= true;
 MainForm.CalibEveryXStepsSE.Enabled:= AllowCalibEveryXSteps;
 MainForm.CalibEveryXStepsL1.Enabled:= AllowCalibEveryXSteps;
 MainForm.CalibEveryXStepsL2.Enabled:= AllowCalibEveryXSteps;

 // en/disable Run button
 if not MainForm.HavePumpSerialCB.Checked then
 begin
  if not MainForm.HasNoPumpsCB.Checked then
  begin
   MainForm.RunBB.Enabled:= false;
   if MainForm.RunBB.Hint = 'Calibration is used but no sensor definition file is loaded' then
    MainForm.RunBB.Hint:= 'Starts the pump action according to the current settings.'
     + LineEnding
     + 'If button is disabled you must first start a SIX measurement.';
  end;
 end
 else
 begin
  if MainForm.UseCalibCB.Checked then
  begin
   if MainForm.HaveDefFileCB.Checked then
    MainForm.RunBB.Enabled:= true
   else
   begin
    MainForm.RunBB.Enabled:= false;
    MainForm.RunBB.Hint:= 'Calibration is used but no sensor definition file is loaded';
   end;
  end
  else
  begin
   MainForm.RunBB.Enabled:= true;
   MainForm.RunBB.Hint:= 'Starts the pump action according to the current settings.'
   + LineEnding
   + 'To enable the button you must first connect to the pump driver'
   + LineEnding + 'using the menu ''Connection''';
  end;
 end;

end;

procedure TPumpControl.PCPumpNumberSEChange(Sender: TObject);
var
 j, k, l : integer;
begin
 PumpNum:= MainForm.PumpNumberSE.Value;
 if PumpNum = 0 then // also set checkbox that there are no pumps
 begin
  if not MainForm.HasNoPumpsCB.Checked then
   MainForm.HasNoPumpsCB.Checked:= true;
 end
 else
 begin
  if MainForm.HasNoPumpsCB.Checked then
   MainForm.HasNoPumpsCB.Checked:= false;
 end;
 for j:= 1 to StepNum do
 begin
  k:= 0;
  if PumpNum > 0 then
   // enable the specified number of pumps if the step is used
   for k:= 1 to PumpNum do
   begin
    (MainForm.FindComponent('Pump' + IntToStr(k) + 'GB' + IntToStr(j))
     as TGroupBox).Enabled:= (MainForm.FindComponent('Step'
                              + IntToStr(j) + 'UseCB') as TCheckBox).checked;
    (MainForm.FindComponent('Pump' + IntToStr(k) + 'GB' + IntToStr(j))
     as TGroupBox).ShowHint:= true;
   end;
  // disable non-existent pumps
  if k < 8 then
  begin
   for l:= PumpNum + 1 to 8 do // currently we only support 8 pumps
   begin
    (MainForm.FindComponent('Pump' + IntToStr(l) + 'OnOffCB' + IntToStr(j))
     as TCheckBox).Checked:= false;
    (MainForm.FindComponent('Pump' + IntToStr(l) + 'GB' + IntToStr(j))
     as TGroupBox).Enabled:= false;
    (MainForm.FindComponent('Pump' + IntToStr(l) + 'GB' + IntToStr(j))
     as TGroupBox).ShowHint:= false;
   end;
  end;
  // show/hide the pumps 5-8 tab
  if k < 5 then
   (MainForm.FindComponent('S' + IntToStr(j) + 'P58')
    as TTabSheet).TabVisible:= false
  else
   (MainForm.FindComponent('S' + IntToStr(j) + 'P58')
    as TTabSheet).TabVisible:= true;
  // show/hide the pumps 1-4 tab
  if k < 1 then
   (MainForm.FindComponent('S' + IntToStr(j) + 'P14')
    as TTabSheet).TabVisible:= false
  else
   (MainForm.FindComponent('S' + IntToStr(j) + 'P14')
    as TTabSheet).TabVisible:= true;
 end;
end;

procedure TPumpControl.PCValveNumberSEChange(Sender: TObject);
var
 j, k : integer;
begin
 ValveNum:= MainForm.ValveNumberSE.Value;
 if ValveNum = 0 then // also set checkbox that there are no valves
 begin
  MainForm.HasNoValvesCB.Checked:= true;
  exit;
 end
 else
  MainForm.HasNoValvesCB.Checked:= false;
 for j:= 1 to StepNum do
 begin
  k:= 1;
  // enable the specified number of valves if the step is used
  for k:= 1 to ValveNum do
  begin
   (MainForm.FindComponent('Valve' + IntToStr(k) + 'RG' + IntToStr(j))
    as TRadioGroup).Enabled:= (MainForm.FindComponent('Step'
                                + IntToStr(j) + 'UseCB') as TCheckBox).checked;
   (MainForm.FindComponent('Valve' + IntToStr(k) + 'RG' + IntToStr(j))
    as TRadioGroup).ShowHint:= true;
  end;
  // disable non-existent valves
  if k < 8 then
  begin
   for k:= ValveNum + 1 to 8 do // currently we only support 8 valves
   begin
    (MainForm.FindComponent('Valve' + IntToStr(k) + 'RG' + IntToStr(j))
     as TRadioGroup).ItemIndex:= 0;
    (MainForm.FindComponent('Valve' + IntToStr(k) + 'RG' + IntToStr(j))
     as TRadioGroup).Enabled:= false;
    (MainForm.FindComponent('Valve' + IntToStr(k) + 'RG' + IntToStr(j))
     as TRadioGroup).ShowHint:= false;
   end;
  end;
  // when there are no pumps, we must activate the valve tab
  if MainForm.HasNoPumpsCB.Checked then
   (MainForm.FindComponent('S' + IntToStr(j) + 'PC')
    as TPageControl).ActivePage:=
     (MainForm.FindComponent('S' + IntToStr(j) + 'Valves')
      as TTabSheet);
 end;
end;

procedure TPumpControl.PCHavePumpSerialCBChange(Sender: TObject);
begin
 if MainForm.HavePumpSerialCB.Checked then
 begin
  MainForm.DriverConnectBB.Caption:= 'Disconnect Driver';
  MainForm.DriverConnectBB.Hint:= 'Disconnects from the pump driver';
 end
 else
 begin
  MainForm.DriverConnectBB.Caption:= 'Connect Driver';
  MainForm.DriverConnectBB.Hint:= 'Connects to a the pump driver';
  MainForm.ConnComPortPumpLE.Color:= clHighlight;
  MainForm.ConnComPortPumpLE.Text:= 'Not connected';
 end;
end;

procedure TPumpControl.AnOutPumpXGBDblClick(Sender: TObject);
var
 Pump : integer;
 SenderName : string;
begin
 SenderName:= (Sender as TComponent).Name;
 // SenderName is in the form "AnOutPumpxGB" and we need the x
 // so get the 10th character of the name
 Pump:= StrToInt(Copy(SenderName, 10, 1));
 // show in dialog always the name from the first step
 NameSettingF.NameE.Text:=
   (MainForm.FindComponent('AnOutPump' + IntToStr(Pump) + 'GB') as TGroupBox).Caption;
 // open connection dialog
 NameSettingF.Caption:= 'Connector name selection';
 NameSettingF.NameL.Caption:= 'Connector name:';
 NameSettingF.ShowModal;
 if NameSettingF.ModalResult = mrCancel then
  exit
 else
  (MainForm.FindComponent('AnOutPump' + IntToStr(Pump) + 'GB')
   as TGroupBox).Caption:= NameSettingF.NameE.Text;
end;

procedure TPumpControl.PCRunEndlessCBChange(Sender: TObject);
// if the pumps should run forever
begin
  if MainForm.RunEndlessCB.Checked then
  begin
   MainForm.RepeatSE.Enabled:= False;
   MainForm.RepeatOutputLE.Visible:= False;
   // disable runtime only, if there is only one step or in live mode
   if (not MainForm.Step2UseCB.Checked) or MainForm.LiveModeCB.Checked then
    MainForm.ActionTime1GB.Enabled:= False;
   if MainForm.UseCalibCB.Checked then
   begin
    MainForm.CalibEveryXStepsSE.Enabled:= True;
    MainForm.CalibEveryXStepsL1.Enabled:= True;
    MainForm.CalibEveryXStepsL2.Enabled:= True;
   end;
  end
  else
  begin
   MainForm.RepeatSE.Enabled:= True;
   if not MainForm.Step2UseCB.Checked then
    MainForm.ActionTime1GB.Enabled:= True;
   // when there would not be any repeat, disable to calibrate every x steps
   if MainForm.RepeatSE.Value = 0 then
   begin
    MainForm.CalibEveryXStepsSE.Enabled:= False;
    MainForm.CalibEveryXStepsL1.Enabled:= False;
    MainForm.CalibEveryXStepsL2.Enabled:= False;
   end;
  end;
end;

// procedure to assure there are valid calibration settings
procedure TPumpControl.PCCalibValueFSEChange(Sender: TObject);
var
 i : integer;
 found : Boolean = false;
 SenderName, SubstanceUIName : string;
begin
 SenderName:= (Sender as TComponent).Name;
 // SenderName is in the form "GlucoseCalibValueFSE" and we need the first part
 SubstanceUIName:= Copy(SenderName, 0, Length(SenderName) - 8);
 // check that there is at least one selected series to calibrate
 // when the value is not zero
 if (MainForm.FindComponent(SenderName) as TFloatSpinEdit).Value > 0.0 then
 begin
  for i:= 0 to (MainForm.FindComponent(SubstanceUIName + 'CLB')
               as TChartListbox).SeriesCount-1 do
  begin
   if (MainForm.FindComponent(SubstanceUIName + 'CLB')
       as TChartListbox).Selected[i] then
    found:= true;
  end;
 end
 else
  // if value is zero, we don't recalibrate, thus can always allow to run
  found:= true;
 MainForm.RunBB.Enabled:= (found and
  (MainForm.HavePumpSerialCB.Checked or MainForm.HasNoPumpsCB.Checked));
end;

procedure TPumpControl.PCRepeatSEChange(Sender: TObject);
// set possible CalibEveryXStepsSE according to the number of repeats
begin
 if not MainForm.UseCalibCB.Checked then
  exit;

 if MainForm.RunEndlessCB.Checked then
 begin
  MainForm.CalibEveryXStepsSE.MaxValue:= 99;
  MainForm.CalibEveryXStepsSE.Enabled:= True;
 end
 else
 begin
  if MainForm.RepeatSE.Value = 0 then
  begin
   MainForm.CalibEveryXStepsSE.Value:= 1;
   MainForm.CalibEveryXStepsSE.Enabled:= False;
   MainForm.CalibEveryXStepsL1.Enabled:= False;
   MainForm.CalibEveryXStepsL2.Enabled:= False;
  end
  else
  begin
   MainForm.CalibEveryXStepsSE.MaxValue:= MainForm.RepeatSE.Value + 1;
   MainForm.CalibEveryXStepsSE.Enabled:= True;
   MainForm.CalibEveryXStepsL1.Enabled:= True;
   MainForm.CalibEveryXStepsL2.Enabled:= True;
  end;
 end;
end;

end.

