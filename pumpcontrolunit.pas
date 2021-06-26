unit PumpControlUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus, Math,
  StdCtrls, ExtCtrls, Spin, Buttons, LCLType,
  SynaSer, LazSerial, Crt, Character, System.UITypes, Types,
  // custom forms
  JTDriverSensingMain, NameSetting, SerialUSBSelection;

type

  TPumpControl = class
    procedure PCDutyCycleXFSEChange(Sender: TObject);
    procedure PCStepTimerXFinished(Sender: TObject);
    procedure PCGenerateCommandBBClick(Sender: TObject);
    procedure PCRunFreeBBClick(Sender: TObject);
    procedure PCRunBBClick(Sender: TObject);
    procedure PCStopBBClick(Sender: TObject);
    procedure PCLiveModeCBChange(Sender: TObject);
    procedure PCPumpOnOffCBLoopChange(Sender: TObject);
    procedure PCRepeatTimerFinished;
    procedure PCOverallTimerFinished;
    procedure PCStepXUseCBChange(Sender: TObject);
    procedure PCRepeatPCChange(Sender: TObject);
    procedure PCPumpGBDblClick(Sender: TObject);
    procedure AnOutPumpXGBDblClick(Sender: TObject);
    procedure PCRunEndlessCBChange(Sender: TObject);

  private

  public
    function GenerateCommand(out command: string): Boolean;
    function ParseCommand(command: string): Boolean;
    procedure RunImmediate;

    class var
     CurrentRepeat : integer;
     GlobalTime : Double;
     GlobalRepeatTime : Double;
     RepeatTime : Double;
     StepNum : integer; // number of steps
     PumpNum : integer; // number of pumps

  end;

var
  PumpControl: TPumpControl;

implementation

procedure TPumpControl.PCGenerateCommandBBClick(Sender: TObject);

// call function to collect data an generate command
var
 command : string;
 i, j : integer;
begin
 GenerateCommand(command);
 MainForm.CommandM.Text:= command;
 // the button re-enables editing after an action file was loaded
 // enable all setting possibilities
 MainForm.LiveModeCB.Enabled:= True;
 MainForm.RunSettingsGB.Enabled:= not MainForm.LiveModeCB.Checked;
 // check all possible steps
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
  // enable tooltips for pump name
  for i:= 1 to PumpNum do
   (MainForm.FindComponent('Pump' + IntToStr(i) + 'GB' + IntToStr(j))
    as TGroupBox).ShowHint:= True;
 end;
 // view tab after last used step
 for j:= 2 to StepNum-1 do
 begin
  if (MainForm.FindComponent('Step' + IntToStr(j) + 'UseCB')
      as TCheckBox).Checked = True then
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
   posSfirst, posSlast: integer;
 MousePointer : TPoint;
 StepTime, M1, M2, DutyStepTime : Double;
 Have2M : Boolean;
begin
 MousePointer:= Mouse.CursorPos; // store mouse position
 StepCounter:= 0; MCounter:= 0; ICounter:= 0;
 M1:= 0; M2:= 0; G1:= 0;
 result:= false; Have2M:= false; StepTime:= 0;
 SOrder:= nil;
 setLength(SOrder, PumpNum);
 for k:= 0 to PumpNum-1 do
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
   for k:= 0 to PumpNum-1 do
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
    for p:= 1 to PumpNum do
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
    if (command[i+PumpNum+1] = 'M') then
    begin
     // determine the length
     j:= i + PumpNum + 1;
     repeat
      inc(j)
     until IsDigit(command[j]) = false;
     StepTime:= StrToFloat(Copy(command, i+PumpNum+2, j-i-(PumpNum+2))) / 1000;
     if (StepTime >= 1) then
     begin
      inc(StepCounter);
      ICounter:= 0;
      MCounter:= 0;
     end;
    end;
   end;
   if (StepCounter = 0)
    or ((LastParsed = 'G') and (command[i+PumpNum+2] <> 'R')) then // not if last 'I'
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
    for p:= 2 to PumpNum do
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

function TPumpControl.GenerateCommand(out command: string): Boolean;
// collect data an generate command to be sent
var
 voltage, jStr, commandSplit, commandSave, commandOriginal : string;
 SOrder : array of char;
 timeFactor, DutyRepeats, XTime, OnTime, OffTime, i, j, k, j2, k2,
   voltageCalc, countPump, countPumpNumber, posS: integer;
 timeCalc, timeOut, timeStep : Double;
 HaveS : Boolean = False;
begin
 timeFactor:= 1; timeCalc:= 0; voltageCalc:= 0;
 command:= ''; commandSplit:= ''; voltage:= '';
 if not MainForm.LiveModeCB.Checked then
 begin
  MainForm.IndicatorPumpP.Color:= clDefault;
  MainForm.IndicatorPumpP.Caption:= '';
 end;
 MainForm.IndicatorPumpP.Hint:= '';
 SOrder:= nil;
 setLength(SOrder, PumpNum);
 posS:= 1; // 1 and not 0 because we use it to access chars in strings

 // address
 command:= '/0';
 // turn on LED
 command:= command + 'L';

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
   // action
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
    MainForm.IndicatorPumpP.Caption:= 'Action Time ' + jStr + 'too long!';
    MainForm.IndicatorPumpP.Hint:= 'The time for one action must not exceed 596 h.';
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
  command:= command + 'G';
  timeCalc:= MAXINT; // set maximal possible int for infinite repeats
 end;
 // if repeated run
 if (StrToInt(MainForm.RepeatSE.Text) > 0) and (not MainForm.RunEndlessCB.Checked) then
 begin
  command:= command + 'G' + IntToStr(MainForm.RepeatSE.Value);
  timeCalc:= timeCalc * (MainForm.RepeatSE.Value + 1);
 end;

  // explicitly turn off all pumps, turn off LED and execute flag
  // the explicite turn off is important because the Arduino command
  // execution loop needs several 100 ms. But when an explicit stop is sent,
  // the loop will end immediately
  command:= command + 'I';
  for k:= 1 to PumpNum do
   command:= command + '0';
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
    timeOut:= timeCalc /1000/60;
    MainForm.TotalTimeLE.Text:= FloatToStr(RoundTo(timeOut, -2));
    MainForm.TotalTimeLE.EditLabel.Caption:= 'Total Time in min';
   end
   else if timeCalc > 60e6 then
   begin
    timeOut:= timeCalc /1000/60/60;
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
 CommandResult:= PumpControl.GenerateCommand(command);
 // if GenerateCommand returns e.g. a too long time stop
 if not CommandResult then
 begin
  MainForm.StopBBClick(MainForm.StopBB);
  exit;
 end;
 MainForm.CommandM.Text:= command;
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
 if HaveSerialPump then
 begin
  // disable the connection menu that the user cannot close
  // the conenction while the pumps are running
  MainForm.PumpDriverMI.Enabled:= False;
  MainForm.FirmwareUpdateMI.Enabled:= False;
  MainForm.FirmwareResetMI.Enabled:= False;
  // send the command
  serPump.SendString(command);
  if serPump.LastError <> 0 then
  begin
   with Application do
    MessageBox(PChar(COMPort + ' error: ' + serPump.LastErrorDesc), 'Error', MB_ICONERROR+MB_OK);
   MainForm.ConnComPortPumpLE.Color:= clRed;
   MainForm.ConnComPortPumpLE.Text:= 'Try to reconnect';
   MainForm.IndicatorPumpP.Caption:= 'Connection failiure';
   MainForm.PumpDriverMI.Enabled:= True;
   MainForm.RunBB.Enabled:= False;
   MainForm.RunFreeBB.Enabled:= False;
   if serPump.LastError = 9997 then
   begin
    MainForm.StopBB.Enabled:= False;
    exit; // we cannot close socket or free if the connection timed out
   end;
   serPump.CloseSocket;
   serPump.Free;
   HaveSerialPump:= False;
   exit;
  end;
 end
 else // no serial connection
 begin
  MainForm.RunBB.Enabled:= False;
  MainForm.RunFreeBB.Enabled:= False;
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
  MainForm.RunSettingsGB.Enabled:= false;
  MainForm.RunFreeBB.Enabled:= false;
  MainForm.Step1TS.Caption:= 'Live';
  // set that run until stop pressed
  MainForm.RunEndlessCB.Checked:= true;
  // en/disable pump setting elements
  PCPumpOnOffCBLoopChange(Sender);
 end
 else
 begin
  MainForm.RunSettingsGB.Enabled:= true;
  MainForm.RunFreeBB.Enabled:= true;
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
 // otherwise the voltage would be to low to start a short movement
 if ((MainForm.FindComponent('DutyCycle' + IntToStr(Step) + 'FSE')
        as TFloatSpinEdit).Value < 100) then
  for j:= 1 to PumpNum do
   (MainForm.FindComponent('Pump' + IntToStr(j) + 'VoltageFS' + IntToStr(Step))
      as TFloatSpinEdit).MinValue:= 1.1
 else
  for j:= 1 to PumpNum do
   (MainForm.FindComponent('Pump' + IntToStr(j) + 'VoltageFS' + IntToStr(Step))
      as TFloatSpinEdit).MinValue:= 0.1;
 // calculate necessary time increment
 if ((MainForm.FindComponent('DutyCycle' + IntToStr(Step) + 'FSE')
        as TFloatSpinEdit).Value / 100) >= 0.05 then
  DutyTime:= 1 // base time is 1s
 else // calculate a base time so that the OnTime is 50 ms
  DutyTime:= 0.05 / ((MainForm.FindComponent('DutyCycle' + IntToStr(Step) + 'FSE')
        as TFloatSpinEdit).Value / 100);
 // if the unit is s, we can also set a new increment
 if (MainForm.FindComponent('Unit' + IntToStr(Step) + 'RBs')
        as TRadioButton).Checked then
  (MainForm.FindComponent('RunTime' + IntToStr(Step) + 'FSE')
        as TFloatSpinEdit).Increment:= round(DutyTime);
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
 if (MainForm.FindComponent('Step' + IntToStr(Step) + 'UseCB') as TCheckBox).Checked then
 begin
  if Step <> StepNum then
   (MainForm.FindComponent('Step' + IntToStr(Step+1) + 'TS') as TTabSheet).TabVisible:= True;
  (MainForm.FindComponent('ActionTime' + IntToStr(Step) + 'GB') as TGroupBox).Enabled:= True;
  (MainForm.FindComponent('DutyCycle' + IntToStr(Step) + 'GB') as TGroupBox).Enabled:= True;
  for j:= 1 to PumpNum do
   (MainForm.FindComponent('Pump' + IntToStr(j) + 'GB' + IntToStr(Step)) as TGroupBox).Enabled:= True;
  // in case it was disabled on unchecking step 2
  if (Step = 2) and (not MainForm.ActionTime1GB.Enabled) then
   MainForm.ActionTime1GB.Enabled:= True;
 end
 else
 begin
  if Step <> StepNum then
   (MainForm.FindComponent('Step' + IntToStr(Step+1) + 'TS') as TTabSheet).TabVisible:= False;
  (MainForm.FindComponent('ActionTime' + IntToStr(Step) + 'GB') as TGroupBox).Enabled:= False;
  (MainForm.FindComponent('DutyCycle' + IntToStr(Step) + 'GB') as TGroupBox).Enabled:= False;
  for j:= 1 to PumpNum do
   (MainForm.FindComponent('Pump' + IntToStr(j) + 'GB' + IntToStr(Step)) as TGroupBox).Enabled:= False;
  // if there is only one step and endless repeat disable time settings
  if (Step = 2) and (MainForm.RunEndlessCB.Checked) then
   MainForm.ActionTime1GB.Enabled:= False;
 end;
 PCRepeatPCChange(Sender);
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
 Step : integer;
 SenderName : string;
begin
 SenderName:= (Sender as TComponent).Name;
 // SenderName is in the form "StepxTS" and we need the x
 // so get the 5th character of the name
 Step:= StrToInt(Copy(SenderName, 5, 1));
 (MainForm.FindComponent('StepTimer' + IntToStr(Step))
        as TTimer).Enabled:= False;
 // if there is a step+1, start its timer and show its tab
 if (MainForm.FindComponent('Step' + IntToStr(Step+1) + 'UseCB')
        as TCheckBox).checked then
 begin
  // the interval is calculated in TMainForm.GenerateCommand
  (MainForm.FindComponent('StepTimer' + IntToStr(Step+1))
        as TTimer).Enabled:= True;
  MainForm.RepeatPC.ActivePage:= (MainForm.FindComponent('StepTimer' + IntToStr(Step+1) + 'TS')
        as TTabSheet);
 end
 else // there might be a repeat
 begin
  // switch to step 1
  MainForm.StepTimer1.Enabled:= True;
  MainForm.RepeatPC.ActivePage:= MainForm.Step1TS;
 end;
end;

procedure TPumpControl.PCRunFreeBBClick(Sender: TObject);
// starts free running cycle:
// run 30 seconds in each direction 10 times
// this is like loading a *.PDAction file, therefore use the file load routines
var
 j : integer;
 command : string;
 ParseSuccess : Boolean;
begin
 MainForm.LoadedActionFileM.Text:= 'Free Pumps';
 MainForm.LoadedActionFileM.Color:= clInfoBK;
 MainForm.LoadedActionFileM.Hint:= 'Free Pumps';
 // start the pumps in blocks of 3 pumps at once
 // input the action as command
 // '/0LgS199929993999D000I11100000M10
 //      S199929993999499959996999D000000I11111100M10
 //      S19992999399949995999699979998999D00000000I11111111M30000
 //      I00000000M999
 //      S199929993999D111I11100000M10
 //      S199929993999499959996999D111111I11111100M10
 //      S19992999399949995999699979998999D11111111I11111111M30000
 //      I00000000M999G9I00000000lR
 command:= '/0Lg';
 command:= command + 'S199929993999D000I11100000M10';
 command:= command + 'S199929993999499959996999D000000I11111100M10';
 command:= command + 'S19992999399949995999699979998999D00000000I11111111M30000';
 command:= command + 'I00000000M999';
 command:= command + 'S199929993999D111I11100000M10';
 command:= command + 'S199929993999499959996999D111111I11111100M10';
 command:= command + 'S19992999399949995999699979998999D11111111I11111111M30000';
 command:= command + 'I00000000M999G9I00000000lR';

 MainForm.CommandM.Text:= command;
 // parse the command
 ParseSuccess:= ParseCommand(command);
 if ParseSuccess then
  // call command generation just to get the action time calculated
  GenerateCommand(command);
 // disable all setting possibilities
 MainForm.RunSettingsGB.Enabled:= False;
 MainForm.LiveModeCB.Enabled:= False;
 for j:= 1 to StepNum do
  (MainForm.FindComponent('Step' + IntToStr(j) + 'TS')
   as TTabSheet).Enabled:= False;
 MainForm.RepeatOutputLE.Visible:= False;
 // do not show unused steps
 for j:= 2 to StepNum do
 begin
  if (MainForm.FindComponent('Step' + IntToStr(j) + 'UseCB')
      as TCheckBox).Checked = False then
   (MainForm.FindComponent('Step' + IntToStr(j) + 'TS')
    as TTabSheet).TabVisible:= False;
 end;
 // disable saving, will be re-enabled by GererateCommand
 MainForm.SaveActionMI.Enabled:= False;
 // show step 1
 MainForm.RepeatPC.ActivePage:= MainForm.Step1TS;
 // run
 MainForm.RunBBClick(Sender);
end;

procedure TPumpControl.PCRunBBClick(Sender: TObject);
// execute generated command
var
  command, StartTime : string;
  i, j : integer;
  CommandResult : Boolean = False;
begin
  // generate command
  CommandResult:= PumpControl.GenerateCommand(command);
  // if GenerateCommand returns e.g. a too long time do nothing
  if not CommandResult then
   exit;
  MainForm.StopTimer.Enabled:= False;
  // whatever might happen, there must be a way to stop
  MainForm.StopBB.Enabled:= True;
  MainForm.CommandM.Text:= command;
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
  if HaveSerialPump then
  begin
   // disable the connection menu that the user cannot close
   // the conenction while the pumps are running
   MainForm.PumpDriverMI.Enabled:= False;
   MainForm.FirmwareUpdateMI.Enabled:= False;
   MainForm.FirmwareResetMI.Enabled:= False;
   // send the command
   serPump.SendString(command);
   if serPump.LastError <> 0 then
   begin
    with Application do
     MessageBox(PChar(COMPort + ' error: ' + serPump.LastErrorDesc), 'Error', MB_ICONERROR+MB_OK);
    MainForm.ConnComPortPumpLE.Color:= clRed;
    MainForm.ConnComPortPumpLE.Text:= 'Try to reconnect';
    MainForm.IndicatorPumpP.Caption:= 'Connection failiure';
    MainForm.PumpDriverMI.Enabled:= True;
    MainForm.RunBB.Enabled:= False;
    MainForm.RunFreeBB.Enabled:= False;
    if serPump.LastError = 9997 then
    begin
     MainForm.StopBB.Enabled:= False;
     exit; // we cannot close socket or free if the connection timed out
    end;
    serPump.CloseSocket;
    serPump.Free;
    HaveSerialPump:= False;
    exit;
   end;
  end
  else // no serial connection
  begin
   MainForm.RunBB.Enabled:= False;
   MainForm.RunFreeBB.Enabled:= False;
   exit;
  end;
  MainForm.RunBB.Caption:= 'Pumps running';
  MainForm.RunBB.Enabled:= False;
  MainForm.RunFreeBB.Enabled:= False;
  MainForm.GenerateCommandBB.Enabled:= False;
  // disable all setting possibilities
  MainForm.RunSettingsGB.Enabled:= False;
  MainForm.LiveModeCB.Enabled:= False;
  // not the pump settings when in live mode
  if not MainForm.LiveModeCB.Checked then
  begin
   for j:= 1 to StepNum do
   begin
    (MainForm.FindComponent('Step' + IntToStr(j) + 'TS')
     as TTabSheet).Enabled:= False;
    // disable tooltips for pump name
    for i:= 1 to PumpNum do
     (MainForm.FindComponent('Pump' + IntToStr(i) + 'GB' + IntToStr(j))
      as TGroupBox).ShowHint:= False;
   end;
  end;
  MainForm.RepeatOutputLE.Visible:= False;
  MainForm.IndicatorPumpP.Caption:= 'Pumps are running';
  MainForm.IndicatorPumpP.Color:= clRed;
  // set timers
  if (StrToInt(MainForm.RepeatSE.Text) > 0)
   and (MainForm.RunEndlessCB.Checked = False) then
  begin
   MainForm.RepeatOutputLE.Visible:= True;
   RepeatTime:= trunc(GlobalTime / (StrToFloat(MainForm.RepeatSE.Text) + 1));
   if RepeatTime < 86400000 then // if less than one day
    MainForm.RepeatTimer.Interval:= trunc(RepeatTime)
   else // to restart timer every day
    MainForm.RepeatTimer.Interval:= 86400000;
   MainForm.RepeatTimer.Enabled:= True;
   CurrentRepeat:= 0;
   MainForm.RepeatOutputLE.Text:= '0';
   // set time that will later be evaluated when the timer ends
  GlobalRepeatTime:= RepeatTime;
  end;
  // delete finish time
  MainForm.FinishTimePumpLE.Text:= '';
  // output start time
  startTime := FormatDateTime('dd.mm.yyyy, hh:nn:ss', now);
  MainForm.StartTimePumpLE.Text:= startTime;

  // start OverallTimer to indicate running state
  if GlobalTime < 86400000 then // if less than one day
   MainForm.OverallTimer.Interval:= trunc(GlobalTime)
  else // to restart timer every day
   MainForm.OverallTimer.Interval:= 86400000;
  MainForm.OverallTimer.Enabled:= True;
  // show first tab and start its timer
  MainForm.RepeatPC.ActivePage:= MainForm.Step1TS;
  MainForm.StepTimer1.Enabled:= true;
  // do not show unused steps
  for j:= 2 to StepNum do
  begin
   if (MainForm.FindComponent('Step' + IntToStr(j) + 'UseCB')
     as TCheckBox).Checked = False then
    (MainForm.FindComponent('Step' + IntToStr(j) + 'TS')
     as TTabSheet).TabVisible:= False;
  end;

end;

procedure TPumpControl.PCStopBBClick(Sender: TObject);
// stop all pumps
var
 command, stopTime : string;
 i, j, k : integer;
begin
 // re-enable the connection menu in every case
 MainForm.PumpDriverMI.Enabled:= True;
 MainForm.FirmwareUpdateMI.Enabled:= True;
 MainForm.FirmwareResetMI.Enabled:= True;
 command:= '';
 // address
 command:= '/0';
 // disable all pumps
 command:= command + 'I';
 for k:= 1 to PumpNum do
  command:= command + '0';
 // execute flag and turn off LED
 command:= command + 'lR';
 // execute
 MainForm.CommandM.Text:= command;
 command:= command + LineEnding;
 if HaveSerialPump then
 begin
  serPump.SendString(command);
  if serPump.LastError <> 0 then
  begin
   with Application do
    MessageBox(PChar(COMPort + 'error: ' + serPump.LastErrorDesc), 'Error', MB_ICONERROR+MB_OK);
   MainForm.ConnComPortPumpLE.Color:= clRed;
   MainForm.ConnComPortPumpLE.Text:= 'Try to reconnect';
   MainForm.IndicatorPumpP.Caption:= 'Connection failiure';
   MainForm.PumpDriverMI.Enabled:= True;
   if serPump.LastError = 9997 then
   begin
    MainForm.StopBB.Enabled:= False;
    exit; // we cannot close socket or free when the connection timed out
   end;
   serPump.CloseSocket;
   serPump.Free;
   HaveSerialPump:= False;
   exit;
  end;
  //received:= ser.RecvString(1000);
  //with Application do
  // MessageBox(PChar(received), 'Information', MB_ICONINFORMATION+MB_OK);
 end;
 // output stop time only when there was actually a run
 if MainForm.IndicatorPumpP.Caption = 'Pumps are running' then
 begin
  stopTime:= FormatDateTime('dd.mm.yyyy, hh:nn:ss', now);
  MainForm.FinishTimePumpLE.Text:= stopTime;
  MainForm.IndicatorPumpP.Caption:= 'Manually stopped';
  MainForm.IndicatorPumpP.Color:= clHighlight;
 end;
 MainForm.OverallTimer.Enabled:= False;
 MainForm.RepeatTimer.Enabled:= False;
 // we must prevent that too long commands overflow the Arduino command buffer
 // therefore block the enabing to start a new action for a second
 MainForm.StopTimer.Enabled:= True;
 MainForm.RunBB.Enabled:= False;
 MainForm.RunFreeBB.Enabled:= False;
 MainForm.GenerateCommandBB.Enabled:= True;
 // stop all timers
 for j:= 1 to StepNum do
  (MainForm.FindComponent('StepTimer' + IntToStr(j))
   as TTimer).Enabled:= False;
 // enable all setting possibilities only if no file is loaded
 if (MainForm.LoadedActionFileM.Text = 'None')
  or (MainForm.LoadedActionFileM.Text = 'Free Pumps') then
 begin
  MainForm.LiveModeCB.Enabled:= True;
  MainForm.RunSettingsGB.Enabled:= not MainForm.LiveModeCB.Checked;
  for j:= 1 to StepNum do
  begin
   (MainForm.FindComponent('Step' + IntToStr(j) + 'TS')
    as TTabSheet).Enabled:= True;
   // enable tooltips for pump name
   for i:= 1 to PumpNum do
    (MainForm.FindComponent('Pump' + IntToStr(i) + 'GB' + IntToStr(j))
     as TGroupBox).ShowHint:= True;
  end;
  // view tab after last used step
  for j:= 2 to StepNum-1 do
  begin
   if (MainForm.FindComponent('Step' + IntToStr(j) + 'UseCB')
       as TCheckBox).Checked = True then
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

procedure TPumpControl.PCRepeatTimerFinished;
// Actions after repeat time interval ends
begin
 // if one day has passed but the pumps must run longer
 if GlobalRepeatTime > 86400000 then
 begin
  GlobalRepeatTime:= GlobalRepeatTime - 4000;
  if GlobalRepeatTime < 86400000 then // if less than one day
   MainForm.RepeatTimer.Interval:= trunc(GlobalRepeatTime)
  else // to restart timer every day
   MainForm.RepeatTimer.Interval:= 86400000;
  MainForm.RepeatTimer.Enabled:= True;
  exit;
 end;
 if StrToInt(MainForm.RepeatSE.Text) > StrToInt(MainForm.RepeatOutputLE.Text) then
 begin
  inc(CurrentRepeat);
  GlobalRepeatTime:= RepeatTime;
  MainForm.RepeatTimer.Enabled:= True;
  // only increase shown repeat if not already stopped
  if MainForm.IndicatorPumpP.Caption <> 'Manually stopped' then
   MainForm.RepeatOutputLE.Text:= IntToStr(CurrentRepeat);
 end
 else
  MainForm.RepeatTimer.Enabled:= False;
end;

procedure TPumpControl.PCOverallTimerFinished;
// Actions after time interval ends
var
  finishTime : string;
  i, j : integer;
begin
 // if one day has passed but the pumps must run longer
 if GlobalTime > 86400000 then
 begin
  GlobalTime:= GlobalTime - 86400000;
  if GlobalTime < 86400000 then // if less than one day
   MainForm.OverallTimer.Interval:= trunc(GlobalTime)
  else // to restart timer every day
   MainForm.OverallTimer.Interval:= 86400000;
  MainForm.OverallTimer.Enabled:= True;
  exit;
 end;
 // output finish time
 finishTime := FormatDateTime('dd.mm.yyyy, hh:nn:ss', now);
 MainForm.FinishTimePumpLE.Text:= finishTime;
 MainForm.OverallTimer.Enabled:= False;
 MainForm.PumpDriverMI.Enabled:= True;
 MainForm.FirmwareUpdateMI.Enabled:= True;
 MainForm.FirmwareResetMI.Enabled:= True;
 MainForm.RunBB.Caption:= 'Run Pumps';
 MainForm.RunBB.Enabled:= True;
 MainForm.RunFreeBB.Enabled:= True;
 MainForm.GenerateCommandBB.Enabled:= True;
 MainForm.IndicatorPumpP.Caption:= 'Run finished';
 MainForm.IndicatorPumpP.Color:= clInfoBk;
 MainForm.RepeatOutputLE.Visible:= False;
 // stop all timers
 for j:= 1 to StepNum do
  (MainForm.FindComponent('StepTimer' + IntToStr(j))
   as TTimer).Enabled:= False;
 // enable all setting possibilities only if no file is loaded
 if (MainForm.LoadedActionFileM.Text = 'None')
  or (MainForm.LoadedActionFileM.Text = 'Free Pumps') then
 begin
  MainForm.LiveModeCB.Enabled:= True;
  MainForm.RunSettingsGB.Enabled:= not MainForm.LiveModeCB.Checked;
  for j:= 1 to StepNum do
  begin
   (MainForm.FindComponent('Step' + IntToStr(j) + 'TS')
    as TTabSheet).Enabled:= True;
   // enable tooltips for pump name
   for i:= 1 to PumpNum do
   (MainForm.FindComponent('Pump' + IntToStr(i) + 'GB' + IntToStr(j))
    as TGroupBox).ShowHint:= True;
  end;
  // view tab after last used step
  for j:= 2 to StepNum-1 do
  begin
   if (MainForm.FindComponent('Step' + IntToStr(j) + 'UseCB')
       as TCheckBox).Checked = True then
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
  end
  else
  begin
   MainForm.RepeatSE.Enabled:= True;
   if not MainForm.Step2UseCB.Checked then
    MainForm.ActionTime1GB.Enabled:= True;
  end;
end;

end.

