unit Fitting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, Buttons, ComCtrls, LazFileUtils, TAGraph, TASources,
  TAFuncSeries, TACustomSource, TAFitLib, TAFitUtils, TATools, SpinEx, Types,
  math, spe, int,
  // custom forms
  JTDriverSensingMain;

type

  { TFitForm }

  TFitForm = class(TForm)
    BtnLoad: TButton;
    FitChartToolset: TChartToolset;
    FitChart: TChart;
    cbFitParam0Fixed: TCheckBox;
    cbFitParam1Fixed: TCheckBox;
    cbShowConfidenceIntervals: TCheckBox;
    cbShowPredictionIntervals: TCheckBox;
    FitChartToolsetDataPointCrosshairTool1: TDataPointCrosshairTool;
    FitChartToolsetDataPointHintTool1: TDataPointHintTool;
    FitChartToolsetZoomDragTool1: TZoomDragTool;
    FitResultInfoM: TMemo;
    FitParam0SE: TFloatSpinEdit;
    FitParam1SE: TFloatSpinEdit;
    ReducedChi2LE: TLabeledEdit;
    OpenCSVDialog: TOpenDialog;
    PChi2LE: TLabeledEdit;
    SaveDataCSVDialog: TSaveDialog;
    UpperConfIntervalSeries: TFuncSeries;
    LowerConfIntervalSeries: TFuncSeries;
    UpperPredIntervalSeries: TFuncSeries;
    LowerPredIntervalSeries: TFuncSeries;
    FitSeries: TFitSeries;
    FitEquationCB: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    gbDataGeneration: TGroupBox;
    gbFitting: TGroupBox;
    lblFitOrder:TLabel;
    lblFitEquation: TLabel;
    lbResults: TListBox;
    ListChartSource: TListChartSource;
    PageControl1: TPageControl;
    pnlParams: TPanel;
    FitOrderSE:TSpinEdit;
    pnlLog: TPanel;
    pnlChart: TPanel;
    btnSave: TSpeedButton;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure BtnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FitEquationCBSelect(Sender: TObject);
    procedure cbShowConfidenceIntervalsChange(Sender: TObject);
    procedure cbShowPredictionIntervalsChange(Sender: TObject);
    procedure FitChartToolsetDataPointHintTool1Hint(ATool: TDataPointHintTool;
      const APoint: TPoint; var AHint: String);
    procedure FixedParamsChanged(Sender: TObject);
    procedure FitOrderSEChange(Sender:TObject);
    procedure FormCreate(Sender: TObject);
    procedure FitCompleteHandler(Sender:TObject);
    procedure FormShow(Sender: TObject);
    procedure lbResultsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect);

  private
    FReportDecimals: Integer;
    FDemoData: Boolean;
    procedure OpenFile(const AFileName: string);
    function PrepareFixedParams: String;
  public
    function SaveCSV : Boolean;
    function CalculateP(Chi2: double) : double;

  end;

var
  FitForm : TFitForm;
  CSVOutName : string = '';
  DOF : integer = 1;

implementation

{$R *.lfm}

const
  // initial fit parameter values
  POLY_PARAMS: array[0..2] of Double = (10, -8, 0.2);
  LIN_PARAMS: array[0..1] of Double = (2.5, 0.5);
  EXP_PARAMS: array[0..1] of Double = (10.0, -0.05);
  PWR_PARAMS: array[0..1] of Double = (3.0, -0.5);
  HARMONIC_PARAMS: array[0..3] of Double = (2.0, 1.0, 1/3, 1/5);

//------------------------------------------------------------------------------

procedure TFitForm.FormShow(Sender: TObject);
// takes the data from the result plot and puts them to the fit plot
Type
  TDoubleArray = Array of double;
var
  i, intCount : longint;
  YArray : TDoubleArray;
  max : double;
begin
 {
 MainForm.ResultCHAverages.AddXY(1.1, 1.1);
 MainForm.ResultCHAverages.AddXY(2.2, 2.2);
 MainForm.ResultCHAverages.AddXY(3.3, 3.3);

 MainForm.ResultCHValues.AddXY(1.1, 0.9);
 MainForm.ResultCHValues.AddXY(1.1, 1.0);
 MainForm.ResultCHValues.AddXY(1.1, 1.5);
 MainForm.ResultCHValues.AddXY(2.2, 2.6);
 MainForm.ResultCHValues.AddXY(3.3, 3.6);
 }
 // always start with fresh data because in the meantime there might be new ones
 FitSeries.Clear;
 YArray:= TDoubleArray.Create(0.0); // initialize to silence compiler warning

 // if there is only one measurement point, nothing can be fit
 // therefore do nothing
 if MainForm.ResultCHAverages.Count < 2 then
  exit;

 for i:= 0 to MainForm.ResultCHAverages.Count-1 do
 begin
  // read out all Y values for the measured X value
  for intCount:= 0 to (MainForm.ResultCHValues.count - 1) do
  begin
   setLength(YArray, MainForm.ResultCHValues.count); // create array with zeroes
   if MainForm.ResultCHValues.XValue[intCount] = MainForm.ResultCHAverages.XValue[i] then
    YArray[intCount]:= MainForm.ResultCHValues.YValue[intCount];
  end;
  // we only need now the maximal y-value because the error bars are
  // symmetric since the point is the average
  // (the {%H-} is a directive to silence a compiler hint)
  max:= maxvalue{%H-}(YArray) - MainForm.ResultCHAverages.YValue[i];
  // if max = 0 we assume an error of 10%
  if max = 0.0 then
   max:= maxvalue{%H-}(YArray) * 0.1;
  // empty the array
  setLength(YArray, 0);
  // add the data to the plot
  FitSeries.AddXY(MainForm.ResultCHAverages.XValue[i], MainForm.ResultCHAverages.YValue[i], [max]);
 end;

 // the axis scale does not take the error bars into account
 // therefore set the X/Y range manually
 FitChart.AxisList[0].Range.Min:= 0;
 FitChart.AxisList[0].Range.Max:= FitSeries.MaxYValue + 1;

end;

//------------------------------------------------------------------------------

procedure TFitForm.FormCreate(Sender: TObject);
begin
 // set to linear fit
 FitEquationCB.ItemIndex:= 1;
 // update fit parameter labels in the form accordingly
 FitEquationCBSelect(Sender);
end;

//------------------------------------------------------------------------------

function RoundToError(AValue, AError: Double): String;
var
  n: Integer;
begin
  if IsNaN(AError) or (AError = 0) then
    Result := Format('%g', [AValue])
  else begin
    n := Round(Log10(AError));
    if n < 0 then
      Result := Format('%.*f ± %.*f', [1 - n, AValue, 1 - n, AError])
    else if n = 0 then
      Result := Format('%.1f ± %.1f', [AValue, AError])
    else
      Result := Format('%.0f ± %.0f', [RoundTo(AValue, n), RoundTo(AError, n)]);
  end;
end;

//------------------------------------------------------------------------------

function TFitForm.SaveCSV : Boolean;
var
 OutNameHelp : string;
 stream: TStream;
 i: Integer;
 si: PChartDataItem;
 line: String;
 dyp, dyn: Double;

begin
 Result:= false;
 // propose a file name
 OutNameHelp:= 'FitResult';
 SaveDataCSVDialog.FileName:= OutNameHelp;
 if SaveDataCSVDialog.Execute = true then
 begin
  OutNameHelp:= SaveDataCSVDialog.FileName;
  //the file dialog does not like dots and commas in file names
  //add file extension '.csv' if it is missing
  if (ExtractFileExt(OutNameHelp) <> '.csv') then
   Insert('.svg', OutNameHelp, Length(OutNameHelp) + 1);
  if FileExistsUTF8(OutNameHelp) = true then
  begin
   case QuestionDLG('Warning',
              'Would you like to overwrite the existing file'#13#10
              + ExtractFileName(OutNameHelp) + ' ?',
              mtWarning, [mrYes, 'Yes', mrNo, 'No'], '') of
    mrNo:
     begin
      CSVOutName:= '';
      SaveCSV();
      Result:= false;
      exit;
     end;
    mrCancel:
     begin
      CSVOutName:= '';
      SaveCSV();
      Result:= false;
      exit;
     end;
   end;
  end; //end if FileExists
  CSVOutName:= OutNameHelp;
 end //end if SaveDataCSVDialog.Execute
 else
  exit; // exit if aborted

 stream:= TFileStream.Create(SaveDataCSVDialog.FileName, fmCreate);
 try
  for i:= 0 to ListChartSource.Count-1 do
  begin
   si:= ListChartSource.item[i];
   line:= Format('%.9g'#9'%.9g', [si^.X, si^.Y], DefaultFormatSettings);
    if ListChartSource.HasYErrorBars then
    begin
     ListChartSource.GetYErrorBarValues(i, dyp, dyn);
     line:= Format('%s'#9'%.9g', [line, (dyp + dyn) / 2], DefaultFormatSettings);
    end;
    line:= line + LineEnding;
    stream.WriteBuffer(line[1], Length(line));
  end;
  finally
   stream.Free;
  end;
 Result:= true;

end;

//------------------------------------------------------------------------------

procedure TFitForm.btnSaveClick(Sender: TObject);
begin
 CSVOutName:= '';
 SaveCSV;
end;

//------------------------------------------------------------------------------

procedure TFitForm.BtnLoadClick(Sender: TObject);
begin
  if OpenCSVDialog.Execute then
    OpenFile(OpenCSVDialog.Filename);
end;

//------------------------------------------------------------------------------

procedure TFitForm.OpenFile(const AFilename: String);
var
  L, LC: TStrings;
  x, y, dy: Double;
  i: Integer;
  res: Integer;
begin
  if not FileExists(AFileName) then
  begin
    ShowMessage('File not found.');
    exit;
  end;

  FitSeries.Clear;
  L := TStringList.Create;
  LC := TStringList.Create;
  try
    L.LoadFromFile(AFileName);
    if pos(';', L[0]) > 0 then
      LC.Delimiter := ';'
    else
    if pos(#9, L[0]) > 0 then
      LC.Delimiter:= #9
    else
    begin
      ShowMessage('Unknown or no delimiter.');
      exit;
    end;

    LC.StrictDelimiter:= true;
    LC.DelimitedText:= L[0];
    if LC.Count = 3 then // if we have 3 columns, treat the third as y-error
    begin
      FitSeries.ListSource.YCount := 2;
      FitSeries.ListSource.YErrorBarData.Kind := ebkChartSource;
      FitSeries.ListSource.YErrorBarData.IndexPlus := 1;
    end
    else
    begin
      FitSeries.ListSource.YCount := 1;
      FitSeries.ListSource.YErrorBarData.Kind := ebkNone;
    end;

    for i:=0 to L.Count-1 do
    begin
      LC.DelimitedText := L[i];
      val(LC[0], x, res);
      if res <> 0 then
        Continue;
      val(LC[1], y, res);
      if res <> 0 then
        Continue;
      if FitSeries.ListSource.YCount > 1 then
      begin
        val(LC[2], dy, res);
        FitSeries.AddXY(x, y, [dy]);
      end else
        FitSeries.AddXY(x, y);
    end;
    FDemoData := false;
  finally
    L.Free;
  end;

  // the axis scale does not take the error bars into account
  // therefore set the X/Y range manually
  FitChart.AxisList[0].Range.Min:= 0;
  FitChart.AxisList[0].Range.Max:= FitSeries.MaxYValue + 1;

end;

//------------------------------------------------------------------------------

function HarmonicBaseFunc(x: double; Param: Integer): double;
begin
  Result := sin(x * (2*Param - 1));
end;

//------------------------------------------------------------------------------

procedure TFitForm.FitEquationCBSelect(Sender: TObject);
var
  eq: TFitEquation;
begin
  eq := TFitEquation(FitEquationCB.ItemIndex);
  FitSeries.FitEquation := eq;
  FitOrderSE.Enabled := (eq = fePolynomial);
  lblFitOrder.Enabled := FitOrderSE.Enabled;

  cbFitParam0Fixed.Caption := 'a = ';
  cbFitParam1Fixed.Caption := 'b = ';
  case eq of
    fePolynomial:
      begin
        cbFitParam0Fixed.Caption := 'b0 = ';
        cbFitParam1Fixed.Caption := 'b1 = ';
        FitParam0SE.Value := POLY_PARAMS[0];
        FitParam1SE.Value := POLY_PARAMS[1];
      end;
    feLinear:
      begin
        FitParam0SE.Value := LIN_PARAMS[0];
        FitParam1SE.Value := LIN_PARAMS[1];
      end;
    feExp:
      begin
        FitParam0SE.Value := EXP_PARAMS[0];
        FitParam1SE.Value := EXP_PARAMS[1];
      end;
    fePower:
      begin
        FitParam0SE.Value := PWR_PARAMS[0];
        FitParam1SE.value := PWR_PARAMS[1];
      end;
    feCustom:
      begin
        FitSeries.ParamCount := 4;
        FitSeries.SetFitBasisFunc(1, @HarmonicBaseFunc, 'sin(x)');
        FitSeries.SetFitBasisFunc(2, @HarmonicBaseFunc, 'sin(3 x)');
        FitSeries.SetFitBasisFunc(3, @HarmonicBaseFunc, 'sin(5 x)');
        cbFitParam0Fixed.Caption := 'b0 = ';
        cbFitParam1Fixed.Caption := 'b1 = ';
        FitParam0SE.Value := HARMONIC_PARAMS[0];
        FitParam1SE.Value := HARMONIC_PARAMS[1];
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TFitForm.cbShowConfidenceIntervalsChange(Sender: TObject);
begin
  UpperConfIntervalSeries.Active:= cbShowConfidenceIntervals.Checked;
  LowerConfIntervalSeries.Active:= cbShowConfidenceIntervals.Checked;
end;

//------------------------------------------------------------------------------

procedure TFitForm.cbShowPredictionIntervalsChange(Sender: TObject);
begin
  UpperPredIntervalSeries.Active:= cbShowPredictionIntervals.Checked;
  LowerPredIntervalSeries.Active:= cbShowPredictionIntervals.Checked;
end;

//------------------------------------------------------------------------------

procedure TFitForm.FitChartToolsetDataPointHintTool1Hint(
  ATool: TDataPointHintTool; const APoint: TPoint; var AHint: String);
var
 x, y: Double;
begin
 with ATool as TDataPointHintTool do
  if (Series <> nil) then
   begin
    // get the X/Y coordinate of the point
    // if the point is a data point we get a valid point index to get the value
    // if the point is on the fit function, we can only use the screen coordinates
    if PointIndex > -1 then
    begin
     x:= FitSeries.GetXValue(PointIndex);
     y:= FitSeries.GetYValue(PointIndex);
    end
    else
    begin
     x:= FitChart.ImageToGraph(APoint).X;
     y:= FitChart.ImageToGraph(APoint).Y;
    end;
    AHint:= Format('x = %f'#13#10'y = %.3f', [x,y]);
   end;
end;

//------------------------------------------------------------------------------

procedure TFitForm.FixedParamsChanged(Sender: TObject);
begin
  FitSeries.FixedParams:= PrepareFixedParams;
end;

//------------------------------------------------------------------------------

procedure TFitForm.FitOrderSEChange(Sender:TObject);
begin
  // Needs one parameter more than degree of fit polynomial.
  FitSeries.ParamCount:= FitOrderSE.Value + 1;
end;

//------------------------------------------------------------------------------

function MyFormatFloat(x: Double; StdFormat, ExpFormat: String): String;
begin
  if (abs(x) <= 1E-6) or (abs(x) >= 1E6) then
    Result := Format(ExpFormat, [x])
  else
    Result := Format(StdFormat, [x]);
end;

//------------------------------------------------------------------------------

procedure TFitForm.FitCompleteHandler(Sender:TObject);
const
  {$IF FPC_FullVersion >= 30004}
  MASK = '%-4s %10s %10s %10s %10s';
  CONF_MASK = '%-4s %10s %10s %10s';
  {$ELSE}
  MASK = '%-4s %10s %10s %10s';
  {$IFEND}
  EXP_FMT = '%.3e';
  STD_FMT = '%.5f';
  PARAM_NAME: array[0..1] of String = ('a', 'b');
var
  i: Integer;
  decsep: Char;
  paramName: String;
  confL, confH: Double;
begin
  // don't do anything if there are no data points yet
  if FitSeries.IsEmpty then
   exit;

  decsep := DefaultFormatSettings.DecimalSeparator;
  with lbResults.Items do
  begin
    BeginUpdate;
    try
      Clear;
      DefaultFormatSettings.DecimalSeparator := '.';
      case FitSeries.ErrCode of
        fitOK:
          begin
            Add('PARAMETERS');
            {$IF FPC_FullVersion >= 30004}
            Add(Format(MASK, ['Name', 'Value', 'Std.Error', 't value', 'p (>|t|)']));
            {$ELSE}
            Add(Format(MASK, ['Name', 'Value', 'Std.Error', 't value']));
            {$IFEND}
            for i := 0 to FitSeries.ParamCount - 1 do begin
              case FitSeries.FitEquation of
                fePolynomial, feCustom:
                  paramName := Format('b[%d]', [i]);
                else
                  paramName := PARAM_NAME[i];
              end;
              Add(Format(MASK, [
                paramName,
                MyFormatFloat(FitSeries.Param[i], STD_FMT, EXP_FMT),
                MyFormatFloat(FitSeries.ParamError[i], STD_FMT, EXP_FMT),
                MyFormatFloat(FitSeries.Param_tValue[i], STD_FMT, EXP_FMT)
                {$IF FPC_FullVersion >= 30004},
                MyFormatFloat(FitSeries.Param_pValue[i], STD_FMT, EXP_FMT)
                {$IFEND}
              ]));
            end;
            Add('');
            {$IF FPC_FullVersion >= 30004}
            Add('CONFIDENCE LIMITS');
            Add(Format(CONF_MASK, ['Name', 'Value', 'Lower', 'Upper']));
            for i := 0 to FitSeries.ParamCount - 1 do begin
              case FitSeries.FitEquation of
                fePolynomial, feCustom:
                  paramname := Format('b[%d]', [i]);
                else
                  paramname := PARAM_NAME[i];
              end;
              FitSeries.GetConfidenceLimits(i, confL, confH);
              Add(Format(CONF_MASK, [
                paramName,
                MyFormatFloat(FitSeries.Param[i], STD_FMT, EXP_FMT),
                MyFormatFloat(confL, STD_FMT, EXP_FMT),
                MyFormatFloat(confH, STD_FMT, EXP_FMT)
              ]));
            end;
            Add('');
            {$IFEND}
            Add('ANALYSIS OF VARIANCE');
            lbResults.Canvas.Font.Assign(lbResults.Font);
            FReportDecimals := 5;
            FitSeries.FitStatistics.Report_ANOVA(lbResults.Items, #9, '%.'+IntToStr(FReportDecimals)+'f');
            Add('');
            Add('VARIANCE-COVARIANCE MATRIX');
            FitSeries.FitStatistics.Report_VarCovar(lbResults.Items);

            {$IF FPC_FullVersion >= 30004}
            UpperConfIntervalSeries.OnCalculate:= @FitSeries.GetUpperConfidenceInterval;
            LowerConfIntervalSeries.OnCalculate:= @FitSeries.GetLowerConfidenceInterval;
            UpperPredIntervalSeries.OnCalculate:= @FitSeries.GetUpperPredictionInterval;
            LowerPredIntervalSeries.OnCalculate:= @FitSeries.GetLowerPredictionInterval;
            {$IFEND}
          end;
        fitDimError:
          Add('The lengths of the data vectors do not match.');
        fitMoreParamsThanValues:
          Add('There are more fitting parameters than data values.');
        fitNoFitParams:
          Add('No fit parameters specified');
        fitSingular:
          Add('Matrix is (nearly) singular');
      end;
    finally
      EndUpdate;
      DefaultFormatSettings.DecimalSeparator := decsep;
    end;
  end;

  FitResultInfoM.Lines.Clear;
  // (DOF) degrees of freedom: number of data points - number of fit parameters
  DOF:= FitSeries.FitStatistics.DOF;

  // output Chi^2
  if IsNan(FitSeries.FitStatistics.ReducedChi2) then
  begin
   ReducedChi2LE.Text:= 'Not available';
   PChi2LE.Text:= 'Not available';
   if FitSeries.ErrCode = fitMoreParamsThanValues then
    FitResultInfoM.Lines.Add('No fit. More fit parameters than data values!')
   else if DOF = 0 then
    FitResultInfoM.Lines.Add('No Chi^2 because there is no degree of freedom');
   exit;
  end
  else
   ReducedChi2LE.Text:= FloatToStr(RoundTo(FitSeries.FitStatistics.ReducedChi2, -3));

  // calculate the probablitty that arbitragy data calculaated
  // with the fit function will result in larger Chi^2
  // so numerical integrate the Chi^2 probability function ChiF
  PChi2LE.Text:= FloatToStr(
             CalculateP(FitSeries.FitStatistics.ReducedChi2));
end;

//------------------------------------------------------------------------------

function ChiF(t: double) : double;
// Note: this function must not be part of any class
// otherwise it cannot be used for numeric integration
begin
 Result:= 1/(power(2, DOF/2)  *spegam(DOF/2)) * power(t, DOF/2-1) *exp(-t/2);
end;

//------------------------------------------------------------------------------

function TFitForm.CalculateP(Chi2: double) : double;
var
  err: double = 0.0;
  term: integer = 0;
  integral: double = 0.0;
begin
  try
    int1fr(@ChiF, Chi2*DOF, Infinity, 1e-3, integral, err, term);
  except
    term:= 4;
  end;
  case term of
    1: ;
    2: FitResultInfoM.Lines.Add('P could be calculated but with a precision < 1e-3');
    3: FitResultInfoM.Lines.Add('P cannot be calculated: Error in input data');
    4: FitResultInfoM.Lines.Add('P cannot be calculated: Divergent, or calculation converges too slowly.');
  end;
  Result:= RoundTo(integral, -3);
end;

//------------------------------------------------------------------------------

procedure TFitForm.lbResultsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect);
var
  s: String;
  p: Integer;
  i: Integer;
  w: Integer;
  lb: TListBox;
begin
  lb := Control as TListbox;
  lb.Canvas.FillRect(ARect);
  s := lb.Items[Index];
  p := pos(#9, lb.Items[Index]);
  if p = 0 then
    lb.Canvas.TextOut(ARect.Left+2, ARect.Top, lb.Items[Index])
  else begin
    s := Copy(lb.Items[Index], 1, p-1);
    lb.Canvas.TextOut(ARect.Left+2, ARect.Top, s);
    s := Copy(lb.Items[Index], p+1, MaxInt);
    if TryStrToInt(s, i) then begin
      p := FReportDecimals + 1;
      while p > 0 do begin
        s := s + ' ';
        dec(p);
      end;
    end;
    w := lb.Canvas.TextWidth(s);
    lb.Canvas.Textout(ARect.Right - w - 2, ARect.Top, s);
  end;
end;

//------------------------------------------------------------------------------

function TFitForm.PrepareFixedParams: String;
var
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  Result:= '';
  if cbFitParam0Fixed.Checked then
    Result := FloatToStr(FitParam0SE.Value, fs);
  if cbFitParam1Fixed.Checked then
    Result := Result + ';' + FloatToStr(FitParam1SE.Value, fs);
end;

end.

