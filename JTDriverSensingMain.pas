unit JTDriverSensingMain;

{$mode objfpc}{$H+}{$R+}{$Q+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus, Math,
  StdCtrls, Streamex, ExtCtrls, Spin, Buttons, LCLType, Registry, Process,
  Fileinfo, LazFileUtils, SynaSer, Crt, StrUtils, PopupNotifier, TAGraph,
  TASeries, TATools, SpinEx, Types, TATextElements, TALegend, DateUtils,
  // the custom forms
  ScanningProgress, SerialUSBSelection, AboutForm, TAChartAxis, TAChartListbox,
  TATransformations, TAChartUtils, TAChartLiveView, TACustomSeries;

type

  { TMainForm }

  TMainForm = class(TForm)
    AnOutPump2GB: TGroupBox;
    AnOutPump3GB: TGroupBox;
    AnOutPump4GB: TGroupBox;
    Appearance1BB: TBitBtn;
    Appearance2BB: TBitBtn;
    Appearance3BB: TBitBtn;
    Appearance6BB: TBitBtn;
    Appearance7BB: TBitBtn;
    Appearance8BB: TBitBtn;
    Appearance5BB: TBitBtn;
    Appearance4BB: TBitBtn;
    AutoscaleB: TButton;
    CalibEveryXStepsL2: TLabel;
    MeasureAverageSE: TSpinEdit;
    LinearityTestGB: TGroupBox;
    MeasurementChannelsPC: TPageControl;
    GlucoseMeasureAvailChanL: TLabel;
    GlucoseMeasureCLB: TChartListbox;
    GlucoseMeasureTS: TTabSheet;
    LactateMeasureAvailChanL: TLabel;
    LactateMeasureCLB: TChartListbox;
    LactateMeasureTS: TTabSheet;
    CalibStepCB: TComboBox;
    Channel3LE: TLabeledEdit;
    Channel6LE: TLabeledEdit;
    Channel5LE: TLabeledEdit;
    Channel4LE: TLabeledEdit;
    Channel3GB: TGroupBox;
    Channel6GB: TGroupBox;
    Channel1LE: TLabeledEdit;
    Channel3OnOffCB: TCheckBox;
    Channel6OnOffCB: TCheckBox;
    AnOutOnOffTB: TToggleBox;
    ChartAxisTransformTime: TChartAxisTransformations;
    DataPointClickTool: TDataPointClickTool;
    DataPointMarksClickTool: TDataPointMarksClickTool;
    HideNotesMI: TMenuItem;
    MeasureConcentrationsGB: TGroupBox;
    Separator2MI: TMenuItem;
    Separator3MI: TMenuItem;
    Separator1MI: TMenuItem;
    StartTimeLE: TEdit;
    HaveSerialSensorCB: TCheckBox;
    HavePumpSerialCB: TCheckBox;
    GlucoseAvailChanL: TLabel;
    ChangeSensorDataFileMI: TMenuItem;
    AutoscaleMI: TMenuItem;
    DriverConnectionGB: TGroupBox;
    DriverConnectBB: TBitBtn;
    HaveDefFileCB: TCheckBox;
    IconImageBlue: TImage;
    IconImageGreen: TImage;
    Label100: TLabel;
    Label101: TLabel;
    Label102: TLabel;
    Label103: TLabel;
    Label104: TLabel;
    Label105: TLabel;
    Label106: TLabel;
    Label107: TLabel;
    Label108: TLabel;
    Label109: TLabel;
    Label110: TLabel;
    Label111: TLabel;
    Label112: TLabel;
    Label113: TLabel;
    Label114: TLabel;
    Label115: TLabel;
    Label116: TLabel;
    Label117: TLabel;
    Label118: TLabel;
    Label119: TLabel;
    Label120: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    Label68: TLabel;
    Label70: TLabel;
    Label72: TLabel;
    Label73: TLabel;
    Label74: TLabel;
    Label75: TLabel;
    Label76: TLabel;
    Label77: TLabel;
    Label78: TLabel;
    Label79: TLabel;
    StartTimeL: TLabel;
    Label80: TLabel;
    Label81: TLabel;
    Label82: TLabel;
    Label83: TLabel;
    Label84: TLabel;
    Label85: TLabel;
    Label86: TLabel;
    Label87: TLabel;
    Label88: TLabel;
    Label89: TLabel;
    Label90: TLabel;
    Label91: TLabel;
    Label92: TLabel;
    Label93: TLabel;
    Label94: TLabel;
    Label95: TLabel;
    Label96: TLabel;
    Label97: TLabel;
    Label98: TLabel;
    Label99: TLabel;
    LoadOtherDefBB: TBitBtn;
    Step1MeasureL: TLabel;
    Step1MeasureValueFSE: TFloatSpinEdit;
    Step2MeasureL: TLabel;
    Step2MeasureValueFSE: TFloatSpinEdit;
    Step3MeasureL: TLabel;
    Step3MeasureValueFSE: TFloatSpinEdit;
    Step4MeasureL: TLabel;
    Step4MeasureValueFSE: TFloatSpinEdit;
    Step5MeasureL: TLabel;
    Step5MeasureValueFSE: TFloatSpinEdit;
    Step6MeasureL: TLabel;
    Step6MeasureValueFSE: TFloatSpinEdit;
    Step7MeasureL: TLabel;
    Step7MeasureValueFSE: TFloatSpinEdit;
    TimeDaysHoursMinMI: TMenuItem;
    SIXConnectBB: TBitBtn;
    UseCalibGB: TGroupBox;
    HasNoPumpsCB: TCheckBox;
    LoadSensorDataMI: TMenuItem;
    CalibEveryXStepsL1: TLabel;
    ShowExpertCB: TCheckBox;
    UsedCalibValueSE: TSpinEdit;
    UsedCalibValueL: TLabel;
    LactateAvailChanL: TLabel;
    GlucoseCalibGB: TGroupBox;
    CalibrationGB: TGroupBox;
    LactateCalibGB: TGroupBox;
    LactateCalibUnitCB: TComboBox;
    LactateCalibValueFSE: TFloatSpinEdit;
    CalibStepL: TLabel;
    GlucoseTS: TTabSheet;
    LactateTS: TTabSheet;
    CalibSubstancesPC: TPageControl;
    GlucoseCalibCLB: TChartListbox;
    GlucoseCalibUnitCB: TComboBox;
    GlucoseCalibValueFSE: TFloatSpinEdit;
    LactateCalibCLB: TChartListbox;
    NoTempCorrectionCB: TCheckBox;
    UseCalibCB: TCheckBox;
    CalibEveryXStepsSE: TSpinEdit;
    UsedMeasureValueL: TLabel;
    ValveNumberL: TLabel;
    PumpNumberL: TLabel;
    ValveNumberSE: TSpinEdit;
    HasNoValvesCB: TCheckBox;
    PumpNumberSE: TSpinEdit;
    ValveSetupGB: TGroupBox;
    S1Valves: TTabSheet;
    S2Valves: TTabSheet;
    S3Valves: TTabSheet;
    S4Valves: TTabSheet;
    S5Valves: TTabSheet;
    S6Valves: TTabSheet;
    S7Valves: TTabSheet;
    ValuesLinearTransform: TLinearAxisTransform;
    ChartLiveView: TChartLiveView;
    ColorDialog: TColorDialog;
    AbortCalibrationMI: TMenuItem;
    ConnComPortSensM: TMemo;
    ContextSensFilePM: TPopupMenu;
    ConnComPortSensL: TLabel;
    Label71: TLabel;
    ChangeSensFileMI: TMenuItem;
    TimeMinuteMI: TMenuItem;
    TimeHourMI: TMenuItem;
    TimeDayMI: TMenuItem;
    ResetChartAppearanceMI: TMenuItem;
    ScrollIntervalFSE: TFloatSpinEdit;
    IndicatorAnOutP: TPanel;
    CalibrateTB: TToggleBox;
    LoadedDefFileL: TLabel;
    Label69: TLabel;
    LoadedDefFileM: TMemo;
    ChangeBackColorMI: TMenuItem;
    ContextChartPM: TPopupMenu;
    ScrollViewP: TPanel;
    CalibrateP: TPanel;
    TopLine: TConstantLine;
    BottomLine: TConstantLine;
    LeftLine: TConstantLine;
    RightLine: TConstantLine;
    UnloadDefBB: TBitBtn;
    RectangleSelectionTool: TUserDefinedTool;
    LineDragTool: TDataPointDragTool;
    ConnComPortPumpGeneralLE: TLabeledEdit;
    FirmwareResetMI: TMenuItem;
    IndicatorPumpGeneralP: TPanel;
    LoadedActionFileGeneraL: TLabel;
    Label67: TLabel;
    LoadedActionFileGeneralM: TMemo;
    PumpStatusGeneralGB: TGroupBox;
    UseAnOutCB: TCheckBox;
    AnOutputOf1CB: TComboBox;
    AnOutPump1GB: TGroupBox;
    AnOutputOf2CB: TComboBox;
    AnOutputOf3CB: TComboBox;
    AnOutputOf4CB: TComboBox;
    AnOutConnector1OnOffCB: TCheckBox;
    AnOutConnector2OnOffCB: TCheckBox;
    AnOutConnector3OnOffCB: TCheckBox;
    AnOutConnector4OnOffCB: TCheckBox;
    ChartAxisTransformValues: TChartAxisTransformations;
    ValuesAutoScaleAxisTransform: TAutoScaleAxisTransform;
    TempAutoScaleAxisTransform: TAutoScaleAxisTransform;
    ChartAxisTransformTemp: TChartAxisTransformations;
    AnOutOf2LE: TLabeledEdit;
    AnOutOf3LE: TLabeledEdit;
    AnOutOf4LE: TLabeledEdit;
    Label1Ch14: TLabel;
    Label1Ch15: TLabel;
    Label1Ch16: TLabel;
    AnOutMaxLabel: TLabel;
    AnOutMaxSignalFSE: TFloatSpinEdit;
    CurrChannel3LE: TLabeledEdit;
    CurrChannel6LE: TLabeledEdit;
    AnOutOf1LE: TLabeledEdit;
    AnOutGeneralGB: TGroupBox;
    Label1Ch13: TLabel;
    PerformTestingGB: TGroupBox;
    PerformLinearityCB: TCheckBox;
    ShowTempCB: TCheckBox;
    Channel2LE: TLabeledEdit;
    SIXCh1Results: TLineSeries;
    SIXCh4Results: TLineSeries;
    SIXTempValues: TLineSeries;
    SIXTempLE: TLabeledEdit;
    SIXTempGB: TGroupBox;
    LoadDefBB: TBitBtn;
    ChartToolset: TChartToolset;
    AxisClickTool: TAxisClickTool;
    DataPointCrosshairTool: TDataPointCrosshairTool;
    DataPointHintTool: TDataPointHintTool;
    LegendClickTool: TLegendClickTool;
    PanDragTool: TPanDragTool;
    PanMouseWheelTool: TPanMouseWheelTool;
    TitleFootClickTool: TTitleFootClickTool;
    ZoomDragTool: TZoomDragTool;
    ZoomMouseWheelTool: TZoomMouseWheelTool;
    RawCurrentCB: TCheckBox;
    ExpertGB: TGroupBox;
    SIXCh7Values: TLineSeries;
    SIXCh7Results: TLineSeries;
    SIXCh8Values: TLineSeries;
    SIXCh8Results: TLineSeries;
    SIXCh1Values: TLineSeries;
    SIXCh4Values: TLineSeries;
    SIXTypeRG: TRadioGroup;
    NoSubtractBlankCB: TCheckBox;
    CurrChannel7LE: TLabeledEdit;
    CurrChannel8LE: TLabeledEdit;
    CurrChannel4LE: TLabeledEdit;
    CurrChannel5LE: TLabeledEdit;
    Channel8CB: TComboBox;
    GeneralGB: TGroupBox;
    Channel7CB: TComboBox;
    EvalTimeFSE: TFloatSpinEdit;
    Channel8L: TLabel;
    ReadTimer: TTimer;
    Channel4OnOffCB: TCheckBox;
    Channel4GB: TGroupBox;
    Channel5GB: TGroupBox;
    Channel5OnOffCB: TCheckBox;
    Channel8GB: TGroupBox;
    Channel8OnOffCB: TCheckBox;
    SIXCh5Values: TLineSeries;
    SIXCh5Results: TLineSeries;
    SIXCh6Values: TLineSeries;
    SIXCh6Results: TLineSeries;
    Channel7GB: TGroupBox;
    IndicatorSensorP: TPanel;
    Channel7L: TLabel;
    LoadedFileSensL: TLabel;
    LoadedFileSensM: TMemo;
    Channel2OnOffCB: TCheckBox;
    Channel1OnOffCB: TCheckBox;
    Channel7OnOffCB: TCheckBox;
    SIXCH: TChart;
    SIXCh2Results: TLineSeries;
    SIXCh2Values: TLineSeries;
    SIXCh3Results: TLineSeries;
    SIXCh3Values: TLineSeries;
    SIXValuesTS: TTabSheet;
    CurrChannel1LE: TLabeledEdit;
    CurrChannel2LE: TLabeledEdit;
    Channel1GB: TGroupBox;
    SixStatusGB: TGroupBox;
    AnalogOutTS: TTabSheet;
    EvalTimeL: TLabel;
    Channel2GB: TGroupBox;
    MainPC: TPageControl;
    LastShownP: TPanel;
    PumpDriverMI: TMenuItem;
    ResultCH: TChart;
    ResultCHAverages: TLineSeries;
    ResultCHValues: TLineSeries;
    ResultTS: TTabSheet;
    SaveCSVResultB: TButton;
    SaveScreenshotLiveB: TButton;
    SaveScreenshotResultB: TButton;
    ScrollViewCB: TCheckBox;
    GeneralTS: TTabSheet;
    SIXBiosensorsMI: TMenuItem;
    LiveModeCB: TCheckBox;
    DutyCycle1FSE: TFloatSpinEdit;
    DutyCycle2FSE: TFloatSpinEdit;
    DutyCycle3FSE: TFloatSpinEdit;
    DutyCycle4FSE: TFloatSpinEdit;
    DutyCycle5FSE: TFloatSpinEdit;
    DutyCycle6FSE: TFloatSpinEdit;
    DutyCycle7FSE: TFloatSpinEdit;
    ActionTime1GB: TGroupBox;
    ActionTime2GB: TGroupBox;
    ActionTime3GB: TGroupBox;
    ActionTime4GB: TGroupBox;
    ActionTime5GB: TGroupBox;
    ActionTime6GB: TGroupBox;
    ActionTime7GB: TGroupBox;
    DutyCycle1GB: TGroupBox;
    DutyCycle2GB: TGroupBox;
    DutyCycle3GB: TGroupBox;
    DutyCycle4GB: TGroupBox;
    DutyCycle5GB: TGroupBox;
    DutyCycle6GB: TGroupBox;
    DutyCycle7GB: TGroupBox;
    GenerateCommandBB: TBitBtn;
    ActionsGB: TGroupBox;
    CommandM: TMemo;
    FirmwareUpdateMI: TMenuItem;
    AboutMI: TMenuItem;
    FirmwareFileDialog: TOpenDialog;
    InfoNote: TPopupNotifier;
    Label1: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    GetFirmwareVersionMI: TMenuItem;
    Label23: TLabel;
    Label28: TLabel;
    Label3: TLabel;
    Label33: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label5: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label6: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    LoadedActionFileL: TLabel;
    LoadActionMI: TMenuItem;
    LoadedActionFileM: TMemo;
    Pump5DirectionRG1: TRadioGroup;
    Pump5DirectionRG2: TRadioGroup;
    Pump5DirectionRG3: TRadioGroup;
    Pump5DirectionRG4: TRadioGroup;
    Pump5DirectionRG5: TRadioGroup;
    Pump5DirectionRG6: TRadioGroup;
    Pump5DirectionRG7: TRadioGroup;
    Pump5GB1: TGroupBox;
    Pump5GB2: TGroupBox;
    Pump5GB3: TGroupBox;
    Pump5GB4: TGroupBox;
    Pump5GB5: TGroupBox;
    Pump5GB6: TGroupBox;
    Pump5GB7: TGroupBox;
    Pump5OnOffCB1: TCheckBox;
    Pump5OnOffCB2: TCheckBox;
    Pump5OnOffCB3: TCheckBox;
    Pump5OnOffCB4: TCheckBox;
    Pump5OnOffCB5: TCheckBox;
    Pump5OnOffCB6: TCheckBox;
    Pump5OnOffCB7: TCheckBox;
    Pump5VoltageFS1: TFloatSpinEdit;
    Pump5VoltageFS2: TFloatSpinEdit;
    Pump5VoltageFS3: TFloatSpinEdit;
    Pump5VoltageFS4: TFloatSpinEdit;
    Pump5VoltageFS5: TFloatSpinEdit;
    Pump5VoltageFS6: TFloatSpinEdit;
    Pump5VoltageFS7: TFloatSpinEdit;
    Pump6DirectionRG1: TRadioGroup;
    Pump6DirectionRG2: TRadioGroup;
    Pump6DirectionRG3: TRadioGroup;
    Pump6DirectionRG4: TRadioGroup;
    Pump6DirectionRG5: TRadioGroup;
    Pump6DirectionRG6: TRadioGroup;
    Pump6DirectionRG7: TRadioGroup;
    Pump6GB1: TGroupBox;
    Pump6GB2: TGroupBox;
    Pump6GB3: TGroupBox;
    Pump6GB4: TGroupBox;
    Pump6GB5: TGroupBox;
    Pump6GB6: TGroupBox;
    Pump6GB7: TGroupBox;
    Pump6OnOffCB1: TCheckBox;
    Pump6OnOffCB2: TCheckBox;
    Pump6OnOffCB3: TCheckBox;
    Pump6OnOffCB4: TCheckBox;
    Pump6OnOffCB5: TCheckBox;
    Pump6OnOffCB6: TCheckBox;
    Pump6OnOffCB7: TCheckBox;
    Pump6VoltageFS1: TFloatSpinEdit;
    Pump6VoltageFS2: TFloatSpinEdit;
    Pump6VoltageFS3: TFloatSpinEdit;
    Pump6VoltageFS4: TFloatSpinEdit;
    Pump6VoltageFS5: TFloatSpinEdit;
    Pump6VoltageFS6: TFloatSpinEdit;
    Pump6VoltageFS7: TFloatSpinEdit;
    Pump7DirectionRG1: TRadioGroup;
    Pump7DirectionRG2: TRadioGroup;
    Pump7DirectionRG3: TRadioGroup;
    Pump7DirectionRG4: TRadioGroup;
    Pump7DirectionRG5: TRadioGroup;
    Pump7DirectionRG6: TRadioGroup;
    Pump7DirectionRG7: TRadioGroup;
    Pump7GB1: TGroupBox;
    Pump7GB2: TGroupBox;
    Pump7GB3: TGroupBox;
    Pump7GB4: TGroupBox;
    Pump7GB5: TGroupBox;
    Pump7GB6: TGroupBox;
    Pump7GB7: TGroupBox;
    Pump7OnOffCB1: TCheckBox;
    Pump7OnOffCB2: TCheckBox;
    Pump7OnOffCB3: TCheckBox;
    Pump7OnOffCB4: TCheckBox;
    Pump7OnOffCB5: TCheckBox;
    Pump7OnOffCB6: TCheckBox;
    Pump7OnOffCB7: TCheckBox;
    Pump7VoltageFS1: TFloatSpinEdit;
    Pump7VoltageFS2: TFloatSpinEdit;
    Pump7VoltageFS3: TFloatSpinEdit;
    Pump7VoltageFS4: TFloatSpinEdit;
    Pump7VoltageFS5: TFloatSpinEdit;
    Pump7VoltageFS6: TFloatSpinEdit;
    Pump7VoltageFS7: TFloatSpinEdit;
    Pump8DirectionRG1: TRadioGroup;
    Pump8DirectionRG2: TRadioGroup;
    Pump8DirectionRG3: TRadioGroup;
    Pump8DirectionRG4: TRadioGroup;
    Pump8DirectionRG5: TRadioGroup;
    Pump8DirectionRG6: TRadioGroup;
    Pump8DirectionRG7: TRadioGroup;
    Pump8GB1: TGroupBox;
    Pump8GB2: TGroupBox;
    Pump8GB3: TGroupBox;
    Pump8GB4: TGroupBox;
    Pump8GB5: TGroupBox;
    Pump8GB6: TGroupBox;
    Pump8GB7: TGroupBox;
    Pump8OnOffCB1: TCheckBox;
    Pump8OnOffCB2: TCheckBox;
    Pump8OnOffCB3: TCheckBox;
    Pump8OnOffCB4: TCheckBox;
    Pump8OnOffCB5: TCheckBox;
    Pump8OnOffCB6: TCheckBox;
    Pump8OnOffCB7: TCheckBox;
    Pump8VoltageFS1: TFloatSpinEdit;
    Pump8VoltageFS2: TFloatSpinEdit;
    Pump8VoltageFS3: TFloatSpinEdit;
    Pump8VoltageFS4: TFloatSpinEdit;
    Pump8VoltageFS5: TFloatSpinEdit;
    Pump8VoltageFS6: TFloatSpinEdit;
    Pump8VoltageFS7: TFloatSpinEdit;
    S2P14: TTabSheet;
    S3P14: TTabSheet;
    S4P14: TTabSheet;
    S5P14: TTabSheet;
    S6P14: TTabSheet;
    S7P14: TTabSheet;
    S2P58: TTabSheet;
    S1PC: TPageControl;
    S3P58: TTabSheet;
    S4P58: TTabSheet;
    S5P58: TTabSheet;
    S6P58: TTabSheet;
    S7P58: TTabSheet;
    S2PC: TPageControl;
    S3PC: TPageControl;
    S4PC: TPageControl;
    S5PC: TPageControl;
    S6PC: TPageControl;
    S7PC: TPageControl;
    SaveActionMI: TMenuItem;
    FileMI: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    StartFitB: TButton;
    StepTimer1: TTimer;
    StepTimer2: TTimer;
    StepTimer3: TTimer;
    StepTimer4: TTimer;
    StepTimer5: TTimer;
    StepTimer6: TTimer;
    StepTimer7: TTimer;
    StopTimer: TTimer;
    StartTimePumpLE: TLabeledEdit;
    FinishTimePumpLE: TLabeledEdit;
    S1P14: TTabSheet;
    S1P58: TTabSheet;
    ActionControlTS: TTabSheet;
    TotalTimeLE: TLabeledEdit;
    ActionCommandSplitter: TPanel;
    Pump1ResultLE1: TLabeledEdit;
    Pump2ResultLE1: TLabeledEdit;
    Pump3ResultLE1: TLabeledEdit;
    Pump4ResultLE1: TLabeledEdit;
    Pump5ResultLE1: TLabeledEdit;
    Pump6ResultLE1: TLabeledEdit;
    Pump7ResultLE1: TLabeledEdit;
    Pump8ResultLE1: TLabeledEdit;
    Pump1ResultLE2: TLabeledEdit;
    Pump2ResultLE2: TLabeledEdit;
    Pump3ResultLE2: TLabeledEdit;
    Pump4ResultLE2: TLabeledEdit;
    Pump5ResultLE2: TLabeledEdit;
    Pump6ResultLE2: TLabeledEdit;
    Pump7ResultLE2: TLabeledEdit;
    Pump8ResultLE2: TLabeledEdit;
    Pump1ResultLE3: TLabeledEdit;
    Pump2ResultLE3: TLabeledEdit;
    Pump3ResultLE3: TLabeledEdit;
    Pump4ResultLE3: TLabeledEdit;
    Pump5ResultLE3: TLabeledEdit;
    Pump6ResultLE3: TLabeledEdit;
    Pump7ResultLE3: TLabeledEdit;
    Pump8ResultLE3: TLabeledEdit;
    Pump1ResultLE4: TLabeledEdit;
    Pump2ResultLE4: TLabeledEdit;
    Pump3ResultLE4: TLabeledEdit;
    Pump4ResultLE4: TLabeledEdit;
    Pump5ResultLE4: TLabeledEdit;
    Pump6ResultLE4: TLabeledEdit;
    Pump7ResultLE4: TLabeledEdit;
    Pump8ResultLE4: TLabeledEdit;
    Pump1ResultLE5: TLabeledEdit;
    Pump2ResultLE5: TLabeledEdit;
    Pump3ResultLE5: TLabeledEdit;
    Pump4ResultLE5: TLabeledEdit;
    Pump5ResultLE5: TLabeledEdit;
    Pump6ResultLE5: TLabeledEdit;
    Pump7ResultLE5: TLabeledEdit;
    Pump8ResultLE5: TLabeledEdit;
    Pump1ResultLE6: TLabeledEdit;
    Pump2ResultLE6: TLabeledEdit;
    Pump3ResultLE6: TLabeledEdit;
    Pump4ResultLE6: TLabeledEdit;
    Pump5ResultLE6: TLabeledEdit;
    Pump6ResultLE6: TLabeledEdit;
    Pump7ResultLE6: TLabeledEdit;
    Pump8ResultLE6: TLabeledEdit;
    Pump1ResultLE7: TLabeledEdit;
    Pump2ResultLE7: TLabeledEdit;
    Pump3ResultLE7: TLabeledEdit;
    Pump4ResultLE7: TLabeledEdit;
    Pump5ResultLE7: TLabeledEdit;
    Pump6ResultLE7: TLabeledEdit;
    Pump7ResultLE7: TLabeledEdit;
    Pump8ResultLE7: TLabeledEdit;
    RunTime1FSE: TFloatSpinEdit;
    RunTime2FSE: TFloatSpinEdit;
    RunTime3FSE: TFloatSpinEdit;
    RunTime4FSE: TFloatSpinEdit;
    RunTime5FSE: TFloatSpinEdit;
    RunTime6FSE: TFloatSpinEdit;
    RunTime7FSE: TFloatSpinEdit;
    RunSettingsGB: TGroupBox;
    StatusGB: TGroupBox;
    IndicatorPumpP: TPanel;
    Label10: TLabel;
    Label11: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    Pump1DirectionRG1: TRadioGroup;
    Pump1DirectionRG2: TRadioGroup;
    Pump1DirectionRG3: TRadioGroup;
    Pump1DirectionRG4: TRadioGroup;
    Pump1DirectionRG5: TRadioGroup;
    Pump1DirectionRG6: TRadioGroup;
    Pump1DirectionRG7: TRadioGroup;
    Pump1GB1: TGroupBox;
    Pump1GB2: TGroupBox;
    Pump1GB3: TGroupBox;
    Pump1GB4: TGroupBox;
    Pump1GB5: TGroupBox;
    Pump1GB6: TGroupBox;
    Pump1GB7: TGroupBox;
    Pump1OnOffCB1: TCheckBox;
    Pump1OnOffCB2: TCheckBox;
    Pump1OnOffCB3: TCheckBox;
    Pump1OnOffCB4: TCheckBox;
    Pump1OnOffCB5: TCheckBox;
    Pump1OnOffCB6: TCheckBox;
    Pump1OnOffCB7: TCheckBox;
    Pump1VoltageFS1: TFloatSpinEdit;
    Pump1VoltageFS2: TFloatSpinEdit;
    Pump1VoltageFS3: TFloatSpinEdit;
    Pump1VoltageFS4: TFloatSpinEdit;
    Pump1VoltageFS5: TFloatSpinEdit;
    Pump1VoltageFS6: TFloatSpinEdit;
    Pump1VoltageFS7: TFloatSpinEdit;
    Pump2DirectionRG1: TRadioGroup;
    Pump2DirectionRG2: TRadioGroup;
    Pump2DirectionRG3: TRadioGroup;
    Pump2DirectionRG4: TRadioGroup;
    Pump2DirectionRG5: TRadioGroup;
    Pump2DirectionRG6: TRadioGroup;
    Pump2DirectionRG7: TRadioGroup;
    Pump2GB1: TGroupBox;
    Pump2GB2: TGroupBox;
    Pump2GB3: TGroupBox;
    Pump2GB4: TGroupBox;
    Pump2GB5: TGroupBox;
    Pump2GB6: TGroupBox;
    Pump2GB7: TGroupBox;
    Pump2OnOffCB1: TCheckBox;
    Pump2OnOffCB2: TCheckBox;
    Pump2OnOffCB3: TCheckBox;
    Pump2OnOffCB4: TCheckBox;
    Pump2OnOffCB5: TCheckBox;
    Pump2OnOffCB6: TCheckBox;
    Pump2OnOffCB7: TCheckBox;
    Pump2VoltageFS1: TFloatSpinEdit;
    Pump2VoltageFS2: TFloatSpinEdit;
    Pump2VoltageFS3: TFloatSpinEdit;
    Pump2VoltageFS4: TFloatSpinEdit;
    Pump2VoltageFS5: TFloatSpinEdit;
    Pump2VoltageFS6: TFloatSpinEdit;
    Pump2VoltageFS7: TFloatSpinEdit;
    Pump3DirectionRG1: TRadioGroup;
    Pump3DirectionRG2: TRadioGroup;
    Pump3DirectionRG3: TRadioGroup;
    Pump3DirectionRG4: TRadioGroup;
    Pump3DirectionRG5: TRadioGroup;
    Pump3DirectionRG6: TRadioGroup;
    Pump3DirectionRG7: TRadioGroup;
    Pump3GB1: TGroupBox;
    Pump3GB2: TGroupBox;
    Pump3GB3: TGroupBox;
    Pump3GB4: TGroupBox;
    Pump3GB5: TGroupBox;
    Pump3GB6: TGroupBox;
    Pump3GB7: TGroupBox;
    Pump3OnOffCB1: TCheckBox;
    Pump3OnOffCB2: TCheckBox;
    Pump3OnOffCB3: TCheckBox;
    Pump3OnOffCB4: TCheckBox;
    Pump3OnOffCB5: TCheckBox;
    Pump3OnOffCB6: TCheckBox;
    Pump3OnOffCB7: TCheckBox;
    Pump3VoltageFS1: TFloatSpinEdit;
    Pump3VoltageFS2: TFloatSpinEdit;
    Pump3VoltageFS3: TFloatSpinEdit;
    Pump3VoltageFS4: TFloatSpinEdit;
    Pump3VoltageFS5: TFloatSpinEdit;
    Pump3VoltageFS6: TFloatSpinEdit;
    Pump3VoltageFS7: TFloatSpinEdit;
    Pump4DirectionRG1: TRadioGroup;
    Pump4DirectionRG2: TRadioGroup;
    Pump4DirectionRG3: TRadioGroup;
    Pump4DirectionRG4: TRadioGroup;
    Pump4DirectionRG5: TRadioGroup;
    Pump4DirectionRG6: TRadioGroup;
    Pump4DirectionRG7: TRadioGroup;
    Pump4GB1: TGroupBox;
    Pump4GB2: TGroupBox;
    Pump4GB3: TGroupBox;
    Pump4GB4: TGroupBox;
    Pump4GB5: TGroupBox;
    Pump4GB6: TGroupBox;
    Pump4GB7: TGroupBox;
    Pump4OnOffCB1: TCheckBox;
    Pump4OnOffCB2: TCheckBox;
    Pump4OnOffCB3: TCheckBox;
    Pump4OnOffCB4: TCheckBox;
    Pump4OnOffCB5: TCheckBox;
    Pump4OnOffCB6: TCheckBox;
    Pump4OnOffCB7: TCheckBox;
    Pump4VoltageFS1: TFloatSpinEdit;
    Pump4VoltageFS2: TFloatSpinEdit;
    Pump4VoltageFS3: TFloatSpinEdit;
    Pump4VoltageFS4: TFloatSpinEdit;
    Pump4VoltageFS5: TFloatSpinEdit;
    Pump4VoltageFS6: TFloatSpinEdit;
    Pump4VoltageFS7: TFloatSpinEdit;
    RepeatL: TLabel;
    RepeatOutputLE: TLabeledEdit;
    RepeatPC: TPageControl;
    RepeatSE: TSpinEdit;
    RunBB: TBitBtn;
    RunEndlessCB: TCheckBox;
    Step1TS: TTabSheet;
    Step1UseCB: TCheckBox;
    Step2TS: TTabSheet;
    Step2UseCB: TCheckBox;
    Step3TS: TTabSheet;
    Step3UseCB: TCheckBox;
    Step4TS: TTabSheet;
    Step4UseCB: TCheckBox;
    Step5TS: TTabSheet;
    Step5UseCB: TCheckBox;
    Step6TS: TTabSheet;
    Step6UseCB: TCheckBox;
    Step7TS: TTabSheet;
    Step7UseCB: TCheckBox;
    StopBB: TBitBtn;
    ConnectionMI: TMenuItem;
    MiscellaneousMI: TMenuItem;
    ConnComPortPumpLE: TLabeledEdit;
    MainMenu: TMainMenu;
    OverallTimer: TTimer;
    RepeatTimer: TTimer;
    Unit1RBh: TRadioButton;
    Unit2RBh: TRadioButton;
    Unit3RBh: TRadioButton;
    Unit4RBh: TRadioButton;
    Unit5RBh: TRadioButton;
    Unit6RBh: TRadioButton;
    Unit7RBh: TRadioButton;
    Unit1RBmin: TRadioButton;
    Unit2RBmin: TRadioButton;
    Unit3RBmin: TRadioButton;
    Unit4RBmin: TRadioButton;
    Unit5RBmin: TRadioButton;
    Unit6RBmin: TRadioButton;
    Unit7RBmin: TRadioButton;
    Unit1RBs: TRadioButton;
    Unit2RBs: TRadioButton;
    Unit3RBs: TRadioButton;
    Unit4RBs: TRadioButton;
    Unit5RBs: TRadioButton;
    Unit6RBs: TRadioButton;
    Unit7RBs: TRadioButton;
    Valve1RG1: TRadioGroup;
    Valve1RG2: TRadioGroup;
    Valve1RG3: TRadioGroup;
    Valve1RG4: TRadioGroup;
    Valve1RG5: TRadioGroup;
    Valve1RG6: TRadioGroup;
    Valve1RG7: TRadioGroup;
    Valve2RG2: TRadioGroup;
    Valve2RG3: TRadioGroup;
    Valve2RG4: TRadioGroup;
    Valve2RG5: TRadioGroup;
    Valve2RG6: TRadioGroup;
    Valve2RG7: TRadioGroup;
    Valve3RG1: TRadioGroup;
    Valve3RG2: TRadioGroup;
    Valve3RG3: TRadioGroup;
    Valve3RG4: TRadioGroup;
    Valve3RG5: TRadioGroup;
    Valve3RG6: TRadioGroup;
    Valve3RG7: TRadioGroup;
    Valve4RG2: TRadioGroup;
    Valve4RG3: TRadioGroup;
    Valve4RG4: TRadioGroup;
    Valve4RG5: TRadioGroup;
    Valve4RG6: TRadioGroup;
    Valve4RG7: TRadioGroup;
    Valve5RG1: TRadioGroup;
    Valve5RG2: TRadioGroup;
    Valve5RG3: TRadioGroup;
    Valve5RG4: TRadioGroup;
    Valve5RG5: TRadioGroup;
    Valve5RG6: TRadioGroup;
    Valve5RG7: TRadioGroup;
    Valve6RG2: TRadioGroup;
    Valve6RG3: TRadioGroup;
    Valve6RG4: TRadioGroup;
    Valve6RG5: TRadioGroup;
    Valve6RG6: TRadioGroup;
    Valve6RG7: TRadioGroup;
    Valve7RG1: TRadioGroup;
    Valve2RG1: TRadioGroup;
    Valve4RG1: TRadioGroup;
    Valve6RG1: TRadioGroup;
    Valve7RG2: TRadioGroup;
    Valve7RG3: TRadioGroup;
    Valve7RG4: TRadioGroup;
    Valve7RG5: TRadioGroup;
    Valve7RG6: TRadioGroup;
    Valve7RG7: TRadioGroup;
    Valve8RG1: TRadioGroup;
    Valve8RG2: TRadioGroup;
    Valve8RG3: TRadioGroup;
    Valve8RG4: TRadioGroup;
    Valve8RG5: TRadioGroup;
    Valve8RG6: TRadioGroup;
    Valve8RG7: TRadioGroup;
    PumpSetupGB: TGroupBox;
    procedure AbortCalibrationMIClick(Sender: TObject);
    procedure AboutMIClick(Sender: TObject);
    procedure AnOutConnectorXOnOffCBChange(Sender: TObject);
    procedure AnOutputOf1CBContextPopup(Sender: TObject; MousePos{%H-}: TPoint;
      var Handled: Boolean);
    procedure AnOutputOf2CBContextPopup(Sender: TObject; MousePos{%H-}: TPoint;
      var Handled: Boolean);
    procedure AnOutputOf3CBContextPopup(Sender: TObject; MousePos{%H-}: TPoint;
      var Handled: Boolean);
    procedure AnOutputOf4CBContextPopup(Sender: TObject; MousePos{%H-}: TPoint;
      var Handled: Boolean);
    procedure AppearanceXBBClick(Sender: TObject);
    procedure AutoscaleMIClick(Sender: TObject);
    procedure CalibrateTBChange(Sender: TObject);
    procedure ChangeBackColorMIClick(Sender: TObject);
    procedure ChangeSensFileMIClick(Sender: TObject);
    procedure Channel7CBContextPopup(Sender: TObject; MousePos{%H-}: TPoint;
      var Handled: Boolean);
    procedure Channel8CBContextPopup(Sender: TObject; MousePos{%H-}: TPoint;
      var Handled: Boolean);
    procedure ChannelXLEChange(Sender: TObject);
    procedure ChannelXCBChange(Sender: TObject);
    procedure ChanAnOutConnectorXOnOffCBChange(Sender: TObject);
    procedure AxisClickToolClick(Sender: TChartTool;
      Axis: TChartAxis; HitInfo: TChartAxisHitTests);
    procedure DataPointClickToolPointClick(ATool: TChartTool;
      APoint{%H-}: TPoint);
    procedure DataPointHintToolHint(ATool: TDataPointHintTool;
      const APoint{%H-}: TPoint; var AHint: String);
    procedure DataPointHintToolHintPosition(
      ATool: TDataPointHintTool; var APoint: TPoint);
    procedure SubstMeasureCLBItemClick(ASender: TObject; AIndex: Integer);
    procedure HideNotesMIClick(Sender: TObject);
    procedure OverallTimerStartTimer(Sender: TObject);
    procedure TimeDaysHoursMinMIClick(Sender: TObject);
    procedure HaveDefFileCBChange(Sender: TObject);
    procedure LegendClickToolClick(Sender: TChartTool;
      Legend: TChartLegend);
    procedure SIXCHAxisList1GetMarkText(Sender: TObject; var AText: String;
      AMark: Double);
    procedure TitleFootClickToolClick(Sender: TChartTool;
      Title: TChartTitle);
    procedure ZoomDragToolAfterMouseUp(ATool{%H-}: TChartTool;
      APoint{%H-}: TPoint);
    procedure ConnComPortPumpLEChange;
    procedure ConnComPortSensMContextPopup(Sender: TObject; MousePos{%H-}: TPoint;
      var Handled: Boolean);
    procedure HaveSerialSensorCBChange(Sender: TObject);
    procedure SIXConnectBBClick(Sender: TObject);
    procedure DriverConnectBBClick(Sender: TObject);
    procedure DutyCycleXFSEChange(Sender: TObject);
    procedure EvalTimeFSEChange(Sender: TObject);
    procedure FirmwareResetMIClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure CLBAddSeries(ASender: TChartListbox;
      ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
    procedure CalibValueFSEChange(Sender: TObject);
    procedure CalibCLBItemClick(ASender{%H-}: TObject; AIndex{%H-}: Integer);
    procedure HasNoPumpsCBChange(Sender: TObject);
    procedure HasNoValvesCBChange(Sender: TObject);
    procedure HavePumpSerialCBChange(Sender: TObject);
    procedure IndicatorPumpPPaint;
    procedure InfoNoteClose(Sender: TObject; var CloseAction{%H-}: TCloseAction);
    procedure LineDragToolDrag(ASender: TDataPointDragTool;
      var AGraphPoint: TDoublePoint);
    procedure LineDragToolDragStart(ASender: TDataPointDragTool;
      var AGraphPoint{%H-}: TDoublePoint);
    procedure LoadDefBBClick(Sender: TObject);
    procedure AnOutOfXLEChange(Sender: TObject);
    procedure LoadedActionFileGeneralMContextPopup(Sender: TObject;
      MousePos{%H-}: TPoint; var Handled: Boolean);
    procedure LoadedActionFileMChange(Sender: TObject);
    procedure LoadedActionFileMContextPopup(Sender: TObject; MousePos{%H-}: TPoint;
      var Handled: Boolean);
    procedure LoadedDefFileMContextPopup(Sender: TObject; MousePos{%H-}: TPoint;
      var Handled: Boolean);
    procedure LoadedFileSensMChange(Sender: TObject);
    procedure LoadedFileSensMContextPopup(Sender: TObject; MousePos{%H-}: TPoint;
      var Handled: Boolean);
    procedure LoadSensorDataMIClick(Sender: TObject);
    procedure NoTempCorrectionCBChange(Sender: TObject);
    procedure PumpNumberSEChange(Sender: TObject);
    procedure RepeatSEChange(Sender: TObject);
    procedure ResetChartAppearanceMIClick(Sender: TObject);
    procedure NoSubtractBlankCBChange(Sender: TObject);
    procedure PerformLinearityCBChange(Sender: TObject);
    procedure RawCurrentCBChange(Sender: TObject);
    procedure ReadTimerTimerFinished(Sender: TObject);
    procedure RectangleSelectionToolAfterMouseDown(ATool{%H-}: TChartTool;
      APoint: TPoint);
    procedure RectangleSelectionToolAfterMouseMove(ATool{%H-}: TChartTool;
      APoint: TPoint);
    procedure SaveCSVResultBClick(Sender: TObject);
    procedure SaveScreenshotLiveBClick(Sender: TObject);
    procedure SaveScreenshotResultBClick(Sender: TObject);
    procedure ScrollIntervalFSEChange(Sender: TObject);
    procedure ScrollViewCBChange(Sender: TObject);
    procedure ChannelXGBDblClick(Sender: TObject);
    procedure PumpConnectionMIClick(Sender: TObject);
    procedure PumpConnectionStart(Sender: TObject; Connect: Boolean);
    procedure FirmwareUpdateMIClick(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames{%H-}: array of String);
    procedure GenerateCommandBBClick(Sender: TObject);
    procedure GetFirmwareVersionMIClick(Sender: TObject);
    procedure LiveModeCBChange(Sender: TObject);
    procedure LoadActionMIClick(Sender: TObject);
    procedure ShowExpertCBChange(Sender: TObject);
    procedure UseCalibCBChange(Sender: TObject);
    procedure ValveNumberSEChange(Sender: TObject);
    procedure ValveRGChange(Sender: TObject);
    procedure PumpVoltageFSChange(Sender: TObject);
    procedure PumpGBDblClick(Sender: TObject);
    procedure AnOutPumpGBDblClick(Sender: TObject);
    procedure ValveRGDblClick(Sender: TObject);
    procedure PumpOnOffCBLoopChange(Sender: TObject);
    procedure RepeatPCChange(Sender: TObject);
    procedure RunBBClick(Sender: TObject);
    procedure RunEndlessCBChange(Sender: TObject);
    procedure SaveActionMIClick(Sender: TObject);
    procedure ShowTempCBChange(Sender: TObject);
    procedure SIXBiosensorsMIClick(Sender: TObject);
    procedure SIXBiosensorsStart(Connected: Boolean; Disconnect: Boolean);
    procedure SIXCHAfterDrawBackWall(ASender{%H-}: TChart; ACanvas: TCanvas;
      const ARect{%H-}: TRect);
    procedure StartFitBClick(Sender: TObject);
    procedure StepXUseCBChange(Sender: TObject);
    procedure StepTimerXFinished(Sender: TObject);
    procedure StepTimerLastFinished(Sender: TObject);
    procedure StopBBClick(Sender: TObject);
    procedure StopTimerFinished;
    procedure OverallTimerFinished;
    procedure RepeatTimerFinished;
    procedure AnOutOnOffTBChange(Sender: TObject);
    procedure TimeDayMIClick(Sender: TObject);
    procedure TimeHourMIClick(Sender: TObject);
    procedure TimeMinuteMIClick(Sender: TObject);
    procedure UnloadDefBBClick(Sender: TObject);
    procedure UseAnOutCBChange(Sender: TObject);
  private

  public
    function DialogWithPos(const Message: string; DialogType: TMsgDlgType;
              Buttons: TMsgDlgButtons; AX, AY: Integer): TModalResult;
    function OpenActionFile(InputName: string): Boolean;
    function OpenHandling(InName: string; FileExt: string): string;
    function SaveHandling(InName: string; FileExt: string): string;
    function ReadSensorData(Input: string; out AppendMinute: double;
              out AppendCounter: Int64; out LastDefFile: string;
              out LastSIXID: string): Boolean;
    procedure CloseLazSerialConn;
    procedure ClosePumpSerialConn;
    procedure FirmwareUpdate(forced: Boolean);
    procedure COMPortScan(PortType: string);
    procedure DisconnectPumpDriver;
    procedure SetSIXFactors;
    procedure DisconnectSIX;
    procedure AssureChannelDisplay;

  end;

type
 {$scopedEnums on}
 Substance = (Glucose, Lactate);

var
  MainForm : TMainForm;
  Version : string = '';
  FirmwareVersion : string = 'unknown';
  RequiredFirmwareVersion : float = 3.0;
  serPump : TBlockSerial;
  serSensor : TBlockSerial;
  COMListPumpDriver : array of Int32; // list with available pump drivers
  COMListSIX : array of Int32; // list with available SIX (list index is COM port number)
  SensorFileStream : TFileStream;
  HaveSensorFileStream : Boolean = False;
  FoundLoadedDefFile : Boolean = True; // .def file in loaded data was found
  InNamePump : string = ''; // name of loaded pump action file
  DropfileNamePump : string = ''; // name of dropped pump action file
  connectedPumpName : string = ''; // name of connected pump COM port
  InNameDef : string = ''; // name of loaded sensor definition file
  DropfileNameDef : string = ''; // name of dropped sensor definition file
  InNameSensor : string = ''; // name of sensor data file
  connectedPumpDriver : longint = 0; // ID of the connected pump driver
  connectedSIX : longint = 0; // ID of the connected SIX
  DropfileNameData : string = ''; // name of dropped sensor data file
  const AppearanceFile : string = 'Appearance-JT-DS.ini'; // filename to store appearance
  const AppearanceDefault : string = 'Appearance-JT-DS.default'; // filename with default appearance

implementation

{$R *.lfm}

uses
  PumpControlUnit, SIXControlUnit, Calibration;

procedure TMainForm.FormCreate(Sender: TObject);
var
 iniFile : string;
 FileVerInfo: TFileVersionInfo;
 i : integer;
begin
 try
  FileVerInfo:= TFileVersionInfo.Create(nil);
  FileVerInfo.ReadFileInfo;
  Version:= FileVerInfo.VersionStrings.Values['ProductVersion'];
 finally
  FileVerInfo.Free;
 end;
 MainForm.Caption:= Application.Title + ' ' + Version;
 DefaultFormatSettings.DecimalSeparator:= '.'; // we use English numbers
 SIXControl.NumChannels:= 6; // if no definition file loaded, output 6 channels

 // pump control initializations
 PumpControl.GlobalTime:= 0.0;
 PumpControl.GlobalRepeatTime:= 0.0;
 PumpControl.RepeatTime:= 0.0;
 PumpControl.StepNum:= 7; // number of steps
 PumpControl.PumpNum:= 8; // number of pumps
 PumpControl.PumpNumFile:= 8; // number of pumps defined in a loaded action file
 PumpControl.ValveNum:= 8; // number of valves
 PumpControl.PumpPrefix:= 'Pump: '; // line prefix for action files
 PumpControl.ValvePrefix:= 'Valve: '; // line prefix for action files
 PumpControl.oneDay:= 86400000; // time of one day in ms

 // explicitly set there because the IDE always
 // stores initial values with trailing LineEnding
 LoadedFileSensM.Text:= 'None';
 LoadedActionFileM.Text:= 'None';
 LoadedDefFileM.Text:= 'None';

 EvalTimeFSE.MaxValue:= MaxDouble; // because MaxDouble cannot be set in the form editor
 // load definition file file directly if it was provided via command line
 if ParamStr(1) <> '' then
 begin
  DropfileNameDef:= ParamStr(1);
  LoadDefBBClick(Sender);
  DropfileNameDef:= '';
 end;
 if ParamStr(2) <> '' then
 begin
  DropfileNamePump:= ParamStr(1);
  LoadActionMIClick(Sender);
  DropfileNamePump:= '';
 end;
 if ParamStr(3) <> '' then
 begin
  DropfileNameData:= ParamStr(1);
  LoadSensorDataMIClick(Sender);
  DropfileNameData:= '';
 end;

 // initialize chart transformation before we can draw in chart, otherwise there
 // would be an error when sensor data is load before chart is shown first time
 SIXCH.Prepare;

 // setup the chart
 SIXControl.wasZoomDragged:= false;
 TopLine.Position:= Infinity;
 BottomLine.Position:= -Infinity;
 LeftLine.Position:= -Infinity;
 RightLine.Position:= Infinity;
 // due to a bug in TAChart the preset title size is not taken on
 // high-DPI screens, therefore explicitly set it on start
 SIXCH.Title.Font.Size:= 11;
 ResultCH.Title.Font.Size:= 11;
 // make the bars' DatapointDragtool react only on the bars, not the data points
 LineDragTool.AffectedSeries:= Format('%d;%d;%d;%d',
  [TopLine.Index, BottomLine.Index, LeftLine.Index, RightLine.Index]);

 // set the button to load the .def files as active control
 ActiveControl:= LoadDefBB;

 // load the current chart appearance settings
 // we load from the same folder than the program .exe
 iniFile:= ExtractFilePath(Application.ExeName) + AppearanceFile;
 if FileExists(iniFile) then
  SIXControl.SCLoadAppearance(iniFile)
 else
  // assume there are no valves
  HasNoValvesCB.Checked:= true;

 // more chart settings after the ini file was loaded
 for i:=1 to 8 do
 begin
  // show the marks
  // we must do this here and not in the .lfm file to have an effect
  // (also in the Lazarus forum they don't know why)
  (FindComponent('SIXCh' + IntToStr(i) + 'Values')
   as TLineSeries).Marks.Style:= smsLabel;
  // Turn of AutoMargins for marks because this adds always additional y-axis
  // range, no matter if actually needed. The result is that hen the axis range
  // setting made by the user would be ignored.
  (FindComponent('SIXCh' + IntToStr(i) + 'Values')
   as TLineSeries).Marks.AutoMargins:= false;
 end;

end;

procedure TMainForm.FormClose(Sender: TObject);
var
 command, iniFile : string;
 k : integer;
begin
 // stop SIX reader timer
 ReadTimer.Enabled:= False;
 // stop the pumps and blink 3 times
 command:= '/0I';
 for k:= 1 to PumpControl.PumpNum do
  command:= command + '0';
 command:= command + 'gLM500lM500G2R' + LineEnding;
 if HavePumpSerialCB.Checked then // the user set a COM port
  try
   serPump.SendString(command);
   // purposely don't emit an error that the serial connection is no longer
   // since the program is closed anyway
  finally
   // close connection
   if HavePumpSerialCB.Checked and (serPump.LastError <> 9997) then
   // we cannot close socket or free when the connection timed out
    ClosePumpSerialConn;
  end;
 // close connection to SIX
 if HaveSerialSensorCB.Checked and (serSensor.LastError <> 9997) then
  // we cannot close socket or free when the connection timed out
  CloseLazSerialConn;

 // save the current chart appearance settings
 // we write into the same folder than the program .exe
 iniFile:= ExtractFilePath(Application.ExeName) + AppearanceFile;
 SIXControl.SCSaveAppearance(iniFile);
end;

procedure TMainForm.PumpConnectionMIClick(Sender: TObject);
begin
 PumpConnectionStart(Sender, true); // always call to connect
end;

procedure TMainForm.DisconnectPumpDriver;
var
 command : string;
 i : integer;
begin
 ConnComPortPumpLE.Color:= clHighlight;
 ConnComPortPumpLE.Text:= 'Not connected';
 IndicatorPumpP.Caption:= '';
 IndicatorPumpP.Color:= clDefault;
 IndicatorPumpPPaint;
 AnOutOnOffTB.Checked:= false;
 AnOutOnOffTB.Enabled:= false;
 AnOutOnOffTB.Hint:= 'Outputs the sensor signal' + LineEnding
                     + 'to the pump connectors.' + LineEnding
                     + 'Connect to a SIX and a pump driver'  + LineEnding
                     + 'to enable the button.';
 if RunBB.Hint= 'Calibration is used but no sensor definition file is loaded' then
  RunBB.Hint:= 'Starts the pump action according to the current settings.'
   + LineEnding
   + 'To enable the button you must first connect to the pump driver'
   + LineEnding + 'using the menu ''Connection''';
 // disable all buttons
 RunBB.Enabled:= false;
 StopBB.Enabled:= false;
 if HavePumpSerialCB.Checked then
 begin
  // stop pumps
  command:= '/0I';
  for i:= 1 to PumpControl.PumpNum do
   command:= command + '0';
  // blink 3 times
  command:= command + 'gLM500lM500G2R' + LineEnding;
  serPump.SendString(command);
  ClosePumpSerialConn;
  IndicatorPumpP.Caption:= 'Pumps stopped';
  IndicatorPumpP.Color:= clHighlight;
  IndicatorPumpPPaint;
 end;
end;

procedure TMainForm.PumpConnectionStart(Sender: TObject; Connect: Boolean);
// opens the connection settings dialog and opens a connections according
// to the dialog input
var
 command, COMPort : string;
 Reg : TRegistry;
 i, k, COMNumber, Channel, COMIndex : integer;
 FirmwareNumber : double = 0.0;
 MousePointer : TPoint;
 gotFirmwareNumber : Boolean = false;
 COMArray : array of string;
begin
 // initialize
 MousePointer:= Mouse.CursorPos;
 COMArray:= [''];
 // enable all menus because they would be disabled when formerly
 // connected to an unknown device
 GetFirmwareVersionMI.Enabled:= true;
 FirmwareUpdateMI.Enabled:= true;
 FirmwareResetMI.Enabled:= true;

 if not Connect then
 begin
  DisconnectPumpDriver;
  exit;
 end;

 // determine all possible COM ports
 Reg:= TRegistry.Create;
 try
  Reg.RootKey:= HKEY_LOCAL_MACHINE;
  if Reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') then
  begin
   with SerialUSBSelectionF do
   begin
    SerialUSBPortCB.Items.Clear;
    SerialUSBPortCB.Sorted:= false;
    Reg.GetValueNames(SerialUSBPortCB.Items);
    for i:= 0 to SerialUSBPortCB.Items.Count - 1 do
     SerialUSBPortCB.Items[i]:= Reg.ReadString(SerialUSBPortCB.Items[i]);
    SerialUSBPortCB.Sorted:= true;
   end;
  end;
 finally
  Reg.Free;
 end;

 // scan for pump drivers
 COMPortScan('PumpDriver');

 with SerialUSBSelectionF do
 begin
  // remove all entries that are no pump drivers
  i:= 0;
  While i < SerialUSBPortCB.Items.Count do
  begin
   COMNumber:= StrToInt(Copy(SerialUSBPortCB.Items[i], 4, 4));
   if COMListPumpDriver[COMNumber] < 1 then
    SerialUSBPortCB.Items.Delete(i)
   else
    inc(i);
  end;

  // output driver ID
  SerialUSBPortCB.Sorted:= false;
  SetLength(COMArray, SerialUSBPortCB.Items.Count);
  for i:= 0 to SerialUSBPortCB.Items.Count-1 do
  begin
   COMNumber:= StrToInt(Copy(SerialUSBPortCB.Items[i], 4, 4));
   COMArray[i]:= SerialUSBPortCB.Items[i];
   if COMListPumpDriver[COMNumber] > 1 then
    SerialUSBPortCB.Items[i]:= 'Driver SN ' +
     IntToStr(COMListPumpDriver[COMNumber]);
  end;

  ConnectBB.Enabled:= true;
  SerialUSBPortCB.Text:= '';
  if SerialUSBPortCB.Items.Count = 0 then
  begin
   ConnectBB.Enabled:= false;
   SerialUSBPortCB.Text:= 'No driver found';
  end
  // if there is only one COM port, preselect it
  else if SerialUSBPortCB.Items.Count = 1 then
   SerialUSBPortCB.ItemIndex:= 0
  else
  begin
   // if there is already a connection, display its port
   if HavePumpSerialCB.Checked then
    SerialUSBPortCB.ItemIndex:=
     SerialUSBPortCB.Items.IndexOf(ConnComPortPumpLE.Text)
   else
    SerialUSBPortCB.ItemIndex:= -1;
  end;
  // update the text since this will be displayed
  // as proposal when the connection dialog is shown
  if SerialUSBPortCB.ItemIndex > -1 then
   SerialUSBPortCB.Text:= SerialUSBPortCB.Items[SerialUSBPortCB.ItemIndex];
  if SerialUSBPortCB.Text = '' then
    COMPort:= '';

  if Connect then
  begin
   // open connection dialog
   ShowModal;
   if ModalResult = mrOK then
   begin
    COMPort:= SerialUSBPortCB.Text;
    COMIndex:= SerialUSBPortCB.ItemIndex;
   end;
  end
  else
   ModalResult:= mrNo;

 end; // end with with SerialUSBSelectionF

 if SerialUSBSelectionF.ModalResult = mrNo then // user pressed Cancel
 begin
  DisconnectPumpDriver;
  exit;
 end;

 if COMPort = '' then // user set no COM port or canceled
 begin
  if SerialUSBSelectionF.ModalResult = mrCancel then
   exit; // nothing needs to be done
  MessageDlgPos('Error: No COM port selected.',
   mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
  // disable all buttons
  RunBB.Enabled:= false;
  StopBB.Enabled:= false;
  if HavePumpSerialCB.Checked then
  begin
   // stop pumps
   command:= '/0I';
   for k:= 1 to PumpControl.PumpNum do
    command:= command + '0';
   command:= command + 'gLM500lM500G2R' + LineEnding;
   serPump.SendString(command);
   ClosePumpSerialConn;
   IndicatorPumpP.Caption:= 'Pumps stopped';
   IndicatorPumpP.Color:= clHighlight;
   IndicatorPumpPPaint;
   AnOutOnOffTB.Checked:= false;
   AnOutOnOffTB.Enabled:= false;
   AnOutOnOffTB.Hint:= 'Outputs the sensor signal' + LineEnding
                       + 'to the pump connectors.' + LineEnding
                       + 'Connect to a SIX and a pump driver'  + LineEnding
                       + 'to enable the button.';
  end;
  exit;
 end;

 COMPort:= COMArray[COMIndex];
 // open new connection if not already available
 if not (HavePumpSerialCB.Checked and (COMPort = ConnComPortPumpLE.Text)) then
 try
  if HavePumpSerialCB.Checked then
   ClosePumpSerialConn;
  ConnComPortPumpLE.Color:= clHighlight;
  ConnComPortPumpLEChange;
  AnOutOnOffTB.Checked:= false;
  AnOutOnOffTB.Enabled:= false;
  AnOutOnOffTB.Hint:= 'Outputs the sensor signal' + LineEnding
                      + 'to the pump connectors.' + LineEnding
                      + 'Connect to a SIX and a pump driver'  + LineEnding
                      + 'to enable the button.';
  serPump:= TBlockSerial.Create;
  serPump.DeadlockTimeout:= 3000; //set timeout to 3 s
  serPump.Connect(COMPort);
  // the config must be set after the connection
  serPump.config(9600, 8, 'N', SB1, False, False);
  if serPump.LastError <> 0 then
  begin
   // disable all buttons
   MessageDlgPos(COMPort + ' connection error: '
    + serPump.LastErrorDesc, mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   RunBB.Enabled:= false;
   StopBB.Enabled:= false;
   IndicatorPumpP.Caption:= 'Connection failure';
   IndicatorPumpP.Color:= clRed;
   IndicatorPumpPPaint;
   if serPump.LastError = 9997 then
    exit; // we cannot close socket or free when the connection timed out
   ClosePumpSerialConn;
   exit;
  end;

  // blink 5 times
  command:= '/0gLM500lM500G4R' + LineEnding;
  serPump.SendString(command);
 finally
  if serPump.LastError <> 0 then
  begin
   MessageDlgPos(COMPort + ' error: ' + serPump.LastErrorDesc,
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   ConnComPortPumpLE.Color:= clRed;
   ConnComPortPumpLEChange;
   // disable all buttons
   RunBB.Enabled:= false;
   StopBB.Enabled:= false;
   IndicatorPumpP.Caption:= 'Connection failure';
   IndicatorPumpP.Color:= clRed;
   IndicatorPumpPPaint;
   if serPump.LastError = 9997 then
    exit; // we cannot close socket or free when the connection timed out
   ClosePumpSerialConn;
   exit;
  end;
  HavePumpSerialCB.Checked:= True;
  // output connected port
  ConnComPortPumpLE.Color:= clDefault;
  ConnComPortPumpLE.Text:= SerialUSBSelectionF.SerialUSBPortCB.Text;
  connectedPumpName:= SerialUSBSelectionF.SerialUSBPortCB.Text;
  Channel:= StrToInt(Copy(COMPort, 4, 4));
  connectedPumpDriver:= COMListPumpDriver[Channel];
  IndicatorPumpP.Caption:= 'Connection successful';
  IndicatorPumpP.Color:= clDefault;
  IndicatorPumpPPaint;
  // no matter if the firmware might be the right one, we can allow to save and
  // load action files
  LoadActionMI.Enabled:= True;
  SaveActionMI.Enabled:= True;
  // get Firmware version
  try
   FirmwareVersion:= serPump.Recvstring(1000);
  finally
   if serPump.LastError <> 0 then
   begin
    MessageDlgPos(COMPort + ' error on reading firmware version: '
     + serPump.LastErrorDesc, mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    ConnComPortPumpLE.Color:= clRed;
    ConnComPortPumpLEChange;
    IndicatorPumpP.Caption:= 'Wrong device';
    IndicatorPumpP.Color:= clRed;
    IndicatorPumpPPaint;
    GetFirmwareVersionMI.Enabled:= false;
    FirmwareUpdateMI.Enabled:= false;
    FirmwareResetMI.Enabled:= false;
    // disable all buttons
    RunBB.Enabled:= false;
    StopBB.Enabled:= false;
    if serPump.LastError = 9997 then
     exit; // we cannot close socket or free when the connection timed out
    ClosePumpSerialConn;
    exit;
   end;
   // FirmwareVersion has now this format:
   // "JT-PumpDriver-Firmware x.y\n Received command: ..."
   // but on old versions the firmware does not have any number,
   // only "received command" is sent back
   // therefore check for a number dot
   if Pos('JT-PumpDriver-Firmware', FirmwareVersion) > 0 then
    FirmwareVersion:= copy(FirmwareVersion, Pos('.', FirmwareVersion) - 1, 3)
   // omit the 'r' because some versions used a capital letter 'R'
   else if Pos('eceived command:', FirmwareVersion) > 0 then
    FirmwareVersion:= 'unknown'
   else
   begin
    MessageDlgPos('Not connected to a supported pump driver.',
     mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    IndicatorPumpP.Caption:= 'Wrong device';
    IndicatorPumpP.Color:= clRed;
    IndicatorPumpPPaint;
    ConnComPortPumpLE.Color:= clRed;
    ConnComPortPumpLEChange;
    ClosePumpSerialConn;
    exit;
   end;
   // JT Pump Driver requires a certain firmware version
   if FirmwareVersion = 'unknown' then
   begin
    MessageDlgPos('JT Pump Driver ' + Version + ' requires firmware version '
     + FloatToStr(RequiredFirmwareVersion) + ' or newer!'
     + LineEnding + 'You have an unknown old firmware version installed.'
     + LineEnding + 'Please use the menu Miscellaneous -> Firmware Update.',
     mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    ClosePumpSerialConn;
    IndicatorPumpP.Caption:= 'Firmware too old';
    IndicatorPumpP.Color:= clRed;
    IndicatorPumpPPaint;
    exit;
   end;
   // when the USB connection got lost, the software is sometimes in a state
   // that Windows set the number format back to Windows' default
   // therefore set here explicitly the number format again
   DefaultFormatSettings.DecimalSeparator:= '.'; // we use English numbers
   gotFirmwareNumber:= TryStrToFloat(FirmwareVersion, FirmwareNumber);

   if (gotFirmwareNumber and (FirmwareNumber < RequiredFirmwareVersion))
    or (not gotFirmwareNumber) then
   begin
    MessageDlgPos('JT Pump Driver ' + Version + ' requires firmware version '
     + FloatToStr(RequiredFirmwareVersion) + ' or newer!'
     + LineEnding + 'You have firmware version ' + FirmwareVersion + ' installed.'
     + LineEnding + 'Please use the menu Miscellaneous -> Firmware Update.',
     mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    ClosePumpSerialConn;
    IndicatorPumpP.Caption:= 'Firmware too old';
    IndicatorPumpP.Color:= clRed;
    IndicatorPumpPPaint;
    exit;
   end;
   // enable all buttons
   // don't allow to run, if calibration is used but no .def file is used
   if not (UseCalibCB.Checked and (not HaveDefFileCB.Checked)) then
    RunBB.Enabled:= true
   else
    RunBB.Hint:= 'Calibration is used but no sensor definition file is loaded';
   StopBB.Enabled:= true;
   // enable analog output when also connected to a pump driver
   if ConnComPortSensM.Color = clDefault then
   begin
    AnOutOnOffTB.Enabled:= true;
    AnOutOnOffTB.Hint:= 'Outputs the sensor signal' + LineEnding
                        + 'to the pump connectors';
   end;
  end; //end inner finally
 end; //end outer finally
end;

procedure TMainForm.FirmwareUpdateMIClick(Sender: TObject);
begin
 FirmwareUpdate(false); // no forced update
end;

procedure TMainForm.FirmwareResetMIClick(Sender: TObject);
begin
 FirmwareUpdate(true); // forced update
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 // if connected to SIX, ask
 if HaveSerialSensorCB.Checked then
 begin
  if MessageDlg('A SIX is connected, do you really want to close?',
                mtConfirmation, [mbYes]+[mbNo], 0, mbNo) = mrNo then
   CanClose:= False
  else
   CanClose:= True;
 end;
 // if action is running, ask
 if OverallTimer.Enabled then
 begin
  if MessageDlg('An action is currently running, do you really want to close?',
                mtConfirmation, [mbYes]+[mbNo], 0, mbNo) = mrNo then
   CanClose:= False
  else
   CanClose:= True;
 end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
 // There is an issue that on smaller high-DPI screens the bottom distance of
 // MainForm to Main PC is too large. Therefore reset the desired ratio on start.
 MainPC.Height:= round(MainForm.Height * 0.9595);
end;

procedure TMainForm.CLBAddSeries(ASender: TChartListbox;
  ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
begin
 CalibrationF.CalibCLBAddSeries(ASender, ASeries, AItems, ASkip);
end;

procedure TMainForm.CalibValueFSEChange(Sender: TObject);
begin
 PumpControl.PCCalibValueFSEChange(Sender);
end;

procedure TMainForm.CalibCLBItemClick(ASender: TObject; AIndex: Integer);
begin
 // we can allow to run the pumps since at least one series is selected
 // for calibration
 RunBB.Enabled:= (HavePumpSerialCB.Checked or HasNoPumpsCB.Checked);
end;

procedure TMainForm.HasNoPumpsCBChange(Sender: TObject);
begin
 PumpControl.PCHasNoPumpsCBChange(Sender);
end;

procedure TMainForm.HasNoValvesCBChange(Sender: TObject);
begin
 PumpControl.PCHasNoValvesCBChange(Sender);
end;

procedure TMainForm.HavePumpSerialCBChange(Sender: TObject);
begin
 PumpControl.PCHavePumpSerialCBChange(Sender);
end;

procedure TMainForm.FirmwareUpdate(forced: Boolean);
// flashes the program cache in the TinyZero controller with a new firmware
var
 COMListStart, COMListBoot : TStringList;
 Reg : TRegistry;
 BootCOM, BossacOut, FirmwareFile, bossacPath, command,
   COMPort, driverFeedback : string;
 i, YesNo : integer;
 MousePointer : TPoint;
 exited : Boolean = false;
begin
 { the flashing works the following way:
 1. Closing the connection
 2. Reopening the connection bit with 1200 baud.
 This sets the controller to boot mode and gives the controller a new COM
  port number, therefore
 3. Wait util the new COM port is available
 4. Execute the bossac.exe to send the firmware
 This will chane the COM port again.
 5. Reconnect to the new COM port with normal baud rate   }

 MousePointer:= Mouse.CursorPos; // store mouse position

 // at first check if the bossac.exe is in the same folder than the executable
 // get path to the boaasc.exe which is the same as the application
 bossacPath:= ExtractFileDir(Application.ExeName);
 bossacPath:= bossacPath + '\bossac.exe';
 if not FileExists(bossacPath) then
 begin
  MessageDlgPos('The file "bossac.exe" is not in the same folder as this program.'
   + LineEnding + 'No firmware update possible.',
   mtError, [mbOK], 0 , MousePointer.X, MousePointer.Y);
  exit;
 end;

 // basic info
 MessageDlgPos('Specify now the COM port of the pump driver' + LineEnding
  + 'and select then the firmware file.',
  mtInformation, [mbOK], 0, MousePointer.X, MousePointer.Y);

 // disable all buttons
 RunBB.Enabled:= false;
 StopBB.Enabled:= false;
 ConnComPortPumpLE.Color:= clHighlight;
 ConnComPortPumpLE.Text:= 'Not connected';
 IndicatorPumpP.Caption:= '';
 IndicatorPumpP.Color:= clDefault;
 IndicatorPumpPPaint;

 try // to free finally the TStringLists
  // determine all possible COM ports
  COMListStart:= TStringList.Create;
  COMListBoot:= TStringList.Create;
  Reg:= TRegistry.Create;
  try
   Reg.RootKey:= HKEY_LOCAL_MACHINE;
   if Reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') then
   begin
    with SerialUSBSelectionF do
    begin
     SerialUSBPortCB.Text:= '';
     SerialUSBPortCB.Items.Clear;
     SerialUSBPortCB.Sorted:= false;
     Reg.GetValueNames(SerialUSBPortCB.Items);
     for i:= 0 to SerialUSBPortCB.Items.Count - 1 do
     begin
      SerialUSBPortCB.Items[i]:= Reg.ReadString(SerialUSBPortCB.Items[i]);
      // store the list
      COMListStart.Add(SerialUSBPortCB.Items[i]);
     end;
     SerialUSBPortCB.Sorted:= true;
    end;
   end;
  finally
   Reg.Free;
  end;

  // open connection dialog
  SerialUSBSelectionF.ShowModal;
  if SerialUSBSelectionF.ModalResult = mrCancel then
   exit;
  if SerialUSBSelectionF.ModalResult = mrOK then
   COMPort:= SerialUSBSelectionF.SerialUSBPortCB.Text;
  if SerialUSBSelectionF.ModalResult = mrNo then
  begin
   MessageDlgPos('No connection, no firmware update possible.',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   exit;
  end;
  if (COMPort = '') then // user forgot to set a COM port
  begin
   MessageDlgPos('Error: No COM port selected, no firmware update possible.',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   exit;
  end;
  // check if we are connected to a pump driver
  try
   // for some odd reason not all pump driver output gets received,
   // therefore establish a new connection
   ClosePumpSerialConn;
   serPump:= TBlockSerial.Create;
   serPump.DeadlockTimeout:= 5000; //set timeout to 5 s
   serPump.Connect(COMPort);
   serPump.config(9600, 8, 'N', SB1, False, False);
   if not forced then
   begin
    // send now a simple command to get the firmware version back
    // blink 1 time
    command:= '/0LM500lM500R' + LineEnding;
    serPump.SendString(command);
    // receive firmware version
    FirmwareVersion:= serPump.Recvstring(1000);
   end;
  finally
   if serPump.LastError <> 0 then
   begin
    MessageDlgPos(COMPort + ' error: ' + serPump.LastErrorDesc,
     mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    ConnComPortPumpLE.Color:= clRed;
    ConnComPortPumpLEChange;
    IndicatorPumpP.Caption:= 'Connection error';
    IndicatorPumpP.Color:= clRed;
    IndicatorPumpPPaint;
    if serPump.LastError = 9997 then
    begin
     exited:= true;
     exit; // we cannot close socket or free when the connection timed out
    end;
    MessageDlgPos('The selected COM port is not one of a pump driver!',
     mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    ClosePumpSerialConn;
    exited:= true;
    exit;
   end;
   // in case of successful data exchange but not a pump driver
   if (Pos('eceived command:', FirmwareVersion) = 0) and (not forced) then
   // (omit the 'r' because some versions used a capital letter 'R')
   begin
    MessageDlgPos('The selected COM port is not the one of a pump driver!',
     mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    IndicatorPumpP.Caption:= 'Wrong device';
    IndicatorPumpP.Color:= clRed;
    IndicatorPumpPPaint;
   end;
  end;

  // if connected to wrong device, exit; only jumps out of try..finally block
  if exited then
   exit;

  HavePumpSerialCB.Checked:= True;
  // allow the user to flush the device anyway
  if forced then
  begin
   with CreateMessageDialog // MessageDlg
       ('Do you really want to force the firmware update anyway on your own risk (guarantee void)?'
        + LineEnding +
        'NOTE: Assure that then no other device is connected to a COM port!',
             mtWarning, [mbYes]+[mbNo]) do
   try
    ActiveControl:= FindComponent('NO') as TWinControl;
    YesNo:= ShowModal;
   finally
    Free;
   end;
   if YesNo = mrNo then // if No
    exit;
  end;
  // open the firmware binary file
  if FirmwareFileDialog.Execute then
  begin
   FirmwareFile := FirmwareFileDialog.Filename;
   if not FileExists(FirmwareFile) then
   begin
    MessageDlgPos('Selected file does not exist, no firmware update possible.',
     mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    exit;
   end;
  end
  else
  begin
   MessageDlgPos('No firmware file selected, no firmware update possible.',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   exit;
  end;

  // Closing open connections
  // we don't call ClosePumpSerialConn because we need the COMPort info
  if HavePumpSerialCB.Checked then
  begin
   serPump.CloseSocket;
   serPump.Free;
   HavePumpSerialCB.Checked:= False;
  end;

  // open new connection with 1200 baud,
  // this rate is mandatory to set the Arduino into boot mode
  try
   serPump:= TBlockSerial.Create;
   serPump.DeadlockTimeout:= 5000; //set timeout to 5 s
   serPump.Connect(COMPort);
   serPump.config(1200, 8, 'N', SB1, False, False);
  except
   MessageDlgPos('Error: A connection to ' + COMPort + ' cannot be opened.',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   exit;
  end;
  // since the process will need more than 10 seconds, show a note
  // at the position where the initial info message was output
  if forced then
  begin
   InfoNote.Text:= 'Firmware reset is in progress';
   InfoNote.Title:= 'Firmware reset';
  end
  else
  begin
   InfoNote.Text:= 'Firmware update is in progress';
   InfoNote.Title:= 'Firmware update';
  end;
  InfoNote.ShowAtPos(MousePointer.X, MousePointer.Y);
  Application.ProcessMessages; // to show the note before going to delay
  Delay(2000); // some time until the connection is in every case established
  Application.ProcessMessages; // keep the program alive to Windows
  // Close the connection
  try
   serPump.CloseSocket;
   serPump.Free;
  except
   MessageDlgPos('Error: ' + COMPort + ' cannot be closed.',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   exit;
  end;
  // now the microcontroller gets to boot mode and establishes two times a
  // connection with Windows. This needs several seconds:
  Application.ProcessMessages;
  Delay(4000);
  Application.ProcessMessages;

  // we need to find out what the new COM port is
  // The number of COM ports have not changed, so we can just read out the
  // current list and compare
  Reg:= TRegistry.Create;
  try
   Reg.RootKey:= HKEY_LOCAL_MACHINE;
   if Reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') then
   begin
    Reg.GetValueNames(COMListBoot);
    for i:= 0 to COMListBoot.Count - 1 do
     COMListBoot[i]:= Reg.ReadString(COMListBoot[i]);
   end;
  finally
   Reg.Free;
  end;
  // compare with prior list
  BootCOM:= 'x';
  for i:= 0 to COMListBoot.Count - 1 do
   if COMListBoot[i] <> COMListStart[i] then
   begin
    BootCOM:= COMListBoot[i];
    break;
   end;
  if BootCOM = 'x' then
   BootCOM:= COMPort;

  // upload the new firmware
  RunCommand(bossacPath,
   ['-i', '-d', '-p' , BootCOM, '-U', 'true', '-e', '-w', '-v', FirmwareFile, '-R'],
   BossacOut, [poNoConsole]);

  Application.ProcessMessages;
  Delay(5000); // wait 5s because the microcontroller needs time
  Application.ProcessMessages;
  InfoNote.Hide; // hide the note

  // the COM port might now be different than at the start
  Reg:= TRegistry.Create;
  try
   Reg.RootKey:= HKEY_LOCAL_MACHINE;
   if Reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') then
   begin
    Reg.GetValueNames(COMListBoot);
    for i:= 0 to COMListBoot.Count - 1 do
     COMListBoot[i]:= Reg.ReadString(COMListBoot[i]);
   end;
  finally
   Reg.Free;
  end;
  // compare with start COM list
  BootCOM:= 'x';
  for i:= 0 to COMListBoot.Count - 1 do
   if COMListBoot[i] <> COMListStart[i] then
   begin
    BootCOM:= COMListBoot[i];
    break;
   end;
  if BootCOM = 'x' then
   BootCOM:= COMPort;

  // reconnect
  try
   serPump:= TBlockSerial.Create;
   serPump.DeadlockTimeout:= 5000; //set timeout to 5 s
   serPump.Connect(BootCOM);
   serPump.config(9600, 8, 'N', SB1, False, False);
   // send now a simple command to get the firmware version back
   // blink 1 time
   command:= '/0LM500lM500R' + LineEnding;
   serPump.SendString(command);
   // receive firmware version
   driverFeedback:= serPump.Recvstring(1000);
  finally
   if serPump.LastError <> 0 then
   begin
    MessageDlgPos(BootCOM + ' error: ' + serPump.LastErrorDesc,
     mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    ConnComPortPumpLE.Color:= clRed;
    ConnComPortPumpLEChange;
    if serPump.LastError = 9997 then
     exit; // we cannot close socket or free when the connection timed out
    ClosePumpSerialConn;
    exit;
   end;
   if Pos('JT-PumpDriver-Firmware', driverFeedback) > 0 then
    FirmwareVersion:= copy(driverFeedback, Pos('.', driverFeedback) - 1, 3)
   else
    FirmwareVersion:= 'unknown';
   // output connected port
   // determine the driver ID: from first space to first #10
   // (driver uses only #10 for the line ending)
   i:= Length('JT-PumpDriver-ID');
   if copy(driverFeedback, 0, i) = 'JT-PumpDriver-ID' then
   begin
    driverFeedback:= copy(driverFeedback, i + 2,
                          (Pos(#10, driverFeedback) - 1) - (i + 1));
    // get rid of leading zeros by a str - int back and forth conversion
    ConnComPortPumpLE.Text:= 'Driver SN ' + IntToStr(StrToInt(driverFeedback));
   end
   else
    ConnComPortPumpLE.Text:= BootCOM;
   ConnComPortPumpLE.Color:= clDefault;
   IndicatorPumpP.Caption:= 'Firmware updated';
   HavePumpSerialCB.Checked:= True;
   // inform the user
   if (AnsiContainsStr(BossacOut, 'Verify successful'))
     and (FirmwareVersion <> 'unknown') then
    MessageDlgPos('The firmware has been updated sucessfully to version '
     + FirmwareVersion + '.', mtInformation, [mbOK], 0, MousePointer.X,
     MousePointer.Y)
   else if FirmwareVersion = 'unknown' then
    begin
     MessageDlgPos('The firmware has been updated sucessfully but to an unknown'
      + LineEnding + 'old version that is not supported by JT Pump Driver '
      + Version + '.' + LineEnding + 'JT Pump Driver ' + Version
      + ' requires firmware version ' + FloatToStr(RequiredFirmwareVersion)
      + ' or newer!' , mtInformation, [mbOK], 0, MousePointer.X, MousePointer.Y);
     IndicatorPumpP.Caption:= 'Firmware too old';
     IndicatorPumpP.Color:= clRed;
     IndicatorPumpPPaint;
     exit;
    end
   else
   begin
    MessageDlgPos('The firmware could not be updated sucessfully.' + LineEnding
     + 'Here is the full output of the failed firmware update attempt:' + LineEnding
     + BossacOut, mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    exit;
   end;
   if StrToFloat(FirmwareVersion) < RequiredFirmwareVersion then
   begin
    MessageDlgPos('JT Pump Driver ' + Version + ' requires firmware version '
     + FloatToStr(RequiredFirmwareVersion) + ' or newer!'
     + LineEnding + 'You have firmware version ' + FirmwareVersion + ' installed.'
     + LineEnding + 'Please use the menu Miscellaneous -> Firmware Update.',
     mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
     IndicatorPumpP.Caption:= 'Firmware too old';
     IndicatorPumpP.Color:= clRed;
     IndicatorPumpPPaint;
    exit;
   end;
   // enable all buttons
   RunBB.Enabled:= true;
   StopBB.Enabled:= true;
   IndicatorPumpP.Color:= clDefault;
   IndicatorPumpPPaint;
  end;

 finally
  FreeAndNil(COMListStart);
  FreeAndNil(COMListBoot);
 end;
end;

function TMainForm.DialogWithPos(const Message: string; DialogType: TMsgDlgType;
           Buttons: TMsgDlgButtons; AX, AY: Integer): TModalResult;
// creates a dialog that will appear with its upper left edge
// at the current mouse position
var
  MessageForm: TForm;
begin
 MessageForm:= CreateMessageDialog(Message, DialogType, Buttons);
 try
   MessageForm.FormStyle:= fsStayOnTop;
   MessageForm.Position:= poDefaultSizeOnly;
   MessageForm.Left:= AX;
   MessageForm.Top:= AY;
   Result:= MessageForm.ShowModal;
 finally
   MessageForm.Free
 end;
end;

procedure TMainForm.GetFirmwareVersionMIClick(Sender: TObject);
// reads the firmware version from the pump driver board
var
 MousePointer : TPoint;
begin
 MousePointer:= Mouse.CursorPos; // store mouse position

 if not HavePumpSerialCB.Checked then // connect first
  PumpConnectionMIClick(Sender);

 if FirmwareVersion <> 'unknown' then
 begin
  MessageDlgPos('Firmware version: ' + FirmwareVersion,
   mtInformation, [mbOK], 0, MousePointer.X, MousePointer.Y)
 end
 else
 begin
  MessageDlgPos('Error: No connection to a pump driver',
   mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
  exit;
 end;
end;

procedure TMainForm.AboutMIClick(Sender: TObject);
begin
 with AboutFormF do
 begin
  // set version number
  NameL.Caption:= Application.Title + ' version ';
  VersionNumberL.Caption:= Version;
  Caption:= Application.Title;
  // open the dialog
  ShowModal;
 end;
end;

procedure TMainForm.AbortCalibrationMIClick(Sender: TObject);
begin
 SIXControl.SCCalibrateTBChange(Sender, true);
 AbortCalibrationMI.Visible:= false;
 Separator3MI.Visible:= false;
end;

procedure TMainForm.AnOutConnectorXOnOffCBChange(Sender: TObject);
begin
 SIXControl.SCAnOutConnectorXOnOffCBChange(Sender);
end;

procedure TMainForm.AppearanceXBBClick(Sender: TObject);
begin
 SIXControl.SCAppearanceXBBClick(Sender);
end;

procedure TMainForm.AutoscaleMIClick(Sender: TObject);
begin
 SIXControl.SCAutoscaleMIClick(Sender);
end;

procedure TMainForm.CalibrateTBChange(Sender: TObject);
begin
 SIXControl.SCCalibrateTBChange(Sender);
end;

procedure TMainForm.ChangeBackColorMIClick(Sender: TObject);
begin
 SIXControl.SCChangeBackColorMIClick(Sender);
end;

procedure TMainForm.ChangeSensFileMIClick(Sender: TObject);
begin
 // restart aquisition with a new file
 SIXBiosensorsStart(true, false);
end;

procedure TMainForm.ChannelXLEChange(Sender: TObject);
begin
 SIXControl.SCChannelXLEChange(Sender);
end;

procedure TMainForm.ChannelXCBChange(Sender: TObject);
begin
 SIXControl.SCChannelXCBChange(Sender);
end;

procedure TMainForm.ChanAnOutConnectorXOnOffCBChange(Sender: TObject);
begin
 SIXControl.SCChannelXOnOffCBChange(Sender);
end;

procedure TMainForm.ShowTempCBChange(Sender: TObject);
begin
 SIXControl.SCShowTempCBChange(Sender);
end;

procedure TMainForm.AxisClickToolClick(Sender: TChartTool;
  Axis: TChartAxis; HitInfo: TChartAxisHitTests);
begin
 SIXControl.SCAxisClickToolClick(Sender, Axis, HitInfo);
end;

procedure TMainForm.DataPointClickToolPointClick(ATool: TChartTool;
  APoint: TPoint);
begin
 SIXControl.SCDataPointClickToolPointClick(ATool, APoint);
end;

procedure TMainForm.DataPointHintToolHint(
  ATool: TDataPointHintTool; const APoint: TPoint; var AHint: String);
begin
 SIXControl.SCDataPointHintToolHint(ATool, APoint, AHint);
end;

procedure TMainForm.DataPointHintToolHintPosition(
 ATool: TDataPointHintTool; var APoint: TPoint);
// moves the hint text above the cursor and center it horizontally to cursor
begin
 SIXControl.SCDataPointHintToolHintPosition(ATool, APoint);
end;

procedure TMainForm.SubstMeasureCLBItemClick(ASender: TObject; AIndex: Integer);
begin
 SIXControl.SubstMeasureCLBItemClick(ASender, AIndex);
end;

procedure TMainForm.HideNotesMIClick(Sender: TObject);
begin
 SIXControl.SCHideNotesMIClick(Sender);
end;

procedure TMainForm.OverallTimerStartTimer(Sender: TObject);
begin
 PumpControl.PCOverallTimerStartTimer(Sender);
end;

procedure TMainForm.HaveDefFileCBChange(Sender: TObject);
var
 hasLoadedSensorData : Boolean;
begin
 // if we have something in the chart but no connection to the SIX,
 // we have a data file loaded
 hasLoadedSensorData:= ((SIXCh1Values.LastValueIndex > 0)
                        and (not HaveSerialSensorCB.Checked));

 LoadedDefFileM.ShowHint:= HaveDefFileCB.Checked;
 LoadOtherDefBB.Enabled:= HaveDefFileCB.Checked;
 UnloadDefBB.Enabled:= HaveDefFileCB.Checked;
 CalibrateTB.Enabled:= HaveDefFileCB.Checked;
 CalibrationGB.Enabled:= HaveDefFileCB.Checked;

 if HaveDefFileCB.Checked then
 begin
  if not HaveSerialSensorCB.Checked then
  begin
   IndicatorSensorP.Color:= clDefault;
   IndicatorSensorP.Caption:= '';
  end;
  SIXBiosensorsMI.Enabled:= true;
  SIXConnectBB.Enabled:= true;
  UseAnOutCB.Enabled:= true;
  RawCurrentCB.Enabled:= true;
  if HaveSerialSensorCB.Checked or (not hasLoadedSensorData) then
  begin
   NoTempCorrectionCB.Enabled:= true;
   NoSubtractBlankCB.Enabled:= true;
  end;
  CalibrationGB.Hint:= '';
  if not RunBB.Enabled then
   RunBB.Enabled:= (HavePumpSerialCB.Checked or HasNoPumpsCB.Checked);
  RunBB.Hint:= 'Starts the pump action according to the current settings.'
    + LineEnding + 'If button is disabled you must first start a SIX measurement.';
 end
 else
 begin
  // only change indicator if no measurement is running because it is
  // more important that the user sees that a measurement is running
  if not HaveSerialSensorCB.Checked then
  begin
   IndicatorSensorP.Color:= clHighlight;
   IndicatorSensorP.Caption:= 'No definition file loaded';
  end;
  LoadedDefFileM.Text:= 'None';
  if (HaveSerialSensorCB.Checked) or (hasLoadedSensorData) then
  begin
   // the values are then in nA
   RawCurrentCB.Checked:= true;
   RawCurrentCB.Enabled:= false;
   // temperature connection is not possible
   NoTempCorrectionCB.Checked:= false;
   NoTempCorrectionCB.Enabled:= false;
   // subtracting blanks not possible because blanks are unknown
   NoSubtractBlankCB.Checked:= false;
   NoSubtractBlankCB.Enabled:= false;
  end;

  // there might be a calibration
  if UseCalibCB.Checked then
  begin
   RunBB.Hint:= 'Calibration is used but no sensor definition file is loaded';
   RunBB.Enabled:= false;
  end;
  CalibrationGB.Hint:= 'Calibration is only possible if a' + LineEnding
                       + 'sensor definition file is loaded';
 end;

 // only for a successful reconnection there is a hint
 // therefore we can always delete it here
 IndicatorSensorP.Hint:= '';
end;

procedure TMainForm.LegendClickToolClick(Sender: TChartTool;
  Legend: TChartLegend);
begin
 SIXControl.SCLegendClickToolClick(Sender, Legend)
end;

procedure TMainForm.SIXCHAxisList1GetMarkText(Sender: TObject;
  var AText: String; AMark: Double);
begin
 SIXControl.SCSIXCHAxisList1GetMarkText(Sender, AText, AMark);
end;

procedure TMainForm.TitleFootClickToolClick(Sender: TChartTool;
  Title: TChartTitle);
begin
 SIXControl.SCTitleFootClickToolClick(Sender, Title);
end;

procedure TMainForm.ZoomDragToolAfterMouseUp(ATool: TChartTool;
  APoint: TPoint);
begin
 SIXControl.SCZoomDragToolAfterMouseUp(ATool, APoint);
end;

procedure TMainForm.DutyCycleXFSEChange(Sender: TObject);
begin
 PumpControl.PCDutyCycleXFSEChange(Sender);
end;

procedure TMainForm.EvalTimeFSEChange(Sender: TObject);
begin
 SIXControl.evalTimeChanged:= true;
 // change the chart scrolling time so that at least 3 data points are visible
 ScrollIntervalFSE.MinValue:= EvalTimeFSE.Value * 3.99 / 60;
end;

procedure TMainForm.LoadDefBBClick(Sender: TObject);
var
 ParseSuccess : Boolean = false;
 MousePointer : TPoint;
 DummyString : string = '';
 HeaderLine : string = '';
 i, j, diff, NumChannelsPrev : integer;
begin
 MousePointer:= Mouse.CursorPos; // store mouse position

 if DropfileNameDef = '' then // no file was dropped into the General tab
 begin
  OpenDialog.InitialDir:= '';
  DummyString:= OpenHandling('', '.def'); // opens file dialog
  if (DummyString = '') and (InNameDef = '') then
  begin
   // user aborted the loading
   HaveDefFileCB.Checked:= false;
   exit;
  end
  else if (DummyString = '') and (InNameDef <> '') then
   // we keep the already loaded file and do nothing
   exit;
 end;

 if DropfileNameDef <> '' then
  InNameDef:= DropfileNameDef
 else
  InNameDef:= DummyString;

 // the previous .def file might have had less channels defined
 // to calculate later the slopes we must therefore use for the next readout
 // still the old channel nummber and thus know this previous value
 NumChannelsPrev:= SIXControl.NumChannels;

 // parse the file
 ParseSuccess:= SIXControl.ParseDefFile(InNameDef);
 if not ParseSuccess then
 begin
  MessageDlgPos('Invalid definition file',
   mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
  HaveDefFileCB.Checked:= false;
  exit;
 end;

 // for the case a def file was loaded without a connection to the SIX or when
 // the SIX type was changed, we need to (re-)initialize the raw gains
 SetSIXFactors;

 // display file name without suffix
 DummyString:= ExtractFileName(InNameDef);
 SetLength(DummyString, Length(DummyString) - 4);
 // show full path as tooltip
 LoadedDefFileM.ShowHint:= true;
 LoadedDefFileM.Hint:= InNameDef;
 // set Text after Hint since this change triggers the sync with the other tabs
 LoadedDefFileM.Text:= DummyString;

 // the previous .def file might have had less channels defined
 // to calculate later the slopes fill the missing x-values of the new channels
 // with zeroes as y-values
 diff:= SIXControl.NumChannels - NumChannelsPrev;
 // if channel 1 is empty, there is nothing to do
 if (diff > 0) and (SIXCh1Values.Count > 0) then
 begin
  for i:= 1 to diff do
  begin
   for j:= (FindComponent('SIXCh' + IntToStr(7-i) + 'Values')
           as TLineSeries).Count to SIXCh1Values.Count-1 do
    // since channel 1 always exists, we can take its x-values
    (FindComponent('SIXCh' + IntToStr(7-i) + 'Values')
     as TLineSeries).AddXY(SIXCh1Values.XValue[j], 0);
  end;
 end;

 // setup UI to start
 HaveDefFileCB.Checked:= true;

 // update chart legend according to channel names
 for i:= 1 to SIXControl.NumChannels do
 begin
  (FindComponent('SIXCh' + IntToStr(i) + 'Values')
   as TLineSeries).Title:=
    (FindComponent('Channel' + IntToStr(i) + 'GB')
     as TGroupBox).Caption;
  (FindComponent('SIXCh' + IntToStr(i) + 'Results')
   as TLineSeries).Title:=
    'Stable ' + (FindComponent('Channel' + IntToStr(i) + 'GB')
     as TGroupBox).Caption;
 end;
 // the channel operations might show the old channel name, thus update them
 for i:= 7 to 8 do
 begin
  if (FindComponent('Channel' + IntToStr(i) + 'CB')
     as TComboBox).Text = 'mean(#2, #5)' then
   (FindComponent('SIXCh' + IntToStr(i) + 'Values') as TLineSeries).Title:=
    'Mean (' + Channel2GB.Caption + ', ' + Channel5GB.Caption + ')'
  else if (FindComponent('Channel' + IntToStr(i) + 'CB')
     as TComboBox).Text = 'mean(#3, #6)' then
   (FindComponent('SIXCh' + IntToStr(i) + 'Values') as TLineSeries).Title:=
    'Mean (' + Channel3GB.Caption + ', ' + Channel6GB.Caption + ')'
  else if (FindComponent('Channel' + IntToStr(i) + 'CB')
     as TComboBox).Text = 'mean(#1, #4)' then
   (FindComponent('SIXCh' + IntToStr(i) + 'Values') as TLineSeries).Title:=
    'Mean (' + Channel1GB.Caption + ', ' + Channel4GB.Caption + ')';
  // we use the same legend name for Live and Result charts
  (FindComponent('SIXCh' + IntToStr(i) + 'Results') as TLineSeries).Title:=
   (FindComponent('SIXCh' + IntToStr(i) + 'Values') as TLineSeries).Title;
 end;

 // determine what are the blank channels
 for i:= 1 to 6 do
 begin
  if (Pos('Blank', SIXControl.HeaderStrings[i]) <> 0)
   or (Pos('blank', SIXControl.HeaderStrings[i]) <> 0) then
  begin
   SIXControl.isBlank[i]:= true;
   // don't show the blank channels by default
   (FindComponent('Channel' + IntToStr(i) + 'OnOffCB')
    as TCheckBox).Checked:= false;
   // change caption
   (FindComponent('CurrChannel' + IntToStr(i) + 'LE')
   as TLabeledEdit).EditLabel.Caption:= 'Actual Signal [nA]';
  end
  else
  begin
   SIXControl.isBlank[i]:= false;
   // change caption because it might have been a blank in previous def file
   (FindComponent('CurrChannel' + IntToStr(i) + 'LE')
   as TLabeledEdit).EditLabel.Caption:= 'Actual Signal [mM]';
  end;
 end;

 // write a new header line to the output file
 if HaveSensorFileStream then
 begin
  HeaderLine:= HeaderLine + 'Used definition file: "' + LoadedDefFileM.Text +
   '.def"' + LineEnding;
  HeaderLine:= HeaderLine + 'Counter' + #9 + 'Time [min]' + #9;
  // output all non-blank channels
  for i:= 1 to SIXControl.NumChannels do
   if not SIXControl.isBlank[i] then
    HeaderLine:= HeaderLine + SIXControl.HeaderStrings[i] + ' [mM]' + #9;
  HeaderLine:= HeaderLine + 'Temp [deg C]' + #9;
  // for the raw values
  for i:= 1 to SIXControl.NumChannels do
   HeaderLine:= HeaderLine + SIXControl.HeaderStrings[i] + ' [nA]' + #9;
  HeaderLine:= HeaderLine + LineEnding;
  // write line
  SensorFileStream.Write(HeaderLine[1], Length(HeaderLine));
 end;

 // assure that at least one channel is displayed in the chart
 j:= 0;
 for i:= 1 to SIXControl.NumChannels do
 begin
  if (FindComponent('Channel' + IntToStr(i) + 'OnOffCB')
     as TCheckBox).Checked then
   inc(j);
 end;
 for i:= 7 to 8 do
 begin
  if (FindComponent('Channel' + IntToStr(i) + 'OnOffCB')
     as TCheckBox).Checked then
   inc(j);
 end;
 if j = 0 then // no channel is on
 begin
  for i:= 1 to SIXControl.NumChannels do
   if not SIXControl.isBlank[i] then
   begin
    (FindComponent('Channel' + IntToStr(i) + 'OnOffCB')
     as TCheckBox).Checked:= true;
    exit; // just one channel
   end;
 end;

end;

procedure TMainForm.UnloadDefBBClick(Sender: TObject);
var
 i, j, diff, NumChannelsPrev : integer;
 HeaderLine : string;
begin
 // write a new header line to the output file if .def file was used
 if HaveSensorFileStream and HaveDefFileCB.Checked then
 begin
  HeaderLine:= 'Definition file: "' + LoadedDefFileM.Text
               + '.def" was unloaded' + LineEnding;
  // write a new header line to the output file
  HeaderLine:= HeaderLine + 'Counter' + #9 + 'Time [min]' + #9;
  for i:= 1 to SIXControl.NumChannels do
   HeaderLine:= HeaderLine + 'Ch' + IntToStr(i) + ' [nA]' + #9;
  HeaderLine:= HeaderLine + 'Temp [deg C]' + #9 + LineEnding;
  SensorFileStream.Write(HeaderLine[1], Length(HeaderLine));
 end;

 HaveDefFileCB.Checked:= false;
 InNameDef:= '';
 LoadDefBB.Enabled:= true;

 // store previous channel number and set to 6 channels
 NumChannelsPrev:= SIXControl.NumChannels;
 SIXControl.NumChannels:= 6;

 // the previous .def file might have had less channels defined
 // to calculate later the slopes fill the missing x-values of the new channels
 // with zeroes as y-values
 diff:= SIXControl.NumChannels - NumChannelsPrev;
 // if channel 1 is empty, there is nothing to do
 if (diff > 0) and (SIXCh1Values.Count > 0) then
 begin
  for i:= 1 to diff do
  begin
   for j:= (FindComponent('SIXCh' + IntToStr(7-i) + 'Values')
           as TLineSeries).Count to SIXCh1Values.Count-1 do
    // since channel 1 always exists, we can take its x-values
    (FindComponent('SIXCh' + IntToStr(7-i) + 'Values')
     as TLineSeries).AddXY(SIXCh1Values.XValue[j], 0);
  end;
 end;

 // enable maybe previously disabled GroupBoxes
 for i:= 1 to 8 do
  (FindComponent('Channel' + IntToStr(i) + 'GB')
   as TGroupBox).Enabled:= true;

 // refill the channel numbers
 for i:= 1 to SIXControl.NumChannels do
  (FindComponent('Channel' + IntToStr(i) + 'LE')
   as TLabeledEdit).Text:= '#' + IntToStr(i);

 // rename the channels
 for i:= 1 to SIXControl.NumChannels do
 begin
  (FindComponent('Channel' + IntToStr(i) + 'GB')
     as TGroupBox).Caption:= 'Channel ' + IntToStr(i);
  SIXControl.HeaderStrings[i]:= '';
 end;

 // update chart legend according to new channel names
 for i:= 1 to SIXControl.NumChannels do
 begin
  (FindComponent('SIXCh' + IntToStr(i) + 'Values')
   as TLineSeries).Title:=
    (FindComponent('Channel' + IntToStr(i) + 'GB')
     as TGroupBox).Caption;
  (FindComponent('SIXCh' + IntToStr(i) + 'Results')
   as TLineSeries).Title:=
    'Stable ' + (FindComponent('Channel' + IntToStr(i) + 'GB')
     as TGroupBox).Caption;
 end;

 // update the possible operations
 for i:= 7 to 8 do
 begin
  // first delete, then refill
  (FindComponent('Channel' + IntToStr(i) + 'CB')
     as TComboBox).Items.Clear;
  (FindComponent('Channel' + IntToStr(i) + 'CB')
    as TComboBox).Items.Add('mean(#2, #5)');
  (FindComponent('Channel' + IntToStr(i) + 'CB')
    as TComboBox).Items.Add('mean(#3, #6)');
  (FindComponent('Channel' + IntToStr(i) + 'CB')
    as TComboBox).Items.Add('mean(#1, #4)');
 end;

 // the channel operations might show the old channel name, thus update them
 for i:= 7 to 8 do
 begin
  if (FindComponent('Channel' + IntToStr(i) + 'CB')
     as TComboBox).Text = 'mean(#2, #5)' then
   (FindComponent('SIXCh' + IntToStr(i) + 'Values') as TLineSeries).Title:=
    'Mean (' + Channel2GB.Caption + ', ' + Channel5GB.Caption + ')'
  else if (FindComponent('Channel' + IntToStr(i) + 'CB')
     as TComboBox).Text = 'mean(#3, #6)' then
   (FindComponent('SIXCh' + IntToStr(i) + 'Values') as TLineSeries).Title:=
    'Mean (' + Channel3GB.Caption + ', ' + Channel6GB.Caption + ')'
  else if (FindComponent('Channel' + IntToStr(i) + 'CB')
     as TComboBox).Text = 'mean(#1, #4)' then
   (FindComponent('SIXCh' + IntToStr(i) + 'Values') as TLineSeries).Title:=
    'Mean (' + Channel1GB.Caption + ', ' + Channel4GB.Caption + ')';
  // we use the same legend name for Live and Result charts
  (FindComponent('SIXCh' + IntToStr(i) + 'Results') as TLineSeries).Title:=
   (FindComponent('SIXCh' + IntToStr(i) + 'Values') as TLineSeries).Title;
 end;

end;

procedure TMainForm.PerformLinearityCBChange(Sender: TObject);
begin
 LinearityTestGB.Visible:= PerformLinearityCB.Checked;
 ResultTS.TabVisible:= PerformLinearityCB.Checked;
 if UseCalibCB.Checked then
  UseCalibCB.Checked:= false;
 UseCalibGB.Enabled:= not PerformLinearityCB.Checked;
 // notify user
 UseCalibGB.ShowHint:= PerformLinearityCB.Checked;
 // enable to start an action if measurement is running
 if HaveSerialSensorCB.Checked then
  RunBB.Enabled:= PerformLinearityCB.Checked;
end;

procedure TMainForm.NoSubtractBlankCBChange(Sender: TObject);
begin
 SIXControl.SCNoSubtractBlankCBChange(Sender);
end;

procedure TMainForm.RawCurrentCBChange(Sender: TObject);
begin
 SIXControl.SCRawCurrentCBChange(Sender);
end;

procedure TMainForm.ReadTimerTimerFinished(Sender: TObject);
begin
 SIXControl.SCReadTimerTimerFinished(Sender);
end;

procedure TMainForm.RectangleSelectionToolAfterMouseDown(ATool: TChartTool;
  APoint: TPoint);
var
 point : TDoublePoint;
begin
 point:= SIXCH.ImageToGraph(APoint);
 LeftLine.Position:= point.X;
 TopLine.Position:= point.Y;
 RightLine.Position:= point.X;
 BottomLine.Position:= point.Y;
 // CalcStats;
end;

procedure TMainForm.RectangleSelectionToolAfterMouseMove(ATool: TChartTool;
  APoint: TPoint);
var
 point : TDoublePoint;
begin
 point:= SIXCH.ImageToGraph(APoint);
 RightLine.Position:= point.X;
 BottomLine.Position:= point.Y;
 //CalcStats;
end;

procedure TMainForm.SaveCSVResultBClick(Sender: TObject);
begin
 SIXControl.SCSaveCSVResultBClick(Sender);
end;

procedure TMainForm.SaveScreenshotLiveBClick(Sender: TObject);
begin
 SIXControl.SCSaveScreenshotLiveBClick(Sender);
end;

procedure TMainForm.SaveScreenshotResultBClick(Sender: TObject);
begin
 SIXControl.SCSaveScreenshotResultBClick(Sender);
end;

procedure TMainForm.ScrollIntervalFSEChange(Sender: TObject);
begin
 ChartLiveView.ViewportSize:= ScrollIntervalFSE.Value;
end;

procedure TMainForm.ScrollViewCBChange(Sender: TObject);
begin
 SIXControl.SCScrollViewCBChange(Sender);
end;

procedure TMainForm.ChannelXGBDblClick(Sender: TObject);
begin
 SIXControl.SCChannelXGBDblClick(Sender);
end;

procedure TMainForm.LiveModeCBChange(Sender: TObject);
// changes the program to transmit every pump settings change immediately
begin
 PumpControl.PCLiveModeCBChange(Sender);
end;

procedure TMainForm.GenerateCommandBBClick(Sender: TObject);
// call function to collect data an generate command
begin
 PumpControl.PCGenerateCommandBBClick(Sender);
end;

procedure TMainForm.PumpOnOffCBLoopChange(Sender: TObject);
begin
 PumpControl.PCPumpOnOffCBLoopChange(Sender);
end;

procedure TMainForm.ValveRGChange(Sender: TObject);
begin
 // if in live mode send trigger command generation and sending
 if LiveModeCB.Checked and OverallTimer.Enabled then
  PumpControl.RunImmediate;
end;

procedure TMainForm.PumpVoltageFSChange(Sender: TObject);
// recalculate pump speed displays
begin
 PumpControl.PCPumpVoltageFSChange(Sender);
end;

procedure TMainForm.RepeatPCChange(Sender: TObject);
// set visibility of repeat tabs
begin
 PumpControl.PCRepeatPCChange(Sender);
end;

procedure TMainForm.RunBBClick(Sender: TObject);
// execute generated command
begin
 PumpControl.PCRunBBClick(Sender);
end;

procedure TMainForm.RepeatTimerFinished;
// Actions after repeat time interval ends
begin
 PumpControl.PCRepeatTimerFinished;
end;

procedure TMainForm.AnOutOnOffTBChange(Sender: TObject);
begin
 SIXControl.SCAnOutOnOffTBChange(Sender);
end;

procedure TMainForm.TimeDayMIClick(Sender: TObject);
begin
 SIXControl.SCTimeDayMIClick(Sender);
end;

procedure TMainForm.TimeHourMIClick(Sender: TObject);
begin
 SIXControl.SCTimeHourMIClick(Sender);
end;

procedure TMainForm.TimeMinuteMIClick(Sender: TObject);
begin
 SIXControl.SCTimeMinuteMIClick(Sender);
end;

procedure TMainForm.TimeDaysHoursMinMIClick(Sender: TObject);
var
 SenderName : string;
begin
 SenderName:= (Sender as TComponent).Name;
 // cbTimeReformat will call this function also on initializing its dialog
 if SenderName <> 'cbTimeReformat' then
  TimeDaysHoursMinMI.Checked:= not TimeDaysHoursMinMI.Checked;
 // trigger the repaint of the x-axis labels
 SIXCH.Invalidate;
end;

procedure TMainForm.UseAnOutCBChange(Sender: TObject);
var
 i : integer;
 Channel : string;
begin
 // first hide pump tab and show output tab
 ActionControlTS.TabVisible:= not UseAnOutCB.Checked;
 AnalogOutTS.TabVisible:= UseAnOutCB.Checked;
 for i:= 1 to 4 do
 begin
  Channel:= IntToStr(i);
  if UseAnOutCB.Checked then
  // allow to output own to 0.0 V and with 3 digits
  begin
   (FindComponent('Pump' + Channel + 'VoltageFS1') as TFloatSpinEdit).MinValue:= 0.0;
   (FindComponent('Pump' + Channel + 'VoltageFS1') as TFloatSpinEdit).DecimalPlaces:= 3;
  end
  else
  begin
   (FindComponent('Pump' + Channel + 'VoltageFS1') as TFloatSpinEdit).MinValue:= 0.1;
   (FindComponent('Pump' + Channel + 'VoltageFS1') as TFloatSpinEdit).DecimalPlaces:= 2;
  end;
 end;
 // enable the Live mode
 LiveModeCB.Checked:= UseAnOutCB.Checked;
 // turn off output when no longer used
 if AnOutOnOffTB.Enabled and (not UseAnOutCB.Checked) then
  AnOutOnOffTB.Checked:= false;
end;

procedure TMainForm.AnOutOfXLEChange(Sender: TObject);
begin
 SIXControl.SCAnOutOfXLEChange(Sender);
end;

procedure TMainForm.IndicatorPumpPPaint;
begin
 IndicatorPumpGeneralP.Color:= IndicatorPumpP.Color;
 IndicatorPumpGeneralP.Caption:= IndicatorPumpP.Caption;
end;

procedure TMainForm.InfoNoteClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 // stop the permanent COM-port scan
 ConnectionLost:= False;
 // close connection to SIX and the sensor file stream
 CloseLazSerialConn;
 // turn off noise
 NoSound;
end;

procedure TMainForm.LineDragToolDrag(ASender: TDataPointDragTool;
  var AGraphPoint: TDoublePoint);
const
 MIN_SEPARATION_X = 1.0;
 MIN_SEPARATION_Y = 1.0;
begin
 if ASender.Series = TopLine then
 begin
  if AGraphPoint.Y > Bottomline.Position + MIN_SEPARATION_Y then
   TopLine.Position:= AGraphPoint.Y
  else
   TopLine.Position:= BottomLine.Position + MIN_SEPARATION_Y;
 end
 else
 if ASender.Series = BottomLine then
 begin
  if AGraphPoint.Y < Topline.Position - MIN_SEPARATION_Y then
   BottomLine.Position:= AGraphPoint.Y
  else
   BottomLine.Position:= TopLine.Position - MIN_SEPARATION_Y;
 end
 else
 if ASender.Series = LeftLine then
 begin
  if AGraphPoint.X < Rightline.Position - MIN_SEPARATION_X then
   LeftLine.Position:= AGraphPoint.X
  else
   LeftLine.Position:= RightLine.Position - MIN_SEPARATION_X;
 end
 else
 if ASender.Series = RightLine then
 begin
  if AGraphPoint.X > RightLine.Position + MIN_SEPARATION_X then
   RightLine.Position:= AGraphPoint.X
  else
   RightLine.Position:= LeftLine.Position + MIN_SEPARATION_X;
 end
 else
  exit;
 //CalcStats;
end;

procedure TMainForm.LineDragToolDragStart(ASender: TDataPointDragTool;
  var AGraphPoint: TDoublePoint);
begin
 if ASender.Series = TopLine then
  ASender.ActiveCursor := crSizeNS
 else
 if ASender.Series = BottomLine then
  ASender.ActiveCursor := crSizeNS
 else
 if ASender.Series = LeftLine then
  ASender.ActiveCursor := crSizeWE
 else
  if ASender.Series = RightLine then
   ASender.ActiveCursor := crSizeWE;
end;

procedure TMainForm.ConnComPortPumpLEChange;
begin
 ConnComPortPumpGeneralLE.Color:= ConnComPortPumpLE.Color;
 ConnComPortPumpGeneralLE.Text:= ConnComPortPumpLE.Text;
end;

// disable context menus for TMemo and TComboBoxObjects
procedure TMainForm.ConnComPortSensMContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
 Handled:= True;
end;

procedure TMainForm.HaveSerialSensorCBChange(Sender: TObject);
begin
 if HaveSerialSensorCB.Checked then
 begin
  SIXConnectBB.Caption:= 'End SIX Measurement';
  SIXConnectBB.Hint:= 'Disconnects from the SIX biosensor'#13#10'and stops the measurement';
  Application.Icon.Assign(IconImageGreen.Picture.Icon);
  IndicatorSensorP.Caption:= 'Measurement running';
  IndicatorSensorP.Color:= clLime;
 end
 else
 begin
  SIXConnectBB.Caption:= 'Start SIX Measurement';
  SIXConnectBB.Hint:= 'Connects to a SIX biosensor and'#13#10'starts immediately a measurement';
  Application.Icon.Assign(IconImageBlue.Picture.Icon);
  if LoadedFileSensM.Text <> 'None' then
   MainForm.Caption:= 'JT Driver Sensing ' + Version
   + ' - Sensor data file: ' + LoadedFileSensM.Text + '.csv'
  else
   MainForm.Caption:= 'JT Driver Sensing';
 end;
 LoadDefBB.Enabled:= not HaveSerialSensorCB.Checked;
 // only for a successful reconnection there is a hint
 // therefore we can always delete it here
 IndicatorSensorP.Hint:= '';
end;

procedure TMainForm.SIXConnectBBClick(Sender: TObject);
begin
 if HaveSerialSensorCB.Checked then
  SIXBiosensorsStart(false, true) // call to disconnect
 else
  SIXBiosensorsStart(false, false);
end;

procedure TMainForm.DriverConnectBBClick(Sender: TObject);
begin
 if HavePumpSerialCB.Checked then
  PumpConnectionStart(Sender, false) // call to disconnect
 else
  PumpConnectionStart(Sender, true);
end;

procedure TMainForm.LoadedActionFileMContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
 Handled:= True;
end;

procedure TMainForm.LoadedActionFileGeneralMContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
 Handled:= True;
end;

procedure TMainForm.LoadedDefFileMContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
 Handled:= True;
end;

procedure TMainForm.Channel7CBContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
 Handled:= True;
end;

procedure TMainForm.Channel8CBContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
 Handled:= True;
end;

procedure TMainForm.AnOutputOf1CBContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
 Handled:= True;
end;

procedure TMainForm.AnOutputOf2CBContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
 Handled:= True;
end;

procedure TMainForm.AnOutputOf3CBContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
 Handled:= True;
end;

procedure TMainForm.AnOutputOf4CBContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
 Handled:= True;
end;

procedure TMainForm.LoadedActionFileMChange(Sender: TObject);
begin
 // just forward the properties to the counterpart of in the tab General
 LoadedActionFileGeneralM.Text:= LoadedActionFileM.Text;
 LoadedActionFileGeneralM.Color:= LoadedActionFileM.Color;
 LoadedActionFileGeneralM.Hint:= LoadedActionFileM.Hint;
end;

procedure TMainForm.LoadedFileSensMChange(Sender: TObject);
begin
 // update the main window title
 if LoadedFileSensM.Text = 'None' then
  MainForm.Caption:= 'JT Driver Sensing ' + Version
 else
 begin
  MainForm.Caption:= 'JT Driver Sensing ' + Version
   + ' - Sensor data file: ' + LoadedFileSensM.Text + '.csv';
  if HaveSerialSensorCB.Checked then
   MainForm.Caption:= MainForm.Caption + ' (autosave on)';
 end;
end;

procedure TMainForm.LoadedFileSensMContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
 // disable if there is no SIX data file or an action is running
 if (not HaveSensorFileStream) or OverallTimer.Enabled then
 begin
  ChangeSensorDataFileMI.Enabled:= False; // just a safe guard
  Handled:= true;
 end;
end;

procedure TMainForm.LoadSensorDataMIClick(Sender: TObject);
var
 AppendCounter : Int64;
 LastDefFile, LastSIXID : string;
 AppendMinute : double;
begin
 // we can have the case that a file is dropped while we cannot load a file
 if not LoadSensorDataMI.enabled then
  exit;

 ReadSensorData((Sender as TComponent).Name, AppendMinute, AppendCounter,
                LastDefFile, LastSIXID);

 // assure that at least one channel is displayed
 AssureChannelDisplay;

 // if the current tab is not the chart tab, we must explicitly
 // reset the axis ranges to get all data displayed
 if MainPC.ActivePage <> SIXValuesTS then
 begin
  SIXCH.AxisList[0].Range.UseMax:= false;
  SIXCH.AxisList[0].Range.UseMin:= false;
  SIXCH.AxisList[1].Range.UseMax:= false;
  SIXCH.AxisList[1].Range.UseMin:= false;
  // for the x-axis also te extent must be set
  SIXCH.Extent.UseXMax:= false;
  SIXCH.Extent.UseXMin:= false;
 end;

 // output the last used SIX
 ConnComPortSensL.Caption:= 'Last Used SIX';
 ConnComPortSensM.Color:= clInfoBK;
 ConnComPortSensM.Text:= LastSIXID;

 // read the notes
 SIXControl.ReadNotes;
end;

function TMainForm.ReadSensorData(Input: string; out AppendMinute: double;
                   out AppendCounter: Int64; out LastDefFile: string;
                   out LastSIXID: string) : Boolean;
// reads data out of sensor file
var
 OpenFileStream : TFileStream;
 LineReader : TStreamReader;
 StringArray : TStringArray;
 ReadLine, ReturnName, testString, tempStr, ChartTitle : string;
 MousePointer : TPoint;
 i, rowCounter, blankCounter, TempRow : integer;
 ChanDbl, ChanRawDbl : array [1..8] of double;
 temperature, time : double;
 counter, previousCounter : longint;
 isBlank : array [1..6] of Boolean;
 testArray : array of string = nil;
 Component : TComponent;
 StartTime : TDateTime;
begin
 // initialize
 Result:= false;
 previousCounter:= 0; // a valid file must begin with counter '1'
 MousePointer:= Mouse.CursorPos; // store mouse position
 LastDefFile:= 'None'; // there might not be any .def file
 LastSIXID:= 'unknown SIX';
 ChartTitle:= 'SIX Values';
 TempRow:= -1;
 for i:= 1 to 8 do
  ChanRawDbl[i]:= 0.0;
 for i:= 1 to 6 do
  isBlank[i]:= false;

 // the data file can have different portions, some just raw values, some with
 // .def file loaded.
 // Since there is no optimal solution, the compromise is to read out
 // either only the raw values when RawCurrentCB is checked
 // otherwise the mmol values. In this case portions without mmol values will
 // be calculated using the default gain factors

 // only when file was loaded by the user
 if (Input = 'LoadSensorDataMI') or (Input = 'MainForm') then
 begin
  // if a measurement is running do nothing
  if OverallTimer.Enabled then
   exit;
  if DropfileNameData <> '' then // a file was dropped into the SIX Values tab
   InNameSensor:= DropfileNameData
  else
  begin
   ReturnName:= OpenHandling('', '.csv');
   if ReturnName = '' then
    exit // user aborted the loading
   else
    InNameSensor:= ReturnName;
  end;
  // set the default calibration factors
  SetSIXFactors;
 end;

try
 try
  OpenFileStream:= TFileStream.Create(InNameSensor, fmOpenRead or fmShareDenyNone);
 except
  on EFOpenError do
  begin
   MessageDlgPos('Sensor data file is used by another program and cannot be opened.',
                 mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   exit;
  end;
 end;
 LineReader:= TStreamReader.Create(OpenFileStream);

 // read the first header line that contains the creation time
 LineReader.ReadLine(ReadLine);
 StringArray:= ReadLine.Split(' ');
 StartTime:= EncodeDate(StrToInt(Copy(StringArray[1], 7, 4)),
                        StrToInt(Copy(StringArray[1], 4, 2)),
                        StrToInt(Copy(StringArray[1], 1, 2)));
 StartTime:= StartTime +
             EncodeTime(StrToInt(Copy(StringArray[2], 1, 2)),
                        StrToInt(Copy(StringArray[2], 4, 2)),
                        StrToInt(Copy(StringArray[2], 7, 2)),
                        0);
 StartTimeLE.Text:= StringArray[1] + ' ' + StringArray[2];

 // now read the row description line
 // NOTE: the line ends with a tab, thus Length(StringArray) is larger than
 // the actual number of columns
 rowCounter:= 1; // because the first line was already read
 Repeat
  if (rowCounter < 4) and LineReader.Eof then
  begin
   MessageDlgPos('File is too short to read data from it.',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   exit;
  end
  else
   LineReader.ReadLine(ReadLine);
  inc(rowCounter);

  StringArray:= ReadLine.Split(' ');
  if (StringArray[0] = 'Used') and (StringArray[1] = 'definition')
   and (StringArray[2] = 'file:') then // line with name of .def file
  begin
   LastDefFile:= '';
   // we can have spaces in the filename therefore concatenate all
   for i:= 3 to Length(StringArray)-1 do
   begin
    LastDefFile:= LastDefFile + StringArray[i];
    if i < (Length(StringArray) - 1) then
     LastDefFile:= LastDefFile + ' ';
   end;
   RemovePadChars(LastDefFile, ['"']); // strip quotes

   // assume we have an existing .def file
   FoundLoadedDefFile:= True;
   // reset temperature connection hint
   NoTempCorrectionCB.Hint:= 'The sensor data will not be corrected' + LineEnding
    + 'by the temperature correction factor that' + LineEnding
    + 'are stored in the sensor definition file' + LineEnding
    + '(only available for running measurements' + LineEnding
    + 'if definition file is loadedor for loaded' + LineEnding
    + 'measurements if values are displayed in nA)';

   // check if .def file exists and if, read its gains
   // this is done for the temperature gains that are the
   // same in all .def files of a measurement
   // first check if .def file is in same folder
   tempStr:= ExtractFilePath(InNameSensor) + LastDefFile;
   if FileExists(tempStr) then
    SIXControl.ParseDefFile(tempStr)
   else
   begin
    // chack also the subfolder 'DefinitionFiles'
    tempStr:= ExtractFilePath(InNameSensor) + 'DefinitionFiles\' + LastDefFile;
    if FileExists(tempStr) then
     SIXControl.ParseDefFile(tempStr)
    else
    begin
     FoundLoadedDefFile:= False;
     NoTempCorrectionCB.Hint:= 'Not available because first .def file in'
      + LineEnding + 'loaded sensor data file cannot be found';
    end;
   end;
   SetLength(LastDefFile, Length(LastDefFile) - 4); // without suffix '.def'
   continue;
  end;

  // determine initial SIX
  if (StringArray[0] = 'Used') and (StringArray[1] = 'SIX')
   and (StringArray[2] = 'ID') then // line with ID of SIX
  begin
   // concatenate all except of the first
   LastSIXID:= '';
   for i:= 1 to Length(StringArray)-1 do
   begin
    LastSIXID:= LastSIXID + StringArray[i];
    if i < (Length(StringArray) - 1) then
     LastSIXID:= LastSIXID + ' ';
   end;
   continue;
  end;

  // determine initial title
  if (StringArray[0] = 'Diagram') and (StringArray[1] = 'title:') then
  begin
   // concatenate all except of the first two
   ChartTitle:= '';
   for i:= 2 to Length(StringArray)-1 do
   begin
    ChartTitle:= ChartTitle + StringArray[i];
    if i < (Length(StringArray) - 1) then
     ChartTitle:= ChartTitle + ' ';
   end;
   continue;
  end;

  StringArray:= ReadLine.Split(#9);
  if StringArray[0] = 'Counter' then // we have a header line
  begin
   for i:= 2 to Length(StringArray) - 1 do // we can exclude the first columns
   begin
    if StringArray[i] = 'Temp [deg C]' then
    begin
     TempRow:= i;
     break;
    end;
   end;
   if TempRow = -1 then
   begin
    // old files have due to a bug no tab at the end of the header line
    if StringArray[Length(StringArray) - 1] = 'Temp [deg C]' then
     TempRow:= Length(StringArray) - 1
    else
    begin
     MessageDlgPos('File contains no temperature data.',
      mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
     exit;
    end;
   end;
   // now we can set the number of channels
   if (TempRow = Length(StringArray) - 2)
     or (TempRow = Length(StringArray) - 1) {for old files} then
    SIXControl.NumChannels:= TempRow - 2
   else
    SIXControl.NumChannels:= Length(StringArray) - TempRow - 2;
   // evaluate the blanks and channel names
   // if TempRow is the last one, we have only raw values
   if (TempRow = Length(StringArray) - 2)
    or (TempRow = Length(StringArray) - 1) then
   begin
    for i:= 1 to 6 do
    isBlank[i]:= false;
   end
   else
    for i:= TempRow + 1 to TempRow + SIXControl.NumChannels do
    begin
     isBlank[i-TempRow]:= false;
     testString:= Copy(StringArray[i], 2, 4);
     if testString = 'lank' then // because some files use "Blank" or "blank"
      isBlank[i-TempRow]:= true;
     // channel names could have ine or two spaces
     // therefore split at space and concatenate
     testArray:= StringArray[i].Split(' ');
     if Length(testArray) = 3 then
      testString:= testArray[0] + ' ' + testArray[1]
     else
      testString:= testArray[0];
     (FindComponent('Channel' + IntToStr(i-TempRow) + 'GB')
      as TGroupBox).Caption:= testString;
    end;
  end;
 until (StringArray[0] = 'Counter') or (rowCounter = 3);

 // we only have 4 header lines
 if (rowCounter = 4) and (StringArray[0] <> 'Counter') then
 begin
  MessageDlgPos('File has no header line defining the value units.',
   mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
  exit;
 end;

 // we know the file is valid thus we can empty the data chart and
 // read in the data

 // for the parsing set time unit to hours since this fits for most cases
 // will be reset later if necessary
 // this must be done before the existing data is deleted
 TimeHourMIClick(MainForm);

 // actions only when file was loaded by the user:
 if (Input = 'LoadSensorDataMI') or (Input = 'MainForm') then
 begin
 // there might have been a definition file load that doesn't fit to the data
 // therefore unload it only when file was loaded by the user
 if HaveDefFileCB.Checked then
  UnloadDefBBClick(MainForm);
 end;

 // delete existing live chart data
 // but purposely not the measurement data
 for i:= 1 to 8 do
  (FindComponent('SIXCh' + IntToStr(i) + 'Values')
   as TLineSeries).Clear;
 SIXTempValues.Clear;

 // disable chart scrolling
 // this assures that the line pen is set to default to speed up the display for
 // large files. To assure this, set the time counter to a dummy value above the
 // scroll length.
 TSIXControl.timeCounter:= ScrollIntervalFSE.Value + 1.0;
 ScrollViewCB.Checked:= false;
 ScrollViewCB.Enabled:= false;

 // assure that the axis range is set to automatic to display everything
 SIXCH.AxisList[0].Range.UseMax:= false;
 SIXCH.AxisList[0].Range.UseMin:= false;
 SIXCH.AxisList[1].Range.UseMax:= false;
 SIXCH.AxisList[1].Range.UseMin:= false;
 // for the x-axis also te extent must be set
 SIXCH.Extent.UseXMax:= false;
 SIXCH.Extent.UseXMin:= false;

 // parse the file to the end
 while not LineReader.Eof do
 begin
  // tell Windows the program is alive because we can have large files
  if rowCounter mod 86400 = 0 then // data for 3 days when data every 3 seconds
   Application.ProcessMessages;

  blankCounter:= 0;
  inc(rowCounter);
  LineReader.ReadLine(ReadLine);
  StringArray:= ReadLine.Split(#9);
  // if the first row is no integer it is an intermediate header line
  // and we can either skip or must re-evaluate the temperature column
  if not TryStrToInt(StringArray[0], i) then
  begin
   if StringArray[0] = 'Counter' then // re-evaluate
   begin
    TempRow:= -1;
    for i:= 1 to Length(StringArray) - 1 do
    begin
     if StringArray[i] = 'Temp [deg C]' then
     begin
      TempRow:= i;
      break;
     end;
    end;
    if TempRow = -1 then
    begin
     MessageDlgPos('File contains no temperature data.',
      mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
     exit;
    end;
    // now re-evaluate the blanks and channels names
    // channel names because many files start with only raw data
    // if TempRow is the last one, we have only raw values
    if (TempRow = Length(StringArray) - 2)
     or (TempRow = Length(StringArray) - 1) then
    begin
     for i:= 1 to 6 do
     isBlank[i]:= false;
    end
    else
     for i:= TempRow + 1 to TempRow + SIXControl.NumChannels do
     begin
      isBlank[i-TempRow]:= false;
      testString:= Copy(StringArray[i], 2, 4);
      if testString = 'lank' then // because some files use "Blank" or "blank"
       isBlank[i-TempRow]:= true;
      // channel names could have ine or two spaces
      // therefore split at space and concatenate
      testArray:= StringArray[i].Split(' ');
      if Length(testArray) = 3 then
       testString:= testArray[0] + ' ' + testArray[1]
      else
       testString:= testArray[0];
      (FindComponent('Channel' + IntToStr(i-TempRow) + 'GB')
       as TGroupBox).Caption:= testString;
     end;
    continue;
   end;
   StringArray:= ReadLine.Split(' ');
   if (StringArray[0] = 'Used') and (StringArray[1] = 'definition')
    and (StringArray[2] = 'file:') then // line with name of .def file
   begin
    LastDefFile:= '';
    // we can have spaces in the filename therefore concatenate all
    for i:= 3 to Length(StringArray)-1 do
    begin
     LastDefFile:= LastDefFile + StringArray[i];
     if i < (Length(StringArray) - 1) then
      LastDefFile:= LastDefFile + ' ';
    end;
    RemovePadChars(LastDefFile, ['"']); // strip quotes
    SetLength(LastDefFile, Length(LastDefFile) - 4); // without suffix '.def'
    continue;
   end;
   if (StringArray[0] = 'Definition')
    and (StringArray[1] = 'file:') then // .def file was unloaded
   begin
    LastDefFile:= 'None';
    continue;
   end;
   // determine SIX
   if (StringArray[0] = 'Used') and (StringArray[1] = 'SIX')
    and (StringArray[2] = 'ID') then // line with ID of SIX
   begin
    // concatenate all except of the first
    LastSIXID:= '';
    for i:= 1 to Length(StringArray)-1 do
    begin
     LastSIXID:= LastSIXID + StringArray[i];
     if i < (Length(StringArray) - 1) then
      LastSIXID:= LastSIXID + ' ';
    end;
    continue;
   end;
   // determine title
   if (StringArray[0] = 'Diagram') and (StringArray[1] = 'title:') then
   begin
    // concatenate all except of the first two
    ChartTitle:= '';
    for i:= 2 to Length(StringArray)-1 do
    begin
     ChartTitle:= ChartTitle + StringArray[i];
     if i < (Length(StringArray) - 1) then
      ChartTitle:= ChartTitle + ' ';
    end;
    continue;
   end;

   continue;
  end; // end evaluation of intermediate header

  // first read the counter because corrupted files might thave missing
  // values and then following routines like calculating slopes would fail
  if not TryStrToInt(StringArray[0], counter) then
  begin
   MessageDlgPos('Counter in line ' + IntToStr(rowCounter) + ', row '
    + IntToStr(TempRow+1) + ' cannot be converted to a number',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   exit;
  end;
  // only when data should be appended
  if (Input = 'none') and (counter > (previousCounter + 1)) then
  begin
   if previousCounter = 0 then
    MessageDlgPos('There is no line with a counter "1"',
     mtError, [mbOK], 0, MousePointer.X, MousePointer.Y)
   else
    MessageDlgPos('Counter in line ' + IntToStr(rowCounter)
     + ' larger than previous counter + 1',
     mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   exit;
  end;
  previousCounter:= counter;
  // first read the temperature
  if not TryStrToFloat(StringArray[TempRow], temperature) then
  begin
   MessageDlgPos('Temperature in line ' + IntToStr(rowCounter) + ', row '
    + IntToStr(TempRow+1) + ' cannot be converted to a number',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   exit;
  end;
  // now the time
  if not TryStrToFloat(StringArray[1], time) then
  begin
   MessageDlgPos('Time in line ' + IntToStr(rowCounter) + ', row '
    + IntToStr(2) + ' cannot be converted to a number',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   exit;
  end;
  // if TempRow is the last one, we have only raw values
  if (TempRow = Length(StringArray) - 2)
   or (TempRow = Length(StringArray) - 1) then
  begin
   for i:= 1 to SIXControl.NumChannels do
   begin
    if not TryStrToFloat(StringArray[i+1], ChanRawDbl[i]) then
    begin
     MessageDlgPos('Number in line ' + IntToStr(rowCounter) + ', row '
      + IntToStr(i+2) + ' cannot be converted to a number',
      mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
     exit;
    end;
    // use raw values also for mmol because there is no other way
    ChanDbl[i]:= ChanRawDbl[i];
   end;
  end
  else // raw data is behind the temperature
  begin
   for i:= 1 to SIXControl.NumChannels do
   begin
    if not TryStrToFloat(StringArray[TempRow+i], ChanRawDbl[i]) then
    begin
     MessageDlgPos('Number in line ' + IntToStr(rowCounter) + ', row '
      + IntToStr(TempRow+i+1) + ' cannot be converted to a number',
      mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
     exit;
    end;
    if isBlank[i] then // there is no mmol row for blanks
    begin
     // blanks get always the raw value
     ChanDbl[i]:= ChanRawDbl[i];
     inc(blankCounter);
     continue;
    end;
    if not TryStrToFloat(StringArray[i+1-blankCounter], ChanDbl[i]) then
    begin
     MessageDlgPos('Number in line ' + IntToStr(rowCounter) + ', row '
      + IntToStr(i+1) + ' cannot be converted to a number',
      mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
     exit;
    end;
   end;
  end;

  // draw SIX data
  // sensor data
  for i:= 1 to SIXControl.NumChannels do
  begin
   if RawCurrentCB.Checked then
    (FindComponent('SIXCh' + IntToStr(i) + 'Values')
     as TLineSeries).AddXY(time, ChanRawDbl[i])
   else
    (FindComponent('SIXCh' + IntToStr(i) + 'Values')
     as TLineSeries).AddXY(time, ChanDbl[i]);
  end;

  // calculation channels
  // first fill them with zeroes to assure they have the
  // same size than the data channels
  for i:= 7 to 8 do
  begin
   ChanRawDbl[i]:= 0.0;
   ChanDbl[i]:= 0.0;
   if RawCurrentCB.Checked then
    (FindComponent('SIXCh' + IntToStr(i) + 'Values')
     as TLineSeries).AddXY(time, ChanRawDbl[i])
   else
    (FindComponent('SIXCh' + IntToStr(i) + 'Values')
     as TLineSeries).AddXY(time, ChanDbl[i]);
  end;

  // temperature
  SIXTempValues.AddXY(time, temperature);

 end; // end of while not LineReader.Eof do

 // trigger calculation of channels 7 and 8 if possible
 // only possible when there at least 4 channels
 if SIXControl.NumChannels > 3 then
 begin
  for i:= 7 to 8 do
  begin
   // handle case that there are items are the item index is -1
   // (can occur depending on prior loaded .def files)
   if ((FindComponent('Channel' + IntToStr(i) + 'CB')
       as TComboBox).ItemIndex = -1)
     and ((FindComponent('Channel' + IntToStr(i) + 'CB')
                as TComboBox).Items.Count > 0) then
    (FindComponent('Channel' + IntToStr(i) + 'CB')
                as TComboBox).ItemIndex:= 0;
   if (FindComponent('Channel' + IntToStr(i) + 'CB')
                as TComboBox).ItemIndex > -1 then
   begin
    Component:= (FindComponent('Channel' + IntToStr(i) + 'CB')
                as TComboBox);
    SIXControl.SCChannelXCBChange(Component);
   end;
   // re-enable channel groupBoxes because they might have been diables
   (FindComponent('Channel' + IntToStr(i) + 'GB')
    as TGroupBox).Enabled:= True;
  end;
 end
 else
 begin
  // disable channel operations
  for i:= 7 to 8 do
   (FindComponent('Channel' + IntToStr(i) + 'GB')
    as TGroupBox).Enabled:= False;
 end;

 // disable channel operations
 if SIXControl.NumChannels < 6 then
 begin
  for i:=  SIXControl.NumChannels + 1 to 6 do
   (FindComponent('Channel' + IntToStr(i) + 'GB')
    as TGroupBox).Enabled:= False;
 end;

 // enable available channels
 for i:= 1 to SIXControl.NumChannels do
  (FindComponent('Channel' + IntToStr(i) + 'GB')
   as TGroupBox).Enabled:= True;

 // update chart legend according to channel names
 for i:= 1 to SIXControl.NumChannels do
 begin
  (FindComponent('SIXCh' + IntToStr(i) + 'Values')
   as TLineSeries).Title:=
    (FindComponent('Channel' + IntToStr(i) + 'GB')
     as TGroupBox).Caption;
  (FindComponent('SIXCh' + IntToStr(i) + 'Results')
   as TLineSeries).Title:=
    'Stable ' + (FindComponent('Channel' + IntToStr(i) + 'GB')
     as TGroupBox).Caption;
 end;

 // to later append to right time, store the last found time and counter
 AppendMinute:= SecondsBetween(Now, StartTime) / 60;
 AppendCounter:= counter;

 Result:= true;

finally
 LineReader.Free;
 OpenFileStream.Free;
end;

  // set proper time unit (hour was already set above)
  SIXCH.Refresh; // necessary to update the plot range Min/Max values
  if time > 60000 then
   TimeDayMIClick(MainForm)
  else if time < 1000 then
   TimeMinuteMIClick(MainForm);

 // only when file was loaded by the user
 if (Input = 'LoadSensorDataMI') or (Input = 'MainForm') then
 begin
  // for existing data we cannot change blank subtraction
  NoSubtractBlankCB.Enabled:= false;
  NoSubtractBlankCB.Checked:= false;
  // only for raw nA values we can recalculate the temperature correction
  NoTempCorrectionCB.Checked:= false;
  if RawCurrentCB.Checked and FoundLoadedDefFile then
   NoTempCorrectionCB.Enabled:= true
  else
   NoTempCorrectionCB.Enabled:= false;
  // also disable to load a new .def file because this would lead to
  // wrong channel names
  LoadDefBB.Enabled:= false;
  // hide the blanks
  for i:= 1 to SIXControl.NumChannels do
  begin
   if isBlank[i] then
    (FindComponent('Channel' + IntToStr(i) + 'OnOffCB')
     as TCheckBox).Checked:= false;
  end;
 end;

 // set the chart title
 SIXCH.Title.Text[0]:= ChartTitle;
 // update file name field and tooltip
 LoadedFileSensM.Text:= ExtractFileNameOnly(InNameSensor);
 LoadedFileSensM.Hint:= InNameSensor;

end;

procedure TMainForm.NoTempCorrectionCBChange(Sender: TObject);
begin
 SIXControl.SCNoTempCorrectionCBChange(Sender);
end;

procedure TMainForm.RepeatSEChange(Sender: TObject);
begin
 PumpControl.PCRepeatSEChange(Sender);
end;

procedure TMainForm.ResetChartAppearanceMIClick(Sender: TObject);
var
 defaultFile : string;
 MousePointer : TPoint;
begin
 defaultFile:= ExtractFilePath(Application.ExeName) + AppearanceDefault;
 if not FileExists(defaultFile) then
 begin
  MousePointer:= Mouse.CursorPos; // store mouse position
  MessageDlgPos('The file "' + AppearanceDefault
   + '" is not in the same folder as this program.'
   + LineEnding + 'The appearance cannot be reset.',
   mtError, [mbOK], 0 , MousePointer.X, MousePointer.Y);
  exit;
 end;
 SIXControl.SCLoadAppearance(defaultFile);
end;

procedure TMainForm.OverallTimerFinished;
// Actions after time interval ends
begin
 PumpControl.PCOverallTimerFinished;
end;

procedure TMainForm.StepTimerXFinished(Sender: TObject);
begin
 PumpControl.PCStepTimerXFinished(Sender);
end;

procedure TMainForm.StepTimerLastFinished(Sender: TObject);
begin
 PumpControl.PCStepTimerLastFinished(Sender);
end;

procedure TMainForm.StopBBClick(Sender: TObject);
begin
 // stop all pumps
 PumpControl.PCStopBBClick(Sender);
end;

procedure TMainForm.StopTimerFinished;
// enable to execute new commands
begin
 RunBB.Caption:= 'Run Action';
 RunBB.Enabled:= (HavePumpSerialCB.Checked or HasNoPumpsCB.Checked)
                 or PerformLinearityCB.Checked;
 StopTimer.Enabled:= False;
end;

procedure TMainForm.RunEndlessCBChange(Sender: TObject);
// if the pumps should run forever
begin
 PumpControl.PCRunEndlessCBChange(Sender);
end;

procedure TMainForm.StepXUseCBChange(Sender: TObject);
begin
 PumpControl.PCStepXUseCBChange(Sender);
end;

procedure TMainForm.PumpGBDblClick(Sender: TObject);
begin
 PumpControl.PCPumpGBDblClick(Sender);
end;

procedure TMainForm.AnOutPumpGBDblClick(Sender: TObject);
begin
 PumpControl.AnOutPumpXGBDblClick(Sender);
end;

procedure TMainForm.ValveRGDblClick(Sender: TObject);
begin
 PumpControl.PCValveRGDblClick(Sender);
end;

// opening --------------------------------------------------------------------

procedure TMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
 // check what the current tab is
 if MainPC.TabIndex = 0 then // sensor definition file
 begin
  DropfileNameDef:= FileNames[0];
  LoadDefBBClick(Sender);
  DropfileNameDef:= '';
 end
 else if MainPC.TabIndex = 2 then // pump action file
 begin
  DropfileNamePump:= FileNames[0];
  LoadActionMIClick(Sender);
  DropfileNamePump:= '';
 end
 else if MainPC.TabIndex = 1 then // sensor data file
 begin
  DropfileNameData:= FileNames[0];
  LoadSensorDataMIClick(Sender);
  DropfileNameData:= '';
 end;
end;

procedure TMainForm.LoadActionMIClick(Sender: TObject);
var
 FileSuccess : Boolean = false;
 ParseSuccess : Boolean = false;
 MousePointer : TPoint;
 command, DummyString, Substance : string;
 i, j : integer;
begin
 MousePointer:= Mouse.CursorPos; // store mouse position
 DummyString:= '';

 if DropfileNamePump <> '' then // a file was dropped into the Pump Control tab
  FileSuccess:= OpenActionFile(DropfileNamePump)
 else
 begin
  DummyString:= OpenHandling('', '.PDAction');
  if DummyString = '' then
   exit; // user aborted the loading
  FileSuccess:= OpenActionFile(DummyString);
 end;

 if not FileSuccess then
  MessageDlgPos('Error while attempting to open file',
   mtError, [mbOK], 0, MousePointer.X, MousePointer.Y)
 else
 begin
  // an action file is never live mode
  LiveModeCB.Checked:= False;
  // reset number of pumps
  if HasNoPumpsCB.Checked then
   HasNoPumpsCB.Checked:= false; // will set PumpNum to 1
  PumpControl.PumpNum:= PumpControl.PumpNumFile;
  PumpNumberSE.Value:= PumpControl.PumpNum;
  // make all steps visible because they might be invisible due to a prior loading
  for j:= 2 to PumpControl.StepNum do
   (FindComponent('Step' + IntToStr(j) + 'TS')
    as TTabSheet).TabVisible:= True;
  if DropfileNamePump <> '' then
   InNamePump:= DropfileNamePump
  else
   InNamePump:= OpenDialog.FileName;
  SaveDialog.FileName:= ''; // will be re-set in SaveHandling()
  // show the full path as tooltip
  if DropfileNamePump <> '' then
   LoadedActionFileM.Hint:= DropfileNamePump
  else
   LoadedActionFileM.Hint:= DummyString;
  // display file name without suffix
  DummyString:= ExtractFileName(InNamePump);
  SetLength(DummyString, Length(DummyString) - 9);
  LoadedActionFileM.Color:= clActiveCaption;
  LoadedActionFileM.Text:= DummyString;
  command:= CommandM.Text;
  // parse the command
  ParseSuccess:= PumpControl.ParseCommand(command);
  if ParseSuccess then
   // call command generation to get the action time calculated and to add
   // time steps in case many pumps have to be started at once
   PumpControl.GenerateCommand(command);

  // disable all setting possibilities
  RunSettingsGB.Enabled:= False;
  LiveModeCB.Enabled:= False;
  PumpSetupGB.Enabled:= False;
  UseCalibGB.Enabled:= False;
  ValveSetupGB.Enabled:= False;
  for j:= 1 to PumpControl.StepNum do
  begin
   // the user must be able to see if the pumps 5 - 8 are set
   // therefore we cannot just disable the StepXTS component but its
   // child components except of SXPC
   (FindComponent('Step' + IntToStr(j) + 'UseCB')
    as TCheckBox).Enabled:= False;
   (FindComponent('ActionTime' + IntToStr(j) + 'GB')
    as TGroupBox).Enabled:= False;
   (FindComponent('DutyCycle' + IntToStr(j) + 'GB')
    as TGroupBox).Enabled:= False;
   (FindComponent('S' + IntToStr(j) + 'P14')
    as TTabSheet).Enabled:= False;
   (FindComponent('S' + IntToStr(j) + 'P58')
    as TTabSheet).Enabled:= False;
   (FindComponent('S' + IntToStr(j) + 'Valves')
    as TTabSheet).Enabled:= False;
   if j = 1 then
    for i:= 1 to PumpControl.PumpNum do
    (FindComponent('Pump' + IntToStr(i) + 'GB' + IntToStr(j))
     as TGroupBox).ShowHint:= False;
  end;
  RepeatOutputLE.Visible:= False;
  CalibStepCB.Enabled:= False;
  CalibStepL.Enabled:= False;
  UseCalibCB.Enabled:= False;
  CalibEveryXStepsSE.Enabled:= False;
  CalibEveryXStepsL1.Enabled:= False;
  CalibEveryXStepsL2.Enabled:= False;
  UsedCalibValueSE.Enabled:= False;
  UsedCalibValueL.Enabled:= False;
  for j:= 1 to CalibSubstancesPC.PageCount do
  begin
   // the user must be able to see the settings for all substances
   // therefore we cannot just disable the CalibSubstancesPC component but its
   // child components except of XTS
   Substance:= CalibSubstancesPC.Pages[j-1].Caption;
   (FindComponent(Substance + 'AvailChanL')
    as TLabel).Enabled:= False;
   (FindComponent(Substance + 'CalibGB')
    as TGroupBox).Enabled:= False;
   (FindComponent(Substance + 'CalibCLB')
    as TChartListbox).Enabled:= False;
  end;
  // do not show unused steps
  for j:= 2 to PumpControl.StepNum do
  begin
   if (FindComponent('Step' + IntToStr(j) + 'UseCB')
       as TCheckBox).Checked = False then
    (FindComponent('Step' + IntToStr(j) + 'TS')
     as TTabSheet).TabVisible:= False;
  end;
 // disable saving, will be re-enabled by GererateCommand
 SaveActionMI.Enabled:= False;
 // show step 1
 RepeatPC.ActivePage:= Step1TS;
 end; // else if not FileSuccess
end;

procedure TMainForm.ShowExpertCBChange(Sender: TObject);
begin
 ExpertGB.Visible:= True;
 ShowExpertCB.Visible:= False;
end;

procedure TMainForm.UseCalibCBChange(Sender: TObject);
begin
 PumpControl.PCUseCalibCBChange(Sender);
end;

procedure TMainForm.PumpNumberSEChange(Sender: TObject);
begin
 PumpControl.PCPumpNumberSEChange(Sender);
end;

procedure TMainForm.ValveNumberSEChange(Sender: TObject);
begin
 PumpControl.PCValveNumberSEChange(Sender);
end;

function TMainForm.OpenActionFile(InputName: string): Boolean;
// read file content
var
 StringList : TStringList;
 j, k, ValveNumFile, startIndex, stopIndex : integer;
 FormPointer : TPoint;
 SeriesName, HelpString : string;
begin
 // initialize
 FormPointer:= MainForm.ControlOrigin;
 result:= False;
 PumpControl.PumpNumFile:= 0;
 ValveNumFile:= 0;

 try
  StringList:= TStringList.Create;
  k:= StringList.Count;
  // add all file lines to the string list
  StringList.LoadFromFile(InputName);

  CommandM.Text:= StringList[0];

  // get the number of pumps and valves
  for j:= 1 to StringList.Count - 1 do
  begin
   // if a line begins with 'Pump' we know it defines a pump
   if LeftStr(StringList[j], 4) = 'Pump' then
    inc(PumpControl.PumpNumFile);
   if LeftStr(StringList[j], 5) = 'Valve' then
    inc(ValveNumFile);
  end;
  ValveNumberSE.Value:= ValveNumFile;

  if StringList.Count = 1 then // no pump names defined (in old files)
  begin
   for k:= 1 to PumpControl.PumpNum do
    (FindComponent('Pump' + IntToStr(k) + 'GB1')
     as TGroupBox).Caption:= 'Pump ' + IntToStr(k);
  end
  else
  begin
   // read the pump and valve names
   for k:= 1 to PumpControl.PumpNumFile do
    (FindComponent('Pump' + IntToStr(k) + 'GB1')
     as TGroupBox).Caption:= Copy(StringList[k], Length(PumpControl.PumpPrefix) + 1,
                                  Length(StringList[k])); // omit the prefix
   if PumpControl.PumpNumFile < PumpControl.PumpNum then // reset names of undefined pumps
   begin
    for k:= PumpControl.PumpNumFile + 1 to PumpControl.PumpNum do
     (FindComponent('Pump' + IntToStr(k) + 'GB1')
      as TGroupBox).Caption:= 'Pump ' + IntToStr(k);
   end;
   if PumpControl.ValveNum > 0 then
   begin
    for k:= PumpControl.PumpNumFile + 1 to PumpControl.PumpNumFile + PumpControl.ValveNum do
     (FindComponent('Valve' + IntToStr(k - PumpControl.PumpNumFile) + 'RG1')
      as TRadioGroup).Caption:= Copy(StringList[k], Length(PumpControl.ValvePrefix) + 1,
                                     Length(StringList[k])); // omit the prefix
   end;
   // reset names of undefined valves
   if PumpControl.ValveNum < 8 then // we only support 8 valves
   begin
    for k:= PumpControl.ValveNum + 1 to 8 do
     (FindComponent('Valve' + IntToStr(k) + 'RG1')
      as TRadioGroup).Caption:= 'Valve ' + IntToStr(k);
   end;
  end;

  // read the calibration settings
  UseCalibCB.Checked:= false; // will later be re-eabled if possible
  for j:= PumpControl.PumpNumFile + 1 to StringList.Count - 1 do
  begin
   if LeftStr(StringList[j], Length('Calibration')) = 'Calibration' then
   begin
    CalibStepCB.Text:= Copy(StringList[j], Length('Calibration') + 3,
                            Length(StringList[j]));
    // we know now that there are calibration settings
    UseCalibCB.Checked:= true;
    // when no .def file is loaded, we must disable the RunBB button
    if not HaveDefFileCB.Checked then
    begin
     RunBB.Enabled:= False;
     RunBB.Hint:= 'Calibration is used but no sensor definition file is loaded';
     MessageDlgPos('Calibration is used but no sensor definition file is loaded yet.'
      + LineEnding + 'Thus the validity of the calibration channels cannot be checked.'
      + LineEnding
      + LineEnding + 'If the calibration channel is not selected after you load the'
      + LineEnding + 'sensor defintion file, you must reload the action file!',
      mtWarning, [mbOK], 0, FormPointer.X, FormPointer.Y);
    end
    else
     RunBB.Enabled:= (HavePumpSerialCB.Checked or HasNoPumpsCB.Checked); // might have been disabled before
   end
   // now the concentration values
   else if LeftStr(StringList[j], Length('Glucose:')) = 'Glucose:' then
   begin
    // the value is between the two spaces in the line
    startIndex:= Pos(' ', StringList[j]);
    stopIndex:= Pos(' ', StringList[j], startIndex + 1);
    GlucoseCalibValueFSE.Value:= StrToFloat(Copy(StringList[j],
                                  startIndex + 1, stopIndex - (startIndex + 1)));
    GlucoseCalibUnitCB.Text:= Copy(StringList[j],
                               stopIndex + 1, Length(StringList[j]) - stopIndex);
   end
   else if LeftStr(StringList[j], Length('Lactate:')) = 'Lactate:' then
   begin
    // the value is between the two spaces in the line
    startIndex:= Pos(' ', StringList[j]);
    stopIndex:= Pos(' ', StringList[j], startIndex + 1);
    LactateCalibValueFSE.Value:= StrToFloat(Copy(StringList[j],
                                  startIndex + 1, stopIndex - (startIndex + 1)));
    LactateCalibUnitCB.Text:= Copy(StringList[j],
                               stopIndex + 1, Length(StringList[j]) - stopIndex);
   end
   // now the calibration channels
   else if LeftStr(StringList[j], Length('Glucose ')) = 'Glucose ' then
   begin
    SeriesName:= Copy(StringList[j], Length('Glucose channel') + 3, Length(StringList[j]));
    // check in the ChartListbox if this series is listed and if so, select it
    for k:= 0 to GlucoseCalibCLB.SeriesCount-1 do
    begin
     if not (GlucoseCalibCLB.Series[k].Name = SeriesName) then
      continue;
     GlucoseCalibCLB.Selected[k]:= True;
     break;
    end;
   end
   else if LeftStr(StringList[j], Length('Lactate ')) = 'Lactate ' then
   begin
    SeriesName:= Copy(StringList[j], Length('Lactate channel') + 3, Length(StringList[j]));
    // check in the ChartListbox if this series is listed and if so, select it
    for k:= 0 to LactateCalibCLB.SeriesCount-1 do
    begin
     if not (LactateCalibCLB.Series[k].Name = SeriesName) then
      continue;
     LactateCalibCLB.Selected[k]:= True;
     break;
    end;
   end
   else if LeftStr(StringList[j], Length('Measurements ')) = 'Measurements ' then
   begin
    HelpString:= Copy(StringList[j], Length('Measurements for mean') + 3,
                      Length(StringList[j]));
    UsedCalibValueSE.Value:= StrToInt(HelpString);
   end
   else if LeftStr(StringList[j], Length('Repeat ')) = 'Repeat ' then
   begin
    HelpString:= Copy(StringList[j], Length('Repeat every repeats') + 3,
                      Length(StringList[j]));
    CalibEveryXStepsSE.Value:= StrToInt(HelpString);
   end;
  end;

  // set the pump and valve names for all other steps
  for j:= 2 to PumpControl.StepNum do
  begin
   for k:= 1 to PumpControl.PumpNum do
   begin
    (FindComponent('Pump' + IntToStr(k) + 'GB' + IntToStr(j))
     as TGroupBox).Caption:= (FindComponent('Pump' + IntToStr(k) + 'GB1')
     as TGroupBox).Caption;
   end;
   for k:= 1 to 8 do // we must to this for all supported valves to reset also the unused ones
   begin
    (FindComponent('Valve' + IntToStr(k) + 'RG' + IntToStr(j))
     as TRadioGroup).Caption:= (FindComponent('Valve' + IntToStr(k) + 'RG1')
     as TRadioGroup).Caption;
   end;
  end;

  // update available pumps and valves
  PumpNumberSE.Value:= PumpControl.PumpNumFile;
  ValveNumberSE.Value:= ValveNumFile;

  result:= True;
 finally
  StringList.Free;
 end;
end;

function TMainForm.OpenHandling(InName: string; FileExt: string): string;
// handles the open dialog
var
 OutNameTemp : string;
begin
 result:= '';
 if FileExt = '.PDAction' then
 begin
  OpenDialog.Filter:= 'Pump Driver Actions|*.PDAction';
  OpenDialog.Title:= 'Open action file';
 end
 else if FileExt = '.def' then
 begin
  OpenDialog.Filter:= 'Definition file (*.def)|*.def';
  OpenDialog.Title:= 'Open sensor definition file';
 end
 else if FileExt = '.csv' then
 begin
  OpenDialog.Filter:= 'Sensor data file (*.csv)|*.csv';
  OpenDialog.Title:= 'Open sensor data file';
 end;
 // propose a file name
 if (InName <> '') and (OpenDialog.FileName = '') then
  OpenDialog.FileName:= ExtractFileName(InName);
 // empty existing dialog file name if the extension does not match
 if ExtractFileExt(OpenDialog.FileName) <> FileExt then
  OpenDialog.FileName:= '';
 if OpenDialog.FileName <> '' then
  OpenDialog.FileName:= ExtractFileName(OpenDialog.FileName);
 if OpenDialog.Execute then
 begin
  OutNameTemp:= OpenDialog.FileName;
  // add file extension if it is missing
  if (ExtractFileExt(OutNameTemp) <> FileExt) then
   Insert(FileExt, OutNameTemp, Length(OutNameTemp) + 1);
  if not FileExists(OutNameTemp) then
  begin
   MessageDlg('The file does not exist!', mtError, [mbOK], 0);
   result:= '';
   exit;
  end;
  result:= OutNameTemp;
  // store last used name
  OpenDialog.FileName:= ExtractFileName(OutNameTemp);
 end
 else // was not executed for some reason
  result:= '';
end;

// saving ---------------------------------------------------------------------

procedure TMainForm.SaveActionMIClick(Sender: TObject);
 // writes serial command and pump names into a text file
var
 OutName, command : string;
 SaveFileStream : TFileStream;
 CommandResult: Boolean;
 k : integer;
 SelectedSeries : TChartSeries;
 MousePointer : TPoint;
begin
 MousePointer:= Mouse.CursorPos; // store mouse position

 // generate command according to current settings
 CommandResult:= PumpControl.GenerateCommand(command);
 // if GenerateCommand returns e.g. a too long time do nothing
 if not CommandResult then
  exit;
 CommandM.Text:= command;
 OutName:= '';
 OutName:= SaveHandling(InNamePump, '.PDAction'); // opens file dialog
 if OutName <> '' then
 begin
  if FileExists(OutName) then
  begin
   try
    SaveFileStream:= TFileStream.Create(OutName, fmOpenReadWrite);
   except
    on EFOpenError do
    begin
     MessageDlgPos('Action file is used by another program and cannot be opened.',
                   mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
     exit;
    end;
   end;
   // the new command might be shorter, therefore delete its content
   SaveFileStream.Size:= 0;
  end
  else
  begin
   try
    SaveFileStream:= TFileStream.Create(OutName, fmCreate);
   except
    on EFOpenError do
    begin
     MessageDlgPos('Action file could not be created.' + LineEnding +
                   'Probably you don''t have write access to the specified folder.',
                   mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
     exit;
    end;
   end;
  end;

  try
   // write the command
   SaveFileStream.Write(command[1], Length(command));
   SaveFileStream.Write(LineEnding, 2); // line break
   // write the pump names
   // only do this if there are pumps
   if PumpControl.PumpNum > 0 then
   begin
    for k:= 1 to PumpControl.PumpNum do
    begin
     SaveFileStream.Write(PumpControl.PumpPrefix[1], Length(PumpControl.PumpPrefix)); // prefix
     if (FindComponent('Pump' + IntToStr(k) + 'GB1')
       as TGroupBox).Caption <> '' then // one cannot output an empty name via FileStream.Write
      SaveFileStream.Write((FindComponent('Pump' + IntToStr(k) + 'GB1')
       as TGroupBox).Caption[1],
       Length((FindComponent('Pump' + IntToStr(k) + 'GB1') as TGroupBox).Caption));
     SaveFileStream.Write(LineEnding, 2);
    end;
   end;
   // write the valve names
   // only do this if there are valves
   if PumpControl.ValveNum > 0 then
   begin
    for k:= 1 to PumpControl.ValveNum do
    begin
     SaveFileStream.Write(PumpControl.ValvePrefix[1], Length(PumpControl.ValvePrefix)); // prefix
     if (FindComponent('Valve' + IntToStr(k) + 'RG1')
       as TRadioGroup).Caption <> '' then // one cannot output an empty name via FileStream.Write
      SaveFileStream.Write((FindComponent('Valve' + IntToStr(k) + 'RG1')
       as TRadioGroup).Caption[1],
       Length((FindComponent('Valve' + IntToStr(k) + 'RG1') as TRadioGroup).Caption));
     SaveFileStream.Write(LineEnding, 2);
    end;
   end;
   // write the calibration settings
   // only do this if calibration is used
   if UseCalibCB.Checked then
   begin
    // first the step
    SaveFileStream.Write('Calibration: ', Length('Calibration: '));
    SaveFileStream.Write(CalibStepCB.Text[1], Length(CalibStepCB.Text));
    SaveFileStream.Write(LineEnding, 2);
    // now the substances
    // get the selected calibration channel
    for k:= 0 to GlucoseCalibCLB.SeriesCount-1 do
    begin
     if not GlucoseCalibCLB.Selected[k] then
      continue;
     SelectedSeries:= GlucoseCalibCLB.Series[k] as TChartSeries;
     SaveFileStream.Write('Glucose Channel: ', Length('Glucose Channel: '));
     SaveFileStream.Write(SelectedSeries.Name[1], Length(SelectedSeries.Name));
     SaveFileStream.Write(LineEnding, 2);
     break;
    end;
    // it is important that the value is stored after the channel
    // since on reading the file it is first checked if there is a vaid channel
    SaveFileStream.Write('Glucose: ', Length('Glucose: '));
    SaveFileStream.Write(FloatToStr(GlucoseCalibValueFSE.Value)[1],
                         Length(FloatToStr(GlucoseCalibValueFSE.Value)));
    SaveFileStream.Write(string(' ')[1] , 1);
    SaveFileStream.Write(GlucoseCalibUnitCB.Text[1],
                         Length(GlucoseCalibUnitCB.Text));
    SaveFileStream.Write(LineEnding, 2);
    for k:= 0 to LactateCalibCLB.SeriesCount-1 do
    begin
     if not LactateCalibCLB.Selected[k] then
      continue;
     selectedSeries:= LactateCalibCLB.Series[k] as TChartSeries;
     SaveFileStream.Write('Lactate Channel: ', Length('Lactate Channel: '));
     SaveFileStream.Write(selectedSeries.Name[1], Length(selectedSeries.Name));
     SaveFileStream.Write(LineEnding, 2);
     break;
    end;
    SaveFileStream.Write('Lactate: ', Length('Lactate: '));
    SaveFileStream.Write(FloatToStr(LactateCalibValueFSE.Value)[1],
                         Length(FloatToStr(LactateCalibValueFSE.Value)));
    SaveFileStream.Write(string(' ')[1] , 1);
    SaveFileStream.Write(LactateCalibUnitCB.Text[1],
                         Length(LactateCalibUnitCB.Text));
    SaveFileStream.Write(LineEnding, 2);
    // now the number of measurement values to calculate the mean
    // value taken as calibration result
    SaveFileStream.Write('Measurements for mean: ',
                         Length('Measurements for mean: '));
    SaveFileStream.Write(IntToStr(UsedCalibValueSE.Value)[1],
                         Length(IntToStr(UsedCalibValueSE.Value)));
    SaveFileStream.Write(LineEnding, 2);
    // eventually the number of each reapeats
    SaveFileStream.Write('Repeat every repeats: ',
                         Length('Repeat every repeats: '));
    SaveFileStream.Write(IntToStr(CalibEveryXStepsSE.Value)[1],
                         Length(IntToStr(CalibEveryXStepsSE.Value)));
    SaveFileStream.Write(LineEnding, 2);

   end;
  finally
   SaveFileStream.Free;
  end; //end finally
 end; //end if OutName <> ''
end;

procedure TMainForm.SIXBiosensorsMIClick(Sender: TObject);
begin
 // start a new connection process
 SIXBiosensorsStart(false, false); // always call to connect
end;

procedure TMainForm.SetSIXFactors;
// sets the SIX conversion factors
var
 i : integer;
begin
for i:= 1 to SIXControl.NumChannels do
 begin
  if SIXTypeRG.ItemIndex = 1 then
   GainsRaw[i]:= 0.1526
  else if SIXTypeRG.ItemIndex = 0 then
   GainsRaw[i]:= 0.0763
  else if SIXTypeRG.ItemIndex = 2 then
   GainsRaw[i]:= 0.763
  else if SIXTypeRG.ItemIndex = 3 then
   GainsRaw[i]:= 1.526;
 end;
end;

procedure TMainForm.DisconnectSIX;
// disconnects from SIX
begin
 ConnComPortSensM.Text:= 'Not connected';
 ConnComPortSensM.Color:= clHighlight;
 IndicatorSensorP.Caption:= '';
 IndicatorSensorP.Color:= clDefault;
 LoadSensorDataMI.Enabled:= true;
 if HaveSerialSensorCB.Checked then
 begin
  CloseLazSerialConn;
  IndicatorSensorP.Caption:= 'SIX stopped';
  IndicatorSensorP.Color:= clHighlight;
 end;
 // SIX type can now be set again
 SIXTypeRG.Enabled:= true;
 RawCurrentCB.Enabled:= true;
 LoadDefBB.Enabled:= true;
end;

procedure TMainForm.SIXBiosensorsStart(Connected: Boolean; Disconnect: Boolean);
// if Connected is false a port scan is done and a dialog to connect is opened
// in every case set the file to store the sensor data
// if Connected is true, a file dialog is opened to change the sensor data file
var
 Reg : TRegistry;
 i, k, COMNumber, COMIndex, YesNo : integer;
 AppendCounter : Int64;
 MousePointer : TPoint;
 HeaderLine, ReturnName, LastLine, COMPort, LastDefFile, LastSIXID : string;
 dataArray : array[0..24] of byte;
 COMArray : array of string;
 BufferSize : integer = 300; // a line has about 90 characters, so 300 is sufficient
 AppendMinute : double;
begin
 // initialize
 MousePointer:= Mouse.CursorPos;
 LastLine:= '';
 COMArray:= [''];
 AppendCounter:= 0;
 AppendMinute:= 0;
 LastDefFile:= 'None';
 // get the default gain for the raw values
 SetSIXFactors;
 // assume we will get a .def file, will be set to false later if not
 FoundLoadedDefFile:= True;
 // reset temperature connection hint
 NoTempCorrectionCB.Hint:= 'The sensor data will not be corrected' + LineEnding
  + 'by the temperature correction factor that' + LineEnding
  + 'are stored in the sensor definition file' + LineEnding
  + '(only available for running measurements' + LineEnding
  + 'if definition file is loadedor for loaded' + LineEnding
  + 'measurements if values are displayed in nA)';
 // reset SIX connection elements
 if ConnComPortSensL.Caption <> 'Connected To' then
 begin
  ConnComPortSensL.Caption:= 'Connected To';
  ConnComPortSensM.Color:= clHighlight;
  ConnComPortSensM.Text:= 'None';
 end;

 if Disconnect then
 begin
  DisconnectSIX;
  exit;
 end;

 // connect to SIX
 if not Connected then
 begin
  // if no .def file loaded, issue file open dialog
  // user can cancel it if he really does not like to have a .def file
  if not HaveDefFileCB.Checked then
  begin
   LoadDefBBClick(LoadDefBB as TObject);
   // if the user canceled only raw values possible
   if not HaveDefFileCB.Checked then
   begin
    RawCurrentCB.Checked:= true;
    RawCurrentCB.Enabled:= false;
    FoundLoadedDefFile:= False;
    NoSubtractBlankCB.Checked:= false;
    NoSubtractBlankCB.Enabled:= false;
    NoTempCorrectionCB.Checked:= false;
    NoTempCorrectionCB.Enabled:= false;
   end;
  end;

  // determine all possible COM ports
  Reg:= TRegistry.Create;
  try
   Reg.RootKey:= HKEY_LOCAL_MACHINE;
   if Reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') then
   begin
    with SerialUSBSelectionF do
    begin
     SerialUSBPortCB.Items.Clear;
     SerialUSBPortCB.Sorted:= false;
     Reg.GetValueNames(SerialUSBPortCB.Items);
     for i:= 0 to SerialUSBPortCB.Items.Count - 1 do
      SerialUSBPortCB.Items[i]:= Reg.ReadString(SerialUSBPortCB.Items[i]);
     SerialUSBPortCB.Sorted:= true;
    end;
   end;
  finally
   Reg.Free;
  end;

  // scan for SIX devices
  COMPortScan('SIX');

  with SerialUSBSelectionF do
  begin
   // remove all entries that are no SIX devices
   i:= 0;
   While i < SerialUSBPortCB.Items.Count do
   begin
    COMNumber:= StrToInt(Copy(SerialUSBPortCB.Items[i], 4, 4));
    if COMListSIX[COMNumber] < 1 then
     SerialUSBPortCB.Items.Delete(i)
    else
     inc(i);
   end;

   // output SIX ID
   SerialUSBPortCB.Sorted:= false;
   SetLength(COMArray, SerialUSBPortCB.Items.Count);
   for i:= 0 to SerialUSBPortCB.Items.Count-1 do
   begin
    COMNumber:= StrToInt(Copy(SerialUSBPortCB.Items[i], 4, 4));
    COMArray[i]:= SerialUSBPortCB.Items[i];
    if COMListSIX[COMNumber] > 0 then
     SerialUSBPortCB.Items[i]:= 'SIX ID #: ' + IntToStr(COMListSIX[COMNumber]);
   end;

   ConnectBB.Enabled:= true;
   SerialUSBPortCB.Text:= '';
   if SerialUSBPortCB.Items.Count = 0 then
   begin
    ConnectBB.Enabled:= false;
    SerialUSBPortCB.Text:= 'No SIX found';
   end
   // if there is only one COM port, preselect it
   else if SerialUSBPortCB.Items.Count = 1 then
    SerialUSBPortCB.ItemIndex:= 0
   else
   begin
    // if there is already a connection, display its port
    if HaveSerialSensorCB.Checked then
     SerialUSBPortCB.ItemIndex:=
      SerialUSBPortCB.Items.IndexOf(ConnComPortSensM.Lines[1])
    else
     SerialUSBPortCB.ItemIndex:= -1;
   end;
   // update the text since this will be displayed
   // as proposal when the connection dialog is shwon
   if SerialUSBPortCB.ItemIndex > -1 then
    SerialUSBPortCB.Text:= SerialUSBPortCB.Items[SerialUSBPortCB.ItemIndex];
   if SerialUSBPortCB.Text = '' then
    COMPort:= '';

   // open connection dialog
   // first change its appearance
   SerialUSBPortL.Caption:= 'Select SIX device';
   Caption:= 'SIX selection';
   ShowModal;
   // change appearance back
   SerialUSBPortL.Caption:= 'Serial USB Port';
   Caption:= 'Serial port selection';

   if ModalResult = mrOK then
   begin
    COMPort:= SerialUSBPortCB.Text;
    COMIndex:= SerialUSBPortCB.ItemIndex;
   end;
  
   if ModalResult = mrNo then // user pressed Cancel
   begin
    DisconnectSIX;
    exit;
   end;
  end; // end with SerialUSBSelectionF

  if COMPort = '' then // user set no COM port or canceled
  begin
   if SerialUSBSelectionF.ModalResult = mrCancel then
    exit; // nothing needs to be done
   MessageDlgPos('Error: No COM port selected.',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   IndicatorSensorP.Caption:= 'Connection failure';
   IndicatorSensorP.Color:= clRed;
   if HaveSerialSensorCB.Checked then
   begin
    CloseLazSerialConn;
    IndicatorSensorP.Caption:= 'SIX stopped';
    IndicatorSensorP.Color:= clHighlight;
   end;
   exit;
  end;

  COMPort:= COMArray[COMIndex];
  // open new connection if not already available
  if not (HaveSerialSensorCB.Checked and (COMPort = ConnComPortSensM.Lines[0])) then
  try
   CloseLazSerialConn;
   // open the connection
   try
    serSensor:= TBlockSerial.Create;
    serSensor.DeadlockTimeout:= 5000; //set timeout to 5 s
    serSensor.Connect(COMPort);
    // the config must be set after the connection
    serSensor.config(9600, 8, 'N', SB1, False, False);
   except
    exit;
   end;

  finally
   if serSensor.LastError <> 0 then // output the error
   begin
    MessageDlgPos(ConnComPortSensM.Lines[0] + ' error: ' + serSensor.LastErrorDesc,
     mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    CloseLazSerialConn;
    IndicatorSensorP.Caption:= 'Connection failure';
    IndicatorSensorP.Color:= clRed;
    ConnComPortSensM.Color:= clRed;
    exit;
   end
   else
   begin
    HaveSerialSensorCB.Checked:= True;
    COMNumber:= StrToInt(Copy(COMPort, 4, 4));
    connectedSIX:= COMListSIX[COMNumber];
   end;
  end
  else // there is nothing to do because the connection is already open
   exit;

  ConnComPortSensM.Color:= clDefault;
  // output the connected port and SIX ID
  // we don't just add a line because this would add a linebreak so that a
  // third memo line would be shown and the memo size is designed for 2 lines
  ConnComPortSensM.Text:= COMPort
   + LineEnding + 'SIX ID #: ' + IntToStr(connectedSIX);

  // read out some data as test
  // first wait until we get bytes to read
  k:= 0;
  while serSensor.WaitingDataEx < 25 do
  begin
   delay(100);
   inc(k);
   if k > 29 then // we reached 3 seconds, so there is something wrong
   begin
    MessageDlgPos('Error: ' + ConnComPortSensM.Text + ' did not deliver data within 3 s.',
     mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
    CloseLazSerialConn;
    ConnComPortSensM.Color:= clRed;
    IndicatorSensorP.Caption:= 'Wrong device';
    IndicatorSensorP.Color:= clRed;
    exit;
   end;
  end;

  // read now 25 bytes
  k:= serSensor.RecvBufferEx(@dataArray[0], 25, 50);
  // in case the read failed or not 25 bytes received
  if (serSensor.LastError <> 0) or (k <> 25) then
  begin
   MessageDlgPos(COMPort + ' error on reading 25 bytes: '
    + serSensor.LastErrorDesc, mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   CloseLazSerialConn;
   ConnComPortSensM.Color:= clRed;
   IndicatorSensorP.Caption:= 'Wrong device';
   IndicatorSensorP.Color:= clRed;
   exit;
  end;

  // There might have been a sensor data file loaded before the SIX connection
  // was established. We need to trigger the loading of a new sensor data file.
  InNameSensor:= '';

 end; // end if not Connected to SIX

 // now open the file dialog to select the file to save the SIX data
 // if there is already a connection, display its port
 if not HaveSerialSensorCB.Checked then
  InNameSensor:= '';
 ReturnName:= SaveHandling(InNameSensor, '.csv'); // opens file dialog

 // it is not sensible to keep the sensor connected if its data
 // is not written to a file, thus close in this case the connection
 if (not HaveSensorFileStream)
   and ((ReturnName = 'canceled') or (ReturnName = '')) then
 begin
  CloseLazSerialConn;
  IndicatorSensorP.Caption:= 'No sensor data file';
  IndicatorSensorP.Color:= clHighlight;
  exit;
 end;

 // if the same file was selected or a file is already loaded and the
 // loading of a new one was canceled, nothing needs to be done
 if (InNameSensor = ReturnName)
   or HaveSensorFileStream and (ReturnName = 'canceled') then
  exit
 else
 begin
  // we first have to assure that a previous file is made free
  if HaveSensorFileStream then
  begin
   SensorFileStream.Free;
   HaveSensorFileStream:= false;
  end;
  InNameSensor:= ReturnName;
 end;
 if InNameSensor <> '' then
 begin
  try
   if FileExists(InNameSensor) then
   begin
    // we can try to append data
    // try to read the data from the file into the chart
    if not ReadSensorData('none', AppendMinute, AppendCounter,
     LastDefFile, LastSIXID) then
    begin
     MessageDlgPos('The input file cannot be used to append sensor data.',
                   mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
     CloseLazSerialConn;
     IndicatorSensorP.Caption:= 'Damaged sensor data file';
     IndicatorSensorP.Color:= clRed;
     exit;
    end;
    if LastDefFile <> LoadedDefFileM.Text then
    begin
     with CreateMessageDialog
        ('The input data file used the sensor definition file "' + LastDefFile + '"'
         + LineEnding
         + 'while the the currently loaded one is "' + LoadedDefFileM.Text + '".'
         + LineEnding + LineEnding
         + 'Do you want to stop, load another data file or definition file' + LineEnding
         + 'and then reconnect to the SIX?',
         mtWarning, [mbYes]+[mbNo]) do
     try
      ActiveControl:= FindComponent('NO') as TWinControl;
      YesNo:= ShowModal;
     finally
      Free;
     end;
     if YesNo = mrYes then // only for Yes we need to do something
     begin
      CloseLazSerialConn;
      IndicatorSensorP.Caption:= 'Connection aborted';
      IndicatorSensorP.Color:= clHighlight;
      exit;
     end;
    end;
    if LastSIXID <> ConnComPortSensM.Lines[1] then
    begin
     with CreateMessageDialog
        ('The input data file used "' + LastSIXID + '"'
         + LineEnding
         + 'while the the currently connected one "' + ConnComPortSensM.Lines[1]
         + '".' + LineEnding + LineEnding
         + 'Do you want to stop and reconnect to another the SIX?',
         mtWarning, [mbYes]+[mbNo]) do
     try
      ActiveControl:= FindComponent('NO') as TWinControl;
      YesNo:= ShowModal;
     finally
      Free;
     end;
     if YesNo = mrYes then // only for Yes we need to do something
     begin
      CloseLazSerialConn;
      IndicatorSensorP.Caption:= 'Connection aborted';
      IndicatorSensorP.Color:= clHighlight;
      exit;
     end;
    end;
    // read second to last line from existing file to determine the last time
    // the last line might be incomplete, thus the second to last
    // Note: we purpusely don't use a TStreamReader because the files can be
    // very big. Instead we put the file into the memory and seek back.
    try
     SensorFileStream:= TFileStream.Create(InNameSensor, fmOpenRead or fmShareDenyNone);
    except
     on EFOpenError do
     begin
      MessageDlgPos('Sensor data file is used by another program and cannot be opened.',
                    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
      CloseLazSerialConn;
      IndicatorSensorP.Caption:= 'Data file inaccessible';
      IndicatorSensorP.Color:= clRed;
      exit;
     end;
    end;
    SensorFileStream.Seek(-1*BufferSize, soFromEnd);
    SetLength(lastLine, BufferSize);
    SensorFileStream.Read(lastLine[1], BufferSize);
    SensorFileStream.Free;
    // we know the file ends with a newline, thus we only need to search the previous one
    for i:= BufferSize - 20 downto 1 do
     if lastLine[i] = #10 then
     begin
      LastLine:= Copy(lastLine, i + 1, BufferSize - i);
      break;
     end;
    // recreate a fresh new file stream
    SensorFileStream:= TFileStream.Create(InNameSensor, fmOpenWrite or fmShareDenyNone);
    HaveSensorFileStream:= true; // assures stream will be closed in case of error
    // go to its end
    SensorFileStream.Seek(0, soFromEnd);
    if AppendCounter < 2 then
    begin
     MessageDlgPos('Either no line of the input file starts with an integer'
      + LineEnding + 'or file contains not at least 2 complete data lines.',
      mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
      CloseLazSerialConn;
      IndicatorSensorP.Caption:= 'Damaged sensor data file';
     exit;
    end
    else
     SIXControl.signalCounter:= AppendCounter;
    SIXControl.timeCounter:= AppendMinute;
    HeaderLine:= 'Appended: ';
    // We must update the header string to the channel names read out from the
    // data file. We do this here after all checks and not during the parsing.
    for i:= 1 to SIXControl.NumChannels do
     SIXControl.HeaderStrings[i]:= (FindComponent('Channel'
                                    + IntToStr(i) + 'GB') as TGroupBox).Caption;
    // the file was sucessfully read and we can read the notes
    SIXControl.ReadNotes;
   end
   else // new file
   begin
    SensorFileStream:= TFileStream.Create(InNameSensor, fmCreate or fmShareDenyNone);
    HeaderLine:= 'Created: ';
    // start the counters
    SIXControl.timeCounter:= 0.0;
    SIXControl.signalCounter:= 0;
    // delete existing live chart data
    // but purposely not the measurement data
    for i:= 1 to 8 do
     (FindComponent('SIXCh' + IntToStr(i) + 'Values')
      as TLineSeries).Clear;
    SIXTempValues.Clear;
    // output start time
    StartTimeLE.Text:= FormatDateTime('dd.mm.yyyy hh:nn:ss', now);
    // assure that not a title of a previous measurement is taken
    SIXCH.Title.Text[0]:= 'SIX Values';
   end;
   SIXControl.DelayReadCounter:= 0; // for the case there was a previous run
  except
   SensorFileStream.Free;
   CloseLazSerialConn;
   LoadedFileSensM.Color:= clRed;
   LoadedFileSensM.Hint:= 'Sensor file could not be created or written';
   exit;
  end;
 end //end if OutName <> ''
 else
 begin
  MessageDlgPos('Error: A filename must be set to store the sensor data.',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   CloseLazSerialConn;
   ConnComPortSensM.Color:= clRed;
   IndicatorSensorP.Caption:= 'No file to save';
   IndicatorSensorP.Color:= clRed;
   exit;
 end;

 // write header lines
 HeaderLine:= HeaderLine + FormatDateTime('dd.mm.yyyy hh:nn:ss', now)
  + LineEnding;
 HeaderLine:= HeaderLine + 'Diagram title: ' + SIXCH.Title.Text[0] + LineEnding;
 // output the used SIX
 HeaderLine:= HeaderLine + 'Used ' + ConnComPortSensM.Lines[1] + LineEnding;
 if not HaveDefFileCB.Checked then
 begin
  HeaderLine:= HeaderLine + 'Counter' + #9 + 'Time [min]' + #9;
  for i:= 1 to SIXControl.NumChannels do
   HeaderLine:= HeaderLine + 'Ch' + IntToStr(i) + ' [nA]' + #9;
  HeaderLine:= HeaderLine + 'Temp [deg C]' + #9 + LineEnding;
 end
 else
 begin
  HeaderLine:= HeaderLine + 'Used definition file: "' + LoadedDefFileM.Text
   + '.def"' + LineEnding;
  HeaderLine:= HeaderLine + 'Counter' + #9 + 'Time [min]' + #9;
  // the blank channels have the unit nA
  for i:= 1 to 6 do
  begin
   if (Pos('Blank', SIXControl.HeaderStrings[i]) <> 0)
    or (Pos('blank', SIXControl.HeaderStrings[i]) <> 0) then
    SIXControl.isBlank[i]:= true
   else
    SIXControl.isBlank[i]:= false;
  end;
  // output all non-blank channels
  for i:= 1 to SIXControl.NumChannels do
   if not SIXControl.isBlank[i] then
    HeaderLine:= HeaderLine + SIXControl.HeaderStrings[i] + ' [mM]' + #9;
  HeaderLine:= HeaderLine + 'Temp [deg C]' + #9;
  // also store the raw values
  for i:= 1 to SIXControl.NumChannels do
   HeaderLine:= HeaderLine + SIXControl.HeaderStrings[i] + ' [nA]' + #9;
  HeaderLine:= HeaderLine + LineEnding;
 end;
 try
  SensorFileStream.Write(HeaderLine[1], Length(HeaderLine));
 except
  SensorFileStream.Free;
  CloseLazSerialConn;
  LoadedFileSensM.Color:= clRed;
  LoadedFileSensM.Hint:= 'Writing to sensor file failed';
  exit;
 end;

 // we have now a valid file stream
 HaveSensorFileStream:= true;
 // the file can now be changed
 ChangeSensorDataFileMI.Enabled:= True;

 LoadedFileSensM.Color:= clActiveCaption;
 // show the full path as tooltip
 LoadedFileSensM.Hint:= InNameSensor;
 // set Text after Hint since this change triggers the sync with the other tabs
 LoadedFileSensM.Text:= ExtractFileNameOnly(InNameSensor);

 // enable chart scrolling
 ScrollViewCB.Enabled:= true;

 // final UI settings

 // enable analog output when also connected to a pump driver
 if ConnComPortPumpLE.Color = clDefault then
 begin
  AnOutOnOffTB.Enabled:= true;
  AnOutOnOffTB.Hint:= 'Outputs the sensor signal' + LineEnding
                          + 'to the pump connectors';
 end;
 // the user must not change the type while it is connected
 SIXTypeRG.Enabled:= false;

 // set chart legend according to current names
 for i:= 1 to 6 do
 begin
  (FindComponent('SIXCh' + IntToStr(i) + 'Values')
   as TLineSeries).Title:=
    (FindComponent('Channel' + IntToStr(i) + 'GB')
     as TGroupBox).Caption;
  (FindComponent('SIXCh' + IntToStr(i) + 'Results')
   as TLineSeries).Title:=
    'Stable ' + (FindComponent('Channel' + IntToStr(i) + 'GB')
     as TGroupBox).Caption;
 end;

 // set legend for the operation channels
 for i:= 7 to 8 do
 begin
  if (FindComponent('Channel' + IntToStr(i) + 'CB')
     as TComboBox).Text = 'mean(#2, #5)' then
   (FindComponent('SIXCh' + IntToStr(i) + 'Values') as TLineSeries).Title:=
    'Mean (' + Channel2GB.Caption + ', ' + Channel5GB.Caption + ')'
  else if (FindComponent('Channel' + IntToStr(i) + 'CB')
     as TComboBox).Text = 'mean(#3, #6)' then
   (FindComponent('SIXCh' + IntToStr(i) + 'Values') as TLineSeries).Title:=
    'Mean (' + Channel3GB.Caption + ', ' + Channel6GB.Caption + ')'
  else if (FindComponent('Channel' + IntToStr(i) + 'CB')
     as TComboBox).Text = 'mean(#1, #4)' then
   (FindComponent('SIXCh' + IntToStr(i) + 'Values') as TLineSeries).Title:=
    'Mean (' + Channel1GB.Caption + ', ' + Channel4GB.Caption + ')';
  // we use the same legend name for Live and Result charts
  (FindComponent('SIXCh' + IntToStr(i) + 'Results') as TLineSeries).Title:=
   (FindComponent('SIXCh' + IntToStr(i) + 'Values') as TLineSeries).Title;
 end;

 // only show data that should be shown
 for i:= 1 to SIXControl.NumChannels do
 (FindComponent('SIXCh' + IntToStr(i) + 'Values')
   as TLineSeries).Active:=
   (FindComponent('Channel' + IntToStr(i) + 'OnOffCB')
    as TCheckBox).Checked;
 if SIXControl.NumChannels < 6 then
  for i:= SIXControl.NumChannels + 1 to 6 do
  (FindComponent('SIXCh' + IntToStr(i) + 'Values')
    as TLineSeries).Active:= false;
 SIXTempValues.Active:= ShowTempCB.Checked;

 // disable menu to load existing sensor data
 LoadSensorDataMI.Enabled:= false;
 if HaveDefFileCB.Checked then
 begin
  RawCurrentCB.Enabled:= true;
  NoTempCorrectionCB.Enabled:= true;
  NoSubtractBlankCB.Enabled:= true;
 end;

 // we can now set the timer interval
 ReadTimer.Interval:= Trunc(EvalTimeFSE.Value * 1000); // in ms
 ReadTimer.Enabled:= true;

 // assure that at least one channel is displayed
 AssureChannelDisplay;

 // if linearity test enabled, enable button to start an action
 if PerformLinearityCB.Checked then
  RunBB.Enabled:= true;

end;

procedure TMainForm.SIXCHAfterDrawBackWall(ASender: TChart; ACanvas: TCanvas;
  const ARect: TRect);
begin
 if LeftLine.Active then
 begin
  ACanvas.Brush.Color := RgbToColor(255, 240, 240);;
  ACanvas.FillRect(
   // (the {%H-} is a directive to silence a compiler hint)
   SIXCH.XGraphToImage(LeftLine.Position){%H-},
   SIXCH.YGraphToImage(TopLine.Position){%H-},
   SIXCH.XGraphToImage(RightLine.Position){%H-},
   SIXCH.YGraphToImage(BottomLine.Position){%H-});
 end;
end;

procedure TMainForm.StartFitBClick(Sender: TObject);
begin
 SIXControl.SCStartFitBClick(Sender);
end;

procedure TMainForm.CloseLazSerialConn;
var
 i : integer;
begin
 // stop timer
 ReadTimer.Enabled:= false;
 // close open file stream
 if HaveSensorFileStream then
 begin
  SensorFileStream.Free;
  HaveSensorFileStream:= false;
  InNameSensor:= '';
 end;
 if HaveSerialSensorCB.Checked then
 begin
  // close connection
  serSensor.CloseSocket;
  serSensor.free;
  HaveSerialSensorCB.Checked:= False;
  connectedSIX:= 0;
  // empty all actual signal outputs
  for i:= 1 to 8 do
   (FindComponent('CurrChannel' + IntToStr(i) + 'LE')
    as TLabeledEdit).Text:= '';
  SIXTempLE.Text:= '';
 end;

 ConnComPortSensM.Text:= 'Not connected';
 ConnComPortSensM.Color:= clHighlight;
 ChangeSensorDataFileMI.Enabled:= False;
 LoadSensorDataMI.Enabled:= true;
 AnOutOnOffTB.Checked:= false;
 AnOutOnOffTB.Enabled:= false;
 AnOutOnOffTB.Hint:= 'Outputs the sensor signal' + LineEnding
                     + 'to the pump connectors.' + LineEnding
                     + 'Connect to a SIX and a pump driver'  + LineEnding
                     + 'to enable the button.';
end;

procedure TMainForm.ClosePumpSerialConn;
begin
 if HavePumpSerialCB.Checked then
 begin
  // close connection
  serPump.CloseSocket;
  serPump.Free;
  HavePumpSerialCB.Checked:= False;
  connectedPumpName:= '';
  connectedPumpDriver:= 0;
  StopBBClick(StopBB as TObject);
 end;
end;

function TMainForm.SaveHandling(InName: string; FileExt: string): string;
// handles the save dialog
var
 YesNo : integer;
 OutNameTemp, DialogText : string;
 MousePointer : TPoint;
begin
 // initialize
 MousePointer:= Mouse.CursorPos;
 result:= '';

 if FileExt = '.PDAction' then
 begin
  SaveDialog.Filter:= 'Pump Driver Actions|*.PDAction';
  SaveDialog.Title:= 'Save file as';
 end
 else if FileExt = '.csv' then
 begin
  SaveDialog.Filter:= 'Table (*.csv)|*.csv';
  SaveDialog.Title:= 'Save data as';
 end
 else if FileExt = '.svg' then
 begin
  SaveDialog.Filter:= 'Vector graphics (*.svg)|*.svg';
  SaveDialog.Title:= 'Save screenshot as';
 end
 else if FileExt = '.def' then
 begin
  SaveDialog.Filter:= 'SIX definition file (*.def)|*.def';
  SaveDialog.Title:= 'Save changed definition file as';
 end;
 // clear filename if the extension does not fit
 if (SaveDialog.FileName <> '')
   and (ExtractFileExt(SaveDialog.FileName) <> FileExt) then
  SaveDialog.FileName:= '';
 // propose a file name
 if ((InName <> '') and (SaveDialog.FileName = ''))
  or
   ((InName <> '') and (SaveDialog.FileName <> '')
    and (ExtractFileExt(InName) <> ExtractFileExt(SaveDialog.FileName))) then
  SaveDialog.FileName:= ExtractFileName(InName);
 if SaveDialog.Execute then
 begin
  OutNameTemp:= SaveDialog.FileName;
  // add file extension if it is missing
  if (ExtractFileExt(OutNameTemp) <> FileExt) then
   Insert(FileExt, OutNameTemp, Length(OutNameTemp) + 1);

  if FileExists(OutNameTemp) and (FileExt = '.def') then
  begin
   MessageDlgPos('The new definition file must have a unique filename'
    + LineEnding + 'to distinguish it from prior definition files.',
    mtError, [mbOK], 0, MousePointer.X, MousePointer.Y);
   exit;
  end;

  if FileExists(OutNameTemp) then
  begin
   if FileExt = '.csv' then
   begin
    // if file is already in use, nothing has to be done
    if HaveSensorFileStream and (OutNameTemp = InNameSensor) then
    begin
     result:= OutNameTemp;
     exit;
    end;
    DialogText:= 'Do you want to append the sensor data to the existing file';
   end
   else
    DialogText:= 'Do you want to overwrite the existing file';

   with CreateMessageDialog
        (DialogText + LineEnding
             + ExtractFileName(OutNameTemp) + ' ?',
             mtWarning, [mbYes]+[mbNo]) do
   try
    ActiveControl:= FindComponent('NO') as TWinControl;
    YesNo:= ShowModal;
   finally
    Free;
   end;
   if YesNo = mrNo then // if No
   begin
    SaveHandling(InName, FileExt);
    exit;
   end
   else // if Yes
   begin
    result:= OutNameTemp;
    // store last used name
    SaveDialog.FileName:= ExtractFileName(OutNameTemp);
    exit;
   end;
  end; // end if FileExists

  result:= OutNameTemp;
  // store last used name
  SaveDialog.FileName:= ExtractFileName(OutNameTemp);
 end
 else // the user canceled the dialog
 begin
  if FileExists(InName) and (SaveDialog.FileName = InName) then
   result:= 'canceled';
 end;

end;

procedure TMainForm.COMPortScan(PortType: string);
// this routine scanes all open COM ports for a SIX and pump drivers
type
 TDataArray = array[0..24] of byte;
var
 Reg : TRegistry;
 RegStrings : TStrings;
 PortName, connectedPortNameSIX, connectedPortNameDriver,
   command, FirmwareVersion, driverFeedback : string;
 serTest : TBlockSerial;
 i, j, k, ErrorCount, StopPos, Attempts, Channel : integer;
 dataString : AnsiString;
 dataArray : TDataArray;
 tempArray : packed array of byte;
 IDArray : array[0..3] of byte;
 NextPort : Boolean;
begin
 // determine all possible COM ports
 Reg:= TRegistry.Create;
 RegStrings:= TStringList.Create;
 try
  Reg.RootKey:= HKEY_LOCAL_MACHINE;
  if Reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') then
  begin
   // if connected, get the port number to exclude it from beeing connected
   if HaveSerialSensorCB.Checked then
    for i:= 1 to Length(COMListSIX) - 1 do
    begin
     if COMListSIX[i] = connectedSIX then
      begin
       connectedPortNameSIX:= 'COM' + IntToStr(i);
       break;
      end;
     end;
   if HavePumpSerialCB.Checked then
    for i:= 1 to Length(COMListPumpDriver) - 1 do
    begin
     if COMListPumpDriver[i] = connectedPumpDriver then
      begin
       connectedPortNameDriver:= 'COM' + IntToStr(i);
       break;
      end;
     end;
   if PortType = 'SIX' then
   begin
    SetLength(COMListSIX, 0); // delete array
    SetLength(COMListSIX, 999); // a PC cannot have more than 999 COM ports
   end
   else if PortType = 'PumpDriver' then
   begin
    SetLength(COMListPumpDriver, 0);
    SetLength(COMListPumpDriver, 999);
   end;

   Reg.GetValueNames(RegStrings);
  end;

  // since the COM port scan can take some time depending on how many SIX/pumps
  // are connected, display a progess bar even if there is only one COM port
  ScanningProgressF:= TScanningProgressF.Create(Nil);
  ScanningProgressF.ScanningPB.Max:= RegStrings.Count;
  ScanningProgressF.Show;
  // that the OS can refresh its window list
  Application.ProcessMessages;

  // test all COM ports
  for i:= 0 to RegStrings.Count - 1 do
  begin
   PortName:= Reg.ReadString(RegStrings[i]);
   ScanningProgressF.ScanningPB.Position:= i;

   // that the application is alive and to assure the changed
   // progress bar is shown
   Application.ProcessMessages;

   // Since every SIX has a unique ID, we can connect the COM port with this
   if PortType = 'SIX' then
   begin
    Attempts:= 0;
    ErrorCount:= 0;

    // exclude connected pump driver port
    if HavePumpSerialCB.Checked and (PortName = connectedPortNameDriver) then
     continue;

    // if there is a connection, we can directly take the SIX number
    if HaveSerialSensorCB.Checked and (PortName = connectedPortNameSIX) then
    begin
     Channel:= StrToInt(Copy(connectedPortNameSIX, 4, 4));
     COMListSIX[Channel]:= connectedSIX;
     continue;
    end;

    // open the connection
   try
    try
     serTest:= TBlockSerial.Create;
     serTest.DeadlockTimeout:= 1000; //set timeout to 1 s
     serTest.Connect(PortName);
     // the config must be set after the connection
     serTest.config(9600, 8, 'N', SB1, False, False);
    except
     continue;
    end;

    while ErrorCount = 0 do
    begin
     NextPort:= false;
     StopPos:= -1;
     // read out some data as test
     // first wait until we get bytes to read
     k:= 0;
     while serTest.WaitingDataEx < 25 do
     begin
      delay(100);
      inc(k);
      if k > 19 then // we reached 2 seconds, so this is no SIX
      begin
       NextPort:= true;
       inc(ErrorCount);
       break;
      end;
     end;

     if NextPort then
      continue;

     // read now a packet
     dataString:= '';
     dataString:= serTest.RecvPacket(100);

     if (serTest.LastError <> 0) or (Length(dataString) < 25) then
     begin
      inc(ErrorCount);
      continue;
     end;

     // convert string to a byte array
     SetLength(tempArray{%H-}, Length(dataString));
     Move(dataString[1], tempArray[0], Length(dataString));
     // now search the byte array for the stop bit
     // since a value byte can also have the value $16, we search backwards
     // and check that the byte 20 positions earlier has the value $4 (begin of
     // a data block)
     for j:= Length(tempArray) - 1 downto Length(tempArray) - 5 do
     begin
      if (tempArray[j] = $16) and (tempArray[j - 20] = $4) then
      begin
       StopPos:= j;
       inc(ErrorCount);
       break;
      end;
     end;
     if (StopPos = -1) and (Attempts > 1) then
     begin
      // try again
      inc(Attempts);
      continue;
     end;
     if (Attempts > 1) then
      inc(ErrorCount);
    end; // end while ErrorCount = 0

    if (ErrorCount > 0) and (StopPos = -1) then
     continue;

    // reset counter since we got no error
    ErrorCount:= 0;

    // copy the relevant 25 bytes to the dataArray
    dataArray:= default(TDataArray); // initialize or clear array
    Move(tempArray[StopPos - 24], dataArray[0], 25);
    for j:= 0 to 3 do
     IDArray[3 - j]:= dataArray[19 + j];
    Channel:= StrToInt(Copy(PortName, 4, 4));
    COMListSIX[Channel]:= Int32(IDArray);

   finally
    serTest.Free;
   end;

   end // if PortType = 'SIX'

   else if PortType = 'PumpDriver' then
   begin
    // the pump drivers emits on every received command the firmware
    // this is how we can detect them
    FirmwareVersion:= '';
    ErrorCount:= 0;

    // exclude connected SIX port
    if PortName = connectedPortNameSIX then
     continue;

    // when there is a connection, we must first test if this is still alive
    // - if yes, we must directly take the driver number
    // since we cannot connect to an already connected port
    // - if not we must close the connection
    if HavePumpSerialCB.Checked and (PortName = connectedPortNameDriver) then
    begin
     // to check the live state send a command
     try
      serPump.SendString('/0lR' + LineEnding);
     finally
      if serPump.LastError <> 0 then
      begin
       ConnComPortPumpLE.Color:= clRed;
       ConnComPortPumpLE.Text:= 'Try to reconnect';
       IndicatorPumpP.Caption:= 'Connection failure';
       IndicatorPumpP.Color:= clRed;
       IndicatorPumpPPaint;
       ConnectionMI.Enabled:= True;
       RunBB.Enabled:= False;
       StopBB.Enabled:= False;
       ClosePumpSerialConn;
       inc(ErrorCount);
      end;
     end;
     if ErrorCount > 0 then
      continue;
     Channel:= StrToInt(Copy(connectedPortNameDriver, 4, 4));
     COMListPumpDriver[Channel]:= connectedPumpDriver;
     continue;
    end;

    // open the connection
    try
     try
      serTest:= TBlockSerial.Create;
      serTest.DeadlockTimeout:= 1000; //set timeout to 1 s
      serTest.Connect(PortName);
      // the config must be set after the connection
      serTest.config(9600, 8, 'N', SB1, False, False);
     except
      continue;
     end;

     // get Firmware version by first sending a command and receiving the reply
     try
      // if another pump driver is currently running, don't send it a command
      if serTest.LastError = 0 then
      begin
       command:= '/0lR' + LineEnding;
       serTest.SendString(command);
       driverFeedback:= serTest.Recvstring(1000);
      end;
     finally
      if serTest.LastError <> 0 then
       inc(ErrorCount);
     end;

    finally
     serTest.Free;
    end;

    if ErrorCount > 0 then
     continue;

    // driverFeedback has now either this format:
    // "JT-PumpDriver-ID zz\nJT-PumpDriver-Firmware x.y\n Received command:..."
    // or this format:
    // "JT-PumpDriver-Firmware x.y\n Received command:..."
    // on very old firmware this format:
    // "received command:..."

    // check for a number dot to get the firmware version
    if Pos('JT-PumpDriver-Firmware', driverFeedback) > 0 then
     FirmwareVersion:= copy(driverFeedback, Pos('.', driverFeedback) - 1, 3)
    // omit the 'r' because some versions used a capital letter 'R'
    else if Pos('eceived command:', FirmwareVersion) > 0 then
     FirmwareVersion:= 'unknown'
    else // no pump driver
     continue;

    Channel:= StrToInt(Copy(PortName, 4, 4));
    // determine the driver ID: from first space to first #10
    // (driver uses only #10 for the line ending)
    k:= Length('JT-PumpDriver-ID');
    if copy(driverFeedback, 0, k) = 'JT-PumpDriver-ID' then
    begin
     driverFeedback:= copy(driverFeedback, k + 2,
                           (Pos(#10, driverFeedback) - 1) - (k + 1));
     COMListPumpDriver[Channel]:= StrToInt(driverFeedback);
    end;
    if COMListPumpDriver[Channel] = 0 then // no driver ID
     COMListPumpDriver[Channel]:= 1;

   end; // end else if PortType = 'PumpDriver'

  end; // test all COM ports

 finally
  Reg.Free;
  RegStrings.Free;
  ScanningProgressF.Close;
  ScanningProgressF.Free;
 end;

end;

procedure TMainForm.AssureChannelDisplay;
var
 i, counter : Byte;
begin
 // assure that at least one channel is displayed
 counter:= 0;
 for i:= 1 to SIXControl.NumChannels do
 begin
  if (MainForm.FindComponent('Channel' + IntToStr(i) + 'OnOffCB')
      as TCheckBox).Checked = true then
  begin
   inc(counter);
   break;
  end;
 end;
 if counter = 0 then
  Channel1OnOffCB.Checked:= true;
end;


end.

