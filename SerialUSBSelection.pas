unit SerialUSBSelection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TSerialUSBSelectionF }

  TSerialUSBSelectionF = class(TForm)
    OKButtonB: TButton;
    CancelButtonB: TButton;
    SerialUSBPortCB: TComboBox;
    SerialUSBPortL: TLabel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  SerialUSBSelectionF: TSerialUSBSelectionF;
  COMPort : string;
  COMIndex : integer;

implementation

{$R *.lfm}

procedure TSerialUSBSelectionF.FormCreate(Sender: TObject);
begin
 ActiveControl:= SerialUSBPortCB;
end;


end.

