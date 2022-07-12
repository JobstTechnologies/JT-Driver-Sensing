unit NoteEditing;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TNoteEditingF }

  TNoteEditingF = class(TForm)
    NoteLabelL: TLabel;
    OKBB: TBitBtn;
    NoteTextM: TMemo;
  private

  public

  end;

var
  NoteEditingF: TNoteEditingF;

implementation

{$R *.lfm}

{ TNoteEditingF }

end.

