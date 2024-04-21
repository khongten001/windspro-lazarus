unit uconfigstyles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ustyles;

type

  { TfrmConfigStyles }

  TfrmConfigStyles = class(TForm)
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmConfigStyles: TfrmConfigStyles;

implementation

{$R *.lfm}

{ TfrmConfigStyles }

procedure TfrmConfigStyles.FormCreate(Sender: TObject);
begin

end;

end.

