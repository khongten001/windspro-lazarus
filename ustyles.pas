unit ustyles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, BGRABitmap, BGRABitmapTypes, BCPanel,
  BCTypes, BCMaterialEdit, Dialogs, StdCtrls, BCButton,
  {$ifdef DEBUG}
  LazLoggerBase
  {$else}
  LazLoggerDummy
  {$endif};

const
  ACCENT_COLOR: TBGRAPixel = (blue: 214; green: 102; red: 3; alpha: 255);
  BACKGROUND_COLOR: TBGRAPixel = (blue: 46; green: 41; red: 36; alpha: 255);
  BACKGROUND_DARK_COLOR: TBGRAPixel = (blue: 37; green: 33; red: 29; alpha: 255);
  BORDER_COLOR: TBGRAPixel = (blue: 20; green: 20; red: 20; alpha: 255);
  TEXT_COLOR: TBGRAPixel = (blue: 250; green: 248; red: 246; alpha: 255);

procedure styleForm(aForm: TForm);

implementation

procedure styleControl(aControl: TWinControl);
var
  i: integer;
begin
  for i := 0 to aControl.ControlCount - 1 do
  begin
    debugln(aControl.Controls[i].ClassName);
    case aControl.Controls[i].ClassName of
      'TBCPanel': begin
        with aControl.Controls[i] as TBCPanel do
        begin
          if (Tag = 1) then
            Background.Color := BACKGROUND_DARK_COLOR
          else
            Background.Color := BACKGROUND_COLOR;
          Border.Color := BORDER_COLOR;
          Border.Style := bboSolid;
          BorderBCStyle := bpsBorder;
          FontEx.Color := TEXT_COLOR;
        end;
      end;
      'TBCMaterialEdit': begin
        with aControl.Controls[i] as TBCMaterialEdit do
        begin
          AccentColor := ACCENT_COLOR;
          Color := BACKGROUND_DARK_COLOR;
          DisabledColor := TEXT_COLOR;
          Edit.Color := BACKGROUND_DARK_COLOR;
          Edit.Font.Color := TEXT_COLOR;
        end;
      end;
      'TListBox': begin
        with aControl.Controls[i] as TListBox do
        begin
          BorderStyle := bsNone;
          Color := BACKGROUND_COLOR;
        end;
      end;
      'TBCButton': begin
        with aControl.Controls[i] as TBCButton do
        begin
          Rounding.RoundX := 0;
          Rounding.RoundY := 0;
          StateNormal.FontEx.Shadow := False;
          StateNormal.Border.Style := bboSolid;
          StateNormal.Border.Color := BORDER_COLOR;
          StateNormal.Background.Style := bbsColor;
          StateNormal.Background.Color := BACKGROUND_DARK_COLOR;
          StateNormal.FontEx.Color := TEXT_COLOR;
          StateHover.Assign(StateNormal);
          StateHover.Background.Color := BACKGROUND_COLOR;
          StateClicked.Assign(StateNormal);
          StateClicked.Background.Color := BACKGROUND_COLOR;
        end;
      end;
    end;
    if aControl.Controls[i] is TWinControl then
      styleControl(TWinControl(aControl.Controls[i]));
  end;
end;

procedure styleForm(aForm: TForm);
begin
  aForm.Color := BACKGROUND_COLOR;
  styleControl(aForm);
end;

end.
