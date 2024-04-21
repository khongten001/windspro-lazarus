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

var
  ACCENT_COLOR: TBGRAPixel;
  BACKGROUND_COLOR: TBGRAPixel;
  BACKGROUND_DARK_COLOR: TBGRAPixel;
  BORDER_COLOR: TBGRAPixel;
  TEXT_COLOR: TBGRAPixel;

procedure styleForm(aForm: TForm);
procedure defaultColors;

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
      'TLabel': begin
        with aControl.Controls[i] as TLabel do
        begin
          Font.Color := TEXT_COLOR;
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
  aForm.Invalidate;
end;

procedure defaultColors;
begin
  ACCENT_COLOR := BGRA(3, 102, 214);
  BACKGROUND_COLOR := BGRA(36, 41, 46);
  BACKGROUND_DARK_COLOR := BGRA(29, 33, 37);
  BORDER_COLOR := BGRA(20, 20, 20);
  TEXT_COLOR := BGRA(246, 248, 250);
end;

initialization
  defaultColors;

end.
