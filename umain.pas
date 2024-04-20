unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ustyles, BCPanel, BCMaterialEdit, BGRAVirtualScreen, BGRABitmap, BCTypes,
  BCListBox, BGRABitmapTypes, LCLType,
  {$ifdef DEBUG}
  LazLoggerBase
  {$else}
  LazLoggerDummy
  {$endif}, Types;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    bcmeSearch: TBCMaterialEdit;
    bcpTop: TBCPanel;
    bvpLeft: TBCPanel;
    vsBackgroundImage: TBGRAVirtualScreen;
    lbPrograms: TListBox;
    procedure bcmeSearchChange(Sender: TObject);
    procedure vsBackgroundImageRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbProgramsDblClick(Sender: TObject);
    procedure lbProgramsDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    backgroundImage: TBGRABitmap;
    programs: TStringList;
    procedure SearchAndFill(aSearch: string);
  public

  end;

resourcestring
  SEARCH = 'SEARCH';

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  programs := TStringList.Create;
  styleForm(Self);
  bcmeSearch.Align := alTop;
  bcmeSearch.Title.Caption := SEARCH;
  backgroundImage := TBGRABitmap.Create;
  backgroundImage.LoadFromResource('BACKGROUND');
  programs.Add('NO$GBA');
  programs.Add('DESMUME');
  programs.Add('VBA-LINK');
  programs.Add('VBA-M');
  programs.Add('SUYU');
  programs.Add('LIME 3DS');
  programs.Add('DOLPHIN');
  programs.Add('PROJECT64');
  programs.Add('CEMU');
  programs.Add('MELON DS');
  SearchAndFill('');
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  backgroundImage.Free;
  programs.Free;
end;

procedure TfrmMain.lbProgramsDblClick(Sender: TObject);
begin

end;

procedure TfrmMain.lbProgramsDrawItem(Control: TWinControl; Index: integer;
  ARect: TRect; State: TOwnerDrawState);
var
  bmp: TBGRABitmap;
  textRect: TRect;
begin
  if odSelected in State then
    bmp := TBGRABitmap.Create(ARect.Width, ARect.Height, ACCENT_COLOR)
  else
    bmp := TBGRABitmap.Create(ARect.Width, ARect.Height, BACKGROUND_COLOR);
  bmp.DrawHorizLine(0, bmp.Height - 1, bmp.Width, BORDER_COLOR);
  textRect := ARect;
  textRect.Left := 64;
  bmp.Draw(TListBox(Control).Canvas, ARect.Left, ARect.Top, True);
  bmp.Free;
  TListBox(Control).Canvas.Font.Color := TEXT_COLOR;
  TListBox(Control).Canvas.TextRect(textRect, textRect.Left, textRect.Top +
    (TListBox(Control).ItemHeight - TListBox(Control).Canvas.GetTextHeight(
    TListBox(Control).Items[Index])) div 2,
    TListBox(Control).Items[Index]);
end;

procedure TfrmMain.SearchAndFill(aSearch: string);
var
  contains: boolean;
  i, j: integer;
  arr: TStringArray;
begin
  lbPrograms.Clear;
  arr := aSearch.ToUpper.Split(' ', TStringSplitOptions.ExcludeEmpty);

  for j := 0 to programs.Count - 1 do
  begin
    contains := True;
    for i := 0 to Length(arr) - 1 do
    begin
      if programs[j].Contains(arr[i]) then
        continue
      else
        contains := False;
    end;
    if (contains) then
    begin
      lbPrograms.AddItem(programs[j], nil);
    end;
  end;
end;

procedure TfrmMain.vsBackgroundImageRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.StretchPutImageProportionally(Rect(0, 0, Bitmap.Width, Bitmap.Height),
    taCenter, tlCenter, backgroundImage, dmSet, 255, True);
end;

procedure TfrmMain.bcmeSearchChange(Sender: TObject);
begin
  debugln(bcmeSearch.Edit.Text);
  SearchAndFill(bcmeSearch.Edit.Text);
end;

end.
