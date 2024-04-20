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

  { TForm1 }

  TForm1 = class(TForm)
    BCMaterialEdit1: TBCMaterialEdit;
    BCPanel1: TBCPanel;
    BCPanel2: TBCPanel;
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    ListBox1: TListBox;
    procedure BCMaterialEdit1Change(Sender: TObject);
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    backgroundImage: TBGRABitmap;
  public

  end;

resourcestring
  SEARCH = 'SEARCH';

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  styleForm(Self);
  BCMaterialEdit1.Align := alTop;
  BCMaterialEdit1.Title.Caption := SEARCH;
  backgroundImage := TBGRABitmap.Create;
  backgroundImage.LoadFromResource('BACKGROUND');
  ListBox1.AddItem('Hello World 1', nil);
  ListBox1.AddItem('Hello World 2', nil);
  ListBox1.AddItem('Hello World 3', nil);
  ListBox1.AddItem('Hello World 4', nil);
  ListBox1.AddItem('Hello World 5', nil);
  ListBox1.AddItem('Hello World 6', nil);
  ListBox1.AddItem('Hello World 7', nil);
  ListBox1.AddItem('Hello World 8', nil);
  ListBox1.AddItem('Hello World 9', nil);
  ListBox1.AddItem('Hello World 10', nil);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  backgroundImage.Free;
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
begin

end;

procedure TForm1.ListBox1DrawItem(Control: TWinControl; Index: integer;
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
    (64 - TListBox(Control).Canvas.GetTextHeight(TListBox(Control).Items[Index])) div 2,
    TListBox(Control).Items[Index]);
end;

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.StretchPutImageProportionally(Rect(0, 0, Bitmap.Width, Bitmap.Height),
    taCenter, tlCenter, backgroundImage, dmSet, 255, True);
end;

procedure TForm1.BCMaterialEdit1Change(Sender: TObject);
begin
  debugln(BCMaterialEdit1.Edit.Text);
end;

end.
