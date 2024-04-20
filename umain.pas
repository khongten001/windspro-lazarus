unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ustyles, BCPanel, BCMaterialEdit, BGRAVirtualScreen, BGRABitmap, BCTypes,
  BGRABitmapTypes,
  {$ifdef DEBUG}
  LazLoggerBase
  {$else}
  LazLoggerDummy
  {$endif};

type

  { TForm1 }

  TForm1 = class(TForm)
    BCMaterialEdit1: TBCMaterialEdit;
    BCPanel1: TBCPanel;
    BCPanel2: TBCPanel;
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    procedure BCMaterialEdit1Change(Sender: TObject);
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  backgroundImage.Free;
end;

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.StretchPutImageProportionally(Rect(0, 0, Bitmap.Width, Bitmap.Height), taCenter, tlCenter, backgroundImage, dmSet, 255, True);
end;

procedure TForm1.BCMaterialEdit1Change(Sender: TObject);
begin
  debugln(BCMaterialEdit1.Edit.Text);
end;

end.
