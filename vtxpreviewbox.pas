unit VTXPreviewBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, Windows, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  VTXConst, VTXSupport, math,
  BGRABitmap,
  BGRABitmapTypes
  ;

type

  { TfPreview }

  TfPreview = class(TForm)
    pbPreview: TPaintBox;
    pbBottomResizer: TPaintBox;
    pbClose: TPaintBox;
    pbTitleBar: TPaintBox;
    pbTopResizer: TPaintBox;
    ScrollBox1: TScrollBox;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbBottomResizerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbCloseClick(Sender: TObject);
    procedure pbClosePaint(Sender: TObject);
    procedure pbPreviewPaint(Sender: TObject);
    procedure pbTitleBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbTitleBarPaint(Sender: TObject);
    procedure pbTopResizerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScrollBox1Paint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fPreview : TfPreview;
  ScrollWidth : integer;

implementation

{$R *.lfm}

const
  Zoom = 1/4;

{ TfPreview }
procedure TfPreview.FormCreate(Sender: TObject);
var
  loc_SBInfo : 		TNonCLientMetrics;
begin
  loc_SBInfo.cbSize := SizeOf(loc_SBInfo);
  SystemParametersInfo(SPI_GetNonClientMetrics,0,@loc_SBInfo,0);
  ScrollWidth := loc_SBInfo.iScrollWidth;
end;

procedure TfPreview.FormActivate(Sender: TObject);
begin
  SetWindowPos(self.handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
end;

procedure TfPreview.ScrollBox1Paint(Sender: TObject);
var
  fw, w, h : integer;
begin
  // set size of pbPreview to max zoom out for bmpPage
	w := floor(bmpPage.Width * Zoom * XScale);
  h := floor(bmpPage.Height * Zoom);
  fw := w + 8;

  if h > ScrollBox1.ClientHeight then
  	fw += ScrollWidth + 2;

  self.Constraints.MaxWidth:=fw;
  self.Constraints.MinWidth:=fw;
  self.Width := fw;

	pbPreview.Width := w;
  pbPreview.Height := h;
end;


procedure TfPreview.pbPreviewPaint(Sender: TObject);
var
  pb : TPaintBox;
  cnv : TCanvas;
  tmp : TBGRABitmap;
begin
  pb := TPaintBox(Sender);
  cnv := pb.Canvas;
  tmp := bmpPage.Resample(pb.Width, pb.Height, rmFineResample) as TBGRABitmap;
	tmp.Draw(cnv, 0, 0);
	tmp.free;
end;

procedure TfPreview.FormResize(Sender: TObject);
begin
  Invalidate;
end;

procedure TfPreview.FormShow(Sender: TObject);
var
  h, w,  fw : integer;
begin
  if bmpPage = nil then exit;

	w := floor(bmpPage.Width * Zoom * XScale);
  h := floor(bmpPage.Height * Zoom);
  fw := w + 8;

  if h > ScrollBox1.ClientHeight then
  	fw += ScrollWidth + 2;

  self.Constraints.MaxWidth:=fw;
  self.Constraints.MinWidth:=fw;
  self.Width := fw;

	pbPreview.Width := w;
  pbPreview.Height := h;
end;

// caption bar

procedure TfPreview.pbClosePaint(Sender: TObject);
var
  pb : TPaintBox;
  cnv : TCanvas;
  r : TRect;
begin
  pb := TPaintBox(Sender);
  cnv := pb.Canvas;
  r := pb.ClientRect;
	captionCloseUp.Draw(cnv, r);
end;

procedure TfPreview.pbTitleBarMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  SendMessage(Handle, WM_NCLBUTTONDOWN, HTCAPTION, 0);
end;

procedure TfPreview.pbTitleBarPaint(Sender: TObject);
var
  pb : TPaintBox;
  cnv : TCanvas;
  r : TRect;
begin
  pb := TPaintBox(Sender);
  cnv := pb.Canvas;
  r := pb.ClientRect;

  cnv.Brush.Color := ANSIColor[UICaption];
  cnv.FillRect(r);
//  DrawBitmapTiled(textureStone.Bitmap, cnv, r);
  DrawRectangle3D(cnv, r, true);
  cnv.Brush.Style:=bsClear;
  cnv.Font.Color := ANSIColor[UICaptionText];
  cnv.Font.Size := -11;
  cnv.Font.Style := [ fsBold ];
  cnv.TextOut(3,1,'Preview');
end;

procedure TfPreview.pbCloseClick(Sender: TObject);
begin
	Hide;
end;

// resize grabber
procedure TfPreview.pbTopResizerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  SendMessage(Handle, WM_NCLBUTTONDOWN, HTTOP, 0);
end;

// resize grabber
procedure TfPreview.pbBottomResizerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  SendMessage(Handle, WM_NCLBUTTONDOWN, HTBOTTOM, 0);
end;

end.

