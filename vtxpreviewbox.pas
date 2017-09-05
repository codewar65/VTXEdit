{

BSD 2-Clause License

Copyright (c) 2017, Daniel Mecklenburg Jr. All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

}

unit VTXPreviewBox;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  {$ifdef WINDOWS} Windows, {$endif}
  SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  VTXConst, VTXSupport, math,
  BGRABitmap,
  BGRABitmapTypes
  ;

type

  { TfPreview }

  TfPreview = class(TForm)
    pbPreview: TPaintBox;
    ScrollBox1: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbPreviewPaint(Sender: TObject);
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

{ TfPreview }
procedure TfPreview.FormCreate(Sender: TObject);
{$ifdef WINDOWS}
var
  loc_SBInfo :    TNonCLientMetrics;
{$endif}
begin
  DoubleBuffered:=true;
{$ifdef WINDOWS}
  loc_SBInfo.cbSize := SizeOf(loc_SBInfo);
  SystemParametersInfo(SPI_GetNonClientMetrics,0,@loc_SBInfo,0);
  ScrollWidth := loc_SBInfo.iScrollWidth;
{$else}
  // calculate scrollbar width.
{$endif}
end;

procedure TfPreview.ScrollBox1Paint(Sender: TObject);
var
  fw, w, h : integer;
begin
  // set size of pbPreview to max zoom out for bmpPage
  w := floor(bmpPreview.Width * XScale);
  h := bmpPreview.Height;
  fw := w + 8;

  if h > ScrollBox1.ClientHeight then
    fw += ScrollWidth + 2;

  if width <> fw then
  begin
    self.Constraints.MaxWidth:=fw;
    self.Constraints.MinWidth:=fw;
    self.Width := fw;
  end;
  if pbPreview.Width <> w then
    pbPreview.Width := w;

  if pbPreview.Height <> h then
    pbPreview.Height := h;
end;

// this routine needs better looking / faster update
// maybe drop the tscrollbox, move to panel/image, add scrollbars,
// and only draw displayable chunk?
procedure TfPreview.pbPreviewPaint(Sender: TObject);
var
  pb : TPaintBox;
  cnv : TCanvas;
  bmp : TBGRABitmap;

begin
  pb := TPaintBox(Sender);
  cnv := pb.Canvas;

  bmpPreview.Draw(cnv, pb.ClientRect, false);

//  bmp := bmpPage.Resample(pb.Width, pb.Height, rmFineResample) as TBGRABitmap;
//  bmp.Draw(cnv, pb.ClientRect);
//  bmp.Free;
//  cnv.StretchDraw(pb.ClientRect, bmpPage.Bitmap);
end;

procedure TfPreview.FormShow(Sender: TObject);
var
  h, w, fw : integer;
begin
  if bmpPage = nil then exit;

  w := floor(bmpPreview.Width * XScale);
  h := bmpPreview.Height;
  fw := w + 8;

  if h > ScrollBox1.ClientHeight then
    fw += ScrollWidth + 2;

  if Width <> fw then
  begin
    self.Constraints.MaxWidth:=fw;
    self.Constraints.MinWidth:=fw;
    self.Width := fw;
  end;

  if pbPreview.Width <> w then
    pbPreview.Width := w;

  if pbPreview.Height <> h then
    pbPreview.Height := h;
end;

end.

