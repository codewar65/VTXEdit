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
  if bmpPreview = nil then exit;

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
  pb :              TPaintBox;
  cnv :             TCanvas;
  bmp, bmp2 :       TBGRABitmap;
  i, r, c, x, y :   integer;
  off :             longint;
  cell :            TCell;
  cp :              TEncoding;
  objonrow :        boolean;
  objnum :          integer;
  neighbors :       byte;
begin
  if (bmpPreview = nil) then exit;

  pb := TPaintBox(Sender);
  cnv := pb.Canvas;
  cnv.Draw(0, 0, bmpPreview.Bitmap);

  // draw objects over top
  // from topmost to bottommost
  bmp := TBGRABitmap.Create(8, 16);
  for r := 0 to NumRows - 1do
  begin
    // any objects on this row?
      y := (r << 2);
      for c := 0 to NumCols - 1 do
      begin
        x := floor((c << 1) * XScale);
        objnum := GetObjectCell(r, c, cell);
        if (objnum >= 0) and (not Objects[objnum].Hidden) then
          if cell.Chr <> _EMPTY then
          begin
            // object here.
            cp := Fonts[GetBits(cell.Attr, A_CELL_FONT_MASK, 28)];
            if (cp = encUTF8) or (cp = encUTF16) then
              off := GetGlyphOff(cell.Chr, CPages[cp].GlyphTable, CPages[cp].GlyphTableSize)
            else
            begin
              if cell.Chr > 255 then cell.Chr := 0;
              off := CPages[cp].QuickGlyph[cell.Chr];
            end;
            GetGlyphBmp(bmp, CPages[cp].GlyphTable, off, cell.Attr, false);
            bmp.ResampleFilter:=rfMitchell;
            bmp2 := bmp.Resample(2, 4, rmFineResample) as TBGRABitmap;
            cnv.Draw(x, y, bmp2.Bitmap);
            bmp2.free;
          end;
      end;
  end;
  bmp.free;
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

