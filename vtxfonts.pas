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

unit VTXFonts;

{$mode objfpc}{$H+}

interface

uses
  Classes, Windows, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BGRABitmap,
  BGRABitmapTypes,
  ExtCtrls, StdCtrls, VTXConst, VTXSupport
  ;

type

  { TfFonts }

  TfFonts = class(TForm)
    cbCodePage: TComboBox;
    cbCodePage1: TComboBox;
    cbCodePage2: TComboBox;
    cbCodePage3: TComboBox;
    cbCodePage4: TComboBox;
    cbCodePage5: TComboBox;
    cbCodePage6: TComboBox;
    cbCodePage7: TComboBox;
    cbCodePage8: TComboBox;
    PaintBox1: TPaintBox;
    pbClose: TPaintBox;
    pbFont0: TPaintBox;
    pbFont1: TPaintBox;
    pbFont2: TPaintBox;
    pbFont3: TPaintBox;
    pbFont4: TPaintBox;
    pbFont5: TPaintBox;
    pbFont6: TPaintBox;
    pbFont7: TPaintBox;
    pbFont8: TPaintBox;
    pbFont9: TPaintBox;
    pbTitleBar: TPaintBox;
    StaticText1: TStaticText;
    StaticText10: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    StaticText7: TStaticText;
    StaticText8: TStaticText;
    StaticText9: TStaticText;
    procedure PaintBox1Paint(Sender: TObject);
    procedure pbCloseClick(Sender: TObject);
    procedure pbClosePaint(Sender: TObject);
    procedure pbFont0Click(Sender: TObject);
    procedure pbFont1Click(Sender: TObject);
    procedure pbFont2Click(Sender: TObject);
    procedure pbFont3Click(Sender: TObject);
    procedure pbFont4Click(Sender: TObject);
    procedure pbFont5Click(Sender: TObject);
    procedure pbFont6Click(Sender: TObject);
    procedure pbFont7Click(Sender: TObject);
    procedure pbFont8Click(Sender: TObject);
    procedure pbFont9Click(Sender: TObject);
    procedure pbTitleBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbTitleBarPaint(Sender: TObject);
    procedure bPBBMouseLeave(Sender: TObject);
    procedure bPBBPaint(Sender: TObject);
    procedure bPBBMouseEnter(Sender: TObject);
    procedure bPBBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bPBBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fFonts: TfFonts;

implementation

{$R *.lfm}

{ TfFonts }

procedure TfFonts.pbCloseClick(Sender: TObject);
begin
  Hide;
end;

procedure TfFonts.PaintBox1Paint(Sender: TObject);
var
  pb : TPaintBox;
  cnv : TCanvas;
  r : TRect;
begin
  pb := TPaintBox(Sender);
  cnv := pb.Canvas;
  r := pb.ClientRect;
  cnv.Brush.COlor := ANSIColor[UIBackground];
  cnv.FillRect(r);
  DrawRectangle3D(cnv, r, true);
  r.inflate(-1,-1);
  DrawRectangle3D(cnv, r, true);
end;

procedure TfFonts.pbClosePaint(Sender: TObject);
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

procedure TfFonts.pbFont0Click(Sender: TObject);
begin
  SetDown(pbFont0, true);
  SetDown(pbFont1, false);
  SetDown(pbFont2, false);
  SetDown(pbFont3, false);
  SetDown(pbFont4, false);
  SetDown(pbFont5, false);
  SetDown(pbFont6, false);
  SetDown(pbFont7, false);
  SetDown(pbFont8, false);
  SetDown(pbFont9, false);
end;

procedure TfFonts.pbFont1Click(Sender: TObject);
begin
  SetDown(pbFont0, false);
  SetDown(pbFont1, true);
  SetDown(pbFont2, false);
  SetDown(pbFont3, false);
  SetDown(pbFont4, false);
  SetDown(pbFont5, false);
  SetDown(pbFont6, false);
  SetDown(pbFont7, false);
  SetDown(pbFont8, false);
  SetDown(pbFont9, false);
end;

procedure TfFonts.pbFont2Click(Sender: TObject);
begin
  SetDown(pbFont0, false);
  SetDown(pbFont1, false);
  SetDown(pbFont2, true);
  SetDown(pbFont3, false);
  SetDown(pbFont4, false);
  SetDown(pbFont5, false);
  SetDown(pbFont6, false);
  SetDown(pbFont7, false);
  SetDown(pbFont8, false);
  SetDown(pbFont9, false);
end;

procedure TfFonts.pbFont3Click(Sender: TObject);
begin
  SetDown(pbFont0, false);
  SetDown(pbFont1, false);
  SetDown(pbFont2, false);
  SetDown(pbFont3, true);
  SetDown(pbFont4, false);
  SetDown(pbFont5, false);
  SetDown(pbFont6, false);
  SetDown(pbFont7, false);
  SetDown(pbFont8, false);
  SetDown(pbFont9, false);
end;

procedure TfFonts.pbFont4Click(Sender: TObject);
begin
  SetDown(pbFont0, false);
  SetDown(pbFont1, false);
  SetDown(pbFont2, false);
  SetDown(pbFont3, false);
  SetDown(pbFont4, true);
  SetDown(pbFont5, false);
  SetDown(pbFont6, false);
  SetDown(pbFont7, false);
  SetDown(pbFont8, false);
  SetDown(pbFont9, false);
end;

procedure TfFonts.pbFont5Click(Sender: TObject);
begin
  SetDown(pbFont0, false);
  SetDown(pbFont1, false);
  SetDown(pbFont2, false);
  SetDown(pbFont3, false);
  SetDown(pbFont4, false);
  SetDown(pbFont5, true);
  SetDown(pbFont6, false);
  SetDown(pbFont7, false);
  SetDown(pbFont8, false);
  SetDown(pbFont9, false);
end;

procedure TfFonts.pbFont6Click(Sender: TObject);
begin
  SetDown(pbFont0, false);
  SetDown(pbFont1, false);
  SetDown(pbFont2, false);
  SetDown(pbFont3, false);
  SetDown(pbFont4, false);
  SetDown(pbFont5, false);
  SetDown(pbFont6, true);
  SetDown(pbFont7, false);
  SetDown(pbFont8, false);
  SetDown(pbFont9, false);
end;

procedure TfFonts.pbFont7Click(Sender: TObject);
begin
  SetDown(pbFont0, false);
  SetDown(pbFont1, false);
  SetDown(pbFont2, false);
  SetDown(pbFont3, false);
  SetDown(pbFont4, false);
  SetDown(pbFont5, false);
  SetDown(pbFont6, false);
  SetDown(pbFont7, true);
  SetDown(pbFont8, false);
  SetDown(pbFont9, false);
end;

procedure TfFonts.pbFont8Click(Sender: TObject);
begin
  SetDown(pbFont0, false);
  SetDown(pbFont1, false);
  SetDown(pbFont2, false);
  SetDown(pbFont3, false);
  SetDown(pbFont4, false);
  SetDown(pbFont5, false);
  SetDown(pbFont6, false);
  SetDown(pbFont7, false);
  SetDown(pbFont8, true);
  SetDown(pbFont9, false);
end;

procedure TfFonts.pbFont9Click(Sender: TObject);
begin
  SetDown(pbFont0, false);
  SetDown(pbFont1, false);
  SetDown(pbFont2, false);
  SetDown(pbFont3, false);
  SetDown(pbFont4, false);
  SetDown(pbFont5, false);
  SetDown(pbFont6, false);
  SetDown(pbFont7, false);
  SetDown(pbFont8, false);
  SetDown(pbFont9, true);
end;

procedure TfFonts.pbTitleBarPaint(Sender: TObject);
var
  pb : TPaintBox;
  cnv : TCanvas;
  r : TRect;
const
  titletxt = 'Fonts';
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
  cnv.TextOut(3,1,titletxt);
end;

// toolbar move routines
procedure TfFonts.pbTitleBarMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  SendMessage(Handle, WM_NCLBUTTONDOWN, HTCAPTION, 0);
end;


// paintboxbutton

procedure TfFonts.bPBBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  v : Uint32;
  pb : TPaintBox;
  down : boolean;
begin
  pb := TPaintBox(Sender);
  v := pb.tag;
  down := HasBits(v, PBB_DOWN);
  if not HasBits(v, PBB_TYPE_BUTTON) then
    SetBit(v, PBB_DOWN, not down)
  else
    SetBit(v, PBB_DOWN, true);
  pb.tag := v;
  pb.Invalidate;
end;

procedure TfFonts.bPBBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  v : Uint32;
  pb : TPaintBox;
begin
  pb := TPaintBox(Sender);
  v := pb.tag;
  if HasBits(v, PBB_TYPE_BUTTON) then
    SetBit(v, PBB_DOWN, false);
  pb.tag := v;
  pb.Invalidate;
end;

procedure TfFonts.bPBBMouseEnter(Sender: TObject);
var
  v : Uint32;
  pb : TPaintBox;
begin
  pb := TPaintBox(Sender);
  v := pb.tag;
  SetBit(v, PBB_HOVER, true);
  pb.tag := v;
  pb.Invalidate;
end;

procedure TfFonts.bPBBMouseLeave(Sender: TObject);
var
  v : Uint32;
  pb : TPaintBox;
begin
  pb := TPaintBox(Sender);
  v := pb.tag;
  SetBit(v, PBB_HOVER, false);
  pb.tag := v;
  pb.Invalidate;
end;


procedure TfFonts.bPBBPaint(Sender: TObject);
var
  pb : TPaintBox;
  cnv : TCanvas;
  r : TRect;
  bmp : TBGRABitmap;
  n : integer;
  size : integer;
  adj : integer;
  down : boolean;
  drawicon : boolean;
begin
  // use tag for states
  pb := TPaintBox(Sender);
  cnv := pb.Canvas;
  down := HasBits(pb.Tag, PBB_DOWN);

  // get icon
  drawicon := true;
  n := GetBits(pb.Tag, PBB_IMAGE_MASK);
  size := iconsNormal.Height;
  r.Top := 0;
  r.Left := n * size;
  r.Width := size;
  r.Height := size;
  if not pb.Enabled then
    bmp := iconsGrayed.GetPart(r) as TBGRABitmap
  else if HasBits(pb.tag, PBB_DOWN) then
    bmp := iconsNormal.GetPart(r) as TBGRABitmap
  else
    drawicon := false;

  // draw button
  if down then
    textureDown.Draw(cnv, 0,0)
  else
    textureUp.Draw(cnv, 0, 0);

  if drawicon then
    begin
    adj := 0;
    if down then adj := 1;
    cnv.Draw(
      ((pb.Width - bmp.Width) >> 1),
      ((pb.Height - bmp.Height) >> 1) - 2 + adj,
      bmp.Bitmap);
    bmp.free;
  end;
end;

end.

