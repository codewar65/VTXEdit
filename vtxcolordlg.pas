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

unit VTXColorDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LResources,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  Buttons,
  Math,
  VTXSupport,
  VTXConst,
  StdCtrls;

type

  { TfColorDialog }

  TfColorDialog = class(TForm)
    bOK: TButton;
    bCancel: TButton;
    tbANSIColor: TEdit;
    tbGreen: TEdit;
    tbHex: TEdit;
    tbRed: TEdit;
    tbHue: TEdit;
    tbL: TEdit;
    tbB: TEdit;
    tbBlue: TEdit;
    tbLum: TEdit;
    tbA: TEdit;
    tbSat: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    pbColors: TPaintBox;
    pbHS: TPaintBox;
    pbL: TPaintBox;
    pbDesiredColor: TPaintBox;
    pbActualColor: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbActualColorPaint(Sender: TObject);
    procedure pbColorsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbColorsPaint(Sender: TObject);
    procedure pbDesiredColorPaint(Sender: TObject);
    procedure pbHSPaint(Sender: TObject);
    procedure pbLPaint(Sender: TObject);
    procedure SetANSIColor(c : integer);
  private
    { private declarations }
  public
    { public declarations }
    fColor : integer;       // ansi color 0-255
  end;

  TLAB = record
    l, a, b : double;
  end;

  THSL = record
    h, s, l : double;
  end;

  TRGB = record
    r, g, b : double;
  end;

  TXYZ = record
    x, y, z : double;
  end;


var
  fColorDialog: TfColorDialog;
  bmpHS : TBitmap;

const
  d65: TXYZ = (
    x: 0.9505;
    y: 1.0;
    z: 1.0890;
  );

  function RGB2XYZ(rgb: TRGB): TXYZ;
  function RGB2HSL(rgb: TRGB): THSL;
  function XYZ2LAB(xyz: TXYZ): TLAB;
  function HSL2RGB(hsl: THSL): TRGB;
  function XYZ2RGB(xyz: TXYZ): TRGB;
  function LAB2XYZ(lab: TLAB): TXYZ;

implementation

{ TfColorDialog }

// paint color picker + highlight selected color
procedure TfColorDialog.pbColorsPaint(Sender: TObject);
var
  pb : TPaintBox;
  cnv : TCanvas;
  x, y, c : integer;
  cw, ch : integer;
  r : TRect;
begin
  pb := TPaintBox(Sender);
  cnv := pb.Canvas;
  cw := pb.Width >> 4;
  ch := pb.Height >> 4;

  // draw selection rect first
  x := fColor and $F;
  y := fColor >> 4;
  r.Top := y * ch;
  r.Left := x * cw;
  r.Width := cw + 2;
  r.Height := ch + 2;
  cnv.Brush.Color := clWhite;
  cnv.Pen.Color := clWhite;
  cnv.DrawFocusRect(r);

  c := 0;
  for y := 0 to 15 do
  begin
    for x := 0 to 15 do
    begin
      r.Top := y * ch + 2;
      r.Left := x * cw + 2;
      r.Width := cw - 2;
      r.Height := ch - 2;

      cnv.Brush.Color := ANSIColor[c];
      cnv.Pen.Color := clBlack;
      cnv.Rectangle(r);
      c += 1;
    end;
  end;
end;

procedure TfColorDialog.pbDesiredColorPaint(Sender: TObject);
var
  pb : TPaintBox;
  cnv : TCanvas;
  r, g, b : integer;
begin
  pb := TPaintBox(Sender);
  cnv := pb.Canvas;
  r := strtoint(tbRed.Text);
  g := strtoint(tbGreen.Text);
  b := strtoint(tbBlue.Text);
  cnv.Brush.Color := RGBToColor(r, g, b);
  cnv.Pen.Color := clBlack;
  cnv.Rectangle(pb.ClientRect);
end;

procedure TfColorDialog.pbHSPaint(Sender: TObject);
var
  pb : TPaintBox;
  cnv : TCanvas;
  hsl : THSL;
  w, h : integer;
  x, y : integer;
  rect : TRect;
begin
  pb := TPaintBox(Sender);
  cnv := pb.Canvas;
  w := pb.ClientRect.Width;
  h := pb.ClientRect.Height;

  cnv.Draw(0, 0, bmpHS);
  cnv.Pen.Color := clBlack;
  cnv.Brush.Style := bsClear;
  cnv.Rectangle(pb.ClientRect);

  x := floor((StrToFloat(tbHue.Text) / 100.0) * w);
  y := floor((StrToFloat(tbSat.Text) / 100.0) * h);
  rect.Top := y - 1;
  rect.Left := x - 1;
  rect.Width := 3;
  rect.Height := 3;
  cnv.Brush.Style := bsClear;
  cnv.Pen.Color := clWhite;
  cnv.Rectangle(rect);
  rect.inflate(1, 1);
  cnv.Pen.Color := clBlack;
  cnv.Rectangle(rect);
end;

procedure TfColorDialog.pbLPaint(Sender: TObject);
var
  w, h : integer;
  pb : TPaintBox;
  cnv : TCanvas;
  y : integer;
  hsl : THSL;
  rgb : TRGB;
  r, g, b : integer;
  rect : TRect;
begin
  // need to draw this custom every time
  pb := TPaintBox(Sender);
  cnv := pb.Canvas;
  w := pb.ClientRect.Width;
  h := pb.ClientRect.Height;

  hsl.h := (StrToFloat(tbHue.Text) / 100);
  hsl.s := (StrToFloat(tbSat.Text) / 100);
  for y := 0 to h - 1 do
  begin
    hsl.l := y / h;
    rgb := HSL2RGB(hsl);
    r := floor(rgb.r * 255.0);
    g := floor(rgb.g * 255.0);
    b := floor(rgb.b * 255.0);
    cnv.Pen.Color := RGBToColor(r, g, b);
    cnv.Line(0, y, w - 1, y);
  end;
  cnv.Pen.Color := clBlack;
  cnv.Brush.Style := bsClear;
  cnv.Rectangle(pb.ClientRect);

  y := floor((StrToFloat(tbLum.Text) / 100.0) * h);
  rect.Top := y - 1;
  rect.Left := 0;
  rect.Height := 3;
  rect.Width := w;

  cnv.Brush.Style := bsClear;
  cnv.Pen.Color := clWhite;
  cnv.Pen.Mode:= pmXor;
  cnv.Rectangle(rect);
end;

procedure TfColorDialog.pbActualColorPaint(Sender: TObject);
var
  pb : TPaintBox;
  cnv : TCanvas;
  r, g, b : integer;
begin
  pb := TPaintBox(Sender);
  cnv := pb.Canvas;
  r := (ANSIColor[fColor]      ) and $FF;
  g := (ANSIColor[fColor] >>  8) and $FF;
  b := (ANSIColor[fColor] >> 16) and $FF;
  cnv.Brush.Color := RGBToColor(r, g, b);
  cnv.Pen.Color := clBlack;
  cnv.Rectangle(pb.ClientRect);
end;


{ CONVERT RGB TO XYZ }
function RGB2XYZ(rgb: TRGB): TXYZ;
var
  r, g, b:  double;
  xyz:      TXYZ;

      {------------------------------------------------------------------------------
      CONVERSION FOR RGB TO XYZ
      }
  function _F2S(v: real): real; inline;
  var
    ret : double;
  begin
    if v > 0.04045 then
      ret := power((v + 0.055) / 1.055, 2.2)
    else
      ret := v / 12.92;
    _F2S := ret;
  end;

begin
  r := _F2S(rgb.r);
  g := _F2S(rgb.g);
  b := _F2S(rgb.b);
  xyz.x := (r * 0.4124 + g * 0.3576 + b * 0.1805);
  xyz.y := (r * 0.2126 + g * 0.7152 + b * 0.0722);
  xyz.z := (r * 0.0193 + g * 0.1192 + b * 0.9505);
  RGB2XYZ := xyz;
end;

{ CONVERT RGB TO HSL }
function RGB2HSL(rgb: TRGB): THSL;
var
  mx, mn, delta:  double;
begin
  mx := Max(rgb.r, Max(rgb.g, rgb.b));
  mn := Min(rgb.r, Min(rgb.g, rgb.b));
  result.l := (mx + mn) / 2.0;

  if mx = mn then
  begin
    result.h := 0;
    result.s := 0;
  end
  else
  begin
    delta := mx - mn;
    if result.l > 0.5 then
      result.s := delta / (2 - mx - mn)
    else
      result.s := delta / (mx + mn);
    if rgb.r = mx then
    begin
      result.h := (rgb.g - rgb.b) / delta;
      if rgb.g < rgb.b then
        result.h := result.h + 6.0;
    end
    else
    if rgb.g = mx then
      result.h := (rgb.b - rgb.r) / delta + 2
    else
      result.h := (rgb.r - rgb.g) / delta + 4;
  end;
  if result.s = 0.0 then
    result.h := 0.0
  else
  begin
    result.h := result.h / 6.0;
    result.h := result.h - floor(result.h);
  end;
end;

{ CONVERT XYZ TO LAB }
function XYZ2LAB(xyz: TXYZ): TLAB;

  {------------------------------------------------------------------------------
    CONVERSION FOR XYZ TO LAB
  }
  function _FXYZ(t: real): real; inline;
  var
    ret: real;
  begin
    if t > 0.008856 then
      ret := power(t, 1.0 / 3.0)
    else
      ret := 7.787 * t + (16.0 / 116.0);
    _FXYZ := ret;
  end;

begin
  result.l := 116.0 * _FXYZ(xyz.y / d65.y) - 16.0;
  result.a := 500.0 * (_FXYZ(xyz.x / d65.x) - _FXYZ(xyz.y / d65.y));
  result.b := 200.0 * (_FXYZ(xyz.y / d65.y) - _FXYZ(xyz.z / d65.z));
end;

{ CONVERT HSL TO RGB }
function HSL2RGB(hsl: THSL): TRGB;
var
  q, p: real;

  {------------------------------------------------------------------------------
    CONVERSION FOR HSL TO RGB
  }
  function _FHSL(p, q, t: real): real; inline;
  begin
    result := p;
    if t < 0.0 then
      t := t + 1.0;

    if t > 1.0 then
      t := t - 1.0;

    if t * 6 < 1 then
      result := p + (q - p) * 6.0 * t
    else if t * 2 < 1 then
      result := q
    else if t * 3 < 2 then
      result := p + (q - p) * (2 / 3 - t) * 6;
    end;

begin
  if hsl.s = 0 then
  begin
    result.r := hsl.l;
    result.g := hsl.l;
    result.b := hsl.l;
  end
  else
  begin
    if hsl.l < 0.5 then
      q := hsl.l * (1 + hsl.s)
    else
      q := (hsl.l + hsl.s) - (hsl.l * hsl.s);
    p := 2.0 * hsl.l - q;
    result.r := _FHSL(p, q, hsl.h + 1 / 3);
    result.g := _FHSL(p, q, hsl.h);
    result.b := _FHSL(p, q, hsl.h - 1 / 3);
  end;
end;

{ CONVERT XYZ to RGB }
function XYZ2RGB(xyz: TXYZ): TRGB;
var
  r, g, b: double;

  {------------------------------------------------------------------------------
  CONVERSION FOR XYZ TO RGB
  }
  function _FFROMS(v: real): real; inline;
  begin
    if v <= 0.0031308 then
      result := 12.92 * v
    else
      result := (1.055 * power(v, 0.416667)) - 0.055;
  end;

begin
  r := xyz.x * 3.2410 - xyz.y * 1.5374 - xyz.z * 0.4986;
  g := -xyz.x * 0.9692 + xyz.y * 1.8760 + xyz.z * 0.0416;
  b := xyz.x * 0.0556 - xyz.y * 0.2040 + xyz.z * 1.0570;
  result.r := _FFROMS(r);
  result.g := _FFROMS(g);
  result.b := _FFROMS(b);
end;

{ CONVERT LAB TO XYZ }
function LAB2XYZ(lab: TLAB): TXYZ;
var
  fx, fy, fz: double;

  {------------------------------------------------------------------------------
    CONVERSION FOR LAB TO XYZ
  }
  function _FLABADJ(v, w: real): real; inline;
  const
    delta: double = 6.0 / 29.0;
  var
    ret: double;
  begin
    if v > delta then
      result := w * (v * v * v)
    else
      result := (v - 16.0 / 116.0) * 3 * (delta * delta) * w;
  end;

begin
  fy := (lab.l + 16.0) / 116.0;
  fx := fy + (lab.a / 500.0);
  fz := fy - (lab.b / 200.0);
  result.x := _FLABADJ(fx, d65.x);
  result.y := _FLABADJ(fy, d65.y);
  result.z := _FLABADJ(fz, d65.z);
end;

function SetRGB(r, g, b : byte) : TRGB;
begin
  result.r := r / 255.0;
  result.g := g / 255.0;
  result.b := b / 255.0;
end;

function doubletostr(v : double) : string;
begin
  result := Format('%.1f', [ v ]);
end;

procedure TfColorDialog.SetANSIColor(c : integer);
var
  r, g, b : integer;
  rgb : TRGB;
  hsl : THSL;
  xyz : TXYZ;
  lab : TLAB;
begin
  // load settings based on fANSIColor.
  r := (ANSIColor[fColor]      ) and $FF;
  g := (ANSIColor[fColor] >>  8) and $FF;
  b := (ANSIColor[fColor] >> 16) and $FF;

  rgb := SetRGB(r, g, b);
  hsl := RGB2HSL(rgb);
  xyz := RGB2XYZ(rgb);
  lab := XYZ2LAB(xyz);

  tbANSIColor.Text := inttostr(c);
  tbHex.Text := Format('$%2.2X%2.2X%2.2X', [r, g, b]);
  tbRed.Text := inttostr(r);
  tbGreen.Text := inttostr(g);
  tbBlue.Text := inttostr(b);
  tbHue.Text := doubletostr(hsl.h * 100);
  tbSat.Text := doubletostr(hsl.s * 100);
  tbLum.Text := doubletostr(hsl.l * 100);
  tbL.Text := doubletostr(lab.l);
  tbA.Text := doubletostr(lab.a);
  tbB.Text := doubletostr(lab.b);
  pbDesiredColor.Invalidate;
  pbActualColor.Invalidate;
  pbHS.Invalidate;
  pbL.Invalidate;
end;

procedure TfColorDialog.FormCreate(Sender: TObject);
var
  w, h : integer;
  x, y : integer;
  r, g, b : integer;
  hsl : THSL;
  rgb : TRGB;
begin
  w := pbHS.Width;
  h := pbHS.Height;

  SetANSIColor(fColor);
  bmpHS := TBitmap.Create;
  bmpHS.Width := w;
  bmpHS.Height := h;
  bmpHS.PixelFormat := pf24bit;
  for y := 0 to h - 1 do
    for x := 0 to w - 1 do
    begin
      hsl.h := x / w;
      hsl.s := y / h;
      hsl.l := 0.5;
      rgb := HSL2RGB(hsl);
      r := floor(rgb.r * 255);
      g := floor(rgb.g * 255);
      b := floor(rgb.b * 255);
      bmpHS.Canvas.Pixels[x, y] := RGBToColor(r, g, b);
    end;
end;

procedure TfColorDialog.FormDestroy(Sender: TObject);
begin
  bmpHS.Free;
end;

procedure TfColorDialog.pbColorsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pb : TPaintBox;
  x1, y1 : integer;
  cw, ch : integer;
begin
  pb := TPaintBox(Sender);
  cw := pb.Width >> 4;
  ch := pb.Height >> 4;
  x1 := x div cw;
  y1 := y div ch;
  if between(x1, 0, 15) and between(y1, 0, 15) then
  begin
    fColor := x1 + (y1 << 4);
    SetANSIColor(fColor);
    pbColors.Invalidate;
  end;
end;

initialization
{$R *.lfm}

end.














