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

unit VTXSupport;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$ASMMODE intel}

interface

uses
  UnicodeHelper,
  Classes,
  Forms,
  SysUtils,
  ExtCtrls,
  VTXConst,
  BGRABitmap,
  BGRABitmapTypes,
  RecList,
  Math,
  {$ifdef WINDOWS}
  Windows,
  {$else}
  LCLType,
  {$endif}
  Graphics;

procedure DrawDashLine(cnv : TCanvas; x1, y1, x2, y2 : integer; clr1, clr2 : TColor);
procedure DrawDashRect(cnv : TCanvas; rect : TRect; clr1, clr2 : TColor);
procedure DrawDashRect(cnv : TCanvas; x1, y1, x2, y2 : integer; clr1, clr2 : TColor);
function GetGlyphOff(codepoint : integer; table : PByte; size : integer) : integer;
procedure GetGlyphBmp(var bmp : TBGRABitmap; base : pbyte; off : integer; attr : Uint32; blink : boolean);
function Between(val, lo, hi : integer) : boolean; inline;
function Between(val, lo, hi : char) : boolean; inline;
function HasBits(val, mask : UInt32) : boolean; inline;
function GetBits(val, mask : UInt32; shift : integer = 0) : UInt32; inline;
procedure SetBits(var val : UInt32; mask, bits : UInt32; shift : integer = 0); inline;
procedure SetBit(var val : byte; mask : byte; bit : boolean); inline;
procedure SetBit(var val : UInt32; mask : UInt32; bit : boolean); inline;
procedure SetBit(var val : longint; mask : longint; bit : boolean); inline;
procedure Swap(var val1, val2 : integer); inline;
procedure Swap(var val1, val2 : UInt32); inline;
function Brighten(color : TColor; factor: real): TColor;
function DrawTextCentered(cnv: TCanvas; const r: TRect; s: unicodeString): Integer;
function DrawTextRight(cnv: TCanvas; const r: TRect; s: unicodeString): Integer;
procedure DrawRectangle(cnv: TCanvas; x1, y1, x2, y2 : integer; clr : TColor);
procedure DrawRectangle(cnv: TCanvas; rect : TRect; clr : TColor);
procedure LineCalcInit(x0, y0, x1, y1 : integer);
function LineCalcNext(var xo, yo : integer) : boolean;
procedure EllipseCalcInit(xrad, yrad : longint);
function EllipseCalcNext(var xo, yo : longint) : boolean;
function QuadToStr(q : TQuad) : unicodestring;
function StrToQuad(str : unicodestring) : TQuad;
procedure SetFormQuad(f : TForm; q : TQuad);
function GetFormQuad(f : TForm) : TQuad;
function CharsToStr(src : array of char; len : integer) : unicodestring;
function CharsToStr(src : array of byte; len : integer) : unicodestring;
function isInteger(str : unicodestring) : boolean;
function iif(cond : boolean; trueval, falseval : integer) : integer; inline;
function iif(cond : boolean; trueval, falseval : byte) : byte; inline;
function iif(cond : boolean; trueval, falseval : char) : char; inline;
function iif(cond : boolean; trueval, falseval : string) : string; inline;
function iif(cond : boolean; trueval, falseval : unicodestring) : unicodestring; inline;
function iif(cond : boolean; trueval, falseval : uint32) : uint32; inline;
function RectWidth(r : TRect) : integer; inline;
function RectHeight(r : TRect) : integer; inline;
procedure DrawStretchedBitmap(cnv : TCanvas; r : TRect; bmp : TBGRABitmap);
function GetObjectCell(row, col : integer; var cell : TCell) : integer;
function InRect(x, y, rx, ry, rw, rh : integer) : boolean; inline;
operator =(cell1, cell2 : TCell) : boolean;
procedure Draw3DRect(cnv : TCanvas; rect : TRect; sunk : boolean);
procedure Draw3DRect(cnv : TCanvas; x1, y1, x2, y2 : integer; sunk : boolean);

var
  // various settings
  PageType :                integer;  // from cbPageType  PAGETYPE_
  ColorScheme :             integer;  // from cbColorScheme COLORSCHEME_

  bmpPage     :             TBGRABitmap;  // the page.
  bmpPreview  :             TBGRABitmap;
  PageZoom :                double;     // 1.0 = 100%
  XScale :                  double;     // horizontal stretch. 1.0 = 100%
  CellWidth, CellHeight :   integer;    // pixels
  CellWidthZ, CellHeightZ : integer;    // adjusted by PageZoom
  NumCols, NumRows :        integer;    // doc size

  Page :                    TPage;      // main doc

  // objects on doc
  Objects :                 TObjList;

  // as cells are painted, updates get added to this. keep the original cell,
  // and update the new cell with the last cell painted.
  CurrUndoData :            TRecList;

  // the undo/redo list
  UndoPos :   integer;    // where are we on the undo list
  Undo :      TRecList;   // the list

  // fonts. (CSI 10-19 / 80-85 <space> D
  Fonts :                   array [0..15] of TEncoding;

  KeyBinds : array of TKeyBinds;

implementation

operator =(cell1, cell2 : TCell) : boolean;
begin
  result := (cell1.Chr = cell2.Chr) and (cell1.Attr = cell2.Attr);
end;

{*****************************************************************************}

{ Support Functions }

function InRect(x, y, rx, ry, rw, rh : integer) : boolean; inline;
begin
  result := (x >= rx) and (x < rx + rw) and (y >= ry) and (y < ry + rh);
end;

function GetObjectCell(row, col : integer; var cell : TCell) : integer;
var
  i :             integer;
  objr, objc, p : integer;
  cellrec : TCell;
begin
  for i := length(Objects) - 1 downto 0 do
  begin
    if InRect(
      col, row,
      Objects[i].Col, Objects[i].Row,
      Objects[i].Width, Objects[i].Height) then
    begin
      objr := row - Objects[i].Row;
      objc := col - Objects[i].Col;
      p := objr * Objects[i].Width + objc;

      Objects[i].Data.Get(@cellrec, p);
      if cellrec.Chr <> _EMPTY then
      begin
        cell := cellrec;
        exit(i);
      end;
    end;
  end;
  cell.Chr := _EMPTY;
  cell.Attr := $0007;
  result := -1;
end;

function VTXRGB(r, g, b : byte) : dword; inline;
begin
  result := ((b << 16) or (g << 8) or r);
end;

function iif(cond : boolean; trueval, falseval : uint32) : uint32; inline;
begin
  if cond then result := trueval else result := falseval;
end;

function iif(cond : boolean; trueval, falseval : unicodestring) : unicodestring; inline;
begin
  if cond then result := trueval else result := falseval;
end;

function iif(cond : boolean; trueval, falseval : string) : string; inline;
begin
  if cond then result := trueval else result := falseval;
end;

function iif(cond : boolean; trueval, falseval : char) : char; inline;
begin
  if cond then result := trueval else result := falseval;
end;

function iif(cond : boolean; trueval, falseval : integer) : integer; inline;
begin
  if cond then result := trueval else result := falseval;
end;

function iif(cond : boolean; trueval, falseval : byte) : byte; inline;
begin
  if cond then result := trueval else result := falseval;
end;

// get offset of codepoint of glyph in UVGA16. return 0 if not found
// called like GetGlyphOff(9673, @UVGA16, sizeof(UVGA16));
function GetGlyphOff(codepoint : integer; table : PByte; size : integer) : integer;
var
  rec, min, max : integer;
  key, off : integer;
  recs : integer;
begin
  recs := size div 18;

  // do binary search for codepoint in glyphtable
  min := 0;
  max := recs;
  repeat
    if max < min then
    begin
      // not found! return 0 (the undef char)
      off := 0;
      break;
    end;

    rec := (max + min) >> 1;
    off := rec * 18;
    key := (table[off] << 8) or table[off + 1];

    if key = codepoint then
      // got a match. exit with off
      break;

    if key < codepoint then
      min := rec + 1
    else if key > codepoint then
      max := rec - 1;

  until key = codepoint;
  result := off + 2;
end;

// return new rendered glyph - does not render blink or double height
procedure GetGlyphBmp(
  var bmp : TBGRABitmap;
  base : pbyte;         // base address of glyph table
  off : integer;        // offset into glyph table points to 8x16
  attr : Uint32;        // standard cell attributes
  blink : boolean       // if on, conceal text.
  );
var
  x, y :        Integer;
  b :           Word;
  ptr :         PBYTE;
  bptr :        PBGRAPixel;
  sptr :        PBGRAPixel;
  fg, bg, sc :  TBGRAPixel;
  italics,
  bold,
  shadow,
  underline,
  strike,
  dstrike :     Boolean;
  disp :        Integer;
  adj :         Integer;
  i, dl :       Integer;
  s :           PBGRAPixel;
  fi, bi :      Integer;
begin
  ptr := @base[off];

  italics :=    HasBits(attr, A_CELL_ITALICS);
  bold :=       HasBits(attr, A_CELL_BOLD);
  shadow :=     HasBits(attr, A_CELL_SHADOW);
  underline :=  HasBits(attr, A_CELL_UNDERLINE);
  strike :=     HasBits(attr, A_CELL_STRIKETHROUGH);
  dstrike :=    HasBits(attr, A_CELL_DOUBLESTRIKE);
  disp :=       GetBits(attr, A_CELL_DISPLAY_MASK);

  // dont' swap bold bit if BBS or CTerm and colors between 8-15
  fi := GetBits(attr, A_CELL_FG_MASK);
  bi := GetBits(attr, A_CELL_BG_MASK, 8);
  if HasBits(attr, A_CELL_REVERSE) then
  begin
    if ColorScheme = COLORSCHEME_BBS then
    begin
      i := fi and $08;
      fi := fi and $07;
      bi := bi or i;
    end;
    fg := ANSIColor[bi];
    bg := ANSIColor[fi];
  end
  else
  begin
    fg := ANSIColor[fi];
    bg := ANSIColor[bi];
  end;

  // get faint foreground color
  if HasBits(attr, A_CELL_FAINT) then
    fg := Brighten(fg, -0.33);

  // compute shadow color
  if shadow then
    sc := Brighten(bg, -0.33);

  // draw background.
  bmp.FillRect(0, 0, 8, 16, bg);

  // draw the cell
  if not blink and (disp <> A_CELL_DISPLAY_CONCEAL) then
  begin

    for y := 0 to 15 do
    begin
      bptr := bmp.ScanLine[y];        // get ptr into bmp
      if (y < 15) then
      begin
        sptr := bmp.ScanLine[y + 1];  // get ptr for shadow
        sptr += 1;
      end;

      b := ptr^;                // get byte of character def
      inc(ptr);

      // alter for underline, strikethrough, and doublestrike
      if underline and (y = 15) then              b := $ff;
      if strike    and (y = 7) then               b := $ff;
      if dstrike   and ((y = 3) or (y = 11)) then b := $ff;

      // build bits
      for x := 0 to 7 do
      begin
        // if bit on at this x,y for this character
        if (b and $80) <> 0 then
        begin
          // shift top portion of bitmap 1 px right for italics
          adj := 0;
          if italics and (y < 8) then
            inc(adj);

          // draw if on the bitmap
          if x + adj < 8 then
          begin
            // draw shadow color bit first
            if shadow and (x + adj < 7) and (y < 15) then
              sptr[adj] := sc;

//            if shadow and (y > 0) and (x + adj < 7) then
//              bptr[adj - 7] := sc;

            // draw character bit
            bptr[adj] := fg;

            // repeat for bold
            if bold and (x + adj < 7) then
              bptr[adj + 1] := fg;

          end;
        end;
        bptr += 1;
        sptr += 1;
        b := b << 1;
      end;
    end;

    // adjust for double height
    if disp = A_CELL_DISPLAY_TOP then
    begin
      // stretch top half down over entire cell
      for i := 7 downto 0 do
      begin
        s := bmp.ScanLine[i];
        dl := i << 1;
        Move(s[0], bmp.ScanLine[dl    ][0], 32);
        Move(s[0], bmp.ScanLine[dl + 1][0], 32);
      end;
    end

    else if disp = A_CELL_DISPLAY_BOTTOM then
    begin
      // stretch bottom half up over entire cell
      for i := 8 to 15 do
      begin
        s := bmp.ScanLine[i];
        dl := (i - 8) << 1;
        Move(s[0], bmp.ScanLine[dl    ][0], 32);
        Move(s[0], bmp.ScanLine[dl + 1][0], 32);
      end;
    end;

//    bmp.InvalidateBitmap;
  end;
end;

// is val between lo and hi?
function Between(val, lo, hi : integer) : boolean; inline;
begin
  result := ((val >= lo) and (val <= hi));
end;

// is val between lo and hi?
function Between(val, lo, hi : char) : boolean; inline;
begin
  result := ((ord(val) >= ord(lo)) and (ord(val) <= ord(hi)));
end;

// any bits set?
function HasBits(val, mask : UInt32) : boolean; inline;
begin
  result := ((val and mask) <> 0);
end;

// return bits under bitmask
function GetBits(val, mask : UInt32; shift : integer = 0) : UInt32; inline;
begin
  result := ((val and mask) >> shift);
end;

// set bits for bitmask
procedure SetBits(var val : UInt32; mask, bits : UInt32; shift : integer = 0); inline;
begin
  val := ((val and not mask) or ((bits << shift) and mask));
end;

procedure SetBit(var val : byte; mask : byte; bit : boolean);
var
  bitval : byte;
begin
  bitval := mask;
  if not bit then
    bitval := 0;
  val := ((val and not mask) or bitval);
end;

procedure SetBit(var val : UInt32; mask : UInt32; bit : boolean);
var
  bitval : UInt32;
begin
  bitval := mask;
  if not bit then
    bitval := 0;
  val := ((val and not mask) or bitval);
end;

procedure SetBit(var val : longint; mask : longint; bit : boolean);
var
  bitval : longint;
begin
  bitval := mask;
  if not bit then
    bitval := 0;
  val := ((val and not mask) or bitval);
end;

procedure Swap(var val1, val2 : integer); inline;
var
  tmp : integer;
begin
  tmp := val1; val1 := val2; val2 := tmp;
end;

procedure Swap(var val1, val2 : UInt32); inline;
var
  tmp : UInt32;
begin
  tmp := val1; val1 := val2; val2 := tmp;
end;

// brighten / darken color
function Brighten(color : TColor; factor: real): TColor;

  function Norm(val : byte) : double; inline;
  begin
    result := val / 255.0;
  end;

  function Unnorm(val : double) : byte; inline;
  begin
    result := round(val * 255.0);
  end;

var
  r, g, b : double;
begin
  r := Norm(Red(color));
  g := Norm(Green(color));
  b := Norm(Blue(color));
  if factor < 0 then
  begin
    factor := factor + 1.0;
    r := r * factor;
    g := g * factor;
    b := b * factor;
  end
  else
  begin
    r := (1.0 - r) * factor + r;
    g := (1.0 - g) * factor + g;
    b := (1.0 - b) * factor + b;
  end;
  result := VTXRGB(Unnorm(r), Unnorm(g), Unnorm(b));
end;

function RectWidth(r : TRect) : integer; inline;
begin
  result := r.Right - r.Left;
end;

function RectHeight(r : TRect) : integer; inline;
begin
  result := r.Bottom - r.Top;
end;

function DrawTextCentered(cnv : TCanvas; const r : TRect; s : unicodeString) : integer;
var
  sz : TSize;
begin
  sz := cnv.TextExtent(s);
  cnv.TextOut(r.Left + ((RectWidth(r) - sz.cx) >> 1), r.Top + ((RectHeight(r) - sz.cy) >> 1), s);
end;

function DrawTextRight(cnv : TCanvas; const r : TRect; s : unicodeString) : integer;
var
  sz : TSize;
  rtop, rleft, rright, rwidth, rheight : integer;
begin
  sz := cnv.TextExtent(s);
  rtop  :=   r.top;
  rleft :=   r.left;
  rright :=  r.right;
  rwidth :=  RectWidth(r);
  rheight := RectHeight(r);
  if rheight < sz.cy then
    rheight := sz.cy;
  cnv.TextOut(rright - sz.cx, rtop + ((rheight - sz.cy) >> 1), s);
end;

procedure DrawRectangle(cnv: TCanvas; rect : TRect; clr : TColor);
begin
  DrawRectangle(cnv, rect.Left, rect.Top, rect.Right - 1, rect.Bottom - 1, clr);
end;

procedure DrawRectangle(cnv: TCanvas; x1, y1, x2, y2 : integer; clr : TColor);
begin
  cnv.Pen.Color := clr;
  cnv.Line(x2, y1, x1, y1);
  cnv.Line(x1, y1, x1, y2);
  cnv.Line(x1, y2, x2, y2);
  cnv.Line(x2, y2, x2, y1);
end;

// http://members.chello.at/~easyfilter/bresenham.html

// encapsulated line plotting globals
var
  LineData : record
    calcX0, calcY0,
    calcX1, calcY1 :  longint;
    calcDX, calcDY,
    calcSX, calcSY :  longint;
    calcErr :         longint;
  end;

// initialize line plotting calculator
procedure LineCalcInit(x0, y0, x1, y1 : longint);
begin
  LineData.calcX0 := x0;
  LineData.calcY0 := y0;
  LineData.calcX1 := x1;
  LineData.calcY1 := y1;
  with LineData do
  begin
    calcDX := abs(x1 - x0);
    calcDY := abs(y1 - y0);
    if x0 < x1 then
      calcSX := 1
    else
      calcSX := -1;
    if y0 < y1 then
      calcSY := 1
    else
      calcSY := -1;
    if calcDX > calcDY then
      calcErr := calcDX div 2
    else
      calcErr := (-calcDY) div 2;
  end;
end;

// get next point
function LineCalcNext(var xo, yo : longint) : boolean;
var
  e2 : longint;
begin
  with LineData do
  begin
    result := ((calcX0 = calcX1) and (calcY0 = calcY1));
    if not result then
    begin
      e2 := calcErr;
      if e2 > -calcDX then
      begin
        calcErr -= calcDY;
        calcX0 += calcSX;
      end;
      if e2 < calcDY then
      begin
        calcErr += calcDX;
        calcY0 += calcSY;
      end;
      result := ((calcX0 = calcX1) and (calcY0 = calcY1));
    end;
    xo := calcX0;
    yo := calcY0;
  end;
end;

// encapsulated ellipse plotting globals
var
  EllipseData : record
    State :                   integer;
    X, Y :                    longint;
    TwoASquare, TwoBSquare :  longint;
    XChange, YChange :        longint;
    EllipseError :            longint;
    StoppingX, StoppingY :    longint;
    XRadius, YRadius :        longint;
  end;

procedure EllipseCalcInit(xrad, yrad : longint);
begin
  EllipseData.XRadius := xrad;
  EllipseData.YRadius := yrad;
  EllipseData.State := 0;

end;

function EllipseCalcNext(var xo, yo : longint) : boolean;
begin
  result := false;
  with EllipseData do
  begin
    if (XRadius = 0) or (YRadius = 0) then
    begin
      xo := 0;
      yo := 0;
      result := true;
      exit;
    end;
    case State of
      0, 1:
        begin
          if State = 0 then
          begin
            // init for first part of ellipse
            TwoASquare := 2 * XRadius * XRadius;
            TwoBSquare := 2 * YRadius * YRadius;
            X := XRadius;
            Y := 0;
            XChange := YRadius * YRadius * (1 - 2 * XRadius);
            YChange := XRadius * XRadius;
            EllipseError := 0;
            StoppingX := TwoBSquare * XRadius;
            StoppingY := 0;
            State := 1;
          end;
          if StoppingX >= StoppingY then
          begin
            // the results.
            xo := X;
            yo := Y;
            y += 1;
            inc(StoppingY, TwoASquare);
            inc(EllipseError, YChange);
            inc(YChange, TwoASquare);
            if ((2 * EllipseError + XChange) > 0) then
            begin
              x -= 1;
              dec(StoppingX, TwoBSquare);
              inc(EllipseError, XChange);
              inc(XChange, TwoBSquare)
            end;
          end
          else
          begin
            X := 0;
            Y := YRadius;
            XChange := YRadius * YRadius;
            YChange := XRadius * XRadius * (1 - 2 * YRadius);
            EllipseError := 0;
            StoppingX := 0;
            StoppingY := TwoASquare * YRadius;
            State := 2;

            if StoppingX <= StoppingY then
            begin
              // the results.
              xo := X;
              yo := Y;
              x += 1;;
              inc(StoppingX, TwoBSquare);
              inc(EllipseError, XChange);
              inc(XChange, TwoBSquare);
              if ((2 * EllipseError + YChange) > 0) then
              begin
                y -= 1;
                dec(StoppingY, TwoASquare);
                inc(EllipseError, YChange);
                inc(YChange, TwoASquare)
              end;
            end
            else
            begin
              // done
              xo := x;
              yo := y;
              result := true;
            end;
          end;
        end;

      2:
        begin
          if StoppingX <= StoppingY then
          begin
            // the results.
            xo := X;
            yo := Y;
            x += 1;;
            inc(StoppingX, TwoBSquare);
            inc(EllipseError, XChange);
            inc(XChange, TwoBSquare);
            if ((2 * EllipseError + YChange) > 0) then
            begin
              y -= 1;
              dec(StoppingY, TwoASquare);
              inc(EllipseError, YChange);
              inc(YChange, TwoASquare)
            end;
          end
          else
          begin
            // done
            xo := x;
            yo := y;
            result := true;
          end;
        end;

    end;
  end;
end;

function QuadToStr(q : TQuad) : unicodestring;
begin
  result := format('%d,%d %d,%d', [ q.v0, q.v1, q.v2, q.v3]);
end;

function isInteger(str : unicodestring) : boolean;
var
  i : integer;
begin
  for i := 1 to str.length do
    if not between(str[i], '0', '9') then
      exit(false);
  result := true;
end;

function StrToQuad(str : unicodestring) : TQuad;
var
  l : integer;
  vals : TUnicodeStringArray;
begin
  result.v0 := 64;
  result.v1 := 64;
  result.v2 := 64;
  result.v3 := 64;
  vals := str.Split([',',' ']);
  l := length(vals);
  if (l >= 1) and isInteger(vals[0]) then result.v0 := strtoint(vals[0]);
  if (l >= 2) and isInteger(vals[1]) then result.v1 := strtoint(vals[1]);
  if (l >= 3) and isInteger(vals[2]) then result.v2 := strtoint(vals[2]);
  if (l >= 4) and isInteger(vals[3]) then result.v3 := strtoint(vals[3]);
  setlength(vals,0);
end;

procedure SetFormQuad(f : TForm; q : TQuad);
begin
  if q.v0 < 0 then q.v0 := 0;
  if q.v1 < 0 then q.v1 := 0;
  if q.v0 > Screen.Width then q.v0 := 0;
  if q.v1 > Screen.Height then q.v1 := 0;

  f.Left := q.v0;
  f.Top := q.v1;
  if q.v2 > 0 then
  begin
    f.Width := q.v2;
    f.Height := q.v3;
  end;
end;

function GetFormQuad(f : TForm) : TQuad;
begin
  result.v0 := f.RestoredLeft;
  result.v1 := f.RestoredTop;
  result.v2 := f.RestoredWidth;
  result.v3 := f.RestoredHeight;
end;

function CharsToStr(src : array of char; len : integer) : unicodestring;
var
  i : integer;
begin
  result := '';
  len := length(src);
  for i := 0 to len - 1 do
  begin
    if src[i] = #0 then
      break;
    result += src[i];
  end;
end;

function CharsToStr(src : array of byte; len : integer) : unicodestring;
var
  i : integer;
begin
  result := '';
  len := length(src);
  for i := 0 to len - 1 do
  begin
    if src[i] = 0 then
      break;
    result += char(src[i]);
  end;
end;

procedure DrawDashLine(cnv : TCanvas; x1, y1, x2, y2 : integer; clr1, clr2 : TColor);
begin
  cnv.Brush.Style := bsClear;
  cnv.Pen.Color := clr1;
  cnv.Pen.Style := psSolid;
  cnv.Line(x1, y1, x2, y2);
  cnv.Brush.Style := bsClear;
  cnv.Pen.Color := clr2;
  cnv.Pen.Style := psDot;
  cnv.Line(x1, y1, x2, y2);
end;

procedure DrawDashRect(cnv : TCanvas; rect : TRect; clr1, clr2 : TColor);
begin
  cnv.Brush.Style := bsClear;
  cnv.Pen.Color := clr1;
  cnv.Pen.Style := psSolid;
  cnv.Rectangle(rect);
  cnv.Brush.Style := bsClear;
  cnv.Pen.Color := clr2;
  cnv.Pen.Style := psDot;
  cnv.Rectangle(rect);
end;

procedure DrawDashRect(cnv : TCanvas; x1, y1, x2, y2 : integer; clr1, clr2 : TColor);
begin
  cnv.Brush.Style := bsClear;
  cnv.Pen.Color := clr1;
  cnv.Pen.Style := psSolid;
  cnv.Rectangle(x1, y1, x2, y2);
  cnv.Brush.Style := bsClear;
  cnv.Pen.Color := clr2;
  cnv.Pen.Style := psDot;
  cnv.Rectangle(x1, y1, x2, y2);
end;

procedure DrawStretchedBitmap(cnv : TCanvas; r : TRect; bmp : TBGRABitmap);
var
  tmpbmp : TBGRABitmap;
begin
  tmpbmp := bmp.Resample(r.Width, r.Height, rmSimpleStretch) as TBGRABitmap;
  tmpbmp.Draw(cnv, r.left, r.top);
  tmpbmp.free;
end;

procedure Draw3DRect(cnv : TCanvas; rect : TRect; sunk : boolean);
begin
  Draw3DRect(cnv, rect.Left, rect.Top, rect.Right, rect.Bottom, sunk);
end;

procedure Draw3DRect(cnv : TCanvas; x1, y1, x2, y2 : integer; sunk : boolean);
var
  c1, c2 : TBGRAPixel;
  bmp : TBGRABitmap;
  w, h : integer;
begin
  w := x2 - x1;
  h := y2 - y1;

  bmp := TBGRABitmap.Create(w, h, BGRAPixelTransparent);

  if sunk then
  begin
    c1 := BGRA(0, 0, 0, 192);
    c2 := BGRA(255, 255, 255, 192);
  end
  else
  begin
    c1 := BGRA(255, 255, 255, 192);
    c2 := BGRA(0, 0, 0, 192);
  end;

  bmp.DrawLine(0, h - 2, 0, 0, c1, true, dmSet);
  bmp.DrawLine(0, 0, w - 2, 0, c1, true, dmSet);
  bmp.DrawLine(1, h - 1, w - 1, h - 1, c2, true, dmSet);
  bmp.DrawLine(w - 1, h - 1, w - 1, 1, c2, true, dmSet);

  cnv.Draw(x1, y1, bmp.Bitmap);
  bmp.Free;
end;

end.

