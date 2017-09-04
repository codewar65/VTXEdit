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

unit VTXAttrBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs,
  ExtCtrls, Buttons, Windows, Messages, VTXConst, VTXSupport, Graphics, BGRABitmap, BGRABitmapTypes;

type

  { TfAttr }

  TfAttr = class(TForm)
    bBlinkFast: TPaintBox;
    bBlinkSlow: TPaintBox;
    bBold: TPaintBox;
    bCharacter: TPaintBox;
    bForeground: TPaintBox;
    bBackground: TPaintBox;
    bBottomHalf: TPaintBox;
    bConceal: TPaintBox;
    bDoublestrike: TPaintBox;
    bFaint: TPaintBox;
    bItalics: TPaintBox;
    bReverse: TPaintBox;
    bShadow: TPaintBox;
    bStrikethrough: TPaintBox;
    bTopHalf: TPaintBox;
    bUnderline: TPaintBox;
    pbClose: TPaintBox;
    pbTitleBar: TPaintBox;
    procedure bToolBarButtonClick(Sender: TObject);
    procedure bBoldMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bBoldMouseEnter(Sender: TObject);
    procedure bBoldMouseLeave(Sender: TObject);
    procedure bBoldMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbCloseClick(Sender: TObject);
    procedure pbClosePaint(Sender: TObject);
    procedure pbTitleBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbTitleBarPaint(Sender: TObject);
    procedure tbPaintBox1Paint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure WndProc(var Msg: TMessage); override;
    var
      Bold, Faint, Italics, Underline, BlinkSlow, BlinkFast, Reverse, Conceal,
      StriketHrough, Doublestrike, Shadow, TopHalf, BottomHalf : boolean;
  end;


var
  fAttr: TfAttr;

implementation

{$R *.lfm}

{ TfAttr }

procedure TfAttr.WndProc(var Msg:TMessage);
var
  attr : Uint32;
  i, thisi : integer;
  twi : TWINDOWINFO;
  pwp : PWINDOWPOS;
  bx, by : integer;
begin
  if Msg.msg = WM_VTXEDIT then
    begin
      case Msg.wParam of
        WA_ATTR_ENABLEALL:
          begin
            bBold.Enabled := true;
            bFaint.Enabled := true;
            bItalics.Enabled := true;
            bUnderline.Enabled := true;
            bBlinkSlow.Enabled := true;
            bBlinkFast.Enabled := true;
            bReverse.Enabled := true;
            bConceal.Enabled := true;
            bStrikethrough.Enabled := true;
            bDoublestrike.Enabled := true;
            bShadow.Enabled := true;
            bTopHalf.Enabled := true;
            bBottomHalf.Enabled := true;
          end;

        WA_ATTR_DISABLENONVTX:
          begin
            bBold.Enabled := false;           SetDown(bBold, false);
            bFaint.Enabled := false;          SetDown(bFaint, false);
            bItalics.Enabled := false;        SetDown(bItalics,false);
            bUnderline.Enabled := false;      SetDown(bUnderline,false);
            bBlinkSlow.Enabled := false;      SetDown(bBlinkSlow,false);
            bBlinkFast.Enabled := true;
            bReverse.Enabled := true;
            bConceal.Enabled := true;
            bStrikethrough.Enabled := false;  SetDown(bStrikethrough, false);
            bDoublestrike.Enabled := false;   SetDown(bDoublestrike, false);
            bShadow.Enabled := false;         SetDown(bShadow, false);
            bTopHalf.Enabled := false;        SetDown(bTopHalf, false);
            bBottomHalf.Enabled := false;     SetDown(bBottomHalf, false);
          end;

        WA_ATTR_SETVALS:
          begin
            attr := Msg.lParam;
            if HasBits(attr, A_CELL_BOLD) and bBold.Enabled then
              SetDown(bBold, true);

            if HasBits(attr, A_CELL_FAINT) and bFaint.Enabled then
              SetDown(bFaint, true);

            if HasBits(attr, A_CELL_ITALICS) and bItalics.Enabled then
              SetDown(bItalics, true);

            if HasBits(attr, A_CELL_UNDERLINE) and bUnderline.Enabled then
              SetDown(bUnderline, true);

            if HasBits(attr, A_CELL_BLINKSLOW) and bBlinkSlow.Enabled then
              SetDown(bBlinkSlow, true);

            if HasBits(attr, A_CELL_BLINKFAST) and bBlinkFast.Enabled then
              SetDown(bBlinkFast, true);

            if HasBits(attr, A_CELL_REVERSE) and bReverse.Enabled then
              SetDown(bReverse, true);

            if (GetBits(attr, A_CELL_DISPLAY_MASK) = A_CELL_DISPLAY_CONCEAL)
            and bConceal.Enabled then
              SetDown(bConceal, true);

            if HasBits(attr, A_CELL_STRIKETHROUGH)
            and bStrikethrough.Enabled then
              SetDown(bStrikethrough, true);

            if HasBits(attr, A_CELL_DOUBLESTRIKE) and bDoublestrike.Enabled then
              SetDown(bDoublestrike, true);

            if HasBits(attr, A_CELL_SHADOW) and bShadow.Enabled then
              SetDown(bShadow, true);

            if (GetBits(attr, A_CELL_DISPLAY_MASK) = A_CELL_DISPLAY_TOP)
            and bTopHalf.Enabled then
              SetDown(bTopHalf, true);

            if (GetBits(attr, A_CELL_DISPLAY_MASK) = A_CELL_DISPLAY_BOTTOM)
            and bBottomHalf.Enabled then
              SetDown(bBottomHalf, true);
          end;
    end;
  end
  else
    inherited WndProc(Msg);
end;

// enable / disable control based on mode
procedure TfAttr.bToolBarButtonClick(Sender: TObject);
var
  pb : TPaintBox;
begin
  pb := TPaintBox(Sender);

  if GetDown(pb) then
    case pb.Name of
      'bConceal':
        begin
          SetDown(bTopHalf, false);
          SetDown(bBottomHalf, false);
        end;

      'bTopHalf':
        begin
          SetDown(bConceal, false);
          SetDOwn(bBottomHalf, false);
        end;

      'bBottomHalf':
        begin
          SetDown(bConceal, false);
          SetDown(bTopHalf, false);
        end;

      'bBlinkSlow':
          SetDown(bBlinkFast, false);

      'bBlinkFast':
          SetDown(bBlinkSlow, false);
    end;

  // build CurrAttr based on controls
  Bold := GetDown(bBold);
  Faint := GetDown(bFaint);
  Italics := GetDown(bItalics);
  Underline := GetDown(bUnderline);
  BlinkSlow := GetDown(bBlinkSlow);
  BlinkFast := GetDown(bBlinkFast);
  Reverse := GetDown(bReverse);
  Conceal := GetDown(bConceal);
  StriketHrough := GetDown(bStrikethrough);
  Doublestrike := GetDown(bDoublestrike);
  Shadow := GetDown(bShadow);
  TopHalf := GetDown(bTopHalf);
  BottomHalf := GetDown(bBottomHalf);

  SendMessage(TForm(Owner).Handle, WM_VTXEDIT, WA_MAIN_UPDATE, 0);
end;

// paintboxbutton

procedure TfAttr.bBoldMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  v : Uint32;
  pb : TPaintBox;
  down : boolean;
  btype : integer;
begin
  pb := TPaintBox(Sender);
  v := pb.tag;
  btype := GetBits(v, PBB_TYPE_MASK);
  if Button = mbLeft then
  begin
    // click
    if btype <> PBB_TYPE_STATIC then
    begin
      // not on statics
      down := HasBits(v, PBB_DOWN);
      if btype = PBB_TYPE_TOGGLE then
        SetBit(v, PBB_DOWN, not down)
      else
        SetBit(v, PBB_DOWN, true);
    end;
  end
  else if (Button = mbRight) and HasBits(v, PBB_FLAG_IGNORABLE) then
  begin
    // set ignore state
    SetBit(v, PBB_IGNORE, not HasBits(v, PBB_IGNORE));
  end;
  pb.tag := v;
  pb.Invalidate;
end;

procedure TfAttr.bBoldMouseUp(Sender: TObject; Button: TMouseButton;
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

// picturebox controls

procedure TfAttr.bBoldMouseEnter(Sender: TObject);
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

procedure TfAttr.bBoldMouseLeave(Sender: TObject);
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


procedure TfAttr.tbPaintBox1Paint(Sender: TObject);
var
  pb : TPaintBox;
  cnv : TCanvas;
  r : TRect;
  bmp : TBGRABitmap;
  n : integer;
  size : integer;
  adj : integer;
  down : boolean;
  x, y : integer;
begin
  // use tag for states
  pb := TPaintBox(Sender);
  cnv := pb.Canvas;
  down := HasBits(pb.Tag, PBB_DOWN);

  // get icon
  n := GetBits(pb.Tag, PBB_IMAGE_MASK);
  size := iconsNormal.Height;
  r.Top := 0;
  r.Left := n * size;
  r.Width := size;
  r.Height := size;
  if not pb.Enabled then
    bmp := iconsGrayed.GetPart(r) as TBGRABitmap
  else if HasBits(pb.tag, PBB_HOVER) then
    bmp := iconsHilite.GetPart(r) as TBGRABitmap
  else if HasBits(pb.tag, PBB_DOWN) then
    bmp := iconsDown.GetPart(r) as TBGRABitmap
  else
    bmp := iconsNormal.GetPart(r) as TBGRABitmap;

  // draw button
  if down and pb.Enabled then
    textureDown.Draw(cnv, 0,0)
  else
    textureUp.Draw(cnv, 0, 0);

  adj := 0;
  if down then
    adj := 1;
  x := ((pb.Width - bmp.Width) >> 1);
  y := ((pb.Height - bmp.Height) >> 1) - 2 + adj;
  cnv.Draw(x, y, bmp.Bitmap);

  if pb.Enabled and HasBits(pb.Tag, PBB_IGNORE) then
  begin
    r.Top := 0;
    r.Left := 31 * size;
    r.Width := size;
    r.Height := size;
    bmp.free;
    bmp := iconsNormal.GetPart(r) as TBGRABitmap;
    cnv.Draw(x, y, bmp.Bitmap);
  end;

  bmp.free;
end;

// CAPTION BAR

procedure TfAttr.pbCloseClick(Sender: TObject);
begin
  Hide;
end;

procedure TfAttr.pbClosePaint(Sender: TObject);
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

procedure TfAttr.pbTitleBarPaint(Sender: TObject);
var
  pb : TPaintBox;
  cnv : TCanvas;
  r : TRect;
const
  titletxt = 'Attributes';
  helptxt = 'left=toggle, right=ignore';
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
  cnv.TextOut(pbClose.Left - cnv.TextWidth(helptxt) - 6, 1, helptxt);
end;

// toolbar move routines
procedure TfAttr.pbTitleBarMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  SendMessage(Handle, WM_NCLBUTTONDOWN, HTCAPTION, 0);
end;

// end toobar move routines

end.

