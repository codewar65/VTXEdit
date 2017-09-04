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

unit VTXColorBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, ComCtrls, Math,
  VTXConst, VTXSupport, BGRABitmap, BGRABitmapTypes, Windows, Messages, Graphics,
  ExtCtrls, StdCtrls;

type

  { TfColor }

  TfColor = class(TForm)
    iFG: TImage;
    iBG: TImage;
    iBlotch: TImage;
    pbColors: TPaintBox;
    procedure pbColorsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbColorsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbColorsPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure WndProc(var Msg: TMessage); override;
    var
      FG, BG : integer;
      PalType : integer;
      MaxFG, MaxBG : integer;
  end;

var
  fColor: TfColor;

implementation

{$R *.lfm}

procedure TfColor.WndProc(var Msg:TMessage);
begin
  if Msg.msg = WM_VTXEDIT then
  begin
    case Msg.wParam of
      WA_COLOR_RESIZE:
        begin
          // resize window (0=8/8, 1=16/16, 2=256/256)
          case Msg.lParam of
            0: // 8/8
              begin
                MaxFG := 8;
                MaxBG := 8;
                self.Width := (8 * 23) + 1;
                self.Height := (1 * 23) + 1;
              end;
            1: // 16/8
              begin
                MaxFG := 16;
                MaxBG := 8;
                self.Width := (16 * 23) + 1;
                self.Height := (1 * 23) + 1;
              end;
            2: // 16/16
              begin
                MaxFG := 16;
                MaxBG := 16;
                self.Width := (16 * 23) + 1;
                self.Height := (1 * 23) + 1;
              end;
            3: // 256/256
              begin
                MaxFG := 256;
                MaxBG := 256;
                self.Width := (16 * 23) + 1;
                self.Height := (16 * 23) + 1;
              end;
          end;
          if FG >= MaxFG then FG := 7;
          if BG >= MaxBG then BG := 0;
          pbColors.Invalidate;
        end;

      WA_COLOR_SETVALS:
        begin
          paltype := 0;
          FG := GetBits(Msg.lParam, A_CELL_FG_MASK);
          BG := GetBits(Msg.lParam, A_CELL_BG_MASK, 8);
          if FG >= MaxFG then FG := 0;
          if BG >= MaxBG then BG := 0;
          pbColors.Invalidate;
        end;

      WA_COLOR_SINGLE:
        begin
          // set for single color.
          paltype := 1;
          FG := Msg.lParam;
          pbColors.Invalidate;
        end;
    end;
  end
  else
    inherited WndProc(Msg);
end;

{ TfColor }

procedure TfColor.pbColorsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  c : integer;
begin
  c := math.floor(x / 23) + (math.floor(y / 23) << 4);
  if PalType = 0 then
  begin
    if Button = mbLeft then
    begin
      if c < MaxFG then
        FG := c
    end
    else
    begin
      if c < MaxBG then
        BG := c;
    end;
  end
  else
    if c < MaxFG then
    begin
      FG := c;
      ModalResult := mrOK;
      Hide;
    end;
  Invalidate;
  SendMessage(TForm(Owner).Handle, WM_VTXEDIT, WA_MAIN_UPDATE, 0);
end;

var
  lastcolor : integer = -1;

procedure TfColor.pbColorsMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  c : integer;
  bgr : integer;
  rgb : integer;
begin
  // set hint
  c := math.floor(x / 23) + (math.floor(y / 23) << 4);
  if (c <> lastcolor) and between(c,0,maxFG-1) then
  begin
    bgr := ANSIColor[c];
    rgb := ((bgr and $0000FF) << 16) +
           ((bgr and $00FF00)) +
           ((bgr and $FF0000) >> 16);
    pbColors.Hint := Format('Color: %d'#10#13'RGB: #%6.6x', [ c, rgb ] );
    Application.ActivateHint(Mouse.CursorPos);
    lastcolor := c;
  end;
end;

procedure TfColor.pbColorsPaint(Sender: TObject);
var
  pb : TPaintBox;
  cnv : TCanvas;
  x, y, i : integer;
begin
  pb := TPaintBox(Sender);
  cnv := pb.Canvas;
  for i := 0 to 255 do
  begin
    x := (i and $F) * 23;
    y := (i >> 4) * 23;
    if (i < MaxFG) or (i < MaxBG) then
      begin
        cnv.Brush.Color := AnsiColor[i];
        cnv.FillRect(x + 2, y + 2, x + 22, y + 22);
        cnv.Draw(x, y, iBlotch.Picture.Bitmap);
        if paltype = 0 then
        begin
          if i = FG then
            cnv.Draw(x + 2, y + 2, iFG.Picture.Bitmap);
          if i = BG then
            cnv.Draw(x + 10, y + 10, iBG.Picture.Bitmap);
        end
        else
        begin
          // single color
          if i = FG then
          begin
            DrawRectangle(cnv, x + 3, y + 3, x + 20, y + 20, clBlack);
            DrawRectangle(cnv, x + 4, y + 4, x + 19, y + 19, clWhite);
            DrawRectangle(cnv, x + 5, y + 5, x + 18, y + 18, clBlack);
          end;
        end;
      end;
  end;
end;

end.

