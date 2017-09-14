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

{

UTF 8 nas no characters in the  128-191 range

  https://gist.github.com/NuSkooler/a235339cf383759c49c57ee30d34876c?fref=gc&dti=254577004687053

  TODO :

    Merge / Move back - from / copy / flip objects

    save / load objects

    save objects in vtx file

    paste object to center of window

    undo / redo painting and object manipulation

    row attributes

    page / border / cursor attributes

    transparent black in vtx mode

    border on display / center document on window

    line / rectangle / ellipse / fill tools

    Zoom on mouse position

    Confirn save on exit / load

    Terminate ANSI @ $1A

    Prefernce window

    Sauce Info Panel

    Perferences:
      [■] Save Sauce
      [■] Save BOM
      (●) LE  ( ) BE  on UTF16 save

    UNDO

    Font Palette.
      Font 0-15 selectable (0=default codepage)
      Fonts 1-9 are programmable.

    Mode Char / Block (sixels in Teletext font)

    ESC cancels tool to default on editor


}

unit VTXEdit;

{$mode objfpc}{$H+}
{$codepage utf8}
{$ASMMODE intel}
{$modeswitch advancedrecords}

interface

uses
  Windows,
  Classes,
  SysUtils,
  strutils,
  FileUtil,
  Forms,
  Controls,
  Dialogs,
  ExtCtrls,
  Menus,
  LCL, LCLType,
  StdCtrls,
  Buttons,
  Graphics,
  Spin, ComCtrls,
  Math,
  BGRABitmap,
  BGRABitmapTypes,
  Types,
  VTXPreviewBox,
  VTXConst,
  VTXSupport,
  VTXEncDetect,
  UnicodeHelper,
  LazUTF8,
  Inifiles;

// used for version
const
  yyyy = 2017;
  mm = 09;
  dd = 02;

type

  { TfMain }

  TfMain = class(TForm)
    cbCodePage: TComboBox;
    cbColorScheme: TComboBox;
    cbPageType: TComboBox;
    CoolBar1: TCoolBar;
    ilButtons: TImageList;
    ilDisabledButtons: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lvObjects: TListView;
    MenuItem1: TMenuItem;
    miObjNext: TMenuItem;
    miObjPrev: TMenuItem;
    miObjBackOne: TMenuItem;
    miObjMerge: TMenuItem;
    MenuItem11: TMenuItem;
    miObjForwardOne: TMenuItem;
    MenuItem3: TMenuItem;
    miObjFlipHorz: TMenuItem;
    MenuItem6: TMenuItem;
    miObjToFront: TMenuItem;
    miObjToBack: TMenuItem;
    miObjFlipVert: TMenuItem;
    miObjects: TMenuItem;
    miEditUndo: TMenuItem;
    miEditCut: TMenuItem;
    miEditRedo: TMenuItem;
    MenuItem4: TMenuItem;
    miEditCopy: TMenuItem;
    miEditPaste: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    miFileOpen: TMenuItem;
    miFileSave: TMenuItem;
    miViewPreview: TMenuItem;
    miView: TMenuItem;
    odAnsi: TOpenDialog;
    pbChars: TPaintBox;
    pbColors: TPaintBox;
    pRightBar: TPanel;
    pbPage: TPaintBox;
    pbRulerLeft: TPaintBox;
    pbRulerTop: TPaintBox;
    pPagePanel: TPanel;
    pbCurrCell: TPaintBox;
    miFileExit: TMenuItem;
    miFileNew: TMenuItem;
    mMenu: TMainMenu;
    miFile: TMenuItem;
    miEdit: TMenuItem;
    miHelp: TMenuItem;
    pbStatusBar: TPaintBox;
    pSettings: TPanel;
    sbHorz: TScrollBar;
    sbVert: TScrollBar;
    sbChars: TScrollBox;
    sdAnsi: TSaveDialog;
    seCharacter: TSpinEdit;
    seCols: TSpinEdit;
    seRows: TSpinEdit;
    seXScale: TFloatSpinEdit;
    irqBlink: TTimer;
    SpeedButton1: TSpeedButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    tbCodePage: TEdit;
    tbSauceAuthor: TEdit;
    tbSauceDate: TEdit;
    tbSauceGroup: TEdit;
    tbSauceTitle: TEdit;
    tbToolPalette: TToolBar;
    tbAttributesPalette: TToolBar;
    tbToolFill: TToolButton;
    tbToolEyedropper: TToolButton;
    tbToolLine: TToolButton;
    tbToolRect: TToolButton;
    tbToolEllipse: TToolButton;
    tbFontPalette: TToolBar;
    tbFont0: TToolButton;
    tbFont8: TToolButton;
    tbFont9: TToolButton;
    tbFont10: TToolButton;
    tbUnicode: TEdit;
    ToolBar1: TToolBar;
    bObjMoveBack: TToolButton;
    ToolButton15: TToolButton;
    tbModeCharacter: TToolButton;
    tbModeLeftRights: TToolButton;
    tbModeTopBottoms: TToolButton;
    tbModeQuarters: TToolButton;
    tbModeSixels: TToolButton;
    tbAttrBold: TToolButton;
    tbAttrFaint: TToolButton;
    tbAttrItalics: TToolButton;
    tbAttrUnderline: TToolButton;
    tbAttrBlinkSlow: TToolButton;
    tbAttrBlinkFast: TToolButton;
    tbAttrReverse: TToolButton;
    tbAttrConceal: TToolButton;
    tbAttrStrikethrough: TToolButton;
    tbAttrDoublestrike: TToolButton;
    tbAttrShadow: TToolButton;
    tbAttrTop: TToolButton;
    tbAttrBottom: TToolButton;
    tbAttrCharacter: TToolButton;
    tbAttrFG: TToolButton;
    tbAttrBG: TToolButton;
    tbPreview: TToolButton;
    tbToolSelect: TToolButton;
    tbFont1: TToolButton;
    tbFont2: TToolButton;
    tbFont3: TToolButton;
    tbFont4: TToolButton;
    tbFont5: TToolButton;
    tbFont6: TToolButton;
    tbFont11: TToolButton;
    tbFont12: TToolButton;
    bObjMoveToBack: TToolButton;
    bObnjMoveForward: TToolButton;
    bObjMoveToFront: TToolButton;
    bObjFlipHorz: TToolButton;
    bObjFlipVert: TToolButton;
    bObjMerge: TToolButton;
    ToolButton8: TToolButton;
    tbToolDraw: TToolButton;
    tbFont7: TToolButton;
    procedure miObjNextClick(Sender: TObject);
    procedure miObjPrevClick(Sender: TObject);
    procedure RemoveObject(objnum : integer);
    procedure ObjFlipHorz;
    procedure ObjFlipVert;
    procedure ObjMoveBack;
    procedure ObjMoveForward;
    procedure ObjMoveToBack;
    procedure ObjMoveToFront;
    procedure ObjMerge;
    procedure ObjNext;
    procedure ObjPrev;
    procedure bObjFlipHorzClick(Sender: TObject);
    procedure bObjFlipVertClick(Sender: TObject);
    procedure bObjMergeClick(Sender: TObject);
    procedure bObjMoveBackClick(Sender: TObject);
    procedure bObjMoveToBackClick(Sender: TObject);
    procedure bObjMoveToFrontClick(Sender: TObject);
    procedure bObnjMoveForwardClick(Sender: TObject);
    procedure RefreshObject(objnum : integer);
    function GetObjectCell(row, col : integer; var cell : TCell; var neighbors : byte) : integer;
    function GetObject(row, col : integer) : integer;
    procedure LoadlvObjects;
    function CopySelectionToObject : TObj;
    procedure BuildCharacterPalette;
    function BuildDisplayCopySelection : TLocList;
    procedure lvObjectsDrawItem(Sender: TCustomListView; AItem: TListItem; ARect: TRect; AState: TOwnerDrawState);
    procedure lvObjectsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbCharsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbCharsPaint(Sender: TObject);
    procedure pbColorsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbColorsPaint(Sender: TObject);
    procedure seCharacterChange(Sender: TObject);
    procedure tbAttrClick(Sender: TObject);
    procedure tbAttrMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure tbFontClick(Sender: TObject);
    procedure tbModeClick(Sender: TObject);
    procedure tbToolClick(Sender: TObject);
    procedure tbAttributesPalettePaintButton(Sender: TToolButton; State: integer);
    procedure UpdateTitles;
    procedure SaveVTXFile(fname : string);
    procedure LoadVTXFile(fname : string);
    procedure pbCurrCellPaint(Sender: TObject);
    procedure UpdatePreview;
    procedure DrawCellEx(
      cnv : TCanvas;
      x, y, row, col : integer;
      skipUpdate : boolean = true;
      skipDraw : boolean = false;
      usech : uint16 = EMPTYCHAR;
      useattr : uint32 = $FFFF);
    procedure SetAttrButtons(attr : Uint32);
    procedure cbCodePageChange(Sender: TObject);
    procedure cbColorSchemeChange(Sender: TObject);
    procedure cbPageTypeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure irqBlinkTimer(Sender: TObject);
    procedure miFileExitClick(Sender: TObject);
    procedure miFileOpenClick(Sender: TObject);
    procedure miFileNewClick(Sender: TObject);
    procedure miFileSaveClick(Sender: TObject);
    procedure pbPageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbPageMouseLeave(Sender: TObject);
    procedure pbPageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pbPageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbPageMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure pbPagePaint(Sender: TObject);
    procedure pbPreviewClick(Sender: TObject);
    procedure pbRulerLeftPaint(Sender: TObject);
    procedure pbRulerTopPaint(Sender: TObject);
    procedure pbStatusBarPaint(Sender: TObject);
    procedure ResizeScrolls;
    procedure ResizePage;
    procedure sbHorzChange(Sender: TObject);
    procedure sbVertChange(Sender: TObject);
    procedure DrawCell(row, col : integer; skipUpdate : boolean = true);
    procedure sePageSizeChange(Sender: TObject);
    procedure ScrollToCursor;
    procedure CursorRight;
    procedure CursorLeft;
    procedure CursorUp;
    procedure CursorDown;
    procedure CursorStatus;
    procedure CursorNewLine;
    procedure CursorForwardTab;
    procedure CursorBackwardTab;
    procedure CursorMove(row, col : integer);
    procedure seXScaleChange(Sender: TObject);
    procedure DrawMouseBox;
    procedure PutCharExpand(ch : integer);
    procedure PutChar(ch : integer);
    procedure PutCharEx(ch, cattr, row, col : integer);
    function HasUnicodes(cp : TEncoding; chars : array of UInt16) : boolean;
    function GetUnicode(cell : TCell) : integer;
    function GetCPChar(cp : TEncoding; unicode : integer) : integer;
    function GetColors2x1(cell : TCell) : TColors2;
    function GetColors1x2(cell : TCell) : TColors2;
    function GetColors2x2(cell : TCell) : TColors4;
    function GetBlockColor(cell : TCell; xsize, ysize, x, y : integer) : integer;
    function SetBlockColor(clr: integer; cell : TCell; xsize, ysize, x, y : integer) : TCell;
    procedure GenerateBmpPage;
    procedure CodePageChange;
    function GetNextCell(r, c : integer) : TRowCol;
    Procedure LoadSettings;
    Procedure SaveSettings;
    function BuildANSI : unicodestring;
    procedure SaveANSIFile(fname : unicodestring; ansi : unicodestring; enc : TEncoding);
    procedure NewFile;
    procedure DoBlink;

  private
    { private declarations }

  public
    { public declarations }

  end;

  // save as native character. (not converted to unicode)
  TFKeySet = array [0..9] of byte;

const
  BlankCell : TCell = (
    Chr:    _SPACE;
    Attr:   7; );

var
  fMain: TfMain;

  FKeys : packed array [0..9] of TFKeySet = (
      ( $DA, $BF, $C0, $D9, $C4, $B3, $C3, $B4, $C1, $C2 ),
      ( $C9, $BB, $C8, $BC, $CD, $BA, $CC, $B9, $CA, $CB ),
      ( $D5, $B8, $D4, $BE, $CD, $B3, $C6, $B5, $CF, $D1 ),
      ( $D6, $B7, $D3, $BD, $C4, $BA, $C7, $B6, $D0, $D2 ),
      ( $C5, $CE, $D8, $D7, $E8, $E8, $9B, $9C, $99, $EF ),
      ( $B0, $B1, $B2, $DB, $DF, $DC, $DD, $DE, $FE, $FA ),
      ( $01, $02, $03, $04, $05, $06, $F0, $0E, $0F, $20 ),
      ( $18, $19, $1E, $1F, $10, $11, $12, $1D, $14, $15 ),
      ( $AE, $AF, $F2, $F3, $A9, $AA, $FD, $F6, $AB, $AC ),
      ( $E3, $F1, $F4, $F5, $EA, $9D, $E4, $F8, $FB, $FC ));


procedure DebugStart;
procedure nop;

procedure CopyObject(objin : TObj; var objout : TObj);


implementation

{$R *.lfm}

{*****************************************************************************}

{ Private Globals }
var
  // tool windows
  fPreviewBox : TfPreview;

  CurrFileName :            unicodestring;
  CurrFileChanged :         boolean;
  Page :                    TPage;      // main doc
  NumCols, NumRows :        integer;    // doc size
  PageTop, PageLeft :       integer;    // upper left corner position
  WindowCols, WindowRows :  integer;    // visible area of window
  MouseRow, MouseCol :      integer;    // mouse position (-1 if off page)
  LastDrawRow, LastDrawCol: integer;
  LastDrawX, LastDrawY :    integer;
  BlinkFast, BlinkSlow :    boolean;    // blink states
  MousePan :                boolean = false;
  MousePanX, MousePanY :    integer;    // current mouse xy
  SubXSize, SubYSize :      integer;    // coords subdivision size
  DrawX, DrawY :            integer;    // drawing position in draw mode coors
  SubX, SubY :              integer;    // sub pos inside character
  MousePanT, MousePanL :    integer;    // current mouse rc
  CursorRow, CursorCol :    integer;    // cursor location.
  CurrChar :                integer;    // selected char to draw with
  CurrAttr :                UInt32;     // current attributes
  CurrFKeySet :             integer = 5;
  ToolMode :                TToolModes;
  DrawMode :                TDrawModes;

  // straighten this out . need one as default.
  CurrFont :                integer;      // current font selected.
  CurrCodePage :            TEncoding;

  MouseLeft,
  MouseMiddle,
  MouseRight :              boolean;

  // objects
  SelectedObject :          integer;
  Objects :                 TObjList;
  dragObj :                 boolean;    // in object move mode?
  Clipboard :               TObj;

  // region selection
  CopySelection :           TLocList;
  drag :                    boolean;    // in drag mode
  dragType :                integer;    // 0=select, 1=add, 2=remove
  dragRow, dragCol :        integer;    // start of drag

  // fonts. (CSI 10-19 / 80-85 <space> D
  Fonts :                   array [0..15] of TEncoding;

  SkipScroll :              boolean;      // disable scroll to cursor
  SkipResize :              boolean;      // skip onchange updates on dynamic change.

  bmpCharPalette : TBGRABitmap = nil;

{*****************************************************************************}

// return the topmost cell of an object at row, cell or EMPTYCELL
function TfMain.GetObjectCell(row, col : integer; var cell : TCell; var neighbors : byte) : integer;
var
  i :             integer;
  po :            PObj;
  objr, objc, p : integer;
begin
  for i := length(Objects) - 1 downto 0 do
  begin
    po := @Objects[i];
    if (row >= po^.Row) and (row < (po^.Row + po^.Height)) and (not po^.Hidden) then
    begin
      // on the row
      if (col >= po^.Col) and (col < (po^.Col + po^.Width)) then
      begin
        // on the col
        objr := row - po^.Row;
        objc := col - po^.Col;
        p := objr * po^.Width + objc;
        if po^.Data[p].Chr <> EMPTYCHAR then
        begin
          neighbors := %0000;
          if (objr > 0) and (po^.Data[p - po^.Width].Chr <> EMPTYCHAR) then
            neighbors := neighbors or NEIGHBOR_NORTH;
          if (objr < po^.Height - 1) and (po^.Data[p + po^.Width].Chr <> EMPTYCHAR) then
            neighbors := neighbors or NEIGHBOR_SOUTH;
          if (objc > 0) and (po^.Data[p - 1].Chr <> EMPTYCHAR) then
            neighbors := neighbors or NEIGHBOR_WEST;
          if (objc < po^.Width - 1) and (po^.Data[p + 1].Chr <> EMPTYCHAR) then
            neighbors := neighbors or NEIGHBOR_EAST;
          cell := po^.Data[objr * po^.Width + objc];
          exit(i);
        end;
      end;
    end;
  end;
  cell.Chr := EMPTYCHAR;
  cell.Attr := $0007;
  neighbors := %1111;
  result := -1;
end;

function TfMain.GetObject(row, col : integer) : integer;
var
  i :             integer;
  po :            PObj;
  objr, objc, p : integer;
begin
  for i := length(Objects) - 1 downto 0 do
  begin
    po := @Objects[i];
    if (row >= po^.Row) and (row < (po^.Row + po^.Height)) and (not po^.Hidden) then
      // on the row
      if (col >= po^.Col) and (col < (po^.Col + po^.Width)) then
      begin
        // on the col
        objr := row - po^.Row;
        objc := col - po^.Col;
        p := objr * po^.Width + objc;
        if po^.Data[p].Chr <> EMPTYCHAR then
          exit(i);
      end;
  end;
  result := -1;
end;

// refresh cursor and blinking text
procedure TfMain.DoBlink;
var
  x, y, r, c :  integer;
  cell :        TCell;
  cnv :         TCanvas;
  docell :      boolean;
  objnum :      integer;
  neighbors :   byte;
  h1, h2,
  v1, v2 :      integer;
  i : integer;
begin
  if bmpPage <> nil then
  begin
    BlinkFast := not BlinkFast;
    if BlinkFast then
    begin
      BlinkSlow := not BlinkSlow;
    end;

    // only display blink on normal or higher zoom
    if PageZoom >= 1 then
    begin
      cnv := pbPage.Canvas;
      y := 0;
      for r := PageTop to NumRows - 1 do
      begin
        if r > PageTop + WindowRows then
          break;
        x := 0;
        for c := PageLeft to NumCols - 1 do
        begin
          if c > PageLeft + WindowCols then
            break;

          docell := false;

          objnum := GetObjectCell(r, c, cell, neighbors);
          if objnum = -1 then
            cell := Page.Rows[r].Cells[c];

          // check / fix this logic
          if HasBits(cell.Attr, A_CELL_BLINKSLOW)
            or ((CursorRow = r) and (CursorCol = c)) then
            docell := true;
          if (HasBits(cell.Attr, A_CELL_BLINKFAST) and (ColorScheme <> COLORSCHEME_ICE)) then
            docell := true;

          if docell then
          begin
            DrawCellEx(cnv, x, y, r, c, false, false, cell.chr, cell.attr);

            // draw cursor over top if cursor location (if on page)
            if (CursorRow = r) and (CursorCol = c) and (objnum = -1) then
            begin

              // draw selection border
              for i := length(CopySelection)-1 downto 0 do
              begin
                if (CopySelection[i].Row = r) and (CopySelection[i].Col = c) then
                begin
                  if not HasBits(CopySelection[i].Neighbors, NEIGHBOR_NORTH) then
                    DrawDashLine(cnv,
                      x, y,
                      x + CellWidthZ, y,
                      clSelectionArea1, clSelectionArea2);

                  if not HasBits(CopySelection[i].Neighbors, NEIGHBOR_SOUTH) then
                    DrawDashLine(cnv,
                      x, y + CellHeightZ - 1,
                      x + CellWidthZ, y + CellHeightZ - 1,
                      clSelectionArea1, clSelectionArea2);

                  if not HasBits(CopySelection[i].Neighbors, NEIGHBOR_WEST) then
                    DrawDashLine(cnv,
                      x, y,
                      x, y + CellHeightZ,
                      clSelectionArea1, clSelectionArea2);

                  if not HasBits(CopySelection[i].Neighbors, NEIGHBOR_EAST) then
                    DrawDashLine(cnv,
                      x + CellWidthZ - 1, y,
                      x + CellWidthZ - 1, y + CellHeightZ,
                      clSelectionArea1, clSelectionArea2);
                  break;
                end;
              end;

              if not BlinkFast then
              begin
                h1 := floor(1 * PageZoom);
                h2 := floor(5 * PageZoom);
                v1 := floor(1 * PageZoom * XScale);
                v2 := floor(3 * PageZoom * XScale);
                cnv.Brush.Color := ANSIColor[GetBits(Page.CrsrAttr, A_CURSOR_COLOR_MASK)];
                case GetBits(Page.CrsrAttr, A_CURSOR_VERTICAL or A_CURSOR_SIZE_MASK, 8) of
                    1:  // horz thin
                      cnv.FillRect(x, y + CellHeightZ - h1, x + CellWidthZ, y + CellHeightZ);

                    2:  // horz thick
                      cnv.FillRect(x, y + CellHeightZ - h2, x + CellWidthZ, y + CellHeightZ);

                    5:  // vert thin
                      cnv.FillRect(x, y, x + v1, y + CellHeightZ);

                    6:  // vert thick
                      cnv.FillRect(x, y, x + v2, y + CellHeight);

                    3, 7: // full
                      cnv.FillRect(x, y, x + CellWidthZ, y + CellHeightZ);
                end;
              end;
            end
            else
            begin
              // never a object border on a cursor
              cnv.Brush.Style := bsClear;
              if objnum = SelectedObject then
                cnv.Pen.Color := clSelectedObject1
              else
                cnv.Pen.Color := clUnselectedObject1;
              cnv.Pen.Style := psSolid;
              if not HasBits(neighbors, NEIGHBOR_NORTH) then
                cnv.Line(x, y, x + CellWidthZ - 1, y);
              if not HasBits(neighbors, NEIGHBOR_SOUTH) then
                cnv.Line(x, y + CellHeightZ - 1, x + CellWidthZ - 1, y + CellHeightZ - 1);
              if not HasBits(neighbors, NEIGHBOR_WEST) then
                cnv.Line(x, y, x, y + CellHeightZ - 1);
              if not HasBits(neighbors, NEIGHBOR_EAST) then
                cnv.Line(x + CellWidthZ - 1, y, x + CellWidthZ - 1, y + CellHeightZ - 1);

              if objnum = SelectedObject then
                cnv.Pen.Color := clSelectedObject2
              else
                cnv.Pen.Color := clUnselectedObject2;
              cnv.Pen.Style := psDot;
              if not HasBits(neighbors, NEIGHBOR_NORTH) then
                cnv.Line(x, y, x + CellWidthZ - 1, y);
              if not HasBits(neighbors, NEIGHBOR_SOUTH) then
                cnv.Line(x, y + CellHeightZ - 1, x + CellWidthZ - 1, y + CellHeightZ - 1);
              if not HasBits(neighbors, NEIGHBOR_WEST) then
                cnv.Line(x, y, x, y + CellHeightZ - 1);
              if not HasBits(neighbors, NEIGHBOR_EAST) then
                cnv.Line(x + CellWidthZ - 1, y, x + CellWidthZ - 1, y + CellHeightZ - 1);
            end;
          end;
          x += CellWidthZ;
        end;
        y += CellHeightZ;
      end;
    end;
  end;
end;

procedure TfMain.irqBlinkTimer(Sender: TObject);
begin
  DoBlink;
end;

// resize Page based on Cols, Rows
procedure TfMain.ResizePage;
var
  r, c, i, j : integer;

begin
  r := length(Page.Rows); // get actual size
  if r < NumRows then     // on add new lines if they do not exist
  begin
    setlength(Page.Rows, NumRows);
    for i := r to NumRows - 1 do
      Page.Rows[i].Attr := A_ROW_HEIGHT_100 or A_ROW_WIDTH_100;
  end;

  for i := 0 to NumRows - 1 do
  begin
    c := length(Page.Rows[i].Cells);  // get actual width
    if c < NumCols then
    begin
      setlength(Page.Rows[i].Cells, NumCols);
      for j := c to NumCols - 1 do
        Page.Rows[i].Cells[j] := BlankCell;
    end;
  end;
end;


{ TfMain }

procedure TfMain.UpdateTitles;
var
  altered : unicodestring;
begin
  altered := '';
  if CurrFileChanged then
    altered := '*';
  Caption := Format('VTXEdit : %s - %s%s', [ Version, altered, CurrFileName ]);
  Application.Title := Format('VTXEdit - %s%s', [ altered, CurrFileName ]);
end;

procedure TfMain.FormCreate(Sender: TObject);
var
  cp :  TEncoding;
  i :   integer;
  gt :  pbyte;
  gts : integer;
  enc : integer;
begin

  DebugStart;

  CurrFileName := 'Untitled.vtx';
  CurrFileChanged := false;

  // build codepage table
  cbCodePage.Enabled := false;
  cbCodePage.Items.Clear;
  for cp := encCP437 to encUTF16 do
    cbCodePage.Items.Add(Cpages[cp].Name);
  cbCodePage.Enabled := true;

  //DecodeDate(now, yyyy, mm, dd);
  Version := Format('%0.4d%0.2d%0.2d alpha', [yyyy, mm, dd]);
  UpdateTitles;

  DoubleBuffered := true;

  bmpPage := nil;
  bmpPreview := nil;
  SkipScroll := false;

  // create tool windows
  fPreviewBox := TfPreview.Create(self);

  // initialize new document
  NumRows := 24;
  NumCols := 80;
  XScale := 1.0;
  skipResize := true;
  seRows.Value := NumRows;
  seCols.Value := NumCols;
  skipResize := false;

  CellWidth := 8;
  CellHeight := 16;
  CellWidthZ := 8;
  CellHeightZ := 16;
  Page.CrsrAttr := 7 or A_CURSOR_SIZE_THICK;
  Page.PageAttr := 0;

  PageTop := 0;
  PageLeft := 0;
  PageZoom := 1;
  WindowCols := (pbPage.Width div CellWidth) + 1;
  WindowRows := (pbPage.Height div CellHeight) + 1;

  // blink states
  BlinkSlow := false;
  BlinkFast := false;

  CursorRow := 0;
  CursorCol := 0;
  CurrChar := $0040;
  CurrAttr := $0007;

  ToolMode := tmSelect;
  DrawMode := dmChars;
  tbToolSelect.Down := true;
  tbModeCharacter.Down := true;
  SubXSize := 1;
  SubYSize := 1;

  LoadSettings;
  NewFile;

  for cp := encCP437 to encUTF16 do
  begin
    // interigate for drawmodes
    CPages[cp].CanDrawMode[dmChars] := true;
    CPages[cp].CanDrawMode[dmLeftRights] := true;
    CPages[cp].CanDrawMode[dmTopBottoms] := true;
    CPages[cp].CanDrawMode[dmQuarters] := true;
    CPages[cp].CanDrawMode[dmSixels] := false;

    // build glyph luts
    if (cp <> encUTF8) and (cp <> encUTF16) then
    begin
      for i := 0 to 255 do
      begin
        gt := CPages[cp].GlyphTable;
        gts := CPages[cp].GlyphTableSize;
        enc := CPages[cp].EncodingLUT[i];
        CPages[cp].QuickGlyph[i] := GetGlyphOff(enc, gt, gts);
      end;

      CPages[cp].CanDrawMode[dmLeftRights] :=
        HasUnicodes(cp, [
          GFX_HALFLEFT, GFX_HALFRIGHT, GFX_BLOCK ]);

      CPages[cp].CanDrawMode[dmTopBottoms] :=
        HasUnicodes(cp, [
          GFX_HALFTOP, GFX_HALFBOTTOM, GFX_BLOCK ]);

      CPages[cp].CanDrawMode[dmQuarters] :=
        HasUnicodes(cp, [
          GFX_HALFLEFT, GFX_HALFRIGHT, GFX_HALFTOP, GFX_HALFBOTTOM,
          GFX_BLOCK, GFX_QUARTER4, GFX_QUARTER8, GFX_QUARTER1,
          GFX_QUARTER13, GFX_QUARTER9, GFX_QUARTER7, GFX_QUARTER11,
          GFX_QUARTER2, GFX_QUARTER6, GFX_QUARTER14 ]);

//      if cp = encTELETEXT1 then CPages[cp].CanDrawMode[dmSixels] := true;
    end;
  end;

  // set default fonts
  for i := 0 to 15 do
    Fonts[0] := encCP437;
//  Fonts[10] := encTeletext;
//  Fonts[11] := encTeletextBlock;
//  Fonts[12] := encTeletextSeparated;

  CurrFont := 0;
  CurrCodePage := Fonts[CurrFont];
  cbCodePage.ItemIndex := ord(CurrCodePage);
  CodePageChange;
  tbFont0.Down := true;
  BuildCharacterPalette;
  seCharacter.Value := CurrChar;

  PageType := PAGETYPE_BBS;
  cbPageType.ItemIndex := PageType;
  cbPageTypeChange(cbPageType);

  ColorScheme := COLORSCHEME_BBS;
  cbColorScheme.ItemIndex := ColorScheme;
  cbColorSchemeChange(cbColorScheme);
  pbColors.Invalidate;

  CurrAttr := $0007;
  pbCurrCell.Invalidate;

  // create the page object
  SelectedObject := -1;
  setlength(Objects, 0);
  LoadlvObjects;

  // clear the clipboard
  Clipboard.Width := 0;
  Clipboard.Height := 0;
  setlength(Clipboard.Data, 0);

end;

procedure TfMain.LoadlvObjects;
var
  i, l : integer;
  str : string;
begin
  // rebuild listview with objects
  lvObjects.Items.Clear;
  l := length(Objects) - 1;
  for i := 0 to l do
  begin
    str :=
      iif(Objects[i].Locked, '1','0') +
      iif(Objects[i].Hidden, '1','0') +
      Objects[i].Name;
    lvObjects.AddItem(str, nil);
  end;
  lvObjects.Invalidate;
end;

procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveSettings;
  fPreviewBox.Hide;
  bmpPage.Free;
  bmpPreview.Free;
  fPreviewBox.Free;
  if bmpCharPalette <> nil then bmpCharPalette.Free;
  setlength(CopySelection, 0);
end;

// create new bmpPage of page at zoom 1
procedure TfMain.GenerateBmpPage;
var
  row, col, x, y : integer;
  ch : integer;
  cp : TEncoding;
  attr : Uint32;
  off : integer;
  bmp : TBGRABitmap;
  w, h : integer;
begin
  // create new
  w := NumCols * CellWidth;
  h := NumRows * CellHeight;
  if (w = 0) or (h = 0) then exit;

  if bmpPage <> nil then
  begin
    if (bmpPage.Width <> w) or (bmpPage.Height <> h) then
    begin
      bmpPage.Free;
      bmpPage := TBGRABitmap.Create(w, h);
    end
    else
      bmpPage.FillRect(0,0,w,h,clBlack);
  end
  else
    bmpPage := TBGRABitmap.Create(w, h);

  if bmpPreview <> nil then
    bmpPreview.Free;

  bmp := TBGRABitmap.Create(CellWidth, CellHeight);
  y := 0;
  for row := 0 to NumRows - 1 do
  begin
    x := 0;
    for col := 0 to NumCols - 1 do
    begin

      ch := Page.Rows[row].Cells[col].Chr;
      attr := Page.Rows[row].Cells[col].Attr;

      // get codepage for this ch
      cp := Fonts[GetBits(attr, A_CELL_FONT_MASK, 28)];
      if (cp = encUTF8) or (cp = encUTF16) then
        off := GetGlyphOff(ch, CPages[cp].GlyphTable, CPages[cp].GlyphTableSize)
      else
      begin
        if ch > 255 then ch := 0;
        off := CPages[cp].QuickGlyph[ch];
      end;

      GetGlyphBmp(bmp, CPages[CurrCodePage].GlyphTable, off, attr, false);
      bmp.Draw(bmpPage.Canvas, x, y);

      x += CellWidth;
    end;

    y += CellHeight;
  end;
  bmp.free;

  bmpPage.ResampleFilter := rfMitchell;
  bmpPreview := bmpPage.Resample(bmpPage.Width>>2, bmpPage.Height>>2) as TBGRABitmap;

  UpdatePreview;

end;

// find unicode char of character base on chars encoding.
function TfMain.GetUnicode(cell : TCell) : integer;
var
  cp : TEncoding;
begin
  // convert chr to unicode
  result := cell.chr;
  cp := Fonts[GetBits(cell.attr, A_CELL_FONT_MASK, 28)];
  if (cp <> encUTF8) and (cp <> encUTF16) then
    result := CPages[cp].EncodingLUT[result];
end;

// find the best match for encoding of unicode char
function TfMain.GetCPChar(cp : TEncoding; unicode : integer) : integer;
var
  i : integer;
begin
  if (cp = encUTF8) or (cp = encUTF16) then
    result := unicode
  else
  begin
    // need to search
    for i := 255 downto 0 do
      if unicode = CPages[cp].EncodingLUT[i] then
        break;
    result := i;
  end;
end;

// get array of colors
function TfMain.GetColors2x1(cell : TCell) : TColors2;
var
  uni, i : integer;
  fg, bg : byte;
begin
  // convert chr to unicode
  uni := GetUnicode(cell);
  if uni = $A0 then uni := $20;
  fg := GetBits(cell.attr, A_CELL_FG_MASK);
  bg := GetBits(cell.attr, A_CELL_BG_MASK, 8);
  for i := 3 downto 0 do
    if uni = Blocks2x1[i] then
      break;

  FillMemory(@result, 2, bg);
  if HasBits(i, %01) then result[0] := fg;
  if HasBits(i, %10) then result[1] := fg;
end;

// get array of colors
function TfMain.GetColors1x2(cell : TCell) : TColors2;
var
  uni, i : integer;
  fg, bg : byte;
begin
  // convert chr to unicode
  uni := GetUnicode(cell);
  if uni = $A0 then uni := $20;
  fg := GetBits(cell.attr, A_CELL_FG_MASK);
  bg := GetBits(cell.attr, A_CELL_BG_MASK, 8);
  for i := 3 downto 0 do
    if uni = Blocks1x2[i] then
      break;

  FillMemory(@result, 2, bg);
  if HasBits(i, %01) then result[0] := fg;
  if HasBits(i, %10) then result[1] := fg;
end;

function TfMain.GetColors2x2(cell : TCell) : TColors4;
var
  uni, i : integer;
  fg, bg : byte;
begin
  // convert chr to unicode
  uni := GetUnicode(cell);
  if uni = $A0 then uni := $20;
  fg := GetBits(cell.attr, A_CELL_FG_MASK);
  bg := GetBits(cell.attr, A_CELL_BG_MASK, 8);
  for i := 15 downto 0 do
    if uni = Blocks2x2[i] then
      break;

  FillMemory(@result, 4, bg);
  if HasBits(i, %0001) then result[0] := fg;
  if HasBits(i, %0010) then result[1] := fg;
  if HasBits(i, %0100) then result[2] := fg;
  if HasBits(i, %1000) then result[3] := fg;
end;

// return color of a subblock
function TfMain.GetBlockColor(
  cell :        TCell;              // the character we are looking for block in
  xsize, ysize,                     // subblock size
  x, y :        integer) : integer; // coors to get in char.
begin
  if (xsize = 2) and (ysize = 1) then
    result := GetColors2x1(cell)[x]
  else if (xsize = 1) and (ysize = 2) then
    result := GetColors1x2(cell)[y]
  else if (xsize = 2) and (ysize = 2) then
    result := GetColors2x2(cell)[x + (y << 1)];
end;

// compute new subblock
function TfMain.SetBlockColor(
  clr:          integer;            // color
  cell :        TCell;              // char in
  xsize, ysize,                     // size of subblocks
  x, y :        integer) : TCell;   // coords
var
  c2 :      TColors2;
  c4 :      TColors4;
  cp :      TEncoding;
  fg, bg :  integer;
  g, i, tot :   integer;
  count :   array of byte;
  mval, mclr : integer;
begin
  // clr at x, y manditory.
  cp := Fonts[GetBits(cell.attr, A_CELL_FONT_MASK, 28)];

  // leftrights
  if (xsize = 2) and (ysize = 1) then
  begin
    c2 := GetColors2x1(cell);
    c2[x] := clr;
    if c2[0] = c2[1] then
    begin
      // same colors
      SetBits(cell.Attr, A_CELL_FG_MASK, c2[0]);
      cell.Chr  := GetCPChar(cp, Blocks2x1[3]);
    end
    else
    begin
      // separate colors - set this block as FG.
      fg := c2[x];
      bg := c2[(x + 1) and 1];

      // truncate BG to max if needed
      if ColorScheme = COLORSCHEME_BBS then
      begin
        // fix for mismatched number of fg and bg colors available
        if (fg < 8) and (bg > 7) then
        begin
          // need to swap.
          fg := fg and $7;
          SetBits(cell.Attr, A_CELL_FG_MASK, bg);
          SetBits(cell.Attr, A_CELL_BG_MASK, fg, 8);
          cell.Chr := GetCPChar(cp, Blocks2x1[2 - x]);
        end
        else
        begin
          bg := bg and $7;
          SetBits(cell.Attr, A_CELL_FG_MASK, fg);
          SetBits(cell.Attr, A_CELL_BG_MASK, bg, 8);
          cell.Chr := GetCPChar(cp, Blocks2x1[x + 1]);
        end;
      end
      else
      begin
        SetBits(cell.Attr, A_CELL_FG_MASK, fg);
        SetBits(cell.Attr, A_CELL_BG_MASK, bg, 8);
        cell.Chr := GetCPChar(cp, Blocks2x1[x + 1]);
      end;
    end;
  end

  // TopBottoms
  else if (xsize = 1) and (ysize = 2) then
  begin
    c2 := GetColors1x2(cell);
    c2[y] := clr;
    if c2[0] = c2[1] then
    begin
      // same colors
      SetBits(cell.Attr, A_CELL_FG_MASK, c2[0]);
      cell.Chr  := GetCPChar(cp, Blocks1x2[3]);
    end
    else
    begin
      // separate colors - set this block as FG.
      fg := c2[y];
      bg := c2[(y + 1) and 1];

      // truncate BG to max if needed
      // truncate BG to max if needed
      if ColorScheme = COLORSCHEME_BBS then
      begin
        // fix for mismatched number of fg and bg colors available
        if (fg < 8) and (bg > 7) then
        begin
          // need to swap.
          fg := fg and $7;
          SetBits(cell.Attr, A_CELL_FG_MASK, bg);
          SetBits(cell.Attr, A_CELL_BG_MASK, fg, 8);
          cell.Chr := GetCPChar(cp, Blocks1x2[2 - y]);
        end
        else
        begin
          bg := bg and $7;
          SetBits(cell.Attr, A_CELL_FG_MASK, fg);
          SetBits(cell.Attr, A_CELL_BG_MASK, bg, 8);
          cell.Chr := GetCPChar(cp, Blocks1x2[y + 1]);
        end;
      end
      else
      begin
        SetBits(cell.Attr, A_CELL_FG_MASK, fg);
        SetBits(cell.Attr, A_CELL_BG_MASK, bg, 8);
        cell.Chr := GetCPChar(cp, Blocks1x2[y + 1]);
      end;
    end;
  end

  // quarters
  else if (xsize = 2) and (ysize = 2) then
  begin
    // need to monkey with this! manditory colors,etc
    c4 := GetColors2x2(cell);
    c4[x + (y << 1)] := clr;

    // reduce other 3 blocks to 1 color + this color
    // get color count.
    setlength(count, 256);
    FillMemory(@count[0], 256, 0);
    tot := 0;
    mval := 0;
    mclr := -1;     // other color with highest count
    for i := 0 to 3 do
    begin
      inc(count[c4[i]]);
      if count[c4[i]] = 1 then
        inc(tot);
      if (c4[i] <> clr) and (count[c4[i]] > mval) then
      begin
        mval := count[c4[i]];
        mclr := c4[i];
      end;
    end;
    if tot > 2 then
    begin
      // reduce other 3 blocks to 1 color + this color
      for i := 0 to 3 do
        if ((c4[i] <> clr) and
            (c4[i] <> mclr)) then
          c4[i] := mclr;
    end;
    if mclr = -1 then mclr := clr;

    // convert c4 to cell
    g := 0;
    for i := 0 to 3 do
      if c4[i] = clr then
        g := g or (1 << i);

    SetBits(cell.Attr, A_CELL_FG_MASK, clr);
    SetBits(cell.Attr, A_CELL_BG_MASK, mclr, 8);
    cell.Chr := GetCPChar(cp, Blocks2x2[g]);
  end;
  result := cell;
end;


{-----------------------------------------------------------------------------}

{ Non Control stuffs }

function TfMain.HasUnicodes(cp : TEncoding; chars : array of UInt16) : boolean;
var
  i, j : integer;
  found : boolean;
begin
  Result := true;
  for i := 0 to length(chars) -1 do
  begin
    found := false;
    for j := 0 to 255  do
      if CPages[cp].EncodingLUT[j] = chars[i] then
      begin
        found := true;
        break;
      end;
    result := found;
    if not found then
      break;
  end;
end;


procedure TfMain.ScrollToCursor;
begin
  if not SkipScroll then
  begin
    if CursorCol < PageLeft then
    begin
      sbHorz.Position := CursorCol;
      ResizeScrolls;
    end
    else if CursorCol >= PageLeft + WindowCols then
    begin
      sbHorz.Position := CursorCol - WindowCols + 1;
      ResizeScrolls;
    end;

    if CursorRow < PageTop then
    begin
      sbVert.Position := CursorRow;
      ResizeScrolls;
    end
    else if CursorRow >= PageTop + WindowRows then
    begin
      sbVert.Position := CursorRow - WindowRows + 1;
      ResizeScrolls;
    end;
  end;
  CursorStatus;
end;

procedure TfMain.CursorRight;
var
  v0, v1 : integer;
begin
  v0 := CursorRow;
  v1 := CursorCol;
  inc(CursorCol);
  if CursorCol >= NumCols then
  begin
    CursorCol := 0;
    Inc(CursorRow);
    if CursorRow >= NumRows then
    begin
      dec(CursorRow);
      CursorCol := NumCols - 1;
    end;
  end;
  DrawCell(v0, v1);
  DrawCell(CursorRow, CursorCol);
  ScrollToCursor; // keep cursor on screen if typing
end;

procedure TfMain.CursorLeft;
var
  v0, v1 : integer;
begin
  v0 := CursorRow;
  v1 := CursorCol;
  dec(CursorCol);
  if CursorCol < 0 then
  begin
    CursorCol := NumCols - 1;
    dec(CursorRow);
    if CursorRow < 0 then
    begin
      inc(CursorRow);
      CursorCol := 0;
    end;
  end;
  DrawCell(v0, v1);
  DrawCell(CursorRow, CursorCol);
  ScrollToCursor; // keep cursor on screen if typing
end;

procedure TfMain.CursorUp;
var
  v0, v1 : integer;
begin
  v0 := CursorRow;
  v1 := CursorCol;
  dec(CursorRow);
  if CursorRow < 0 then
    inc(CursorRow);
  DrawCell(v0, v1);
  DrawCell(CursorRow, CursorCol);
  ScrollToCursor; // keep cursor on screen if typing
end;

procedure TfMain.CursorDown;
var
  v0, v1 : integer;
begin
  v0 := CursorRow;
  v1 := CursorCol;
  inc(CursorRow);
  if CursorRow >= NumRows then
    dec(CursorRow);
  DrawCell(v0, v1);
  DrawCell(CursorRow, CursorCol);
  ScrollToCursor; // keep cursor on screen if typing
end;

procedure TfMain.CursorNewLine;
var
  v0, v1 : integer;
begin
  v0 := CursorRow;
  v1 := CursorCol;
  inc(CursorRow);
  if CursorRow >= NumRows then
    dec(CursorRow);
  CursorCol := 0;
  DrawCell(v0, v1);
  DrawCell(CursorRow, CursorCol);
  ScrollToCursor; // keep cursor on screen if typing
end;

procedure TfMain.CursorForwardTab;
var
  v0, v1 : integer;
begin
  v0 := CursorRow;
  v1 := CursorCol;
  CursorCol := ((CursorCol >> 3) + 1) << 3;
  if CursorCol >= NumCols then
    CursorCol := NumCols - 1;
  DrawCell(v0, v1);
  DrawCell(CursorRow, CursorCol);
  ScrollToCursor; // keep cursor on screen if typing
end;

procedure TfMain.CursorBackwardTab;
var
  v0, v1 : integer;
begin
  v0 := CursorRow;
  v1 := CursorCol;
  CursorCol := (((CursorCol - 1) >> 3)) << 3;
  if CursorCol < 0 then
    CursorCol := 0;
  DrawCell(v0, v1);
  DrawCell(CursorRow, CursorCol);
  ScrollToCursor; // keep cursor on screen if typing
end;

procedure TfMain.CursorMove(row, col : integer);
var
  v0, v1 : integer;
begin
  v0 := CursorRow;
  v1 := CursorCol;
  CursorRow := row;
  CursorCol := col;
  DrawCell(v0, v1);
  DrawCell(CursorRow, CursorCol);
  ScrollToCursor; // keep cursor on screen if typing
end;

{-----------------------------------------------------------------------------}

{ Keyboard stuff }

procedure TfMain.CursorStatus;
begin
  pbStatusBar.Invalidate;
end;

procedure TfMain.seXScaleChange(Sender: TObject);
begin
  XScale := seXScale.Value;
  if floor(CellWidth * PageZoom * XScale) = 0 then
    PageZoom *= 2;
  CellWidthZ := floor(CellWidth * PageZoom * XScale);
  CellHeightZ := floor(CellHeight * PageZoom);
  ResizeScrolls;
  UpdatePreview;
end;

procedure CopyObject(objin : TObj; var objout : TObj);
var
  i, l : integer;
begin
  objout.Name:= objin.Name;
  objout.Row := objin.Row;
  objout.Col := objin.Col;
  objout.Width := objin.Width;
  objout.Height := objin.Height;
  objout.Page := objin.Page;
  objout.Locked := objin.Locked;
  objout.Hidden:= objin.Hidden;
  l := length(objin.Data);
  setlength(objout.Data, l);
  for i := 0 to l - 1 do
    objout.Data[i] := objin.Data[i];
end;

// for special keys
procedure TfMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i, l, ch : integer;
  r, c :        integer;
  shft :        TShiftState;
  KeyAction :   integer;
  KeyValue :    string;
  tmp :         string;
  unk :         boolean;
  found :       boolean;
begin

  // let system handle these controls
  if seRows.Focused
    or seCols.Focused
    or seXScale.Focused
    or seCharacter.Focused then
    exit;

  if (Key = VK_SHIFT) or (Key = VK_CONTROL) or (Key = VK_MENU) then exit;

  // find action / val
  found := false;
  for i := length(KeyBinds) - 1 downto 0 do
  begin
    shft := [];
    if KeyBinds[i].Shift then shft := shft + [ssShift];
    if KeyBinds[i].Ctrl then shft := shft + [ssCtrl];
    if KeyBinds[i].Alt then shft := shft + [ssAlt];
    if (Key = KeyBinds[i].KeyCode) and (Shift = shft) then
    begin
      found := true;
      KeyAction := KeyBinds[i].Action;
      KeyValue := KeyBinds[i].Val;
      break;
    end;
  end;
  if not found then exit;

  unk := false;
  case KeyAction of
    KA_CURSORUP:      CursorUp;
    KA_CURSORDOWN:    CursorDown;
    KA_CURSORLEFT:    CursorLeft;
    KA_CURSORRIGHT:   CursorRight;

    KA_NEXTFG:
      begin
        // next fg
        i := GetBits(CurrAttr, A_CELL_FG_MASK) + 1;
        case ColorScheme of
          COLORSCHEME_BASIC:  i := i and 7;
          COLORSCHEME_BBS,
          COLORSCHEME_ICE:    i := i and 15;
          COLORSCHEME_256:    i := i and 255;
        end;
        SetBits(CurrAttr, A_CELL_FG_MASK, i);
        pbColors.Invalidate;
        pbCurrCell.Invalidate;
      end;

    KA_PREVFG:
      begin
        // prev fg
        i := GetBits(CurrAttr, A_CELL_FG_MASK) - 1;
        case ColorScheme of
          COLORSCHEME_BASIC:    i := i and 7;
          COLORSCHEME_BBS,
          COLORSCHEME_ICE:      i := i and 15;
          COLORSCHEME_256:      i := i and 255;
        end;
        SetBits(CurrAttr, A_CELL_FG_MASK, i);
        pbColors.Invalidate;
        pbCurrCell.Invalidate;
      end;

    KA_NEXTBG:
      begin
        // next bg
        i := GetBits(CurrAttr, A_CELL_BG_MASK, 8) + 1;
        case ColorScheme of
          COLORSCHEME_BASIC,
          COLORSCHEME_BBS:      i := i and 7;
          COLORSCHEME_ICE:      i := i and 15;
          COLORSCHEME_256:      i := i and 255;
        end;
        SetBits(CurrAttr, A_CELL_BG_MASK, i, 8);
        pbColors.Invalidate;
        pbCurrCell.Invalidate;
      end;

    KA_PREVBG:
      begin
        // next bg
        i := GetBits(CurrAttr, A_CELL_BG_MASK, 8) - 1;
        case ColorScheme of
          COLORSCHEME_BASIC,
          COLORSCHEME_BBS:      i := i and 7;
          COLORSCHEME_ICE:      i := i and 15;
          COLORSCHEME_256:      i := i and 255;
        end;
        SetBits(CurrAttr, A_CELL_BG_MASK, i, 8);
        pbColors.Invalidate;
        pbCurrCell.Invalidate;
      end;

    KA_CURSORNEWLINE:       CursorNewLine;
    KA_CURSORFORWARDTAB:    CursorForwardTab;
    KA_CURSORBACKWARDTAB:   CursorBackwardTab;

    KA_CURSORBACK:
      begin
        if CursorCol > 0 then
          CursorLeft;
      end;

    KA_FKEYSET:
      begin
        i := 0;
        if isInteger(KeyValue) then i := strtoint(KeyValue) - 1;
        if not between(i, 0, 9) then i := 0;
        CurrFKeySet := i;
        pbStatusBar.Invalidate;
      end;

    KA_PRINT:
      begin
        // if @val then built in variable
        // if $nnnn then hex char
        // else rest is string . putchar with currattr
        KeyValue := KeyValue.Replace('\@', #0); // holding place for @
        for i := 0 to 9 do
        begin
          tmp := '@FKey' + inttostr(i + 1) + '@';
          if KeyValue.IndexOf(tmp) <> -1 then
          begin
            ch := FKeys[CurrFKeySet][i];
            if CurrCodePage in [encUTF8, encUTF16 ] then
              ch := CPages[CurrCodePage].EncodingLUT[ch];

            KeyValue := KeyValue.Replace(tmp, char(ch));
          end;
        end;
        if KeyValue.IndexOf('@CurrChar@') <> -1 then
          KeyValue := KeyValue.Replace('@CurrChar@', char(CurrChar));
        KeyValue := KeyValue.Replace(#0, '@');

        // c stule /'s
        KeyValue := KeyValue.Replace('\n', char(10));
        KeyValue := KeyValue.Replace('\r', char(13));
        i := KeyValue.IndexOf('\x');
        while i <> -1 do
        begin
          ch := hex2dec(KeyValue.Substring(i + 2, 2));
          KeyValue := KeyValue.substring(0, i) + char(ch) + KeyValue.substring(i + 4);
          i := KeyValue.IndexOf('\x');
        end;
        for i := 0 to KeyValue.Length - 1 do
          PutChar(ord(KeyValue.Chars[i]));
        ScrollToCursor;
      end;

    KA_MODECHARS:
      if tbModeCharacter.Enabled then tbModeCharacter.Click;

    KA_MODELEFTRIGHTBLOCKS:
      if tbModeLeftRights.Enabled then tbModeLeftRights.Click;

    KA_MODETOPBOTTOMBLOCKS:
      if tbModeTopBottoms.Enabled then tbModeTopBottoms.Click;

    KA_MODEQUARTERBLOCKS:
      if tbModeQuarters.Enabled then tbModeQuarters.Click;

    KA_MODESIXELS:
      if tbModeSixels.Enabled then tbModeSixels.Click;

    KA_TOOLSELECT:
      if tbToolSelect.Enabled then tbToolSelect.Click;

    KA_TOOLDRAW:
      if tbToolDraw.Enabled then tbToolDraw.Click;

    KA_TOOLFILL:
      if tbToolFill.Enabled then tbToolFill.Click;

    KA_TOOLLINE:
      if tbToolLine.Enabled then tbToolLine.Click;

    KA_TOOLRECTANGLE:
      if tbToolRect.Enabled then tbToolRect.Click;

    KA_TOOLELLIPSE:
      if tbToolEllipse.Enabled then tbToolEllipse.Click;

    KA_TOOLEYEDROPPER:
      if tbToolEyedropper.Enabled then tbToolEyedropper.Click;

    KA_FILENEW:   NewFile;
    KA_FILEOPEN:  miFileOpenClick(miFileOpen);
    KA_FILESAVE:  miFileSaveClick(miFileSave);
    KA_FILEEXIT:  Close;

    KA_EDITREDO:
      ;

    KA_EDITUNDO:
      ;

    KA_EDITCUT:
      // copy selection or object to clipboard, removing the thing
      begin
      if length(CopySelection) > 0 then
        begin
          Clipboard := CopySelectionToObject;
          for i := 0 to length(CopySelection) - 1 do
          begin
            r := CopySelection[i].Row;
            c := CopySelection[i].Col;
            Page.Rows[r].Cells[c] := BLANK;
            DrawCell(CopySelection[i].Row, CopySelection[i].Col, false);
          end;
        end;
      end;

    KA_EDITCOPY:
      // copy selection or object to clipboard.
      begin
      if length(CopySelection) > 0 then
        begin
          Clipboard := CopySelectionToObject;
        end;
      end;

    KA_EDITPASTE:
      // paste clipboard contents as new object.
      begin
        if (Clipboard.Width > 0) and (Clipboard.Height > 0) then
        begin
          // paste as new object.
          l := length(Objects);
          setlength(Objects, l + 1);
          CopyObject(Clipboard, Objects[l]);
          // drop it onto window. top left for now
          Objects[l].Row := PageTop;
          Objects[l].Col := PageLeft;
          LoadlvObjects;
          pbPage.Invalidate;
        end;
      end;

    KA_OBJECTMOVEBACK:      ObjMoveBack;
    KA_OBJECTMOVEFORWARD:   ObjMoveForward;
    KA_OBJECTMOVETOBACK:    ObjMoveToBack;
    KA_OBJECTMOVETOFRONT:   ObjMoveToFront;
    KA_OBJECTFLIPHORZ:      ObjFlipHorz;
    KA_OBJECTFLIPVERT:      ObjFlipVert;
    KA_OBJECTMERGE:         ObjMerge;
    KA_OBJECTNEXT:          ObjNext;
    KA_OBJECTPREV:          ObjPrev;

    KA_DELETE:
      begin
        if ToolMode = tmSelect then
        begin
          if SelectedObject >= 0 then
          begin
            // delete object
            RemoveObject(SelectedObject);
            LoadlvObjects;
            SelectedObject := -1;
            lvObjects.ItemIndex := selectedObject;
            pbPage.Invalidate;
          end
          else if length(CopySelection) > 0 then
          begin
            // delete selected area
            for i := length(CopySelection) - 1 downto 0 do
            begin
              r := CopySelection[i].Row;
              c := CopySelection[i].Col;
              Page.Rows[r].Cells[c] := BlankCell;
              DrawCell(r, c, false);
            end;
            setlength(CopySelection, 0);
            pbPage.Invalidate;
          end;
        end;
      end;

    KA_ESCAPE:
      begin
        if ToolMode = tmSelect then
        begin
          if SelectedObject >= 0 then
          begin
            SelectedObject := -1;
            lvObjects.ItemIndex := selectedObject;
            pbPage.Invalidate;
          end
          else if length(CopySelection) > 0 then
          begin
            setlength(CopySelection, 0);
            pbPage.Invalidate;
          end;
        end;
      end;

    KA_SHOWPREVIEW:
      begin
        case KeyValue of
          '0':  fPreviewBox.Hide;
          '1':  fPreviewBox.Show;
          else
            fPreviewBox.Visible:=not fPreviewBox.Visible;
        end;
      end;

    else
      unk := true;
  end;
  if not unk then
    Key := 0;
end;

// for alphanumerica etc
procedure TfMain.FormKeyPress(Sender: TObject; var Key: char);
begin
  if seRows.Focused
    or seCols.Focused
    or seXScale.Focused
    or seCharacter.Focused then
    exit;

  if Between(Key, ' ', '~') then
  begin
    PutChar(ord(Key));
    ScrollToCursor;
    Key := #0;
  end;
end;

procedure TfMain.PutCharExpand(ch : integer);
begin
  Page.Rows[CursorRow].Cells[CursorCol].Chr := ch;
  Page.Rows[CursorRow].Cells[CursorCol].Attr := CurrAttr;
  DrawCell(CursorRow, CursorCol, false);

  inc(CursorCol);
  if CursorCol >= NumCols then
  begin
    CursorCol := 0;
    Inc(CursorRow);
    if CursorRow >= NumRows then
    begin
      seRows.Value := CursorRow + 1;
      ResizePage;
    end;
  end;
  CurrFileChanged := true;
  UpdateTitles;
end;


procedure TfMain.PutCharEx(ch, cattr, row, col : integer);
var
  attr, mask : UInt32;
begin
//  ScrollToCursor; // keep cursor on screen if typing

  if tbAttrCharacter.Tag = 0 then     Page.Rows[row].Cells[col].Chr := ch;

  // build attribute Mask
  mask := $00000000;
  if tbAttrFG.Tag = 0 then            mask := mask or A_CELL_FG_MASK;
  if tbAttrBG.Tag = 0 then            mask := mask or A_CELL_BG_MASK;

  if tbAttrBold.Tag = 0 then          mask := mask or A_CELL_BOLD;
  if tbAttrFaint.Tag = 0 then         mask := mask or A_CELL_FAINT;
  if tbAttrUnderline.Tag = 0 then     mask := mask or A_CELL_UNDERLINE;
  if tbAttrBlinkSlow.Tag = 0 then     mask := mask or A_CELL_BLINKSLOW;
  if tbAttrBlinkFast.Tag = 0 then     mask := mask or A_CELL_BLINKFAST;
  if tbAttrReverse.Tag = 0 then       mask := mask or A_CELL_REVERSE;
  if tbAttrStrikethrough.Tag = 0 then mask := mask or A_CELL_STRIKETHROUGH;
  if tbAttrDoublestrike.Tag = 0 then  mask := mask or A_CELL_DOUBLESTRIKE;
  if tbAttrShadow.Tag = 0 then        mask := mask or A_CELL_SHADOW;

  if (tbAttrConceal.Tag = 0)
  or (tbAttrTop.Tag = 0)
  or (tbAttrBottom.Tag = 0) then      mask := mask or A_CELL_DISPLAY_MASK;

  attr := Page.Rows[row].Cells[col].Attr;
  attr := attr and (not mask);
  attr := attr or (cattr and mask);
  Page.Rows[row].Cells[col].Attr := attr;
  DrawCell(row, col, false);
  CurrFileChanged := true;
  UpdateTitles;
end;

procedure TfMain.PutChar(ch : integer);
var
  attr, mask : UInt32;
begin
//  ScrollToCursor; // keep cursor on screen if typing

  if tbAttrCharacter.Tag = 0 then     Page.Rows[CursorRow].Cells[CursorCol].Chr := ch;

  // build attribute Mask
  mask := $00000000;
  if tbAttrFG.Tag = 0 then            mask := mask or A_CELL_FG_MASK;
  if tbAttrBG.Tag = 0 then            mask := mask or A_CELL_BG_MASK;

  if tbAttrBold.Tag = 0 then          mask := mask or A_CELL_BOLD;
  if tbAttrFaint.Tag = 0 then         mask := mask or A_CELL_FAINT;
  if tbAttrUnderline.Tag = 0 then     mask := mask or A_CELL_UNDERLINE;
  if tbAttrBlinkSlow.Tag = 0 then     mask := mask or A_CELL_BLINKSLOW;
  if tbAttrBlinkFast.Tag = 0 then     mask := mask or A_CELL_BLINKFAST;
  if tbAttrReverse.Tag = 0 then       mask := mask or A_CELL_REVERSE;
  if tbAttrStrikethrough.Tag = 0 then mask := mask or A_CELL_STRIKETHROUGH;
  if tbAttrDoublestrike.Tag = 0 then  mask := mask or A_CELL_DOUBLESTRIKE;
  if tbAttrShadow.Tag = 0 then        mask := mask or A_CELL_SHADOW;

  if (tbAttrConceal.Tag = 0)
  or (tbAttrTop.Tag = 0)
  or (tbAttrBottom.Tag = 0) then      mask := mask or A_CELL_DISPLAY_MASK;

  attr := Page.Rows[CursorRow].Cells[CursorCol].Attr;
  attr := attr and (not mask);
  attr := attr or (CurrAttr and mask);
  Page.Rows[CursorRow].Cells[CursorCol].Attr := attr;
//  Page.Rows[CursorRow].Cells[CursorCol].Attr := CurrAttr;

  DrawCell(CursorRow, CursorCol, false);
  CursorRight;
  CurrFileChanged := true;
  UpdateTitles;
end;

{-----------------------------------------------------------------------------}

{ Control events }

procedure TfMain.CodePageChange;
begin
// CODEPAGE
  if cbCodePage.Enabled then
  begin
    Fonts[0] := TEncoding(cbCodePage.ItemIndex);
    CurrCodePage := Fonts[CurrFont];
    tbCodePage.Text := CPages[Fonts[CurrFont]].Name;

    // rebuild page
    GenerateBmpPage;
    pbPage.Invalidate;
    BuildCharacterPalette;

    // enable / disable Modes
    tbModeLeftRights.Enabled := CPages[CurrCodePage].CanDrawMode[dmLeftRights];
    tbModeTopBottoms.Enabled := CPages[CurrCodePage].CanDrawMode[dmTopBottoms];
    tbModeQuarters.Enabled := CPages[CurrCodePage].CanDrawMode[dmQuarters];
    tbModeSixels.Enabled := CPages[CurrCodePage].CanDrawMode[dmSixels];
  end;
end;

procedure TfMain.cbCodePageChange(Sender: TObject);
begin
  CodePageChange;
end;

procedure TfMain.cbColorSchemeChange(Sender: TObject);
var
  r, c : integer;
  sc : integer; // 0=basic,1=bbs,2=ice,3=all
  attr : Uint32;
begin
  // change color scheme.
  // alter page contents to conform.
  sc := TComboBox(Sender).ItemIndex;
  ColorScheme := sc;
  for r := 0 to NumRows - 1 do
    for c := 0 to NumCols - 1 do
    begin
      attr := Page.Rows[r].Cells[c].Attr;
      case sc of
        0: // BASIC : only colors 0-7 FB/BG
          begin
            SetBits(attr, A_CELL_FG_MASK, GetBits(attr, A_CELL_FG_MASK) and $0007);
            SetBits(attr, A_CELL_BG_MASK, GetBits(attr, A_CELL_BG_MASK) and $0700);
          end;

        1: // BBS : only colors 0-15;FG, 0-7 BG
          begin
            SetBits(attr, A_CELL_FG_MASK, GetBits(attr, A_CELL_FG_MASK) and $000F);
            SetBits(attr, A_CELL_BG_MASK, GetBits(attr, A_CELL_BG_MASK) and $0700);
          end;

        2: // iCE : only color 0-15: FG/BG
          begin
            SetBits(attr, A_CELL_FG_MASK, GetBits(attr, A_CELL_FG_MASK) and $000F);
            SetBits(attr, A_CELL_BG_MASK, GetBits(attr, A_CELL_BG_MASK) and $0F00);
          end;
      end;
      Page.Rows[r].Cells[c].Attr := attr;
      DrawCell(r, c, false);
    end;
  pbPage.Invalidate;

  pbColors.Invalidate;
end;

procedure TfMain.cbPageTypeChange(Sender: TObject);
var
  cb : TComboBox;
begin
  // enable / disable control based on mode
  cb := TComboBox(Sender);
  PageType := cb.ItemIndex;
  case PageType of
    PAGETYPE_BBS, PAGETYPE_CTERM: // BBS / CTerm
      begin
        tbAttrBold.Enabled := false;
        tbAttrFaint.Enabled := false;
        tbAttrItalics.Enabled := false;
        tbAttrUnderline.Enabled := false;
        tbAttrBlinkSlow.Enabled := false;
        tbAttrStrikethrough.Enabled := false;
        tbAttrDoublestrike.Enabled := false;
        tbAttrShadow.Enabled := false;
        tbAttrTop.Enabled := false;
        tbAttrBottom.Enabled := false;
      end;

    PAGETYPE_VTX: // VTX
      begin
        tbAttrBold.Enabled := true;
        tbAttrFaint.Enabled := true;
        tbAttrItalics.Enabled := true;
        tbAttrUnderline.Enabled := true;
        tbAttrBlinkSlow.Enabled := true;
        tbAttrStrikethrough.Enabled := true;
        tbAttrDoublestrike.Enabled := true;
        tbAttrShadow.Enabled := true;
        tbAttrTop.Enabled := true;
        tbAttrBottom.Enabled := true;
      end;
  end;

  SetBits(CurrAttr, A_CELL_BOLD or A_CELL_FAINT or A_CELL_ITALICS
      or A_CELL_UNDERLINE or A_CELL_BLINKSLOW or A_CELL_STRIKETHROUGH
      or A_CELL_DOUBLESTRIKE or A_CELL_SHADOW or A_CELL_DISPLAY_TOP
      or A_CELL_DISPLAY_BOTTOM, 0);
  pbCurrCell.Invalidate;
end;

procedure TfMain.ResizeScrolls;
begin
  // get width of page panel
  WindowCols := (pbPage.Width div CellWidthZ) + 1;
  if WindowCols >= NumCols then
  begin
    sbHorz.Enabled:=false;
    PageLeft := 0;
  end
  else
  begin
    PageLeft := sbHorz.Position;
    sbHorz.Enabled:=true;
    sbHorz.Min := 0;
    sbHorz.Max := NumCols - 1;
    sbHorz.PageSize := WindowCols;
  end;

  WindowRows := (pbPage.Height div CellHeightZ) + 1;
  if WindowRows >= NumRows then
  begin
    sbVert.Enabled:=false;
    PageTop := 0;
  end
  else
  begin
    PageTop := sbVert.Position;
    sbVert.Enabled:=true;
    sbVert.Min := 0;
    sbVert.Max := NumRows - 1;
    sbVert.PageSize := WindowRows;
  end;

  pbRulerLeft.Invalidate;
  pbRulerTop.Invalidate;
  pbPage.Invalidate;
end;


procedure TfMain.sbHorzChange(Sender: TObject);
begin
  ResizeScrolls;
end;

procedure TfMain.pbStatusBarPaint(Sender: TObject);
var
  pb:TPaintBox;
  cnv:TCanvas;
  r : TRect;
  ch, i, u, off : integer;
  bmp : tbgrabitmap;
  str : unicodestring;
  style : TTextStyle;
begin
  // background.
  pb:=TPaintBox(Sender);
  cnv:=pb.Canvas;
  r := pb.ClientRect;

  style.Layout := tlCenter;
  style.Alignment:= taLeftJustify;

  cnv.Font.Size := -11;
  r.left += 6;
  cnv.TextRect(r, r.left, 0, Format('Cursor: R:%0.3d C:%0.3d', [ CursorRow + 1, CursorCol + 1 ]), style);
  r.left += 128;

  if between(MouseRow, 0, NumRows - 1) and between(MouseCol, 0, NumCols - 1) then
    cnv.TextRect(r, r.left, 0, Format('Mouse: R:%0.3d C:%0.3d', [ MouseRow + 1, MouseCol + 1 ]), style);
  r.left += 128;

  // draw fkeys
  str := '[' + IntToStr(CurrFKeySet+1) + ']';
  cnv.TextRect(r, r.left, 0, str, style);
  r.left += cnv.TextWidth(str) + 4;

  bmp := TBGRABitmap.Create(8,16);
  for i := 0 to 9 do
  begin
    str := 'F' + IntToStr(i+1) + ':';
    cnv.TextRect(r, r.left, r.top, str, style);
    r.left += cnv.TextWidth(str) + 2;
    ch := FKeys[CurrFKeySet][i];  // 437 char
    u := CP437[ch];               // unicode char
    off := GetGlyphOff(u, CPages[CurrCodePage].GlyphTable, CPages[CurrCodePage].GlyphTableSize);
    GetGlyphBmp(bmp, CPages[CurrCodePage].GlyphTable, off, $0007, false);
    bmp.Draw(cnv, r.left, 4);
    r.left += 12;
  end;
  bmp.Free();

end;

procedure TfMain.sbVertChange(Sender: TObject);
begin
  ResizeScrolls;
end;


procedure TfMain.FormResize(Sender: TObject);
begin
  ResizeScrolls;
end;


{-----------------------------------------------------------------------------}

{ Menu Routines }

procedure TfMain.NewFile;
var
  r, c : integer;
begin
  // truncate page
  for r := 0 to length(Page.Rows) - 1 do
  begin
    if r < NumRows then
      setlength(Page.Rows[r].Cells, NumCols)
    else
      setlength(Page.Rows[r].Cells, 0);
  end;
  for r := length(Page.Rows) to NumRows - 1 do
  begin
    setlength(Page.Rows, length(Page.Rows) + 1);
    if r < NumRows then
      setlength(Page.Rows[r].Cells, NumCols)
    else
      setlength(Page.Rows[r].Cells, 0);
  end;
  setlength(Page.Rows, NumRows);
  for r := 0 to NumRows - 1 do
    for c := 0 to NumCols - 1 do
      Page.Rows[r].Cells[c] := BlankCell;
  FillByte(Page.Sauce, sizeof(TSauceHeader), 0);
  CurrFileName := 'Untitled.vtx';
  CurrFileChanged := false;
  tbSauceAuthor.Text:='';
  tbSauceDate.Text:='';
  tbSauceGroup.Text:='';
  tbSauceTitle.Text:='';
  ResizePage;
  GenerateBmpPage;
  UpdateTitles;

  setlength(CopySelection, 0);
  pbPage.Invalidate();
end;

procedure TfMain.miFileNewClick(Sender: TObject);
begin
  NewFile;
end;

procedure TfMain.LoadVTXFile(fname : string);
var
  fin : TFileStream;
  head : TVTXFileHeader;
  i, r, c : integer;
const
  ID = 'VTXEDIT';
begin
  fin := TFileStream.Create(fname, fmOpenRead or fmShareDenyNone);
  fin.Read(head, sizeof(TVTXFileHeader));

  if CompareByte(ID, head.ID, 7) <> 0 then
  begin
    ShowMessage('Bad Header.');
    NewFile;
    exit;
  end;
  if head.Version <> $0001 then
  begin
    ShowMessage('Bad Version.');
    NewFile;
    exit;
  end;

  PageType := head.PageType;
  for i := 0 to 15 do
    Fonts[i] := TEncoding(head.Fonts[i]);
  ColorScheme := head.Colors;
  NumRows := head.NumRows;
  NumCols := head.NumCols;
  XScale := head.XScale;
  Page.PageAttr := head.PageAttr;
  Page.CrsrAttr := head.CrsrAttr;
  move(head.Sauce, Page.Sauce, sizeof(TSauceHeader));

  if length(Page.Rows) < NumRows then
    setlength(Page.Rows, NumRows);
  for r := 0 to NumRows - 1 do
  begin
    // row attributes here
    fin.Read(Page.Rows[r].Attr, sizeof (Page.Rows[r].Attr));
    if length(Page.Rows[r].Cells) < NumCols then
      setlength(Page.Rows[r].Cells, NumCols);

    for c := 0 to NumCols - 1 do
    begin
      // cells here
      fin.Read(Page.Rows[r].Cells[c], sizeof(TCell));
    end;
  end;

  fin.free;
end;

procedure TfMain.SaveVTXFile(fname : string);
var
  fout : TFileStream;
  head : TVTXFileHeader;
  i, r, c : integer;

const
  ID = 'VTXEDIT';
begin
  fout := TFileStream.Create(fname, fmCreate or fmOpenWrite or fmShareDenyNone);
  move(ID, head.ID, 7);
  head.ID[7] := 0;
  head.Version := $0001;
  head.PageType := PageType;
  for i := 0 to 15 do
    head.Fonts[i] := word(ord(Fonts[i]));
  head.Colors := ColorScheme;
  head.NumRows := NumRows;
  head.NumCols := NumCols;

  // page / cursor attr here
  head.XScale := XScale;
  head.PageAttr := Page.PageAttr;
  head.CrsrAttr := Page.CrsrAttr;
  move(Page.Sauce, head.Sauce, sizeof(TSauceHeader));
  fout.Write(head, sizeof(TVTXFileHeader));

  for r := 0 to numrows-1 do
  begin
    // row attributes here
    fout.WriteDWord(Page.Rows[r].Attr);

    for c := 0 to numcols-1 do
    begin
      // cells here
      fout.Write(Page.Rows[r].Cells[c], sizeof(TCell));
    end;
  end;
  fout.free;
end;

procedure TfMain.miFileSaveClick(Sender: TObject);
var
  ansi : unicodestring;
begin
  sdAnsi.Filename := CurrFileName;
  if sdAnsi.Execute then
  begin
    // get filter.
    case sdAnsi.FilterIndex of
      1:    // vtxedit file - save EVERYTHING
        begin
          SaveVTXFile(sdAnsi.Filename);
        end;

      else  // everything else
        begin
          ansi := BuildANSI;
          SaveANSIFile(sdAnsi.FileName, ansi, CurrCodePage);
        end;
    end;
    CurrFileName := ExtractFileName(sdAnsi.FileName);
    CurrFileChanged := false;
    UpdateTitles;
  end;
end;

procedure TfMain.BuildCharacterPalette;
var
  rows : integer;
  i : integer;
  x, y : integer;
  off : integer;
  cell : TBGRABitmap;
  rect : TRect;
  NumChars : integer;

const
  PALCOLS = 16;
  CELL_WIDTH = 21;
  CELL_HEIGHT = 40;

begin
  // build palette
  seCharacter.Enabled := false;
  cell := TBGRABitmap.Create(8,16);
  if (CurrCodePage = encUTF8) or (CurrCodePage = encUTF16) then
  begin
    NumChars := math.floor(length(UVGA16) / 18) - 1;
    seCharacter.MinValue := $0020;
    seCharacter.MaxValue := $FFFF;
  end
  else
  begin
    NumChars := 256;
    seCharacter.MinValue := $0000;
    seCharacter.MaxValue := $00FF;
  end;
  if not between(CurrChar, seCharacter.MinValue, seCharacter.MaxValue) then
  begin
    // todo : check invalid if UTF8/16
    CurrChar := 32;
    seCharacter.Value := CurrChar;
    tbUnicode.Text := '32';
  end;
  seCharacter.Enabled := true;

  rows := (NumChars - 1) div PALCOLS + 1;
  if bmpCharPalette <> nil then
    bmpCharPalette.Free;
  bmpCharPalette := TBGRABitmap.Create(PALCOLS * CELL_WIDTH + 4, rows * CELL_HEIGHT + 4);
  bmpCharPalette.FillRect(0,0,bmpCharPalette.Width,bmpCharPalette.Height,clBlack);

  for i := 0 to NumChars - 1 do
  begin

    if CurrCodePage in [ encUTF8, encUTF16 ]then
      off := (i + 1) * 18 + 2
    else
      off := CPages[CurrCodePage].QuickGlyph[i];

    y := i div PALCOLS;
    x := i - (y * PALCOLS);

    x := x * CELL_WIDTH + 2;
    y := y * CELL_HEIGHT + 2;

    // draw simple glyph in cell (8x16)
    GetGlyphBmp(cell, CPages[CurrCodePage].GlyphTable, off, 15, false);

    rect.Left := 2 + x;
    rect.Top := 2 + y;
    rect.Width := 16;
    rect.Height := 32;

    bmpCharPalette.FillRect(2 + x - 1, 2 + y - 1, 4 + x + 17, 2 + y + 33, clDkGray);
    cell.Draw(bmpCharPalette.Canvas, rect);
  end;
  cell.Free;
  pbChars.Invalidate;
end;

procedure TfMain.pbCharsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  off, i : integer;

const
  PALCOLS = 16;
  CELL_WIDTH = 21;
  CELL_HEIGHT = 40;

begin
  // click to select
  y := (y - 4) div CELL_HEIGHT;
  x := (x - 4) div CELL_WIDTH;
  seCharacter.Enabled := false;
  if between(x, 0, 15) and (y >= 0) then
  begin
    i := y * PALCOLS + x;
    if CurrCodePage in [ encUTF8, encUTF16 ] then
    begin
      off := (i + 1) * 18;
      i := (UVGA16[off] << 8) or UVGA16[off+1];
      CurrChar := i;
      seCharacter.value := i;
      tbUnicode.Text := IntToStr(i);
    end
    else
    begin
      CurrChar := i;
      seCharacter.value := i;
      tbUnicode.Text := IntToStr(CPages[CurrCodePage].EncodingLUT[i]);
    end;
  end;
  seCharacter.Enabled := true;
  pbChars.Invalidate;
end;

procedure TfMain.pbCharsPaint(Sender: TObject);
var
  pb : TPaintBox;
  cnv : TCanvas;
  i, x, y : integer;

const
  PALCOLS = 16;
  CELL_WIDTH = 21;
  CELL_HEIGHT = 40;

begin
  pb := TPaintBox(Sender);
  cnv := pb.Canvas;

  pb.Width := bmpCharPalette.Width;
  pb.Height := bmpCharPalette.Height;
  bmpCharPalette.Draw(cnv, 0, 0);

  // hilight the selected char
  if CurrCodePage in [ encUTF8, encUTF16 ] then
    // convert unicode to offset
    i := (GetGlyphOff(
      CurrChar,
      CPages[CurrCodePage].GlyphTable,
      CPages[CurrCodePage].GlyphTableSize) - 2) div 18 - 1
  else
    i := CurrChar;

  y := i div PALCOLS;
  x := i - (y * PALCOLS);
  x := x * CELL_WIDTH + 4;
  y := y * CELL_HEIGHT + 2;

  cnv.Brush.Style := bsClear;
  cnv.Pen.Color := clRed;
  cnv.Pen.Width := 1;
  cnv.Rectangle(x, y, x + 20, y + 36);

  pbCurrCell.Invalidate;
end;

procedure TfMain.pbColorsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  fgs, bgs, cls : integer;
  cl, maxr, r, c : integer;
begin
  // max colors
  case ColorScheme of
    COLORSCHEME_BASIC: begin fgs := 8; bgs := 8; cls := 8; end;
    COLORSCHEME_BBS: begin fgs := 16; bgs := 8; cls := 16; end;
    COLORSCHEME_ICE: begin fgs := 16; bgs := 16; cls := 16; end;
    COLORSCHEME_256: begin fgs := 256; bgs := 256; cls := 256; end;
  end;

  r := Y div 22;
  c := X div 22;
  maxr := cls div 16;  // 16 colors per row

  if not between(r, 0, maxr) or not between(c, 0, 15) then
    exit;

  cl := (r << 4) + c;

  case button of
    mbLeft:
      if between(cl, 0, fgs - 1) then
        SetBits(CurrAttr, A_CELL_FG_MASK, cl);

    mbRight:
      if between(cl, 0, bgs - 1) then
        SetBits(CurrAttr, A_CELL_BG_MASK, cl, 8);
  end;
  pbColors.Invalidate;
  pbCurrCell.Invalidate;
end;

procedure TfMain.pbColorsPaint(Sender: TObject);
var
  pb : TPaintBox;
  cnv : TCanvas;
  cls : integer;
  maxr : integer;
  cl, x, y, r, c : integer;
  rect : TRect;
  bmp : TBitmap;
begin
  pb := TPaintBox(Sender);
  cnv := pb.Canvas;

  // max colors
  case ColorScheme of
    COLORSCHEME_BASIC: begin cls := 8; end;
    COLORSCHEME_BBS: begin cls := 16; end;
    COLORSCHEME_ICE: begin cls := 16; end;
    COLORSCHEME_256: begin cls := 256; end;
  end;

  maxr := cls div 16;  // 16 colors per row
  y := 0;
  for r := 0 to maxr - 1 do
  begin
    x := 0;
    for c := 0 to 15 do
    begin
      rect.left :=   x;
      rect.top :=    y;
      rect.width :=  20;
      rect.height := 20;

      cl := (r << 4) + c;

      cnv.Brush.Color := ANSIColor[cl];
      cnv.FillRect(rect);
      cnv.pen.color := clBlack;
      cnv.Rectangle(rect);

      if cl = GetBits(CurrAttr, A_CELL_FG_MASK) then
      begin
        bmp := TBitmap.create;
        bmp.PixelFormat:=pf32bit;
        ilButtons.GetBitmap(47, bmp);
        cnv.Draw(x + 2, y + 2, bmp);
        bmp.free;
      end;

      if cl = GetBits(CurrAttr, A_CELL_BG_MASK, 8) then
      begin
        bmp := TBitmap.create;
        bmp.PixelFormat:=pf32bit;
        ilButtons.GetBitmap(48, bmp);
        cnv.Draw(x + 2, y + 2, bmp);
        bmp.free;
      end;

      x += 22;
      end;
    y += 22;
  end;

end;

// get next available glyph in uvga
function GetNextUnicodeChar(chr : integer) : integer;
var
  i : integer;
begin
  for i := chr to $FFFF do
    if GetGlyphOff(i, @UVGA16, sizeof(UVGA16)) <> 2 then
      exit (i);
  result := 0;
end;

function GetPrevUnicodeChar(chr : integer) : integer;
var
  i : integer;
begin
  for i := chr downto $0000 do
    if GetGlyphOff(i, @UVGA16, sizeof(UVGA16)) <> 2 then
      exit (i);
  result := 0;
end;

var
  LastCharNum : integer = 0;

procedure TfMain.seCharacterChange(Sender: TObject);
var
  chr : integer;
begin
  if TSpinEdit(sender).enabled then
  begin
    chr := seCharacter.Value;
    if CurrCodePage in [ encUTF8, encUTF16 ] then
    begin
      // code to skip to char
      if GetGlyphOff(chr, @UVGA16, sizeof(UVGA16)) <> 2 then
        CurrChar := chr
      else
      begin
        if LastCharNum > chr then
        begin
          CurrChar := GetPrevUnicodeChar(chr);
          if CurrChar = 0 then
            CurrChar := GetPrevUnicodeChar($FFFF);
        end
        else
        begin
          CurrChar := GetNextUnicodeChar(chr);
          if CurrChar = 0 then
            CurrChar := GetNextUnicodeChar(1);
        end;

        seCharacter.Enabled := false;
        seCharacter.Value := CurrChar;
        seCharacter.Enabled := true;
      end;
      tbUnicode.Text := IntToStr(CurrChar);
    end
    else
    begin
      CurrChar := seCharacter.Value;
      tbUnicode.Text := IntToStr(CPages[CurrCodePage].EncodingLUT[CurrChar]);
    end;
    LastCharNum := CurrChar;
    pbChars.Invalidate;
  end;
end;

procedure TfMain.tbAttrClick(Sender: TObject);
var
  tb : TToolButton;
begin
  // click down / up
  tb := TToolButton(Sender);

  case tb.Name of
    'tbAttrCharacter',
    'tbAttrFG',
    'tbAttrBG':
      tb.Down := false;

    'tbBlinkSlow':
      tbAttrBlinkFast.Down := false;

    'tbBlinkFast':
      tbAttrBlinkSlow.Down := false;

    'tbAttrConceal':
      begin
        tbAttrTop.Down := false;
        tbAttrBottom.Down := false;
        if tb.Down then
          SetBits(CurrAttr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_CONCEAL)
        else
          SetBits(CurrAttr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_NORMAL);
      end;

    'tbAttrTop':
      begin
        tbAttrConceal.Down := false;
        tbAttrBottom.Down := false;
        if tb.Down then
          SetBits(CurrAttr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_TOP)
        else
          SetBits(CurrAttr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_NORMAL);
      end;

    'tbAttrBottom':
      begin
        tbAttrConceal.Down := false;
        tbAttrTop.Down := false;
        if tb.Down then
          SetBits(CurrAttr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_BOTTOM)
        else
          SetBits(CurrAttr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_NORMAL);
      end;
  end;

  SetBit(CurrAttr, A_CELL_BOLD, tbAttrBold.Down);
  SetBit(CurrAttr, A_CELL_FAINT, tbAttrFaint.Down);
  SetBit(CurrAttr, A_CELL_ITALICS, tbAttrItalics.Down);
  SetBit(CurrAttr, A_CELL_UNDERLINE, tbAttrUnderline.Down);
  SetBit(CurrAttr, A_CELL_BLINKSLOW, tbAttrBlinkSlow.Down);
  SetBit(CurrAttr, A_CELL_BLINKFAST, tbAttrBlinkFast.Down);
  SetBit(CurrAttr, A_CELL_REVERSE, tbAttrReverse.Down);
  SetBit(CurrAttr, A_CELL_STRIKETHROUGH, tbAttrStrikethrough.Down);
  SetBit(CurrAttr, A_CELL_DOUBLESTRIKE, tbAttrDoublestrike.Down);
  SetBit(CurrAttr, A_CELL_SHADOW, tbAttrShadow.Down);

  pbCurrCell.Invalidate;
end;

procedure TfMain.tbAttrMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  tb : TToolButton;
begin
  // right click enable/disable
  if button = mbRight then
  begin
    tb := TToolButton(Sender);
    tb.Tag := iif(tb.Tag = 0, 1, 0);
    tb.invalidate;
  end;
end;

procedure TfMain.tbFontClick(Sender: TObject);
var
  tb : TToolButton;
begin
  tb := TToolButton(Sender);
  case tb.Name of
    'tbFont0':
      begin
        CurrFont := 0;
      end;
    'tbFont1':
      begin
        CurrFont := 1;
      end;
    'tbFont2':
      begin
        CurrFont := 2;
      end;
    'tbFont3':
      begin
        CurrFont := 3;
      end;
    'tbFont4':
      begin
        CurrFont := 4;
      end;
    'tbFont5':
      begin
        CurrFont := 5;
      end;
    'tbFont6':
      begin
        CurrFont := 6;
      end;
    'tbFont7':
      begin
        CurrFont := 7;
      end;
    'tbFont8':
      begin
        CurrFont := 8;
      end;
    'tbFont9':
      begin
        CurrFont := 9;
      end;
    'tbFont10':
      begin
        CurrFont := 10;
      end;
    'tbFont11':
      begin
        CurrFont := 11;
      end;
    'tbFont12':
      begin
        CurrFont := 12;
      end;
  end;

  tbFont0.Down := (tb.Name = 'tbFont0');
  tbFont1.Down := (tb.Name = 'tbFont1');
  tbFont2.Down := (tb.Name = 'tbFont2');
  tbFont3.Down := (tb.Name = 'tbFont3');
  tbFont4.Down := (tb.Name = 'tbFont4');
  tbFont5.Down := (tb.Name = 'tbFont5');
  tbFont6.Down := (tb.Name = 'tbFont6');
  tbFont7.Down := (tb.Name = 'tbFont7');
  tbFont8.Down := (tb.Name = 'tbFont8');
  tbFont9.Down := (tb.Name = 'tbFont9');
  tbFont10.Down := (tb.Name = 'tbFont10');
  tbFont11.Down := (tb.Name = 'tbFont11');
  tbFont12.Down := (tb.Name = 'tbFont12');

  // update character palette here.

end;

procedure TfMain.tbModeClick(Sender: TObject);
var
  tb : TToolButton;
  n : string;
begin
  tb := TToolButton(Sender);

  n := tb.Name;
  case n of
    'tbModeCharacter':
      begin
        DrawMode := dmChars;
        SubXSIze := 1;
        SubYSize := 1;
      end;

    'tbModeLeftRights':
      begin
        DrawMode := dmLeftRights;
        SubXSIze := 2;
        SubYSize := 1;
      end;

    'tbModeTopBottoms':
      begin
        DrawMode := dmTopBottoms;
        SubXSIze := 1;
        SubYSize := 2;
      end;

    'tbModeQuarters':
      begin
        DrawMode := dmQuarters;
        SubXSIze := 2;
        SubYSize := 2;
      end;

    'tbModeSixels':
      begin
        DrawMode := dmSixels;
        SubXSIze := 2;
        SubYSize := 3;
      end;
  end;

  tbModeCharacter.Down := (tb.Name = 'tbModeCharacter');
  tbModeLeftRights.Down := (tb.Name = 'tbModeLeftRights');
  tbModeTopBottoms.Down := (tb.Name = 'tbModeTopBottoms');
  tbModeQuarters.Down := (tb.Name = 'tbModeQuarters');
  tbModeSixels.Down := (tb.Name = 'tbModeSixels');
end;

procedure TfMain.tbToolClick(Sender: TObject);
var
  tb : TToolButton;
begin
  tb := TToolButton(Sender);

  case tb.Name of
    'tbToolSelect':     ToolMode := tmSelect;
    'tbToolDraw':       ToolMode := tmDraw;
//    'tbToolEyedropper': ToolMode := tmEy;
    'tbToolFill':       ToolMode := tmFill;
    'tbToolLine':       ToolMode := tmLine;
    'tbToolRect':       ToolMode := tmRect;
    'tbToolEllipse':    ToolMode := tmEllipse;
  end;

  tbToolSelect.Down := (tb.Name = 'tbToolSelect');
  tbToolDraw.Down := (tb.Name = 'tbToolDraw');
  tbToolEyedropper.Down := (tb.Name = 'tbToolEyedropper');
  tbToolFill.Down := (tb.Name = 'tbToolFill');
  tbToolLine.Down := (tb.Name = 'tbToolLine');
  tbToolRect.Down := (tb.Name = 'tbToolRect');
  tbToolEllipse.Down := (tb.Name = 'tbToolEllipse');
end;

procedure TfMain.tbAttributesPalettePaintButton(Sender: TToolButton; State: integer);
var
  tb : TToolButton;
  cnv : TCanvas;
  bmp : TBitmap;
  off : integer;
  r : trect;
  d : integer;
  c, c0, c1, c2 : TColor;

begin
  // draw normal button if tag = 0.
  // draw normal button with X if tab <> 0
  tb := TToolButton(Sender);
  cnv := tb.Canvas;
  r := cnv.ClipRect;
  d := (r.width - 16) >> 1;

  {$ifdef WINDOWS}
    c := GetSysColor(COLOR_HOTLIGHT);
  {$else}
    // get a color for button clicks.
    c := clBlue;
  {$ENDIF}
  c0 := Brighten(c, 0.60);  // border color
  c1 := Brighten(c, 0.85);  // hottrack color
  c2 := Brighten(c, 0.75);  // pressed color

  bmp := TBitmap.create;
  bmp.PixelFormat:=pf32bit;

  case State of

      4, 0:  // disabled
        begin
          off := 0;
          TToolBar(tb.parent).DisabledImages.GetBitmap(tb.ImageIndex, bmp);
        end;

      1:  // normal
        begin
          off := 0;
          TToolBar(tb.parent).Images.GetBitmap(tb.ImageIndex, bmp);
        end;

      2:  // hot
        begin
          cnv.brush.color := c1;
          cnv.FillRect(r);
          cnv.Pen.color := c0;
          cnv.Brush.style := bsClear;
          cnv.Rectangle(r);
          off := 0;
          TToolBar(tb.parent).Images.GetBitmap(tb.ImageIndex, bmp);
        end;

      3:  // clicked
        begin
          cnv.brush.color := c1;
          cnv.FillRect(r);
          cnv.Pen.color := c0;
          cnv.Brush.style := bsClear;
          cnv.Rectangle(r);
          off := 1;
          TToolBar(tb.parent).Images.GetBitmap(tb.ImageIndex, bmp);
        end;

      5: // down
        begin
          cnv.brush.color := c2;
          cnv.FillRect(r);
          cnv.Pen.color := c0;
          cnv.Brush.style := bsClear;
          cnv.Rectangle(r);
          off := 1;
          TToolBar(tb.parent).Images.GetBitmap(tb.ImageIndex, bmp);
        end;

      6:  // clicked
        begin
          cnv.brush.color := c1;
          cnv.FillRect(r);
          cnv.Pen.color := c0;
          cnv.Brush.style := bsClear;
          cnv.Rectangle(r);
          off := 1;
          TToolBar(tb.parent).Images.GetBitmap(tb.ImageIndex, bmp);
        end;

      7: //??
        nop;

      else
        begin
          nop;
        end;
  end;
  cnv.Draw(d + off, d + off, bmp);
  bmp.free;

  if tb.Tag <> 0 then
  begin
    bmp := TBitmap.create;
    bmp.PixelFormat:=pf32bit;
    ilButtons.GetBitmap(31, bmp);
    cnv.Draw(d, d, bmp);
    bmp.free;
  end;
end;

procedure TfMain.miFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfMain.miFileOpenClick(Sender: TObject);
var
  fin :               TFileStream;

  buff : TBytes;

  len :               integer;
  SaveRow, SaveCol :  integer;
  SaveAttr :          Uint32;
  chr :               integer;
  state :             integer;
  dochar, docsi :     boolean;
  doeof :             boolean;  // premature EOF (SAUSE for now);
  parms, inter :      string;
  pvals :             TStringArray;
  fg, bg :            integer;
  bold, blink :       boolean;
  FileEnc :           TEncoding;
  bomskip :           integer;
  checkenc :          TDetectEnc;
  charslen :          integer;
  ansi :              UnicodeString;
  chars :             array of UInt16;
  Sauce :             TSauceHeader;
  ValidSauce :        Boolean;

  function GetIntCSIVal(num : integer; defval : integer) : integer;
  var
    val : integer;
  begin
    if num < length(pvals) then
    begin
      if not TryStrToInt(pvals[num], val) then
        val := defval;
    end
    else
      val := defval;
    result := val;
  end;

  function GetStrCSIVal(num : integer; defval : unicodestring) : unicodestring;
  begin
    if num < length(pvals) then
    begin
      result := pvals[num];
      if result = '' then
        result := defval;
    end
    else
      result := defval;
  end;

  procedure LoadANSIFile(fname : string; force8bit : boolean = false);
  var
    i, j, k : integer;
  begin
    // load file
    fin := TFileStream.Create(fname, fmOpenRead or fmShareDenyNone);
    len := fin.Size;

    //getmemory(buff, len);
    setlength(buff, len);
    fin.ReadBuffer(buff[0], len);
    fin.free;

    // detect data - move to UnicodeHelper
// CODEPAGE - no way of knowing unless from sauce
    FileEnc := encCP437;
    if not force8bit then
    begin
      bomskip := 0;
      checkenc := CheckBom(buff);
      if checkenc = deNone then
        checkenc := DetectEncoding(buff);
      case checkenc of
        deUtf16LeBom:
          begin
            FileEnc := encUtf16;
            bomskip := 2;
          end;

        deUtf16LeNoBom:
          FileEnc := encUtf16;

        deUtf16BeBom, deUtf16BeNoBom:
          exit;

        deUtf8Bom:
          begin
            FileEnc := encUTF8;
            bomskip := 3;
          end;

        deUtf8NoBom:
          FileEnc := encUTF8;
      end;
    end;

    if FileEnc = encUTF8 then
    begin
      // UTF8
      if CurrCodePage <> encUTF8 then
      begin
        CurrCodePage := encUTF8;
        cbCodePage.ItemIndex := ord(CurrCodePage);
        cbCodePageChange(cbCodePage);
        BuildCharacterPalette;
      end;
      ansi := ansi.fromUTF8Bytes(buff);
    end

    else if FileEnc = encUTF16 then
    begin
      // UTF16
      if CurrCodePage <> encUTF16 then
      begin
        CurrCodePage := encUTF16;
        cbCodePage.ItemIndex := ord(CurrCodePage);
        cbCodePageChange(cbCodePage);
        BuildCharacterPalette;
      end;
      ansi := ansi.fromUTF16Bytes(buff);
    end

    else
    begin
// CODEPAGE - check sauce
      // assume CP437 if not UTF8/16
      if (CurrCodePage = encUTF8) or (CurrCodePage = encUTF16) then
      begin
        CurrCodePage := encCP437;
        cbCodePage.ItemIndex := ord(CurrCodePage);
        cbCodePageChange(cbCodePage);
        BuildCharacterPalette;
      end;

      // only use sause in 8bit CodePaged files.
      ValidSauce := false;
      if length(buff) > 128 then
      begin
        move(buff[length(buff)-128], Sauce, 128);
        ValidSauce := CompareMem(@Sauce.ID, @SauceID, 5);
        if ValidSauce then
        begin
          Page.Sauce := Sauce;
          if Sauce.TInfo1 > 0 then
            NumCols := Sauce.TInfo1;
          if Sauce.TInfo2 > 0 then
            NumRows := Sauce.TInfo2;
          if HasBits(Sauce.TFlags, SAUCE_FLAG_ICE) then
          begin
            PageType := PAGETYPE_CTERM;
            cbPageType.ItemIndex := PAGETYPE_CTERM;
            ColorScheme := COLORSCHEME_ICE;
            cbColorScheme.ItemIndex := COLORSCHEME_ICE;
          end;
          ResizePage;
          //    TInfoS = Font name (from SauceFonts pattern)
        end;
      end;
      ansi := ansi.fromCPBytes(buff);
    end;
    chars := ansi.toWordArray;
    charslen := length(chars);

    // populate page.
    SkipScroll := true;
    CursorRow := 0;
    CursorCol := 0;
    CurrAttr := BlankCell.Attr;
    CurrChar := BlankCell.Chr;
    state := 0;

    for i := bomskip to charslen - 1 do
    begin

      docsi := false;
      dochar := false;
      doeof := false;

      chr := chars[i];

      // do main C0 first
      case chr of
        8: // BS
          begin
            if CursorCol > 0 then
              CursorCol -= 1;
          end;

        9: // HT
          begin
            CursorCol := (CursorCol and $7) + 8;
            if CursorCol >= NumCols then
              CursorCol := NumCols - 1;
          end;

        10: // LF
          begin
            CursorRow += 1;
            if CursorRow >= NumRows then
            begin
              NumRows := CursorRow + 1;
              skipResize := true;
              seRows.Value := NumRows;
              skipResize := false;
              ResizePage;
            end;
          end;

        13: // CR
          begin
            CursorCol := 0;
          end;

        26: // SAUSE RECORD.
          begin
            doeof := true;
          end;

        else
          begin
            if between(chr, 0, 26) or between(chr, 28, 31) then
              nop;

            case state of
              0:  // no state yet
                begin
                  if chr = 27 then
                    state := 1
                  else
                  begin
                    // add to doc
                    CurrChar := ord(chr);

                    // adjust attr for page type / colors
                    SaveAttr := CurrAttr;
                    fg := GetBits(CurrAttr, A_CELL_FG_MASK);
                    bg := GetBits(CurrAttr, A_CELL_BG_MASK, 8);
                    bold := HasBits(CurrAttr, A_CELL_BOLD);
                    blink := HasBits(CurrAttr, A_CELL_BLINKFAST or A_CELL_BLINKSLOW);
                    if PageType <> PAGETYPE_VTX then
                    begin
                      if bold and between(fg, 0, 7) then
                      begin
                        // adj FG
                        SetBit(CurrAttr, A_CELL_BOLD, false);
                        SetBits(CurrAttr, A_CELL_FG_MASK, fg + 8);
                      end;
                    end;

                    if ColorScheme = COLORSCHEME_ICE then
                    begin
                      if blink then
                      begin
                        if between(bg, 0, 7) then
                        begin
                          // adj FG
                          SetBits(CurrAttr, A_CELL_BLINKFAST or A_CELL_BLINKSLOW, 0);
                          SetBits(CurrAttr, A_CELL_BG_MASK, bg + 8, 8);
                        end;
                      end;
                    end;
                    dochar := true;
                  end;
                end;

              1: // got esc / awaiting [
                begin
                  case chr of
                    91:   // [
                      begin
                        state := 2;
                        parms := '';    // for collecting parameter bytes
                      end;

                    // other codes here (ESC A, ESC # n, etc)
                    // ,,,

                    else
                      begin
                        // unknown
                        dochar := true;
                        state := 0;
                      end;
                  end;
                end;

              2:  // CSI
                begin
                  if between(chr, $30, $3F) then
                    // parameter bytes
                    parms += WideChar(chr)
                  else if between(chr, $20, $2F) then
                  begin
                    // itermediate bytes
                    inter := WideChar(chr);
                    state := 3;
                  end
                  else if between(chr, $40, $7E) then
                  begin
                    // final byte
                    docsi := true;
                    state := 0;
                  end
                  else
                  begin
                    // unknown
                    dochar := true;
                    state := 0;
                  end;
                end;

              3:  // collect intermediate
                begin
                  if between(chr, $20, $2F) then
                    // intermediate bytes
                    inter += WideChar(chr)
                  else if between(chr, $40, $7E) then
                  begin
                    // final byte
                    docsi := true;
                    state := 0;
                  end
                  else
                  begin
                    // unknown
                    dochar := true;
                    state := 0;
                  end;
                end;
            end;
        end;
      end;

      if dochar then
      begin
        if CursorRow >= NumRows then
        begin
          NumRows := CursorRow + 1;
          skipResize := true;
          seRows.Value := NumRows;
          skipResize := false;
          ResizePage;
        end;
        Page.Rows[CursorRow].Cells[CursorCol].Chr := ord(chr);
        Page.Rows[CursorRow].Cells[CursorCol].Attr := CurrAttr;
        CursorCol += 1;
        if CursorCol >= NumCols then
        begin
          CursorCol := 0;
          CursorRow += 1;
          if CursorRow >= NumRows then
          begin
            NumRows := CursorRow + 1;
            skipResize := true;
            seRows.Value := NumRows;
            skipResize := false;
            ResizePage;
          end;
        end;
        CurrAttr := SaveAttr;
      end;

      if docsi then
      begin
        pvals := parms.Split([';']);
        case WideChar(chr) of
          '@':  // insert n chars
            ;

          'A':  // CUU - up n
            begin
              CursorRow -= GetIntCSIVal(0, 1);
              if CursorRow < 0 then
                CursorRow := 0;
            end;

          'B':  // CUD - down n
            begin
              CursorRow += GetIntCSIVal(0, 1);
              if CursorRow >= NumRows then
                CursorRow := NumRows - 1;
            end;

          'C':  // CUF - forward n
            begin
              CursorCol += GetIntCSIVal(0, 1);
              if CursorCol > NumCols then
                CursorCol := NumCols - 1;
            end;

          'D':  // CUB - back n
            begin
              CursorCol -= GetIntCSIVal(0, 1);
              if CursorCol < 0 then
                CursorCol := 0;
            end;

          'E':  // CNL - col 0 down n lines
            begin
              CursorCol := 0;
              CursorRow += GetIntCSIVal(0, 1);
              if CursorRow >= NumRows then
              begin
                seRows.Value := CursorRow + 1;
                ResizePage;
              end;
            end;

          'F':  // CPL - col 0 up n lines
            begin
              CursorCol := 0;
              CursorRow -= GetIntCSIVal(0, 1);
              if CursorRow < 0 then
                CursorRow := 0;
            end;

          'G':  // CHA - move to col n
            begin
              CursorCol := GetIntCSIVal(0, 1) - 1;
              if CursorCol >= NumCols then
                CursorCol := NumCols - 1;
            end;

          'H', 'f': // CUP / HVP - move to r,c
            begin
              CursorRow:= GetIntCSIVal(0, 1) - 1;
              CursorCol := GetIntCSIVal(1, 1) - 1;
              if CursorRow  >= NumRows then
                CursorRow := NumRows - 1;
              if CursorCol >= NumCols then
                CursorCol := NumCols - 1;
            end;

          'I':  ;
          'J':  // ED - erase screen 1=sos,0=eos,2=all
            begin
              case GetIntCSIVal(0, 0) of
                0:  // end of screen
                  begin
                    for k := CursorCol to NumCols - 1 do
                      Page.Rows[CursorRow].Cells[k] := BlankCell;
                    for j := CursorRow + 1 to NumRows - 1 do
                      for k := 0 to NumCols - 1 do
                        Page.Rows[j].Cells[k] := BlankCell;
                  end;

                1:  // start of screen
                  begin
                    for k := 0 to CursorCol do
                      Page.Rows[CursorRow].Cells[k] := BlankCell;
                    for j := 0 to CursorRow - 1 do
                      for k := 0 to NumCols - 1 do
                        Page.Rows[j].Cells[k] := BlankCell;
                  end;

                2:  // all
                  begin
                    CursorRow := 0;
                    CursorCol := 0;
                    for j := 0 to NumRows - 1 do
                      for k := 0 to NumCols - 1 do
                        Page.Rows[j].Cells[k] := BlankCell;
                  end;
              end;
            end;

          'K':  // EL - erase line 1=sol,0=eol,2=all
            begin
              case GetIntCSIVal(0, 0) of
                0:  // end of line
                  begin
                    for k := CursorCol to NumCols - 1 do
                      Page.Rows[CursorRow].Cells[k] := BlankCell;
                  end;

                1:  // start of line
                  begin
                    for k := 0 to CursorCol do
                      Page.Rows[CursorRow].Cells[k] := BlankCell;
                  end;

                2:  // all
                  begin
                    for k := 0 to NumCols - 1 do
                      Page.Rows[CursorRow].Cells[k] := BlankCell;
                  end;
              end;
            end;

          'L':  ;
          'M':  ;
          'N':  ;
          'O':  ;
          'P':  ;
          'Q':  ;
          'R':  ;

          'S':  // SU - scroll up n
            ;

          'T':  // ST - scroll down n
            ;

          'U':  ;
          'V':  ;
          'W':  ;
          'X':  ;
          'Y':  ;
          'Z':  ;
          '[':  ;
          '\':  ;
          ']':  ;
          '^':  ;
          '_':  ;
          '`':  ;
          'a':  ;
          'b':  ;
          'c':  ;
          'd':  ;
          'e':  ;
          'g':  ;
          'h':  ;
          'i':  ;
          'j':  ;
          'k':  ;
          'l':  ;

          'm':  // SGR - set attributes
            begin
              k := 0;
              while k < length(pvals) do
              begin
                j := GetIntCSIVal(k, 0);
                case j of
                  0:  // reset
                    CurrAttr := $0007;

                  1:  // bold
                    SetBit(CurrAttr, A_CELL_BOLD, true);

                  2:  // faint
                    SetBit(CurrAttr, A_CELL_FAINT, true);

                  3:  // italics
                    SetBit(CurrAttr, A_CELL_ITALICS, true);

                  4:  // underline
                    SetBit(CurrAttr, A_CELL_UNDERLINE, true);

                  5:  // blink slow
                    SetBit(CurrAttr, A_CELL_BLINKSLOW, true);

                  6:  // blink fast
                    SetBit(CurrAttr, A_CELL_BLINKFAST, true);

                  7:  // reverse
                    SetBit(CurrAttr, A_CELL_REVERSE, true);

                  8:  // conceal
                    SetBits(CurrAttr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_CONCEAL);

                  9:  // strikethrough
                    SetBit(CurrAttr, A_CELL_STRIKETHROUGH, true);

                  10..19: // font - skip for now
                    ;

                  21: // bold off
                    SetBit(CurrAttr, A_CELL_BOLD, false);

                  22: // faint off
                    SetBit(CurrAttr, A_CELL_FAINT, false);

                  23: // italics off
                    SetBit(CurrAttr, A_CELL_ITALICS, false);

                  24: // underline off
                    SetBit(CurrAttr, A_CELL_UNDERLINE, false);

                  25, 26: // blink off
                    SetBits(CurrAttr, A_CELL_BLINKSLOW or A_CELL_BLINKFAST, 0);

                  27: // reverse off
                    SetBit(CurrAttr, A_CELL_REVERSE, false);

                  28: // concear off
                    SetBits(CurrAttr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_NORMAL);

                  29: // strikethrough off
                    SetBit(CurrAttr, A_CELL_STRIKETHROUGH, false);

                  30..37: // fg color
                    SetBits(CurrAttr, A_CELL_FG_MASK, j - 30);

                  38: // get 5 , fg
                    begin
                      k += 1;
                      if GetIntCSIVal(k, 0) = 5 then
                      begin
                        k += 1;
                        SetBits(CurrAttr, A_CELL_FG_MASK, GetIntCSIVal(k, 0));
                      end;
                    end;

                  39: // reset fg color
                    SetBits(CurrAttr, A_CELL_FG_MASK, 7);

                  40..47: // bg color
                    SetBits(CurrAttr, A_CELL_BG_MASK, j - 40, 8);

                  48: // get 5, bg
                    begin
                      k += 1;
                      if GetIntCSIVal(k, 0) = 5 then
                      begin
                        k += 1;
                        SetBits(CurrAttr, A_CELL_BG_MASK, GetIntCSIVal(k, 0), 8);
                      end;
                    end;

                  49: // reset bg color
                    SetBits(CurrAttr, A_CELL_BG_MASK, 0, 8);

                  56: // doublestrike
                    SetBit(CurrAttr, A_CELL_DOUBLESTRIKE, true);

                  57: // shadow
                    SetBit(CurrAttr, A_CELL_SHADOW, true);

                  58: // top half
                    SetBits(CurrAttr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_TOP);

                  59: // bottom half
                    SetBits(CurrAttr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_BOTTOM);

                  76:
                    SetBit(CurrAttr, A_CELL_DOUBLESTRIKE, false);

                  77:
                    SetBit(CurrAttr, A_CELL_SHADOW, false);

                  78, 79:
                    SetBits(CurrAttr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_NORMAL);

                  90..97: // aixterm fg color
                    SetBits(CurrAttr, A_CELL_FG_MASK, j - 90 + 8);

                  100..107: // aixterm bg color
                    SetBits(CurrAttr, A_CELL_BG_MASK, j - 100 + 8);

                end;
                k += 1;
              end;
            end;

          'n':  ;
          'o':  ;
          'p':  ;
          'q':  ;
          'r':  ;

          's':  // SCP - save cursor pos
            begin
              SaveRow := CursorRow;
              SaveCol := CursorCol;
            end;

          't':  ;

          'u':  // RCP - restore cursor pos
            begin
              CursorRow := SaveRow;
              CursorCol := SaveCol;
            end;

          'v':  ;
          'w':  ;
          'x':  ;
          'y':  ;
          'z':  ;
          '{':  ;
          '|':  ;
          '}':  ;
          '~':  ;

        end;
      end;

      if doeof then
        break;
    end;
  end;

begin
  // ask to save

  NumRows := 24;
  NewFile;
  if odAnsi.Execute then
  begin

    case odAnsi.FilterIndex of
      1:  // vtx
        begin
          LoadVTXFile(odAnsi.FileName);
        end;

      2: // autodetect
        begin
          LoadANSIFile(odAnsi.Filename, false);
        end;

      3: // 8bit
        begin
          LoadANSIFile(odAnsi.Filename, true);
        end;

      else
        begin
          LoadANSIFile(odAnsi.Filename, false);
        end;
    end;

    CurrFileName := ExtractFileName(odAnsi.FileName);
    CurrFileChanged := false;
    UpdateTitles;

    cbPageType.ItemIndex := PageType;
    cbCodePage.ItemIndex := ord(CurrCodePage);
    cbColorScheme.ItemIndex := ColorScheme;
    seRows.Value := NumRows;
    seCols.Value := NumCols;
    seXScale.Value := XScale;

    tbSauceTitle.Text :=  CharsToStr(Page.Sauce.Title,  sizeof(TSauceHeader.Title));
    tbSauceAuthor.Text := CharsToStr(Page.Sauce.Author, sizeof(TSauceHeader.Author));
    tbSauceGroup.Text :=  CharsToStr(Page.Sauce.Group,  sizeof(TSauceHeader.Group));
    tbSauceDate.Text :=   CharsToStr(Page.Sauce.Date,   sizeof(TSauceHeader.Date));

    CurrAttr := $0007;
    CurrChar := _SPACE;
    CursorRow := 0;
    CursorCol := 0;
    SkipScroll := false;
    SetAttrButtons(CurrAttr);
    pbColors.Invalidate;

    ResizeScrolls;
    GenerateBmpPage;
    Invalidate;
  end;
end;

procedure TfMain.SetAttrButtons(attr : Uint32);
var
  bits : integer;
begin
  tbAttrBold.Down := HasBits(attr, A_CELL_BOLD);
  tbAttrFaint.Down := HasBits(attr, A_CELL_FAINT);
  tbAttrItalics.Down := HasBits(attr, A_CELL_ITALICS);
  tbAttrUnderline.Down := HasBits(attr, A_CELL_UNDERLINE);
  tbAttrBlinkSlow.Down := HasBits(attr, A_CELL_BLINKSLOW);
  tbAttrBlinkFast.Down := HasBits(attr, A_CELL_BLINKFAST);
  tbAttrReverse.Down := HasBits(attr, A_CELL_REVERSE);
  tbAttrStrikethrough.Down := HasBits(attr, A_CELL_STRIKETHROUGH);
  tbAttrDoublestrike.Down := HasBits(attr, A_CELL_DOUBLESTRIKE);
  tbAttrShadow.Down := HasBits(attr, A_CELL_SHADOW);

  bits := GetBits(attr, A_CELL_DISPLAY_MASK);
  tbAttrConceal.Down := (bits = A_CELL_DISPLAY_CONCEAL);
  tbAttrTop.Down := (bits = A_CELL_DISPLAY_TOP);
  tbAttrBottom.Down := (bits = A_CELL_DISPLAY_BOTTOM);
end;

{-----------------------------------------------------------------------------}

{ Mouse Routines }


procedure TfMain.pbPageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft : MouseLeft := false;
    mbMiddle : MouseMiddle := false;
    mbRight : MouseRight := false;
  end;

  if Button = mbMiddle then
    MousePan := false;

  // stop any dragging
  if drag then
  begin
    // add / remove selection
    CopySelection := BuildDisplayCopySelection;
    drag := false;
    pbPage.invalidate;
  end
  else if dragObj then
    dragObj := false;

end;


procedure TfMain.pbPageMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  val : integer;
begin
  // zoom
  if ssShift in Shift then
  begin
    if WheelDelta < 0 then
    begin
      PageZoom /= 2;
      if PageZoom < 1/8 then
        PageZoom := 1/8;
    end
    else if WheelDelta > 0 then
    begin
      PageZoom *= 2;
      if PageZoom > 16 then
        PageZoom := 16;
    end;
    if floor(CellWidth * PageZoom * XScale) = 0 then
    begin
      // don't allow 0's for cellwidthz
      PageZoom *= 2;
    end;

    CellWidthZ := floor(CellWidth * PageZoom * XScale);
    CellHeightZ := floor(CellHeight * PageZoom);

    ResizeScrolls;
  end
  else
  begin
    // scroll
    val := sbVert.Position;
    if WheelDelta > 0 then
    begin
      val -= 4;
      if val < 0 then
        val := 0;
    end
    else
    begin
      val += 4;
      if val > sbvert.Max - WindowRows then
        val  := sbvert.max - WindowRows;
    end;
    sbVert.Position := val;
  end;
  Handled := true;
end;


procedure TfMain.pbPageMouseLeave(Sender: TObject);
begin
  MousePan := false;
  MouseRow := -1;
  pbStatusBar.Invalidate;

  if ToolMode = tmDraw then
    DrawCell(LastDrawRow, LastDrawCol);
end;

procedure TfMain.pbPageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  bcolor :        integer;
  dattr, dchar :  integer;
  objnum :        integer;
  tmp :           integer;
begin
  // left click to type.
  // left drag to select
  // right drag to pan
  case Button of
    mbLeft : MouseLeft := true;
    mbMiddle : MouseMiddle := true;
    mbRight : MouseRight := true;
  end;

  pSettings.SetFocus;
  if Button = mbMiddle then
  begin
    // pan
    DrawCell(LastDrawRow, LastDrawCol);
    MousePan := true;
    MousePanT := PageTop;
    MousePanL := PageLeft;
    MousePanX := X;
    MousePanY := Y;
  end
  else
  begin
    case ToolMode of
      tmSelect:
        begin

          // check if clicking object
          objnum := GetObject(MouseRow, MouseCol);
          if objnum <> SelectedObject then
          begin
            tmp := SelectedObject;
            SelectedObject := objnum;
            RefreshObject(tmp);
            RefreshObject(SelectedObject);
          end;

          lvObjects.ItemIndex := SelectedObject;
          lvObjects.Invalidate;
          if (SelectedObject <> -1) then
          begin
            if not Objects[SelectedObject].Locked then
            begin
              dragRow := MouseRow - Objects[SelectedObject].Row; // save delta
              dragCol := MouseCol - Objects[SelectedObject].Col;
              dragObj := true;
            end;
          end
          else
          begin
            // move cursor or select region
            // drag = new selection
            // shift drag = add to selection
            // ctrl drag = remove from selection
            // click = cell to new selection / move cursor
            // shift click = add cell from selection
            // ctrl click = remove cell from selection
            if between(MouseRow, 0, NumRows - 1)
              and between(MouseCol, 0, NumCols - 1) then
            begin

              if ssShift in Shift then
              begin
                // add to selection
                // clear previous selection.
                drag := true;
                dragType := DRAG_ADD;
                dragRow := MouseRow;
                dragCol := MouseCol;
              end
              else if ssCtrl in Shift then
              begin
                // remove from selection
                // clear previous selection.
                drag := true;
                dragType := DRAG_REMOVE;
                dragRow := MouseRow;
                dragCol := MouseCol;
              end
              else
              begin
                CursorMove(MouseRow, MouseCol);

                // clear previous selection.
                setlength(CopySelection, 0);
                pbPage.invalidate;

                // normal click / start selection
                drag := true;
                dragType := DRAG_NEW;
                dragRow := MouseRow;
                dragCol := MouseCol;
              end;
            end;
          end;
        end;

      tmDraw:
        begin
          // draw current character.
          // left click = draw, right click = erase
          if (MouseLeft or MouseRight) and between(MouseRow, 0, NumRows-1)
            and between(MouseCol, 0, NumCols-1) then
          begin
            DrawX := ((PageLeft * SubXSize) +  Floor((X / CellWidthZ) * SubXSize)) ;
            DrawY := ((PageTop * SubYSize) +  Floor((Y / CellHeightZ) * SubYSize));
            SubX := DrawX mod SubXSize;
            SubY := DrawY mod SubYSize;

            case DrawMode of
              dmChars:
                begin
                  dchar := iif(Button = mbLeft, CurrChar, $20);
                  dattr := iif(Button = mbLeft, CurrAttr, $0007);
                  PutCharEx(dchar, dattr, MouseRow, MouseCol);
                end;

              dmLeftRights:
                begin
                  bcolor := iif(Button = mbLeft,
                    GetBits(CurrAttr, A_CELL_FG_MASK),
                    GetBits(CurrAttr, A_CELL_BG_MASK, 8));
                  Page.Rows[MouseRow].Cells[MouseCol] :=
                    SetBlockColor(bcolor, Page.Rows[MouseRow].Cells[MouseCol],
                      2, 1, SubX, SubY);
                  DrawCell(MouseRow, MouseCol, false);
                  CurrFileChanged := true;
                  UpdateTitles;
                end;

              dmTopBottoms:
                begin
                  bcolor := iif(Button = mbLeft,
                    GetBits(CurrAttr, A_CELL_FG_MASK),
                    GetBits(CurrAttr, A_CELL_BG_MASK, 8));
                  Page.Rows[MouseRow].Cells[MouseCol] :=
                    SetBlockColor(bcolor, Page.Rows[MouseRow].Cells[MouseCol],
                      1, 2, SubX, SubY);
                  DrawCell(MouseRow, MouseCol,false);
                  CurrFileChanged := true;
                  UpdateTitles;
                end;

              dmQuarters:
                begin
                  bcolor := iif(Button = mbLeft,
                    GetBits(CurrAttr, A_CELL_FG_MASK),
                    GetBits(CurrAttr, A_CELL_BG_MASK, 8));
                  Page.Rows[MouseRow].Cells[MouseCol] :=
                    SetBlockColor(bcolor, Page.Rows[MouseRow].Cells[MouseCol],
                      2, 2, SubX, SubY);
                  DrawCell(MouseRow, MouseCol,false);
                  CurrFileChanged := true;
                  UpdateTitles;
                end;
            end;
          end;
        end;
    end;
  end;
end;

procedure TfMain.pbPageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  i, dx, dy :       integer;
  done :            boolean;
  mr, mc, sx, sy :  integer;
  bcolor :          integer;
  dattr, dchar :    integer;
begin

  // update mouse position
  MouseRow := PageTop + floor(Y / CellHeightZ);
  MouseCol := PageLeft + floor(X / CellWidthZ);
  if Between(MouseRow, 0, NumRows - 1) and Between(MouseCol, 0, NumCols - 1) then
  begin
    // calc subx,y
    DrawX := ((PageLeft * SubXSize) +  Floor((X / CellWidthZ) * SubXSize)) ;
    DrawY := ((PageTop * SubYSize) +  Floor((Y / CellHeightZ) * SubYSize));
    SubX := DrawX mod SubXSize;
    SubY := DrawY mod SubYSize;
  end
  else
  begin
    MouseRow := -1;
    dragObj := false; // stop moving if off page
  end;

  // update mouse r,c on status bar
  pbStatusBar.Invalidate;
  if (DrawX <> LastDrawX) or (DrawY <> LastDrawY) then
  begin
    pbRulerLeft.Invalidate;
    pbRulerTop.Invalidate;
  end;

  if MousePan then
  begin
    // panning the document
    dx := round((MousePanX - X) / CellWidthZ);
    dy := round((MousePanY - Y) / CellHeightZ);

    if NumCols > WindowCols then
    begin
      i := MousePanL + dx;
      if i < sbHorz.Min then
        i := sbHorz.Min;
      if i > sbHorz.Max - WindowCols then
        i := sbHorz.Max - WindowCols + 1;
      sbHorz.Position := i;
    end;

    if NumRows > WindowRows then
    begin
      i := MousePanT + dy;
      if i < sbVert.Min then
        i := sbVert.Min;
      if i > sbVert.Max - WindowRows then
        i := sbVert.Max - WindowRows + 1;
      sbVert.Position := i;
    end;
  end
  else if drag then
  begin
    // dragging copy selection
    // let paint draw selection information
    if (dragType = DRAG_NEW) and ((MouseRow <> dragRow) or (MouseCol <> dragCol)) then
      dragType := DRAG_ADD;

    pbPage.Invalidate();
  end
  else if dragObj then
  begin
    // moving an object
    Objects[SelectedObject].Row := MouseRow - DragRow;
    Objects[SelectedObject].Col := MouseCol - DragCol;
    pbPage.Invalidate;
  end
  else
  begin
    case ToolMode of
      tmDraw:
        begin
          if between(MouseRow, 0, NumRows-1) and between(MouseCol, 0, NumCols-1) then
          begin
            if (MouseLeft or MouseRight) and ((DrawX <> LastDrawX) or (DrawY <> LastDrawY)) then
            begin
              case DrawMode of
                dmChars:
                  begin
                    // add move to
                    dchar := iif(MouseLeft, CurrChar, $20);
                    dattr := iif(MouseLeft, CurrAttr, $0007);
                    LineCalcInit(LastDrawX, LastDrawY, DrawX, DrawY);
                    repeat
                      done := LineCalcNext(LastDrawX, LastDrawY);
                      PutCharEx(dchar, dattr, LastDrawY, LastDrawX);
                    until done;
                  end;

                dmLeftRights:
                  begin
                    bcolor := iif(MouseLeft,
                      GetBits(CurrAttr, A_CELL_FG_MASK),
                      GetBits(CurrAttr, A_CELL_BG_MASK, 8));
                    LineCalcInit(lastDrawX, LastDrawY, DrawX, DrawY);
                    repeat
                      done := LineCalcNext(LastDrawX, LastDrawY);
                      mr := LastDrawY div SubYSize;
                      mc := LastDrawX div SubXSize;
                      sx := LastDrawX mod SubXSize;
                      sy := LastDrawY mod SubYSize;
                      Page.Rows[mr].Cells[mc] :=
                        SetBlockColor(bcolor, Page.Rows[mr].Cells[mc],
                          2, 1, sx, sy);
                      DrawCell(mr, mc, false);
                      CurrFileChanged := true;
                      UpdateTitles;
                    until done;
                  end;

                dmTopBottoms:
                  begin
                    bcolor := iif(MouseLeft,
                      GetBits(CurrAttr, A_CELL_FG_MASK),
                      GetBits(CurrAttr, A_CELL_BG_MASK, 8));
                    LineCalcInit(lastDrawX, LastDrawY, DrawX, DrawY);
                    repeat
                      done := LineCalcNext(LastDrawX, LastDrawY);
                      mr := LastDrawY div SubYSize;
                      mc := LastDrawX div SubXSize;
                      sx := LastDrawX mod SubXSize;
                      sy := LastDrawY mod SubYSize;
                      Page.Rows[mr].Cells[mc] :=
                        SetBlockColor(bcolor, Page.Rows[mr].Cells[mc],
                          1, 2, sx, sy);
                      DrawCell(mr, mc, false);
                      CurrFileChanged := true;
                      UpdateTitles;
                    until done;
                  end;

                dmQuarters:
                  begin
                    bcolor := iif(MouseLeft,
                      GetBits(CurrAttr, A_CELL_FG_MASK),
                      GetBits(CurrAttr, A_CELL_BG_MASK, 8));
                    LineCalcInit(lastDrawX, LastDrawY, DrawX, DrawY);
                    repeat
                      done := LineCalcNext(LastDrawX, LastDrawY);
                      mr := LastDrawY div SubYSize;
                      mc := LastDrawX div SubXSize;
                      sx := LastDrawX mod SubXSize;
                      sy := LastDrawY mod SubYSize;
                      Page.Rows[mr].Cells[mc] :=
                        SetBlockColor(bcolor, Page.Rows[mr].Cells[mc],
                          2, 2, sx, sy);
                      DrawCell(mr, mc,false);
                      CurrFileChanged := true;
                      UpdateTitles;
                    until done;
                  end;
              end;
            end;

            // draw square box retinal
            DrawMouseBox;
          end
          else
            DrawCell(LastDrawRow, LastDrawCol);
        end;

      tmLine: ;
      tmRect:  ;
      tmEllipse: ;
    end
  end;
end;


{-----------------------------------------------------------------------------}

{ Drawing functions }

procedure TfMain.DrawMouseBox;
var
  x, y : integer;
begin
  // erase the previous
  if (DrawX <> LastDrawX) or (DrawY <> LastDrawY) then
  begin
    DrawCell(LastDrawRow, LastDrawCol);

    // compute x, y of little box
    x := floor((DrawX - (PageLeft * SubXSize)) * CellWidthZ) div SubXSize;
    y := floor((DrawY - (PageTop * SubYSize)) * CellHeightZ) div SubYSize;

    if Between(MouseRow, PageTop, NumRows - 1)
      and Between(MouseCol, PageLeft, NumCols - 1)
      and (GetObject(MouseRow, MouseCol) = -1) then
      DrawDashRect(pbPage.Canvas, x, y,
        x + (CellWidthZ div SubXSize), y + (CellHeightZ div SubYSize),
        clDrawCursor1, clDrawCursor2);

    LastDrawRow := MouseRow;
    LastDrawCol := MouseCol;
    LastDrawX := DrawX;
    LastDrawY := DrawY;
  end;
end;

// draw a cell on screen from object or Page.Rows[].Cells[].
procedure TfMain.DrawCell(row, col : integer; skipUpdate : boolean = true);
var
  cnv :       TCanvas;
  x, y :      integer;
  cell :      TCell;
  objnum :    integer;
  neighbors : byte;
  cl1, cl2 :  TColor;
  i : integer;
begin
  // compute x, y of row, col
  cnv := pbPage.Canvas;
  x := (col - PageLeft) * CellWidthZ;
  y := (row - PageTop) * CellHeightZ;
  if Between(x, 0, pbPage.Width + CellWidthZ)
    and Between(y, 0, pbPage.Height + CellHeightZ) then
  begin
    objnum := GetObjectCell(row, col, cell, neighbors);
    if (objnum = -1) then
    begin
      DrawCellEx(cnv, x, y, row, col, skipUpdate);
      // draw selection borders
      for i := length(CopySelection) - 1 downto 0 do
      begin
        if (CopySelection[i].Row = row) and (CopySelection[i].Col = col) then
        begin
          if not HasBits(CopySelection[i].Neighbors, NEIGHBOR_NORTH) then
            DrawDashLine(cnv,
              x, y,
              x + CellWidthZ, y,
              clSelectionArea1, clSelectionArea2);

          if not HasBits(CopySelection[i].Neighbors, NEIGHBOR_SOUTH) then
            DrawDashLine(cnv,
              x, y + CellHeightZ - 1,
              x + CellWidthZ, y + CellHeightZ - 1,
              clSelectionArea1, clSelectionArea2);

          if not HasBits(CopySelection[i].Neighbors, NEIGHBOR_WEST) then
            DrawDashLine(cnv,
              x, y,
              x, y + CellHeightZ,
              clSelectionArea1, clSelectionArea2);

          if not HasBits(CopySelection[i].Neighbors, NEIGHBOR_EAST) then
            DrawDashLine(cnv,
              x + CellWidthZ - 1, y,
              x + CellWidthZ - 1, y + CellHeightZ,
              clSelectionArea1, clSelectionArea2);
          break;
        end;
      end;
    end
    else
    begin
      if not skipupdate then
        cell := Page.Rows[row].Cells[col];

      DrawCellEx(cnv, x, y, row, col, skipUpdate, true, cell.Chr, cell.Attr);

      // draw object edges
      if objnum = SelectedObject then
      begin
        cl1 := clSelectedObject1;
        cl2 := clSelectedObject2;
      end
      else
      begin
        cl1 := clUnselectedObject1;
        cl2 := clUnselectedObject2;
      end;

      if not HasBits(neighbors, NEIGHBOR_NORTH) then
        DrawDashLine(cnv, x, y, x + CellWidthZ, y, cl1, cl2);

      if not HasBits(neighbors, NEIGHBOR_SOUTH) then
        DrawDashLine(cnv, x, y + CellHeightZ - 1, x + CellWidthZ, y + CellHeightZ - 1, cl1, cl2);

      if not HasBits(neighbors, NEIGHBOR_WEST) then
        DrawDashLine(cnv, x, y, x, y + CellHeightZ, cl1, cl2);

      if not HasBits(neighbors, NEIGHBOR_EAST) then
        DrawDashLine(cnv, x + CellWidthZ - 1, y, x + CellWidthZ - 1, y + CellHeightZ, cl1, cl2);

    end;
  end;
end;

procedure TfMain.sePageSizeChange(Sender: TObject);
begin
  // resize document
  if not SkipResize then
  begin
    NumRows := seRows.Value;
    NumCols := seCols.Value;
    ResizePage;
    GenerateBmpPage;
    ResizeScrolls;
  end;
end;

// add to selection. no dupes allowed
procedure SelectionAdd(var selection : TLocList; r, c : integer);
var
  neighbor : integer;
  i, l : integer;
begin
  neighbor := 0;
  l := length(selection);
  for i := 0 to l - 1 do
  begin
    if (selection[i].Row = r) and (selection[i].Col = c) then
      exit;
    if (selection[i].Row = r - 1) and (selection[i].Col = c) then
    begin
      SetBit(selection[i].Neighbors, NEIGHBOR_SOUTH, true);
      SetBit(neighbor, NEIGHBOR_NORTH, true);
    end;
    if (selection[i].Row = r + 1) and (selection[i].Col = c) then
    begin
      SetBit(selection[i].Neighbors, NEIGHBOR_NORTH, true);
      SetBit(neighbor, NEIGHBOR_SOUTH, true);
    end;
    if (selection[i].Row = r) and (selection[i].Col = c - 1) then
    begin
      SetBit(selection[i].Neighbors, NEIGHBOR_EAST, true);
      SetBit(neighbor, NEIGHBOR_WEST, true);
    end;
    if (selection[i].Row = r) and (selection[i].Col = c + 1) then
    begin
      SetBit(selection[i].Neighbors, NEIGHBOR_WEST, true);
      SetBit(neighbor, NEIGHBOR_EAST, true);
    end;
  end;
  setlength(selection, l + 1);
  selection[l].Neighbors := neighbor;
  selection[l].Row := r;
  selection[l].Col := c;
end;

// create an object from the CopySelection data.
function TfMain.CopySelectionToObject : TObj;
var
  i, l :        integer;
  r, c :        integer;
  w, h :        integer;
  maxr, maxc,
  minr, minc :  integer;
  p :           integer;
begin
  // get bounding box of copy selection
  l := length(CopySelection);
  maxr := 0;
  maxc := 0;
  minr := 99999;
  minc := 99999;
  for i := 0 to l - 1 do
  begin
    r := CopySelection[i].Row;
    c := CopySelection[i].Col;
    minr := min(minr, r);
    minc := min(minc, c);
    maxr := max(maxr, r);
    maxc := max(maxc, c);
  end;
  w := maxc - minc + 1;
  h := maxr - minr + 1;
  result.Width := w;
  result.Height := h;

  // allocate space for data and clear
  setlength(result.Data, w * h);
  for i := 0 to (w * h) - 1 do
  begin
    result.Data[i].Attr := $0007;
    result.Data[i].Chr :=  EMPTYCHAR;  // empty cell
  end;

  // copyselection moved in
  for i := 0 to l - 1 do
  begin
    r := CopySelection[i].Row;
    c := CopySelection[i].Col;
    p := ((r - minr) * w) + (c - minc);
    result.Data[p] := Page.Rows[r].Cells[c];
  end;

  // normalize : make ul of bounding box 0,0 for object
  result.Row := 0;
  result.Col := 0;
  result.Locked:=false;
  result.Hidden:=false;
  result.Page:=false;
  result.Name := '[Clipboard]';
end;

function TfMain.BuildDisplayCopySelection : TLocList;
var
  i, l, r, c, r1, c1, r2, c2 : integer;
begin
  setlength(result, 0);
  if drag then
  begin
    r1 := MouseRow;
    c1 := MouseCol;
    r2 := dragRow;
    c2 := dragCol;
    if r2 < r1 then Swap(r1, r2);
    if c2 < c1 then Swap(c1, c2);
    if dragType = DRAG_ADD then
      // add these straight up
      for r := r1 to r2 do
        for c := c1 to c2 do
          SelectionAdd(result, r, c);
  end;
  l := length(CopySelection);
  for i := 0 to l - 1 do
  begin
    // add if not already in selection and not inside a remove drag
    if drag and (dragType = DRAG_REMOVE) then
    begin
      if not between(CopySelection[i].Row, r1, r2)
        or not between(CopySelection[i].Col, c1, c2) then
        SelectionAdd(result, CopySelection[i].Row, CopySelection[i].Col);
    end
    else
      SelectionAdd(result, CopySelection[i].Row, CopySelection[i].Col);
  end;
end;

procedure TfMain.lvObjectsDrawItem(Sender: TCustomListView; AItem: TListItem;
  ARect: TRect; AState: TOwnerDrawState);
var
  cnv : TCanvas;
  bmp : TBitmap;
  val : string;
begin
  // draw lock / hidden icons and name
  // 56/57 = unlock/lock
  // 58/59 = hidden/shown
  cnv := Sender.Canvas;
  bmp := TBitmap.Create;
  bmp.PixelFormat:=pf32bit;
  bmp.SetSize(16,16);

  // draw row backgrounds
  if LCLType.odSelected in AState then
  begin
    cnv.Brush.Color := clHighlight;
    cnv.Font.Color := clHighlightText;
  end
  else
  begin
    cnv.Brush.Color := clBtnFace;
    cnv.Font.Color := clBtnText;
  end;

  cnv.FillRect(ARect);
  val := AItem.Caption;
  if val.Chars[0] = '0' then
    ilButtons.GetBitmap(56, bmp)
  else
    ilButtons.GetBitmap(57, bmp);
  cnv.Draw(ARect.Left + 6, ARect.Top,bmp);
  if val.Chars[1] = '1' then
    ilButtons.GetBitmap(58, bmp)
  else
    ilButtons.GetBitmap(59, bmp);
  cnv.Draw(ARect.Left + 24, ARect.Top,bmp);
  bmp.free;
  cnv.Brush.Style := bsClear;
  cnv.TextOut(ARect.Left + 48, ARect.Top, val.substring(2));
end;

procedure TfMain.lvObjectsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  lv : TListView;
  itm : TListItem;
  cv : char;
begin
  // click on lock/unlock hide/shown
  lv := TListView(Sender);
  itm := lv.GetItemAt(X, Y);
  SelectedObject := lv.ItemIndex;
  if between(X, 6, 6 + 16) then
  begin
    cv := itm.Caption.Chars[0];
    if cv = '0' then
      cv := '1'
    else
      cv := '0';
    itm.Caption := cv + itm.Caption.substring(1);
    Objects[SelectedObject].Locked := (cv = '1');
    lvObjects.Invalidate;
  end
  else if between(X, 24, 24 + 16) then
  begin
    cv := itm.Caption.Chars[1];
    if cv = '0' then
      cv := '1'
    else
      cv := '0';
    itm.Caption := itm.Caption.Chars[0] + cv + itm.Caption.substring(2);
    Objects[SelectedObject].Hidden := (cv = '1');
    lvObjects.Invalidate;
  end;
  pbPage.Invalidate;
end;

procedure TfMain.RefreshObject(objnum : integer);
var
  r, c : integer;
  rr, cc : integer;
begin
  if objnum >= 0 then
    for r := Objects[objnum].Height - 1 downto 0 do
      for c := Objects[objnum].Width - 1 downto 0 do
      begin
        rr := Objects[objnum].Row + r;
        cc := Objects[objnum].Col + c;
        if between(rr, 0, NumRows - 1) and between(cc, 0, NumCols - 1) then
          DrawCell(rr, cc);
      end;
end;

procedure TfMain.ObjFlipHorz;
var
  po :            PObj;
  i, j, r, c :    integer;
  p1, p2 :        integer;
  tmp :           TCell;
  fnt :           integer;
  cp :            TEncoding;
  chr, flipchr :  integer;
begin
  if SelectedObject >= 0 then
  begin
    po := @Objects[SelectedObject];
    for r := 0 to po^.Height - 1 do
    begin
      // flip this rows character positions
      for c := 1 to po^.Width div 2 do
      begin
        p1 := (r * po^.Width) + (c - 1);
        p2 := ((r + 1) * po^.Width) - 1 - (c - 1);
        tmp := po^.Data[p1];
        po^.Data[p1] := po^.Data[p2];
        po^.Data[p2] := tmp;
      end;
      //  flip the characters in the row if we can
      for c := 0 to po^.Width - 1 do
      begin
        p1 := (r * po^.Width) + c;

        // get encoding number for this character
        fnt := GetBits(po^.Data[p1].Attr, A_CELL_FONT_MASK, 28);
        cp := Fonts[fnt];

        chr := GetUnicode(po^.Data[p1]);
        i := 0;
        while i < CPages[cp].MirrorTableSize do
        begin
          if CPages[cp].MirrorTable[i] = chr then
          begin
            flipchr := CPages[cp].MirrorTable[i + 1];
            // convert flip unicode back to this codepage
            for j := 0 to 255 do
              if flipchr = CPages[cp].EncodingLUT[j] then
              begin
                po^.Data[p1].Chr := j;
                break;
              end;
            break;
          end;
          i += 3;
        end;
      end;
    end;
    pbPage.Invalidate;
  end;
end;

procedure TfMain.ObjFlipVert;
var
  po :            PObj;
  i, j, r, c :    integer;
  p1, p2 :        integer;
  tmp :           TCell;
  fnt :           integer;
  cp :            TEncoding;
  chr, flipchr :  integer;
begin
  if SelectedObject >= 0 then
  begin
    po := @Objects[SelectedObject];
    for c := 0 to po^.Width - 1 do
    begin
      // flip this rows character positions
      for r := 1 to po^.Height div 2 do
      begin
        p1 := ((r - 1) * po^.Width) + c;
        p2 := ((po^.Height - r) * po^.Width) + c;
        tmp := po^.Data[p1];
        po^.Data[p1] := po^.Data[p2];
        po^.Data[p2] := tmp;
      end;
      //  flip the characters in the row if we can
      for r := 0 to po^.Height - 1 do
      begin
        p1 := (r * po^.Width) + c;

        // get encoding number for this character
        fnt := GetBits(po^.Data[p1].Attr, A_CELL_FONT_MASK, 28);
        cp := Fonts[fnt];

        chr := GetUnicode(po^.Data[p1]);
        i := 0;
        while i < CPages[cp].MirrorTableSize do
        begin
          if CPages[cp].MirrorTable[i] = chr then
          begin
            flipchr := CPages[cp].MirrorTable[i + 2];
            // convert flip unicode back to this codepage
            for j := 0 to 255 do
              if flipchr = CPages[cp].EncodingLUT[j] then
              begin
                po^.Data[p1].Chr := j;
                break;
              end;
            break;
          end;
          i += 3;
        end;
      end;
    end;
    pbPage.Invalidate;
  end;
end;

procedure TfMain.ObjMoveBack;
var
  tmp : TObj;
begin
  if SelectedObject > 0 then
  begin
    // swap objects
    tmp := Objects[SelectedObject];
    Objects[SelectedObject] := Objects[SelectedObject - 1];
    Objects[SelectedObject - 1] := tmp;
    SelectedObject -= 1;
    LoadlvObjects;
    lvObjects.ItemIndex := SelectedObject;
    pbPage.Invalidate;
  end;
end;

procedure TfMain.ObjMoveForward;
var
  tmp : TObj;
begin
  if SelectedObject < length(Objects) - 1 then
  begin
    // swap objects
    tmp := Objects[SelectedObject];
    Objects[SelectedObject] := Objects[SelectedObject + 1];
    Objects[SelectedObject + 1] := tmp;
    SelectedObject += 1;
    LoadlvObjects;
    lvObjects.ItemIndex := SelectedObject;
    pbPage.Invalidate;
  end;
end;

procedure TfMain.ObjMoveToBack;
var
  tmp : TObj;
begin
  if SelectedObject > 0 then
  begin
    tmp := Objects[0];
    Objects[0] := Objects[SelectedObject];
    Objects[SelectedObject] := tmp;
    SelectedObject := 0;
    LoadlvObjects;
    lvObjects.ItemIndex := SelectedObject;
    pbPage.Invalidate;
  end;
end;

procedure TfMain.ObjMoveToFront;
var
  l : integer;
  tmp : TObj;
begin
  l := length(Objects);
  if SelectedObject < l - 1 then
  begin
    tmp := Objects[l - 1];
    Objects[l - 1] := Objects[SelectedObject];
    Objects[SelectedObject] := tmp;
    SelectedObject := l - 1;
    LoadlvObjects;
    lvObjects.ItemIndex := SelectedObject;
    pbPage.Invalidate;
  end;
end;

procedure TfMain.RemoveObject(objnum : integer);
var
  i : integer;
begin
  for i := objnum + 1 to length(Objects) - 1 do
    Objects[i - 1] := Objects[i];
  Setlength(Objects, length(Objects) - 1);
end;

procedure TfMain.ObjMerge;
var
  po : PObj;
  pd : PCell;
  r, c : integer;
begin
  // merge this object to page. and remove object
  if SelectedObject >= 0 then
  begin
    po := @Objects[SelectedObject];
    pd := @po^.Data[0];
    for r := po^.Row to po^.Row + po^.Height - 1 do
      for c := po^.Col to po^.Col + po^.Width - 1 do
      begin
        if between(r, 0, NumRows - 1) and between(c, 0, NumCols - 1) then
          if pd^.Chr <> EMPTYCHAR then
          begin
            Page.Rows[r].Cells[c] := pd^;
            DrawCell(r, c, false);
          end;
        pd += 1;
      end;
    RemoveObject(SelectedObject);
    SelectedObject := -1;
    LoadlvObjects;
    lvObjects.ItemIndex := SelectedObject;
    pbPage.Invalidate;
  end;
end;

procedure TfMain.ObjNext;
begin
  if length(Objects) >= 0 then
  begin
    SelectedObject += 1;
    if SelectedObject >= length(Objects) then
      SelectedObject := 0;
    lvObjects.ItemIndex := SelectedObject;
    pbPage.Invalidate;
  end;
end;

procedure TfMain.ObjPrev;
begin
  if length(Objects) >= 0 then
  begin
    SelectedObject -= 1;
    if SelectedObject < 0 then
      SelectedObject := length(Objects) - 1;
    lvObjects.ItemIndex := SelectedObject;
    pbPage.Invalidate;
  end;
end;

procedure TfMain.bObjMoveBackClick(Sender: TObject);
begin
  ObjMoveBack;
end;

procedure TfMain.bObnjMoveForwardClick(Sender: TObject);
begin
  ObjMoveForward;
end;

procedure TfMain.bObjMoveToBackClick(Sender: TObject);
begin
  ObjMoveToBack;
end;

procedure TfMain.bObjMoveToFrontClick(Sender: TObject);
begin
  ObjMoveToFront;
end;

procedure TfMain.bObjMergeClick(Sender: TObject);
begin
  ObjMerge;
end;

procedure TfMain.bObjFlipHorzClick(Sender: TObject);
begin
  ObjFlipHorz;
end;

procedure TfMain.bObjFlipVertClick(Sender: TObject);
begin
  ObjFlipVert;
end;

procedure TfMain.miObjPrevClick(Sender: TObject);
begin
  ObjPrev;
end;

procedure TfMain.miObjNextClick(Sender: TObject);
begin
  ObjNext;
end;

// draw the document
procedure TfMain.pbPagePaint(Sender: TObject);
var
  panel :     TPaintBox;
  cnv :       TCanvas;
  r, c :      integer;
  pr :        TRect;
  tmp, tmp2 : TBGRABitmap;
  tmpreg :    TLocList;
  i :         integer;
  objonrow :  boolean;
  x, y :      integer;
  cell :      TCell;
  objnum :    integer;
  neighbors : byte;
  cl1, cl2 :  TColor;

begin
  if bmpPage = nil then exit;

  panel := TPaintBox(Sender);
  cnv := panel.Canvas;

  // clear page : todo : border
  cnv.Brush.Color := AnsiColor[GetBits(Page.PageAttr, A_PAGE_PAGE_MASK)];
  cnv.FillRect(0, 0, cnv.Width, cnv.Height);

  // extract displayable part unscaled
  pr.Top := PageTop * CellHeight;
  pr.Left := PageLeft * CellWidth;
  if PageLeft + WindowCols >= NumCols then
    pr.Width := (NumCols - PageLeft) * CellWidth
  else
    pr.Width := WindowCols * CellWidth;

  if PageTop + WindowRows >= NumRows then
    pr.Height := (NumRows - PageTop) * CellHeight
  else
    pr.Height := WindowRows * CellHeight;

  tmp := bmpPage.GetPart(pr) as TBGRABitmap;

  // scale up for display
  pr.Top := 0;
  pr.Left := 0;
  if PageLeft + WindowCols >= NumCols then
    pr.Width := (NumCols - PageLeft) * CellWidthZ
  else
    pr.Width := WindowCols * CellWidthZ;

  if PageTop + WindowRows >= NumRows then
    pr.Height := (NumRows - PageTop) * CellHeightZ
  else
    pr.Height := WindowRows * CellHeightZ;

  if PageZoom < 1 then
    tmp2 := tmp.Resample(pr.Width, pr.Height, rmFineResample) as TBGRABitmap
  else
    tmp2 := tmp.Resample(pr.Width, pr.Height, rmSimpleStretch) as TBGRABitmap;

  // display page from bitmap
  tmp2.Draw(cnv, 0, 0);
  tmp.Free;
  tmp2.free;

  // draw selection information
  tmpreg := BuildDisplayCopySelection;
  for i := length(tmpreg) - 1 downto 0 do
  begin
    pr.Top := (tmpreg[i].Row - PageTop) * CellHeightZ;
    pr.Left := (tmpreg[i].Col - PageLeft) * CellWidthZ;
    pr.Width := CellWidthZ;
    pr.Height := CellHeightZ;
    if not HasBits(tmpreg[i].Neighbors, NEIGHBOR_NORTH) then
      DrawDashLine(cnv,
        pr.Left, pr.Top,
        pr.Left + CellWidthZ, pr.Top,
        clSelectionArea1, clSelectionArea2);
    if not HasBits(tmpreg[i].Neighbors, NEIGHBOR_SOUTH) then
      DrawDashLine(cnv,
        pr.Left, pr.Top + CellHeightZ - 1,
        pr.Left + CellWidthZ, pr.Top + CellHeightZ - 1,
        clSelectionArea1, clSelectionArea2);
    if not HasBits(tmpreg[i].Neighbors, NEIGHBOR_WEST) then
      DrawDashLine(cnv,
        pr.Left, pr.Top,
        pr.Left, pr.Top + CellHeightZ - 1,
        clSelectionArea1, clSelectionArea2);
    if not HasBits(tmpreg[i].Neighbors, NEIGHBOR_EAST) then
      DrawDashLine(cnv,
        pr.Left + CellWidthZ - 1, pr.Top,
        pr.Left + CellWidthZ - 1, pr.Top + CellHeightZ,
        clSelectionArea1, clSelectionArea2);
  end;
  setlength(tmpreg, 0);

  // draw objects over top
  // from topmost to bottommost
  for r := PageTop to PageTop + WindowRows do
  begin
    if r >= NumRows then break;

    // any objects on this row?
    objonrow := false;
    for i := length(Objects) - 1 downto 0 do
      if (Objects[i].Row <= r) and (Objects[i].Row + Objects[i].Height > r) and (not Objects[i].Hidden) then
      begin
        objonrow := true;
        break;
      end;

    if objonrow then
    begin

      y := (r - PageTop) * CellHeightZ;
      for c := PageLeft to PageLeft + WindowCols do
      begin
        if c >= NumCols then break;

        x := (c - PageLeft) * CellWidthZ;
        objnum := GetObjectCell(r, c, cell, neighbors);
        if cell.Chr <> EMPTYCHAR then
        begin
          // object here.
          DrawCellEx(cnv, x, y,  r, c, true,  false, cell.Chr, cell.Attr);
          if objnum = SelectedObject then
          begin
            cl1 := clSelectedObject1;
            cl2 := clSelectedObject2;
          end
          else
          begin
            cl1 := clUnselectedObject1;
            cl2 := clUnselectedObject2;
          end;
          if not HasBits(neighbors, NEIGHBOR_NORTH) then
            DrawDashLine(cnv,
              x, y,
              x + CellWidthZ, y,
              cl1, cl2);
          if not HasBits(neighbors, NEIGHBOR_SOUTH) then
            DrawDashLine(cnv,
              x, y + CellHeightZ - 1,
              x + CellWidthZ, y + CellHeightZ - 1,
              cl1, cl2);
          if not HasBits(neighbors, NEIGHBOR_WEST) then
            DrawDashLine(cnv,
              x, y,
              x, y + CellHeightZ,
              cl1, cl2);
          if not HasBits(neighbors, NEIGHBOR_EAST) then
            DrawDashLine(cnv,
              x + CellWidthZ - 1, y,
              x + CellWidthZ - 1, y + CellHeightZ,
              cl1, cl2);
        end;
      end;
    end;
  end;

  // draw page guidelines
  r := floor((NumRows - PageTop) * CellHeightZ);
  c := floor((NumCols - PageLeft) * CellWidthZ);
  DrawDashLine(cnv, 0, r, pbPage.Width, r, clPageBorder1, clPageBorder2);
  DrawDashLine(cnv, c, 0, c, pbPage.Height, clPageBorder1, clPageBorder2);

end;

procedure TfMain.UpdatePreview;
begin
  // update the preview.
  if Assigned(fPreviewBox) then
    if fPreviewBox.Visible then
      fPreviewBox.Invalidate;
end;

// draw the current cell preview
procedure TfMain.pbCurrCellPaint(Sender: TObject);
var
  pb : TPaintBox;
  cnv : TCanvas;
  r, rarea : TRect;
  bmp : TBGRABitmap;
  off, ch : integer;
  cp : TEncoding;
  h, w : integer;
  fg, bg : integer;
const
  crsize = 38;
begin
  pb := TPaintBox(Sender);
  cnv := pb.Canvas;

  ch := CurrChar;
  cp := Fonts[GetBits(CurrAttr, A_CELL_FONT_MASK, 28)];
  fg := GetBits(CurrAttr, A_CELL_FG_MASK);
  bg := GetBits(CurrAttr, A_CELL_BG_MASK, 8);

  // get codepage / offset for this ch
  if (cp = encUTF8) or (cp = encUTF16) then
    off := GetGlyphOff(CurrChar, CPages[cp].GlyphTable, CPages[cp].GlyphTableSize)
  else
  begin
    if ch > 255 then ch := 0;
    off := CPages[cp].QuickGlyph[ch];
  end;

  // draw right side
  rarea := pb.ClientRect;
  rarea.left := rarea.Right - rarea.Height;
  rarea.width := rarea.height;
  r := rarea;

  // change this to background color in VTX mode
  cnv.Brush.Color := ANSIColor[16];
  cnv.FillRect(r);

  bmp := TBGRABitmap.Create(CellWidth, CellHeight);
  GetGlyphBmp(bmp, CPages[cp].GlyphTable, off, CurrAttr, false);
  w := 40;
  h := 80;
  r.left := rarea.left + (rarea.Width - w) >> 1;
  r.top :=  rarea.top + (rarea.Height - h) >> 1;
  r.width := w;
  r.height := h;
  bmp.Draw(cnv, r);

  // draw colors
  rarea := pb.ClientRect;
  w := (rarea.height >> 1);
  h := (rarea.height >> 1);
  rarea.width := w;
  rarea.height := h;

  // draw background color
  r := rarea;
  r.Left += w - crsize;
  r.Top += h - crsize + 2;
  r.width := crsize;
  r.height := crsize;
  DrawRectangle(cnv, r, clBlack);
  r.inflate(-1,-1);
  cnv.brush.color := AnsiColor[bg];
  cnv.FillRect(r);

  // draw foreground color
  r := rarea;
  r.top += 2;
  r.width := crsize;
  r.height := crsize;
  cnv.brush.color := AnsiColor[fg];
  cnv.FillRect(r);
  DrawRectangle(cnv, r, clBlack);

  // draw plain character
  rarea := pb.ClientRect;
  w := (rarea.height >> 1);
  h := (rarea.height >> 1);
  rarea.top += h;
  rarea.width := w;
  rarea.height := h;

  GetGlyphBmp(bmp, CPages[cp].GlyphTable, off, $000F, false);
  w := 20;
  h := 40;
  r.left := rarea.left + (rarea.Width - w) >> 1;
  r.top :=  rarea.bottom - h - 2;
  r.width := w;
  r.height := h;
  bmp.Draw(cnv, r);
  bmp.free;
end;

procedure TfMain.pbPreviewClick(Sender: TObject);
begin
  if fPreviewBox.Visible then
    fPreviewBox.Hide
  else
    fPreviewBox.Show;
end;

procedure TfMain.pbRulerLeftPaint(Sender: TObject);
var
  pb : TPaintBox;
  cnv : TCanvas;
  r, x, y : integer;
  my : integer;
  rect : Trect;
  nonum : boolean;
begin
  pb := TPaintBox(Sender);
  cnv := pb.Canvas;
  with cnv do
  begin
    Pen.Color := clBlack;
    Brush.Style := bsClear;
    Font.Color := clBlack;
    Font.Size := 6;
    rect.Left := 1;
    for r := 0 to WindowRows - 1 do
    begin
      if PageTop + r > NumRows then break;

      y := r * CellHeightZ;
      x := 3;
      nonum := true;
      if ((PageTop + r) mod 10) = 0 then
      begin
        if PageTop + r > 0 then
        begin
          rect.Top := y - 10;
          rect.Width := 13;
          rect.Height := 10;
          DrawTextRight(cnv, rect, IntToStr(PageTop + r));
          nonum := false;
        end;
        x := 8
      end
      else if ((PageTop + r) mod 5) = 0 then
        x := 5;

      if (PageZoom > 2) and nonum then
      begin
        rect.Top := y - 10;
        rect.Width := 13;
        rect.Height := 10;
        DrawTextRight(cnv, rect, IntToStr(PageTop + r));
      end;
      Line(pb.Width - x, y, pb.Width - 1, y);
    end;
    // draw row marker
    if MouseRow >= 0 then
    begin
      Brush.Color := clBlack;
      my := (MouseRow - PageTop) * CellHeightZ;
      FillRect(1, my + 1, 3, my + CellHeightZ - 1);
    end;
  end;
end;

procedure TfMain.pbRulerTopPaint(Sender: TObject);
var
  pb :      TPaintBox;
  cnv :     TCanvas;
  c, x, y : integer;
  mx :      integer;
  rect :    TRect;
  nonum :   boolean;
begin
  pb := TPaintBox(Sender);
  cnv := pb.Canvas;
  with cnv do
  begin
    Pen.Color := clBlack;
    Brush.Style := bsClear;
    Font.Color := clBlack;
    Font.Size := 6;
    rect.Top := 1;
    for c := 0 to WindowCols - 1 do
    begin
      if PageLeft + c > NumCols then break;

      x := pbRulerLeft.Width + c * CellWidthZ;
      y := 3;
      nonum := true;
      if ((PageLeft + c) mod 10) = 0 then
      begin
        if PageLeft + c > 0 then
        begin
          rect.Left := x - 17;
          rect.Width := 16;
          rect.Height := 10;
          DrawTextRight(cnv, rect, IntToStr(PageLeft+c));
          nonum := false;
        end;
        y := 8
      end
      else if ((PageLeft + c) mod 5) = 0 then
        y := 5;

      if (PageZoom > 2) and nonum then
      begin
        rect.Left := x - 17;
        rect.Width := 16;
        rect.Height := 10;
        DrawTextRight(cnv, rect, IntToStr(PageLeft+c));
      end;

      Line(x, pb.Height - y, x, pb.Height - 1);
    end;
    // draw row marker
    if MouseRow >= 0 then
    begin
      Brush.Color := clBlack;
      mx := (MouseCol - PageLeft) * CellWidthZ;
      FillRect(pbRulerLeft.Width + mx + 1, 1, pbRulerLeft.Width + mx + CellWidthZ - 1, 3);
    end;
  end;
end;

// draw cell at row, col at x, y of cnv (also copy to bmpPage)
// draw topmost cell from any objects first. exit after blocking object cell has
// been drawn.
procedure TfMain.DrawCellEx(
  cnv : TCanvas;
  x, y,
  row, col : integer;
  skipUpdate : boolean = true;
  skipDraw : boolean = false;
  usech : uint16 = EMPTYCHAR;   // overrides
  useattr : uint32 = $FFFF);
var
  bmp, bmp2 :     TBGRABitmap;
  ch :            Uint16;
  off :           integer;
  attr :          Uint32;
  rect :          TRect;
  bslow, bfast :  boolean;
  cp :            TEncoding;
begin
  if bmpPage = nil then exit; // ?!

  if between(row, PageTop, PageTop + WindowRows)
    and Between(col, PageLeft, PageLeft + WindowCols) then
  begin
    // on screen.

    // load page cell
    if usech = EMPTYCHAR then
    begin
      ch := Page.Rows[row].Cells[col].Chr;
      attr := Page.Rows[row].Cells[col].Attr;
    end else begin
      ch := usech;
      attr := useattr;
    end;

    bslow := HasBits(attr, A_CELL_BLINKSLOW);
    bfast  := HasBits(attr, A_CELL_BLINKFAST);

    // convert value in chr to unicode.
    cp := Fonts[GetBits(attr, A_CELL_FONT_MASK, 28)];
    if (cp = encUTF8) or (cp = encUTF16) then
      off := GetGlyphOff(ch, CPages[CurrCodePage].GlyphTable, CPages[CurrCodePage].GlyphTableSize)
    else
    begin
      if ch > 255 then ch := 0;
      off := CPages[CurrCodePage].QuickGlyph[ch];
    end;

    // create bmp for cell
    bmp := TBGRABitmap.Create(8,16);
    if not skipUpdate then
    begin
      GetGlyphBmp(bmp, CPages[CurrCodePage].GlyphTable, off, attr, false);
      bmp.Draw(bmpPage.Canvas, col * CellWidth, row * CellHeight);

      bmp.ResampleFilter := rfMitchell;
      bmp2 := bmp.Resample(CellWidth>>2, CellHeight >>2) as TBGRABitmap;
      bmp2.Draw(bmpPreview.Canvas, col * (CellWidth >> 2), row * (CellHeight >> 2));
      bmp2.free;

      UpdatePreview;

      if bslow and not BlinkSlow then
      begin
        SetBits(attr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_CONCEAL); // hide blink
        GetGlyphBmp(bmp, CPages[CurrCodePage].GlyphTable, off, attr, false);
      end;

      if bfast and not BlinkFast and (ColorScheme <> COLORSCHEME_ICE) then
      begin
        SetBits(attr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_CONCEAL); // hide blink
        GetGlyphBmp(bmp, CPages[CurrCodePage].GlyphTable, off, attr, false);
      end;
    end
    else
    begin
      if bslow and not BlinkSlow then
        SetBits(attr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_CONCEAL); // hide blink
      if bfast and not BlinkFast and (ColorScheme <> COLORSCHEME_ICE) then
        SetBits(attr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_CONCEAL); // hide blink
      GetGlyphBmp(bmp, CPages[CurrCodePage].GlyphTable, off, attr, false);
    end;

    // plop it down- scale if needed
    if not skipDraw then
    begin
      rect.Top := y;
      rect.Left := x;
      rect.Width := CellWidthZ;
      rect.Height := CellHeightZ;
      if PageZoom < 1 then
      begin
        bmp.ResampleFilter:=rfMitchell;
        BGRAReplace(bmp, bmp.Resample(CellWidthZ, CellHeightZ));
      end;
      bmp.Draw(cnv, rect);
    end;
    bmp.free;
  end;
end;

function GetKeyAction(str : string) : integer;
var
  i : integer;
begin
  for i := 0 to length(KeyActions)-1 do
    if str.ToUpper = KeyActions[i].ToUpper then
      exit(i);
  result := -1;
end;

procedure TfMain.LoadSettings;
var
  iin : TIniFile;
  q : TQuad;
  keys : TStringList;
  subkeys : TStringArray;
  keyval0, keyval1, tmp : string;
  i, j, len : integer;
  shortcut : integer;
const
  sect : unicodestring = 'VTXEdit';

begin
  iin := TIniFile.Create('vtxedit.ini');

  q := StrToQuad(iin.ReadString(sect, 'Window','64,64 640,480'));
  SetFormQuad(fMain, q);

  q := StrToQuad(iin.ReadString(sect, 'PreviewBox', '64,64 640,480'));
  q.v2 := 0;
  SetFormQuad(fPreviewBox, q);

  if iin.ReadBool(sect, 'PreviewBoxOpen', false) then fPreviewBox.Show;
  if iin.ReadBool(sect, 'WindowMax', false) then fMain.WindowState := wsMaximized;

  keys := TStringList.Create;
  if iin.SectionExists('KeyBinds') then
  begin
    iin.ReadSectionValues('KeyBinds', keys);
    setlength(KeyBinds, keys.Count);
    for i := 0 to keys.Count - 1 do
    begin
      tmp := keys[i];

      len := tmp.IndexOf('=');
      if len = -1 then continue;

      keyval0 := tmp.substring(0, len);
      keyval1 := tmp.substring(len + 1);

      KeyBinds[i].Ctrl := false;
      KeyBinds[i].Shift := false;
      KeyBinds[i].Alt := false;
      KeyBinds[i].KeyStr := '';

      tmp := keyval0.ToUpper;
      subkeys := tmp.Split(['+']);
      for j := 0 to length(subkeys) - 1 do
      begin
        case subkeys[j] of
          'CTRL': Keybinds[i].Ctrl := true;
          'SHIFT': Keybinds[i].Shift := true;
          'ALT':   KeyBinds[i].Alt := true;

          'BACK':
            begin
              KeyBinds[i].KeyCode := 8;
              keybinds[i].KeyStr := 'Backspace';
            end;

          'TAB':
            begin
              KeyBinds[i].KeyCode := 9;
              keybinds[i].KeyStr := 'Tab';
            end;

          'CLEAR':
            begin
              KeyBinds[i].KeyCode := 12;
              keybinds[i].KeyStr := 'Clear';
            end;

          'RETURN','ENTER':
            begin
              KeyBinds[i].KeyCode := 13;
              keybinds[i].KeyStr := 'Return';
            end;

          'PAUSE':
            begin
              KeyBinds[i].KeyCode := 19;
              keybinds[i].KeyStr := 'Pause';
            end;

          'ESCAPE','ESC':
            begin
              KeyBinds[i].KeyCode := 27;
              keybinds[i].KeyStr := 'Esc';
            end;

          'SPACE':
            begin
              KeyBinds[i].KeyCode := 32;
              keybinds[i].KeyStr := 'Space';
            end;

          'PRIOR', 'PGUP':
            begin
              KeyBinds[i].KeyCode := 33;
              keybinds[i].KeyStr := 'PgUp';
            end;
          'NEXT', 'PGDN':
            begin
              KeyBinds[i].KeyCode := 34;
              keybinds[i].KeyStr := 'PgDn';
            end;

          'END':
            begin
              KeyBinds[i].KeyCode := 35;
              keybinds[i].KeyStr := 'End';
            end;
          'HOME':
            begin
              KeyBinds[i].KeyCode := 36;
              keybinds[i].KeyStr := 'Home';
            end;

          'LEFT':
            begin
              KeyBinds[i].KeyCode := 37;
              keybinds[i].KeyStr := 'Left';
            end;
          'UP':
            begin
              KeyBinds[i].KeyCode := 38;
              keybinds[i].KeyStr := 'Up';
            end;
          'RIGHT':
            begin
              KeyBinds[i].KeyCode := 39;
              keybinds[i].KeyStr := 'Right';
            end;
          'DOWN':
            begin
              KeyBinds[i].KeyCode := 40;
              keybinds[i].KeyStr := 'Down';
            end;

          'SNAPSHOT','PRTSCR','PRINTSCREEN':
            begin
              KeyBinds[i].KeyCode := 44;
              keybinds[i].KeyStr := 'PrtScr';
            end;

          'INSERT','INS':
            begin
              KeyBinds[i].KeyCode := 45;
              keybinds[i].KeyStr := 'Insert';
            end;

          'DELETE','DEL':
            begin
              KeyBinds[i].KeyCode := 46;
              keybinds[i].KeyStr := 'Delete';
            end;

          '0':
            begin
              KeyBinds[i].KeyCode := 48;
              keybinds[i].KeyStr := subkeys[j];
            end;
          '1':
            begin
              KeyBinds[i].KeyCode := 49;
              keybinds[i].KeyStr := subkeys[j];
            end;
          '2':
            begin
              KeyBinds[i].KeyCode := 50;
              keybinds[i].KeyStr := subkeys[j];
            end;
          '3':
            begin
              KeyBinds[i].KeyCode := 51;
              keybinds[i].KeyStr := subkeys[j];
            end;
          '4':
            begin
              KeyBinds[i].KeyCode := 52;
              keybinds[i].KeyStr := subkeys[j];
            end;
          '5':
            begin
              KeyBinds[i].KeyCode := 53;
              keybinds[i].KeyStr := subkeys[j];
            end;
          '6':
            begin
              KeyBinds[i].KeyCode := 54;
              keybinds[i].KeyStr := subkeys[j];
            end;
          '7':
            begin
              KeyBinds[i].KeyCode := 55;
              keybinds[i].KeyStr := subkeys[j];
            end;
          '8':
            begin
              KeyBinds[i].KeyCode := 56;
              keybinds[i].KeyStr := subkeys[j];
            end;
          '9':
            begin
              KeyBinds[i].KeyCode := 57;
              keybinds[i].KeyStr := subkeys[j];
            end;


          'A':
            begin
              KeyBinds[i].KeyCode := 65;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'B':
            begin
              KeyBinds[i].KeyCode := 66;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'C':
            begin
              KeyBinds[i].KeyCode := 67;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'D':
            begin
              KeyBinds[i].KeyCode := 68;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'E':
            begin
              KeyBinds[i].KeyCode := 69;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'F':
            begin
              KeyBinds[i].KeyCode := 70;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'G':
            begin
              KeyBinds[i].KeyCode := 71;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'H':
            begin
              KeyBinds[i].KeyCode := 72;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'I':
            begin
              KeyBinds[i].KeyCode := 73;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'J':
            begin
              KeyBinds[i].KeyCode := 74;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'K':
            begin
              KeyBinds[i].KeyCode := 75;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'L':
            begin
              KeyBinds[i].KeyCode := 76;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'M':
            begin
              KeyBinds[i].KeyCode := 77;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'N':
            begin
              KeyBinds[i].KeyCode := 78;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'O':
            begin
              KeyBinds[i].KeyCode := 79;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'P':
            begin
              KeyBinds[i].KeyCode := 80;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'Q':
            begin
              KeyBinds[i].KeyCode := 81;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'R':
            begin
              KeyBinds[i].KeyCode := 82;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'S':
            begin
              KeyBinds[i].KeyCode := 83;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'T':
            begin
              KeyBinds[i].KeyCode := 84;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'U':
            begin
              KeyBinds[i].KeyCode := 85;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'V':
            begin
              KeyBinds[i].KeyCode := 86;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'W':
            begin
              KeyBinds[i].KeyCode := 87;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'X':
            begin
              KeyBinds[i].KeyCode := 88;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'Y':
            begin
              KeyBinds[i].KeyCode := 89;
              keybinds[i].KeyStr := subkeys[j];
            end;
          'Z':
            begin
              KeyBinds[i].KeyCode := 90;
              keybinds[i].KeyStr := subkeys[j];
            end;

          'NUMPAD0':
            begin
              KeyBinds[i].KeyCode := 96;
              keybinds[i].KeyStr := 'NumPad0';
            end;
          'NUMPAD1':
            begin
              KeyBinds[i].KeyCode := 97;
              keybinds[i].KeyStr := 'NumPad1';
            end;
          'NUMPAD2':
            begin
              KeyBinds[i].KeyCode := 98;
              keybinds[i].KeyStr := 'NumPad2';
            end;
          'NUMPAD3':
            begin
              KeyBinds[i].KeyCode := 99;
              keybinds[i].KeyStr := 'NumPad3';
            end;
          'NUMPAD4':
            begin
              KeyBinds[i].KeyCode := 100;
              keybinds[i].KeyStr := 'NumPad4';
            end;
          'NUMPAD5':
            begin
              KeyBinds[i].KeyCode := 101;
              keybinds[i].KeyStr := 'NumPad5';
            end;
          'NUMPAD6':
            begin
              KeyBinds[i].KeyCode := 102;
              keybinds[i].KeyStr := 'NumPad6';
            end;
          'NUMPAD7':
            begin
              KeyBinds[i].KeyCode := 103;
              keybinds[i].KeyStr := 'NumPad7';
            end;
          'NUMPAD8':
            begin
              KeyBinds[i].KeyCode := 104;
              keybinds[i].KeyStr := 'NumPad8';
            end;
          'NUMPAD9':
            begin
              KeyBinds[i].KeyCode := 105;
              keybinds[i].KeyStr := 'NumPad9';
            end;
          'MULTIPLY':
            begin
              KeyBinds[i].KeyCode := 106;
              keybinds[i].KeyStr := 'NumPad*';
            end;
          'ADD':
            begin
              KeyBinds[i].KeyCode := 107;
              keybinds[i].KeyStr := 'NumPad+';
            end;
          'SUBTRACT':
            begin
              KeyBinds[i].KeyCode := 109;
              keybinds[i].KeyStr := 'NumPad-';
            end;
          'DECIMAL':
            begin
              KeyBinds[i].KeyCode := 110;
              keybinds[i].KeyStr := 'NumPad.';
            end;
          'DIVIDE':
            begin
              KeyBinds[i].KeyCode := 111;
              keybinds[i].KeyStr := 'NumPad/';
            end;

          'F1':
            begin
              KeyBinds[i].KeyCode := 112;
              keybinds[i].KeyStr := 'F1';
            end;
          'F2':
            begin
              KeyBinds[i].KeyCode := 113;
              keybinds[i].KeyStr := 'F2';
            end;
          'F3':
            begin
              KeyBinds[i].KeyCode := 114;
              keybinds[i].KeyStr := 'F3';
            end;
          'F4':
            begin
              KeyBinds[i].KeyCode := 115;
              keybinds[i].KeyStr := 'F4';
            end;
          'F5':
            begin
              KeyBinds[i].KeyCode := 116;
              keybinds[i].KeyStr := 'F5';
            end;
          'F6':
            begin
              KeyBinds[i].KeyCode := 117;
              keybinds[i].KeyStr := 'F6';
            end;
          'F7':
            begin
              KeyBinds[i].KeyCode := 118;
              keybinds[i].KeyStr := 'F7';
            end;
          'F8':
            begin
              KeyBinds[i].KeyCode := 119;
              keybinds[i].KeyStr := 'F8';
            end;
          'F9':
            begin
              KeyBinds[i].KeyCode := 120;
              keybinds[i].KeyStr := 'F9';
            end;
          'F10':
            begin
              KeyBinds[i].KeyCode := 121;
              keybinds[i].KeyStr := 'F10';
            end;
          'F11':
            begin
              KeyBinds[i].KeyCode := 122;
              keybinds[i].KeyStr := 'F11';
            end;
          'F12':
            begin
              KeyBinds[i].KeyCode := 123;
              keybinds[i].KeyStr := 'F12';
            end;

          'SEMICOLON':
            begin
              KeyBinds[i].KeyCode := $BA;
              KeyBinds[i].KeyStr := ';';
            end;

          'PLUS':
            begin
              KeyBinds[i].KeyCode := $BB;
              KeyBinds[i].KeyStr := '+';
            end;

          'COMMA':
            begin
              KeyBinds[i].KeyCode := $BC;
              KeyBinds[i].KeyStr := ',';
            end;

          'MINUS':
            begin
              KeyBinds[i].KeyCode := $BD;
              KeyBinds[i].KeyStr := '-';
            end;

          'PERIOD':
            begin
              KeyBinds[i].KeyCode := $BE;
              KeyBinds[i].KeyStr := '.';
            end;

          'SLASH':
            begin
              KeyBinds[i].KeyCode := $BF;
              KeyBinds[i].KeyStr := ',';
            end;

          'ACCENT':
            begin
              KeyBinds[i].KeyCode := $C0;
              KeyBinds[i].KeyStr := '`';
            end;

          'LEFTSQUARE':
            begin
              KeyBinds[i].KeyCode := $DB;
              KeyBinds[i].KeyStr := '[';
            end;

          'BACKSLASH':
            begin
              KeyBinds[i].KeyCode := $DC;
              KeyBinds[i].KeyStr := '\';
            end;

          'RIGHTSQUARE':
            begin
              KeyBinds[i].KeyCode := $DD;
              KeyBinds[i].KeyStr := ']';
            end;

          'QUOTE':
            begin
              KeyBinds[i].KeyCode := $DE;
              KeyBinds[i].KeyStr := '''';
            end;
          else
          begin
            ShowMessage('Unknown Key in .INI file: ' + subkeys[j]);
            break;
          end;
        end;
      end;

      if KeyBinds[i].Ctrl then KeyBinds[i].KeyStr := 'Ctrl+' + KeyBinds[i].KeyStr;
      if KeyBinds[i].Shift then KeyBinds[i].KeyStr := 'Shift+' + KeyBinds[i].KeyStr;
      if KeyBinds[i].Alt then KeyBinds[i].KeyStr := 'Alt+' + KeyBinds[i].KeyStr;

      // action is first word of val
      tmp := keyval1;
      len := tmp.IndexOf(' ');
      if len = -1 then
      begin
        KeyBinds[i].Action := GetKeyAction(tmp);
        KeyBinds[i].Val := '';
      end
      else
      begin
        KeyBinds[i].Action := GetKeyAction(tmp.substring(0,len));
        KeyBinds[i].Val := tmp.Substring(len + 1);
      end;

      // load hints and menu items shortcuts
      shortcut := Keybinds[i].KeyCode;
      if Keybinds[i].Shift then shortcut := (shortcut or $2000);
      if Keybinds[i].Ctrl then shortcut := (shortcut or $4000);
      if Keybinds[i].Alt then shortcut := (shortcut or $8000);
      case KeyBinds[i].Action of
        KA_MODECHARS:
          tbModeCharacter.Hint := tbModeCharacter.Hint + ' ' + KeyBinds[i].KeyStr;

        KA_MODELEFTRIGHTBLOCKS:
          tbModeLeftRights.Hint := tbModeLeftRights.Hint+' ' + KeyBinds[i].KeyStr;

        KA_MODETOPBOTTOMBLOCKS:
          tbModeTopBottoms.Hint := tbModeTopBottoms.Hint+' ' + KeyBinds[i].KeyStr;

        KA_MODEQUARTERBLOCKS:
          tbModeQuarters.Hint := tbModeQuarters.Hint+' ' + KeyBinds[i].KeyStr;

        KA_MODESIXELS:
          tbModeSixels.Hint := tbModeSixels.Hint+' ' + KeyBinds[i].KeyStr;

        KA_TOOLSELECT:
          tbToolSelect.Hint := tbToolSelect.Hint+ ' ' + KeyBinds[i].KeyStr;

        KA_TOOLDRAW:
          tbToolDraw.Hint := tbToolDraw.Hint+' ' + KeyBinds[i].KeyStr;

        KA_TOOLFILL:
          tbToolFill.Hint := tbToolFill.Hint+' ' + KeyBinds[i].KeyStr;

        KA_TOOLLINE:
          tbToolLine.Hint := tbToolLine.Hint+' ' + KeyBinds[i].KeyStr;

        KA_TOOLRECTANGLE:
          tbToolRect.Hint := tbToolRect.Hint+' ' + KeyBinds[i].KeyStr;

        KA_TOOLELLIPSE:
          tbToolEllipse.Hint := tbToolEllipse.Hint + ' ' + KeyBinds[i].KeyStr;

        KA_TOOLEYEDROPPER:  tbToolEyedropper.Hint := tbToolEyedropper.Hint+' ' + KeyBinds[i].KeyStr;

        KA_FILENEW:           miFileNew.ShortCut := shortcut;
        KA_FILEOPEN:          miFileOpen.ShortCut := shortcut;
        KA_FILESAVE:          miFileSave.ShortCut := shortcut;
        KA_FILEEXIT:          miFileExit.ShortCut := shortcut;

        KA_EDITREDO:          miEditRedo.ShortCut := shortcut;
        KA_EDITUNDO:          miEditUndo.ShortCut := shortcut;
        KA_EDITCUT:           miEditCut.ShortCut := shortcut;
        KA_EDITCOPY:          miEditCopy.ShortCut := shortcut;
        KA_EDITPASTE:         miEditPaste.ShortCut := shortcut;

        KA_OBJECTMOVEBACK:    miObjBackOne.ShortCut := shortcut;
        KA_OBJECTMOVEFORWARD: miObjForwardOne.ShortCut := shortcut;
        KA_OBJECTMOVETOBACK:  miObjToBack.ShortCut := shortcut;
        KA_OBJECTMOVETOFRONT: miObjToFront.ShortCut := shortcut;
        KA_OBJECTFLIPHORZ:    miObjFlipHorz.ShortCut := shortcut;
        KA_OBJECTFLIPVERT:    miObjFlipVert.ShortCut := shortcut;
        KA_OBJECTMERGE:       miObjMerge.ShortCut := shortcut;
        KA_OBJECTNEXT:        miObjNext.ShortCut := shortcut;
        KA_OBJECTPREV:        miObjPrev.ShortCut := shortcut;

        KA_SHOWPREVIEW:       miViewPreview.ShortCut := shortcut;

      end;
    end;
  end;
  keys.free;

  iin.free;
end;

procedure TfMain.SaveSettings;
var
  iin : TIniFile;
const
  sect : unicodestring = 'VTXEdit';
begin
  iin := TIniFile.Create('vtxedit.ini');

  // window positions
  iin.WriteString(sect, 'Window', QuadToStr(GetFormQuad(fMain)));
  iin.WriteString(sect, 'PreviewBox', QuadToStr(GetFormQuad(fPreviewBox)));
  iin.WriteBool(sect, 'PreviewBoxOpen', fPreviewBox.Showing);

  iin.WriteBool(sect, 'WindowMax', fMain.WindowState = wsMaximized);

  iin.free;
end;

function TfMain.GetNextCell(r, c : integer) : TRowCol;
var
  c1, r1 : integer;
begin
  c1 := c;
  r1 := r;
  while (Page.Rows[r1].Cells[c1].Chr = _SPACE)
    and (GetBits(Page.Rows[r1].Cells[c1].Attr, A_CELL_BG_MASK, 8) = 0)
    and (r1 < NumRows) do
  begin
    c1 += 1;
    if (c1 >= NumCols) then
    begin
      c1 := 0;
      if r1 = NumRows - 1 then break;
      r1 += 1;
    end;
  end;
  result.row := r1;
  result.col := c1;
end;


procedure TfMain.SaveANSIFile(fname : unicodestring; ansi : unicodestring; enc : TEncoding);
var
  fout : TFileStream;
  l : integer;
  tmp : TBytes;
begin

  fout := TFileStream.Create(fname, fmCreate or fmOpenWrite or fmShareDenyNone);

  // save ansi - mind codepage
  if enc = encUTF8 then
    tmp := ansi.toUTF8Bytes
  else if enc = encUTF16 then
    tmp := ansi.toUTF16Bytes
  else
    tmp := ansi.toCPBytes;

  l := length(tmp);
  fout.WriteBuffer(tmp[0], l);
  setlength(tmp, 0);
  fout.free;
end;

function TfMain.BuildANSI : unicodestring;
var
  ansi :          unicodestring;
  r, c :          integer;      // current r/c
  dc :            integer;      // col delta
  pt :            TRowCol;
  cell :          TCell;
  attr :          integer;
  cattr :         integer;
  fg,bg :         uint32;
  sgr :           unicodestring;

begin
  // for now, save everthing on screen as is

  // reset / home / clear screen.
  ansi := CSI + '0m' + CSI + '2J';
  cattr := $0007;
  r := 0;
  c := 0;

  while r < NumRows do
  begin
    pt := GetNextCell(r,c);

    if pt.row >= NumRows then
      break;

    dc := pt.col - c;
    if pt.row = r then
    begin
      // same row.
      if dc > 0 then
      begin
        if (GetBits(cattr, A_CELL_BG_MASK, 8) = 0) and (dc < 4) then
          ansi += space(dc)
        else
          ansi += CSI + inttostr(dc) + 'C';
        c += dc;
      end
      else
      begin

        // character here. output.
        cell :=   Page.Rows[r].Cells[c];
        attr :=   cell.attr;

        // adjust bold/FG - blink/BG
        fg := GetBits(attr, A_CELL_FG_MASK);
        bg := GetBits(attr, A_CELL_BG_MASK, 8);
        if ColorScheme <> COLORSCHEME_256 then
        begin
          if fg > 7 then
          begin
            fg -= 8;
            SetBit(attr, A_CELL_BOLD, true);
          end;
          if bg > 7 then
          begin
            bg -= 8;
            SetBit(attr, A_CELL_BLINKFAST, true);
          end;
        end;

        if cattr <> attr then
        begin

          sgr := CSI;
          // need to reset ?
          if (not HasBits(attr, A_CELL_BOLD) and HasBits(cattr, A_CELL_BOLD))
            or (not HasBits(attr, A_CELL_FAINT) and HasBits(cattr, A_CELL_FAINT))
            or (not HasBits(attr, A_CELL_ITALICS) and HasBits(cattr, A_CELL_ITALICS))
            or (not HasBits(attr, A_CELL_UNDERLINE) and HasBits(cattr, A_CELL_UNDERLINE))
            or (not HasBits(attr, A_CELL_BLINKFAST) and HasBits(cattr, A_CELL_BLINKFAST))
            or (not HasBits(attr, A_CELL_BLINKSLOW) and HasBits(cattr, A_CELL_BLINKSLOW))
            or (not HasBits(attr, A_CELL_REVERSE) and HasBits(cattr, A_CELL_REVERSE))
            or (not HasBits(attr, A_CELL_STRIKETHROUGH) and HasBits(cattr, A_CELL_STRIKETHROUGH))
            or (not HasBits(attr, A_CELL_DOUBLESTRIKE) and HasBits(cattr, A_CELL_DOUBLESTRIKE))
            or (not HasBits(attr, A_CELL_SHADOW) and HasBits(cattr, A_CELL_SHADOW))
            or ((GetBits(attr, A_CELL_DISPLAY_MASK) <> A_CELL_DISPLAY_CONCEAL)
              and (GetBits(cattr, A_CELL_DISPLAY_MASK) = A_CELL_DISPLAY_CONCEAL))
            or ((GetBits(attr, A_CELL_DISPLAY_MASK) <> A_CELL_DISPLAY_TOP)
              and (GetBits(cattr, A_CELL_DISPLAY_MASK) = A_CELL_DISPLAY_TOP))
            or ((GetBits(attr, A_CELL_DISPLAY_MASK) <> A_CELL_DISPLAY_BOTTOM)
              and (GetBits(cattr, A_CELL_DISPLAY_MASK) = A_CELL_DISPLAY_BOTTOM)) then
            begin
              sgr += '0;';    // reset. use shortest.
              cattr := $0007;
            end;

          // mind the color mode! - besure colors are cleaned up on
          if fg <> GetBits(cattr, A_CELL_FG_MASK) then
          begin
            if fg < 8 then
              sgr += inttostr(30 + integer(fg)) + ';'
            else
              sgr += '38;5;' + inttostr(fg) + ';';
          end;

          if bg <> GetBits(cattr, A_CELL_BG_MASK, 8) then
          begin
            if bg < 8 then
              sgr += inttostr(40 + integer(bg)) + ';'
            else
              sgr += '48;5;' + inttostr(bg) + ';';
          end;

          if HasBits(attr, A_CELL_BOLD) then            sgr += '1;';
          if HasBits(attr, A_CELL_FAINT) then           sgr += '2;';
          if HasBits(attr, A_CELL_ITALICS) then         sgr += '3;';
          if HasBits(attr, A_CELL_UNDERLINE) then       sgr += '4;';
          if HasBits(attr, A_CELL_BLINKSLOW) then       sgr += '5;';
          if HasBits(attr, A_CELL_BLINKFAST) then       sgr += '6;';
          if HasBits(attr, A_CELL_REVERSE) then         sgr += '7;';
          if HasBits(attr, A_CELL_STRIKETHROUGH) then   sgr += '9;';
          if HasBits(attr, A_CELL_DOUBLESTRIKE) then    sgr += '56;';
          if HasBits(attr, A_CELL_SHADOW) then          sgr += '57;';

          if GetBits(attr, A_CELL_DISPLAY_MASK) = A_CELL_DISPLAY_CONCEAL then sgr += '8;';
          if GetBits(attr, A_CELL_DISPLAY_TOP) = A_CELL_DISPLAY_TOP then sgr += '8;';
          if GetBits(attr, A_CELL_DISPLAY_BOTTOM) = A_CELL_DISPLAY_BOTTOM then sgr += '8;';

          sgr := leftstr(sgr, length(sgr) - 1) + 'm';
          ansi += sgr;
          cattr := attr;
        end;

        if c = NumCols - 1 then
          ansi += #13#10#27'[A'#27'['+inttostr(NumCols-1)+'C'#27'[s' + WideChar(cell.chr) + #27'[u'#13#10
        else
          ansi += WideChar(cell.chr);

        c += 1;
        if c >= NumCols then
        begin
          c := 0;
          r += 1;
        end;
      end;
    end
    else
    begin
      // another row.
      ansi += CRLF;
      inc(r);
      c := 0;
    end;
  end;
  result := ansi;
end;




procedure nop; begin end;

procedure DebugStart;
begin

  exit;


  nop;

end;

end.

{
// LOAD BINARY FONT FILE 8x16
// load raw file. 8x8
//  fn := 'MicroKnightPlus_v1.0.raw';
//  fn := 'MicroKnight_v1.0.raw';
//  fn := 'P0T-NOoDLE_v1.0.raw';
//  fn := 'TopazPlus_a1200_v1.0.raw';
//fn := 'Topaz_a1200_v1.0.raw';
var
  fin : TFileStream;
  fout : Text;

  fn : string;
  fsize : integer;
  chars : integer;
  c, b, v : integer;
  enc : integer;
  h : integer = 16;


fn := 'mO''sOul_v1.0.raw';
if fileexists(fn) then
begin

  fin := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  fsize := fin.Size;
  chars := fsize div h;

  system.assign(fout, fn + '.pas');
  rewrite(fout);
  writeln(fout, 'cont SomeFont : array [0..' + inttostr(
    chars * (h + 2)) + '] = ('); // unicode char

  enc := 0;
  for c := 0 to chars - 1 do
  begin
    write(fout, format(' $%2.2x, $%2.2x, | ', [ (enc and $ff00) shr 8, enc and $FF ] ));
    for b := 0 to h - 1 do
    begin
      v := fin.readbyte;
      if (c = chars - 1) and (b = h - 1) then
        write(fout, format('$%2.2x', [ v, v ]))
      else
        write(fout, format('$%2.2x, ', [ v, v ]));
    end;
    writeln(fout, '');
    enc += 1;
  end;
  writeln(fout, ');');
  closefile(fout);
  fin.free;
end;
}

{
// LOAD BDF
  // will need this for other fonts to be brought in.
var
  fin : Text;
  fout : Text;
  lin, key, val : string;
  i, spos, enc : integer;
  img : array [0..15] of byte;

   // load UVGA.bdf save in PASCAL format
  system.assign(fin, 'u_vga16.bdf');
  reset(fin);
  system.assign(fout, 'uvga.pas');
  rewrite(fout);
  writeln(fout, 'cont UVGA16 : array [0.999] of byte = [');

  while not eof(fin) do
  begin
    readln(fin, lin);

    spos := lin.IndexOf(' ');
    key := lin;
    val := '';
    if spos > -1 then
    begin
      key := lin.substring(0, spos);
      val := lin.substring(spos + 1);
    end;

    if key = 'ENCODING' then
      enc := strtoint(val);

    if key = 'BITMAP' then
    begin
      write(fout, format('  $%2.2x, $%2.2x, | ', [ (enc and $ff00) shr 8, enc and $FF ] ));
      for i := 0 to 15 do
      begin
        readln(fin, val);
        write(fout, format(' $%2.2x,', [ Hex2Dec(val) ] ));
      end;
      writeln(fout, format('  // char %d', [ enc ]));
    end;
  end;
  writeln(fout, '];');
  closefile(fin);
  closefile(fout);
}

