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

  Tip :

    Avoid drawing to TBGRABitmap directly. It's SLOOOOOOOOOW.

  TODO :

    ImportANSI needs font select code

    VTX specific
      row attributes
      page / border / cursor attributes
      transparent black in vtx mode
      fonts 10-12 in VTX only
        Mode Char / Block (sixels in Teletext font)


    border on display / center document on window

    paint / line / rectangle / ellipse / fill tools

    export as bitmap, rtf, html

    import xbin

}

unit VTXEdit;

{$mode objfpc}{$H+}
{$codepage utf8}
{$ASMMODE intel}
{$modeswitch advancedrecords}

interface

uses
  {$ifdef WINDOWS}
  Windows,
  {$endif}
  LCLType,
  Classes,
  SysUtils,
  strutils,
  FileUtil, DateTimePicker,
  Forms,
  Controls,
  Dialogs,
  ExtCtrls,
  Menus,
  LCL,
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
  VTXExportOptions,
  VTXFontConfig,
  UnicodeHelper,
  RecList,
  LResources, EditBtn,
  Memory,
  dateutils,
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
    dtpSauceDate: TDateTimePicker;
    ilButtons: TImageList;
    ilDisabledButtons: TImageList;
    ilCursors: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lvObjects: TListView;
    memSauceComments: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem7: TMenuItem;
    miFileImport: TMenuItem;
    miObjMergeAll: TMenuItem;
    miFileSaveAs: TMenuItem;
    miFileExport: TMenuItem;
    miEditDelete: TMenuItem;
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
    odObject: TOpenDialog;
    odImage: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    miFileOpen: TMenuItem;
    miFileSave: TMenuItem;
    miViewPreview: TMenuItem;
    miView: TMenuItem;
    odAnsi: TOpenDialog;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    pbChars: TPaintBox;
    pbColors: TPaintBox;
    pbCurrCell: TPaintBox;
    pRightBar: TPanel;
    pbPage: TPaintBox;
    pbRulerLeft: TPaintBox;
    pbRulerTop: TPaintBox;
    pPagePanel: TPanel;
    miFileExit: TMenuItem;
    miFileNew: TMenuItem;
    mMenu: TMainMenu;
    miFile: TMenuItem;
    miEdit: TMenuItem;
    miHelp: TMenuItem;
    pbStatusBar: TPaintBox;
    pSettings: TPanel;
    sdObject: TSaveDialog;
    sbHorz: TScrollBar;
    sbVert: TScrollBar;
    sbChars: TScrollBox;
    sdAnsi: TSaveDialog;
    seCharacter: TSpinEdit;
    irqBlink: TTimer;
    seCols: TSpinEdit;
    seRows: TSpinEdit;
    seXScale: TFloatSpinEdit;
    SpeedButton1: TSpeedButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    tbCodePage: TEdit;
    tbSauceAuthor: TEdit;
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
    bObjSave: TToolButton;
    bObjLoad: TToolButton;
    bObjHideOutlines: TToolButton;
    ToolButton1: TToolButton;
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
    ToolButton2: TToolButton;
    bObjMergeAll: TToolButton;
    tbToolPaint: TToolButton;
    tbFontConfig: TToolButton;
    ToolButton8: TToolButton;
    tbToolDraw: TToolButton;
    tbFont7: TToolButton;
    procedure dtpSauceDateEditingDone(Sender: TObject);
    procedure ExportTextFile(fname : string; useBOM, usesauce : boolean);
    function ComputeSGR(currattr, targetattr : DWORD) : unicodestring;
    procedure FormDestroy(Sender: TObject);
    procedure memSauceCommentsEditingDone(Sender: TObject);
    procedure tbFontConfigClick(Sender: TObject);
    procedure tbSauceAuthorEditingDone(Sender: TObject);
    procedure tbSauceGroupEditingDone(Sender: TObject);
    procedure tbSauceTitleEditingDone(Sender: TObject);
    procedure WriteANSI(fs : TFileStream; str : unicodestring);
    procedure WriteANSIChunk(fs : TFileStream; maxlen : integer; var rowansi, ansi : unicodestring);
    procedure miFileImportClick(Sender: TObject);
    procedure OpenVTXFile(fname : string);
    procedure SaveVTXFile(fname : string);
    procedure SaveAsVTXFile;
    procedure ImportANSIFile(fname : string; force8bit : boolean);
    procedure ExportANSIFile(fname : string; maxlen : integer; usebom, usesauce, staticobjects: boolean);
    procedure CheckToSave;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SaveUndoKeys;
    procedure ClearAllUndo;
    procedure DoObjFlipHorz(objnum : integer);
    procedure DoObjFlipVert(objnum : integer);
    procedure bObjHideOutlinesClick(Sender: TObject);
    procedure bObjMergeAllClick(Sender: TObject);
    function lvObjIndex(idx : integer) : integer;
    procedure bObjLoadClick(Sender: TObject);
    procedure bObjSaveClick(Sender: TObject);
    procedure lvObjectsEdited(Sender: TObject; Item: TListItem; var AValue: string);
    procedure lvObjectsEditing(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
    procedure miFileExportClick(Sender: TObject);
    procedure miFileSaveAsClick(Sender: TObject);
    procedure miObjNextClick(Sender: TObject);
    procedure miObjPrevClick(Sender: TObject);
    procedure RemoveObject(objnum : integer);
    procedure InsertObject(obj : TObj; pos : integer);
    procedure ObjFlipHorz;
    procedure ObjFlipVert;
    procedure ObjMoveBack;
    procedure ObjMoveForward;
    procedure ObjMoveToBack;
    procedure ObjMoveToFront;
    procedure ObjMerge;
    procedure ObjMergeAll;
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
    function GetObjectCell(row, col : integer; var cell : TCell) : integer;
    function GetObject(row, col : integer) : integer;
    procedure LoadlvObjects;
    function CopySelectionToObject : TObj;
    procedure BuildCharacterPalette;
    function BuildDisplayCopySelection : TRecList;
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
    procedure pbCurrCellPaint(Sender: TObject);
    procedure DrawCellEx(
      cnv : TCanvas;
      x, y, row, col : integer;
      skipUpdate : boolean = true;
      skipDraw : boolean = false;
      usech : uint16 = _EMPTY;
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
    procedure ResizePage(row, col : integer);
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
    function GetColors2x3(cell : TCell) : TColors6;
    function GetBlockColor(cell : TCell; xsize, ysize, x, y : integer) : integer;
    function SetBlockColor(clr: integer; cell : TCell; xsize, ysize, x, y : integer) : TCell;
    procedure GenerateBmpPage;
    procedure CodePageChange;
    function GetNextCell(r, c : integer; staticobjects : boolean; var cell : TCell) : TRowCol;
    function GetNextObjectCell(Obj : TObj; r, c : integer; var cell : TCell) : TRowCol;
    Procedure LoadSettings;
    Procedure SaveSettings;
    procedure InitSauce;
    procedure NewFile;
    procedure DoBlink;

    procedure RecordUndoCell(row, col : uint16; newcell : TCell);
    procedure UndoTruncate(pos : integer);
    procedure UndoAdd(undoblk : TUndoBlock);
    procedure UndoPerform(pos : integer);
    procedure RedoPerform(pos : integer);

  private
    { private declarations }

  public
    { public declarations }


  end;

  // save as native character. (not converted to unicode)
  TFKeySet = array [0..9] of byte;

var
  fMain: TfMain;

procedure CopyObject(src : TObj; var dst : TObj);
procedure DebugStart;
procedure nop;



implementation

{$R *.lfm}

{*****************************************************************************}

{ Private Globals }
var
  // tool windows
  fPreviewBox : TfPreview;

  CurrFileName :            unicodestring;
  CurrFileChanged :         boolean;
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
//  CurrCodePage :            TEncoding;  // use Fonts[CurrFont]

  MouseLeft,
  MouseMiddle,
  MouseRight :              boolean;

  // objects
  SelectedObject :          integer;
  dragObj :                 boolean;    // in object move mode?
  Clipboard :               TObj;

  // region selection
  CopySelection :           TRecList;
  drag :                    boolean;    // in drag mode
  dragType :                integer;    // 0=select, 1=add, 2=remove
  dragRow, dragCol :        integer;    // drag delta
  dragObjRow, dragObjCol :  integer;    // start pos of obj drag

  SkipScroll :              boolean;      // disable scroll to cursor
  SkipResize :              boolean;      // skip onchange updates on dynamic change.

  bmpCharPalette :          TBitmap = nil;

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

  ObjectRename :            boolean = false;
  ObjectOutlines :          boolean = true;

  LastCharNum :             integer = 0;

  CustomCursors : array [ CURSOR_ARROW .. CURSOR_EYEDROPPER ] of TCursorImage;

{*****************************************************************************}

// convert object number to lvObjects index
function TfMain.lvObjIndex(idx : integer) : integer;
begin
  if idx = -1 then exit(-1);
  result := lvObjects.Items.Count - idx - 1;
end;

procedure TfMain.bObjHideOutlinesClick(Sender: TObject);
begin
  // toggle outlines on objects
  ObjectOutlines := bObjHideOutlines.Down;
  pbPage.Invalidate;
end;

// return the topmost cell of an object at row, cell or EMPTYCELL
function TfMain.GetObjectCell(row, col : integer; var cell : TCell) : integer;
var
  i :             integer;
  po :            PObj;
  objr, objc, p : integer;
  cellrec :       TCell;
begin
  for i := length(Objects) - 1 downto 0 do
  begin
    po := @Objects[i];

    if InRect(col, row, po^.Col, po^.Row, po^.Width, po^.Height) then
    begin
      // on the col
      objr := row - po^.Row;
      objc := col - po^.Col;
      p := objr * po^.Width + objc;
      po^.Data.Get(@cellrec, p);
      if cellrec.Chr <> _EMPTY then
      begin
        cell := cellrec;
        exit(i);
      end;
    end;
  end;
  cell := Page.Rows[row].Cells[col];
  result := -1;
end;

// return the topmost cell of an object at row, cell or EMPTYCELL
function TfMain.GetObject(row, col : integer) : integer;
var
  i :             integer;
  po :            PObj;
  objr, objc, p : integer;
  cellrec :       TCell;
begin
  for i := length(Objects) - 1 downto 0 do
  begin
    po := @Objects[i];

    if InRect(col, row, po^.Col, po^.Row, po^.Width, po^.Height) then
    begin
      // on the col
      objr := row - po^.Row;
      objc := col - po^.Col;
      p := objr * po^.Width + objc;
      po^.Data.Get(@cellrec, p);
      if cellrec.Chr <> _EMPTY then
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
  h1, h2,
  v1, v2 :      integer;
  i :           integer;
  cl1, cl2 :    TColor;
  copyrec :     TLoc;
begin
  if bmpPage <> nil then
  begin
    cell := BLANK;
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

          objnum := GetObjectCell(r, c, cell);
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
            DrawCellEx(cnv, x, y, r, c, true, false, cell.chr, cell.attr);

            // draw cursor over top if cursor location (if on page)
            if (CursorRow = r) and (CursorCol = c) and (objnum = -1) then
            begin

              // draw selection border
              for i := CopySelection.Count - 1 downto 0 do
              begin
                CopySelection.Get(@copyrec, i);
                if (copyrec.Row = r) and (copyrec.Col = c) then
                begin
                  if not HasBits(copyrec.Neighbors, NEIGHBOR_NORTH) then
                    DrawDashLine(cnv,
                      x, y,
                      x + CellWidthZ, y,
                      clSelectionArea1, clSelectionArea2);

                  if not HasBits(copyrec.Neighbors, NEIGHBOR_SOUTH) then
                    DrawDashLine(cnv,
                      x, y + CellHeightZ - 1,
                      x + CellWidthZ, y + CellHeightZ - 1,
                      clSelectionArea1, clSelectionArea2);

                  if not HasBits(copyrec.Neighbors, NEIGHBOR_WEST) then
                    DrawDashLine(cnv,
                      x, y,
                      x, y + CellHeightZ,
                      clSelectionArea1, clSelectionArea2);

                  if not HasBits(copyrec.Neighbors, NEIGHBOR_EAST) then
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
              if ObjectOutlines then
              begin
                cnv.Brush.Style := bsClear;

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

                if not HasBits(cell.neighbors, NEIGHBOR_NORTH) then
                  DrawDashLine(cnv, x, y,
                    x + CellWidthZ - 1, y, cl1, cl2);
                if not HasBits(cell.neighbors, NEIGHBOR_SOUTH) then
                  DrawDashLine(cnv, x, y + CellHeightZ - 1,
                    x + CellWidthZ - 1, y + CellHeightZ - 1, cl1, cl2);
                if not HasBits(cell.neighbors, NEIGHBOR_WEST) then
                  DrawDashLine(cnv, x, y,
                    x, y + CellHeightZ - 1, cl1, cl2);
                if not HasBits(cell.neighbors, NEIGHBOR_EAST) then
                  DrawDashLine(cnv, x + CellWidthZ - 1, y,
                    x + CellWidthZ - 1, y + CellHeightZ - 1, cl1, cl2);
              end;
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
      Page.Rows[i].Attr := A_ROW_WIDTH_100;
  end;

  for i := 0 to NumRows - 1 do
  begin
    c := length(Page.Rows[i].Cells);  // get actual width
    if c < NumCols then
    begin
      setlength(Page.Rows[i].Cells, NumCols);
      for j := c to NumCols - 1 do
        Page.Rows[i].Cells[j] := BLANK;
    end;
  end;
  if CursorRow >= NumRows then CursorRow := NumRows - 1;
  if CursorCol >= NumCols then CursorCol := NumCols - 1;
  GenerateBmpPage;
  pbPage.Invalidate;
end;

procedure TfMain.ResizePage(row, col : integer);
begin
  if row >= NumRows then
    seRows.Value := row + 1;
  if col >= NumCols then
    seCols.Value := col + 1;
end;

procedure TfMain.UpdateTitles;
var
  altered : unicodestring;
  displayname : unicodestring;
begin
  altered := '';
  if CurrFileChanged then
    altered := '*';

  displayname := CurrFileName;
  if displayname = '' then
    displayname := '<untitled>';

  Caption := Format('VTXEdit : %s - %s%s', [ Version, altered, displayname ]);
  Application.Title := Format('VTXEdit - %s%s', [ altered, displayname ]);
end;

procedure TfMain.FormCreate(Sender: TObject);
var
  cp :  TEncoding;
  i :   integer;
  gt :  pbyte;
  gts : integer;
  enc : integer;
begin

  // create custom cursors from imagelist

  Screen.Cursors[CURSOR_ARROW] := LoadCursorFromLazarusResource('C0');
  Screen.Cursors[CURSOR_ARROWPLUS] := LoadCursorFromLazarusResource('C1');
  Screen.Cursors[CURSOR_ARROWMINUS] := LoadCursorFromLazarusResource('C2');
  Screen.Cursors[CURSOR_DRAW] := LoadCursorFromLazarusResource('C3');
  Screen.Cursors[CURSOR_FILL] := LoadCursorFromLazarusResource('C4');
  Screen.Cursors[CURSOR_LINE] := LoadCursorFromLazarusResource('C5');
  Screen.Cursors[CURSOR_RECT] := LoadCursorFromLazarusResource('C6');
  Screen.Cursors[CURSOR_ELLIPSE] := LoadCursorFromLazarusResource('C7');
  Screen.Cursors[CURSOR_EYEDROPPER] := LoadCursorFromLazarusResource('C8');
  Screen.Cursors[CURSOR_PAINT] := LoadCursorFromLazarusResource('C9');
  pbPage.Cursor := CURSOR_ARROW;
  Application.ProcessMessages;

  seRows.MaxValue := MaxRows;
  seCols.MaxValue := MaxCols;

  CurrFileName := '';
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

  for cp := encCP437 to encUTF16 do
  begin
    // interigate for drawmodes
    CPages[cp].CanDrawMode[dmChars] := true;
    CPages[cp].CanDrawMode[dmLeftRights] := false;
    CPages[cp].CanDrawMode[dmTopBottoms] := false;
    CPages[cp].CanDrawMode[dmQuarters] := false;
    CPages[cp].CanDrawMode[dmSixels] := false;

    // build glyph luts
    gt := CPages[cp].GlyphTable;
    gts := CPages[cp].GlyphTableSize;
    if not (cp in [ encUTF8, encUTF16 ]) then
    begin
      for i := 0 to 255 do
      begin
        enc := CPages[cp].EncodingLUT[i];
        CPages[cp].QuickGlyph[i] := GetGlyphOff(enc, gt, gts);
      end;
      if gt = @UVGA16 then
      begin
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
      end;

      if cp in [ encTeletextBlock, encTeletextSeparated ] then
        CPages[cp].CanDrawMode[dmSixels] := true;

    end
    else
    begin
      CPages[cp].CanDrawMode[dmLeftRights] := true;
      CPages[cp].CanDrawMode[dmTopBottoms] := true;
      CPages[cp].CanDrawMode[dmQuarters] := true;
    end;
  end;

  // set default fonts
  for i := 0 to 15 do
    Fonts[i] := encCP437;
  Fonts[10] := encTeletext;
  Fonts[11] := encTeletextBlock;
  Fonts[12] := encTeletextSeparated;

  // clear undo stuff
  CurrUndoData.Create(sizeof(TUndoCells), rleDoubles);
  Undo.Create(sizeof(TUndoBlock), rleDoubles);
  UndoPos := 0;

  // create copyselection
  CopySelection.Create(sizeof(TLoc), rleDoubles);

  LoadSettings;
  NewFile;

  CurrFont := 0;
  cbCodePage.enabled := false;
  cbCodePage.ItemIndex := ord(Fonts[CurrFont]);
  cbCodePage.enabled := true;
  tbFont0.Down := true;
  BuildCharacterPalette;
  seCharacter.Value := CurrChar;
  CodePageChange;

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
  Clipboard.Data.Create(sizeof(TCell), rleDoubles);

  // set font 0
  tbFontClick(tbFont0);

  dtpSauceDate.Date := now;

  Page.SauceComments := TStringList.Create;
end;

procedure TfMain.LoadlvObjects;
var
  i, l : integer;
begin
  // rebuild listview with objects
  lvObjects.Items.Clear;
  l := length(Objects) - 1;
  for i := l downto 0 do
    lvObjects.AddItem(Objects[i].Name, nil);
  lvObjects.Invalidate;
end;

procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CheckToSave;
  SaveSettings;
  fPreviewBox.Hide;
  fPreviewBox.Free;
end;

procedure TfMain.FormDestroy(Sender: TObject);
var
  i : integer;
begin
  // free up document objects
  for i := 0 to length(Objects) - 1 do
    Objects[i].Data.free;
  setlength(Objects, 0);

  // free TRecLists
  Clipboard.Data.Free;
  CopySelection.Free;
  CurrUndoData.Free;
  ClearAllUndo;
  Undo.Free;

  // free bitmaps
  bmpPage.Free;
  bmpPreview.Free;
  bmpCharPalette.Free;
  bmpCharPalette := nil;

  Page.SauceComments.Free;
end;

procedure TfMain.tbFontConfigClick(Sender: TObject);
var
  fc : TfFontConfig;
  i : integer;
  str : string;
begin
  // config fonts
  fc := TfFontConfig.Create(self);

  fc.cbFont1.Items.Clear;
  fc.cbFont2.Items.Clear;
  fc.cbFont3.Items.Clear;
  fc.cbFont4.Items.Clear;
  fc.cbFont5.Items.Clear;
  fc.cbFont6.Items.Clear;
  fc.cbFont7.Items.Clear;
  fc.cbFont8.Items.Clear;
  fc.cbFont9.Items.Clear;
  for i := 0 to length(FontSelectFonts) - 1 do
  begin
    str := CPages[FontSelectFonts[i].CodePage].Name;
    fc.cbFont1.Items.Add(str);
    fc.cbFont2.Items.Add(str);
    fc.cbFont3.Items.Add(str);
    fc.cbFont4.Items.Add(str);
    fc.cbFont5.Items.Add(str);
    fc.cbFont6.Items.Add(str);
    fc.cbFont7.Items.Add(str);
    fc.cbFont8.Items.Add(str);
    fc.cbFont9.Items.Add(str);

   if Fonts[1] = FontSelectFonts[i].CodePage then fc.cbFont1.ItemIndex := i;
   if Fonts[2] = FontSelectFonts[i].CodePage then fc.cbFont2.ItemIndex := i;
   if Fonts[3] = FontSelectFonts[i].CodePage then fc.cbFont3.ItemIndex := i;
   if Fonts[4] = FontSelectFonts[i].CodePage then fc.cbFont4.ItemIndex := i;
   if Fonts[5] = FontSelectFonts[i].CodePage then fc.cbFont5.ItemIndex := i;
   if Fonts[6] = FontSelectFonts[i].CodePage then fc.cbFont6.ItemIndex := i;
   if Fonts[7] = FontSelectFonts[i].CodePage then fc.cbFont7.ItemIndex := i;
   if Fonts[8] = FontSelectFonts[i].CodePage then fc.cbFont8.ItemIndex := i;
   if Fonts[9] = FontSelectFonts[i].CodePage then fc.cbFont9.ItemIndex := i;
  end;

  if fc.ShowModal = mrOK then
  begin
    // get any changes
    Fonts[1] := FontSelectFonts[fc.cbFont1.ItemIndex].CodePage;
    Fonts[2] := FontSelectFonts[fc.cbFont2.ItemIndex].CodePage;
    Fonts[3] := FontSelectFonts[fc.cbFont3.ItemIndex].CodePage;
    Fonts[4] := FontSelectFonts[fc.cbFont4.ItemIndex].CodePage;
    Fonts[5] := FontSelectFonts[fc.cbFont5.ItemIndex].CodePage;
    Fonts[6] := FontSelectFonts[fc.cbFont6.ItemIndex].CodePage;
    Fonts[7] := FontSelectFonts[fc.cbFont7.ItemIndex].CodePage;
    Fonts[8] := FontSelectFonts[fc.cbFont8.ItemIndex].CodePage;
    Fonts[9] := FontSelectFonts[fc.cbFont9.ItemIndex].CodePage;
    nop;
  end;
  fc.Free;
end;

procedure StrToChars(str : unicodestring; buff : PByte; len : integer);
var
  b : TBytes;
  i : integer;
begin
  MemZero(buff, len);
  b := str.toEncodedCPBytes(CPages[Fonts[0]].EncodingLUT);
  for i := 0 to len - 1 do
    if i < length(b) then
      buff[i] := b[i];
end;

procedure StrToSauce(str : unicodestring; sauceBuff : PByte; len : integer);
var
  b : TBytes;
  i : integer;
begin
  (*
    "The Character type is a string of characters encoded according to code
    page 437 (IBM PC / OEM ASCII). It is neither a pascal type string with a
    leading length byte nor is it a C-style string with a trailing terminator
    character. Any value shorter than the available length should be padded
    with spaces."
  *)
  FillChar(sauceBuff^, len, ' ');
  b := str.toEncodedCPBytes(CPages[Fonts[0]].EncodingLUT);
    for i := 0 to len - 1 do
      if i < length(b) then
        sauceBuff[i] := b[i];
end;

procedure TfMain.tbSauceAuthorEditingDone(Sender: TObject);
begin
  StrToSauce(unicodestring(tbSauceAuthor.Text), @Page.Sauce.Author[0], sizeof(Page.Sauce.Author));
end;

procedure TfMain.tbSauceGroupEditingDone(Sender: TObject);
begin
  StrToSauce(unicodestring(tbSauceGroup.Text), @Page.Sauce.Group[0], sizeof(Page.Sauce.Group));
end;

procedure TfMain.tbSauceTitleEditingDone(Sender: TObject);
begin
  StrToSauce(unicodestring(tbSauceTitle.Text), @Page.Sauce.Title[0], sizeof(Page.Sauce.Title));
end;

procedure TfMain.dtpSauceDateEditingDone(Sender: TObject);
var
  dateStr : string;
begin
  dateStr := FormatDateTime('YYYYMMDD', dtpSauceDate.Date);
  StrToChars(unicodestring(dateStr), @Page.Sauce.Date[0], sizeof(Page.Sauce.Date));
end;

procedure TfMain.memSauceCommentsEditingDone(Sender : TObject);
var
  i : byte;
begin
  Page.Sauce.Comments := Min(255, memSauceComments.Lines.Count);
  Page.SauceComments.Clear;
  for i := 0 to Page.Sauce.Comments - 1 do
    Page.SauceComments.Add(memSauceComments.Lines[i]);
end;

// create new bmpPage of page at zoom 1
procedure TfMain.GenerateBmpPage;
var
  row, col,
  x, y :    integer;
  ch :      integer;
  cp :      TEncoding;
  attr :    Uint32;
  off :     integer;
  bmp :     TBGRABitmap;
  w, h :    integer;
  tmp :     TBitmap;
  fntnum :  integer;
begin

  // create new
  w := NumCols * CellWidth;
  h := NumRows * CellHeight;
  if (w = 0) or (h = 0) then
    exit;

  tmp := TBitmap.Create;
  tmp.SetSize(w, h);
  tmp.PixelFormat := pf16bit;

  // create mini bmp for character cells
  bmp := TBGRABitmap.Create(CellWidth, CellHeight);

  // draw it.
  y := 0;
  for row := 0 to NumRows - 1 do
  begin
    x := 0;
    for col := 0 to NumCols - 1 do
    begin

      ch := Page.Rows[row].Cells[col].Chr;
      attr := Page.Rows[row].Cells[col].Attr;

      if PageType in [ PAGETYPE_CTERM, PAGETYPE_VTX ] then
      begin
        fntnum := GetBits(attr, A_CELL_FONT_MASK, 28);
        if (fntnum >= 10) and (PageType <> PAGETYPE_VTX) then
          fntnum := 0;
        cp := Fonts[fntnum];
      end
      else
      begin
        cp := Fonts[0];
      end;
      if cp in [ encUTF8, encUTF16 ] then
        off := GetGlyphOff(ch, CPages[cp].GlyphTable, CPages[cp].GlyphTableSize)
      else
      begin
        if ch > 255 then
          ch := 0;
        off := CPages[cp].QuickGlyph[ch];
      end;

      GetGlyphBmp(bmp, CPages[cp].GlyphTable, off, attr, false);
      tmp.Canvas.Draw(x, y, bmp.Bitmap);

      x += CellWidth;
    end;

    y += CellHeight;
  end;
  bmp.free;

  if bmpPage <> nil then
    bmpPage.Free;
  bmpPage := TBGRABitmap.Create(tmp, false);
  tmp.free;

  // create preview image
  if bmpPreview <> nil then
    bmpPreview.Free;
  bmpPage.ResampleFilter := rfMitchell;
  bmpPreview := bmpPage.Resample(bmpPage.Width>>2, bmpPage.Height>>2) as TBGRABitmap;

  fPreviewBox.Invalidate;

end;

// find unicode char of character base on chars encoding.
function TfMain.GetUnicode(cell : TCell) : integer;
var
  cp : TEncoding;
begin
  // convert chr to unicode
  result := cell.chr;
  cp := Fonts[GetBits(cell.attr, A_CELL_FONT_MASK, 28)];
  if not (cp in [ encUTF8, encUTF16 ]) then
    result := CPages[cp].EncodingLUT[result];
end;

// find the best match for encoding of unicode char
function TfMain.GetCPChar(cp : TEncoding; unicode : integer) : integer;
var
  i : integer;
begin
  if cp in [ encUTF8, encUTF16 ] then
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

  MemFill(@result, 2, bg);
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

  MemFill(@result, 2, bg);
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

  MemFill(@result, 4, bg);
  if HasBits(i, %0001) then result[0] := fg;
  if HasBits(i, %0010) then result[1] := fg;
  if HasBits(i, %0100) then result[2] := fg;
  if HasBits(i, %1000) then result[3] := fg;
end;

function TfMain.GetColors2x3(cell : TCell) : TColors6;
var
  uni, i : integer;
  fg, bg : byte;
begin
  // convert chr to unicode
  uni := cell.Chr;  // special for teletext
  fg := GetBits(cell.attr, A_CELL_FG_MASK);
  bg := GetBits(cell.attr, A_CELL_BG_MASK, 8);
  for i := 63 downto 0 do
    if uni = Blocks2x3[i] then
      break;

  MemFill(@result, 6, bg);
  if HasBits(i, %000001) then result[0] := fg;
  if HasBits(i, %000010) then result[1] := fg;
  if HasBits(i, %000100) then result[2] := fg;
  if HasBits(i, %001000) then result[3] := fg;
  if HasBits(i, %010000) then result[4] := fg;
  if HasBits(i, %100000) then result[5] := fg;
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
  c6 :      TColors6;
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
    MemZero(@count[0], 256);
    tot := 0;
    mval := 0;
    mclr := -1;     // other color with highest count
    for i := 0 to 3 do
    begin
      count[c4[i]] += 1;
      if count[c4[i]] = 1 then
        tot += 1;
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
  end

  else if (xsize = 2) and (ysize = 3) then
  begin
    c6 := GetColors2x3(cell);
    c6[x + (y << 1)] := clr;

    if Fonts[CurrFont] = encTeletextSeparated then
//    if GetBits(cell.Attr, A_CELL_FONT_MASK, 28) = 12 then
      for i := 0 to 5 do
        if c6[i] <> clr then
          c6[i] := GetBits(CurrAttr, A_CELL_BG_MASK, 8);

    // reduce other 5 blocks to 1 color + this color
    // get color count.
    setlength(count, 256);
    MemZero(@count[0], 256);
    tot := 0;
    mval := 0;
    mclr := -1;     // other color with highest count
    for i := 0 to 5 do
    begin
      count[c6[i]] += 1;
      if count[c6[i]] = 1 then
        tot += 1;
      if (c6[i] <> clr) and (count[c6[i]] > mval) then
      begin
        mval := count[c6[i]];
        mclr := c6[i];
      end;
    end;

    if tot > 2 then
    begin
      // reduce other 5 blocks to 1 color + this color
      for i := 0 to 5 do
        if ((c6[i] <> clr) and
            (c6[i] <> mclr)) then
          c6[i] := mclr;
    end;

    if mclr = -1 then
    begin
      if Fonts[CurrFont] = encTeletextSeparated then
//      if GetBits(cell.Attr, A_CELL_FONT_MASK, 28) = 12 then
        mclr := GetBits(CurrAttr, A_CELL_BG_MASK, 8)
      else
        mclr := clr;
    end;

    // convert c6 to cell
    g := 0;
    for i := 0 to 5 do
      if c6[i] = clr then
        g := g or (1 << i);

    SetBits(cell.Attr, A_CELL_FG_MASK, clr);
    SetBits(cell.Attr, A_CELL_BG_MASK, mclr, 8);
    cell.Chr := Blocks2x3[g];
  end;
  result := cell;
end;


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
  CursorCol += 1;
  if CursorCol >= NumCols then
  begin
    CursorCol := 0;
    CursorRow += 1;
    if CursorRow >= NumRows then
    begin
      seRows.Value := CursorRow + 1;
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
  CursorCol -= 1;
  if CursorCol < 0 then
  begin
    CursorCol := NumCols - 1;
    CursorRow -= 1;
    if CursorRow < 0 then
    begin
      CursorRow -= 1;
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
  CursorRow -= 1;
  if CursorRow < 0 then
    CursorRow := 0;
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
  CursorRow += 1;
  if CursorRow >= NumRows then
  begin
    seRows.Value := CursorRow + 1;
  end;
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
  CursorRow += 1;
  if CursorRow >= NumRows then
  begin
    seRows.Value := CursorRow + 1;
  end;
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
  fPreviewBox.Invalidate;
end;

procedure CopyObject(src : TObj; var dst : TObj);
begin
  dst := src;
  dst.Data := src.Data.Copy;
end;

procedure TfMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_SHIFT) and (ToolMode = tmSelect) then
  begin
    pbPage.Cursor := CURSOR_ARROW;
    Application.ProcessMessages;
  end
  else if (Key = VK_CONTROL) and (ToolMode = tmSelect) then
  begin
    pbPage.Cursor := CURSOR_ARROW;
    Application.ProcessMessages;
  end;
end;

// for special keys
procedure TfMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i, l, ch :    integer;
  r, c :        integer;
  shft :        TShiftState;
  KeyAction :   integer;
  KeyValue :    string;
  tmp :         string;
  unk :         boolean;
  found :       boolean;
  copyrec :     TLoc;
  pagebottom,
  pageright :   integer;
  undoblk :     TUndoBlock;
begin

  // let system handle these controls
  if seRows.Focused
    or seCols.Focused
    or seXScale.Focused
    or seCharacter.Focused
    or tbSauceAuthor.Focused
    or dtpSauceDate.Focused
    or memSauceComments.Focused
    or tbSauceGroup.Focused
    or tbSauceTitle.Focused
    or ObjectRename then
    exit;

  if (Key = VK_SHIFT) and (ToolMode = tmSelect) then
  begin
    pbPage.Cursor := CURSOR_ARROWPLUS;
    Application.ProcessMessages;
  end
  else if (Key = VK_CONTROL) and (ToolMode = tmSelect) then
  begin
    pbPage.Cursor := CURSOR_ARROWMINUS;
    Application.ProcessMessages;
  end;

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

    KA_CURSORNEWLINE:
      CursorNewLine;

    KA_CURSORFORWARDTAB:
      begin
        SaveUndoKeys;
        CursorForwardTab;
      end;

    KA_CURSORBACKWARDTAB:
      begin
        SaveUndoKeys;
        CursorBackwardTab;
      end;

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
            if Fonts[CurrFont] in [encUTF8, encUTF16 ] then
              ch := CPages[Fonts[CurrFont]].EncodingLUT[ch];

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

    KA_TOOLPAINT:
      if tbToolPaint.Enabled then tbToolPaint.Click;

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

    KA_FILENEW:
      begin
        CheckToSave;
        NewFile;
      end;

    KA_FILEOPEN:    miFileOpenClick(miFileOpen);
    KA_FILESAVE:    miFileSaveClick(miFileSave);
    KA_FILESAVEAS:
      ;
    KA_FILEIMPORT:
      ;
    KA_FILEEXPORT:
      ;

    KA_FILEEXIT:
      begin
        CheckToSave;
        Close;
      end;

    KA_EDITREDO:
      begin
        SaveUndoKeys;
        if UndoPos < Undo.Count then
        begin
          RedoPerform(UndoPos);
          UndoPos += 1;
          CurrFileChanged := true;
          UpdateTitles;
        end;
      end;

    KA_EDITUNDO:
      begin
        SaveUndoKeys;
        if UndoPos > 0 then
        begin
          // only if there is some undo to be done.
          UndoPerform(UndoPos - 1);
          UndoPos -= 1;
          CurrFileChanged := true;
          UpdateTitles;
        end;
      end;

    KA_EDITCUT:
      // copy selection or object to clipboard, removing the thing
      begin
        SaveUndoKeys;
        if SelectedObject >= 0 then
        begin
          Clipboard.Data.Free;
          CopyObject(Objects[SelectedObject], Clipboard);
          RemoveObject(SelectedObject);
          LoadlvObjects;
          SelectedObject := -1;
          lvObjects.ItemIndex := lvObjIndex(selectedObject);
          pbPage.Invalidate;
          CurrFileChanged := true;
          UpdateTitles;
        end
        else if CopySelection.Count > 0 then
        begin
          Clipboard.Data.Free;
          Clipboard := CopySelectionToObject;
          for i := 0 to CopySelection.Count - 1 do
          begin
            CopySelection.Get(@copyrec, i);
            r := copyrec.Row;
            c := copyrec.Col;
            Page.Rows[r].Cells[c] := BLANK;
            DrawCell(r, c, false);
          end;
          CurrFileChanged := true;
          UpdateTitles;
        end;
      end;

    KA_EDITCOPY:
      // copy selection or object to clipboard.
      begin
        SaveUndoKeys;
        if SelectedObject >= 0 then
        begin
          // copy object to clipboard
          Clipboard.Data.Free;
          CopyObject(Objects[SelectedObject], Clipboard);
          CurrFileChanged := true;
          UpdateTitles;
        end
        else if CopySelection.Count > 0 then
        begin
          Clipboard.Data.Free;
          Clipboard := CopySelectionToObject;
          CurrFileChanged := true;
          UpdateTitles;
        end;
      end;

    KA_EDITPASTE:
      // paste clipboard contents as new object.
      begin
        SaveUndoKeys;
        if (Clipboard.Width > 0) and (Clipboard.Height > 0) then
        begin
          // paste as new object.
          l := length(Objects);
          setlength(Objects, l + 1);
          CopyObject(Clipboard, Objects[l]);

          // drop it onto window. top left for now
          pagebottom := min(PageTop + WindowRows, NumRows);
          pageright := min(PageLeft + WindowCols, NumCols);
          Objects[l].Row := ((pagebottom + PageTop) >> 1) - (Objects[l].Height >> 1);
          Objects[l].Col := ((pageright + PageLeft) >> 1) - (Objects[l].Width >> 1);

          undoblk.UndoType := utObjAdd;
          undoblk.OldRow := Objects[l].Row;
          undoblk.OldCol := Objects[l].Col;
          CopyObject(Objects[l], undoblk.Obj);
          UndoAdd(undoblk);

          LoadlvObjects;
          SelectedObject := l;
          lvObjects.ItemIndex := lvObjIndex(SelectedObject);
          pbPage.Invalidate;
          fPreviewBox.Invalidate;
          CurrFileChanged := true;
          UpdateTitles;
        end;
      end;

    KA_OBJECTMOVEBACK:
      begin
        ObjMoveBack;
        CurrFileChanged := true;
        UpdateTitles;
      end;

    KA_OBJECTMOVEFORWARD:
      begin
        ObjMoveForward;
        CurrFileChanged := true;
        UpdateTitles;
      end;

    KA_OBJECTMOVETOBACK:
      begin
        ObjMoveToBack;
        CurrFileChanged := true;
        UpdateTitles;
      end;

    KA_OBJECTMOVETOFRONT:
      begin
        ObjMoveToFront;
        CurrFileChanged := true;
        UpdateTitles;
      end;

    KA_OBJECTFLIPHORZ:
      begin
        ObjFlipHorz;
        CurrFileChanged := true;
        UpdateTitles;
      end;

    KA_OBJECTFLIPVERT:
      begin
        ObjFlipVert;
        CurrFileChanged := true;
        UpdateTitles;
      end;

    KA_OBJECTMERGE:
      begin
        ObjMerge;
        CurrFileChanged := true;
        UpdateTitles;
      end;

    KA_OBJECTMERGEALL:
      begin
        ObjMergeAll;
        CurrFileChanged := true;
        UpdateTitles;
      end;

    KA_OBJECTNEXT:
      begin
        ObjNext;
        CurrFileChanged := true;
        UpdateTitles;
      end;

    KA_OBJECTPREV:
      begin
        ObjPrev;
        CurrFileChanged := true;
        UpdateTitles;
      end;

    KA_DELETE:
      begin
        if ToolMode = tmSelect then
        begin
          SaveUndoKeys;
          if SelectedObject >= 0 then
          begin
            // delete object
            if not Objects[SelectedObject].Locked then
            begin

              undoblk.UndoType := utObjRemove;
              undoblk.OldRow := Objects[SelectedObject].Row;
              undoblk.OldCol := Objects[SelectedObject].Col;
              undoblk.OldNum := SelectedObject;
              CopyObject(Objects[SelectedObject], undoblk.Obj);
              UndoAdd(undoblk);

              RemoveObject(SelectedObject);
              SelectedObject := -1;
              LoadlvObjects;
              pbPage.Invalidate;
            end;
          end
          else if CopySelection.Count > 0 then
          begin
            // delete selected area
            for i := CopySelection.Count - 1 downto 0 do
            begin
              CopySelection.Get(@copyrec, i);
              r := copyrec.Row;
              c := copyrec.Col;

              RecordUndoCell(r, c, BLANK);

              Page.Rows[r].Cells[c] := BLANK;
            end;

            undoblk.UndoType := utCells;
            undoblk.CellData := CurrUndoData.Copy;
            undoblk.CellData.Trim;
            UndoAdd(undoblk);

            GenerateBmpPage;
            CopySelection.Clear;
            pbPage.Invalidate;
          end;
          CurrFileChanged := true;
          UpdateTitles;
        end;
      end;

    KA_ESCAPE:
      begin
        if ToolMode = tmSelect then
        begin
          if SelectedObject >= 0 then
          begin
            SelectedObject := -1;
            lvObjects.ItemIndex := lvObjIndex(SelectedObject);
            pbPage.Invalidate;
          end
          else if CopySelection.Count > 0 then
          begin
            CopySelection.Clear;
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
    or seCharacter.Focused
    or tbSauceAuthor.Focused
    or dtpSauceDate.Focused
    or memSauceComments.Focused
    or tbSauceGroup.Focused
    or tbSauceTitle.Focused
    or ObjectRename then
    exit;

  if Between(Key, ' ', '~') then
  begin
    // break on spaces
    if Key = ' ' then
      SaveUndoKeys;

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

  CursorCol += 1;
  if CursorCol >= NumCols then
  begin
    CursorCol := 0;
    CursorRow += 1;
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

  // tags on attribute buttons used to mark if attribute is to be
  // ignored.
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

//  attr := Page.Rows[row].Cells[col].Attr;
  attr := cattr;
  attr := attr and (not mask);
  attr := attr or (cattr and mask);
  Page.Rows[row].Cells[col].Attr := attr;
  DrawCell(row, col, false);

  CurrFileChanged := true;
  UpdateTitles;
end;

// called from keypress or Print in keybinds
procedure TfMain.PutChar(ch : integer);
var
  attr,
  mask :    UInt32;
  cell :    TCell;
begin
//  ScrollToCursor; // keep cursor on screen if typing

  if tbAttrCharacter.Tag <> 0 then
    ch := Page.Rows[CursorRow].Cells[CursorCol].Chr;

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
    or (tbAttrBottom.Tag = 0) then    mask := mask or A_CELL_DISPLAY_MASK;

  attr := Page.Rows[CursorRow].Cells[CursorCol].Attr;
  attr := attr and (not mask);
  attr := attr or (CurrAttr and mask);

  SetBits(attr, A_CELL_FONT_MASK, CurrFont, 28);

  cell.Chr := ch;
  cell.Attr := attr;
  RecordUndoCell(CursorRow, CursorCol, cell);

  Page.Rows[CursorRow].Cells[CursorCol].Chr := ch;
  Page.Rows[CursorRow].Cells[CursorCol].Attr := attr;

  DrawCell(CursorRow, CursorCol, false);
  CursorRight;
  CurrFileChanged := true;
  UpdateTitles;
end;

procedure TfMain.CodePageChange;
begin
// CODEPAGE
  if cbCodePage.Enabled then
  begin
    Fonts[0] := TEncoding(cbCodePage.ItemIndex);
    tbCodePage.Text := CPages[Fonts[CurrFont]].Name;

    // rebuild page
    GenerateBmpPage;
    pbPage.Invalidate;
    BuildCharacterPalette;

    // enable / disable Modes
    tbModeLeftRights.Enabled := CPages[Fonts[CurrFont]].CanDrawMode[dmLeftRights];
    tbModeTopBottoms.Enabled := CPages[Fonts[CurrFont]].CanDrawMode[dmTopBottoms];
    tbModeQuarters.Enabled := CPages[Fonts[CurrFont]].CanDrawMode[dmQuarters];
    tbModeSixels.Enabled := CPages[Fonts[CurrFont]].CanDrawMode[dmSixels];
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
//      DrawCell(r, c, false);
    end;
  GenerateBmpPage;
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
    PAGETYPE_BBS:
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

        // no fonts
        tbFontClick(tbFont0);
        tbFont0.Enabled:=false;
        tbFont1.Enabled:=false;
        tbFont2.Enabled:=false;
        tbFont3.Enabled:=false;
        tbFont4.Enabled:=false;
        tbFont5.Enabled:=false;
        tbFont6.Enabled:=false;
        tbFont7.Enabled:=false;
        tbFont8.Enabled:=false;
        tbFont9.Enabled:=false;
        tbFont10.Enabled:=false;
        tbFont11.Enabled:=false;
        tbFont12.Enabled:=false;
        tbFontConfig.Enabled:=false;
      end;

    PAGETYPE_CTERM:
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

        tbFont0.Enabled:=true;
        tbFont1.Enabled:=true;
        tbFont2.Enabled:=true;
        tbFont3.Enabled:=true;
        tbFont4.Enabled:=true;
        tbFont5.Enabled:=true;
        tbFont6.Enabled:=true;
        tbFont7.Enabled:=true;
        tbFont8.Enabled:=true;
        tbFont9.Enabled:=true;
        tbFont10.Enabled:=false;
        tbFont11.Enabled:=false;
        tbFont12.Enabled:=false;
        tbFontConfig.Enabled:=true;
      end;

    PAGETYPE_VTX:
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

        tbFont0.Enabled:=true;
        tbFont1.Enabled:=true;
        tbFont2.Enabled:=true;
        tbFont3.Enabled:=true;
        tbFont4.Enabled:=true;
        tbFont5.Enabled:=true;
        tbFont6.Enabled:=true;
        tbFont7.Enabled:=true;
        tbFont8.Enabled:=true;
        tbFont9.Enabled:=true;
        tbFont10.Enabled:=true;
        tbFont11.Enabled:=true;
        tbFont12.Enabled:=true;
        tbFontConfig.Enabled:=true;
      end;
  end;

  GenerateBmpPage;
  pbPage.Invalidate;

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
  pb:             TPaintBox;
  cnv:            TCanvas;
  r :             TRect;
  ch, i, u, off : integer;
  bmp :           TBGRABitmap;
  str :           UnicodeString;
  style :         TTextStyle;
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
    off := GetGlyphOff(u, CPages[Fonts[0]].GlyphTable, CPages[Fonts[CurrFont]].GlyphTableSize);
    GetGlyphBmp(bmp, CPages[Fonts[0]].GlyphTable, off, $0007, false);
    cnv.Draw(r.left, 4, bmp.Bitmap);
    r.left += 12;
  end;
  bmp.Free;

end;

procedure TfMain.sbVertChange(Sender: TObject);
begin
  ResizeScrolls;
end;

procedure TfMain.FormResize(Sender: TObject);
begin
  ResizeScrolls;
end;

procedure TfMain.InitSauce;
begin
  MemZero(@Page.Sauce, sizeof(TSauceHeader));

  StrToSauce('', @Page.Sauce.Title, sizeof(Page.Sauce.Title));
  StrToSauce('', @Page.Sauce.Author, sizeof(Page.Sauce.Author));
  StrToSauce('', @Page.Sauce.Group, sizeof(Page.Sauce.Group));
  StrToSauce(FormatDateTime('YYYYMMDD', now), @Page.Sauce.Date, sizeof(Page.Sauce.Date));
end;

procedure TfMain.NewFile;
var
  i :       integer;
  r, c :    integer;
  undoblk : TUndoBlock;
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
      Page.Rows[r].Cells[c] := BLANK;

  InitSauce;

  CurrFileName := '';
  CurrFileChanged := false;
  tbSauceAuthor.Text:='';
  dtpSauceDate.Date := now;
  tbSauceGroup.Text:='';
  tbSauceTitle.Text:='';
  memSauceComments.Lines.Clear;
  ResizePage;
  GenerateBmpPage;
  UpdateTitles;

  // delete objects
  for i := 0 to length(Objects) - 1 do
    Objects[i].Data.Free;
  setlength(Objects, 0);
  LoadlvObjects;
  pbPage.Invalidate;
  fPreviewBox.Invalidate;

  // clear selection
  CopySelection.Clear;

  // clear any UndoData left over.
  ClearAllUndo;

  seRows.Value := 24;
  seCols.Value := 80;
  PageZoom := 1.0;
  PageTop := 0;
  PageLeft := 0;
  ResizeScrolls;
  pbPage.Invalidate;
end;

procedure TfMain.miFileNewClick(Sender: TObject);
begin
  NewFile;
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
  cp : TEncoding;
const
  PALCOLS = 16;
  CELL_WIDTH = 21;
  CELL_HEIGHT = 40;

begin
  cp := Fonts[CurrFont];

  // build palette
  seCharacter.Enabled := false;
  cell := TBGRABitmap.Create(8,16);
  if cp in [ encUTF8, encUTF16 ] then
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
  begin
    bmpCharPalette.Free;
    bmpCharPalette := nil;
  end;

  bmpCharPalette := TBitmap.Create;
  bmpCharPalette.SetSize(PALCOLS * CELL_WIDTH + 4, rows * CELL_HEIGHT + 4);
  bmpCharPalette.PixelFormat := pf16bit;

  bmpCharPalette.Canvas.Brush.Style := bsSolid;
  bmpCharPalette.Canvas.Brush.Color := clBlack;
  bmpCharPalette.Canvas.FillRect(0, 0, bmpCharPalette.Width, bmpCharPalette.Height);
  for i := 0 to NumChars - 1 do
  begin
    if cp in [ encUTF8, encUTF16 ]then
      off := (i + 1) * 18 + 2
    else
      off := CPages[cp].QuickGlyph[i];

    y := i div PALCOLS;
    x := i - (y * PALCOLS);

    x := x * CELL_WIDTH + 2;
    y := y * CELL_HEIGHT + 2;

    // draw simple glyph in cell (8x16)
    GetGlyphBmp(cell, CPages[cp].GlyphTable, off, 15, false);

    rect.Left := 2 + x;
    rect.Top := 2 + y;
    rect.Width := 16;
    rect.Height := 32;

    rect.Inflate(1, 1);
    bmpCharPalette.Canvas.Brush.Color := clDkGray;
    bmpCharPalette.Canvas.FillRect(rect);
    rect.Inflate(-1, -1);
    bmpCharPalette.Canvas.StretchDraw(rect, cell.Bitmap);
  end;
  cell.Free;
  pbChars.Invalidate;
end;

procedure TfMain.pbCharsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  off, i : integer;
  cp : TEncoding;
const
  PALCOLS = 16;
  CELL_WIDTH = 21;
  CELL_HEIGHT = 40;

begin
  cp := Fonts[CurrFont];

  // click to select
  y := (y - 4) div CELL_HEIGHT;
  x := (x - 4) div CELL_WIDTH;
  seCharacter.Enabled := false;
  if between(x, 0, 15) and (y >= 0) then
  begin
    i := y * PALCOLS + x;
    if cp in [ encUTF8, encUTF16 ] then
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
      tbUnicode.Text := IntToStr(CPages[cp].EncodingLUT[i]);
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

  if bmpCharPalette = nil then
    exit;

  pb.Width := bmpCharPalette.Width;
  pb.Height := bmpCharPalette.Height;
  cnv.Draw(0, 0, bmpCharPalette);

  // hilight the selected char
  if Fonts[CurrFont] in [ encUTF8, encUTF16 ] then
    // convert unicode to offset
    i := (GetGlyphOff(
      CurrChar,
      CPages[Fonts[CurrFont]].GlyphTable,
      CPages[Fonts[CurrFont]].GlyphTableSize) - 2) div 18 - 1
  else
    i := CurrChar;

  y := i div PALCOLS;
  x := i - (y * PALCOLS);
  x := x * CELL_WIDTH + 4;
  y := y * CELL_HEIGHT + 2;

  cnv.Brush.Style := bsClear;
  cnv.Pen.Color := clRed;
  cnv.Pen.Width := 1;
  cnv.Rectangle(x - 2, y, x + 18, y + 36);

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

procedure TfMain.seCharacterChange(Sender: TObject);
var
  chr : integer;
begin
  if TSpinEdit(sender).enabled then
  begin
    chr := seCharacter.Value;
    if Fonts[CurrFont] in [ encUTF8, encUTF16 ] then
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
      tbUnicode.Text := IntToStr(CPages[Fonts[CurrFont]].EncodingLUT[CurrChar]);
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

    'tbAttrBlinkSlow':
      tbAttrBlinkFast.Down := false;

    'tbAttrBlinkFast':
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

  // buttons are named tbFont__. get number from name
  CurrFont := strtoint(tb.Name.Substring(6));

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

  SetBits(CurrAttr, A_CELL_FONT_MASK, CurrFont, 28);

  // update character palette here.
  CodePageChange;
  pbChars.Invalidate;
  tbModeClick(tbModeCharacter);
end;

procedure TfMain.tbModeClick(Sender: TObject);
var
  tb :    TToolButton;
  n :     string;
  Pt :    TPoint;
  X, Y :  integer;
begin
  tb := TToolButton(Sender);

  DrawCell(LastDrawRow, LastDrawCol);
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
  if ToolMode <> tmSelect then
  begin
    Pt := pbPage.ScreenToClient(Mouse.CursorPos);
    X := Pt.x;
    Y := Pt.y;
    DrawX := ((PageLeft * SubXSize) +  Floor((X / CellWidthZ) * SubXSize)) ;
    DrawY := ((PageTop * SubYSize) +  Floor((Y / CellHeightZ) * SubYSize));
    LastDrawRow := MouseRow;
    LastDrawCol := MouseCol;
    DrawMouseBox;
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

  // disable the mousebox
  DrawCell(LastDrawRow, LastDrawCol);
  case tb.Name of
    'tbToolSelect':
      begin
        ToolMode := tmSelect;
        pbPage.Cursor := CURSOR_ARROW;
        Application.ProcessMessages;
      end;

    'tbToolDraw':
      begin
        ToolMode := tmDraw;
        pbPage.Cursor := CURSOR_DRAW;
        Application.ProcessMessages;
        DrawMouseBox;
      end;

    'tbToolPaint':
      begin
        ToolMode := tmPaint;
        pbPage.Cursor := CURSOR_PAINT;
        Application.ProcessMessages;
        DrawMouseBox;
      end;

    'tbToolEyedropper':
      begin
        ToolMode := tmEyedropper;
        pbPage.Cursor := CURSOR_EYEDROPPER;
        Application.ProcessMessages;
        DrawMouseBox;
      end;

    'tbToolFill':
      begin
        ToolMode := tmFill;
        pbPage.Cursor := CURSOR_FILL;
        Application.ProcessMessages;
        DrawMouseBox;
      end;

    'tbToolLine':
      begin
        ToolMode := tmLine;
        pbPage.Cursor := CURSOR_LINE;
        Application.ProcessMessages;
        DrawMouseBox;
      end;

    'tbToolRect':
      begin
        ToolMode := tmRect;
        pbPage.Cursor := CURSOR_RECT;
        Application.ProcessMessages;
        DrawMouseBox;
      end;

    'tbToolEllipse':
      begin
        ToolMode := tmEllipse;
        pbPage.Cursor := CURSOR_ELLIPSE;
        Application.ProcessMessages;
        DrawMouseBox;
      end;
  end;

  tbToolSelect.Down := (tb.Name = 'tbToolSelect');
  tbToolDraw.Down := (tb.Name = 'tbToolDraw');
  tbToolPaint.Down := (tb.Name = 'tbToolPaint');
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
  CheckToSave;
  Close;
end;

procedure TfMain.pbPageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  undoblk :   TUndoBlock;
  tmp :       TRecList;
begin
  case Button of
    mbLeft : MouseLeft := false;
    mbMiddle : MouseMiddle := false;
    mbRight : MouseRight := false;
  end;

  if Button = mbMiddle then
    MousePan := false
  else if drag then
  begin
    // add / remove selection
    tmp := BuildDisplayCopySelection;
    CopySelection.Free;
    CopySelection := tmp;
    drag := false;
    pbPage.invalidate;
  end
  else if dragObj then
  begin
    // move object (dragrow/dragcol = initial pos)
    SaveUndoKeys;
    undoblk.UndoType := utObjMove;
    undoblk.NewRow := Objects[SelectedObject].Row;
    undoblk.NewCol := Objects[SelectedObject].Col;
    undoblk.OldRow := dragObjRow;
    undoblk.OldCol := dragObjCol;
    undoblk.OldNum := SelectedObject;
    undoblk.NewNum := SelectedObject;
    UndoAdd(undoblk);

    dragObj := false;
    fPreviewBox.Invalidate;
  end
  else
  begin
    // save CurrUndoData to undolist
    if CurrUndoData.Count > 0 then
    begin
      undoblk.UndoType := utCells;
      undoblk.CellData := CurrUndoData.Copy;
      undoblk.CellData.Trim;
      UndoAdd(undoblk);
      CurrUndoData.Clear;
    end;
  end;
end;

procedure TfMain.pbPageMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  val :       integer;
  PrevZoom :  double;
  NewWindowCols, NewWindowRows : integer;
begin
  // zoom
  if ssShift in Shift then
  begin
    PrevZoom := PageZoom;
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

    // don't allow 0's for cellwidthz
    if floor(CellWidth * PageZoom * XScale) = 0 then
      PageZoom *= 2;

    CellWidthZ := floor(CellWidth * PageZoom * XScale);
    CellHeightZ := floor(CellHeight * PageZoom);

    NewWindowCols := (pbPage.Width div CellWidthZ) + 1;
    NewWindowRows := (pbPage.Height div CellHeightZ) + 1;

    // adjust pagetop / pageleft so centered on mousepos
    PageLeft := MouseCol - (NewWindowCols >> 1);
    PageTop := MouseRow - (NewWindowRows >> 1);
    sbHorz.Position:=PageLeft;
    sbVert.Position:=PageTop;

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

procedure TfMain.SaveUndoKeys;
var
  undoblk : TUndoBlock;
begin
  if CurrUndoData.Count > 0 then
  begin
    // save key presses.
    undoblk.UndoType := utCells;
    undoblk.CellData := CurrUndoData.Copy;
    undoblk.CellData.Trim;
    UndoAdd(undoblk);
    CurrUndoData.Clear;
  end;
end;

procedure TfMain.pbPageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  bcolor :        integer;
  dcell :         TCell;
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

    SaveUndoKeys;

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

          lvObjects.ItemIndex := lvObjIndex(SelectedObject);
          lvObjects.Invalidate;
          if (SelectedObject <> -1) then
          begin
            if not Objects[SelectedObject].Locked then
            begin
              dragObjRow := Objects[SelectedObject].Row;
              dragObjCol := Objects[SelectedObject].Col;
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
                CopySelection.Clear;
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
                  dcell.Chr := iif(Button = mbLeft, CurrChar, $20);
                  dcell.Attr := iif(Button = mbLeft, CurrAttr, $0007);
                  RecordUndoCell(MouseRow, MouseCol, dcell);

                  PutCharEx(dcell.Chr, dcell.Attr, MouseRow, MouseCol);
                end;

              dmLeftRights:
                begin
                  bcolor := iif(Button = mbLeft,
                    GetBits(CurrAttr, A_CELL_FG_MASK),
                    GetBits(CurrAttr, A_CELL_BG_MASK, 8));

                  dcell := SetBlockColor(
                    bcolor,
                    Page.Rows[MouseRow].Cells[MouseCol],
                    2, 1, SubX, SubY);
SetBits(dcell.attr, A_CELL_FONT_MASK, CurrFont, 28);
                  RecordUndoCell(MouseRow, MouseCol, dcell);
                  Page.Rows[MouseRow].Cells[MouseCol] := dcell;
                  DrawCell(MouseRow, MouseCol, false);
                  CurrFileChanged := true;
                  UpdateTitles;
                end;

              dmTopBottoms:
                begin
                  bcolor := iif(Button = mbLeft,
                    GetBits(CurrAttr, A_CELL_FG_MASK),
                    GetBits(CurrAttr, A_CELL_BG_MASK, 8));

                  dcell := SetBlockColor(
                    bcolor,
                    Page.Rows[MouseRow].Cells[MouseCol],
                    1, 2, SubX, SubY);
SetBits(dcell.attr, A_CELL_FONT_MASK, CurrFont, 28);
                  RecordUndoCell(MouseRow, MouseCol, dcell);

                  Page.Rows[MouseRow].Cells[MouseCol] := dcell;
                  DrawCell(MouseRow, MouseCol,false);
                  CurrFileChanged := true;
                  UpdateTitles;
                end;

              dmQuarters:
                begin
                  bcolor := iif(Button = mbLeft,
                    GetBits(CurrAttr, A_CELL_FG_MASK),
                    GetBits(CurrAttr, A_CELL_BG_MASK, 8));

                  dcell := SetBlockColor(
                    bcolor,
                    Page.Rows[MouseRow].Cells[MouseCol],
                    2, 2, SubX, SubY);
SetBits(dcell.attr, A_CELL_FONT_MASK, CurrFont, 28);
                  RecordUndoCell(MouseRow, MouseCol, dcell);

                  Page.Rows[MouseRow].Cells[MouseCol] := dcell;
                  DrawCell(MouseRow, MouseCol,false);
                  CurrFileChanged := true;
                  UpdateTitles;
                end;

              // teletext mosaic blocks
              dmSixels:
                begin
                  bcolor := iif(Button = mbLeft,
                    GetBits(CurrAttr, A_CELL_FG_MASK),
                    GetBits(CurrAttr, A_CELL_BG_MASK, 8));

                  dcell := SetBlockColor(
                    bcolor,
                    Page.Rows[MouseRow].Cells[MouseCol],
                    2, 3, SubX, SubY);
SetBits(dcell.attr, A_CELL_FONT_MASK, CurrFont, 28);
                  RecordUndoCell(MouseRow, MouseCol, dcell);

                  Page.Rows[MouseRow].Cells[MouseCol] := dcell;
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
  dcell :           TCell;
//  dattr, dchar :    integer;
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

    pbPage.Invalidate;
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
                    dcell.chr := iif(MouseLeft, CurrChar, $20);
                    dcell.attr := iif(MouseLeft, CurrAttr, $0007);
                    LineCalcInit(LastDrawX, LastDrawY, DrawX, DrawY);
                    repeat
                      done := LineCalcNext(LastDrawX, LastDrawY);
                      RecordUndoCell(LastDrawY, LastDrawX, dcell);
                      PutCharEx(dcell.Chr, dcell.Attr, LastDrawY, LastDrawX);
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

                      dcell := SetBlockColor(
                        bcolor,
                        Page.Rows[mr].Cells[mc],
                        2, 1, sx, sy);
SetBits(dcell.attr, A_CELL_FONT_MASK, CurrFont, 28);

                      RecordUndoCell(mr, mc, dcell);
                      Page.Rows[mr].Cells[mc] := dcell;
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

                      dcell := SetBlockColor(
                        bcolor,
                        Page.Rows[mr].Cells[mc],
                        1, 2, sx, sy);
SetBits(dcell.attr, A_CELL_FONT_MASK, CurrFont, 28);

                      RecordUndoCell(mr, mc, dcell);
                      Page.Rows[mr].Cells[mc] := dcell;
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

                      dcell := SetBlockColor(
                        bcolor,
                        Page.Rows[mr].Cells[mc],
                        2, 2, sx, sy);
SetBits(dcell.attr, A_CELL_FONT_MASK, CurrFont, 28);

                      RecordUndoCell(mr, mc, dcell);
                      Page.Rows[mr].Cells[mc] := dcell;
                      DrawCell(mr, mc,false);
                      CurrFileChanged := true;
                      UpdateTitles;
                    until done;
                  end;

                dmSixels:
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

                      dcell := SetBlockColor(
                        bcolor,
                        Page.Rows[mr].Cells[mc],
                        2, 3, sx, sy);
SetBits(dcell.attr, A_CELL_FONT_MASK, CurrFont, 28);

                      RecordUndoCell(mr, mc, dcell);
                      Page.Rows[mr].Cells[mc] := dcell;
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

// draw a cell on screen from object on Page.Rows[].Cells[].
// THIS ROUTINE IS TOOOOOOOOO SLOW!
procedure TfMain.DrawCell(row, col : integer; skipUpdate : boolean = true);
var
  cnv :       TCanvas;
  x, y :      integer;
  cell :      TCell;
  objnum :    integer;
  cl1, cl2 :  TColor;
  i :         integer;
  copyrec :   TLoc;
begin
  // compute x, y of row, col
  cnv := pbPage.Canvas;
  x := (col - PageLeft) * CellWidthZ;
  y := (row - PageTop) * CellHeightZ;
  if Between(x, 0, pbPage.Width + CellWidthZ)
    and Between(y, 0, pbPage.Height + CellHeightZ) then
  begin
    objnum := GetObjectCell(row, col, cell);
    if (objnum = -1) then
    begin
      DrawCellEx(cnv, x, y, row, col, skipUpdate);
      // draw selection borders
      for i := CopySelection.Count - 1 downto 0 do
      begin
        CopySelection.Get(@copyrec, i);
        if (copyrec.Row = row) and (copyrec.Col = col) then
        begin
          if not HasBits(copyrec.Neighbors, NEIGHBOR_NORTH) then
            DrawDashLine(cnv,
              x, y,
              x + CellWidthZ, y,
              clSelectionArea1, clSelectionArea2);

          if not HasBits(copyrec.Neighbors, NEIGHBOR_SOUTH) then
            DrawDashLine(cnv,
              x, y + CellHeightZ - 1,
              x + CellWidthZ, y + CellHeightZ - 1,
              clSelectionArea1, clSelectionArea2);

          if not HasBits(copyrec.Neighbors, NEIGHBOR_WEST) then
            DrawDashLine(cnv,
              x, y,
              x, y + CellHeightZ,
              clSelectionArea1, clSelectionArea2);

          if not HasBits(copyrec.Neighbors, NEIGHBOR_EAST) then
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
      // prevent stamping object onto bmp
      if not skipupdate then
        cell := Page.Rows[row].Cells[col];

      DrawCellEx(cnv, x, y, row, col, skipUpdate, true, cell.Chr, cell.Attr);

      // draw object edges
      if ObjectOutlines then
      begin
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

        if not HasBits(cell.neighbors, NEIGHBOR_NORTH) then
          DrawDashLine(cnv, x, y, x + CellWidthZ, y, cl1, cl2);

        if not HasBits(cell.neighbors, NEIGHBOR_SOUTH) then
          DrawDashLine(cnv, x, y + CellHeightZ - 1, x + CellWidthZ, y + CellHeightZ - 1, cl1, cl2);

        if not HasBits(cell.neighbors, NEIGHBOR_WEST) then
          DrawDashLine(cnv, x, y, x, y + CellHeightZ, cl1, cl2);

        if not HasBits(cell.neighbors, NEIGHBOR_EAST) then
          DrawDashLine(cnv, x + CellWidthZ - 1, y, x + CellWidthZ - 1, y + CellHeightZ, cl1, cl2);
      end;
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
    ResizeScrolls;
  end;
end;

// add to selection. no dupes allowed
procedure SelectionAdd(var selection : TRecList; r, c : integer);
var
  neighbor :  integer;
  i, l :      integer;
  copyrec :   TLoc;
begin
  neighbor := 0;
  l := selection.count;
  for i := 0 to l - 1 do
  begin
    selection.Get(@copyrec, i);

    // already exists
    if (copyrec.Row = r) and (copyrec.Col = c) then
      exit;

    // build neighbors
    if (copyrec.Row = r - 1) and (copyrec.Col = c) then
    begin
      SetBit(copyrec.Neighbors, NEIGHBOR_SOUTH, true);
      SetBit(neighbor, NEIGHBOR_NORTH, true);
    end;

    if (copyrec.Row = r + 1) and (copyrec.Col = c) then
    begin
      SetBit(copyrec.Neighbors, NEIGHBOR_NORTH, true);
      SetBit(neighbor, NEIGHBOR_SOUTH, true);
    end;

    if (copyrec.Row = r) and (copyrec.Col = c - 1) then
    begin
      SetBit(copyrec.Neighbors, NEIGHBOR_EAST, true);
      SetBit(neighbor, NEIGHBOR_WEST, true);
    end;

    if (copyrec.Row = r) and (copyrec.Col = c + 1) then
    begin
      SetBit(copyrec.Neighbors, NEIGHBOR_WEST, true);
      SetBit(neighbor, NEIGHBOR_EAST, true);
    end;

    selection.Put(@copyrec, i);
  end;

  copyrec.Row := r;
  copyrec.Col := c;
  copyrec.Neighbors := neighbor;
  selection.Add(@copyrec);
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
  n :           byte;
  copyrec :     TLoc;
  cellrec :     TCell;
begin
  // get bounding box of copy selection
  l := CopySelection.Count;
  maxr := 0;
  maxc := 0;
  minr := 99999;
  minc := 99999;
  for i := 0 to l - 1 do
  begin
    CopySelection.Get(@copyrec, i);
    r := copyrec.Row;
    c := copyrec.Col;
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
  result.Data.Create(sizeof(TCell), rleDoubles);
  for i := 0 to (w * h) - 1 do
  begin
    cellrec.Attr := $0007;
    cellrec.Chr := _EMPTY;
    result.Data.Add(@cellrec);
  end;

  // copyselection moved in
  for i := 0 to l - 1 do
  begin
    CopySelection.Get(@copyrec, i);
    r := copyrec.Row;
    c := copyrec.Col;
    p := ((r - minr) * w) + (c - minc);
    result.Data.Get(@cellrec, p);
    cellrec.Chr := Page.Rows[r].Cells[c].Chr;
    cellrec.Attr := Page.Rows[r].Cells[c].Attr;
    result.Data.Put(@cellrec, p);
  end;

  // populate the neighbors in cells
  // copyrec has neighbors - copy it!
  p := 0;
  for r := 0 to h - 1 do
  begin
    for c := 0 to w - 1 do
    begin
      n := 0;

      if r > 0 then
      begin
        result.Data.Get(@cellrec, p - w);
        if cellrec.Chr <> _EMPTY then
          n := n or NEIGHBOR_NORTH;
      end;

      if r < h - 1 then
      begin
        result.Data.Get(@cellrec, p + w);
        if cellrec.Chr <> _EMPTY then
          n := n or NEIGHBOR_SOUTH;
      end;

      if c > 0 then
      begin
        result.Data.Get(@cellrec, p - 1);
        if cellrec.Chr <> _EMPTY then
          n := n or NEIGHBOR_WEST;
      end;

      if c < w - 1 then
      begin
        result.Data.Get(@cellrec, p + 1);
        if cellrec.Chr <> _EMPTY then
          n := n or NEIGHBOR_EAST;
      end;

      result.Data.Get(@cellrec, p);
      cellrec.Neighbors:=n;
      result.Data.Put(@cellrec, p);

      p += 1;
    end;
  end;

  // normalize : make ul of bounding box 0,0 for object
  result.Row := 0;
  result.Col := 0;
  result.Locked:=false;
  result.Hidden:=false;
  result.Data.Trim;
  result.Name := '[Clipboard]';
end;

function TfMain.BuildDisplayCopySelection : TRecList;
var
  i, l,
  r, c,
  r1, c1,
  r2, c2 :  integer;
  copyrec : TLoc;
begin
  result.Create(sizeof(TLoc), rleDoubles);
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
  l := CopySelection.Count;
  for i := 0 to l - 1 do
  begin
    CopySelection.Get(@copyrec, i);
    // add if not already in selection and not inside a remove drag
    if drag and (dragType = DRAG_REMOVE) then
    begin
      if not between(copyrec.Row, r1, r2)
        or not between(copyrec.Col, c1, c2) then
        SelectionAdd(result, copyrec.Row, copyrec.Col);
    end
    else
      SelectionAdd(result, copyrec.Row, copyrec.Col);
  end;
end;

procedure TfMain.lvObjectsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  lv :  TListView;
begin
  // click on lock/unlock hide/shown
  lv := TListView(Sender);
  SelectedObject := lvObjIndex(lv.ItemIndex);
  if between(X, 6, 6 + 16) then
  begin
    Objects[SelectedObject].Locked := not Objects[SelectedObject].Locked;
    lvObjects.Invalidate;
  end
  else if between(X, 24, 24 + 16) then
  begin
    Objects[SelectedObject].Hidden := not Objects[SelectedObject].Hidden;
    lvObjects.Invalidate;
    fPreviewBox.Invalidate;
  end;
  pbPage.Invalidate;
end;

procedure TfMain.RefreshObject(objnum : integer);
var
  r, c : integer;
  rr, cc : integer;
begin
  if objnum >= 0 then
  begin
    rr := Objects[objnum].Row;
    for r := Objects[objnum].Height - 1 downto 0 do
    begin
      cc := Objects[objnum].Col;
      for c := Objects[objnum].Width - 1 downto 0 do
      begin
        if between(rr, 0, NumRows - 1) and between(cc, 0, NumCols - 1) then
          DrawCell(rr, cc);
        cc += 1;
      end;
      rr += 1;
    end;
  end;
end;

procedure TfMain.DoObjFlipHorz(objnum : integer);
var
  po :            PObj;
  i, j, r, c :    integer;
  p1, p2 :        integer;
  fnt :           integer;
  cp :            TEncoding;
  chr, flipchr :  integer;
  n :             byte;
  cellrec : TCell;
begin
  po := @Objects[objnum];

  for r := 0 to po^.Height - 1 do
  begin
    // flip this rows character positions

    for c := 1 to po^.Width div 2 do
    begin
      p1 := (r * po^.Width) + (c - 1);
      p2 := ((r + 1) * po^.Width) - 1 - (c - 1);
      po^.Data.Swap(p1, p2);
    end;

    //  flip the characters in the row if we can, plus neighbors
    p1 := (r * po^.Width);
    for c := 0 to po^.Width - 1 do
    begin
      po^.Data.Get(@cellrec, p1);

      // get encoding for this character
      fnt := GetBits(cellrec.Attr, A_CELL_FONT_MASK, 28);
      cp := Fonts[fnt];
      chr := GetUnicode(cellrec);
      i := 0;
      while i < CPages[cp].MirrorTableSize do
      begin
        if CPages[cp].MirrorTable[i] = chr then
        begin
          flipchr := CPages[cp].MirrorTable[i + 1];

          // convert flip unicode back to this codepage
          if Fonts[CurrFont] in [ encUTF8, encUTF16] then
            cellrec.Chr := flipchr
          else
            for j := 0 to 255 do
              if flipchr = CPages[cp].EncodingLUT[j] then
              begin
                cellrec.Chr := j;
                break;
              end;
          break;
        end;
        i += 3;
      end;

      n := cellrec.Neighbors;
      cellrec.Neighbors :=
        (n and %0101) or ((n and %1000) >> 2) or ((n and %0010) << 2);

      po^.Data.Put(@cellrec, p1);
      p1 += 1;
    end;

    pbPage.Invalidate;
    fPreviewBox.Invalidate;
  end;
end;

procedure TfMain.DoObjFlipVert(objnum : integer);
var
  po :            PObj;
  i, j, r, c :    integer;
  p1, p2 :        integer;
  fnt :           integer;
  cp :            TEncoding;
  chr, flipchr :  integer;
  n :             byte;
  cellrec :       TCell;
begin
  po := @Objects[objnum];
  for c := 0 to po^.Width - 1 do
  begin

    // flip this rows character positions
    for r := 1 to po^.Height div 2 do
    begin
      p1 := ((r - 1) * po^.Width) + c;
      p2 := ((po^.Height - r) * po^.Width) + c;
      po^.Data.Swap(p1, p2);
    end;

    //  flip the characters in the row if we can
    p1 := c;
    for r := 0 to po^.Height - 1 do
    begin
      po^.Data.Get(@cellrec, p1);

      // get encoding number for this character
      fnt := GetBits(cellrec.Attr, A_CELL_FONT_MASK, 28);
      cp := Fonts[fnt];
        chr := GetUnicode(cellrec);
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
              cellrec.Chr := j;
              break;
            end;
          break;
        end;
        i += 3;
      end;

      n := cellrec.Neighbors;
      cellrec.Neighbors :=
        (n and %1010) or ((n and %0100) >> 2) or ((n and %0001) << 2);

      po^.Data.Put(@cellrec, p1);
      p1 += po^.Width;
    end;

    pbPage.Invalidate;
    fPreviewBox.Invalidate;
  end;
end;

procedure TfMain.ObjFlipHorz;
var
  undoblk : TUndoBlock;
begin
  if SelectedObject >= 0 then
  begin
    SaveUndoKeys;
    DoObjFlipHorz(SelectedObject);
    undoblk.UndoType:= utObjFlipHorz;
    undoblk.OldNum := SelectedObject;
    UndoAdd(undoblk);
  end;
end;

procedure TfMain.ObjFlipVert;
var
  undoblk : TUndoBlock;
begin
  if SelectedObject >= 0 then
  begin
    SaveUndoKeys;
    DoObjFlipVert(SelectedObject);
    undoblk.UndoType:= utObjFlipHorz;
    undoblk.OldNum := SelectedObject;
    UndoAdd(undoblk);
  end;
end;

procedure TfMain.ObjMoveBack;
var
  undoblk : TUndoBlock;
  tmp :     TObj;
begin
  if SelectedObject > 0 then
  begin
    SaveUndoKeys;
    undoblk.UndoType := utObjMove;
    undoblk.NewRow := Objects[SelectedObject].Row;
    undoblk.NewCol := Objects[SelectedObject].Col;
    undoblk.OldRow := undoblk.NewRow;
    undoblk.OldCol := undoblk.NewCol;
    undoblk.OldNum := SelectedObject;

    // swap objects
    tmp := Objects[SelectedObject];
    Objects[SelectedObject] := Objects[SelectedObject - 1];
    Objects[SelectedObject - 1] := tmp;
    SelectedObject -= 1;
    LoadlvObjects;
    lvObjects.ItemIndex := lvObjIndex(SelectedObject);

    // add to undo.
    undoblk.NewNum := SelectedObject;
    UndoAdd(undoblk);

    pbPage.Invalidate;
    fPreviewBox.Invalidate;
  end;
end;

procedure TfMain.ObjMoveForward;
var
  undoblk : TUndoBlock;
  tmp :     TObj;
begin
  if SelectedObject < length(Objects) - 1 then
  begin
    SaveUndoKeys;
    undoblk.UndoType := utObjMove;
    undoblk.NewRow := Objects[SelectedObject].Row;
    undoblk.NewCol := Objects[SelectedObject].Col;
    undoblk.OldRow := undoblk.NewRow;
    undoblk.OldCol := undoblk.NewCol;
    undoblk.OldNum := SelectedObject;

    // swap objects
    tmp := Objects[SelectedObject];
    Objects[SelectedObject] := Objects[SelectedObject + 1];
    Objects[SelectedObject + 1] := tmp;
    SelectedObject += 1;
    LoadlvObjects;
    lvObjects.ItemIndex := lvObjIndex(SelectedObject);

    // add to undo.
    undoblk.NewNum := SelectedObject;
    UndoAdd(undoblk);

    pbPage.Invalidate;
    fPreviewBox.Invalidate;
  end;
end;

procedure TfMain.ObjMoveToBack;
var
  undoblk : TUndoBlock;
  tmp :     TObj;
begin
  if SelectedObject > 0 then
  begin
    SaveUndoKeys;
    undoblk.UndoType := utObjMove;
    undoblk.NewRow := Objects[SelectedObject].Row;
    undoblk.NewCol := Objects[SelectedObject].Col;
    undoblk.OldRow := undoblk.NewRow;
    undoblk.OldCol := undoblk.NewCol;
    undoblk.OldNum := SelectedObject;

    tmp := Objects[SelectedObject];
    RemoveObject(SelectedObject);
    InsertObject(tmp, 0);
    SelectedObject := 0;
    LoadlvObjects;
    lvObjects.ItemIndex := lvObjIndex(SelectedObject);

    // add to undo.
    undoblk.NewNum := SelectedObject;
    UndoAdd(undoblk);

    pbPage.Invalidate;
    fPreviewBox.Invalidate;
  end;
end;

procedure TfMain.ObjMoveToFront;
var
  l :       integer;
  tmp :     TObj;
  undoblk : TUndoBlock;
begin
  l := length(Objects);
  if SelectedObject < l - 1 then
  begin
    SaveUndoKeys;
    undoblk.UndoType := utObjMove;
    undoblk.NewRow := Objects[SelectedObject].Row;
    undoblk.NewCol := Objects[SelectedObject].Col;
    undoblk.OldRow := undoblk.NewRow;
    undoblk.OldCol := undoblk.NewCol;
    undoblk.OldNum := SelectedObject;

    tmp := Objects[SelectedObject];
    RemoveObject(SelectedObject);
    InsertObject(tmp, l - 1);
    SelectedObject := l - 1;
    LoadlvObjects;
    lvObjects.ItemIndex := lvObjIndex(SelectedObject);

    // add to undo.
    undoblk.NewNum := SelectedObject;
    UndoAdd(undoblk);

    pbPage.Invalidate;
    fPreviewBox.Invalidate;
  end;
end;

procedure TfMain.RemoveObject(objnum : integer);
var
  i : integer;
begin
  Objects[objnum].Data.Free;
  for i := objnum + 1 to length(Objects) - 1 do
    Objects[i - 1] := Objects[i];
  Setlength(Objects, length(Objects) - 1);
end;

procedure TfMain.InsertObject(obj : TObj; pos : integer);
var
  i, l : integer;
begin
  l := length(Objects);
  Setlength(Objects, l + 1);
  for i := l - 1 downto pos do
    Objects[i + 1] := Objects[i];
  CopyObject(obj, Objects[pos]);
//  Objects[pos] := obj;
end;

procedure TfMain.ObjMerge;
var
  po :      PObj;
  p :       integer;
  r, c :    integer;
  cellrec : TCell;
  undoblk : TUndoBlock;
begin
  // merge this object to page. and remove object
  if SelectedObject >= 0 then
  begin
    SaveUndoKeys;

    po := @Objects[SelectedObject];
    p := 0;
    for r := po^.Row to po^.Row + po^.Height - 1 do
      for c := po^.Col to po^.Col + po^.Width - 1 do
      begin
        po^.Data.Get(@cellrec, p);
        if between(r, 0, NumRows - 1) and between(c, 0, NumCols - 1) then
          if cellrec.Chr <> _EMPTY then
          begin
            RecordUndoCell(r, c, cellrec);
            Page.Rows[r].Cells[c] := cellrec;
          end;
        p += 1;
      end;

    undoblk.UndoType := utObjMerge;
    undoblk.CellData := CurrUndoData.Copy;
    CurrUndoData.Clear;
    undoblk.CellData.Trim;
    undoblk.OldRow := Objects[SelectedObject].Row;
    undoblk.OldCol := Objects[SelectedObject].Col;
    undoblk.OldNum := SelectedObject;
    CopyObject(Objects[SelectedObject], undoblk.Obj);
    UndoAdd(undoblk);

    RemoveObject(SelectedObject);
    SelectedObject := -1;


    LoadlvObjects;
    lvObjects.ItemIndex := lvObjIndex(SelectedObject);
    GenerateBmpPage;
    pbPage.Invalidate;
    fPreviewBox.Invalidate;
  end;
end;

procedure TfMain.ObjMergeAll;
var
  po :      PObj;
  p :       integer;
  r, c :    integer;
  objnum :  integer;
  cellrec : TCell;
  undoblk : TUndoBlock;
begin
  // merge this object to page. and remove object
  SelectedObject := -1;
  if length(Objects) > 0 then
  begin
    SaveUndoKeys;
    for objnum := 0 to length(Objects) - 1 do
    begin

      po := @Objects[objnum];
      p := 0;
      for r := po^.Row to po^.Row + po^.Height - 1 do
        for c := po^.Col to po^.Col + po^.Width - 1 do
        begin
          po^.Data.Get(@cellrec, p);
          if between(r, 0, NumRows - 1) and between(c, 0, NumCols - 1) then
            if cellrec.Chr <> _EMPTY then
            begin
              RecordUndoCell(r, c, cellrec);
              Page.Rows[r].Cells[c] := cellrec;
            end;
          p += 1;
        end;

      undoblk.UndoType := utObjMerge;
      undoblk.CellData := CurrUndoData.Copy;
      CurrUndoData.Clear;
      undoblk.CellData.Trim;
      undoblk.OldRow := Objects[objnum].Row;
      undoblk.OldCol := Objects[objnum].Col;
      undoblk.OldNum := 0;
      CopyObject(Objects[objnum], undoblk.Obj);
      UndoAdd(undoblk);

      Objects[objnum].Data.Free;
    end;

    setlength(Objects, 0);
    LoadlvObjects;
    lvObjects.ItemIndex := lvObjIndex(SelectedObject);
    GenerateBmpPage;
    pbPage.Invalidate;
    fPreviewBox.Invalidate;
  end;
end;

procedure TfMain.ObjNext;
begin
  if length(Objects) >= 0 then
  begin
    SelectedObject += 1;
    if SelectedObject >= length(Objects) then
      SelectedObject := 0;
    lvObjects.ItemIndex := lvObjIndex(SelectedObject);
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
    lvObjects.ItemIndex := lvObjIndex(SelectedObject);
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

procedure TfMain.bObjMergeAllClick(Sender: TObject);
begin
  ObjMergeAll;
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

procedure TfMain.bObjLoadClick(Sender: TObject);
var
  fin :     TFileStream;
  head :    TVTXObjHeader;
  i, l :    integer;
  obj :     TObj;
  cellrec : TCell;
const
  ID = 'VTXEDIT';
begin
  if odObject.Execute then
  begin
    fin := TFileStream.Create(odObject.FileName, fmOpenRead or fmShareDenyNone);

    fin.Read(head, sizeof(TVTXObjHeader));

    if not MemComp(@ID[0], @head.ID, 7) then
    begin
      ShowMessage('Bad Header.');
      fin.Free;
      exit;
    end;
    if head.Version <> $0001 then
    begin
      ShowMessage('Bad Version.');
      fin.Free;
      exit;
    end;

    if head.PageType <> 3 then
    begin
      ShowMessage('Not an Object file.');
      fin.Free;
      exit;
    end;

    obj.Name := ExtractFileNameOnly(odObject.FileName);
    obj.Width := head.Width;
    obj.Height := head.Height;
    obj.Image := false;
    obj.Locked := false;
    obj.Hidden := false;

    obj.Data.Create(sizeof(TCell), rleDoubles);
    for i := 0 to head.Width * head.Height - 1 do
    begin
      fin.Read(cellrec, sizeof(TCell));
      obj.Data.Add(@cellrec);
    end;
    fin.free;

    l := length(Objects);
    setlength(Objects, l + 1);
    CopyObject(Obj, Objects[l]);
    obj.Data.Free;

    // drop it onto window. top left for now
    Objects[l].Row := PageTop;
    Objects[l].Col := PageLeft;
    LoadlvObjects;
    pbPage.Invalidate;
  end
end;

// save selected object to file
procedure TfMain.bObjSaveClick(Sender: TObject);
var
  fout :    TFileStream;
  head :    TVTXObjHeader;
  i :       integer;
  fname :   string;
  cellrec : TCell;
const
  ID = 'VTXEDIT';
begin
  if SelectedObject >= 0 then
  begin
    sdObject.FileName := Objects[SelectedObject].Name + '.vof';
    if sdObject.Execute then
    begin
      fout := TFileStream.Create(sdObject.FileName, fmCreate or fmShareExclusive);
      MemCopy(@ID[0], @head.ID, 7);
      head.ID[7] := 0;
      head.Version := $0001;
      head.PageType := 3;
      head.Width := Objects[SelectedObject].Width;
      head.Height := Objects[SelectedObject].Height;
      MemZero(@head.Name[0], 64);
      fname := ExtractFileNameOnly(sdObject.FileName);
      for i := 0 to length(sdObject.Filename) - 1 do
      begin
        if i >= 63 then break;
        head.Name[i] := fname.Chars[i];
      end;
      fout.Write(head, sizeof(TVTXObjHeader));
      for i := 0 to head.Height * head.Width - 1 do
      begin
        Objects[SelectedObject].Data.Get(@cellrec, i);
        fout.Write(cellrec, sizeof(TCell));
      end;
      fout.free;
    end;
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

  cnv.FillRect(ARect.Left + 4, ARect.Top, ARect.Right, ARect.Bottom);

  ilButtons.GetBitmap(iif(Objects[lvObjIndex(AItem.Index)].Locked, 57, 56), bmp);
  cnv.Draw(ARect.Left + 6, ARect.Top,bmp);
  ilButtons.GetBitmap(iif(Objects[lvObjIndex(AItem.Index)].Hidden, 58, 59), bmp);
  cnv.Draw(ARect.Left + 24, ARect.Top,bmp);
  bmp.free;

  cnv.Brush.Style := bsClear;
  val := AItem.Caption;
  cnv.TextOut(ARect.Left + 48, ARect.Top, val);
end;

procedure TfMain.lvObjectsEdited(Sender: TObject; Item: TListItem;
  var AValue: string);
begin
  ObjectRename := false;
  Objects[Item.Index].Name := AValue;
  nop;
end;

procedure TfMain.lvObjectsEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  ObjectRename := true;
  nop;
end;

// draw the document
procedure TfMain.pbPagePaint(Sender: TObject);
var
  panel :     TPaintBox;
  cnv :       TCanvas;
  r, c :      integer;
  pr :        TRect;
  tmp, tmp2 : TBGRABitmap;
  tmpreg :    TRecList;
  i :         integer;
  x, y :      integer;
  cell :      TCell;
  objnum :    integer;
  cl1, cl2 :  TColor;
  copyrec :   TLoc;

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
  tmp.Free;

  // display page from bitmap
  cnv.Draw(0, 0, tmp2.Bitmap);
  tmp2.free;

  // draw selection information
  tmpreg := BuildDisplayCopySelection;
  for i := tmpreg.Count - 1 downto 0 do
  begin
    tmpreg.Get(@copyrec, i);
    pr.Top := (copyrec.Row - PageTop) * CellHeightZ;
    pr.Left := (copyrec.Col - PageLeft) * CellWidthZ;
    pr.Width := CellWidthZ;
    pr.Height := CellHeightZ;
    if not HasBits(copyrec.Neighbors, NEIGHBOR_NORTH) then
      DrawDashLine(cnv,
        pr.Left, pr.Top,
        pr.Left + CellWidthZ, pr.Top,
        clSelectionArea1, clSelectionArea2);
    if not HasBits(copyrec.Neighbors, NEIGHBOR_SOUTH) then
      DrawDashLine(cnv,
        pr.Left, pr.Top + CellHeightZ - 1,
        pr.Left + CellWidthZ, pr.Top + CellHeightZ - 1,
        clSelectionArea1, clSelectionArea2);
    if not HasBits(copyrec.Neighbors, NEIGHBOR_WEST) then
      DrawDashLine(cnv,
        pr.Left, pr.Top,
        pr.Left, pr.Top + CellHeightZ - 1,
        clSelectionArea1, clSelectionArea2);
    if not HasBits(copyrec.Neighbors, NEIGHBOR_EAST) then
      DrawDashLine(cnv,
        pr.Left + CellWidthZ - 1, pr.Top,
        pr.Left + CellWidthZ - 1, pr.Top + CellHeightZ,
        clSelectionArea1, clSelectionArea2);
  end;
  tmpreg.Free;

  // draw objects over top
  for r := PageTop to PageTop + WindowRows do
  begin
    if r >= NumRows then break;

      y := (r - PageTop) * CellHeightZ;
      for c := PageLeft to PageLeft + WindowCols do
      begin
        if c >= NumCols then break;

        x := (c - PageLeft) * CellWidthZ;
        objnum := GetObjectCell(r, c, cell);
        if (objnum <> -1) and (cell.Chr <> _EMPTY) then
        begin
          // object here.
          DrawCellEx(cnv, x, y,  r, c, true,  false, cell.Chr, cell.Attr);
          if ObjectOutlines then
          begin
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
            if not HasBits(cell.neighbors, NEIGHBOR_NORTH) then
              DrawDashLine(cnv,
                x, y,
                x + CellWidthZ, y,
                cl1, cl2);
            if not HasBits(cell.neighbors, NEIGHBOR_SOUTH) then
              DrawDashLine(cnv,
                x, y + CellHeightZ - 1,
                x + CellWidthZ, y + CellHeightZ - 1,
                cl1, cl2);
            if not HasBits(cell.neighbors, NEIGHBOR_WEST) then
              DrawDashLine(cnv,
                x, y,
                x, y + CellHeightZ,
                cl1, cl2);
            if not HasBits(cell.neighbors, NEIGHBOR_EAST) then
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

// draw the current cell preview
procedure TfMain.pbCurrCellPaint(Sender: TObject);
var
  pb :        TPaintBox;
  cnv :       TCanvas;
  r, rarea :  TRect;
  bmp :       TBGRABitmap;
  off, ch :   integer;
  cp :        TEncoding;
  h, w :      integer;
  fg, bg :    integer;
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
  if cp in [ encUTF8, encUTF16 ] then
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
  DrawStretchedBitmap(cnv, r, bmp);

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
  DrawStretchedBitmap(cnv, r, bmp);
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
  usech : uint16 = _EMPTY;   // overrides
  useattr : uint32 = $FFFF);
var
  bmp, bmp2 :     TBGRABitmap;
  ch :            Uint16;
  off :           integer;
  attr :          Uint32;
  rect :          TRect;
  bslow, bfast :  boolean;
  cp :            TEncoding;
  fntnum :        integer;
begin
  if bmpPage = nil then exit; // ?!

  if between(row, PageTop, PageTop + WindowRows)
    and Between(col, PageLeft, PageLeft + WindowCols) then
  begin
    // on screen.

    // load page cell
    if usech = _EMPTY then
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
    if PageType in [ PAGETYPE_CTERM, PAGETYPE_VTX ] then
    begin
      fntnum := GetBits(attr, A_CELL_FONT_MASK, 28);
      if (fntnum >= 10) and (PageType <> PAGETYPE_VTX) then
        fntnum := 0;
      cp := Fonts[fntnum];
    end
    else
    begin
      cp := Fonts[0];
    end;
    if cp in [ encUTF8, encUTF16 ] then
      off := GetGlyphOff(ch, CPages[cp].GlyphTable, CPages[cp].GlyphTableSize)
    else
    begin
      if ch > 255 then
        ch := 0;
      off := CPages[cp].QuickGlyph[ch];
    end;

    // create bmp for cell
    bmp := TBGRABitmap.Create(8,16);
    if not skipUpdate then
    begin
      GetGlyphBmp(bmp, CPages[cp].GlyphTable, off, attr, false);
      bmpPage.Canvas.Draw(col * CellWidth, row * CellHeight, bmp.Bitmap);

      bmp.ResampleFilter := rfMitchell;
      bmp2 := bmp.Resample(CellWidth>>2, CellHeight >>2) as TBGRABitmap;
      bmpPreview.Canvas.Draw(col * (CellWidth >> 2), row * (CellHeight >> 2), bmp2.Bitmap);
      bmp2.free;

      fPreviewBox.Invalidate;

      if bslow and not BlinkSlow then
      begin
        SetBits(attr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_CONCEAL); // hide blink
        GetGlyphBmp(bmp, CPages[cp].GlyphTable, off, attr, false);
      end;

      if bfast and not BlinkFast and (ColorScheme <> COLORSCHEME_ICE) then
      begin
        SetBits(attr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_CONCEAL); // hide blink
        GetGlyphBmp(bmp, CPages[cp].GlyphTable, off, attr, false);
      end;
    end
    else
    begin
      if bslow and not BlinkSlow then
        SetBits(attr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_CONCEAL); // hide blink
      if bfast and not BlinkFast and (ColorScheme <> COLORSCHEME_ICE) then
        SetBits(attr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_CONCEAL); // hide blink
      GetGlyphBmp(bmp, CPages[cp].GlyphTable, off, attr, false);
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
      DrawStretchedBitmap(cnv, rect, bmp);
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

        KA_TOOLPAINT:
          tbToolPaint.Hint := tbToolPaint.Hint+' ' + KeyBinds[i].KeyStr;

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
        KA_FILESAVEAS:        miFileSaveAs.ShortCut := shortcut;
        KA_FILEIMPORT:        miFileImport.ShortCut := shortcut;
        KA_FILEEXPORT:        miFileExport.ShortCut := shortcut;
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
        KA_OBJECTMERGEALL:    miObjMergeAll.ShortCut := shortcut;
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

// append a cell to an undo cell delta reclist
procedure TfMain.RecordUndoCell(row, col : uint16; newcell : TCell);
var
  rec :   TUndoCells;
  i, l :  integer;
begin
  // look for this cell in undo data
  l := CurrUndoData.Count;
  for i := 0 to l - 1 do
  begin
    CurrUndoData.Get(@rec, i);
    if (rec.Row = row) and (rec.Col = col) then
    begin
      // if exists, update the new cell - if different
      if (newcell.Chr <> rec.NewCell.Chr)
        or (newcell.Attr <> rec.NewCell.Attr) then
      begin
        rec.NewCell := newcell;
        CurrUndoData.Put(@rec, i);
      end;
      exit;
    end;
  end;
  // new record - if different
  if (newcell.Chr <> Page.Rows[row].Cells[col].Chr)
    or (newcell.Attr <> Page.Rows[row].Cells[col].Attr) then
  begin
    rec.Row := row;
    rec.Col := col;
    rec.NewCell := newcell;
    rec.OldCell := Page.Rows[row].Cells[col];
    CurrUndoData.Add(@rec);
  end;
end;

// clear all data from pos to end of list. move list count down.
procedure TfMain.UndoTruncate(pos : integer);
var
  i : integer;
  undoblk : TUndoBlock;
begin
  for i := pos to Undo.Count - 1 do
  begin
    Undo.Get(@undoblk, i);
    case undoblk.UndoType of

      utCells:      undoblk.CellData.Free;

      utObjAdd:     undoblk.Obj.Data.Free;
      utObjRemove:  undoblk.Obj.Data.Free;

      utObjMerge:
        begin
          undoblk.Obj.Data.Free;
          undoblk.CellData.Free;
        end;
    end;
  end;
  Undo.Count := pos;
end;

// truncate if needed. add undo block to undo list at undopos.
procedure TfMain.UndoAdd(undoblk : TUndoBlock);
var
  tmpblk : TUndoBlock;
begin
  // don't add empty cell deltas
  if (undoblk.UndoType = utCells) and (undoblk.CellData.Count = 0) then exit;

  UndoTruncate(UndoPos);
  Undo.Add(@undoblk);

  // remove top if beyond UNDO_LEVELS
  if Undo.Count > UNDO_LEVELS then
  begin
    Undo.Get(@tmpblk, 0);
    case tmpblk.UndoType of

      utCells:      tmpblk.CellData.Free;

      utObjAdd:     tmpblk.Obj.Data.Free;
      utObjRemove:  tmpblk.Obj.Data.Free;

      utObjMerge:
        begin
          tmpblk.Obj.Data.Free;
          tmpblk.CellData.Free;
        end;
    end;
    Undo.Remove(0);
    UndoPos -= 1;
  end;

  UndoPos += 1;
end;

// undo stuff at this pos
procedure TfMain.UndoPerform(pos : integer);
var
  undocell :  TUndoCells;
  i :         integer;
  undoblk :   TUndoBlock;
  tmpobj :    TObj;
begin
  Undo.Get(@undoblk, pos);
  case undoblk.UndoType of
    utCells:
      begin
        for i := 0 to undoblk.CellData.Count - 1 do
        begin
          undoblk.CellData.Get(@undocell, i);
          ResizePage(undocell.Row, undocell.Col);
          Page.Rows[undocell.Row].Cells[undocell.Col] := undocell.OldCell;
        end;
        GenerateBmpPage;
        pbPage.Invalidate;
        CurrFileChanged := true;
      end;

    utObjMove:
      begin
        // move from oldrow/col/num to newrow/col/num
        if undoblk.OldNum <> undoblk.NewNum then
        begin
          // move forward / back
          tmpobj := Objects[undoblk.NewNum];
          RemoveObject(undoblk.NewNum);
          InsertObject(tmpobj, undoblk.OldNum);
        end;
        if (undoblk.OldRow <> undoblk.NewRow)
          or (undoblk.OldCol <> undoblk.NewCol) then
        begin
          Objects[undoblk.OldNum].Row := undoblk.OldRow;
          Objects[undoblk.OldNum].Col := undoblk.OldCol;
        end;
        pbPage.Invalidate;
      end;

    utObjAdd:
      begin
        // an added object on top. remove it.
        SelectedObject := -1;
        RemoveObject(length(Objects) - 1);
        LoadlvObjects;
        pbPage.Invalidate;
      end;

    utObjRemove:
      begin
        // readd at oldnum
        SelectedObject := -1;
        InsertObject(undoblk.Obj, undoblk.OldNum);
        LoadlvObjects;
        pbPage.Invalidate;
      end;

    utObjMerge:
      begin
        for i := 0 to undoblk.CellData.Count - 1 do
        begin
          undoblk.CellData.Get(@undocell, i);
          ResizePage(undocell.Row, undocell.Col);
          Page.Rows[undocell.Row].Cells[undocell.Col] := undocell.OldCell;
        end;
        SelectedObject := -1;
        InsertObject(undoblk.Obj, undoblk.OldNum);
        LoadlvObjects;
        GenerateBmpPage;
        pbPage.Invalidate;
        CurrFileChanged := true;
      end;

    utObjFlipHorz:
      begin
        DoObjFlipHorz(undoblk.OldNum);
      end;

    utObjFlipVert:
      begin
        DoObjFlipVert(undoblk.OldNum);
      end;

  end;
  CurrFileChanged := true;
end;

// redo
procedure TfMain.RedoPerform(pos : integer);
var
  undocell :  TUndoCells;
  i, l :      integer;
  undoblk :   TUndoBlock;
  tmpobj :    TObj;
begin
  Undo.Get(@undoblk, pos);
  case undoblk.UndoType of
    utCells:
      begin
        for i := 0 to undoblk.CellData.Count - 1 do
        begin
          undoblk.CellData.Get(@undocell, i);
          ResizePage(undocell.Row, undocell.Col);
          Page.Rows[undocell.Row].Cells[undocell.Col] := undocell.NewCell;
        end;
        GenerateBmpPage;
        pbPage.Invalidate;
        CurrFileChanged := true;
      end;

    utObjMove:
      begin
        // move from oldrow/col/num to newrow/col/num
        if undoblk.OldNum <> undoblk.NewNum then
        begin
          // move forward / back
          tmpobj := Objects[undoblk.OldNum];
          RemoveObject(undoblk.OldNum);
          InsertObject(tmpobj, undoblk.NewNum);
        end;
        if (undoblk.OldRow <> undoblk.NewRow)
          or (undoblk.OldCol <> undoblk.NewCol) then
        begin
          Objects[undoblk.NewNum].Row := undoblk.NewRow;
          Objects[undoblk.NewNum].Col := undoblk.NewCol;
        end;
        pbPage.Invalidate;
      end;

    utObjAdd:
      // readd object
      begin
        SelectedObject := -1;
        l := length(Objects);
        setlength(Objects, l + 1);
        CopyObject(undoblk.Obj, Objects[l]);
        Objects[l].Row := undoblk.OldRow;
        Objects[l].Col := undoblk.OldCol;
        LoadlvObjects;
        pbPage.Invalidate;
      end;

    utObjRemove:
      begin
        // reremove object
        SelectedObject := -1;
        RemoveObject(undoblk.OldNum);
        LoadlvObjects;
        pbPage.Invalidate;
      end;

    utObjMerge:
      begin
        for i := 0 to undoblk.CellData.Count - 1 do
        begin
          undoblk.CellData.Get(@undocell, i);
          ResizePage(undocell.Row, undocell.Col);
          Page.Rows[undocell.Row].Cells[undocell.Col] := undocell.NewCell;
        end;
        SelectedObject := -1;
        RemoveObject(undoblk.OldNum);
        LoadlvObjects;
        GenerateBmpPage;
        pbPage.Invalidate;
        CurrFileChanged := true;
      end;

    utObjFlipHorz:
      begin
        DoObjFlipHorz(undoblk.OldNum);
      end;

    utObjFlipVert:
      begin
        DoObjFlipVert(undoblk.OldNum);
      end;

  end;
end;

procedure TfMain.ClearAllUndo;
begin
  // clear any UndoData left over.
  UndoTruncate(0);
  UndoPos:=0;
end;

procedure TfMain.OpenVTXFile(fname : string);
var
  fin :         TFileStream;
  head :        TVTXFileHeader;
  i, j, r, c :  integer;
  numobj :      integer;
  b :           byte;
  namein :      packed array [0..63] of char;
  cellrec :     TCell;
const
  ID = 'VTXEDIT';
begin
  fin := TFileStream.Create(fname, fmOpenRead or fmShareDenyNone);
  fin.Read(head, sizeof(TVTXFileHeader));

  if not MemComp(@ID[0], @head.ID, 7) then
  begin
    ShowMessage('Bad Header.');
    exit;
  end;
  if head.Version <> $0001 then
  begin
    ShowMessage('Bad Version.');
    exit;
  end;

  NewFile;

  PageType := head.PageType;
  for i := 0 to 15 do
    Fonts[i] := TEncoding(head.Fonts[i]);
  ColorScheme := head.Colors;
  NumRows := head.Height;
  NumCols := head.Width;
  XScale := head.XScale;
  Page.PageAttr := head.PageAttr;
  Page.CrsrAttr := head.CrsrAttr;
  MemCopy(@head.Sauce, @Page.Sauce, sizeof(TSauceHeader));
  numobj := head.NumObjects;

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
      // don't save / load neighbors.
      fin.ReadBuffer(Page.Rows[r].Cells[c].Chr, sizeof(Word));
      fin.ReadBuffer(Page.Rows[r].Cells[c].Attr, sizeof(DWord));
    end;
  end;

  // load objects
  setlength(Objects, numobj);
  for i := 0 to numobj - 1 do
  begin
    Objects[i].Row := fin.ReadWord;
    Objects[i].Col := fin.ReadWord;
    Objects[i].Width := fin.ReadWord;
    Objects[i].Height := fin.ReadWord;
    Objects[i].Locked := (fin.ReadByte = 1);
    Objects[i].Hidden := (fin.ReadByte = 1);

    // get name
    fin.Read(namein, 64);
    Objects[i].Name := namein;
    Objects[i].Data.Create(sizeof(TCell), rleDoubles);
    for j := 0 to Objects[i].Width * Objects[i].Height - 1 do
    begin
      // don't load / save neighbors
      fin.ReadBuffer(cellrec.Chr, sizeof(Word));
      fin.ReadBuffer(cellrec.Attr, sizeof(DWord));
      Objects[i].Data.Add(@cellrec);
    end;
  end;

  fin.free;
end;

procedure TfMain.SaveVTXFile(fname : string);
var
  fout : TFileStream;
  head : TVTXFileHeader;
  i, j, r, c : integer;
  nameout : packed array [0..63] of char;
  cellrec : TCell;
const
  ID = 'VTXEDIT';
begin
  fout := TFileStream.Create(fname, fmCreate or fmOpenWrite or fmShareDenyNone);
  MemCopy(@ID[0], @head.ID, 7);
  head.ID[7] := 0;
  head.Version := $0001;
  head.PageType := PageType;
  for i := 0 to 15 do
    head.Fonts[i] := word(ord(Fonts[i]));
  head.Colors := ColorScheme;
  head.Height := NumRows;
  head.Width := NumCols;
  MemZero(@head.Name[0], 64);
  for i := 0 to length(fname) - 1 do
  begin
    if i >= 63 then break;
    head.Name[i] := fname.Chars[i];
  end;

  head.XScale := XScale;
  head.PageAttr := Page.PageAttr;
  head.CrsrAttr := Page.CrsrAttr;
  MemCopy(@Page.Sauce, @head.Sauce, sizeof(TSauceHeader));
  head.NumObjects := length(Objects);
  fout.Write(head, sizeof(TVTXFileHeader));

  for r := 0 to numrows-1 do
  begin
    fout.WriteDWord(Page.Rows[r].Attr);
    for c := 0 to numcols-1 do
    begin
      fout.Write(Page.Rows[r].Cells[c].Chr, sizeof(WORD));
      fout.Write(Page.Rows[r].Cells[c].Attr, sizeof(DWORD));
    end;
  end;

  // save objects
  for i := 0 to length(Objects) - 1 do
  begin
    // save row, col, width, height, Page, Locked, Hidden, name, and data only
    fout.WriteWord(Objects[i].Row);
    fout.WriteWord(Objects[i].Col);
    fout.WriteWord(Objects[i].Width);
    fout.WriteWord(Objects[i].Height);
    fout.WriteByte(iif(Objects[i].Locked, 1, 0));
    fout.WriteByte(iif(Objects[i].Hidden, 1, 0));

    MemZero(@nameout[0], 64);
    for j := 0 to length(Objects[i].Name) - 1 do
    begin
      if j >= 63 then break;
      nameout[j] := char(Objects[i].Name.charAt(j));
    end;
    fout.Write(nameout[0], 64);

    for j := 0 to Objects[i].Width * Objects[i].Height - 1 do
    begin
      Objects[i].Data.Get(@cellrec, j);
      fout.Write(cellrec.Chr, sizeof(WORD));
      fout.Write(cellrec.Attr, sizeof(DWORD));
    end;
  end;

  fout.free;
end;

{ load a VTX file }
procedure TfMain.miFileOpenClick(Sender: TObject);
begin
  CheckToSave;

  odAnsi.Filter:=     'VTX File (*.vtx)|*.vtx';
  odAnsi.Title:=      'Open VTX File';
  odAnsi.Filename :=  '';
  if odAnsi.Execute then
  begin
    OpenVTXFile(odAnsi.Filename);
    GenerateBmpPage;
    pbPage.Invalidate;
    CurrFileName := ExtractFileName(odAnsi.FileName);
    CurrFileChanged := false;
    UpdateTitles;
  end;
end;

procedure TfMain.SaveAsVTXFile;
begin
  sdAnsi.Filter:=     'VTX File (*.vtx)|*.vtx';
  sdAnsi.Title:=      'Save VTX File';
  sdAnsi.Filename :=  '';
  if sdAnsi.Execute then
  begin
    SaveVTXFile(sdAnsi.Filename);
    CurrFileName := ExtractFileName(sdAnsi.FileName);
    CurrFileChanged := false;
    UpdateTitles;
  end;
end;

// if current document needs saving, ask to save.
procedure TfMain.CheckToSave;
begin
  // if no filename assigned yet, ask
  if CurrFileChanged then
  begin
    if QuestionDlg(
      'Save',
      'Save current VTX file first?',
      mtConfirmation, [mrYes, mrNo], '') = mrYes then
    begin
      if CurrFileName = '' then
        SaveAsVTXFile
      else
      begin
        SaveVTXFile(CurrFileName);
        CurrFileName := ExtractFileName(sdAnsi.FileName);
        CurrFileChanged := false;
        UpdateTitles;
      end;
    end;
  end;
end;

procedure TfMain.miFileSaveAsClick(Sender: TObject);
begin
  SaveAsVTXFile;
end;

procedure TfMain.miFileSaveClick(Sender: TObject);
var
  ansi : unicodestring;
begin
  if CurrFileName = '' then
    SaveAsVTXFile
  else
    SaveVTXFile(CurrFileName);
  CurrFileChanged := false;
  UpdateTitles;
end;

procedure TfMain.miFileExportClick(Sender: TObject);
var
  xo :          TfExportOptions;
  linelen :     integer;
  uselinelen,
  staticobjects,
  usebom,
  usesauce :    boolean;
begin
  // need to ask if
  //  ANSI
  //    restrict to 79 column output
  //    append SAUCE

  xo := TfExportOptions.Create(self);
  xo.cbUseSauce.Enabled := not (Fonts[CurrFont] in [ encUTF8, encUTF16 ]);

  if xo.ShowModal = mrOK then
  begin
    usebom := xo.cbUseBOM.Checked;
    usesauce := xo.cbUseSauce.Checked;
    uselinelen := xo.cbUseLineLen.Checked;
    linelen := xo.seLineLen.Value;
    staticobjects := xo.cbStaticObjects.Checked;

    sdAnsi.Filter:=     'ANSI File (*.ans)|*.ans|Text File (*.txt,*.nfo,*.asc)|*.txt;*.nfo;*.asc';
    sdAnsi.Title:=      'Export File';
    sdAnsi.Filename :=  '';
    if sdAnsi.Execute then
    begin
      case sdAnsi.FilterIndex of
        1:
          ExportANSIFile(sdAnsi.Filename, iif(uselinelen, linelen, -1), usebom, usesauce, staticobjects);

        2:
          ExportTextFile(sdAnsi.Filename, usebom, usesauce);
      end;
    end;
  end;
  xo.Free;
end;

// find next character in object from r, c. r/c relative to UL of object (0/0).
function TfMain.GetNextObjectCell(Obj : TObj; r, c : integer; var cell : TCell) : TRowCol;
var
  c1, r1 : integer;
begin
  c1 := c;
  r1 := r;

  if c1 >= obj.Width then
  begin
    c1 := 0;
    r1 += 1;
  end;
  if r1 < obj.Height then
  begin
    Obj.Data.Get(@cell, r1 * obj.Width + c1);
    while (cell.Chr = _EMPTY) and (r1 < obj.Height) do
    begin
      c1 += 1;
      if c1 >= obj.Width then
      begin
        c1 := 0;
        r1 += 1;
        if r1 >= obj.Height then break;
      end;
      Obj.Data.Get(@cell, r1 * obj.Width + c1);
    end;
  end;
  result.row := r1;
  result.col := c1;
end;

// find next character on page from r, c. include objects if staticobjects
function TfMain.GetNextCell(r, c : integer; staticobjects : boolean; var cell : TCell) : TRowCol;
var
  c1, r1 : integer;
begin
  c1 := c;
  r1 := r;

  // look for viewable cell.
  if staticobjects then
    GetObjectCell(r1, c1, cell)
  else
    cell := Page.Rows[r1].Cells[c1];

  while (cell.Chr = _SPACE)
    and (GetBits(cell.Attr, A_CELL_BG_MASK, 8) = 0)
    and (r1 < NumRows) do
  begin
    c1 += 1;
    if (c1 >= NumCols) then
    begin
      c1 := 0;
      r1 += 1;
      if r1 >= NumRows then break;
    end;

    if staticobjects then
      GetObjectCell(r1, c1, cell)
    else
      cell := Page.Rows[r1].Cells[c1];

  end;
  result.row := r1;
  result.col := c1;
end;

procedure TfMain.WriteANSI(fs : TFileStream; str : unicodestring);
var
  buff :  TBytes;
  l :     longint;
begin
  // save ansi - mind codepage
  case Fonts[CurrFont] of
    encUTF8:    buff := str.toUTF8Bytes;
    encUTF16:   buff := str.toUTF16Bytes;
    else        buff := str.toCPBytes;
  end;

  l := length(buff);
  fs.WriteBuffer(buff[0], l);
  setlength(buff, 0);
end;

procedure TfMain.WriteANSIChunk(fs : TFileStream; maxlen : integer; var rowansi, ansi : unicodestring);
begin
  if (maxlen > 0) and (rowansi.length + ansi.length > (maxlen - 3)) then
  begin
    rowansi += CSI + 's' + CRLF;
    WriteANSI(fs, rowansi);
    rowansi := CSI + 'u' + ansi;
    ansi := '';
  end
  else
  begin
    rowansi += ansi;
    ansi := '';
  end;
end;

function Space(num : integer) : unicodestring;
var
  i : integer;
begin
  result := '';
  for i := 1 to num do
    result += widechar(' ');
end;

// need function that takes 2 cell attr (current and target) and returns
// required SGR given PageType
function TfMain.ComputeSGR(currattr, targetattr : DWORD) : unicodestring;
var
  sgr :     unicodestring;
  fg, bg :  integer;
begin

  fg := GetBits(targetattr, A_CELL_FG_MASK);
  bg := GetBits(targetattr, A_CELL_BG_MASK, 8);

  if ColorScheme <> COLORSCHEME_256 then
  begin
    fg := fg and $07;
    if fg > 7 then
    begin
      fg -= 8;
      SetBit(targetattr, A_CELL_BOLD, true);
    end;
    bg := bg and $07;
    if bg > 7 then
    begin
      bg -= 8;
      SetBit(targetattr, A_CELL_BLINKFAST, true);
    end;
  end;

  sgr := CSI;

  // better logic - if VTX mode, can use turn offs
  // build two sets of SGR's, 1 with reset + ons, 2 with offs
  //  compare lengths

  // need to reset? if a SGR flips to off, need to reset.
  if   (not HasBits(targetattr, A_CELL_BOLD) and           HasBits(currattr, A_CELL_BOLD))
    or (not HasBits(targetattr, A_CELL_FAINT) and          HasBits(currattr, A_CELL_FAINT))
    or (not HasBits(targetattr, A_CELL_ITALICS) and        HasBits(currattr, A_CELL_ITALICS))
    or (not HasBits(targetattr, A_CELL_UNDERLINE) and      HasBits(currattr, A_CELL_UNDERLINE))
    or (not HasBits(targetattr, A_CELL_BLINKFAST) and      HasBits(currattr, A_CELL_BLINKFAST))
    or (not HasBits(targetattr, A_CELL_BLINKSLOW) and      HasBits(currattr, A_CELL_BLINKSLOW))
    or (not HasBits(targetattr, A_CELL_REVERSE) and        HasBits(currattr, A_CELL_REVERSE))
    or (not HasBits(targetattr, A_CELL_STRIKETHROUGH) and  HasBits(currattr, A_CELL_STRIKETHROUGH))
    or (not HasBits(targetattr, A_CELL_DOUBLESTRIKE) and   HasBits(currattr, A_CELL_DOUBLESTRIKE))
    or (not HasBits(targetattr, A_CELL_SHADOW) and         HasBits(currattr, A_CELL_SHADOW))
    or ((GetBits(targetattr, A_CELL_DISPLAY_MASK) <> A_CELL_DISPLAY_CONCEAL) and (GetBits(currattr, A_CELL_DISPLAY_MASK) = A_CELL_DISPLAY_CONCEAL))
    or ((GetBits(targetattr, A_CELL_DISPLAY_MASK) <> A_CELL_DISPLAY_TOP) and (GetBits(currattr, A_CELL_DISPLAY_MASK) = A_CELL_DISPLAY_TOP))
    or ((GetBits(targetattr, A_CELL_DISPLAY_MASK) <> A_CELL_DISPLAY_BOTTOM) and (GetBits(currattr, A_CELL_DISPLAY_MASK) = A_CELL_DISPLAY_BOTTOM)) then
  begin
    sgr += '0;';    // reset. use shortest.
    currattr := $0007;
  end;

  // mind the color mode! - be sure colors are cleaned up
  if fg <> GetBits(currattr, A_CELL_FG_MASK) then
  begin
    if fg < 8 then
      sgr += IntToStr(30 + integer(fg)) + ';'
    else
      sgr += '38;5;' + inttostr(fg) + ';';
  end;

  if bg <> GetBits(currattr, A_CELL_BG_MASK, 8) then
  begin
    if bg < 8 then
      sgr += IntToStr(40 + integer(bg)) + ';'
    else
      sgr += '48;5;' + inttostr(bg) + ';';
  end;

  if HasBits(targetattr, A_CELL_BOLD) then            sgr += '1;';
  if HasBits(targetattr, A_CELL_FAINT) then           sgr += '2;';
  if HasBits(targetattr, A_CELL_ITALICS) then         sgr += '3;';
  if HasBits(targetattr, A_CELL_UNDERLINE) then       sgr += '4;';
  if HasBits(targetattr, A_CELL_BLINKSLOW) then       sgr += '5;';
  if HasBits(targetattr, A_CELL_BLINKFAST) then       sgr += '6;';
  if HasBits(targetattr, A_CELL_REVERSE) then         sgr += '7;';
  if HasBits(targetattr, A_CELL_STRIKETHROUGH) then   sgr += '9;';
  if HasBits(targetattr, A_CELL_DOUBLESTRIKE) then    sgr += '56;';
  if HasBits(targetattr, A_CELL_SHADOW) then          sgr += '57;';

  if GetBits(targetattr, A_CELL_DISPLAY_MASK) = A_CELL_DISPLAY_CONCEAL then  sgr += '8;';
  if GetBits(targetattr, A_CELL_DISPLAY_TOP) = A_CELL_DISPLAY_TOP then       sgr += '58;';
  if GetBits(targetattr, A_CELL_DISPLAY_BOTTOM) = A_CELL_DISPLAY_BOTTOM then sgr += '59;';

  sgr := leftstr(sgr, length(sgr) - 1) + 'm';

  // nothing changed. probably a font change
  if sgr = #27'm' then sgr := '';

  result := sgr;
end;

function GetSauceFontName(enc : TEncoding) : string;
begin
  case enc of
    encCP437:   result := 'IBM VGA 437';
    encWIN1256: result := 'IBM VGA 720'; // aka 720
    encCP737:   result := 'IBM VGA 737';
    encCP775:   result := 'IBM VGA 775';
    encCP819:   result := 'IBM VGA 819';
    encCP850:   result := 'IBM VGA 850';
    encCP852:   result := 'IBM VGA 852';
    encCP855:   result := 'IBM VGA 855';
    encCP857:   result := 'IBM VGA 857';
    encCP858:   result := 'IBM VGA 858';
    encCP860:   result := 'IBM VGA 860';
    encCP861:   result := 'IBM VGA 861';
    encCP862:   result := 'IBM VGA 862';
    encCP863:   result := 'IBM VGA 863';
    encCP864:   result := 'IBM VGA 864';
    encCP865:   result := 'IBM VGA 865';
    encCP866:   result := 'IBM VGA 866';
    encCP869:   result := 'IBM VGA 869';
    encCP872:   result := 'IBM VGA 872';
    encCP867:   result := 'IBM VGA KAM';
    encCPMIK:   result := 'IBM VGA MIK';
    encTopaz:               result := 'Amiga Topaz 1';
    encTopazPlus:           result := 'Amiga Topaz 1+';
    encP0T_NOoDLE:          result := 'Amiga P0T-NOoDLE';
    encMicroKnight:         result := 'Amiga MicroKnight';
    encMicroKnightPlus:     result := 'Amiga MicroKnight+';
    encmOsOul:              result := 'Amiga mOsOul';
    encCBM64U, encCBM128U:  result := 'C64 PETSCII unshifted';
    encCBM64L, encCBM128L:  result := 'C64 PETSCII shifted';
    encAtari:               result := 'Atari ATASCII';
    else                    result := '';
  end;
end;

// export contents of Page to fname using CurrCodePage encoding
// if maxlen = -1, no max length check
// append sauce if usesauce
// if staticobjects, use topmost object cell for page cell,
// else draw objects bottom to top after page
procedure TfMain.ExportANSIFile(fname : string; maxlen : integer;
  useBOM, usesauce, staticobjects: boolean);
var
  fout :        TFileStream;
  po :          PObj;
  ansi,
  sgr,
  rowansi:      unicodestring;
  cell :        TCell;
  i, j :        integer;
  o, r, c :     integer;
  ro, co,
  wo, ho :      integer;
  objr, objc :  integer;
  cattr :       DWORD;
  pt :          TRowCol;
  delta :       integer;
  fg, bg :      integer;
  tmp :         TObj;
  p :           longint;
  fontsused :   array [0..15] of byte;
  fnt, cfnt :   integer;
  sauceCommentHdr : TSauceCommentHeader;
  commentBuf : TSauceComment;
begin
  fout := TFileStream.Create(fname, fmCreate or fmOpenWrite or fmShareDenyNone);

  cfnt := 0; // start with font 0

  if useBOM then
    case Fonts[0] of
      encUTF16:
        begin   // little endian
          fout.writebyte($FF);
          fout.writebyte($FE);
        end;

      encUTF8:
        begin
          fout.writebyte($EF);
          fout.writebyte($BB);
          fout.writebyte($BF);
        end;
    end;

  // initial reset
  cell := BLANK;
  cattr := BLANK.Attr;
  rowansi := CSI + '0m' + CSI + '0;0H' + CSI + '2J';

  // gather up the fonts needed for export.
  MemZero(@fontsused[0], 15);
  if PageType in [ PAGETYPE_CTERM, PAGETYPE_VTX ] then
  begin
    for r := 0 to NumRows - 1 do
      for c := 0 to NumCols - 1 do
        fontsused[(Page.Rows[r].Cells[c].Attr and A_CELL_FONT_MASK) >> 28] := 1;
    for o := 0 to length(Objects) - 1 do
      for r := 0 to Objects[o].Height - 1 do
        for c := 0 to Objects[o].Width - 1 do
        begin
          Objects[o].Data.Get(@cell, r * Objects[i].Width + c);
          fontsused[(GetBits(cell.Attr, A_CELL_FONT_MASK, 28) and $0F)] := 1;
        end;

    // don't send font 0. font 10+ don't need to be allocated in VTX mode
    for i := 1 to 9 do
      if fontsused[i] <> 0 then
        // find the font select for this font
        for j := 0 to length(FontSelectFonts) - 1 do
          if FontSelectFonts[j].CodePage = Fonts[i] then
          begin
            ansi := CSI + inttostr(i) + ';' + inttostr(FontSelectFonts[j].SGR) + ' D';
            WriteANSIChunk(fout, maxlen, rowansi, ansi);
            break;
          end;
  end;

  r := 0;
  c := 0;
  // do page first
  while r < NumRows do
  begin
    // find next cell
    pt := GetNextCell(r, c, staticobjects, cell);

    if pt.row >= NumRows then
    begin
      nop;
      break;
    end;

    if pt.row = r then
    begin
      // on the same row. advance
      delta := pt.col - c;
      if delta > 0 then
      begin
        if delta < 4 then
          ansi := Space(delta)
        else
          ansi := CSI + inttostr(delta) + 'C';
        c := pt.col;
        WriteANSIChunk(fout, maxlen, rowansi, ansi);
      end
      else
      begin
        // at character. adjust attributes
        // adjust bold/FG - blink/BG
        fg := GetBits(cell.attr, A_CELL_FG_MASK);
        bg := GetBits(cell.attr, A_CELL_BG_MASK, 8);
        if ColorScheme <> COLORSCHEME_256 then
        begin
          if fg > 7 then
          begin
            fg -= 8;
            SetBit(cell.attr, A_CELL_BOLD, true);
          end;
          if bg > 7 then
          begin
            bg -= 8;
            SetBit(cell.attr, A_CELL_BLINKFAST, true);
          end;
        end;

        if cattr <> cell.attr then
        begin
          ansi := ComputeSGR(cattr, cell.attr);
          if ansi <> '' then
            WriteANSIChunk(fout, maxlen, rowansi, ansi);

          // font change?
          fnt := (cell.attr and A_CELL_FONT_MASK) >> 28;
          if (fnt <> cfnt) and (PageType in [ PAGETYPE_CTERM, PAGETYPE_VTX ]) then
          begin
            if fnt < 10 then
            begin
              ansi := CSI + inttostr(10 + fnt) + 'm';
              WriteANSIChunk(fout, maxlen, rowansi, ansi);
            end
            else if (fnt >= 10) and (PageType = PAGETYPE_VTX) then
            begin
              ansi := CSI + inttostr(70 + fnt) + 'm';
              WriteANSIChunk(fout, maxlen, rowansi, ansi);
            end;
            cfnt := fnt;
          end;

          cattr := cell.attr;
        end;

        if c = NumCols - 1 then
          // character on last column
          ansi := #13#10#27'[A'#27'['+inttostr(NumCols-1)+'C'#27'[s' + WideChar(cell.chr) + #27'[u'#13#10
        else
          ansi := WideChar(cell.chr);
        WriteANSIChunk(fout, maxlen, rowansi, ansi);

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
      // on a different row
      // another row.
      rowansi += CRLF;
      WriteANSI(fout, rowansi);
      rowansi := '';

      r += 1;
      c := 0;
    end;
  end;

  // do dynamic objects last
  if not staticobjects then
  begin
    for i := 0 to length(Objects) - 1 do
    begin

      po := @Objects[i];

      // build temp object cropped to page dimensions
      // get destination height
      ro := po^.Row;
      co := po^.Col;
      wo := po^.Width;
      ho := po^.Height;
      if ro < 0 then
      begin
        ro := 0;
        ho += po^.Row;
      end;
      if co < 0 then
      begin
        co := 0;
        wo += po^.Col;
      end;
      if ro + ho >= NumRows then
        ho -= (ro + ho) - NumRows;
      if co + wo >= NumCols then
        wo -= (co + wo) - NumCols;

      tmp := po^;
      tmp.Row := ro;
      tmp.Col := co;
      tmp.Width := wo;
      tmp.Height := ho;
      tmp.Data.Data := nil;
      tmp.Data.Create(sizeof(TCell), rleAdds);

      p := 0;
      for r := 0 to po^.Height - 1 do
        for c := 0 to po^.Width - 1 do
        begin
          po^.Data.Get(@cell, p);
          p += 1;
          if between(po^.Row + r, 0, NumRows - 1)
            and between(po^.Col + c, 0, NumCols - 1) then
            tmp.Data.Add(@cell);
        end;

      po := @tmp;
      objr := po^.Row;
      objc := po^.Col;

      ansi := CSI + '0;0m';
      WriteANSIChunk(fout, maxlen, rowansi, ansi);
      cattr := $0007;

      ansi := CSI
        + inttostr(objr + 1) + ';'
        + inttostr(objc + 1) +'H';
      WriteANSIChunk(fout, maxlen, rowansi, ansi);

      r := 0;
      c := 0;
      while r < po^.Height do
      begin
        // find next cell in this object
        pt := GetNextObjectCell(po^, r, c, cell);

        if pt.row >= po^.Height then
          break;

        // on the page
        if pt.row = r then
        begin
          // on the same row. advance
          delta := pt.col - c;
          if delta > 0 then
          begin
            c := pt.col;
            ansi := CSI + inttostr(delta) + 'C';
            WriteANSIChunk(fout, maxlen, rowansi, ansi);
          end
          else
          begin
            // at character. adjust attributes
            // adjust bold/FG - blink/BG
            fg := GetBits(cell.attr, A_CELL_FG_MASK);
            bg := GetBits(cell.attr, A_CELL_BG_MASK, 8);
            if ColorScheme <> COLORSCHEME_256 then
            begin
              if fg > 7 then
              begin
                fg -= 8;
                SetBit(cell.attr, A_CELL_BOLD, true);
              end;
              if bg > 7 then
              begin
                bg -= 8;
                SetBit(cell.attr, A_CELL_BLINKFAST, true);
              end;
            end;

            if cattr <> cell.attr then
            begin
              ansi := ComputeSGR(cattr, cell.attr);
              if ansi <> '' then
                WriteANSIChunk(fout, maxlen, rowansi, ansi);

              // font change?
              fnt := (cell.attr and A_CELL_FONT_MASK) >> 28;
              if (fnt <> cfnt) and (PageType in [ PAGETYPE_CTERM, PAGETYPE_VTX ]) then
              begin
                if fnt < 10 then
                begin
                  ansi := CSI + inttostr(10 + fnt) + 'm';
                  WriteANSIChunk(fout, maxlen, rowansi, ansi);
                end
                else if (fnt >= 10) and (PageType = PAGETYPE_VTX) then
                begin
                  ansi := CSI + inttostr(70 + fnt) + 'm';
                  WriteANSIChunk(fout, maxlen, rowansi, ansi);
                end;
                cfnt := fnt;
              end;

              cattr := cell.attr;
            end;

            if (objc + c) = (NumCols - 1) then
              // character on last column
              ansi := #13#10#27'[A'#27'['+inttostr(NumCols-1)+'C'#27'[s' + WideChar(cell.chr) + #27'[u'#13#10
            else
              ansi := WideChar(cell.chr);
            WriteANSIChunk(fout, maxlen, rowansi, ansi);

            c += 1;
          end;
        end
        else
        begin
          // on a different row
          r := pt.Row;
          c := pt.Col;
          ansi := CSI
            + inttostr(objr + pt.row + 1) + ';'
            + inttostr(objc + pt.col + 1) +'H';
          WriteANSIChunk(fout, maxlen, rowansi, ansi);
        end;
      end;
      po^.Data.Free
    end;
  end;

  // write any remaining row ansi
  WriteANSI(fout, rowansi);

  // append sauce
  if usesauce then
  begin
    StrToChars(unicodestring('SAUCE'), @Page.Sauce.ID[0], 5);
    StrToChars(unicodestring('00'), @Page.Sauce.Version[0], 2);
    Page.Sauce.DataFileType := SAUCE_CHR_ANSI;
    if ColorScheme = COLORSCHEME_ICE then
      Page.Sauce.TFlags := SAUCE_FLAG_ICE;
    Page.Sauce.TInfo1 := NumCols;
    Page.Sauce.TInfo2 := NumRows;
    StrToChars(unicodestring(GetSauceFontName(Fonts[0])), @Page.Sauce.TInfoS, sizeof(Page.Sauce.TInfoS));
    fout.WriteByte(_CPMEOF);

    //
    // Write out any SAUCE comments
    // Up to 255 lines of 64 space-padded blocks each
    //
    if(Page.Sauce.Comments > 0) then
    begin
      StrToSauce('COMNT', @sauceCommentHdr, sizeof(sauceCommentHdr));
      fout.WriteBuffer(sauceCommentHdr, sizeof(sauceCommentHdr));
      for i := 0 to Page.Sauce.Comments - 1 do
      begin
        StrToSauce(Page.SauceComments[i], @commentBuf, sizeof(commentBuf));
        fout.WriteBuffer(commentBuf, sizeof(commentBuf));
      end;
    end;

    fout.write(Page.Sauce, sizeof(TSauceHeader));
  end;

  fout.Free;
end;

procedure TfMain.ExportTextFile(fname : string; useBOM, usesauce : boolean);
var
  fout :        TFileStream;
  ansi :        unicodestring;
  cell :        TCell;
  r, c :        integer;
  pt :          TRowCol;
  delta :       integer;
  fg, bg :      integer;

begin
  fout := TFileStream.Create(fname, fmCreate or fmOpenWrite or fmShareDenyNone);

  if useBOM then
    case Fonts[0] of
      encUTF16:
        begin   // little endian
          fout.writebyte($FF);
          fout.writebyte($FE);
        end;

      encUTF8:
        begin
          fout.writebyte($EF);
          fout.writebyte($BB);
          fout.writebyte($BF);
        end;
    end;

  // initial reset
  r := 0;
  c := 0;
  // do page first
  while r < NumRows do
  begin
    // find next cell
    pt := GetNextCell(r, c, true, cell);

    if pt.row >= NumRows then
    begin
      break;
    end;

    if pt.row = r then
    begin
      // on the same row. advance
      delta := pt.col - c;
      if delta > 0 then
      begin
        ansi := Space(delta);
        c := pt.col;
        WriteANSI(fout, ansi);
      end
      else
      begin
        // at character. adjust attributes
        // adjust bold/FG - blink/BG
        fg := GetBits(cell.attr, A_CELL_FG_MASK);
        bg := GetBits(cell.attr, A_CELL_BG_MASK, 8);
        if ColorScheme <> COLORSCHEME_256 then
        begin
          if fg > 7 then
          begin
            fg -= 8;
            SetBit(cell.attr, A_CELL_BOLD, true);
          end;
          if bg > 7 then
          begin
            bg -= 8;
            SetBit(cell.attr, A_CELL_BLINKFAST, true);
          end;
        end;

        // check block characters for reverse, etc
//        if cattr <> cell.attr then
//        begin
//          sgr := ComputeSGR(cattr, cell.attr);
//          ansi := sgr;
//          WriteANSI(fout, ansi);
//          cattr := cell.attr;
//        end;

        ansi := WideChar(cell.chr);
        WriteANSI(fout, ansi);

        c += 1;
        if c >= NumCols then
        begin
          c := 0;
          r += 1;
          WriteANSI(fout, #13#10);
        end;
      end;
    end
    else
    begin
      // on a different row
      // another row.
      WriteANSI(fout, #13#10);
      c := 0;
      r += 1;
    end;
  end;

  // append sauce
  if usesauce then
  begin
    StrToChars(unicodestring('SAUCE'), @Page.Sauce.ID[0], 5);
    StrToChars(unicodestring('00'), @Page.Sauce.Version[0], 2);
    Page.Sauce.DataFileType := SAUCE_CHR_ASCII;
    if ColorScheme = COLORSCHEME_ICE then
      Page.Sauce.TFlags := SAUCE_FLAG_ICE;
    Page.Sauce.TInfo1 := NumCols;
    Page.Sauce.TInfo2 := NumRows;
    StrToChars(unicodestring(GetSauceFontName(Fonts[0])), @Page.Sauce.TInfoS, sizeof(Page.Sauce.TInfoS));
    fout.WriteByte(_CPMEOF);
    fout.write(Page.Sauce, sizeof(TSauceHeader));
  end;

  fout.Free;
end;

// may need Import Options for
procedure TfMain.miFileImportClick(Sender: TObject);
begin
  CheckToSave;

  odAnsi.Filter:=     'ANSI File (*.ans)|*.ans|Text File (*.txt,*.nfo,*.asc)|*.txt;*.nfo;*.asc';
  odAnsi.Title:=      'Import File';
  odAnsi.Filename :=  '';
  if odAnsi.Execute then
  begin
    ImportANSIFile(odAnsi.Filename, false);
    GenerateBmpPage;
    pbPage.Invalidate;
    CurrFileName := ExtractFileNameWithoutExt(odAnsi.FileName) + '.vtx';
    CurrFileChanged := true;
    UpdateTitles;
  end;
end;

procedure TfMain.ImportANSIFile(fname : string; force8bit : boolean);
var
  fin :       TFileStream;
  buff :      TBytes;
  len :       longint;
  FileEnc :   TEncoding;
  checkenc :  TDetectEnc;
  bomskip :   integer;
  ansi :      unicodestring;
  ValidSauce : boolean;
  Sauce :     TSauceHeader;
  chars :     TWords;
  chr :       WORD;
  charslen :  longint;
  i, j, k,
  state :     integer;
  docsi,
  dochar,
  doeof :     boolean;
  SaveAttr :  DWord;
  fg, bg :    integer;
  bold,
  blink :     boolean;
  fontname,
  parms,
  inter:      string;
  pvals :     TStringArray;
  SaveRow,
  SaveCol :   integer;
  cp :        TEncoding;
  sauceComment : array[0..63] of char;
  ptype,
  pcolors :   integer;
  p1, p2 :      integer;

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

  // see if last three characters match an encoding spec
  function GetSauceEncoding(fname : string) : TEncoding;
  var
    l3 : string;
  begin
    l3 := RightStr(fname, 3);
    case l3 of
      '437':  result := encCP437;
      '720':  result := encWIN1256; // aka 720
      '737':  result := encCP737;
      '775':  result := encCP775;
      '819':  result := encCP819;
      '850':  result := encCP850;
      '852':  result := encCP852;
      '855':  result := encCP855;
      '857':  result := encCP857;
      '858':  result := encCP858;
      '860':  result := encCP860;
      '861':  result := encCP861;
      '862':  result := encCP862;
      '863':  result := encCP863;
      '864':  result := encCP864;
      '865':  result := encCP865;
      '866':  result := encCP866;
      '869':  result := encCP869;
      '872':  result := encCP872;
      'KAM':  result := encCP867;
      'MIK':  result := encCPMIK;
      else    result := encCP437; // default
    end;
  end;

begin
  NewFile;

  // read entire file in
  fin := TFileStream.Create(fname, fmOpenRead or fmShareDenyNone);
  len := fin.Size;
  setlength(buff, len);
  fin.Read(buff[0], len);
  fin.free;

  ptype := PAGETYPE_BBS;
  pcolors := COLORSCHEME_BBS;

  // convert to string
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
        // don't support big endian yet
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
    if Fonts[0] <> encUTF8 then
    begin
      Fonts[0] := encUTF8;
      cbCodePage.ItemIndex := ord(Fonts[0]);
      CodePageChange;
      BuildCharacterPalette;
    end;
    ansi := ansi.fromUTF8Bytes(buff);
  end
  else if FileEnc = encUTF16 then
  begin
    // UTF16
    if Fonts[0] <> encUTF16 then
    begin
      Fonts[0] := encUTF16;
      cbCodePage.ItemIndex := ord(Fonts[0]);
      cbCodePageChange(cbCodePage);
      BuildCharacterPalette;
    end;
    ansi := ansi.fromUTF16Bytes(buff);
  end
  else
  begin
    // only use sauce in 8bit CodePaged files.
    ValidSauce := false;
    if length(buff) > 128 then
    begin
      MemCopy(@buff[length(buff)-128], @Sauce, 128);
      ValidSauce := MemComp(@Sauce.ID, @SauceID, 5);
      if ValidSauce then
      begin
        Page.Sauce := Sauce;
        if Sauce.TInfo1 > 0 then
          seCols.Value := Sauce.TInfo1;
        if Sauce.TInfo2 > 0 then
          seRows.Value := Sauce.TInfo2;
        if HasBits(Sauce.TFlags, SAUCE_FLAG_ICE) then
        begin
          ptype := max(ptype, PAGETYPE_CTERM);
          pcolors := max(pcolors, COLORSCHEME_ICE);
        end;
        tbSauceTitle.Text := Trim(CharsToStr(Sauce.Title, sizeof(Sauce.Title)));
        tbSauceAuthor.Text := Trim(CharsToStr(Sauce.Author, sizeof(Sauce.Author)));
        tbSauceGroup.Text := Trim(CharsToStr(Sauce.Group, sizeof(Sauce.Group)));
        dtpSauceDate.Date := ScanDateTime('YYYYMMDD', CharsToStr(Sauce.Date, sizeof(Sauce.Date)));

        //
        // Read in comments, if any
        //
        if(Sauce.Comments > 0) then
        begin
          j := length(buff) - 128 - (Sauce.Comments * 64) -5;
          if 'COMNT' = CharsToStr(Copy(buff, j, 5), 5) then
          begin
            j += 5;
            for i := 1 to Sauce.Comments do
            begin
              memSauceComments.Lines.Add(Trim(CharsToStr(Copy(buff, j, 64), 64)));
              j += 64;
            end;
          end;
        end;

        ResizePage;

        //    TInfoS = Font name (from SauceFonts pattern)
        fontname := CharsToStr(Sauce.TInfoS, sizeof(Sauce.TInfoS));

        if LeftStr(fontname, 9) = 'IBM VGA50' then
        begin
          cp := GetSauceEncoding(fontname);
          seXScale.Value := 2.0;
        end
        else if LeftStr(fontname, 7) = 'IBM VGA' then
        begin
          cp := GetSauceEncoding(fontname);
          seXScale.Value := 1.0;
        end
        else if LeftStr(fontname, 9) = 'IBM EGA43' then
        begin
          cp := GetSauceEncoding(fontname);
          seXScale.Value := 2.0;
        end
        else if LeftStr(fontname,  7) = 'IBM EGA' then
        begin
          cp := GetSauceEncoding(fontname);
          seXScale.Value := 1.0;
        end
        else
        begin
          case fontname of
            'Amiga Topaz 1',
            'Amiga Topaz 2':
              begin
                cp := encTopaz;
                seXScale.Value := 2.0;
              end;

            'Amiga Topaz 1+',
            'Amiga Topaz 2+':
              begin
                cp := encTopazPlus;
                seXScale.Value := 2.0;
              end;

            'Amiga P0T-NOoDLE':
              begin
                cp := encP0T_NOoDLE;
                seXScale.Value := 2.0;
              end;

            'Amiga MicroKnight':
              begin
                cp := encMicroKnight;
                seXScale.Value := 2.0;
              end;

            'Amiga MicroKnight+':
              begin
                cp := encMicroKnightPlus;
                seXScale.Value := 2.0;
              end;

            'C64 PETSCII unshifted':
              begin
                cp := encCBM64U;
                seXScale.Value := 2.0;
              end;

            'C64 PETSCII shifted':
              begin
                cp := encCBM64L;
                seXScale.Value := 2.0;
              end;

            'Atari ATASCII':
              begin
                cp := encAtari;
                seXScale.Value := 2.0;
              end;

            else
              begin
                cp := encCP437;
                seXScale.Value := 1.0;
              end;
          end;
        end;

        cbCodePage.ItemIndex := ord(cp);
        Fonts[0] := cp;
      end;
    end;

    if not ValidSauce then
    begin
      cp := encCP437;
      seXScale.Value := 1.0;
      cbCodePage.ItemIndex := ord(cp);
      Fonts[0] := cp;
      CurrFont := 0;
    end;
    CodePageChange;
    BuildCharacterPalette;

    ansi := ansi.fromCPBytes(buff);
  end;
  setlength(buff, 0);

  // remove bom
  // convert to word array
  ansi := ansi.substring(bomskip);
  chars := ansi.toWordArray;
  charslen := length(chars);
  ansi := '';

  // populate page.
  SkipScroll := true;
  CursorRow := 0;
  CursorCol := 0;
  CurrAttr := BLANK.Attr;
  CurrChar := BLANK.Chr;
  state := 0;

  for i := 0 to charslen - 1 do
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

        'D':  // CUB - back n | Font Select defintions
          begin
            if inter = ' ' then
            begin
              p1 := GetIntCSIVal(0, 1);
              p2 := GetIntCSIVal(1, 0);
              Fonts[p1] := FontSelectFonts[p2].CodePage;
            end
            else
            begin
              CursorCol -= GetIntCSIVal(0, 1);
              if CursorCol < 0 then
                CursorCol := 0;
            end
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

        'I':  ; //Cursor Horizontal Tabulation (CHT). Move cursor forward n tab stops. Stops at last tab stop.

        'J':  // ED - erase screen 1=sos,0=eos,2=all
          begin
            case GetIntCSIVal(0, 0) of
              0:  // end of screen
                begin
                  for k := CursorCol to NumCols - 1 do
                    Page.Rows[CursorRow].Cells[k] := BLANK;
                  for j := CursorRow + 1 to NumRows - 1 do
                    for k := 0 to NumCols - 1 do
                      Page.Rows[j].Cells[k] := BLANK;
                end;

              1:  // start of screen
                begin
                  for k := 0 to CursorCol do
                    Page.Rows[CursorRow].Cells[k] := BLANK;
                  for j := 0 to CursorRow - 1 do
                    for k := 0 to NumCols - 1 do
                      Page.Rows[j].Cells[k] := BLANK;
                end;

              2:  // all
                begin
                  CursorRow := 0;
                  CursorCol := 0;
                  for j := 0 to NumRows - 1 do
                    for k := 0 to NumCols - 1 do
                      Page.Rows[j].Cells[k] := BLANK;
                end;
            end;
          end;

        'K':  // EL - erase line 1=sol,0=eol,2=all
          begin
            case GetIntCSIVal(0, 0) of
              0:  // end of line
                begin
                  for k := CursorCol to NumCols - 1 do
                    Page.Rows[CursorRow].Cells[k] := BLANK;
                end;

              1:  // start of line
                begin
                  for k := 0 to CursorCol do
                    Page.Rows[CursorRow].Cells[k] := BLANK;
                end;

              2:  // all
                begin
                  for k := 0 to NumCols - 1 do
                    Page.Rows[CursorRow].Cells[k] := BLANK;
                end;
            end;
          end;

        'L':  ; // Insert Lines (IL). Insert n rows at current row. {1}
        'M':  ; // Delete Lines (DL). Delete n rows at current row. {1}

        'N':  ;
        'O':  ;

        'P':  ; // Delete Character (DCH). Delete n characters at cursor position. {1}

        'Q':  ;
        'R':  ;

        'S':  ; // SU - scroll up n
        'T':  ; // ST - scroll down n

        'U':  ;
        'V':  ;
        'W':  ;
        'X':  ; // Erase Character (ECH). Erase next n characters {1}
        'Y':  ;
        'Z':  ; // Cursor Back Tab (CBT). Move cursor backwards n tab stops. Stops at left of page {1}
        '[':  ;
        '\':  ;
        ']':  ;
        '^':  ;
        '_':  ;
        '`':  ;
        'a':  ;
        'b':  ; // Repeat last printed character n times. {1}
        'c':  ; // Device Attributes.
        'd':  ;
        'e':  ;
        'g':  ;
        'h':  ; // Set Mode (SM).
        'i':  ;
        'j':  ;
        'k':  ;
        'l':  ; // Reset Mode (RM).

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
                  begin
                    ptype := max(ptype, PAGETYPE_VTX);
                    SetBit(CurrAttr, A_CELL_FAINT, true);
                  end;

                3:  // italics
                  begin
                    SetBit(CurrAttr, A_CELL_ITALICS, true);
                  end;

                4:  // underline
                  begin
                    ptype := max(ptype, PAGETYPE_VTX);
                    SetBit(CurrAttr, A_CELL_UNDERLINE, true);
                  end;

                5:  // blink slow
                  SetBit(CurrAttr, A_CELL_BLINKSLOW, true);

                6:  // blink fast
                  SetBit(CurrAttr, A_CELL_BLINKFAST, true);

                7:  // reverse
                  SetBit(CurrAttr, A_CELL_REVERSE, true);

                8:  // conceal
                  SetBits(CurrAttr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_CONCEAL);

                9:  // strikethrough
                  begin
                    ptype := max(ptype, PAGETYPE_VTX);
                    SetBit(CurrAttr, A_CELL_STRIKETHROUGH, true);
                  end;

                10..19: // font - skip for now
                  begin
                    ptype := max(ptype, PAGETYPE_CTERM);
                    SetBits(CurrAttr, A_CELL_FONT_MASK, j - 10, 28);
                  end;

                21: // bold off
                  begin
                    ptype := max(ptype, PAGETYPE_VTX);
                    SetBit(CurrAttr, A_CELL_BOLD, false);
                  end;

                22: // faint off
                  begin
                    cbPageType.ItemIndex := PAGETYPE_CTERM;
                    SetBit(CurrAttr, A_CELL_FAINT, false);
                  end;

                23: // italics off
                  begin
                    ptype := max(ptype, PAGETYPE_VTX);
                    SetBit(CurrAttr, A_CELL_ITALICS, false);
                  end;

                24: // underline off
                  begin
                    ptype := max(ptype, PAGETYPE_VTX);
                    SetBit(CurrAttr, A_CELL_UNDERLINE, false);
                  end;

                25, 26: // blink off
                  begin
                    cbPageType.ItemIndex := PAGETYPE_CTERM;
                    SetBits(CurrAttr, A_CELL_BLINKSLOW or A_CELL_BLINKFAST, 0);
                  end;

                27: // reverse off
                  begin
                    cbPageType.ItemIndex := PAGETYPE_CTERM;
                    SetBit(CurrAttr, A_CELL_REVERSE, false);
                  end;

                28: // conceal off
                  begin
                    ptype := max(ptype, PAGETYPE_VTX);
                    SetBits(CurrAttr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_NORMAL);
                  end;

                29: // strikethrough off
                  begin
                    ptype := max(ptype, PAGETYPE_VTX);
                    SetBit(CurrAttr, A_CELL_STRIKETHROUGH, false);
                  end;

                30..37: // fg color
                  SetBits(CurrAttr, A_CELL_FG_MASK, j - 30);

                38: // get 5 , fg
                  begin
                    k += 1;
                    if GetIntCSIVal(k, 0) = 5 then
                    begin
                      k += 1;
                      pcolors := max(color, COLORSCHEME_256);
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
                      pcolors := max(color, COLORSCHEME_256);
                      SetBits(CurrAttr, A_CELL_BG_MASK, GetIntCSIVal(k, 0), 8);
                    end;
                  end;

                49: // reset bg color
                  SetBits(CurrAttr, A_CELL_BG_MASK, 0, 8);

                56: // doublestrike
                  begin
                    ptype := max(ptype, PAGETYPE_VTX);
                    SetBit(CurrAttr, A_CELL_DOUBLESTRIKE, true);
                  end;

                57: // shadow
                  begin
                    ptype := max(ptype, PAGETYPE_VTX);
                    SetBit(CurrAttr, A_CELL_SHADOW, true);
                  end;

                58: // top half
                  begin
                    ptype := max(ptype, PAGETYPE_VTX);
                    SetBits(CurrAttr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_TOP);
                  end;

                59: // bottom half
                  begin
                    ptype := max(ptype, PAGETYPE_VTX);
                    SetBits(CurrAttr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_BOTTOM);
                  end;

                76: // double strike off
                  begin
                    ptype := max(ptype, PAGETYPE_VTX);
                    SetBit(CurrAttr, A_CELL_DOUBLESTRIKE, false);
                  end;

                77: // shadow off
                  begin
                    ptype := max(ptype, PAGETYPE_VTX);
                    SetBit(CurrAttr, A_CELL_SHADOW, false);
                  end;

                78, 79: // normal
                  begin
                    ptype := max(ptype, PAGETYPE_VTX);
                    SetBits(CurrAttr, A_CELL_DISPLAY_MASK, A_CELL_DISPLAY_NORMAL);
                  end;

                80..85: // VTX fonts. TODO
                  begin
                    ptype := max(ptype, PAGETYPE_VTX);
                    SetBits(CurrAttr, A_CELL_FONT_MASK, j - 70, 28);
                  end;

                90..97: // aixterm fg color
                  begin
                    ptype := max(ptype, PAGETYPE_VTX);
                    SetBits(CurrAttr, A_CELL_FG_MASK, j - 90 + 8);
                  end;

                100..107: // aixterm bg color
                  begin
                    ptype := max(ptype, PAGETYPE_VTX);
                    SetBits(CurrAttr, A_CELL_BG_MASK, j - 100 + 8);
                  end;
              end;
              k += 1;
            end;
          end;

        'n':  ; // Device Status Report. {0}
        'o':  ;
        'p':  ;
        'q':  ;
        'r':  ; // '*r' : Baud rate emulation.| 'r' : Set scroll window (DECSTBM).

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

  PageType := ptype;
  cbPageType.ItemIndex := ptype;
  ColorScheme := pcolors;
  cbColorScheme.ItemIndex := pcolors;

end;

procedure nop; begin end;

procedure DebugStart;
begin

  nop;

end;


initialization
{$I vtxcursors.lrs}

end.

