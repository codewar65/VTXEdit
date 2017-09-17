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

unit VTXConst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TKeyBinds = record
    Ctrl, Shift, Alt :  boolean;
    KeyCode :           int16;
    KeyStr :            string;  // the string rep of this key ( Ctrl+C eg.)
    Action :            int16;
    Val :               string;
  end;

  TCell = packed record
    Chr :               UInt16;     // unicode codepoint
    Attr :              UInt32;     // attributes
  end;

  PCell = ^TCell;

  TCellList = array of TCell;

  // item in ANSI art object linked list.
  TPlacedCell = packed record
    Row, Col :          UInt16;
    Cell :              TCell;
  end;

  TPlacedCellList = array of TPlacedCell;

  // ANSI art objects / pasted object from clipboard.
  TObj = record
    Name :              unicodestring;
    Row, Col :          integer;          // position on page
    Width, Height :     integer;          // size
    Page,
    Locked,
    Hidden :            boolean;
    Data :              TCellList;
  end;

  PObj = ^TObj;

  // objects on document
  TObjList = array of TObj;

  TUndoTypes = ( utCells, utObjAdd, utObjRemove, utObjMove, utObjMerge );

  // a single undo/redo item on undo 'stack'
  TUndoCells = packed record
    Row, Col :          UInt16;
    OldCell, NewCell :  TCell;
  end;

  // copy / paste region selection info
  TLoc = packed record
    Neighbors :         byte;       // 1=n,2=e,4=s,8=w
    Row, Col :          int16;
  end;

  // copy selection area
  TLocList = array of TLoc;

  // http://www.acid.org/info/sauce/sauce.htm
  TSauceHeader = packed record
    ID:             array [1..5] of char;   // "SAUCE"
    Version:        array [1..2] of char;
    Title:          array [1..35] of char;
    Author:         array [1..20] of char;
    Group:          array [1..20] of char;
    Date:           array [1..8] of char;
    FileSIze :      Uint32;

    DataFileType :  UInt16;
    // see SAUCE_ below
    // if = SAUCE_CHAR_ANSI / ANSIMATION / ASCII
    // then
    //    TInfo1 = cols (0=use default)
    //    TInfo2 = rows (0=use default)
    //    TFlags = SAUSE_FLAG_*
    //    TInfoS = Font name (from SauceFonts pattern)

    TInfo1 :        Uint16;
    TInfo2 :        Uint16;
    TInfo3 :        Uint16;
    TInfo4 :        Uint16;
    Comments:       Byte;
    TFlags:         Byte;
    TInfoS:         array [1..22] of char;  // null terminated - FontName
  end;

  TSauseCommentHeader = packed record
    ID:         array [1..5] of char;   // "COMNT"
  end;

  TSauseComment = packed record
    ID:         array [1..64] of char;
  end;

  TVTXObjHeader = record
    ID          : array [0..7] of Byte;     // VTXEDIT
    Version     : Word;                     //
    PageType    : Word;                     // 0=bbs, 1=cterm, 2=vtx, 3=obj
    Width       : Word;
    Height      : Word;
    Name        : packed array [0..63] of char;
  end;

  TVTXFileHeader = record
    ID          : array [0..7] of Byte;     // VTXEDIT
    Version     : Word;                     //
    PageType    : Word;                     // 0=bbs, 1=cterm, 2=vtx, 3=obj
    Width       : Word;
    Height      : Word;
    Name        : packed array [0..63] of char;

    // page / cursor attr - rest of Page rec
    Fonts       : array [0..15] of Word;    // all the font / codepages. internal vals
    Colors      : Word;                     // basic, bbs, ice, 256
    XScale      : double;
    PageAttr    : DWORD;
    CrsrAttr    : DWORD;
    Sauce       : TSauceHeader;
    NumObjects  : word;                     // number of object recors appended to end of file.
  end;

  TRow = packed record
    Cells :     Array of TCell;
    Attr :      UInt32;     // row attributes
  end;

  TPage = packed record
    Rows :      Array of TRow;
    PageAttr :  UInt32;     // page / border color
    CrsrAttr :  UInt32;     // cursor size / color
    Sauce :     TSauceHeader;
  end;

  TEncoding = ( encCP437, encCP667, encCP668, encCP737, encCP770, encCP771,
    encCP772, encCP773, encCP774, encCP775, encCP790, encCP808, encCP813,
    encCP819, encCP850, encCP851, encCP852, encCP853, encCP855, encCP857,
    encCP858, encCP859, encCP860, encCP861, encCP862, encCP863, encCP864,
    encCP865, encCP866, encCP867, encCP869, encCP872, encCP878, encCP895,
    encCP900, encCP912, encCP915, encCP920, encCP991, encCP1117, encCP1118,
    encCP1119, encCP1131, encCP28593, encCP28594, encCPMIK, encARMSCII_8,
    encISO8859_1, encISO8859_2, encISO8859_3, encISO8859_4, encISO8859_5,
    encISO8859_6, encISO8859_7, encISO8859_8, encISO8859_9, encISO8859_10,
    encISO8859_13, encISO8859_14, encISO8859_15, encISO8859_16, encKAIK8,
    encKOI8_R, encKOI8_U, encWIN1250, encWIN1251, encWIN1253, encWIN1254,
    encWIN1255, encWIN1256, encWIN1257,
    encUTF8, encUTF16 );

  TToolModes = ( tmSelect, tmDraw, tmFill, tmLine, tmRect, tmEllipse );
  TDrawModes = ( dmChars, dmLeftRights, dmTopBottoms, dmQuarters, dmSixels );

  TCodePageRec = packed record
    Name :            string;       // name of this codepage 'CP437' e.g.
    EncodingLUT :     PUint16;      // in this table. (256 entrues)
    MirrorTable :     PUint16;      // mirroring table
    MirrorTableSize : integer;
    GlyphTable :      PByte;        // base pointer to Glyph Array (Unicode + 8x16)
    GlyphTableSize :  integer;      // size of glyph table
    QuickGlyph :      array [0..255] of integer;  // pointer to glyph in glyph table
    CanDrawMode :     array [dmChars..dmSixels] of boolean;
  end;

  TColors2 = array [0..1] of byte; // for return blocks from chars
  TColors4 = array [0..3] of byte;
  TColors16 = array [0..15] of byte;

  TQuad = record
    v0, v1, v2, v3 : integer;
  end;

  TRowCol = record
    row, col : integer;
  end;

const

  clSelectedObject1 =   $FFFF00;
  clSelectedObject2 =   $888800;
  clUnselectedObject1 = $888800;
  clUnselectedObject2 = $444400;
  clSelectionArea1 =    $0088FF;
  clSelectionArea2 =    $004488;
  clDrawCursor1 =       $FFFFFF;  // drawing mode cursor colors
  clDrawCursor2 =       $888888;
  clPageBorder1 =       $00FF00;
  clPageBorder2 =       $008800;

  BLANK : TCell = ( Chr: $20; Attr: $0007; );

  SauceID : array [1..5] of char = 'SAUCE';

  SauceFonts : array [0..20] of unicodestring = (
    'IBM VGA',
    'IBM VGA50',
    'IBM VGA25G',
    'IBM EGA',
    'IBM EGA43',
    'IBM VGA ___',    // ___ = codepage
    'IBM VGA50 ___',  // ___ = codepage
    'IBM VGA25G ___', // ___ = codepage
    'IBM EGA ___',    // ___ = codepage
    'IBM EGA43 ___',  // ___ = codepage
    'Amiga Topaz 1',
    'Amiga Topaz 1+',
    'Amiga Topaz 2',
    'Amiga Topaz 2+',
    'Amiga P0T-NOoDLE',
    'Amiga MicroKnight',
    'Amiga MicroKnight+',
    'Amiga mOsOul',
    'C64 PETSCII unshifted',
    'C64 PETSCII shifted',
    'Atari ATASCII'
  );

  SAUCE_NIL_NIL =         $0000;
  SAUCE_CHR_ASCII =       $0100;
  SAUCE_CHR_ANSI =        $0101;
  SAUCE_CHR_ANSIMATION =  $0102;
  SAUCE_CHR_RIP =         $0103;
  SAUCE_CHR_PCBOARD =     $0104;
  SAUCE_CHR_AVATAR =      $0105;
  SAUCE_CHR_HTML =        $0106;
  SAUCE_CHR_SOURCE =      $0107;
  SAUCE_CHR_TUNDRADRAW =  $0108;
  SAUCE_BMP_GIF =         $0200;
  SAUCE_BMP_PCX =         $0201;
  SAUCE_BMP_LBMIFF  =     $0202;
  SAUCE_BMP_TGA =         $0203;
  SAUCE_BMP_FLI =         $0204;
  SAUCE_BMP_FLC =         $0205;
  SAUCE_BMP_BMP =         $0206;
  SAUCE_BMP_GL  =         $0207;
  SAUCE_BMP_DL  =         $0208;
  SAUCE_BMP_WPG =         $0209;
  SAUCE_BMP_PNG =         $020A;
  SAUCE_BMP_JPG =         $020B;
  SAUCE_BMP_MPG =         $020C;
  SAUCE_BMP_AVI =         $020D;
  SAUCE_VEC_DXF =         $0300;
  SAUCE_VEC_DWG =         $0301;
  SAUCE_VEC_WPG =         $0302;
  SAUCE_VEC_3DS =         $0303;
  SAUCE_AUD_MOD =         $0400;
  SAUCE_AUD_669 =         $0401;
  SAUCE_AUD_STM =         $0402;
  SAUCE_AUD_S2M =         $0403;
  SAUCE_AUD_MTM =         $0404;
  SAUCE_AUD_FAR =         $0405;
  SAUCE_AUD_ULT =         $0406;
  SAUCE_AUD_AMF =         $0407;
  SAUCE_AUD_DMF =         $0408;
  SAUCE_AUD_OKT =         $0409;
  SAUCE_AUD_ROL =         $040A;
  SAUCE_AUD_CMF =         $040B;
  SAUCE_AUD_MID =         $040C;
  SAUCE_AUD_SADT =        $040D;
  SAUCE_AUD_VOC =         $040E;
  SAUCE_AUD_WAV =         $040F;
  SAUCE_AUD_SMP8 =        $0410;
  SAUCE_AUD_SMP8S =       $0411;
  SAUCE_AUD_SMP16 =       $0412;
  SAUCE_AUD_SMP16S =      $0413;
  SAUCE_AUD_PATCH8 =      $0414;
  SAUCE_AUD_PATCH16 =     $0415;
  SAUCE_AUD_XM =          $0416;
  SAUCE_AUD_HSC =         $0417;
  SAUCE_AUD_IT =          $0418;
  SAUCE_BIN =             $0500;
  SAUCE_XBN =             $0600;
  SAUCE_ARC_ZIP =         $0700;
  SAUCE_ARC_ARJ =         $0701;
  SAUCE_ARC_LZH =         $0702;
  SAUCE_ARC_ARC =         $0703;
  SAUCE_ARC_TAR =         $0704;
  SAUCE_ARC_ZOO =         $0705;
  SAUCE_ARC_RAR =         $0706;
  SAUCE_ARC_UC2 =         $0707;
  SAUCE_ARC_PAK =         $0708;
  SAUCE_ARC_SQZ =         $0709;
  SAUCE_EXE =             $0800;

  // Sause TFlags
  SAUCE_FLAG_ICE =        %00000001;    // ice colors (blink = bright BG)
  SAUCE_FLAG_LS_MASK =    %00000110;
  SAUCE_FLAG_LS_NA =      %00000000;
  SAUCE_FLAG_LS_8 =       %00000010;    // 8 x M fontsize (ignored for now)
  SAUCE_FLAG_LS_9 =       %00000100;    // 9 x N fontsize (ignored for now)
  SAUCE_FLAG_LS_RES =     %00000110;
  SAUCE_FLAG_AR_MASK =    %00011000;
  SAUCE_FLAG_AR_NA =      %00000000;
  SAUCE_FLAG_AR_LEG =     %00001000;    // Legacy Aspect Ratio (ignored for now)
  SAUCE_FLAG_AR_MOD =     %00010000;    // Modern Aspect Ratio (ignored for now)
  SAUCE_FLAG_AR_RES =     %00011000;

  CSI = #27'[';
  CRLF = #13#10;

  // cell attributes
  A_CELL_FG_MASK =        %00000000000000000000000011111111;
  A_CELL_BG_MASK =        %00000000000000001111111100000000;
  A_CELL_REVERSE =        %00000000000000010000000000000000;
  A_CELL_BOLD =           %00000000000000100000000000000000;
  A_CELL_ITALICS =        %00000000000001000000000000000000;
  A_CELL_UNDERLINE =      %00000000000010000000000000000000;
  A_CELL_STRIKETHROUGH =  %00000000000100000000000000000000;
  A_CELL_BLINKSLOW =      %00000000001000000000000000000000;
  A_CELL_BLINKFAST =      %00000000010000000000000000000000;
  A_CELL_FAINT =          %00000000100000000000000000000000;
  A_CELL_DISPLAY_MASK =   %00000011000000000000000000000000;
  A_CELL_DISPLAY_NORMAL = %00000000000000000000000000000000;
  A_CELL_DISPLAY_CONCEAL =%00000001000000000000000000000000;
  A_CELL_DISPLAY_TOP =    %00000010000000000000000000000000;
  A_CELL_DISPLAY_BOTTOM = %00000011000000000000000000000000;
  A_CELL_SHADOW =         %00000100000000000000000000000000;
  A_CELL_DOUBLESTRIKE =   %00001000000000000000000000000000;
  A_CELL_FONT_MASK =      %11110000000000000000000000000000;

  A_ROW_COLOR1_MASK =     %00000000000000000000000011111111;
  A_ROW_COLOR2_MASK =     %00000000000000001111111100000000;
  A_ROW_PATTERN_MASK =    %00000000000000110000000000000000;
  A_ROW_PATTERN_NONE =    %00000000000000000000000000000000;
  A_ROW_PATTERN_SOLID =   %00000000000000010000000000000000;
  A_ROW_PATTERN_HORZ =    %00000000000000100000000000000000;
  A_ROW_PATTERN_VERT =    %00000000000000110000000000000000;
  A_ROW_HEIGHT_MASK =     %00000000000111000000000000000000;
  A_ROW_HEIGHT_25 =       %00000000000000000000000000000000;
  A_ROW_HEIGHT_50 =       %00000000000001000000000000000000;
  A_ROW_HEIGHT_75 =       %00000000000010000000000000000000;
  A_ROW_HEIGHT_100 =      %00000000000011000000000000000000;
  A_ROW_HEIGHT_125 =      %00000000000100000000000000000000;
  A_ROW_HEIGHT_150 =      %00000000000101000000000000000000;
  A_ROW_HEIGHT_175 =      %00000000000110000000000000000000;
  A_ROW_HEIGHT_200 =      %00000000000111000000000000000000;
  A_ROW_WIDTH_MASK =      %00000000011000000000000000000000;
  A_ROW_WIDTH_50 =        %00000000000000000000000000000000;
  A_ROW_WIDTH_100 =       %00000000001000000000000000000000;
  A_ROW_WIDTH_150 =       %00000000010000000000000000000000;
  A_ROW_WIDTH_200 =       %00000000011000000000000000000000;
  A_ROW_MARQUEE =         %00000000100000000000000000000000;
  A_ROW_DISPLAY_MASK =    %00000011000000000000000000000000;
  A_ROW_DISPLAY_NORMAL =  %00000000000000000000000000000000;
  A_ROW_DISPLAY_CONCEAL = %00000001000000000000000000000000;
  A_ROW_DISPLAY_TOP =     %00000010000000000000000000000000;
  A_ROW_DISPLAY_BOTTOM =  %00000011000000000000000000000000;

  A_PAGE_PAGE_MASK =      %0000000011111111;
  A_PAGE_BORDER_MASK =    %1111111100000000;

  A_CURSOR_COLOR_MASK =   %0000000011111111;
  A_CURSOR_SIZE_MASK =    %0000001100000000;
  A_CURSOR_SIZE_NONE =    %0000000000000000;
  A_CURSOR_SIZE_THIN =    %0000000100000000;
  A_CURSOR_SIZE_THICK =   %0000001000000000;
  A_CURSOR_SIZE_FULL =    %0000001100000000;
  A_CURSOR_VERTICAL =     %0000010000000000;

  // ASCII C0 Codes
  _NUL     = $00;
  _SOH     = $01;
  _STX     = $02;
  _ETX     = $03;
  _EOT     = $04;
  _ENQ     = $05;
  _ACK     = $06;
  _BEL     = $07;
  _BS      = $08;
  _HT      = $09;
  _LF      = $0A;
  _VT      = $0B;
  _FF      = $0C;
  _CR      = $0D;
  _SO      = $0E;
  _SI      = $0F;
  _DLE     = $10;
  _DC1     = $11;  //   same
  _XON     = $11;  // values
  _DC2     = $12;
  _DC3     = $13;  //   same
  _XOFF    = $13;  // values
  _DC4     = $14;
  _NAK     = $15;
  _SYN     = $16;
  _ETB     = $17;
  _CAN     = $18;
  _EM      = $19;
  _SUB     = $1A;  //   same
  _CPMEOF  = $1A;  // values
  _ESC     = $1B;
  _FS      = $1C;
  _GS      = $1D;
  _RS      = $1E;
  _US      = $1F;
  _SPACE   = $20;
  _C       = $43;
  _G       = $47;
  _DEL     = $7F;
  _SHY     = $2010;  // similar character to replace soft-hyphen

  EMPTYCHAR = $DFFF;   // invalid character used to mark holes in objects

  ANSIColor : packed array [0..255] of UInt32 = (
      // these are in BBGGRR order
      // VGA 0-15 - transparent will switch to #000000 when appropriate
      $000000, $0000AA, $00AA00, $0055AA, $AA0000, $AA00AA, $AAAA00, $AAAAAA,
      $555555, $5555FF, $55FF55, $55FFFF, $FF5555, $FF55FF, $FFFF55, $FFFFFF,
      $000000, $5F0000, $870000, $AF0000, $D70000, $FF0000, $005F00, $5F5F00,
      $875F00, $AF5F00, $D75F00, $FF5F00, $008700, $5F8700, $878700, $AF8700,
      $D78700, $FF8700, $00AF00, $5FAF00, $87AF00, $AFAF00, $D7AF00, $FFAF00,
      $00D700, $5FD700, $87D700, $AFD700, $D7D700, $FFD700, $00FF00, $5FFF00,
      $87FF00, $AFFF00, $D7FF00, $FFFF00, $00005F, $5F005F, $87005F, $AF005F,
      $D7005F, $FF005F, $005F5F, $5F5F5F, $875F5F, $AF5F5F, $D75F5F, $FF5F5F,
      $00875F, $5F875F, $87875F, $AF875F, $D7875F, $FF875F, $00AF5F, $5FAF5F,
      $87AF5F, $AFAF5F, $D7AF5F, $FFAF5F, $00D75F, $5FD75F, $87D75F, $AFD75F,
      $D7D75F, $FFD75F, $00FF5F, $5FFF5F, $87FF5F, $AFFF5F, $D7FF5F, $FFFF5F,
      $000087, $5F0087, $870087, $AF0087, $D70087, $FF0087, $005F87, $5F5F87,
      $875F87, $AF5F87, $D75F87, $FF5F87, $008787, $5F8787, $878787, $AF8787,
      $D78787, $FF8787, $00AF87, $5FAF87, $87AF87, $AFAF87, $D7AF87, $FFAF87,
      $00D787, $5FD787, $87D787, $AFD787, $D7D787, $FFD787, $00FF87, $5FFF87,
      $87FF87, $AFFF87, $D7FF87, $FFFF87, $0000AF, $5F00AF, $8700AF, $AF00AF,
      $D700AF, $FF00AF, $005FAF, $5F5FAF, $875FAF, $AF5FAF, $D75FAF, $FF5FAF,
      $0087AF, $5F87AF, $8787AF, $AF87AF, $D787AF, $FF87AF, $00AFAF, $5FAFAF,
      $87AFAF, $AFAFAF, $D7AFAF, $FFAFAF, $00D7AF, $5FD7AF, $87D7AF, $AFD7AF,
      $D7D7AF, $FFD7AF, $00FFAF, $5FFFAF, $87FFAF, $AFFFAF, $D7FFAF, $FFFFAF,
      $0000D7, $5F00D7, $8700D7, $AF00D7, $D700D7, $FF00D7, $005FD7, $5F5FD7,
      $875FD7, $AF5FD7, $D75FD7, $FF5FD7, $0087D7, $5F87D7, $8787D7, $AF87D7,
      $D787D7, $FF87D7, $00AFD7, $5FAFD7, $87AFD7, $AFAFD7, $D7AFD7, $FFAFD7,
      $00D7D7, $5FD7D7, $87D7D7, $AFD7D7, $D7D7D7, $FFD7D7, $00FFD7, $5FFFD7,
      $87FFD7, $AFFFD7, $D7FFD7, $FFFFD7, $0000FF, $5F00FF, $8700FF, $AF00FF,
      $D700FF, $FF00FF, $005FFF, $5F5FFF, $875FFF, $AF5FFF, $D75FFF, $FF5FFF,
      $0087FF, $5F87FF, $8787FF, $AF87FF, $D787FF, $FF87FF, $00AFFF, $5FAFFF,
      $87AFFF, $AFAFFF, $D7AFFF, $FFAFFF, $00D7FF, $5FD7FF, $87D7FF, $AFD7FF,
      $D7D7FF, $FFD7FF, $00FFFF, $5FFFFF, $87FFFF, $AFFFFF, $D7FFFF, $FFFFFF,
      $080808, $121212, $1C1C1C, $262626, $303030, $3A3A3A, $444444, $4E4E4E,
      $585858, $626262, $6C6C6C, $767676, $808080, $8A8A8A, $949494, $9E9E9E,
      $A8A8A8, $B2B2B2, $BCBCBC, $C6C6C6, $D0D0D0, $DADADA, $E4E4E4, $EEEEEE );

  CP437 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9,
      $00FF, $00D6, $00DC, $00A2, $00A3, $00A5, $20A7, $0192,
      $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA,
      $00BF, $2310, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
      $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
      $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
      $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
      $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4,
      $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
      $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248,
      $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, _SPACE );


  CP667 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $0105, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $0107, $00C4, $0104,
      $0118, $0119, $0142, $00F4, $00F6, $0106, $00FB, $00F9,
      $015A, $00D6, $00DC, $00A2, $0141, $00A5, $015B, $0192,
      $0179, $017B, $00F3, $00D3, $0144, $0143, $017A, $017C,
      $00BF, $2310, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
      $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
      $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
      $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
      $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4,
      $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
      $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248,
      $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0 );

  CP668 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $0107, $00E7,
      $0142, $00EB, $00E8, $00EF, $00EE, $0179, $00C4, $0106,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $015A,
      $015B, $00D6, $00DC, $00A2, $00A3, $0141, $00D3, $0192,
      $00E1, $00ED, $00F3, $00FA, $0104, $0105, $017B, $017C,
      $0118, $0119, $00AC, $017A, $0143, $0144, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
      $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
      $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
      $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
      $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4,
      $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
      $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248,
      $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0 );

  CP737 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $0391, $0392, $0393, $0394, $0395, $0396, $0397, $0398,
      $0399, $039A, $039B, $039C, $039D, $039E, $039F, $03A0,
      $03A1, $03A3, $03A4, $03A5, $03A6, $03A7, $03A8, $03A9,
      $03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7, $03B8,
      $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF, $03C0,
      $03C1, $03C3, $03C2, $03C4, $03C5, $03C6, $03C7, $03C8,
      $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
      $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
      $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
      $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
      $03C9, $03AC, $03AD, $03AE, $03CA, $03AF, $03CC, $03CD,
      $03CB, $03CE, $0386, $0388, $0389, $038A, $038C, $038E,
      $038F, $00B1, $2265, $2264, $03AA, $03AB, $00F7, $2248,
      $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0 );

  CP770 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $010C, $00FC, $0117, $0101, $00E4, $0105, $013C, $010D,
      $0113, $0112, $0119, $0118, $012B, $012F, $00C4, $0104,
      $0116, $017E, $017D, $00F5, $00F6, $00D5, $016B, $0173,
      $0123, $00D6, $00DC, $00A2, $013B, $201E, $0161, $0160,
      $0100, $012A, $0137, $0136, $0146, $0145, $016A, $0172,
      $0122, $2310, $00AC, $00BD, $00BC, $012E, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
      $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
      $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
      $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
      $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4,
      $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
      $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248,
      $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0 );

  CP771 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417,
      $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
      $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427,
      $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
      $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437,
      $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
      $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
      $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
      $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
      $256A, $2518, $250C, $2588, $0104, $0105, $010C, $010D,
      $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447,
      $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F,
      $0118, $0119, $0116, $0117, $012E, $012F, $0160, $0161,
      $0172, $0173, $016A, $016B, $017D, $017E, $25A0, $00A0 );

  CP772 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417,
      $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
      $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427,
      $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
      $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437,
      $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
      $2591, $2592, $2593, $2502, $2524, $0104, $010C, $0118,
      $0116, $2563, $2551, $2557, $255D, $012E, $0160, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $0172, $016A,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $017D,
      $0105, $010D, $0119, $0117, $012F, $0161, $0173, $016B,
      $017E, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
      $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447,
      $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F,
      $0401, $0451, $201E, $201C, $2320, $2321, $00F7, $2248,
      $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0 );

  CP773 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $0106, $00FC, $00E9, $0101, $00E4, $0123, $00E5, $0107,
      $0142, $0113, $0156, $0157, $012B, $0179, $00C4, $00C5,
      $00C9, $00E6, $00C6, $014D, $00F6, $0122, $00A2, $015A,
      $015B, $00D6, $00DC, $00F8, $00A3, $00D8, $00D7, $00A4,
      $0100, $012A, $00F3, $017B, $017C, $017A, $201D, $00A6,
      $00A9, $00AE, $00AC, $00BD, $00BC, $0141, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
      $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
      $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
      $256A, $2518, $250C, $2588, $0104, $0105, $010C, $010D,
      $00D3, $00DF, $014C, $0143, $00F5, $00D5, $00B5, $0144,
      $0136, $0137, $013B, $013C, $0146, $0112, $0145, $2019,
      $0118, $0119, $0116, $0117, $012E, $012F, $0160, $0161,
      $0172, $0173, $016A, $016B, $017D, $017E, $25A0, $00A0 );

  CP774 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9,
      $00FF, $00D6, $00DC, $00A2, $00A3, $00A5, $20A7, $0192,
      $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA,
      $00BF, $2310, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $0104, $010C, $0118,
      $0116, $2563, $2551, $2557, $255D, $012E, $0160, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $0172, $016A,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $017D,
      $0105, $010D, $0119, $0117, $012F, $0161, $0173, $016B,
      $017E, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
      $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4,
      $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
      $2261, $00B1, $2265, $2264, $201E, $201C, $00F7, $2248,
      $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0 );

  CP775 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $0106, $00FC, $00E9, $0101, $00E4, $0123, $00E5, $0107,
      $0142, $0113, $0156, $0157, $012B, $0179, $00C4, $00C5,
      $00C9, $00E6, $00C6, $014D, $00F6, $0122, $00A2, $015A,
      $015B, $00D6, $00DC, $00F8, $00A3, $00D8, $00D7, $00A4,
      $0100, $012A, $00F3, $017B, $017C, $017A, $201D, $00A6,
      $00A9, $00AE, $00AC, $00BD, $00BC, $0141, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $0104, $010C, $0118,
      $0116, $2563, $2551, $2557, $255D, $012E, $0160, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $0172, $016A,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $017D,
      $0105, $010D, $0119, $0117, $012F, $0161, $0173, $016B,
      $017E, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
      $00D3, $00DF, $014C, $0143, $00F5, $00D5, $00B5, $0144,
      $0136, $0137, $013B, $013C, $0146, $0112, $0145, $2019,
      _SHY , $00B1, $201C, $00BE, $00B6, $00A7, $00F7, $201E,
      $00B0, $2219, $00B7, $00B9, $00B3, $00B2, $25A0, $00A0 );

  CP808 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417,
      $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
      $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427,
      $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
      $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437,
      $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
      $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
      $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
      $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
      $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
      $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447,
      $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F,
      $0401, $0451, $0404, $0454, $0407, $0457, $040E, $045E,
      $00B0, $2219, $00B7, $221A, $2116, $20AC, $25A0, $00A0 );

  CP813 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      _SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,
      _SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,
      _SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,
      _SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,
      $00A0, $2018, $2019, $00A3, $20AC, _SPACE, $00A6, $00A7,
      $00A8, $00A9, _SPACE, $00AB, $00AC, _SHY  , _SPACE, $2015,
      $00B0, $00B1, $00B2, $00B3, $00B4, $0385, $0386, $0387,
      $0388, $0389, $038A, $00BB, $038C, $00BD, $038E, $038F,
      $0390, $0391, $0392, $0393, $0394, $0395, $0396, $0397,
      $0398, $0399, $039A, $039B, $039C, $039D, $039E, $039F,
      $03A0, $03A1, _SPACE, $03A3, $03A4, $03A5, $03A6, $03A7,
      $03A8, $03A9, $03AA, $03AB, $03AC, $03AD, $03AE, $03AF,
      $03B0, $03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7,
      $03B8, $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF,
      $03C0, $03C1, $03C2, $03C3, $03C4, $03C5, $03C6, $03C7,
      $03C8, $03C9, $03CA, $03CB, $03CC, $03CD, $03CE, _SPACE );

  CP850 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9,
      $00FF, $00D6, $00DC, $00F8, $00A3, $00D8, $00D7, $0192,
      $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA,
      $00BF, $00AE, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $00C1, $00C2, $00C0,
      $00A9, $2563, $2551, $2557, $255D, $00A2, $00A5, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $00E3, $00C3,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
      $00F0, $00D0, $00CA, $00CB, $00C8, $0131, $00CD, $00CE,
      $00CF, $2518, $250C, $2588, $2584, $00A6, $00CC, $2580,
      $00D3, $00DF, $00D4, $00D2, $00F5, $00D5, $00B5, $00FE,
      $00DE, $00DA, $00DB, $00D9, $00FD, $00DD, $00AF, $00B4,
      _SHY , $00B1, $2017, $00BE, $00B6, $00A7, $00F7, $00B8,
      $00B0, $00A8, $00B7, $00B9, $00B3, $00B2, $25A0, $00A0 );

  CP851 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $0386, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $0388, $00C4, $0389,
      $038A, _SPACE, $038C, $00F4, $00F6, $038E, $00FB, $00F9,
      $038F, $00D6, $00DC, $03AC, $00A3, $03AD, $03AE, $03AF,
      $03CA, $0390, $03CC, $03CD, $0391, $0392, $0393, $0394,
      $0395, $0396, $0397, $00BD, $0398, $0399, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $039A, $039B, $039C,
      $039D, $2563, $2551, $2557, $255D, $039E, $039F, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $03A0, $03A1,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $03A3,
      $03A4, $03A5, $03A6, $03A7, $03A8, $03A9, $03B1, $03B2,
      $03B3, $2518, $250C, $2588, $2584, $03B4, $03B5, $2580,
      $03B6, $03B7, $03B8, $03B9, $03BA, $03BB, $03BC, $03BD,
      $03BE, $03BF, $03C0, $03C1, $03C3, $03C2, $03C4, $0384,
      _SHY , $00B1, $03C5, $03C6, $03C7, $00A7, $03C8, $0385,
      $00B0, $00A8, $03C9, $03CB, $03B0, $03CE, $25A0, $00A0 );

  CP852 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $016F, $0107, $00E7,
      $0142, $00EB, $0150, $0151, $00EE, $0179, $00C4, $0106,
      $00C9, $0139, $013A, $00F4, $00F6, $013D, $013E, $015A,
      $015B, $00D6, $00DC, $0164, $0165, $0141, $00D7, $010D,
      $00E1, $00ED, $00F3, $00FA, $0104, $0105, $017D, $017E,
      $0118, $0119, $00AC, $017A, $010C, $015F, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $00C1, $00C2, $011A,
      $015E, $2563, $2551, $2557, $255D, $017B, $017C, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $0102, $0103,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
      $0111, $0110, $010E, $00CB, $010F, $0147, $00CD, $00CE,
      $011B, $2518, $250C, $2588, $2584, $0162, $016E, $2580,
      $00D3, $00DF, $00D4, $0143, $0144, $0148, $0160, $0161,
      $0154, $00DA, $0155, $0170, $00FD, $00DD, $0163, $00B4,
      _SHY , $02DD, $02DB, $02C7, $02D8, $00A7, $00F7, $00B8,
      $00B0, $00A8, $02D9, $0171, $0158, $0159, $25A0, $00A0 );

  CP853 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $0109, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $0108,
      $00C9, $010B, $010A, $00F4, $00F6, $00F2, $00FB, $00F9,
      $0130, $00D6, $00DC, $011D, $00A3, $011C, $00D7, $0135,
      $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $011E, $011F,
      $0124, $0125, _SPACE, $00BD, $0134, $015F, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $00C1, $00C2, $00C0,
      $015E, $2563, $2551, $2557, $255D, $017B, $017C, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $015C, $015D,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
      _SPACE, _SPACE, $00CA, $00CB, $00C8, $0131, $00CD, $00CE,
      $00CF, $2518, $250C, $2588, $2584, _SPACE, $00CC, $2580,
      $00D3, $00DF, $00D4, $00D2, $0120, $0121, $00B5, $0126,
      $0127, $00DA, $00DB, $00D9, $016C, $016D, $00B7, $00B4,
      _SHY  , _SPACE, $2113, $0149, $02D8, $00A7, $00F7, $00B8,
      $00B0, $00A8, $02D9, _SPACE, $00B3, $00B2, $25A0, $00A0 );

  CP855 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $0452, $0402, $0453, $0403, $0451, $0401, $0454, $0404,
      $0455, $0405, $0456, $0406, $0457, $0407, $0458, $0408,
      $0459, $0409, $045A, $040A, $045B, $040B, $045C, $040C,
      $045E, $040E, $045F, $040F, $044E, $042E, $044A, $042A,
      $0430, $0410, $0431, $0411, $0446, $0426, $0434, $0414,
      $0435, $0415, $0444, $0424, $0433, $0413, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $0445, $0425, $0438,
      $0418, $2563, $2551, $2557, $255D, $0439, $0419, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $043A, $041A,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
      $043B, $041B, $043C, $041C, $043D, $041D, $043E, $041E,
      $043F, $2518, $250C, $2588, $2584, $041F, $044F, $2580,
      $042F, $0440, $0420, $0441, $0421, $0442, $0422, $0443,
      $0423, $0436, $0416, $0432, $0412, $044C, $042C, $2116,
      _SHY , $044B, $042B, $0437, $0417, $0448, $0428, $044D,
      $042D, $0449, $0429, $0447, $0427, $00A7, $25A0, $00A0 );

  CP857 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $0131, $00C4, $00C5,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9,
      $0130, $00D6, $00DC, $00F8, $00A3, $00D8, $015E, $015F,
      $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $011E, $011F,
      $00BF, $00AE, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $00C1, $00C2, $00C0,
      $00A9, $2563, $2551, $2557, $255D, $00A2, $00A5, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $00E3, $00C3,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
      $00BA, $00AA, $00CA, $00CB, $00C8, $20AC, $00CD, $00CE,
      $00CF, $2518, $250C, $2588, $2584, $00A6, $00CC, $2580,
      $00D3, $00DF, $00D4, $00D2, $00F5, $00D5, $00B5, _SPACE,
      $00D7, $00DA, $00DB, $00D9, $00EC, $00FF, $00AF, $00B4,
      _SHY , $00B1, _SPACE, $00BE, $00B6, $00A7, $00F7, $00B8,
      $00B0, $00A8, $00B7, $00B9, $00B3, $00B2, $25A0, $00A0 );

  CP858 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,

      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9,
      $00FF, $00D6, $00DC, $00F8, $00A3, $00D8, $00D7, $0192,
      $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA,
      $00BF, $00AE, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $00C1, $00C2, $00C0,
      $00A9, $2563, $2551, $2557, $255D, $00A2, $00A5, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $00E3, $00C3,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
      $00F0, $00D0, $00CA, $00CB, $00C8, $20AC, $00CD, $00CE,
      $00CF, $2518, $250C, $2588, $2584, $00A6, $00CC, $2580,
      $00D3, $00DF, $00D4, $00D2, $00F5, $00D5, $00B5, $00FE,
      $00DE, $00DA, $00DB, $00D9, $00FD, $00DD, $00AF, $00B4,
      _SHY  , $00B1, $2017, $00BE, $00B6, $00A7, $00F7, $00B8,
      $00B0, $00A8, $00B7, $00B9, $00B3, $00B2, $25A0, $00A0 );

  CP859 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,

      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9,
      $00FF, $00D6, $00DC, $00F8, $00A3, $00D8, $00D7, $0192,
      $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA,
      $00BF, $00AE, $00AC, $0153, $0152, $00A1, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $00C1, $00C2, $00C0,
      $00A9, $2563, $2551, $2557, $255D, $00A2, $00A5, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $00E3, $00C3,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
      $00F0, $00D0, $00CA, $00CB, $00C8, $20AC, $00CD, $00CE,
      $00CF, $2518, $250C, $2588, $2584, $0160, $00CC, $2580,
      $00D3, $00DF, $00D4, $00D2, $00F5, $00D5, $00B5, $00FE,
      $00DE, $00DA, $00DB, $00D9, $00FD, $00DD, $00AF, $017D,
      _SHY  , $00B1, _SPACE, $0178, $00B6, $00A7, $00F7, $017E,
      $00B0, $0161, $00B7, $00B9, $00B3, $00B2, $25A0, $00A0 );

  CP860 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,

      $00C7, $00FC, $00E9, $00E2, $00E3, $00E0, $00C1, $00E7,
      $00EA, $00CA, $00E8, $00CD, $00D4, $00EC, $00C3, $00C2,
      $00C9, $00C0, $00C8, $00F4, $00F5, $00F2, $00DA, $00F9,
      $00CC, $00D5, $00DC, $00A2, $00A3, $00D9, $20A7, $00D3,
      $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA,
      $00BF, $00D2, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
      $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
      $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
      $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
      $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4,
      $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
      $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248,
      $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0 );

  CP861 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,

      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
      $00EA, $00EB, $00E8, $00D0, $00F0, $00DE, $00C4, $00C5,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00FE, $00FB, $00DD,
      $00FD, $00D6, $00DC, $00F8, $00A3, $00D8, $20A7, $0192,
      $00E1, $00ED, $00F3, $00FA, $00C1, $00CD, $00D3, $00DA,
      $00BF, $2310, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
      $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
      $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
      $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
      $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4,
      $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
      $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248,
      $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0 );

  CP863 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00C2, $00E0, $00B6, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $2017, $00C0, $00A7,
      $00C9, $00C8, $00CA, $00F4, $00CB, $00CF, $00FB, $00F9,
      $00A4, $00D4, $00DC, $00A2, $00A3, $00D9, $00DB, $0192,
      $00A6, $00B4, $00F3, $00FA, $00A8, $00B8, $00B3, $00AF,
      $00CE, $2310, $00AC, $00BD, $00BC, $00BE, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
      $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
      $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
      $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
      $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4,
      $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
      $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248,
      $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0 );

  CP865 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9,
      $00FF, $00D6, $00DC, $00F8, $00A3, $00D8, $20A7, $0192,
      $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA,
      $00BF, $2310, $00AC, $00BD, $00BC, $00A1, $00AB, $00A4,
      $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
      $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
      $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
      $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
      $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4,
      $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
      $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248,
      $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0 );

  CP866 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417,
      $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
      $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427,
      $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
      $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437,
      $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
      $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
      $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
      $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
      $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
      $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447,
      $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F,
      $0401, $0451, $0404, $0454, $0407, $0457, $040E, $045E,
      $00B0, $2219, $00B7, $221A, $2116, $00A4, $25A0, $00A0 );

  CP867 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $010C, $00FC, $00E9, $010F, $00E4, $010E, $0164, $010D,
      $011B, $011A, $0139, $00CD, $013E, $013A, $00C4, $00C1,
      $00C9, $017E, $017D, $00F4, $00F6, $00D3, $016F, $00DA,
      $00FD, $00D6, $00DC, $0160, $013D, $00DD, $0158, $0165,
      $00E1, $00ED, $00F3, $00FA, $0148, $0147, $016E, $00D4,
      $0161, $0159, $0155, $0154, $00BC, $00A7, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
      $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
      $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
      $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
      $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4,
      $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
      $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248,
      $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0 );

  CP869 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, $0386, _SPACE,
      $00B7, $00AC, $00A6, $2018, $2019, $0388, $2015, $0389,
      $038A, $03AA, $038C, _SPACE, _SPACE, $038E, $03AB, $00A9,
      $038F, $00B2, $00B3, $03AC, $00A3, $03AD, $03AE, $03AF,
      $03CA, $0390, $03CC, $03CD, $0391, $0392, $0393, $0394,
      $0395, $0396, $0397, $00BD, $0398, $0399, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $039A, $039B, $039C,
      $039D, $2563, $2551, $2557, $255D, $039E, $039F, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $03A0, $03A1,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $03A3,
      $03A4, $03A5, $03A6, $03A7, $03A8, $03A9, $03B1, $03B2,
      $03B3, $2518, $250C, $2588, $2584, $03B4, $03B5, $2580,
      $03B6, $03B7, $03B8, $03B9, $03BA, $03BB, $03BC, $03BD,
      $03BE, $03BF, $03C0, $03C1, $03C3, $03C2, $03C4, $0384,
      _SHY  , $00B1, $03C5, $03C6, $03C7, $00A7, $03C8, $0385,
      $00B0, $00A8, $03C9, $03CB, $03B0, $03CE, $25A0, $00A0 );

  CP872 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $0452, $0402, $0453, $0403, $0451, $0401, $0454, $0404,
      $0455, $0405, $0456, $0406, $0457, $0407, $0458, $0408,
      $0459, $0409, $045A, $040A, $045B, $040B, $045C, $040C,
      $045E, $040E, $045F, $040F, $044E, $042E, $044A, $042A,
      $0430, $0410, $0431, $0411, $0446, $0426, $0434, $0414,
      $0435, $0415, $0444, $0424, $0433, $0413, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $0445, $0425, $0438,
      $0418, $2563, $2551, $2557, $255D, $0439, $0419, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $043A, $041A,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $20AC,
      $043B, $041B, $043C, $041C, $043D, $041D, $043E, $041E,
      $043F, $2518, $250C, $2588, $2584, $041F, $044F, $2580,
      $042F, $0440, $0420, $0441, $0421, $0442, $0422, $0443,
      $0423, $0436, $0416, $0432, $0412, $044C, $042C, $2116,
      _SHY  , $044B, $042B, $0437, $0417, $0448, $0428, $044D,
      $042D, $0449, $0429, $0447, $0427, $00A7, $25A0, $00A0 );

  CP878 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $2500, $2502, $250C, $2510, $2514, $2518, $251C, $2524,
      $252C, $2534, $253C, $2580, $2584, $2588, $258C, $2590,
      $2591, $2592, $2593, $2320, $25A0, $2219, $221A, $2248,
      $2264, $2265, $00A0, $2321, $00B0, $00B2, $00B7, $00F7,
      $2550, $2551, $2552, $0451, $2553, $2554, $2555, $2556,
      $2557, $2558, $2559, $255A, $255B, $255C, $255D, $255E,
      $255F, $2560, $2561, $0401, $2562, $2563, $2564, $2565,
      $2566, $2567, $2568, $2569, $256A, $256B, $256C, $00A9,
      $044E, $0430, $0431, $0446, $0434, $0435, $0444, $0433,
      $0445, $0438, $0439, $043A, $043B, $043C, $043D, $043E,
      $043F, $044F, $0440, $0441, $0442, $0443, $0436, $0432,
      $044C, $044B, $0437, $0448, $044D, $0449, $0447, $044A,
      $042E, $0410, $0411, $0426, $0414, $0415, $0424, $0413,
      $0425, $0418, $0419, $041A, $041B, $041C, $041D, $041E,
      $041F, $042F, $0420, $0421, $0422, $0423, $0416, $0412,
      $042C, $042B, $0417, $0428, $042D, $0429, $0427, $042A );

  CP912 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $2591, $2592, $2593, $2502, $2524, $2518, $250C, $2588,
      $00A9, $2563, $2551, $2557, $255D, $00A2, $00A5, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $2584, $2580,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00AE,
      $00A0, $0104, $02D8, $0141, $00A4, $013D, $015A, $00A7,
      $00A8, $0160, $015E, $0164, $0179, _SHY  , $017D, $017B,
      $00B0, $0105, $02DB, $0142, $00B4, $013E, $015B, $02C7,
      $00B8, $0161, $015F, $0165, $017A, $02DD, $017E, $017C,
      $0154, $00C1, $00C2, $0102, $00C4, $0139, $0106, $00C7,
      $010C, $00C9, $0118, $00CB, $011A, $00CD, $00CE, $010E,
      $0110, $0143, $0147, $00D3, $00D4, $0150, $00D6, $00D7,
      $0158, $016E, $00DA, $0170, $00DC, $00DD, $0162, $00DF,
      $0155, $00E1, $00E2, $0103, $00E4, $013A, $0107, $00E7,
      $010D, $00E9, $0119, $00EB, $011B, $00ED, $00EE, $010F,
      $0111, $0144, $0148, $00F3, $00F4, $0151, $00F6, $00F7,
      $0159, $016F, $00FA, $0171, $00FC, $00FD, $0163, $02D9 );

  CP915 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $2591, $2592, $2593, $2502, $2524, $2518, $250C, $2588,
      $00A9, $2563, $2551, $2557, $255D, $00A2, $00A5, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $2584, $2580,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
      $00A0, $0401, $0402, $0403, $0404, $0405, $0406, $0407,
      $0408, $0409, $040A, $040B, $040C, _SHY  , $040E, $040F,
      $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417,
      $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
      $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427,
      $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
      $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437,
      $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
      $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447,
      $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F,
      $2116, $0451, $0452, $0453, $0454, $0455, $0456, $0457,
      $0458, $0459, $045A, $045B, $045C, $00A7, $045E, $045F );

  CP920 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      _SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,
      _SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,
      _SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,
      _SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,
      $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
      $00A8, $00A9, $00AA, $00AB, $00AC, _SHY  , $00AE, $00AF,
      $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
      $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
      $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
      $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
      $011E, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7,
      $00D8, $00D9, $00DA, $00DB, $00DC, $0130, $015E, $00DF,
      $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
      $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
      $011F, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7,
      $00F8, $00F9, $00FA, $00FB, $00FC, $0131, $015F, $00FF );

  CP1117 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $0106, $00FC, $00E9, $0101, $00E4, $0123, $00E5, $0107,
      $0142, $0113, $0117, $012F, $012B, $0179, $00C4, $00C5,
      $00C9, $017B, $017C, $014D, $00F6, $0122, $016B, $015A,
      $015B, $00D6, $00DC, $0144, $013B, $0141, $00D7, $010D,
      $0100, $012A, $00F3, $0173, $0104, $0105, $017D, $017E,
      $0118, $0119, $0116, $017A, $010C, $012E, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
      $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
      $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
      $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
      $00D3, $00DF, $014C, $0143, $00F5, $00D5, $0160, $0161,
      $0136, $0137, $016A, $0172, $013C, $0112, $0145, $0146,
      _SHY , $00B1, $00E6, $00C6, $00B6, $00A4, $00F7, $00F8,
      $00B0, $00D8, $00B7, $0157, $0156, $201E, $201C, $00A0 );

  CPMIK : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,

      $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417,
      $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
      $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427,
      $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
      $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437,
      $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
      $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447,
      $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F,
      $2514, $2534, $252C, $251C, $2500, $253C, $2563, $2551,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2510,
      $2591, $2592, $2593, $2502, $2524, $2116, $00A7, $2557,
      $255D, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
      $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4,
      $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
      $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248,
      $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0 );

  WIN1250 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $20AC, _SPACE, $201A, _SPACE, $201E, $2026, $2020, $2021,
      _SPACE, $2030, $0160, $2039, $015A, $0164, $017D, $0179,
      _SPACE, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
      _SPACE, $2122, $0161, $203A, $015B, $0165, $017E, $017A,
      $00A0, $02C7, $02D8, $0141, $00A4, $0104, $00A6, $00A7,
      $00A8, $00A9, $015E, $00AB, $00AC, _SHY  , $00AE, $017B,
      $00B0, $00B1, $02DB, $0142, $00B4, $00B5, $00B6, $00B7,
      $00B8, $0105, $015F, $00BB, $013D, $02DD, $013E, $017C,
      $0154, $00C1, $00C2, $0102, $00C4, $0139, $0106, $00C7,
      $010C, $00C9, $0118, $00CB, $011A, $00CD, $00CE, $010E,
      $0110, $0143, $0147, $00D3, $00D4, $0150, $00D6, $00D7,
      $0158, $016E, $00DA, $0170, $00DC, $00DD, $0162, $00DF,
      $0155, $00E1, $00E2, $0103, $00E4, $013A, $0107, $00E7,
      $010D, $00E9, $0119, $00EB, $011B, $00ED, $00EE, $010F,
      $0111, $0144, $0148, $00F3, $00F4, $0151, $00F6, $00F7,
      $0159, $016F, $00FA, $0171, $00FC, $00FD, $0163, $02D9 );

  WIN1251 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $0402, $0403, $201A, $0453, $201E, $2026, $2020, $2021,
      $20AC, $2030, $0409, $2039, $040A, $040C, $040B, $040F,
      $0452, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
      _SPACE, $2122, $0459, $203A, $045A, $045C, $045B, $045F,
      $00A0, $040E, $045E, $0408, $00A4, $0490, $00A6, $00A7,
      $0401, $00A9, $0404, $00AB, $00AC, _SHY  , $00AE, $0407,
      $00B0, $00B1, $0406, $0456, $0491, $00B5, $00B6, $00B7,
      $0451, $2116, $0454, $00BB, $0458, $0405, $0455, $0457,
      $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417,
      $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
      $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427,
      $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
      $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437,
      $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
      $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447,
      $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F );

  WIN1253 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $20AC, _SPACE, $201A, $0192, $201E, $2026, $2020, $2021,
      _SPACE, $2030, _SPACE, $2039, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
      _SPACE, $2122, _SPACE, $203A, _SPACE, _SPACE, _SPACE, _SPACE,
      $00A0, $0385, $0386, $00A3, $00A4, $00A5, $00A6, $00A7,
      $00A8, $00A9, _SPACE, $00AB, $00AC, _SHY  , $00AE, $2015,
      $00B0, $00B1, $00B2, $00B3, $0384, $00B5, $00B6, $00B7,
      $0388, $0389, $038A, $00BB, $038C, $00BD, $038E, $038F,
      $0390, $0391, $0392, $0393, $0394, $0395, $0396, $0397,
      $0398, $0399, $039A, $039B, $039C, $039D, $039E, $039F,
      $03A0, $03A1, _SPACE, $03A3, $03A4, $03A5, $03A6, $03A7,
      $03A8, $03A9, $03AA, $03AB, $03AC, $03AD, $03AE, $03AF,
      $03B0, $03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7,
      $03B8, $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF,
      $03C0, $03C1, $03C2, $03C3, $03C4, $03C5, $03C6, $03C7,
      $03C8, $03C9, $03CA, $03CB, $03CC, $03CD, $03CE, _SPACE );

  WIN1254 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $20AC, _SPACE, $201A, $0192, $201E, $2026, $2020, $2021,
      $02C6, $2030, $0160, $2039, $0152, _SPACE, _SPACE, _SPACE,
      _SPACE, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
      $02DC, $2122, $0161, $203A, $0153, _SPACE, _SPACE, $0178,
      $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
      $00A8, $00A9, $00AA, $00AB, $00AC, _SHY  , $00AE, $00AF,
      $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
      $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
      $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
      $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
      $011E, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7,
      $00D8, $00D9, $00DA, $00DB, $00DC, $0130, $015E, $00DF,
      $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
      $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
      $011F, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7,
      $00F8, $00F9, $00FA, $00FB, $00FC, $0131, $015F, $00FF );

  WIN1257 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $20AC, _SPACE, $201A, _SPACE, $201E, $2026, $2020, $2021,
      _SPACE, $2030, _SPACE, $2039, _SPACE, $00A8, $02C7, $00B8,
      _SPACE, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
      _SPACE, $2122, _SPACE, $203A, _SPACE, $00AF, $02DB, _SPACE,
      $00A0, _SPACE, $00A2, $00A3, $00A4, _SPACE, $00A6, $00A7,
      $00D8, $00A9, $0156, $00AB, $00AC, _SHY  , $00AE, $00C6,
      $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
      $00F8, $00B9, $0157, $00BB, $00BC, $00BD, $00BE, $00E6,
      $0104, $012E, $0100, $0106, $00C4, $00C5, $0118, $0112,
      $010C, $00C9, $0179, $0116, $0122, $0136, $012A, $013B,
      $0160, $0143, $0145, $00D3, $014C, $00D5, $00D6, $00D7,
      $0172, $0141, $015A, $016A, $00DC, $017B, $017D, $00DF,
      $0105, $012F, $0101, $0107, $00E4, $00E5, $0119, $0113,
      $010D, $00E9, $017A, $0117, $0123, $0137, $012B, $013C,
      $0161, $0144, $0146, $00F3, $014D, $00F5, $00F6, $00F7,
      $0173, $0142, $015B, $016B, $00FC, $017C, $017E, $02D9 );

  KOI8_R : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $2500, $2502, $250C, $2510, $2514, $2518, $251C, $2524,
      $252C, $2534, $253C, $2580, $2584, $2588, $258C, $2590,
      $2591, $2592, $2593, $2320, $25A0, $2219, $221A, $2248,
      $2264, $2265, $00A0, $2321, $00B0, $00B2, $00B7, $00F7,
      $2550, $2551, $2552, $0451, $2553, $2554, $2555, $2556,
      $2557, $2558, $2559, $255A, $255B, $255C, $255D, $255E,
      $255F, $2560, $2561, $0401, $2562, $2563, $2564, $2565,
      $2566, $2567, $2568, $2569, $256A, $256B, $256C, $00A9,
      $044E, $0430, $0431, $0446, $0434, $0435, $0444, $0433,
      $0445, $0438, $0439, $043A, $043B, $043C, $043D, $043E,
      $043F, $044F, $0440, $0441, $0442, $0443, $0436, $0432,
      $044C, $044B, $0437, $0448, $044D, $0449, $0447, $044A,
      $042E, $0410, $0411, $0426, $0414, $0415, $0424, $0413,
      $0425, $0418, $0419, $041A, $041B, $041C, $041D, $041E,
      $041F, $042F, $0420, $0421, $0422, $0423, $0416, $0412,
      $042C, $042B, $0417, $0428, $042D, $0429, $0427, $042A );

  KOI8_U : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $2500, $2502, $250C, $2510, $2514, $2518, $251C, $2524,
      $252C, $2534, $253C, $2580, $2584, $2588, $258C, $2590,
      $2591, $2592, $2593, $2320, $25A0, $2219, $221A, $2248,
      $2264, $2265, $00A0, $2321, $00B0, $00B2, $00B7, $00F7,
      $2550, $2551, $2552, $0451, $0454, $2554, $0456, $0457,
      $2557, $2558, $2559, $255A, $255B, $0491, $255D, $255E,
      $255F, $2560, $2561, $0401, $0404, $2563, $0406, $0407,
      $2566, $2567, $2568, $2569, $256A, $0490, $256C, $00A9,
      $044E, $0430, $0431, $0446, $0434, $0435, $0444, $0433,
      $0445, $0438, $0439, $043A, $043B, $043C, $043D, $043E,
      $043F, $044F, $0440, $0441, $0442, $0443, $0436, $0432,
      $044C, $044B, $0437, $0448, $044D, $0449, $0447, $044A,
      $042E, $0410, $0411, $0426, $0414, $0415, $0424, $0413,
      $0425, $0418, $0419, $041A, $041B, $041C, $041D, $041E,
      $041F, $042F, $0420, $0421, $0422, $0423, $0416, $0412,
      $042C, $042B, $0417, $0428, $042D, $0429, $0427, $042A );

  ISO8859_1 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9,
      $00FF, $00D6, $00DC, $00A2, $00A3, $00A5, $20A7, $0192,
      $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
      $00A8, $00A9, $00AA, $00AB, $00AC, $2010, $00AE, $00AF,
      $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
      $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
      $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
      $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
      $00D0, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7,
      $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
      $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
      $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
      $00F0, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7,
      $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $00FE, $00FF );

  ISO8859_2 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $2500, $2502, $250C, $2510, $2514, $2518, $251C, $2524,
      $252C, $2534, $253C, $2580, $2584, $2588, $258C, $2590,
      $2591, $2592, $2593, $2320, $25A0, $2219, $221A, $2248,
      $2264, $2265, $00A0, $2321, $00B0, $00B2, $00B7, $00F7,
      $00A0, $0104, $02D8, $0141, $00A4, $013D, $015A, $00A7,
      $00A8, $0160, $015E, $0164, $0179, $2010, $017D, $017B,
      $00B0, $0105, $02DB, $0142, $00B4, $013E, $015B, $02C7,
      $00B8, $0161, $015F, $0165, $017A, $02DD, $017E, $017C,
      $0154, $00C1, $00C2, $0102, $00C4, $0139, $0106, $00C7,
      $010C, $00C9, $0118, $00CB, $011A, $00CD, $00CE, $010E,
      $0110, $0143, $0147, $00D3, $00D4, $0150, $00D6, $00D7,
      $0158, $016E, $00DA, $0170, $00DC, $00DD, $0162, $00DF,
      $0155, $00E1, $00E2, $0103, $00E4, $013A, $0107, $00E7,
      $010D, $00E9, $0119, $00EB, $011B, $00ED, $00EE, $010F,
      $0111, $0144, $0148, $00F3, $00F4, $0151, $00F6, $00F7,
      $0159, $016F, $00FA, $0171, $00FC, $00FD, $0163, $02D9 );

  ISO8859_3 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9,
      $00FF, $00D6, $00DC, $00A2, $00A3, $00A5, $20A7, $0192,
      $00A0, $0126, $02D8, $00A3, $00A4, _SPACE, $0124, $00A7,
      $00A8, $0130, $015E, $011E, $0134, $2010, _SPACE, $017B,
      $00B0, $0127, $00B2, $00B3, $00B4, $00B5, $0125, $00B7,
      $00B8, $0131, $015F, $011F, $0135, $00BD, _SPACE, $017C,
      $00C0, $00C1, $00C2, _SPACE, $00C4, $010A, $0108, $00C7,
      $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
      _SPACE, $00D1, $00D2, $00D3, $00D4, $0120, $00D6, $00D7,
      $011C, $00D9, $00DA, $00DB, $00DC, $016C, $015C, $00DF,
      $00E0, $00E1, $00E2, _SPACE, $00E4, $010B, $0109, $00E7,
      $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
      _SPACE, $00F1, $00F2, $00F3, $00F4, $0121, $00F6, $00F7,
      $011D, $00F9, $00FA, $00FB, $00FC, $016D, $015D, $02D9 );

  ISO8859_4 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $2500, $2502, $250C, $2510, $2514, $2518, $251C, $2524,
      $252C, $2534, $253C, $2580, $2584, $2588, $258C, $2590,
      $2591, $2592, $2593, $2320, $25A0, $2219, $221A, $2248,
      $2264, $2265, $00A0, $2321, $00B0, $00B2, $00B7, $00F7,
      $00A0, $0104, $0138, $0156, $00A4, $0128, $013B, $00A7,
      $00A8, $0160, $0112, $0122, $0166, $2010, $017D, $00AF,
      $00B0, $0105, $02DB, $0157, $00B4, $0129, $013C, $02C7,
      $00B8, $0161, $0113, $0123, $0167, $014A, $017E, $014B,
      $0100, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $012E,
      $010C, $00C9, $0118, $00CB, $0116, $00CD, $00CE, $012A,
      $0110, $0145, $014C, $0136, $00D4, $00D5, $00D6, $00D7,
      $00D8, $0172, $00DA, $00DB, $00DC, $0168, $016A, $00DF,
      $0101, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $012F,
      $010D, $00E9, $0119, $00EB, $0117, $00ED, $00EE, $012B,
      $0111, $0146, $014D, $0137, $00F4, $00F5, $00F6, $00F7,
      $00F8, $0173, $00FA, $00FB, $00FC, $0169, $016B, $02D9 );

  ISO8859_5 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9,
      $00FF, $00D6, $00DC, $00A2, $00A3, $00A5, $20A7, $0192,
      $00A0, $0401, $0402, $0403, $0404, $0405, $0406, $0407,
      $0408, $0409, $040A, $040B, $040C, $2010, $040E, $040F,
      $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417,
      $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
      $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427,
      $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
      $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437,
      $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
      $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447,
      $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F,
      $2116, $0451, $0452, $0453, $0454, $0455, $0456, $0457,
      $0458, $0459, $045A, $045B, $045C, $00A7, $045E, $045F );

  ISO8859_7 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9,
      $00FF, $00D6, $00DC, $00A2, $00A3, $00A5, $20A7, $0192,
      $00A0, $2018, $2019, $00A3, $20AC, $20AF, $00A6, $00A7,
      $00A8, $00A9, $037A, $00AB, $00AC, $2010, _SPACE, $2015,
      $00B0, $00B1, $00B2, $00B3, $0384, $0385, $0386, $00B7,
      $0388, $0389, $038A, $00BB, $038C, $00BD, $038E, $038F,
      $0390, $0391, $0392, $0393, $0394, $0395, $0396, $0397,
      $0398, $0399, $039A, $039B, $039C, $039D, $039E, $039F,
      $03A0, $03A1, _SPACE, $03A3, $03A4, $03A5, $03A6, $03A7,
      $03A8, $03A9, $03AA, $03AB, $03AC, $03AD, $03AE, $03AF,
      $03B0, $03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7,
      $03B8, $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF,
      $03C0, $03C1, $03C2, $03C3, $03C4, $03C5, $03C6, $03C7,
      $03C8, $03C9, $03CA, $03CB, $03CC, $03CD, $03CE, _SPACE );

  ISO8859_9 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9,
      $00FF, $00D6, $00DC, $00A2, $00A3, $00A5, $20A7, $0192,
      $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
      $00A8, $00A9, $00AA, $00AB, $00AC, _SHY,   $00AE, $00AF,
      $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
      $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
      $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
      $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
      $011E, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7,
      $00D8, $00D9, $00DA, $00DB, $00DC, $0130, $015E, $00DF,
      $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
      $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
      $011F, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7,
      $00F8, $00F9, $00FA, $00FB, $00FC, $0131, $015F, $00FF );

  ISO8859_10 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9,
      $00FF, $00D6, $00DC, $00A2, $00A3, $00A5, $20A7, $0192,
      $00A0, $0104, $0112, $0122, $012A, $0128, $0136, $00A7,
      $013B, $0110, $0160, $0166, $017D, _SHY,   $016A, $014A,
      $00B0, $0105, $0113, $0123, $012B, $0129, $0137, $00B7,
      $013C, $0111, $0161, $0167, $017E, $2015, $016B, $014B,
      $0100, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $012E,
      $010C, $00C9, $0118, $00CB, $0116, $00CD, $00CE, $00CF,
      $00D0, $0145, $014C, $00D3, $00D4, $00D5, $00D6, $0168,
      $00D8, $0172, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
      $0101, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $012F,
      $010D, $00E9, $0119, $00EB, $0117, $00ED, $00EE, $00EF,
      $00F0, $0146, $014D, $00F3, $00F4, $00F5, $00F6, $0169,
      $00F8, $0173, $00FA, $00FB, $00FC, $00FD, $00FE, $0138 );

  ISO8859_13 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9,
      $00FF, $00D6, $00DC, $00A2, $00A3, $00A5, $20A7, $0192,
      $00A0, $201D, $00A2, $00A3, $00A4, $201E, $00A6, $00A7,
      $00D8, $00A9, $0156, $00AB, $00AC, _SHY,   $00AE, $00C6,
      $00B0, $00B1, $00B2, $00B3, $201C, $00B5, $00B6, $00B7,
      $00F8, $00B9, $0157, $00BB, $00BC, $00BD, $00BE, $00E6,
      $0104, $012E, $0100, $0106, $00C4, $00C5, $0118, $0112,
      $010C, $00C9, $0179, $0116, $0122, $0136, $012A, $013B,
      $0160, $0143, $0145, $00D3, $014C, $00D5, $00D6, $00D7,
      $0172, $0141, $015A, $016A, $00DC, $017B, $017D, $00DF,
      $0105, $012F, $0101, $0107, $00E4, $00E5, $0119, $0113,
      $010D, $00E9, $017A, $0117, $0123, $0137, $012B, $013C,
      $0161, $0144, $0146, $00F3, $014D, $00F5, $00F6, $00F7,
      $0173, $0142, $015B, $016B, $00FC, $017C, $017E, $2019 );

  ISO8859_14 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9,
      $00FF, $00D6, $00DC, $00A2, $00A3, $00A5, $20A7, $0192,
      $00A0, $1E02, $1E03, $00A3, $010A, $010B, $1E0A, $00A7,
      $1E80, $00A9, $1E82, $1E0B, $1EF2, _SHY,   $00AE, $0178,
      $1E1E, $1E1F, $0120, $0121, $1E40, $1E41, $00B6, $1E56,
      $1E81, $1E57, $1E83, $1E60, $1EF3, $1E84, $1E85, $1E61,
      $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
      $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
      $0174, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $1E6A,
      $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $0176, $00DF,
      $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
      $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
      $0175, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $1E6B,
      $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $0177, $00FF );

  ISO8859_15 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9,
      $00FF, $00D6, $00DC, $00A2, $00A3, $00A5, $20A7, $0192,
      $00A0, $00A1, $00A2, $00A3, $20AC, $00A5, $0160, $00A7,
      $0161, $00A9, $00AA, $00AB, $00AC, _SHY,   $00AE, $00AF,
      $00B0, $00B1, $00B2, $00B3, $017D, $00B5, $00B6, $00B7,
      $017E, $00B9, $00BA, $00BB, $0152, $0153, $0178, $00BF,
      $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
      $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
      $00D0, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7,
      $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
      $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
      $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
      $00F0, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7,
      $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $00FE, $00FF );

  ISO8859_16 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00C7, $00FC, $00E9, $00E2, $00E4, $00E0, $00E5, $00E7,
      $00EA, $00EB, $00E8, $00EF, $00EE, $00EC, $00C4, $00C5,
      $00C9, $00E6, $00C6, $00F4, $00F6, $00F2, $00FB, $00F9,
      $00FF, $00D6, $00DC, $00A2, $00A3, $00A5, $20A7, $0192,
      $00A0, $0104, $0105, $0141, $20AC, $201E, $0160, $00A7,
      $0161, $00A9, $0218, $00AB, $0179, _SHY,   $017A, $017B,
      $00B0, $00B1, $010C, $0142, $017D, $201D, $00B6, $00B7,
      $017E, $010D, $0219, $00BB, $0152, $0153, $0178, $017C,
      $00C0, $00C1, $00C2, $0102, $00C4, $0106, $00C6, $00C7,
      $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
      $0110, $0143, $00D2, $00D3, $00D4, $0150, $00D6, $015A,
      $0170, $00D9, $00DA, $00DB, $00DC, $0118, $021A, $00DF,
      $00E0, $00E1, $00E2, $0103, $00E4, $0107, $00E6, $00E7,
      $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
      $0111, $0144, $00F2, $00F3, $00F4, $0151, $00F6, $015B,
      $0171, $00F9, $00FA, $00FB, $00FC, $0119, $021B, $00FF );

  ARMSCII_8 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      _SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,
      _SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,
      _SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,
      _SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,_SPACE,
      $00a0, $058E, $0587, $0589, $0029, $0028, $00bb, $00a8,
      $2014, $00b7, $0559, $055d, $002d, $055f, $2026, $055c,
      $055b, $055e, $0531, $0561, $0532, $0562, $0533, $0563,
      $0534, $0564, $0535, $0565, $0536, $0566, $0537, $0567,
      $0538, $0568, $0539, $0569, $053a, $056a, $053b, $056b,
      $053c, $056c, $053d, $056d, $053e, $056e, $053f, $056f,
      $0540, $0570, $0541, $0571, $0542, $0572, $0543, $0573,
      $0544, $0574, $0545, $0575, $0546, $0576, $0547, $0577,
      $0548, $0578, $0549, $0579, $054a, $057a, $054b, $057b,
      $054c, $057c, $054d, $057d, $054e, $057e, $054f, $057f,
      $0550, $0580, $0551, $0581, $0552, $0582, $0553, $0583,
      $0554, $0584, $0555, $0585, $0556, $0586, $055a, _SPACE );

  HAIK8 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00a0, $058E, $0587, $0589, $0029, $0028, $00bb, $00a8,
      $2014, $00b7, $0559, $055d, $002d, $055f, $2026, $055c,
      $055b, $055e, $0531, $0561, $0532, $0562, $0533, $0563,
      $0534, $0564, $0535, $0565, $0536, $0566, $0537, $0567,
      $0538, $0568, $0539, $0569, $053a, $056a, $053b, $056b,
      $053c, $056c, $053d, $056d, $053e, $056e, $053f, $056f,
      $0540, $0570, $0541, $0571, $0542, $0572, $0543, $0573,
      $0544, $0574, $0545, $0575, $0546, $0576, $0547, $0577,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      $0548, $0578, $0549, $0579, $054a, $057a, $054b, $057b,
      $054c, $057c, $054d, $057d, $054e, $057e, $054f, $057f,
      $0550, $0580, $0551, $0581, $0552, $0582, $0553, $0583,
      $0554, $0584, $0555, $0585, $0556, $0586, $055a, _SPACE );

  CP1131 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417,
      $0418, $0419, $041a, $041b, $041c, $041d, $041e, $041f,
      $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427,
      $0428, $0429, $042a, $042b, $042c, $042d, $042e, $042f,
      $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437,
      $0438, $0439, $043a, $043b, $043c, $043d, $043e, $043f,
      $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
      $2555, $2563, $2551, $2557, $255d, $255c, $255b, $2510,
      $2514, $2534, $252c, $251c, $2500, $253c, $255e, $255f,
      $255a, $2554, $2569, $2566, $2560, $2550, $256c, $2567,
      $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256b,
      $256a, $2518, $250c, $2588, $2584, $258c, $2590, $2580,
      $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447,
      $0448, $0449, $044a, $044b, $044c, $044d, $044e, $044f,
      $0401, $0451, $0404, $0454, $0407, $0457, $040e, $045e,
      $0406, $0456, $00b7, $00a4, $0490, $0491, $2219, $00a0 );

  CP862 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $05D0, $05D1, $05D2, $05D3, $05D4, $05D5, $05D6, $05D7,
      $05D8, $05D9, $05DA, $05DB, $05DC, $05DD, $05DE, $05DF,
      $05E0, $05E1, $05E2, $05E3, $05E4, $05E5, $05E6, $05E7,
      $05E8, $05E9, $05EA, $00A2, $00A3, $00A5, $20A7, $0192,
      $00E1, $00ED, $00F3, $00FA, $00F1, $00D1, $00AA, $00BA,
      $00BF, $2310, $00AC, $00BD, $00BC, $00A1, $00AB, $00BB,
      $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
      $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
      $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
      $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
      $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
      $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
      $03B1, $00DF, $0393, $03C0, $03A3, $03C3, $00B5, $03C4,
      $03A6, $0398, $03A9, $03B4, $221E, $03C6, $03B5, $2229,
      $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248,
      $00B0, $2219, $00B7, $221A, $207F, $00B2, $25A0, $00A0 );

  ISO8859_8 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      $00A0, _SPACE, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
      $00A8, $00A9, $00D7, $00AB, $00AC, _SHY,   $00AE, $00AF,
      $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
      $00B8, $00B9, $00F7, $00BB, $00BC, $00BD, $00BE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, $2017,
      $05D0, $05D1, $05D2, $05D3, $05D4, $05D5, $05D6, $05D7,
      $05D8, $05D9, $05DA, $05DB, $05DC, $05DD, $05DE, $05DF,
      $05E0, $05E1, $05E2, $05E3, $05E4, $05E5, $05E6, $05E7,
      $05E8, $05E9, $05EA, _SPACE, _SPACE, $200E, $200F, _SPACE );

  WIN1255 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $20AC, _SPACE, $201A, $0192, $201E, $2026, $2020, $2021,
      $02C6, $2030, _SPACE, $2039, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
      $02DC, $2122, _SPACE, $203A, _SPACE, _SPACE, _SPACE, _SPACE,
      $00A0, $00A1, $00A2, $00A3, $20AA, $00A5, $00A6, $00A7,
      $00A8, $00A9, $00D7, $00AB, $00AC, _SHY,   $00AE, $00AF,
      $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
      $00B8, $00B9, $00F7, $00BB, $00BC, $00BD, $00BE, $00BF,
      $05B0, $05B1, $05B2, $05B3, $05B4, $05B5, $05B6, $05B7,
      $05B8, $05B9, $05BA, $05BB, $05BC, $05BD, $05BE, $05BF,
      $05C0, $05C1, $05C2, $05C3, $05F0, $05F1, $05F2, $05F3,
      $05F4, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      $05D0, $05D1, $05D2, $05D3, $05D4, $05D5, $05D6, $05D7,
      $05D8, $05D9, $05DA, $05DB, $05DC, $05DD, $05DE, $05DF,
      $05E0, $05E1, $05E2, $05E3, $05E4, $05E5, $05E6, $05E7,
      $05E8, $05E9, $05EA, _SPACE, _SPACE, $200E, $200F, _SPACE );

  WIN1256 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $20AC, $067E, $201A, $0192, $201E, $2026, $2020, $2021,
      $02C6, $2030, $0679, $2039, $0152, $0686, $0698, $0688,
      $06AF, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
      $06A9, $2122, $0691, $203A, $0153, $200C, $200D, $06BA,
      $00A0, $060C, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
      $00A8, $00A9, $06BE, $00AB, $00AC, _SHY,   $00AE, $00AF,
      $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
      $00B8, $00B9, $061B, $00BB, $00BC, $00BD, $00BE, $061F,
      $06C1, $0621, $0622, $0623, $0624, $0625, $0626, $0627,
      $0628, $0629, $062A, $062B, $062C, $062D, $062E, $062F,
      $0630, $0631, $0632, $0633, $0634, $0635, $0636, $00D7,
      $0637, $0638, $0639, $063A, $0640, $0641, $0642, $0643,
      $00E0, $0644, $00E2, $0645, $0646, $0647, $0648, $00E7,
      $00E8, $00E9, $00EA, $00EB, $0649, $064A, $00EE, $00EF,
      $064B, $064C, $064D, $064E, $00F4, $064F, $0650, $00F7,
      $0651, $00F9, $0652, $00FB, $00FC, $200E, $200F, $06D2 );

  ISO8859_6 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      $00A0, _SPACE, _SPACE, _SPACE, $00A4, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, $060C, _SHY,   _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, $0618, _SPACE, _SPACE, _SPACE, $061F,
      _SPACE, $0621, $0622, $0623, $0624, $0625, $0626, $0627,
      $0628, $0629, $062A, $062B, $062C, $062D, $062E, $062F,
      $0630, $0631, $0632, $0633, $0634, $0635, $0636, $0637,
      $0638, $0639, $063A, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      $0640, $0641, $0642, $0643, $0644, $0645, $0646, $0647,
      $0648, $0649, $064A, $064B, $064C, $064D, $064E, $064F,
      $0650, $0651, $0652, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE );

  CP864 : packed array [0..255] of UInt16 = (
      _SPACE, $263A, $263B, $2665, $2666, $2663, $2660, $2022,
      $25D8, $25CB, $25D9, $2642, $2640, $266A, $266B, $263C,
      $25BA, $25C4, $2195, $203C, $00B6, $00A7, $25AC, $21A8,
      $2191, $2193, $2192, $2190, $221F, $2194, $25B2, $25BC,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,
      $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $007B, $007C, $007D, $007E, $2302,
      $00B0, $00B7, $2219, $221A, $2592, $2500, $2502, $253C,
      $2524, $252C, $251C, $2534, $2510, $250C, $2514, $2518,
      $03B2, $221E, $03C6, $00B1, $00BD, $00BC, $2248, $00AB,
      $00BB, $FEF7, $FEF8, _SPACE, _SPACE, $FEFB, $FEFC, _SPACE,
      $00A0, _SHY,   $FE82, $00A3, $00A4, $FE84, _SPACE, $20AC,
      $FE8E, $FE8F, $FE95, $FE99, $060C, $FE9D, $FEA1, $FEA5,
      $0660, $0661, $0662, $0663, $0664, $0665, $0666, $0667,
      $0668, $0669, $FED1, $061B, $FEB1, $FEB5, $FEB9, $061F,
      $00A2, $FE80, $FE81, $FE83, $FE85, $FECA, $FE8B, $FE8D,
      $FE91, $FE93, $FE97, $FE9B, $FE9F, $FEA3, $FEA7, $FEA9,
      $FEAB, $FEAD, $FEAF, $FEB3, $FEB7, $FEBB, $FEBF, $FEC1,
      $FEC5, $FECB, $FECF, $00A6, $00AC, $00F7, $00D7, $FEC9,
      $0640, $FED3, $FED7, $FEDB, $FEDF, $FEE3, $FEE7, $FEEB,
      $FEED, $FEEF, $FEF3, $FEBD, $FECC, $FECE, $FECD, $FEE1,
      $FE7D, $0651, $FEE5, $FEE9, $FEEC, $FEF0, $FEF2, $FED0,
      $FED5, $FEF5, $FEF6, $FEDD, $FED9, $FEF1, $25A0, _SPACE );

  TELETEXT : packed array [0..255] of UInt16 = (
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
      $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,
      $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
      $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,
      $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
      $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,
      $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
      $0058, $0059, $005A, $2190, $00BD, $2192, $2191, $2014,
      $00A3, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
      $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,
      $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
      $0078, $0079, $007A, $00BC, $2016, $00BE, $00F7, $25A0,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE,
      _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE, _SPACE );

  RAW : packed array [0..255] of UInt16 = (
      $E000, $E001, $E002, $E003, $E004, $E005, $E006, $E007,
      $E008, $E009, $E00A, $E00B, $E00C, $E00D, $E00E, $E00F,
      $E010, $E011, $E012, $E013, $E014, $E015, $E016, $E017,
      $E018, $E019, $E01A, $E01B, $E01C, $E01D, $E01E, $E01F,
      $E020, $E021, $E022, $E023, $E024, $E025, $E026, $E027,
      $E028, $E029, $E02A, $E02B, $E02C, $E02D, $E02E, $E02F,
      $E030, $E031, $E032, $E033, $E034, $E035, $E036, $E037,
      $E038, $E039, $E03A, $E03B, $E03C, $E03D, $E03E, $E03F,
      $E040, $E041, $E042, $E043, $E044, $E045, $E046, $E047,
      $E048, $E049, $E04A, $E04B, $E04C, $E04D, $E04E, $E04F,
      $E050, $E051, $E052, $E053, $E054, $E055, $E056, $E057,
      $E058, $E059, $E05A, $E05B, $E05C, $E05D, $E05E, $E05F,
      $E060, $E061, $E062, $E063, $E064, $E065, $E066, $E067,
      $E068, $E069, $E06A, $E06B, $E06C, $E06D, $E06E, $E06F,
      $E070, $E071, $E072, $E073, $E074, $E075, $E076, $E077,
      $E078, $E079, $E07A, $E07B, $E07C, $E07D, $E07E, $E07F,
      $E080, $E081, $E082, $E083, $E084, $E085, $E086, $E087,
      $E088, $E089, $E08A, $E08B, $E08C, $E08D, $E08E, $E08F,
      $E090, $E091, $E092, $E093, $E094, $E095, $E096, $E097,
      $E098, $E099, $E09A, $E09B, $E09C, $E09D, $E09E, $E09F,
      $E0A0, $E0A1, $E0A2, $E0A3, $E0A4, $E0A5, $E0A6, $E0A7,
      $E0A8, $E0A9, $E0AA, $E0AB, $E0AC, $E0AD, $E0AE, $E0AF,
      $E0B0, $E0B1, $E0B2, $E0B3, $E0B4, $E0B5, $E0B6, $E0B7,
      $E0B8, $E0B9, $E0BA, $E0BB, $E0BC, $E0BD, $E0BE, $E0BF,
      $E0C0, $E0C1, $E0C2, $E0C3, $E0C4, $E0C5, $E0C6, $E0C7,
      $E0C8, $E0C9, $E0CA, $E0CB, $E0CC, $E0CD, $E0CE, $E0CF,
      $E0D0, $E0D1, $E0D2, $E0D3, $E0D4, $E0D5, $E0D6, $E0D7,
      $E0D8, $E0D9, $E0DA, $E0DB, $E0DC, $E0DD, $E0DE, $E0DF,
      $E0E0, $E0E1, $E0E2, $E0E3, $E0E4, $E0E5, $E0E6, $E0E7,
      $E0E8, $E0E9, $E0EA, $E0EB, $E0EC, $E0ED, $E0EE, $E0EF,
      $E0F0, $E0F1, $E0F2, $E0F3, $E0F4, $E0F5, $E0F6, $E0F7,
      $E0F8, $E0F9, $E0FA, $E0FB, $E0FC, $E0FD, $E0FE, $E0FF );

  // UVGA16 is the entire Unicode VGA 8x16 bitmap font.
  // the first two bytes are the unicode code point followed
  // by 16 bytes for the bitmap image. Glyphs are located using
  // a binary serch (GetGlyphOff).
  // UVGA16 font - 18 bytes per glyph }
  UVGA16_COUNT = 2910;  // number of glyphs
  UVGA16 : packed array [0..UVGA16_COUNT * 18 - 1] of byte = (
    $00, $00, {|}  $00, $00, $DA, $02, $80, $82, $02, $80, $82, $02, $80, $B6, $00, $00, $00, $00,  // char 0
    $00, $20, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 32
    $00, $21, {|}  $00, $00, $18, $3C, $3C, $3C, $18, $18, $18, $00, $18, $18, $00, $00, $00, $00,  // char 33
    $00, $22, {|}  $00, $66, $66, $66, $24, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 34
    $00, $23, {|}  $00, $00, $00, $6C, $6C, $FE, $6C, $6C, $6C, $FE, $6C, $6C, $00, $00, $00, $00,  // char 35
    $00, $24, {|}  $18, $18, $7C, $C6, $C2, $C0, $7C, $06, $06, $86, $C6, $7C, $18, $18, $00, $00,  // char 36
    $00, $25, {|}  $00, $00, $00, $00, $C2, $C6, $0C, $18, $30, $60, $C6, $86, $00, $00, $00, $00,  // char 37
    $00, $26, {|}  $00, $00, $38, $6C, $6C, $38, $76, $DC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 38
    $00, $27, {|}  $00, $30, $30, $30, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 39
    $00, $28, {|}  $00, $00, $0C, $18, $30, $30, $30, $30, $30, $30, $18, $0C, $00, $00, $00, $00,  // char 40
    $00, $29, {|}  $00, $00, $30, $18, $0C, $0C, $0C, $0C, $0C, $0C, $18, $30, $00, $00, $00, $00,  // char 41
    $00, $2A, {|}  $00, $00, $00, $00, $00, $66, $3C, $FF, $3C, $66, $00, $00, $00, $00, $00, $00,  // char 42
    $00, $2B, {|}  $00, $00, $00, $00, $00, $18, $18, $7E, $18, $18, $00, $00, $00, $00, $00, $00,  // char 43
    $00, $2C, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $30, $00, $00, $00,  // char 44
    $00, $2D, {|}  $00, $00, $00, $00, $00, $00, $00, $FE, $00, $00, $00, $00, $00, $00, $00, $00,  // char 45
    $00, $2E, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $00, $00, $00, $00,  // char 46
    $00, $2F, {|}  $00, $00, $00, $00, $02, $06, $0C, $18, $30, $60, $C0, $80, $00, $00, $00, $00,  // char 47
    $00, $30, {|}  $00, $00, $38, $6C, $C6, $C6, $D6, $D6, $C6, $C6, $6C, $38, $00, $00, $00, $00,  // char 48
    $00, $31, {|}  $00, $00, $18, $38, $78, $18, $18, $18, $18, $18, $18, $7E, $00, $00, $00, $00,  // char 49
    $00, $32, {|}  $00, $00, $7C, $C6, $06, $0C, $18, $30, $60, $C0, $C6, $FE, $00, $00, $00, $00,  // char 50
    $00, $33, {|}  $00, $00, $7C, $C6, $06, $06, $3C, $06, $06, $06, $C6, $7C, $00, $00, $00, $00,  // char 51
    $00, $34, {|}  $00, $00, $0C, $1C, $3C, $6C, $CC, $FE, $0C, $0C, $0C, $1E, $00, $00, $00, $00,  // char 52
    $00, $35, {|}  $00, $00, $FE, $C0, $C0, $C0, $FC, $06, $06, $06, $C6, $7C, $00, $00, $00, $00,  // char 53
    $00, $36, {|}  $00, $00, $38, $60, $C0, $C0, $FC, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 54
    $00, $37, {|}  $00, $00, $FE, $C6, $06, $06, $0C, $18, $30, $30, $30, $30, $00, $00, $00, $00,  // char 55
    $00, $38, {|}  $00, $00, $7C, $C6, $C6, $C6, $7C, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 56
    $00, $39, {|}  $00, $00, $7C, $C6, $C6, $C6, $7E, $06, $06, $06, $0C, $78, $00, $00, $00, $00,  // char 57
    $00, $3A, {|}  $00, $00, $00, $00, $18, $18, $00, $00, $00, $18, $18, $00, $00, $00, $00, $00,  // char 58
    $00, $3B, {|}  $00, $00, $00, $00, $18, $18, $00, $00, $00, $18, $18, $30, $00, $00, $00, $00,  // char 59
    $00, $3C, {|}  $00, $00, $00, $06, $0C, $18, $30, $60, $30, $18, $0C, $06, $00, $00, $00, $00,  // char 60
    $00, $3D, {|}  $00, $00, $00, $00, $00, $7E, $00, $00, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 61
    $00, $3E, {|}  $00, $00, $00, $60, $30, $18, $0C, $06, $0C, $18, $30, $60, $00, $00, $00, $00,  // char 62
    $00, $3F, {|}  $00, $00, $7C, $C6, $C6, $0C, $18, $18, $18, $00, $18, $18, $00, $00, $00, $00,  // char 63
    $00, $40, {|}  $00, $00, $00, $7C, $C6, $C6, $DE, $DE, $DE, $DC, $C0, $7C, $00, $00, $00, $00,  // char 64
    $00, $41, {|}  $00, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 65
    $00, $42, {|}  $00, $00, $FC, $66, $66, $66, $7C, $66, $66, $66, $66, $FC, $00, $00, $00, $00,  // char 66
    $00, $43, {|}  $00, $00, $3C, $66, $C2, $C0, $C0, $C0, $C0, $C2, $66, $3C, $00, $00, $00, $00,  // char 67
    $00, $44, {|}  $00, $00, $F8, $6C, $66, $66, $66, $66, $66, $66, $6C, $F8, $00, $00, $00, $00,  // char 68
    $00, $45, {|}  $00, $00, $FE, $66, $62, $68, $78, $68, $60, $62, $66, $FE, $00, $00, $00, $00,  // char 69
    $00, $46, {|}  $00, $00, $FE, $66, $62, $68, $78, $68, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 70
    $00, $47, {|}  $00, $00, $3C, $66, $C2, $C0, $C0, $DE, $C6, $C6, $66, $3A, $00, $00, $00, $00,  // char 71
    $00, $48, {|}  $00, $00, $C6, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 72
    $00, $49, {|}  $00, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 73
    $00, $4A, {|}  $00, $00, $1E, $0C, $0C, $0C, $0C, $0C, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 74
    $00, $4B, {|}  $00, $00, $E6, $66, $66, $6C, $78, $78, $6C, $66, $66, $E6, $00, $00, $00, $00,  // char 75
    $00, $4C, {|}  $00, $00, $F0, $60, $60, $60, $60, $60, $60, $62, $66, $FE, $00, $00, $00, $00,  // char 76
    $00, $4D, {|}  $00, $00, $C6, $EE, $FE, $FE, $D6, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 77
    $00, $4E, {|}  $00, $00, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 78
    $00, $4F, {|}  $00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 79
    $00, $50, {|}  $00, $00, $FC, $66, $66, $66, $7C, $60, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 80
    $00, $51, {|}  $00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $D6, $DE, $7C, $0C, $0E, $00, $00,  // char 81
    $00, $52, {|}  $00, $00, $FC, $66, $66, $66, $7C, $6C, $66, $66, $66, $E6, $00, $00, $00, $00,  // char 82
    $00, $53, {|}  $00, $00, $7C, $C6, $C6, $60, $38, $0C, $06, $C6, $C6, $7C, $00, $00, $00, $00,  // char 83
    $00, $54, {|}  $00, $00, $7E, $7E, $5A, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 84
    $00, $55, {|}  $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 85
    $00, $56, {|}  $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $6C, $38, $10, $00, $00, $00, $00,  // char 86
    $00, $57, {|}  $00, $00, $C6, $C6, $C6, $C6, $D6, $D6, $D6, $FE, $EE, $6C, $00, $00, $00, $00,  // char 87
    $00, $58, {|}  $00, $00, $C6, $C6, $6C, $7C, $38, $38, $7C, $6C, $C6, $C6, $00, $00, $00, $00,  // char 88
    $00, $59, {|}  $00, $00, $66, $66, $66, $66, $3C, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 89
    $00, $5A, {|}  $00, $00, $FE, $C6, $86, $0C, $18, $30, $60, $C2, $C6, $FE, $00, $00, $00, $00,  // char 90
    $00, $5B, {|}  $00, $00, $3C, $30, $30, $30, $30, $30, $30, $30, $30, $3C, $00, $00, $00, $00,  // char 91
    $00, $5C, {|}  $00, $00, $00, $80, $C0, $E0, $70, $38, $1C, $0E, $06, $02, $00, $00, $00, $00,  // char 92
    $00, $5D, {|}  $00, $00, $3C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $3C, $00, $00, $00, $00,  // char 93
    $00, $5E, {|}  $10, $38, $6C, $C6, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 94
    $00, $5F, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00,  // char 95
    $00, $60, {|}  $30, $30, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 96
    $00, $61, {|}  $00, $00, $00, $00, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 97
    $00, $62, {|}  $00, $00, $E0, $60, $60, $78, $6C, $66, $66, $66, $66, $7C, $00, $00, $00, $00,  // char 98
    $00, $63, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C0, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 99
    $00, $64, {|}  $00, $00, $1C, $0C, $0C, $3C, $6C, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 100
    $00, $65, {|}  $00, $00, $00, $00, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 101
    $00, $66, {|}  $00, $00, $38, $6C, $64, $60, $F0, $60, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 102
    $00, $67, {|}  $00, $00, $00, $00, $00, $76, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $CC, $78, $00,  // char 103
    $00, $68, {|}  $00, $00, $E0, $60, $60, $6C, $76, $66, $66, $66, $66, $E6, $00, $00, $00, $00,  // char 104
    $00, $69, {|}  $00, $00, $18, $18, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 105
    $00, $6A, {|}  $00, $00, $06, $06, $00, $0E, $06, $06, $06, $06, $06, $06, $66, $66, $3C, $00,  // char 106
    $00, $6B, {|}  $00, $00, $E0, $60, $60, $66, $6C, $78, $78, $6C, $66, $E6, $00, $00, $00, $00,  // char 107
    $00, $6C, {|}  $00, $00, $38, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 108
    $00, $6D, {|}  $00, $00, $00, $00, $00, $EC, $FE, $D6, $D6, $D6, $D6, $C6, $00, $00, $00, $00,  // char 109
    $00, $6E, {|}  $00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $66, $00, $00, $00, $00,  // char 110
    $00, $6F, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 111
    $00, $70, {|}  $00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $7C, $60, $60, $F0, $00,  // char 112
    $00, $71, {|}  $00, $00, $00, $00, $00, $76, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $0C, $1E, $00,  // char 113
    $00, $72, {|}  $00, $00, $00, $00, $00, $DC, $76, $66, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 114
    $00, $73, {|}  $00, $00, $00, $00, $00, $7C, $C6, $60, $38, $0C, $C6, $7C, $00, $00, $00, $00,  // char 115
    $00, $74, {|}  $00, $00, $10, $30, $30, $FC, $30, $30, $30, $30, $36, $1C, $00, $00, $00, $00,  // char 116
    $00, $75, {|}  $00, $00, $00, $00, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 117
    $00, $76, {|}  $00, $00, $00, $00, $00, $66, $66, $66, $66, $66, $3C, $18, $00, $00, $00, $00,  // char 118
    $00, $77, {|}  $00, $00, $00, $00, $00, $C6, $C6, $D6, $D6, $D6, $FE, $6C, $00, $00, $00, $00,  // char 119
    $00, $78, {|}  $00, $00, $00, $00, $00, $C6, $6C, $38, $38, $38, $6C, $C6, $00, $00, $00, $00,  // char 120
    $00, $79, {|}  $00, $00, $00, $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00,  // char 121
    $00, $7A, {|}  $00, $00, $00, $00, $00, $FE, $CC, $18, $30, $60, $C6, $FE, $00, $00, $00, $00,  // char 122
    $00, $7B, {|}  $00, $00, $0E, $18, $18, $18, $70, $18, $18, $18, $18, $0E, $00, $00, $00, $00,  // char 123
    $00, $7C, {|}  $00, $00, $18, $18, $18, $18, $00, $18, $18, $18, $18, $18, $00, $00, $00, $00,  // char 124
    $00, $7D, {|}  $00, $00, $70, $18, $18, $18, $0E, $18, $18, $18, $18, $70, $00, $00, $00, $00,  // char 125
    $00, $7E, {|}  $00, $00, $76, $DC, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 126
    $00, $7F, {|}  $00, $00, $00, $00, $10, $38, $6C, $C6, $C6, $C6, $FE, $00, $00, $00, $00, $00,  // char 127
    $00, $A0, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 160
    $00, $A1, {|}  $00, $00, $18, $18, $00, $18, $18, $18, $3C, $3C, $3C, $18, $00, $00, $00, $00,  // char 161
    $00, $A2, {|}  $00, $18, $18, $3C, $66, $60, $60, $60, $66, $3C, $18, $18, $00, $00, $00, $00,  // char 162
    $00, $A3, {|}  $00, $38, $6C, $64, $60, $F0, $60, $60, $60, $60, $E6, $FC, $00, $00, $00, $00,  // char 163
    $00, $A4, {|}  $00, $00, $00, $00, $66, $3C, $66, $66, $66, $3C, $66, $00, $00, $00, $00, $00,  // char 164
    $00, $A5, {|}  $00, $00, $66, $66, $3C, $18, $7E, $18, $7E, $18, $18, $18, $00, $00, $00, $00,  // char 165
    $00, $A6, {|}  $00, $00, $18, $18, $18, $18, $00, $18, $18, $18, $18, $18, $00, $00, $00, $00,  // char 166
    $00, $A7, {|}  $00, $7C, $C6, $60, $38, $6C, $C6, $C6, $6C, $38, $0C, $C6, $7C, $00, $00, $00,  // char 167
    $00, $A8, {|}  $00, $00, $6C, $6C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 168
    $00, $A9, {|}  $00, $00, $3C, $42, $99, $A5, $A1, $A1, $A5, $99, $42, $3C, $00, $00, $00, $00,  // char 169
    $00, $AA, {|}  $00, $3C, $6C, $6C, $3E, $00, $7E, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 170
    $00, $AB, {|}  $00, $00, $00, $00, $00, $36, $6C, $D8, $6C, $36, $00, $00, $00, $00, $00, $00,  // char 171
    $00, $AC, {|}  $00, $00, $00, $00, $00, $00, $FE, $06, $06, $06, $06, $00, $00, $00, $00, $00,  // char 172
    $00, $AD, {|}  $00, $00, $00, $00, $00, $00, $00, $3C, $00, $00, $00, $00, $00, $00, $00, $00,  // char 173
    $00, $AE, {|}  $00, $00, $38, $44, $BA, $B2, $AA, $44, $38, $00, $00, $00, $00, $00, $00, $00,  // char 174
    $00, $AF, {|}  $00, $00, $00, $7C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 175
    $00, $B0, {|}  $00, $38, $6C, $6C, $38, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 176
    $00, $B1, {|}  $00, $00, $00, $00, $18, $18, $7E, $18, $18, $00, $7E, $00, $00, $00, $00, $00,  // char 177
    $00, $B2, {|}  $00, $70, $D8, $30, $60, $C8, $F8, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 178
    $00, $B3, {|}  $00, $70, $D8, $30, $18, $D8, $70, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 179
    $00, $B4, {|}  $00, $00, $0C, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 180
    $00, $B5, {|}  $00, $00, $00, $00, $00, $CC, $CC, $CC, $CC, $CC, $CC, $F6, $C0, $C0, $C0, $00,  // char 181
    $00, $B6, {|}  $00, $00, $7F, $DB, $DB, $DB, $7B, $1B, $1B, $1B, $1B, $1B, $00, $00, $00, $00,  // char 182
    $00, $B7, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $18, $00, $00, $00, $00, $00, $00, $00,  // char 183
    $00, $B8, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $0C, $38, $00,  // char 184
    $00, $B9, {|}  $00, $30, $70, $30, $30, $30, $78, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 185
    $00, $BA, {|}  $00, $38, $6C, $6C, $38, $00, $7C, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 186
    $00, $BB, {|}  $00, $00, $00, $00, $00, $D8, $6C, $36, $6C, $D8, $00, $00, $00, $00, $00, $00,  // char 187
    $00, $BC, {|}  $00, $C0, $C0, $C2, $C6, $CC, $18, $30, $66, $CE, $9E, $3E, $06, $06, $00, $00,  // char 188
    $00, $BD, {|}  $00, $C0, $C0, $C2, $C6, $CC, $18, $30, $60, $DC, $86, $0C, $18, $3E, $00, $00,  // char 189
    $00, $BE, {|}  $00, $E0, $30, $62, $36, $EC, $18, $30, $66, $CE, $9E, $3E, $06, $06, $00, $00,  // char 190
    $00, $BF, {|}  $00, $00, $30, $30, $00, $30, $30, $60, $C0, $C6, $C6, $7C, $00, $00, $00, $00,  // char 191
    $00, $C0, {|}  $60, $30, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 192
    $00, $C1, {|}  $0C, $18, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 193
    $00, $C2, {|}  $10, $38, $6C, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 194
    $00, $C3, {|}  $76, $DC, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 195
    $00, $C4, {|}  $6C, $6C, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 196
    $00, $C5, {|}  $38, $6C, $38, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 197
    $00, $C6, {|}  $00, $00, $3E, $6C, $CC, $CC, $FE, $CC, $CC, $CC, $CC, $CE, $00, $00, $00, $00,  // char 198
    $00, $C7, {|}  $00, $00, $3C, $66, $C2, $C0, $C0, $C0, $C0, $C2, $66, $3C, $18, $0C, $38, $00,  // char 199
    $00, $C8, {|}  $30, $18, $00, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $00, $00, $00, $00,  // char 200
    $00, $C9, {|}  $0C, $18, $00, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $00, $00, $00, $00,  // char 201
    $00, $CA, {|}  $10, $38, $44, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $00, $00, $00, $00,  // char 202
    $00, $CB, {|}  $6C, $6C, $00, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $00, $00, $00, $00,  // char 203
    $00, $CC, {|}  $30, $18, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 204
    $00, $CD, {|}  $0C, $18, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 205
    $00, $CE, {|}  $18, $3C, $42, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 206
    $00, $CF, {|}  $66, $66, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 207
    $00, $D0, {|}  $00, $00, $F8, $6C, $66, $66, $F6, $66, $66, $66, $6C, $F8, $00, $00, $00, $00,  // char 208
    $00, $D1, {|}  $76, $DC, $00, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 209
    $00, $D2, {|}  $60, $30, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 210
    $00, $D3, {|}  $0C, $18, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 211
    $00, $D4, {|}  $10, $38, $44, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 212
    $00, $D5, {|}  $76, $DC, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 213
    $00, $D6, {|}  $6C, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 214
    $00, $D7, {|}  $00, $00, $00, $00, $00, $66, $3C, $18, $3C, $66, $00, $00, $00, $00, $00, $00,  // char 215
    $00, $D8, {|}  $00, $00, $7A, $C4, $CE, $CE, $D6, $D6, $E6, $E6, $46, $BC, $00, $00, $00, $00,  // char 216
    $00, $D9, {|}  $60, $30, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 217
    $00, $DA, {|}  $0C, $18, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 218
    $00, $DB, {|}  $10, $38, $44, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 219
    $00, $DC, {|}  $6C, $6C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 220
    $00, $DD, {|}  $0C, $18, $00, $66, $66, $66, $3C, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 221
    $00, $DE, {|}  $00, $00, $F0, $60, $7C, $66, $66, $66, $66, $7C, $60, $F0, $00, $00, $00, $00,  // char 222
    $00, $DF, {|}  $00, $00, $3C, $66, $66, $66, $6C, $66, $66, $66, $66, $EC, $00, $00, $00, $00,  // char 223
    $00, $E0, {|}  $00, $00, $60, $30, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 224
    $00, $E1, {|}  $00, $00, $18, $30, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 225
    $00, $E2, {|}  $00, $10, $38, $6C, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 226
    $00, $E3, {|}  $00, $00, $76, $DC, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 227
    $00, $E4, {|}  $00, $00, $6C, $6C, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 228
    $00, $E5, {|}  $00, $38, $6C, $38, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 229
    $00, $E6, {|}  $00, $00, $00, $00, $00, $CC, $76, $36, $7E, $D8, $D8, $6E, $00, $00, $00, $00,  // char 230
    $00, $E7, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C0, $C0, $C0, $C6, $7C, $18, $0C, $38, $00,  // char 231
    $00, $E8, {|}  $00, $00, $60, $30, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 232
    $00, $E9, {|}  $00, $00, $0C, $18, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 233
    $00, $EA, {|}  $00, $10, $38, $6C, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 234
    $00, $EB, {|}  $00, $00, $6C, $6C, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 235
    $00, $EC, {|}  $00, $00, $30, $18, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 236
    $00, $ED, {|}  $00, $00, $0C, $18, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 237
    $00, $EE, {|}  $00, $10, $38, $6C, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 238
    $00, $EF, {|}  $00, $00, $66, $66, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 239
    $00, $F0, {|}  $00, $00, $76, $1C, $3C, $06, $7E, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 240
    $00, $F1, {|}  $00, $00, $76, $DC, $00, $DC, $66, $66, $66, $66, $66, $66, $00, $00, $00, $00,  // char 241
    $00, $F2, {|}  $00, $00, $60, $30, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 242
    $00, $F3, {|}  $00, $00, $0C, $18, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 243
    $00, $F4, {|}  $00, $10, $38, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 244
    $00, $F5, {|}  $00, $00, $76, $DC, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 245
    $00, $F6, {|}  $00, $00, $6C, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 246
    $00, $F7, {|}  $00, $00, $00, $00, $18, $18, $00, $7E, $00, $18, $18, $00, $00, $00, $00, $00,  // char 247
    $00, $F8, {|}  $00, $00, $00, $00, $00, $7A, $C4, $CE, $D6, $E6, $46, $BC, $00, $00, $00, $00,  // char 248
    $00, $F9, {|}  $00, $00, $60, $30, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 249
    $00, $FA, {|}  $00, $00, $18, $30, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 250
    $00, $FB, {|}  $00, $10, $38, $6C, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 251
    $00, $FC, {|}  $00, $00, $CC, $CC, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 252
    $00, $FD, {|}  $00, $00, $0C, $18, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00,  // char 253
    $00, $FE, {|}  $00, $00, $E0, $60, $60, $7C, $66, $66, $66, $66, $66, $7C, $60, $60, $F0, $00,  // char 254
    $00, $FF, {|}  $00, $00, $6C, $6C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00,  // char 255
    $01, $00, {|}  $00, $7C, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 256
    $01, $01, {|}  $00, $00, $00, $7C, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 257
    $01, $02, {|}  $6C, $38, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 258
    $01, $03, {|}  $00, $00, $6C, $38, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 259
    $01, $04, {|}  $00, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $0C, $18, $0E, $00,  // char 260
    $01, $05, {|}  $00, $00, $00, $00, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $0C, $18, $0E, $00,  // char 261
    $01, $06, {|}  $0C, $18, $00, $3C, $66, $C2, $C0, $C0, $C0, $C2, $66, $3C, $00, $00, $00, $00,  // char 262
    $01, $07, {|}  $00, $00, $0C, $18, $00, $7C, $C6, $C0, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 263
    $01, $08, {|}  $10, $38, $44, $3C, $66, $C2, $C0, $C0, $C0, $C2, $66, $3C, $00, $00, $00, $00,  // char 264
    $01, $09, {|}  $00, $10, $38, $6C, $00, $7C, $C6, $C0, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 265
    $01, $0A, {|}  $18, $18, $00, $3C, $66, $C2, $C0, $C0, $C0, $C2, $66, $3C, $00, $00, $00, $00,  // char 266
    $01, $0B, {|}  $00, $00, $30, $30, $00, $7C, $C6, $C0, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 267
    $01, $0C, {|}  $6C, $38, $10, $3C, $66, $C2, $C0, $C0, $C0, $C2, $66, $3C, $00, $00, $00, $00,  // char 268
    $01, $0D, {|}  $00, $6C, $38, $10, $00, $7C, $C6, $C0, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 269
    $01, $0E, {|}  $6C, $38, $10, $F8, $6C, $66, $66, $66, $66, $66, $6C, $F8, $00, $00, $00, $00,  // char 270
    $01, $0F, {|}  $6C, $38, $10, $0C, $0C, $3C, $6C, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 271
    $01, $10, {|}  $00, $00, $F8, $6C, $66, $66, $F6, $66, $66, $66, $6C, $F8, $00, $00, $00, $00,  // char 272
    $01, $11, {|}  $00, $00, $0C, $3E, $0C, $3C, $6C, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 273
    $01, $12, {|}  $00, $7C, $00, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $00, $00, $00, $00,  // char 274
    $01, $13, {|}  $00, $00, $00, $7C, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 275
    $01, $14, {|}  $6C, $38, $00, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $00, $00, $00, $00,  // char 276
    $01, $15, {|}  $00, $00, $6C, $38, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 277
    $01, $16, {|}  $18, $18, $00, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $00, $00, $00, $00,  // char 278
    $01, $17, {|}  $00, $00, $30, $30, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 279
    $01, $18, {|}  $00, $00, $00, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $18, $30, $1C, $00,  // char 280
    $01, $19, {|}  $00, $00, $00, $00, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $30, $60, $38, $00,  // char 281
    $01, $1A, {|}  $6C, $38, $10, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $00, $00, $00, $00,  // char 282
    $01, $1B, {|}  $00, $6C, $38, $10, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 283
    $01, $1C, {|}  $10, $38, $44, $3C, $66, $C2, $C0, $DE, $C6, $C6, $66, $3A, $00, $00, $00, $00,  // char 284
    $01, $1D, {|}  $00, $10, $38, $6C, $00, $76, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $CC, $78, $00,  // char 285
    $01, $1E, {|}  $6C, $38, $00, $3C, $66, $C2, $C0, $DE, $C6, $C6, $66, $3A, $00, $00, $00, $00,  // char 286
    $01, $1F, {|}  $00, $00, $6C, $38, $00, $76, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $CC, $78, $00,  // char 287
    $01, $20, {|}  $18, $18, $00, $3C, $66, $C2, $C0, $DE, $C6, $C6, $66, $3A, $00, $00, $00, $00,  // char 288
    $01, $21, {|}  $00, $00, $30, $30, $00, $76, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $CC, $78, $00,  // char 289
    $01, $22, {|}  $00, $00, $3C, $66, $C2, $C0, $C0, $DE, $C6, $C6, $66, $3A, $00, $18, $18, $30,  // char 290
    $01, $23, {|}  $00, $18, $30, $30, $00, $76, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $CC, $78, $00,  // char 291
    $01, $24, {|}  $10, $38, $00, $C6, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 292
    $01, $25, {|}  $10, $38, $44, $E0, $60, $6C, $76, $66, $66, $66, $66, $E6, $00, $00, $00, $00,  // char 293
    $01, $26, {|}  $00, $00, $66, $FF, $66, $66, $7E, $66, $66, $66, $66, $66, $00, $00, $00, $00,  // char 294
    $01, $27, {|}  $00, $00, $60, $F8, $60, $6C, $76, $66, $66, $66, $66, $E6, $00, $00, $00, $00,  // char 295
    $01, $28, {|}  $76, $DC, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 296
    $01, $29, {|}  $00, $00, $76, $DC, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 297
    $01, $2A, {|}  $00, $7E, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 298
    $01, $2B, {|}  $00, $00, $00, $7E, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 299
    $01, $2C, {|}  $66, $3C, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 300
    $01, $2D, {|}  $00, $00, $66, $3C, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 301
    $01, $2E, {|}  $00, $00, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $18, $30, $1C, $00,  // char 302
    $01, $2F, {|}  $00, $00, $18, $18, $00, $38, $18, $18, $18, $18, $18, $3C, $18, $30, $1C, $00,  // char 303
    $01, $30, {|}  $18, $18, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 304
    $01, $31, {|}  $00, $00, $00, $00, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 305
    $01, $32, {|}  $00, $00, $F7, $63, $63, $63, $63, $63, $63, $7B, $7B, $EE, $00, $00, $00, $00,  // char 306
    $01, $33, {|}  $00, $00, $66, $66, $00, $EE, $66, $66, $66, $66, $66, $F6, $06, $66, $3C, $00,  // char 307
    $01, $34, {|}  $08, $1C, $22, $1E, $0C, $0C, $0C, $0C, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 308
    $01, $35, {|}  $00, $04, $0E, $1B, $00, $0E, $06, $06, $06, $06, $06, $06, $66, $66, $3C, $00,  // char 309
    $01, $36, {|}  $00, $00, $E6, $66, $66, $6C, $78, $78, $6C, $66, $66, $E6, $00, $18, $18, $30,  // char 310
    $01, $37, {|}  $00, $00, $E0, $60, $60, $66, $6C, $78, $78, $6C, $66, $E6, $00, $18, $18, $30,  // char 311
    $01, $38, {|}  $00, $00, $00, $00, $00, $E6, $6C, $78, $78, $6C, $66, $E6, $00, $00, $00, $00,  // char 312
    $01, $39, {|}  $18, $30, $00, $F0, $60, $60, $60, $60, $60, $62, $66, $FE, $00, $00, $00, $00,  // char 313
    $01, $3A, {|}  $0C, $18, $00, $38, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 314
    $01, $3B, {|}  $00, $00, $F0, $60, $60, $60, $60, $60, $60, $62, $66, $FE, $00, $18, $18, $30,  // char 315
    $01, $3C, {|}  $00, $00, $38, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $18, $18, $30,  // char 316
    $01, $3D, {|}  $6C, $38, $10, $F0, $60, $60, $60, $60, $60, $62, $66, $FE, $00, $00, $00, $00,  // char 317
    $01, $3E, {|}  $6C, $38, $10, $38, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 318
    $01, $3F, {|}  $00, $00, $F0, $60, $60, $60, $66, $66, $60, $62, $66, $FE, $00, $00, $00, $00,  // char 319
    $01, $40, {|}  $00, $00, $70, $30, $30, $30, $36, $36, $30, $30, $30, $78, $00, $00, $00, $00,  // char 320
    $01, $41, {|}  $00, $00, $F0, $60, $60, $60, $78, $E0, $60, $62, $66, $FE, $00, $00, $00, $00,  // char 321
    $01, $42, {|}  $00, $00, $38, $18, $18, $18, $1E, $78, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 322
    $01, $43, {|}  $0C, $18, $00, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 323
    $01, $44, {|}  $00, $00, $0C, $18, $00, $DC, $66, $66, $66, $66, $66, $66, $00, $00, $00, $00,  // char 324
    $01, $45, {|}  $00, $00, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $C6, $C6, $C6, $00, $18, $18, $30,  // char 325
    $01, $46, {|}  $00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $66, $00, $18, $18, $30,  // char 326
    $01, $47, {|}  $6C, $38, $10, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 327
    $01, $48, {|}  $00, $6C, $38, $10, $00, $DC, $66, $66, $66, $66, $66, $66, $00, $00, $00, $00,  // char 328
    $01, $49, {|}  $00, $60, $60, $C0, $00, $DC, $66, $66, $66, $66, $66, $66, $00, $00, $00, $00,  // char 329
    $01, $4A, {|}  $00, $00, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $C6, $C6, $C6, $06, $06, $1C, $00,  // char 330
    $01, $4B, {|}  $00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $66, $06, $06, $1C, $00,  // char 331
    $01, $4C, {|}  $00, $7C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 332
    $01, $4D, {|}  $00, $00, $00, $7C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 333
    $01, $4E, {|}  $6C, $38, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 334
    $01, $4F, {|}  $00, $00, $6C, $38, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 335
    $01, $50, {|}  $66, $CC, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 336
    $01, $51, {|}  $00, $00, $66, $CC, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 337
    $01, $52, {|}  $00, $00, $6E, $D8, $D8, $D8, $DE, $D8, $D8, $D8, $D8, $6E, $00, $00, $00, $00,  // char 338
    $01, $53, {|}  $00, $00, $00, $00, $00, $6C, $D6, $D6, $DE, $D8, $D8, $6E, $00, $00, $00, $00,  // char 339
    $01, $54, {|}  $0C, $18, $00, $FC, $66, $66, $66, $7C, $6C, $66, $66, $E6, $00, $00, $00, $00,  // char 340
    $01, $55, {|}  $00, $00, $0C, $18, $00, $DC, $76, $66, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 341
    $01, $56, {|}  $00, $00, $FC, $66, $66, $66, $7C, $6C, $66, $66, $66, $E6, $00, $18, $18, $30,  // char 342
    $01, $57, {|}  $00, $00, $00, $00, $00, $DC, $76, $66, $60, $60, $60, $F0, $00, $18, $18, $30,  // char 343
    $01, $58, {|}  $6C, $38, $10, $FC, $66, $66, $66, $7C, $6C, $66, $66, $E6, $00, $00, $00, $00,  // char 344
    $01, $59, {|}  $00, $6C, $38, $10, $00, $DC, $76, $66, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 345
    $01, $5A, {|}  $0C, $18, $00, $7C, $C6, $C6, $60, $38, $0C, $C6, $C6, $7C, $00, $00, $00, $00,  // char 346
    $01, $5B, {|}  $00, $00, $0C, $18, $00, $7C, $C6, $60, $38, $0C, $C6, $7C, $00, $00, $00, $00,  // char 347
    $01, $5C, {|}  $10, $38, $44, $7C, $C6, $C6, $60, $38, $0C, $C6, $C6, $7C, $00, $00, $00, $00,  // char 348
    $01, $5D, {|}  $00, $10, $38, $6C, $00, $7C, $C6, $60, $38, $0C, $C6, $7C, $00, $00, $00, $00,  // char 349
    $01, $5E, {|}  $00, $00, $00, $7C, $C6, $C6, $60, $38, $0C, $C6, $C6, $7C, $18, $0C, $38, $00,  // char 350
    $01, $5F, {|}  $00, $00, $00, $00, $00, $7C, $C6, $60, $38, $0C, $C6, $7C, $18, $0C, $38, $00,  // char 351
    $01, $60, {|}  $6C, $38, $10, $7C, $C6, $C6, $60, $38, $0C, $C6, $C6, $7C, $00, $00, $00, $00,  // char 352
    $01, $61, {|}  $00, $6C, $38, $10, $00, $7C, $C6, $60, $38, $0C, $C6, $7C, $00, $00, $00, $00,  // char 353
    $01, $62, {|}  $00, $00, $7E, $7E, $5A, $18, $18, $18, $18, $18, $18, $3C, $18, $0C, $38, $00,  // char 354
    $01, $63, {|}  $00, $00, $10, $30, $30, $FC, $30, $30, $30, $30, $36, $1C, $18, $0C, $38, $00,  // char 355
    $01, $64, {|}  $6C, $38, $10, $7E, $7E, $5A, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 356
    $01, $65, {|}  $6C, $38, $10, $10, $30, $FC, $30, $30, $30, $30, $36, $1C, $00, $00, $00, $00,  // char 357
    $01, $66, {|}  $00, $00, $7E, $7E, $5A, $18, $18, $3C, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 358
    $01, $67, {|}  $00, $00, $10, $30, $30, $FC, $30, $FC, $30, $30, $36, $1C, $00, $00, $00, $00,  // char 359
    $01, $68, {|}  $76, $DC, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 360
    $01, $69, {|}  $00, $00, $76, $DC, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 361
    $01, $6A, {|}  $00, $7C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 362
    $01, $6B, {|}  $00, $00, $00, $78, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 363
    $01, $6C, {|}  $6C, $38, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 364
    $01, $6D, {|}  $00, $00, $CC, $78, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 365
    $01, $6E, {|}  $38, $6C, $38, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 366
    $01, $6F, {|}  $00, $38, $6C, $38, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 367
    $01, $70, {|}  $66, $CC, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 368
    $01, $71, {|}  $00, $00, $66, $CC, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 369
    $01, $72, {|}  $00, $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $30, $60, $38, $00,  // char 370
    $01, $73, {|}  $00, $00, $00, $00, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $30, $60, $38, $00,  // char 371
    $01, $74, {|}  $10, $38, $44, $C6, $C6, $C6, $D6, $D6, $D6, $FE, $EE, $6C, $00, $00, $00, $00,  // char 372
    $01, $75, {|}  $00, $10, $38, $6C, $00, $C6, $C6, $D6, $D6, $D6, $FE, $6C, $00, $00, $00, $00,  // char 373
    $01, $76, {|}  $10, $38, $44, $66, $66, $66, $3C, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 374
    $01, $77, {|}  $00, $10, $38, $6C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00,  // char 375
    $01, $78, {|}  $66, $66, $00, $66, $66, $66, $3C, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 376
    $01, $79, {|}  $0C, $18, $00, $FE, $C6, $8C, $18, $30, $60, $C2, $C6, $FE, $00, $00, $00, $00,  // char 377
    $01, $7A, {|}  $00, $00, $0C, $18, $00, $FE, $CC, $18, $30, $60, $C6, $FE, $00, $00, $00, $00,  // char 378
    $01, $7B, {|}  $18, $18, $00, $FE, $C6, $8C, $18, $30, $60, $C2, $C6, $FE, $00, $00, $00, $00,  // char 379
    $01, $7C, {|}  $00, $00, $18, $18, $00, $FE, $CC, $18, $30, $60, $C6, $FE, $00, $00, $00, $00,  // char 380
    $01, $7D, {|}  $6C, $38, $10, $FE, $C6, $8C, $18, $30, $60, $C2, $C6, $FE, $00, $00, $00, $00,  // char 381
    $01, $7E, {|}  $00, $6C, $38, $10, $00, $FE, $CC, $18, $30, $60, $C6, $FE, $00, $00, $00, $00,  // char 382
    $01, $7F, {|}  $00, $00, $38, $6C, $64, $60, $60, $60, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 383
    $01, $80, {|}  $00, $00, $60, $F8, $60, $78, $6C, $66, $66, $66, $66, $7C, $00, $00, $00, $00,  // char 384
    $01, $81, {|}  $00, $00, $7E, $B3, $B3, $33, $3E, $33, $33, $33, $33, $7E, $00, $00, $00, $00,  // char 385
    $01, $82, {|}  $00, $00, $FC, $64, $60, $60, $7C, $66, $66, $66, $66, $FC, $00, $00, $00, $00,  // char 386
    $01, $83, {|}  $00, $00, $7E, $62, $60, $78, $6C, $66, $66, $66, $66, $7C, $00, $00, $00, $00,  // char 387
    $01, $84, {|}  $00, $00, $7C, $E6, $E6, $66, $7C, $66, $66, $66, $66, $FC, $00, $00, $00, $00,  // char 388
    $01, $85, {|}  $00, $00, $60, $E0, $E0, $78, $6C, $66, $66, $66, $66, $7C, $00, $00, $00, $00,  // char 389
    $01, $86, {|}  $00, $00, $78, $CC, $86, $06, $06, $06, $06, $86, $CC, $78, $00, $00, $00, $00,  // char 390
    $01, $87, {|}  $00, $03, $3E, $66, $C2, $C0, $C0, $C0, $C0, $C2, $66, $3C, $00, $00, $00, $00,  // char 391
    $01, $88, {|}  $00, $00, $00, $00, $03, $7E, $C6, $C0, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 392
    $01, $89, {|}  $00, $00, $F8, $6C, $66, $66, $F6, $66, $66, $66, $6C, $F8, $00, $00, $00, $00,  // char 393
    $01, $8A, {|}  $00, $00, $7C, $B6, $B3, $33, $33, $33, $33, $33, $36, $7C, $00, $00, $00, $00,  // char 394
    $01, $8B, {|}  $00, $00, $7E, $4C, $0C, $0C, $7C, $CC, $CC, $CC, $CC, $7E, $00, $00, $00, $00,  // char 395
    $01, $8C, {|}  $00, $00, $7C, $4C, $0C, $3C, $6C, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 396
    $01, $8D, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C6, $C6, $C6, $7C, $30, $18, $CC, $78, $00,  // char 397
    $01, $8E, {|}  $00, $00, $FE, $CC, $8C, $2C, $3C, $2C, $0C, $8C, $CC, $FE, $00, $00, $00, $00,  // char 398
    $01, $8F, {|}  $00, $00, $7C, $C6, $06, $06, $06, $FE, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 399
    $01, $90, {|}  $00, $00, $7C, $C6, $C2, $C0, $78, $C0, $C0, $C2, $C6, $7C, $00, $00, $00, $00,  // char 400
    $01, $91, {|}  $00, $00, $FE, $66, $62, $68, $78, $68, $60, $60, $60, $60, $60, $60, $C0, $00,  // char 401
    $01, $92, {|}  $00, $00, $1C, $36, $32, $30, $78, $30, $30, $30, $30, $30, $30, $30, $E0, $00,  // char 402
    $01, $93, {|}  $00, $03, $3E, $66, $C2, $C0, $C0, $DE, $C6, $C6, $66, $3A, $00, $00, $00, $00,  // char 403
    $01, $94, {|}  $00, $00, $C6, $C6, $C6, $6C, $6C, $6C, $38, $38, $10, $38, $6C, $38, $00, $00,  // char 404
    $01, $95, {|}  $00, $00, $C0, $C0, $C0, $F3, $DB, $DB, $DB, $DB, $DB, $CE, $00, $00, $00, $00,  // char 405
    $01, $96, {|}  $00, $00, $70, $30, $30, $30, $30, $30, $30, $30, $36, $1C, $00, $00, $00, $00,  // char 406
    $01, $97, {|}  $00, $00, $3C, $18, $18, $18, $3C, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 407
    $01, $98, {|}  $00, $00, $E6, $6D, $6C, $78, $70, $78, $6C, $66, $66, $E6, $00, $00, $00, $00,  // char 408
    $01, $99, {|}  $00, $00, $38, $6C, $60, $66, $6C, $78, $78, $6C, $66, $E6, $00, $00, $00, $00,  // char 409
    $01, $9A, {|}  $00, $00, $38, $18, $18, $18, $3C, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 410
    $01, $9B, {|}  $00, $00, $C8, $38, $70, $D0, $38, $38, $6C, $64, $C6, $C2, $00, $00, $00, $00,  // char 411
    $01, $9C, {|}  $00, $00, $D6, $D6, $D6, $D6, $D6, $D6, $D6, $D6, $FE, $EC, $00, $00, $00, $00,  // char 412
    $01, $9D, {|}  $00, $00, $66, $66, $76, $7E, $7E, $6E, $66, $66, $66, $66, $60, $60, $C0, $00,  // char 413
    $01, $9E, {|}  $00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $66, $06, $06, $06, $00,  // char 414
    $01, $9F, {|}  $00, $00, $7C, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 415
    $01, $A0, {|}  $03, $03, $7A, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 416
    $01, $A1, {|}  $00, $00, $03, $03, $06, $78, $CC, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 417
    $01, $A2, {|}  $00, $00, $73, $DF, $DB, $DB, $DB, $DB, $DB, $DB, $DB, $73, $03, $03, $03, $00,  // char 418
    $01, $A3, {|}  $00, $00, $00, $00, $00, $73, $DF, $DB, $DB, $DB, $DB, $73, $03, $03, $03, $00,  // char 419
    $01, $A4, {|}  $00, $00, $7E, $B3, $B3, $33, $3E, $30, $30, $30, $30, $78, $00, $00, $00, $00,  // char 420
    $01, $A5, {|}  $00, $00, $38, $6C, $60, $7C, $66, $66, $66, $66, $66, $7C, $60, $60, $F0, $00,  // char 421
    $01, $A6, {|}  $00, $00, $F0, $60, $7C, $66, $66, $7C, $78, $6C, $6C, $E6, $06, $00, $00, $00,  // char 422
    $01, $A7, {|}  $00, $00, $7C, $C6, $C6, $0C, $38, $60, $C0, $C6, $C6, $7C, $00, $00, $00, $00,  // char 423
    $01, $A8, {|}  $00, $00, $00, $00, $00, $7C, $C6, $0C, $38, $60, $C6, $7C, $00, $00, $00, $00,  // char 424
    $01, $A9, {|}  $00, $00, $FE, $C6, $62, $30, $18, $18, $30, $62, $C6, $FE, $00, $00, $00, $00,  // char 425
    $01, $AA, {|}  $00, $00, $70, $D8, $78, $18, $18, $18, $18, $18, $18, $18, $18, $1B, $0E, $00,  // char 426
    $01, $AB, {|}  $00, $00, $10, $30, $30, $FC, $30, $30, $30, $30, $36, $1C, $0C, $6C, $38, $00,  // char 427
    $01, $AC, {|}  $00, $00, $7E, $FE, $9A, $58, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 428
    $01, $AD, {|}  $00, $00, $1C, $36, $30, $FC, $30, $30, $30, $30, $36, $1C, $00, $00, $00, $00,  // char 429
    $01, $AE, {|}  $00, $00, $7E, $7E, $5A, $18, $18, $18, $18, $18, $18, $18, $18, $18, $0E, $00,  // char 430
    $01, $AF, {|}  $03, $03, $CE, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 431
    $01, $B0, {|}  $00, $00, $03, $03, $06, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 432
    $01, $B1, {|}  $00, $00, $EE, $6C, $6C, $6C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 433
    $01, $B2, {|}  $00, $00, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $CC, $78, $00, $00, $00, $00,  // char 434
    $01, $B3, {|}  $00, $00, $63, $B3, $B3, $33, $1E, $0C, $0C, $0C, $0C, $1E, $00, $00, $00, $00,  // char 435
    $01, $B4, {|}  $00, $00, $00, $06, $0D, $CC, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $18, $F0, $00,  // char 436
    $01, $B5, {|}  $00, $00, $FE, $C6, $86, $0C, $7E, $30, $60, $C2, $C6, $FE, $00, $00, $00, $00,  // char 437
    $01, $B6, {|}  $00, $00, $00, $00, $00, $FE, $CC, $18, $FC, $60, $C6, $FE, $00, $00, $00, $00,  // char 438
    $01, $B7, {|}  $00, $00, $FE, $06, $0C, $18, $30, $7C, $06, $06, $06, $06, $C6, $7C, $00, $00,  // char 439
    $01, $B8, {|}  $00, $00, $FE, $C0, $60, $30, $18, $7C, $C0, $C0, $C0, $C0, $C6, $7C, $00, $00,  // char 440
    $01, $B9, {|}  $00, $00, $00, $00, $00, $FE, $C0, $60, $30, $78, $C0, $C0, $C0, $C6, $7C, $00,  // char 441
    $01, $BA, {|}  $00, $00, $00, $00, $00, $FE, $06, $0C, $18, $3C, $06, $7C, $C0, $C6, $7C, $00,  // char 442
    $01, $BB, {|}  $00, $00, $7C, $C6, $06, $0C, $7E, $30, $60, $C0, $C6, $FE, $00, $00, $00, $00,  // char 443
    $01, $BC, {|}  $00, $00, $FE, $60, $60, $78, $0C, $06, $06, $06, $66, $3C, $00, $00, $00, $00,  // char 444
    $01, $BD, {|}  $00, $00, $00, $00, $00, $FE, $60, $78, $0C, $06, $66, $3C, $00, $00, $00, $00,  // char 445
    $01, $BE, {|}  $00, $00, $10, $30, $30, $FC, $30, $30, $18, $4C, $6C, $38, $00, $00, $00, $00,  // char 446
    $01, $BF, {|}  $00, $00, $00, $00, $00, $DC, $66, $66, $66, $6C, $78, $70, $60, $60, $F0, $00,  // char 447
    $01, $C0, {|}  $00, $00, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00, $00, $00,  // char 448
    $01, $C1, {|}  $00, $00, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $00, $00, $00, $00,  // char 449
    $01, $C2, {|}  $00, $00, $18, $18, $18, $7E, $18, $7E, $18, $18, $18, $18, $00, $00, $00, $00,  // char 450
    $01, $C3, {|}  $00, $00, $18, $3C, $3C, $3C, $18, $18, $18, $00, $18, $18, $00, $00, $00, $00,  // char 451
    $01, $C4, {|}  $1B, $0E, $04, $F7, $D9, $D9, $DA, $DA, $DA, $DC, $DC, $F7, $00, $00, $00, $00,  // char 452
    $01, $C5, {|}  $00, $00, $F5, $DA, $D8, $DF, $D9, $DA, $DA, $DA, $DC, $F7, $00, $00, $00, $00,  // char 453
    $01, $C6, {|}  $00, $00, $3D, $1A, $18, $7F, $D9, $DA, $DA, $DA, $DC, $6F, $00, $00, $00, $00,  // char 454
    $01, $C7, {|}  $00, $00, $C7, $C3, $C3, $C3, $C3, $C3, $C3, $C3, $CB, $F6, $00, $00, $00, $00,  // char 455
    $01, $C8, {|}  $00, $00, $F3, $63, $60, $67, $63, $63, $63, $67, $6F, $FF, $03, $1B, $0E, $00,  // char 456
    $01, $C9, {|}  $00, $00, $E3, $63, $60, $67, $63, $63, $63, $63, $63, $F3, $03, $33, $1E, $00,  // char 457
    $01, $CA, {|}  $00, $00, $DB, $DB, $FB, $FB, $FB, $DB, $DB, $DB, $DB, $DE, $00, $00, $00, $00,  // char 458
    $01, $CB, {|}  $00, $00, $DB, $DB, $F8, $FF, $FB, $FB, $DB, $DB, $DB, $DB, $03, $33, $1E, $00,  // char 459
    $01, $CC, {|}  $00, $00, $03, $03, $00, $B7, $DB, $DB, $DB, $DB, $DB, $DB, $03, $33, $1E, $00,  // char 460
    $01, $CD, {|}  $6C, $38, $10, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 461
    $01, $CE, {|}  $00, $6C, $38, $10, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 462
    $01, $CF, {|}  $6C, $38, $10, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 463
    $01, $D0, {|}  $00, $6C, $38, $10, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 464
    $01, $D1, {|}  $6C, $38, $10, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 465
    $01, $D2, {|}  $00, $6C, $38, $10, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 466
    $01, $D3, {|}  $6C, $38, $10, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 467
    $01, $D4, {|}  $00, $6C, $38, $10, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 468
    $01, $D5, {|}  $7C, $00, $6C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 469
    $01, $D6, {|}  $00, $78, $00, $CC, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 470
    $01, $D7, {|}  $08, $10, $6C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 471
    $01, $D8, {|}  $18, $30, $00, $CC, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 472
    $01, $D9, {|}  $28, $10, $6C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 473
    $01, $DA, {|}  $78, $30, $00, $CC, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 474
    $01, $DB, {|}  $20, $10, $6C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 475
    $01, $DC, {|}  $60, $30, $00, $CC, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 476
    $01, $DD, {|}  $00, $00, $00, $00, $00, $7C, $C6, $06, $06, $FE, $C6, $7C, $00, $00, $00, $00,  // char 477
    $01, $DE, {|}  $7C, $00, $6C, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 478
    $01, $DF, {|}  $00, $7C, $00, $6C, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 479
    $01, $E0, {|}  $7C, $30, $30, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 480
    $01, $E1, {|}  $00, $7C, $00, $30, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 481
    $01, $E2, {|}  $00, $7C, $00, $3E, $6C, $CC, $CC, $FE, $CC, $CC, $CC, $CE, $00, $00, $00, $00,  // char 482
    $01, $E3, {|}  $00, $00, $00, $7C, $00, $CC, $76, $36, $7E, $D8, $D8, $6E, $00, $00, $00, $00,  // char 483
    $01, $E4, {|}  $00, $00, $3C, $66, $C2, $C0, $C0, $DE, $C6, $DF, $66, $3A, $00, $00, $00, $00,  // char 484
    $01, $E5, {|}  $00, $00, $00, $00, $00, $76, $CC, $CC, $CC, $CC, $7C, $0C, $3E, $CC, $78, $00,  // char 485
    $01, $E6, {|}  $6C, $38, $10, $3C, $66, $C2, $C0, $DE, $C6, $C6, $66, $3A, $00, $00, $00, $00,  // char 486
    $01, $E7, {|}  $00, $6C, $38, $10, $00, $76, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $CC, $78, $00,  // char 487
    $01, $E8, {|}  $6C, $38, $10, $E6, $66, $66, $6C, $78, $6C, $66, $66, $E6, $00, $00, $00, $00,  // char 488
    $01, $E9, {|}  $6C, $38, $10, $E0, $60, $66, $6C, $78, $78, $6C, $66, $E6, $00, $00, $00, $00,  // char 489
    $01, $EA, {|}  $00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $30, $60, $38, $00,  // char 490
    $01, $EB, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $30, $60, $38, $00,  // char 491
    $01, $EC, {|}  $00, $7C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $30, $60, $38, $00,  // char 492
    $01, $ED, {|}  $00, $00, $00, $7C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $30, $60, $38, $00,  // char 493
    $01, $EE, {|}  $6C, $38, $10, $FE, $0C, $18, $30, $7C, $06, $06, $06, $06, $C6, $7C, $00, $00,  // char 494
    $01, $EF, {|}  $00, $6C, $38, $10, $00, $FE, $06, $0C, $18, $3C, $06, $06, $06, $C6, $7C, $00,  // char 495
    $01, $F0, {|}  $00, $1B, $0E, $04, $00, $0E, $06, $06, $06, $06, $06, $06, $66, $66, $3C, $00,  // char 496
    $01, $F1, {|}  $00, $00, $F7, $D9, $D9, $DA, $DA, $DA, $DA, $DC, $DC, $F7, $00, $00, $00, $00,  // char 497
    $01, $F2, {|}  $00, $00, $F0, $D8, $D8, $DF, $D9, $DA, $DA, $DA, $DC, $F7, $00, $00, $00, $00,  // char 498
    $01, $F3, {|}  $00, $00, $38, $18, $18, $7F, $D9, $DA, $DA, $DA, $DC, $6F, $00, $00, $00, $00,  // char 499
    $01, $F4, {|}  $0C, $18, $00, $3C, $66, $C2, $C0, $DE, $C6, $C6, $66, $3A, $00, $00, $00, $00,  // char 500
    $01, $F5, {|}  $00, $00, $18, $30, $00, $76, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $CC, $78, $00,  // char 501
    $01, $F6, {|}  $00, $00, $D8, $D8, $D8, $DB, $FB, $DB, $DB, $DB, $DB, $CE, $00, $00, $00, $00,  // char 502
    $01, $F7, {|}  $00, $00, $FC, $66, $66, $66, $66, $6C, $78, $70, $60, $60, $60, $60, $E0, $00,  // char 503
    $01, $F8, {|}  $60, $30, $00, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 504
    $01, $F9, {|}  $00, $00, $30, $18, $00, $DC, $66, $66, $66, $66, $66, $66, $00, $00, $00, $00,  // char 505
    $01, $FA, {|}  $0C, $18, $38, $6C, $38, $38, $6C, $C6, $C6, $FE, $C6, $C6, $00, $00, $00, $00,  // char 506
    $01, $FB, {|}  $0C, $18, $38, $6C, $38, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 507
    $01, $FC, {|}  $0C, $18, $00, $3E, $6C, $CC, $CC, $FE, $CC, $CC, $CC, $CE, $00, $00, $00, $00,  // char 508
    $01, $FD, {|}  $00, $00, $0C, $18, $00, $CC, $76, $36, $7E, $D8, $D8, $6E, $00, $00, $00, $00,  // char 509
    $01, $FE, {|}  $0C, $18, $00, $7A, $C4, $CE, $CE, $D6, $E6, $E6, $46, $BC, $00, $00, $00, $00,  // char 510
    $01, $FF, {|}  $00, $00, $0C, $18, $00, $7A, $C4, $CE, $D6, $E6, $46, $BC, $00, $00, $00, $00,  // char 511
    $02, $00, {|}  $CC, $66, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 512
    $02, $01, {|}  $00, $00, $CC, $66, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 513
    $02, $02, {|}  $38, $6C, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 514
    $02, $03, {|}  $00, $00, $38, $6C, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 515
    $02, $04, {|}  $CC, $66, $00, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $00, $00, $00, $00,  // char 516
    $02, $05, {|}  $00, $00, $CC, $66, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 517
    $02, $06, {|}  $38, $6C, $00, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $00, $00, $00, $00,  // char 518
    $02, $07, {|}  $00, $00, $38, $6C, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 519
    $02, $08, {|}  $CC, $66, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 520
    $02, $09, {|}  $00, $00, $CC, $66, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 521
    $02, $0A, {|}  $3C, $66, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 522
    $02, $0B, {|}  $00, $00, $3C, $66, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 523
    $02, $0C, {|}  $CC, $66, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 524
    $02, $0D, {|}  $00, $00, $CC, $66, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 525
    $02, $0E, {|}  $38, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 526
    $02, $0F, {|}  $00, $00, $38, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 527
    $02, $10, {|}  $CC, $66, $00, $FC, $66, $66, $66, $7C, $6C, $66, $66, $E6, $00, $00, $00, $00,  // char 528
    $02, $11, {|}  $00, $00, $CC, $66, $00, $DC, $76, $66, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 529
    $02, $12, {|}  $38, $6C, $00, $FC, $66, $66, $66, $7C, $6C, $66, $66, $E6, $00, $00, $00, $00,  // char 530
    $02, $13, {|}  $00, $00, $38, $6C, $00, $DC, $76, $66, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 531
    $02, $14, {|}  $CC, $66, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 532
    $02, $15, {|}  $00, $00, $CC, $66, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 533
    $02, $16, {|}  $38, $6C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 534
    $02, $17, {|}  $00, $00, $78, $CC, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 535
    $02, $18, {|}  $00, $00, $7C, $C6, $C6, $60, $38, $0C, $06, $C6, $C6, $7C, $00, $18, $18, $30,  // char 536
    $02, $19, {|}  $00, $00, $00, $00, $00, $7C, $C6, $60, $38, $0C, $C6, $7C, $00, $18, $18, $30,  // char 537
    $02, $1A, {|}  $00, $00, $7E, $7E, $5A, $18, $18, $18, $18, $18, $18, $3C, $00, $18, $18, $30,  // char 538
    $02, $1B, {|}  $00, $00, $10, $30, $30, $FC, $30, $30, $30, $30, $36, $1C, $00, $18, $18, $30,  // char 539
    $02, $1C, {|}  $00, $00, $7C, $C6, $86, $06, $1C, $74, $06, $06, $06, $06, $1C, $F0, $00, $00,  // char 540
    $02, $1D, {|}  $00, $00, $00, $00, $00, $7C, $C6, $06, $0E, $3C, $06, $06, $1C, $F0, $00, $00,  // char 541
    $02, $1E, {|}  $6C, $38, $10, $C6, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 542
    $02, $1F, {|}  $6C, $38, $10, $E0, $60, $6C, $76, $66, $66, $66, $66, $E6, $00, $00, $00, $00,  // char 543
    $02, $22, {|}  $00, $00, $6C, $C6, $C6, $C6, $7C, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 546
    $02, $23, {|}  $00, $00, $24, $66, $66, $66, $3C, $66, $66, $66, $66, $3C, $00, $00, $00, $00,  // char 547
    $02, $24, {|}  $00, $00, $FE, $C6, $86, $0C, $18, $30, $60, $C0, $C0, $FC, $06, $0C, $00, $00,  // char 548
    $02, $25, {|}  $00, $00, $00, $00, $00, $FE, $CC, $18, $30, $60, $C0, $FC, $06, $0C, $00, $00,  // char 549
    $02, $26, {|}  $30, $30, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 550
    $02, $27, {|}  $00, $00, $30, $30, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 551
    $02, $28, {|}  $00, $00, $FE, $66, $62, $68, $78, $68, $60, $62, $66, $FE, $18, $0C, $38, $00,  // char 552
    $02, $29, {|}  $00, $00, $00, $00, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $18, $0C, $38, $00,  // char 553
    $02, $2A, {|}  $7C, $00, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 554
    $02, $2B, {|}  $00, $7C, $00, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 555
    $02, $2C, {|}  $7C, $00, $72, $9C, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 556
    $02, $2D, {|}  $7C, $00, $76, $DC, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 557
    $02, $2E, {|}  $30, $30, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 558
    $02, $2F, {|}  $00, $00, $30, $30, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 559
    $02, $30, {|}  $7C, $00, $30, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 560
    $02, $31, {|}  $00, $7C, $00, $30, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 561
    $02, $32, {|}  $00, $3C, $00, $66, $66, $66, $3C, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 562
    $02, $33, {|}  $00, $00, $00, $7C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00,  // char 563
    $02, $50, {|}  $00, $00, $00, $00, $00, $DC, $66, $66, $66, $7C, $60, $3C, $00, $00, $00, $00,  // char 592
    $02, $51, {|}  $00, $00, $00, $00, $00, $74, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 593
    $02, $52, {|}  $00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $5C, $00, $00, $00, $00,  // char 594
    $02, $53, {|}  $00, $00, $38, $6C, $60, $78, $6C, $66, $66, $66, $66, $7C, $00, $00, $00, $00,  // char 595
    $02, $54, {|}  $00, $00, $00, $00, $00, $7C, $C6, $06, $06, $06, $C6, $7C, $00, $00, $00, $00,  // char 596
    $02, $55, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C0, $C0, $DC, $E6, $7C, $80, $00, $00, $00,  // char 597
    $02, $56, {|}  $00, $00, $1C, $0C, $0C, $3C, $6C, $CC, $CC, $CC, $CC, $7C, $0C, $0D, $06, $00,  // char 598
    $02, $57, {|}  $00, $00, $06, $0D, $0C, $3C, $6C, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 599
    $02, $58, {|}  $00, $00, $00, $00, $00, $7C, $C6, $FE, $06, $06, $C6, $7C, $00, $00, $00, $00,  // char 600
    $02, $59, {|}  $00, $00, $00, $00, $00, $7C, $C6, $06, $06, $FE, $C6, $7C, $00, $00, $00, $00,  // char 601
    $02, $5A, {|}  $00, $00, $00, $00, $00, $3B, $6C, $8E, $16, $26, $6C, $38, $00, $00, $00, $00,  // char 602
    $02, $5B, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C0, $78, $C0, $C6, $7C, $00, $00, $00, $00,  // char 603
    $02, $5C, {|}  $00, $00, $00, $00, $00, $7C, $C6, $06, $3C, $06, $C6, $7C, $00, $00, $00, $00,  // char 604
    $02, $5D, {|}  $00, $00, $00, $00, $00, $7A, $CD, $0D, $38, $0C, $CC, $78, $00, $00, $00, $00,  // char 605
    $02, $5E, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C6, $DC, $C6, $C6, $7C, $00, $00, $00, $00,  // char 606
    $02, $5F, {|}  $00, $00, $00, $00, $00, $0E, $06, $06, $1F, $06, $06, $06, $66, $66, $3C, $00,  // char 607
    $02, $60, {|}  $00, $00, $00, $06, $0D, $7C, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $CC, $78, $00,  // char 608
    $02, $61, {|}  $00, $00, $00, $00, $00, $76, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $CC, $78, $00,  // char 609
    $02, $62, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C0, $CE, $C6, $C6, $7A, $00, $00, $00, $00,  // char 610
    $02, $63, {|}  $00, $00, $00, $00, $00, $C6, $46, $6C, $2C, $2C, $38, $18, $18, $18, $18, $00,  // char 611
    $02, $64, {|}  $00, $00, $00, $00, $00, $C2, $64, $28, $38, $38, $6C, $38, $00, $00, $00, $00,  // char 612
    $02, $65, {|}  $00, $00, $00, $00, $00, $CE, $CC, $CC, $CC, $CC, $DC, $6C, $0C, $0C, $0E, $00,  // char 613
    $02, $66, {|}  $00, $00, $38, $6C, $60, $6C, $76, $66, $66, $66, $66, $E6, $00, $00, $00, $00,  // char 614
    $02, $67, {|}  $00, $00, $38, $6C, $60, $6C, $76, $66, $66, $66, $66, $E6, $06, $06, $1C, $00,  // char 615
    $02, $68, {|}  $00, $00, $18, $18, $00, $38, $18, $18, $3C, $18, $18, $3C, $00, $00, $00, $00,  // char 616
    $02, $69, {|}  $00, $00, $00, $00, $00, $38, $18, $18, $18, $18, $18, $0E, $00, $00, $00, $00,  // char 617
    $02, $6A, {|}  $00, $00, $00, $00, $00, $3C, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 618
    $02, $6B, {|}  $00, $00, $38, $18, $18, $18, $7B, $DE, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 619
    $02, $6C, {|}  $00, $00, $38, $18, $18, $18, $78, $58, $3E, $18, $18, $3C, $00, $00, $00, $00,  // char 620
    $02, $6D, {|}  $00, $00, $38, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $1B, $0E, $00,  // char 621
    $02, $6E, {|}  $00, $00, $E0, $60, $60, $7F, $63, $66, $6C, $7E, $63, $F3, $03, $33, $1E, $00,  // char 622
    $02, $6F, {|}  $00, $00, $00, $00, $00, $C6, $D6, $D6, $D6, $D6, $FE, $6E, $00, $00, $00, $00,  // char 623
    $02, $70, {|}  $00, $00, $00, $00, $00, $C6, $D6, $D6, $D6, $D6, $FE, $6E, $06, $06, $06, $00,  // char 624
    $02, $71, {|}  $00, $00, $00, $00, $00, $EC, $FE, $D6, $D6, $D6, $D6, $C6, $06, $06, $1C, $00,  // char 625
    $02, $72, {|}  $00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $66, $60, $60, $C0, $00,  // char 626
    $02, $73, {|}  $00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $66, $06, $06, $03, $00,  // char 627
    $02, $74, {|}  $00, $00, $00, $00, $00, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $00, $00, $00, $00,  // char 628
    $02, $75, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C6, $FE, $C6, $C6, $7C, $00, $00, $00, $00,  // char 629
    $02, $76, {|}  $00, $00, $00, $00, $00, $6E, $D8, $D8, $DE, $D8, $D8, $6E, $00, $00, $00, $00,  // char 630
    $02, $77, {|}  $00, $00, $00, $00, $00, $6C, $C6, $D6, $D6, $D6, $D6, $6C, $00, $00, $00, $00,  // char 631
    $02, $78, {|}  $00, $00, $10, $10, $10, $7C, $D6, $D6, $D6, $D6, $D6, $7C, $10, $10, $10, $00,  // char 632
    $02, $79, {|}  $00, $00, $00, $00, $00, $1E, $0C, $0C, $0C, $CC, $DC, $76, $00, $00, $00, $00,  // char 633
    $02, $7A, {|}  $00, $00, $1E, $0C, $0C, $0C, $0C, $0C, $0C, $CC, $DC, $76, $00, $00, $00, $00,  // char 634
    $02, $7B, {|}  $00, $00, $00, $00, $00, $1E, $0C, $0C, $0C, $CC, $DC, $6C, $0C, $0D, $06, $00,  // char 635
    $02, $7C, {|}  $00, $00, $00, $00, $00, $DC, $76, $66, $60, $60, $60, $60, $60, $60, $F0, $00,  // char 636
    $02, $7D, {|}  $00, $00, $00, $00, $00, $DC, $76, $66, $60, $60, $60, $60, $60, $6C, $38, $00,  // char 637
    $02, $7E, {|}  $00, $00, $00, $00, $00, $3C, $66, $66, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 638
    $02, $7F, {|}  $00, $00, $00, $00, $00, $78, $CC, $CC, $0C, $0C, $0C, $1E, $00, $00, $00, $00,  // char 639
    $02, $80, {|}  $00, $00, $00, $00, $00, $FC, $66, $66, $7C, $6C, $66, $E6, $00, $00, $00, $00,  // char 640
    $02, $81, {|}  $00, $00, $00, $00, $00, $E6, $66, $6C, $7C, $66, $66, $FC, $00, $00, $00, $00,  // char 641
    $02, $82, {|}  $00, $00, $00, $00, $00, $7C, $C6, $60, $38, $0C, $C6, $FC, $C0, $D8, $70, $00,  // char 642
    $02, $83, {|}  $00, $00, $0E, $1B, $19, $18, $18, $18, $18, $18, $18, $18, $98, $D8, $70, $00,  // char 643
    $02, $84, {|}  $00, $00, $0E, $1B, $19, $18, $18, $18, $18, $18, $18, $3C, $98, $D8, $70, $00,  // char 644
    $02, $85, {|}  $00, $00, $00, $00, $70, $D8, $18, $18, $18, $18, $1B, $0E, $00, $00, $00, $00,  // char 645
    $02, $86, {|}  $00, $00, $00, $00, $0E, $1B, $18, $18, $18, $18, $18, $18, $7E, $D8, $70, $00,  // char 646
    $02, $87, {|}  $00, $00, $70, $D8, $18, $18, $18, $18, $7E, $18, $18, $10, $00, $00, $00, $00,  // char 647
    $02, $88, {|}  $00, $00, $10, $30, $30, $FC, $30, $30, $30, $30, $30, $30, $30, $36, $1C, $00,  // char 648
    $02, $89, {|}  $00, $00, $00, $00, $00, $66, $66, $66, $FF, $66, $66, $3B, $00, $00, $00, $00,  // char 649
    $02, $8A, {|}  $00, $00, $00, $00, $00, $EE, $6C, $6C, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 650
    $02, $8B, {|}  $00, $00, $00, $00, $00, $CC, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 651
    $02, $8C, {|}  $00, $00, $00, $00, $00, $18, $3C, $66, $66, $66, $66, $66, $00, $00, $00, $00,  // char 652
    $02, $8D, {|}  $00, $00, $00, $00, $00, $6C, $FE, $D6, $D6, $D6, $C6, $C6, $00, $00, $00, $00,  // char 653
    $02, $8E, {|}  $00, $00, $3E, $60, $C0, $FC, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 654
    $02, $8F, {|}  $00, $00, $00, $00, $00, $66, $66, $66, $3C, $18, $18, $3C, $00, $00, $00, $00,  // char 655
    $02, $90, {|}  $00, $00, $00, $00, $00, $FE, $CC, $18, $30, $60, $C6, $FE, $06, $06, $03, $00,  // char 656
    $02, $91, {|}  $00, $00, $00, $00, $00, $FE, $CC, $18, $30, $66, $CB, $FE, $10, $00, $00, $00,  // char 657
    $02, $92, {|}  $00, $00, $00, $00, $00, $FE, $06, $0C, $18, $3C, $06, $06, $06, $C6, $7C, $00,  // char 658
    $02, $93, {|}  $00, $00, $00, $00, $00, $FE, $06, $0C, $18, $3C, $06, $06, $7E, $C7, $7C, $00,  // char 659
    $02, $94, {|}  $00, $00, $7C, $C6, $C6, $06, $1C, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 660
    $02, $95, {|}  $00, $00, $7C, $C6, $C6, $C0, $70, $30, $30, $30, $30, $78, $00, $00, $00, $00,  // char 661
    $02, $96, {|}  $00, $00, $3C, $18, $18, $18, $18, $1C, $06, $C6, $C6, $7C, $00, $00, $00, $00,  // char 662
    $02, $97, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C0, $C0, $C0, $C0, $C0, $C0, $C6, $7C, $00,  // char 663
    $02, $98, {|}  $00, $00, $3C, $66, $C3, $C3, $DB, $DB, $C3, $C3, $66, $3C, $00, $00, $00, $00,  // char 664
    $02, $99, {|}  $00, $00, $00, $00, $00, $FC, $66, $66, $7C, $66, $66, $FC, $00, $00, $00, $00,  // char 665
    $02, $9A, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C6, $76, $C6, $C6, $7C, $00, $00, $00, $00,  // char 666
    $02, $9B, {|}  $00, $00, $00, $00, $03, $7E, $C6, $C0, $CE, $C6, $C6, $7A, $00, $00, $00, $00,  // char 667
    $02, $9C, {|}  $00, $00, $00, $00, $00, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 668
    $02, $9D, {|}  $00, $00, $0C, $0C, $00, $1C, $0C, $0C, $0C, $0C, $0C, $0C, $7E, $CC, $78, $00,  // char 669
    $02, $9E, {|}  $00, $00, $00, $00, $00, $CE, $CC, $6C, $3C, $3C, $6C, $CC, $0C, $0C, $0E, $00,  // char 670
    $02, $9F, {|}  $00, $00, $00, $00, $00, $F0, $60, $60, $60, $62, $66, $FE, $00, $00, $00, $00,  // char 671
    $02, $A0, {|}  $00, $00, $00, $06, $0D, $7C, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $0C, $1E, $00,  // char 672
    $02, $A1, {|}  $00, $00, $7C, $C6, $C6, $06, $1C, $18, $7E, $18, $18, $3C, $00, $00, $00, $00,  // char 673
    $02, $A2, {|}  $00, $00, $7C, $C6, $C6, $C0, $70, $30, $FC, $30, $30, $78, $00, $00, $00, $00,  // char 674
    $02, $A3, {|}  $00, $00, $38, $18, $18, $7F, $D9, $DA, $DA, $DA, $DC, $6F, $00, $00, $00, $00,  // char 675
    $02, $A4, {|}  $00, $00, $38, $18, $18, $7F, $DB, $DB, $DE, $DE, $DB, $6B, $03, $1B, $0E, $00,  // char 676
    $02, $A5, {|}  $00, $00, $38, $18, $18, $7F, $D9, $DA, $DA, $DB, $DD, $6F, $04, $00, $00, $00,  // char 677
    $02, $A6, {|}  $00, $00, $20, $60, $60, $F6, $6D, $6C, $66, $63, $6B, $36, $00, $00, $00, $00,  // char 678
    $02, $A7, {|}  $00, $00, $26, $6D, $6C, $FC, $6C, $6C, $6C, $6C, $6C, $3C, $0C, $2C, $18, $00,  // char 679
    $02, $A8, {|}  $00, $00, $20, $60, $60, $F6, $6D, $6C, $6C, $6E, $6D, $36, $00, $00, $00, $00,  // char 680
    $02, $A9, {|}  $00, $00, $70, $D8, $C0, $FE, $DB, $DB, $DB, $DB, $DB, $DB, $03, $03, $0E, $00,  // char 681
    $02, $AA, {|}  $00, $00, $E0, $60, $60, $66, $6D, $6C, $66, $63, $6B, $F6, $00, $00, $00, $00,  // char 682
    $02, $AB, {|}  $00, $00, $E0, $60, $60, $6F, $6B, $63, $66, $6C, $6D, $FF, $00, $00, $00, $00,  // char 683
    $02, $AC, {|}  $00, $00, $C6, $D6, $7C, $6C, $28, $C6, $D6, $7C, $6C, $28, $00, $00, $00, $00,  // char 684
    $02, $AD, {|}  $00, $00, $FE, $C6, $C6, $C6, $00, $00, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 685
    $02, $B0, {|}  $00, $C0, $C0, $F0, $D8, $D8, $D8, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 688
    $02, $B1, {|}  $00, $70, $C0, $F0, $D8, $D8, $D8, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 689
    $02, $B2, {|}  $00, $18, $00, $38, $18, $18, $D8, $70, $00, $00, $00, $00, $00, $00, $00, $00,  // char 690
    $02, $B3, {|}  $00, $00, $00, $B0, $D8, $C0, $C0, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 691
    $02, $B4, {|}  $00, $00, $00, $18, $18, $D8, $68, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 692
    $02, $B5, {|}  $00, $00, $00, $18, $18, $D8, $68, $0C, $00, $00, $00, $00, $00, $00, $00, $00,  // char 693
    $02, $B6, {|}  $00, $D8, $D8, $F0, $D8, $D8, $F0, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 694
    $02, $B7, {|}  $00, $00, $00, $C6, $D6, $7C, $6C, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 695
    $02, $B8, {|}  $00, $00, $00, $D8, $D8, $78, $18, $70, $00, $00, $00, $00, $00, $00, $00, $00,  // char 696
    $02, $B9, {|}  $00, $18, $30, $60, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 697
    $02, $BA, {|}  $00, $36, $6C, $D8, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 698
    $02, $BB, {|}  $00, $18, $30, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 699
    $02, $BC, {|}  $00, $18, $18, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 700
    $02, $BD, {|}  $00, $30, $30, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 701
    $02, $BE, {|}  $00, $18, $0C, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 702
    $02, $BF, {|}  $00, $30, $60, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 703
    $02, $C0, {|}  $00, $00, $70, $D8, $18, $30, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 704
    $02, $C1, {|}  $00, $00, $70, $D8, $C0, $60, $60, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 705
    $02, $C2, {|}  $00, $00, $18, $70, $C0, $70, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 706
    $02, $C3, {|}  $00, $00, $C0, $70, $18, $70, $C0, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 707
    $02, $C4, {|}  $00, $20, $20, $70, $70, $D8, $D8, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 708
    $02, $C5, {|}  $00, $D8, $D8, $70, $70, $20, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 709
    $02, $C6, {|}  $00, $10, $38, $6C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 710
    $02, $C7, {|}  $00, $6C, $38, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 711
    $02, $C8, {|}  $00, $18, $18, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 712
    $02, $C9, {|}  $00, $00, $00, $7C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 713
    $02, $CA, {|}  $00, $00, $0C, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 714
    $02, $CB, {|}  $00, $00, $60, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 715
    $02, $CC, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $00,  // char 716
    $02, $CD, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $7C, $00, $00,  // char 717
    $02, $CE, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $0C, $00,  // char 718
    $02, $CF, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $30, $60, $00,  // char 719
    $02, $D8, {|}  $00, $00, $6C, $38, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 728
    $02, $D9, {|}  $00, $00, $18, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 729
    $02, $DA, {|}  $00, $38, $6C, $38, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 730
    $02, $DB, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $30, $1C, $00,  // char 731
    $02, $DC, {|}  $00, $00, $76, $DC, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 732
    $02, $DD, {|}  $00, $00, $66, $CC, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 733
    $02, $EE, {|}  $00, $6C, $6C, $6C, $48, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 750
    $03, $00, {|}  $00, $00, $60, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 768
    $03, $01, {|}  $00, $00, $0C, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 769
    $03, $03, {|}  $00, $00, $34, $58, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 771
    $03, $09, {|}  $00, $70, $18, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 777
    $03, $12, {|}  $00, $18, $30, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 786
    $03, $13, {|}  $00, $18, $18, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 787
    $03, $14, {|}  $00, $30, $30, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 788
    $03, $23, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $00,  // char 803
    $03, $40, {|}  $C0, $60, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 832
    $03, $41, {|}  $03, $06, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 833
    $03, $74, {|}  $00, $30, $20, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 884
    $03, $75, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $10, $20, $60, $00, $00, $00,  // char 885
    $03, $7A, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $30, $34, $18, $00,  // char 890
    $03, $7E, {|}  $00, $00, $00, $00, $18, $18, $00, $00, $00, $18, $18, $30, $00, $00, $00, $00,  // char 894
    $03, $84, {|}  $60, $C0, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 900
    $03, $85, {|}  $0C, $18, $00, $6C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 901
    $03, $86, {|}  $60, $C0, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 902
    $03, $87, {|}  $00, $00, $00, $00, $00, $00, $00, $18, $18, $00, $00, $00, $00, $00, $00, $00,  // char 903
    $03, $88, {|}  $60, $C0, $3E, $32, $30, $34, $3C, $34, $30, $30, $32, $3E, $00, $00, $00, $00,  // char 904
    $03, $89, {|}  $60, $C0, $33, $33, $33, $33, $3F, $33, $33, $33, $33, $33, $00, $00, $00, $00,  // char 905
    $03, $8A, {|}  $60, $C0, $3C, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 906
    $03, $8C, {|}  $60, $C0, $3C, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $00, $00, $00, $00,  // char 908
    $03, $8E, {|}  $60, $C0, $66, $66, $66, $66, $3C, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 910
    $03, $8F, {|}  $60, $C0, $3E, $63, $63, $63, $63, $63, $36, $36, $36, $77, $00, $00, $00, $00,  // char 911
    $03, $90, {|}  $0C, $18, $00, $6C, $00, $38, $18, $18, $18, $18, $18, $0E, $00, $00, $00, $00,  // char 912
    $03, $91, {|}  $00, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 913
    $03, $92, {|}  $00, $00, $FC, $66, $66, $66, $7C, $66, $66, $66, $66, $FC, $00, $00, $00, $00,  // char 914
    $03, $93, {|}  $00, $00, $FE, $66, $62, $60, $60, $60, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 915
    $03, $94, {|}  $00, $00, $10, $10, $38, $38, $6C, $6C, $C6, $C6, $C6, $FE, $00, $00, $00, $00,  // char 916
    $03, $95, {|}  $00, $00, $FE, $66, $62, $68, $78, $68, $60, $62, $66, $FE, $00, $00, $00, $00,  // char 917
    $03, $96, {|}  $00, $00, $FE, $C6, $86, $0C, $18, $30, $60, $C2, $C6, $FE, $00, $00, $00, $00,  // char 918
    $03, $97, {|}  $00, $00, $C6, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 919
    $03, $98, {|}  $00, $00, $7C, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 920
    $03, $99, {|}  $00, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 921
    $03, $9A, {|}  $00, $00, $E6, $66, $66, $6C, $78, $78, $6C, $66, $66, $E6, $00, $00, $00, $00,  // char 922
    $03, $9B, {|}  $00, $00, $10, $10, $38, $38, $6C, $6C, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 923
    $03, $9C, {|}  $00, $00, $C6, $EE, $FE, $FE, $D6, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 924
    $03, $9D, {|}  $00, $00, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 925
    $03, $9E, {|}  $00, $00, $FE, $C6, $82, $44, $7C, $44, $00, $82, $C6, $FE, $00, $00, $00, $00,  // char 926
    $03, $9F, {|}  $00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 927
    $03, $A0, {|}  $00, $00, $FE, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 928
    $03, $A1, {|}  $00, $00, $FC, $66, $66, $66, $7C, $60, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 929
    $03, $A3, {|}  $00, $00, $FE, $C6, $62, $30, $18, $18, $30, $62, $C6, $FE, $00, $00, $00, $00,  // char 931
    $03, $A4, {|}  $00, $00, $7E, $7E, $5A, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 932
    $03, $A5, {|}  $00, $00, $66, $66, $66, $66, $3C, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 933
    $03, $A6, {|}  $00, $00, $10, $7C, $D6, $D6, $D6, $D6, $D6, $7C, $10, $10, $00, $00, $00, $00,  // char 934
    $03, $A7, {|}  $00, $00, $C6, $C6, $6C, $7C, $38, $38, $7C, $6C, $C6, $C6, $00, $00, $00, $00,  // char 935
    $03, $A8, {|}  $00, $00, $10, $92, $D6, $D6, $D6, $D6, $D6, $7C, $10, $10, $00, $00, $00, $00,  // char 936
    $03, $A9, {|}  $00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $6C, $6C, $6C, $EE, $00, $00, $00, $00,  // char 937
    $03, $AA, {|}  $66, $66, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 938
    $03, $AB, {|}  $66, $66, $00, $66, $66, $66, $3C, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 939
    $03, $AC, {|}  $00, $00, $0C, $18, $00, $76, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 940
    $03, $AD, {|}  $00, $00, $0C, $18, $00, $7C, $C6, $C0, $78, $C0, $C6, $7C, $00, $00, $00, $00,  // char 941
    $03, $AE, {|}  $00, $00, $0C, $18, $00, $DC, $66, $66, $66, $66, $66, $66, $06, $06, $06, $00,  // char 942
    $03, $AF, {|}  $00, $00, $0C, $18, $00, $38, $18, $18, $18, $18, $18, $0E, $00, $00, $00, $00,  // char 943
    $03, $B0, {|}  $0C, $18, $00, $6C, $00, $CC, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 944
    $03, $B1, {|}  $00, $00, $00, $00, $00, $76, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 945
    $03, $B2, {|}  $00, $00, $3C, $66, $66, $66, $6C, $66, $66, $66, $66, $6C, $60, $60, $60, $00,  // char 946
    $03, $B3, {|}  $00, $00, $00, $00, $00, $C6, $46, $6C, $2C, $2C, $38, $18, $18, $18, $18, $00,  // char 947
    $03, $B4, {|}  $00, $00, $3C, $66, $30, $18, $7C, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 948
    $03, $B5, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C0, $78, $C0, $C6, $7C, $00, $00, $00, $00,  // char 949
    $03, $B6, {|}  $00, $00, $46, $7C, $18, $30, $60, $60, $C0, $C0, $C0, $7C, $06, $06, $1C, $00,  // char 950
    $03, $B7, {|}  $00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $66, $06, $06, $06, $00,  // char 951
    $03, $B8, {|}  $00, $00, $3C, $66, $66, $66, $7E, $66, $66, $66, $66, $3C, $00, $00, $00, $00,  // char 952
    $03, $B9, {|}  $00, $00, $00, $00, $00, $38, $18, $18, $18, $18, $18, $0E, $00, $00, $00, $00,  // char 953
    $03, $BA, {|}  $00, $00, $00, $00, $00, $E6, $6C, $78, $78, $6C, $66, $E6, $00, $00, $00, $00,  // char 954
    $03, $BB, {|}  $00, $00, $C0, $20, $30, $10, $38, $38, $6C, $64, $C6, $C2, $00, $00, $00, $00,  // char 955
    $03, $BC, {|}  $00, $00, $00, $00, $00, $CC, $CC, $CC, $CC, $CC, $CC, $F6, $C0, $C0, $C0, $00,  // char 956
    $03, $BD, {|}  $00, $00, $00, $00, $00, $C6, $C6, $66, $6C, $3C, $38, $10, $00, $00, $00, $00,  // char 957
    $03, $BE, {|}  $00, $00, $66, $3C, $30, $60, $3C, $60, $C0, $C0, $C0, $7C, $06, $06, $1C, $00,  // char 958
    $03, $BF, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 959
    $03, $C0, {|}  $00, $00, $00, $00, $00, $FE, $6C, $6C, $6C, $6C, $6C, $66, $00, $00, $00, $00,  // char 960
    $03, $C1, {|}  $00, $00, $00, $00, $00, $3C, $66, $66, $66, $66, $66, $7C, $60, $60, $60, $00,  // char 961
    $03, $C2, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C0, $C0, $C0, $7C, $06, $06, $1C, $00, $00,  // char 962
    $03, $C3, {|}  $00, $00, $00, $00, $00, $7E, $CC, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 963
    $03, $C4, {|}  $00, $00, $00, $00, $00, $7E, $18, $18, $18, $18, $18, $0E, $00, $00, $00, $00,  // char 964
    $03, $C5, {|}  $00, $00, $00, $00, $00, $CC, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 965
    $03, $C6, {|}  $00, $00, $00, $00, $00, $6C, $D6, $D6, $D6, $D6, $D6, $7C, $10, $10, $10, $00,  // char 966
    $03, $C7, {|}  $00, $00, $00, $00, $00, $86, $46, $2C, $2C, $18, $30, $68, $68, $C4, $C2, $00,  // char 967
    $03, $C8, {|}  $00, $00, $00, $00, $00, $92, $D6, $D6, $D6, $D6, $D6, $7C, $10, $10, $10, $00,  // char 968
    $03, $C9, {|}  $00, $00, $00, $00, $00, $6C, $C6, $D6, $D6, $D6, $D6, $6C, $00, $00, $00, $00,  // char 969
    $03, $CA, {|}  $00, $00, $66, $66, $00, $38, $18, $18, $18, $18, $18, $0E, $00, $00, $00, $00,  // char 970
    $03, $CB, {|}  $00, $00, $6C, $6C, $00, $CC, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 971
    $03, $CC, {|}  $00, $00, $0C, $18, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 972
    $03, $CD, {|}  $00, $00, $0C, $18, $00, $CC, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 973
    $03, $CE, {|}  $00, $00, $0C, $18, $00, $6C, $C6, $D6, $D6, $D6, $D6, $6C, $00, $00, $00, $00,  // char 974
    $03, $D0, {|}  $00, $00, $3C, $66, $66, $6C, $7C, $66, $66, $66, $66, $3C, $00, $00, $00, $00,  // char 976
    $03, $D1, {|}  $00, $00, $3C, $66, $66, $36, $1E, $C6, $66, $66, $66, $3C, $00, $00, $00, $00,  // char 977
    $03, $D2, {|}  $00, $00, $C2, $65, $24, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 978
    $03, $D3, {|}  $60, $C0, $21, $32, $12, $1C, $0C, $0C, $0C, $0C, $0C, $1E, $00, $00, $00, $00,  // char 979
    $03, $D4, {|}  $6C, $6C, $00, $C2, $65, $24, $38, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 980
    $03, $D5, {|}  $00, $00, $10, $10, $10, $7C, $D6, $D6, $D6, $D6, $D6, $7C, $10, $10, $10, $00,  // char 981
    $03, $D6, {|}  $00, $00, $00, $00, $00, $FE, $6C, $C6, $D6, $D6, $D6, $6C, $00, $00, $00, $00,  // char 982
    $03, $D7, {|}  $00, $00, $00, $00, $00, $46, $A6, $2C, $38, $68, $CA, $C4, $0C, $18, $00, $00,  // char 983
    $03, $DA, {|}  $00, $00, $3C, $66, $C2, $C0, $C0, $C0, $C0, $60, $3C, $06, $06, $1C, $00, $00,  // char 986
    $03, $DB, {|}  $00, $00, $00, $00, $00, $7E, $C0, $C0, $C0, $C0, $C0, $7C, $06, $06, $1C, $00,  // char 987
    $03, $DC, {|}  $00, $00, $FE, $66, $62, $60, $78, $68, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 988
    $03, $DD, {|}  $00, $00, $00, $00, $00, $7E, $32, $30, $30, $3C, $34, $30, $30, $30, $30, $00,  // char 989
    $03, $DE, {|}  $00, $60, $30, $30, $60, $63, $FF, $C6, $06, $0C, $0C, $06, $00, $00, $00, $00,  // char 990
    $03, $DF, {|}  $00, $00, $30, $30, $60, $60, $FE, $FE, $0C, $0C, $18, $18, $00, $00, $00, $00,  // char 991
    $03, $E0, {|}  $00, $00, $78, $AC, $26, $26, $0B, $0B, $1B, $1B, $1B, $1B, $03, $02, $04, $00,  // char 992
    $03, $E1, {|}  $00, $00, $00, $00, $C0, $60, $30, $18, $28, $4C, $14, $24, $06, $02, $02, $00,  // char 993
    $03, $E2, {|}  $00, $00, $DB, $DB, $DB, $DB, $DB, $DB, $DB, $DB, $DB, $6D, $03, $3E, $00, $00,  // char 994
    $03, $E3, {|}  $00, $00, $00, $00, $00, $DB, $DB, $DB, $DB, $DB, $DB, $6D, $03, $7E, $00, $00,  // char 995
    $03, $E4, {|}  $00, $00, $76, $D6, $C6, $C6, $C6, $7E, $06, $06, $06, $06, $00, $00, $00, $00,  // char 996
    $03, $E5, {|}  $00, $00, $36, $66, $66, $66, $66, $3E, $06, $06, $06, $06, $00, $00, $00, $00,  // char 997
    $03, $E6, {|}  $00, $00, $C0, $C0, $C0, $C0, $FC, $C6, $C6, $C6, $C6, $C6, $06, $76, $9C, $00,  // char 998
    $03, $E7, {|}  $00, $00, $00, $00, $08, $7C, $D6, $66, $06, $06, $66, $BC, $00, $00, $00, $00,  // char 999
    $03, $E8, {|}  $00, $00, $7C, $C6, $C6, $06, $3C, $60, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1000
    $03, $E9, {|}  $00, $00, $3C, $66, $66, $26, $1C, $70, $C0, $C2, $C6, $7C, $00, $00, $00, $00,  // char 1001
    $03, $EA, {|}  $00, $00, $C6, $68, $38, $30, $38, $38, $4C, $4C, $86, $FE, $00, $00, $00, $00,  // char 1002
    $03, $EB, {|}  $00, $00, $00, $00, $00, $64, $BA, $30, $38, $28, $4C, $7C, $00, $00, $00, $00,  // char 1003
    $03, $EC, {|}  $00, $00, $7E, $C0, $CC, $D6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1004
    $03, $ED, {|}  $00, $00, $00, $00, $00, $7E, $C0, $DC, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1005
    $03, $EE, {|}  $00, $00, $3C, $18, $7E, $99, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 1006
    $03, $EF, {|}  $00, $00, $18, $1C, $18, $7E, $58, $18, $18, $18, $38, $18, $00, $00, $00, $00,  // char 1007
    $03, $F0, {|}  $00, $00, $00, $00, $00, $46, $A6, $2C, $38, $68, $CA, $C4, $00, $00, $00, $00,  // char 1008
    $03, $F1, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C6, $C6, $C6, $FC, $C0, $60, $3C, $06, $00,  // char 1009
    $03, $F2, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C0, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 1010
    $03, $F3, {|}  $00, $00, $06, $06, $00, $0E, $06, $06, $06, $06, $06, $06, $66, $66, $3C, $00,  // char 1011
    $04, $00, {|}  $30, $18, $00, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $00, $00, $00, $00,  // char 1024
    $04, $01, {|}  $6C, $6C, $00, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $00, $00, $00, $00,  // char 1025
    $04, $02, {|}  $00, $00, $FE, $B2, $30, $3E, $33, $33, $33, $33, $33, $33, $03, $06, $00, $00,  // char 1026
    $04, $03, {|}  $0C, $18, $00, $FE, $66, $62, $60, $60, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 1027
    $04, $04, {|}  $00, $00, $3C, $66, $C2, $C0, $F8, $C0, $C0, $C2, $66, $3C, $00, $00, $00, $00,  // char 1028
    $04, $05, {|}  $00, $00, $7C, $C6, $C6, $60, $38, $0C, $06, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1029
    $04, $06, {|}  $00, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 1030
    $04, $07, {|}  $66, $66, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 1031
    $04, $08, {|}  $00, $00, $1E, $0C, $0C, $0C, $0C, $0C, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 1032
    $04, $09, {|}  $00, $00, $78, $D8, $D8, $D8, $DE, $DB, $DB, $DB, $DB, $DE, $00, $00, $00, $00,  // char 1033
    $04, $0A, {|}  $00, $00, $D8, $D8, $D8, $D8, $FE, $DB, $DB, $DB, $DB, $DE, $00, $00, $00, $00,  // char 1034
    $04, $0B, {|}  $00, $00, $FE, $B2, $30, $3E, $33, $33, $33, $33, $33, $33, $00, $00, $00, $00,  // char 1035
    $04, $0C, {|}  $0C, $18, $00, $E6, $66, $66, $6C, $78, $6C, $66, $66, $E6, $00, $00, $00, $00,  // char 1036
    $04, $0D, {|}  $30, $18, $00, $C6, $C6, $CE, $DE, $FE, $F6, $E6, $C6, $C6, $00, $00, $00, $00,  // char 1037
    $04, $0E, {|}  $6C, $38, $00, $C6, $C6, $C6, $C6, $7E, $06, $06, $C6, $7C, $00, $00, $00, $00,  // char 1038
    $04, $0F, {|}  $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $FE, $38, $10, $10, $00,  // char 1039
    $04, $10, {|}  $00, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 1040
    $04, $11, {|}  $00, $00, $FE, $66, $62, $60, $7C, $66, $66, $66, $66, $FC, $00, $00, $00, $00,  // char 1041
    $04, $12, {|}  $00, $00, $FC, $66, $66, $66, $7C, $66, $66, $66, $66, $FC, $00, $00, $00, $00,  // char 1042
    $04, $13, {|}  $00, $00, $FE, $66, $62, $60, $60, $60, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 1043
    $04, $14, {|}  $00, $00, $3C, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $FE, $C6, $82, $00, $00,  // char 1044
    $04, $15, {|}  $00, $00, $FE, $66, $62, $68, $78, $68, $60, $62, $66, $FE, $00, $00, $00, $00,  // char 1045
    $04, $16, {|}  $00, $00, $D6, $D6, $D6, $7C, $38, $7C, $D6, $D6, $D6, $D6, $00, $00, $00, $00,  // char 1046
    $04, $17, {|}  $00, $00, $7C, $C6, $86, $06, $3C, $06, $06, $86, $C6, $7C, $00, $00, $00, $00,  // char 1047
    $04, $18, {|}  $00, $00, $C6, $C6, $C6, $CE, $DE, $FE, $F6, $E6, $C6, $C6, $00, $00, $00, $00,  // char 1048
    $04, $19, {|}  $6C, $38, $C6, $C6, $C6, $CE, $DE, $FE, $F6, $E6, $C6, $C6, $00, $00, $00, $00,  // char 1049
    $04, $1A, {|}  $00, $00, $E6, $66, $66, $6C, $78, $78, $6C, $66, $66, $E6, $00, $00, $00, $00,  // char 1050
    $04, $1B, {|}  $00, $00, $3E, $66, $66, $66, $66, $66, $66, $66, $66, $C6, $00, $00, $00, $00,  // char 1051
    $04, $1C, {|}  $00, $00, $C6, $EE, $FE, $FE, $D6, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 1052
    $04, $1D, {|}  $00, $00, $C6, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 1053
    $04, $1E, {|}  $00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1054
    $04, $1F, {|}  $00, $00, $FE, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 1055
    $04, $20, {|}  $00, $00, $FC, $66, $66, $66, $7C, $60, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 1056
    $04, $21, {|}  $00, $00, $3C, $66, $C2, $C0, $C0, $C0, $C0, $C2, $66, $3C, $00, $00, $00, $00,  // char 1057
    $04, $22, {|}  $00, $00, $7E, $7E, $5A, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 1058
    $04, $23, {|}  $00, $00, $C6, $C6, $C6, $C6, $C6, $7E, $06, $06, $C6, $7C, $00, $00, $00, $00,  // char 1059
    $04, $24, {|}  $00, $00, $10, $7C, $D6, $D6, $D6, $D6, $D6, $7C, $10, $10, $00, $00, $00, $00,  // char 1060
    $04, $25, {|}  $00, $00, $C6, $C6, $6C, $7C, $38, $38, $7C, $6C, $C6, $C6, $00, $00, $00, $00,  // char 1061
    $04, $26, {|}  $00, $00, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $FE, $06, $02, $00, $00,  // char 1062
    $04, $27, {|}  $00, $00, $C6, $C6, $C6, $C6, $C6, $7E, $06, $06, $06, $06, $00, $00, $00, $00,  // char 1063
    $04, $28, {|}  $00, $00, $C6, $D6, $D6, $D6, $D6, $D6, $D6, $D6, $D6, $FE, $00, $00, $00, $00,  // char 1064
    $04, $29, {|}  $00, $00, $C6, $D6, $D6, $D6, $D6, $D6, $D6, $D6, $D6, $FF, $03, $01, $00, $00,  // char 1065
    $04, $2A, {|}  $00, $00, $F8, $B0, $B0, $30, $3C, $36, $36, $36, $36, $7C, $00, $00, $00, $00,  // char 1066
    $04, $2B, {|}  $00, $00, $C3, $C3, $C3, $C3, $F3, $DB, $DB, $DB, $DB, $F3, $00, $00, $00, $00,  // char 1067
    $04, $2C, {|}  $00, $00, $F0, $60, $60, $60, $7C, $66, $66, $66, $66, $FC, $00, $00, $00, $00,  // char 1068
    $04, $2D, {|}  $00, $00, $78, $CC, $86, $06, $3E, $06, $06, $86, $CC, $78, $00, $00, $00, $00,  // char 1069
    $04, $2E, {|}  $00, $00, $9C, $B6, $B6, $B6, $F6, $B6, $B6, $B6, $B6, $9C, $00, $00, $00, $00,  // char 1070
    $04, $2F, {|}  $00, $00, $7E, $CC, $CC, $CC, $7C, $6C, $6C, $6C, $6C, $CE, $00, $00, $00, $00,  // char 1071
    $04, $30, {|}  $00, $00, $00, $00, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 1072
    $04, $31, {|}  $00, $00, $06, $7C, $C0, $C0, $FC, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1073
    $04, $32, {|}  $00, $00, $00, $00, $00, $FC, $66, $66, $7C, $66, $66, $FC, $00, $00, $00, $00,  // char 1074
    $04, $33, {|}  $00, $00, $00, $00, $00, $FE, $66, $62, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 1075
    $04, $34, {|}  $00, $00, $00, $00, $00, $3C, $6C, $6C, $6C, $6C, $6C, $FE, $C6, $82, $00, $00,  // char 1076
    $04, $35, {|}  $00, $00, $00, $00, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 1077
    $04, $36, {|}  $00, $00, $00, $00, $00, $D6, $D6, $D6, $7C, $D6, $D6, $D6, $00, $00, $00, $00,  // char 1078
    $04, $37, {|}  $00, $00, $00, $00, $00, $7C, $C6, $06, $3C, $06, $C6, $7C, $00, $00, $00, $00,  // char 1079
    $04, $38, {|}  $00, $00, $00, $00, $00, $C6, $CE, $DE, $FE, $F6, $E6, $C6, $00, $00, $00, $00,  // char 1080
    $04, $39, {|}  $00, $00, $6C, $38, $00, $C6, $CE, $DE, $FE, $F6, $E6, $C6, $00, $00, $00, $00,  // char 1081
    $04, $3A, {|}  $00, $00, $00, $00, $00, $E6, $6C, $78, $78, $6C, $66, $E6, $00, $00, $00, $00,  // char 1082
    $04, $3B, {|}  $00, $00, $00, $00, $00, $3E, $66, $66, $66, $66, $66, $C6, $00, $00, $00, $00,  // char 1083
    $04, $3C, {|}  $00, $00, $00, $00, $00, $C6, $EE, $FE, $FE, $D6, $C6, $C6, $00, $00, $00, $00,  // char 1084
    $04, $3D, {|}  $00, $00, $00, $00, $00, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 1085
    $04, $3E, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1086
    $04, $3F, {|}  $00, $00, $00, $00, $00, $FE, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 1087
    $04, $40, {|}  $00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $7C, $60, $60, $F0, $00,  // char 1088
    $04, $41, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C0, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 1089
    $04, $42, {|}  $00, $00, $00, $00, $00, $7E, $5A, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 1090
    $04, $43, {|}  $00, $00, $00, $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00,  // char 1091
    $04, $44, {|}  $00, $00, $00, $00, $10, $7C, $D6, $D6, $D6, $D6, $D6, $7C, $10, $10, $10, $00,  // char 1092
    $04, $45, {|}  $00, $00, $00, $00, $00, $C6, $6C, $38, $38, $38, $6C, $C6, $00, $00, $00, $00,  // char 1093
    $04, $46, {|}  $00, $00, $00, $00, $00, $CC, $CC, $CC, $CC, $CC, $CC, $FE, $06, $02, $00, $00,  // char 1094
    $04, $47, {|}  $00, $00, $00, $00, $00, $C6, $C6, $C6, $7E, $06, $06, $06, $00, $00, $00, $00,  // char 1095
    $04, $48, {|}  $00, $00, $00, $00, $00, $C6, $D6, $D6, $D6, $D6, $D6, $FE, $00, $00, $00, $00,  // char 1096
    $04, $49, {|}  $00, $00, $00, $00, $00, $C6, $D6, $D6, $D6, $D6, $D6, $FF, $03, $01, $00, $00,  // char 1097
    $04, $4A, {|}  $00, $00, $00, $00, $00, $F8, $B0, $30, $3C, $36, $36, $7C, $00, $00, $00, $00,  // char 1098
    $04, $4B, {|}  $00, $00, $00, $00, $00, $C3, $C3, $C3, $F3, $DB, $DB, $F3, $00, $00, $00, $00,  // char 1099
    $04, $4C, {|}  $00, $00, $00, $00, $00, $F0, $60, $60, $7C, $66, $66, $FC, $00, $00, $00, $00,  // char 1100
    $04, $4D, {|}  $00, $00, $00, $00, $00, $7C, $C6, $06, $3E, $06, $C6, $7C, $00, $00, $00, $00,  // char 1101
    $04, $4E, {|}  $00, $00, $00, $00, $00, $9C, $B6, $B6, $F6, $B6, $B6, $9C, $00, $00, $00, $00,  // char 1102
    $04, $4F, {|}  $00, $00, $00, $00, $00, $7E, $CC, $CC, $7C, $6C, $6C, $CE, $00, $00, $00, $00,  // char 1103
    $04, $50, {|}  $00, $00, $60, $30, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 1104
    $04, $51, {|}  $00, $00, $6C, $6C, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 1105
    $04, $52, {|}  $00, $00, $60, $F8, $60, $6C, $76, $66, $66, $66, $66, $E6, $06, $06, $1C, $00,  // char 1106
    $04, $53, {|}  $00, $00, $0C, $18, $00, $FE, $66, $62, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 1107
    $04, $54, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C0, $F8, $C0, $C6, $7C, $00, $00, $00, $00,  // char 1108
    $04, $55, {|}  $00, $00, $00, $00, $00, $7C, $C6, $60, $38, $0C, $C6, $7C, $00, $00, $00, $00,  // char 1109
    $04, $56, {|}  $00, $00, $18, $18, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 1110
    $04, $57, {|}  $00, $00, $66, $66, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 1111
    $04, $58, {|}  $00, $00, $06, $06, $00, $0E, $06, $06, $06, $06, $06, $06, $66, $66, $3C, $00,  // char 1112
    $04, $59, {|}  $00, $00, $00, $00, $00, $78, $D8, $D8, $DE, $DB, $DB, $DE, $00, $00, $00, $00,  // char 1113
    $04, $5A, {|}  $00, $00, $00, $00, $00, $D8, $D8, $D8, $FE, $DB, $DB, $DE, $00, $00, $00, $00,  // char 1114
    $04, $5B, {|}  $00, $00, $60, $F8, $60, $6C, $76, $66, $66, $66, $66, $E6, $00, $00, $00, $00,  // char 1115
    $04, $5C, {|}  $00, $00, $0C, $18, $00, $E6, $6C, $78, $78, $6C, $66, $E6, $00, $00, $00, $00,  // char 1116
    $04, $5D, {|}  $00, $00, $60, $30, $00, $C6, $CE, $DE, $FE, $F6, $E6, $C6, $00, $00, $00, $00,  // char 1117
    $04, $5E, {|}  $00, $00, $6C, $38, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00,  // char 1118
    $04, $5F, {|}  $00, $00, $00, $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $FE, $38, $10, $10, $00,  // char 1119
    $04, $60, {|}  $00, $00, $6C, $C6, $C6, $D6, $D6, $D6, $D6, $D6, $FE, $6C, $00, $00, $00, $00,  // char 1120
    $04, $61, {|}  $00, $00, $00, $00, $00, $6C, $C6, $D6, $D6, $D6, $FE, $6C, $00, $00, $00, $00,  // char 1121
    $04, $62, {|}  $00, $00, $30, $FC, $B4, $30, $3C, $36, $36, $36, $36, $7C, $00, $00, $00, $00,  // char 1122
    $04, $63, {|}  $00, $00, $30, $30, $30, $FC, $B4, $30, $3C, $36, $36, $7C, $00, $00, $00, $00,  // char 1123
    $04, $64, {|}  $00, $00, $CE, $DB, $D9, $D8, $FE, $D8, $D8, $D9, $DB, $CE, $00, $00, $00, $00,  // char 1124
    $04, $65, {|}  $00, $00, $00, $00, $00, $CE, $DB, $D8, $FE, $D8, $DB, $CE, $00, $00, $00, $00,  // char 1125
    $04, $66, {|}  $00, $00, $10, $38, $38, $6C, $6C, $6C, $FE, $D6, $D6, $D6, $00, $00, $00, $00,  // char 1126
    $04, $67, {|}  $00, $00, $00, $00, $00, $10, $38, $38, $6C, $7C, $D6, $D6, $00, $00, $00, $00,  // char 1127
    $04, $68, {|}  $00, $00, $88, $8C, $9C, $96, $F6, $B6, $BF, $AB, $EB, $EB, $00, $00, $00, $00,  // char 1128
    $04, $69, {|}  $00, $00, $00, $00, $00, $88, $8C, $9C, $F6, $BE, $AB, $EB, $00, $00, $00, $00,  // char 1129
    $04, $6A, {|}  $00, $00, $FE, $C6, $6C, $6C, $38, $7C, $D6, $D6, $D6, $D6, $00, $00, $00, $00,  // char 1130
    $04, $6B, {|}  $00, $00, $00, $00, $00, $FE, $C6, $6C, $7C, $D6, $D6, $D6, $00, $00, $00, $00,  // char 1131
    $04, $6C, {|}  $00, $00, $FF, $A3, $B2, $96, $FC, $9C, $BE, $AA, $AB, $EB, $00, $00, $00, $00,  // char 1132
    $04, $6D, {|}  $00, $00, $00, $00, $00, $BF, $A3, $96, $FE, $AB, $AB, $EB, $00, $00, $00, $00,  // char 1133
    $04, $6E, {|}  $6C, $38, $10, $7C, $C6, $06, $06, $7C, $06, $06, $06, $7C, $C0, $7C, $00, $00,  // char 1134
    $04, $6F, {|}  $00, $6C, $38, $10, $00, $7C, $86, $06, $7C, $06, $06, $7C, $C0, $7C, $00, $00,  // char 1135
    $04, $70, {|}  $00, $00, $10, $96, $D6, $D6, $D6, $D6, $D6, $7C, $10, $10, $00, $00, $00, $00,  // char 1136
    $04, $71, {|}  $00, $00, $10, $10, $10, $96, $D6, $D6, $D6, $D6, $D6, $7C, $10, $10, $10, $00,  // char 1137
    $04, $72, {|}  $00, $00, $7C, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1138
    $04, $73, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C6, $FE, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1139
    $04, $74, {|}  $00, $00, $C2, $C6, $C6, $C4, $CC, $6C, $68, $78, $38, $30, $00, $00, $00, $00,  // char 1140
    $04, $75, {|}  $00, $00, $00, $00, $00, $C2, $C6, $64, $6C, $38, $38, $10, $00, $00, $00, $00,  // char 1141
    $04, $76, {|}  $CC, $66, $00, $C2, $C6, $C4, $C4, $6C, $68, $78, $38, $30, $00, $00, $00, $00,  // char 1142
    $04, $77, {|}  $00, $00, $CC, $66, $00, $C2, $C6, $64, $6C, $38, $38, $10, $00, $00, $00, $00,  // char 1143
    $04, $78, {|}  $00, $00, $70, $D8, $D8, $DB, $DB, $DB, $DB, $DB, $DB, $6F, $03, $06, $1C, $00,  // char 1144
    $04, $79, {|}  $00, $00, $00, $00, $00, $73, $DB, $DB, $DB, $DB, $DB, $6F, $03, $06, $1C, $00,  // char 1145
    $04, $7A, {|}  $00, $10, $7C, $D6, $C6, $C6, $C6, $C6, $C6, $C6, $D6, $7C, $10, $00, $00, $00,  // char 1146
    $04, $7B, {|}  $00, $00, $00, $00, $10, $7C, $D6, $C6, $C6, $C6, $D6, $7C, $10, $00, $00, $00,  // char 1147
    $04, $7C, {|}  $04, $7C, $40, $6C, $C6, $C6, $D6, $D6, $D6, $D6, $FE, $6C, $00, $00, $00, $00,  // char 1148
    $04, $7D, {|}  $00, $04, $7C, $40, $00, $6C, $C6, $D6, $D6, $D6, $FE, $6C, $00, $00, $00, $00,  // char 1149
    $04, $7E, {|}  $7C, $54, $00, $6C, $C6, $C6, $D6, $D6, $D6, $D6, $FE, $6C, $00, $00, $00, $00,  // char 1150
    $04, $7F, {|}  $00, $00, $7C, $54, $00, $6C, $C6, $D6, $D6, $D6, $FE, $6C, $00, $00, $00, $00,  // char 1151
    $04, $80, {|}  $00, $00, $3C, $66, $C2, $C0, $C0, $C0, $C0, $C0, $60, $3C, $0C, $0C, $0C, $00,  // char 1152
    $04, $81, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C0, $C0, $C0, $C0, $78, $18, $18, $00, $00,  // char 1153
    $04, $82, {|}  $00, $00, $06, $06, $3C, $0F, $18, $18, $F0, $3C, $60, $60, $00, $00, $00, $00,  // char 1154
    $04, $83, {|}  $00, $04, $7C, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1155
    $04, $84, {|}  $00, $18, $24, $44, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1156
    $04, $85, {|}  $00, $40, $7C, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1157
    $04, $86, {|}  $00, $04, $7C, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1158
    $04, $88, {|}  $00, $00, $00, $18, $66, $00, $C3, $00, $66, $18, $00, $00, $00, $00, $00, $00,  // char 1160
    $04, $89, {|}  $00, $00, $08, $50, $46, $00, $82, $41, $00, $62, $0A, $10, $00, $00, $00, $00,  // char 1161
    $04, $8A, {|}  $6C, $38, $C6, $C6, $C6, $CE, $DE, $FE, $F6, $E6, $C6, $C7, $03, $01, $00, $00,  // char 1162
    $04, $8B, {|}  $00, $00, $6C, $38, $00, $C6, $CE, $DE, $FE, $F6, $E6, $C7, $03, $01, $00, $00,  // char 1163
    $04, $8C, {|}  $00, $60, $F0, $60, $60, $60, $7C, $66, $66, $66, $66, $FC, $00, $00, $00, $00,  // char 1164
    $04, $8D, {|}  $00, $00, $00, $00, $60, $F0, $60, $60, $7C, $66, $66, $FC, $00, $00, $00, $00,  // char 1165
    $04, $8E, {|}  $00, $00, $FC, $66, $6E, $64, $7A, $60, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 1166
    $04, $8F, {|}  $00, $00, $00, $00, $00, $DC, $66, $66, $66, $6E, $64, $7A, $60, $60, $F0, $00,  // char 1167
    $04, $90, {|}  $02, $06, $FE, $60, $60, $60, $60, $60, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 1168
    $04, $91, {|}  $00, $00, $00, $02, $06, $FE, $60, $60, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 1169
    $04, $92, {|}  $00, $00, $FE, $66, $62, $60, $F8, $60, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 1170
    $04, $93, {|}  $00, $00, $00, $00, $00, $FE, $66, $62, $F8, $60, $60, $F0, $00, $00, $00, $00,  // char 1171
    $04, $94, {|}  $00, $00, $FE, $66, $62, $60, $7C, $66, $66, $66, $66, $F6, $06, $16, $0C, $00,  // char 1172
    $04, $95, {|}  $00, $00, $00, $00, $00, $FE, $66, $62, $78, $6C, $66, $F6, $06, $16, $0C, $00,  // char 1173
    $04, $96, {|}  $00, $00, $D6, $D6, $D6, $7C, $38, $7C, $D6, $D6, $D6, $D7, $03, $01, $00, $00,  // char 1174
    $04, $97, {|}  $00, $00, $00, $00, $00, $D6, $D6, $D6, $7C, $D6, $D6, $D7, $03, $01, $00, $00,  // char 1175
    $04, $98, {|}  $00, $00, $7C, $C6, $86, $06, $3C, $06, $06, $86, $C6, $7C, $30, $60, $38, $00,  // char 1176
    $04, $99, {|}  $00, $00, $00, $00, $00, $7C, $C6, $06, $3C, $06, $C6, $7C, $30, $60, $38, $00,  // char 1177
    $04, $9A, {|}  $00, $00, $E6, $66, $66, $6C, $78, $78, $6C, $66, $66, $E7, $03, $01, $00, $00,  // char 1178
    $04, $9B, {|}  $00, $00, $00, $00, $00, $E6, $6C, $78, $78, $6C, $66, $E7, $03, $01, $00, $00,  // char 1179
    $04, $9C, {|}  $00, $00, $C6, $D6, $D6, $DC, $F8, $DC, $D6, $D6, $C6, $C6, $00, $00, $00, $00,  // char 1180
    $04, $9D, {|}  $00, $00, $00, $00, $00, $C6, $D6, $DC, $F8, $DC, $D6, $C6, $00, $00, $00, $00,  // char 1181
    $04, $9E, {|}  $00, $00, $E6, $66, $F6, $6C, $78, $78, $6C, $66, $66, $E6, $00, $00, $00, $00,  // char 1182
    $04, $9F, {|}  $00, $00, $E0, $60, $F0, $66, $6C, $78, $78, $6C, $66, $E6, $00, $00, $00, $00,  // char 1183
    $04, $A0, {|}  $00, $00, $F3, $B3, $B3, $36, $3C, $3C, $36, $33, $33, $73, $00, $00, $00, $00,  // char 1184
    $04, $A1, {|}  $00, $00, $00, $00, $00, $F3, $B6, $BC, $3C, $36, $33, $73, $00, $00, $00, $00,  // char 1185
    $04, $A2, {|}  $00, $00, $C6, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $C7, $03, $01, $00, $00,  // char 1186
    $04, $A3, {|}  $00, $00, $00, $00, $00, $C6, $C6, $C6, $FE, $C6, $C6, $C7, $03, $01, $00, $00,  // char 1187
    $04, $A4, {|}  $00, $00, $DF, $DB, $D9, $D8, $F8, $D8, $D8, $D8, $D8, $D8, $00, $00, $00, $00,  // char 1188
    $04, $A5, {|}  $00, $00, $00, $00, $00, $DF, $DB, $D9, $F8, $D8, $D8, $D8, $00, $00, $00, $00,  // char 1189
    $04, $A6, {|}  $00, $00, $F8, $D8, $D8, $D8, $DE, $DB, $DB, $DB, $DB, $DB, $03, $0B, $06, $00,  // char 1190
    $04, $A7, {|}  $00, $00, $00, $00, $00, $D8, $D8, $D8, $FE, $DB, $DB, $DB, $03, $0B, $06, $00,  // char 1191
    $04, $A8, {|}  $00, $00, $7C, $C2, $CC, $D6, $D6, $D6, $D6, $D6, $CC, $7A, $00, $00, $00, $00,  // char 1192
    $04, $A9, {|}  $00, $00, $00, $00, $00, $7C, $C2, $CC, $D6, $D6, $CC, $7A, $00, $00, $00, $00,  // char 1193
    $04, $AA, {|}  $00, $00, $3C, $66, $C2, $C0, $C0, $C0, $C0, $C2, $66, $3C, $18, $0C, $38, $00,  // char 1194
    $04, $AB, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C0, $C0, $C0, $C6, $7C, $18, $0C, $38, $00,  // char 1195
    $04, $AC, {|}  $00, $00, $7E, $7E, $5A, $18, $18, $18, $18, $18, $18, $3C, $0C, $04, $00, $00,  // char 1196
    $04, $AD, {|}  $00, $00, $00, $00, $00, $7E, $5A, $18, $18, $18, $18, $3C, $0C, $04, $00, $00,  // char 1197
    $04, $AE, {|}  $00, $00, $66, $66, $66, $66, $3C, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 1198
    $04, $AF, {|}  $00, $00, $00, $00, $00, $66, $66, $66, $66, $66, $3C, $18, $18, $18, $3C, $00,  // char 1199
    $04, $B0, {|}  $00, $00, $66, $66, $66, $66, $3C, $18, $7E, $18, $18, $3C, $00, $00, $00, $00,  // char 1200
    $04, $B1, {|}  $00, $00, $00, $00, $00, $66, $66, $66, $66, $66, $3C, $18, $7E, $18, $3C, $00,  // char 1201
    $04, $B2, {|}  $00, $00, $C6, $C6, $6C, $7C, $38, $38, $7C, $6C, $C6, $C7, $03, $01, $00, $00,  // char 1202
    $04, $B3, {|}  $00, $00, $00, $00, $00, $C6, $6C, $38, $38, $38, $6C, $C7, $03, $01, $00, $00,  // char 1203
    $04, $B4, {|}  $00, $00, $F6, $66, $66, $66, $66, $66, $66, $66, $66, $7F, $03, $01, $00, $00,  // char 1204
    $04, $B5, {|}  $00, $00, $00, $00, $00, $F6, $66, $66, $66, $66, $66, $7F, $03, $01, $00, $00,  // char 1205
    $04, $B6, {|}  $00, $00, $C6, $C6, $C6, $C6, $C6, $7E, $06, $06, $06, $07, $03, $01, $00, $00,  // char 1206
    $04, $B7, {|}  $00, $00, $00, $00, $00, $C6, $C6, $C6, $7E, $06, $06, $07, $03, $01, $00, $00,  // char 1207
    $04, $B8, {|}  $00, $00, $C6, $C6, $C6, $D6, $D6, $7E, $16, $16, $06, $06, $00, $00, $00, $00,  // char 1208
    $04, $B9, {|}  $00, $00, $00, $00, $00, $C6, $D6, $D6, $7E, $16, $16, $06, $00, $00, $00, $00,  // char 1209
    $04, $BA, {|}  $00, $00, $C0, $C0, $C0, $C0, $FC, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 1210
    $04, $BB, {|}  $00, $00, $00, $00, $00, $C0, $C0, $C0, $FC, $C6, $C6, $C6, $00, $00, $00, $00,  // char 1211
    $04, $BC, {|}  $00, $00, $1E, $33, $B3, $B3, $7F, $30, $30, $30, $33, $1E, $00, $00, $00, $00,  // char 1212
    $04, $BD, {|}  $00, $00, $00, $00, $00, $9E, $B3, $7F, $30, $30, $33, $1E, $00, $00, $00, $00,  // char 1213
    $04, $BE, {|}  $00, $00, $1E, $33, $B3, $B3, $7F, $30, $30, $30, $33, $1E, $0C, $18, $0E, $00,  // char 1214
    $04, $BF, {|}  $00, $00, $00, $00, $00, $9E, $B3, $7F, $30, $30, $33, $1E, $0C, $18, $0E, $00,  // char 1215
    $04, $C0, {|}  $00, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 1216
    $04, $C1, {|}  $6C, $38, $D6, $D6, $D6, $7C, $38, $7C, $D6, $D6, $D6, $D6, $00, $00, $00, $00,  // char 1217
    $04, $C2, {|}  $00, $00, $6C, $38, $00, $D6, $D6, $D6, $7C, $D6, $D6, $D6, $00, $00, $00, $00,  // char 1218
    $04, $C3, {|}  $00, $00, $E6, $66, $66, $6C, $78, $78, $6C, $66, $66, $E6, $06, $16, $0C, $00,  // char 1219
    $04, $C4, {|}  $00, $00, $00, $00, $00, $E6, $6C, $78, $78, $6C, $66, $E6, $06, $16, $0C, $00,  // char 1220
    $04, $C5, {|}  $00, $00, $3E, $66, $66, $66, $66, $66, $66, $66, $66, $C7, $03, $01, $00, $00,  // char 1221
    $04, $C6, {|}  $00, $00, $00, $00, $00, $3E, $66, $66, $66, $66, $66, $C7, $03, $01, $00, $00,  // char 1222
    $04, $C7, {|}  $00, $00, $C6, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $C6, $06, $16, $0C, $00,  // char 1223
    $04, $C8, {|}  $00, $00, $00, $00, $00, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $06, $16, $0C, $00,  // char 1224
    $04, $C9, {|}  $00, $00, $C6, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $C7, $03, $01, $00, $00,  // char 1225
    $04, $CA, {|}  $00, $00, $00, $00, $00, $C6, $C6, $C6, $FE, $C6, $C6, $C7, $03, $01, $00, $00,  // char 1226
    $04, $CB, {|}  $00, $00, $C6, $C6, $C6, $C6, $C6, $7E, $06, $06, $06, $0E, $0C, $08, $00, $00,  // char 1227
    $04, $CC, {|}  $00, $00, $00, $00, $00, $C6, $C6, $C6, $7E, $06, $06, $0E, $0C, $08, $00, $00,  // char 1228
    $04, $CD, {|}  $00, $00, $C6, $EE, $FE, $FE, $D6, $C6, $C6, $C6, $C6, $C7, $03, $01, $00, $00,  // char 1229
    $04, $CE, {|}  $00, $00, $00, $00, $00, $C6, $EE, $FE, $FE, $D6, $C6, $C7, $03, $01, $00, $00,  // char 1230
    $04, $D0, {|}  $6C, $38, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 1232
    $04, $D1, {|}  $00, $00, $6C, $38, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 1233
    $04, $D2, {|}  $6C, $6C, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 1234
    $04, $D3, {|}  $00, $00, $6C, $6C, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 1235
    $04, $D4, {|}  $00, $00, $3E, $6C, $CC, $CC, $FE, $CC, $CC, $CC, $CC, $CE, $00, $00, $00, $00,  // char 1236
    $04, $D5, {|}  $00, $00, $00, $00, $00, $CC, $76, $36, $7E, $D8, $D8, $6E, $00, $00, $00, $00,  // char 1237
    $04, $D6, {|}  $6C, $38, $00, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $00, $00, $00, $00,  // char 1238
    $04, $D7, {|}  $00, $00, $6C, $38, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 1239
    $04, $D8, {|}  $00, $00, $7C, $C6, $06, $06, $06, $FE, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1240
    $04, $D9, {|}  $00, $00, $00, $00, $00, $7C, $C6, $06, $06, $FE, $C6, $7C, $00, $00, $00, $00,  // char 1241
    $04, $DA, {|}  $6C, $6C, $00, $7C, $C6, $06, $06, $FE, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1242
    $04, $DB, {|}  $00, $00, $6C, $6C, $00, $7C, $C6, $06, $06, $FE, $C6, $7C, $00, $00, $00, $00,  // char 1243
    $04, $DC, {|}  $6C, $6C, $00, $D6, $D6, $D6, $7C, $38, $7C, $D6, $D6, $D6, $00, $00, $00, $00,  // char 1244
    $04, $DD, {|}  $00, $00, $6C, $6C, $00, $D6, $D6, $D6, $7C, $D6, $D6, $D6, $00, $00, $00, $00,  // char 1245
    $04, $DE, {|}  $6C, $6C, $00, $7C, $C6, $86, $06, $3C, $06, $86, $C6, $7C, $00, $00, $00, $00,  // char 1246
    $04, $DF, {|}  $00, $00, $6C, $6C, $00, $7C, $C6, $06, $3C, $06, $C6, $7C, $00, $00, $00, $00,  // char 1247
    $04, $E0, {|}  $00, $00, $FE, $06, $0C, $18, $3C, $06, $06, $86, $C6, $7C, $00, $00, $00, $00,  // char 1248
    $04, $E1, {|}  $00, $00, $00, $00, $00, $FE, $06, $0C, $18, $3C, $06, $06, $06, $C6, $7C, $00,  // char 1249
    $04, $E2, {|}  $00, $7C, $00, $C6, $C6, $CE, $DE, $FE, $F6, $E6, $C6, $C6, $00, $00, $00, $00,  // char 1250
    $04, $E3, {|}  $00, $00, $00, $7C, $00, $C6, $CE, $DE, $FE, $F6, $E6, $C6, $00, $00, $00, $00,  // char 1251
    $04, $E4, {|}  $6C, $6C, $00, $C6, $C6, $CE, $DE, $FE, $F6, $E6, $C6, $C6, $00, $00, $00, $00,  // char 1252
    $04, $E5, {|}  $00, $00, $6C, $6C, $00, $C6, $CE, $DE, $FE, $F6, $E6, $C6, $00, $00, $00, $00,  // char 1253
    $04, $E6, {|}  $6C, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1254
    $04, $E7, {|}  $00, $00, $6C, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1255
    $04, $E8, {|}  $00, $00, $7C, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1256
    $04, $E9, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C6, $FE, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1257
    $04, $EA, {|}  $6C, $6C, $00, $7C, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1258
    $04, $EB, {|}  $00, $00, $6C, $6C, $00, $7C, $C6, $C6, $FE, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1259
    $04, $EC, {|}  $6C, $6C, $00, $78, $CC, $86, $06, $3E, $06, $86, $CC, $78, $00, $00, $00, $00,  // char 1260
    $04, $ED, {|}  $00, $00, $6C, $6C, $00, $7C, $C6, $06, $3E, $06, $C6, $7C, $00, $00, $00, $00,  // char 1261
    $04, $EE, {|}  $00, $7C, $00, $C6, $C6, $C6, $C6, $7E, $06, $06, $C6, $7C, $00, $00, $00, $00,  // char 1262
    $04, $EF, {|}  $00, $00, $00, $7C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00,  // char 1263
    $04, $F0, {|}  $6C, $6C, $00, $C6, $C6, $C6, $C6, $7E, $06, $06, $C6, $7C, $00, $00, $00, $00,  // char 1264
    $04, $F1, {|}  $00, $00, $6C, $6C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00,  // char 1265
    $04, $F2, {|}  $66, $CC, $00, $C6, $C6, $C6, $C6, $7E, $06, $06, $C6, $7C, $00, $00, $00, $00,  // char 1266
    $04, $F3, {|}  $00, $00, $66, $CC, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00,  // char 1267
    $04, $F4, {|}  $6C, $6C, $00, $C6, $C6, $C6, $C6, $7E, $06, $06, $06, $06, $00, $00, $00, $00,  // char 1268
    $04, $F5, {|}  $00, $00, $6C, $6C, $00, $C6, $C6, $C6, $7E, $06, $06, $06, $00, $00, $00, $00,  // char 1269
    $04, $F8, {|}  $6C, $6C, $00, $C3, $C3, $C3, $F3, $DB, $DB, $DB, $DB, $F3, $00, $00, $00, $00,  // char 1272
    $04, $F9, {|}  $00, $00, $6C, $6C, $00, $C3, $C3, $C3, $F3, $DB, $DB, $F3, $00, $00, $00, $00,  // char 1273
    $05, $00, {|}  $00, $00, $1E, $0C, $0C, $0C, $7C, $CC, $CC, $CC, $CC, $7E, $00, $00, $00, $00,  // char 1280
    $05, $01, {|}  $00, $00, $1C, $0C, $0C, $3C, $6C, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 1281
    $05, $02, {|}  $00, $00, $1E, $0C, $0C, $0C, $7C, $CD, $CD, $CD, $CD, $76, $00, $00, $00, $00,  // char 1282
    $05, $03, {|}  $00, $00, $1C, $0C, $0C, $3C, $6C, $CC, $CD, $CD, $CD, $76, $00, $00, $00, $00,  // char 1283
    $05, $04, {|}  $00, $00, $78, $CC, $8C, $0C, $38, $0D, $0D, $0D, $0D, $06, $00, $00, $00, $00,  // char 1284
    $05, $05, {|}  $00, $00, $00, $00, $00, $78, $CC, $0C, $39, $0D, $0D, $06, $00, $00, $00, $00,  // char 1285
    $05, $06, {|}  $00, $00, $7C, $C6, $86, $06, $3C, $06, $06, $06, $06, $07, $03, $01, $00, $00,  // char 1286
    $05, $07, {|}  $00, $00, $00, $00, $00, $7C, $C6, $06, $3C, $06, $06, $07, $03, $01, $00, $00,  // char 1287
    $05, $08, {|}  $00, $00, $3C, $6C, $6C, $6C, $6C, $6D, $6D, $6D, $6D, $C6, $00, $00, $00, $00,  // char 1288
    $05, $09, {|}  $00, $00, $00, $00, $00, $3C, $6C, $6C, $6D, $6D, $6D, $C6, $00, $00, $00, $00,  // char 1289
    $05, $0A, {|}  $00, $00, $CC, $CC, $CC, $CC, $FC, $CD, $CD, $CD, $CD, $C6, $00, $00, $00, $00,  // char 1290
    $05, $0B, {|}  $00, $00, $00, $00, $00, $CC, $CC, $CC, $FD, $CD, $CD, $C6, $00, $00, $00, $00,  // char 1291
    $05, $0C, {|}  $00, $00, $3C, $66, $C2, $C0, $C0, $CE, $C6, $C6, $66, $3C, $00, $00, $00, $00,  // char 1292
    $05, $0D, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C0, $CE, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1293
    $05, $0E, {|}  $00, $00, $7E, $7E, $5A, $18, $18, $1B, $1B, $1B, $1B, $0E, $00, $00, $00, $00,  // char 1294
    $05, $0F, {|}  $00, $00, $00, $00, $00, $7E, $5A, $18, $1B, $1B, $1B, $0E, $00, $00, $00, $00,  // char 1295
    $05, $30, {|}  $00, $00, $00, $00, $18, $64, $94, $BA, $52, $4C, $30, $00, $00, $00, $00, $00,  // char 1328
    $05, $31, {|}  $00, $00, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CF, $7B, $00, $00, $00, $00,  // char 1329
    $05, $32, {|}  $00, $00, $3C, $66, $66, $66, $66, $60, $60, $7E, $60, $60, $00, $00, $00, $00,  // char 1330
    $05, $33, {|}  $00, $00, $78, $CC, $CC, $CC, $CC, $7F, $0C, $0C, $0C, $0C, $00, $00, $00, $00,  // char 1331
    $05, $34, {|}  $00, $00, $78, $CC, $CC, $CC, $CC, $CF, $0C, $0C, $0C, $0C, $00, $00, $00, $00,  // char 1332
    $05, $35, {|}  $00, $00, $60, $7E, $60, $60, $66, $66, $66, $66, $66, $3C, $00, $00, $00, $00,  // char 1333
    $05, $36, {|}  $00, $00, $3C, $66, $66, $66, $66, $3E, $06, $0C, $D8, $FE, $C3, $00, $00, $00,  // char 1334
    $05, $37, {|}  $00, $00, $60, $60, $7E, $60, $60, $60, $60, $60, $60, $7C, $06, $00, $00, $00,  // char 1335
    $05, $38, {|}  $00, $00, $3C, $66, $66, $66, $66, $66, $60, $60, $60, $7E, $00, $00, $00, $00,  // char 1336
    $05, $39, {|}  $00, $00, $7C, $C6, $CF, $D6, $D6, $D6, $D6, $CC, $C0, $C0, $00, $00, $00, $00,  // char 1337
    $05, $3A, {|}  $00, $00, $0C, $0C, $0C, $0C, $7F, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 1338
    $05, $3B, {|}  $00, $00, $60, $60, $7C, $66, $66, $66, $66, $66, $60, $60, $00, $00, $00, $00,  // char 1339
    $05, $3C, {|}  $00, $00, $60, $60, $60, $60, $60, $60, $60, $60, $60, $7C, $06, $00, $00, $00,  // char 1340
    $05, $3D, {|}  $00, $00, $C0, $C0, $F6, $D6, $D6, $D6, $D6, $CC, $C0, $C0, $00, $00, $00, $00,  // char 1341
    $05, $3E, {|}  $00, $00, $C0, $7F, $24, $66, $66, $66, $66, $66, $66, $3C, $00, $00, $00, $00,  // char 1342
    $05, $3F, {|}  $00, $00, $60, $60, $66, $66, $66, $66, $66, $3E, $06, $06, $00, $00, $00, $00,  // char 1343
    $05, $40, {|}  $00, $00, $18, $30, $18, $0C, $18, $30, $60, $F0, $3C, $0E, $04, $00, $00, $00,  // char 1344
    $05, $41, {|}  $00, $00, $3C, $66, $66, $66, $06, $06, $06, $6C, $D8, $6E, $00, $00, $00, $00,  // char 1345
    $05, $42, {|}  $00, $00, $78, $CC, $CC, $CC, $CC, $CC, $0C, $0C, $0C, $0F, $00, $00, $00, $00,  // char 1346
    $05, $43, {|}  $00, $00, $1E, $30, $60, $F8, $6C, $66, $66, $66, $66, $7E, $00, $00, $00, $00,  // char 1347
    $05, $44, {|}  $00, $00, $CF, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 1348
    $05, $45, {|}  $00, $00, $3C, $66, $06, $06, $7C, $06, $06, $66, $66, $3C, $00, $00, $00, $00,  // char 1349
    $05, $46, {|}  $00, $00, $E0, $60, $60, $60, $66, $66, $66, $66, $66, $3C, $00, $00, $00, $00,  // char 1350
    $05, $47, {|}  $00, $00, $3C, $06, $3E, $66, $60, $60, $60, $66, $66, $3C, $00, $00, $00, $00,  // char 1351
    $05, $48, {|}  $00, $00, $3C, $66, $66, $66, $66, $66, $66, $66, $66, $66, $00, $00, $00, $00,  // char 1352
    $05, $49, {|}  $00, $00, $3C, $66, $66, $66, $06, $06, $06, $6C, $38, $0E, $00, $00, $00, $00,  // char 1353
    $05, $4A, {|}  $00, $00, $7C, $D6, $D6, $D6, $D6, $D6, $16, $16, $06, $06, $00, $00, $00, $00,  // char 1354
    $05, $4B, {|}  $00, $00, $3C, $66, $66, $66, $76, $1E, $0E, $0C, $D8, $FE, $C3, $00, $00, $00,  // char 1355
    $05, $4C, {|}  $00, $00, $78, $CC, $CC, $CC, $CC, $CF, $CC, $CC, $CC, $CC, $00, $00, $00, $00,  // char 1356
    $05, $4D, {|}  $00, $00, $66, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $00, $00, $00, $00,  // char 1357
    $05, $4E, {|}  $00, $00, $0C, $0C, $CC, $CC, $CC, $CC, $7C, $0C, $0C, $0F, $00, $00, $00, $00,  // char 1358
    $05, $4F, {|}  $00, $00, $7C, $C6, $C6, $60, $30, $18, $0C, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1359
    $05, $50, {|}  $00, $00, $3C, $66, $66, $66, $66, $66, $60, $60, $60, $60, $00, $00, $00, $00,  // char 1360
    $05, $51, {|}  $00, $00, $3C, $66, $66, $66, $3C, $66, $06, $06, $66, $3C, $00, $00, $00, $00,  // char 1361
    $05, $52, {|}  $00, $00, $60, $60, $7C, $66, $66, $66, $66, $63, $60, $60, $00, $00, $00, $00,  // char 1362
    $05, $53, {|}  $00, $00, $10, $7C, $D6, $D6, $D6, $D6, $D6, $D6, $7C, $10, $00, $00, $00, $00,  // char 1363
    $05, $54, {|}  $00, $00, $3C, $66, $66, $66, $7C, $60, $60, $FE, $60, $60, $00, $00, $00, $00,  // char 1364
    $05, $55, {|}  $00, $00, $3C, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $00, $00, $00, $00,  // char 1365
    $05, $56, {|}  $00, $00, $70, $D0, $D0, $D0, $7C, $16, $16, $16, $D6, $7C, $00, $00, $00, $00,  // char 1366
    $05, $59, {|}  $00, $18, $30, $30, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1369
    $05, $5A, {|}  $00, $30, $18, $18, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1370
    $05, $5B, {|}  $00, $18, $30, $60, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1371
    $05, $5C, {|}  $00, $06, $3C, $60, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1372
    $05, $5D, {|}  $00, $30, $18, $0C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1373
    $05, $5E, {|}  $00, $1C, $36, $6C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1374
    $05, $5F, {|}  $00, $70, $60, $3E, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1375
    $05, $61, {|}  $00, $00, $00, $00, $00, $D6, $D6, $D6, $D6, $D6, $D6, $6A, $00, $00, $00, $00,  // char 1377
    $05, $62, {|}  $00, $00, $00, $00, $00, $7C, $66, $66, $66, $60, $60, $7E, $60, $60, $60, $00,  // char 1378
    $05, $63, {|}  $00, $00, $00, $00, $00, $7C, $CC, $CC, $CC, $CC, $CC, $7F, $0C, $0C, $0C, $00,  // char 1379
    $05, $64, {|}  $00, $00, $00, $00, $00, $F8, $CC, $CC, $CC, $CC, $CC, $CF, $0C, $0C, $0C, $00,  // char 1380
    $05, $65, {|}  $00, $00, $60, $60, $7E, $60, $60, $66, $66, $66, $66, $3E, $00, $00, $00, $00,  // char 1381
    $05, $66, {|}  $00, $00, $00, $00, $00, $7C, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $0C, $0F, $00,  // char 1382
    $05, $67, {|}  $00, $00, $60, $60, $60, $7C, $60, $60, $60, $60, $60, $78, $0C, $18, $00, $00,  // char 1383
    $05, $68, {|}  $00, $00, $00, $00, $00, $7C, $66, $66, $66, $66, $66, $66, $60, $60, $7E, $00,  // char 1384
    $05, $69, {|}  $00, $00, $00, $00, $00, $FC, $C6, $C6, $CF, $D6, $D6, $CC, $C0, $C0, $C0, $00,  // char 1385
    $05, $6A, {|}  $00, $00, $0C, $0C, $0C, $7F, $CC, $CC, $CC, $CC, $CC, $7C, $00, $00, $00, $00,  // char 1386
    $05, $6B, {|}  $00, $00, $60, $60, $60, $7C, $66, $66, $66, $66, $66, $66, $60, $60, $60, $00,  // char 1387
    $05, $6C, {|}  $00, $00, $00, $00, $00, $30, $30, $30, $30, $30, $30, $30, $30, $30, $3E, $00,  // char 1388
    $05, $6D, {|}  $00, $00, $C0, $C0, $C0, $F6, $D6, $D6, $D6, $D6, $D6, $CA, $C0, $C0, $C0, $00,  // char 1389
    $05, $6E, {|}  $00, $00, $60, $38, $0C, $7F, $CC, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 1390
    $05, $6F, {|}  $00, $00, $60, $60, $60, $66, $66, $66, $66, $66, $66, $3E, $06, $06, $06, $00,  // char 1391
    $05, $70, {|}  $00, $00, $60, $60, $60, $7C, $66, $66, $66, $66, $66, $66, $00, $00, $00, $00,  // char 1392
    $05, $71, {|}  $00, $00, $30, $1C, $0C, $18, $3C, $64, $66, $62, $63, $3D, $00, $00, $00, $00,  // char 1393
    $05, $72, {|}  $00, $00, $00, $00, $00, $F8, $CC, $CC, $CC, $CC, $CC, $CC, $0C, $0C, $0F, $00,  // char 1394
    $05, $73, {|}  $00, $00, $0E, $18, $30, $F8, $6C, $66, $66, $66, $66, $3E, $00, $00, $00, $00,  // char 1395
    $05, $74, {|}  $00, $00, $0F, $0C, $0C, $CC, $CC, $CC, $CC, $CC, $CC, $7C, $00, $00, $00, $00,  // char 1396
    $05, $75, {|}  $00, $00, $00, $00, $00, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $6C, $38, $00,  // char 1397
    $05, $76, {|}  $00, $00, $E0, $60, $60, $66, $66, $66, $66, $66, $66, $3E, $00, $00, $00, $00,  // char 1398
    $05, $77, {|}  $00, $00, $00, $00, $00, $3C, $66, $66, $66, $06, $0C, $18, $30, $60, $3E, $00,  // char 1399
    $05, $78, {|}  $00, $00, $00, $00, $00, $7C, $66, $66, $66, $66, $66, $66, $00, $00, $00, $00,  // char 1400
    $05, $79, {|}  $00, $00, $00, $00, $00, $0C, $18, $18, $0C, $06, $0C, $18, $30, $60, $3E, $00,  // char 1401
    $05, $7A, {|}  $00, $00, $00, $00, $00, $D6, $D6, $D6, $D6, $D6, $D6, $7E, $06, $06, $06, $00,  // char 1402
    $05, $7B, {|}  $00, $00, $00, $00, $00, $3C, $66, $66, $66, $76, $1C, $18, $30, $60, $3E, $00,  // char 1403
    $05, $7C, {|}  $00, $00, $00, $00, $00, $F8, $CC, $CC, $CC, $CC, $CC, $CF, $00, $00, $00, $00,  // char 1404
    $05, $7D, {|}  $00, $00, $00, $00, $00, $66, $66, $66, $66, $66, $66, $3E, $00, $00, $00, $00,  // char 1405
    $05, $7E, {|}  $00, $00, $0C, $0C, $0C, $CC, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $0C, $0F, $00,  // char 1406
    $05, $7F, {|}  $00, $00, $00, $00, $00, $DC, $D6, $D6, $D6, $D6, $D6, $76, $00, $00, $00, $00,  // char 1407
    $05, $80, {|}  $00, $00, $00, $00, $00, $7C, $66, $66, $66, $66, $66, $66, $60, $60, $60, $00,  // char 1408
    $05, $81, {|}  $00, $00, $00, $00, $00, $3E, $66, $66, $66, $66, $66, $3E, $06, $66, $3C, $00,  // char 1409
    $05, $82, {|}  $00, $00, $00, $00, $00, $30, $30, $30, $30, $30, $30, $3E, $00, $00, $00, $00,  // char 1410
    $05, $83, {|}  $00, $00, $10, $10, $10, $DC, $D6, $D6, $D6, $D6, $D6, $76, $10, $10, $10, $00,  // char 1411
    $05, $84, {|}  $00, $00, $00, $00, $00, $7C, $66, $66, $66, $66, $7C, $60, $FE, $60, $60, $00,  // char 1412
    $05, $85, {|}  $00, $00, $00, $00, $00, $3C, $66, $66, $66, $66, $66, $3C, $00, $00, $00, $00,  // char 1413
    $05, $86, {|}  $00, $00, $70, $D0, $D0, $7C, $16, $16, $16, $16, $D6, $7C, $10, $10, $10, $00,  // char 1414
    $05, $87, {|}  $00, $00, $C0, $C0, $C0, $C0, $C0, $CC, $CC, $CC, $CC, $77, $00, $00, $00, $00,  // char 1415
    $05, $89, {|}  $00, $00, $00, $00, $00, $18, $18, $00, $00, $00, $18, $18, $00, $00, $00, $00,  // char 1417
    $05, $8A, {|}  $00, $00, $00, $00, $00, $C0, $C6, $7C, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1418
    $05, $8E, {|}  $00, $00, $00, $00, $18, $64, $94, $BA, $52, $4C, $30, $00, $00, $00, $00, $00,  // char 1422
    $05, $91, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $10, $10, $38, $6C,  // char 1425
    $05, $92, {|}  $18, $00, $66, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1426
    $05, $93, {|}  $08, $38, $10, $38, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1427
    $05, $94, {|}  $18, $00, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1428
    $05, $95, {|}  $6C, $60, $6C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1429
    $05, $96, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $10, $18, $0C, $00,  // char 1430
    $05, $97, {|}  $10, $38, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1431
    $05, $98, {|}  $74, $D6, $5C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1432
    $05, $99, {|}  $C0, $60, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1433
    $05, $9A, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $06, $03, $00,  // char 1434
    $05, $9B, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $28, $18, $30, $00,  // char 1435
    $05, $9C, {|}  $0C, $18, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1436
    $05, $9D, {|}  $03, $06, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1437
    $05, $9E, {|}  $1B, $36, $24, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1438
    $05, $9F, {|}  $44, $AA, $6C, $28, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1439
    $05, $A0, {|}  $04, $0A, $0C, $08, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1440
    $05, $A1, {|}  $40, $50, $E0, $80, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1441
    $05, $A3, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0C, $0C, $3C, $00,  // char 1443
    $05, $A4, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $30, $18, $00,  // char 1444
    $05, $A5, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $10, $30, $60, $00,  // char 1445
    $05, $A6, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $24, $6C, $D8, $00,  // char 1446
    $05, $A7, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $30, $08, $30,  // char 1447
    $05, $A8, {|}  $60, $30, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1448
    $05, $A9, {|}  $40, $A0, $60, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1449
    $05, $AA, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $6C, $38, $10, $10,  // char 1450
    $05, $AB, {|}  $18, $30, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1451
    $05, $AC, {|}  $0C, $0C, $3C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1452
    $05, $AD, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $04, $06, $03, $00,  // char 1453
    $05, $AE, {|}  $48, $A8, $90, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1454
    $05, $AF, {|}  $38, $6C, $38, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1455
    $05, $B0, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $00, $18, $00,  // char 1456
    $05, $B1, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $DB, $00, $33, $00,  // char 1457
    $05, $B2, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $F6, $00, $06, $00,  // char 1458
    $05, $B3, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $F6, $60, $66, $00,  // char 1459
    $05, $B4, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $00, $00, $00,  // char 1460
    $05, $B5, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $66, $00, $00, $00,  // char 1461
    $05, $B6, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $66, $00, $18, $00,  // char 1462
    $05, $B7, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $7E, $00, $00,  // char 1463
    $05, $B8, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $7E, $18, $18,  // char 1464
    $05, $B9, {|}  $00, $60, $60, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1465
    $05, $BB, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $C0, $18, $03, $00,  // char 1467
    $05, $BC, {|}  $00, $00, $00, $00, $00, $00, $00, $18, $18, $00, $00, $00, $00, $00, $00, $00,  // char 1468
    $05, $BD, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18,  // char 1469
    $05, $BE, {|}  $00, $00, $00, $7E, $FC, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1470
    $05, $BF, {|}  $00, $00, $7C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1471
    $05, $C0, {|}  $00, $00, $00, $00, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00, $00, $00,  // char 1472
    $05, $C1, {|}  $00, $06, $06, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1473
    $05, $C2, {|}  $00, $C0, $C0, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1474
    $05, $C3, {|}  $00, $00, $00, $00, $18, $18, $00, $00, $00, $00, $18, $18, $00, $00, $00, $00,  // char 1475
    $05, $C4, {|}  $00, $18, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1476
    $05, $D0, {|}  $00, $00, $00, $00, $C6, $C6, $66, $76, $DC, $CC, $C6, $C6, $00, $00, $00, $00,  // char 1488
    $05, $D1, {|}  $00, $00, $00, $00, $F8, $0C, $0C, $0C, $0C, $0C, $0C, $FE, $00, $00, $00, $00,  // char 1489
    $05, $D2, {|}  $00, $00, $00, $00, $38, $0C, $0C, $0C, $0C, $1C, $36, $E6, $00, $00, $00, $00,  // char 1490
    $05, $D3, {|}  $00, $00, $00, $00, $FE, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $00, $00, $00, $00,  // char 1491
    $05, $D4, {|}  $00, $00, $00, $00, $FC, $06, $06, $06, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 1492
    $05, $D5, {|}  $00, $00, $00, $00, $70, $18, $18, $18, $18, $18, $18, $18, $00, $00, $00, $00,  // char 1493
    $05, $D6, {|}  $00, $00, $00, $00, $7E, $18, $18, $18, $0C, $0C, $18, $30, $00, $00, $00, $00,  // char 1494
    $05, $D7, {|}  $00, $00, $00, $00, $FC, $66, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 1495
    $05, $D8, {|}  $00, $00, $00, $00, $CC, $D6, $D6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 1496
    $05, $D9, {|}  $00, $00, $00, $00, $70, $18, $18, $18, $30, $00, $00, $00, $00, $00, $00, $00,  // char 1497
    $05, $DA, {|}  $00, $00, $00, $00, $FC, $06, $06, $06, $0C, $0C, $0C, $0C, $0C, $0C, $0E, $00,  // char 1498
    $05, $DB, {|}  $00, $00, $00, $00, $FC, $06, $06, $06, $06, $06, $06, $FC, $00, $00, $00, $00,  // char 1499
    $05, $DC, {|}  $00, $00, $C0, $C0, $FC, $06, $06, $06, $06, $0C, $18, $18, $00, $00, $00, $00,  // char 1500
    $05, $DD, {|}  $00, $00, $00, $00, $FC, $66, $C6, $C6, $C6, $C6, $C6, $FE, $00, $00, $00, $00,  // char 1501
    $05, $DE, {|}  $00, $00, $00, $00, $DC, $76, $66, $C6, $C6, $C6, $C6, $DE, $00, $00, $00, $00,  // char 1502
    $05, $DF, {|}  $00, $00, $00, $00, $38, $0C, $0C, $18, $18, $18, $18, $18, $18, $18, $1C, $00,  // char 1503
    $05, $E0, {|}  $00, $00, $00, $00, $38, $0C, $0C, $0C, $0C, $0C, $0C, $7C, $00, $00, $00, $00,  // char 1504
    $05, $E1, {|}  $00, $00, $00, $00, $FC, $66, $C6, $C6, $C6, $C6, $CC, $78, $00, $00, $00, $00,  // char 1505
    $05, $E2, {|}  $00, $00, $00, $00, $EE, $66, $66, $66, $66, $66, $2C, $F8, $00, $00, $00, $00,  // char 1506
    $05, $E3, {|}  $00, $00, $00, $00, $F8, $4C, $CC, $CC, $EC, $0C, $0C, $0C, $0C, $0C, $0E, $00,  // char 1507
    $05, $E4, {|}  $00, $00, $00, $00, $FC, $46, $C6, $C6, $E6, $06, $06, $FE, $00, $00, $00, $00,  // char 1508
    $05, $E5, {|}  $00, $00, $00, $00, $EE, $66, $66, $6C, $78, $60, $60, $60, $60, $60, $70, $00,  // char 1509
    $05, $E6, {|}  $00, $00, $00, $00, $EE, $66, $66, $34, $18, $0C, $06, $FE, $00, $00, $00, $00,  // char 1510
    $05, $E7, {|}  $00, $00, $00, $00, $FC, $06, $06, $66, $64, $6C, $6E, $60, $60, $60, $60, $00,  // char 1511
    $05, $E8, {|}  $00, $00, $00, $00, $FC, $06, $06, $06, $06, $06, $06, $06, $00, $00, $00, $00,  // char 1512
    $05, $E9, {|}  $00, $00, $00, $00, $D6, $D6, $D6, $D6, $D6, $F6, $C6, $7C, $00, $00, $00, $00,  // char 1513
    $05, $EA, {|}  $00, $00, $00, $00, $FC, $66, $66, $66, $66, $66, $E6, $E6, $00, $00, $00, $00,  // char 1514
    $05, $F0, {|}  $00, $00, $00, $00, $CC, $66, $66, $66, $66, $66, $66, $66, $00, $00, $00, $00,  // char 1520
    $05, $F1, {|}  $00, $00, $00, $00, $CC, $66, $66, $66, $C6, $06, $06, $06, $00, $00, $00, $00,  // char 1521
    $05, $F2, {|}  $00, $00, $00, $00, $CC, $66, $66, $66, $CC, $00, $00, $00, $00, $00, $00, $00,  // char 1522
    $05, $F3, {|}  $00, $00, $0C, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1523
    $05, $F4, {|}  $00, $00, $66, $CC, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1524
    $06, $0C, {|}  $00, $00, $00, $00, $00, $08, $10, $18, $18, $00, $00, $00, $00, $00, $00, $00,  // char 1548
    $06, $1B, {|}  $00, $00, $0C, $18, $18, $00, $00, $00, $18, $18, $00, $00, $00, $00, $00, $00,  // char 1563
    $06, $1F, {|}  $00, $00, $1C, $22, $20, $10, $08, $08, $00, $08, $00, $00, $00, $00, $00, $00,  // char 1567
    $06, $21, {|}  $00, $00, $00, $00, $18, $24, $20, $1E, $20, $00, $00, $00, $00, $00, $00, $00,  // char 1569
    $06, $22, {|}  $02, $3C, $40, $10, $10, $10, $10, $10, $10, $00, $00, $00, $00, $00, $00, $00,  // char 1570
    $06, $23, {|}  $18, $20, $1C, $20, $08, $08, $08, $08, $08, $00, $00, $00, $00, $00, $00, $00,  // char 1571
    $06, $24, {|}  $00, $1C, $20, $1C, $20, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 1572
    $06, $25, {|}  $00, $00, $00, $08, $08, $08, $08, $08, $08, $00, $18, $20, $1C, $20, $00, $00,  // char 1573
    $06, $26, {|}  $00, $30, $40, $38, $40, $07, $08, $88, $86, $81, $82, $7C, $00, $00, $00, $00,  // char 1574
    $06, $27, {|}  $00, $00, $00, $08, $08, $08, $08, $08, $08, $00, $00, $00, $00, $00, $00, $00,  // char 1575
    $06, $28, {|}  $00, $00, $00, $00, $00, $40, $81, $81, $7E, $00, $00, $08, $00, $00, $00, $00,  // char 1576
    $06, $29, {|}  $00, $28, $00, $00, $18, $24, $22, $22, $1C, $00, $00, $00, $00, $00, $00, $00,  // char 1577
    $06, $2A, {|}  $00, $00, $00, $00, $14, $40, $81, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 1578
    $06, $2B, {|}  $00, $08, $00, $14, $00, $40, $81, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 1579
    $06, $2C, {|}  $00, $00, $00, $00, $70, $8F, $30, $40, $80, $88, $80, $41, $3E, $00, $00, $00,  // char 1580
    $06, $2D, {|}  $00, $00, $00, $00, $70, $8F, $30, $40, $80, $80, $80, $41, $3E, $00, $00, $00,  // char 1581
    $06, $2E, {|}  $00, $20, $00, $00, $70, $8F, $30, $40, $80, $80, $80, $41, $3E, $00, $00, $00,  // char 1582
    $06, $2F, {|}  $00, $00, $00, $00, $08, $04, $02, $42, $3C, $00, $00, $00, $00, $00, $00, $00,  // char 1583
    $06, $30, {|}  $00, $00, $10, $00, $08, $04, $02, $42, $3C, $00, $00, $00, $00, $00, $00, $00,  // char 1584
    $06, $31, {|}  $00, $00, $00, $00, $00, $04, $02, $02, $04, $18, $60, $00, $00, $00, $00, $00,  // char 1585
    $06, $32, {|}  $00, $00, $08, $00, $00, $04, $02, $02, $04, $18, $60, $00, $00, $00, $00, $00,  // char 1586
    $06, $33, {|}  $00, $00, $00, $00, $00, $01, $15, $88, $84, $84, $78, $00, $00, $00, $00, $00,  // char 1587
    $06, $34, {|}  $00, $08, $00, $14, $00, $01, $15, $88, $84, $84, $78, $00, $00, $00, $00, $00,  // char 1588
    $06, $35, {|}  $00, $00, $00, $00, $06, $09, $31, $9E, $88, $88, $70, $00, $00, $00, $00, $00,  // char 1589
    $06, $36, {|}  $00, $00, $04, $00, $06, $09, $31, $9E, $88, $88, $70, $00, $00, $00, $00, $00,  // char 1590
    $06, $37, {|}  $00, $00, $20, $20, $20, $2C, $32, $A2, $7C, $00, $00, $00, $00, $00, $00, $00,  // char 1591
    $06, $38, {|}  $00, $00, $24, $20, $20, $2C, $32, $A2, $7C, $00, $00, $00, $00, $00, $00, $00,  // char 1592
    $06, $39, {|}  $00, $00, $00, $00, $70, $80, $8C, $70, $40, $80, $80, $80, $41, $3E, $00, $00,  // char 1593
    $06, $3A, {|}  $00, $40, $00, $00, $70, $80, $8C, $70, $40, $80, $80, $80, $41, $3E, $00, $00,  // char 1594
    $06, $40, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00,  // char 1600
    $06, $41, {|}  $00, $00, $04, $00, $06, $49, $85, $83, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 1601
    $06, $42, {|}  $00, $00, $0A, $00, $06, $09, $09, $47, $81, $81, $81, $42, $3C, $00, $00, $00,  // char 1602
    $06, $43, {|}  $00, $00, $19, $21, $19, $21, $01, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 1603
    $06, $44, {|}  $00, $02, $02, $02, $02, $02, $42, $82, $82, $84, $78, $00, $00, $00, $00, $00,  // char 1604
    $06, $45, {|}  $00, $00, $00, $00, $0C, $12, $12, $3C, $40, $40, $40, $40, $40, $40, $00, $00,  // char 1605
    $06, $46, {|}  $00, $00, $08, $00, $00, $41, $81, $81, $81, $42, $3C, $00, $00, $00, $00, $00,  // char 1606
    $06, $47, {|}  $00, $00, $00, $40, $30, $4E, $49, $39, $E6, $00, $00, $00, $00, $00, $00, $00,  // char 1607
    $06, $48, {|}  $00, $00, $00, $00, $00, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 1608
    $06, $49, {|}  $00, $00, $00, $00, $00, $07, $08, $88, $86, $81, $81, $7E, $00, $00, $00, $00,  // char 1609
    $06, $4A, {|}  $00, $00, $00, $00, $00, $07, $08, $88, $86, $81, $81, $7E, $00, $00, $24, $00,  // char 1610
    $06, $4B, {|}  $0C, $30, $0C, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1611
    $06, $4C, {|}  $0C, $12, $CA, $2C, $70, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1612
    $06, $4D, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0C, $30, $0C, $30, $00, $00,  // char 1613
    $06, $4E, {|}  $0C, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1614
    $06, $4F, {|}  $08, $14, $0C, $08, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1615
    $06, $50, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0C, $30, $00, $00,  // char 1616
    $06, $51, {|}  $0A, $2A, $2C, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1617
    $06, $52, {|}  $18, $24, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1618
    $06, $53, {|}  $01, $7E, $80, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1619
    $06, $54, {|}  $0C, $10, $0C, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1620
    $06, $55, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0C, $10, $0C, $10,  // char 1621
    $06, $60, {|}  $00, $00, $00, $00, $00, $00, $30, $78, $30, $00, $00, $00, $00, $00, $00, $00,  // char 1632
    $06, $61, {|}  $00, $00, $20, $70, $70, $38, $18, $18, $08, $08, $08, $00, $00, $00, $00, $00,  // char 1633
    $06, $62, {|}  $00, $00, $44, $FC, $F8, $60, $30, $30, $10, $10, $10, $00, $00, $00, $00, $00,  // char 1634
    $06, $63, {|}  $00, $00, $4A, $FE, $F4, $60, $30, $30, $10, $10, $10, $00, $00, $00, $00, $00,  // char 1635
    $06, $64, {|}  $00, $00, $10, $20, $40, $30, $10, $20, $42, $7C, $38, $00, $00, $00, $00, $00,  // char 1636
    $06, $65, {|}  $00, $00, $10, $38, $2C, $44, $42, $82, $82, $FE, $7C, $00, $00, $00, $00, $00,  // char 1637
    $06, $66, {|}  $00, $00, $80, $F8, $78, $08, $08, $08, $0C, $0E, $06, $00, $00, $00, $00, $00,  // char 1638
    $06, $67, {|}  $00, $00, $82, $C6, $C6, $6C, $28, $38, $10, $10, $10, $00, $00, $00, $00, $00,  // char 1639
    $06, $68, {|}  $00, $00, $10, $10, $10, $38, $28, $6C, $C6, $C6, $82, $00, $00, $00, $00, $00,  // char 1640
    $06, $69, {|}  $00, $00, $70, $F8, $88, $F8, $78, $08, $0C, $0E, $06, $00, $00, $00, $00, $00,  // char 1641
    $06, $6A, {|}  $00, $00, $60, $91, $92, $64, $08, $10, $26, $49, $89, $06, $00, $00, $00, $00,  // char 1642
    $06, $6B, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $30, $30, $60, $60, $00, $00,  // char 1643
    $06, $6C, {|}  $00, $10, $38, $18, $10, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1644
    $06, $6D, {|}  $00, $00, $00, $10, $10, $38, $FE, $7C, $38, $6C, $44, $00, $00, $00, $00, $00,  // char 1645
    $06, $70, {|}  $10, $10, $10, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1648
    $06, $71, {|}  $06, $29, $5E, $00, $04, $04, $04, $04, $04, $00, $00, $00, $00, $00, $00, $00,  // char 1649
    $06, $72, {|}  $06, $28, $5E, $00, $04, $04, $04, $04, $04, $00, $00, $00, $00, $00, $00, $00,  // char 1650
    $06, $73, {|}  $00, $00, $00, $04, $04, $04, $04, $04, $04, $00, $06, $28, $5E, $00, $00, $00,  // char 1651
    $06, $74, {|}  $00, $07, $08, $07, $08, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1652
    $06, $75, {|}  $03, $04, $03, $14, $10, $10, $10, $10, $10, $00, $00, $00, $00, $00, $00, $00,  // char 1653
    $06, $76, {|}  $03, $04, $03, $04, $00, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 1654
    $06, $77, {|}  $23, $54, $33, $24, $40, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 1655
    $06, $78, {|}  $03, $04, $03, $04, $00, $07, $08, $88, $86, $81, $81, $7E, $00, $00, $00, $00,  // char 1656
    $06, $79, {|}  $10, $1C, $14, $38, $00, $40, $81, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 1657
    $06, $7A, {|}  $00, $08, $00, $08, $00, $40, $81, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 1658
    $06, $7B, {|}  $00, $00, $00, $00, $00, $40, $81, $81, $7E, $00, $08, $00, $08, $00, $00, $00,  // char 1659
    $06, $7C, {|}  $00, $00, $00, $00, $14, $40, $81, $81, $7E, $08, $14, $08, $00, $00, $00, $00,  // char 1660
    $06, $7D, {|}  $00, $14, $00, $08, $00, $40, $81, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 1661
    $06, $7E, {|}  $00, $00, $00, $00, $00, $40, $81, $81, $7E, $00, $00, $14, $00, $08, $00, $00,  // char 1662
    $06, $7F, {|}  $00, $14, $00, $14, $00, $40, $81, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 1663
    $06, $80, {|}  $00, $00, $00, $00, $00, $40, $81, $81, $7E, $00, $00, $14, $00, $14, $00, $00,  // char 1664
    $06, $81, {|}  $06, $08, $06, $08, $70, $8F, $30, $40, $80, $80, $80, $41, $3E, $00, $00, $00,  // char 1665
    $06, $82, {|}  $08, $00, $08, $00, $70, $8F, $30, $40, $80, $80, $80, $41, $3E, $00, $00, $00,  // char 1666
    $06, $83, {|}  $00, $00, $00, $00, $70, $8F, $30, $40, $80, $94, $80, $41, $3E, $00, $00, $00,  // char 1667
    $06, $84, {|}  $00, $00, $00, $00, $70, $8F, $30, $40, $88, $80, $88, $41, $3E, $00, $00, $00,  // char 1668
    $06, $85, {|}  $08, $00, $14, $00, $70, $8F, $30, $40, $80, $80, $80, $41, $3E, $00, $00, $00,  // char 1669
    $06, $86, {|}  $00, $00, $00, $00, $70, $8F, $30, $40, $94, $80, $88, $41, $3E, $00, $00, $00,  // char 1670
    $06, $87, {|}  $00, $00, $00, $00, $70, $8F, $30, $40, $94, $80, $94, $41, $3E, $00, $00, $00,  // char 1671
    $06, $88, {|}  $20, $38, $28, $70, $08, $04, $02, $42, $3C, $00, $00, $00, $00, $00, $00, $00,  // char 1672
    $06, $89, {|}  $00, $00, $00, $00, $08, $04, $02, $42, $3C, $08, $14, $08, $00, $00, $00, $00,  // char 1673
    $06, $8A, {|}  $00, $00, $00, $00, $08, $04, $02, $42, $3C, $00, $00, $08, $00, $00, $00, $00,  // char 1674
    $06, $8B, {|}  $20, $38, $28, $70, $08, $04, $02, $42, $3C, $00, $00, $08, $00, $00, $00, $00,  // char 1675
    $06, $8C, {|}  $00, $14, $00, $00, $08, $04, $02, $42, $3C, $00, $00, $00, $00, $00, $00, $00,  // char 1676
    $06, $8D, {|}  $00, $00, $00, $00, $08, $04, $02, $42, $3C, $00, $00, $14, $00, $00, $00, $00,  // char 1677
    $06, $8E, {|}  $08, $00, $14, $00, $08, $04, $02, $42, $3C, $00, $00, $00, $00, $00, $00, $00,  // char 1678
    $06, $8F, {|}  $14, $00, $08, $00, $08, $04, $02, $42, $3C, $00, $00, $00, $00, $00, $00, $00,  // char 1679
    $06, $90, {|}  $14, $00, $14, $00, $08, $04, $02, $42, $3C, $00, $00, $00, $00, $00, $00, $00,  // char 1680
    $06, $91, {|}  $10, $1C, $14, $38, $00, $04, $02, $02, $04, $18, $60, $00, $00, $00, $00, $00,  // char 1681
    $06, $92, {|}  $00, $14, $08, $00, $00, $04, $02, $02, $04, $18, $60, $00, $00, $00, $00, $00,  // char 1682
    $06, $93, {|}  $00, $00, $00, $00, $00, $04, $02, $02, $04, $1C, $6A, $04, $00, $00, $00, $00,  // char 1683
    $06, $94, {|}  $00, $00, $00, $00, $00, $04, $02, $02, $04, $18, $62, $00, $00, $00, $00, $00,  // char 1684
    $06, $95, {|}  $00, $00, $00, $00, $00, $04, $02, $02, $04, $18, $60, $00, $14, $08, $00, $00,  // char 1685
    $06, $96, {|}  $00, $00, $00, $00, $00, $04, $02, $12, $04, $18, $62, $00, $00, $00, $00, $00,  // char 1686
    $06, $97, {|}  $00, $00, $14, $00, $00, $04, $02, $02, $04, $18, $60, $00, $00, $00, $00, $00,  // char 1687
    $06, $98, {|}  $08, $00, $14, $00, $00, $04, $02, $02, $04, $18, $60, $00, $00, $00, $00, $00,  // char 1688
    $06, $99, {|}  $14, $00, $14, $00, $00, $04, $02, $02, $04, $18, $60, $00, $00, $00, $00, $00,  // char 1689
    $06, $9A, {|}  $00, $00, $00, $04, $00, $01, $15, $88, $88, $8A, $70, $00, $00, $00, $00, $00,  // char 1690
    $06, $9B, {|}  $00, $00, $00, $00, $00, $01, $15, $88, $88, $90, $65, $00, $02, $00, $00, $00,  // char 1691
    $06, $9C, {|}  $00, $04, $00, $0A, $00, $01, $15, $88, $88, $90, $65, $00, $02, $00, $00, $00,  // char 1692
    $06, $9D, {|}  $00, $00, $00, $00, $06, $09, $31, $9E, $88, $90, $65, $00, $00, $00, $00, $00,  // char 1693
    $06, $9E, {|}  $04, $00, $0A, $00, $06, $09, $31, $9E, $88, $88, $70, $00, $00, $00, $00, $00,  // char 1694
    $06, $9F, {|}  $04, $00, $2A, $20, $20, $2C, $32, $A2, $7C, $00, $00, $00, $00, $00, $00, $00,  // char 1695
    $06, $A0, {|}  $20, $00, $50, $00, $70, $80, $8C, $70, $40, $80, $80, $80, $41, $3E, $00, $00,  // char 1696
    $06, $A1, {|}  $00, $00, $00, $00, $06, $49, $85, $83, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 1697
    $06, $A2, {|}  $00, $00, $00, $00, $06, $49, $85, $83, $7E, $00, $02, $00, $00, $00, $00, $00,  // char 1698
    $06, $A3, {|}  $00, $00, $04, $00, $06, $49, $85, $83, $7E, $00, $08, $00, $00, $00, $00, $00,  // char 1699
    $06, $A4, {|}  $04, $00, $0A, $00, $06, $49, $85, $83, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 1700
    $06, $A5, {|}  $00, $00, $00, $00, $06, $49, $85, $83, $7E, $00, $0A, $00, $04, $00, $00, $00,  // char 1701
    $06, $A6, {|}  $0A, $00, $0A, $00, $06, $49, $85, $83, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 1702
    $06, $A7, {|}  $00, $00, $04, $00, $06, $09, $09, $47, $81, $81, $81, $42, $3C, $00, $00, $00,  // char 1703
    $06, $A8, {|}  $04, $00, $0A, $00, $06, $09, $09, $47, $81, $81, $81, $42, $3C, $00, $00, $00,  // char 1704
    $06, $A9, {|}  $00, $01, $02, $04, $08, $44, $82, $82, $7C, $00, $00, $00, $00, $00, $00, $00,  // char 1705
    $06, $AA, {|}  $00, $00, $00, $18, $20, $40, $3E, $01, $81, $7E, $00, $00, $00, $00, $00, $00,  // char 1706
    $06, $AB, {|}  $00, $01, $02, $05, $0B, $48, $84, $82, $7C, $00, $00, $00, $00, $00, $00, $00,  // char 1707
    $06, $AC, {|}  $04, $00, $19, $21, $19, $21, $01, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 1708
    $06, $AD, {|}  $10, $00, $29, $01, $19, $21, $19, $A1, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 1709
    $06, $AE, {|}  $00, $00, $19, $21, $19, $21, $01, $81, $7E, $00, $14, $00, $08, $00, $00, $00,  // char 1710
    $06, $AF, {|}  $04, $09, $12, $04, $08, $44, $82, $82, $7C, $00, $00, $00, $00, $00, $00, $00,  // char 1711
    $06, $B0, {|}  $04, $09, $12, $05, $0B, $48, $84, $82, $7C, $00, $00, $00, $00, $00, $00, $00,  // char 1712
    $06, $B1, {|}  $A4, $09, $12, $04, $08, $44, $82, $82, $7C, $00, $00, $00, $00, $00, $00, $00,  // char 1713
    $06, $B2, {|}  $04, $09, $12, $04, $08, $44, $82, $82, $7C, $00, $14, $00, $00, $00, $00, $00,  // char 1714
    $06, $B3, {|}  $04, $09, $12, $04, $08, $44, $82, $82, $7C, $00, $08, $00, $08, $00, $00, $00,  // char 1715
    $06, $B4, {|}  $44, $09, $A2, $04, $08, $44, $82, $82, $7C, $00, $00, $00, $00, $00, $00, $00,  // char 1716
    $06, $B5, {|}  $14, $08, $02, $02, $02, $02, $42, $82, $82, $84, $78, $00, $00, $00, $00, $00,  // char 1717
    $06, $B6, {|}  $08, $02, $02, $02, $02, $02, $42, $82, $82, $84, $78, $00, $00, $00, $00, $00,  // char 1718
    $06, $B7, {|}  $10, $02, $2A, $02, $02, $02, $42, $82, $82, $84, $78, $00, $00, $00, $00, $00,  // char 1719
    $06, $B8, {|}  $00, $02, $02, $02, $02, $02, $42, $82, $82, $84, $78, $00, $14, $00, $08, $00,  // char 1720
    $06, $B9, {|}  $00, $08, $00, $00, $00, $41, $81, $81, $81, $42, $3C, $00, $00, $04, $00, $00,  // char 1721
    $06, $BA, {|}  $00, $00, $00, $00, $00, $41, $81, $81, $81, $42, $3C, $00, $00, $00, $00, $00,  // char 1722
    $06, $BB, {|}  $10, $1C, $14, $38, $00, $41, $81, $81, $81, $42, $3C, $00, $00, $00, $00, $00,  // char 1723
    $06, $BC, {|}  $00, $00, $08, $00, $00, $41, $81, $81, $81, $42, $3C, $08, $14, $08, $00, $00,  // char 1724
    $06, $BD, {|}  $08, $00, $14, $00, $00, $41, $81, $81, $81, $42, $3C, $00, $00, $00, $00, $00,  // char 1725
    $06, $BE, {|}  $00, $00, $00, $10, $38, $4C, $52, $32, $3C, $40, $00, $00, $00, $00, $00, $00,  // char 1726
    $06, $BF, {|}  $00, $00, $10, $00, $70, $8F, $30, $40, $94, $80, $88, $41, $3E, $00, $00, $00,  // char 1727
    $06, $C0, {|}  $38, $40, $38, $40, $18, $24, $22, $22, $1C, $00, $00, $00, $00, $00, $00, $00,  // char 1728
    $06, $C1, {|}  $00, $00, $00, $00, $00, $00, $00, $0C, $13, $00, $00, $00, $00, $00, $00, $00,  // char 1729
    $06, $C2, {|}  $00, $00, $38, $40, $38, $40, $00, $0C, $13, $00, $00, $00, $00, $00, $00, $00,  // char 1730
    $06, $C3, {|}  $00, $00, $00, $14, $00, $00, $00, $0C, $13, $00, $00, $00, $00, $00, $00, $00,  // char 1731
    $06, $C4, {|}  $00, $00, $00, $00, $00, $0C, $12, $0E, $12, $2C, $18, $60, $00, $00, $00, $00,  // char 1732
    $06, $C5, {|}  $00, $00, $00, $00, $00, $0C, $12, $0E, $02, $3C, $18, $60, $00, $00, $00, $00,  // char 1733
    $06, $C6, {|}  $00, $14, $08, $00, $00, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 1734
    $06, $C7, {|}  $10, $28, $18, $10, $20, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 1735
    $06, $C8, {|}  $08, $08, $08, $08, $00, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 1736
    $06, $C9, {|}  $00, $08, $14, $00, $00, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 1737
    $06, $CA, {|}  $00, $00, $14, $00, $00, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 1738
    $06, $CB, {|}  $08, $00, $14, $00, $00, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 1739
    $06, $CC, {|}  $00, $00, $00, $00, $00, $07, $08, $88, $86, $81, $81, $7E, $00, $00, $00, $00,  // char 1740
    $06, $CD, {|}  $00, $00, $00, $00, $20, $47, $88, $08, $86, $81, $81, $7E, $00, $00, $00, $00,  // char 1741
    $06, $CE, {|}  $00, $00, $00, $50, $20, $07, $08, $88, $86, $81, $81, $7E, $00, $00, $00, $00,  // char 1742
    $06, $CF, {|}  $00, $00, $08, $00, $00, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 1743
    $06, $D0, {|}  $00, $00, $00, $00, $00, $07, $08, $88, $86, $81, $82, $7C, $00, $08, $00, $08,  // char 1744
    $06, $D1, {|}  $00, $00, $00, $00, $00, $07, $08, $88, $86, $81, $82, $7C, $00, $14, $00, $08,  // char 1745
    $06, $D2, {|}  $00, $00, $00, $00, $00, $00, $08, $14, $60, $80, $FE, $00, $00, $00, $00, $00,  // char 1746
    $06, $D3, {|}  $00, $00, $60, $80, $60, $80, $08, $14, $60, $80, $FE, $00, $00, $00, $00, $00,  // char 1747
    $06, $D4, {|}  $00, $00, $00, $00, $00, $00, $00, $3C, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1748
    $06, $D5, {|}  $00, $00, $00, $00, $18, $24, $22, $22, $1C, $00, $00, $00, $00, $00, $00, $00,  // char 1749
    $06, $D6, {|}  $40, $40, $43, $55, $7E, $80, $7C, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1750
    $06, $D7, {|}  $2A, $20, $26, $2A, $7C, $80, $7C, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1751
    $06, $D8, {|}  $0C, $0E, $1C, $60, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1752
    $06, $D9, {|}  $14, $14, $08, $14, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1753
    $06, $DA, {|}  $18, $2C, $10, $28, $22, $1C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1754
    $06, $DB, {|}  $08, $00, $14, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1755
    $06, $DC, {|}  $15, $8A, $90, $60, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1756
    $06, $DD, {|}  $00, $5A, $A5, $66, $5B, $81, $81, $81, $81, $81, $81, $5A, $66, $A5, $5A, $00,  // char 1757
    $06, $DE, {|}  $00, $00, $18, $66, $42, $42, $81, $99, $99, $81, $42, $42, $66, $18, $00, $00,  // char 1758
    $06, $DF, {|}  $08, $1C, $08, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1759
    $06, $E0, {|}  $18, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1760
    $06, $E1, {|}  $18, $26, $08, $70, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1761
    $06, $E2, {|}  $0C, $0C, $10, $10, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1762
    $06, $E3, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $15, $8A, $90, $60,  // char 1763
    $06, $E4, {|}  $32, $2C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1764
    $06, $E5, {|}  $00, $00, $00, $00, $00, $08, $14, $0C, $04, $08, $30, $00, $00, $00, $00, $00,  // char 1765
    $06, $E6, {|}  $00, $00, $00, $00, $00, $00, $10, $30, $40, $3E, $00, $00, $00, $00, $00, $00,  // char 1766
    $06, $E7, {|}  $10, $30, $40, $3E, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1767
    $06, $E8, {|}  $08, $22, $22, $1C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1768
    $06, $E9, {|}  $10, $28, $54, $AA, $92, $54, $54, $54, $54, $54, $54, $54, $82, $FE, $00, $00,  // char 1769
    $06, $EA, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $08, $14, $22, $14, $08,  // char 1770
    $06, $EB, {|}  $08, $14, $22, $14, $08, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1771
    $06, $EC, {|}  $18, $3C, $3C, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 1772
    $06, $ED, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0C, $0C, $10, $10, $10,  // char 1773
    $06, $F0, {|}  $00, $00, $00, $00, $00, $38, $44, $44, $38, $00, $00, $00, $00, $00, $00, $00,  // char 1776
    $06, $F1, {|}  $00, $00, $20, $70, $70, $38, $18, $18, $08, $08, $08, $00, $00, $00, $00, $00,  // char 1777
    $06, $F2, {|}  $00, $00, $44, $FC, $F8, $60, $30, $30, $10, $10, $10, $00, $00, $00, $00, $00,  // char 1778
    $06, $F3, {|}  $00, $00, $4A, $FE, $F4, $60, $30, $30, $10, $10, $10, $00, $00, $00, $00, $00,  // char 1779
    $06, $F4, {|}  $00, $00, $4E, $F0, $FE, $7C, $30, $30, $10, $10, $10, $00, $00, $00, $00, $00,  // char 1780
    $06, $F5, {|}  $00, $00, $30, $38, $2C, $44, $42, $82, $92, $FE, $6C, $00, $00, $00, $00, $00,  // char 1781
    $06, $F6, {|}  $00, $00, $18, $3C, $60, $7E, $3C, $30, $60, $40, $80, $00, $00, $00, $00, $00,  // char 1782
    $06, $F7, {|}  $00, $00, $82, $C6, $C6, $6C, $28, $38, $10, $10, $10, $00, $00, $00, $00, $00,  // char 1783
    $06, $F8, {|}  $00, $00, $10, $10, $10, $38, $28, $6C, $C6, $C6, $82, $00, $00, $00, $00, $00,  // char 1784
    $06, $F9, {|}  $00, $00, $70, $F8, $88, $F8, $78, $08, $0C, $0E, $06, $00, $00, $00, $00, $00,  // char 1785
    $06, $FA, {|}  $00, $08, $00, $14, $00, $01, $15, $88, $88, $88, $72, $00, $00, $00, $00, $00,  // char 1786
    $06, $FB, {|}  $00, $00, $04, $00, $06, $09, $31, $9E, $88, $88, $72, $00, $00, $00, $00, $00,  // char 1787
    $06, $FC, {|}  $00, $40, $00, $00, $70, $80, $8C, $70, $40, $80, $88, $80, $41, $3E, $00, $00,  // char 1788
    $06, $FD, {|}  $00, $00, $00, $00, $18, $24, $20, $1E, $20, $00, $14, $14, $14, $14, $00, $00,  // char 1789
    $06, $FE, {|}  $00, $00, $00, $00, $0C, $12, $12, $3C, $40, $54, $54, $54, $54, $40, $00, $00,  // char 1790
    $10, $D3, {|}  $00, $00, $00, $00, $00, $6C, $92, $92, $92, $92, $92, $4C, $30, $4C, $02, $00,  // char 4307
    $10, $D7, {|}  $00, $00, $00, $00, $00, $6C, $92, $92, $92, $92, $92, $64, $00, $00, $00, $00,  // char 4311
    $10, $DA, {|}  $00, $00, $00, $00, $00, $54, $AA, $AA, $AA, $82, $82, $40, $30, $4C, $02, $00,  // char 4314
    $10, $DD, {|}  $00, $00, $00, $00, $00, $6C, $92, $92, $92, $82, $82, $44, $00, $00, $00, $00,  // char 4317
    $10, $E6, {|}  $00, $00, $00, $00, $00, $6C, $92, $92, $92, $82, $82, $40, $30, $4C, $02, $00,  // char 4326
    $1E, $00, {|}  $00, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $38, $6C, $38,  // char 7680
    $1E, $01, {|}  $00, $00, $00, $00, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $38, $6C, $38,  // char 7681
    $1E, $02, {|}  $18, $18, $00, $FC, $66, $66, $66, $7C, $66, $66, $66, $FC, $00, $00, $00, $00,  // char 7682
    $1E, $03, {|}  $00, $0C, $EC, $60, $60, $78, $6C, $66, $66, $66, $66, $7C, $00, $00, $00, $00,  // char 7683
    $1E, $04, {|}  $00, $00, $FC, $66, $66, $66, $7C, $66, $66, $66, $66, $FC, $00, $18, $18, $00,  // char 7684
    $1E, $05, {|}  $00, $00, $E0, $60, $60, $78, $6C, $66, $66, $66, $66, $7C, $00, $18, $18, $00,  // char 7685
    $1E, $06, {|}  $00, $00, $FC, $66, $66, $66, $7C, $66, $66, $66, $66, $FC, $00, $7C, $00, $00,  // char 7686
    $1E, $07, {|}  $00, $00, $E0, $60, $60, $78, $6C, $66, $66, $66, $66, $7C, $00, $7C, $00, $00,  // char 7687
    $1E, $08, {|}  $0C, $18, $00, $3C, $66, $C2, $C0, $C0, $C0, $C2, $66, $3C, $18, $0C, $38, $00,  // char 7688
    $1E, $09, {|}  $00, $00, $0C, $18, $00, $7C, $C6, $C0, $C0, $C0, $C6, $7C, $18, $0C, $38, $00,  // char 7689
    $1E, $0A, {|}  $30, $30, $00, $F8, $6C, $66, $66, $66, $66, $66, $6C, $F8, $00, $00, $00, $00,  // char 7690
    $1E, $0B, {|}  $60, $60, $0C, $0C, $0C, $3C, $6C, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7691
    $1E, $0C, {|}  $00, $00, $F8, $6C, $66, $66, $66, $66, $66, $66, $6C, $F8, $00, $30, $30, $00,  // char 7692
    $1E, $0D, {|}  $00, $00, $1C, $0C, $0C, $3C, $6C, $CC, $CC, $CC, $CC, $76, $00, $30, $30, $00,  // char 7693
    $1E, $0E, {|}  $00, $00, $F8, $6C, $66, $66, $66, $66, $66, $66, $6C, $F8, $00, $7C, $00, $00,  // char 7694
    $1E, $0F, {|}  $00, $00, $1C, $0C, $0C, $3C, $6C, $CC, $CC, $CC, $CC, $76, $00, $7C, $00, $00,  // char 7695
    $1E, $10, {|}  $00, $00, $F8, $6C, $66, $66, $66, $66, $66, $66, $6C, $F8, $30, $18, $70, $00,  // char 7696
    $1E, $11, {|}  $00, $00, $1C, $0C, $0C, $3C, $6C, $CC, $CC, $CC, $CC, $76, $30, $18, $70, $00,  // char 7697
    $1E, $12, {|}  $00, $00, $F8, $6C, $66, $66, $66, $66, $66, $66, $6C, $F8, $10, $38, $6C, $00,  // char 7698
    $1E, $13, {|}  $00, $00, $1C, $0C, $0C, $3C, $6C, $CC, $CC, $CC, $CC, $76, $10, $38, $6C, $00,  // char 7699
    $1E, $14, {|}  $60, $30, $00, $7C, $00, $FE, $62, $68, $78, $68, $62, $FE, $00, $00, $00, $00,  // char 7700
    $1E, $15, {|}  $60, $30, $00, $7C, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 7701
    $1E, $16, {|}  $0C, $18, $00, $7C, $00, $FE, $62, $68, $78, $68, $62, $FE, $00, $00, $00, $00,  // char 7702
    $1E, $17, {|}  $0C, $18, $00, $7C, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 7703
    $1E, $18, {|}  $00, $00, $FE, $66, $62, $68, $78, $68, $60, $62, $66, $FE, $10, $38, $6C, $00,  // char 7704
    $1E, $19, {|}  $00, $00, $00, $00, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $10, $38, $6C, $00,  // char 7705
    $1E, $1A, {|}  $00, $00, $FE, $66, $62, $68, $78, $68, $60, $62, $66, $FE, $00, $76, $DC, $00,  // char 7706
    $1E, $1B, {|}  $00, $00, $00, $00, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $76, $DC, $00,  // char 7707
    $1E, $1C, {|}  $6C, $38, $00, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $18, $0C, $38, $00,  // char 7708
    $1E, $1D, {|}  $00, $00, $6C, $38, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $18, $0C, $38, $00,  // char 7709
    $1E, $1E, {|}  $18, $18, $00, $FE, $66, $62, $68, $78, $68, $60, $60, $F0, $00, $00, $00, $00,  // char 7710
    $1E, $1F, {|}  $30, $30, $00, $38, $6C, $64, $F0, $60, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 7711
    $1E, $20, {|}  $00, $7C, $00, $3C, $66, $C2, $C0, $DE, $C6, $C6, $66, $3A, $00, $00, $00, $00,  // char 7712
    $1E, $21, {|}  $00, $00, $00, $7C, $00, $76, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $CC, $78, $00,  // char 7713
    $1E, $22, {|}  $30, $30, $00, $C6, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 7714
    $1E, $23, {|}  $0C, $0C, $E0, $60, $60, $6C, $76, $66, $66, $66, $66, $E6, $00, $00, $00, $00,  // char 7715
    $1E, $24, {|}  $00, $00, $C6, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $C6, $00, $30, $30, $00,  // char 7716
    $1E, $25, {|}  $00, $00, $E0, $60, $60, $6C, $76, $66, $66, $66, $66, $E6, $00, $18, $18, $00,  // char 7717
    $1E, $26, {|}  $6C, $6C, $00, $C6, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 7718
    $1E, $27, {|}  $6C, $6C, $00, $E0, $60, $6C, $76, $66, $66, $66, $66, $E6, $00, $00, $00, $00,  // char 7719
    $1E, $28, {|}  $00, $00, $C6, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $C6, $60, $30, $E0, $00,  // char 7720
    $1E, $29, {|}  $00, $00, $E0, $60, $60, $6C, $76, $66, $66, $66, $66, $E6, $30, $18, $70, $00,  // char 7721
    $1E, $2A, {|}  $00, $00, $C6, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $C6, $00, $6C, $38, $00,  // char 7722
    $1E, $2B, {|}  $00, $00, $E0, $60, $60, $6C, $76, $66, $66, $66, $66, $E6, $00, $6C, $38, $00,  // char 7723
    $1E, $2C, {|}  $00, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $76, $DC, $00,  // char 7724
    $1E, $2D, {|}  $00, $00, $18, $18, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $76, $DC, $00,  // char 7725
    $1E, $2E, {|}  $0C, $18, $66, $00, $3C, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 7726
    $1E, $2F, {|}  $0C, $18, $66, $66, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 7727
    $1E, $30, {|}  $18, $18, $00, $E6, $66, $66, $6C, $78, $6C, $66, $66, $E6, $00, $00, $00, $00,  // char 7728
    $1E, $31, {|}  $00, $0C, $EC, $60, $60, $66, $6C, $78, $78, $6C, $66, $E6, $00, $00, $00, $00,  // char 7729
    $1E, $32, {|}  $00, $00, $E6, $66, $66, $6C, $78, $78, $6C, $66, $66, $E6, $00, $18, $18, $00,  // char 7730
    $1E, $33, {|}  $00, $00, $E0, $60, $60, $66, $6C, $78, $78, $6C, $66, $E6, $00, $18, $18, $00,  // char 7731
    $1E, $34, {|}  $00, $00, $E6, $66, $66, $6C, $78, $78, $6C, $66, $66, $E6, $00, $7C, $00, $00,  // char 7732
    $1E, $35, {|}  $00, $00, $E0, $60, $60, $66, $6C, $78, $78, $6C, $66, $E6, $00, $7C, $00, $00,  // char 7733
    $1E, $36, {|}  $00, $00, $F0, $60, $60, $60, $60, $60, $60, $62, $66, $FE, $00, $18, $18, $00,  // char 7734
    $1E, $37, {|}  $00, $00, $38, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $18, $18, $00,  // char 7735
    $1E, $38, {|}  $00, $7C, $00, $F0, $60, $60, $60, $60, $60, $62, $66, $FE, $00, $18, $18, $00,  // char 7736
    $1E, $39, {|}  $00, $7C, $00, $38, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $18, $18, $00,  // char 7737
    $1E, $3A, {|}  $00, $00, $F0, $60, $60, $60, $60, $60, $60, $62, $66, $FE, $00, $7C, $00, $00,  // char 7738
    $1E, $3B, {|}  $00, $00, $38, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $7E, $00, $00,  // char 7739
    $1E, $3C, {|}  $00, $00, $F0, $60, $60, $60, $60, $60, $60, $62, $66, $FE, $10, $38, $6C, $00,  // char 7740
    $1E, $3D, {|}  $00, $00, $38, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $10, $38, $6C, $00,  // char 7741
    $1E, $3E, {|}  $0C, $18, $00, $C6, $EE, $FE, $FE, $D6, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 7742
    $1E, $3F, {|}  $00, $00, $0C, $18, $00, $EC, $FE, $D6, $D6, $D6, $D6, $C6, $00, $00, $00, $00,  // char 7743
    $1E, $40, {|}  $30, $30, $00, $C6, $EE, $FE, $FE, $D6, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 7744
    $1E, $41, {|}  $00, $00, $30, $30, $00, $EC, $FE, $D6, $D6, $D6, $D6, $C6, $00, $00, $00, $00,  // char 7745
    $1E, $42, {|}  $00, $00, $C6, $EE, $FE, $FE, $D6, $C6, $C6, $C6, $C6, $C6, $00, $30, $30, $00,  // char 7746
    $1E, $43, {|}  $00, $00, $00, $00, $00, $EC, $FE, $D6, $D6, $D6, $D6, $C6, $00, $30, $30, $00,  // char 7747
    $1E, $44, {|}  $30, $30, $00, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 7748
    $1E, $45, {|}  $00, $00, $18, $18, $00, $DC, $66, $66, $66, $66, $66, $66, $00, $00, $00, $00,  // char 7749
    $1E, $46, {|}  $00, $00, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $C6, $C6, $C6, $00, $30, $30, $00,  // char 7750
    $1E, $47, {|}  $00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $66, $00, $18, $18, $00,  // char 7751
    $1E, $48, {|}  $00, $00, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $C6, $C6, $C6, $00, $7C, $00, $00,  // char 7752
    $1E, $49, {|}  $00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $66, $00, $7C, $00, $00,  // char 7753
    $1E, $4A, {|}  $00, $00, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $C6, $C6, $C6, $10, $38, $6C, $00,  // char 7754
    $1E, $4B, {|}  $00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $66, $10, $38, $6C, $00,  // char 7755
    $1E, $4C, {|}  $0C, $18, $72, $9C, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7756
    $1E, $4D, {|}  $0C, $18, $76, $DC, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7757
    $1E, $4E, {|}  $6C, $00, $72, $9C, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7758
    $1E, $4F, {|}  $6C, $00, $76, $DC, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7759
    $1E, $50, {|}  $60, $30, $7C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7760
    $1E, $51, {|}  $60, $30, $00, $7C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7761
    $1E, $52, {|}  $0C, $18, $7C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7762
    $1E, $53, {|}  $0C, $18, $00, $7C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7763
    $1E, $54, {|}  $0C, $18, $00, $FC, $66, $66, $66, $7C, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 7764
    $1E, $55, {|}  $00, $00, $0C, $18, $00, $DC, $66, $66, $66, $66, $66, $7C, $60, $60, $F0, $00,  // char 7765
    $1E, $56, {|}  $18, $18, $00, $FC, $66, $66, $66, $7C, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 7766
    $1E, $57, {|}  $00, $00, $18, $18, $00, $DC, $66, $66, $66, $66, $66, $7C, $60, $60, $F0, $00,  // char 7767
    $1E, $58, {|}  $18, $18, $00, $FC, $66, $66, $66, $7C, $6C, $66, $66, $E6, $00, $00, $00, $00,  // char 7768
    $1E, $59, {|}  $00, $00, $18, $18, $00, $DC, $76, $66, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 7769
    $1E, $5A, {|}  $00, $00, $FC, $66, $66, $66, $7C, $6C, $66, $66, $66, $E6, $00, $18, $18, $00,  // char 7770
    $1E, $5B, {|}  $00, $00, $00, $00, $00, $DC, $76, $66, $60, $60, $60, $F0, $00, $18, $18, $00,  // char 7771
    $1E, $5C, {|}  $00, $7C, $00, $FC, $66, $66, $66, $7C, $6C, $66, $66, $E6, $00, $18, $18, $00,  // char 7772
    $1E, $5D, {|}  $00, $00, $00, $7C, $00, $DC, $76, $66, $60, $60, $60, $F0, $00, $18, $18, $00,  // char 7773
    $1E, $5E, {|}  $00, $00, $FC, $66, $66, $66, $7C, $6C, $66, $66, $66, $E6, $00, $7C, $00, $00,  // char 7774
    $1E, $5F, {|}  $00, $00, $00, $00, $00, $DC, $76, $66, $60, $60, $60, $F0, $00, $7C, $00, $00,  // char 7775
    $1E, $60, {|}  $30, $30, $00, $7C, $C6, $C6, $60, $38, $0C, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7776
    $1E, $61, {|}  $00, $00, $30, $30, $00, $7C, $C6, $60, $38, $0C, $C6, $7C, $00, $00, $00, $00,  // char 7777
    $1E, $62, {|}  $00, $00, $7C, $C6, $C6, $60, $38, $0C, $06, $C6, $C6, $7C, $00, $30, $30, $00,  // char 7778
    $1E, $63, {|}  $00, $00, $00, $00, $00, $7C, $C6, $60, $38, $0C, $C6, $7C, $00, $30, $30, $00,  // char 7779
    $1E, $64, {|}  $60, $6C, $18, $00, $7C, $C6, $C6, $70, $1C, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7780
    $1E, $65, {|}  $60, $60, $0C, $18, $00, $7C, $C6, $60, $38, $0C, $C6, $7C, $00, $00, $00, $00,  // char 7781
    $1E, $66, {|}  $30, $44, $38, $10, $7C, $C6, $C6, $70, $1C, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7782
    $1E, $67, {|}  $30, $30, $44, $38, $10, $7C, $C6, $60, $38, $0C, $C6, $7C, $00, $00, $00, $00,  // char 7783
    $1E, $68, {|}  $30, $30, $00, $7C, $C6, $C6, $60, $38, $0C, $C6, $C6, $7C, $00, $30, $30, $00,  // char 7784
    $1E, $69, {|}  $00, $00, $30, $30, $00, $7C, $C6, $60, $38, $0C, $C6, $7C, $00, $30, $30, $00,  // char 7785
    $1E, $6A, {|}  $18, $18, $00, $7E, $7E, $5A, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 7786
    $1E, $6B, {|}  $00, $06, $16, $30, $30, $FC, $30, $30, $30, $30, $36, $1C, $00, $00, $00, $00,  // char 7787
    $1E, $6C, {|}  $00, $00, $7E, $7E, $5A, $18, $18, $18, $18, $18, $18, $3C, $00, $18, $18, $00,  // char 7788
    $1E, $6D, {|}  $00, $00, $10, $30, $30, $FC, $30, $30, $30, $30, $36, $1C, $00, $18, $18, $00,  // char 7789
    $1E, $6E, {|}  $00, $00, $7E, $7E, $5A, $18, $18, $18, $18, $18, $18, $3C, $00, $7E, $00, $00,  // char 7790
    $1E, $6F, {|}  $00, $00, $10, $30, $30, $FC, $30, $30, $30, $30, $36, $1C, $00, $7E, $00, $00,  // char 7791
    $1E, $70, {|}  $00, $00, $7E, $7E, $5A, $18, $18, $18, $18, $18, $18, $3C, $10, $38, $6C, $00,  // char 7792
    $1E, $71, {|}  $00, $00, $10, $30, $30, $FC, $30, $30, $30, $30, $36, $1C, $08, $1C, $36, $00,  // char 7793
    $1E, $72, {|}  $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $6C, $6C, $00,  // char 7794
    $1E, $73, {|}  $00, $00, $00, $00, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $6C, $6C, $00,  // char 7795
    $1E, $74, {|}  $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $76, $DC, $00,  // char 7796
    $1E, $75, {|}  $00, $00, $00, $00, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $76, $DC, $00,  // char 7797
    $1E, $76, {|}  $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $10, $38, $6C, $00,  // char 7798
    $1E, $77, {|}  $00, $00, $00, $00, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $10, $38, $6C, $00,  // char 7799
    $1E, $78, {|}  $0C, $18, $72, $9C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7800
    $1E, $79, {|}  $0C, $18, $76, $DC, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7801
    $1E, $7A, {|}  $6C, $00, $7C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7802
    $1E, $7B, {|}  $CC, $CC, $00, $FC, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7803
    $1E, $7C, {|}  $76, $DC, $00, $C6, $C6, $C6, $C6, $C6, $C6, $6C, $38, $10, $00, $00, $00, $00,  // char 7804
    $1E, $7D, {|}  $00, $00, $76, $DC, $00, $66, $66, $66, $66, $66, $3C, $18, $00, $00, $00, $00,  // char 7805
    $1E, $7E, {|}  $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $6C, $38, $10, $00, $30, $30, $00,  // char 7806
    $1E, $7F, {|}  $00, $00, $00, $00, $00, $66, $66, $66, $66, $66, $3C, $18, $00, $18, $18, $00,  // char 7807
    $1E, $80, {|}  $60, $30, $00, $C6, $C6, $C6, $D6, $D6, $D6, $FE, $EE, $6C, $00, $00, $00, $00,  // char 7808
    $1E, $81, {|}  $00, $00, $60, $30, $00, $C6, $C6, $D6, $D6, $D6, $FE, $6C, $00, $00, $00, $00,  // char 7809
    $1E, $82, {|}  $0C, $18, $00, $C6, $C6, $C6, $D6, $D6, $D6, $FE, $EE, $6C, $00, $00, $00, $00,  // char 7810
    $1E, $83, {|}  $00, $00, $0C, $18, $00, $C6, $C6, $D6, $D6, $D6, $FE, $6C, $00, $00, $00, $00,  // char 7811
    $1E, $84, {|}  $6C, $6C, $00, $C6, $C6, $C6, $D6, $D6, $D6, $FE, $EE, $6C, $00, $00, $00, $00,  // char 7812
    $1E, $85, {|}  $00, $00, $6C, $6C, $00, $C6, $C6, $D6, $D6, $D6, $FE, $6C, $00, $00, $00, $00,  // char 7813
    $1E, $86, {|}  $30, $30, $00, $C6, $C6, $C6, $D6, $D6, $D6, $FE, $EE, $6C, $00, $00, $00, $00,  // char 7814
    $1E, $87, {|}  $00, $00, $30, $30, $00, $C6, $C6, $D6, $D6, $D6, $FE, $6C, $00, $00, $00, $00,  // char 7815
    $1E, $88, {|}  $00, $00, $C6, $C6, $C6, $C6, $D6, $D6, $D6, $FE, $EE, $6C, $00, $30, $30, $00,  // char 7816
    $1E, $89, {|}  $00, $00, $00, $00, $00, $C6, $C6, $D6, $D6, $D6, $FE, $6C, $00, $30, $30, $00,  // char 7817
    $1E, $8A, {|}  $30, $30, $00, $C6, $C6, $6C, $7C, $38, $7C, $6C, $C6, $C6, $00, $00, $00, $00,  // char 7818
    $1E, $8B, {|}  $00, $00, $30, $30, $00, $C6, $6C, $38, $38, $38, $6C, $C6, $00, $00, $00, $00,  // char 7819
    $1E, $8C, {|}  $6C, $6C, $00, $C6, $C6, $6C, $7C, $38, $7C, $6C, $C6, $C6, $00, $00, $00, $00,  // char 7820
    $1E, $8D, {|}  $00, $00, $6C, $6C, $00, $C6, $6C, $38, $38, $38, $6C, $C6, $00, $00, $00, $00,  // char 7821
    $1E, $8E, {|}  $18, $18, $00, $66, $66, $66, $3C, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 7822
    $1E, $8F, {|}  $00, $00, $30, $30, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00,  // char 7823
    $1E, $90, {|}  $10, $38, $44, $FE, $C6, $8C, $18, $30, $60, $C2, $C6, $FE, $00, $00, $00, $00,  // char 7824
    $1E, $91, {|}  $00, $10, $38, $6C, $00, $FE, $CC, $18, $30, $60, $C6, $FE, $00, $00, $00, $00,  // char 7825
    $1E, $92, {|}  $00, $00, $FE, $C6, $86, $0C, $18, $30, $60, $C2, $C6, $FE, $00, $30, $30, $00,  // char 7826
    $1E, $93, {|}  $00, $00, $00, $00, $00, $FE, $CC, $18, $30, $60, $C6, $FE, $00, $30, $30, $00,  // char 7827
    $1E, $94, {|}  $00, $00, $FE, $C6, $86, $0C, $18, $30, $60, $C2, $C6, $FE, $00, $7C, $00, $00,  // char 7828
    $1E, $95, {|}  $00, $00, $00, $00, $00, $FE, $CC, $18, $30, $60, $C6, $FE, $00, $7C, $00, $00,  // char 7829
    $1E, $96, {|}  $00, $00, $E0, $60, $60, $6C, $76, $66, $66, $66, $66, $E6, $00, $7C, $00, $00,  // char 7830
    $1E, $97, {|}  $6C, $6C, $00, $10, $30, $FC, $30, $30, $30, $30, $36, $1C, $00, $00, $00, $00,  // char 7831
    $1E, $98, {|}  $00, $38, $6C, $38, $00, $C6, $C6, $D6, $D6, $D6, $FE, $6C, $00, $00, $00, $00,  // char 7832
    $1E, $99, {|}  $00, $38, $6C, $38, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00,  // char 7833
    $1E, $9A, {|}  $00, $18, $0C, $18, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7834
    $1E, $9B, {|}  $30, $30, $00, $38, $6C, $64, $60, $60, $60, $60, $60, $F0, $00, $00, $00, $00,  // char 7835
    $1E, $A0, {|}  $00, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $30, $30, $00,  // char 7840
    $1E, $A1, {|}  $00, $00, $00, $00, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $30, $30, $00,  // char 7841
    $1E, $A2, {|}  $38, $0C, $18, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00,  // char 7842
    $1E, $A3, {|}  $00, $38, $0C, $18, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7843
    $1E, $A4, {|}  $03, $16, $38, $6C, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $00, $00, $00, $00,  // char 7844
    $1E, $A5, {|}  $03, $16, $38, $6C, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7845
    $1E, $A6, {|}  $C0, $68, $1C, $36, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $00, $00, $00, $00,  // char 7846
    $1E, $A7, {|}  $C0, $68, $1C, $36, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7847
    $1E, $A8, {|}  $0E, $13, $3A, $6C, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $00, $00, $00, $00,  // char 7848
    $1E, $A9, {|}  $0E, $13, $3A, $6C, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7849
    $1E, $AA, {|}  $76, $DC, $10, $28, $54, $38, $6C, $C6, $C6, $FE, $C6, $C6, $00, $00, $00, $00,  // char 7850
    $1E, $AB, {|}  $76, $DC, $10, $38, $44, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7851
    $1E, $AC, {|}  $10, $38, $6C, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $30, $30, $00,  // char 7852
    $1E, $AD, {|}  $00, $10, $38, $6C, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $30, $30, $00,  // char 7853
    $1E, $AE, {|}  $0C, $18, $44, $38, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $00, $00, $00, $00,  // char 7854
    $1E, $AF, {|}  $0C, $18, $44, $38, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7855
    $1E, $B0, {|}  $60, $30, $44, $38, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $00, $00, $00, $00,  // char 7856
    $1E, $B1, {|}  $60, $30, $44, $38, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7857
    $1E, $B2, {|}  $30, $08, $54, $38, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $00, $00, $00, $00,  // char 7858
    $1E, $B3, {|}  $30, $08, $54, $38, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7859
    $1E, $B4, {|}  $72, $9C, $44, $38, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $00, $00, $00, $00,  // char 7860
    $1E, $B5, {|}  $72, $9C, $44, $38, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7861
    $1E, $B6, {|}  $6C, $38, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $30, $30, $00,  // char 7862
    $1E, $B7, {|}  $00, $00, $6C, $38, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $30, $30, $00,  // char 7863
    $1E, $B8, {|}  $00, $00, $FE, $66, $62, $68, $78, $68, $60, $62, $66, $FE, $00, $18, $18, $00,  // char 7864
    $1E, $B9, {|}  $00, $00, $00, $00, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $30, $30, $00,  // char 7865
    $1E, $BA, {|}  $38, $0C, $18, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $00, $00, $00, $00,  // char 7866
    $1E, $BB, {|}  $00, $38, $0C, $18, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 7867
    $1E, $BC, {|}  $76, $DC, $00, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $00, $00, $00, $00,  // char 7868
    $1E, $BD, {|}  $00, $00, $76, $DC, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 7869
    $1E, $BE, {|}  $23, $76, $88, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $00, $00, $00, $00,  // char 7870
    $1E, $BF, {|}  $03, $26, $70, $D8, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 7871
    $1E, $C0, {|}  $C4, $6E, $11, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $00, $00, $00, $00,  // char 7872
    $1E, $C1, {|}  $C0, $64, $0E, $1B, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 7873
    $1E, $C2, {|}  $0E, $13, $3A, $6C, $00, $FE, $62, $68, $78, $68, $62, $FE, $00, $00, $00, $00,  // char 7874
    $1E, $C3, {|}  $0E, $13, $3A, $6C, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 7875
    $1E, $C4, {|}  $76, $DC, $10, $38, $44, $FE, $62, $68, $78, $68, $62, $FE, $00, $00, $00, $00,  // char 7876
    $1E, $C5, {|}  $76, $DC, $10, $38, $44, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 7877
    $1E, $C6, {|}  $10, $38, $44, $FE, $66, $62, $68, $78, $68, $62, $66, $FE, $00, $18, $18, $00,  // char 7878
    $1E, $C7, {|}  $00, $10, $38, $6C, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $30, $30, $00,  // char 7879
    $1E, $C8, {|}  $38, $0C, $18, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 7880
    $1E, $C9, {|}  $00, $38, $0C, $18, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 7881
    $1E, $CA, {|}  $00, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $18, $18, $00,  // char 7882
    $1E, $CB, {|}  $00, $00, $18, $18, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $18, $18, $00,  // char 7883
    $1E, $CC, {|}  $00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $30, $30, $00,  // char 7884
    $1E, $CD, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $30, $30, $00,  // char 7885
    $1E, $CE, {|}  $38, $0C, $18, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7886
    $1E, $CF, {|}  $00, $38, $0C, $18, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7887
    $1E, $D0, {|}  $23, $76, $88, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7888
    $1E, $D1, {|}  $03, $26, $70, $D8, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7889
    $1E, $D2, {|}  $C4, $6E, $11, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7890
    $1E, $D3, {|}  $C0, $64, $0E, $1B, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7891
    $1E, $D4, {|}  $0E, $13, $3A, $44, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7892
    $1E, $D5, {|}  $0E, $13, $3A, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7893
    $1E, $D6, {|}  $76, $DC, $10, $28, $44, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7894
    $1E, $D7, {|}  $76, $DC, $10, $38, $44, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7895
    $1E, $D8, {|}  $10, $38, $44, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $30, $30, $00,  // char 7896
    $1E, $D9, {|}  $00, $10, $38, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $30, $30, $00,  // char 7897
    $1E, $DA, {|}  $18, $33, $03, $7A, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 7898
    $1E, $DB, {|}  $00, $00, $1B, $33, $06, $78, $CC, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 7899
    $1E, $DC, {|}  $60, $33, $03, $7A, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 7900
    $1E, $DD, {|}  $00, $00, $63, $33, $06, $78, $CC, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 7901
    $1E, $DE, {|}  $70, $1B, $33, $7A, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 7902
    $1E, $DF, {|}  $00, $70, $1B, $33, $06, $78, $CC, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 7903
    $1E, $E0, {|}  $68, $B3, $03, $7A, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 7904
    $1E, $E1, {|}  $00, $68, $B3, $03, $06, $78, $CC, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 7905
    $1E, $E2, {|}  $03, $03, $7A, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $78, $00, $30, $30, $00,  // char 7906
    $1E, $E3, {|}  $00, $00, $03, $03, $06, $78, $CC, $CC, $CC, $CC, $CC, $78, $00, $30, $30, $00,  // char 7907
    $1E, $E4, {|}  $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $30, $30, $00,  // char 7908
    $1E, $E5, {|}  $00, $00, $00, $00, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $30, $30, $00,  // char 7909
    $1E, $E6, {|}  $38, $0C, $18, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 7910
    $1E, $E7, {|}  $00, $70, $18, $30, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7911
    $1E, $E8, {|}  $18, $33, $03, $CE, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 7912
    $1E, $E9, {|}  $00, $00, $1B, $33, $06, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7913
    $1E, $EA, {|}  $60, $33, $03, $CE, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 7914
    $1E, $EB, {|}  $00, $00, $63, $33, $06, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7915
    $1E, $EC, {|}  $70, $1B, $33, $CE, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 7916
    $1E, $ED, {|}  $00, $70, $1B, $33, $06, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7917
    $1E, $EE, {|}  $68, $B3, $03, $CE, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 7918
    $1E, $EF, {|}  $00, $68, $B3, $03, $06, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7919
    $1E, $F0, {|}  $03, $03, $CE, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $78, $00, $30, $30, $00,  // char 7920
    $1E, $F1, {|}  $00, $00, $03, $03, $06, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $30, $30, $00,  // char 7921
    $1E, $F2, {|}  $30, $18, $00, $66, $66, $66, $3C, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 7922
    $1E, $F3, {|}  $00, $00, $60, $30, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00,  // char 7923
    $1E, $F4, {|}  $00, $00, $66, $66, $66, $66, $3C, $18, $18, $18, $18, $3C, $00, $18, $18, $00,  // char 7924
    $1E, $F5, {|}  $00, $00, $00, $00, $00, $C6, $C6, $C6, $C6, $C6, $7E, $0C, $18, $F6, $06, $00,  // char 7925
    $1E, $F6, {|}  $38, $0C, $18, $66, $66, $66, $3C, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 7926
    $1E, $F7, {|}  $00, $38, $0C, $18, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00,  // char 7927
    $1E, $F8, {|}  $3A, $5C, $00, $66, $66, $66, $3C, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 7928
    $1E, $F9, {|}  $00, $00, $76, $DC, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00,  // char 7929
    $1F, $00, {|}  $00, $18, $18, $30, $00, $76, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7936
    $1F, $01, {|}  $00, $30, $30, $18, $00, $76, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7937
    $1F, $02, {|}  $00, $60, $6C, $C6, $00, $76, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7938
    $1F, $03, {|}  $00, $C0, $CC, $66, $00, $76, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7939
    $1F, $04, {|}  $00, $60, $66, $CC, $00, $76, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7940
    $1F, $05, {|}  $00, $C0, $C6, $6C, $00, $76, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7941
    $1F, $06, {|}  $34, $58, $0C, $18, $00, $76, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7942
    $1F, $07, {|}  $34, $58, $30, $18, $00, $76, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 7943
    $20, $00, {|}  $FE, $00, $A4, $AA, $EA, $EA, $A6, $00, $6C, $8A, $4C, $28, $C8, $00, $FE, $00,  // char 8192
    $20, $01, {|}  $FE, $00, $A4, $EA, $AA, $AA, $A6, $00, $6C, $8A, $4C, $28, $C8, $00, $FE, $00,  // char 8193
    $20, $02, {|}  $FE, $00, $EA, $8A, $CE, $8E, $EA, $00, $6C, $8A, $4C, $28, $C8, $00, $FE, $00,  // char 8194
    $20, $03, {|}  $FE, $00, $EA, $8E, $CA, $8A, $EA, $00, $6C, $8A, $4C, $28, $C8, $00, $FE, $00,  // char 8195
    $20, $04, {|}  $FE, $00, $CA, $2E, $4A, $2A, $CA, $00, $6C, $8A, $4C, $28, $C8, $00, $FE, $00,  // char 8196
    $20, $05, {|}  $FE, $00, $AA, $AE, $EA, $2A, $2A, $00, $6C, $8A, $4C, $28, $C8, $00, $FE, $00,  // char 8197
    $20, $06, {|}  $FE, $00, $6A, $8E, $CA, $AA, $4A, $00, $6C, $8A, $4C, $28, $C8, $00, $FE, $00,  // char 8198
    $20, $07, {|}  $FE, $00, $38, $20, $30, $20, $20, $00, $6C, $8A, $4C, $28, $C8, $00, $FE, $00,  // char 8199
    $20, $08, {|}  $FE, $00, $30, $28, $30, $20, $20, $00, $6C, $8A, $4C, $28, $C8, $00, $FE, $00,  // char 8200
    $20, $09, {|}  $FE, $00, $EA, $4A, $4E, $4A, $4A, $00, $6C, $8A, $4C, $28, $C8, $00, $FE, $00,  // char 8201
    $20, $0A, {|}  $FE, $00, $28, $28, $38, $28, $28, $00, $6C, $8A, $4C, $28, $C8, $00, $FE, $00,  // char 8202
    $20, $0B, {|}  $FE, $00, $EA, $2A, $4E, $8E, $EE, $00, $6C, $8A, $4C, $28, $C8, $00, $FE, $00,  // char 8203
    $20, $0C, {|}  $FE, $00, $EA, $2A, $4E, $8E, $EE, $00, $A2, $A2, $E2, $EA, $A4, $00, $FE, $00,  // char 8204
    $20, $0D, {|}  $FE, $00, $EA, $2A, $4E, $8E, $EE, $00, $08, $08, $08, $28, $10, $00, $FE, $00,  // char 8205
    $20, $0E, {|}  $FE, $00, $80, $80, $80, $F0, $28, $30, $28, $0A, $0E, $0E, $0A, $00, $FE, $00,  // char 8206
    $20, $0F, {|}  $FE, $00, $C0, $A0, $C0, $A0, $20, $20, $38, $0A, $0E, $0E, $0A, $00, $FE, $00,  // char 8207
    $20, $10, {|}  $00, $00, $00, $00, $00, $00, $00, $7C, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8208
    $20, $11, {|}  $00, $9C, $D2, $BC, $92, $9C, $00, $3C, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8209
    $20, $12, {|}  $00, $00, $00, $00, $00, $00, $00, $7E, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8210
    $20, $13, {|}  $00, $00, $00, $00, $00, $00, $00, $7E, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8211
    $20, $14, {|}  $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8212
    $20, $15, {|}  $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8213
    $20, $16, {|}  $00, $00, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $00, $00, $00, $00,  // char 8214
    $20, $17, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FE, $00, $FE, $00,  // char 8215
    $20, $18, {|}  $00, $18, $30, $30, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8216
    $20, $19, {|}  $00, $18, $18, $18, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8217
    $20, $1A, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $30, $00, $00,  // char 8218
    $20, $1B, {|}  $00, $30, $30, $30, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8219
    $20, $1C, {|}  $00, $66, $CC, $CC, $CC, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8220
    $20, $1D, {|}  $00, $66, $66, $66, $CC, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8221
    $20, $1E, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $66, $66, $66, $CC, $00, $00,  // char 8222
    $20, $1F, {|}  $00, $CC, $CC, $CC, $66, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8223
    $20, $20, {|}  $00, $00, $18, $18, $18, $7E, $18, $18, $18, $18, $18, $18, $00, $00, $00, $00,  // char 8224
    $20, $21, {|}  $00, $00, $18, $18, $18, $7E, $18, $18, $7E, $18, $18, $18, $00, $00, $00, $00,  // char 8225
    $20, $22, {|}  $00, $00, $00, $00, $00, $00, $18, $3C, $3C, $18, $00, $00, $00, $00, $00, $00,  // char 8226
    $20, $23, {|}  $00, $00, $00, $00, $20, $30, $38, $3C, $38, $30, $20, $00, $00, $00, $00, $00,  // char 8227
    $20, $24, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $00, $00, $00, $00,  // char 8228
    $20, $25, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $66, $66, $00, $00, $00, $00,  // char 8229
    $20, $26, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $DB, $DB, $00, $00, $00, $00,  // char 8230
    $20, $27, {|}  $00, $00, $00, $00, $00, $00, $00, $18, $18, $00, $00, $00, $00, $00, $00, $00,  // char 8231
    $20, $30, {|}  $00, $00, $00, $C0, $C6, $0C, $18, $30, $60, $C0, $36, $36, $00, $00, $00, $00,  // char 8240
    $20, $31, {|}  $00, $00, $00, $C0, $C6, $0C, $18, $30, $60, $C0, $6B, $6B, $00, $00, $00, $00,  // char 8241
    $20, $32, {|}  $00, $00, $18, $18, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8242
    $20, $33, {|}  $00, $00, $6C, $6C, $48, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8243
    $20, $34, {|}  $00, $00, $DB, $DB, $92, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8244
    $20, $35, {|}  $00, $00, $18, $18, $08, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8245
    $20, $36, {|}  $00, $00, $6C, $6C, $24, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8246
    $20, $37, {|}  $00, $00, $DB, $DB, $49, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8247
    $20, $38, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $10, $38, $6C, $C6, $00,  // char 8248
    $20, $39, {|}  $00, $00, $00, $00, $0C, $18, $30, $60, $30, $18, $0C, $00, $00, $00, $00, $00,  // char 8249
    $20, $3A, {|}  $00, $00, $00, $00, $60, $30, $18, $0C, $18, $30, $60, $00, $00, $00, $00, $00,  // char 8250
    $20, $3B, {|}  $00, $00, $00, $00, $92, $44, $28, $92, $28, $44, $92, $00, $00, $00, $00, $00,  // char 8251
    $20, $3C, {|}  $00, $00, $66, $66, $66, $66, $66, $66, $66, $00, $66, $66, $00, $00, $00, $00,  // char 8252
    $20, $3D, {|}  $00, $00, $7E, $C3, $DB, $1B, $1E, $1C, $18, $00, $18, $18, $00, $00, $00, $00,  // char 8253
    $20, $3E, {|}  $00, $FE, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8254
    $20, $3F, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $82, $7C, $00, $00,  // char 8255
    $20, $40, {|}  $7C, $82, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8256
    $20, $41, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $0C, $18, $38, $6C, $00,  // char 8257
    $20, $42, {|}  $00, $00, $00, $10, $38, $28, $00, $00, $44, $EE, $AA, $00, $00, $00, $00, $00,  // char 8258
    $20, $43, {|}  $00, $00, $00, $00, $00, $00, $00, $3C, $3C, $00, $00, $00, $00, $00, $00, $00,  // char 8259
    $20, $44, {|}  $00, $00, $06, $06, $0C, $0C, $18, $18, $30, $30, $60, $60, $00, $00, $00, $00,  // char 8260
    $20, $45, {|}  $00, $00, $3C, $30, $30, $30, $3C, $30, $30, $30, $30, $3C, $00, $00, $00, $00,  // char 8261
    $20, $46, {|}  $00, $00, $3C, $0C, $0C, $0C, $3C, $0C, $0C, $0C, $0C, $3C, $00, $00, $00, $00,  // char 8262
    $20, $48, {|}  $00, $00, $7B, $CF, $CF, $1B, $33, $33, $33, $00, $33, $33, $00, $00, $00, $00,  // char 8264
    $20, $49, {|}  $00, $00, $DE, $F3, $F3, $C6, $CC, $CC, $CC, $00, $CC, $CC, $00, $00, $00, $00,  // char 8265
    $20, $4A, {|}  $00, $00, $00, $00, $00, $00, $00, $7E, $7E, $06, $06, $0C, $0C, $00, $00, $00,  // char 8266
    $20, $4B, {|}  $00, $00, $FE, $DB, $DB, $DB, $DE, $D8, $D8, $D8, $D8, $D8, $00, $00, $00, $00,  // char 8267
    $20, $4C, {|}  $00, $00, $00, $00, $3E, $72, $F2, $F2, $F2, $72, $3E, $00, $00, $00, $00, $00,  // char 8268
    $20, $4D, {|}  $00, $00, $00, $00, $F8, $9C, $9E, $9E, $9E, $9C, $F8, $00, $00, $00, $00, $00,  // char 8269
    $20, $70, {|}  $00, $70, $D8, $D8, $D8, $D8, $70, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8304
    $20, $74, {|}  $00, $18, $38, $78, $D8, $FC, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8308
    $20, $75, {|}  $00, $F8, $C0, $F0, $18, $D8, $70, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8309
    $20, $76, {|}  $00, $78, $C0, $F0, $D8, $D8, $70, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8310
    $20, $77, {|}  $00, $F8, $D8, $30, $30, $60, $60, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8311
    $20, $78, {|}  $00, $70, $D8, $70, $D8, $D8, $70, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8312
    $20, $79, {|}  $00, $70, $D8, $D8, $78, $18, $F0, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8313
    $20, $7A, {|}  $00, $30, $30, $FC, $30, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8314
    $20, $7B, {|}  $00, $00, $00, $FC, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8315
    $20, $7C, {|}  $00, $00, $FC, $00, $FC, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8316
    $20, $7D, {|}  $00, $30, $60, $60, $60, $60, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8317
    $20, $7E, {|}  $00, $60, $30, $30, $30, $30, $60, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8318
    $20, $7F, {|}  $00, $00, $B0, $D8, $D8, $D8, $D8, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8319
    $20, $80, {|}  $00, $00, $00, $00, $00, $00, $00, $70, $D8, $D8, $D8, $D8, $70, $00, $00, $00,  // char 8320
    $20, $81, {|}  $00, $00, $00, $00, $00, $00, $00, $30, $70, $30, $30, $30, $78, $00, $00, $00,  // char 8321
    $20, $82, {|}  $00, $00, $00, $00, $00, $00, $00, $70, $D8, $30, $60, $C8, $F8, $00, $00, $00,  // char 8322
    $20, $83, {|}  $00, $00, $00, $00, $00, $00, $00, $70, $D8, $30, $18, $D8, $70, $00, $00, $00,  // char 8323
    $20, $84, {|}  $00, $00, $00, $00, $00, $00, $00, $18, $38, $78, $D8, $FC, $18, $00, $00, $00,  // char 8324
    $20, $85, {|}  $00, $00, $00, $00, $00, $00, $00, $F8, $C0, $F0, $18, $D8, $70, $00, $00, $00,  // char 8325
    $20, $86, {|}  $00, $00, $00, $00, $00, $00, $00, $78, $C0, $F0, $D8, $D8, $70, $00, $00, $00,  // char 8326
    $20, $87, {|}  $00, $00, $00, $00, $00, $00, $00, $F8, $D8, $30, $30, $60, $60, $00, $00, $00,  // char 8327
    $20, $88, {|}  $00, $00, $00, $00, $00, $00, $00, $70, $D8, $70, $D8, $D8, $70, $00, $00, $00,  // char 8328
    $20, $89, {|}  $00, $00, $00, $00, $00, $00, $00, $70, $D8, $D8, $78, $18, $F0, $00, $00, $00,  // char 8329
    $20, $8A, {|}  $00, $00, $00, $00, $00, $00, $00, $30, $30, $FC, $30, $30, $00, $00, $00, $00,  // char 8330
    $20, $8B, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $FC, $00, $00, $00, $00, $00, $00,  // char 8331
    $20, $8C, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $FC, $00, $FC, $00, $00, $00, $00, $00,  // char 8332
    $20, $8D, {|}  $00, $00, $00, $00, $00, $00, $00, $30, $60, $60, $60, $60, $30, $00, $00, $00,  // char 8333
    $20, $8E, {|}  $00, $00, $00, $00, $00, $00, $00, $60, $30, $30, $30, $30, $60, $00, $00, $00,  // char 8334
    $20, $8F, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $B0, $D8, $D8, $D8, $D8, $00, $00, $00,  // char 8335
    $20, $A0, {|}  $00, $00, $78, $CC, $C0, $CF, $CC, $CC, $7F, $0C, $0C, $0F, $00, $00, $00, $00,  // char 8352
    $20, $A1, {|}  $04, $04, $7C, $CE, $C8, $C8, $D0, $D0, $D0, $E0, $E6, $7C, $40, $40, $00, $00,  // char 8353
    $20, $A2, {|}  $00, $00, $7C, $C6, $C0, $C0, $D6, $D8, $D8, $D8, $DE, $7C, $00, $00, $00, $00,  // char 8354
    $20, $A3, {|}  $00, $00, $F8, $C0, $C0, $C0, $F0, $CD, $CE, $CC, $CC, $CC, $00, $00, $00, $00,  // char 8355
    $20, $A4, {|}  $00, $00, $38, $6C, $60, $F8, $60, $F8, $60, $60, $E6, $FC, $00, $00, $00, $00,  // char 8356
    $20, $A5, {|}  $00, $00, $00, $00, $02, $EC, $D6, $DE, $D6, $D6, $F6, $D6, $40, $00, $00, $00,  // char 8357
    $20, $A6, {|}  $00, $00, $66, $66, $76, $FF, $76, $6E, $FF, $6E, $66, $66, $00, $00, $00, $00,  // char 8358
    $20, $A7, {|}  $00, $00, $FC, $66, $66, $7C, $62, $66, $6F, $66, $66, $F3, $00, $00, $00, $00,  // char 8359
    $20, $A8, {|}  $00, $00, $F8, $CC, $CC, $CC, $F8, $E0, $F3, $D6, $DB, $CE, $00, $00, $00, $00,  // char 8360
    $20, $A9, {|}  $00, $00, $81, $81, $81, $5A, $FF, $5A, $FF, $24, $24, $24, $00, $00, $00, $00,  // char 8361
    $20, $AA, {|}  $00, $00, $00, $00, $F2, $8A, $AA, $AA, $AA, $AA, $A2, $BC, $00, $00, $00, $00,  // char 8362
    $20, $AB, {|}  $00, $00, $0C, $3E, $0C, $3C, $6C, $CC, $CC, $CC, $CC, $76, $00, $7C, $00, $00,  // char 8363
    $20, $AC, {|}  $00, $00, $1C, $36, $60, $FC, $60, $F8, $60, $60, $36, $1C, $00, $00, $00, $00,  // char 8364
    $20, $AD, {|}  $00, $00, $E6, $66, $6C, $78, $FE, $78, $6C, $66, $66, $E6, $00, $00, $00, $00,  // char 8365
    $20, $AE, {|}  $00, $00, $7E, $7E, $5A, $18, $1E, $78, $1E, $78, $18, $3C, $00, $00, $00, $00,  // char 8366
    $20, $AF, {|}  $00, $10, $7C, $B6, $36, $33, $33, $33, $63, $66, $F6, $DC, $00, $00, $00, $00,  // char 8367
    $21, $00, {|}  $00, $70, $D0, $D2, $D6, $7C, $18, $30, $6E, $D8, $98, $18, $0E, $00, $00, $00,  // char 8448
    $21, $01, {|}  $00, $70, $D0, $D2, $D6, $7C, $18, $30, $6E, $D8, $8C, $06, $1C, $00, $00, $00,  // char 8449
    $21, $02, {|}  $00, $00, $3C, $62, $A0, $A0, $A0, $A0, $A0, $A0, $62, $3C, $00, $00, $00, $00,  // char 8450
    $21, $03, {|}  $00, $00, $4E, $B9, $58, $18, $18, $18, $18, $18, $19, $0E, $00, $00, $00, $00,  // char 8451
    $21, $04, {|}  $00, $00, $3C, $18, $7C, $DA, $D8, $D8, $DA, $7C, $19, $3F, $00, $00, $00, $00,  // char 8452
    $21, $05, {|}  $00, $70, $C0, $C2, $C6, $7C, $18, $30, $6E, $DB, $9B, $1B, $0E, $00, $00, $00,  // char 8453
    $21, $06, {|}  $00, $70, $C0, $C2, $C6, $7C, $18, $30, $7B, $DB, $9B, $1B, $0D, $00, $00, $00,  // char 8454
    $21, $07, {|}  $00, $00, $7C, $C6, $C2, $C0, $78, $C0, $C0, $C2, $C6, $7C, $00, $00, $00, $00,  // char 8455
    $21, $08, {|}  $00, $00, $78, $CC, $86, $26, $3E, $26, $06, $86, $CC, $78, $00, $00, $00, $00,  // char 8456
    $21, $09, {|}  $00, $00, $7F, $B9, $58, $1A, $1E, $1A, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 8457
    $21, $0A, {|}  $00, $00, $00, $00, $00, $1A, $66, $46, $87, $8C, $8C, $7C, $98, $98, $70, $00,  // char 8458
    $21, $0B, {|}  $00, $00, $52, $B5, $15, $16, $2C, $34, $68, $A9, $AA, $4C, $00, $00, $00, $00,  // char 8459
    $21, $0C, {|}  $00, $00, $48, $B0, $80, $48, $7C, $66, $26, $26, $A6, $46, $06, $34, $48, $00,  // char 8460
    $21, $0D, {|}  $00, $00, $E2, $A2, $A2, $A2, $BE, $A2, $A2, $A2, $A2, $E2, $00, $00, $00, $00,  // char 8461
    $21, $0E, {|}  $00, $00, $38, $18, $18, $36, $3B, $3B, $33, $66, $66, $E6, $00, $00, $00, $00,  // char 8462
    $21, $0F, {|}  $00, $00, $38, $1E, $18, $76, $3B, $3B, $33, $66, $66, $E6, $00, $00, $00, $00,  // char 8463
    $21, $10, {|}  $00, $00, $33, $4E, $06, $06, $0C, $0C, $1F, $6C, $88, $70, $00, $00, $00, $00,  // char 8464
    $21, $11, {|}  $00, $00, $71, $8E, $04, $0C, $06, $03, $63, $C3, $42, $3C, $00, $00, $00, $00,  // char 8465
    $21, $12, {|}  $00, $00, $46, $49, $39, $1E, $18, $30, $70, $B1, $BA, $6C, $00, $00, $00, $00,  // char 8466
    $21, $13, {|}  $00, $00, $0E, $19, $31, $32, $64, $68, $70, $E1, $66, $38, $00, $00, $00, $00,  // char 8467
    $21, $14, {|}  $00, $00, $D8, $FF, $D8, $DE, $DB, $DB, $DB, $DB, $DB, $DE, $00, $00, $00, $00,  // char 8468
    $21, $15, {|}  $00, $00, $E2, $A2, $B2, $B2, $AA, $AA, $A6, $A6, $A2, $E2, $00, $00, $00, $00,  // char 8469
    $21, $16, {|}  $00, $00, $CC, $CF, $ED, $FF, $FC, $DF, $CC, $CC, $CC, $CC, $00, $00, $00, $00,  // char 8470
    $21, $17, {|}  $00, $00, $3C, $42, $B9, $A5, $A5, $B9, $A1, $A1, $42, $3C, $00, $00, $00, $00,  // char 8471
    $21, $18, {|}  $00, $00, $00, $00, $40, $8E, $93, $A3, $63, $4B, $6A, $A4, $B0, $B0, $60, $00,  // char 8472
    $21, $19, {|}  $00, $00, $FC, $A2, $A2, $A2, $A2, $BC, $A0, $A0, $A0, $E0, $00, $00, $00, $00,  // char 8473
    $21, $1A, {|}  $00, $00, $7C, $A2, $A2, $A2, $A2, $A2, $A2, $AA, $A6, $7E, $01, $00, $00, $00,  // char 8474
    $21, $1B, {|}  $00, $00, $3E, $4D, $4D, $19, $1E, $1C, $34, $34, $B5, $62, $00, $00, $00, $00,  // char 8475
    $21, $1C, {|}  $00, $00, $58, $A6, $A3, $6C, $B8, $26, $26, $26, $A7, $C2, $00, $00, $00, $00,  // char 8476
    $21, $1D, {|}  $00, $00, $FC, $A2, $A2, $A2, $A2, $BC, $B0, $A8, $A4, $E2, $00, $00, $00, $00,  // char 8477
    $21, $1E, {|}  $00, $00, $FC, $66, $66, $66, $7C, $6C, $6D, $67, $66, $EE, $08, $00, $00, $00,  // char 8478
    $21, $1F, {|}  $30, $0C, $FC, $C6, $C6, $C6, $FC, $D8, $DC, $DC, $E6, $E6, $20, $00, $00, $00,  // char 8479
    $21, $20, {|}  $00, $00, $71, $DB, $35, $D1, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8480
    $21, $21, {|}  $00, $00, $00, $00, $00, $00, $00, $FE, $52, $5A, $52, $5F, $00, $00, $00, $00,  // char 8481
    $21, $22, {|}  $00, $00, $F1, $5B, $55, $51, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8482
    $21, $23, {|}  $60, $18, $CE, $D6, $D6, $D6, $D6, $D6, $E6, $6C, $38, $50, $40, $00, $00, $00,  // char 8483
    $21, $24, {|}  $00, $00, $FE, $0A, $14, $14, $28, $28, $50, $50, $A0, $FE, $00, $00, $00, $00,  // char 8484
    $21, $25, {|}  $00, $00, $FE, $9C, $30, $FE, $0C, $18, $38, $0C, $06, $06, $C6, $C6, $7C, $00,  // char 8485
    $21, $26, {|}  $00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $6C, $6C, $6C, $EE, $00, $00, $00, $00,  // char 8486
    $21, $27, {|}  $00, $00, $EE, $6C, $6C, $6C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 8487
    $21, $28, {|}  $00, $00, $18, $26, $03, $13, $0C, $13, $03, $03, $66, $98, $00, $00, $00, $00,  // char 8488
    $21, $29, {|}  $00, $00, $00, $00, $00, $70, $18, $18, $18, $18, $1C, $18, $00, $00, $00, $00,  // char 8489
    $21, $2A, {|}  $00, $00, $E6, $66, $66, $6C, $78, $78, $6C, $66, $66, $E6, $00, $00, $00, $00,  // char 8490
    $21, $2B, {|}  $38, $6C, $38, $10, $38, $38, $6C, $6C, $C6, $FE, $C6, $C6, $00, $00, $00, $00,  // char 8491
    $21, $2C, {|}  $00, $00, $1E, $2D, $2D, $1A, $1E, $1B, $31, $31, $B2, $6C, $00, $00, $00, $00,  // char 8492
    $21, $2D, {|}  $00, $00, $14, $6B, $C8, $CC, $C6, $C6, $CC, $C0, $63, $1C, $00, $00, $00, $00,  // char 8493
    $21, $2E, {|}  $00, $00, $00, $00, $00, $3C, $66, $E7, $FF, $E0, $67, $3E, $00, $00, $00, $00,  // char 8494
    $21, $2F, {|}  $00, $00, $00, $00, $00, $1C, $26, $44, $F8, $C0, $C8, $70, $00, $00, $00, $00,  // char 8495
    $21, $30, {|}  $00, $00, $4C, $52, $3C, $10, $3C, $60, $C0, $C3, $CC, $78, $00, $00, $00, $00,  // char 8496
    $21, $31, {|}  $00, $00, $73, $CE, $18, $3A, $5C, $18, $30, $30, $B0, $60, $00, $00, $00, $00,  // char 8497
    $21, $32, {|}  $00, $00, $1E, $0C, $0C, $0C, $2C, $3C, $2C, $8C, $CC, $FE, $00, $00, $00, $00,  // char 8498
    $21, $33, {|}  $00, $00, $12, $12, $12, $16, $36, $3E, $3A, $5A, $52, $91, $00, $00, $00, $00,  // char 8499
    $21, $34, {|}  $00, $00, $00, $00, $00, $1C, $26, $46, $C6, $C4, $C8, $70, $00, $00, $00, $00,  // char 8500
    $21, $35, {|}  $00, $00, $42, $C6, $E7, $7A, $38, $5C, $CE, $E7, $63, $E2, $00, $00, $00, $00,  // char 8501
    $21, $36, {|}  $00, $00, $80, $FC, $7E, $06, $06, $06, $06, $0C, $7E, $FE, $00, $00, $00, $00,  // char 8502
    $21, $37, {|}  $00, $00, $40, $78, $3C, $0C, $0C, $0C, $0C, $1E, $7E, $F2, $00, $00, $00, $00,  // char 8503
    $21, $38, {|}  $00, $00, $80, $FE, $7E, $0C, $0C, $0C, $0C, $0C, $1C, $18, $00, $00, $00, $00,  // char 8504
    $21, $39, {|}  $00, $00, $38, $38, $00, $78, $38, $38, $38, $38, $38, $7C, $00, $00, $00, $00,  // char 8505
    $21, $3A, {|}  $00, $00, $00, $00, $00, $7D, $FF, $83, $86, $82, $FE, $7C, $00, $00, $00, $00,  // char 8506
    $21, $53, {|}  $00, $C0, $C0, $C2, $C6, $CC, $18, $30, $60, $DC, $86, $1C, $06, $1C, $00, $00,  // char 8531
    $21, $54, {|}  $00, $E0, $30, $62, $C6, $FC, $18, $30, $60, $DC, $86, $1C, $06, $1C, $00, $00,  // char 8532
    $21, $55, {|}  $00, $C0, $C0, $C2, $C6, $CC, $18, $30, $60, $DE, $98, $1C, $06, $1C, $00, $00,  // char 8533
    $21, $56, {|}  $00, $E0, $30, $62, $C6, $FC, $18, $30, $60, $DE, $98, $1C, $06, $1C, $00, $00,  // char 8534
    $21, $57, {|}  $00, $E0, $30, $62, $36, $EC, $18, $30, $60, $DE, $98, $1C, $06, $1C, $00, $00,  // char 8535
    $21, $58, {|}  $00, $30, $70, $B2, $F6, $3C, $18, $30, $60, $DE, $98, $1C, $06, $1C, $00, $00,  // char 8536
    $21, $59, {|}  $00, $C0, $C0, $C2, $C6, $CC, $18, $30, $60, $DC, $B0, $3C, $36, $1C, $00, $00,  // char 8537
    $21, $5A, {|}  $00, $F0, $C0, $E2, $36, $EC, $18, $30, $60, $DC, $B0, $3C, $36, $1C, $00, $00,  // char 8538
    $21, $5B, {|}  $00, $C0, $C0, $C2, $C6, $CC, $18, $30, $60, $DC, $B6, $1C, $36, $1C, $00, $00,  // char 8539
    $21, $5C, {|}  $00, $E0, $30, $62, $36, $EC, $18, $30, $60, $DC, $B6, $1C, $36, $1C, $00, $00,  // char 8540
    $21, $5D, {|}  $00, $F0, $80, $E2, $36, $EC, $18, $30, $60, $DC, $B6, $1C, $36, $1C, $00, $00,  // char 8541
    $21, $5E, {|}  $00, $F0, $30, $62, $66, $6C, $18, $30, $60, $DC, $B6, $1C, $36, $1C, $00, $00,  // char 8542
    $21, $5F, {|}  $00, $C0, $C0, $C2, $C6, $CC, $18, $30, $60, $C0, $80, $00, $00, $00, $00, $00,  // char 8543
    $21, $60, {|}  $00, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 8544
    $21, $61, {|}  $00, $00, $FE, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $FE, $00, $00, $00, $00,  // char 8545
    $21, $62, {|}  $00, $00, $FF, $DB, $DB, $DB, $DB, $DB, $DB, $DB, $DB, $FF, $00, $00, $00, $00,  // char 8546
    $21, $63, {|}  $00, $00, $DB, $DB, $DB, $DB, $DB, $DB, $DB, $CE, $CE, $C4, $00, $00, $00, $00,  // char 8547
    $21, $64, {|}  $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $6C, $38, $10, $00, $00, $00, $00,  // char 8548
    $21, $65, {|}  $00, $00, $DB, $DB, $DB, $DB, $DB, $DB, $DB, $73, $73, $23, $00, $00, $00, $00,  // char 8549
    $21, $66, {|}  $00, $00, $95, $95, $95, $95, $95, $95, $95, $65, $65, $65, $00, $00, $00, $00,  // char 8550
    $21, $67, {|}  $00, $00, $B5, $B5, $B5, $B5, $B5, $B5, $55, $55, $55, $55, $00, $00, $00, $00,  // char 8551
    $21, $68, {|}  $00, $00, $DB, $DB, $DB, $CE, $CE, $CE, $DB, $DB, $DB, $DB, $00, $00, $00, $00,  // char 8552
    $21, $69, {|}  $00, $00, $C6, $C6, $6C, $7C, $38, $38, $7C, $6C, $C6, $C6, $00, $00, $00, $00,  // char 8553
    $21, $6A, {|}  $00, $00, $DB, $DB, $DB, $73, $73, $73, $DB, $DB, $DB, $DB, $00, $00, $00, $00,  // char 8554
    $21, $6B, {|}  $00, $00, $95, $95, $95, $65, $65, $65, $95, $95, $95, $95, $00, $00, $00, $00,  // char 8555
    $21, $6C, {|}  $00, $00, $F0, $60, $60, $60, $60, $60, $60, $62, $66, $FE, $00, $00, $00, $00,  // char 8556
    $21, $6D, {|}  $00, $00, $3C, $66, $C2, $C0, $C0, $C0, $C0, $C2, $66, $3C, $00, $00, $00, $00,  // char 8557
    $21, $6E, {|}  $00, $00, $F8, $6C, $66, $66, $66, $66, $66, $66, $6C, $F8, $00, $00, $00, $00,  // char 8558
    $21, $6F, {|}  $00, $00, $C6, $EE, $FE, $FE, $D6, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 8559
    $21, $70, {|}  $00, $00, $18, $18, $00, $18, $18, $18, $18, $18, $18, $18, $00, $00, $00, $00,  // char 8560
    $21, $71, {|}  $00, $00, $6C, $6C, $00, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $00, $00, $00, $00,  // char 8561
    $21, $72, {|}  $00, $00, $DB, $DB, $00, $DB, $DB, $DB, $DB, $DB, $DB, $DB, $00, $00, $00, $00,  // char 8562
    $21, $73, {|}  $00, $00, $C0, $C0, $00, $DB, $DB, $DB, $DB, $DB, $CE, $C4, $00, $00, $00, $00,  // char 8563
    $21, $74, {|}  $00, $00, $00, $00, $00, $66, $66, $66, $66, $66, $3C, $18, $00, $00, $00, $00,  // char 8564
    $21, $75, {|}  $00, $00, $03, $03, $00, $DB, $DB, $DB, $DB, $DB, $73, $23, $00, $00, $00, $00,  // char 8565
    $21, $76, {|}  $00, $00, $05, $05, $00, $95, $95, $95, $95, $95, $65, $65, $00, $00, $00, $00,  // char 8566
    $21, $77, {|}  $00, $00, $15, $15, $00, $B5, $B5, $B5, $B5, $55, $55, $55, $00, $00, $00, $00,  // char 8567
    $21, $78, {|}  $00, $00, $C0, $C0, $00, $DB, $DB, $CE, $CE, $CE, $DB, $DB, $00, $00, $00, $00,  // char 8568
    $21, $79, {|}  $00, $00, $00, $00, $00, $C6, $6C, $38, $38, $38, $6C, $C6, $00, $00, $00, $00,  // char 8569
    $21, $7A, {|}  $00, $00, $03, $03, $00, $DB, $DB, $73, $73, $73, $DB, $DB, $00, $00, $00, $00,  // char 8570
    $21, $7B, {|}  $00, $00, $05, $05, $00, $95, $95, $65, $65, $95, $95, $95, $00, $00, $00, $00,  // char 8571
    $21, $7C, {|}  $00, $00, $38, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00,  // char 8572
    $21, $7D, {|}  $00, $00, $00, $00, $00, $7C, $C6, $C0, $C0, $C0, $C6, $7C, $00, $00, $00, $00,  // char 8573
    $21, $7E, {|}  $00, $00, $1C, $0C, $0C, $3C, $6C, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00,  // char 8574
    $21, $7F, {|}  $00, $00, $00, $00, $00, $EC, $FE, $D6, $D6, $D6, $D6, $C6, $00, $00, $00, $00,  // char 8575
    $21, $80, {|}  $00, $00, $3C, $5A, $DB, $DB, $DB, $DB, $DB, $DB, $5A, $3C, $00, $00, $00, $00,  // char 8576
    $21, $81, {|}  $00, $00, $F8, $CC, $E6, $D6, $D6, $D6, $D6, $E6, $CC, $F8, $00, $00, $00, $00,  // char 8577
    $21, $82, {|}  $00, $00, $3C, $5A, $99, $BD, $DB, $DB, $BD, $99, $5A, $3C, $00, $00, $00, $00,  // char 8578
    $21, $83, {|}  $00, $00, $78, $CC, $86, $06, $06, $06, $06, $86, $CC, $78, $00, $00, $00, $00,  // char 8579
    $21, $90, {|}  $00, $00, $00, $00, $00, $30, $60, $FF, $60, $30, $00, $00, $00, $00, $00, $00,  // char 8592
    $21, $91, {|}  $00, $00, $18, $3C, $7E, $18, $18, $18, $18, $18, $18, $18, $00, $00, $00, $00,  // char 8593
    $21, $92, {|}  $00, $00, $00, $00, $00, $0C, $06, $FF, $06, $0C, $00, $00, $00, $00, $00, $00,  // char 8594
    $21, $93, {|}  $00, $00, $18, $18, $18, $18, $18, $18, $18, $7E, $3C, $18, $00, $00, $00, $00,  // char 8595
    $21, $94, {|}  $00, $00, $00, $00, $00, $24, $66, $FF, $66, $24, $00, $00, $00, $00, $00, $00,  // char 8596
    $21, $95, {|}  $00, $00, $18, $3C, $7E, $18, $18, $18, $7E, $3C, $18, $00, $00, $00, $00, $00,  // char 8597
    $21, $96, {|}  $00, $00, $00, $F0, $E0, $B0, $18, $0C, $06, $03, $00, $00, $00, $00, $00, $00,  // char 8598
    $21, $97, {|}  $00, $00, $00, $0F, $07, $0D, $18, $30, $60, $C0, $00, $00, $00, $00, $00, $00,  // char 8599
    $21, $98, {|}  $00, $00, $00, $00, $C0, $60, $30, $18, $0D, $07, $0F, $00, $00, $00, $00, $00,  // char 8600
    $21, $99, {|}  $00, $00, $00, $00, $03, $06, $0C, $18, $B0, $E0, $F0, $00, $00, $00, $00, $00,  // char 8601
    $21, $9A, {|}  $00, $00, $00, $00, $00, $32, $62, $FF, $64, $34, $00, $00, $00, $00, $00, $00,  // char 8602
    $21, $9B, {|}  $00, $00, $00, $00, $00, $2C, $26, $FF, $46, $4C, $00, $00, $00, $00, $00, $00,  // char 8603
    $21, $9C, {|}  $00, $00, $00, $00, $00, $00, $E0, $CE, $BB, $00, $00, $00, $00, $00, $00, $00,  // char 8604
    $21, $9D, {|}  $00, $00, $00, $00, $00, $00, $07, $73, $DD, $00, $00, $00, $00, $00, $00, $00,  // char 8605
    $21, $9E, {|}  $00, $00, $00, $00, $00, $36, $6C, $FF, $6C, $36, $00, $00, $00, $00, $00, $00,  // char 8606
    $21, $9F, {|}  $00, $00, $18, $3C, $7E, $18, $3C, $7E, $18, $18, $18, $18, $00, $00, $00, $00,  // char 8607
    $21, $A0, {|}  $00, $00, $00, $00, $00, $6C, $36, $FF, $36, $6C, $00, $00, $00, $00, $00, $00,  // char 8608
    $21, $A1, {|}  $00, $00, $18, $18, $18, $18, $7E, $3C, $18, $7E, $3C, $18, $00, $00, $00, $00,  // char 8609
    $21, $A2, {|}  $00, $00, $00, $00, $00, $33, $66, $FC, $66, $33, $00, $00, $00, $00, $00, $00,  // char 8610
    $21, $A3, {|}  $00, $00, $00, $00, $00, $CC, $66, $3F, $66, $CC, $00, $00, $00, $00, $00, $00,  // char 8611
    $21, $A4, {|}  $00, $00, $00, $00, $00, $33, $63, $FF, $63, $33, $00, $00, $00, $00, $00, $00,  // char 8612
    $21, $A5, {|}  $00, $00, $18, $3C, $7E, $18, $18, $18, $18, $18, $18, $7E, $00, $00, $00, $00,  // char 8613
    $21, $A6, {|}  $00, $00, $00, $00, $00, $CC, $C6, $FF, $C6, $CC, $00, $00, $00, $00, $00, $00,  // char 8614
    $21, $A7, {|}  $00, $00, $7E, $18, $18, $18, $18, $18, $18, $7E, $3C, $18, $00, $00, $00, $00,  // char 8615
    $21, $A8, {|}  $00, $00, $18, $3C, $7E, $18, $18, $18, $7E, $3C, $18, $7E, $00, $00, $00, $00,  // char 8616
    $21, $A9, {|}  $00, $00, $00, $00, $06, $33, $63, $FE, $60, $30, $00, $00, $00, $00, $00, $00,  // char 8617
    $21, $AA, {|}  $00, $00, $00, $00, $60, $CC, $C6, $7F, $06, $0C, $00, $00, $00, $00, $00, $00,  // char 8618
    $21, $AB, {|}  $00, $00, $00, $00, $06, $3B, $6B, $FE, $68, $30, $00, $00, $00, $00, $00, $00,  // char 8619
    $21, $AC, {|}  $00, $00, $00, $00, $60, $DC, $D6, $7F, $16, $0C, $00, $00, $00, $00, $00, $00,  // char 8620
    $21, $AD, {|}  $00, $00, $00, $00, $00, $24, $5A, $FF, $66, $24, $00, $00, $00, $00, $00, $00,  // char 8621
    $21, $AE, {|}  $00, $00, $00, $00, $00, $2C, $6E, $FF, $76, $34, $00, $00, $00, $00, $00, $00,  // char 8622
    $21, $AF, {|}  $00, $00, $30, $30, $60, $63, $FF, $C6, $16, $1C, $1C, $1E, $00, $00, $00, $00,  // char 8623
    $21, $B0, {|}  $00, $00, $00, $30, $60, $FE, $66, $36, $06, $06, $06, $06, $00, $00, $00, $00,  // char 8624
    $21, $B1, {|}  $00, $00, $00, $18, $0C, $FE, $CC, $D8, $C0, $C0, $C0, $C0, $00, $00, $00, $00,  // char 8625
    $21, $B2, {|}  $00, $00, $00, $06, $06, $06, $06, $36, $66, $FE, $60, $30, $00, $00, $00, $00,  // char 8626
    $21, $B3, {|}  $00, $00, $00, $C0, $C0, $C0, $C0, $D8, $CC, $FE, $0C, $18, $00, $00, $00, $00,  // char 8627
    $21, $B4, {|}  $00, $00, $00, $00, $00, $00, $00, $FC, $0C, $0C, $3F, $1E, $0C, $00, $00, $00,  // char 8628
    $21, $B5, {|}  $00, $00, $00, $00, $00, $00, $06, $36, $66, $FE, $60, $30, $00, $00, $00, $00,  // char 8629
    $21, $B6, {|}  $00, $00, $00, $00, $1E, $33, $33, $33, $30, $FC, $78, $30, $00, $00, $00, $00,  // char 8630
    $21, $B7, {|}  $00, $00, $00, $00, $78, $CC, $CC, $CC, $0C, $3F, $1E, $0C, $00, $00, $00, $00,  // char 8631
    $21, $B8, {|}  $00, $00, $FF, $00, $F0, $E0, $B0, $18, $0C, $06, $03, $00, $00, $00, $00, $00,  // char 8632
    $21, $B9, {|}  $00, $00, $98, $B0, $FF, $B0, $98, $19, $0D, $FF, $0D, $19, $00, $00, $00, $00,  // char 8633
    $21, $BA, {|}  $00, $00, $00, $1E, $1C, $16, $03, $C3, $C3, $C3, $66, $3C, $00, $00, $00, $00,  // char 8634
    $21, $BB, {|}  $00, $00, $00, $78, $38, $68, $C0, $C3, $C3, $C3, $66, $3C, $00, $00, $00, $00,  // char 8635
    $21, $BC, {|}  $00, $00, $00, $00, $00, $30, $60, $FF, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8636
    $21, $BD, {|}  $00, $00, $00, $00, $00, $00, $00, $FF, $60, $30, $00, $00, $00, $00, $00, $00,  // char 8637
    $21, $BE, {|}  $00, $00, $18, $1C, $1E, $18, $18, $18, $18, $18, $18, $18, $00, $00, $00, $00,  // char 8638
    $21, $BF, {|}  $00, $00, $18, $38, $78, $18, $18, $18, $18, $18, $18, $18, $00, $00, $00, $00,  // char 8639
    $21, $C0, {|}  $00, $00, $00, $00, $00, $0C, $06, $FF, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8640
    $21, $C1, {|}  $00, $00, $00, $00, $00, $00, $00, $FF, $06, $0C, $00, $00, $00, $00, $00, $00,  // char 8641
    $21, $C2, {|}  $00, $00, $18, $18, $18, $18, $18, $18, $18, $1E, $1C, $18, $00, $00, $00, $00,  // char 8642
    $21, $C3, {|}  $00, $00, $18, $18, $18, $18, $18, $18, $18, $78, $38, $18, $00, $00, $00, $00,  // char 8643
    $21, $C4, {|}  $00, $00, $0C, $06, $FF, $06, $0C, $30, $60, $FF, $60, $30, $00, $00, $00, $00,  // char 8644
    $21, $C5, {|}  $00, $00, $24, $2E, $3F, $24, $24, $24, $24, $FC, $74, $24, $00, $00, $00, $00,  // char 8645
    $21, $C6, {|}  $00, $00, $30, $60, $FF, $60, $30, $0C, $06, $FF, $06, $0C, $00, $00, $00, $00,  // char 8646
    $21, $C7, {|}  $00, $00, $30, $60, $FE, $60, $30, $00, $30, $60, $FE, $60, $30, $00, $00, $00,  // char 8647
    $21, $C8, {|}  $00, $00, $24, $7E, $FF, $24, $24, $24, $24, $24, $24, $24, $00, $00, $00, $00,  // char 8648
    $21, $C9, {|}  $00, $00, $18, $0C, $FE, $0C, $18, $00, $18, $0C, $FE, $0C, $18, $00, $00, $00,  // char 8649
    $21, $CA, {|}  $00, $00, $24, $24, $24, $24, $24, $24, $24, $FF, $7E, $24, $00, $00, $00, $00,  // char 8650
    $21, $CB, {|}  $00, $00, $00, $00, $30, $60, $FF, $00, $FF, $06, $0C, $00, $00, $00, $00, $00,  // char 8651
    $21, $CC, {|}  $00, $00, $00, $00, $0C, $06, $FF, $00, $FF, $60, $30, $00, $00, $00, $00, $00,  // char 8652
    $21, $CD, {|}  $00, $00, $00, $00, $00, $22, $7F, $C4, $7F, $24, $00, $00, $00, $00, $00, $00,  // char 8653
    $21, $CE, {|}  $00, $00, $00, $00, $00, $2C, $7E, $CB, $7E, $34, $00, $00, $00, $00, $00, $00,  // char 8654
    $21, $CF, {|}  $00, $00, $00, $00, $00, $24, $FE, $23, $FE, $44, $00, $00, $00, $00, $00, $00,  // char 8655
    $21, $D0, {|}  $00, $00, $00, $00, $00, $20, $7F, $C0, $7F, $20, $00, $00, $00, $00, $00, $00,  // char 8656
    $21, $D1, {|}  $00, $00, $38, $7C, $EE, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $00, $00, $00, $00,  // char 8657
    $21, $D2, {|}  $00, $00, $00, $00, $00, $04, $FE, $03, $FE, $04, $00, $00, $00, $00, $00, $00,  // char 8658
    $21, $D3, {|}  $00, $00, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $EE, $7C, $38, $00, $00, $00, $00,  // char 8659
    $21, $D4, {|}  $00, $00, $00, $00, $00, $24, $7E, $C3, $7E, $24, $00, $00, $00, $00, $00, $00,  // char 8660
    $21, $D5, {|}  $00, $00, $38, $7C, $EE, $6C, $6C, $6C, $6C, $EE, $7C, $38, $00, $00, $00, $00,  // char 8661
    $21, $D6, {|}  $00, $00, $00, $F0, $E0, $B0, $D8, $EC, $B6, $1B, $0C, $04, $00, $00, $00, $00,  // char 8662
    $21, $D7, {|}  $00, $00, $00, $0F, $07, $0D, $1B, $37, $6D, $D8, $30, $20, $00, $00, $00, $00,  // char 8663
    $21, $D8, {|}  $00, $00, $00, $20, $30, $D8, $6D, $37, $1B, $0D, $07, $0F, $00, $00, $00, $00,  // char 8664
    $21, $D9, {|}  $00, $00, $00, $04, $0C, $1B, $B6, $EC, $D8, $B0, $E0, $F0, $00, $00, $00, $00,  // char 8665
    $21, $DA, {|}  $00, $00, $00, $00, $10, $3F, $60, $FF, $60, $3F, $10, $00, $00, $00, $00, $00,  // char 8666
    $21, $DB, {|}  $00, $00, $00, $00, $08, $FC, $06, $FF, $06, $FC, $08, $00, $00, $00, $00, $00,  // char 8667
    $21, $DC, {|}  $00, $00, $00, $00, $00, $30, $6A, $FF, $65, $30, $00, $00, $00, $00, $00, $00,  // char 8668
    $21, $DD, {|}  $00, $00, $00, $00, $00, $0C, $A6, $FF, $56, $0C, $00, $00, $00, $00, $00, $00,  // char 8669
    $21, $DE, {|}  $00, $00, $18, $3C, $7E, $18, $18, $3C, $18, $3C, $18, $18, $00, $00, $00, $00,  // char 8670
    $21, $DF, {|}  $00, $00, $18, $18, $3C, $18, $3C, $18, $18, $7E, $3C, $18, $00, $00, $00, $00,  // char 8671
    $21, $E0, {|}  $00, $00, $00, $00, $00, $30, $60, $D5, $60, $30, $00, $00, $00, $00, $00, $00,  // char 8672
    $21, $E1, {|}  $00, $00, $18, $3C, $66, $18, $00, $18, $00, $18, $00, $18, $00, $00, $00, $00,  // char 8673
    $21, $E2, {|}  $00, $00, $00, $00, $00, $0C, $06, $AB, $06, $0C, $00, $00, $00, $00, $00, $00,  // char 8674
    $21, $E3, {|}  $00, $00, $18, $00, $18, $00, $18, $00, $18, $66, $3C, $18, $00, $00, $00, $00,  // char 8675
    $21, $E4, {|}  $00, $00, $00, $00, $00, $98, $B0, $FF, $B0, $98, $00, $00, $00, $00, $00, $00,  // char 8676
    $21, $E5, {|}  $00, $00, $00, $00, $00, $19, $0D, $FF, $0D, $19, $00, $00, $00, $00, $00, $00,  // char 8677
    $21, $E6, {|}  $00, $00, $00, $00, $10, $3F, $41, $81, $41, $3F, $10, $00, $00, $00, $00, $00,  // char 8678
    $21, $E7, {|}  $00, $00, $10, $28, $44, $C6, $44, $44, $44, $44, $44, $7C, $00, $00, $00, $00,  // char 8679
    $21, $E8, {|}  $00, $00, $00, $00, $08, $FC, $82, $81, $82, $FC, $08, $00, $00, $00, $00, $00,  // char 8680
    $21, $E9, {|}  $00, $00, $7C, $44, $44, $44, $44, $44, $C6, $44, $28, $10, $00, $00, $00, $00,  // char 8681
    $21, $EA, {|}  $00, $00, $10, $28, $44, $C6, $44, $44, $7C, $00, $7C, $44, $7C, $00, $00, $00,  // char 8682
    $21, $EB, {|}  $00, $00, $10, $28, $44, $C6, $44, $44, $44, $C6, $82, $FE, $00, $00, $00, $00,  // char 8683
    $21, $EC, {|}  $00, $00, $10, $28, $7C, $C6, $44, $44, $44, $C6, $82, $FE, $00, $00, $00, $00,  // char 8684
    $21, $ED, {|}  $00, $00, $10, $38, $54, $D6, $54, $54, $54, $D6, $92, $FE, $00, $00, $00, $00,  // char 8685
    $21, $EE, {|}  $00, $10, $28, $54, $EE, $44, $C6, $44, $44, $44, $44, $7C, $00, $00, $00, $00,  // char 8686
    $21, $EF, {|}  $00, $10, $28, $54, $EE, $44, $C6, $44, $44, $C6, $82, $FE, $00, $00, $00, $00,  // char 8687
    $21, $F0, {|}  $00, $00, $00, $00, $E8, $BC, $82, $81, $82, $BC, $E8, $00, $00, $00, $00, $00,  // char 8688
    $21, $F1, {|}  $00, $00, $FF, $80, $BC, $B8, $AC, $86, $83, $81, $80, $00, $00, $00, $00, $00,  // char 8689
    $21, $F2, {|}  $00, $00, $00, $01, $81, $C1, $61, $35, $1D, $3D, $01, $FF, $00, $00, $00, $00,  // char 8690
    $21, $F3, {|}  $00, $00, $10, $28, $44, $C6, $44, $44, $44, $C6, $44, $28, $10, $00, $00, $00,  // char 8691
    $22, $00, {|}  $00, $00, $C6, $C6, $FE, $C6, $6C, $6C, $38, $38, $10, $10, $00, $00, $00, $00,  // char 8704
    $22, $01, {|}  $00, $00, $00, $00, $00, $3C, $66, $60, $60, $60, $60, $60, $66, $3C, $00, $00,  // char 8705
    $22, $02, {|}  $00, $00, $38, $6C, $06, $06, $3E, $66, $C6, $C6, $CC, $78, $00, $00, $00, $00,  // char 8706
    $22, $03, {|}  $00, $00, $FE, $06, $06, $06, $7E, $06, $06, $06, $06, $FE, $00, $00, $00, $00,  // char 8707
    $22, $04, {|}  $00, $08, $FE, $16, $16, $16, $7E, $16, $26, $26, $26, $FE, $40, $00, $00, $00,  // char 8708
    $22, $05, {|}  $00, $00, $03, $3E, $66, $CF, $DB, $DB, $F3, $66, $7C, $C0, $00, $00, $00, $00,  // char 8709
    $22, $06, {|}  $00, $00, $10, $10, $38, $38, $6C, $6C, $C6, $C6, $C6, $FE, $00, $00, $00, $00,  // char 8710
    $22, $07, {|}  $00, $00, $FE, $C6, $C6, $C6, $6C, $6C, $38, $38, $10, $10, $00, $00, $00, $00,  // char 8711
    $22, $08, {|}  $00, $00, $00, $3E, $60, $C0, $C0, $FE, $C0, $C0, $60, $3E, $00, $00, $00, $00,  // char 8712
    $22, $09, {|}  $00, $00, $04, $3E, $64, $C8, $C8, $FE, $C8, $D0, $70, $3E, $20, $00, $00, $00,  // char 8713
    $22, $0A, {|}  $00, $00, $00, $00, $3E, $60, $C0, $FE, $C0, $60, $3E, $00, $00, $00, $00, $00,  // char 8714
    $22, $0B, {|}  $00, $00, $00, $F8, $0C, $06, $06, $FE, $06, $06, $0C, $F8, $00, $00, $00, $00,  // char 8715
    $22, $0C, {|}  $00, $00, $08, $F8, $1C, $16, $26, $FE, $26, $26, $4C, $F8, $40, $00, $00, $00,  // char 8716
    $22, $0D, {|}  $00, $00, $00, $00, $F8, $0C, $06, $FE, $06, $0C, $F8, $00, $00, $00, $00, $00,  // char 8717
    $22, $0E, {|}  $00, $00, $00, $00, $7E, $7E, $7E, $7E, $7E, $7E, $7E, $00, $00, $00, $00, $00,  // char 8718
    $22, $0F, {|}  $00, $00, $FF, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $E7, $00, $00,  // char 8719
    $22, $10, {|}  $00, $00, $E7, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $FF, $00, $00,  // char 8720
    $22, $11, {|}  $00, $00, $FF, $C1, $60, $30, $18, $0C, $0C, $18, $30, $60, $C1, $FF, $00, $00,  // char 8721
    $22, $12, {|}  $00, $00, $00, $00, $00, $00, $00, $7E, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8722
    $22, $13, {|}  $00, $00, $00, $00, $7E, $00, $18, $18, $7E, $18, $18, $00, $00, $00, $00, $00,  // char 8723
    $22, $14, {|}  $00, $00, $00, $18, $18, $00, $18, $18, $7E, $18, $18, $00, $00, $00, $00, $00,  // char 8724
    $22, $15, {|}  $00, $06, $06, $0C, $0C, $18, $18, $30, $30, $60, $60, $C0, $C0, $00, $00, $00,  // char 8725
    $22, $16, {|}  $00, $C0, $C0, $60, $60, $30, $30, $18, $18, $0C, $0C, $06, $06, $00, $00, $00,  // char 8726
    $22, $17, {|}  $00, $00, $00, $00, $00, $24, $18, $7E, $18, $24, $00, $00, $00, $00, $00, $00,  // char 8727
    $22, $18, {|}  $00, $00, $00, $00, $00, $18, $24, $24, $18, $00, $00, $00, $00, $00, $00, $00,  // char 8728
    $22, $19, {|}  $00, $00, $00, $00, $00, $18, $3C, $3C, $18, $00, $00, $00, $00, $00, $00, $00,  // char 8729
    $22, $1A, {|}  $00, $00, $03, $03, $06, $06, $06, $0C, $CC, $6C, $38, $18, $00, $00, $00, $00,  // char 8730
    $22, $1B, {|}  $00, $00, $E3, $33, $66, $36, $E6, $0C, $CC, $6C, $38, $18, $00, $00, $00, $00,  // char 8731
    $22, $1C, {|}  $00, $00, $33, $73, $B6, $F6, $36, $0C, $CC, $6C, $38, $18, $00, $00, $00, $00,  // char 8732
    $22, $1D, {|}  $00, $00, $00, $00, $00, $76, $D8, $D8, $6E, $00, $00, $00, $00, $00, $00, $00,  // char 8733
    $22, $1E, {|}  $00, $00, $00, $00, $00, $76, $DB, $DB, $6E, $00, $00, $00, $00, $00, $00, $00,  // char 8734
    $22, $1F, {|}  $00, $00, $00, $00, $00, $00, $C0, $C0, $C0, $C0, $C0, $FE, $00, $00, $00, $00,  // char 8735
    $22, $20, {|}  $00, $00, $00, $00, $00, $02, $06, $0C, $18, $30, $60, $FF, $00, $00, $00, $00,  // char 8736
    $22, $21, {|}  $00, $00, $00, $00, $00, $02, $16, $0C, $1C, $34, $62, $FF, $02, $00, $00, $00,  // char 8737
    $22, $22, {|}  $00, $00, $00, $00, $10, $0E, $38, $E8, $38, $0E, $10, $00, $00, $00, $00, $00,  // char 8738
    $22, $23, {|}  $00, $00, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00, $00, $00,  // char 8739
    $22, $24, {|}  $00, $00, $18, $18, $18, $1A, $1C, $38, $58, $18, $18, $18, $00, $00, $00, $00,  // char 8740
    $22, $25, {|}  $00, $00, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $6C, $00, $00, $00, $00,  // char 8741
    $22, $26, {|}  $00, $00, $6C, $6C, $6C, $6E, $7C, $EC, $6C, $6C, $6C, $6C, $00, $00, $00, $00,  // char 8742
    $22, $27, {|}  $00, $00, $00, $00, $10, $10, $38, $38, $6C, $6C, $C6, $C6, $00, $00, $00, $00,  // char 8743
    $22, $28, {|}  $00, $00, $00, $00, $C6, $C6, $6C, $6C, $38, $38, $10, $10, $00, $00, $00, $00,  // char 8744
    $22, $29, {|}  $00, $00, $00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 8745
    $22, $2A, {|}  $00, $00, $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00,  // char 8746
    $22, $2B, {|}  $0C, $1A, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $58, $30, $00, $00,  // char 8747
    $22, $2C, {|}  $33, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $CC, $00, $00,  // char 8748
    $22, $2D, {|}  $2A, $54, $54, $54, $54, $54, $54, $54, $54, $54, $54, $54, $54, $A8, $00, $00,  // char 8749
    $22, $2E, {|}  $0C, $1A, $18, $18, $3C, $5A, $99, $99, $5A, $3C, $18, $18, $58, $30, $00, $00,  // char 8750
    $22, $2F, {|}  $33, $66, $66, $66, $7E, $E7, $E7, $E7, $E7, $7E, $66, $66, $66, $CC, $00, $00,  // char 8751
    $22, $30, {|}  $2A, $54, $54, $54, $7C, $D6, $D6, $D6, $D6, $7C, $54, $54, $54, $A8, $00, $00,  // char 8752
    $22, $31, {|}  $18, $34, $30, $30, $38, $35, $33, $37, $30, $30, $30, $30, $B0, $60, $00, $00,  // char 8753
    $22, $32, {|}  $0C, $1A, $18, $18, $3C, $5A, $FA, $5A, $5A, $3C, $18, $18, $58, $30, $00, $00,  // char 8754
    $22, $33, {|}  $0C, $1A, $18, $18, $3C, $5A, $5F, $5A, $5A, $3C, $18, $18, $58, $30, $00, $00,  // char 8755
    $22, $34, {|}  $00, $00, $00, $00, $00, $18, $18, $00, $00, $00, $66, $66, $00, $00, $00, $00,  // char 8756
    $22, $35, {|}  $00, $00, $00, $00, $00, $66, $66, $00, $00, $00, $18, $18, $00, $00, $00, $00,  // char 8757
    $22, $36, {|}  $00, $00, $00, $00, $00, $18, $18, $00, $00, $00, $18, $18, $00, $00, $00, $00,  // char 8758
    $22, $37, {|}  $00, $00, $00, $00, $00, $66, $66, $00, $00, $00, $66, $66, $00, $00, $00, $00,  // char 8759
    $22, $38, {|}  $00, $00, $00, $00, $18, $18, $00, $7E, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8760
    $22, $39, {|}  $00, $00, $00, $00, $03, $03, $00, $FC, $00, $03, $03, $00, $00, $00, $00, $00,  // char 8761
    $22, $3A, {|}  $00, $00, $00, $00, $66, $66, $00, $7E, $00, $66, $66, $00, $00, $00, $00, $00,  // char 8762
    $22, $3B, {|}  $00, $00, $00, $18, $18, $00, $76, $DC, $00, $30, $30, $00, $00, $00, $00, $00,  // char 8763
    $22, $3C, {|}  $00, $00, $00, $00, $00, $00, $76, $DC, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8764
    $22, $3D, {|}  $00, $00, $00, $00, $00, $00, $DC, $76, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8765
    $22, $3E, {|}  $00, $00, $00, $00, $00, $72, $DB, $DB, $4E, $00, $00, $00, $00, $00, $00, $00,  // char 8766
    $22, $3F, {|}  $00, $00, $00, $00, $70, $D8, $DB, $DB, $1B, $0E, $00, $00, $00, $00, $00, $00,  // char 8767
    $22, $40, {|}  $00, $00, $00, $1C, $30, $30, $30, $18, $0C, $0C, $0C, $38, $00, $00, $00, $00,  // char 8768
    $22, $41, {|}  $00, $00, $00, $08, $08, $10, $76, $DC, $10, $20, $20, $00, $00, $00, $00, $00,  // char 8769
    $22, $42, {|}  $00, $00, $00, $00, $00, $FE, $00, $76, $DC, $00, $00, $00, $00, $00, $00, $00,  // char 8770
    $22, $43, {|}  $00, $00, $00, $00, $00, $76, $DC, $00, $FE, $00, $00, $00, $00, $00, $00, $00,  // char 8771
    $22, $44, {|}  $00, $00, $00, $08, $08, $76, $DC, $10, $FE, $20, $20, $00, $00, $00, $00, $00,  // char 8772
    $22, $45, {|}  $00, $00, $00, $00, $76, $DC, $00, $FE, $00, $FE, $00, $00, $00, $00, $00, $00,  // char 8773
    $22, $46, {|}  $00, $00, $00, $00, $76, $DC, $08, $FE, $10, $FE, $20, $00, $00, $00, $00, $00,  // char 8774
    $22, $47, {|}  $00, $00, $08, $08, $76, $DC, $10, $FE, $20, $FE, $40, $40, $00, $00, $00, $00,  // char 8775
    $22, $48, {|}  $00, $00, $00, $00, $00, $76, $DC, $00, $76, $DC, $00, $00, $00, $00, $00, $00,  // char 8776
    $22, $49, {|}  $00, $00, $00, $08, $08, $76, $DC, $10, $76, $DC, $20, $20, $00, $00, $00, $00,  // char 8777
    $22, $4A, {|}  $00, $00, $00, $00, $76, $DC, $00, $76, $DC, $00, $FE, $00, $00, $00, $00, $00,  // char 8778
    $22, $4B, {|}  $00, $00, $00, $76, $DC, $00, $76, $DC, $00, $76, $DC, $00, $00, $00, $00, $00,  // char 8779
    $22, $4C, {|}  $00, $00, $00, $00, $DC, $76, $00, $FE, $00, $FE, $00, $00, $00, $00, $00, $00,  // char 8780
    $22, $4D, {|}  $00, $00, $00, $00, $00, $C6, $7C, $00, $7C, $C6, $00, $00, $00, $00, $00, $00,  // char 8781
    $22, $4E, {|}  $00, $00, $00, $00, $18, $66, $00, $00, $66, $18, $00, $00, $00, $00, $00, $00,  // char 8782
    $22, $4F, {|}  $00, $00, $00, $00, $18, $66, $00, $00, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 8783
    $22, $50, {|}  $00, $00, $18, $18, $00, $7E, $00, $00, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 8784
    $22, $51, {|}  $00, $00, $18, $18, $00, $7E, $00, $00, $7E, $00, $18, $18, $00, $00, $00, $00,  // char 8785
    $22, $52, {|}  $00, $00, $60, $60, $00, $7E, $00, $00, $7E, $00, $06, $06, $00, $00, $00, $00,  // char 8786
    $22, $53, {|}  $00, $00, $06, $06, $00, $7E, $00, $00, $7E, $00, $60, $60, $00, $00, $00, $00,  // char 8787
    $22, $54, {|}  $00, $00, $00, $00, $C0, $DF, $00, $00, $DF, $C0, $00, $00, $00, $00, $00, $00,  // char 8788
    $22, $55, {|}  $00, $00, $00, $00, $03, $FB, $00, $00, $FB, $03, $00, $00, $00, $00, $00, $00,  // char 8789
    $22, $56, {|}  $00, $00, $00, $00, $00, $00, $FE, $28, $FE, $00, $00, $00, $00, $00, $00, $00,  // char 8790
    $22, $57, {|}  $00, $00, $10, $28, $10, $00, $FE, $00, $FE, $00, $00, $00, $00, $00, $00, $00,  // char 8791
    $22, $58, {|}  $00, $00, $00, $38, $44, $00, $FE, $00, $FE, $00, $00, $00, $00, $00, $00, $00,  // char 8792
    $22, $59, {|}  $00, $00, $10, $28, $44, $00, $FE, $00, $FE, $00, $00, $00, $00, $00, $00, $00,  // char 8793
    $22, $5A, {|}  $00, $00, $44, $28, $10, $00, $FE, $00, $FE, $00, $00, $00, $00, $00, $00, $00,  // char 8794
    $22, $5B, {|}  $00, $10, $10, $7C, $28, $00, $FE, $00, $FE, $00, $00, $00, $00, $00, $00, $00,  // char 8795
    $22, $5C, {|}  $00, $10, $28, $44, $7C, $00, $FE, $00, $FE, $00, $00, $00, $00, $00, $00, $00,  // char 8796
    $22, $5D, {|}  $00, $26, $7C, $B4, $6C, $00, $FE, $00, $FE, $00, $00, $00, $00, $00, $00, $00,  // char 8797
    $22, $5E, {|}  $00, $00, $68, $54, $54, $00, $FE, $00, $FE, $00, $00, $00, $00, $00, $00, $00,  // char 8798
    $22, $5F, {|}  $00, $30, $08, $10, $00, $10, $FE, $00, $FE, $00, $00, $00, $00, $00, $00, $00,  // char 8799
    $22, $60, {|}  $00, $00, $00, $04, $08, $7E, $08, $10, $7E, $10, $20, $00, $00, $00, $00, $00,  // char 8800
    $22, $61, {|}  $00, $00, $00, $00, $FE, $00, $00, $FE, $00, $00, $FE, $00, $00, $00, $00, $00,  // char 8801
    $22, $62, {|}  $00, $00, $04, $08, $FE, $08, $10, $FE, $10, $20, $FE, $20, $40, $00, $00, $00,  // char 8802
    $22, $63, {|}  $00, $00, $FE, $00, $00, $FE, $00, $00, $FE, $00, $00, $FE, $00, $00, $00, $00,  // char 8803
    $22, $64, {|}  $00, $00, $00, $00, $0E, $38, $E0, $38, $0E, $00, $FE, $00, $00, $00, $00, $00,  // char 8804
    $22, $65, {|}  $00, $00, $00, $00, $E0, $38, $0E, $38, $E0, $00, $FE, $00, $00, $00, $00, $00,  // char 8805
    $22, $66, {|}  $00, $00, $00, $0E, $38, $E0, $38, $0E, $00, $FE, $00, $FE, $00, $00, $00, $00,  // char 8806
    $22, $67, {|}  $00, $00, $00, $E0, $38, $0E, $38, $E0, $00, $FE, $00, $FE, $00, $00, $00, $00,  // char 8807
    $22, $68, {|}  $00, $00, $00, $0E, $38, $E0, $38, $0E, $08, $FE, $10, $FE, $20, $00, $00, $00,  // char 8808
    $22, $69, {|}  $00, $00, $00, $E0, $38, $0E, $38, $E0, $08, $FE, $10, $FE, $20, $00, $00, $00,  // char 8809
    $22, $6A, {|}  $00, $00, $00, $00, $1B, $36, $6C, $D8, $6C, $36, $1B, $00, $00, $00, $00, $00,  // char 8810
    $22, $6B, {|}  $00, $00, $00, $00, $D8, $6C, $36, $1B, $36, $6C, $D8, $00, $00, $00, $00, $00,  // char 8811
    $22, $6C, {|}  $00, $00, $66, $18, $3C, $66, $66, $66, $66, $3C, $18, $66, $00, $00, $00, $00,  // char 8812
    $22, $6D, {|}  $00, $00, $00, $00, $08, $CE, $7C, $10, $7C, $E6, $20, $00, $00, $00, $00, $00,  // char 8813
    $22, $6E, {|}  $00, $00, $04, $04, $0E, $38, $E8, $38, $0E, $10, $10, $00, $00, $00, $00, $00,  // char 8814
    $22, $6F, {|}  $00, $00, $10, $10, $E0, $38, $2E, $38, $E0, $40, $40, $00, $00, $00, $00, $00,  // char 8815
    $22, $70, {|}  $00, $00, $04, $04, $0E, $38, $E8, $38, $0E, $10, $FE, $10, $00, $00, $00, $00,  // char 8816
    $22, $71, {|}  $00, $00, $10, $10, $E0, $38, $2E, $38, $E0, $40, $FE, $40, $00, $00, $00, $00,  // char 8817
    $22, $72, {|}  $00, $00, $00, $00, $0E, $38, $E0, $38, $0E, $00, $76, $DC, $00, $00, $00, $00,  // char 8818
    $22, $73, {|}  $00, $00, $00, $00, $E0, $38, $0E, $38, $E0, $00, $76, $DC, $00, $00, $00, $00,  // char 8819
    $22, $74, {|}  $00, $00, $04, $04, $0E, $38, $E8, $38, $0E, $10, $76, $DC, $20, $20, $00, $00,  // char 8820
    $22, $75, {|}  $00, $00, $10, $10, $E0, $38, $2E, $38, $E0, $40, $76, $DC, $80, $00, $00, $00,  // char 8821
    $22, $76, {|}  $00, $00, $0E, $38, $E0, $38, $0E, $E0, $38, $0E, $38, $E0, $00, $00, $00, $00,  // char 8822
    $22, $77, {|}  $00, $00, $E0, $38, $0E, $38, $E0, $0E, $38, $E0, $38, $0E, $00, $00, $00, $00,  // char 8823
    $22, $78, {|}  $00, $08, $0E, $38, $E8, $38, $1E, $F0, $38, $2E, $38, $E0, $20, $00, $00, $00,  // char 8824
    $22, $79, {|}  $00, $08, $E8, $38, $0E, $38, $F0, $1E, $38, $E0, $38, $2E, $20, $00, $00, $00,  // char 8825
    $22, $7A, {|}  $00, $00, $00, $02, $06, $1C, $F0, $1C, $06, $02, $00, $00, $00, $00, $00, $00,  // char 8826
    $22, $7B, {|}  $00, $00, $00, $80, $C0, $70, $1E, $70, $C0, $80, $00, $00, $00, $00, $00, $00,  // char 8827
    $22, $7C, {|}  $00, $00, $02, $06, $1C, $F0, $1C, $06, $F2, $1C, $06, $02, $00, $00, $00, $00,  // char 8828
    $22, $7D, {|}  $00, $00, $80, $C0, $70, $1E, $70, $C0, $9E, $70, $C0, $80, $00, $00, $00, $00,  // char 8829
    $22, $7E, {|}  $00, $00, $02, $06, $1C, $F0, $1C, $06, $02, $00, $76, $DC, $00, $00, $00, $00,  // char 8830
    $22, $7F, {|}  $00, $00, $80, $C0, $70, $1E, $70, $C0, $80, $00, $76, $DC, $00, $00, $00, $00,  // char 8831
    $22, $80, {|}  $00, $00, $04, $06, $0C, $38, $E8, $38, $1C, $16, $10, $00, $00, $00, $00, $00,  // char 8832
    $22, $81, {|}  $00, $00, $10, $D0, $70, $38, $2E, $38, $60, $C0, $40, $00, $00, $00, $00, $00,  // char 8833
    $22, $82, {|}  $00, $00, $00, $00, $7E, $C0, $C0, $C0, $C0, $7E, $00, $00, $00, $00, $00, $00,  // char 8834
    $22, $83, {|}  $00, $00, $00, $00, $FC, $06, $06, $06, $06, $FC, $00, $00, $00, $00, $00, $00,  // char 8835
    $22, $84, {|}  $00, $00, $00, $04, $7E, $C8, $C8, $D0, $D0, $7E, $20, $00, $00, $00, $00, $00,  // char 8836
    $22, $85, {|}  $00, $00, $00, $08, $FC, $16, $16, $26, $26, $FC, $40, $00, $00, $00, $00, $00,  // char 8837
    $22, $86, {|}  $00, $00, $00, $7E, $C0, $C0, $C0, $C0, $7E, $00, $FE, $00, $00, $00, $00, $00,  // char 8838
    $22, $87, {|}  $00, $00, $00, $FC, $06, $06, $06, $06, $FC, $00, $FE, $00, $00, $00, $00, $00,  // char 8839
    $22, $88, {|}  $00, $00, $04, $7E, $C8, $C8, $D0, $D0, $7E, $20, $FE, $40, $00, $00, $00, $00,  // char 8840
    $22, $89, {|}  $00, $00, $08, $FC, $16, $16, $26, $26, $FC, $40, $FE, $80, $00, $00, $00, $00,  // char 8841
    $22, $8A, {|}  $00, $00, $00, $7E, $C0, $C0, $C0, $C0, $7E, $08, $FE, $10, $00, $00, $00, $00,  // char 8842
    $22, $8B, {|}  $00, $00, $00, $FC, $06, $06, $06, $06, $FC, $10, $FE, $20, $00, $00, $00, $00,  // char 8843
    $22, $8C, {|}  $00, $00, $00, $00, $82, $92, $A2, $FA, $A2, $92, $44, $38, $00, $00, $00, $00,  // char 8844
    $22, $8D, {|}  $00, $00, $00, $00, $82, $82, $92, $BA, $BA, $92, $44, $38, $00, $00, $00, $00,  // char 8845
    $22, $8E, {|}  $00, $00, $00, $00, $82, $82, $92, $BA, $92, $82, $44, $38, $00, $00, $00, $00,  // char 8846
    $22, $8F, {|}  $00, $00, $00, $00, $FE, $C0, $C0, $C0, $C0, $FE, $00, $00, $00, $00, $00, $00,  // char 8847
    $22, $90, {|}  $00, $00, $00, $00, $FE, $06, $06, $06, $06, $FE, $00, $00, $00, $00, $00, $00,  // char 8848
    $22, $91, {|}  $00, $00, $00, $FE, $C0, $C0, $C0, $C0, $FE, $00, $FE, $00, $00, $00, $00, $00,  // char 8849
    $22, $92, {|}  $00, $00, $00, $FE, $06, $06, $06, $06, $FE, $00, $FE, $00, $00, $00, $00, $00,  // char 8850
    $22, $93, {|}  $00, $00, $00, $00, $FE, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00,  // char 8851
    $22, $94, {|}  $00, $00, $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $FE, $00, $00, $00, $00,  // char 8852
    $22, $95, {|}  $00, $00, $00, $00, $38, $54, $92, $FE, $92, $54, $38, $00, $00, $00, $00, $00,  // char 8853
    $22, $96, {|}  $00, $00, $00, $00, $38, $44, $82, $FE, $82, $44, $38, $00, $00, $00, $00, $00,  // char 8854
    $22, $97, {|}  $00, $00, $00, $00, $38, $44, $AA, $92, $AA, $44, $38, $00, $00, $00, $00, $00,  // char 8855
    $22, $98, {|}  $00, $00, $00, $00, $38, $44, $8A, $92, $A2, $44, $38, $00, $00, $00, $00, $00,  // char 8856
    $22, $99, {|}  $00, $00, $00, $00, $38, $44, $92, $BA, $92, $44, $38, $00, $00, $00, $00, $00,  // char 8857
    $22, $9A, {|}  $00, $00, $00, $00, $38, $44, $92, $AA, $92, $44, $38, $00, $00, $00, $00, $00,  // char 8858
    $22, $9B, {|}  $00, $00, $00, $00, $38, $54, $D6, $BA, $D6, $54, $38, $00, $00, $00, $00, $00,  // char 8859
    $22, $9C, {|}  $00, $00, $00, $00, $38, $44, $BA, $82, $BA, $44, $38, $00, $00, $00, $00, $00,  // char 8860
    $22, $9D, {|}  $00, $00, $00, $00, $38, $44, $82, $BA, $82, $44, $38, $00, $00, $00, $00, $00,  // char 8861
    $22, $9E, {|}  $00, $00, $00, $00, $FE, $92, $92, $FE, $92, $92, $FE, $00, $00, $00, $00, $00,  // char 8862
    $22, $9F, {|}  $00, $00, $00, $00, $FE, $82, $82, $FE, $82, $82, $FE, $00, $00, $00, $00, $00,  // char 8863
    $22, $A0, {|}  $00, $00, $00, $00, $FE, $C6, $AA, $92, $AA, $C6, $FE, $00, $00, $00, $00, $00,  // char 8864
    $22, $A1, {|}  $00, $00, $00, $00, $FE, $82, $92, $BA, $92, $82, $FE, $00, $00, $00, $00, $00,  // char 8865
    $22, $A2, {|}  $00, $00, $00, $C0, $C0, $C0, $C0, $FF, $C0, $C0, $C0, $C0, $00, $00, $00, $00,  // char 8866
    $22, $A3, {|}  $00, $00, $00, $03, $03, $03, $03, $FF, $03, $03, $03, $03, $00, $00, $00, $00,  // char 8867
    $22, $A4, {|}  $00, $00, $FF, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00, $00, $00,  // char 8868
    $22, $A5, {|}  $00, $00, $18, $18, $18, $18, $18, $18, $18, $18, $18, $FF, $00, $00, $00, $00,  // char 8869
    $22, $A6, {|}  $00, $00, $00, $60, $60, $60, $60, $7E, $60, $60, $60, $60, $00, $00, $00, $00,  // char 8870
    $22, $A7, {|}  $00, $00, $00, $60, $60, $60, $7E, $60, $7E, $60, $60, $60, $00, $00, $00, $00,  // char 8871
    $22, $A8, {|}  $00, $00, $00, $C0, $C0, $C0, $FF, $C0, $FF, $C0, $C0, $C0, $00, $00, $00, $00,  // char 8872
    $22, $A9, {|}  $00, $00, $00, $D8, $D8, $D8, $D8, $DF, $D8, $D8, $D8, $D8, $00, $00, $00, $00,  // char 8873
    $22, $AA, {|}  $00, $00, $00, $A8, $A8, $A8, $A8, $AF, $A8, $A8, $A8, $A8, $00, $00, $00, $00,  // char 8874
    $22, $AB, {|}  $00, $00, $00, $D8, $D8, $D8, $DF, $D8, $DF, $D8, $D8, $D8, $00, $00, $00, $00,  // char 8875
    $22, $AC, {|}  $00, $00, $00, $C0, $C2, $C4, $C4, $FF, $C8, $C8, $D0, $C0, $00, $00, $00, $00,  // char 8876
    $22, $AD, {|}  $00, $00, $00, $C0, $C4, $C4, $FF, $C8, $FF, $D0, $D0, $C0, $00, $00, $00, $00,  // char 8877
    $22, $AE, {|}  $00, $00, $00, $D8, $D9, $DA, $DA, $DF, $DA, $DA, $DC, $D8, $00, $00, $00, $00,  // char 8878
    $22, $AF, {|}  $00, $00, $00, $D8, $D9, $DA, $DF, $DA, $DF, $DA, $DC, $D8, $00, $00, $00, $00,  // char 8879
    $22, $B0, {|}  $00, $00, $00, $0C, $06, $1C, $F0, $1C, $06, $0C, $00, $00, $00, $00, $00, $00,  // char 8880
    $22, $B1, {|}  $00, $00, $00, $60, $C0, $70, $1E, $70, $C0, $60, $00, $00, $00, $00, $00, $00,  // char 8881
    $22, $B2, {|}  $00, $00, $00, $06, $1E, $76, $C6, $76, $1E, $06, $00, $00, $00, $00, $00, $00,  // char 8882
    $22, $B3, {|}  $00, $00, $00, $C0, $F0, $DC, $C6, $DC, $F0, $C0, $00, $00, $00, $00, $00, $00,  // char 8883
    $22, $B4, {|}  $00, $00, $06, $1E, $76, $C6, $76, $1E, $06, $00, $FE, $00, $00, $00, $00, $00,  // char 8884
    $22, $B5, {|}  $00, $00, $C0, $F0, $DC, $C6, $DC, $F0, $C0, $00, $FE, $00, $00, $00, $00, $00,  // char 8885
    $22, $B6, {|}  $00, $00, $00, $00, $00, $00, $42, $BF, $42, $00, $00, $00, $00, $00, $00, $00,  // char 8886
    $22, $B7, {|}  $00, $00, $00, $00, $00, $00, $42, $FD, $42, $00, $00, $00, $00, $00, $00, $00,  // char 8887
    $22, $B8, {|}  $00, $00, $00, $00, $00, $00, $02, $7D, $02, $00, $00, $00, $00, $00, $00, $00,  // char 8888
    $22, $B9, {|}  $00, $00, $00, $00, $10, $10, $00, $C6, $00, $10, $10, $00, $00, $00, $00, $00,  // char 8889
    $22, $BA, {|}  $00, $00, $7E, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00, $00, $00,  // char 8890
    $22, $BB, {|}  $00, $00, $C6, $C6, $6C, $6C, $38, $38, $10, $10, $00, $FE, $00, $00, $00, $00,  // char 8891
    $22, $BC, {|}  $00, $00, $FE, $00, $10, $10, $38, $38, $6C, $6C, $C6, $C6, $00, $00, $00, $00,  // char 8892
    $22, $BD, {|}  $00, $00, $FE, $00, $C6, $C6, $6C, $6C, $38, $38, $10, $10, $00, $00, $00, $00,  // char 8893
    $22, $BE, {|}  $00, $00, $00, $00, $C0, $C0, $C0, $F0, $C8, $C4, $C4, $FF, $00, $00, $00, $00,  // char 8894
    $22, $BF, {|}  $00, $00, $00, $00, $03, $07, $0B, $13, $23, $43, $83, $FF, $00, $00, $00, $00,  // char 8895
    $22, $C0, {|}  $00, $00, $00, $10, $10, $38, $38, $6C, $6C, $C6, $C6, $00, $00, $00, $00, $00,  // char 8896
    $22, $C1, {|}  $00, $00, $00, $C6, $C6, $6C, $6C, $38, $38, $10, $10, $00, $00, $00, $00, $00,  // char 8897
    $22, $C2, {|}  $00, $00, $00, $38, $6C, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00, $00,  // char 8898
    $22, $C3, {|}  $00, $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $6C, $38, $00, $00, $00, $00, $00,  // char 8899
    $22, $C4, {|}  $00, $00, $00, $00, $00, $10, $38, $6C, $38, $10, $00, $00, $00, $00, $00, $00,  // char 8900
    $22, $C5, {|}  $00, $00, $00, $00, $00, $00, $00, $18, $18, $00, $00, $00, $00, $00, $00, $00,  // char 8901
    $22, $C6, {|}  $00, $00, $00, $00, $00, $10, $10, $7C, $38, $28, $00, $00, $00, $00, $00, $00,  // char 8902
    $22, $C7, {|}  $00, $00, $00, $00, $92, $44, $28, $92, $28, $44, $92, $00, $00, $00, $00, $00,  // char 8903
    $22, $C8, {|}  $00, $00, $00, $00, $82, $C6, $AA, $92, $AA, $C6, $82, $00, $00, $00, $00, $00,  // char 8904
    $22, $C9, {|}  $00, $00, $00, $00, $82, $C4, $A8, $90, $A8, $C4, $82, $00, $00, $00, $00, $00,  // char 8905
    $22, $CA, {|}  $00, $00, $00, $00, $82, $46, $2A, $12, $2A, $46, $82, $00, $00, $00, $00, $00,  // char 8906
    $22, $CB, {|}  $00, $00, $00, $00, $80, $40, $20, $10, $28, $44, $82, $00, $00, $00, $00, $00,  // char 8907
    $22, $CC, {|}  $00, $00, $00, $00, $02, $04, $08, $10, $28, $44, $82, $00, $00, $00, $00, $00,  // char 8908
    $22, $CD, {|}  $00, $00, $00, $00, $00, $DC, $76, $00, $FE, $00, $00, $00, $00, $00, $00, $00,  // char 8909
    $22, $CE, {|}  $00, $00, $00, $10, $10, $10, $38, $38, $6C, $6C, $C6, $00, $00, $00, $00, $00,  // char 8910
    $22, $CF, {|}  $00, $00, $00, $C6, $6C, $6C, $38, $38, $10, $10, $10, $00, $00, $00, $00, $00,  // char 8911
    $22, $D0, {|}  $00, $00, $00, $3E, $60, $CE, $D8, $D8, $CE, $60, $3E, $00, $00, $00, $00, $00,  // char 8912
    $22, $D1, {|}  $00, $00, $00, $F8, $0C, $E6, $36, $36, $E6, $0C, $F8, $00, $00, $00, $00, $00,  // char 8913
    $22, $D2, {|}  $00, $00, $00, $38, $44, $92, $AA, $AA, $AA, $AA, $AA, $00, $00, $00, $00, $00,  // char 8914
    $22, $D3, {|}  $00, $00, $00, $AA, $AA, $AA, $AA, $AA, $92, $44, $38, $00, $00, $00, $00, $00,  // char 8915
    $22, $D4, {|}  $00, $00, $10, $10, $38, $54, $92, $92, $92, $92, $92, $92, $00, $00, $00, $00,  // char 8916
    $22, $D5, {|}  $00, $00, $18, $18, $18, $7E, $18, $18, $7E, $18, $18, $18, $00, $00, $00, $00,  // char 8917
    $22, $D6, {|}  $00, $00, $00, $00, $18, $30, $64, $CE, $64, $30, $18, $00, $00, $00, $00, $00,  // char 8918
    $22, $D7, {|}  $00, $00, $00, $00, $30, $18, $4C, $E6, $4C, $18, $30, $00, $00, $00, $00, $00,  // char 8919
    $22, $D8, {|}  $00, $00, $00, $00, $15, $2A, $54, $A8, $54, $2A, $15, $00, $00, $00, $00, $00,  // char 8920
    $22, $D9, {|}  $00, $00, $00, $00, $A8, $54, $2A, $15, $2A, $54, $A8, $00, $00, $00, $00, $00,  // char 8921
    $22, $DA, {|}  $00, $0E, $38, $E0, $38, $0E, $00, $FE, $00, $E0, $38, $0E, $38, $E0, $00, $00,  // char 8922
    $22, $DB, {|}  $00, $E0, $38, $0E, $38, $E0, $00, $FE, $00, $0E, $38, $E0, $38, $0E, $00, $00,  // char 8923
    $22, $DC, {|}  $00, $00, $00, $00, $FE, $00, $0E, $38, $E0, $38, $0E, $00, $00, $00, $00, $00,  // char 8924
    $22, $DD, {|}  $00, $00, $00, $00, $FE, $00, $E0, $38, $0E, $38, $E0, $00, $00, $00, $00, $00,  // char 8925
    $22, $DE, {|}  $00, $00, $02, $06, $1C, $F2, $06, $1C, $F0, $1C, $06, $02, $00, $00, $00, $00,  // char 8926
    $22, $DF, {|}  $00, $00, $80, $C0, $70, $9E, $C0, $70, $1E, $70, $C0, $80, $00, $00, $00, $00,  // char 8927
    $22, $E0, {|}  $00, $00, $0A, $0E, $1C, $F0, $1C, $16, $F2, $1C, $26, $22, $00, $00, $00, $00,  // char 8928
    $22, $E1, {|}  $00, $00, $88, $C8, $70, $1E, $70, $D0, $9E, $70, $E0, $A0, $00, $00, $00, $00,  // char 8929
    $22, $E2, {|}  $00, $00, $04, $FE, $C8, $C8, $D0, $D0, $FE, $20, $FE, $40, $00, $00, $00, $00,  // char 8930
    $22, $E3, {|}  $00, $00, $08, $FE, $16, $16, $26, $26, $FE, $40, $FE, $80, $00, $00, $00, $00,  // char 8931
    $22, $E4, {|}  $00, $00, $00, $FE, $C0, $C0, $C0, $C0, $FE, $08, $FE, $10, $00, $00, $00, $00,  // char 8932
    $22, $E5, {|}  $00, $00, $00, $FE, $06, $06, $06, $06, $FE, $10, $FE, $20, $00, $00, $00, $00,  // char 8933
    $22, $E6, {|}  $00, $00, $00, $00, $0E, $38, $E0, $38, $0E, $10, $76, $DC, $10, $00, $00, $00,  // char 8934
    $22, $E7, {|}  $00, $00, $00, $00, $E0, $38, $0E, $38, $E0, $10, $76, $DC, $10, $00, $00, $00,  // char 8935
    $22, $E8, {|}  $00, $00, $02, $06, $1C, $F0, $1C, $06, $02, $10, $76, $DC, $10, $00, $00, $00,  // char 8936
    $22, $E9, {|}  $00, $00, $80, $C0, $70, $1E, $70, $C0, $80, $10, $76, $DC, $10, $00, $00, $00,  // char 8937
    $22, $EA, {|}  $00, $00, $08, $0E, $1E, $76, $D6, $76, $1E, $26, $20, $00, $00, $00, $00, $00,  // char 8938
    $22, $EB, {|}  $00, $00, $08, $C8, $F0, $DC, $D6, $DC, $F0, $E0, $20, $00, $00, $00, $00, $00,  // char 8939
    $22, $EC, {|}  $00, $08, $0E, $1E, $76, $D6, $76, $1E, $26, $20, $FE, $40, $00, $00, $00, $00,  // char 8940
    $22, $ED, {|}  $00, $08, $C8, $F0, $DC, $D6, $DC, $F0, $E0, $20, $FE, $40, $00, $00, $00, $00,  // char 8941
    $22, $EE, {|}  $00, $00, $18, $18, $00, $00, $18, $18, $00, $00, $18, $18, $00, $00, $00, $00,  // char 8942
    $22, $EF, {|}  $00, $00, $00, $00, $00, $00, $DB, $DB, $00, $00, $00, $00, $00, $00, $00, $00,  // char 8943
    $22, $F0, {|}  $00, $00, $03, $03, $00, $00, $18, $18, $00, $00, $C0, $C0, $00, $00, $00, $00,  // char 8944
    $22, $F1, {|}  $00, $00, $C0, $C0, $00, $00, $18, $18, $00, $00, $03, $03, $00, $00, $00, $00,  // char 8945
    $23, $00, {|}  $00, $00, $00, $00, $3D, $66, $C7, $CB, $D3, $E3, $66, $BC, $00, $00, $00, $00,  // char 8960
    $23, $02, {|}  $00, $00, $00, $00, $10, $38, $6C, $C6, $C6, $C6, $FE, $00, $00, $00, $00, $00,  // char 8962
    $23, $08, {|}  $00, $00, $1E, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18,  // char 8968
    $23, $09, {|}  $00, $00, $78, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18,  // char 8969
    $23, $0A, {|}  $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $1E, $00, $00, $00, $00,  // char 8970
    $23, $0B, {|}  $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $78, $00, $00, $00, $00,  // char 8971
    $23, $10, {|}  $00, $00, $00, $00, $00, $00, $FE, $C0, $C0, $C0, $C0, $00, $00, $00, $00, $00,  // char 8976
    $23, $18, {|}  $00, $00, $00, $42, $A5, $7E, $24, $24, $7E, $A5, $42, $00, $00, $00, $00, $00,  // char 8984
    $23, $1A, {|}  $00, $00, $00, $38, $38, $54, $8A, $F6, $82, $54, $38, $38, $00, $00, $00, $00,  // char 8986
    $23, $1B, {|}  $00, $FE, $FE, $44, $44, $28, $10, $28, $44, $44, $FE, $FE, $00, $00, $00, $00,  // char 8987
    $23, $20, {|}  $00, $00, $0E, $1B, $1B, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18,  // char 8992
    $23, $21, {|}  $18, $18, $18, $18, $18, $18, $18, $18, $D8, $D8, $D8, $70, $00, $00, $00, $00,  // char 8993
    $23, $29, {|}  $0C, $0C, $18, $18, $30, $30, $60, $60, $30, $30, $18, $18, $0C, $0C, $00, $00,  // char 9001
    $23, $2A, {|}  $60, $60, $30, $30, $18, $18, $0C, $0C, $18, $18, $30, $30, $60, $60, $00, $00,  // char 9002
    $23, $9B, {|}  $00, $03, $06, $0C, $0C, $18, $18, $18, $30, $30, $30, $30, $30, $30, $30, $30,  // char 9115
    $23, $9C, {|}  $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,  // char 9116
    $23, $9D, {|}  $30, $30, $30, $30, $30, $30, $30, $18, $18, $18, $0C, $0C, $06, $03, $00, $00,  // char 9117
    $23, $9E, {|}  $00, $C0, $60, $30, $30, $18, $18, $18, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C,  // char 9118
    $23, $9F, {|}  $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C,  // char 9119
    $23, $A0, {|}  $0C, $0C, $0C, $0C, $0C, $0C, $0C, $18, $18, $18, $30, $30, $60, $C0, $00, $00,  // char 9120
    $23, $A1, {|}  $00, $3F, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,  // char 9121
    $23, $A2, {|}  $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,  // char 9122
    $23, $A3, {|}  $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $3F, $00, $00,  // char 9123
    $23, $A4, {|}  $00, $FC, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C,  // char 9124
    $23, $A5, {|}  $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C,  // char 9125
    $23, $A6, {|}  $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $FC, $00, $00,  // char 9126
    $23, $A7, {|}  $00, $07, $0C, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18,  // char 9127
    $23, $A8, {|}  $18, $18, $18, $18, $18, $30, $E0, $30, $18, $18, $18, $18, $18, $18, $18, $18,  // char 9128
    $23, $A9, {|}  $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $0C, $07, $00, $00,  // char 9129
    $23, $AA, {|}  $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18,  // char 9130
    $23, $AB, {|}  $00, $E0, $30, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18,  // char 9131
    $23, $AC, {|}  $18, $18, $18, $18, $18, $0C, $07, $0C, $18, $18, $18, $18, $18, $18, $18, $18,  // char 9132
    $23, $AD, {|}  $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $30, $E0, $00, $00,  // char 9133
    $23, $AE, {|}  $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18,  // char 9134
    $23, $AF, {|}  $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9135
    $23, $B0, {|}  $07, $0C, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $30, $E0,  // char 9136
    $23, $B1, {|}  $E0, $30, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $0C, $07,  // char 9137
    $23, $B2, {|}  $00, $00, $FF, $C1, $C0, $60, $60, $60, $60, $30, $30, $30, $30, $18, $18, $18,  // char 9138
    $23, $B3, {|}  $18, $18, $18, $30, $30, $30, $30, $60, $60, $60, $60, $C0, $C1, $FF, $00, $00,  // char 9139
    $23, $B4, {|}  $00, $00, $FF, $C3, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9140
    $23, $B5, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $C3, $FF, $00, $00, $00, $00,  // char 9141
    $23, $B6, {|}  $00, $00, $00, $00, $00, $C3, $FF, $00, $FF, $C3, $00, $00, $00, $00, $00, $00,  // char 9142
    $23, $B7, {|}  $18, $18, $18, $18, $18, $18, $D8, $D8, $78, $78, $38, $38, $18, $18, $00, $00,  // char 9143
    $23, $B8, {|}  $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0,  // char 9144
    $23, $B9, {|}  $03, $03, $03, $03, $03, $03, $03, $03, $03, $03, $03, $03, $03, $03, $03, $03,  // char 9145
    $23, $BA, {|}  $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9146
    $23, $BB, {|}  $00, $00, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9147
    $23, $BC, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00,  // char 9148
    $23, $BD, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $00,  // char 9149
    $23, $CE, {|}  $00, $00, $07, $05, $05, $05, $05, $25, $79, $C2, $7C, $20, $00, $00, $00, $00,  // char 9166
    $24, $09, {|}  $00, $00, $D8, $D8, $F8, $D8, $D8, $0F, $06, $06, $06, $06, $00, $00, $00, $00,  // char 9225
    $24, $0A, {|}  $00, $00, $C0, $C0, $C0, $C0, $F0, $0F, $0C, $0E, $0C, $0C, $00, $00, $00, $00,  // char 9226
    $24, $0B, {|}  $00, $00, $D8, $D8, $F8, $70, $20, $0F, $06, $06, $06, $06, $00, $00, $00, $00,  // char 9227
    $24, $0C, {|}  $00, $00, $F0, $C0, $E0, $C0, $C0, $0F, $0C, $0E, $0C, $0C, $00, $00, $00, $00,  // char 9228
    $24, $0D, {|}  $00, $00, $70, $C0, $C0, $C0, $70, $1E, $1B, $1E, $1B, $1B, $00, $00, $00, $00,  // char 9229
    $24, $23, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $82, $FE, $00, $00, $00, $00,  // char 9251
    $24, $24, {|}  $00, $00, $98, $D8, $F8, $D8, $D8, $0C, $0C, $0C, $0C, $0F, $00, $00, $00, $00,  // char 9252
    $24, $26, {|}  $00, $00, $7C, $FE, $C6, $C6, $60, $30, $30, $00, $30, $30, $00, $00, $00, $00,  // char 9254
    $25, $00, {|}  $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9472
    $25, $01, {|}  $00, $00, $00, $00, $00, $00, $00, $FF, $FF, $00, $00, $00, $00, $00, $00, $00,  // char 9473
    $25, $02, {|}  $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18,  // char 9474
    $25, $03, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9475
    $25, $04, {|}  $00, $00, $00, $00, $00, $00, $00, $6D, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9476
    $25, $05, {|}  $00, $00, $00, $00, $00, $00, $00, $6D, $6D, $00, $00, $00, $00, $00, $00, $00,  // char 9477
    $25, $06, {|}  $18, $18, $18, $18, $00, $18, $18, $18, $18, $18, $00, $18, $18, $18, $18, $00,  // char 9478
    $25, $07, {|}  $1C, $1C, $1C, $1C, $00, $1C, $1C, $1C, $1C, $1C, $00, $1C, $1C, $1C, $1C, $00,  // char 9479
    $25, $08, {|}  $00, $00, $00, $00, $00, $00, $00, $AA, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9480
    $25, $09, {|}  $00, $00, $00, $00, $00, $00, $00, $AA, $AA, $00, $00, $00, $00, $00, $00, $00,  // char 9481
    $25, $0A, {|}  $18, $18, $18, $00, $18, $18, $18, $00, $18, $18, $18, $00, $18, $18, $18, $00,  // char 9482
    $25, $0B, {|}  $1C, $1C, $1C, $00, $1C, $1C, $1C, $00, $1C, $1C, $1C, $00, $1C, $1C, $1C, $00,  // char 9483
    $25, $0C, {|}  $00, $00, $00, $00, $00, $00, $00, $1F, $18, $18, $18, $18, $18, $18, $18, $18,  // char 9484
    $25, $0D, {|}  $00, $00, $00, $00, $00, $00, $00, $1F, $1F, $18, $18, $18, $18, $18, $18, $18,  // char 9485
    $25, $0E, {|}  $00, $00, $00, $00, $00, $00, $00, $1F, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9486
    $25, $0F, {|}  $00, $00, $00, $00, $00, $00, $00, $1F, $1F, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9487
    $25, $10, {|}  $00, $00, $00, $00, $00, $00, $00, $F8, $18, $18, $18, $18, $18, $18, $18, $18,  // char 9488
    $25, $11, {|}  $00, $00, $00, $00, $00, $00, $00, $F8, $F8, $18, $18, $18, $18, $18, $18, $18,  // char 9489
    $25, $12, {|}  $00, $00, $00, $00, $00, $00, $00, $FC, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9490
    $25, $13, {|}  $00, $00, $00, $00, $00, $00, $00, $FC, $FC, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9491
    $25, $14, {|}  $18, $18, $18, $18, $18, $18, $18, $1F, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9492
    $25, $15, {|}  $18, $18, $18, $18, $18, $18, $18, $1F, $1F, $00, $00, $00, $00, $00, $00, $00,  // char 9493
    $25, $16, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1F, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9494
    $25, $17, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1F, $1F, $00, $00, $00, $00, $00, $00, $00,  // char 9495
    $25, $18, {|}  $18, $18, $18, $18, $18, $18, $18, $F8, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9496
    $25, $19, {|}  $18, $18, $18, $18, $18, $18, $18, $F8, $F8, $00, $00, $00, $00, $00, $00, $00,  // char 9497
    $25, $1A, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $FC, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9498
    $25, $1B, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $FC, $FC, $00, $00, $00, $00, $00, $00, $00,  // char 9499
    $25, $1C, {|}  $18, $18, $18, $18, $18, $18, $18, $1F, $18, $18, $18, $18, $18, $18, $18, $18,  // char 9500
    $25, $1D, {|}  $18, $18, $18, $18, $18, $18, $18, $1F, $1F, $18, $18, $18, $18, $18, $18, $18,  // char 9501
    $25, $1E, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1F, $18, $18, $18, $18, $18, $18, $18, $18,  // char 9502
    $25, $1F, {|}  $18, $18, $18, $18, $18, $18, $18, $1F, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9503
    $25, $20, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1F, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9504
    $25, $21, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1F, $1F, $18, $18, $18, $18, $18, $18, $18,  // char 9505
    $25, $22, {|}  $18, $18, $18, $18, $18, $18, $18, $1F, $1F, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9506
    $25, $23, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1F, $1F, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9507
    $25, $24, {|}  $18, $18, $18, $18, $18, $18, $18, $F8, $18, $18, $18, $18, $18, $18, $18, $18,  // char 9508
    $25, $25, {|}  $18, $18, $18, $18, $18, $18, $18, $F8, $F8, $18, $18, $18, $18, $18, $18, $18,  // char 9509
    $25, $26, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $FC, $18, $18, $18, $18, $18, $18, $18, $18,  // char 9510
    $25, $27, {|}  $18, $18, $18, $18, $18, $18, $18, $FC, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9511
    $25, $28, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $FC, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9512
    $25, $29, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $FC, $FC, $18, $18, $18, $18, $18, $18, $18,  // char 9513
    $25, $2A, {|}  $18, $18, $18, $18, $18, $18, $18, $FC, $FC, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9514
    $25, $2B, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $FC, $FC, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9515
    $25, $2C, {|}  $00, $00, $00, $00, $00, $00, $00, $FF, $18, $18, $18, $18, $18, $18, $18, $18,  // char 9516
    $25, $2D, {|}  $00, $00, $00, $00, $00, $00, $00, $FF, $F8, $18, $18, $18, $18, $18, $18, $18,  // char 9517
    $25, $2E, {|}  $00, $00, $00, $00, $00, $00, $00, $FF, $1F, $18, $18, $18, $18, $18, $18, $18,  // char 9518
    $25, $2F, {|}  $00, $00, $00, $00, $00, $00, $00, $FF, $FF, $18, $18, $18, $18, $18, $18, $18,  // char 9519
    $25, $30, {|}  $00, $00, $00, $00, $00, $00, $00, $FF, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9520
    $25, $31, {|}  $00, $00, $00, $00, $00, $00, $00, $FF, $FC, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9521
    $25, $32, {|}  $00, $00, $00, $00, $00, $00, $00, $FF, $1F, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9522
    $25, $33, {|}  $00, $00, $00, $00, $00, $00, $00, $FF, $FF, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9523
    $25, $34, {|}  $18, $18, $18, $18, $18, $18, $18, $FF, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9524
    $25, $35, {|}  $18, $18, $18, $18, $18, $18, $18, $FF, $F8, $00, $00, $00, $00, $00, $00, $00,  // char 9525
    $25, $36, {|}  $18, $18, $18, $18, $18, $18, $18, $FF, $1F, $00, $00, $00, $00, $00, $00, $00,  // char 9526
    $25, $37, {|}  $18, $18, $18, $18, $18, $18, $18, $FF, $FF, $00, $00, $00, $00, $00, $00, $00,  // char 9527
    $25, $38, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $FF, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9528
    $25, $39, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $FF, $FC, $00, $00, $00, $00, $00, $00, $00,  // char 9529
    $25, $3A, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $FF, $1F, $00, $00, $00, $00, $00, $00, $00,  // char 9530
    $25, $3B, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $FF, $FF, $00, $00, $00, $00, $00, $00, $00,  // char 9531
    $25, $3C, {|}  $18, $18, $18, $18, $18, $18, $18, $FF, $18, $18, $18, $18, $18, $18, $18, $18,  // char 9532
    $25, $3D, {|}  $18, $18, $18, $18, $18, $18, $18, $FF, $F8, $18, $18, $18, $18, $18, $18, $18,  // char 9533
    $25, $3E, {|}  $18, $18, $18, $18, $18, $18, $18, $FF, $1F, $18, $18, $18, $18, $18, $18, $18,  // char 9534
    $25, $3F, {|}  $18, $18, $18, $18, $18, $18, $18, $FF, $FF, $18, $18, $18, $18, $18, $18, $18,  // char 9535
    $25, $40, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $FF, $18, $18, $18, $18, $18, $18, $18, $18,  // char 9536
    $25, $41, {|}  $18, $18, $18, $18, $18, $18, $18, $FF, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9537
    $25, $42, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $FF, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9538
    $25, $43, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $FF, $FC, $18, $18, $18, $18, $18, $18, $18,  // char 9539
    $25, $44, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $FF, $1F, $18, $18, $18, $18, $18, $18, $18,  // char 9540
    $25, $45, {|}  $18, $18, $18, $18, $18, $18, $18, $FF, $FC, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9541
    $25, $46, {|}  $18, $18, $18, $18, $18, $18, $18, $FF, $1F, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9542
    $25, $47, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $FF, $FF, $18, $18, $18, $18, $18, $18, $18,  // char 9543
    $25, $48, {|}  $18, $18, $18, $18, $18, $18, $18, $FF, $FF, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9544
    $25, $49, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $FF, $FC, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9545
    $25, $4A, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $FF, $1F, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9546
    $25, $4B, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $FF, $FF, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9547
    $25, $4C, {|}  $00, $00, $00, $00, $00, $00, $00, $EE, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9548
    $25, $4D, {|}  $00, $00, $00, $00, $00, $00, $00, $EE, $EE, $00, $00, $00, $00, $00, $00, $00,  // char 9549
    $25, $4E, {|}  $18, $18, $18, $18, $18, $18, $18, $00, $18, $18, $18, $18, $18, $18, $18, $00,  // char 9550
    $25, $4F, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $00, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $00,  // char 9551
    $25, $50, {|}  $00, $00, $00, $00, $00, $00, $FF, $00, $FF, $00, $00, $00, $00, $00, $00, $00,  // char 9552
    $25, $51, {|}  $36, $36, $36, $36, $36, $36, $36, $36, $36, $36, $36, $36, $36, $36, $36, $36,  // char 9553
    $25, $52, {|}  $00, $00, $00, $00, $00, $00, $1F, $18, $1F, $18, $18, $18, $18, $18, $18, $18,  // char 9554
    $25, $53, {|}  $00, $00, $00, $00, $00, $00, $00, $3F, $36, $36, $36, $36, $36, $36, $36, $36,  // char 9555
    $25, $54, {|}  $00, $00, $00, $00, $00, $00, $3F, $30, $37, $36, $36, $36, $36, $36, $36, $36,  // char 9556
    $25, $55, {|}  $00, $00, $00, $00, $00, $00, $F8, $18, $F8, $18, $18, $18, $18, $18, $18, $18,  // char 9557
    $25, $56, {|}  $00, $00, $00, $00, $00, $00, $00, $FE, $36, $36, $36, $36, $36, $36, $36, $36,  // char 9558
    $25, $57, {|}  $00, $00, $00, $00, $00, $00, $FE, $06, $F6, $36, $36, $36, $36, $36, $36, $36,  // char 9559
    $25, $58, {|}  $18, $18, $18, $18, $18, $18, $1F, $18, $1F, $00, $00, $00, $00, $00, $00, $00,  // char 9560
    $25, $59, {|}  $36, $36, $36, $36, $36, $36, $36, $3F, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9561
    $25, $5A, {|}  $36, $36, $36, $36, $36, $36, $37, $30, $3F, $00, $00, $00, $00, $00, $00, $00,  // char 9562
    $25, $5B, {|}  $18, $18, $18, $18, $18, $18, $F8, $18, $F8, $00, $00, $00, $00, $00, $00, $00,  // char 9563
    $25, $5C, {|}  $36, $36, $36, $36, $36, $36, $36, $FE, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9564
    $25, $5D, {|}  $36, $36, $36, $36, $36, $36, $F6, $06, $FE, $00, $00, $00, $00, $00, $00, $00,  // char 9565
    $25, $5E, {|}  $18, $18, $18, $18, $18, $18, $1F, $18, $1F, $18, $18, $18, $18, $18, $18, $18,  // char 9566
    $25, $5F, {|}  $36, $36, $36, $36, $36, $36, $36, $37, $36, $36, $36, $36, $36, $36, $36, $36,  // char 9567
    $25, $60, {|}  $36, $36, $36, $36, $36, $36, $37, $30, $37, $36, $36, $36, $36, $36, $36, $36,  // char 9568
    $25, $61, {|}  $18, $18, $18, $18, $18, $18, $F8, $18, $F8, $18, $18, $18, $18, $18, $18, $18,  // char 9569
    $25, $62, {|}  $36, $36, $36, $36, $36, $36, $36, $F6, $36, $36, $36, $36, $36, $36, $36, $36,  // char 9570
    $25, $63, {|}  $36, $36, $36, $36, $36, $36, $F6, $06, $F6, $36, $36, $36, $36, $36, $36, $36,  // char 9571
    $25, $64, {|}  $00, $00, $00, $00, $00, $00, $FF, $00, $FF, $18, $18, $18, $18, $18, $18, $18,  // char 9572
    $25, $65, {|}  $00, $00, $00, $00, $00, $00, $00, $FF, $36, $36, $36, $36, $36, $36, $36, $36,  // char 9573
    $25, $66, {|}  $00, $00, $00, $00, $00, $00, $FF, $00, $F7, $36, $36, $36, $36, $36, $36, $36,  // char 9574
    $25, $67, {|}  $18, $18, $18, $18, $18, $18, $FF, $00, $FF, $00, $00, $00, $00, $00, $00, $00,  // char 9575
    $25, $68, {|}  $36, $36, $36, $36, $36, $36, $36, $FF, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9576
    $25, $69, {|}  $36, $36, $36, $36, $36, $36, $F7, $00, $FF, $00, $00, $00, $00, $00, $00, $00,  // char 9577
    $25, $6A, {|}  $18, $18, $18, $18, $18, $18, $FF, $18, $FF, $18, $18, $18, $18, $18, $18, $18,  // char 9578
    $25, $6B, {|}  $36, $36, $36, $36, $36, $36, $36, $FF, $36, $36, $36, $36, $36, $36, $36, $36,  // char 9579
    $25, $6C, {|}  $36, $36, $36, $36, $36, $36, $F7, $00, $F7, $36, $36, $36, $36, $36, $36, $36,  // char 9580
    $25, $6D, {|}  $00, $00, $00, $00, $00, $00, $00, $03, $0E, $0C, $18, $18, $18, $18, $18, $18,  // char 9581
    $25, $6E, {|}  $00, $00, $00, $00, $00, $00, $00, $C0, $70, $30, $18, $18, $18, $18, $18, $18,  // char 9582
    $25, $6F, {|}  $18, $18, $18, $18, $18, $30, $70, $C0, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9583
    $25, $70, {|}  $18, $18, $18, $18, $18, $0C, $0E, $03, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9584
    $25, $71, {|}  $01, $01, $02, $02, $04, $04, $08, $08, $10, $10, $20, $20, $40, $40, $80, $80,  // char 9585
    $25, $72, {|}  $80, $80, $40, $40, $20, $20, $10, $10, $08, $08, $04, $04, $02, $02, $01, $01,  // char 9586
    $25, $73, {|}  $81, $81, $42, $42, $24, $24, $18, $18, $18, $18, $24, $24, $42, $42, $81, $81,  // char 9587
    $25, $74, {|}  $00, $00, $00, $00, $00, $00, $00, $F8, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9588
    $25, $75, {|}  $18, $18, $18, $18, $18, $18, $18, $18, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9589
    $25, $76, {|}  $00, $00, $00, $00, $00, $00, $00, $1F, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9590
    $25, $77, {|}  $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $18, $18, $18, $18, $18, $18,  // char 9591
    $25, $78, {|}  $00, $00, $00, $00, $00, $00, $00, $FC, $FC, $00, $00, $00, $00, $00, $00, $00,  // char 9592
    $25, $79, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $00, $00, $00, $00, $00, $00, $00,  // char 9593
    $25, $7A, {|}  $00, $00, $00, $00, $00, $00, $00, $1F, $1F, $00, $00, $00, $00, $00, $00, $00,  // char 9594
    $25, $7B, {|}  $00, $00, $00, $00, $00, $00, $00, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9595
    $25, $7C, {|}  $00, $00, $00, $00, $00, $00, $00, $FF, $1F, $00, $00, $00, $00, $00, $00, $00,  // char 9596
    $25, $7D, {|}  $18, $18, $18, $18, $18, $18, $18, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C,  // char 9597
    $25, $7E, {|}  $00, $00, $00, $00, $00, $00, $00, $FF, $FC, $00, $00, $00, $00, $00, $00, $00,  // char 9598
    $25, $7F, {|}  $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $18, $18, $18, $18, $18, $18, $18,  // char 9599
    $25, $80, {|}  $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9600
    $25, $81, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $FF,  // char 9601
    $25, $82, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $FF, $FF, $FF,  // char 9602
    $25, $83, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $FF, $FF, $FF, $FF, $FF,  // char 9603
    $25, $84, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,  // char 9604
    $25, $85, {|}  $00, $00, $00, $00, $00, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,  // char 9605
    $25, $86, {|}  $00, $00, $00, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,  // char 9606
    $25, $87, {|}  $00, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,  // char 9607
    $25, $88, {|}  $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,  // char 9608
    $25, $89, {|}  $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE,  // char 9609
    $25, $8A, {|}  $FC, $FC, $FC, $FC, $FC, $FC, $FC, $FC, $FC, $FC, $FC, $FC, $FC, $FC, $FC, $FC,  // char 9610
    $25, $8B, {|}  $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8,  // char 9611
    $25, $8C, {|}  $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0,  // char 9612
    $25, $8D, {|}  $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0,  // char 9613
    $25, $8E, {|}  $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0,  // char 9614
    $25, $8F, {|}  $80, $80, $80, $80, $80, $80, $80, $80, $80, $80, $80, $80, $80, $80, $80, $80,  // char 9615
    $25, $90, {|}  $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F,  // char 9616
    $25, $91, {|}  $11, $44, $11, $44, $11, $44, $11, $44, $11, $44, $11, $44, $11, $44, $11, $44,  // char 9617
    $25, $92, {|}  $55, $AA, $55, $AA, $55, $AA, $55, $AA, $55, $AA, $55, $AA, $55, $AA, $55, $AA,  // char 9618
    $25, $93, {|}  $DD, $77, $DD, $77, $DD, $77, $DD, $77, $DD, $77, $DD, $77, $DD, $77, $DD, $77,  // char 9619
    $25, $94, {|}  $FF, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9620
    $25, $95, {|}  $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01,  // char 9621

    // quarter blocks
    $25, $96,    $00, $00, $00, $00, $00, $00, $00, $00, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0,
    $25, $97,    $00, $00, $00, $00, $00, $00, $00, $00, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F,
    $25, $98,    $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $00, $00, $00, $00, $00, $00, $00, $00,
    $25, $99,    $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $25, $9A,    $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F,
    $25, $9B,    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0,
    $25, $9C,    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F,
    $25, $9D,    $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $00, $00, $00, $00, $00, $00, $00, $00,
    $25, $9E,    $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0,
    $25, $9F,    $0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,

    $25, $A0, {|}  $00, $00, $00, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $00, $00, $00, $00,  // char 9632
    $25, $A1, {|}  $00, $00, $00, $FE, $82, $82, $82, $82, $82, $82, $82, $FE, $00, $00, $00, $00,  // char 9633
    $25, $A2, {|}  $00, $00, $00, $7C, $82, $82, $82, $82, $82, $82, $82, $7C, $00, $00, $00, $00,  // char 9634
    $25, $A3, {|}  $00, $00, $00, $FE, $82, $BA, $BA, $BA, $BA, $BA, $82, $FE, $00, $00, $00, $00,  // char 9635
    $25, $A4, {|}  $00, $00, $00, $FE, $82, $FE, $82, $FE, $82, $FE, $82, $FE, $00, $00, $00, $00,  // char 9636
    $25, $A5, {|}  $00, $00, $00, $FE, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $FE, $00, $00, $00, $00,  // char 9637
    $25, $A6, {|}  $00, $00, $00, $FE, $AA, $FE, $AA, $FE, $AA, $FE, $AA, $FE, $00, $00, $00, $00,  // char 9638
    $25, $A7, {|}  $00, $00, $00, $FE, $8A, $C6, $A2, $92, $8A, $C6, $A2, $FE, $00, $00, $00, $00,  // char 9639
    $25, $A8, {|}  $00, $00, $00, $FE, $A2, $C6, $8A, $92, $A2, $C6, $8A, $FE, $00, $00, $00, $00,  // char 9640
    $25, $A9, {|}  $00, $00, $00, $FE, $AA, $C6, $AA, $92, $AA, $C6, $AA, $FE, $00, $00, $00, $00,  // char 9641
    $25, $AA, {|}  $00, $00, $00, $00, $00, $00, $00, $7C, $7C, $7C, $7C, $7C, $00, $00, $00, $00,  // char 9642
    $25, $AB, {|}  $00, $00, $00, $00, $00, $00, $00, $7C, $44, $44, $44, $7C, $00, $00, $00, $00,  // char 9643
    $25, $AC, {|}  $00, $00, $00, $00, $00, $00, $00, $FE, $FE, $FE, $FE, $FE, $00, $00, $00, $00,  // char 9644
    $25, $AD, {|}  $00, $00, $00, $00, $00, $00, $00, $FE, $82, $82, $82, $FE, $00, $00, $00, $00,  // char 9645
    $25, $AE, {|}  $00, $00, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE, $00, $00,  // char 9646
    $25, $AF, {|}  $00, $00, $FE, $82, $82, $82, $82, $82, $82, $82, $82, $82, $82, $FE, $00, $00,  // char 9647
    $25, $B0, {|}  $00, $00, $00, $00, $00, $00, $7E, $7E, $7E, $FC, $FC, $FC, $00, $00, $00, $00,  // char 9648
    $25, $B1, {|}  $00, $00, $00, $00, $00, $00, $7E, $42, $42, $84, $84, $FC, $00, $00, $00, $00,  // char 9649
    $25, $B2, {|}  $00, $00, $00, $00, $00, $10, $38, $38, $7C, $7C, $FE, $FE, $00, $00, $00, $00,  // char 9650
    $25, $B3, {|}  $00, $00, $00, $00, $00, $10, $28, $28, $44, $44, $82, $FE, $00, $00, $00, $00,  // char 9651
    $25, $B4, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $10, $38, $38, $7C, $00, $00, $00, $00,  // char 9652
    $25, $B5, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $10, $28, $28, $7C, $00, $00, $00, $00,  // char 9653
    $25, $B6, {|}  $00, $80, $C0, $E0, $F0, $F8, $FC, $F8, $F0, $E0, $C0, $80, $00, $00, $00, $00,  // char 9654
    $25, $B7, {|}  $00, $80, $C0, $A0, $90, $88, $84, $88, $90, $A0, $C0, $80, $00, $00, $00, $00,  // char 9655
    $25, $B8, {|}  $00, $00, $00, $00, $00, $00, $00, $40, $70, $78, $70, $40, $00, $00, $00, $00,  // char 9656
    $25, $B9, {|}  $00, $00, $00, $00, $00, $00, $00, $40, $70, $48, $70, $40, $00, $00, $00, $00,  // char 9657
    $25, $BA, {|}  $00, $00, $00, $00, $80, $E0, $F8, $FE, $F8, $E0, $80, $00, $00, $00, $00, $00,  // char 9658
    $25, $BB, {|}  $00, $00, $00, $00, $80, $E0, $98, $86, $98, $E0, $80, $00, $00, $00, $00, $00,  // char 9659
    $25, $BC, {|}  $00, $00, $FE, $FE, $7C, $7C, $38, $38, $10, $00, $00, $00, $00, $00, $00, $00,  // char 9660
    $25, $BD, {|}  $00, $00, $FE, $82, $44, $44, $28, $28, $10, $00, $00, $00, $00, $00, $00, $00,  // char 9661
    $25, $BE, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $7C, $38, $38, $10, $00, $00, $00, $00,  // char 9662
    $25, $BF, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $7C, $28, $28, $10, $00, $00, $00, $00,  // char 9663
    $25, $C0, {|}  $00, $02, $06, $0E, $1E, $3E, $7E, $3E, $1E, $0E, $06, $02, $00, $00, $00, $00,  // char 9664
    $25, $C1, {|}  $00, $02, $06, $0A, $12, $22, $42, $22, $12, $0A, $06, $02, $00, $00, $00, $00,  // char 9665
    $25, $C2, {|}  $00, $00, $00, $00, $00, $00, $00, $08, $38, $78, $38, $08, $00, $00, $00, $00,  // char 9666
    $25, $C3, {|}  $00, $00, $00, $00, $00, $00, $00, $08, $38, $48, $38, $08, $00, $00, $00, $00,  // char 9667
    $25, $C4, {|}  $00, $00, $00, $00, $02, $0E, $3E, $FE, $3E, $0E, $02, $00, $00, $00, $00, $00,  // char 9668
    $25, $C5, {|}  $00, $00, $00, $00, $02, $0E, $32, $C2, $32, $0E, $02, $00, $00, $00, $00, $00,  // char 9669
    $25, $C6, {|}  $00, $00, $10, $38, $7C, $7C, $FE, $FE, $7C, $7C, $38, $10, $00, $00, $00, $00,  // char 9670
    $25, $C7, {|}  $00, $00, $10, $28, $44, $44, $82, $82, $44, $44, $28, $10, $00, $00, $00, $00,  // char 9671
    $25, $C8, {|}  $00, $00, $10, $28, $44, $54, $BA, $BA, $54, $44, $28, $10, $00, $00, $00, $00,  // char 9672
    $25, $C9, {|}  $00, $00, $3C, $42, $99, $BD, $BD, $BD, $BD, $99, $42, $3C, $00, $00, $00, $00,  // char 9673
    $25, $CA, {|}  $00, $00, $10, $28, $28, $44, $82, $82, $44, $28, $28, $10, $00, $00, $00, $00,  // char 9674
    $25, $CB, {|}  $00, $00, $3C, $42, $81, $81, $81, $81, $81, $81, $42, $3C, $00, $00, $00, $00,  // char 9675
    $25, $CC, {|}  $00, $00, $14, $40, $01, $80, $01, $80, $01, $80, $02, $28, $00, $00, $00, $00,  // char 9676
    $25, $CD, {|}  $00, $00, $3C, $66, $A5, $A5, $A5, $A5, $A5, $A5, $66, $3C, $00, $00, $00, $00,  // char 9677
    $25, $CE, {|}  $00, $00, $3C, $42, $99, $A5, $A5, $A5, $A5, $99, $42, $3C, $00, $00, $00, $00,  // char 9678
    $25, $CF, {|}  $00, $00, $3C, $7E, $FF, $FF, $FF, $FF, $FF, $FF, $7E, $3C, $00, $00, $00, $00,  // char 9679
    $25, $D0, {|}  $00, $00, $3C, $72, $F1, $F1, $F1, $F1, $F1, $F1, $72, $3C, $00, $00, $00, $00,  // char 9680
    $25, $D1, {|}  $00, $00, $3C, $4E, $8F, $8F, $8F, $8F, $8F, $8F, $4E, $3C, $00, $00, $00, $00,  // char 9681
    $25, $D2, {|}  $00, $00, $3C, $42, $81, $81, $81, $FF, $FF, $FF, $7E, $3C, $00, $00, $00, $00,  // char 9682
    $25, $D3, {|}  $00, $00, $3C, $7E, $FF, $FF, $FF, $81, $81, $81, $42, $3C, $00, $00, $00, $00,  // char 9683
    $25, $D4, {|}  $00, $00, $3C, $4E, $8F, $8F, $8F, $81, $81, $81, $42, $3C, $00, $00, $00, $00,  // char 9684
    $25, $D5, {|}  $00, $00, $3C, $4E, $8F, $8F, $8F, $FF, $FF, $FF, $7E, $3C, $00, $00, $00, $00,  // char 9685
    $25, $D6, {|}  $00, $00, $30, $70, $F0, $F0, $F0, $F0, $F0, $F0, $70, $30, $00, $00, $00, $00,  // char 9686
    $25, $D7, {|}  $00, $00, $0C, $0E, $0F, $0F, $0F, $0F, $0F, $0F, $0E, $0C, $00, $00, $00, $00,  // char 9687
    $25, $D8, {|}  $FF, $FF, $FF, $FF, $FF, $FF, $E7, $C3, $C3, $E7, $FF, $FF, $FF, $FF, $FF, $FF,  // char 9688
    $25, $D9, {|}  $FF, $FF, $C3, $BD, $7E, $7E, $7E, $7E, $7E, $7E, $BD, $C3, $FF, $FF, $FF, $FF,  // char 9689
    $25, $DA, {|}  $FF, $FF, $C3, $BD, $7E, $7E, $7E, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9690
    $25, $DB, {|}  $00, $00, $00, $00, $00, $00, $00, $7E, $7E, $7E, $BD, $C3, $FF, $FF, $FF, $FF,  // char 9691
    $25, $DC, {|}  $00, $00, $30, $40, $80, $80, $80, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9692
    $25, $DD, {|}  $00, $00, $0C, $02, $01, $01, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9693
    $25, $DE, {|}  $00, $00, $00, $00, $00, $00, $00, $01, $01, $01, $02, $0C, $00, $00, $00, $00,  // char 9694
    $25, $DF, {|}  $00, $00, $00, $00, $00, $00, $00, $80, $80, $80, $40, $30, $00, $00, $00, $00,  // char 9695
    $25, $E0, {|}  $00, $00, $3C, $42, $81, $81, $81, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9696
    $25, $E1, {|}  $00, $00, $00, $00, $00, $00, $00, $81, $81, $81, $42, $3C, $00, $00, $00, $00,  // char 9697
    $25, $E2, {|}  $01, $01, $03, $03, $07, $07, $0F, $0F, $1F, $1F, $3F, $3F, $7F, $7F, $FF, $FF,  // char 9698
    $25, $E3, {|}  $80, $80, $C0, $C0, $E0, $E0, $F0, $F0, $F8, $F8, $FC, $FC, $FE, $FE, $FF, $FF,  // char 9699
    $25, $E4, {|}  $FF, $FF, $FE, $FE, $FC, $FC, $F8, $F8, $F0, $F0, $E0, $E0, $C0, $C0, $80, $80,  // char 9700
    $25, $E5, {|}  $FF, $FF, $7F, $7F, $3F, $3F, $1F, $1F, $0F, $0F, $07, $07, $03, $03, $01, $01,  // char 9701
    $25, $E6, {|}  $00, $00, $00, $00, $00, $3C, $66, $42, $42, $66, $3C, $00, $00, $00, $00, $00,  // char 9702
    $25, $E7, {|}  $00, $00, $FF, $F1, $F1, $F1, $F1, $F1, $F1, $F1, $F1, $FF, $00, $00, $00, $00,  // char 9703
    $25, $E8, {|}  $00, $00, $FF, $8F, $8F, $8F, $8F, $8F, $8F, $8F, $8F, $FF, $00, $00, $00, $00,  // char 9704
    $25, $E9, {|}  $00, $00, $FF, $FF, $FD, $F9, $F1, $F1, $E1, $C1, $81, $FF, $00, $00, $00, $00,  // char 9705
    $25, $EA, {|}  $00, $00, $FF, $81, $83, $87, $8F, $8F, $9F, $BF, $FF, $FF, $00, $00, $00, $00,  // char 9706
    $25, $EB, {|}  $00, $00, $FF, $99, $99, $99, $99, $99, $99, $99, $99, $FF, $00, $00, $00, $00,  // char 9707
    $25, $EC, {|}  $00, $00, $00, $10, $10, $28, $28, $44, $44, $92, $82, $FE, $00, $00, $00, $00,  // char 9708
    $25, $ED, {|}  $00, $00, $00, $10, $10, $38, $38, $74, $74, $F2, $F2, $FE, $00, $00, $00, $00,  // char 9709
    $25, $EE, {|}  $00, $00, $00, $10, $10, $38, $38, $5C, $5C, $9E, $9E, $FE, $00, $00, $00, $00,  // char 9710
    $25, $EF, {|}  $00, $00, $3C, $42, $81, $81, $81, $81, $81, $81, $81, $42, $3C, $00, $00, $00,  // char 9711
    $25, $F0, {|}  $00, $00, $00, $FE, $92, $92, $92, $F2, $82, $82, $82, $FE, $00, $00, $00, $00,  // char 9712
    $25, $F1, {|}  $00, $00, $00, $FE, $82, $82, $82, $F2, $92, $92, $92, $FE, $00, $00, $00, $00,  // char 9713
    $25, $F2, {|}  $00, $00, $00, $FE, $82, $82, $82, $9E, $92, $92, $92, $FE, $00, $00, $00, $00,  // char 9714
    $25, $F3, {|}  $00, $00, $00, $FE, $92, $92, $92, $9E, $82, $82, $82, $FE, $00, $00, $00, $00,  // char 9715
    $25, $F4, {|}  $00, $00, $3C, $4A, $89, $89, $89, $F9, $81, $81, $42, $3C, $00, $00, $00, $00,  // char 9716
    $25, $F5, {|}  $00, $00, $3C, $42, $81, $81, $F9, $89, $89, $89, $4A, $3C, $00, $00, $00, $00,  // char 9717
    $25, $F6, {|}  $00, $00, $3C, $42, $81, $81, $9F, $91, $91, $91, $52, $3C, $00, $00, $00, $00,  // char 9718
    $25, $F7, {|}  $00, $00, $3C, $52, $91, $91, $91, $9F, $81, $81, $42, $3C, $00, $00, $00, $00,  // char 9719
    $26, $00, {|}  $00, $00, $00, $10, $10, $54, $38, $FE, $38, $54, $10, $10, $00, $00, $00, $00,  // char 9728
    $26, $01, {|}  $00, $00, $00, $00, $30, $7C, $FE, $FF, $00, $00, $00, $00, $00, $00, $00, $00,  // char 9729
    $26, $02, {|}  $00, $00, $00, $38, $7C, $FE, $10, $10, $10, $10, $14, $08, $00, $00, $00, $00,  // char 9730
    $26, $05, {|}  $00, $00, $00, $10, $10, $38, $FE, $7C, $38, $6C, $44, $00, $00, $00, $00, $00,  // char 9733
    $26, $06, {|}  $00, $00, $00, $10, $10, $28, $EE, $44, $54, $6C, $44, $00, $00, $00, $00, $00,  // char 9734
    $26, $07, {|}  $00, $00, $08, $18, $30, $60, $C0, $60, $34, $1C, $3C, $00, $00, $00, $00, $00,  // char 9735
    $26, $08, {|}  $00, $00, $FE, $C6, $CC, $D8, $F0, $D8, $CD, $C7, $CF, $00, $00, $00, $00, $00,  // char 9736
    $26, $09, {|}  $00, $00, $00, $00, $38, $44, $82, $92, $82, $44, $38, $00, $00, $00, $00, $00,  // char 9737
    $26, $0A, {|}  $00, $00, $00, $00, $18, $24, $42, $42, $42, $A5, $A5, $42, $00, $00, $00, $00,  // char 9738
    $26, $0B, {|}  $00, $00, $00, $00, $42, $A5, $A5, $42, $42, $42, $24, $18, $00, $00, $00, $00,  // char 9739
    $26, $0C, {|}  $00, $00, $00, $00, $00, $06, $0C, $78, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 9740
    $26, $0D, {|}  $00, $00, $1C, $36, $36, $1C, $18, $30, $70, $D8, $D8, $70, $00, $00, $00, $00,  // char 9741
    $26, $10, {|}  $00, $00, $00, $00, $FE, $82, $82, $82, $82, $82, $82, $FE, $00, $00, $00, $00,  // char 9744
    $26, $11, {|}  $00, $00, $00, $00, $FE, $82, $86, $8A, $BA, $92, $82, $FE, $00, $00, $00, $00,  // char 9745
    $26, $12, {|}  $00, $00, $00, $00, $FE, $82, $EE, $BA, $BA, $EE, $82, $FE, $00, $00, $00, $00,  // char 9746
    $26, $13, {|}  $00, $00, $00, $82, $C6, $6C, $7C, $38, $7C, $6C, $C6, $82, $00, $00, $00, $00,  // char 9747
    $26, $20, {|}  $00, $7C, $82, $AA, $82, $44, $7C, $44, $38, $00, $44, $C6, $38, $C6, $44, $00,  // char 9760
    $26, $22, {|}  $00, $00, $00, $38, $44, $EE, $FE, $92, $BA, $7C, $38, $00, $00, $00, $00, $00,  // char 9762
    $26, $26, {|}  $00, $00, $18, $7E, $18, $18, $FF, $18, $1E, $78, $18, $18, $00, $00, $00, $00,  // char 9766
    $26, $28, {|}  $00, $00, $18, $7E, $18, $18, $FF, $18, $18, $18, $18, $18, $00, $00, $00, $00,  // char 9768
    $26, $29, {|}  $00, $00, $00, $38, $10, $10, $92, $FE, $92, $10, $10, $38, $00, $00, $00, $00,  // char 9769
    $26, $2A, {|}  $00, $00, $3C, $72, $E0, $C4, $DF, $CE, $CA, $E0, $72, $3C, $00, $00, $00, $00,  // char 9770
    $26, $2B, {|}  $00, $00, $54, $28, $54, $AA, $AA, $AA, $AA, $54, $38, $00, $00, $00, $00, $00,  // char 9771
    $26, $2E, {|}  $00, $00, $3C, $5A, $99, $99, $99, $BD, $FF, $DB, $5A, $3C, $00, $00, $00, $00,  // char 9774
    $26, $2F, {|}  $00, $00, $3C, $42, $81, $85, $B1, $F9, $FF, $DF, $7E, $3C, $00, $00, $00, $00,  // char 9775
    $26, $30, {|}  $00, $00, $FE, $FE, $00, $00, $FE, $FE, $00, $00, $FE, $FE, $00, $00, $00, $00,  // char 9776
    $26, $31, {|}  $00, $00, $EE, $EE, $00, $00, $FE, $FE, $00, $00, $FE, $FE, $00, $00, $00, $00,  // char 9777
    $26, $32, {|}  $00, $00, $FE, $FE, $00, $00, $EE, $EE, $00, $00, $FE, $FE, $00, $00, $00, $00,  // char 9778
    $26, $33, {|}  $00, $00, $EE, $EE, $00, $00, $EE, $EE, $00, $00, $FE, $FE, $00, $00, $00, $00,  // char 9779
    $26, $34, {|}  $00, $00, $FE, $FE, $00, $00, $FE, $FE, $00, $00, $EE, $EE, $00, $00, $00, $00,  // char 9780
    $26, $35, {|}  $00, $00, $EE, $EE, $00, $00, $FE, $FE, $00, $00, $EE, $EE, $00, $00, $00, $00,  // char 9781
    $26, $36, {|}  $00, $00, $FE, $FE, $00, $00, $EE, $EE, $00, $00, $EE, $EE, $00, $00, $00, $00,  // char 9782
    $26, $37, {|}  $00, $00, $EE, $EE, $00, $00, $EE, $EE, $00, $00, $EE, $EE, $00, $00, $00, $00,  // char 9783
    $26, $39, {|}  $00, $00, $7E, $81, $A5, $81, $81, $81, $99, $A5, $81, $7E, $00, $00, $00, $00,  // char 9785
    $26, $3A, {|}  $00, $00, $7E, $81, $A5, $81, $81, $A5, $99, $81, $81, $7E, $00, $00, $00, $00,  // char 9786
    $26, $3B, {|}  $00, $00, $7E, $FF, $DB, $FF, $FF, $DB, $E7, $FF, $FF, $7E, $00, $00, $00, $00,  // char 9787
    $26, $3C, {|}  $00, $00, $00, $10, $10, $54, $28, $C6, $28, $54, $10, $10, $00, $00, $00, $00,  // char 9788
    $26, $3D, {|}  $00, $00, $78, $14, $0A, $0A, $0A, $0A, $0A, $0A, $14, $78, $00, $00, $00, $00,  // char 9789
    $26, $3E, {|}  $00, $00, $1E, $28, $50, $50, $50, $50, $50, $50, $28, $1E, $00, $00, $00, $00,  // char 9790
    $26, $3F, {|}  $00, $66, $66, $3C, $66, $66, $66, $66, $3C, $18, $7E, $18, $18, $00, $00, $00,  // char 9791
    $26, $40, {|}  $00, $00, $3C, $66, $66, $66, $66, $3C, $18, $7E, $18, $18, $00, $00, $00, $00,  // char 9792
    $26, $41, {|}  $00, $00, $18, $18, $7E, $18, $3C, $66, $66, $66, $66, $3C, $00, $00, $00, $00,  // char 9793
    $26, $42, {|}  $00, $00, $1E, $06, $0E, $1A, $78, $CC, $CC, $CC, $CC, $78, $00, $00, $00, $00,  // char 9794
    $26, $43, {|}  $00, $03, $03, $7B, $CF, $CF, $CF, $0F, $1B, $33, $FF, $03, $03, $03, $00, $00,  // char 9795
    $26, $44, {|}  $00, $00, $60, $F0, $60, $6E, $73, $63, $63, $66, $66, $63, $00, $00, $00, $00,  // char 9796
    $26, $45, {|}  $00, $00, $D6, $54, $54, $7C, $54, $54, $D6, $38, $44, $44, $38, $00, $00, $00,  // char 9797
    $26, $46, {|}  $00, $00, $18, $3C, $DB, $DB, $DB, $DB, $7E, $3C, $18, $3C, $18, $18, $00, $00,  // char 9798
    $26, $47, {|}  $00, $00, $F8, $CC, $CC, $CC, $CC, $F8, $C0, $C0, $C0, $C0, $FE, $00, $00, $00,  // char 9799
    $26, $48, {|}  $00, $00, $42, $A5, $A5, $3C, $3C, $18, $18, $18, $18, $18, $18, $00, $00, $00,  // char 9800
    $26, $49, {|}  $00, $00, $00, $C3, $66, $66, $66, $66, $3C, $66, $66, $66, $66, $3C, $00, $00,  // char 9801
    $26, $4A, {|}  $00, $00, $C3, $7E, $66, $66, $66, $66, $66, $66, $7E, $C3, $00, $00, $00, $00,  // char 9802
    $26, $4B, {|}  $00, $00, $7E, $DB, $D8, $70, $00, $00, $0E, $1B, $DB, $7E, $00, $00, $00, $00,  // char 9803
    $26, $4C, {|}  $00, $00, $1E, $33, $33, $33, $33, $1B, $7B, $DB, $DB, $73, $03, $01, $00, $00,  // char 9804
    $26, $4D, {|}  $00, $00, $94, $7C, $55, $57, $55, $55, $55, $55, $55, $55, $06, $0B, $00, $00,  // char 9805
    $26, $4E, {|}  $00, $00, $3C, $66, $66, $66, $66, $24, $E7, $00, $FF, $00, $00, $00, $00, $00,  // char 9806
    $26, $4F, {|}  $00, $00, $94, $7C, $54, $54, $54, $54, $54, $54, $54, $54, $04, $03, $00, $00,  // char 9807
    $26, $50, {|}  $00, $00, $1F, $07, $8F, $DB, $73, $70, $D8, $88, $00, $00, $00, $00, $00, $00,  // char 9808
    $26, $51, {|}  $00, $00, $90, $50, $70, $68, $48, $48, $4E, $49, $09, $0E, $10, $20, $00, $00,  // char 9809
    $26, $52, {|}  $00, $00, $00, $2A, $7E, $AA, $00, $00, $2A, $7E, $AA, $00, $00, $00, $00, $00,  // char 9810
    $26, $53, {|}  $00, $00, $C3, $66, $66, $66, $66, $FF, $66, $66, $66, $66, $C3, $00, $00, $00,  // char 9811
    $26, $60, {|}  $00, $00, $00, $18, $3C, $7E, $FF, $FF, $7E, $18, $18, $3C, $00, $00, $00, $00,  // char 9824
    $26, $61, {|}  $00, $00, $00, $00, $6C, $92, $82, $82, $82, $44, $28, $10, $00, $00, $00, $00,  // char 9825
    $26, $62, {|}  $00, $00, $00, $00, $10, $28, $44, $82, $44, $28, $10, $00, $00, $00, $00, $00,  // char 9826
    $26, $63, {|}  $00, $00, $00, $18, $3C, $3C, $E7, $E7, $E7, $18, $18, $3C, $00, $00, $00, $00,  // char 9827
    $26, $64, {|}  $00, $00, $00, $18, $24, $42, $81, $81, $66, $18, $18, $3C, $00, $00, $00, $00,  // char 9828
    $26, $65, {|}  $00, $00, $00, $00, $6C, $FE, $FE, $FE, $FE, $7C, $38, $10, $00, $00, $00, $00,  // char 9829
    $26, $66, {|}  $00, $00, $00, $00, $10, $38, $7C, $FE, $7C, $38, $10, $00, $00, $00, $00, $00,  // char 9830
    $26, $67, {|}  $00, $00, $00, $18, $24, $3C, $E7, $A5, $E7, $18, $18, $3C, $00, $00, $00, $00,  // char 9831
    $26, $69, {|}  $00, $00, $18, $18, $18, $18, $18, $18, $18, $38, $78, $70, $00, $00, $00, $00,  // char 9833
    $26, $6A, {|}  $00, $00, $30, $3C, $3E, $32, $30, $30, $30, $70, $F0, $E0, $00, $00, $00, $00,  // char 9834
    $26, $6B, {|}  $00, $00, $70, $7F, $6F, $63, $63, $63, $63, $E3, $E7, $C7, $06, $00, $00, $00,  // char 9835
    $26, $6C, {|}  $00, $00, $7F, $63, $7F, $63, $63, $63, $63, $63, $E7, $E7, $C6, $00, $00, $00,  // char 9836
    $26, $6D, {|}  $00, $00, $C0, $C0, $C0, $C0, $CC, $DE, $E6, $C4, $D8, $E0, $00, $00, $00, $00,  // char 9837
    $26, $6E, {|}  $00, $00, $40, $40, $44, $5C, $74, $44, $44, $5C, $74, $44, $04, $04, $00, $00,  // char 9838
    $26, $6F, {|}  $00, $00, $04, $04, $46, $5C, $74, $C4, $46, $5C, $74, $C4, $40, $40, $00, $00,  // char 9839
    $FB, $00, {|}  $00, $00, $3B, $66, $66, $66, $FF, $66, $66, $66, $66, $FF, $00, $00, $00, $00,  // char 64256
    $FB, $01, {|}  $00, $00, $3C, $6E, $66, $60, $FE, $66, $66, $66, $66, $FF, $00, $00, $00, $00,  // char 64257
    $FB, $02, {|}  $00, $00, $3E, $6E, $66, $66, $FE, $66, $66, $66, $66, $FF, $00, $00, $00, $00,  // char 64258
    $FB, $03, {|}  $00, $00, $6E, $DB, $DB, $D8, $FF, $DB, $DB, $DB, $DB, $FF, $00, $00, $00, $00,  // char 64259
    $FB, $04, {|}  $00, $00, $6F, $DB, $DB, $DB, $FF, $DB, $DB, $DB, $DB, $FF, $00, $00, $00, $00,  // char 64260
    $FB, $05, {|}  $00, $00, $38, $6C, $6C, $6F, $6C, $6C, $6C, $6C, $6D, $F6, $00, $00, $00, $00,  // char 64261
    $FB, $50, {|}  $06, $29, $5E, $00, $04, $04, $04, $04, $04, $04, $00, $00, $00, $00, $00, $00,  // char 64336
    $FB, $51, {|}  $06, $29, $5E, $00, $04, $04, $04, $04, $04, $03, $00, $00, $00, $00, $00, $00,  // char 64337
    $FB, $52, {|}  $00, $00, $00, $00, $00, $40, $81, $81, $7E, $00, $08, $00, $08, $00, $00, $00,  // char 64338
    $FB, $53, {|}  $00, $00, $00, $00, $00, $40, $80, $81, $7E, $00, $08, $00, $08, $00, $00, $00,  // char 64339
    $FB, $54, {|}  $00, $00, $00, $00, $00, $08, $04, $04, $F8, $00, $08, $00, $08, $00, $00, $00,  // char 64340
    $FB, $55, {|}  $00, $00, $00, $00, $00, $00, $08, $08, $F7, $00, $08, $00, $08, $00, $00, $00,  // char 64341
    $FB, $56, {|}  $00, $00, $00, $00, $00, $40, $81, $81, $7E, $00, $14, $00, $08, $00, $00, $00,  // char 64342
    $FB, $57, {|}  $00, $00, $00, $00, $00, $40, $80, $81, $7E, $00, $14, $00, $08, $00, $00, $00,  // char 64343
    $FB, $58, {|}  $00, $00, $00, $00, $00, $08, $04, $04, $F8, $00, $14, $00, $08, $00, $00, $00,  // char 64344
    $FB, $59, {|}  $00, $00, $00, $00, $00, $00, $08, $08, $F7, $00, $14, $00, $08, $00, $00, $00,  // char 64345
    $FB, $5A, {|}  $00, $00, $00, $00, $00, $40, $81, $81, $7E, $00, $14, $00, $14, $00, $00, $00,  // char 64346
    $FB, $5B, {|}  $00, $00, $00, $00, $00, $40, $80, $81, $7E, $00, $14, $00, $14, $00, $00, $00,  // char 64347
    $FB, $5C, {|}  $00, $00, $00, $00, $00, $08, $04, $04, $F8, $00, $14, $00, $14, $00, $00, $00,  // char 64348
    $FB, $5D, {|}  $00, $00, $00, $00, $00, $00, $08, $08, $F7, $00, $14, $00, $14, $00, $00, $00,  // char 64349
    $FB, $5E, {|}  $00, $08, $00, $08, $00, $40, $81, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 64350
    $FB, $5F, {|}  $00, $08, $00, $08, $00, $40, $80, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 64351
    $FB, $60, {|}  $00, $08, $00, $08, $00, $08, $04, $04, $F8, $00, $00, $00, $00, $00, $00, $00,  // char 64352
    $FB, $61, {|}  $00, $08, $00, $08, $00, $00, $08, $08, $F7, $00, $00, $00, $00, $00, $00, $00,  // char 64353
    $FB, $62, {|}  $00, $14, $00, $14, $00, $40, $81, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 64354
    $FB, $63, {|}  $00, $14, $00, $14, $00, $40, $80, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 64355
    $FB, $64, {|}  $00, $14, $00, $14, $00, $08, $04, $04, $F8, $00, $00, $00, $00, $00, $00, $00,  // char 64356
    $FB, $65, {|}  $00, $14, $00, $14, $00, $00, $08, $08, $F7, $00, $00, $00, $00, $00, $00, $00,  // char 64357
    $FB, $66, {|}  $10, $1C, $14, $38, $00, $40, $81, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 64358
    $FB, $67, {|}  $10, $1C, $14, $38, $00, $40, $80, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 64359
    $FB, $68, {|}  $10, $1C, $14, $38, $00, $08, $04, $04, $F8, $00, $00, $00, $00, $00, $00, $00,  // char 64360
    $FB, $69, {|}  $10, $1C, $14, $38, $00, $00, $08, $08, $F7, $00, $00, $00, $00, $00, $00, $00,  // char 64361
    $FB, $6A, {|}  $04, $00, $0A, $00, $06, $49, $85, $83, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 64362
    $FB, $6B, {|}  $04, $00, $0A, $00, $06, $49, $89, $86, $7D, $00, $00, $00, $00, $00, $00, $00,  // char 64363
    $FB, $6C, {|}  $08, $00, $14, $00, $0C, $12, $0A, $06, $FC, $00, $00, $00, $00, $00, $00, $00,  // char 64364
    $FB, $6D, {|}  $08, $00, $14, $00, $0C, $12, $12, $0C, $F3, $00, $00, $00, $00, $00, $00, $00,  // char 64365
    $FB, $6E, {|}  $0A, $00, $0A, $00, $06, $49, $85, $83, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 64366
    $FB, $6F, {|}  $0A, $00, $0A, $00, $06, $49, $89, $86, $7D, $00, $00, $00, $00, $00, $00, $00,  // char 64367
    $FB, $70, {|}  $14, $00, $14, $00, $0C, $12, $0A, $06, $FC, $00, $00, $00, $00, $00, $00, $00,  // char 64368
    $FB, $71, {|}  $14, $00, $14, $00, $0C, $12, $12, $0C, $F3, $00, $00, $00, $00, $00, $00, $00,  // char 64369
    $FB, $72, {|}  $00, $00, $00, $00, $70, $8F, $30, $40, $88, $80, $88, $41, $3E, $00, $00, $00,  // char 64370
    $FB, $73, {|}  $00, $00, $00, $00, $00, $70, $8F, $32, $41, $88, $80, $88, $40, $3E, $00, $00,  // char 64371
    $FB, $74, {|}  $00, $00, $00, $00, $00, $38, $47, $18, $E0, $00, $04, $00, $04, $00, $00, $00,  // char 64372
    $FB, $75, {|}  $00, $00, $00, $00, $00, $70, $8F, $32, $C1, $00, $08, $00, $08, $00, $00, $00,  // char 64373
    $FB, $76, {|}  $00, $00, $00, $00, $70, $8F, $30, $40, $80, $94, $80, $41, $3E, $00, $00, $00,  // char 64374
    $FB, $77, {|}  $00, $00, $00, $00, $00, $70, $8F, $32, $41, $80, $94, $80, $40, $3E, $00, $00,  // char 64375
    $FB, $78, {|}  $00, $00, $00, $00, $00, $38, $47, $18, $E0, $00, $00, $0A, $00, $00, $00, $00,  // char 64376
    $FB, $79, {|}  $00, $00, $00, $00, $00, $70, $8F, $32, $C1, $00, $00, $14, $00, $00, $00, $00,  // char 64377
    $FB, $7A, {|}  $00, $00, $00, $00, $70, $8F, $30, $40, $94, $80, $88, $41, $3E, $00, $00, $00,  // char 64378
    $FB, $7B, {|}  $00, $00, $00, $00, $00, $70, $8F, $32, $41, $94, $80, $88, $40, $3E, $00, $00,  // char 64379
    $FB, $7C, {|}  $00, $00, $00, $00, $00, $38, $47, $18, $E0, $00, $0A, $00, $04, $00, $00, $00,  // char 64380
    $FB, $7D, {|}  $00, $00, $00, $00, $00, $70, $8F, $32, $C1, $00, $14, $00, $08, $00, $00, $00,  // char 64381
    $FB, $7E, {|}  $00, $00, $00, $00, $70, $8F, $30, $40, $94, $80, $94, $41, $3E, $00, $00, $00,  // char 64382
    $FB, $7F, {|}  $00, $00, $00, $00, $00, $70, $8F, $32, $41, $94, $80, $94, $40, $3E, $00, $00,  // char 64383
    $FB, $80, {|}  $00, $00, $00, $00, $00, $38, $47, $18, $E0, $00, $0A, $00, $0A, $00, $00, $00,  // char 64384
    $FB, $81, {|}  $00, $00, $00, $00, $00, $70, $8F, $32, $C1, $00, $14, $00, $14, $00, $00, $00,  // char 64385
    $FB, $82, {|}  $00, $00, $00, $00, $08, $04, $02, $42, $3C, $00, $00, $14, $00, $00, $00, $00,  // char 64386
    $FB, $83, {|}  $00, $00, $00, $00, $08, $08, $04, $46, $39, $00, $00, $14, $00, $00, $00, $00,  // char 64387
    $FB, $84, {|}  $00, $00, $28, $00, $08, $04, $02, $42, $3C, $00, $00, $00, $00, $00, $00, $00,  // char 64388
    $FB, $85, {|}  $00, $00, $28, $00, $08, $08, $04, $46, $39, $00, $00, $00, $00, $00, $00, $00,  // char 64389
    $FB, $86, {|}  $10, $00, $28, $00, $08, $04, $02, $42, $3C, $00, $00, $00, $00, $00, $00, $00,  // char 64390
    $FB, $87, {|}  $10, $00, $28, $00, $08, $08, $04, $46, $39, $00, $00, $00, $00, $00, $00, $00,  // char 64391
    $FB, $88, {|}  $20, $38, $28, $70, $08, $04, $02, $42, $3C, $00, $00, $00, $00, $00, $00, $00,  // char 64392
    $FB, $89, {|}  $20, $38, $28, $70, $08, $08, $04, $46, $39, $00, $00, $00, $00, $00, $00, $00,  // char 64393
    $FB, $8A, {|}  $08, $00, $14, $00, $00, $04, $02, $02, $04, $18, $60, $00, $00, $00, $00, $00,  // char 64394
    $FB, $8B, {|}  $00, $00, $08, $00, $14, $00, $00, $04, $07, $04, $18, $60, $00, $00, $00, $00,  // char 64395
    $FB, $8C, {|}  $10, $1C, $14, $38, $00, $04, $02, $02, $04, $18, $60, $00, $00, $00, $00, $00,  // char 64396
    $FB, $8D, {|}  $10, $1C, $14, $38, $00, $00, $00, $04, $07, $04, $18, $60, $00, $00, $00, $00,  // char 64397
    $FB, $8E, {|}  $00, $01, $02, $04, $08, $44, $82, $82, $7C, $00, $00, $00, $00, $00, $00, $00,  // char 64398
    $FB, $8F, {|}  $00, $01, $02, $04, $08, $48, $84, $82, $7D, $00, $00, $00, $00, $00, $00, $00,  // char 64399
    $FB, $90, {|}  $00, $00, $06, $18, $20, $10, $08, $08, $F0, $00, $00, $00, $00, $00, $00, $00,  // char 64400
    $FB, $91, {|}  $00, $00, $06, $18, $20, $20, $10, $08, $F7, $00, $00, $00, $00, $00, $00, $00,  // char 64401
    $FB, $92, {|}  $04, $09, $12, $04, $08, $44, $82, $82, $7C, $00, $00, $00, $00, $00, $00, $00,  // char 64402
    $FB, $93, {|}  $04, $09, $12, $04, $08, $48, $84, $82, $7D, $00, $00, $00, $00, $00, $00, $00,  // char 64403
    $FB, $94, {|}  $0C, $30, $46, $18, $20, $10, $08, $08, $F0, $00, $00, $00, $00, $00, $00, $00,  // char 64404
    $FB, $95, {|}  $0C, $30, $46, $18, $20, $20, $10, $08, $F7, $00, $00, $00, $00, $00, $00, $00,  // char 64405
    $FB, $96, {|}  $04, $09, $12, $04, $08, $44, $82, $82, $7C, $00, $10, $00, $10, $00, $00, $00,  // char 64406
    $FB, $97, {|}  $04, $09, $12, $04, $08, $48, $84, $82, $7D, $00, $10, $00, $10, $00, $00, $00,  // char 64407
    $FB, $98, {|}  $0C, $30, $46, $18, $20, $10, $08, $08, $F0, $00, $10, $00, $10, $00, $00, $00,  // char 64408
    $FB, $99, {|}  $0C, $30, $46, $18, $20, $20, $10, $08, $F7, $00, $08, $00, $08, $00, $00, $00,  // char 64409
    $FB, $9A, {|}  $A4, $09, $12, $04, $08, $44, $82, $82, $7C, $00, $00, $00, $00, $00, $00, $00,  // char 64410
    $FB, $9B, {|}  $A4, $09, $12, $04, $08, $48, $84, $82, $7D, $00, $00, $00, $00, $00, $00, $00,  // char 64411
    $FB, $9C, {|}  $A2, $0C, $13, $0C, $10, $08, $04, $04, $F8, $00, $00, $00, $00, $00, $00, $00,  // char 64412
    $FB, $9D, {|}  $A2, $0C, $13, $0C, $10, $10, $08, $04, $FB, $00, $00, $00, $00, $00, $00, $00,  // char 64413
    $FB, $9E, {|}  $00, $00, $00, $00, $00, $41, $81, $81, $81, $42, $3C, $00, $00, $00, $00, $00,  // char 64414
    $FB, $9F, {|}  $00, $00, $00, $00, $00, $00, $00, $01, $41, $81, $81, $42, $3C, $00, $00, $00,  // char 64415
    $FB, $A0, {|}  $10, $1C, $14, $38, $00, $41, $81, $81, $81, $42, $3C, $00, $00, $00, $00, $00,  // char 64416
    $FB, $A1, {|}  $10, $1C, $14, $38, $00, $00, $00, $01, $41, $81, $81, $42, $3C, $00, $00, $00,  // char 64417
    $FB, $A2, {|}  $10, $1C, $14, $38, $00, $08, $04, $04, $F8, $00, $00, $00, $00, $00, $00, $00,  // char 64418
    $FB, $A3, {|}  $10, $1C, $14, $38, $00, $00, $08, $08, $F7, $00, $00, $00, $00, $00, $00, $00,  // char 64419
    $FB, $A4, {|}  $38, $40, $38, $40, $18, $24, $22, $22, $1C, $00, $00, $00, $00, $00, $00, $00,  // char 64420
    $FB, $A5, {|}  $70, $80, $70, $80, $10, $68, $88, $74, $03, $00, $00, $00, $00, $00, $00, $00,  // char 64421
    $FB, $A6, {|}  $00, $00, $00, $00, $18, $24, $22, $22, $1C, $00, $00, $00, $00, $00, $00, $00,  // char 64422
    $FB, $A7, {|}  $00, $00, $00, $00, $00, $00, $00, $0C, $13, $00, $00, $00, $00, $00, $00, $00,  // char 64423
    $FB, $A8, {|}  $00, $00, $00, $00, $00, $00, $00, $10, $F0, $10, $14, $08, $00, $00, $00, $00,  // char 64424
    $FB, $A9, {|}  $00, $00, $00, $00, $00, $00, $00, $10, $F7, $08, $08, $08, $00, $00, $00, $00,  // char 64425
    $FB, $AA, {|}  $00, $00, $00, $10, $38, $4C, $52, $32, $3D, $40, $00, $00, $00, $00, $00, $00,  // char 64426
    $FB, $AB, {|}  $00, $00, $00, $10, $38, $4C, $52, $32, $3D, $40, $00, $00, $00, $00, $00, $00,  // char 64427
    $FB, $AC, {|}  $00, $00, $00, $10, $38, $4C, $52, $32, $FC, $00, $00, $00, $00, $00, $00, $00,  // char 64428
    $FB, $AD, {|}  $00, $00, $00, $10, $38, $4C, $52, $32, $FD, $00, $00, $00, $00, $00, $00, $00,  // char 64429
    $FB, $AE, {|}  $00, $00, $00, $00, $00, $00, $08, $14, $60, $80, $FE, $00, $00, $00, $00, $00,  // char 64430
    $FB, $AF, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $07, $18, $20, $3F, $00, $00, $00, $00,  // char 64431
    $FB, $B0, {|}  $00, $00, $70, $80, $70, $80, $08, $14, $60, $80, $FE, $00, $00, $00, $00, $00,  // char 64432
    $FB, $B1, {|}  $00, $00, $1C, $20, $1C, $20, $00, $00, $07, $18, $20, $3F, $00, $00, $00, $00,  // char 64433
    $FB, $D3, {|}  $10, $00, $29, $01, $19, $21, $19, $A1, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 64467
    $FB, $D4, {|}  $10, $00, $2A, $02, $32, $42, $32, $C6, $79, $00, $00, $00, $00, $00, $00, $00,  // char 64468
    $FB, $D5, {|}  $40, $00, $A3, $0C, $10, $08, $04, $04, $F8, $00, $00, $00, $00, $00, $00, $00,  // char 64469
    $FB, $D6, {|}  $40, $00, $A3, $0C, $10, $10, $08, $04, $FB, $00, $00, $00, $00, $00, $00, $00,  // char 64470
    $FB, $D7, {|}  $10, $28, $18, $10, $20, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 64471
    $FB, $D8, {|}  $10, $28, $18, $10, $20, $00, $0C, $12, $0F, $02, $04, $18, $60, $00, $00, $00,  // char 64472
    $FB, $D9, {|}  $00, $14, $08, $00, $00, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 64473
    $FB, $DA, {|}  $00, $14, $08, $00, $00, $00, $0C, $12, $0F, $02, $04, $18, $60, $00, $00, $00,  // char 64474
    $FB, $DB, {|}  $08, $08, $08, $08, $00, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 64475
    $FB, $DC, {|}  $08, $08, $08, $08, $00, $00, $0C, $12, $0F, $02, $04, $18, $60, $00, $00, $00,  // char 64476
    $FB, $DD, {|}  $23, $54, $33, $24, $40, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 64477
    $FB, $DE, {|}  $00, $08, $00, $14, $00, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 64478
    $FB, $DF, {|}  $00, $00, $08, $00, $14, $00, $0C, $12, $0F, $02, $04, $18, $60, $00, $00, $00,  // char 64479
    $FB, $E0, {|}  $00, $00, $00, $00, $00, $0C, $12, $0E, $02, $3C, $18, $60, $00, $00, $00, $00,  // char 64480
    $FB, $E1, {|}  $00, $14, $08, $00, $00, $00, $0C, $12, $0F, $02, $3C, $18, $60, $00, $00, $00,  // char 64481
    $FB, $E2, {|}  $00, $08, $14, $00, $00, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 64482
    $FB, $E3, {|}  $00, $08, $14, $00, $00, $00, $0C, $12, $0F, $02, $04, $18, $60, $00, $00, $00,  // char 64483
    $FB, $E4, {|}  $00, $00, $00, $00, $00, $07, $08, $88, $86, $81, $81, $7E, $00, $10, $00, $10,  // char 64484
    $FB, $E5, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $07, $88, $87, $81, $7E, $10, $00, $10,  // char 64485
    $FB, $E6, {|}  $00, $00, $00, $00, $00, $08, $04, $04, $F8, $00, $08, $00, $08, $00, $00, $00,  // char 64486
    $FB, $E7, {|}  $00, $00, $00, $00, $00, $00, $08, $08, $F7, $00, $08, $00, $08, $00, $00, $00,  // char 64487
    $FB, $E8, {|}  $00, $00, $00, $00, $00, $08, $04, $04, $F8, $00, $00, $00, $00, $00, $00, $00,  // char 64488
    $FB, $E9, {|}  $00, $00, $00, $00, $00, $00, $08, $08, $F7, $00, $00, $00, $00, $00, $00, $00,  // char 64489
    $FB, $FC, {|}  $00, $00, $00, $00, $00, $07, $08, $88, $86, $81, $82, $7C, $00, $00, $00, $00,  // char 64508
    $FB, $FD, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $07, $88, $87, $81, $7E, $00, $00, $00,  // char 64509
    $FB, $FE, {|}  $00, $00, $00, $00, $00, $08, $04, $04, $F8, $00, $00, $14, $00, $00, $00, $00,  // char 64510
    $FB, $FF, {|}  $00, $00, $00, $00, $00, $00, $08, $08, $F7, $00, $00, $14, $00, $00, $00, $00,  // char 64511
    $FC, $5B, {|}  $20, $20, $20, $20, $08, $04, $02, $42, $3C, $00, $00, $00, $00, $00, $00, $00,  // char 64603
    $FC, $5C, {|}  $10, $10, $10, $10, $00, $04, $02, $02, $04, $18, $60, $00, $00, $00, $00, $00,  // char 64604
    $FC, $5D, {|}  $10, $10, $10, $10, $00, $07, $08, $88, $86, $81, $81, $7E, $00, $00, $00, $00,  // char 64605
    $FC, $5E, {|}  $18, $24, $98, $60, $C2, $12, $92, $9C, $60, $00, $00, $00, $00, $00, $00, $00,  // char 64606
    $FC, $5F, {|}  $02, $92, $9C, $60, $0E, $70, $0E, $70, $00, $00, $00, $00, $00, $00, $00, $00,  // char 64607
    $FC, $60, {|}  $18, $60, $04, $54, $58, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 64608
    $FC, $61, {|}  $10, $28, $10, $60, $04, $54, $58, $20, $00, $00, $00, $00, $00, $00, $00, $00,  // char 64609
    $FC, $62, {|}  $04, $54, $58, $20, $0C, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 64610
    $FC, $63, {|}  $10, $10, $04, $54, $58, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 64611
    $FC, $90, {|}  $10, $10, $10, $10, $00, $00, $00, $00, $07, $88, $87, $81, $7E, $00, $00, $00,  // char 64656
    $FC, $F2, {|}  $0E, $F0, $02, $92, $9C, $60, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00,  // char 64754
    $FC, $F3, {|}  $18, $24, $18, $60, $02, $92, $9C, $60, $FF, $00, $00, $00, $00, $00, $00, $00,  // char 64755
    $FC, $F4, {|}  $02, $12, $9C, $60, $0E, $70, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00,  // char 64756
    $FD, $3C, {|}  $10, $28, $50, $28, $08, $08, $08, $08, $07, $00, $00, $00, $00, $00, $00, $00,  // char 64828
    $FD, $3D, {|}  $08, $14, $28, $14, $04, $04, $04, $04, $04, $00, $00, $00, $00, $00, $00, $00,  // char 64829
    $FD, $3E, {|}  $00, $00, $04, $08, $10, $54, $38, $54, $10, $08, $04, $00, $00, $00, $00, $00,  // char 64830
    $FD, $3F, {|}  $00, $00, $20, $10, $08, $2A, $1C, $2A, $08, $10, $20, $00, $00, $00, $00, $00,  // char 64831
    $FD, $F2, {|}  $00, $2A, $14, $00, $21, $69, $A9, $69, $16, $00, $00, $00, $00, $00, $00, $00,  // char 65010
    $FE, $50, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $08, $10, $00, $00, $00, $00, $00,  // char 65104
    $FE, $51, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $10, $08, $00, $00, $00, $00, $00,  // char 65105
    $FE, $52, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $10, $00, $00, $00, $00, $00, $00,  // char 65106
    $FE, $54, {|}  $00, $00, $00, $00, $00, $00, $08, $00, $00, $08, $10, $00, $00, $00, $00, $00,  // char 65108
    $FE, $55, {|}  $00, $00, $00, $00, $00, $00, $10, $00, $00, $10, $00, $00, $00, $00, $00, $00,  // char 65109
    $FE, $56, {|}  $00, $00, $00, $00, $00, $18, $24, $08, $10, $00, $10, $00, $00, $00, $00, $00,  // char 65110
    $FE, $57, {|}  $00, $00, $00, $00, $00, $10, $10, $10, $10, $00, $10, $00, $00, $00, $00, $00,  // char 65111
    $FE, $58, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $3C, $00, $00, $00, $00, $00, $00, $00,  // char 65112
    $FE, $59, {|}  $00, $00, $00, $00, $00, $08, $10, $10, $10, $10, $10, $08, $00, $00, $00, $00,  // char 65113
    $FE, $5A, {|}  $00, $00, $00, $00, $00, $10, $08, $08, $08, $08, $08, $10, $00, $00, $00, $00,  // char 65114
    $FE, $5B, {|}  $00, $00, $00, $00, $00, $18, $10, $10, $20, $10, $10, $18, $00, $00, $00, $00,  // char 65115
    $FE, $5C, {|}  $00, $00, $00, $00, $00, $18, $08, $08, $04, $08, $08, $18, $00, $00, $00, $00,  // char 65116
    $FE, $5D, {|}  $00, $00, $00, $00, $00, $18, $10, $10, $10, $10, $10, $18, $00, $00, $00, $00,  // char 65117
    $FE, $5E, {|}  $00, $00, $00, $00, $00, $18, $08, $08, $08, $08, $08, $18, $00, $00, $00, $00,  // char 65118
    $FE, $5F, {|}  $00, $00, $00, $00, $00, $00, $28, $7C, $28, $7C, $28, $00, $00, $00, $00, $00,  // char 65119
    $FE, $60, {|}  $00, $00, $00, $00, $00, $10, $28, $28, $10, $2A, $24, $1A, $00, $00, $00, $00,  // char 65120
    $FE, $61, {|}  $00, $00, $00, $00, $00, $10, $10, $7C, $38, $38, $44, $00, $00, $00, $00, $00,  // char 65121
    $FE, $62, {|}  $00, $00, $00, $00, $00, $00, $10, $10, $7C, $10, $10, $00, $00, $00, $00, $00,  // char 65122
    $FE, $63, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $3C, $00, $00, $00, $00, $00, $00, $00,  // char 65123
    $FE, $64, {|}  $00, $00, $00, $00, $00, $00, $08, $10, $20, $10, $08, $00, $00, $00, $00, $00,  // char 65124
    $FE, $65, {|}  $00, $00, $00, $00, $00, $00, $20, $10, $08, $10, $20, $00, $00, $00, $00, $00,  // char 65125
    $FE, $66, {|}  $00, $00, $00, $00, $00, $00, $00, $3C, $00, $3C, $00, $00, $00, $00, $00, $00,  // char 65126
    $FE, $68, {|}  $00, $00, $00, $00, $00, $20, $10, $10, $08, $08, $04, $00, $00, $00, $00, $00,  // char 65128
    $FE, $69, {|}  $00, $00, $00, $00, $10, $38, $54, $30, $18, $54, $38, $10, $00, $00, $00, $00,  // char 65129
    $FE, $6A, {|}  $00, $00, $00, $00, $00, $24, $08, $08, $10, $10, $24, $00, $00, $00, $00, $00,  // char 65130
    $FE, $6B, {|}  $00, $00, $00, $00, $00, $38, $44, $54, $58, $40, $38, $00, $00, $00, $00, $00,  // char 65131
    $FE, $70, {|}  $0C, $30, $0C, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 65136
    $FE, $71, {|}  $06, $18, $60, $06, $18, $60, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00,  // char 65137
    $FE, $72, {|}  $0C, $12, $CA, $2C, $70, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 65138
    $FE, $74, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0C, $30, $0C, $30, $00, $00,  // char 65140
    $FE, $76, {|}  $0C, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 65142
    $FE, $77, {|}  $00, $06, $18, $60, $00, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00,  // char 65143
    $FE, $78, {|}  $08, $14, $0C, $08, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 65144
    $FE, $79, {|}  $18, $24, $14, $18, $60, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00,  // char 65145
    $FE, $7A, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0C, $30, $00, $00,  // char 65146
    $FE, $7B, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $06, $18, $60, $00, $00,  // char 65147
    $FE, $7C, {|}  $0A, $2A, $2C, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 65148
    $FE, $7D, {|}  $02, $12, $92, $9C, $60, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00,  // char 65149
    $FE, $7E, {|}  $18, $24, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  // char 65150
    $FE, $7F, {|}  $18, $24, $24, $18, $00, $00, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00,  // char 65151
    $FE, $80, {|}  $00, $00, $00, $00, $18, $24, $20, $1E, $20, $00, $00, $00, $00, $00, $00, $00,  // char 65152
    $FE, $81, {|}  $02, $3C, $40, $10, $10, $10, $10, $10, $10, $00, $00, $00, $00, $00, $00, $00,  // char 65153
    $FE, $82, {|}  $01, $3E, $40, $08, $08, $08, $08, $08, $07, $00, $00, $00, $00, $00, $00, $00,  // char 65154
    $FE, $83, {|}  $18, $20, $1C, $20, $08, $08, $08, $08, $08, $00, $00, $00, $00, $00, $00, $00,  // char 65155
    $FE, $84, {|}  $0C, $10, $0E, $10, $04, $04, $04, $04, $03, $00, $00, $00, $00, $00, $00, $00,  // char 65156
    $FE, $85, {|}  $00, $1C, $20, $1C, $20, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 65157
    $FE, $86, {|}  $00, $1C, $20, $1C, $20, $00, $0C, $12, $0F, $02, $04, $18, $60, $00, $00, $00,  // char 65158
    $FE, $87, {|}  $00, $00, $00, $04, $04, $04, $04, $04, $04, $00, $0C, $10, $0E, $10, $00, $00,  // char 65159
    $FE, $88, {|}  $00, $00, $00, $04, $04, $04, $04, $04, $03, $00, $0C, $10, $0E, $10, $00, $00,  // char 65160
    $FE, $89, {|}  $00, $30, $40, $38, $40, $07, $08, $88, $86, $81, $81, $7E, $00, $00, $00, $00,  // char 65161
    $FE, $8A, {|}  $00, $30, $40, $38, $40, $00, $00, $00, $07, $88, $87, $81, $7E, $00, $00, $00,  // char 65162
    $FE, $8B, {|}  $00, $1C, $20, $1C, $20, $08, $04, $04, $F8, $00, $00, $00, $00, $00, $00, $00,  // char 65163
    $FE, $8C, {|}  $00, $1C, $20, $1C, $20, $00, $08, $08, $F7, $00, $00, $00, $00, $00, $00, $00,  // char 65164
    $FE, $8D, {|}  $00, $00, $00, $08, $08, $08, $08, $08, $08, $00, $00, $00, $00, $00, $00, $00,  // char 65165
    $FE, $8E, {|}  $00, $00, $00, $08, $08, $08, $08, $08, $07, $00, $00, $00, $00, $00, $00, $00,  // char 65166
    $FE, $8F, {|}  $00, $00, $00, $00, $00, $40, $81, $81, $7E, $00, $00, $08, $00, $00, $00, $00,  // char 65167
    $FE, $90, {|}  $00, $00, $00, $00, $00, $40, $80, $81, $7E, $00, $00, $08, $00, $00, $00, $00,  // char 65168
    $FE, $91, {|}  $00, $00, $00, $00, $00, $08, $04, $04, $F8, $00, $00, $10, $00, $00, $00, $00,  // char 65169
    $FE, $92, {|}  $00, $00, $00, $00, $00, $00, $08, $08, $F7, $00, $00, $08, $00, $00, $00, $00,  // char 65170
    $FE, $93, {|}  $00, $28, $00, $00, $18, $24, $22, $22, $1C, $00, $00, $00, $00, $00, $00, $00,  // char 65171
    $FE, $94, {|}  $00, $50, $00, $10, $10, $68, $88, $74, $03, $00, $00, $00, $00, $00, $00, $00,  // char 65172
    $FE, $95, {|}  $00, $00, $00, $00, $14, $40, $81, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 65173
    $FE, $96, {|}  $00, $00, $00, $14, $00, $40, $80, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 65174
    $FE, $97, {|}  $00, $00, $00, $14, $00, $08, $04, $04, $F8, $00, $00, $00, $00, $00, $00, $00,  // char 65175
    $FE, $98, {|}  $00, $00, $00, $14, $00, $00, $08, $08, $F7, $00, $00, $00, $00, $00, $00, $00,  // char 65176
    $FE, $99, {|}  $00, $08, $00, $14, $00, $40, $81, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 65177
    $FE, $9A, {|}  $00, $08, $00, $14, $00, $40, $80, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 65178
    $FE, $9B, {|}  $00, $08, $00, $14, $00, $08, $04, $04, $F8, $00, $00, $00, $00, $00, $00, $00,  // char 65179
    $FE, $9C, {|}  $00, $08, $00, $14, $00, $00, $08, $08, $F7, $00, $00, $00, $00, $00, $00, $00,  // char 65180
    $FE, $9D, {|}  $00, $00, $00, $00, $70, $8F, $30, $40, $80, $88, $80, $41, $3E, $00, $00, $00,  // char 65181
    $FE, $9E, {|}  $00, $00, $00, $00, $00, $70, $8F, $32, $41, $80, $88, $80, $40, $3E, $00, $00,  // char 65182
    $FE, $9F, {|}  $00, $00, $00, $00, $00, $38, $47, $18, $E0, $00, $00, $10, $00, $00, $00, $00,  // char 65183
    $FE, $A0, {|}  $00, $00, $00, $00, $00, $70, $8F, $32, $C1, $00, $00, $08, $00, $00, $00, $00,  // char 65184
    $FE, $A1, {|}  $00, $00, $00, $00, $70, $8F, $30, $40, $80, $80, $80, $41, $3E, $00, $00, $00,  // char 65185
    $FE, $A2, {|}  $00, $00, $00, $00, $00, $70, $8F, $32, $41, $80, $80, $80, $40, $3E, $00, $00,  // char 65186
    $FE, $A3, {|}  $00, $00, $00, $00, $00, $38, $47, $18, $E0, $00, $00, $00, $00, $00, $00, $00,  // char 65187
    $FE, $A4, {|}  $00, $00, $00, $00, $00, $70, $8F, $32, $C1, $00, $00, $00, $00, $00, $00, $00,  // char 65188
    $FE, $A5, {|}  $00, $20, $00, $00, $70, $8F, $30, $40, $80, $80, $80, $41, $3E, $00, $00, $00,  // char 65189
    $FE, $A6, {|}  $00, $00, $20, $00, $00, $70, $8F, $32, $41, $80, $80, $80, $40, $3E, $00, $00,  // char 65190
    $FE, $A7, {|}  $00, $00, $00, $10, $00, $38, $47, $18, $E0, $00, $00, $00, $00, $00, $00, $00,  // char 65191
    $FE, $A8, {|}  $00, $00, $10, $00, $00, $70, $8F, $32, $C1, $00, $00, $00, $00, $00, $00, $00,  // char 65192
    $FE, $A9, {|}  $00, $00, $00, $00, $08, $04, $02, $42, $3C, $00, $00, $00, $00, $00, $00, $00,  // char 65193
    $FE, $AA, {|}  $00, $00, $00, $00, $08, $08, $04, $46, $39, $00, $00, $00, $00, $00, $00, $00,  // char 65194
    $FE, $AB, {|}  $00, $00, $10, $00, $08, $04, $02, $42, $3C, $00, $00, $00, $00, $00, $00, $00,  // char 65195
    $FE, $AC, {|}  $00, $00, $10, $00, $08, $08, $04, $46, $39, $00, $00, $00, $00, $00, $00, $00,  // char 65196
    $FE, $AD, {|}  $00, $00, $00, $00, $00, $04, $02, $02, $04, $18, $60, $00, $00, $00, $00, $00,  // char 65197
    $FE, $AE, {|}  $00, $00, $00, $00, $00, $00, $00, $04, $07, $04, $18, $60, $00, $00, $00, $00,  // char 65198
    $FE, $AF, {|}  $00, $00, $08, $00, $00, $04, $02, $02, $04, $18, $60, $00, $00, $00, $00, $00,  // char 65199
    $FE, $B0, {|}  $00, $00, $00, $00, $08, $00, $00, $04, $07, $04, $18, $60, $00, $00, $00, $00,  // char 65200
    $FE, $B1, {|}  $00, $00, $00, $00, $00, $01, $15, $88, $84, $84, $78, $00, $00, $00, $00, $00,  // char 65201
    $FE, $B2, {|}  $00, $00, $00, $00, $00, $00, $01, $15, $88, $84, $84, $78, $00, $00, $00, $00,  // char 65202
    $FE, $B3, {|}  $00, $00, $00, $00, $00, $00, $01, $15, $EA, $00, $00, $00, $00, $00, $00, $00,  // char 65203
    $FE, $B4, {|}  $00, $00, $00, $00, $00, $00, $00, $2A, $D5, $00, $00, $00, $00, $00, $00, $00,  // char 65204
    $FE, $B5, {|}  $00, $08, $00, $14, $00, $01, $15, $88, $84, $84, $78, $00, $00, $00, $00, $00,  // char 65205
    $FE, $B6, {|}  $00, $00, $08, $00, $14, $00, $01, $15, $88, $84, $84, $78, $00, $00, $00, $00,  // char 65206
    $FE, $B7, {|}  $00, $04, $00, $0A, $00, $00, $01, $15, $EA, $00, $00, $00, $00, $00, $00, $00,  // char 65207
    $FE, $B8, {|}  $00, $08, $00, $14, $00, $00, $00, $2A, $D5, $00, $00, $00, $00, $00, $00, $00,  // char 65208
    $FE, $B9, {|}  $00, $00, $00, $00, $06, $09, $31, $9E, $88, $88, $70, $00, $00, $00, $00, $00,  // char 65209
    $FE, $BA, {|}  $00, $00, $00, $00, $00, $06, $09, $31, $9E, $88, $88, $70, $00, $00, $00, $00,  // char 65210
    $FE, $BB, {|}  $00, $00, $00, $00, $00, $06, $29, $31, $DE, $00, $00, $00, $00, $00, $00, $00,  // char 65211
    $FE, $BC, {|}  $00, $00, $00, $00, $00, $06, $29, $31, $DE, $00, $00, $00, $00, $00, $00, $00,  // char 65212
    $FE, $BD, {|}  $00, $00, $04, $00, $06, $09, $31, $9E, $88, $88, $70, $00, $00, $00, $00, $00,  // char 65213
    $FE, $BE, {|}  $00, $00, $00, $04, $00, $06, $09, $31, $9E, $88, $88, $70, $00, $00, $00, $00,  // char 65214
    $FE, $BF, {|}  $00, $00, $04, $00, $00, $06, $29, $31, $DE, $00, $00, $00, $00, $00, $00, $00,  // char 65215
    $FE, $C0, {|}  $00, $00, $04, $00, $00, $06, $29, $31, $DE, $00, $00, $00, $00, $00, $00, $00,  // char 65216
    $FE, $C1, {|}  $00, $00, $20, $20, $20, $2C, $32, $A2, $7C, $00, $00, $00, $00, $00, $00, $00,  // char 65217
    $FE, $C2, {|}  $00, $00, $20, $20, $20, $2C, $32, $A2, $7D, $00, $00, $00, $00, $00, $00, $00,  // char 65218
    $FE, $C3, {|}  $00, $00, $20, $20, $20, $2C, $32, $22, $FC, $00, $00, $00, $00, $00, $00, $00,  // char 65219
    $FE, $C4, {|}  $00, $00, $20, $20, $20, $2C, $32, $22, $FD, $00, $00, $00, $00, $00, $00, $00,  // char 65220
    $FE, $C5, {|}  $00, $00, $24, $20, $20, $2C, $32, $A2, $7C, $00, $00, $00, $00, $00, $00, $00,  // char 65221
    $FE, $C6, {|}  $00, $00, $24, $20, $20, $2C, $32, $A2, $7D, $00, $00, $00, $00, $00, $00, $00,  // char 65222
    $FE, $C7, {|}  $00, $00, $24, $20, $20, $2C, $32, $22, $FC, $00, $00, $00, $00, $00, $00, $00,  // char 65223
    $FE, $C8, {|}  $00, $00, $24, $20, $20, $2C, $32, $22, $FD, $00, $00, $00, $00, $00, $00, $00,  // char 65224
    $FE, $C9, {|}  $00, $00, $00, $00, $70, $80, $8C, $70, $40, $80, $80, $80, $41, $3E, $00, $00,  // char 65225
    $FE, $CA, {|}  $00, $00, $00, $00, $1E, $22, $1C, $22, $41, $80, $80, $80, $41, $3E, $00, $00,  // char 65226
    $FE, $CB, {|}  $00, $00, $00, $00, $00, $0E, $10, $10, $FE, $00, $00, $00, $00, $00, $00, $00,  // char 65227
    $FE, $CC, {|}  $00, $00, $00, $00, $00, $1E, $22, $1C, $E3, $00, $00, $00, $00, $00, $00, $00,  // char 65228
    $FE, $CD, {|}  $00, $40, $00, $00, $70, $80, $8C, $70, $40, $80, $80, $80, $41, $3E, $00, $00,  // char 65229
    $FE, $CE, {|}  $00, $08, $00, $00, $1E, $22, $1C, $22, $41, $80, $80, $80, $41, $3E, $00, $00,  // char 65230
    $FE, $CF, {|}  $00, $00, $10, $00, $00, $0E, $10, $10, $FE, $00, $00, $00, $00, $00, $00, $00,  // char 65231
    $FE, $D0, {|}  $00, $00, $08, $00, $00, $1E, $22, $1C, $E3, $00, $00, $00, $00, $00, $00, $00,  // char 65232
    $FE, $D1, {|}  $00, $00, $04, $00, $06, $49, $85, $83, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 65233
    $FE, $D2, {|}  $00, $00, $04, $00, $06, $49, $89, $86, $7D, $00, $00, $00, $00, $00, $00, $00,  // char 65234
    $FE, $D3, {|}  $00, $00, $08, $00, $0C, $12, $0A, $06, $FC, $00, $00, $00, $00, $00, $00, $00,  // char 65235
    $FE, $D4, {|}  $00, $00, $08, $00, $0C, $12, $12, $0C, $F3, $00, $00, $00, $00, $00, $00, $00,  // char 65236
    $FE, $D5, {|}  $00, $00, $0A, $00, $06, $09, $09, $47, $81, $81, $81, $42, $3C, $00, $00, $00,  // char 65237
    $FE, $D6, {|}  $00, $00, $0A, $00, $00, $06, $09, $49, $87, $81, $81, $42, $3C, $00, $00, $00,  // char 65238
    $FE, $D7, {|}  $00, $14, $00, $00, $0C, $12, $0A, $06, $FC, $00, $00, $00, $00, $00, $00, $00,  // char 65239
    $FE, $D8, {|}  $00, $14, $00, $00, $0C, $12, $12, $0C, $F3, $00, $00, $00, $00, $00, $00, $00,  // char 65240
    $FE, $D9, {|}  $00, $00, $19, $21, $19, $21, $01, $81, $7E, $00, $00, $00, $00, $00, $00, $00,  // char 65241
    $FE, $DA, {|}  $00, $00, $32, $42, $32, $42, $02, $86, $79, $00, $00, $00, $00, $00, $00, $00,  // char 65242
    $FE, $DB, {|}  $00, $00, $06, $18, $20, $10, $08, $08, $F0, $00, $00, $00, $00, $00, $00, $00,  // char 65243
    $FE, $DC, {|}  $00, $00, $06, $18, $20, $20, $10, $08, $F7, $00, $00, $00, $00, $00, $00, $00,  // char 65244
    $FE, $DD, {|}  $00, $02, $02, $02, $02, $02, $42, $82, $82, $84, $78, $00, $00, $00, $00, $00,  // char 65245
    $FE, $DE, {|}  $00, $00, $00, $02, $02, $02, $02, $02, $03, $42, $82, $82, $84, $78, $00, $00,  // char 65246
    $FE, $DF, {|}  $00, $00, $08, $08, $08, $08, $08, $08, $F0, $00, $00, $00, $00, $00, $00, $00,  // char 65247
    $FE, $E0, {|}  $00, $00, $08, $08, $08, $08, $08, $08, $F7, $00, $00, $00, $00, $00, $00, $00,  // char 65248
    $FE, $E1, {|}  $00, $00, $00, $00, $0C, $12, $12, $3C, $40, $40, $40, $40, $40, $40, $00, $00,  // char 65249
    $FE, $E2, {|}  $00, $00, $00, $00, $00, $00, $08, $3C, $4B, $4A, $44, $40, $40, $40, $00, $00,  // char 65250
    $FE, $E3, {|}  $00, $00, $00, $00, $00, $0C, $12, $32, $CC, $00, $00, $00, $00, $00, $00, $00,  // char 65251
    $FE, $E4, {|}  $00, $00, $00, $00, $00, $0C, $12, $32, $CD, $00, $00, $00, $00, $00, $00, $00,  // char 65252
    $FE, $E5, {|}  $00, $00, $08, $00, $00, $41, $81, $81, $81, $42, $3C, $00, $00, $00, $00, $00,  // char 65253
    $FE, $E6, {|}  $00, $00, $00, $00, $00, $00, $08, $01, $41, $81, $81, $42, $3C, $00, $00, $00,  // char 65254
    $FE, $E7, {|}  $00, $00, $00, $10, $00, $08, $04, $04, $F8, $00, $00, $00, $00, $00, $00, $00,  // char 65255
    $FE, $E8, {|}  $00, $00, $00, $08, $00, $00, $08, $08, $F7, $00, $00, $00, $00, $00, $00, $00,  // char 65256
    $FE, $E9, {|}  $00, $00, $00, $00, $18, $24, $22, $22, $1C, $00, $00, $00, $00, $00, $00, $00,  // char 65257
    $FE, $EA, {|}  $00, $00, $00, $10, $10, $68, $88, $74, $03, $00, $00, $00, $00, $00, $00, $00,  // char 65258
    $FE, $EB, {|}  $00, $00, $00, $40, $30, $4E, $49, $39, $E6, $00, $00, $00, $00, $00, $00, $00,  // char 65259
    $FE, $EC, {|}  $00, $00, $00, $00, $08, $14, $24, $28, $F3, $24, $18, $0C, $00, $00, $00, $00,  // char 65260
    $FE, $ED, {|}  $00, $00, $00, $00, $00, $0C, $12, $0E, $02, $04, $18, $60, $00, $00, $00, $00,  // char 65261
    $FE, $EE, {|}  $00, $00, $00, $00, $00, $00, $0C, $12, $0F, $02, $04, $18, $60, $00, $00, $00,  // char 65262
    $FE, $EF, {|}  $00, $00, $00, $00, $00, $07, $08, $88, $86, $81, $81, $7E, $00, $00, $00, $00,  // char 65263
    $FE, $F0, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $07, $88, $87, $81, $7E, $00, $00, $00,  // char 65264
    $FE, $F1, {|}  $00, $00, $00, $00, $00, $07, $08, $88, $86, $81, $81, $7E, $00, $00, $24, $00,  // char 65265
    $FE, $F2, {|}  $00, $00, $00, $00, $00, $00, $00, $00, $07, $88, $87, $81, $7E, $00, $24, $00,  // char 65266
    $FE, $F3, {|}  $00, $00, $00, $00, $00, $08, $04, $04, $F8, $00, $00, $14, $00, $00, $00, $00,  // char 65267
    $FE, $F4, {|}  $00, $00, $00, $00, $00, $00, $08, $08, $F7, $00, $00, $14, $00, $00, $00, $00,  // char 65268
    $FE, $F5, {|}  $68, $90, $02, $22, $12, $0A, $04, $0C, $12, $3C, $00, $00, $00, $00, $00, $00,  // char 65269
    $FE, $F6, {|}  $68, $90, $02, $32, $12, $0A, $0A, $0E, $3D, $00, $00, $00, $00, $00, $00, $00,  // char 65270
    $FE, $F7, {|}  $60, $80, $72, $A2, $12, $0A, $04, $0C, $12, $3C, $00, $00, $00, $00, $00, $00,  // char 65271
    $FE, $F8, {|}  $30, $40, $3A, $52, $12, $0A, $0A, $0E, $3D, $00, $00, $00, $00, $00, $00, $00,  // char 65272
    $FE, $F9, {|}  $00, $00, $42, $22, $12, $0A, $04, $0C, $12, $3C, $00, $18, $20, $1C, $20, $00,  // char 65273
    $FE, $FA, {|}  $00, $00, $22, $12, $12, $0A, $0A, $0E, $3D, $00, $00, $18, $20, $1C, $20, $00,  // char 65274
    $FE, $FB, {|}  $00, $00, $42, $22, $12, $0A, $04, $0C, $12, $3C, $00, $00, $00, $00, $00, $00,  // char 65275
    $FE, $FC, {|}  $00, $00, $22, $12, $12, $0A, $0A, $0E, $3D, $00, $00, $00, $00, $00, $00, $00,  // char 65276
    $FE, $FF, {|}  $F1, $35, $55, $8A, $E0, $06, $95, $D6, $B5, $97, $00, $EE, $8A, $EE, $28, $E8,  // char 65279
    $FF, $FD, {|}  $00, $38, $7C, $7C, $C6, $92, $F2, $E6, $FE, $E6, $7C, $7C, $38, $00, $00, $00   // char 65533
  );
  UVGA16Mirrors : packed array [0..(167*3)-1] of Uint16 = (
  //nrml horz vert
        40,  41,  40,
        41,  40,  41,
        47,  92,  92,
        92,  47,  47,
        60,  62,  60,
        62,  60,  62,
        91,  93,  91,
        93,  91,  93,
       123, 125, 123,
       125, 123, 125,
      9484,9488,9492,
      9485,9489,9493,
      9486,9490,9494,
      9487,9491,9495,
      9488,9494,9496,
      9489,9495,9497,
      9490,9496,9498,
      9491,9497,9499,
      9492,9496,9484,
      9493,9497,9485,
      9494,9498,9486,
      9495,9499,9487,
      9496,9492,9488,
      9497,9493,9489,
      9498,9494,9490,
      9499,9495,9491,
      9500,9508,9500,
      9501,9509,9501,
      9502,9510,9503,
      9503,9511,9502,
      9504,9512,9504,
      9505,9513,9506,
      9506,9514,9505,
      9507,9515,9507,
      9508,9500,9508,
      9509,9501,9509,
      9510,9502,9511,
      9511,9503,9510,
      9512,9504,9512,
      9513,9505,9514,
      9514,9506,9513,
      9515,9507,9515,
      9516,9516,9524,
      9517,9518,9525,
      9518,9517,9526,
      9519,9519,9527,
      9520,9520,9528,
      9521,9522,9529,
      9522,9521,9530,
      9523,9523,9531,
      9524,9524,9516,
      9525,9526,9517,
      9526,9525,9518,
      9527,9527,9519,
      9528,9528,9520,
      9529,9530,9521,
      9530,9529,9522,
      9531,9531,9523,
      9533,9534,9533,
      9534,9533,9534,
      9536,9536,9537,
      9537,9537,9536,
      9539,9540,9541,
      9540,9539,9542,
      9541,9542,9540,
      9542,9541,9539,
      9543,9543,9544,
      9544,9544,9543,
      9545,9546,9545,
      9546,9545,9546,
      9554,9557,9560,
      9555,9558,9561,
      9556,9559,9562,
      9557,9554,9563,
      9558,9555,9564,
      9559,9556,9565,
      9560,9563,9554,
      9561,9564,9555,
      9562,9565,9556,
      9563,9560,9557,
      9564,9561,9558,
      9565,9562,9559,
      9566,9569,9566,
      9567,9570,9567,
      9568,9571,9568,
      9569,9566,9569,
      9570,9567,9570,
      9571,9568,9571,
      9572,9572,9575,
      9573,9573,9576,
      9574,9574,9577,
      9575,9575,9572,
      9576,9576,9573,
      9577,9577,9574,
      9581,9582,9584,
      9582,9581,9583,
      9583,9584,9582,
      9584,9583,9581,
      9585,9586,9586,
      9586,9585,9585,
      9588,9590,9588,
      9589,9589,9591,
      9590,9588,9590,
      9591,9591,9589,
      9592,9594,9592,
      9593,9593,9595,
      9594,9592,9594,
      9595,9595,9593,
      9596,9598,9596,
      9597,9597,9599,
      9598,9596,9598,
      9599,9599,9597,
      9601,9602,9620,
      9600,9600,9604,
      9604,9604,9600,
      9612,9616,9612,
      9615,9621,9615,
      9616,9612,9616,
      9621,9615,9621,
      9622,9623,9624,
      9623,9622,9629,
      9624,9629,9622,
      9625,9631,9627,
      9626,9630,9630,
      9627,9628,9625,
      9628,9627,9631,
      9629,9624,9623,
      9630,9626,9626,
      9631,9625,9628,
      9650,9650,9660,
      9651,9651,9661,
      9652,9652,9662,
      9653,9653,9663,
      9654,9664,9654,
      9655,9665,9655,
      9556,9666,9656,
      9557,9667,9657,
      9658,9668,9658,
      9659,9669,9659,
      9660,9660,9650,
      9661,9661,9651,
      9662,9662,9652,
      9663,9663,9653,
      9664,9654,9664,
      9665,9655,9665,
      9666,9656,9666,
      9667,9657,9667,
      9668,9658,9668,
      9669,9659,9669,
      9680,9681,9680,
      9681,9680,9681,
      9682,9682,9683,
      9683,9683,9682,
      9686,9687,9686,
      9687,9686,9687,
      9690,9690,9691,
      9691,9691,9690,
      9692,9693,9695,
      9693,9692,9694,
      9694,9695,9693,
      9695,9594,9692,
      9696,9696,9697,
      9697,9697,9696,
      9698,9699,9701,
      9699,9698,9700,
      9700,9701,9699,
      9701,9700,9698 );

  // MicroKnightPlus
  MicroKnightPlus_COUNT = 256;  // number of glyphs
  MicroKnightPlus : packed array [0..MicroKnightPlus_COUNT * 18 - 1] of byte = (
    $00, $00, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $01, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $02, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $03, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $04, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $05, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $06, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $07, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $08, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $09, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $0A, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $0B, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $0C, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $0D, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $0E, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $0F, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $10, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $11, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $12, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $13, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $14, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $15, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $16, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $17, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $18, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $19, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $1A, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $1B, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $1C, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $1D, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $1E, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $1F, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $20, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $21, {|} $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00, $18, $18, $00, $00,
    $00, $22, {|} $6C, $6C, $6C, $6C, $24, $24, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $23, {|} $6C, $6C, $6C, $6C, $FE, $FE, $6C, $6C, $FE, $FE, $6C, $6C, $6C, $6C, $00, $00,
    $00, $24, {|} $10, $10, $7C, $7C, $D0, $D0, $7C, $7C, $16, $16, $16, $16, $7C, $7C, $10, $10,
    $00, $25, {|} $60, $60, $96, $96, $6C, $6C, $18, $18, $30, $30, $6C, $6C, $D2, $D2, $0C, $0C,
    $00, $26, {|} $70, $70, $D8, $D8, $70, $70, $F6, $F6, $DC, $DC, $D8, $D8, $7C, $7C, $06, $06,
    $00, $27, {|} $18, $18, $18, $18, $30, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $28, {|} $03, $07, $0E, $0C, $18, $18, $18, $18, $18, $18, $0C, $0E, $07, $03, $00, $00,
    $00, $29, {|} $C0, $E0, $70, $30, $18, $18, $18, $18, $18, $18, $30, $70, $E0, $C0, $00, $00,
    $00, $2A, {|} $00, $00, $6C, $6C, $38, $38, $FE, $FE, $38, $38, $6C, $6C, $00, $00, $00, $00,
    $00, $2B, {|} $00, $00, $18, $18, $18, $18, $7E, $7E, $18, $18, $18, $18, $00, $00, $00, $00,
    $00, $2C, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $18, $30, $30,
    $00, $2D, {|} $00, $00, $00, $00, $00, $00, $FF, $FF, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $2E, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $18, $00, $00,
    $00, $2F, {|} $03, $03, $06, $06, $0C, $0C, $18, $18, $30, $30, $60, $60, $C0, $C0, $80, $80,
    $00, $30, {|} $00, $00, $78, $78, $CC, $CC, $DE, $DE, $F6, $F6, $E6, $E6, $7C, $7C, $00, $00,
    $00, $31, {|} $18, $18, $18, $18, $38, $38, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $32, {|} $7C, $7C, $06, $06, $3C, $3C, $60, $60, $C0, $C0, $C0, $C0, $FE, $FE, $00, $00,
    $00, $33, {|} $3C, $3C, $06, $06, $1C, $1C, $06, $06, $46, $46, $C6, $C6, $7C, $7C, $00, $00,
    $00, $34, {|} $18, $18, $18, $18, $30, $30, $6C, $6C, $CC, $CC, $FE, $FE, $0C, $0C, $00, $00,
    $00, $35, {|} $F8, $F8, $C0, $C0, $FC, $FC, $06, $06, $46, $46, $CC, $CC, $78, $78, $00, $00,
    $00, $36, {|} $70, $70, $C0, $C0, $FC, $FC, $C6, $C6, $C6, $C6, $CC, $CC, $78, $78, $00, $00,
    $00, $37, {|} $FE, $FE, $06, $06, $0C, $0C, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $38, {|} $78, $78, $CC, $CC, $7C, $7C, $C6, $C6, $C6, $C6, $CC, $CC, $78, $78, $00, $00,
    $00, $39, {|} $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7E, $7E, $06, $06, $1C, $1C, $00, $00,
    $00, $3A, {|} $00, $00, $00, $00, $18, $18, $18, $18, $00, $00, $18, $18, $18, $18, $00, $00,
    $00, $3B, {|} $00, $00, $00, $00, $18, $18, $18, $18, $00, $00, $18, $18, $18, $18, $30, $30,
    $00, $3C, {|} $03, $03, $06, $06, $0C, $0C, $18, $18, $0C, $0C, $06, $06, $03, $03, $00, $00,
    $00, $3D, {|} $00, $00, $00, $00, $7E, $7E, $00, $00, $00, $00, $7E, $7E, $00, $00, $00, $00,
    $00, $3E, {|} $C0, $C0, $60, $60, $30, $30, $18, $18, $30, $30, $60, $60, $C0, $C0, $00, $00,
    $00, $3F, {|} $7C, $7C, $C6, $C6, $06, $06, $3C, $3C, $30, $30, $00, $00, $30, $30, $00, $00,
    $00, $40, {|} $7C, $7C, $C6, $C6, $DE, $DE, $D6, $D6, $DE, $DE, $C0, $C0, $7E, $7E, $00, $00,
    $00, $41, {|} $78, $78, $CC, $CC, $C6, $C6, $FE, $FE, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $42, {|} $F8, $F8, $CC, $CC, $FC, $FC, $C6, $C6, $C6, $C6, $CC, $CC, $F8, $F8, $00, $00,
    $00, $43, {|} $78, $78, $CC, $CC, $C0, $C0, $C0, $C0, $C0, $C0, $C6, $C6, $7C, $7C, $00, $00,
    $00, $44, {|} $F8, $F8, $CC, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $FC, $FC, $00, $00,
    $00, $45, {|} $FE, $FE, $C0, $C0, $FC, $FC, $C0, $C0, $C0, $C0, $C0, $C0, $FE, $FE, $00, $00,
    $00, $46, {|} $FE, $FE, $C0, $C0, $FC, $FC, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $00, $00,
    $00, $47, {|} $38, $38, $60, $60, $C0, $C0, $CE, $CE, $C6, $C6, $C6, $C6, $7E, $7E, $06, $06,
    $00, $48, {|} $C6, $C6, $C6, $C6, $C6, $C6, $FE, $FE, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $49, {|} $7E, $7E, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $4A, {|} $0E, $0E, $06, $06, $06, $06, $06, $06, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $4B, {|} $C6, $C6, $CC, $CC, $D8, $D8, $F0, $F0, $D8, $D8, $CC, $CC, $C6, $C6, $00, $00,
    $00, $4C, {|} $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $FE, $FE, $00, $00,
    $00, $4D, {|} $C6, $C6, $EE, $EE, $FE, $FE, $D6, $D6, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $4E, {|} $C6, $C6, $E6, $E6, $F6, $F6, $DE, $DE, $CE, $CE, $C6, $C6, $C6, $C6, $00, $00,
    $00, $4F, {|} $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $50, {|} $F8, $F8, $CC, $CC, $C6, $C6, $C6, $C6, $FC, $FC, $C0, $C0, $C0, $C0, $00, $00,
    $00, $51, {|} $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $D6, $D6, $7C, $7C, $0C, $0C,
    $00, $52, {|} $F8, $F8, $CC, $CC, $C6, $C6, $C6, $C6, $FC, $FC, $D8, $D8, $CC, $CC, $06, $06,
    $00, $53, {|} $78, $78, $C0, $C0, $7C, $7C, $06, $06, $46, $46, $C6, $C6, $7C, $7C, $00, $00,
    $00, $54, {|} $7E, $7E, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $55, {|} $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $56, {|} $C6, $C6, $C6, $C6, $C6, $C6, $6C, $6C, $6C, $6C, $38, $38, $38, $38, $00, $00,
    $00, $57, {|} $C6, $C6, $C6, $C6, $C6, $C6, $D6, $D6, $FE, $FE, $EE, $EE, $C6, $C6, $00, $00,
    $00, $58, {|} $C6, $C6, $6C, $6C, $38, $38, $38, $38, $6C, $6C, $C6, $C6, $C6, $C6, $00, $00,
    $00, $59, {|} $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $0C, $0C, $0C, $0C, $0C, $0C, $00, $00,
    $00, $5A, {|} $FE, $FE, $0C, $0C, $18, $18, $30, $30, $60, $60, $C0, $C0, $FE, $FE, $00, $00,
    $00, $5B, {|} $3C, $3F, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $3F, $3C, $00, $00,
    $00, $5C, {|} $C0, $C0, $60, $60, $30, $30, $18, $18, $0C, $0C, $06, $06, $03, $03, $01, $01,
    $00, $5D, {|} $3C, $FC, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $FC, $3C, $00, $00,
    $00, $5E, {|} $10, $10, $38, $38, $6C, $6C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $5F, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $FF,
    $00, $60, {|} $18, $18, $18, $18, $0C, $0C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $61, {|} $00, $00, $3C, $3C, $06, $06, $7E, $7E, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $62, {|} $C0, $C0, $F8, $F8, $CC, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $FC, $FC, $00, $00,
    $00, $63, {|} $00, $00, $78, $78, $CC, $CC, $C0, $C0, $C0, $C0, $C6, $C6, $7C, $7C, $00, $00,
    $00, $64, {|} $06, $06, $3E, $3E, $66, $66, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $65, {|} $00, $00, $78, $78, $CC, $CC, $FC, $FC, $C0, $C0, $C6, $C6, $7C, $7C, $00, $00,
    $00, $66, {|} $38, $38, $6C, $6C, $60, $60, $78, $78, $60, $60, $60, $60, $60, $60, $60, $60,
    $00, $67, {|} $00, $00, $7E, $7E, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $06, $06, $7C, $7C,
    $00, $68, {|} $C0, $C0, $F8, $F8, $CC, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $69, {|} $18, $18, $00, $00, $38, $38, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $6A, {|} $0C, $0C, $00, $00, $1C, $1C, $0C, $0C, $0C, $0C, $0C, $0C, $4C, $4C, $38, $38,
    $00, $6B, {|} $C0, $C0, $CC, $CC, $D8, $D8, $F0, $F0, $D8, $D8, $CC, $CC, $C6, $C6, $00, $00,
    $00, $6C, {|} $38, $38, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $6D, {|} $00, $00, $C4, $C4, $EE, $EE, $FE, $FE, $D6, $D6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $6E, {|} $00, $00, $F8, $F8, $CC, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $6F, {|} $00, $00, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $70, {|} $00, $00, $F8, $F8, $CC, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $FC, $FC, $C0, $C0,
    $00, $71, {|} $00, $00, $3E, $3E, $66, $66, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $06, $06,
    $00, $72, {|} $00, $00, $FC, $FC, $C6, $C6, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $00, $00,
    $00, $73, {|} $00, $00, $78, $78, $C0, $C0, $7C, $7C, $06, $06, $C6, $C6, $7C, $7C, $00, $00,
    $00, $74, {|} $30, $30, $7C, $7C, $30, $30, $30, $30, $30, $30, $32, $32, $1C, $1C, $00, $00,
    $00, $75, {|} $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $76, {|} $00, $00, $C6, $C6, $C6, $C6, $6C, $6C, $6C, $6C, $38, $38, $38, $38, $00, $00,
    $00, $77, {|} $00, $00, $C6, $C6, $D6, $D6, $FE, $FE, $7C, $7C, $6C, $6C, $44, $44, $00, $00,
    $00, $78, {|} $00, $00, $C6, $C6, $6C, $6C, $38, $38, $38, $38, $6C, $6C, $C6, $C6, $00, $00,
    $00, $79, {|} $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $06, $06, $7C, $7C,
    $00, $7A, {|} $00, $00, $FE, $FE, $0C, $0C, $18, $18, $30, $30, $60, $60, $FE, $FE, $00, $00,
    $00, $7B, {|} $0E, $0F, $19, $18, $18, $18, $70, $70, $18, $18, $18, $19, $0F, $0E, $00, $00,
    $00, $7C, {|} $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18,
    $00, $7D, {|} $70, $F0, $98, $18, $18, $18, $0E, $0E, $18, $18, $18, $98, $F0, $70, $00, $00,
    $00, $7E, {|} $72, $72, $9C, $9C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $7F, {|} $38, $38, $70, $70, $E0, $E0, $C1, $C1, $83, $83, $07, $07, $0E, $0E, $1C, $1C,
    $00, $80, {|} $CF, $CF, $F7, $F7, $87, $87, $33, $33, $01, $01, $39, $39, $39, $39, $FF, $FF,
    $00, $81, {|} $E7, $E7, $DF, $DF, $87, $87, $33, $33, $01, $01, $39, $39, $39, $39, $FF, $FF,
    $00, $82, {|} $CF, $CF, $33, $33, $87, $87, $33, $33, $01, $01, $39, $39, $39, $39, $FF, $FF,
    $00, $83, {|} $8D, $8D, $63, $63, $87, $87, $33, $33, $01, $01, $39, $39, $39, $39, $FF, $FF,
    $00, $84, {|} $93, $93, $FF, $FF, $87, $87, $33, $33, $01, $01, $39, $39, $39, $39, $FF, $FF,
    $00, $85, {|} $87, $87, $33, $33, $87, $87, $33, $33, $01, $01, $39, $39, $39, $39, $FF, $FF,
    $00, $86, {|} $C0, $C0, $93, $93, $30, $30, $03, $03, $33, $33, $33, $33, $30, $30, $FF, $FF,
    $00, $87, {|} $87, $87, $33, $33, $3F, $3F, $3F, $3F, $3F, $3F, $33, $33, $87, $87, $CF, $CF,
    $00, $88, {|} $CF, $CF, $F7, $F7, $01, $01, $3F, $3F, $03, $03, $3F, $3F, $01, $01, $FF, $FF,
    $00, $89, {|} $E7, $E7, $DF, $DF, $01, $01, $3F, $3F, $03, $03, $3F, $3F, $01, $01, $FF, $FF,
    $00, $8A, {|} $E7, $E7, $99, $99, $01, $01, $3F, $3F, $03, $03, $3F, $3F, $01, $01, $FF, $FF,
    $00, $8B, {|} $93, $93, $FF, $FF, $01, $01, $3F, $3F, $03, $03, $3F, $3F, $01, $01, $FF, $FF,
    $00, $8C, {|} $CF, $CF, $F7, $F7, $81, $81, $E7, $E7, $E7, $E7, $E7, $E7, $81, $81, $FF, $FF,
    $00, $8D, {|} $E7, $E7, $DF, $DF, $81, $81, $E7, $E7, $E7, $E7, $E7, $E7, $81, $81, $FF, $FF,
    $00, $8E, {|} $E7, $E7, $99, $99, $81, $81, $E7, $E7, $E7, $E7, $E7, $E7, $81, $81, $FF, $FF,
    $00, $8F, {|} $93, $93, $FF, $FF, $81, $81, $E7, $E7, $E7, $E7, $E7, $E7, $81, $81, $FF, $FF,
    $00, $90, {|} $07, $07, $33, $33, $39, $39, $19, $19, $39, $39, $39, $39, $03, $03, $FF, $FF,
    $00, $91, {|} $8D, $8D, $63, $63, $19, $19, $09, $09, $21, $21, $31, $31, $39, $39, $FF, $FF,
    $00, $92, {|} $CF, $CF, $F7, $F7, $87, $87, $33, $33, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $93, {|} $E7, $E7, $DF, $DF, $87, $87, $33, $33, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $94, {|} $CF, $CF, $33, $33, $87, $87, $33, $33, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $95, {|} $8D, $8D, $63, $63, $87, $87, $33, $33, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $96, {|} $93, $93, $FF, $FF, $87, $87, $33, $33, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $97, {|} $FF, $FF, $83, $83, $29, $29, $00, $00, $24, $24, $38, $38, $81, $81, $FF, $FF,
    $00, $98, {|} $87, $87, $33, $33, $21, $21, $09, $09, $19, $19, $39, $39, $83, $83, $FF, $FF,
    $00, $99, {|} $CF, $CF, $F7, $F7, $39, $39, $39, $39, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $9A, {|} $E7, $E7, $DF, $DF, $39, $39, $39, $39, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $9B, {|} $E7, $E7, $99, $99, $39, $39, $39, $39, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $9C, {|} $93, $93, $FF, $FF, $39, $39, $39, $39, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $9D, {|} $E7, $E7, $DF, $DF, $39, $39, $39, $39, $83, $83, $F3, $F3, $F3, $F3, $FF, $FF,
    $00, $9E, {|} $3F, $3F, $07, $07, $33, $33, $39, $39, $03, $03, $3F, $3F, $3F, $3F, $3F, $3F,
    $00, $9F, {|} $FF, $FF, $87, $87, $33, $33, $23, $23, $39, $39, $39, $39, $23, $23, $3F, $3F,
    $00, $A0, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $A1, {|} $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $A2, {|} $30, $30, $78, $78, $CC, $CC, $C0, $C0, $C0, $C0, $C6, $C6, $7C, $7C, $30, $30,
    $00, $A3, {|} $38, $38, $6C, $6C, $60, $60, $F8, $F8, $60, $60, $60, $60, $FE, $FE, $00, $00,
    $00, $A4, {|} $00, $00, $C6, $C6, $7C, $7C, $C6, $C6, $C6, $C6, $7C, $7C, $C6, $C6, $00, $00,
    $00, $A5, {|} $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $0C, $0C, $3E, $3E, $0C, $0C, $00, $00,
    $00, $A6, {|} $18, $18, $18, $18, $18, $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $18,
    $00, $A7, {|} $78, $78, $C0, $C0, $7C, $7C, $C6, $C6, $C6, $C6, $7C, $7C, $06, $06, $3C, $3C,
    $00, $A8, {|} $6C, $6C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $A9, {|} $3C, $3C, $42, $42, $99, $99, $A1, $A1, $A1, $A1, $99, $99, $42, $42, $3C, $3C,
    $00, $AA, {|} $00, $00, $7E, $7E, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00, $7C, $7C, $00, $00,
    $00, $AB, {|} $00, $00, $36, $36, $6C, $6C, $D8, $D8, $6C, $6C, $36, $36, $00, $00, $00, $00,
    $00, $AC, {|} $FE, $7F, $03, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $AD, {|} $00, $00, $00, $00, $00, $00, $7E, $7E, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $AE, {|} $3C, $3C, $42, $42, $B9, $B9, $A5, $A5, $B9, $B9, $A5, $A5, $42, $42, $3C, $3C,
    $00, $AF, {|} $FF, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B0, {|} $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B1, {|} $00, $00, $10, $10, $7C, $7C, $10, $10, $00, $00, $7C, $7C, $00, $00, $00, $00,
    $00, $B2, {|} $70, $70, $18, $18, $30, $30, $60, $60, $78, $78, $00, $00, $00, $00, $00, $00,
    $00, $B3, {|} $70, $70, $18, $18, $30, $30, $18, $18, $70, $70, $00, $00, $00, $00, $00, $00,
    $00, $B4, {|} $18, $18, $30, $30, $60, $60, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B5, {|} $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $FC, $FC, $C0, $C0,
    $00, $B6, {|} $00, $00, $7E, $7E, $F4, $F4, $74, $74, $14, $14, $14, $14, $14, $14, $00, $00,
    $00, $B7, {|} $00, $00, $00, $00, $18, $18, $18, $18, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B8, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $30, $30,
    $00, $B9, {|} $30, $30, $70, $70, $30, $30, $30, $30, $78, $78, $00, $00, $00, $00, $00, $00,
    $00, $BA, {|} $00, $00, $7C, $7C, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00, $7C, $7C, $00, $00,
    $00, $BB, {|} $00, $00, $D8, $D8, $6C, $6C, $36, $36, $6C, $6C, $D8, $D8, $00, $00, $00, $00,
    $00, $BC, {|} $63, $63, $E6, $E6, $6C, $6C, $7B, $7B, $37, $35, $6D, $6F, $C3, $C3, $80, $80,
    $00, $BD, {|} $63, $63, $E6, $E6, $6C, $6C, $7E, $7F, $33, $36, $66, $6F, $CF, $C0, $80, $80,
    $00, $BE, {|} $C0, $C0, $66, $C6, $6C, $6C, $DB, $DB, $37, $35, $6D, $6F, $C3, $C3, $00, $00,
    $00, $BF, {|} $30, $30, $00, $00, $30, $30, $3C, $3C, $06, $06, $C6, $C6, $7C, $7C, $00, $00,
    $00, $C0, {|} $30, $30, $08, $08, $78, $78, $CC, $CC, $FE, $FE, $C6, $C6, $C6, $C6, $00, $00,
    $00, $C1, {|} $18, $18, $20, $20, $78, $78, $CC, $CC, $FE, $FE, $C6, $C6, $C6, $C6, $00, $00,
    $00, $C2, {|} $30, $30, $CC, $CC, $78, $78, $CC, $CC, $FE, $FE, $C6, $C6, $C6, $C6, $00, $00,
    $00, $C3, {|} $72, $72, $9C, $9C, $78, $78, $CC, $CC, $FE, $FE, $C6, $C6, $C6, $C6, $00, $00,
    $00, $C4, {|} $6C, $6C, $00, $00, $78, $78, $CC, $CC, $FE, $FE, $C6, $C6, $C6, $C6, $00, $00,
    $00, $C5, {|} $78, $78, $CC, $CC, $78, $78, $CC, $CC, $FE, $FE, $C6, $C6, $C6, $C6, $00, $00,
    $00, $C6, {|} $3F, $3F, $6C, $6C, $CF, $CF, $FC, $FC, $CC, $CC, $CC, $CC, $CF, $CF, $00, $00,
    $00, $C7, {|} $78, $78, $CC, $CC, $C0, $C0, $C0, $C0, $C0, $C0, $C6, $C6, $7C, $7C, $30, $30,
    $00, $C8, {|} $30, $30, $08, $08, $FE, $FE, $C0, $C0, $FC, $FC, $C0, $C0, $FE, $FE, $00, $00,
    $00, $C9, {|} $18, $18, $20, $20, $FE, $FE, $C0, $C0, $FC, $FC, $C0, $C0, $FE, $FE, $00, $00,
    $00, $CA, {|} $18, $18, $66, $66, $FE, $FE, $C0, $C0, $FC, $FC, $C0, $C0, $FE, $FE, $00, $00,
    $00, $CB, {|} $6C, $6C, $00, $00, $FE, $FE, $C0, $C0, $FC, $FC, $C0, $C0, $FE, $FE, $00, $00,
    $00, $CC, {|} $30, $30, $08, $08, $7E, $7E, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $CD, {|} $18, $18, $20, $20, $7E, $7E, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $CE, {|} $18, $18, $66, $66, $7E, $7E, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $CF, {|} $6C, $6C, $00, $00, $7E, $7E, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $D0, {|} $F8, $F8, $CC, $CC, $C6, $C6, $E6, $E6, $C6, $C6, $C6, $C6, $FC, $FC, $00, $00,
    $00, $D1, {|} $72, $72, $9C, $9C, $E6, $E6, $F6, $F6, $DE, $DE, $CE, $CE, $C6, $C6, $00, $00,
    $00, $D2, {|} $30, $30, $08, $08, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $D3, {|} $18, $18, $20, $20, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $D4, {|} $30, $30, $CC, $CC, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $D5, {|} $72, $72, $9C, $9C, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $D6, {|} $6C, $6C, $00, $00, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $D7, {|} $00, $00, $7C, $7C, $D6, $D6, $FF, $FF, $DB, $DB, $C7, $C7, $7E, $7E, $00, $00,
    $00, $D8, {|} $78, $78, $CC, $CC, $DE, $DE, $F6, $F6, $E6, $E6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $D9, {|} $30, $30, $08, $08, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $DA, {|} $18, $18, $20, $20, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $DB, {|} $18, $18, $66, $66, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $DC, {|} $6C, $6C, $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $DD, {|} $18, $18, $20, $20, $C6, $C6, $C6, $C6, $7C, $7C, $0C, $0C, $0C, $0C, $00, $00,
    $00, $DE, {|} $C0, $C0, $F8, $F8, $CC, $CC, $C6, $C6, $FC, $FC, $C0, $C0, $C0, $C0, $C0, $C0,
    $00, $DF, {|} $00, $00, $78, $78, $CC, $CC, $DC, $DC, $C6, $C6, $C6, $C6, $DC, $DC, $C0, $C0,
    $00, $E0, {|} $30, $30, $08, $08, $3C, $3C, $06, $06, $7E, $7E, $C6, $C6, $7E, $7E, $00, $00,
    $00, $E1, {|} $18, $18, $20, $20, $3C, $3C, $06, $06, $7E, $7E, $C6, $C6, $7E, $7E, $00, $00,
    $00, $E2, {|} $18, $18, $66, $66, $3C, $3C, $06, $06, $7E, $7E, $C6, $C6, $7E, $7E, $00, $00,
    $00, $E3, {|} $72, $72, $9C, $9C, $3C, $3C, $06, $06, $7E, $7E, $C6, $C6, $7E, $7E, $00, $00,
    $00, $E4, {|} $6C, $6C, $00, $00, $3C, $3C, $06, $06, $7E, $7E, $C6, $C6, $7E, $7E, $00, $00,
    $00, $E5, {|} $3C, $3C, $66, $66, $3C, $3C, $06, $06, $7E, $7E, $C6, $C6, $7E, $7E, $00, $00,
    $00, $E6, {|} $00, $00, $7E, $7E, $1B, $1B, $7F, $7F, $D8, $D8, $D8, $D8, $7F, $7F, $00, $00,
    $00, $E7, {|} $00, $00, $78, $78, $CC, $CC, $C0, $C0, $C0, $C0, $C6, $C6, $7C, $7C, $30, $30,
    $00, $E8, {|} $30, $30, $08, $08, $78, $78, $FC, $FC, $C0, $C0, $C6, $C6, $7C, $7C, $00, $00,
    $00, $E9, {|} $18, $18, $20, $20, $78, $78, $FC, $FC, $C0, $C0, $C6, $C6, $7C, $7C, $00, $00,
    $00, $EA, {|} $30, $30, $CC, $CC, $78, $78, $FC, $FC, $C0, $C0, $C6, $C6, $7C, $7C, $00, $00,
    $00, $EB, {|} $6C, $6C, $00, $00, $78, $78, $FC, $FC, $C0, $C0, $C6, $C6, $7C, $7C, $00, $00,
    $00, $EC, {|} $30, $30, $08, $08, $38, $38, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $ED, {|} $18, $18, $20, $20, $38, $38, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $EE, {|} $18, $18, $66, $66, $38, $38, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $EF, {|} $6C, $6C, $00, $00, $38, $38, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $F0, {|} $0C, $0C, $16, $16, $3E, $3E, $66, $66, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $F1, {|} $72, $72, $9C, $9C, $F8, $F8, $CC, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $F2, {|} $30, $30, $08, $08, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $F3, {|} $18, $18, $20, $20, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $F4, {|} $18, $18, $66, $66, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $F5, {|} $72, $72, $9C, $9C, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $F6, {|} $6C, $6C, $00, $00, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $F7, {|} $00, $00, $18, $18, $00, $00, $7F, $FE, $00, $00, $18, $18, $00, $00, $00, $00,
    $00, $F8, {|} $00, $00, $78, $78, $CC, $CC, $DE, $DE, $F6, $F6, $E6, $E6, $7C, $7C, $00, $00,
    $00, $F9, {|} $30, $30, $08, $08, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $FA, {|} $18, $18, $20, $20, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $FB, {|} $18, $18, $66, $66, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $FC, {|} $6C, $6C, $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $FD, {|} $18, $18, $20, $20, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $06, $06, $7C, $7C,
    $00, $FE, {|} $C0, $C0, $C0, $C0, $F8, $F8, $CC, $CC, $C6, $C6, $FC, $FC, $C0, $C0, $C0, $C0,
    $00, $FF, {|} $6C, $6C, $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $06, $06, $7C, $7C );
  //MicroKnightPlusMirrors = packed array [0..(0*3)-1] of Uint16;

  // MicroKnight
  MicroKnight_COUNT = 256;  // number of glyphs
  MicroKnight : packed array [0..MicroKnight_COUNT * 18 - 1] of byte = (
    $00, $00, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $01, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $02, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $03, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $04, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $05, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $06, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $07, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $08, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $09, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $0A, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $0B, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $0C, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $0D, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $0E, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $0F, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $10, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $11, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $12, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $13, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $14, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $15, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $16, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $17, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $18, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $19, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $1A, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $1B, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $1C, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $1D, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $1E, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $1F, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $20, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $21, {|} $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00, $18, $18, $00, $00,
    $00, $22, {|} $6C, $6C, $6C, $6C, $24, $24, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $23, {|} $6C, $6C, $6C, $6C, $FE, $FE, $6C, $6C, $FE, $FE, $6C, $6C, $6C, $6C, $00, $00,
    $00, $24, {|} $10, $10, $7C, $7C, $D0, $D0, $7C, $7C, $16, $16, $16, $16, $7C, $7C, $10, $10,
    $00, $25, {|} $60, $60, $96, $96, $7C, $7C, $18, $18, $30, $30, $6C, $6C, $D2, $D2, $0C, $0C,
    $00, $26, {|} $70, $70, $D8, $D8, $70, $70, $F6, $F6, $DC, $DC, $D8, $D8, $7C, $7C, $06, $06,
    $00, $27, {|} $18, $18, $18, $18, $30, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $28, {|} $18, $18, $30, $30, $60, $60, $60, $60, $60, $60, $30, $30, $18, $18, $00, $00,
    $00, $29, {|} $30, $30, $18, $18, $0C, $0C, $0C, $0C, $0C, $0C, $18, $18, $30, $30, $00, $00,
    $00, $2A, {|} $00, $00, $6C, $6C, $38, $38, $FE, $FE, $38, $38, $6C, $6C, $00, $00, $00, $00,
    $00, $2B, {|} $00, $00, $18, $18, $18, $18, $7E, $7E, $18, $18, $18, $18, $00, $00, $00, $00,
    $00, $2C, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $18, $30, $30,
    $00, $2D, {|} $00, $00, $00, $00, $00, $00, $7F, $7F, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $2E, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $18, $00, $00,
    $00, $2F, {|} $03, $03, $06, $06, $0C, $0C, $18, $18, $30, $30, $60, $60, $C0, $C0, $80, $80,
    $00, $30, {|} $00, $00, $78, $78, $CC, $CC, $DE, $DE, $F6, $F6, $E6, $E6, $7C, $7C, $00, $00,
    $00, $31, {|} $18, $18, $18, $18, $38, $38, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $32, {|} $7C, $7C, $06, $06, $3C, $3C, $60, $60, $C0, $C0, $C0, $C0, $FE, $FE, $00, $00,
    $00, $33, {|} $3C, $3C, $06, $06, $1C, $1C, $06, $06, $46, $46, $C6, $C6, $7C, $7C, $00, $00,
    $00, $34, {|} $18, $18, $18, $18, $30, $30, $6C, $6C, $CC, $CC, $FE, $FE, $0C, $0C, $00, $00,
    $00, $35, {|} $F8, $F8, $C0, $C0, $FC, $FC, $06, $06, $46, $46, $CC, $CC, $78, $78, $00, $00,
    $00, $36, {|} $70, $70, $C0, $C0, $FC, $FC, $C6, $C6, $C6, $C6, $CC, $CC, $78, $78, $00, $00,
    $00, $37, {|} $FE, $FE, $06, $06, $0C, $0C, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $38, {|} $78, $78, $CC, $CC, $7C, $7C, $C6, $C6, $C6, $C6, $CC, $CC, $78, $78, $00, $00,
    $00, $39, {|} $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7E, $7E, $06, $06, $1C, $1C, $00, $00,
    $00, $3A, {|} $00, $00, $00, $00, $18, $18, $18, $18, $00, $00, $18, $18, $18, $18, $00, $00,
    $00, $3B, {|} $00, $00, $00, $00, $18, $18, $18, $18, $00, $00, $18, $18, $18, $18, $30, $30,
    $00, $3C, {|} $00, $00, $18, $18, $30, $30, $60, $60, $30, $30, $18, $18, $00, $00, $00, $00,
    $00, $3D, {|} $00, $00, $00, $00, $7E, $7E, $00, $00, $00, $00, $7E, $7E, $00, $00, $00, $00,
    $00, $3E, {|} $00, $00, $30, $30, $18, $18, $0C, $0C, $18, $18, $30, $30, $00, $00, $00, $00,
    $00, $3F, {|} $7C, $7C, $C6, $C6, $06, $06, $3C, $3C, $30, $30, $00, $00, $30, $30, $00, $00,
    $00, $40, {|} $7C, $7C, $C6, $C6, $DE, $DE, $D6, $D6, $DE, $DE, $C0, $C0, $7E, $7E, $00, $00,
    $00, $41, {|} $78, $78, $CC, $CC, $C6, $C6, $FE, $FE, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $42, {|} $F8, $F8, $CC, $CC, $FC, $FC, $C6, $C6, $C6, $C6, $CC, $CC, $F8, $F8, $00, $00,
    $00, $43, {|} $78, $78, $CC, $CC, $C0, $C0, $C0, $C0, $C0, $C0, $C6, $C6, $7C, $7C, $00, $00,
    $00, $44, {|} $F8, $F8, $CC, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $FC, $FC, $00, $00,
    $00, $45, {|} $FE, $FE, $C0, $C0, $FC, $FC, $C0, $C0, $C0, $C0, $C0, $C0, $FE, $FE, $00, $00,
    $00, $46, {|} $FE, $FE, $C0, $C0, $FC, $FC, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $00, $00,
    $00, $47, {|} $38, $38, $60, $60, $C0, $C0, $CE, $CE, $C6, $C6, $C6, $C6, $7E, $7E, $06, $06,
    $00, $48, {|} $C6, $C6, $C6, $C6, $C6, $C6, $FE, $FE, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $49, {|} $7E, $7E, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $4A, {|} $0E, $0E, $06, $06, $06, $06, $06, $06, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $4B, {|} $C6, $C6, $CC, $CC, $D8, $D8, $F0, $F0, $D8, $D8, $CC, $CC, $C6, $C6, $00, $00,
    $00, $4C, {|} $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $FE, $FE, $00, $00,
    $00, $4D, {|} $C6, $C6, $EE, $EE, $FE, $FE, $D6, $D6, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $4E, {|} $C6, $C6, $E6, $E6, $F6, $F6, $DE, $DE, $CE, $CE, $C6, $C6, $C6, $C6, $00, $00,
    $00, $4F, {|} $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $50, {|} $F8, $F8, $CC, $CC, $C6, $C6, $C6, $C6, $FC, $FC, $C0, $C0, $C0, $C0, $00, $00,
    $00, $51, {|} $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $D6, $D6, $7C, $7C, $0C, $0C,
    $00, $52, {|} $F8, $F8, $CC, $CC, $C6, $C6, $C6, $C6, $FC, $FC, $D8, $D8, $CC, $CC, $06, $06,
    $00, $53, {|} $78, $78, $C0, $C0, $7C, $7C, $06, $06, $46, $46, $C6, $C6, $7C, $7C, $00, $00,
    $00, $54, {|} $7E, $7E, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $55, {|} $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $56, {|} $C6, $C6, $C6, $C6, $C6, $C6, $6C, $6C, $6C, $6C, $38, $38, $38, $38, $00, $00,
    $00, $57, {|} $C6, $C6, $C6, $C6, $C6, $C6, $D6, $D6, $FE, $FE, $EE, $EE, $C6, $C6, $00, $00,
    $00, $58, {|} $C6, $C6, $6C, $6C, $38, $38, $38, $38, $6C, $6C, $C6, $C6, $C6, $C6, $00, $00,
    $00, $59, {|} $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $0C, $0C, $0C, $0C, $0C, $0C, $00, $00,
    $00, $5A, {|} $FE, $FE, $0C, $0C, $18, $18, $30, $30, $60, $60, $C0, $C0, $FE, $FE, $00, $00,
    $00, $5B, {|} $38, $38, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $38, $38, $00, $00,
    $00, $5C, {|} $C0, $C0, $60, $60, $30, $30, $18, $18, $0C, $0C, $06, $06, $03, $03, $01, $01,
    $00, $5D, {|} $38, $38, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $38, $38, $00, $00,
    $00, $5E, {|} $10, $10, $38, $38, $6C, $6C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $5F, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $FF,
    $00, $60, {|} $18, $18, $18, $18, $0C, $0C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $61, {|} $00, $00, $3C, $3C, $06, $06, $7E, $7E, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $62, {|} $C0, $C0, $F8, $F8, $CC, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $FC, $FC, $00, $00,
    $00, $63, {|} $00, $00, $78, $78, $CC, $CC, $C0, $C0, $C0, $C0, $C6, $C6, $7C, $7C, $00, $00,
    $00, $64, {|} $06, $06, $3E, $3E, $66, $66, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $65, {|} $00, $00, $78, $78, $CC, $CC, $FC, $FC, $C0, $C0, $C6, $C6, $7C, $7C, $00, $00,
    $00, $66, {|} $38, $38, $6C, $6C, $60, $60, $78, $78, $60, $60, $60, $60, $60, $60, $60, $60,
    $00, $67, {|} $00, $00, $7E, $7E, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $06, $06, $7C, $7C,
    $00, $68, {|} $C0, $C0, $F8, $F8, $CC, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $69, {|} $18, $18, $00, $00, $38, $38, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $6A, {|} $0C, $0C, $00, $00, $1C, $1C, $0C, $0C, $0C, $0C, $0C, $0C, $4C, $4C, $38, $38,
    $00, $6B, {|} $C0, $C0, $CC, $CC, $D8, $D8, $F0, $F0, $D8, $D8, $CC, $CC, $C6, $C6, $00, $00,
    $00, $6C, {|} $38, $38, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $6D, {|} $00, $00, $C4, $C4, $EE, $EE, $FE, $FE, $D6, $D6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $6E, {|} $00, $00, $F8, $F8, $CC, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $6F, {|} $00, $00, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $70, {|} $00, $00, $F8, $F8, $CC, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $FC, $FC, $C0, $C0,
    $00, $71, {|} $00, $00, $3E, $3E, $66, $66, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $06, $06,
    $00, $72, {|} $00, $00, $FC, $FC, $C6, $C6, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $00, $00,
    $00, $73, {|} $00, $00, $78, $78, $C0, $C0, $7C, $7C, $06, $06, $C6, $C6, $7C, $7C, $00, $00,
    $00, $74, {|} $30, $30, $7C, $7C, $30, $30, $30, $30, $30, $30, $32, $32, $1C, $1C, $00, $00,
    $00, $75, {|} $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $76, {|} $00, $00, $C6, $C6, $C6, $C6, $6C, $6C, $6C, $6C, $38, $38, $38, $38, $00, $00,
    $00, $77, {|} $00, $00, $C6, $C6, $D6, $D6, $FE, $FE, $7C, $7C, $6C, $6C, $44, $44, $00, $00,
    $00, $78, {|} $00, $00, $C6, $C6, $6C, $6C, $38, $38, $38, $38, $6C, $6C, $C6, $C6, $00, $00,
    $00, $79, {|} $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $06, $06, $7C, $7C,
    $00, $7A, {|} $00, $00, $FE, $FE, $0C, $0C, $18, $18, $30, $30, $60, $60, $FE, $FE, $00, $00,
    $00, $7B, {|} $0C, $0C, $18, $18, $18, $18, $30, $30, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $7C, {|} $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18,
    $00, $7D, {|} $30, $30, $18, $18, $18, $18, $0C, $0C, $18, $18, $18, $18, $30, $30, $00, $00,
    $00, $7E, {|} $72, $72, $9C, $9C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $7F, {|} $38, $38, $70, $70, $E0, $E0, $C1, $C1, $83, $83, $07, $07, $0E, $0E, $1C, $1C,
    $00, $80, {|} $CF, $CF, $F7, $F7, $87, $87, $33, $33, $01, $01, $39, $39, $39, $39, $FF, $FF,
    $00, $81, {|} $E7, $E7, $DF, $DF, $87, $87, $33, $33, $01, $01, $39, $39, $39, $39, $FF, $FF,
    $00, $82, {|} $CF, $CF, $33, $33, $87, $87, $33, $33, $01, $01, $39, $39, $39, $39, $FF, $FF,
    $00, $83, {|} $8D, $8D, $63, $63, $87, $87, $33, $33, $01, $01, $39, $39, $39, $39, $FF, $FF,
    $00, $84, {|} $93, $93, $FF, $FF, $87, $87, $33, $33, $01, $01, $39, $39, $39, $39, $FF, $FF,
    $00, $85, {|} $87, $87, $33, $33, $87, $87, $33, $33, $01, $01, $39, $39, $39, $39, $FF, $FF,
    $00, $86, {|} $C0, $C0, $93, $93, $30, $30, $03, $03, $33, $33, $33, $33, $30, $30, $FF, $FF,
    $00, $87, {|} $87, $87, $33, $33, $3F, $3F, $3F, $3F, $3F, $3F, $33, $33, $87, $87, $CF, $CF,
    $00, $88, {|} $CF, $CF, $F7, $F7, $01, $01, $3F, $3F, $03, $03, $3F, $3F, $01, $01, $FF, $FF,
    $00, $89, {|} $E7, $E7, $DF, $DF, $01, $01, $3F, $3F, $03, $03, $3F, $3F, $01, $01, $FF, $FF,
    $00, $8A, {|} $E7, $E7, $99, $99, $01, $01, $3F, $3F, $03, $03, $3F, $3F, $01, $01, $FF, $FF,
    $00, $8B, {|} $93, $93, $FF, $FF, $01, $01, $3F, $3F, $03, $03, $3F, $3F, $01, $01, $FF, $FF,
    $00, $8C, {|} $CF, $CF, $F7, $F7, $81, $81, $E7, $E7, $E7, $E7, $E7, $E7, $81, $81, $FF, $FF,
    $00, $8D, {|} $E7, $E7, $DF, $DF, $81, $81, $E7, $E7, $E7, $E7, $E7, $E7, $81, $81, $FF, $FF,
    $00, $8E, {|} $E7, $E7, $99, $99, $81, $81, $E7, $E7, $E7, $E7, $E7, $E7, $81, $81, $FF, $FF,
    $00, $8F, {|} $93, $93, $FF, $FF, $81, $81, $E7, $E7, $E7, $E7, $E7, $E7, $81, $81, $FF, $FF,
    $00, $90, {|} $07, $07, $33, $33, $39, $39, $19, $19, $39, $39, $39, $39, $03, $03, $FF, $FF,
    $00, $91, {|} $8D, $8D, $63, $63, $19, $19, $09, $09, $21, $21, $31, $31, $39, $39, $FF, $FF,
    $00, $92, {|} $CF, $CF, $F7, $F7, $87, $87, $33, $33, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $93, {|} $E7, $E7, $DF, $DF, $87, $87, $33, $33, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $94, {|} $CF, $CF, $33, $33, $87, $87, $33, $33, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $95, {|} $8D, $8D, $63, $63, $87, $87, $33, $33, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $96, {|} $93, $93, $FF, $FF, $87, $87, $33, $33, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $97, {|} $FF, $FF, $83, $83, $29, $29, $00, $00, $24, $24, $38, $38, $81, $81, $FF, $FF,
    $00, $98, {|} $87, $87, $33, $33, $21, $21, $09, $09, $19, $19, $39, $39, $83, $83, $FF, $FF,
    $00, $99, {|} $CF, $CF, $F7, $F7, $39, $39, $39, $39, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $9A, {|} $E7, $E7, $DF, $DF, $39, $39, $39, $39, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $9B, {|} $E7, $E7, $99, $99, $39, $39, $39, $39, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $9C, {|} $93, $93, $FF, $FF, $39, $39, $39, $39, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $9D, {|} $E7, $E7, $DF, $DF, $39, $39, $39, $39, $83, $83, $F3, $F3, $F3, $F3, $FF, $FF,
    $00, $9E, {|} $3F, $3F, $07, $07, $33, $33, $39, $39, $03, $03, $3F, $3F, $3F, $3F, $3F, $3F,
    $00, $9F, {|} $FF, $FF, $87, $87, $33, $33, $23, $23, $39, $39, $39, $39, $23, $23, $3F, $3F,
    $00, $A0, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $A1, {|} $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $A2, {|} $30, $30, $78, $78, $CC, $CC, $C0, $C0, $C0, $C0, $C6, $C6, $7C, $7C, $30, $30,
    $00, $A3, {|} $38, $38, $6C, $6C, $60, $60, $F8, $F8, $60, $60, $60, $60, $FE, $FE, $00, $00,
    $00, $A4, {|} $00, $00, $C6, $C6, $7C, $7C, $C6, $C6, $C6, $C6, $7C, $7C, $C6, $C6, $00, $00,
    $00, $A5, {|} $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $0C, $0C, $3E, $3E, $0C, $0C, $00, $00,
    $00, $A6, {|} $18, $18, $18, $18, $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $A7, {|} $78, $78, $C0, $C0, $7C, $7C, $C6, $C6, $C6, $C6, $7C, $7C, $06, $06, $3C, $3C,
    $00, $A8, {|} $6C, $6C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $A9, {|} $3C, $3C, $42, $42, $99, $99, $A1, $A1, $A1, $A1, $99, $99, $42, $42, $3C, $3C,
    $00, $AA, {|} $00, $00, $7E, $7E, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00, $7C, $7C, $00, $00,
    $00, $AB, {|} $00, $00, $36, $36, $6C, $6C, $D8, $D8, $6C, $6C, $36, $36, $00, $00, $00, $00,
    $00, $AC, {|} $7C, $7C, $0C, $0C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $AD, {|} $00, $00, $00, $00, $00, $00, $7E, $7E, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $AE, {|} $3C, $3C, $42, $42, $B9, $B9, $A5, $A5, $B9, $B9, $A5, $A5, $42, $42, $3C, $3C,
    $00, $AF, {|} $7C, $7C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B0, {|} $38, $38, $6C, $6C, $6C, $6C, $38, $38, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B1, {|} $00, $00, $10, $10, $7C, $7C, $10, $10, $00, $00, $7C, $7C, $00, $00, $00, $00,
    $00, $B2, {|} $70, $70, $18, $18, $30, $30, $60, $60, $78, $78, $00, $00, $00, $00, $00, $00,
    $00, $B3, {|} $70, $70, $18, $18, $30, $30, $18, $18, $70, $70, $00, $00, $00, $00, $00, $00,
    $00, $B4, {|} $18, $18, $30, $30, $60, $60, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B5, {|} $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $FC, $FC, $C0, $C0,
    $00, $B6, {|} $00, $00, $7E, $7E, $F4, $F4, $74, $74, $14, $14, $14, $14, $14, $14, $00, $00,
    $00, $B7, {|} $00, $00, $00, $00, $00, $00, $18, $18, $18, $18, $00, $00, $00, $00, $00, $00,
    $00, $B8, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $30, $30,
    $00, $B9, {|} $30, $30, $70, $70, $30, $30, $30, $30, $78, $78, $00, $00, $00, $00, $00, $00,
    $00, $BA, {|} $00, $00, $7C, $7C, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00, $7C, $7C, $00, $00,
    $00, $BB, {|} $00, $00, $D8, $D8, $6C, $6C, $36, $36, $6C, $6C, $D8, $D8, $00, $00, $00, $00,
    $00, $BC, {|} $60, $60, $E6, $E6, $6C, $6C, $7A, $7A, $36, $36, $6F, $6F, $CF, $CF, $03, $03,
    $00, $BD, {|} $60, $60, $E6, $E6, $6C, $6C, $78, $78, $3E, $3E, $63, $63, $CE, $CE, $1F, $1F,
    $00, $BE, {|} $E0, $E0, $36, $36, $6C, $6C, $3A, $3A, $F6, $F6, $6F, $6F, $CF, $CF, $03, $03,
    $00, $BF, {|} $30, $30, $00, $00, $30, $30, $3C, $3C, $06, $06, $C6, $C6, $7C, $7C, $00, $00,
    $00, $C0, {|} $30, $30, $08, $08, $78, $78, $CC, $CC, $FE, $FE, $C6, $C6, $C6, $C6, $00, $00,
    $00, $C1, {|} $18, $18, $20, $20, $78, $78, $CC, $CC, $FE, $FE, $C6, $C6, $C6, $C6, $00, $00,
    $00, $C2, {|} $30, $30, $CC, $CC, $78, $78, $CC, $CC, $FE, $FE, $C6, $C6, $C6, $C6, $00, $00,
    $00, $C3, {|} $72, $72, $9C, $9C, $78, $78, $CC, $CC, $FE, $FE, $C6, $C6, $C6, $C6, $00, $00,
    $00, $C4, {|} $6C, $6C, $00, $00, $78, $78, $CC, $CC, $FE, $FE, $C6, $C6, $C6, $C6, $00, $00,
    $00, $C5, {|} $78, $78, $CC, $CC, $78, $78, $CC, $CC, $FE, $FE, $C6, $C6, $C6, $C6, $00, $00,
    $00, $C6, {|} $3F, $3F, $6C, $6C, $CF, $CF, $FC, $FC, $CC, $CC, $CC, $CC, $CF, $CF, $00, $00,
    $00, $C7, {|} $78, $78, $CC, $CC, $C0, $C0, $C0, $C0, $C0, $C0, $C6, $C6, $7C, $7C, $30, $30,
    $00, $C8, {|} $30, $30, $08, $08, $FE, $FE, $C0, $C0, $FC, $FC, $C0, $C0, $FE, $FE, $00, $00,
    $00, $C9, {|} $18, $18, $20, $20, $FE, $FE, $C0, $C0, $FC, $FC, $C0, $C0, $FE, $FE, $00, $00,
    $00, $CA, {|} $18, $18, $66, $66, $FE, $FE, $C0, $C0, $FC, $FC, $C0, $C0, $FE, $FE, $00, $00,
    $00, $CB, {|} $6C, $6C, $00, $00, $FE, $FE, $C0, $C0, $FC, $FC, $C0, $C0, $FE, $FE, $00, $00,
    $00, $CC, {|} $30, $30, $08, $08, $7E, $7E, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $CD, {|} $18, $18, $20, $20, $7E, $7E, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $CE, {|} $18, $18, $66, $66, $7E, $7E, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $CF, {|} $6C, $6C, $00, $00, $7E, $7E, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $D0, {|} $F8, $F8, $CC, $CC, $C6, $C6, $E6, $E6, $C6, $C6, $C6, $C6, $FC, $FC, $00, $00,
    $00, $D1, {|} $72, $72, $9C, $9C, $E6, $E6, $F6, $F6, $DE, $DE, $CE, $CE, $C6, $C6, $00, $00,
    $00, $D2, {|} $30, $30, $08, $08, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $D3, {|} $18, $18, $20, $20, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $D4, {|} $30, $30, $CC, $CC, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $D5, {|} $72, $72, $9C, $9C, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $D6, {|} $6C, $6C, $00, $00, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $D7, {|} $00, $00, $7C, $7C, $D6, $D6, $FF, $FF, $DB, $DB, $C7, $C7, $7E, $7E, $00, $00,
    $00, $D8, {|} $78, $78, $CC, $CC, $DE, $DE, $F6, $F6, $E6, $E6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $D9, {|} $30, $30, $08, $08, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $DA, {|} $18, $18, $20, $20, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $DB, {|} $18, $18, $66, $66, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $DC, {|} $6C, $6C, $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $DD, {|} $18, $18, $20, $20, $C6, $C6, $C6, $C6, $7C, $7C, $0C, $0C, $0C, $0C, $00, $00,
    $00, $DE, {|} $C0, $C0, $F8, $F8, $CC, $CC, $C6, $C6, $FC, $FC, $C0, $C0, $C0, $C0, $C0, $C0,
    $00, $DF, {|} $00, $00, $78, $78, $CC, $CC, $DC, $DC, $C6, $C6, $C6, $C6, $DC, $DC, $C0, $C0,
    $00, $E0, {|} $30, $30, $08, $08, $3C, $3C, $06, $06, $7E, $7E, $C6, $C6, $7E, $7E, $00, $00,
    $00, $E1, {|} $18, $18, $20, $20, $3C, $3C, $06, $06, $7E, $7E, $C6, $C6, $7E, $7E, $00, $00,
    $00, $E2, {|} $18, $18, $66, $66, $3C, $3C, $06, $06, $7E, $7E, $C6, $C6, $7E, $7E, $00, $00,
    $00, $E3, {|} $72, $72, $9C, $9C, $3C, $3C, $06, $06, $7E, $7E, $C6, $C6, $7E, $7E, $00, $00,
    $00, $E4, {|} $6C, $6C, $00, $00, $3C, $3C, $06, $06, $7E, $7E, $C6, $C6, $7E, $7E, $00, $00,
    $00, $E5, {|} $3C, $3C, $66, $66, $3C, $3C, $06, $06, $7E, $7E, $C6, $C6, $7E, $7E, $00, $00,
    $00, $E6, {|} $00, $00, $7E, $7E, $1B, $1B, $7F, $7F, $D8, $D8, $D8, $D8, $7F, $7F, $00, $00,
    $00, $E7, {|} $00, $00, $78, $78, $CC, $CC, $C0, $C0, $C0, $C0, $C6, $C6, $7C, $7C, $30, $30,
    $00, $E8, {|} $30, $30, $08, $08, $78, $78, $FC, $FC, $C0, $C0, $C6, $C6, $7C, $7C, $00, $00,
    $00, $E9, {|} $18, $18, $20, $20, $78, $78, $FC, $FC, $C0, $C0, $C6, $C6, $7C, $7C, $00, $00,
    $00, $EA, {|} $30, $30, $CC, $CC, $78, $78, $FC, $FC, $C0, $C0, $C6, $C6, $7C, $7C, $00, $00,
    $00, $EB, {|} $6C, $6C, $00, $00, $78, $78, $FC, $FC, $C0, $C0, $C6, $C6, $7C, $7C, $00, $00,
    $00, $EC, {|} $30, $30, $08, $08, $38, $38, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $ED, {|} $18, $18, $20, $20, $38, $38, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $EE, {|} $18, $18, $66, $66, $38, $38, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $EF, {|} $6C, $6C, $00, $00, $38, $38, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $F0, {|} $0C, $0C, $16, $16, $3E, $3E, $66, $66, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $F1, {|} $72, $72, $9C, $9C, $F8, $F8, $CC, $CC, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $F2, {|} $30, $30, $08, $08, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $F3, {|} $18, $18, $20, $20, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $F4, {|} $18, $18, $66, $66, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $F5, {|} $72, $72, $9C, $9C, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $F6, {|} $6C, $6C, $00, $00, $78, $78, $CC, $CC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $F7, {|} $00, $00, $18, $18, $00, $00, $7E, $7E, $00, $00, $18, $18, $00, $00, $00, $00,
    $00, $F8, {|} $00, $00, $78, $78, $CC, $CC, $DE, $DE, $F6, $F6, $E6, $E6, $7C, $7C, $00, $00,
    $00, $F9, {|} $30, $30, $08, $08, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $FA, {|} $18, $18, $20, $20, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $FB, {|} $18, $18, $66, $66, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $FC, {|} $6C, $6C, $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $FD, {|} $18, $18, $20, $20, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $06, $06, $7C, $7C,
    $00, $FE, {|} $C0, $C0, $C0, $C0, $F8, $F8, $CC, $CC, $C6, $C6, $FC, $FC, $C0, $C0, $C0, $C0,
    $00, $FF, {|} $6C, $6C, $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $06, $06, $7C, $7C );
  //MicroKnightMirrors = packed array [0..(0*3)-1] of Uint16;

  // P0T-NOoDLE
  P0T_NOoDLE_COUNT = 256; // number of glyphs
  P0T_NOoDLE : packed array [0..P0T_NOoDLE_COUNT * 18 - 1] of byte = (
    $00, $00, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $01, {|} $FF, $FF, $83, $83, $39, $39, $29, $29, $23, $23, $3F, $3F, $81, $81, $FF, $FF,
    $00, $02, {|} $FF, $FF, $83, $83, $39, $39, $21, $21, $39, $39, $39, $39, $39, $39, $7F, $7F,
    $00, $03, {|} $FF, $FF, $03, $03, $99, $99, $93, $93, $99, $99, $99, $99, $03, $03, $FF, $FF,
    $00, $04, {|} $FF, $FF, $83, $83, $39, $39, $3F, $3F, $3F, $3F, $39, $39, $83, $83, $F7, $F7,
    $00, $05, {|} $FF, $FF, $0F, $0F, $87, $87, $93, $93, $99, $99, $99, $99, $03, $03, $FF, $FF,
    $00, $06, {|} $FF, $FF, $01, $01, $9F, $9F, $83, $83, $9F, $9F, $99, $99, $03, $03, $F7, $F7,
    $00, $07, {|} $FB, $FB, $01, $01, $9C, $9C, $87, $87, $9F, $9F, $9F, $9F, $1F, $1F, $FF, $FF,
    $00, $08, {|} $F7, $F7, $83, $83, $39, $39, $3F, $3F, $31, $31, $39, $39, $81, $81, $FD, $FD,
    $00, $09, {|} $FF, $FF, $39, $39, $39, $39, $21, $21, $39, $39, $39, $39, $39, $39, $7F, $7F,
    $00, $0A, {|} $FF, $FF, $81, $81, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $81, $81, $FF, $FF,
    $00, $0B, {|} $FD, $FD, $F9, $F9, $F9, $F9, $F9, $F9, $19, $19, $39, $39, $03, $03, $7F, $7F,
    $00, $0C, {|} $FF, $FF, $39, $39, $33, $33, $27, $27, $33, $33, $39, $39, $39, $39, $7F, $7F,
    $00, $0D, {|} $FF, $FF, $1F, $1F, $9F, $9F, $9F, $9F, $9F, $9F, $99, $99, $03, $03, $F7, $F7,
    $00, $0E, {|} $FF, $FF, $39, $39, $11, $11, $01, $01, $29, $29, $39, $39, $39, $39, $7F, $7F,
    $00, $0F, {|} $FF, $FF, $39, $39, $29, $29, $21, $21, $31, $31, $39, $39, $39, $39, $FF, $FF,
    $00, $10, {|} $FF, $FF, $83, $83, $39, $39, $39, $39, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $11, {|} $FF, $FF, $03, $03, $99, $99, $99, $99, $93, $93, $9F, $9F, $1F, $1F, $FF, $FF,
    $00, $12, {|} $FF, $FF, $83, $83, $39, $39, $39, $39, $29, $29, $35, $35, $8B, $8B, $FD, $FD,
    $00, $13, {|} $FF, $FF, $03, $03, $99, $99, $93, $93, $99, $99, $99, $99, $19, $19, $FB, $FB,
    $00, $14, {|} $FF, $FF, $83, $83, $3F, $3F, $83, $83, $F9, $F9, $39, $39, $03, $03, $7F, $7F,
    $00, $15, {|} $F7, $F7, $03, $03, $C9, $C9, $CF, $CF, $CF, $CF, $CF, $CF, $CF, $CF, $DF, $DF,
    $00, $16, {|} $7F, $7F, $31, $31, $33, $33, $33, $33, $33, $33, $33, $33, $81, $81, $FF, $FF,
    $00, $17, {|} $7F, $7F, $39, $39, $39, $39, $93, $93, $93, $93, $C7, $C7, $EF, $EF, $FF, $FF,
    $00, $18, {|} $7F, $7F, $39, $39, $39, $39, $29, $29, $01, $01, $11, $11, $39, $39, $FF, $FF,
    $00, $19, {|} $3F, $3F, $39, $39, $39, $39, $A3, $A3, $39, $39, $39, $39, $39, $39, $FD, $FD,
    $00, $1A, {|} $FD, $FD, $39, $39, $39, $39, $A3, $A3, $E7, $E7, $E7, $E7, $E7, $E7, $FF, $FF,
    $00, $1B, {|} $FF, $FF, $01, $01, $F3, $F3, $E7, $E7, $CD, $CD, $99, $99, $81, $81, $FD, $FD,
    $00, $1C, {|} $FF, $FF, $81, $81, $CF, $CF, $CF, $CF, $CF, $CF, $CF, $CF, $81, $81, $FF, $FF,
    $00, $1D, {|} $3F, $3F, $9F, $9F, $CF, $CF, $E7, $E7, $F3, $F3, $F9, $F9, $FC, $FC, $FE, $FE,
    $00, $1E, {|} $FF, $FF, $81, $81, $F3, $F3, $F3, $F3, $F3, $F3, $F3, $F3, $81, $81, $FF, $FF,
    $00, $1F, {|} $E7, $E7, $C3, $C3, $99, $99, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $00, $20, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $21, {|} $00, $00, $1C, $1C, $1C, $1C, $18, $18, $18, $18, $00, $00, $18, $18, $10, $10,
    $00, $22, {|} $66, $66, $66, $66, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $23, {|} $6C, $6C, $6C, $6C, $FE, $FE, $6C, $6C, $FE, $FE, $6C, $6C, $6C, $6C, $00, $00,
    $00, $24, {|} $18, $18, $7E, $7E, $C0, $C0, $7C, $7C, $06, $06, $FC, $FC, $30, $30, $00, $00,
    $00, $25, {|} $63, $63, $A6, $A6, $CC, $CC, $18, $18, $33, $33, $65, $65, $C6, $C6, $80, $80,
    $00, $26, {|} $3C, $3C, $66, $66, $64, $64, $7B, $7B, $CE, $CE, $C6, $C6, $7B, $7B, $00, $00,
    $00, $27, {|} $18, $18, $18, $18, $30, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $28, {|} $0C, $0C, $18, $18, $30, $30, $30, $30, $30, $30, $18, $18, $0C, $0C, $00, $00,
    $00, $29, {|} $30, $30, $18, $18, $0C, $0C, $0C, $0C, $0C, $0C, $18, $18, $30, $30, $00, $00,
    $00, $2A, {|} $00, $00, $66, $66, $3C, $3C, $FF, $FF, $3C, $3C, $66, $66, $00, $00, $00, $00,
    $00, $2B, {|} $00, $00, $18, $18, $18, $18, $7E, $7E, $18, $18, $18, $18, $00, $00, $00, $00,
    $00, $2C, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $18, $30, $30,
    $00, $2D, {|} $00, $00, $00, $00, $00, $00, $FF, $FF, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $2E, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $18, $00, $00,
    $00, $2F, {|} $03, $03, $06, $06, $0C, $0C, $18, $18, $30, $30, $60, $60, $C0, $C0, $80, $80,
    $00, $30, {|} $00, $00, $7C, $7C, $CE, $CE, $D6, $D6, $D6, $D6, $E6, $E6, $7C, $7C, $00, $00,
    $00, $31, {|} $00, $00, $18, $18, $38, $38, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $32, {|} $00, $00, $7C, $7C, $C6, $C6, $8C, $8C, $38, $38, $66, $66, $FE, $FE, $00, $00,
    $00, $33, {|} $20, $20, $7C, $7C, $C6, $C6, $1E, $1E, $06, $06, $C6, $C6, $7C, $7C, $00, $00,
    $00, $34, {|} $80, $80, $C6, $C6, $C6, $C6, $7E, $7E, $06, $06, $06, $06, $06, $06, $02, $02,
    $00, $35, {|} $00, $00, $FE, $FE, $C0, $C0, $FC, $FC, $06, $06, $C6, $C6, $FC, $FC, $00, $00,
    $00, $36, {|} $00, $00, $7C, $7C, $C0, $C0, $FC, $FC, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $37, {|} $80, $80, $FE, $FE, $CC, $CC, $98, $98, $30, $30, $60, $60, $C0, $C0, $00, $00,
    $00, $38, {|} $00, $00, $7C, $7C, $C6, $C6, $7C, $7C, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $39, {|} $00, $00, $7C, $7C, $C6, $C6, $C6, $C6, $7E, $7E, $06, $06, $7C, $7C, $00, $00,
    $00, $3A, {|} $00, $00, $18, $18, $18, $18, $00, $00, $00, $00, $18, $18, $18, $18, $00, $00,
    $00, $3B, {|} $00, $00, $18, $18, $18, $18, $00, $00, $00, $00, $18, $18, $18, $18, $30, $30,
    $00, $3C, {|} $18, $18, $30, $30, $60, $60, $C0, $C0, $60, $60, $30, $30, $18, $18, $00, $00,
    $00, $3D, {|} $00, $00, $00, $00, $FF, $FF, $00, $00, $00, $00, $FF, $FF, $00, $00, $00, $00,
    $00, $3E, {|} $30, $30, $18, $18, $0C, $0C, $06, $06, $0C, $0C, $18, $18, $30, $30, $00, $00,
    $00, $3F, {|} $00, $00, $3C, $3C, $66, $66, $0C, $0C, $18, $18, $00, $00, $18, $18, $10, $10,
    $00, $40, {|} $00, $00, $7C, $7C, $C6, $C6, $D6, $D6, $DC, $DC, $C0, $C0, $7E, $7E, $00, $00,
    $00, $41, {|} $00, $00, $7C, $7C, $C6, $C6, $DE, $DE, $C6, $C6, $C6, $C6, $C6, $C6, $80, $80,
    $00, $42, {|} $00, $00, $FC, $FC, $66, $66, $6C, $6C, $66, $66, $66, $66, $FC, $FC, $00, $00,
    $00, $43, {|} $00, $00, $7C, $7C, $C6, $C6, $C0, $C0, $C0, $C0, $C6, $C6, $7C, $7C, $08, $08,
    $00, $44, {|} $00, $00, $F0, $F0, $78, $78, $6C, $6C, $66, $66, $66, $66, $FC, $FC, $00, $00,
    $00, $45, {|} $00, $00, $FE, $FE, $60, $60, $7C, $7C, $60, $60, $66, $66, $FC, $FC, $08, $08,
    $00, $46, {|} $04, $04, $FE, $FE, $63, $63, $78, $78, $60, $60, $60, $60, $E0, $E0, $00, $00,
    $00, $47, {|} $08, $08, $7C, $7C, $C6, $C6, $C0, $C0, $CE, $CE, $C6, $C6, $7E, $7F, $02, $02,
    $00, $48, {|} $00, $00, $C6, $C6, $C6, $C6, $DE, $DE, $C6, $C6, $C6, $C6, $C6, $C6, $80, $80,
    $00, $49, {|} $00, $00, $7E, $7E, $18, $18, $18, $18, $18, $18, $18, $18, $7E, $7E, $00, $00,
    $00, $4A, {|} $02, $02, $06, $06, $06, $06, $06, $06, $E6, $E6, $C6, $C6, $FC, $FC, $80, $80,
    $00, $4B, {|} $00, $00, $C6, $C6, $CC, $CC, $D8, $D8, $CC, $CC, $C6, $C6, $C6, $C6, $80, $80,
    $00, $4C, {|} $00, $00, $E0, $E0, $60, $60, $60, $60, $60, $60, $66, $66, $FC, $FC, $08, $08,
    $00, $4D, {|} $00, $00, $C6, $C6, $EE, $EE, $FE, $FE, $D6, $D6, $C6, $C6, $C6, $C6, $80, $80,
    $00, $4E, {|} $00, $00, $C6, $C6, $D6, $D6, $DE, $DE, $CE, $CE, $C6, $C6, $C6, $C6, $00, $00,
    $00, $4F, {|} $00, $00, $7C, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $50, {|} $00, $00, $FC, $FC, $66, $66, $66, $66, $6C, $6C, $60, $60, $E0, $E0, $00, $00,
    $00, $51, {|} $00, $00, $7C, $7C, $C6, $C6, $C6, $C6, $D6, $D6, $CA, $CA, $74, $74, $02, $02,
    $00, $52, {|} $00, $00, $FC, $FC, $66, $66, $6C, $6C, $66, $66, $66, $66, $E6, $E6, $04, $04,
    $00, $53, {|} $00, $00, $7E, $7E, $C0, $C0, $7C, $7C, $06, $06, $C6, $C6, $FC, $FC, $80, $80,
    $00, $54, {|} $08, $08, $FC, $FC, $36, $36, $30, $30, $30, $30, $30, $30, $30, $30, $20, $20,
    $00, $55, {|} $80, $80, $CE, $CE, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $7E, $7E, $00, $00,
    $00, $56, {|} $80, $80, $C6, $C6, $C6, $C6, $6C, $6C, $6C, $6C, $38, $38, $10, $10, $00, $00,
    $00, $57, {|} $80, $80, $C6, $C6, $C6, $C6, $D6, $D6, $FE, $FE, $EE, $EE, $C6, $C6, $00, $00,
    $00, $58, {|} $80, $80, $C6, $C6, $C6, $C6, $5C, $5C, $C6, $C6, $C6, $C6, $C6, $C6, $02, $02,
    $00, $59, {|} $02, $02, $C6, $C6, $C6, $C6, $5C, $5C, $18, $18, $18, $18, $18, $18, $08, $08,
    $00, $5A, {|} $00, $00, $FE, $FE, $0C, $0C, $18, $18, $32, $32, $66, $66, $FE, $FE, $02, $02,
    $00, $5B, {|} $00, $00, $7E, $7E, $30, $30, $30, $30, $30, $30, $30, $30, $7E, $7E, $00, $00,
    $00, $5C, {|} $C0, $C0, $60, $60, $30, $30, $18, $18, $0C, $0C, $06, $06, $03, $03, $01, $01,
    $00, $5D, {|} $00, $00, $7E, $7E, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $7E, $7E, $00, $00,
    $00, $5E, {|} $18, $18, $3C, $3C, $66, $66, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $5F, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $FF,
    $00, $60, {|} $18, $18, $18, $18, $0C, $0C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $61, {|} $00, $00, $00, $00, $7C, $7C, $06, $06, $76, $76, $C6, $C6, $7E, $7E, $00, $00,
    $00, $62, {|} $E0, $E0, $60, $60, $6C, $6C, $66, $66, $66, $66, $66, $66, $FC, $FC, $00, $00,
    $00, $63, {|} $00, $00, $00, $00, $7E, $7E, $C2, $C2, $C0, $C0, $C2, $C2, $7E, $7E, $00, $00,
    $00, $64, {|} $0E, $0E, $0C, $0C, $6C, $6C, $CC, $CC, $CC, $CC, $CC, $CC, $7E, $7E, $00, $00,
    $00, $65, {|} $00, $00, $00, $00, $7C, $7C, $C6, $C6, $DC, $DC, $C0, $C0, $7E, $7E, $00, $00,
    $00, $66, {|} $1E, $1E, $30, $30, $7C, $7C, $30, $30, $30, $30, $30, $30, $30, $30, $20, $20,
    $00, $67, {|} $00, $00, $00, $00, $7C, $7C, $C6, $C6, $C6, $C6, $76, $76, $06, $06, $FC, $FC,
    $00, $68, {|} $E0, $E0, $60, $60, $6C, $6C, $66, $66, $66, $66, $66, $66, $E6, $E6, $00, $00,
    $00, $69, {|} $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $10, $10,
    $00, $6A, {|} $02, $02, $06, $06, $00, $00, $06, $06, $06, $06, $C6, $C6, $C6, $C6, $7C, $7C,
    $00, $6B, {|} $E0, $E0, $60, $60, $66, $66, $6C, $6C, $78, $78, $6C, $6C, $E6, $E6, $02, $02,
    $00, $6C, {|} $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $10, $10,
    $00, $6D, {|} $00, $00, $00, $00, $FC, $FC, $D6, $D6, $D6, $D6, $D6, $D6, $D6, $D6, $80, $80,
    $00, $6E, {|} $00, $00, $00, $00, $FC, $FC, $66, $66, $66, $66, $66, $66, $E6, $E6, $04, $04,
    $00, $6F, {|} $00, $00, $00, $00, $7C, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $70, {|} $00, $00, $00, $00, $FC, $FC, $66, $66, $66, $66, $6C, $6C, $60, $60, $E0, $E0,
    $00, $71, {|} $00, $00, $00, $00, $7E, $7E, $CC, $CC, $CC, $CC, $6C, $6C, $0C, $0C, $0E, $0E,
    $00, $72, {|} $00, $00, $08, $08, $FC, $FC, $66, $66, $60, $60, $60, $60, $60, $60, $40, $40,
    $00, $73, {|} $00, $00, $00, $00, $7E, $7E, $C0, $C0, $7C, $7C, $06, $06, $FC, $FC, $00, $00,
    $00, $74, {|} $30, $30, $30, $30, $7C, $7C, $30, $30, $30, $30, $30, $30, $30, $30, $20, $20,
    $00, $75, {|} $00, $00, $80, $80, $CE, $CE, $CC, $CC, $CC, $CC, $CC, $CC, $7E, $7E, $00, $00,
    $00, $76, {|} $00, $00, $80, $80, $C6, $C6, $C6, $C6, $6C, $6C, $6C, $6C, $38, $38, $00, $00,
    $00, $77, {|} $00, $00, $02, $02, $D6, $D6, $D6, $D6, $D6, $D6, $D6, $D6, $7C, $7C, $00, $00,
    $00, $78, {|} $00, $00, $00, $00, $C6, $C6, $C6, $C6, $5C, $5C, $C6, $C6, $C6, $C6, $02, $02,
    $00, $79, {|} $00, $00, $80, $80, $C6, $C6, $C6, $C6, $C6, $C6, $5E, $5E, $06, $06, $7C, $7C,
    $00, $7A, {|} $00, $00, $00, $00, $FE, $FE, $8C, $8C, $18, $18, $32, $32, $FE, $FE, $00, $00,
    $00, $7B, {|} $1C, $1C, $30, $30, $30, $30, $60, $60, $30, $30, $30, $30, $1C, $1C, $00, $00,
    $00, $7C, {|} $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18,
    $00, $7D, {|} $38, $38, $0C, $0C, $0C, $0C, $06, $06, $0C, $0C, $0C, $0C, $38, $38, $00, $00,
    $00, $7E, {|} $76, $76, $DC, $DC, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $7F, {|} $CC, $CC, $33, $33, $CC, $CC, $33, $33, $CC, $CC, $33, $33, $CC, $CC, $33, $33,
    $00, $80, {|} $9F, $9F, $E7, $E7, $83, $83, $39, $39, $21, $21, $39, $39, $39, $39, $7B, $7B,
    $00, $81, {|} $F3, $F3, $CF, $CF, $83, $83, $39, $39, $21, $21, $39, $39, $39, $39, $7B, $7B,
    $00, $82, {|} $83, $83, $39, $39, $83, $83, $39, $39, $21, $21, $39, $39, $39, $39, $7B, $7B,
    $00, $83, {|} $89, $89, $23, $23, $83, $83, $39, $39, $21, $21, $39, $39, $39, $39, $7B, $7B,
    $00, $84, {|} $39, $39, $FF, $FF, $83, $83, $39, $39, $21, $21, $39, $39, $39, $39, $7B, $7B,
    $00, $85, {|} $83, $83, $39, $39, $83, $83, $39, $39, $21, $21, $39, $39, $39, $39, $7B, $7B,
    $00, $86, {|} $FF, $FF, $80, $80, $33, $33, $20, $20, $33, $33, $33, $33, $30, $30, $77, $77,
    $00, $87, {|} $FF, $FF, $83, $83, $39, $39, $3F, $3F, $3F, $3F, $39, $39, $83, $83, $CF, $CF,
    $00, $88, {|} $9F, $9F, $CF, $CF, $03, $03, $9F, $9F, $83, $83, $9F, $9F, $01, $01, $FF, $FF,
    $00, $89, {|} $E7, $E7, $CF, $CF, $03, $03, $9F, $9F, $83, $83, $9F, $9F, $01, $01, $FF, $FF,
    $00, $8A, {|} $CF, $CF, $33, $33, $03, $03, $9F, $9F, $83, $83, $9F, $9F, $01, $01, $FF, $FF,
    $00, $8B, {|} $33, $33, $FF, $FF, $03, $03, $9F, $9F, $83, $83, $9F, $9F, $01, $01, $FF, $FF,
    $00, $8C, {|} $3F, $3F, $CF, $CF, $03, $03, $CF, $CF, $CF, $CF, $CF, $CF, $03, $03, $FF, $FF,
    $00, $8D, {|} $F3, $F3, $CF, $CF, $03, $03, $CF, $CF, $CF, $CF, $CF, $CF, $03, $03, $FF, $FF,
    $00, $8E, {|} $CF, $CF, $33, $33, $FF, $FF, $03, $03, $CF, $CF, $CF, $CF, $03, $03, $FF, $FF,
    $00, $8F, {|} $33, $33, $FF, $FF, $03, $03, $CF, $CF, $CF, $CF, $CF, $CF, $03, $03, $FF, $FF,
    $00, $90, {|} $FF, $FF, $1F, $1F, $8F, $8F, $87, $87, $13, $13, $99, $99, $03, $03, $FF, $FF,
    $00, $91, {|} $89, $89, $23, $23, $19, $19, $09, $09, $21, $21, $31, $31, $39, $39, $FF, $FF,
    $00, $92, {|} $CF, $CF, $E7, $E7, $83, $83, $39, $39, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $93, {|} $E7, $E7, $CF, $CF, $83, $83, $39, $39, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $94, {|} $CF, $CF, $33, $33, $FF, $FF, $87, $87, $33, $33, $33, $33, $87, $87, $FF, $FF,
    $00, $95, {|} $89, $89, $23, $23, $FF, $FF, $83, $83, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $96, {|} $39, $39, $FF, $FF, $83, $83, $39, $39, $39, $39, $39, $39, $83, $83, $FF, $FF,
    $00, $97, {|} $FF, $FF, $FF, $FF, $3C, $3C, $99, $99, $E7, $E7, $99, $99, $3C, $3C, $FF, $FF,
    $00, $98, {|} $FE, $FE, $85, $85, $3B, $3B, $35, $35, $29, $29, $59, $59, $A3, $A3, $7F, $7F,
    $00, $99, {|} $CF, $CF, $E7, $E7, $7F, $7F, $31, $31, $33, $33, $33, $33, $81, $81, $FF, $FF,
    $00, $9A, {|} $E7, $E7, $CF, $CF, $7F, $7F, $31, $31, $33, $33, $33, $33, $81, $81, $FF, $FF,
    $00, $9B, {|} $CF, $CF, $33, $33, $7F, $7F, $31, $31, $33, $33, $33, $33, $81, $81, $FF, $FF,
    $00, $9C, {|} $FF, $FF, $33, $33, $7F, $7F, $31, $31, $33, $33, $33, $33, $81, $81, $FF, $FF,
    $00, $9D, {|} $F3, $F3, $E7, $E7, $3C, $3C, $99, $99, $C3, $C3, $E7, $E7, $C3, $C3, $FF, $FF,
    $00, $9E, {|} $0F, $0F, $9F, $9F, $83, $83, $99, $99, $83, $83, $9F, $9F, $9F, $9F, $0F, $0F,
    $00, $9F, {|} $83, $83, $39, $39, $39, $39, $23, $23, $39, $39, $39, $39, $23, $23, $3F, $3F,
    $00, $A0, {|} $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $00, $00,
    $00, $A1, {|} $10, $10, $18, $18, $00, $00, $18, $18, $18, $18, $1C, $1C, $1C, $1C, $00, $00,
    $00, $A2, {|} $00, $00, $18, $18, $7E, $7E, $C0, $C0, $C0, $C0, $C0, $C0, $7E, $7E, $18, $18,
    $00, $A3, {|} $08, $08, $3C, $3C, $66, $66, $F8, $F8, $60, $60, $66, $66, $FC, $FC, $08, $08,
    $00, $A4, {|} $C6, $C6, $7C, $7C, $C6, $C6, $C6, $C6, $7C, $7C, $C6, $C6, $00, $00, $00, $00,
    $00, $A5, {|} $C6, $C6, $C6, $C6, $6C, $6C, $38, $38, $10, $10, $FE, $FE, $10, $10, $10, $10,
    $00, $A6, {|} $18, $18, $18, $18, $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $A7, {|} $7C, $7C, $C0, $C0, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $03, $03, $3E, $3E,
    $00, $A8, {|} $24, $24, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $A9, {|} $7C, $7C, $82, $82, $9A, $9A, $A2, $A2, $A2, $A2, $9A, $9A, $82, $82, $7C, $7C,
    $00, $AA, {|} $00, $00, $78, $78, $CC, $CC, $CC, $CC, $F6, $F6, $00, $00, $FE, $FE, $00, $00,
    $00, $AB, {|} $00, $00, $12, $12, $6C, $6C, $D8, $D8, $6C, $6C, $12, $12, $00, $00, $00, $00,
    $00, $AC, {|} $FE, $FE, $06, $06, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $AD, {|} $00, $00, $00, $00, $7E, $7E, $7E, $7E, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $AE, {|} $7C, $7C, $82, $82, $B2, $B2, $AA, $AA, $B2, $B2, $AA, $AA, $82, $82, $7C, $7C,
    $00, $AF, {|} $FF, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B0, {|} $78, $78, $CC, $CC, $78, $78, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B1, {|} $30, $30, $30, $30, $FC, $FC, $30, $30, $30, $30, $00, $00, $FC, $FC, $00, $00,
    $00, $B2, {|} $70, $70, $D8, $D8, $30, $30, $60, $60, $F8, $F8, $00, $00, $00, $00, $00, $00,
    $00, $B3, {|} $F0, $F0, $18, $18, $70, $70, $18, $18, $F0, $F0, $00, $00, $00, $00, $00, $00,
    $00, $B4, {|} $30, $30, $60, $60, $C0, $C0, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B5, {|} $00, $00, $00, $00, $80, $80, $C6, $C6, $C6, $C6, $C6, $C6, $FF, $FF, $C0, $C0,
    $00, $B6, {|} $7E, $7E, $EC, $EC, $EC, $EC, $6C, $6C, $2C, $2C, $2C, $2C, $2E, $2E, $00, $00,
    $00, $B7, {|} $00, $00, $00, $00, $18, $18, $18, $18, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B8, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $30, $30,
    $00, $B9, {|} $60, $60, $E0, $E0, $60, $60, $60, $60, $F0, $F0, $00, $00, $00, $00, $00, $00,
    $00, $BA, {|} $00, $00, $78, $78, $CC, $CC, $78, $78, $00, $00, $FC, $FC, $00, $00, $00, $00,
    $00, $BB, {|} $00, $00, $48, $48, $36, $36, $1B, $1B, $36, $36, $48, $48, $00, $00, $00, $00,
    $00, $BC, {|} $63, $63, $E6, $E6, $6C, $6C, $7B, $7B, $37, $37, $6F, $6F, $C3, $C3, $00, $00,
    $00, $BD, {|} $63, $63, $E6, $E6, $6C, $6C, $7E, $7E, $33, $33, $66, $66, $CF, $CF, $00, $00,
    $00, $BE, {|} $C3, $C3, $E6, $E6, $6C, $6C, $DB, $DB, $37, $37, $6F, $6F, $C3, $C3, $00, $00,
    $00, $BF, {|} $08, $08, $18, $18, $00, $00, $18, $18, $30, $30, $66, $66, $3C, $3C, $00, $00,
    $00, $C0, {|} $60, $60, $18, $18, $7C, $7C, $C6, $C6, $DE, $DE, $C6, $C6, $C6, $C6, $84, $84,
    $00, $C1, {|} $0C, $0C, $30, $30, $7C, $7C, $C6, $C6, $DE, $DE, $C6, $C6, $C6, $C6, $84, $84,
    $00, $C2, {|} $7C, $7C, $C6, $C6, $7C, $7C, $C6, $C6, $DE, $DE, $C6, $C6, $C6, $C6, $84, $84,
    $00, $C3, {|} $76, $76, $DC, $DC, $7C, $7C, $C6, $C6, $DE, $DE, $C6, $C6, $C6, $C6, $84, $84,
    $00, $C4, {|} $C6, $C6, $00, $00, $7C, $7C, $C6, $C6, $DE, $DE, $C6, $C6, $C6, $C6, $84, $84,
    $00, $C5, {|} $7C, $7C, $C6, $C6, $7C, $7C, $C6, $C6, $DE, $DE, $C6, $C6, $C6, $C6, $84, $84,
    $00, $C6, {|} $00, $00, $7F, $7F, $CC, $CC, $DF, $DF, $CC, $CC, $CC, $CC, $CF, $CF, $88, $88,
    $00, $C7, {|} $00, $00, $7C, $7C, $C6, $C6, $C0, $C0, $C0, $C0, $C6, $C6, $7C, $7C, $30, $30,
    $00, $C8, {|} $60, $60, $30, $30, $FC, $FC, $60, $60, $7C, $7C, $60, $60, $FE, $FE, $00, $00,
    $00, $C9, {|} $18, $18, $30, $30, $FC, $FC, $60, $60, $7C, $7C, $60, $60, $FE, $FE, $00, $00,
    $00, $CA, {|} $30, $30, $CC, $CC, $FC, $FC, $60, $60, $7C, $7C, $60, $60, $FE, $FE, $00, $00,
    $00, $CB, {|} $CC, $CC, $00, $00, $FC, $FC, $60, $60, $7C, $7C, $60, $60, $FE, $FE, $00, $00,
    $00, $CC, {|} $C0, $C0, $30, $30, $FC, $FC, $30, $30, $30, $30, $30, $30, $FC, $FC, $00, $00,
    $00, $CD, {|} $0C, $0C, $30, $30, $FC, $FC, $30, $30, $30, $30, $30, $30, $FC, $FC, $00, $00,
    $00, $CE, {|} $30, $30, $CC, $CC, $00, $00, $FC, $FC, $30, $30, $30, $30, $FC, $FC, $00, $00,
    $00, $CF, {|} $CC, $CC, $00, $00, $FC, $FC, $30, $30, $30, $30, $30, $30, $FC, $FC, $00, $00,
    $00, $D0, {|} $00, $00, $E0, $E0, $70, $70, $78, $78, $EC, $EC, $66, $66, $FC, $FC, $00, $00,
    $00, $D1, {|} $76, $76, $DC, $DC, $E6, $E6, $F6, $F6, $DE, $DE, $CE, $CE, $C6, $C6, $00, $00,
    $00, $D2, {|} $30, $30, $18, $18, $7C, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $D3, {|} $18, $18, $30, $30, $7C, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $D4, {|} $30, $30, $CC, $CC, $00, $00, $78, $78, $CC, $CC, $CC, $CC, $78, $78, $00, $00,
    $00, $D5, {|} $76, $76, $DC, $DC, $00, $00, $7C, $7C, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $D6, {|} $C6, $C6, $00, $00, $7C, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $D7, {|} $00, $00, $00, $00, $C3, $C3, $66, $66, $18, $18, $66, $66, $C3, $C3, $00, $00,
    $00, $D8, {|} $01, $01, $7A, $7A, $C4, $C4, $CA, $CA, $D6, $D6, $A6, $A6, $5C, $5C, $80, $80,
    $00, $D9, {|} $30, $30, $18, $18, $80, $80, $CE, $CE, $CC, $CC, $CC, $CC, $7E, $7E, $00, $00,
    $00, $DA, {|} $18, $18, $30, $30, $80, $80, $CE, $CE, $CC, $CC, $CC, $CC, $7E, $7E, $00, $00,
    $00, $DB, {|} $30, $30, $CC, $CC, $80, $80, $CE, $CE, $CC, $CC, $CC, $CC, $7E, $7E, $00, $00,
    $00, $DC, {|} $00, $00, $CC, $CC, $80, $80, $CE, $CE, $CC, $CC, $CC, $CC, $7E, $7E, $00, $00,
    $00, $DD, {|} $0C, $0C, $18, $18, $C3, $C3, $66, $66, $3C, $3C, $18, $18, $3C, $3C, $00, $00,
    $00, $DE, {|} $F0, $F0, $60, $60, $7C, $7C, $66, $66, $7C, $7C, $60, $60, $60, $60, $F0, $F0,
    $00, $DF, {|} $7C, $7C, $C6, $C6, $C6, $C6, $DC, $DC, $C6, $C6, $C6, $C6, $DC, $DC, $C0, $C0,
    $00, $E0, {|} $30, $30, $18, $18, $7C, $7C, $06, $06, $76, $76, $C6, $C6, $7E, $7E, $00, $00,
    $00, $E1, {|} $18, $18, $30, $30, $7C, $7C, $06, $06, $76, $76, $C6, $C6, $7E, $7E, $00, $00,
    $00, $E2, {|} $18, $18, $66, $66, $7C, $7C, $06, $06, $76, $76, $C6, $C6, $7E, $7E, $00, $00,
    $00, $E3, {|} $76, $76, $DC, $DC, $7C, $7C, $06, $06, $76, $76, $C6, $C6, $7E, $7E, $00, $00,
    $00, $E4, {|} $C6, $C6, $00, $00, $7C, $7C, $06, $06, $76, $76, $C6, $C6, $7E, $7E, $00, $00,
    $00, $E5, {|} $7C, $7C, $C6, $C6, $7C, $7C, $06, $06, $76, $76, $C6, $C6, $7E, $7E, $00, $00,
    $00, $E6, {|} $00, $00, $00, $00, $7E, $7E, $1B, $1B, $7E, $7E, $C8, $C8, $7F, $7F, $00, $00,
    $00, $E7, {|} $00, $00, $00, $00, $7C, $7C, $C6, $C6, $C0, $C0, $C6, $C6, $7C, $7C, $30, $30,
    $00, $E8, {|} $30, $30, $18, $18, $7C, $7C, $C6, $C6, $DC, $DC, $C0, $C0, $7E, $7E, $00, $00,
    $00, $E9, {|} $18, $18, $30, $30, $7C, $7C, $C6, $C6, $DC, $DC, $C0, $C0, $7E, $7E, $00, $00,
    $00, $EA, {|} $18, $18, $66, $66, $7C, $7C, $C6, $C6, $DC, $DC, $C0, $C0, $7E, $7E, $00, $00,
    $00, $EB, {|} $C6, $C6, $00, $00, $7C, $7C, $C6, $C6, $DC, $DC, $C0, $C0, $7E, $7E, $00, $00,
    $00, $EC, {|} $30, $30, $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $ED, {|} $0C, $0C, $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $EE, {|} $18, $18, $66, $66, $00, $00, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $EF, {|} $00, $00, $66, $66, $00, $00, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $F0, {|} $30, $30, $7E, $7E, $18, $18, $7C, $7C, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $F1, {|} $76, $76, $DC, $DC, $00, $00, $FC, $FC, $C6, $C6, $C6, $C6, $C6, $C6, $02, $02,
    $00, $F2, {|} $30, $30, $18, $18, $00, $00, $7C, $7C, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $F3, {|} $18, $18, $30, $30, $00, $00, $7C, $7C, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $F4, {|} $18, $18, $66, $66, $00, $00, $7C, $7C, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $F5, {|} $76, $76, $DC, $DC, $00, $00, $7C, $7C, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $F6, {|} $00, $00, $6C, $6C, $00, $00, $7C, $7C, $C6, $C6, $C6, $C6, $7C, $7C, $00, $00,
    $00, $F7, {|} $00, $00, $18, $18, $00, $00, $7E, $7E, $00, $00, $18, $18, $00, $00, $00, $00,
    $00, $F8, {|} $00, $00, $03, $03, $3E, $3E, $6E, $6E, $7E, $7E, $76, $76, $7C, $7C, $C0, $C0,
    $00, $F9, {|} $76, $76, $DC, $DC, $80, $80, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $FA, {|} $0C, $0C, $18, $18, $80, $80, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $FB, {|} $18, $18, $66, $66, $80, $80, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $FC, {|} $00, $00, $C6, $C6, $80, $80, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $7E, $00, $00,
    $00, $FD, {|} $0C, $0C, $18, $18, $80, $80, $C6, $C6, $C6, $C6, $6C, $6C, $38, $38, $F0, $F0,
    $00, $FE, {|} $F0, $F0, $60, $60, $7C, $7C, $66, $66, $66, $66, $7C, $7C, $60, $60, $F0, $F0,
    $00, $FF, {|} $00, $00, $C6, $C6, $80, $80, $C6, $C6, $C6, $C6, $6C, $6C, $38, $38, $F0, $F0
  );
  //P0T_NOoDLEMirrors = packed array [0..(0*3)-1] of Uint16;

  // Topaz Plus
  TopazPlus_COUNT = 256;  // number of glyphs
  TopazPlus : packed array [0..TopazPlus_COUNT * 18 - 1] of byte = (
    $00, $00, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $01, {|} $83, $83, $39, $39, $21, $21, $29, $29, $21, $21, $3F, $3F, $87, $87, $FF, $FF,
    $00, $02, {|} $C3, $C3, $99, $99, $99, $99, $81, $81, $99, $99, $99, $99, $99, $99, $FF, $FF,
    $00, $03, {|} $83, $83, $99, $99, $99, $99, $83, $83, $99, $99, $99, $99, $83, $83, $FF, $FF,
    $00, $04, {|} $E1, $E1, $CF, $CF, $9F, $9F, $9F, $9F, $9F, $9F, $CF, $CF, $E1, $E1, $FF, $FF,
    $00, $05, {|} $87, $87, $93, $93, $99, $99, $99, $99, $99, $99, $93, $93, $87, $87, $FF, $FF,
    $00, $06, {|} $81, $81, $9F, $9F, $9F, $9F, $87, $87, $9F, $9F, $9F, $9F, $81, $81, $FF, $FF,
    $00, $07, {|} $81, $81, $9F, $9F, $9F, $9F, $87, $87, $9F, $9F, $9F, $9F, $9F, $9F, $FF, $FF,
    $00, $08, {|} $C3, $C3, $99, $99, $9F, $9F, $91, $91, $99, $99, $99, $99, $C1, $C1, $FF, $FF,
    $00, $09, {|} $99, $99, $99, $99, $99, $99, $81, $81, $99, $99, $99, $99, $99, $99, $FF, $FF,
    $00, $0A, {|} $C3, $C3, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $C3, $C3, $FF, $FF,
    $00, $0B, {|} $F9, $F9, $F9, $F9, $F9, $F9, $F9, $F9, $F9, $F9, $99, $99, $C3, $C3, $FF, $FF,
    $00, $0C, {|} $9C, $9C, $99, $99, $93, $93, $87, $87, $93, $93, $99, $99, $9C, $9C, $FF, $FF,
    $00, $0D, {|} $9F, $9F, $9F, $9F, $9F, $9F, $9F, $9F, $9F, $9F, $9F, $9F, $81, $81, $FF, $FF,
    $00, $0E, {|} $9C, $9C, $88, $88, $80, $80, $94, $94, $9C, $9C, $9C, $9C, $9C, $9C, $FF, $FF,
    $00, $0F, {|} $9C, $9C, $8C, $8C, $84, $84, $90, $90, $98, $98, $9C, $9C, $9C, $9C, $FF, $FF,
    $00, $10, {|} $C3, $C3, $99, $99, $99, $99, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $11, {|} $83, $83, $99, $99, $99, $99, $83, $83, $9F, $9F, $9F, $9F, $9F, $9F, $FF, $FF,
    $00, $12, {|} $C3, $C3, $99, $99, $99, $99, $99, $99, $99, $99, $91, $91, $C0, $C0, $FF, $FF,
    $00, $13, {|} $83, $83, $99, $99, $99, $99, $83, $83, $93, $93, $99, $99, $99, $99, $FF, $FF,
    $00, $14, {|} $C3, $C3, $99, $99, $8F, $8F, $C3, $C3, $F1, $F1, $99, $99, $C3, $C3, $FF, $FF,
    $00, $15, {|} $81, $81, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $FF, $FF,
    $00, $16, {|} $99, $99, $99, $99, $99, $99, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $17, {|} $99, $99, $99, $99, $99, $99, $99, $99, $99, $81, $C3, $C3, $E7, $E7, $FF, $FF,
    $00, $18, {|} $9C, $9C, $9C, $9C, $9C, $9C, $94, $94, $80, $80, $88, $88, $9C, $9C, $FF, $FF,
    $00, $19, {|} $3C, $3C, $99, $99, $C3, $C3, $E7, $E7, $C3, $C3, $99, $99, $3C, $3C, $FF, $FF,
    $00, $1A, {|} $3C, $3C, $99, $99, $C3, $C3, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $FF, $FF,
    $00, $1B, {|} $80, $80, $F9, $F9, $F3, $F3, $E7, $E7, $CF, $CF, $9F, $9F, $80, $80, $FF, $FF,
    $00, $1C, {|} $C3, $C0, $CF, $CF, $CF, $CF, $CF, $CF, $CF, $CF, $CF, $CF, $C0, $C3, $FF, $FF,
    $00, $1D, {|} $3F, $3F, $9F, $9F, $CF, $CF, $E7, $E7, $F3, $F3, $F9, $F9, $FC, $FC, $FE, $FE,
    $00, $1E, {|} $C3, $03, $F3, $F3, $F3, $F3, $F3, $F3, $F3, $F3, $F3, $F3, $03, $C3, $FF, $FF,
    $00, $1F, {|} $F7, $F7, $E3, $E3, $C9, $C9, $9C, $9C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $00, $20, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $21, {|} $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00, $18, $18, $00, $00,
    $00, $22, {|} $6C, $6C, $6C, $6C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $23, {|} $6C, $6C, $6C, $6C, $FE, $FE, $6C, $6C, $FE, $FE, $6C, $6C, $6C, $6C, $00, $00,
    $00, $24, {|} $18, $18, $3E, $3E, $60, $60, $3C, $3C, $06, $06, $7C, $7C, $18, $18, $00, $00,
    $00, $25, {|} $03, $03, $66, $66, $AC, $AC, $D8, $D8, $36, $36, $6A, $6A, $CC, $CC, $00, $00,
    $00, $26, {|} $38, $38, $6C, $6C, $68, $68, $76, $76, $DC, $DC, $CE, $CE, $7B, $7B, $00, $00,
    $00, $27, {|} $18, $18, $18, $18, $30, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $28, {|} $03, $07, $0E, $0C, $18, $18, $18, $18, $18, $18, $0C, $0E, $07, $03, $00, $00,
    $00, $29, {|} $C0, $E0, $70, $30, $18, $18, $18, $18, $18, $18, $30, $70, $E0, $C0, $00, $00,
    $00, $2A, {|} $00, $00, $66, $66, $3C, $3C, $FF, $FF, $3C, $3C, $66, $66, $00, $00, $00, $00,
    $00, $2B, {|} $00, $00, $18, $18, $18, $18, $7E, $7E, $18, $18, $18, $18, $00, $00, $00, $00,
    $00, $2C, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $18, $30, $30,
    $00, $2D, {|} $00, $00, $00, $00, $00, $00, $FF, $FF, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $2E, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $18, $00, $00,
    $00, $2F, {|} $03, $03, $06, $06, $0C, $0C, $18, $18, $30, $30, $60, $60, $C0, $C0, $80, $80,
    $00, $30, {|} $3C, $3C, $66, $66, $6E, $6E, $7E, $7E, $76, $76, $66, $66, $3C, $3C, $00, $00,
    $00, $31, {|} $18, $18, $38, $38, $78, $78, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $32, {|} $3C, $3C, $66, $66, $06, $06, $0C, $0C, $18, $18, $30, $30, $7E, $7E, $00, $00,
    $00, $33, {|} $3C, $3C, $66, $66, $06, $06, $1C, $1C, $06, $06, $66, $66, $3C, $3C, $00, $00,
    $00, $34, {|} $1C, $1C, $3C, $3C, $6C, $6C, $CC, $CC, $FE, $FE, $0C, $0C, $0C, $0C, $00, $00,
    $00, $35, {|} $7E, $7E, $60, $60, $7C, $7C, $06, $06, $06, $06, $66, $66, $3C, $3C, $00, $00,
    $00, $36, {|} $1C, $1C, $30, $30, $60, $60, $7C, $7C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $37, {|} $7E, $7E, $06, $06, $06, $06, $0C, $0C, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $38, {|} $3C, $3C, $66, $66, $66, $66, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $39, {|} $3C, $3C, $66, $66, $66, $66, $3E, $3E, $06, $06, $0C, $0C, $38, $38, $00, $00,
    $00, $3A, {|} $00, $00, $18, $18, $18, $18, $00, $00, $00, $00, $18, $18, $18, $18, $00, $00,
    $00, $3B, {|} $00, $00, $18, $18, $18, $18, $00, $00, $00, $00, $18, $18, $18, $18, $30, $30,
    $00, $3C, {|} $03, $03, $06, $06, $0C, $0C, $18, $18, $0C, $0C, $06, $06, $03, $03, $00, $00,
    $00, $3D, {|} $00, $00, $00, $00, $FF, $FF, $00, $00, $FF, $FF, $00, $00, $00, $00, $00, $00,
    $00, $3E, {|} $C0, $C0, $60, $60, $30, $30, $18, $18, $30, $30, $60, $60, $C0, $C0, $00, $00,
    $00, $3F, {|} $3C, $3C, $66, $66, $06, $06, $0C, $0C, $18, $18, $00, $00, $18, $18, $00, $00,
    $00, $40, {|} $7C, $7C, $C6, $C6, $DE, $DE, $D6, $D6, $DE, $DE, $C0, $C0, $78, $78, $00, $00,
    $00, $41, {|} $3C, $3C, $66, $66, $66, $66, $7E, $7E, $66, $66, $66, $66, $66, $66, $00, $00,
    $00, $42, {|} $7C, $7C, $66, $66, $66, $66, $7C, $7C, $66, $66, $66, $66, $7C, $7C, $00, $00,
    $00, $43, {|} $1E, $1E, $30, $30, $60, $60, $60, $60, $60, $60, $30, $30, $1E, $1E, $00, $00,
    $00, $44, {|} $78, $78, $6C, $6C, $66, $66, $66, $66, $66, $66, $6C, $6C, $78, $78, $00, $00,
    $00, $45, {|} $7E, $7E, $60, $60, $60, $60, $78, $78, $60, $60, $60, $60, $7E, $7E, $00, $00,
    $00, $46, {|} $7E, $7E, $60, $60, $60, $60, $78, $78, $60, $60, $60, $60, $60, $60, $00, $00,
    $00, $47, {|} $3C, $3C, $66, $66, $60, $60, $6E, $6E, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $48, {|} $66, $66, $66, $66, $66, $66, $7E, $7E, $66, $66, $66, $66, $66, $66, $00, $00,
    $00, $49, {|} $3C, $3C, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $3C, $00, $00,
    $00, $4A, {|} $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $66, $66, $3C, $3C, $00, $00,
    $00, $4B, {|} $C6, $C6, $CC, $CC, $D8, $D8, $F0, $F0, $D8, $D8, $CC, $CC, $C6, $C6, $00, $00,
    $00, $4C, {|} $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $7E, $7E, $00, $00,
    $00, $4D, {|} $C6, $C6, $EE, $EE, $FE, $FE, $D6, $D6, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $4E, {|} $C6, $C6, $E6, $E6, $F6, $F6, $DE, $DE, $CE, $CE, $C6, $C6, $C6, $C6, $00, $00,
    $00, $4F, {|} $3C, $3C, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $50, {|} $7C, $7C, $66, $66, $66, $66, $7C, $7C, $60, $60, $60, $60, $60, $60, $00, $00,
    $00, $51, {|} $78, $78, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $DC, $DC, $7E, $7E, $00, $00,
    $00, $52, {|} $7C, $7C, $66, $66, $66, $66, $7C, $7C, $6C, $6C, $66, $66, $66, $66, $00, $00,
    $00, $53, {|} $3C, $3C, $66, $66, $70, $70, $3C, $3C, $0E, $0E, $66, $66, $3C, $3C, $00, $00,
    $00, $54, {|} $7E, $7E, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $55, {|} $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $56, {|} $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $3C, $3C, $18, $18, $00, $00,
    $00, $57, {|} $C6, $C6, $C6, $C6, $C6, $C6, $D6, $D6, $FE, $FE, $EE, $EE, $C6, $C6, $00, $00,
    $00, $58, {|} $C3, $C3, $66, $66, $3C, $3C, $18, $18, $3C, $3C, $66, $66, $C3, $C3, $00, $00,
    $00, $59, {|} $C3, $C3, $66, $66, $3C, $3C, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $5A, {|} $FE, $FE, $0C, $0C, $18, $18, $30, $30, $60, $60, $C0, $C0, $FE, $FE, $00, $00,
    $00, $5B, {|} $3C, $3F, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $3F, $3C, $00, $00,
    $00, $5C, {|} $C0, $C0, $60, $60, $30, $30, $18, $18, $0C, $0C, $06, $06, $03, $03, $01, $01,
    $00, $5D, {|} $3C, $FC, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $FC, $3C, $00, $00,
    $00, $5E, {|} $10, $10, $38, $38, $6C, $6C, $C6, $C6, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $5F, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $FF,
    $00, $60, {|} $18, $18, $18, $18, $0C, $0C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $61, {|} $00, $00, $00, $00, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $62, {|} $60, $60, $60, $60, $7C, $7C, $66, $66, $66, $66, $66, $66, $7C, $7C, $00, $00,
    $00, $63, {|} $00, $00, $00, $00, $3C, $3C, $60, $60, $60, $60, $60, $60, $3C, $3C, $00, $00,
    $00, $64, {|} $06, $06, $06, $06, $3E, $3E, $66, $66, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $65, {|} $00, $00, $00, $00, $3C, $3C, $66, $66, $7E, $7E, $60, $60, $3C, $3C, $00, $00,
    $00, $66, {|} $1C, $1C, $30, $30, $7C, $7C, $30, $30, $30, $30, $30, $30, $30, $30, $00, $00,
    $00, $67, {|} $00, $00, $00, $00, $3E, $3E, $66, $66, $66, $66, $3E, $3E, $06, $06, $3C, $3C,
    $00, $68, {|} $60, $60, $60, $60, $7C, $7C, $66, $66, $66, $66, $66, $66, $66, $66, $00, $00,
    $00, $69, {|} $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $6A, {|} $0C, $0C, $00, $00, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $78, $78,
    $00, $6B, {|} $60, $60, $60, $60, $66, $66, $6C, $6C, $78, $78, $6C, $6C, $66, $66, $00, $00,
    $00, $6C, {|} $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $6D, {|} $00, $00, $00, $00, $EC, $EC, $FE, $FE, $D6, $D6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $6E, {|} $00, $00, $00, $00, $7C, $7C, $66, $66, $66, $66, $66, $66, $66, $66, $00, $00,
    $00, $6F, {|} $00, $00, $00, $00, $3C, $3C, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $70, {|} $00, $00, $00, $00, $7C, $7C, $66, $66, $66, $66, $7C, $7C, $60, $60, $60, $60,
    $00, $71, {|} $00, $00, $00, $00, $3E, $3E, $66, $66, $66, $66, $3E, $3E, $06, $06, $06, $06,
    $00, $72, {|} $00, $00, $00, $00, $7C, $7C, $66, $66, $60, $60, $60, $60, $60, $60, $00, $00,
    $00, $73, {|} $00, $00, $00, $00, $3C, $3C, $60, $60, $3C, $3C, $06, $06, $7C, $7C, $00, $00,
    $00, $74, {|} $30, $30, $30, $30, $7C, $7C, $30, $30, $30, $30, $30, $30, $1C, $1C, $00, $00,
    $00, $75, {|} $00, $00, $00, $00, $66, $66, $66, $66, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $76, {|} $00, $00, $00, $00, $66, $66, $66, $66, $66, $66, $3C, $3C, $18, $18, $00, $00,
    $00, $77, {|} $00, $00, $00, $00, $C6, $C6, $C6, $C6, $D6, $D6, $FE, $FE, $6C, $6C, $00, $00,
    $00, $78, {|} $00, $00, $00, $00, $C6, $C6, $6C, $6C, $38, $38, $6C, $6C, $C6, $C6, $00, $00,
    $00, $79, {|} $00, $00, $00, $00, $66, $66, $66, $66, $66, $66, $3C, $3C, $18, $18, $30, $30,
    $00, $7A, {|} $00, $00, $00, $00, $7E, $7E, $0C, $0C, $18, $18, $30, $30, $7E, $7E, $00, $00,
    $00, $7B, {|} $0E, $0F, $19, $18, $18, $18, $70, $70, $18, $18, $18, $19, $0F, $0E, $00, $00,
    $00, $7C, {|} $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18,
    $00, $7D, {|} $70, $F0, $98, $18, $18, $18, $0E, $0E, $18, $18, $18, $98, $F0, $70, $00, $00,
    $00, $7E, {|} $72, $72, $9C, $9C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $7F, {|} $0F, $0F, $3C, $3C, $F0, $F0, $C3, $C3, $0F, $0F, $3C, $3C, $F0, $F0, $00, $00,
    $00, $80, {|} $CF, $CF, $E7, $E7, $C3, $C3, $99, $99, $81, $81, $99, $99, $99, $99, $FF, $FF,
    $00, $81, {|} $F3, $F3, $E7, $E7, $C3, $C3, $99, $99, $81, $81, $99, $99, $99, $99, $FF, $FF,
    $00, $82, {|} $E7, $E7, $99, $99, $C3, $C3, $99, $99, $81, $81, $99, $99, $99, $99, $FF, $FF,
    $00, $83, {|} $8E, $8E, $71, $71, $C3, $C3, $99, $99, $81, $81, $99, $99, $99, $99, $FF, $FF,
    $00, $84, {|} $99, $99, $FF, $FF, $C3, $C3, $99, $99, $81, $81, $99, $99, $99, $99, $FF, $FF,
    $00, $85, {|} $E7, $E7, $DB, $DB, $C3, $C3, $99, $99, $81, $81, $99, $99, $99, $99, $FF, $FF,
    $00, $86, {|} $E0, $E0, $C3, $C3, $C3, $C3, $90, $90, $83, $83, $33, $33, $30, $30, $FF, $FF,
    $00, $87, {|} $E1, $E1, $CF, $CF, $9F, $9F, $9F, $9F, $CF, $CF, $E1, $E1, $F3, $F3, $E7, $E7,
    $00, $88, {|} $CF, $CF, $E7, $E7, $81, $81, $9F, $9F, $87, $87, $9F, $9F, $81, $81, $FF, $FF,
    $00, $89, {|} $F3, $F3, $E7, $E7, $81, $81, $9F, $9F, $87, $87, $9F, $9F, $81, $81, $FF, $FF,
    $00, $8A, {|} $E7, $E7, $99, $99, $81, $81, $9F, $9F, $87, $87, $9F, $9F, $81, $81, $FF, $FF,
    $00, $8B, {|} $99, $99, $FF, $FF, $81, $81, $9F, $9F, $87, $87, $9F, $9F, $81, $81, $FF, $FF,
    $00, $8C, {|} $CF, $CF, $E7, $E7, $C3, $C3, $E7, $E7, $E7, $E7, $E7, $E7, $C3, $C3, $FF, $FF,
    $00, $8D, {|} $F3, $F3, $E7, $E7, $C3, $C3, $E7, $E7, $E7, $E7, $E7, $E7, $C3, $C3, $FF, $FF,
    $00, $8E, {|} $E7, $E7, $99, $99, $C3, $C3, $E7, $E7, $E7, $E7, $E7, $E7, $C3, $C3, $FF, $FF,
    $00, $8F, {|} $99, $99, $FF, $FF, $C3, $C3, $E7, $E7, $E7, $E7, $E7, $E7, $C3, $C3, $FF, $FF,
    $00, $90, {|} $87, $87, $93, $93, $99, $99, $09, $09, $99, $99, $93, $93, $87, $87, $FF, $FF,
    $00, $91, {|} $8E, $8E, $31, $31, $19, $19, $09, $09, $21, $21, $31, $31, $39, $39, $FF, $FF,
    $00, $92, {|} $CF, $CF, $E7, $E7, $C3, $C3, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $93, {|} $F3, $F3, $E7, $E7, $C3, $C3, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $94, {|} $E7, $E7, $99, $99, $C3, $C3, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $95, {|} $8E, $8E, $71, $71, $C3, $C3, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $96, {|} $3C, $3C, $C3, $C3, $99, $99, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $97, {|} $FF, $FF, $39, $39, $93, $93, $C7, $C7, $93, $93, $39, $39, $FF, $FF, $FF, $FF,
    $00, $98, {|} $C0, $C0, $99, $99, $91, $91, $81, $81, $89, $89, $99, $99, $03, $03, $FF, $FF,
    $00, $99, {|} $CF, $CF, $E7, $E7, $99, $99, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $9A, {|} $F3, $F3, $E7, $E7, $99, $99, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $9B, {|} $E7, $E7, $DB, $DB, $99, $99, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $9C, {|} $99, $99, $FF, $FF, $99, $99, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $9D, {|} $F9, $F9, $F7, $F7, $3C, $3C, $99, $99, $C3, $C3, $E7, $E7, $E7, $E7, $FF, $FF,
    $00, $9E, {|} $3F, $3F, $3F, $3F, $03, $03, $39, $39, $03, $03, $3F, $3F, $3F, $3F, $FF, $FF,
    $00, $9F, {|} $C3, $C3, $99, $99, $99, $99, $93, $93, $99, $99, $99, $99, $93, $93, $9F, $9F,
    $00, $A0, {|} $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $00, $00,
    $00, $A1, {|} $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $A2, {|} $00, $00, $0C, $0C, $3E, $3E, $6C, $6C, $3E, $3E, $0C, $0C, $00, $00, $00, $00,
    $00, $A3, {|} $1C, $1C, $36, $36, $30, $30, $78, $78, $30, $30, $30, $30, $7E, $7E, $00, $00,
    $00, $A4, {|} $42, $42, $3C, $3C, $66, $66, $3C, $3C, $42, $42, $00, $00, $00, $00, $00, $00,
    $00, $A5, {|} $C3, $C3, $66, $66, $3C, $3C, $18, $18, $3C, $3C, $18, $18, $18, $18, $00, $00,
    $00, $A6, {|} $18, $18, $18, $18, $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $A7, {|} $3C, $3C, $60, $60, $3C, $3C, $66, $66, $3C, $3C, $06, $06, $3C, $3C, $00, $00,
    $00, $A8, {|} $66, $66, $66, $66, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $A9, {|} $7E, $7E, $81, $81, $9D, $9D, $B1, $B1, $9D, $9D, $81, $81, $7E, $7E, $00, $00,
    $00, $AA, {|} $1C, $1C, $24, $24, $44, $44, $3C, $3C, $00, $00, $7E, $7E, $00, $00, $00, $00,
    $00, $AB, {|} $00, $00, $33, $33, $66, $66, $CC, $CC, $66, $66, $33, $33, $00, $00, $00, $00,
    $00, $AC, {|} $FE, $7F, $03, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $AD, {|} $00, $00, $00, $00, $00, $00, $FF, $FF, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $AE, {|} $7E, $7E, $81, $81, $B9, $B9, $A5, $A5, $B9, $B9, $A5, $A5, $81, $81, $7E, $7E,
    $00, $AF, {|} $FF, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B0, {|} $3C, $3C, $66, $66, $3C, $3C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B1, {|} $18, $18, $18, $18, $7E, $7E, $18, $18, $18, $18, $00, $00, $7E, $7E, $00, $00,
    $00, $B2, {|} $78, $78, $0C, $0C, $18, $18, $30, $30, $7C, $7C, $00, $00, $00, $00, $00, $00,
    $00, $B3, {|} $78, $78, $0C, $0C, $18, $18, $0C, $0C, $78, $78, $00, $00, $00, $00, $00, $00,
    $00, $B4, {|} $18, $18, $30, $30, $60, $60, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B5, {|} $00, $00, $00, $00, $66, $66, $66, $66, $66, $66, $66, $66, $7F, $7F, $60, $60,
    $00, $B6, {|} $3E, $3E, $7A, $7A, $7A, $7A, $3A, $3A, $0A, $0A, $0A, $0A, $0A, $0A, $00, $00,
    $00, $B7, {|} $00, $00, $18, $18, $18, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B8, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $30, $30,
    $00, $B9, {|} $30, $30, $70, $70, $30, $30, $30, $30, $30, $30, $00, $00, $00, $00, $00, $00,
    $00, $BA, {|} $38, $38, $44, $44, $44, $44, $38, $38, $00, $00, $7C, $7C, $00, $00, $00, $00,
    $00, $BB, {|} $00, $00, $CC, $CC, $66, $66, $33, $33, $66, $66, $CC, $CC, $00, $00, $00, $00,
    $00, $BC, {|} $40, $40, $C6, $C6, $4C, $4C, $58, $58, $30, $32, $62, $66, $C6, $CF, $02, $02,
    $00, $BD, {|} $40, $40, $C6, $C6, $4C, $4C, $58, $58, $3E, $3E, $62, $62, $C4, $C4, $0E, $0E,
    $00, $BE, {|} $C0, $C0, $23, $23, $66, $66, $2C, $2C, $D8, $1A, $32, $36, $66, $6F, $C2, $C2,
    $00, $BF, {|} $18, $18, $00, $00, $18, $18, $30, $30, $60, $60, $66, $66, $3C, $3C, $00, $00,
    $00, $C0, {|} $30, $30, $18, $18, $3C, $3C, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $C1, {|} $0C, $0C, $18, $18, $3C, $3C, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $C2, {|} $18, $18, $66, $66, $3C, $3C, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $C3, {|} $71, $71, $8E, $8E, $3C, $3C, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $C4, {|} $66, $66, $00, $00, $3C, $3C, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $C5, {|} $18, $18, $24, $24, $3C, $3C, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $C6, {|} $1F, $1F, $3C, $3C, $3C, $3C, $6F, $6F, $7C, $7C, $CC, $CC, $CF, $CF, $00, $00,
    $00, $C7, {|} $1E, $1E, $30, $30, $60, $60, $60, $60, $30, $30, $1E, $1E, $0C, $0C, $18, $18,
    $00, $C8, {|} $30, $30, $18, $18, $7E, $7E, $60, $60, $78, $78, $60, $60, $7E, $7E, $00, $00,
    $00, $C9, {|} $0C, $0C, $18, $18, $7E, $7E, $60, $60, $78, $78, $60, $60, $7E, $7E, $00, $00,
    $00, $CA, {|} $18, $18, $66, $66, $7E, $7E, $60, $60, $78, $78, $60, $60, $7E, $7E, $00, $00,
    $00, $CB, {|} $66, $66, $00, $00, $7E, $7E, $60, $60, $78, $78, $60, $60, $7E, $7E, $00, $00,
    $00, $CC, {|} $30, $30, $18, $18, $3C, $3C, $18, $18, $18, $18, $18, $18, $3C, $3C, $00, $00,
    $00, $CD, {|} $0C, $0C, $18, $18, $3C, $3C, $18, $18, $18, $18, $18, $18, $3C, $3C, $00, $00,
    $00, $CE, {|} $18, $18, $66, $66, $3C, $3C, $18, $18, $18, $18, $18, $18, $3C, $3C, $00, $00,
    $00, $CF, {|} $66, $66, $00, $00, $3C, $3C, $18, $18, $18, $18, $18, $18, $3C, $3C, $00, $00,
    $00, $D0, {|} $78, $78, $6C, $6C, $66, $66, $F6, $F6, $66, $66, $6C, $6C, $78, $78, $00, $00,
    $00, $D1, {|} $71, $71, $CE, $CE, $E6, $E6, $F6, $F6, $DE, $DE, $CE, $CE, $C6, $C6, $00, $00,
    $00, $D2, {|} $30, $30, $18, $18, $3C, $3C, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $D3, {|} $0C, $0C, $18, $18, $3C, $3C, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $D4, {|} $18, $18, $66, $66, $3C, $3C, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $D5, {|} $71, $71, $8E, $8E, $3C, $3C, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $D6, {|} $C3, $C3, $3C, $3C, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $D7, {|} $00, $00, $C6, $C6, $6C, $6C, $38, $38, $6C, $6C, $C6, $C6, $00, $00, $00, $00,
    $00, $D8, {|} $3F, $3F, $66, $66, $6E, $6E, $7E, $7E, $76, $76, $66, $66, $FC, $FC, $00, $00,
    $00, $D9, {|} $30, $30, $18, $18, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $DA, {|} $0C, $0C, $18, $18, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $DB, {|} $18, $18, $24, $24, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $DC, {|} $66, $66, $00, $00, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $DD, {|} $06, $06, $08, $08, $C3, $C3, $66, $66, $3C, $3C, $18, $18, $18, $18, $00, $00,
    $00, $DE, {|} $C0, $C0, $C0, $C0, $FC, $FC, $C6, $C6, $FC, $FC, $C0, $C0, $C0, $C0, $00, $00,
    $00, $DF, {|} $3C, $3C, $66, $66, $66, $66, $6C, $6C, $66, $66, $66, $66, $6C, $6C, $60, $60,
    $00, $E0, {|} $30, $30, $18, $18, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $E1, {|} $0C, $0C, $18, $18, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $E2, {|} $18, $18, $66, $66, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $E3, {|} $71, $71, $8E, $8E, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $E4, {|} $66, $66, $00, $00, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $E5, {|} $18, $18, $24, $24, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $E6, {|} $00, $00, $00, $00, $7E, $7E, $1B, $1B, $7F, $7F, $D8, $D8, $77, $77, $00, $00,
    $00, $E7, {|} $00, $00, $00, $00, $3C, $3C, $60, $60, $60, $60, $60, $60, $3C, $3C, $18, $18,
    $00, $E8, {|} $30, $30, $18, $18, $3C, $3C, $66, $66, $7E, $7E, $60, $60, $3C, $3C, $00, $00,
    $00, $E9, {|} $0C, $0C, $18, $18, $3C, $3C, $66, $66, $7E, $7E, $60, $60, $3C, $3C, $00, $00,
    $00, $EA, {|} $18, $18, $66, $66, $3C, $3C, $66, $66, $7E, $7E, $60, $60, $3C, $3C, $00, $00,
    $00, $EB, {|} $66, $66, $00, $00, $3C, $3C, $66, $66, $7E, $7E, $60, $60, $3C, $3C, $00, $00,
    $00, $EC, {|} $30, $30, $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $ED, {|} $0C, $0C, $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $EE, {|} $18, $18, $66, $66, $00, $00, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $EF, {|} $00, $00, $66, $66, $00, $00, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $F0, {|} $60, $60, $FC, $FC, $18, $18, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $F1, {|} $71, $71, $8E, $8E, $00, $00, $7C, $7C, $66, $66, $66, $66, $66, $66, $00, $00,
    $00, $F2, {|} $30, $30, $18, $18, $00, $00, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $F3, {|} $0C, $0C, $18, $18, $00, $00, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $F4, {|} $18, $18, $66, $66, $00, $00, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $F5, {|} $71, $71, $8E, $8E, $00, $00, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $F6, {|} $00, $00, $66, $66, $00, $00, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $F7, {|} $00, $00, $18, $18, $00, $00, $7E, $7E, $00, $00, $18, $18, $00, $00, $00, $00,
    $00, $F8, {|} $00, $00, $02, $02, $7C, $7C, $CE, $CE, $D6, $D6, $E6, $E6, $7C, $7C, $80, $80,
    $00, $F9, {|} $30, $30, $18, $18, $00, $00, $66, $66, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $FA, {|} $0C, $0C, $18, $18, $00, $00, $66, $66, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $FB, {|} $18, $18, $66, $66, $00, $00, $66, $66, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $FC, {|} $00, $00, $66, $66, $00, $00, $66, $66, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $FD, {|} $0C, $0C, $18, $18, $00, $00, $66, $66, $66, $66, $3C, $3C, $18, $18, $30, $30,
    $00, $FE, {|} $60, $60, $60, $60, $7C, $7C, $66, $66, $66, $66, $7C, $7C, $60, $60, $60, $60,
    $00, $FF, {|} $00, $00, $66, $66, $00, $00, $66, $66, $66, $66, $3C, $3C, $18, $18, $30, $30 );
  //TopazPlusMirrors = packed array [0..(0*3)-1] of Uint16;

  // Topaz
  Topaz_COUNT = 256;  // number of glyphs
  Topaz : packed array [0..Topaz_COUNT * 18 - 1] of byte = (
    $00, $00, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $01, {|} $83, $83, $39, $39, $21, $21, $29, $29, $21, $21, $3F, $3F, $87, $87, $FF, $FF,
    $00, $02, {|} $C3, $C3, $99, $99, $99, $99, $81, $81, $99, $99, $99, $99, $99, $99, $FF, $FF,
    $00, $03, {|} $83, $83, $99, $99, $99, $99, $83, $83, $99, $99, $99, $99, $83, $83, $FF, $FF,
    $00, $04, {|} $E1, $E1, $CF, $CF, $9F, $9F, $9F, $9F, $9F, $9F, $CF, $CF, $E1, $E1, $FF, $FF,
    $00, $05, {|} $87, $87, $93, $93, $99, $99, $99, $99, $99, $99, $93, $93, $87, $87, $FF, $FF,
    $00, $06, {|} $81, $81, $9F, $9F, $9F, $9F, $87, $87, $9F, $9F, $9F, $9F, $81, $81, $FF, $FF,
    $00, $07, {|} $81, $81, $9F, $9F, $9F, $9F, $87, $87, $9F, $9F, $9F, $9F, $9F, $9F, $FF, $FF,
    $00, $08, {|} $C3, $C3, $99, $99, $9F, $9F, $91, $91, $99, $99, $99, $99, $C1, $C1, $FF, $FF,
    $00, $09, {|} $99, $99, $99, $99, $99, $99, $81, $81, $99, $99, $99, $99, $99, $99, $FF, $FF,
    $00, $0A, {|} $C3, $C3, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $C3, $C3, $FF, $FF,
    $00, $0B, {|} $F9, $F9, $F9, $F9, $F9, $F9, $F9, $F9, $F9, $F9, $99, $99, $C3, $C3, $FF, $FF,
    $00, $0C, {|} $9C, $9C, $99, $99, $93, $93, $87, $87, $93, $93, $99, $99, $9C, $9C, $FF, $FF,
    $00, $0D, {|} $9F, $9F, $9F, $9F, $9F, $9F, $9F, $9F, $9F, $9F, $9F, $9F, $81, $81, $FF, $FF,
    $00, $0E, {|} $9C, $9C, $88, $88, $80, $80, $94, $94, $9C, $9C, $9C, $9C, $9C, $9C, $FF, $FF,
    $00, $0F, {|} $9C, $9C, $8C, $8C, $84, $84, $90, $90, $98, $98, $9C, $9C, $9C, $9C, $FF, $FF,
    $00, $10, {|} $C3, $C3, $99, $99, $99, $99, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $11, {|} $83, $83, $99, $99, $99, $99, $83, $83, $9F, $9F, $9F, $9F, $9F, $9F, $FF, $FF,
    $00, $12, {|} $C3, $C3, $99, $99, $99, $99, $99, $99, $99, $99, $91, $91, $C0, $C0, $FF, $FF,
    $00, $13, {|} $83, $83, $99, $99, $99, $99, $83, $83, $93, $93, $99, $99, $99, $99, $FF, $FF,
    $00, $14, {|} $C3, $C3, $99, $99, $8F, $8F, $C3, $C3, $F1, $F1, $99, $99, $C3, $C3, $FF, $FF,
    $00, $15, {|} $81, $81, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $FF, $FF,
    $00, $16, {|} $99, $99, $99, $99, $99, $99, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $17, {|} $99, $99, $99, $99, $99, $99, $99, $99, $99, $81, $C3, $C3, $E7, $E7, $FF, $FF,
    $00, $18, {|} $9C, $9C, $9C, $9C, $9C, $9C, $94, $94, $80, $80, $88, $88, $9C, $9C, $FF, $FF,
    $00, $19, {|} $3C, $3C, $99, $99, $C3, $C3, $E7, $E7, $C3, $C3, $99, $99, $3C, $3C, $FF, $FF,
    $00, $1A, {|} $3C, $3C, $99, $99, $C3, $C3, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $E7, $FF, $FF,
    $00, $1B, {|} $80, $80, $F9, $F9, $F3, $F3, $E7, $E7, $CF, $CF, $9F, $9F, $80, $80, $FF, $FF,
    $00, $1C, {|} $C3, $C3, $CF, $CF, $CF, $CF, $CF, $CF, $CF, $CF, $CF, $CF, $C3, $C3, $FF, $FF,
    $00, $1D, {|} $3F, $3F, $9F, $9F, $CF, $CF, $E7, $E7, $F3, $F3, $F9, $F9, $FC, $FC, $FF, $FF,
    $00, $1E, {|} $C3, $C3, $F3, $F3, $F3, $F3, $F3, $F3, $F3, $F3, $F3, $F3, $C3, $C3, $FF, $FF,
    $00, $1F, {|} $F7, $F7, $E3, $E3, $C9, $C9, $9C, $9C, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $00, $20, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $21, {|} $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00, $18, $18, $00, $00,
    $00, $22, {|} $6C, $6C, $6C, $6C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $23, {|} $6C, $6C, $6C, $6C, $FE, $FE, $6C, $6C, $FE, $FE, $6C, $6C, $6C, $6C, $00, $00,
    $00, $24, {|} $18, $18, $3E, $3E, $60, $60, $3C, $3C, $06, $06, $7C, $7C, $18, $18, $00, $00,
    $00, $25, {|} $00, $00, $66, $66, $AC, $AC, $D8, $D8, $36, $36, $6A, $6A, $CC, $CC, $00, $00,
    $00, $26, {|} $38, $38, $6C, $6C, $68, $68, $76, $76, $DC, $DC, $CE, $CE, $7B, $7B, $00, $00,
    $00, $27, {|} $18, $18, $18, $18, $30, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $28, {|} $0C, $0C, $18, $18, $30, $30, $30, $30, $30, $30, $18, $18, $0C, $0C, $00, $00,
    $00, $29, {|} $30, $30, $18, $18, $0C, $0C, $0C, $0C, $0C, $0C, $18, $18, $30, $30, $00, $00,
    $00, $2A, {|} $00, $00, $66, $66, $3C, $3C, $FF, $FF, $3C, $3C, $66, $66, $00, $00, $00, $00,
    $00, $2B, {|} $00, $00, $18, $18, $18, $18, $7E, $7E, $18, $18, $18, $18, $00, $00, $00, $00,
    $00, $2C, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $18, $30, $30,
    $00, $2D, {|} $00, $00, $00, $00, $00, $00, $7E, $7E, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $2E, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $18, $00, $00,
    $00, $2F, {|} $03, $03, $06, $06, $0C, $0C, $18, $18, $30, $30, $60, $60, $C0, $C0, $00, $00,
    $00, $30, {|} $3C, $3C, $66, $66, $6E, $6E, $7E, $7E, $76, $76, $66, $66, $3C, $3C, $00, $00,
    $00, $31, {|} $18, $18, $38, $38, $78, $78, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $32, {|} $3C, $3C, $66, $66, $06, $06, $0C, $0C, $18, $18, $30, $30, $7E, $7E, $00, $00,
    $00, $33, {|} $3C, $3C, $66, $66, $06, $06, $1C, $1C, $06, $06, $66, $66, $3C, $3C, $00, $00,
    $00, $34, {|} $1C, $1C, $3C, $3C, $6C, $6C, $CC, $CC, $FE, $FE, $0C, $0C, $0C, $0C, $00, $00,
    $00, $35, {|} $7E, $7E, $60, $60, $7C, $7C, $06, $06, $06, $06, $66, $66, $3C, $3C, $00, $00,
    $00, $36, {|} $1C, $1C, $30, $30, $60, $60, $7C, $7C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $37, {|} $7E, $7E, $06, $06, $06, $06, $0C, $0C, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $38, {|} $3C, $3C, $66, $66, $66, $66, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $39, {|} $3C, $3C, $66, $66, $66, $66, $3E, $3E, $06, $06, $0C, $0C, $38, $38, $00, $00,
    $00, $3A, {|} $00, $00, $18, $18, $18, $18, $00, $00, $00, $00, $18, $18, $18, $18, $00, $00,
    $00, $3B, {|} $00, $00, $18, $18, $18, $18, $00, $00, $00, $00, $18, $18, $18, $18, $30, $30,
    $00, $3C, {|} $00, $00, $06, $06, $18, $18, $60, $60, $18, $18, $06, $06, $00, $00, $00, $00,
    $00, $3D, {|} $00, $00, $00, $00, $7E, $7E, $00, $00, $7E, $7E, $00, $00, $00, $00, $00, $00,
    $00, $3E, {|} $00, $00, $60, $60, $18, $18, $06, $06, $18, $18, $60, $60, $00, $00, $00, $00,
    $00, $3F, {|} $3C, $3C, $66, $66, $06, $06, $0C, $0C, $18, $18, $00, $00, $18, $18, $00, $00,
    $00, $40, {|} $7C, $7C, $C6, $C6, $DE, $DE, $D6, $D6, $DE, $DE, $C0, $C0, $78, $78, $00, $00,
    $00, $41, {|} $3C, $3C, $66, $66, $66, $66, $7E, $7E, $66, $66, $66, $66, $66, $66, $00, $00,
    $00, $42, {|} $7C, $7C, $66, $66, $66, $66, $7C, $7C, $66, $66, $66, $66, $7C, $7C, $00, $00,
    $00, $43, {|} $1E, $1E, $30, $30, $60, $60, $60, $60, $60, $60, $30, $30, $1E, $1E, $00, $00,
    $00, $44, {|} $78, $78, $6C, $6C, $66, $66, $66, $66, $66, $66, $6C, $6C, $78, $78, $00, $00,
    $00, $45, {|} $7E, $7E, $60, $60, $60, $60, $78, $78, $60, $60, $60, $60, $7E, $7E, $00, $00,
    $00, $46, {|} $7E, $7E, $60, $60, $60, $60, $78, $78, $60, $60, $60, $60, $60, $60, $00, $00,
    $00, $47, {|} $3C, $3C, $66, $66, $60, $60, $6E, $6E, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $48, {|} $66, $66, $66, $66, $66, $66, $7E, $7E, $66, $66, $66, $66, $66, $66, $00, $00,
    $00, $49, {|} $3C, $3C, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $3C, $00, $00,
    $00, $4A, {|} $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $66, $66, $3C, $3C, $00, $00,
    $00, $4B, {|} $C6, $C6, $CC, $CC, $D8, $D8, $F0, $F0, $D8, $D8, $CC, $CC, $C6, $C6, $00, $00,
    $00, $4C, {|} $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $7E, $7E, $00, $00,
    $00, $4D, {|} $C6, $C6, $EE, $EE, $FE, $FE, $D6, $D6, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $4E, {|} $C6, $C6, $E6, $E6, $F6, $F6, $DE, $DE, $CE, $CE, $C6, $C6, $C6, $C6, $00, $00,
    $00, $4F, {|} $3C, $3C, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $50, {|} $7C, $7C, $66, $66, $66, $66, $7C, $7C, $60, $60, $60, $60, $60, $60, $00, $00,
    $00, $51, {|} $78, $78, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $DC, $DC, $7E, $7E, $00, $00,
    $00, $52, {|} $7C, $7C, $66, $66, $66, $66, $7C, $7C, $6C, $6C, $66, $66, $66, $66, $00, $00,
    $00, $53, {|} $3C, $3C, $66, $66, $70, $70, $3C, $3C, $0E, $0E, $66, $66, $3C, $3C, $00, $00,
    $00, $54, {|} $7E, $7E, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $55, {|} $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $56, {|} $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $3C, $3C, $18, $18, $00, $00,
    $00, $57, {|} $C6, $C6, $C6, $C6, $C6, $C6, $D6, $D6, $FE, $FE, $EE, $EE, $C6, $C6, $00, $00,
    $00, $58, {|} $C3, $C3, $66, $66, $3C, $3C, $18, $18, $3C, $3C, $66, $66, $C3, $C3, $00, $00,
    $00, $59, {|} $C3, $C3, $66, $66, $3C, $3C, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $5A, {|} $FE, $FE, $0C, $0C, $18, $18, $30, $30, $60, $60, $C0, $C0, $FE, $FE, $00, $00,
    $00, $5B, {|} $3C, $3C, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $3C, $3C, $00, $00,
    $00, $5C, {|} $C0, $C0, $60, $60, $30, $30, $18, $18, $0C, $0C, $06, $06, $03, $03, $00, $00,
    $00, $5D, {|} $3C, $3C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $3C, $3C, $00, $00,
    $00, $5E, {|} $10, $10, $38, $38, $6C, $6C, $C6, $C6, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $5F, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FE, $FE,
    $00, $60, {|} $18, $18, $18, $18, $0C, $0C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $61, {|} $00, $00, $00, $00, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $62, {|} $60, $60, $60, $60, $7C, $7C, $66, $66, $66, $66, $66, $66, $7C, $7C, $00, $00,
    $00, $63, {|} $00, $00, $00, $00, $3C, $3C, $60, $60, $60, $60, $60, $60, $3C, $3C, $00, $00,
    $00, $64, {|} $06, $06, $06, $06, $3E, $3E, $66, $66, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $65, {|} $00, $00, $00, $00, $3C, $3C, $66, $66, $7E, $7E, $60, $60, $3C, $3C, $00, $00,
    $00, $66, {|} $1C, $1C, $30, $30, $7C, $7C, $30, $30, $30, $30, $30, $30, $30, $30, $00, $00,
    $00, $67, {|} $00, $00, $00, $00, $3E, $3E, $66, $66, $66, $66, $3E, $3E, $06, $06, $3C, $3C,
    $00, $68, {|} $60, $60, $60, $60, $7C, $7C, $66, $66, $66, $66, $66, $66, $66, $66, $00, $00,
    $00, $69, {|} $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $6A, {|} $0C, $0C, $00, $00, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $78, $78,
    $00, $6B, {|} $60, $60, $60, $60, $66, $66, $6C, $6C, $78, $78, $6C, $6C, $66, $66, $00, $00,
    $00, $6C, {|} $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $6D, {|} $00, $00, $00, $00, $EC, $EC, $FE, $FE, $D6, $D6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $6E, {|} $00, $00, $00, $00, $7C, $7C, $66, $66, $66, $66, $66, $66, $66, $66, $00, $00,
    $00, $6F, {|} $00, $00, $00, $00, $3C, $3C, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $70, {|} $00, $00, $00, $00, $7C, $7C, $66, $66, $66, $66, $7C, $7C, $60, $60, $60, $60,
    $00, $71, {|} $00, $00, $00, $00, $3E, $3E, $66, $66, $66, $66, $3E, $3E, $06, $06, $06, $06,
    $00, $72, {|} $00, $00, $00, $00, $7C, $7C, $66, $66, $60, $60, $60, $60, $60, $60, $00, $00,
    $00, $73, {|} $00, $00, $00, $00, $3C, $3C, $60, $60, $3C, $3C, $06, $06, $7C, $7C, $00, $00,
    $00, $74, {|} $30, $30, $30, $30, $7C, $7C, $30, $30, $30, $30, $30, $30, $1C, $1C, $00, $00,
    $00, $75, {|} $00, $00, $00, $00, $66, $66, $66, $66, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $76, {|} $00, $00, $00, $00, $66, $66, $66, $66, $66, $66, $3C, $3C, $18, $18, $00, $00,
    $00, $77, {|} $00, $00, $00, $00, $C6, $C6, $C6, $C6, $D6, $D6, $FE, $FE, $6C, $6C, $00, $00,
    $00, $78, {|} $00, $00, $00, $00, $C6, $C6, $6C, $6C, $38, $38, $6C, $6C, $C6, $C6, $00, $00,
    $00, $79, {|} $00, $00, $00, $00, $66, $66, $66, $66, $66, $66, $3C, $3C, $18, $18, $30, $30,
    $00, $7A, {|} $00, $00, $00, $00, $7E, $7E, $0C, $0C, $18, $18, $30, $30, $7E, $7E, $00, $00,
    $00, $7B, {|} $0E, $0E, $18, $18, $18, $18, $70, $70, $18, $18, $18, $18, $0E, $0E, $00, $00,
    $00, $7C, {|} $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $7D, {|} $70, $70, $18, $18, $18, $18, $0E, $0E, $18, $18, $18, $18, $70, $70, $00, $00,
    $00, $7E, {|} $72, $72, $9C, $9C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $7F, {|} $0F, $0F, $3C, $3C, $F0, $F0, $C3, $C3, $0F, $0F, $3C, $3C, $F0, $F0, $00, $00,
    $00, $80, {|} $CF, $CF, $E7, $E7, $C3, $C3, $99, $99, $81, $81, $99, $99, $99, $99, $FF, $FF,
    $00, $81, {|} $F3, $F3, $E7, $E7, $C3, $C3, $99, $99, $81, $81, $99, $99, $99, $99, $FF, $FF,
    $00, $82, {|} $E7, $E7, $99, $99, $C3, $C3, $99, $99, $81, $81, $99, $99, $99, $99, $FF, $FF,
    $00, $83, {|} $8E, $8E, $71, $71, $C3, $C3, $99, $99, $81, $81, $99, $99, $99, $99, $FF, $FF,
    $00, $84, {|} $99, $99, $FF, $FF, $C3, $C3, $99, $99, $81, $81, $99, $99, $99, $99, $FF, $FF,
    $00, $85, {|} $E7, $E7, $DB, $DB, $C3, $C3, $99, $99, $81, $81, $99, $99, $99, $99, $FF, $FF,
    $00, $86, {|} $E0, $E0, $C3, $C3, $C3, $C3, $90, $90, $83, $83, $33, $33, $30, $30, $FF, $FF,
    $00, $87, {|} $E1, $E1, $CF, $CF, $9F, $9F, $9F, $9F, $CF, $CF, $E1, $E1, $F3, $F3, $E7, $E7,
    $00, $88, {|} $CF, $CF, $E7, $E7, $81, $81, $9F, $9F, $87, $87, $9F, $9F, $81, $81, $FF, $FF,
    $00, $89, {|} $F3, $F3, $E7, $E7, $81, $81, $9F, $9F, $87, $87, $9F, $9F, $81, $81, $FF, $FF,
    $00, $8A, {|} $E7, $E7, $99, $99, $81, $81, $9F, $9F, $87, $87, $9F, $9F, $81, $81, $FF, $FF,
    $00, $8B, {|} $99, $99, $FF, $FF, $81, $81, $9F, $9F, $87, $87, $9F, $9F, $81, $81, $FF, $FF,
    $00, $8C, {|} $CF, $CF, $E7, $E7, $C3, $C3, $E7, $E7, $E7, $E7, $E7, $E7, $C3, $C3, $FF, $FF,
    $00, $8D, {|} $F3, $F3, $E7, $E7, $C3, $C3, $E7, $E7, $E7, $E7, $E7, $E7, $C3, $C3, $FF, $FF,
    $00, $8E, {|} $E7, $E7, $99, $99, $C3, $C3, $E7, $E7, $E7, $E7, $E7, $E7, $C3, $C3, $FF, $FF,
    $00, $8F, {|} $99, $99, $FF, $FF, $C3, $C3, $E7, $E7, $E7, $E7, $E7, $E7, $C3, $C3, $FF, $FF,
    $00, $90, {|} $87, $87, $93, $93, $99, $99, $09, $09, $99, $99, $93, $93, $87, $87, $FF, $FF,
    $00, $91, {|} $8E, $8E, $31, $31, $19, $19, $09, $09, $21, $21, $31, $31, $39, $39, $FF, $FF,
    $00, $92, {|} $CF, $CF, $E7, $E7, $C3, $C3, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $93, {|} $F3, $F3, $E7, $E7, $C3, $C3, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $94, {|} $E7, $E7, $99, $99, $C3, $C3, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $95, {|} $8E, $8E, $71, $71, $C3, $C3, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $96, {|} $3C, $3C, $C3, $C3, $99, $99, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $97, {|} $FF, $FF, $39, $39, $93, $93, $C7, $C7, $93, $93, $39, $39, $FF, $FF, $FF, $FF,
    $00, $98, {|} $C0, $C0, $99, $99, $91, $91, $81, $81, $89, $89, $99, $99, $03, $03, $FF, $FF,
    $00, $99, {|} $CF, $CF, $E7, $E7, $99, $99, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $9A, {|} $F3, $F3, $E7, $E7, $99, $99, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $9B, {|} $E7, $E7, $DB, $DB, $99, $99, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $9C, {|} $99, $99, $FF, $FF, $99, $99, $99, $99, $99, $99, $99, $99, $C3, $C3, $FF, $FF,
    $00, $9D, {|} $F9, $F9, $F7, $F7, $3C, $3C, $99, $99, $C3, $C3, $E7, $E7, $E7, $E7, $FF, $FF,
    $00, $9E, {|} $3F, $3F, $3F, $3F, $03, $03, $39, $39, $03, $03, $3F, $3F, $3F, $3F, $FF, $FF,
    $00, $9F, {|} $C3, $C3, $99, $99, $99, $99, $93, $93, $99, $99, $99, $99, $93, $93, $9F, $9F,
    $00, $A0, {|} $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $80, $80,
    $00, $A1, {|} $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $A2, {|} $00, $00, $0C, $0C, $3E, $3E, $6C, $6C, $3E, $3E, $0C, $0C, $00, $00, $00, $00,
    $00, $A3, {|} $1C, $1C, $36, $36, $30, $30, $78, $78, $30, $30, $30, $30, $7E, $7E, $00, $00,
    $00, $A4, {|} $42, $42, $3C, $3C, $66, $66, $3C, $3C, $42, $42, $00, $00, $00, $00, $00, $00,
    $00, $A5, {|} $C3, $C3, $66, $66, $3C, $3C, $18, $18, $3C, $3C, $18, $18, $18, $18, $00, $00,
    $00, $A6, {|} $18, $18, $18, $18, $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $A7, {|} $3C, $3C, $60, $60, $3C, $3C, $66, $66, $3C, $3C, $06, $06, $3C, $3C, $00, $00,
    $00, $A8, {|} $66, $66, $66, $66, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $A9, {|} $7E, $7E, $81, $81, $9D, $9D, $B1, $B1, $9D, $9D, $81, $81, $7E, $7E, $00, $00,
    $00, $AA, {|} $1C, $1C, $24, $24, $44, $44, $3C, $3C, $00, $00, $7E, $7E, $00, $00, $00, $00,
    $00, $AB, {|} $00, $00, $33, $33, $66, $66, $CC, $CC, $66, $66, $33, $33, $00, $00, $00, $00,
    $00, $AC, {|} $3E, $3E, $06, $06, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $AD, {|} $00, $00, $00, $00, $00, $00, $7E, $7E, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $AE, {|} $7E, $7E, $81, $81, $B9, $B9, $A5, $A5, $B9, $B9, $A5, $A5, $81, $81, $7E, $7E,
    $00, $AF, {|} $7E, $7E, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B0, {|} $3C, $3C, $66, $66, $3C, $3C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B1, {|} $18, $18, $18, $18, $7E, $7E, $18, $18, $18, $18, $00, $00, $7E, $7E, $00, $00,
    $00, $B2, {|} $78, $78, $0C, $0C, $18, $18, $30, $30, $7C, $7C, $00, $00, $00, $00, $00, $00,
    $00, $B3, {|} $78, $78, $0C, $0C, $18, $18, $0C, $0C, $78, $78, $00, $00, $00, $00, $00, $00,
    $00, $B4, {|} $18, $18, $30, $30, $60, $60, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B5, {|} $00, $00, $00, $00, $66, $66, $66, $66, $66, $66, $66, $66, $7F, $7F, $60, $60,
    $00, $B6, {|} $3E, $3E, $7A, $7A, $7A, $7A, $3A, $3A, $0A, $0A, $0A, $0A, $0A, $0A, $00, $00,
    $00, $B7, {|} $00, $00, $00, $00, $18, $18, $18, $18, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B8, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $30, $30,
    $00, $B9, {|} $30, $30, $70, $70, $30, $30, $30, $30, $30, $30, $00, $00, $00, $00, $00, $00,
    $00, $BA, {|} $38, $38, $44, $44, $44, $44, $38, $38, $00, $00, $7C, $7C, $00, $00, $00, $00,
    $00, $BB, {|} $00, $00, $CC, $CC, $66, $66, $33, $33, $66, $66, $CC, $CC, $00, $00, $00, $00,
    $00, $BC, {|} $40, $40, $C6, $C6, $4C, $4C, $58, $58, $32, $32, $66, $66, $CF, $CF, $02, $02,
    $00, $BD, {|} $40, $40, $C6, $C6, $4C, $4C, $58, $58, $3E, $3E, $62, $62, $C4, $C4, $0E, $0E,
    $00, $BE, {|} $C0, $C0, $23, $23, $66, $66, $2C, $2C, $D9, $D9, $33, $33, $67, $67, $01, $01,
    $00, $BF, {|} $18, $18, $00, $00, $18, $18, $30, $30, $60, $60, $66, $66, $3C, $3C, $00, $00,
    $00, $C0, {|} $30, $30, $18, $18, $3C, $3C, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $C1, {|} $0C, $0C, $18, $18, $3C, $3C, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $C2, {|} $18, $18, $66, $66, $3C, $3C, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $C3, {|} $71, $71, $8E, $8E, $3C, $3C, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $C4, {|} $66, $66, $00, $00, $3C, $3C, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $C5, {|} $18, $18, $24, $24, $3C, $3C, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $C6, {|} $1F, $1F, $3C, $3C, $3C, $3C, $6F, $6F, $7C, $7C, $CC, $CC, $CF, $CF, $00, $00,
    $00, $C7, {|} $1E, $1E, $30, $30, $60, $60, $60, $60, $30, $30, $1E, $1E, $0C, $0C, $18, $18,
    $00, $C8, {|} $30, $30, $18, $18, $7E, $7E, $60, $60, $78, $78, $60, $60, $7E, $7E, $00, $00,
    $00, $C9, {|} $0C, $0C, $18, $18, $7E, $7E, $60, $60, $78, $78, $60, $60, $7E, $7E, $00, $00,
    $00, $CA, {|} $18, $18, $66, $66, $7E, $7E, $60, $60, $78, $78, $60, $60, $7E, $7E, $00, $00,
    $00, $CB, {|} $66, $66, $00, $00, $7E, $7E, $60, $60, $78, $78, $60, $60, $7E, $7E, $00, $00,
    $00, $CC, {|} $30, $30, $18, $18, $3C, $3C, $18, $18, $18, $18, $18, $18, $3C, $3C, $00, $00,
    $00, $CD, {|} $0C, $0C, $18, $18, $3C, $3C, $18, $18, $18, $18, $18, $18, $3C, $3C, $00, $00,
    $00, $CE, {|} $18, $18, $66, $66, $3C, $3C, $18, $18, $18, $18, $18, $18, $3C, $3C, $00, $00,
    $00, $CF, {|} $66, $66, $00, $00, $3C, $3C, $18, $18, $18, $18, $18, $18, $3C, $3C, $00, $00,
    $00, $D0, {|} $78, $78, $6C, $6C, $66, $66, $F6, $F6, $66, $66, $6C, $6C, $78, $78, $00, $00,
    $00, $D1, {|} $71, $71, $CE, $CE, $E6, $E6, $F6, $F6, $DE, $DE, $CE, $CE, $C6, $C6, $00, $00,
    $00, $D2, {|} $30, $30, $18, $18, $3C, $3C, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $D3, {|} $0C, $0C, $18, $18, $3C, $3C, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $D4, {|} $18, $18, $66, $66, $3C, $3C, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $D5, {|} $71, $71, $8E, $8E, $3C, $3C, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $D6, {|} $C3, $C3, $3C, $3C, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $D7, {|} $00, $00, $C6, $C6, $6C, $6C, $38, $38, $6C, $6C, $C6, $C6, $00, $00, $00, $00,
    $00, $D8, {|} $3F, $3F, $66, $66, $6E, $6E, $7E, $7E, $76, $76, $66, $66, $FC, $FC, $00, $00,
    $00, $D9, {|} $30, $30, $18, $18, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $DA, {|} $0C, $0C, $18, $18, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $DB, {|} $18, $18, $24, $24, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $DC, {|} $66, $66, $00, $00, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $DD, {|} $06, $06, $08, $08, $C3, $C3, $66, $66, $3C, $3C, $18, $18, $18, $18, $00, $00,
    $00, $DE, {|} $C0, $C0, $C0, $C0, $FC, $FC, $C6, $C6, $FC, $FC, $C0, $C0, $C0, $C0, $00, $00,
    $00, $DF, {|} $3C, $3C, $66, $66, $66, $66, $6C, $6C, $66, $66, $66, $66, $6C, $6C, $60, $60,
    $00, $E0, {|} $30, $30, $18, $18, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $E1, {|} $0C, $0C, $18, $18, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $E2, {|} $18, $18, $66, $66, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $E3, {|} $71, $71, $8E, $8E, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $E4, {|} $66, $66, $00, $00, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $E5, {|} $18, $18, $24, $24, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $E6, {|} $00, $00, $00, $00, $7E, $7E, $1B, $1B, $7F, $7F, $D8, $D8, $77, $77, $00, $00,
    $00, $E7, {|} $00, $00, $00, $00, $3C, $3C, $60, $60, $60, $60, $60, $60, $3C, $3C, $18, $18,
    $00, $E8, {|} $30, $30, $18, $18, $3C, $3C, $66, $66, $7E, $7E, $60, $60, $3C, $3C, $00, $00,
    $00, $E9, {|} $0C, $0C, $18, $18, $3C, $3C, $66, $66, $7E, $7E, $60, $60, $3C, $3C, $00, $00,
    $00, $EA, {|} $18, $18, $66, $66, $3C, $3C, $66, $66, $7E, $7E, $60, $60, $3C, $3C, $00, $00,
    $00, $EB, {|} $66, $66, $00, $00, $3C, $3C, $66, $66, $7E, $7E, $60, $60, $3C, $3C, $00, $00,
    $00, $EC, {|} $30, $30, $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $ED, {|} $0C, $0C, $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $EE, {|} $18, $18, $66, $66, $00, $00, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $EF, {|} $00, $00, $66, $66, $00, $00, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $F0, {|} $60, $60, $FC, $FC, $18, $18, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $F1, {|} $71, $71, $8E, $8E, $00, $00, $7C, $7C, $66, $66, $66, $66, $66, $66, $00, $00,
    $00, $F2, {|} $30, $30, $18, $18, $00, $00, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $F3, {|} $0C, $0C, $18, $18, $00, $00, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $F4, {|} $18, $18, $66, $66, $00, $00, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $F5, {|} $71, $71, $8E, $8E, $00, $00, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $F6, {|} $00, $00, $66, $66, $00, $00, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $F7, {|} $00, $00, $18, $18, $00, $00, $7E, $7E, $00, $00, $18, $18, $00, $00, $00, $00,
    $00, $F8, {|} $00, $00, $02, $02, $7C, $7C, $CE, $CE, $D6, $D6, $E6, $E6, $7C, $7C, $80, $80,
    $00, $F9, {|} $30, $30, $18, $18, $00, $00, $66, $66, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $FA, {|} $0C, $0C, $18, $18, $00, $00, $66, $66, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $FB, {|} $18, $18, $66, $66, $00, $00, $66, $66, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $FC, {|} $00, $00, $66, $66, $00, $00, $66, $66, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $FD, {|} $0C, $0C, $18, $18, $00, $00, $66, $66, $66, $66, $3C, $3C, $18, $18, $30, $30,
    $00, $FE, {|} $60, $60, $60, $60, $7C, $7C, $66, $66, $66, $66, $7C, $7C, $60, $60, $60, $60,
    $00, $FF, {|} $00, $00, $66, $66, $00, $00, $66, $66, $66, $66, $3C, $3C, $18, $18, $30, $30 );
  //TopazMirrors = packed array [0..(0*3)-1] of Uint16;

  // mO'sOul
  mOsOul_COUNT = 256; // number of glyphs
  mOsOul : packed array [0..mOsOul_COUNT * 18 - 1] of byte = (
    $00, $00, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $01, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $02, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $03, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $04, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $05, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $06, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $07, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $08, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $09, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $0A, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $0B, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $0C, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $0D, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $0E, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $0F, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $10, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $11, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $12, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $13, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $14, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $15, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $16, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $17, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $18, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $19, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $1A, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $1B, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $1C, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $1D, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $1E, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $1F, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $20, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $21, {|} $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00, $18, $18, $00, $00,
    $00, $22, {|} $6C, $6C, $6C, $6C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $23, {|} $6C, $6C, $6C, $6C, $FE, $FE, $6C, $6C, $FE, $FE, $6C, $6C, $6C, $6C, $00, $00,
    $00, $24, {|} $18, $18, $3E, $3E, $60, $60, $3C, $3C, $06, $06, $7C, $7C, $18, $18, $00, $00,
    $00, $25, {|} $00, $00, $66, $66, $AC, $AC, $D8, $D8, $36, $36, $6A, $6A, $CC, $CC, $00, $00,
    $00, $26, {|} $38, $38, $6C, $6C, $68, $68, $76, $76, $DC, $DC, $CE, $CE, $7B, $7B, $00, $00,
    $00, $27, {|} $18, $18, $18, $18, $30, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $28, {|} $0C, $0C, $18, $18, $30, $30, $30, $30, $30, $30, $18, $18, $0C, $0C, $00, $00,
    $00, $29, {|} $30, $30, $18, $18, $0C, $0C, $0C, $0C, $0C, $0C, $18, $18, $30, $30, $00, $00,
    $00, $2A, {|} $00, $00, $66, $66, $3C, $3C, $FF, $FF, $3C, $3C, $66, $66, $00, $00, $00, $00,
    $00, $2B, {|} $00, $00, $18, $18, $18, $18, $7E, $7E, $18, $18, $18, $18, $00, $00, $00, $00,
    $00, $2C, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $18, $30, $30,
    $00, $2D, {|} $00, $00, $00, $00, $00, $00, $FF, $FF, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $2E, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $18, $00, $00,
    $00, $2F, {|} $03, $03, $06, $06, $0C, $0C, $18, $18, $30, $30, $60, $60, $C0, $C0, $80, $80,
    $00, $30, {|} $3C, $3C, $66, $66, $6E, $6E, $7E, $7E, $76, $76, $66, $66, $3C, $3C, $00, $00,
    $00, $31, {|} $38, $38, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $32, {|} $3C, $3C, $06, $06, $3C, $3C, $60, $60, $60, $60, $60, $60, $7E, $7E, $00, $00,
    $00, $33, {|} $7C, $7C, $06, $06, $1C, $1C, $06, $06, $06, $06, $06, $06, $7C, $7C, $00, $00,
    $00, $34, {|} $0C, $0C, $CC, $CC, $CC, $CC, $CC, $CC, $FE, $FE, $0C, $0C, $0C, $0C, $00, $00,
    $00, $35, {|} $7C, $7C, $60, $60, $7C, $7C, $06, $06, $06, $06, $06, $06, $7C, $7C, $00, $00,
    $00, $36, {|} $3C, $3C, $60, $60, $7C, $7C, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $37, {|} $7E, $7E, $06, $06, $06, $06, $0C, $0C, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $38, {|} $3C, $3C, $66, $66, $3C, $3C, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $39, {|} $3C, $3C, $66, $66, $66, $66, $66, $66, $3E, $3E, $06, $06, $3C, $3C, $00, $00,
    $00, $3A, {|} $00, $00, $18, $18, $18, $18, $00, $00, $00, $00, $18, $18, $18, $18, $00, $00,
    $00, $3B, {|} $00, $00, $18, $18, $18, $18, $00, $00, $00, $00, $18, $18, $18, $18, $30, $30,
    $00, $3C, {|} $00, $00, $06, $06, $18, $18, $60, $60, $18, $18, $06, $06, $00, $00, $00, $00,
    $00, $3D, {|} $00, $00, $00, $00, $FF, $FF, $00, $00, $FF, $FF, $00, $00, $00, $00, $00, $00,
    $00, $3E, {|} $00, $00, $60, $60, $18, $18, $06, $06, $18, $18, $60, $60, $00, $00, $00, $00,
    $00, $3F, {|} $3C, $3C, $66, $66, $06, $06, $1C, $1C, $18, $18, $00, $00, $18, $18, $00, $00,
    $00, $40, {|} $7C, $7C, $C6, $C6, $DE, $DE, $D6, $D6, $DE, $DE, $C0, $C0, $78, $78, $00, $00,
    $00, $41, {|} $3C, $3C, $66, $66, $66, $66, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $42, {|} $7C, $7C, $66, $66, $7C, $7C, $66, $66, $66, $66, $66, $66, $7C, $7C, $00, $00,
    $00, $43, {|} $1E, $1E, $30, $30, $60, $60, $60, $60, $60, $60, $30, $30, $1E, $1E, $00, $00,
    $00, $44, {|} $7C, $7C, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $7C, $7C, $00, $00,
    $00, $45, {|} $7E, $7E, $60, $60, $60, $60, $60, $60, $78, $78, $60, $60, $7E, $7E, $00, $00,
    $00, $46, {|} $7E, $7E, $60, $60, $60, $60, $60, $60, $78, $78, $60, $60, $60, $60, $00, $00,
    $00, $47, {|} $3C, $3C, $60, $60, $6E, $6E, $66, $66, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $48, {|} $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $49, {|} $3C, $3C, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $3C, $00, $00,
    $00, $4A, {|} $0E, $0E, $06, $06, $06, $06, $06, $06, $06, $06, $06, $06, $3C, $3C, $00, $00,
    $00, $4B, {|} $C6, $C6, $CC, $CC, $D8, $D8, $F0, $F0, $D8, $D8, $CC, $CC, $C6, $C6, $00, $00,
    $00, $4C, {|} $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $60, $7E, $7E, $00, $00,
    $00, $4D, {|} $C6, $C6, $EE, $EE, $FE, $FE, $D6, $D6, $C6, $C6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $4E, {|} $C6, $C6, $E6, $E6, $F6, $F6, $DE, $DE, $CE, $CE, $C6, $C6, $C6, $C6, $00, $00,
    $00, $4F, {|} $3C, $3C, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $50, {|} $7C, $7C, $66, $66, $66, $66, $66, $66, $7C, $7C, $60, $60, $60, $60, $00, $00,
    $00, $51, {|} $78, $78, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $CC, $DC, $DC, $7E, $7E, $00, $00,
    $00, $52, {|} $7C, $7C, $66, $66, $66, $66, $66, $66, $7C, $7C, $66, $66, $66, $66, $00, $00,
    $00, $53, {|} $3C, $3C, $60, $60, $3C, $3C, $06, $06, $06, $06, $06, $06, $7C, $7C, $00, $00,
    $00, $54, {|} $7E, $7E, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $55, {|} $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $56, {|} $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $3C, $3C, $18, $18, $00, $00,
    $00, $57, {|} $C6, $C6, $C6, $C6, $C6, $C6, $D6, $D6, $FE, $FE, $EE, $EE, $C6, $C6, $00, $00,
    $00, $58, {|} $C3, $C3, $66, $66, $3C, $3C, $18, $18, $3C, $3C, $66, $66, $C3, $C3, $00, $00,
    $00, $59, {|} $C3, $C3, $66, $66, $3C, $3C, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $5A, {|} $FE, $FE, $0C, $0C, $18, $18, $30, $30, $60, $60, $C0, $C0, $FC, $FC, $00, $00,
    $00, $5B, {|} $3C, $3C, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $3C, $3C, $00, $00,
    $00, $5C, {|} $C0, $C0, $60, $60, $30, $30, $18, $18, $0C, $0C, $06, $06, $03, $03, $01, $01,
    $00, $5D, {|} $3C, $3C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $3C, $3C, $00, $00,
    $00, $5E, {|} $10, $10, $38, $38, $6C, $6C, $C6, $C6, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $5F, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $FF,
    $00, $60, {|} $18, $18, $18, $18, $0C, $0C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $61, {|} $00, $00, $00, $00, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $62, {|} $60, $60, $60, $60, $7C, $7C, $66, $66, $66, $66, $66, $66, $7C, $7C, $00, $00,
    $00, $63, {|} $00, $00, $00, $00, $3C, $3C, $60, $60, $60, $60, $60, $60, $3C, $3C, $00, $00,
    $00, $64, {|} $06, $06, $06, $06, $3E, $3E, $66, $66, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $65, {|} $00, $00, $00, $00, $3C, $3C, $66, $66, $7E, $7E, $60, $60, $3C, $3C, $00, $00,
    $00, $66, {|} $1C, $1C, $30, $30, $30, $30, $30, $30, $7C, $7C, $30, $30, $30, $30, $00, $00,
    $00, $67, {|} $00, $00, $00, $00, $3E, $3E, $66, $66, $66, $66, $3E, $3E, $06, $06, $3C, $3C,
    $00, $68, {|} $60, $60, $60, $60, $7C, $7C, $66, $66, $66, $66, $66, $66, $66, $66, $00, $00,
    $00, $69, {|} $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $6A, {|} $0C, $0C, $00, $00, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $78, $78,
    $00, $6B, {|} $60, $60, $60, $60, $66, $66, $6C, $6C, $78, $78, $6C, $6C, $66, $66, $00, $00,
    $00, $6C, {|} $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $6D, {|} $00, $00, $00, $00, $EC, $EC, $FE, $FE, $D6, $D6, $C6, $C6, $C6, $C6, $00, $00,
    $00, $6E, {|} $00, $00, $00, $00, $7C, $7C, $66, $66, $66, $66, $66, $66, $66, $66, $00, $00,
    $00, $6F, {|} $00, $00, $00, $00, $3C, $3C, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $70, {|} $00, $00, $00, $00, $7C, $7C, $66, $66, $66, $66, $7C, $7C, $60, $60, $60, $60,
    $00, $71, {|} $00, $00, $00, $00, $3E, $3E, $66, $66, $66, $66, $3E, $3E, $06, $06, $06, $06,
    $00, $72, {|} $00, $00, $00, $00, $3E, $3E, $60, $60, $60, $60, $60, $60, $60, $60, $00, $00,
    $00, $73, {|} $00, $00, $00, $00, $3C, $3C, $60, $60, $3C, $3C, $06, $06, $7C, $7C, $00, $00,
    $00, $74, {|} $30, $30, $30, $30, $7C, $7C, $30, $30, $30, $30, $30, $30, $1C, $1C, $00, $00,
    $00, $75, {|} $00, $00, $00, $00, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $76, {|} $00, $00, $00, $00, $66, $66, $66, $66, $66, $66, $3C, $3C, $18, $18, $00, $00,
    $00, $77, {|} $00, $00, $00, $00, $C6, $C6, $C6, $C6, $D6, $D6, $FE, $FE, $6C, $6C, $00, $00,
    $00, $78, {|} $00, $00, $00, $00, $C6, $C6, $6C, $6C, $38, $38, $6C, $6C, $C6, $C6, $00, $00,
    $00, $79, {|} $00, $00, $00, $00, $66, $66, $66, $66, $66, $66, $3E, $3E, $06, $06, $3C, $3C,
    $00, $7A, {|} $00, $00, $00, $00, $7E, $7E, $0C, $0C, $18, $18, $30, $30, $7E, $7E, $00, $00,
    $00, $7B, {|} $0E, $0E, $18, $18, $18, $18, $70, $70, $18, $18, $18, $18, $0E, $0E, $00, $00,
    $00, $7C, {|} $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18,
    $00, $7D, {|} $70, $70, $18, $18, $18, $18, $0E, $0E, $18, $18, $18, $18, $70, $70, $00, $00,
    $00, $7E, {|} $72, $72, $9C, $9C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $7F, {|} $0F, $0F, $3C, $3C, $F0, $F0, $C3, $C3, $0F, $0F, $3C, $3C, $F0, $F0, $00, $00,
    $00, $80, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $81, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $82, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $83, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $84, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $85, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $86, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $87, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $88, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $89, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $8A, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $8B, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $8C, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $8D, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $8E, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $8F, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $90, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $91, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $92, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $93, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $94, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $95, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $96, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $97, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $98, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $99, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $9A, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $9B, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $9C, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $9D, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $9E, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $9F, {|} $00, $00, $7E, $7E, $66, $66, $66, $66, $66, $66, $66, $66, $7E, $7E, $00, $00,
    $00, $A0, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $A1, {|} $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $A2, {|} $00, $00, $0C, $0C, $3E, $3E, $6C, $6C, $3E, $3E, $0C, $0C, $00, $00, $00, $00,
    $00, $A3, {|} $1C, $1C, $36, $36, $30, $30, $78, $78, $30, $30, $30, $30, $7E, $7E, $00, $00,
    $00, $A4, {|} $42, $42, $3C, $3C, $66, $66, $3C, $3C, $42, $42, $00, $00, $00, $00, $00, $00,
    $00, $A5, {|} $C3, $C3, $66, $66, $3C, $3C, $18, $18, $3C, $3C, $18, $18, $18, $18, $00, $00,
    $00, $A6, {|} $18, $18, $18, $18, $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $00, $00,
    $00, $A7, {|} $3C, $3C, $60, $60, $3C, $3C, $66, $66, $3C, $3C, $06, $06, $3C, $3C, $00, $00,
    $00, $A8, {|} $66, $66, $66, $66, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $A9, {|} $3C, $3C, $42, $42, $9D, $9D, $A1, $A1, $A1, $A1, $9D, $9D, $42, $42, $3C, $3C,
    $00, $AA, {|} $1C, $1C, $24, $24, $44, $44, $3C, $3C, $00, $00, $7E, $7E, $00, $00, $00, $00,
    $00, $AB, {|} $00, $00, $33, $33, $66, $66, $CC, $CC, $66, $66, $33, $33, $00, $00, $00, $00,
    $00, $AC, {|} $3E, $3E, $06, $06, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $AD, {|} $00, $00, $00, $00, $00, $00, $FF, $FF, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $AE, {|} $3C, $3C, $42, $42, $B9, $B9, $A5, $A5, $B9, $B9, $A5, $A5, $42, $42, $3C, $3C,
    $00, $AF, {|} $FF, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B0, {|} $3C, $3C, $66, $66, $3C, $3C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B1, {|} $18, $18, $18, $18, $7E, $7E, $18, $18, $18, $18, $00, $00, $7E, $7E, $00, $00,
    $00, $B2, {|} $78, $78, $0C, $0C, $18, $18, $30, $30, $7C, $7C, $00, $00, $00, $00, $00, $00,
    $00, $B3, {|} $78, $78, $0C, $0C, $18, $18, $0C, $0C, $78, $78, $00, $00, $00, $00, $00, $00,
    $00, $B4, {|} $18, $18, $30, $30, $60, $60, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B5, {|} $00, $00, $00, $00, $66, $66, $66, $66, $66, $66, $66, $66, $7F, $7F, $60, $60,
    $00, $B6, {|} $3E, $3E, $7A, $7A, $7A, $7A, $3A, $3A, $0A, $0A, $0A, $0A, $0A, $0A, $00, $00,
    $00, $B7, {|} $00, $00, $00, $00, $18, $18, $18, $18, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $B8, {|} $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $30, $30,
    $00, $B9, {|} $30, $30, $70, $70, $30, $30, $30, $30, $30, $30, $00, $00, $00, $00, $00, $00,
    $00, $BA, {|} $38, $38, $44, $44, $44, $44, $38, $38, $00, $00, $7C, $7C, $00, $00, $00, $00,
    $00, $BB, {|} $00, $00, $CC, $CC, $66, $66, $33, $33, $66, $66, $CC, $CC, $00, $00, $00, $00,
    $00, $BC, {|} $40, $40, $C6, $C6, $4C, $4C, $58, $58, $32, $32, $66, $66, $CF, $CF, $02, $02,
    $00, $BD, {|} $40, $40, $C6, $C6, $4C, $4C, $58, $58, $3E, $3E, $62, $62, $C4, $C4, $0E, $0E,
    $00, $BE, {|} $C0, $C0, $23, $23, $66, $66, $2C, $2C, $D9, $D9, $33, $33, $67, $67, $01, $01,
    $00, $BF, {|} $18, $18, $00, $00, $18, $18, $30, $30, $60, $60, $66, $66, $3C, $3C, $00, $00,
    $00, $C0, {|} $30, $30, $18, $18, $3C, $3C, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $C1, {|} $0C, $0C, $18, $18, $3C, $3C, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $C2, {|} $18, $18, $66, $66, $3C, $3C, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $C3, {|} $71, $71, $8E, $8E, $3C, $3C, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $C4, {|} $66, $66, $00, $00, $3C, $3C, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $C5, {|} $18, $18, $24, $24, $3C, $3C, $66, $66, $7E, $7E, $66, $66, $66, $66, $00, $00,
    $00, $C6, {|} $1F, $1F, $3C, $3C, $3C, $3C, $6F, $6F, $7C, $7C, $CC, $CC, $CF, $CF, $00, $00,
    $00, $C7, {|} $1E, $1E, $30, $30, $60, $60, $60, $60, $30, $30, $1E, $1E, $0C, $0C, $18, $18,
    $00, $C8, {|} $30, $30, $18, $18, $7E, $7E, $60, $60, $78, $78, $60, $60, $7E, $7E, $00, $00,
    $00, $C9, {|} $0C, $0C, $18, $18, $7E, $7E, $60, $60, $78, $78, $60, $60, $7E, $7E, $00, $00,
    $00, $CA, {|} $18, $18, $66, $66, $7E, $7E, $60, $60, $78, $78, $60, $60, $7E, $7E, $00, $00,
    $00, $CB, {|} $66, $66, $00, $00, $7E, $7E, $60, $60, $78, $78, $60, $60, $7E, $7E, $00, $00,
    $00, $CC, {|} $30, $30, $18, $18, $3C, $3C, $18, $18, $18, $18, $18, $18, $3C, $3C, $00, $00,
    $00, $CD, {|} $0C, $0C, $18, $18, $3C, $3C, $18, $18, $18, $18, $18, $18, $3C, $3C, $00, $00,
    $00, $CE, {|} $18, $18, $66, $66, $3C, $3C, $18, $18, $18, $18, $18, $18, $3C, $3C, $00, $00,
    $00, $CF, {|} $66, $66, $00, $00, $3C, $3C, $18, $18, $18, $18, $18, $18, $3C, $3C, $00, $00,
    $00, $D0, {|} $78, $78, $6C, $6C, $66, $66, $F6, $F6, $66, $66, $6C, $6C, $78, $78, $00, $00,
    $00, $D1, {|} $71, $71, $CE, $CE, $E6, $E6, $F6, $F6, $DE, $DE, $CE, $CE, $C6, $C6, $00, $00,
    $00, $D2, {|} $30, $30, $18, $18, $3C, $3C, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $D3, {|} $0C, $0C, $18, $18, $3C, $3C, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $D4, {|} $18, $18, $66, $66, $3C, $3C, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $D5, {|} $71, $71, $8E, $8E, $3C, $3C, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $D6, {|} $C3, $C3, $3C, $3C, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $D7, {|} $00, $00, $C6, $C6, $6C, $6C, $38, $38, $6C, $6C, $C6, $C6, $00, $00, $00, $00,
    $00, $D8, {|} $3F, $3F, $66, $66, $6E, $6E, $7E, $7E, $76, $76, $66, $66, $FC, $FC, $00, $00,
    $00, $D9, {|} $30, $30, $18, $18, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $DA, {|} $0C, $0C, $18, $18, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $DB, {|} $18, $18, $24, $24, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $DC, {|} $66, $66, $00, $00, $66, $66, $66, $66, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $DD, {|} $06, $06, $08, $08, $C3, $C3, $66, $66, $3C, $3C, $18, $18, $18, $18, $00, $00,
    $00, $DE, {|} $C0, $C0, $C0, $C0, $FC, $FC, $C6, $C6, $FC, $FC, $C0, $C0, $C0, $C0, $00, $00,
    $00, $DF, {|} $3C, $3C, $66, $66, $66, $66, $6C, $6C, $66, $66, $66, $66, $6C, $6C, $60, $60,
    $00, $E0, {|} $30, $30, $18, $18, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $E1, {|} $0C, $0C, $18, $18, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $E2, {|} $18, $18, $66, $66, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $E3, {|} $71, $71, $8E, $8E, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $E4, {|} $66, $66, $00, $00, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $E5, {|} $18, $18, $24, $24, $3C, $3C, $06, $06, $3E, $3E, $66, $66, $3E, $3E, $00, $00,
    $00, $E6, {|} $00, $00, $00, $00, $7E, $7E, $1B, $1B, $7F, $7F, $D8, $D8, $77, $77, $00, $00,
    $00, $E7, {|} $00, $00, $00, $00, $3C, $3C, $60, $60, $60, $60, $60, $60, $3C, $3C, $18, $18,
    $00, $E8, {|} $30, $30, $18, $18, $3C, $3C, $66, $66, $7E, $7E, $60, $60, $3C, $3C, $00, $00,
    $00, $E9, {|} $0C, $0C, $18, $18, $3C, $3C, $66, $66, $7E, $7E, $60, $60, $3C, $3C, $00, $00,
    $00, $EA, {|} $18, $18, $66, $66, $3C, $3C, $66, $66, $7E, $7E, $60, $60, $3C, $3C, $00, $00,
    $00, $EB, {|} $66, $66, $00, $00, $3C, $3C, $66, $66, $7E, $7E, $60, $60, $3C, $3C, $00, $00,
    $00, $EC, {|} $30, $30, $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $ED, {|} $0C, $0C, $18, $18, $00, $00, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $EE, {|} $18, $18, $66, $66, $00, $00, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $EF, {|} $00, $00, $66, $66, $00, $00, $18, $18, $18, $18, $18, $18, $0C, $0C, $00, $00,
    $00, $F0, {|} $60, $60, $FC, $FC, $18, $18, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $F1, {|} $71, $71, $8E, $8E, $00, $00, $7C, $7C, $66, $66, $66, $66, $66, $66, $00, $00,
    $00, $F2, {|} $30, $30, $18, $18, $00, $00, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $F3, {|} $0C, $0C, $18, $18, $00, $00, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $F4, {|} $18, $18, $66, $66, $00, $00, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $F5, {|} $71, $71, $8E, $8E, $00, $00, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $F6, {|} $00, $00, $66, $66, $00, $00, $3C, $3C, $66, $66, $66, $66, $3C, $3C, $00, $00,
    $00, $F7, {|} $00, $00, $18, $18, $00, $00, $FF, $FF, $00, $00, $18, $18, $00, $00, $00, $00,
    $00, $F8, {|} $00, $00, $02, $02, $7C, $7C, $CE, $CE, $D6, $D6, $E6, $E6, $7C, $7C, $80, $80,
    $00, $F9, {|} $30, $30, $18, $18, $00, $00, $66, $66, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $FA, {|} $0C, $0C, $18, $18, $00, $00, $66, $66, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $FB, {|} $18, $18, $66, $66, $00, $00, $66, $66, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $FC, {|} $00, $00, $66, $66, $00, $00, $66, $66, $66, $66, $66, $66, $3E, $3E, $00, $00,
    $00, $FD, {|} $0C, $0C, $18, $18, $00, $00, $66, $66, $66, $66, $3C, $3C, $18, $18, $30, $30,
    $00, $FE, {|} $60, $60, $60, $60, $7C, $7C, $66, $66, $66, $66, $7C, $7C, $60, $60, $60, $60,
    $00, $FF, {|} $00, $00, $66, $66, $00, $00, $66, $66, $66, $66, $3C, $3C, $18, $18, $30, $30 );
  //mOsOulMirrors = packed array [0..(0*3)-1] of Uint16;

  CPages : array [encCP437 .. encUTF16] of TCodePageRec = (

    (   Name :            'CP437';          // keep this one as the first
        EncodingLUT :     @CP437;           // encoding. insert more between
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable :      @UVGA16;          // here and UTF8 / UTF16 on the
        GlyphTableSize :  sizeof(UVGA16)),  // end.

    (   Name: 'CP667';
        EncodingLUT: @CP667;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP668';
        EncodingLUT: @CP668;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP737';
        EncodingLUT: @CP737;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP770';
        EncodingLUT: @CP770;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP771';
        EncodingLUT: @CP771;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP772';
        EncodingLUT: @CP772;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP773';
        EncodingLUT: @CP773;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP774';
        EncodingLUT: @CP774;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP775';
        EncodingLUT: @CP775;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP790';
        EncodingLUT: @CP667;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP808';
        EncodingLUT: @CP808;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP813';
        EncodingLUT: @CP813;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP819';
        EncodingLUT: @ISO8859_1;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP850';
        EncodingLUT: @CP850;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP851';
        EncodingLUT: @CP851;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP852';
        EncodingLUT: @CP852;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP853';
        EncodingLUT: @CP853;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP855';
        EncodingLUT: @CP855;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP857';
        EncodingLUT: @CP857;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP858';
        EncodingLUT: @CP858;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP859';
        EncodingLUT: @CP859;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP860';
        EncodingLUT: @CP860;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP861';
        EncodingLUT: @CP861;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP862';
        EncodingLUT: @CP862;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP863';
        EncodingLUT: @CP863;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP864';
        EncodingLUT: @CP864;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP865';
        EncodingLUT: @CP865;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP866';
        EncodingLUT: @CP866;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP867';
        EncodingLUT: @CP867;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP869';
        EncodingLUT: @CP869;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP872';
        EncodingLUT: @CP872;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP878';
        EncodingLUT: @CP878;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP895';
        EncodingLUT: @CP867;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP900';
        EncodingLUT: @CP866;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP912';
        EncodingLUT: @CP912;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP915';
        EncodingLUT: @CP915;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP920';
        EncodingLUT: @CP920;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP991';
        EncodingLUT: @CP667;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP1117';
        EncodingLUT: @CP1117;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP1118';
        EncodingLUT: @CP774;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP1119';
        EncodingLUT: @CP772;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP1131';
        EncodingLUT: @CP1131;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP28593';
        EncodingLUT: @ISO8859_3;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CP28594';
        EncodingLUT: @ISO8859_4;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'CPMIK';
        EncodingLUT: @CPMIK;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'ARMSCII-8';
        EncodingLUT: @ARMSCII_8;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'ISO8859-1';
        EncodingLUT: @ISO8859_1;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'ISO8859-2';
        EncodingLUT: @ISO8859_2;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'ISO8859-3';
        EncodingLUT: @ISO8859_3;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'ISO8859-4';
        EncodingLUT: @ISO8859_4;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'ISO8859-5';
        EncodingLUT: @ISO8859_5;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'ISO8859-6';
        EncodingLUT: @ISO8859_6;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'ISO8859-7';
        EncodingLUT: @ISO8859_7;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'ISO8859-8';
        EncodingLUT: @ISO8859_8;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'ISO8859-9';
        EncodingLUT: @ISO8859_9;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'ISO8859-10';
        EncodingLUT: @ISO8859_10;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'ISO8859-13';
        EncodingLUT: @ISO8859_13;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'ISO8859-14';
        EncodingLUT: @ISO8859_14;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'ISO8859-15';
        EncodingLUT: @ISO8859_15;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'ISO8859-16';
        EncodingLUT: @ISO8859_16;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'KAIK8';
        EncodingLUT: @HAIK8;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'KOI8-R';
        EncodingLUT: @KOI8_R;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'KOI8-U';
        EncodingLUT: @KOI8_U;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'WIN1250';
        EncodingLUT: @WIN1250;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'WIN1251';
        EncodingLUT: @WIN1251;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'WIN1253';
        EncodingLUT: @WIN1253;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'WIN1254';
        EncodingLUT: @WIN1254;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'WIN1255';
        EncodingLUT: @WIN1255;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'WIN1256';
        EncodingLUT: @WIN1256;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),
    (   Name: 'WIN1257';
        EncodingLUT: @WIN1257;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable: @UVGA16;
        GlyphTableSize: sizeof(UVGA16)),

//TELETEXT: new Uint16Array([ // TELETEXT
//RAW: new Uint16Array([   // for RAW converted fonts - mapped to ASCII/PETSCII points

    (   Name :            'UTF8';
        EncodingLUT :     nil;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable :      @UVGA16;
        GlyphTableSize :  sizeof(UVGA16)),

    (   Name :            'UTF16';
        EncodingLUT :     nil;
        MirrorTable :     @UVGA16Mirrors;
        MirrorTableSize : sizeof(UVGA16Mirrors);
        GlyphTable :      @UVGA16;
        GlyphTableSize :  sizeof(UVGA16))
  );

  // unicode points for block chars
  GFX_EMPTY =         $0020;
  GFX_HALFTOP =       $2580;
  GFX_HALFBOTTOM =    $2584;
  GFX_BLOCK =         $2588;
  GFX_HALFLEFT =      $258C;
  GFX_HALFRIGHT =     $2590;
  GFX_QUARTER4 =      $2596;
  GFX_QUARTER8 =      $2597;
  GFX_QUARTER1 =      $2598;
  GFX_QUARTER13 =     $2599;
  GFX_QUARTER9 =      $259A;
  GFX_QUARTER7 =      $259B;
  GFX_QUARTER11 =     $259C;
  GFX_QUARTER2 =      $259D;
  GFX_QUARTER6 =      $259E;
  GFX_QUARTER14 =     $259F;

  Blocks1x2 : array [0..3] of UInt16 = (
    GFX_EMPTY, GFX_HALFTOP, GFX_HALFBOTTOM, GFX_BLOCK );

  Blocks2x1 : array [0..3] of UInt16 = (
    GFX_EMPTY, GFX_HALFLEFT, GFX_HALFRIGHT, GFX_BLOCK );

  Blocks2x2 : array [0..15] of UInt16 = (
    GFX_EMPTY, GFX_QUARTER1, GFX_QUARTER2, GFX_HALFTOP,
    GFX_QUARTER4, GFX_HALFLEFT, GFX_QUARTER6, GFX_QUARTER7,
    GFX_QUARTER8, GFX_QUARTER9, GFX_HALFRIGHT, GFX_QUARTER11,
    GFX_HALFBOTTOM, GFX_QUARTER13, GFX_QUARTER14, GFX_QUARTER11 );

  Blocks2x3 : array [0..63] of UInt16 = (
    32, 33, 34, 35, 36, 37, 38, 39,
    40, 41, 42, 43, 44, 45, 46, 47,
    48, 49, 50, 51, 52, 53, 54, 55,
    56, 57, 58, 59, 60, 61, 62, 63,
    96, 97, 98, 99, 100, 101, 102, 103,
    104, 105, 106, 107, 108, 109, 110, 111,
    112, 113, 114, 115, 116, 117, 118, 119,
    120, 121, 122, 123, 124, 125, 126, 127 );

  // tpicturebox buttons tag info
  PBB_IMAGE_MASK =      $00FF;  // 0-255
  // behavour
  PBB_TYPE_MASK =       $0300;
  PBB_TYPE_TOGGLE =     $0000;  // click down, click up
  PBB_TYPE_BUTTON =     $0100;  // click down only
  PBB_TYPE_STATIC =     $0200;  // no clicks
  PBB_TYPE_RESERVED =   $0300;
  // flags
  PBB_FLAG_IGNORABLE =  $0400;
  // states
  PBB_DOWN =            $1000;  // depressed
  PBB_HOVER =           $2000;  // mouse over
  PBB_IGNORE =          $4000;  // ignored

  COLORSCHEME_BASIC =   0;
  COLORSCHEME_BBS =     1;
  COLORSCHEME_ICE =     2;
  COLORSCHEME_256 =     3;

  PAGETYPE_BBS =        0;
  PAGETYPE_CTERM =      1;
  PAGETYPE_VTX =        2;

  // Keyboard Actions
  KA_CURSORUP =             0;
  KA_CURSORDOWN =           1;
  KA_CURSORLEFT =           2;
  KA_CURSORRIGHT =          3;
  KA_NEXTFG =               4;
  KA_PREVFG =               5;
  KA_NEXTBG =               6;
  KA_PREVBG =               7;
  KA_CURSORNEWLINE =        8;
  KA_CURSORFORWARDTAB =     9;
  KA_CURSORBACKWARDTAB =    10;
  KA_CURSORBACK =           11;
  KA_PRINT =                12;
  KA_FKEYSET =              13;
  KA_MODECHARS =            14;
  KA_MODELEFTRIGHTBLOCKS =  15;
  KA_MODETOPBOTTOMBLOCKS =  16;
  KA_MODEQUARTERBLOCKS =    17;
  KA_MODESIXELS =           18;
  KA_TOOLSELECT =           19;
  KA_TOOLDRAW =             20;
  KA_TOOLFILL =             21;
  KA_TOOLLINE =             22;
  KA_TOOLRECTANGLE =        23;
  KA_TOOLELLIPSE =          24;
  KA_TOOLEYEDROPPER =       25;
  KA_FILENEW =              26;
  KA_FILEOPEN =             27;
  KA_FILESAVE =             28;
  KA_FILEEXIT =             29;
  KA_EDITREDO =             30;
  KA_EDITUNDO =             31;
  KA_EDITCUT =              32;
  KA_EDITCOPY =             33;
  KA_EDITPASTE =            34;

  KA_OBJECTMOVEBACK =       35;
  KA_OBJECTMOVEFORWARD =    36;
  KA_OBJECTMOVETOBACK =     37;
  KA_OBJECTMOVETOFRONT =    38;
  KA_OBJECTFLIPHORZ =       39;
  KA_OBJECTFLIPVERT =       40;
  KA_OBJECTMERGE =          41;
  KA_OBJECTMERGEALL =       42;
  KA_OBJECTNEXT =           43;
  KA_OBJECTPREV =           44;
  KA_OBJECTDELETE =         45;

  KA_DELETE =               46;   // delete selection, object, etc
  KA_ESCAPE =               47;   // clear selection, unselect object, etc

  KA_SHOWPREVIEW =          48;
  KA_EOL =                  49;

  KeyActions : array [0..KA_EOL] of string = (

    'CursorUp','CursorDown','CursorLeft','CursorRight',

    'NextFG','PrevFG','NextBG','PrevBG',

    'CursorNewLine',
    'CursorForwardTab',
    'CursorBackwardTab',
    'CursorBack',

    'Print',      // takes parameters Val
    'FKeySet',    // takes parameters Val

    'ModeChars','ModeLeftRightBlocks','ModeTopBottomBlocks','ModeQuarterBlocks',
    'ModeSixels',

    'ToolSelect','ToolDraw','ToolFill','ToolLine','ToolRectangle','ToolEllipse',
    'ToolEyeDropper',

    'FileNew','FileOpen','FileSave','FileExit',

    'EditRedo', 'EditUndo','EditCut','EditCopy','EditPaste',

    'ObjectMoveBack', 'ObjectMoveForward', 'ObjectMoveToBack',
    'ObjectMoveToFront', 'ObjectFlipHorz', 'ObjectFlipVert',
    'ObjectMerge', 'ObjectMergeAll', 'ObjectNext', 'ObjectPrev',
    'ObjectDelete',

    'Delete', 'Escape',

    'ShowPreview',

    ''
  );

  // selection flags for nbeightboring cells
  NEIGHBOR_NORTH = %0001;
  NEIGHBOR_EAST =  %0010;
  NEIGHBOR_SOUTH = %0100;
  NEIGHBOR_WEST =  %1000;

  // copy selection drag modes
  DRAG_NEW    = 0;
  DRAG_ADD    = 1;
  DRAG_REMOVE = 2;

implementation

end.

