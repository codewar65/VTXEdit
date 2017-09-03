{

// Copyright 2015-2016 Jonathan Bennett <jon@autoitscript.com>
//
// https://www.autoitscript.com
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// To Free Pascal - codewar65
}

unit VTXEncDetect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDetectEnc = (
  	deNone, deAnsi, deAscii, deUtf8Bom, deUtf8NoBom,
    deUtf16LeBom, deUtf16LeNoBom, deUtf16BeBom, deUtf16BeNoBom );


function DetectEncoding(buffer : TBytes) : TDetectEnc;
function CheckBom(buffer : TBytes) : TDetectEnc;


implementation

const
	_utf16BeBom : array [0..1] of byte = ( $FE, $FF );
	_utf16LeBom : array [0..1] of byte = ( $FF, $FE );
	_utf8Bom : array [0..2] of byte = ( $EF, $BB, $BF );

var
	_nullSuggestsBinary : boolean = true;
  _utf16ExpectedNullPercent : double = 70;
  _utf16UnexpectedNullPercent : double = 10;

function GetBomLengthFromEncodingMode(encoding : TDetectEnc) : integer;
begin
	case encoding of
  	deUtf16BeBom,
    deUtf16LeBom:
      result := 2;

    deUtf8Bom:
      result := 3;

    else
      result := 0;
  end;
end;

function CheckBom(buffer : TBytes) : TDetectEnc;
var
  size : longint;
begin
  size := length(buffer);
  result := deNone;
  if (size >= 2)
  	and (buffer[0] = _utf16LeBom[0])
    and (buffer[1] = _utf16LeBom[1]) then
	  result := deUtf16LeBom;
  if (size >= 2)
    and (buffer[0] = _utf16BeBom[0])
    and (buffer[1] = _utf16BeBom[1]) then
    result := deUtf16BeBom;
  if (size >= 3)
    and (buffer[0] = _utf8Bom[0])
    and (buffer[1] = _utf8Bom[1])
    and (buffer[2] = _utf8Bom[2]) then
    result := deUtf8Bom;
end;

function CheckUtf8(buffer : TBytes) : TDetectEnc;
var
  pos : 								integer;
  modeChars : 					integer;
  ch : 									byte;
  onlySawAsciiRange : 	boolean;
  size : longint;
begin
  size := length(buffer);
  pos := 0;
  while pos < size do
  begin
		ch := buffer[pos];
    pos += 1;
    if (ch = $00) and _nullSuggestsBinary then
    	begin result := deNone; exit; end;

		if ch <= 127 then													modeChars := 0
    else if (ch >= 194) and (ch <= 223) then	modeChars := 1
    else if (ch >= 224) and (ch <= 239) then	modeChars := 2
    else if (ch >= 240) and (ch <= 244) then  modeChars := 4
    else
    	begin result := deNone; exit; end;

		while (modechars > 0) and (pos < size) do
    begin
      onlySawAsciiRange := false;
      ch := buffer[pos];
      pos += 1;
      if (ch < 127) or (ch > 191) then
        begin result := deNone; exit; end;
      modeChars -= 1;
    end;
  end;
  if onlySawAsciiRange then
    result := deAscii
  else
    result := deUtf8NoBom;
end;


function CheckUtf16NewLineChars(buffer : TBytes) : TDetectEnc;
var
  leControlChars : 	integer;
  beControlChars : 	integer;
  pos :							integer;
  ch1, ch2 :				byte;
  size : longint;
begin
  size := length(buffer);
  if size < 2 then
  begin
    result := deNone;
    exit;
  end;
  size -= 1;

  leControlChars := 0;
  beControlChars := 0;

  pos := 0;
  while pos < size do
	begin
    ch1 := buffer[pos];
    pos += 1;
    ch2 := buffer[pos];
    pos += 1;
    if ch1 = $00 then
    begin
      if (ch2 = $0A) or (ch2 = $0d) then
      	beControlChars += 1;
    end
    else if ch2 = $00 then
    begin
      if (ch1 = $0a) or (ch1 = $0d) then
      	leControlChars += 1;
    end;
    if (leControlChars > 0) and (beControlChars > 0) then
    	begin result := deNone; exit; end;
  end;
  if leControlChars > 0 then
	  begin result := deUtf16LeNoBom; exit; end;
  if beControlChars > 0 then
	  result := deUtf16BeNoBom
  else
    result := deNone;
end;

function DoesContainNulls(buffer : TBytes) : boolean;
var
  pos : integer;
  size : longint;
begin
  size := length(buffer);
 	pos := 0;
  result := false;
  while pos < size do
  begin
  	if buffer[pos] = $00 then
    	begin result := true; break; end;

    pos += 1;
  end;
end;

function CheckUtf16Ascii(buffer : TBytes) : TDetectEnc;
var
  numOddNulls,
  numEvenNulls : 							integer;
  pos : 											integer;
  evenNullThreshold,
  oddNullThreshold,
  expectedNullThreshold,
  unexpectedNullThreashold : 	double;
  size : longint;
begin
  size := length(buffer);
  numOddNulls := 0;
  numEvenNulls := 0;
  pos := 0;
  while pos < size do
  begin
    if buffer[pos] = $00 then
    	numEvenNulls += 1;
    if pos + 1 < size then
	    if buffer[pos + 1] = $00 then
  	  	numOddNulls += 1;
    pos += 2;
  end;
	evenNullThreshold := numEvenNulls * 2.0 / size;
  oddNullThreshold := numOddNulls * 2.0 / size;
  expectedNullThreshold := _utf16ExpectedNullPercent / 100;
  unexpectedNullThreashold := _utf16UnexpectedNullPercent / 100;

  if (evenNullThreshold < unexpectedNullThreashold) and (oddNullThreshold > expectedNullThreshold) then
  	begin result := deUtf16LeNoBom; exit end;

  if (oddNullThreshold < unexpectedNullThreashold) and (evenNullThreshold > expectedNullThreshold) then
	  begin result := deUtf16BeBom; exit; end;

  result := deNone;
end;

function DetectEncoding(buffer : TBytes) : TDetectEnc;
var
  encoding : TDetectEnc;
  size : longint;
begin
  size := length(buffer);
  encoding := CheckBom(buffer);
  if encoding <> deNone then
  	begin result := encoding; exit; end;

  encoding := CheckUtf8(buffer);
  if encoding <> deNone then
   	begin result := encoding; exit; end;

//  encoding := CheckUtf16NewlineChars(buffer);
//  if encoding <> deNone then
//   	begin result := encoding; exit; end;

  encoding := CheckUtf16Ascii(buffer);
  if encoding <> deNone then
  	begin result := encoding; exit; end;

	if not DoesContainNulls(buffer) then
  	begin result := deAnsi; exit; end;

  if _nullSuggestsBinary then
  	result := deNone
  else
    result := deAnsi;

end;

end.

