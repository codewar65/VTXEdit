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

unit UnicodeHelper;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  SysUtils;

type

  TUnicodeStringArray = array of UnicodeString;

  TWords = array of Word;

  TWideChars = array of WideChar;

  TWideCharHelper = type helper for WideChar
    public
      function getUTF8Length : integer;   // from 1 to 3 (4=not yet).
      function getUTF16Length : integer;  // always 2
      function getCPLength : integer;     // always 1
      function toCharCode : integer;
      function fromCharCode(chr : integer) : WideChar;
  end;

  TUnicodeStringHelper = type helper for UnicodeString
    public
      function length : integer; overload;
      function substring(index : Integer): unicodestring; overload;
      function substring(index : Integer; len : Integer): unicodestring; overload;
      function charCodeAt(index : integer) : integer;
      function charAt(index : integer) : WideChar;
      function split(const Separators: array of WideChar): TUnicodeStringArray; overload;
      // other String Helper type functions can be added as required.

      function toWideCharArray : TWideChars;
      function toWordArray : TWords;

      function toUTF8Bytes : TBytes;
      function toUTF16Bytes : TBytes;
      function toCPBytes : TBytes;

      function toEncodedCPBytes(table : PWord) : TBytes;

      function getUTF8BytesLength : integer;                  // varies
      function getUTF16BytesLength : integer;                 // length * 2
      function getCPBytesLength : integer;                    // length

      function fromUTF8Bytes(bytes : TBytes) : UnicodeString;
      function fromUTF16Bytes(bytes : TBytes) : UnicodeString;
      function fromCPBytes(bytes : TBytes) : UnicodeString;

      function hasUTF8BrokenBytes(bytes : TBytes) : boolean;
      function hasUTF16BrokenBytes(bytes : TBytes) : boolean;
      function hasCPBrokenBytes(bytes : TBytes) : boolean;    // always false

      function getUTF8BrokenBytes(bytes : TBytes) : TBytes;
      function getUTF16BrokenBytes(bytes : TBytes) : TBytes;
      function getCPBrokenBytes(bytes : TBytes) : TBytes;     // always returns []

      procedure mapCP(map : TWideChars);
  end;


implementation

{ TWideCHarHelper }

function TWideCharHelper.getUTF8Length : integer;
begin
  if integer(self) < $80 then           result := 1
  else if integer(self)  < $800 then    result := 2
  else if integer(self)  < $10000 then  result := 3
  else                                  result := 4;
end;

function TWideCharHelper.getUTF16Length : integer; inline;
begin
  result := 2;
end;

function TWideCharHelper.getCPLength : integer; inline;
begin
  result := 1;
end;

function TWideCharHelper.toCharCode : integer; inline;
begin
  result := integer(self);
end;

function TWideCharHelper.fromCharCode(chr : integer) : WideChar; inline;
begin
  result := WideChar(chr);
end;

{ TUnicodeStringHelper }

{
  length : length of the UnicodeString in WideChars.
}
function TUnicodeStringHelper.length : integer; inline;
begin
  result := system.length(self);
end;

function TUnicodeStringHelper.substring(index : Integer): unicodestring;
var
  strlen, len : integer;
begin
  strlen := self.length;
  if (index < 0) or (index >= strlen) then
    result := ''
  else
  begin
    len := strlen - index;
    setlength(result, len);
    move(self[1 + index], result[1], len * sizeof(WideChar));
  end;
end;

function TUnicodeStringHelper.substring(index : Integer; len : Integer): unicodestring;
var
  strlen : integer;
begin
  strlen := self.length;
  if (index < 0) or (index >= strlen) or (len <= 0) then
    result := ''
  else
  begin
    if index + len > strlen then
      len := strlen - index;
    setlength(result, len);
    move(self[1 + index], result[1], len * sizeof(WideChar));
  end;
end;

function TUnicodeStringHelper.charCodeAt(index : integer) : integer; inline;
begin
  if (index < 0) or (index >= self.length) then
    result := 0
  else
    result := self[index + 1].toCharCode;
end;

function TUnicodeStringHelper.charAt(index : integer) : WideChar; inline;
begin
  if (index < 0) or (index >= self.length) then
    result := WideChar(0)
  else
    result := self[index + 1];
end;


Function TUnicodeStringHelper.split(const Separators: array of WideChar): TUnicodeStringArray;
var
  i, j, lastpos : integer;
  ch : widechar;

  x : UnicodeString;
begin
  x := self;
  setlength(result, 0);
  lastpos := 0;
  for i := 0 to self.length - 1 do
  begin
    ch := self.charAt(i);
    for j := 0 to system.length(Separators) - 1 do
    begin
      if ch = Separators[j] then
      begin
        setlength(result, system.length(result) + 1);
        result[system.length(result) - 1] := self.substring(lastpos, i - lastpos);
        lastpos := i + 1;
        break;
      end;
    end;
  end;
  setlength(result, system.length(result) + 1);
  result[system.length(result) - 1] := self.substring(lastpos);
end;

{
Function Split(const Separators: array of Char; ACount: Integer): TStringArray; overload;
Function Split(const Separators: array of Char; Options: TStringSplitOptions): TStringArray; overload;
Function Split(const Separators: array of Char; ACount: Integer; Options: TStringSplitOptions): TStringArray; overload;
Function Split(const Separators: array of string): TStringArray; overload;
Function Split(const Separators: array of string; ACount: Integer): TStringArray; overload;
Function Split(const Separators: array of string; Options: TStringSplitOptions): TStringArray; overload;
Function Split(const Separators: array of string; ACount: Integer; Options: TStringSplitOptions): TStringArray; overload;
Function Split(const Separators: array of Char; AQuote: Char): TStringArray; overload;
Function Split(const Separators: array of Char; AQuoteStart, AQuoteEnd: Char): TStringArray; overload;
Function Split(const Separators: array of Char; AQuoteStart, AQuoteEnd: Char; Options: TStringSplitOptions): TStringArray; overload;
Function Split(const Separators: array of Char; AQuoteStart, AQuoteEnd: Char; ACount: Integer): TStringArray; overload;
Function Split(const Separators: array of Char; AQuoteStart, AQuoteEnd: Char; ACount: Integer; Options: TStringSplitOptions): TStringArray; overload;
Function Split(const Separators: array of string; AQuote: Char): TStringArray; overload;
Function Split(const Separators: array of string; AQuoteStart, AQuoteEnd: Char): TStringArray; overload;
Function Split(const Separators: array of string; AQuoteStart, AQuoteEnd: Char; Options: TStringSplitOptions): TStringArray; overload;
Function Split(const Separators: array of string; AQuoteStart, AQuoteEnd: Char; ACount: Integer): TStringArray; overload;
Function Split(const Separators: array of string; AQuoteStart, AQuoteEnd: Char; ACount: Integer; Options: TStringSplitOptions): TStringArray; overload;
}

{
  toUTF8Bytes : returns array of bytes encoded in UTF8.
}

function TUnicodeStringHelper.toWideCharArray : TWideChars;
begin
  setlength(result, self.length);
  move(self[1], result[0], sizeof(WideChar));
end;

function TUnicodeStringHelper.toWordArray : TWords;
var
  len : longint;
begin
  len := self.length;
  setlength(result, len);
  move(self[1], result[0], sizeof(WideChar) * len);
end;

function TUnicodeStringHelper.toUTF8Bytes : TBytes;
var
  cv, i, len, cl :  integer;
  p :               pbyte;
  cw :              WideChar;
begin
  len := self.getUTF8BytesLength;
  setlength(Result, len);
  p := @Result[0];
  for i := 1 to self.length do
  begin
    cw := self[i];
    cv := cw.toCharCode;
    cl := cw.getUTF8Length;
    case cl of
      1:
        begin
          p^ := cv;
        end;

      2:
        begin
          p^ := %11000000 or ((cv >>  6) and %00011111);
          p += 1;
          p^ := %10000000 or (cv         and %00111111);
        end;

      3:
        begin
          p^ := %11100000 or ((cv >> 12) and %00001111);
          p += 1;
          p^ := %10000000 or ((cv >>  6) and %00111111);
          p += 1;
          p^ := %10000000 or (cv         and %00111111);
        end;

      4:  raise exception.create('Characters $10000+ unsupported');
    end;
    p += 1;
  end;
end;

{
  toUTF16Bytes : returns array of bytes encoded in UTF16.
}
function TUnicodeStringHelper.toUTF16Bytes : TBytes;
var
  len : integer;
begin
  len := self.getUTF16BytesLength;
  setlength(Result, len);
  move(self[1], Result[0], len);
end;

{
  toCPBytes : returns array of bytes. if character is beyond the 255 range, it
  is converted to NULL.
}
function TUnicodeStringHelper.toCPBytes : TBytes;
var
  len, i, cv : integer;
begin
  len := self.length;
  setlength(Result, len);
  for i := 1 to len do
  begin
    cv := self[i].toCharCode;
    if cv > 255 then
      cv := 0;
    Result[i - 1] := cv;
  end;
end;

{
  toEncodedCPBytes : convert unicodestring to 8 bit ascii using mapping table
}
function TUnicodeStringHelper.toEncodedCPBytes(table : PWord) : TBytes;
var
  len, i, cv : integer;
  ascii, j : integer;
begin
  len := self.length;
  setlength(Result, len);
  for i := 1 to len do
  begin
    cv := self[i].toCharCode;

    ascii := 0;
    // skip control codes
    for j := 32 to 255 do
    begin
      if cv = table[j] then
      begin
        ascii := j;
        break;
      end;
    end;
    Result[i - 1] := ascii;
  end;
end;

{
  getUTF8BytesLength : returns number of bytes required to encode as UTF8.
}
function TUnicodeStringHelper.getUTF8BytesLength : integer;
var
  i : integer;
begin
  result := 0;
  for i := 1 to system.length(self) do
    result += self[i].getUTF8Length;
end;

{
  getUTF16BytesLength : returns number of bytes required to encode as UTF16.
}
function TUnicodeStringHelper.getUTF16BytesLength : integer; inline;
begin
  result := self.length << 1;
end;

{
  getCPBytesLength : returns number of bytes required to encode as codepage.
  Does not consider any characters beyond the 255 charcode value.
}
function TUnicodeStringHelper.getCPBytesLength : integer; inline;
begin
  result := self.length;
end;

{
  fromUTF8Bytes : returns unicodestring of UTF8 in bytes. ignores broken bytes
  of partial codepoints on end. use hasUTF8BrokenBytes / getUTF8BrokenBytes to
  detect / retrieve the broken bytes to pump into next chunk from stream.
}
function TUnicodeStringHelper.fromUTF8Bytes(bytes : TBytes) : UnicodeString;
var
  len, pos : integer;
  val : UInt32;
  b : byte;
begin
  len := system.length(bytes);
  result := '';
  pos := 0;
  while pos < len do
  begin
    b := bytes[pos];
    if      (b and %11111000) = %11110000 then
    begin
      // 4 bytes
      raise exception.create('Characters $10000+ unsupported');
    end
    else if (b and %11110000) = %11100000 then
    begin
      // 3 bytes
      if pos + 3 <= len then
      begin
        val :=  (bytes[pos + 2] and $3F)
            or ((bytes[pos + 1] and $3F) << 6)
            or ((b              and $0F) << 12);
        result += WideChar(val);
      end;
      //else broken
      pos += 3;
    end
    else if (b and %11100000) = %11000000 then
    begin
      // 2 bytes
      if pos + 2 <= len then
      begin
        val :=  (bytes[pos + 1] and $3F)
            or ((b              and $1F) << 6);
        result += WideChar(val);
      end;
      //else broken
      pos += 2;

    end
    else if (b and %10000000) = %00000000 then
    begin
      // 1 byte
      result += WideChar(b);
      pos += 1;
    end;
  end;
end;

{
  fromUTF16Bytes : returns unicodestring of UTF16 in bytes. ignores broken bytes
  of partial codepoints on end. use hasUTF16BrokenBytes / getUTF16BrokenBytes
  to detect / retrieve the broken bytes to pump into next chunk from stream.
}
function TUnicodeStringHelper.fromUTF16Bytes(bytes : TBytes) : UnicodeString;
var
  len, pos : integer;
begin
  len := system.length(bytes);
  result := '';
  pos := 0;
  while pos < len do
  begin
    if pos + 1 < len then
      result += widechar(bytes[pos] + (bytes[pos + 1] << 8)); // little endian
    pos += 2;
  end;
end;

{
  fromCPBytes : returns unicodestring of ascii in bytes.
}
function TUnicodeStringHelper.fromCPBytes(bytes : TBytes) : UnicodeString;
var
  len, i : integer;
begin
  len := system.length(bytes);
  result := '';
  for i := 0 to len - 1 do
    result += WideChar(bytes[i]);
end;

{
  getUTF8BrokenBytes : returns left overs of broken codepoints in byte array.
}
function TUnicodeStringHelper.getUTF8BrokenBytes(bytes : TBytes) : TBytes;
var
  len, pos : integer;
  b : byte;
begin
  len := system.length(bytes);
  pos := 0;
  while pos < len do
  begin
    b := bytes[pos];
    if      (b and %11111000) = %11110000 then
    begin
      // 4 bytes
      raise exception.create('Characters $10000+ unsupported');
    end
    else if (b and %11110000) = %11100000 then
    begin
      // 3 bytes
      if pos + 3 > len then
      begin
        setlength(result, len - pos);
        move(bytes[pos], result[0], len-pos);
        exit;
      end;
      pos += 3;
    end
    else if (b and %11100000) = %11000000 then
    begin
      // 2 bytes
      if pos + 2 > len then
      begin
        setlength(result, len - pos);
        move(bytes[pos], result[0], len-pos);
        exit;
      end;
      pos += 2;
    end
    else if (b and %10000000) = %00000000 then
    begin
      pos += 1;
    end;
  end;
  setlength(result, 0);
end;

{
  getUTF16BrokenBytes : returns left overs of broken codepoints in byte array.
}
function TUnicodeStringHelper.getUTF16BrokenBytes(bytes : TBytes) : TBytes;
begin
  if self.HasUTF16BrokenBytes(bytes) then
  begin
    setlength(Result, 1);
    Result[0] := bytes[system.length(bytes) - 1];
  end
  else
    setlength(Result, 0);
end;

{
  getCPBrokenBytes : always returns empty byte array.
}
function TUnicodeStringHelper.getCPBrokenBytes(bytes : TBytes) : TBytes; inline;
begin
  setlength(Result, 0);
end;

{
  hasUTF8BrokenBytes : returns true if there is a broken codepoint at the end
  of the byte array.
}
function TUnicodeStringHelper.hasUTF8BrokenBytes(bytes : TBytes) : boolean;
var
  len, pos : integer;
  b : byte;
begin
  len := system.length(bytes);
  pos := 0;
  while pos < len do
  begin
    b := bytes[pos];
    if      (b and %11111000) = %11110000 then
    begin
      // 4 bytes
      raise exception.create('Characters $10000+ unsupported');
    end
    else if (b and %11110000) = %11100000 then
    begin
      // 3 bytes
      if pos + 3 >= len then
        exit(true);
      pos += 3;
    end
    else if (b and %11100000) = %11000000 then
    begin
      // 2 bytes
      if pos + 2 >= len then
        exit(true);
      pos += 2;
    end
    else if (b and %10000000) = %00000000 then
    begin
      pos += 1;
    end;
  end;
  result := false;
end;

{
  hasUTF16BrokenBytes : returns true if there is a broken codepoint at the end
  of the byte array.
}
function TUnicodeStringHelper.hasUTF16BrokenBytes(bytes : TBytes) : boolean; inline;
begin
  result := ((system.length(bytes) and $1) <> 0);
end;

{
  hasCPBrokenBytes : always returns false.
}
function TUnicodeStringHelper.hasCPBrokenBytes(bytes : TBytes) : boolean; inline;
begin
  result := false;
end;

{
  mapCP : will convert a codepaged unicodestring to true unicode using an
  array [0..255] of WideChars. if a character is outside the 0-255 range, it
  will be mapped to null.
}
procedure TUnicodeStringHelper.mapCP(map : TWideChars);
var
  len, i : integer;
  pwc : PWideChar;
  cpchr : integer;
begin
  if system.length(map) <> 256 then
    raise exception.create('Invalid mapping table length. Needs 256 characters.');

  len := self.length;
  pwc := getmemory(len * sizeof(WideChar));
  move(self[1], pwc, len * sizeof(WideChar));
  self := '';
  for i := 0 to len - 1 do
  begin
    cpchr := pwc[i].toCharCode;
    if cpchr > 255 then
      cpchr := 0; // set to null if out of range.
    self += map[cpchr];
  end;
  freememory(pwc);
end;

end.

