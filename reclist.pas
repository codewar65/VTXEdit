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
  Generic record lists using byte buffer data.
}
unit RecList;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$asmmode intel}

interface

uses
  Classes, SysUtils, Memory;

type
  TRecListExpansion = ( rleDoubles, rleAdds );

  TRecList = record
    Data :    PBYTE;  // pointer to data records
    Count :   DWORD;  // current number of records in Data.
    Size :    DWORD;  // current allocated space
    RecSize : DWORD;
    Flags :   BYTE;
    procedure Create(recsz : DWORD; expansion : TRecListExpansion);
    procedure Free;
    procedure Add(rec : Pointer);  // add new record
    procedure Remove(recnum : DWORD);
    procedure Push(rec : Pointer);
    procedure Pop(rec : Pointer);
    procedure Put(rec : Pointer; recnum : DWORD);
    procedure Get(rec : Pointer; recnum : DWORD);
    procedure Clear;
    function  Copy : TRecList;
    procedure Swap(rec1, rec2 : DWORD);
    procedure Trim;
    function Locked : Boolean;
  end;

implementation

const
  TRECLIST_INITSIZE =   16;           // initial number of records allocated on create
  TRECLIST_LOCKED =     %00000001;    // object size is locked. no additions or removals allowed.
  TRECLIST_ADDEXPAND =  %00000010;    // of set, size increases by recsize instead of doubles.

procedure TRecList.Create(recsz : DWORD; expansion : TRecListExpansion);
begin
  self.RecSize := recsz;
  self.Count := 0;
  self.Size := TRECLIST_INITSIZE;
  self.Data := GetMemory(self.RecSize * Size);
  FillByte(self.Data[0], self.RecSize * Size, $00);
  self.Flags := %00000000;
  if expansion = rleAdds then
    self.Flags := (self.Flags or TRECLIST_ADDEXPAND);
end;

function TRecList.Locked : boolean;
begin
  result := ((self.Flags and TRECLIST_LOCKED) <> 0);
end;

procedure TRecList.Free;
begin
  if self.Size > 0 then
    Freememory(self.Data);
  self.Size := 0;
  self.Count := 0;
end;

procedure TRecList.Push(rec : Pointer);
begin
  self.Add(rec);
end;

procedure TRecList.Add(rec : Pointer);
var
  newsz : DWORD;
  newdata : PBYTE;
begin

  if (self.Flags and TRECLIST_LOCKED) <> 0 then
    raise Exception.Create('TRecList Locked.');

  if Count >= Size then
  begin
    // grow the data
    if (self.Flags and TRECLIST_ADDEXPAND) <> 0 then
      newsz := self.Size + (TRECLIST_INITSIZE * self.RecSize)
    else
      newsz := self.Size << 1;

    newdata := getmemory(newsz * self.RecSize);
    MemFill(newdata, newsz * self.RecSize, $00);
    MemCopy(self.Data, newdata, self.Size * self.RecSize);
    FreeMemory(self.Data);
    self.Data := newdata;
    self.Size := newsz;
  end;

  MemCopy(rec, @self.Data[self.Count * self.RecSize], self.RecSize);
  self.Count += 1;
end;

procedure TRecList.Pop(rec : Pointer);
begin
  if self.Count = 0 then
    raise Exception.Create('TRecList Stack Underflow.');
  self.Get(rec, self.Count - 1);
  self.Count -= 1;
end;

procedure TRecList.Remove(recnum : DWORD);
var
  totsz : DWORD;
  endsz : DWORD;
begin

  if (self.Flags and TRECLIST_LOCKED) <> 0 then
    raise Exception.Create('TRecList Locked.');

  if recnum >= self.Count then
    raise Exception.Create('TRecList Out of Bounds.');

  totsz := (self.RecSize * self.Size);
  endsz := totsz - ((recnum + 1) * self.RecSize);
  move(
    self.Data[(recnum + 1) * self.RecSize],
    self.Data[recnum * self.RecSize],
    endsz);
  self.Count -= 1;
end;

procedure TRecList.Put(rec : Pointer; recnum : DWORD);
begin
  if recnum >= self.Count then
    raise Exception.Create('TRecList Out of Bounds.');

  MemCopy(rec, @self.Data[recnum * self.RecSize], self.RecSize);
end;

procedure TRecList.Get(rec : Pointer; recnum : DWORD);
begin
  if recnum >= self.Count then
    raise Exception.Create('TRecList Out of Bounds.');

  MemCopy(@self.Data[recnum * self.RecSize], rec, self.RecSize);
end;

procedure TRecList.Clear;
begin
  FreeMemory(self.Data);
  self.Count := 0;
  self.Size := TRECLIST_INITSIZE;
  self.Data := GetMemory(self.RecSize * Size);
end;

// create copy of TRecList
// WARNING : CLEAR ANY DATA INSIDE THAT CONTAINS OTHER TRECLISTS
function TRecList.Copy : TRecList;
var
  memsize : longint;
begin
  result.Count :=   self.Count;
  result.RecSize := self.RecSize;
  result.Size :=    self.Size;
  memsize := self.RecSize * self.Size;
  result.Data :=    Getmemory(memsize);
  MemCopy(self.Data, result.Data, memsize);
end;

// swap contents of two recors.
procedure TRecList.Swap(rec1, rec2 : DWORD);
var
  idx1, idx2 : DWORD;
  tmp : PBYTE;
begin
  if (rec1 >= self.Count) or (rec2 >= self.Count) then
    raise Exception.Create('TRecList Out of Bounds.');

  tmp := GetMemory(self.RecSize);
  idx1 := rec1 * self.RecSize;
  idx2 := rec2 * self.RecSize;

  MemCopy(@self.Data[idx1], tmp, self.RecSize);
  MemCopy(@self.Data[idx2], @self.Data[idx1], self.RecSize);
  MemCopy(tmp, @self.Data[idx2], self.RecSize);
  FreeMemory(tmp);
end;

// trim memory/ used when rec is not expecet to grow any more.
procedure TRecList.Trim;
var
  tmp : PBYTE;
  l : DWORD;
begin
  l := self.Count * self.RecSize;
  tmp := GetMemory(l);
  MemCopy(self.Data, tmp, l);
  Freememory(self.Data);
  self.Data := tmp;
  self.Flags := self.Flags or TRECLIST_LOCKED;
end;

end.

