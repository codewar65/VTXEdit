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
  TRecList = record
    Data :    PBYTE;  // pointer to data records
    Count :   DWORD;  // current number of records in Data.
    Size :    DWORD;  // current allocated space
    RecSize : DWORD;
    procedure Create(recsz : DWORD);
    procedure Free;
    procedure Add(rec : Pointer);  // add new record
    procedure Remove(recnum : DWORD);
    procedure Put(rec : Pointer; recnum : DWORD); inline;
    procedure Get(rec : Pointer; recnum : DWORD); inline;
    procedure Clear;
    function Copy : TRecList;
  end;

implementation

const
  INIT_RECSIZE = 16;

procedure TRecList.Create(recsz : DWORD);
begin
  self.RecSize := recsz;
  self.Count := 0;
  self.Size := INIT_RECSIZE;
  self.Data := GetMemory(self.RecSize * Size);
  FillByte(self.Data[0], self.RecSize * Size, $00);
end;

procedure TRecList.Free;
begin
  if self.Size > 0 then
    Freememory(self.Data);
  self.Size := 0;
  self.Count := 0;
end;

procedure TRecList.Add(rec : Pointer);
var
  newsz : DWORD;
  newdata : PBYTE;
begin
  if Count >= Size then
  begin
    // grow the data
    newsz := self.Size << 1;
    newdata := getmemory(newsz * self.RecSize);
    FillByte(newdata[0], newsz * self.RecSize, $00);
    MemCopy(self.Data, newdata, self.Size * self.RecSize);
    FreeMemory(self.Data);
    self.Data := newdata;
    self.Size := newsz;
  end;

  MemCopy(rec, @self.Data[self.Count * self.RecSize], self.RecSize);
  self.Count += 1;
end;

procedure TRecList.Remove(recnum : DWORD);
var
  totsz : DWORD;
  endsz : DWORD;
begin
  if recnum >= self.Count then
    exit;

  totsz := (self.RecSize * self.Size);
  endsz := totsz - ((recnum + 1) * self.RecSize);
  move(
    self.Data[(recnum + 1) * self.RecSize],
    self.Data[recnum * self.RecSize],
    endsz);
  self.Count -= 1;
end;

procedure TRecList.Put(rec : Pointer; recnum : DWORD); inline;
begin
  MemCopy(rec, @self.Data[recnum * self.RecSize], self.RecSize);
end;

procedure TRecList.Get(rec : Pointer; recnum : DWORD); inline;
begin
  MemCopy(@self.Data[recnum * self.RecSize], rec, self.RecSize);
end;

procedure TRecList.Clear;
begin
  FreeMemory(self.Data);
  self.Count := 0;
  self.Size := INIT_RECSIZE;
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


end.













































































