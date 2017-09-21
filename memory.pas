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

unit Memory;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$ASMMODE intel}

interface

uses
  Classes, SysUtils;

procedure MemZero(dst : Pointer; size : longint); inline;
procedure MemFill(dst : Pointer; size : longint; val : byte); inline;
procedure MemCopy(src, dst : Pointer; size : longint); inline;
function  MemComp(src, dst : Pointer; size : longint) : boolean; inline;

implementation

procedure MemZero(dst : Pointer; size : longint); inline;
begin
  FillByte(dst^, size, $00);
//  asm
//        MOV   EDI, dst
//        MOV   ECX, size
//        XOR   AL, AL
//        REP   STOSB
//  end ['AL', 'EDI', 'ECX'];
end;

procedure MemFill(dst : Pointer; size : longint; val : byte); inline;
begin
  FillByte(dst^, size, val);
//  asm
//        MOV   EDI, dst
//        MOV   ECX, size
//        MOV   AL, val
//        REP   STOSB
//  end ['AL', 'EDI', 'ECX'];
end;

procedure MemCopy(src, dst : Pointer; size : longint); inline;
begin
  Move(src^, dst^, size);
//  asm
//        MOV   ESI, src
//        MOV   EDI, dst
//        MOV   ECX, size
//        REP   MOVSB
//  end ['ESI', 'EDI', 'ECX'];
end;

function MemComp(src, dst : Pointer; size : longint) : boolean; inline;
//label
//  done;
begin
  result := CompareMem(src,dst,size);
//  asm
//        MOV   result, $01
//        MOV   ESI, src
//        MOV   EDI, dst
//        MOV   ECX, size
//        REPE  CMPSB
//        JZ    DONE
//        DEC   result
//DONE:
//  end;
end;

end.

