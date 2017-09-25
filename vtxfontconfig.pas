unit VTXFontConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TfFontConfig }

  TfFontConfig = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbFont1: TComboBox;
    cbFont2: TComboBox;
    cbFont3: TComboBox;
    cbFont4: TComboBox;
    cbFont5: TComboBox;
    cbFont6: TComboBox;
    cbFont7: TComboBox;
    cbFont8: TComboBox;
    cbFont9: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fFontConfig: TfFontConfig;

implementation

initialization
  {$I vtxfontconfig.lrs}

end.

