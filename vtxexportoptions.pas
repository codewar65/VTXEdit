unit VTXExportOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin;

type

  { TfExportOptions }

  TfExportOptions = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbUseLineLen: TCheckBox;
    cbUseSauce: TCheckBox;
    cbStaticObjects: TCheckBox;
    cbUseBOM: TCheckBox;
    seLineLen: TSpinEdit;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fExportOptions: TfExportOptions;

implementation

{ TfExportOptions }

initialization
{$I vtxexportoptions.lrs}

end.

