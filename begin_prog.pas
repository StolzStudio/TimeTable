unit begin_prog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Windows;

type

  { TBeginForm }

  TBeginForm = class(TForm)
    LogoImg: TImage;
    BeginTimer: TTimer;
    procedure BeginTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  BeginForm: TBeginForm;

implementation

{$R *.lfm}
{ TBeginForm }

procedure TBeginForm.BeginTimerTimer(Sender: TObject);
begin
  BeginTimer.Enabled := false;
  BeginForm.Close;
end;

procedure TBeginForm.FormCreate(Sender: TObject);
var
  rgn: HRGN;
  Size: integer;
  ChngSize: integer;
begin
  Size := 355;
  ChngSize := 1;
  rgn := CreateEllipticRgn(0 + ChngSize, 0 + ChngSize,
                           Size - ChngSize, Size - ChngSize);
  SetWindowRgn(Handle, rgn, true);
  DeleteObject(rgn);
end;

end.

