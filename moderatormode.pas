unit ModeratorMode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, Forms, DBconnection, Dialogs, Meta;

type
  TModeratorMode = class
  public
    ConnectPanel         : TPanel;
    HelpLabel            : TLabel;
    AboutConnectLabel    : Tlabel;

    { /db interface }
    AddressEdit          : TEdit;
    AddressLabel         : TLabel;

    PasswordEdit         : TEdit;
    PasswordLabel        : TLabel;

    UserEdit             : TEdit;
    UserLabel            : TLabel;

    ConnectBtn           : TButton;
    IdCheckBox           : TCheckBox;
    { /end }

    procedure OnModeratorMode(AForm : TForm);
    procedure OffModeratorMode(AForm : TForm);
    procedure SetDBProperties();
  private
    ModeratorCheck       : boolean;
    CheckIdVisible       : boolean;
  published
    property Moderator_check  : boolean read ModeratorCheck write ModeratorCheck;
    property id_visible       : boolean read CheckIdVisible write CheckIdVisible;
  end;


var
  Moderator              : TModeratorMode;
  ConnectButtonClick     : TnotifyEvent;
  IdCheckBoxClick        : TnotifyEvent;

const ChngForm           = 50;
const Component_offset   = 5;
const Label_offset       = 25;
const Left_offset        = 50;
const Width_label        = 80;
const ProgName           = 'ELCARO';

implementation

procedure TModeratorMode.OnModeratorMode(AForm : TForm);
var
  SL : TStringList;
begin
  SL               := MetaData.TranslateList;
  Moderator_check  := true;
  AForm.Caption    := ProgName + SL.Values['ModerMode'];

  { /HelpLabel }
  HelpLabel := TLabel.Create(AForm);
  with (HelpLabel) do
  begin
    Parent    := AForm;
    Top       := 10;
    Left      := Label_offset;
    Caption   := SL.Values['HelpLabel'];
  end;
  { /end }

  { /AboutConnectLabel }
  AboutConnectLabel := TLabel.Create(AForm);
  with (AboutConnectLabel) do
  begin
    Parent    := AForm;
    Top       := HelpLabel.Top + HelpLabel.Height + Component_offset;
    Left      := Label_offset;
  end;
  { /end }

  { /Id checkbox }
  if (IdCheckBox = nil) then
  begin
    IdCheckBox    := TCheckBox.Create(AForm);
    with (IdCheckbox) do
    begin
      Parent      := AForm;
      Top         := AboutConnectLabel.Top + AboutConnectLabel.Height + 4*Component_offset;
      Left        := Label_offset;
      Caption     := SL.Values['ShowID'];
      OnClick     := IdCheckBoxClick;
    end;
  end
  else
  begin
    if (Moderator.id_visible = true) then
      IdCheckBox.Checked := true
    else
      IdCheckBox.Checked := false;
  end;
  IdCheckBox.Visible     := true;
  { /end }

  { /ConnectPanel }
  ConnectPanel    := Tpanel.Create(AForm);
  with (ConnectPanel) do
  begin
    Parent        := AForm;
    Height        := 2 * ChngForm;
    Top           := AForm.Height - ConnectPanel.Height;
    Width         := AForm.Width;
    BevelOuter    := TPanelBevel.bvNone;
  end;
  { /end }

  { /Address }
  AddressLabel    := TLabel.Create(ConnectPanel);
  with (AddressLAbel) do
  begin
    Parent        := ConnectPanel;
    Left          := Left_offset;
    Width         := Width_label;
    Top           := Component_offset;
    Alignment     := taRightJustify;
    Caption       := SL.Values['Address'];
  end;

  AddressEdit     := Tedit.Create(ConnectPanel);
  with (AddressEdit) do
  begin
    Parent        := ConnectPanel;
    Width         := 150;
    Left          := AddressLabel.Left + Width_label + Component_offset;
    Caption       := 'TIMETABLE.FDB';
  end;
  { /end }

  { /UserName }
  UserLabel       := TLabel.Create(ConnectPanel);
  with (UserLabel) do
  begin
    Parent        := ConnectPanel;
    Left          := Left_offset;
    Width         := Width_label;
    Top           := AddressEdit.Height + 2 * Component_offset;
    Alignment     := taRightJustify;
    Caption       := SL.Values['User'];
  end;

  UserEdit        := Tedit.Create(ConnectPanel);
  with (UserEdit) do
  begin
    Parent        := ConnectPanel;
    Width         := 150;
    Top           := UserLabel.Top - Component_offset;
    Left          := UserLabel.Left + Width_label + Component_offset;
    Caption       := 'SYSDBA';
  end;
  { /end }

  { /Password }
  PasswordLabel   := TLabel.Create(ConnectPanel);
  with (PasswordLabel) do
  begin
    Parent        := ConnectPanel;
    Left          := Left_offset;
    Width         := 30;
    Top           := UserEdit.Top + UserEdit.Height + 2 * Component_offset;
    Caption       := SL.Values['Password'];
  end;

  PasswordEdit    := Tedit.Create(ConnectPanel);
  with (PasswordEdit) do
  begin
    Parent        := ConnectPanel;
    Width         := 150;
    Top           := PasswordLabel.Top - Component_offset;
    Left          := PasswordLabel.Left + Width_label + Component_offset;
    PasswordChar  := '*';
    Caption       := 'masterkey';
  end;
  { /end }

  { /ConnectButton }
  ConnectBtn      := Tbutton.Create(AForm);
  with (ConnectBtn) do
  begin
    Parent        := ConnectPanel;
    Width         := 90;
    Height        := Width - 20;
    Left          := ConnectPanel.Width - Width - 3 * Component_offset;
    Top           := Component_offset;
    if ((DBProperties.DBConnect = Connect) or (DBProperties.DBConnect = Error)) then
      Caption     := SL.Values['Disconnection']
    else
      Caption     := SL.Values['Connection'];
    OnClick       := ConnectButtonClick;
  end;
  { /end }
end;


procedure TModeratorMode.OffModeratorMode(AForm : Tform);
begin
  Moderator.Moderator_check := false;
  AForm.Caption             := ProgName;
  IdCheckBox.visible        := false;

  ConnectPanel.free;
  HelpLabel.free;
  AboutConnectLabel.free;
end;

procedure TModeratorMode.SetDBProperties();
begin
  with (DBProperties) do
  begin
    DBPassword   := PasswordEdit.Caption;
    DBName       := AddressEdit.Caption;
    DBUserName   := UserEdit.Caption;
  end;
end;

end.

