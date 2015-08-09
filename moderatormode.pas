unit ModeratorMode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, Forms, DBconnection, Dialogs;

type
  TModeratorMode = class
  public
    ConnectPanel: TPanel;
    HelpLabel: TLabel;
    AboutConnectLabel: Tlabel;
    AddressEdit: TEdit;
    AddressLabel: TLabel;
    PasswordEdit: TEdit;
    PasswordLabel: TLabel;
    UserEdit: TEdit;
    UserLabel: TLabel;
    ConnectBtn: TButton;
    IdCheckBox: TCheckBox;
    procedure OnModeratorMode(AForm: TForm);
    procedure OffModeratorMode(AForm: TForm);
    procedure SetDBProperties();
  private
    ModeratorCheck: boolean;
    CheckIdVisible: boolean;
  published
    property Moderator_check: boolean read ModeratorCheck write ModeratorCheck;
    property id_visible: boolean read CheckIdVisible write CheckIdVisible;
  end;


var
  Moderator: TModeratorMode;
  ConnectButtonClick: TnotifyEvent;
  IdCheckBoxClick: TnotifyEvent;

const ChngForm = 50;
const Component_offset = 5;
const Label_offset = 25;
const Cnct = 'Подключение';
const DisCnct = 'Отключение';
const ProgName = 'ELCARO';

implementation

procedure TModeratorMode.OnModeratorMode(AForm: TForm);
begin
  Moderator.Moderator_check := true;
  AForm.Caption := ProgName + ' - Moderator Mode';

  { HelpLabel }
  HelpLabel := TLabel.Create(AForm);
  HelpLabel.Parent := AForm;
  HelpLabel.Top := 10;
  HelpLabel.Left := Label_offset;
  HelpLabel.Caption := 'Для выхода из режима администрирования нажмите "Ctrl + M"';

  { AboutConnectLabel }
  AboutConnectLabel := TLabel.Create(AForm);
  AboutConnectLabel.Parent := AForm;
  AboutConnectLabel.Top := HelpLabel.Top + HelpLabel.Height + Component_offset;
  AboutConnectLabel.Left := Label_offset;

  { Id checkbox }
  if IdCheckBox = nil then
  begin
    IdCheckBox := TCheckBox.Create(AForm);
    IdCheckBox.Parent := AForm;
    IdCheckBox.Top := AboutConnectLabel.Top + AboutConnectLabel.Height + 4*Component_offset;
    IdCheckBox.Left := Label_offset;
    IdCheckBox.Caption := 'Показывать ИН';
    IdCheckBox.OnClick := IdCheckBoxClick;
  end
  else
  begin
    if (Moderator.id_visible = true) then
    begin
      IdCheckBox.Checked := true;
    end
    else
      IdCheckBox.Checked := false;
    IdCheckBox.Visible := true;
  end;

  { ConnectPanel }
  ConnectPanel := Tpanel.Create(AForm);
  ConnectPanel.Parent := AForm;
  ConnectPanel.Height := 2 * ChngForm;
  ConnectPanel.Top := AForm.Height - ConnectPanel.Height;
  ConnectPanel.Width := AForm.Width;
  ConnectPanel.BevelOuter := TPanelBevel.bvNone;

  { Address }
  AddressLabel := TLabel.Create(ConnectPanel);
  AddressLabel.Parent := ConnectPanel;
  AddressLabel.Left := 101;
  AddressLabel.Width := 30;
  AddressLabel.Top := Component_offset;
  AddressLabel.Caption := 'Адрес:';

  AddressEdit := Tedit.Create(ConnectPanel);
  AddressEdit.Parent := ConnectPanel;
  AddressEdit.Width := 150;
  AddressEdit.Left := AddressLabel.Left + AddressLabel.Width + Component_offset;
  AddressEdit.Caption := 'TIMETABLE.FDB';

  { UserName }
  UserLabel := TLabel.Create(ConnectPanel);
  UserLabel.Parent := ConnectPanel;
  UserLabel.Left := 56;
  UserLabel.Width := 30;
  UserLabel.Top := AddressEdit.Height + 2 * Component_offset;
  UserLabel.Caption := 'Пользователь:';

  UserEdit := Tedit.Create(ConnectPanel);
  UserEdit.Parent := ConnectPanel;
  UserEdit.Width := 150;
  UserEdit.Top := UserLabel.Top - Component_offset;
  UserEdit.Left := UserLabel.Left + UserLabel.Width + Component_offset;
  UserEdit.Caption := 'SYSDBA';

  { Password }
  PasswordLabel := TLabel.Create(ConnectPanel);
  PasswordLabel.Parent := ConnectPanel;
  PasswordLabel.Left := 92;
  PasswordLabel.Width := 30;
  PasswordLabel.Top := UserEdit.Top + UserEdit.Height + 2 * Component_offset;
  PasswordLabel.Caption := 'Пароль:';

  PasswordEdit := Tedit.Create(ConnectPanel);
  PasswordEdit.Parent := ConnectPanel;
  PasswordEdit.Width := 150;
  PasswordEdit.Top := PasswordLabel.Top - Component_offset;
  PasswordEdit.Left := PasswordLabel.Left + PasswordLabel.Width + Component_offset;
  PasswordEdit.PasswordChar := '*';
  PasswordEdit.Caption := 'masterkey';

  { ConnectButton }
  ConnectBtn := Tbutton.Create(AForm);
  ConnectBtn.Parent := ConnectPanel;
  ConnectBtn.Width := 90;
  ConnectBtn.Height := ConnectBtn.Width - 20;
  ConnectBtn.Left := ConnectPanel.Width - ConnectBtn.Width - 3 * Component_offset;
  ConnectBtn.Top := Component_offset;
  if (DBProperties.DBConnect = Connect) or (DBProperties.DBConnect = Error) then
    ConnectBtn.Caption := DisCnct
  else
    ConnectBtn.Caption := Cnct;
  ConnectBtn.OnClick := ConnectButtonClick;
end;

procedure TModeratorMode.OffModeratorMode(AForm: Tform);
begin
  Moderator.Moderator_check := false;
  AForm.Caption := ProgName;
  IdCheckBox.visible := false;
  ConnectPanel.free;
  HelpLabel.free;
  AboutConnectLabel.free;
end;

procedure TModeratorMode.SetDBProperties();
begin
  DBProperties.DBPassword := PasswordEdit.Caption;
  DBProperties.DBName := AddressEdit.Caption;
  DBProperties.DBUserName := UserEdit.Caption;
end;

end.

