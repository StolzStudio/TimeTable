unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  DBConnection, ExtCtrls, ModeratorMode, TypInfo, Meta,
  DirectoryForms, SQLgen;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemDirectory: TMenuItem;
    MenuItemAbout: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormUpdate(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure PressConnectBtn(Sender: TObject);
    procedure ClickIdCheckBox(Sender: TObject);
    procedure CreateDirectoryMenuItems();
    procedure DestroyDirectoryMenuItems();
    procedure OnClickMenuItem(Sender: TObject);
  end;

var
  MainForm: TMainForm;

  const M = 77;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Moderator := TModeratorMode.Create;
  Moderator.Moderator_check := false;
  MetaData := TMeta.Create;
  SQLGenerator := TSQL.Create;
  MainForm.Caption := ProgName;
  ConnectButtonClick := @PressConnectBtn;
  IdCheckBoxClick := @ClickIdCheckBox;
  try
    DBDataModule.DBConnect();
    CreateDirectoryMenuItems();
  except
    DBProperties.DBConnect := Error;
  end;
  MainForm.show;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = M) then
    if (Moderator.Moderator_check = false) then
      Moderator.OnModeratorMode(MainForm)
    else
      Moderator.OffModeratorMode(MainForm);
end;

procedure TMainForm.FormUpdate(Sender: TObject);
begin
  if Moderator.Moderator_check = true then
    Moderator.AboutConnectLabel.Caption := MetaData.TranslateList.Values['ConnectStatus']
    + MetaData.TranslateList.Values[GetEnumName(TypeInfo(Tconnect), ord(DBProperties.DBConnect))];
end;

procedure TMainForm.MenuItemAboutClick(Sender: TObject);
begin
  ShowMessage(MetaData.TranslateList.Values['AboutProgram'] + #13 +
              MetaData.TranslateList.Values['AboutProgramName']);
end;

procedure TMainForm.CreateDirectoryMenuItems();
var
  MenuItem: TMenuItem;
  i: integer;
  s: string;
begin
  MetaData.TranslateList := TStringList.Create;
  MetaData.TranslateList.LoadFromFile('ruRUMeta.in');
  DBDataModule.SQLQuery.Active := false;
  DBDataModule.SQLQuery.SQL.Text := 'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS '
                                 + 'WHERE RDB$SYSTEM_FLAG = 0';
  DBDataModule.SQLQuery.Open;
  i := 0;
  while not DBDataModule.SQLQuery.EOF do
  begin
    s := DBDataModule.SQLQuery.Fields[0].AsString;
    SetLength(MetaData.Tables, length(MetaData.Tables) + 1);
    MetaData.Tables[high(MetaData.Tables)] := TTable.Create;
    MetaData.Tables[high(MetaData.Tables)].FillDataTable(s);
    MenuItem:= TMenuItem.Create(MenuItemDirectory);
    MenuItem.Caption := MetaData.TranslateList.Values[CreateItemName(s)];
    MenuItem.OnClick := @OnClickMenuItem;
    MenuItem.Tag := i;
    MenuItemDirectory.Add(MenuItem);
    inc(i);
    DBDataModule.SQLQuery.Next;
  end;
  for i := 0 to high(MetaData.Tables) do
     MetaData.Tables[i].FillReferencedField;
  TableForms := TTableForms.Create;
  DBDataModule.SQLQuery.Close;
end;

procedure TMainForm.DestroyDirectoryMenuItems();
begin
  MenuItemDirectory.Clear;
  SetLength(MetaData.Tables, 0);
  TableForms.Destroy;
  MetaData.Destroy;
end;

procedure TMainForm.OnClickMenuItem(Sender: TObject);
begin
  if TableForms.FForms[(Sender as TMenuItem).Tag] = nil then
    TableForms.FForms[(Sender as TMenuItem).Tag] := TDirectoryForm.Create(Application);
  TableForms.FForms[(Sender as TMenuItem).Tag].SetParams(Sender);
  TableForms.FForms[(Sender as TMenuItem).Tag].Show;
end;

{ Moderator Mode }

procedure TMainForm.PressConnectBtn(Sender: TObject);
begin
  if Moderator.ConnectBtn.Caption = MetaData.TranslateList.Values['Disconnection'] then
  begin
      DestroyDirectoryMenuItems();
      Moderator.ConnectBtn.Caption := MetaData.TranslateList.Values['Connection'];
      DBDataModule.DBDisconnect();
  end
  else begin
    try
      Moderator.SetDBProperties();
      DBDataModule.DBConnect();
      Moderator.ConnectBtn.Caption := MetaData.TranslateList.Values['Disconnection'];
      CreateDirectoryMenuItems();
    except
      DBProperties.DBConnect := Error;
    end;
  end;
  MainForm.Invalidate;
end;

procedure TMainForm.ClickIdCheckBox(Sender: TObject);
begin
  Moderator.id_visible := Moderator.IdCheckBox.Checked;
end;

end.

