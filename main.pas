unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  DBConnection, ExtCtrls, ModeratorMode, TypInfo, Meta,
  DirectoryForms, SQLgen, UTimeTable, ChangeFormData;

type

  { TMainForm }

  TMainForm = class(TForm)
    { /menu }
    MainMenu            : TMainMenu;
    MenuItemTimeTable   : TMenuItem;
    MenuItemFile        : TMenuItem;
    MenuItemDirectory   : TMenuItem;
    MenuItemAbout       : TMenuItem;

    procedure GlobalUpdate();
    { /form procedures }
    procedure FormCreate(Sender : TObject);
    procedure FormKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
    procedure FormUpdate(Sender : TObject);
    procedure MenuItemAboutClick(Sender : TObject);
    procedure MenuItemTimeTableClick(Sender : TObject);
    procedure OnClickMenuItem(Sender : TObject);

    { /work with derectory}
    procedure CreateDirectoryMenuItems();
    procedure DestroyDirectoryMenuItems();

    { /moderator mode }
    procedure PressConnectBtn(Sender : TObject);
    procedure ClickIdCheckBox(Sender : TObject);
  end;

var
  MainForm : TMainForm;



implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender : TObject);
begin
  MainForm.Caption := ProgName;
  UpdateEvent      := @GlobalUpdate;

  { /moderator}
  Moderator                   := TModeratorMode.Create;
  Moderator.Moderator_check   := false;
  ConnectButtonClick          := @PressConnectBtn;
  IdCheckBoxClick             := @ClickIdCheckBox;

  MetaData       := TMeta.Create;
  SQLGenerator   := TSQL.Create;

  try
    DBDataModule.DBConnect();
    CreateDirectoryMenuItems();
  except
    DBProperties.DBConnect := Error;
  end;
  MainForm.show;
end;

procedure TMainForm.FormKeyDown(Sender : TObject; var Key : Word;
  Shift : TShiftState);
var
  M : integer = 77;
begin
  if ((ssCtrl in Shift) and (Key = M)) then
    if (Moderator.Moderator_check = false) then
      Moderator.OnModeratorMode(MainForm)
    else
      Moderator.OffModeratorMode(MainForm);
end;

procedure TMainForm.FormUpdate(Sender : TObject);
begin
  if (Moderator.Moderator_check = true) then
    with MetaData.TranslateList do
      Moderator.AboutConnectLabel.Caption := Values['ConnectStatus'] +
                                             Values[GetEnumName(TypeInfo(Tconnect),
                                             ord(DBProperties.DBConnect))];
end;

procedure TMainForm.MenuItemAboutClick(Sender : TObject);
begin
  with (MetaData.TranslateList) do
    ShowMessage(Values['AboutProgram'] + #13 + Values['AboutProgramName']);
end;

procedure TMainForm.MenuItemTimeTableClick(Sender: TObject);
begin
  TimeTableForm := TTimeTableForm.Create(Application);
  TimeTableForm.SetParam(Sender);
  TimeTableForm.Show;
end;

procedure TMainForm.CreateDirectoryMenuItems();
var
  MenuItem    : TMenuItem;
  i           : integer;
  s           : string;
begin
  { /set meta }
  MetaData.TranslateList := TStringList.Create;
  MetaData.TranslateList.LoadFromFile('ruRUMeta.in');

  with (DBDataModule.SQLQuery) do
  begin
    Active     := false;
    SQL.Text   := 'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS '
                  + 'WHERE RDB$SYSTEM_FLAG = 0';
    Open;
  end;
  i := 0;
  while not DBDataModule.SQLQuery.EOF do
  begin
    s := DBDataModule.SQLQuery.Fields[0].AsString;

    { /fill tables }
    with (MetaData) do
    begin
      SetLength(Tables, length(Tables) + 1);
      Tables[high(Tables)] := TTable.Create;
      Tables[high(Tables)].FillDataTable(s);
    end;

    { /set new menu item }
    MenuItem := TMenuItem.Create(MenuItemDirectory);
    with (MenuItem) do
    begin
      Caption   := MetaData.TranslateList.Values[MetaData.CreateItemName(s)];
      OnClick   := @OnClickMenuItem;
      Tag       := i;
    end;
    MenuItemDirectory.Add(MenuItem);
    inc(i);

    DBDataModule.SQLQuery.Next;
  end;

  { /fill reference table }
  with (MetaData) do
    for i := 0 to high(Tables) do
    Tables[i].FillReferencedField;

  TableForms := TTableForms.Create;
  DBDataModule.SQLQuery.Close;
end;

procedure TMainForm.DestroyDirectoryMenuItems();
begin
  MenuItemDirectory.Clear;
  SetLength(MetaData.Tables, 0);
  TableForms.Destroy;
end;

procedure TMainForm.OnClickMenuItem(Sender : TObject);
var
  i : integer;
begin
  i := (Sender as TMenuItem).Tag;
  with (TableForms) do
  begin
    if (FForms[i] = nil) then
      FForms[i] := TDirectoryForm.Create(Application);
    FForms[i].SetParams(Sender);
    FForms[i].Show;

  end;
end;

{ Moderator Mode }
procedure TMainForm.PressConnectBtn(Sender : TObject);
var
  stl: TStringList;
begin
  stl := MetaData.TranslateList;
  if Moderator.ConnectBtn.Caption = stl.Values['Disconnection'] then
  begin
      Moderator.ConnectBtn.Caption := stl.Values['Connection'];
      DestroyDirectoryMenuItems();
      DBDataModule.DBDisconnect();
  end
  else begin
    try
      Moderator.SetDBProperties();
      DBDataModule.DBConnect();
      CreateDirectoryMenuItems();
      Moderator.ConnectBtn.Caption := stl.Values['Disconnection'];
    except
      DBProperties.DBConnect := Error;
    end;
  end;
  MainForm.Invalidate;
end;

procedure TMainForm.ClickIdCheckBox(Sender : TObject);
begin
  Moderator.id_visible := Moderator.IdCheckBox.Checked;
end;

procedure TMainForm.GlobalUpdate();
var
  i: integer;
begin
  for i := 0 to high(TableForms.FForms) do
    if (TableForms.FForms[i] <> nil) then
      with (TableForms.FForms[i]) do
      begin
        SQLGenerator.GenFilters(Tag, DirectoryFilter, FSQLQuery);
        SQLGenerator.SetColName(FDBGrid, Tag);
      end;
  if (TimeTableForm <> nil) then
    TimeTableForm.FillGridData();
end;
end.

