unit ChangeFormData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, meta, DBConnection, SQLgen;

type

  TChangeType = (ctEdit, ctInsert, ctDelete);

  { TFormData }

  TUpdateEvent = procedure () of Object;

  TFormData = class(TForm)
  private
    FID: integer;
    FAction : TChangeType;
    UsedHeight: integer;
    UsedWidth: integer;
  public
    DataControl: array of TControl;
    ApplyBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure CreateComboBox(AList: TStringList; AName: string; AWidth: integer);
    procedure CreateEdit(AName: string; AWidth: integer);
    procedure CreateApplyBtn();
    procedure ChangeApplyClick(Sender: TObject);
  published
    property Action: TChangeType read FAction write FAction;
    property DataID: integer read FID write FID;
  end;

var
  FormData: TFormData;
  UpdateEvent: TUpdateEvent;
implementation

{$R *.lfm}

{ TFormData }

procedure TFormData.FormCreate(Sender: TObject);
begin
  UsedWidth := 0;
  UsedHeight := 0;
end;

procedure TFormData.CreateComboBox(AList: TStringList; AName: string; AWidth: integer);
var
  i: integer;
  ALabel: TLabel;
begin
  ALabel := TLabel.Create(Self);
  with ALabel do
  begin
    Parent := self;
    Top := 10 + UsedHeight;
    Left := 10;
    Width := AWidth + 20;
    Height := 18;
    Visible := true;
    UsedHeight += Height;
  end;
  ALabel.Caption := AName + ':';
  DataControl[high(DataControl)] := TComboBox.Create(Self);
  with (DataControl[high(DataControl)] as TComboBox) do
  begin
    Parent := self;
    Top := 10 + UsedHeight;
    Left := 10;
    Width := ALabel.Width + 20;
    Height := 24;
    ReadOnly := true;
    Visible := true;
    UsedHeight += Height + 5;
  end;
  for i := 0 to AList.Count - 1 do
    (DataControl[high(DataControl)] as TComboBox).Items.Add(
      AList.ValueFromIndex[i]);
end;

procedure TFormData.CreateEdit(AName: string; AWidth: integer);
var
  ALabel: TLabel;
begin
  ALabel := TLabel.Create(Self);
  with ALabel do
  begin
    Parent := self;
    Top := 10 + UsedHeight;
    Left := 10;
    Width := AWidth + 20;
    Height := 18;
    Visible := true;
    UsedHeight += Height;
  end;
  ALabel.Caption := AName + ':';
  DataControl[high(DataControl)] := TEdit.Create(Self);
  with (DataControl[high(DataControl)] as TEdit) do
  begin
    Parent := self;
    Top := 10 + UsedHeight;
    Left := 10;
    Width := AWidth + 10;
    Height := 24;
    Visible := true;
    UsedHeight += Height + 5;
  end;
end;

procedure TFormData.CreateApplyBtn();
begin
  Width := 500;
  ApplyBtn := TBitBtn.Create(Self);
  with ApplyBtn do
  begin
     Parent := self;
     Top := UsedHeight + 10;
     Kind := bkOK;
     Width := 104;
     Left := self.Width - Width - 10;
     Height := 24;
     Caption := MetaData.TranslateList.Values['Apply'];
     Visible := true;
     OnClick := @ChangeApplyClick;
     UsedHeight += Height + 5;
  end;
  UsedWidth += ApplyBtn.Width + 10;
  Height := UsedHeight + 15;
  UsedHeight := 0;
  UsedWidth := 0;
end;

procedure TFormData.ChangeApplyClick(Sender: TObject);
var
  i, key: integer;
  s: string;

  function CheckEdit(): boolean;
  begin
    if (DataControl[0] as TEdit).text = '' then
    begin
      exit(true);
    end;
    result := false;
  end;

begin
  if (Action = ctEdit) or (Action = ctInsert) then
  begin
    if CheckEdit() then
    begin
      ShowMessage(MetaData.TranslateList.Values['FillField']);
      exit;
    end;
  end;
  DBDataModule.SQLQuery.Active := False;
  if Action = ctInsert then
  begin
    key := SQLGenerator.GenUniqId();
    DBDataModule.SQLQuery.SQL.Text := SQLGenerator.GenInsertQuery(Tag).Text;
    DBDataModule.SQLQuery.ParamByName('p0').AsInteger := key;
  end
  else
  begin
    DBDataModule.SQLQuery.SQL.Text:= SQLGenerator.GenUpdateQuery(Tag).Text;
    DBDataModule.SQLQuery.ParamByName('p0').AsInteger := DataID;
    showmessage(DBDataModule.SQLQuery.SQL.Text);
  end;
  for i:= 0 to high(DataControl) do
  begin
    s:= 'p' + IntToStr(i + 1);
    if MetaData.Tables[Tag].Fields[i + 1].Reference <> nil then
      DBDataModule.SQLQuery.ParamByName(s).AsInteger := SQLGenerator.GetId(Tag, i,
        (DataControl[i] as TComboBox).ItemIndex)
    else
      DBDataModule.SQLQuery.ParamByName(s).AsString:=
        (DataControl[i] as TEdit).Text;
  end;
  DBDataModule.SQLQuery.ExecSQL;
  DBDataModule.SQLTransaction.Commit;
  UpdateEvent;
  Close;
end;
end.

