unit Filters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, Buttons, Forms, Controls, dialogs, Meta;

type
  TFilter = class
  private
    FQuery    : string;
    FTag      : integer;
    FStatus   : boolean;
  published
    property Query    : string    read FQuery    write FQuery;
    property Tag      : integer   read FTag      write FTag;
    property Status   : boolean   read FStatus   write FStatus;
  end;

  TDirectoryFilter = class(TFilter)
  public
    FilPanel     : TPanel;
    FieldBox     : TComboBox;
    ActionBox    : TComboBox;
    FilterEdit   : TEdit;
    ApplyBtn     : TSpeedButton;
    DelBtn       : TSpeedButton;
    procedure CreateFilter(APanel : TPanel; ANum : integer; AList : TStringList);
    procedure DeleteFilter(Sender : TObject);
    procedure ApplyFilter(Sender : TObject);
    procedure ChangeParam(Sender : TObject);
    procedure Update(AList : TStringList);
  private
    FField       : string;
    FAction      : string;
    FTextEdit    : string;
    FFormTag     : integer;
  published
    property FieldName   : string    read FField      write FField;
    property Action      : string    read FAction     write FAction;
    property TextEdit    : string    read FTextEdit   write FTextEdit;
    property FormTag     : integer   read FFormTag    write FFormTag;
  end;

  const PnlWidth    = 422;
  const PnlHeight   = 28;
  const BrdrSize    = 8;

var
  DelFilterNum         : integer;
  FilterChangeStatus   : boolean = false;
  InvalidateEvent      : TNotifyEvent;
  Actions              : array [0..6] of string = ('<', '>', '>=', '<=',
                                                   '=', '<>', 'включает');

implementation

procedure TDirectoryFilter.CreateFilter(APanel : TPanel; ANum : integer;
                                                AList : TStringList);
var
  TopSize, i : integer;
begin
  TopSize   := 1;
  Tag       := ANum;
  Status    := False;

  { FilPanel }
  FilPanel        := TPanel.Create(APanel);
  with (FilPanel) do
  begin
    Parent        := APanel;
    Left          := APanel.Width - PnlWidth;
    Width         := PnlWidth    - BrdrSize;
    Top           := PnlHeight   * (ANum - 1) + BrdrSize;
    Height        := PnlHeight;
    Anchors       := [akRight, akTop];
    BorderStyle   := bsSingle;
    BevelOuter    := bvNone;
  end;

  { FieldBox }
  FieldBox      := TComboBox.Create(FilPanel);
  with (FieldBox) do
  begin
    Parent      := FilPanel;
    Top         := TopSize;
    Left        := 4;
    ReadOnly    := true;
    OnChange    := @ChangeParam;
  end;
  Update(AList);
  FieldBox.ItemIndex := 0;

  { ActionBox }
  ActionBox     := TComboBox.Create(FilPanel);
  with (ActionBox) do
  begin
    Parent      := FilPanel;
    Left        := FieldBox.Left + FieldBox.Width;
    Width       := 84;
    Top         := TopSize;
    ReadOnly    := true;
    OnChange    := @ChangeParam;
    for i :=0 to 6 do
      Items.Add(Actions[i]);
    ItemIndex   := 0;
  end;

  { FilterEdit }
  FilterEdit   := TEdit.Create(FilPanel);
  with (FilterEdit) do
  begin
    Parent     := FilPanel;
    Left       := ActionBox.Left + ActionBox.Width;
    Width      := 174;
    Top        := TopSize;
    OnChange   := @ChangeParam;
  end;

  { ApplyBtn }
  ApplyBtn := TSpeedButton.Create(FilPanel);
  with (ApplyBtn) do
  begin
    Parent    := FilPanel;
    Left      := FilterEdit.Left + FilterEdit.Width;
    Top       := TopSize;
    OnClick   := @ApplyFilter;
    Glyph.LoadFromFile('apply_icon.bmp');
  end;

  { DelBtn }
  DelBtn      := TSpeedButton.Create(FilPanel);
  with (DelBtn) do
  begin
    Parent    := FilPanel;
    Left      := ApplyBtn.Left + ApplyBtn.Width;
    Top       := TopSize;
    OnClick   := @DeleteFilter;
    Glyph.LoadFromFile('del_icon.bmp');
  end;
end;

procedure TDirectoryFilter.ChangeParam(Sender : TObject);
begin
  Status               := False;
  ApplyBtn.Enabled     := True;
  FilterChangeStatus   := True;

  if (Sender = FieldBox) then
    FilterEdit.Clear;
  InvalidateEvent(Sender);
end;

procedure TDirectoryFilter.DeleteFilter(Sender : TObject);
begin
  FilterChangeStatus   := True;
  DelFilterNum         := Tag;
  Status               := False;

  FilPanel.free;
  InvalidateEvent(Sender);
end;

procedure TDirectoryFilter.ApplyFilter(Sender : TObject);
var
  i, j : integer;
begin
  if (FilterEdit.Caption = '') then
  begin
    showmessage(MetaData.TranslateList.Values['CheckFilter']);
    exit;
  end;
  FilterChangeStatus   := true;
  ApplyBtn.Enabled     := false;
  Status               := True;
  i                    := FieldBox.ItemIndex + 1;
  with (MetaData) do
  begin
    if Tables[FormTag].Fields[i].Reference <> nil then
    begin
        j           := Tables[FormTag].Fields[i].Reference.TableTag;
        FieldName   := Tables[j].Name +
                       '.' + Tables[j].Fields[1].Name;
    end
    else
      FieldName     := Tables[FormTag].Name + '.' +
                       Tables[FormTag].Fields[i].Name;
  end;

  if (ActionBox.Caption <> Actions[6]) then
    Action := ActionBox.Caption + ' '
  else
    Action := 'LIKE ';

  TextEdit := FilterEdit.Caption;
  InvalidateEvent(Sender);
end;

procedure TDirectoryFilter.Update(AList : TStringList);
var
  i : integer;
begin
  FieldBox.Items.Clear;
  for i := 0 to AList.Count - 1 do
    FieldBox.Items.Add(AList.ValueFromIndex[i]);
end;

end.

