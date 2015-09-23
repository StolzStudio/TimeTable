unit Filters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, Buttons, Forms, Controls, dialogs, Meta;

type
  TFilter = class
  private
    FQuery: string;
    FTag: integer;
    FStatus: boolean;
  published
    property Query: string read FQuery write FQuery;
    property Tag: integer read FTag write FTag;
    property Status: boolean read FStatus write FStatus;
  end;

  TDirectoryFilter = class(TFilter)
  public
    FilPanel: TPanel;
    FieldBox: TComboBox;
    ActionBox: TComboBox;
    FilterEdit: TEdit;
    ApplyBtn: TSpeedButton;
    DelBtn: TSpeedButton;
    procedure CreateFilter(AForm: TForm; ANum: integer; AList: TStringList);
    procedure DeleteFilter(Sender: TObject);
    procedure ApplyFilter(Sender: TObject);
    procedure ChangeParam(Sender: TObject);
    procedure Update(AList: TStringList);
  private
    FField: string;
    FAction: string;
    FTextEdit: string;
    FFormTag: integer;
  published
    property FieldName: string read FField write FField;
    property Action: string read FAction write FAction;
    property TextEdit: string read FTextEdit write FTextEdit;
    property FormTag: integer read FFormTag write FFormTag;
  end;

  const PnlWidth = 422;
  const PnlHeight = 28;
  const BrdrSize = 8;

var
  Actions: array[0..6] of string = ('<', '>', '>=', '<=', '=',
    '<>', 'включает');
  DelFilterNum: integer;
  FilterChangeStatus: boolean = false;
  InvalidateEvent: TNotifyEvent;

implementation

procedure TDirectoryFilter.CreateFilter(AForm: TForm; ANum: integer;
                                                AList: TStringList);
var
  TopSize, i: integer;
begin
  TopSize := 1;
  Tag := ANum;
  Status := False;

  { FilPanel }
  FilPanel := TPanel.Create(AForm);
  FilPanel.Parent := AForm;
  FilPanel.Left := AForm.Width - PnlWidth;
  FilPanel.Width := PnlWidth - BrdrSize;
  FilPanel.Height := PnlHeight;
  FilPanel.Top := PnlHeight * (ANum - 1) + BrdrSize;
  FilPanel.Anchors := [akRight, akTop];
  FilPanel.BorderStyle := bsSingle;
  FilPanel.BevelOuter := bvNone;

  { FieldBox }
  FieldBox := TComboBox.Create(FilPanel);
  FieldBox.Parent := FilPanel;
  FieldBox.Top := TopSize;
  FieldBox.Left := 4;
  FieldBox.ReadOnly := true;
  FieldBox.OnChange := @ChangeParam;
  Update(AList);
  //for i := 0 to AList.Count - 1 do
  //  FieldBox.Items.Add(AList.ValueFromIndex[i]);
  FieldBox.ItemIndex := 0;

  { ActionBox }
  ActionBox := TComboBox.Create(FilPanel);
  ActionBox.Parent := FilPanel;
  ActionBox.Left := FieldBox.Left + FieldBox.Width;
  ActionBox.Width := 84;
  ActionBox.Top := TopSize;
  ActionBox.ReadOnly := true;
  ActionBox.OnChange := @ChangeParam;

  for i :=0 to 6 do
    ActionBox.Items.Add(Actions[i]);
  ActionBox.ItemIndex := 0;

  { FilterEdit }
  FilterEdit := TEdit.Create(FilPanel);
  FilterEdit.Parent := FilPanel;
  FilterEdit.Left := ActionBox.Left + ActionBox.Width;
  FilterEdit.Width := 174;
  FilterEdit.Top := TopSize;
  FilterEdit.OnChange := @ChangeParam;

  { ApplyBtn }
  ApplyBtn := TSpeedButton.Create(FilPanel);
  ApplyBtn.Parent := FilPanel;
  ApplyBtn.Left := FilterEdit.Left + FilterEdit.Width;
  ApplyBtn.Top := TopSize;
  ApplyBtn.Glyph.LoadFromFile('apply_icon.bmp');
  ApplyBtn.OnClick := @ApplyFilter;

  { DelBtn }
  DelBtn := TSpeedButton.Create(FilPanel);
  DelBtn.Parent := FilPanel;
  DelBtn.Left := ApplyBtn.Left + ApplyBtn.Width;
  DelBtn.Top := TopSize;
  DelBtn.Glyph.LoadFromFile('del_icon.bmp');
  DelBtn.OnClick := @DeleteFilter;
end;

procedure TDirectoryFilter.ChangeParam(Sender: TObject);
begin
  Status := False;
  ApplyBtn.Enabled := True;
  if (Sender = FieldBox) then
    FilterEdit.Clear;
  FilterChangeStatus := True;
  InvalidateEvent(Sender);
end;

procedure TDirectoryFilter.DeleteFilter(Sender: TObject);
begin
  FilPanel.free;
  DelFilterNum := Tag;
  Status := False;
  FilterChangeStatus := True;
  InvalidateEvent(Sender);
end;

procedure TDirectoryFilter.ApplyFilter(Sender: TObject);
var
  i, j: integer;
begin
  if (FilterEdit.Caption = '') then
  begin
    showmessage(MetaData.TranslateList.Values['CheckFilter']);
    exit;
  end;
  Status := True;
  FilterChangeStatus := true;
  ApplyBtn.Enabled := false;
  i := FieldBox.ItemIndex + 1;
  if MetaData.Tables[FormTag].Fields[i].Reference <> nil then
    begin
      j := MetaData.Tables[FormTag].Fields[i].Reference.TableTag;
      FieldName := MetaData.Tables[j].Name +
        '.' + MetaData.Tables[j].Fields[1].Name;
      showmessage(FieldName);
    end
    else
    begin
      FieldName := MetaData.Tables[FormTag].Name + '.' +
        MetaData.Tables[FormTag].Fields[i].Name;
    end;
  if (ActionBox.Caption <> Actions[6]) then
  begin
    Action := ActionBox.Caption + ' ';
  end
  else
  begin
    Action := 'LIKE '
  end;

  TextEdit := FilterEdit.Caption;
  InvalidateEvent(Sender);
end;

procedure TDirectoryFilter.Update(AList: TStringList);
var
i: integer;
begin
  FieldBox.Items.Clear;
  for i := 0 to AList.Count - 1 do
    FieldBox.Items.Add(AList.ValueFromIndex[i]);
end;

end.

