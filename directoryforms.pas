unit directoryforms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, Meta, Menus, DbCtrls, Buttons, ExtCtrls, ModeratorMode,
  SQLgen, Filters;

type

  { TDirectoryForm }

  TDirectoryForm = class(TForm)
    FDataSource: TDataSource;
    FDBGrid: TDBGrid;
    FSQLQuery: TSQLQuery;
    FSQLTransaction: TSQLTransaction;
    FilterButton: TSpeedButton;
    AddFilterButton: TSpeedButton;
    InsertButton: TSpeedButton;
    EditButton: TSpeedButton;
    DeleteButton: TSpeedButton;
    procedure DeleteButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure FilterButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InsertButtonClick(Sender: TObject);
    procedure SetParams(Sender: TObject);
    procedure AddFilterButtonClick(Sender: TObject);
    procedure AddNewFilter();
    function GenDATA(ATag: integer): TStringList;
  private
    FilterNum: integer;
    DirectoryFilter: array of TDirectoryFilter;

  end;

  TTableForms = class
    FForms: array of TDirectoryForm;
    constructor Create;
  end;

var
  DirectoryForm: TDirectoryForm;
  TableForms: TTableForms;

implementation

{$R *.lfm}

constructor TTableForms.Create;
begin
  SetLength(FForms, length(MetaData.Tables));
  DelFilterNum := -1;
end;

procedure TDirectoryForm.SetParams(Sender: TObject);
begin
  Tag:= (Sender as TMenuItem).Tag;
  Caption:= MetaData.Tables[Tag].Caption;
  FSQLQuery.Close;
  FSQLQuery.SQL.Text := SQLGenerator.GenParams(Tag).Text;
  FSQLQuery.Active:= true;
  SQLGenerator.SetColName(FDBGrid, Tag);
  FilterNum := 1;
end;

procedure TDirectoryForm.FilterButtonClick(Sender: TObject);
begin
  FilterButton.free;
  FDBGrid.Width := Width - (PnlWidth + 2 * BrdrSize);

  { AddFilterButton }
  AddFilterButton := TSpeedButton.Create(TableForms.FForms[Tag]);
  AddFilterButton.Parent := TableForms.FForms[Tag];
  AddFilterButton.Left := TableForms.FForms[Tag].Width - PnlWidth;
  AddFilterButton.Top := TableForms.FForms[Tag].Height
                       - BrdrSize - AddFilterButton.Height;
  AddFilterButton.Width := PnlWidth - BrdrSize;
  AddFilterButton.Caption := MetaData.TranslateList.Values['CreateFilter'];
  AddFilterButton.OnClick := @AddFilterButtonClick;
  AddFilterButton.Anchors := [akRight, akBottom];
  AddNewFilter();
end;

procedure TDirectoryForm.EditButtonClick(Sender: TObject);
begin

end;

procedure TDirectoryForm.DeleteButtonClick(Sender: TObject);
begin

end;

procedure TDirectoryForm.FormCreate(Sender: TObject);
begin
  InvalidateEvent := @FormPaint;
end;

procedure TDirectoryForm.FormPaint(Sender: TObject);
var
  i: integer;
begin
  if (DelFilterNum <> -1) then
  begin
    for i := DelFilterNum to high(DirectoryFilter) - 1 do
    begin
      DirectoryFilter[i] := DirectoryFilter[i + 1];
      DirectoryFilter[i].Tag := i;
      DirectoryFilter[i].FilPanel.Top := PnlHeight * (i - 1) + BrdrSize;
    end;
    SetLength(DirectoryFilter, Length(DirectoryFilter) - 1);
    DelFilterNum := -1;
    FilterNum := FilterNum - 1;
  end;

  if (FilterChangeStatus = true) then
  begin
    SQLGenerator.GenFilters(Tag, DirectoryFilter, FSQLQuery);
    SQLGenerator.SetColName(FDBGrid, Tag);
    FilterChangeStatus := false;
  end;

end;

procedure TDirectoryForm.FormShow(Sender: TObject);
var
i: integer;
begin
  SQLGenerator.GenFilters(Tag, DirectoryFilter, FSQLQuery);
  SQLGenerator.SetColName(FDBGrid, Tag);
end;

procedure TDirectoryForm.InsertButtonClick(Sender: TObject);
begin

end;

procedure TDirectoryForm.AddFilterButtonClick(Sender: TObject);
var
  MaxNum: integer;
begin
  MaxNum := 16;
  if (FilterNum < MaxNum) then
  begin
    AddNewFilter();
  end;
end;

procedure TDirectoryForm.AddNewFilter();
begin
  SetLength(DirectoryFilter, FilterNum + 1);
  DirectoryFilter[FilterNum] := TDirectoryFilter.Create;
  DirectoryFilter[FilterNum].FormTag := Tag;
  DirectoryFilter[FilterNum].CreateFilter(TableForms.FForms[Tag], FilterNum, GenDATA(Tag));
  inc(FilterNum);
end;

function TDirectoryForm.GenDATA(ATag: integer): TStringList;
var
  TempList: TStringList;
  i: integer;
begin
  TempList:= TStringList.Create;
  for i:=0 to length(MetaData.Tables[ATag].Fields) - 1 do
  begin
    if (not Moderator.id_visible) then
    begin
      if (MetaData.Tables[ATag].Fields[i].Caption <> MetaData.TranslateList.Values['id']) then
      begin
        TempList.Append(MetaData.Tables[ATag].Fields[i].Caption);
      end;
    end
    else
    begin
      TempList.Append(MetaData.Tables[ATag].Fields[i].Caption);
    end;
  result:= TempList;
  end;
end;

end.

