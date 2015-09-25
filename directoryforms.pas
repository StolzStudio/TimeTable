unit directoryforms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, Meta, Menus, DbCtrls, Buttons, ExtCtrls, ModeratorMode,
  SQLgen, Filters, ChangeFormData, StdCtrls, DBConnection;

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
    procedure EditRecord(ATag: integer; AList: TStringList);
    procedure InsertRecord(ATag: integer; AList: TStringList);
    procedure DeleteRecord(AId, ATag: integer);
    procedure AddNewFilter();
    procedure OpenFormEditingTable(AChangeType: TChangeType;
                                        ATag: integer; AList: TStringList);
    function GenDATA(ATag: integer): TStringList;
    function CheckFormOpenForId(AId: integer): TFormData;
  private
    FilterNum: integer;
    FFormsChange: array of TFormData;
    DirectoryFilter: array of TDirectoryFilter;
  end;

  TTableForms = class
    FForms: array of TDirectoryForm;
    procedure GlobalUpdate();
    constructor Create;
  end;

var
  DirectoryForm: TDirectoryForm;
  TableForms: TTableForms;

  const Left_ = 50;

implementation

{$R *.lfm}

constructor TTableForms.Create;
begin
  SetLength(FForms, length(MetaData.Tables));
  UpdateEvent := @GlobalUpdate;
  DelFilterNum := -1;
end;

procedure TTableForms.GlobalUpdate();
var
  i: integer;
begin
  for i := 0 to high(FForms) do
  begin
    if (FForms[i] <> nil) then
    begin
      with FForms[i] do
      begin
        SQLGenerator.GenFilters(Tag, DirectoryFilter, FSQLQuery);
        SQLGenerator.SetColName(FDBGrid, Tag);
      end;
    end;
  end;
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
var
  i: integer;
  temp: TStringList;
begin
  temp:= TStringList.Create;
  for i:=0 to FSQLQuery.Fields.Count - 1 do
  begin
    temp.Append(string(FSQLQuery.Fields.Fields[i].Value));
  end;
  EditRecord(Tag, temp);
end;

procedure TDirectoryForm.EditRecord(ATag: integer; AList: TStringList);
var
 temp: TFormData;
 i: integer;
begin
 temp:= nil;
 for i :=0 to high(FFormsChange) do
    if FFormsChange[i].DataId = StrToInt(AList[0]) then
      temp := FFormsChange[i];
 if temp = nil then
 begin
   OpenFormEditingTable(ctEdit, ATag, AList);
 end
 else
   temp.show;
end;

procedure TDirectoryForm.OpenFormEditingTable(AChangeType: TChangeType;
  ATag: integer; AList: TStringList);
var
  i, k: integer;
  temp: TStringList;
begin
  Randomize;
  SetLength(FFormsChange, length(FFormsChange) + 1);
  FFormsChange[high(FFormsChange)] := TFormData.Create(Application);
  with FFormsChange[high(FFormsChange)] do
  begin
    Tag := ATag;
    Left := high(FFormsChange)*Left_;
    Top := ATag*Height;
    Action := AChangeType;
    BorderStyle := bsSingle;
    if AList.Count <> 0 then
      DataID := StrToInt(AList[0]);
    temp:= TStringList.Create;
    k := SQLGenerator.GenUniqId();
    with MetaData.Tables[ATag] do
    begin
      for i:= 0 to high(Fields) do
      begin
        if (Fields[i].Caption <> MetaData.TranslateList.Values['id']) then
        begin
          temp:= GetDataFieldOfIndex(i);
          SetLength(DataControl, length(DataControl) + 1);
          if (Fields[i].Reference <> nil) then
          begin
            CreateComboBox(temp, Fields[i].Caption, Fields[i].Width);
            k:= high(DataControl);
            if (AChangeType = ctEdit) then
            begin
              (DataControl[k] as TComboBox).ItemIndex:= temp.IndexOf(Alist[i])
            end
            else
            begin
              (DataControl[k] as TComboBox).ItemIndex:= Random(temp.Count);
            end;
          end
          else
          begin
            CreateEdit(Fields[i].Caption, Fields[i].Width);
            k:= high(DataControl);
            (DataControl[k] as TEdit).Text:= Alist[i];
          end;
        end;
      end;
    end;
    if AChangeType = ctEdit then
    begin
      Caption:= MetaData.TranslateList.Values['UpdateRecord'];
    end
    else
    begin
      Caption:= MetaData.TranslateList.Values['InsertRecord'];
    end;
    DataControl[0].Tag:= high(FFormsChange);
    CreateApplyBtn();
    Show;
  end;
end;
procedure TDirectoryForm.DeleteButtonClick(Sender: TObject);
begin
  if FSQLQuery.Fields.FieldByNumber(1).Value = Null then exit;
  DeleteRecord(FSQLQuery.Fields.FieldByNumber(1).Value, Tag);
end;

procedure TDirectoryForm.DeleteRecord(AId, ATag: integer);
var
  i: LongInt;
  temp: TFormData;
begin
  DBDataModule.SQLQuery.Close;
  DBDataModule.SQLQuery.SQL.Text := SQLGenerator.GenDeleteQuery(MetaData.Tables[ATag].Name, AId);
  i := MessageDLG(MetaData.TranslateList.Values['DeleteRecord'],
      mtConfirmation, mbYesNoCancel, 0);
  if (i = mrYes) then
  begin
    try
      DBDataModule.SQLQuery.ExecSQL;
      DBDataModule.SQLTransaction.Commit;
      temp := CheckFormOpenForId(AId);
      if temp <> nil then
        temp.close;
    except
      MessageDLG(MetaData.TranslateList.Values['DeleteError'],
      mtError,[mbYes], 0);
    end;
  end;
  UpdateEvent;
end;

function TDirectoryForm.CheckFormOpenForId(AId: integer): TFormData;
var
  i, k: integer;
begin
  k:= -1;
  for i:=0 to high(FFormsChange) do
  begin
    if (FFormsChange[i].DataID = AId) then
    begin
      exit(FFormsChange[i]);
    end;
  end;
  result:= nil;
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
var
  i: integer;
  temp: TStringList;
begin
  temp:= TStringList.Create;
  for i:=0 to FSQLQuery.Fields.Count - 1 do
    temp.Append(string(FSQLQuery.Fields.Fields[i].Value));
  InsertRecord(Tag, temp);
end;

procedure TDirectoryForm.InsertRecord(ATag: integer; AList: TstringList);
begin
  OpenFormEditingTable(ctInsert, ATag, AList);
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

