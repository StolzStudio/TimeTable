unit directoryforms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, Meta, Menus, DbCtrls, Buttons, ExtCtrls, ModeratorMode,
  SQLgen, Filters, ChangeFormData, StdCtrls, DBConnection;

type

  { TEditingManager }
  TEditingManager = class
  private
    FFormsChange : array of TFormData;
  public
    procedure OpenFormEditingTable(AChangeType: TChangeType;
                                   ATag: integer;
                                   AList: TStringList);
    procedure InsertRecord(ATag: integer; AList: TStringList);
    procedure EditRecord(ATag: integer; AList: TStringList);
    procedure DeleteRecord(AId, ATag: integer);

    function CheckFormOpenForId(AId : integer) : TFormData;
    procedure SetItemIndex(ANum, AIndex, ANum1, AIndex1: integer);
  end;

  { TDirectoryForm }
  TDirectoryForm = class(TForm)
    AddFilterButton   : TSpeedButton;
    FilterButton1: TButton;
    FilterPanel: TPanel;
    InsertButton      : TSpeedButton;
    EditButton        : TSpeedButton;
    DeleteButton      : TSpeedButton;

    { /SQL }
    FDataSource       : TDataSource;
    FDBGrid           : TDBGrid;
    FSQLQuery         : TSQLQuery;
    FSQLTransaction   : TSQLTransaction;

    { /form procedures }
    procedure InsertButtonClick(Sender : TObject);
    procedure DeleteButtonClick(Sender : TObject);
    procedure EditButtonClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure FormPaint(Sender : TObject);
    procedure FormShow(Sender : TObject);

    { /work with sql }
    procedure SetParams(Sender : TObject);
    procedure AddFilterButtonClick(Sender : TObject);
    procedure AddNewFilter();
  private
    FilterNum         : integer;
    EditingManager    : TEditingManager;
    DirectoryFilter   : array of TDirectoryFilter;
  end;

  TTableForms = class
    FForms : array of TDirectoryForm;
    procedure GlobalUpdate();
    constructor Create;
  end;

  function GenDATA(ATag : integer) : TStringList;
var
  TableForms: TTableForms;

  const Left_ = 50;

implementation

{$R *.lfm}

constructor TTableForms.Create;
begin
  SetLength(FForms, length(MetaData.Tables));
  UpdateEvent    := @GlobalUpdate;
  DelFilterNum   := -1;
end;

procedure TTableForms.GlobalUpdate();
var
  i: integer;
begin
  for i := 0 to high(FForms) do
    if (FForms[i] <> nil) then
      with FForms[i] do
      begin
        SQLGenerator.GenFilters(Tag, DirectoryFilter, FSQLQuery);
        SQLGenerator.SetColName(FDBGrid, Tag);
      end;
end;

procedure TDirectoryForm.SetParams(Sender: TObject);
begin
  Tag       := (Sender as TMenuItem).Tag;
  Caption   := MetaData.Tables[Tag].Caption;

  FSQLQuery.Close;
  FSQLQuery.SQL.Text   := SQLGenerator.GenParams(Tag).Text;
  FSQLQuery.Active     := true;

  SQLGenerator.SetColName(FDBGrid, Tag);
  FilterNum := 1;
  EditingManager := TEditingManager.Create;
end;

procedure TDirectoryForm.EditButtonClick(Sender: TObject);
var
  i    : integer;
  LS   : TStringList;
begin
  LS    := TStringList.Create;
  for i :=0 to FSQLQuery.Fields.Count - 1 do
  begin
    LS.Append(string(FSQLQuery.Fields.Fields[i].Value));
  end;
  EditingManager.EditRecord(Tag, LS);
  Ls.free;
end;

procedure TEditingManager.EditRecord(ATag : integer; AList : TStringList);
var
 Fd  : TFormData;
 i   : integer;
begin
 Fd    := nil;
 for i :=0 to high(FFormsChange) do
    if FFormsChange[i].DataId = StrToInt(AList[0]) then
      Fd := FFormsChange[i];
 if Fd = nil then
 begin
   OpenFormEditingTable(ctEdit, ATag, AList);
 end
 else
   Fd.show;
end;

procedure TEditingManager.OpenFormEditingTable(AChangeType : TChangeType;
  ATag : integer; AList : TStringList);
var
  i, k   : integer;
  SL     : TStringList;
begin
  Randomize;
  SetLength(FFormsChange, length(FFormsChange) + 1);
  FFormsChange[high(FFormsChange)] := TFormData.Create(Application);
  SL := TStringList.Create;

  with FFormsChange[high(FFormsChange)] do
  begin
    Tag           := ATag;
    Left          := high(FFormsChange) * Left_;
    Top           := ATag * Height;
    Action        := AChangeType;
    BorderStyle   := bsSingle;

    if AList.Count <> 0 then
      DataID := StrToInt(AList[0]);

    with MetaData.Tables[ATag] do
      for i := 0 to high(Fields) do
        if (Fields[i].Caption <> MetaData.TranslateList.Values['id']) then
        begin
          SL := GetDataFieldOfIndex(i);

          SetLength(DataControl, length(DataControl) + 1);
          if (Fields[i].Reference <> nil) then
          begin
            CreateComboBox(SL, Fields[i].Caption, Fields[i].Width);
            k := high(DataControl);
            if (AChangeType = ctEdit) then
              (DataControl[k] as TComboBox).ItemIndex := SL.IndexOf(Alist[i])
            else
              (DataControl[k] as TComboBox).ItemIndex := Random(SL.Count);
          end
          else
          begin
            CreateEdit(Fields[i].Caption, Fields[i].Width);
            k := high(DataControl);
            (DataControl[k] as TEdit).Text := Alist[i];
          end;
        end;
    if AChangeType = ctEdit then
    begin
      Caption := MetaData.TranslateList.Values['UpdateRecord'];
    end
    else
    begin
      Caption := MetaData.TranslateList.Values['InsertRecord'];
    end;

    DataControl[0].Tag := high(FFormsChange);
    CreateApplyBtn();
    Show;
  end;
  UpdateEvent;
end;
procedure TDirectoryForm.DeleteButtonClick(Sender: TObject);
begin
  if FSQLQuery.Fields.FieldByNumber(1).Value = Null then exit;
  EditingManager.DeleteRecord(FSQLQuery.Fields.FieldByNumber(1).Value, Tag);
end;

procedure TEditingManager.DeleteRecord(AId, ATag : integer);
var
  i: LongInt;
  FD: TFormData;
begin
  with (DBDataModule) do
  begin
    SQLQuery.Close;
    SQLQuery.SQL.Text := SQLGenerator.GenDeleteQuery(MetaData.Tables[ATag].Name, AId);
    i                 := MessageDLG(MetaData.TranslateList.Values['DeleteRecord'],
                         mtConfirmation, mbYesNoCancel, 0);
    if (i = mrYes) then
    try
      SQLQuery.ExecSQL;
      SQLTransaction.Commit;
      FD := CheckFormOpenForId(AId);
      if (FD <> nil) then
        FD.close;
    except
      MessageDLG(MetaData.TranslateList.Values['DeleteError'], mtError,[mbYes], 0);
    end;
  end;
  UpdateEvent;
end;

function TEditingManager.CheckFormOpenForId(AId : integer) : TFormData;
var
  i, k : integer;
begin
  k := -1;
  for i := 0 to high(FFormsChange) do
  begin
    if (FFormsChange[i].DataID = AId) then
    begin
      exit(FFormsChange[i]);
    end;
  end;
  result := nil;
end;

procedure TDirectoryForm.FormCreate(Sender : TObject);
begin
  InvalidateEvent := @FormPaint;
end;

procedure TDirectoryForm.FormPaint(Sender : TObject);
var
  i : integer;
begin
  if (DelFilterNum <> -1) then
  begin
    for i := DelFilterNum to high(DirectoryFilter) - 1 do
    begin
      DirectoryFilter[i]                := DirectoryFilter[i + 1];
      DirectoryFilter[i].Tag            := i;
      DirectoryFilter[i].FilPanel.Top   := PnlHeight * (i - 1) + BrdrSize;
    end;
    SetLength(DirectoryFilter, Length(DirectoryFilter) - 1);
    DelFilterNum   := -1;
    FilterNum      := FilterNum - 1;
  end;

  if (FilterChangeStatus = true) then
  begin
    SQLGenerator.GenFilters(Tag, DirectoryFilter, FSQLQuery);
    SQLGenerator.SetColName(FDBGrid, Tag);
    FilterChangeStatus := false;
  end;

end;

procedure TDirectoryForm.FormShow(Sender : TObject);
begin
  SQLGenerator.GenFilters(Tag, DirectoryFilter, FSQLQuery);
  SQLGenerator.SetColName(FDBGrid, Tag);
end;

procedure TDirectoryForm.InsertButtonClick(Sender : TObject);
var
  i: integer;
  temp: TStringList;
begin
  temp:= TStringList.Create;
  for i:=0 to FSQLQuery.Fields.Count - 1 do
    temp.Append(string(FSQLQuery.Fields.Fields[i].Value));
  EditingManager.InsertRecord(Tag, temp);
end;

procedure TEditingManager.InsertRecord(ATag : integer; AList : TstringList);
begin
  OpenFormEditingTable(ctInsert, ATag, AList);
end;

procedure TDirectoryForm.AddFilterButtonClick(Sender : TObject);
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
  DirectoryFilter[FilterNum].CreateFilter(TableForms.FForms[Tag].FilterPanel, FilterNum, GenDATA(Tag));
  inc(FilterNum);
end;

function GenDATA(ATag : integer) : TStringList;
var
  TempList : TStringList;
  i        : integer;
begin
  TempList := TStringList.Create;

  for i := 0 to length(MetaData.Tables[ATag].Fields) - 1 do
  begin
    if (not Moderator.id_visible) then
    begin
      if (MetaData.Tables[ATag].Fields[i].Caption <> MetaData.TranslateList.Values['id']) then
        TempList.Append(MetaData.Tables[ATag].Fields[i].Caption);
    end
    else
      TempList.Append(MetaData.Tables[ATag].Fields[i].Caption);
  end;
  Result := TempList;
end;

procedure TEditingManager.SetItemIndex(ANum, AIndex, ANum1, AIndex1 : integer);
begin
  with FFormsChange[high(FFormsChange)] do
  begin
    (DataControl[ANum] as TComboBox).ItemIndex:= AIndex;
    (DataControl[ANum1] as TComboBox).ItemIndex:= AIndex1;
  end;
end;
end.

