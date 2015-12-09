unit Meta;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, db, DBConnection, Dialogs;

type

  TTypeField    = (TInt, TStr, TDate);
  TLanguage     = (ruRU, enUS);

  TField = class
  private
    FName       : string;
    FCaption    : string;
    FWidth      : integer;
    FTableTag   : integer;
    FType       : TTypeField;
    FReference  : TField;
  published
    property Name        : string read FName write FName;
    property Caption     : string read FCaption write FCaption;
    property Width       : integer read FWidth write FWidth;
    property TableTag    : integer read FTableTag write FTableTag;
    property FieldType   : TTypeField read FType write FType;
    property Reference   : TField read FReference write FReference;
  end;

  TTable = class
  public
    Fields      : array of TField;

    { /fill MetaData }
    procedure FillDataTable(ANameTable : string);
    procedure FillDataField(ANameTable : string);
    procedure FillReferencedField();

    { /work with fields }
    function FindFieldName(AName : string)          : TField;
    function GetDataFieldOfIndex(AIndex : integer)  : TStringList;
  private
    FName       : string;
    FCaption    : string;
    function GetFieldType(ANum : integer) : TTypeField;
  published
    property Name        : string read FName write FName;
    property Caption     : string read FCaption write FCaption;
  end;

  TMeta = class
    Tables               : array of TTable;
    TranslateList        : TStringList;

    { /work with tables }
    function FindTableName(AName : string)    : TTable;
    function CreateItemName(AName : string)   : string;
    function CheckReference(AName : string)   : integer;
  end;


var
  MetaData: TMeta;



implementation

{ work with table }
function TMeta.CreateItemName(AName :String) : string;
var
  i : integer;
begin
  for i := 1 to length(AName) - 1 do
    if AName[i] <> ' ' then
      Result += AName[i]
    else
      break;
end;

function TMeta.CheckReference(AName : string) : integer;
var
  i : integer;
begin
  i := 0;
  while i <= length(AName) do
  begin
    if AName[i] = '_' then
      exit(i);
    inc(i);
  end;
  Result := 0;
end;

function TMeta.FindTableName(AName : string) : TTable;
var
  i : integer = 0;
begin
  while i < length(Tables) do
  begin
    if Tables[i].Name = AName then
      exit(Tables[i]);
    inc(i);
  end;
  Result := nil;
end;

{ fill tables and fields }
procedure TTable.FillDataTable(ANameTable : string);
begin
   Name      := MetaData.CreateItemName(ANameTable);
   FCaption  := MetaData.TranslateList.Values[Name];
   FillDataField(ANameTable);
end;

procedure TTable.FillDataField(ANameTable : string);
var
   s                     : string;
   TempDSource           : TDataSource;
   TempSQLQuery          : TSQLQuery;
   TempSQLTransaction    : TSQLTransaction;
begin

   TempSQLTransaction          := TSQLTransaction.Create(DBDataModule);
   TempSQLTransaction.DataBase := DBDataModule.IBConnection;

   TempSQLQuery                := TSQLQuery.Create(DBDataModule);
   TempDSource                 := TDataSource.Create(DBDataModule);
   TempDSource.DataSet         := TempSQLQuery;
   with (TempSQLQuery) do
   begin
     DataBase       := DBDataModule.IBConnection;
     Transaction    := TempSQLTransaction;
     Active         := false;
     SQL.Text       :=
       'SELECT  R.RDB$FIELD_NAME, F.RDB$FIELD_TYPE ' +
       'FROM RDB$FIELDS F, RDB$RELATION_FIELDS R WHERE F.RDB$FIELD_NAME = ' +
       'R.RDB$FIELD_SOURCE and R.RDB$SYSTEM_FLAG = 0 and RDB$RELATION_NAME = ' +
       '''' + ANameTable + '''';
     Open;
   end;
   while not TempSQLQuery.EOF do
   begin
     s := '';
     s := MetaData.CreateItemName(TempSQLQuery.Fields[0].AsString);
     SetLength(Fields, length(Fields) + 1);
     Fields[high(Fields)] := TField.Create;
     with (Fields[high(Fields)]) do
     begin
       Name         := s;
       Caption      := MetaData.TranslateList.Values[s];
       TableTag     := high(MetaData.Tables);
       Width        := StrToInt(MetaData.TranslateList.Values['width_' + s]);
       FieldType    := GetFieldType(TempSQLQuery.Fields[1].AsInteger);
     end;
     TempSQLQuery.Next;
   end;
   TempSQLQuery.Close;
   TempDSource.Free;
   TempSQLQuery.Free;
   TempSQLTransaction.Free;
end;

function TTable.GetFieldType(ANum : integer): TTypeField;
begin
  case ANum of
    8  : Result := TInt;
    37 : Result := TStr;
    12 : Result := TDate;
  end;
end;

procedure TTable.FillReferencedField();
var
  i, j, n  : integer;
  s, s1    : string;
begin
  for i := 0 to high(Fields) do
  begin
    s    := '';
    s1   := '';
    n    := MetaData.CheckReference(Fields[i].Name);
    if n <> 0 then
      with (Fields[i]) do
      begin
        for j := 1 to n - 1 do
          s          += Name[j];
        for j := n + 1 to length(Name) do
          s1         += Name[j];
        Reference    := TField.Create;
        Reference    := MetaData.FindTableName(s + 'S').FindFieldName(s1);
        Caption      := MetaData.TranslateList.Values['reference_' + Fields[i].Name];
      end;
  end;
end;

function TTable.FindFieldName(AName : string) : TField;
var
  i : integer;
begin
  for i := 0 to length(Fields) - 1 do
    if Fields[i].Name = AName then
      exit(Fields[i]);
  Result := nil;
end;

function TTable.GetDataFieldOfIndex(AIndex : integer) : TStringList;
var
  SL        : TStringList;
  FSQLQuery : TSQLQuery;
begin
  FSQLQuery            := DBDataModule.SQLQuery1;
  FSQLQuery.Close;
  FSQLQuery.SQL.Text   := 'SELECT * FROM ';
  SL                   := TStringList.Create;

  if Fields[AIndex].Reference <> nil then
    FSQLQuery.SQL.Text := FSQLQuery.SQL.Text +
      MetaData.Tables[Fields[AIndex].Reference.TableTag].Name
  else
    FSQLQuery.SQL.Text := FSQLQuery.SQL.Text +
      MetaData.Tables[Fields[AIndex].TableTag].Name;

  FSQLQuery.Open;
  while not FSQLQuery.EOF do
  begin
    SL.Append(FSQLQuery.Fields[1].AsString);
    FSQLQuery.Next;
  end;
  Result := SL;
end;

end.

