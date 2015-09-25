unit Meta;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, db, DBConnection, Dialogs;

type

  TTypeField = (TInt, TStr);
  TLanguage = (ruRU, enUS);

  TField = class
  private
    FName: string;
    FCaption: string;
    FWidth: integer;
    FTableTag: integer;
    FType: TTypeField;
    FReference: TField;
  published
    property Name: string read FName write FName;
    property Caption: string read FCaption write FCaption;
    property Width: integer read FWidth write FWidth;
    property TableTag: integer read FTableTag write FTableTag;
    property FieldType: TTypeField read FType write FType;
    property Reference: TField read FReference write FReference;
  end;

  TTable = class
  public
    Fields: array of TField;
    procedure FillDataTable(ANameTable: string);
    procedure FillDataField(ANameTable: string);
    procedure FillReferencedField();
    function FindFieldName(AName: string): TField;
    function GetDataFieldOfIndex(AIndex: integer): TStringList;
  private
    FName: string;
    FCaption: string;
  published
    property Name: string read FName write FName;
    property Caption: string read FCaption write FCaption;
  end;

  TMeta = class
    Tables: array of TTable;
    TranslateList: TStringList;
    function FindTableName(AName: string): TTable;
  end;


var
  MetaData: TMeta;


  function CreateItemName(AName: string): string;
  function CheckReference(AName: string): integer;

implementation

{ not classes functions }
function CreateItemName(AName:String): string;
var
   i: integer;
begin
   for i := 1 to length(AName) - 1 do
   begin
     if AName[i] <> ' ' then
       Result += AName[i]
     else
       break;
   end;
end;

function CheckReference(AName: string): integer;
 var
   i: integer;
 begin
   i := 0;
   while i <= length(AName) do
   begin
     if AName[i] = '_' then
       exit(i);
     inc(i);
   end;
 result := 0;
 end;

{ fill tables and fields }
procedure TTable.FillDataTable(ANameTable: string);
begin
   Name := CreateItemName(ANameTable);
   FCaption := MetaData.TranslateList.Values[Name];
   FillDataField(ANameTable);
end;

procedure TTable.FillDataField(ANameTable: string);
var
   s: string;
   TempDSource: TDataSource;
   TempSQLQuery: TSQLQuery;
   TempSQLTransaction: TSQLTransaction;
begin
   TempDSource := TDataSource.Create(DBDataModule);
   TempSQLQuery := TSQLQuery.Create(DBDataModule);
   TempSQLTransaction := TSQLTransaction.Create(DBDataModule);
   TempDSource.DataSet := TempSQLQuery;
   TempSQLQuery.DataBase := DBDataModule.IBConnection;
   TempSQLQuery.Transaction := TempSQLTransaction;
   TempSQLTransaction.DataBase := DBDataModule.IBConnection;
   TempSQLQuery.Active := false;
   TempSQLQuery.SQL.Text :=
     'SELECT  R.RDB$FIELD_NAME, F.RDB$FIELD_TYPE ' +
     'FROM RDB$FIELDS F, RDB$RELATION_FIELDS R WHERE F.RDB$FIELD_NAME = ' +
     'R.RDB$FIELD_SOURCE and R.RDB$SYSTEM_FLAG = 0 and RDB$RELATION_NAME = ' +
     '''' + ANameTable + '''';
   TempSQLQuery.Open;
   while not TempSQLQuery.EOF do
   begin
     s := '';
     s := CreateItemName(TempSQLQuery.Fields[0].AsString);
     SetLength(Fields, length(Fields) + 1);
     Fields[high(Fields)] := TField.Create;
     Fields[high(Fields)].Name := s;
     Fields[high(Fields)].Caption := MetaData.TranslateList.Values[s];
     Fields[high(Fields)].TableTag := high(MetaData.Tables);
     Fields[high(Fields)].Width := StrToInt(MetaData.TranslateList.Values['width_' + s]);
     TempSQLQuery.Next;
   end;
   TempSQLQuery.Close;
   TempDSource.Free;
   TempSQLQuery.Free;
   TempSQLTransaction.Free;
end;

procedure TTable.FillReferencedField();
var
  i, j, n: integer;
  s, s1: string;
begin
  for i := 0 to high(Fields) do
  begin
    s := ''; s1 := '';
    n := CheckReference(Fields[i].Name);
    if n <> 0 then
    begin
      for j := 1 to n - 1 do
        s += Fields[i].Name[j];
      for j := n + 1 to length(Fields[i].Name) do
        s1 += Fields[i].Name[j];
      Fields[i].Reference := TField.Create;
      Fields[i].Reference := MetaData.FindTableName(s + 'S').FindFieldName(s1);
      Fields[i].Caption := MetaData.TranslateList.Values['reference_' + Fields[i].Name];
    end;
  end;
end;

function TMeta.FindTableName(AName: string): TTable;
var
  i: integer = 0;
begin
  while i < length(Tables) do
  begin
    if Tables[i].Name = AName then
      exit(Tables[i]);
    inc(i);
  end;
  result:= nil;
end;

function TTable.FindFieldName(AName: string): TField;
 var
   i: integer;
 begin
   for i := 0 to length(Fields) - 1 do
   begin
     if Fields[i].Name = AName then
     begin
       exit(Fields[i]);
     end;
   end;
   result:= nil;
 end;

function TTable.GetDataFieldOfIndex(AIndex: integer): TStringList;
 var
   i: integer;
   temp: TStringList;
 begin
   temp:= TStringList.Create;
   DBDataModule.SQLQuery.Close;
   DBDataModule.SQLQuery.SQL.Text:= 'SELECT * FROM ';
   if Fields[AIndex].Reference <> nil then
     DBDataModule.SQLQuery.SQL.Text:= DBDataModule.SQLQuery.SQL.Text +
       MetaData.Tables[Fields[AIndex].Reference.TableTag].Name
   else
     DBDataModule.SQLQuery.SQL.Text:= DBDataModule.SQLQuery.SQL.Text +
       MetaData.Tables[Fields[AIndex].TableTag].Name;
   DBDataModule.SQLQuery.Open;
   while not DBDataModule.SQLQuery.EOF do
   begin
     temp.Append(DBDataModule.SQLQuery.Fields[1].AsString);
     DBDataModule.SQLQuery.Next;
   end;
   result:= temp;
 end;

end.

