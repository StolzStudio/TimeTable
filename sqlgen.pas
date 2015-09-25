unit SQLgen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, Meta, Filters, dialogs, DBGrids, ModeratorMode, sqldb,
  DBConnection, IBConnection;

type
  TSQL = class
    function GenParams(ANum: integer): TStringList;
    function GenUniqId(): integer;
    function GenInsertQuery(ANum: integer): TStringList;
    function GenUpdateQuery(ANum: integer): TStringList;
    function GenDeleteQuery(AName: string; ANum: integer): string;
    function GetId(ATag, ANum, Index: integer): integer;
    procedure GenFilters(ANum: integer; AFilter: array of TDirectoryFilter; ASQLQuery: TSQLQuery);
    procedure SetColName(ADBGrid: TDBGrid; ANum: integer);
  private
    QueryCmd: string;
    ResultQuery: TStringList;
  end;

var
  SQLGenerator: TSQL;

implementation

function TSQL.GenParams(ANum: integer): TStringList;
var
  i: integer;
  a, b: TStringList;
  s: String;
  temp: TField;
begin
  a := TStringList.Create;
  b := TStringList.Create;
  ResultQuery := TStringList.Create;
  QueryCmd := 'SELECT ';
  a.Append('FROM ' + MetaData.Tables[ANum].Name);
  for i:=0 to high(MetaData.Tables[ANum].Fields) do
  begin
    temp:= MetaData.Tables[ANum].Fields[i];
    if i > 0 then QueryCmd += ', ';
    if temp.Reference <> nil then
    begin
      QueryCmd += MetaData.Tables[temp.Reference.TableTag].Name + '.' +
           MetaData.Tables[temp.Reference.TableTag].Fields[1].Name;
      s := 'INNER JOIN ' + MetaData.Tables[temp.Reference.TableTag].Name +
         ' ON ' + MetaData.Tables[temp.Reference.TableTag].Name  + '.' +
         temp.Reference.Name + ' = ' + MetaData.Tables[ANum].Name + '.' +
         MetaData.Tables[ANum].Fields[i].Name;
      b.Append(s);
      a.Append(b.text);
      b.clear;
    end
    else
    begin
      QueryCmd += ' '+ MetaData.Tables[ANum].Name + '.' +
          MetaData.Tables[ANum].Fields[i].Name;
    end;
  end;
  ResultQuery.Append(QueryCmd);
  ResultQuery.Append(a.text);
  Result := ResultQuery;
end;

procedure TSQL.SetColName(ADBGrid: TDBGrid; ANum: integer);
var
  i: integer;
begin
  ADBGrid.Columns.Items[0].Visible := Moderator.id_visible;
  for i:=0 to ADBGrid.Columns.Count - 1 do
  begin
    ADBGrid.Columns.Items[i].Title.Caption := MetaData.Tables[ANum].Fields[i].Caption;
    ADBGrid.Columns.Items[i].Width := MetaData.Tables[ANum].Fields[i].Width;
  end;
end;

procedure TSQL.GenFilters(ANum: integer; AFilter: array of TDirectoryFilter; ASQLQuery: TSQLQuery);
var
  i, param: integer;
  First: boolean = True;
begin
  ASQLQuery.Active := False;
  param := 0;
  ResultQuery := GenParams(ANum);
  QueryCmd := '';
  for i := 1 to high(AFilter) do
  begin
    if (AFilter[i].Status) then
    begin
      if (First) then
      begin
        QueryCmd := 'WHERE ';
      end
      else
      begin
        QueryCmd += ' AND ';
      end;
      QueryCmd += AFilter[i].FieldName
               + ' ' + AFilter[i].Action + ':' + IntToStr(param);
      inc(param);
      First := False;

    end;
  end;
  ResultQuery.Append(QueryCmd);
  ASQLQuery.SQL.Text := ResultQuery.Text;
  if (param > 0) then
  begin
    param := 0;
    for i := 1 to high(AFilter) do
    begin
      if (AFilter[i].status) then
      begin
        with ASQLQuery.ParamByName(IntToStr(param)) do
        begin
          if (AFilter[i].Action = 'LIKE ') then
          begin
            AsString := '%' + AFilter[i].TextEdit + '%';
          end
          else
          begin
            AsString := AFilter[i].TextEdit;
          end;
        end;
        param += 1;
      end;
    end;
  end;
  ASQLQuery.Active := true;
end;

function TSQL.GenUniqId(): integer;
begin
  DBDataModule.SQLQuery.Close;
  DBDataModule.SQLQuery.SQL.Text:=
    'SELECT GEN_ID(genuniq_id, 1) FROM RDB$DATABASE';
  DBDataModule.SQLQuery.Open;
  result := DBDataModule.SQLQuery.Fields[0].AsInteger;
end;

function TSQL.GenInsertQuery(ANum: integer): TStringList;
var
  i, k: integer;
  s1, s2, s3: string;
  temp: TStringList;
begin
  temp := TStringList.Create;
  s1 := 'INSERT INTO ' + MetaData.Tables[ANum].Name;
  for i:=0 to high(MetaData.Tables[ANum].Fields) do
  begin
    s2 := s2 + MetaData.Tables[ANum].Fields[i].Name;
    if i <> high(MetaData.Tables[ANum].Fields) then
      s2 += ', ';
    s3 := s3 + ':p' + IntToStr(i) + ' ';
    if i <> high(MetaData.Tables[ANum].Fields) then
      s3 += ', ';
  end;
  s1 += ' (' + s2 + ')';
  temp.Append(s1);
  temp.Append('VALUES (' + s3 + ')');
  result := temp;
end;

function TSQL.GenUpdateQuery(ANum: integer): TStringList;
var
  i, k: integer;
  s1, s2: string;
  temp: TStringList;
begin
  temp := TStringList.Create;
  s1 := 'UPDATE ' + MetaData.Tables[ANum].Name + ' SET ';
  temp.Append(s1);
  s1 := '';
  for i := 1 to high(MetaData.Tables[ANum].Fields) do
  begin
    s1 :=MetaData.Tables[ANum].Fields[i].Name + ' = :p' + IntToStr(i);
    if i <> high(MetaData.Tables[ANum].Fields) then
      s1 += ' , ';
    temp.Append(s1);
  end;
  temp.Append('WHERE ID = :p0' );
  result := temp;
end;

function TSQL.GenDeleteQuery(AName: string; ANum: integer): string;
begin
  Result :='DELETE FROM ' + AName + ' WHERE ID = ' + IntToStr(ANum);
end;

function TSQL.GetId(ATag, ANum, Index: integer): integer;
var
  i, k, j: integer;
  temp: TStringList;
  TempDSource: TDataSource;
  TempSQLQuery: TSQLQuery;
  TempSQLTransaction: TSQLTransaction;
begin
  TempDSource:= TDataSource.Create(DBDataModule);
  TempSQLQuery:= TSQLQuery.Create(DBDataModule);
  TempSQLTransaction:= TSQLTransaction.Create(DBDataModule);
  TempDSource.DataSet:= TempSQLQuery;
  TempSQLQuery.DataBase:= DBDataModule.IBConnection;
  TempSQLQuery.Transaction:= TempSQLTransaction;
  TempSQLTransaction.DataBase:= DBDataModule.IBConnection;
  temp:= TStringList.Create;
  i:= MetaData.Tables[ATag].Fields[ANum + 1].Reference.TableTag;
  TempSQLQuery.Close;
  TempSQLQuery.SQL.Text:= 'SELECT * FROM ' + MetaData.Tables[i].Name;
  TempSQLQuery.Open;
  while not TempSQLQuery.EOF do
  begin
    temp.Append(TempSQLQuery.Fields[0].AsString);
    TempSQLQuery.Next;
  end;
  TempSQLQuery.Close;
  TempDSource.Free;
  TempSQLQuery.Free;
  TempSQLTransaction.Free;
  result:= StrToInt(temp[Index]);
end;
end.

