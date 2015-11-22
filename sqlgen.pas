unit SQLgen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, Meta, Filters, dialogs, DBGrids, ModeratorMode, sqldb,
  DBConnection, IBConnection;

type
  TSQL = class
    function GenParams(ANum : integer)                        : TStringList;
    function GenUniqId()                                      : integer;
    function GenInsertQuery(ANum : integer)                   : TStringList;
    function GenUpdateQuery(ANum : integer)                   : TStringList;
    function GenDeleteQuery(AName : string; ANum : integer)   : string;
    function GetId(ATag, ANum, Index : integer)               : integer;
    function GetNameField(ATag, Index : integer)              : string;

    procedure SetColName(ADBGrid : TDBGrid; ANum : integer);
    procedure GenFilters(ANum : integer; AFilter : array of TDirectoryFilter;
      ASQLQuery : TSQLQuery);
  end;

var
  SQLGenerator   : TSQL;

implementation

function TSQL.GenParams(ANum : integer) : TStringList;
var
  i, j       : integer;
  a, b       : TStringList;
  s          : String;
  fld        : TField;
  QueryCmd   : string;
begin
  a          := TStringList.Create;
  b          := TStringList.Create;
  Result     := TStringList.Create;
  QueryCmd   := 'SELECT ';

  with (MetaData) do
  begin
    a.Append('FROM ' + Tables[ANum].Name);
    for i :=0 to high(Tables[ANum].Fields) do
    begin
      fld    := Tables[ANum].Fields[i];

      if i > 0 then QueryCmd += ', ';
      if fld.Reference <> nil then
      begin
        j          := fld.Reference.TableTag;
        QueryCmd   += Tables[j].Name + '.' + Tables[j].Fields[1].Name
                      + ' AS ' + Tables[j].Name + Tables[j].Fields[1].Name;
        s          := 'INNER JOIN ' + Tables[j].Name + ' ON ' + Tables[j].Name
                      + '.' + fld.Reference.Name + ' = ' + Tables[ANum].Name
                      + '.' + Tables[ANum].Fields[i].Name;
        b.Append(s);
        a.Append(b.text);
        b.clear;
      end
      else
        QueryCmd   +=   ' ' + Tables[ANum].Name
                      + '.' + Tables[ANum].Fields[i].Name + ' AS '
                      + Tables[ANum].Name + Tables[ANum].Fields[i].Name;
    end;
  end;
  Result.Append(QueryCmd);
  Result.Append(a.text);
end;

procedure TSQL.SetColName(ADBGrid : TDBGrid; ANum : integer);
var
  i : integer;
begin
  with (ADBGrid.Columns) do
  begin
    Items[0].Visible            := Moderator.id_visible;
    for i :=0 to Count - 1 do
    begin
      Items[i].Title.Caption    := MetaData.Tables[ANum].Fields[i].Caption;
      Items[i].Width            := MetaData.Tables[ANum].Fields[i].Width;
    end;
  end;
end;

procedure TSQL.GenFilters(ANum : integer; AFilter : array of TDirectoryFilter;
  ASQLQuery : TSQLQuery);
var
  i, param      : integer;
  First         : boolean = True;
  QueryCmd      : string;
  ResultQuery   : TStringList;
begin
  ASQLQuery.Active   := False;
  param              := 0;
  ResultQuery        := GenParams(ANum);

  for i := 1 to high(AFilter) do
    if (AFilter[i].Status) then
    begin
      if (First) then QueryCmd   := 'WHERE '
      else QueryCmd              += ' AND ';
      First                      := False;
      QueryCmd                   += AFilter[i].FieldName
                                    + ' ' + AFilter[i].Action
                                    + ':' + IntToStr(param);
      inc(param);
    end;
  ResultQuery.Append(QueryCmd);
  ASQLQuery.SQL.Text := ResultQuery.Text;

  { /set perameters }
  if (param > 0) then
  begin
    param := 0;
    for i := 1 to high(AFilter) do
      if (AFilter[i].status) then
      begin
        with ASQLQuery.ParamByName(IntToStr(param)) do
        begin
          if (AFilter[i].Action = 'LIKE ') then
            AsString := '%' + AFilter[i].TextEdit + '%'
          else
            AsString := AFilter[i].TextEdit;
        end;
        param += 1;
      end;
  end;
  { /end }

  ASQLQuery.Active := true;
end;

function TSQL.GenUniqId() : integer;
begin
  with (DBDataModule.SQLQuery) do
  begin
    Close;
    SQL.Text   := 'SELECT GEN_ID(genuniq_id, 1) FROM RDB$DATABASE';
    Open;
    Result     := Fields[0].AsInteger;
  end;
end;

function TSQL.GenInsertQuery(ANum : integer) : TStringList;
var
  i            : integer;
  s1, s2, s3   : string;
begin
  with (MetaData.Tables[ANum]) do
  begin
    Result   := TStringList.Create;
    s1       := 'INSERT INTO ' + Name;
    s2       := '';
    s3       := '';
    for i := 0 to high(Fields) do
    begin
      s2  := s2 + Fields[i].Name;
      if i <> high(Fields) then s2 += ', ';

      s3  := s3 + ':p' + IntToStr(i) + ' ';
      if i <> high(Fields) then s3 += ', ';
    end;
  end;

  s1 += ' (' + s2 + ')';
  Result.Append(s1);
  Result.Append('VALUES (' + s3 + ')');
end;

function TSQL.GenUpdateQuery(ANum : integer) : TStringList;
var
  i      : integer;
  s     : string;
begin
  Result  := TStringList.Create;
  s       := '';

  with (MetaData.Tables[ANum]) do
  begin
    Result.Append('UPDATE ' + Name + ' SET ');
    for i := 1 to high(Fields) do
    begin
      s   := Fields[i].Name + ' = :p' + IntToStr(i);
      if i <> high(Fields) then
        s += ' , ';
      Result.Append(s);
    end;
  end;
  Result.Append('WHERE ID = :p0' );
end;

function TSQL.GenDeleteQuery(AName : string; ANum : integer) : string;
begin
  Result := 'DELETE FROM ' + AName + ' WHERE ID = ' + IntToStr(ANum);
end;

function TSQL.GetNameField(ATag, Index: integer): string;
var
  t: integer;
begin
  if MetaData.Tables[ATag].Fields[index + 1].Reference <> nil then
  begin
    t:= MetaData.Tables[ATag].Fields[index + 1].Reference.TableTag;
    Result:= MetaData.Tables[t].Name + '.' +
      MetaData.Tables[t].Fields[1].Name;
  end
  else
    Result:= MetaData.Tables[ATag].Name + '.' +
      MetaData.Tables[ATag].Fields[index + 1].Name;
end;

function TSQL.GetId(ATag, ANum, Index : integer) : integer;
var
  i                 : integer;
  SL                : TStringList;
  LDSource          : TDataSource;
  LSQLQuery         : TSQLQuery;
  LSQLTrans         : TSQLTransaction;
begin
  SL                   := TStringList.Create;
  LDSource             := TDataSource.Create(DBDataModule);
  LSQLQuery            := TSQLQuery.Create(DBDataModule);
  LSQLTrans            := TSQLTransaction.Create(DBDataModule);
  LDSource.DataSet     := LSQLQuery;
  LSQLTrans.DataBase   := DBDataModule.IBConnection;
  with (LSQLQuery) do
  begin
    DataBase           := DBDataModule.IBConnection;
    Transaction        := LSQLTrans;
  end;

  with (LSQLQuery) do
  begin
    Close;
    i          := MetaData.Tables[ATag].Fields[ANum + 1].Reference.TableTag;
    SQL.Text   := 'SELECT * FROM ' + MetaData.Tables[i].Name;
    Open;
  end;

  while not LSQLQuery.EOF do
  begin
    SL.Append(LSQLQuery.Fields[0].AsString);
    LSQLQuery.Next;
  end;
  LSQLQuery.Close;
  LDSource.Free;
  LSQLQuery.Free;
  LSQLTrans.Free;
  result := StrToInt(SL[Index]);
end;
end.

