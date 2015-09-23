unit SQLgen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Meta, Filters, dialogs, DBGrids, ModeratorMode, sqldb;

type
  TSQL = class
    function GenParams(ANum: integer): TStringList;
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

end.

