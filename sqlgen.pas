unit SQLgen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Meta;

function SQLGenParams(ANum: integer): TStringList;

implementation

function SQLGenParams(ANum: integer): TStringList;
var
  i: integer;
  a, b, res: TStringList;
  head: string;
  s: String;
  temp: TField;
begin
  a:= TStringList.Create;
  b:= TStringList.Create;
  res:= TStringList.Create;
  head:= 'SELECT ';
  a.Append('FROM ' + MetaData.Tables[ANum].Name);
  for i:=0 to high(MetaData.Tables[ANum].Fields) do
  begin
    temp:= MetaData.Tables[ANum].Fields[i];
    if i > 0 then head += ', ';
    if temp.Reference <> nil then
    begin
      head += MetaData.Tables[temp.Reference.TableTag].Name + '.' +
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
      head+= ' '+ MetaData.Tables[ANum].Name + '.' +
          MetaData.Tables[ANum].Fields[i].Name;
    end;
  end;
  res.Append(head);
  res.Append(a.text);
  result:= res;
end;
end.

