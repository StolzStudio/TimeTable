unit directoryforms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, Meta, Menus, ModeratorMode, SQLgen;

type

  { TDirectoryForm }

  TDirectoryForm = class(TForm)
    FDataSource: TDataSource;
    FDBGrid: TDBGrid;
    FSQLQuery: TSQLQuery;
    FSQLTransaction: TSQLTransaction;
    procedure SetParams(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
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
end;

procedure TDirectoryForm.SetParams(Sender: TObject);
var
  i: integer;
begin
  Tag:= (Sender as TMenuItem).Tag;
  Caption:= MetaData.Tables[Tag].Caption;
  FSQLQuery.Close;
  FSQLQuery.SQL.Text := SQLGenParams(Tag).Text;
  FSQLQuery.Active:= true;
  if not (Moderator.id_visible) then
    FDBGrid.Columns.Items[0].Visible := false;
  for i:=0 to FDBGrid.Columns.Count - 1 do
  begin
    FDBGrid.Columns.Items[i].Title.Caption := MetaData.Tables[Tag].Fields[i].Caption;
    FDBGrid.Columns.Items[i].Width := MetaData.Tables[Tag].Fields[i].Width;
  end;

end;

end.

