unit DBConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, db, FileUtil;

type
  TConnect = (Connect, Disconnect, Error);

  TProperties = class
  private
    FDBPassword: string;
    FDBUserName: string;
    FDBName: string;
    FDBConnect: TConnect;
  published
    property DBPassword: string read FDBPassword write FDBPassword;
    property DBName: string read FDBName write FDBName;
    property DBUserName: string read FDBUserName write FDBUserName;
    property DBConnect: TConnect read FDBConnect write FDBConnect;
  end;

  { TDBDataModule }

  TDBDataModule = class(TDataModule)
    DSource: TDataSource;
    IBConnection: TIBConnection;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
    procedure DBConnect();
    procedure DBDisconnect();
  end;

var
  DBDataModule: TDBDataModule;
  DBProperties: TProperties;

implementation

{$R *.lfm}

{ TDBDataModule }

procedure TDBDataModule.DataModuleCreate(Sender: TObject);
begin
  DBProperties := TProperties.Create;
  DBProperties.DBPassword := 'masterkey';
  DBProperties.DBName := 'TIMETABLE.FDB';
  DBProperties.DBUserName := 'SYSDBA';
end;

procedure TDBDataModule.DBConnect();
begin
  IBConnection.UserName := DBProperties.DBUserName;
  IBConnection.Password := DBProperties.DBPassword;
  IBConnection.DatabaseName := DBProperties.DBName;
  IBConnection.Connected := true;
  DBProperties.DBConnect := Connect;
end;

procedure TDBDataModule.DBDisconnect();
begin
  IBConnection.Connected := false;
  DBProperties.DBConnect := Disconnect;
end;

end.

