program TimeTableClient;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  DBConnection, Forms, Main, ModeratorMode, Meta, directoryforms, SQLgen
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TDBDataModule, DBDataModule);
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDirectoryForm, DirectoryForm);
  Application.Run;
end.

