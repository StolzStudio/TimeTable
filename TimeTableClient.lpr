program TimeTableClient;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  DBConnection, Forms, Main, ModeratorMode, Meta, directoryforms,
  SQLgen, begin_prog, Filters, ChangeFormData, Utimetable, Conflicts;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  BeginForm := TBeginForm.Create(Application);
  BeginForm.Show;
  while BeginForm.BeginTimer.Enabled do
    Application.ProcessMessages;
  Application.CreateForm(TDBDataModule, DBDataModule);
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFormData, FormData);
  Application.CreateForm(TTimeTableForm, TimeTableForm);
  Application.Run;
end.

