unit Conflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Meta;

type

  TConflict = class;
  TConflictClass = class of TConflict;

  TConflict = class
  private
    RecordID     : integer;
    ConflictType : TConflictClass;
  public
    constructor Create(ARecordID : integer; AConflictType : TConflictClass);
  end;

  TTeacherConflict = class (TConflict)
  private
    class var FCaption : string;
  public
    class property Caption : string read FCaption write FCaption;
  end;

  TTeacherCourseConflict = class (TConflict)
  private
    class var FCaption : string;
  public
    class property Caption : string read FCaption write FCaption;
  end;

  TGroupConflict = class (TConflict)
  private
    class var FCaption : string;
  public
    class property Caption : string read FCaption write FCaption;
  end;

  TGroupCourseConflict = class (TConflict)
  private
    class var FCaption : string;
  public
    class property Caption : string read FCaption write FCaption;
  end;

  TClassRoomConflict = class (TConflict)
  private
    class var FCaption : string;
  public
    class property Caption : string read FCaption write FCaption;
  end;

  TConflictForm = class(TForm)
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ConflictForm: TConflictForm;

implementation

{$R *.lfm}

constructor TConflict.Create(ARecordID : integer; AConflictType : TConflictClass);
begin
  RecordID     := ARecordID;
  ConflictType := AconflictType;
end;


initialization

TTeacherConflict.Caption       := 'ПРЕПОДОВАТЕЛИ';
TTeacherCourseConflict.Caption := 'ПРЕПОДОВАТЕЛЬ КУРИРУЕТ';
TGroupConflict.Caption         := 'ГРУППЫ';
TGroupCourseConflict.Caption   := 'ПРЕДМЕТ У ГРУППЫ';
TClassroomConflict.Caption     := 'КАБИНЕТ';

end.


