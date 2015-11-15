unit Conflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Meta;

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

implementation


constructor TConflict.Create(ARecordID : integer; AConflictType : TConflictClass);
begin
  RecordID     := ARecordID;
  ConflictType := AconflictType;
end;


initialization

  TTeacherConflict.Caption       := 'преподователи';
  TTeacherCourseConflict.Caption := 'преподователь курирует';
  TGroupConflict.Caption         := 'группы';
  TGroupCourseConflict.Caption   := 'предмет у группы';
  TClassroomConflict.Caption     := 'аудитории';

end.

