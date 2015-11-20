unit Conflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  DBGrids, StdCtrls, Grids, Meta, sqldb, db, DBConnection;

type

  TConflict = class;
  TConflictClass = class of TConflict;

  TConflict = class
  private
    RecordID     : integer;
    ConflictType : TConflictClass;
  public
    class procedure Check(LeftTree, RightTree : TTreeView);
    constructor Create(ARecordID : integer; AConflictType : TConflictClass);
  end;

  TTeacherConflict = class (TConflict)
  private
    class var FCaption : string;
  public
    class procedure Check(LeftTree, RightTree : TTreeView);
    class property Caption : string read FCaption write FCaption;
  end;

  TTeacherCourseConflict = class (TConflict)
  private
    class var FCaption : string;
  public
    class procedure Check(LeftTree, RightTree : TTreeView);
    class property Caption : string read FCaption write FCaption;
  end;

  TGroupConflict = class (TConflict)
  private
    class var FCaption : string;
  public
    class procedure Check(LeftTree, RightTree : TTreeView);
    class property Caption : string read FCaption write FCaption;
  end;

  TGroupCourseConflict = class (TConflict)
  private
    class var FCaption : string;
  public
    class procedure Check(LeftTree, RightTree : TTreeView);
    class property Caption : string read FCaption write FCaption;
  end;

  TClassRoomConflict = class (TConflict)
  private
    class var FCaption : string;
  public
    class procedure Check(LeftTree, RightTree : TTreeView);
    class property Caption : string read FCaption write FCaption;
  end;

  { /TConflictForm }

  { TConflictForm }

  TConflictForm = class(TForm)
    DataSource    : TDataSource;
    HelpLabel     : TLabel;
    DblClickLabel : TLabel;
    ClickLabel    : TLabel;
    SQLQuery      : TSQLQuery;
    LeftTreeView  : TTreeView;
    RightTreeView : TTreeView;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ConflictForm : TConflictForm;

implementation

{$R *.lfm}

constructor TConflict.Create(ARecordID : integer; AConflictType : TConflictClass);
begin
  RecordID     := ARecordID;
  ConflictType := AconflictType;
end;

{ /check }
class procedure TConflict.Check(LeftTree, RightTree: TTreeView);
begin
  RightTree.Items.Clear;
  LeftTree.Items.Clear;

  TTeacherConflict.Check(LeftTree, RightTree);
  TGroupConflict.Check(LeftTree, RightTree);
  TClassRoomConflict.Check(LeftTree, RightTree);
  TTeacherCourseConflict.Check(LeftTree, RightTree);
  TGroupCourseConflict.Check(LeftTree, RightTree);
end;

class procedure TTeacherConflict.Check(LeftTree, RightTree: TTreeView);
begin

end;

class procedure TTeacherCourseConflict.Check(LeftTree, RightTree: TTreeView);
begin

end;

class procedure TGroupConflict.Check(LeftTree, RightTree: TTreeView);
begin

end;

class procedure TGroupCourseConflict.Check(LeftTree, RightTree: TTreeView);
begin

end;

class procedure TClassRoomConflict.Check(LeftTree, RightTree: TTreeView);
begin

end;
{ /end }

initialization

TTeacherConflict.Caption       := 'ПРЕПОДОВАТЕЛИ';
TTeacherCourseConflict.Caption := 'ПРЕПОДОВАТЕЛЬ КУРИРУЕТ';
TGroupConflict.Caption         := 'ГРУППЫ';
TGroupCourseConflict.Caption   := 'ПРЕДМЕТ У ГРУППЫ';
TClassroomConflict.Caption     := 'КАБИНЕТ';

end.


