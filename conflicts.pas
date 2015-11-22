unit Conflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  DBGrids, StdCtrls, Grids, Meta, sqldb, db, DBConnection, SQLgen;

type

  TConflict = class;
  TConflictClass = class of TConflict;

  TConflict = class
  private
    RecordID     : integer;
    ConflictID   : array of TConflict;
    ConflictType : TConflictClass;
    class procedure ExceptionalRows(AQuery: TSQLQuery; ASL, AConflictObjects: TStringList;
                                    AConflictType: TConflictClass; Field1, Field2, Field3: string);
    class procedure NonExistentLinks(AQuery: TSQLQuery; ASL, AConflictObjects: TStringList;
                                     AConflictType: TConflictClass; Field1, Field2, LinkTable: string);
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
    RightTreeView  : TTreeView;
    LeftTreeView : TTreeView;
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ConflictForm : TConflictForm;

implementation

{$R *.lfm}

{ TConflictForm }

procedure TConflictForm.FormPaint(Sender: TObject);
begin
  RightTreeView.Items.Clear;
  LeftTreeView.Items.Clear;
  with SQLQuery do begin
    Close;
    SQL.Append(SQLGenerator.GenParams(8).Text);
    Open;
  end;
  TConflict.Check(LeftTreeView, RightTreeView);
end;

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
var
  Query           : TSQLQuery;
  SL              : TStringList;
  ConflictObjects : TStringList;
  Node            : TTreeNode;
begin
  SL              := TStringList.Create;
  ConflictObjects := TStringList.Create;
  Query           := DBDataModule.SQLQuery;
  ExceptionalRows(
    Query, SL, ConflictObjects, TTeacherConflict,
    'weekday_id', 'pair_id', 'teacher_id');

  Node := RightTree.Items.Add(TTreeNode.Create(RightTree.Items), TTeacherConflict.Caption);
  //AddToTrees(LeftTree, RightTree, Node, ConfObjects);
end;

class procedure TTeacherCourseConflict.Check(LeftTree, RightTree: TTreeView);
var
  Query           : TSQLQuery;
  SL              : TStringList;
  ConflictObjects : TStringList;
  Node            : TTreeNode;
begin
  SL              := TStringList.Create;
  ConflictObjects := TStringList.Create;
  Query           := DBDataModule.SQLQuery;
  NonExistentLinks(
    Query, SL, ConflictObjects, TTeacherConflict,
    'teacher_id', 'subject_id', 'teachers_subjects');

  Node := RightTree.Items.Add(TTreeNode.Create(RightTree.Items), TTeacherCourseConflict.Caption);
end;

class procedure TGroupConflict.Check(LeftTree, RightTree: TTreeView);
var
  Query           : TSQLQuery;
  SL              : TStringList;
  ConflictObjects : TStringList;
  Node            : TTreeNode;
begin
  SL              := TStringList.Create;
  ConflictObjects := TStringList.Create;
  Query           := DBDataModule.SQLQuery;
  ExceptionalRows(
    Query, SL, ConflictObjects, TTeacherConflict,
    'weekday_id', 'pair_id', 'group_id');

  Node := RightTree.Items.Add(TTreeNode.Create(RightTree.Items), TGroupConflict.Caption);
end;

class procedure TGroupCourseConflict.Check(LeftTree, RightTree: TTreeView);
var
  Query           : TSQLQuery;
  SL              : TStringList;
  ConflictObjects : TStringList;
  Node            : TTreeNode;
begin
  SL              := TStringList.Create;
  ConflictObjects := TStringList.Create;
  Query           := DBDataModule.SQLQuery;
  NonExistentLinks(
    Query, SL, ConflictObjects, TTeacherConflict,
    'group_id', 'subject_id', 'groups_subjects');

  Node := RightTree.Items.Add(TTreeNode.Create(RightTree.Items), TGroupCourseConflict.Caption);
end;

class procedure TClassRoomConflict.Check(LeftTree, RightTree: TTreeView);
var
  Query           : TSQLQuery;
  SL              : TStringList;
  ConflictObjects : TStringList;
  Node            : TTreeNode;
begin
  SL              := TStringList.Create;
  ConflictObjects := TStringList.Create;
  Query           := DBDataModule.SQLQuery;
  ExceptionalRows(
    Query, SL, ConflictObjects, TTeacherConflict,
    'weekday_id', 'pair_id', 'classroom_id');

  Node := RightTree.Items.Add(TTreeNode.Create(RightTree.Items), TClassRoomConflict.Caption);
end;

{ /work with conflict procedures }
class procedure TConflict.ExceptionalRows(AQuery: TSQLQuery; ASL, AConflictObjects: TStringList;
                                          AConflictType: TConflictClass; Field1, Field2, Field3: string);
var
  i, j: integer;
  Conflict: TConflict;
begin
  AQuery.Close;
  with AQuery.SQL do
  begin
    Clear;
    Append('WITH sel AS');
    Append('( SELECT COUNT(*) AS cnt');
    Append(', l.' + Field1 + ' AS sel' + Field1);
    Append(', l.' + Field2 + ' AS sel' + Field2);
    Append(', l.' + Field3 + ' AS sel' + Field3);
    Append('FROM lessons l');
    Append('GROUP BY l.' + Field1 + ', l.' + Field2 + ', l.' + Field3);
    Append('HAVING COUNT(*) > 1');
    Append(') SELECT l.id AS lid');
    Append(', l.' + Field1 + ' AS l' + Field1);
    Append(', l.' + Field2 + ' AS l' + Field2);
    Append(', l.' + Field3 + ' AS l' + Field3);
    Append('FROM lessons l RIGHT JOIN sel ON');
    Append('l.' + Field1+ ' = sel' + Field1 + ' AND');
    Append('l.' + Field2+ ' = sel' + Field2 + ' AND');
    Append('l.' + Field3+ ' = sel' + Field3);
    Append('ORDER BY l.' + Field1 + ', l.' + Field2 + ', l.' + Field3);
  end;

  with AQuery do
  begin
    Open;
    First;
    while not EOF do
    begin
      ASL.AddObject(
        FieldByName('l' + Field1).AsString + '#' +
        FieldByName('l' + Field2).AsString + '##' +
        FieldByName('l' + Field3).AsString,
        TObject(Pointer(Integer(FieldByName('lid').AsInteger))));
      Next;
    end;
  end;

  for i := 0 to ASL.Count - 1 do
    AConflictObjects.AddObject(
      IntToStr(Integer(Pointer(ASL.Objects[i]))),
      TConflict.Create(Integer(Pointer(ASL.Objects[i])), AConflictType));

  with ASL do
    for i := 0 to Count - 1 do
      for j := 0 to Count - 1 do
        if (Strings[i] = Strings[j]) and (Objects[i] <> Objects[j]) then
        begin
          Conflict := AConflictObjects.Objects[i] as TConflict;
          SetLength(Conflict.ConflictID, Length(Conflict.ConflictID) + 1);
          Conflict.ConflictID[High(Conflict.ConflictID)] := AConflictObjects.Objects[j] as TConflict;
	end;
  showmessage(AConflictObjects.text);
end;

class procedure TConflict.NonExistentLinks(AQuery: TSQLQuery; ASL, AConflictObjects: TStringList;
                                           AConflictType: TConflictClass; Field1, Field2, LinkTable: string);
var
  i : integer;
begin
  AQuery.Close;
  with AQuery.SQL do begin
    Clear;
    Append('SELECT l.id AS lid, l.' + Field1 + ' AS l'+ Field1 +', l.' + Field2 + ' AS l' + Field2);
    Append('FROM LESSONS l WHERE NOT EXISTS( SELECT *');
    Append('FROM ' + LinkTable + ' AS tab');
    Append('WHERE tab.' + Field1 + ' = l.' + Field1 + ' AND tab.' + Field2 + ' = l.' + Field2 + ')');
  end;
  with AQuery do begin
    Open; First;
    while not EOF do begin
      ASL.AddObject(
        FieldByName('l' + Field1).AsString + '#' +
        FieldByName('l' + Field2).AsString,
        TObject(Pointer(Integer(FieldByName('lid').AsInteger))));
      Next;
    end;
  end;
  for i := 0 to ASL.Count - 1 do
    AConflictObjects.AddObject(
      IntToStr(Integer(Pointer(ASL.Objects[i]))),
      TConflict.Create(Integer(Pointer(ASL.Objects[i])), AConflictType));
end;
{ /end }

initialization

TTeacherConflict.Caption       := 'ПРЕПОДОВАТЕЛИ';
TTeacherCourseConflict.Caption := 'ПРЕПОДОВАТЕЛЬ КУРИРУЕТ';
TGroupConflict.Caption         := 'ГРУППЫ';
TGroupCourseConflict.Caption   := 'ПРЕДМЕТ У ГРУППЫ';
TClassroomConflict.Caption     := 'КАБИНЕТ';

end.


