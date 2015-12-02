unit Conflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, sqldb, db, DBConnection, SQLgen, DirectoryForms, ChangeFormData, Meta;

type

  TConflict = class;
  TConflictClass = class of TConflict;

  TConflict = class
  private
    RecordID     : integer;
    ConflictID   : array of TConflict;
    ConflictType : TConflictClass;
    class procedure ExceptionalRows(AQuery : TSQLQuery; ASL, AConflictObjects : TStringList;
                                    AConflictType : TConflictClass; Field1, Field2, Field3 : string);
    class procedure NonExistentLinks(AQuery : TSQLQuery; ASL, AConflictObjects : TStringList;
                                     AConflictType : TConflictClass; Field1, Field2, LinkTable : string);
    class procedure TimeRows(AQuery : TSQLQuery; ASL, AConflictObjects : TStringList;
                             AConflictType : TConflictClass; Field1, Field2 : string);
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

  TDateTimeConflict = class (Tconflict)
  private
    class var FCaption : string;
  public
    class procedure Check(LeftTree, RightTree : TTreeView);
    class property Caption : string read FCaption write Fcaption;
  end;

  { /TConflictForm }

  { TConflictForm }

  TConflictForm = class(TForm)
    DataSource    : TDataSource;
    HelpLabel     : TLabel;
    DblClickLabel : TLabel;
    ClickLabel    : TLabel;
    SQLQuery      : TSQLQuery;
    RightTreeView : TTreeView;
    LeftTreeView  : TTreeView;
    procedure ConflictUpdate;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);

    function GetRecord(RecordID: integer): TStringList;
    procedure LeftTreeViewDblClick(Sender: TObject);
    procedure RightTreeViewDblClick(Sender: TObject);
  private
    EditingManager : TEditingManager;

    function ParseNode(ARecordID : integer) : TStringList;
  public
    { public declarations }
  end;

var
  ConflictForm : TConflictForm;

  procedure AddToTrees(LeftTree, RightTree: TTreeView; AParentNode: TTreeNode;
                       AConflictObjects: TStringList);
  function ConflictCaption(AConflictType: TConflictClass): string;

implementation

{$R *.lfm}

{ TConflictForm }

procedure TConflictForm.ConflictUpdate;
begin
  RightTreeView.Items.Clear;
  LeftTreeView.Items.Clear;
  with SQLQuery do begin
    Close;
    SQL.Clear;
    SQL.Append(SQLGenerator.GenParams(high(MetaData.Tables)).Text);
    Open;
  end;
  TConflict.Check(LeftTreeView, RightTreeView);
end;

procedure TConflictForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TConflictForm.FormCreate(Sender: TObject);
begin
  EditingManager := TEditingManager.Create;
end;

procedure TConflictForm.FormShow(Sender: TObject);
begin
  ConflictUpdate;
end;

function TConflictForm.GetRecord(RecordID: integer): TStringList;
var
  FieldsName : array of String;

  i, j       : integer;
  ATag       : integer;
  fld        : TField;
begin
  Result := TStringList.Create;

  with (MetaData) do
  begin
    ATag  := high(Tables);
    for i := 0 to high(Tables[ATag].Fields) do
    begin
      fld := Tables[ATag].Fields[i];
      setlength(FieldsName, Length(FieldsName) + 1);

      if fld.Reference <> nil then
      begin
        j          := fld.Reference.TableTag;
        FieldsName[i] := Tables[j].Name + Tables[j].Fields[1].Name;
      end else
        FieldsName[i] := Tables[ATag].Name + Tables[ATag].Fields[i].Name;
    end;
    SQLQuery.Locate(FieldsName[0], Integer(RecordId), []);
    for i := 1 to 8 do
    begin
      Result.Append(string(SQLQuery.FieldByName(FieldsName[i]).value) + '  ');
    end;
  end;
  setlength(FieldsName, 0);
end;

function TConflictForm.ParseNode(ARecordID : integer) : TStringList;
var
  i, k : integer;
  s    : TStringList;
  m    : string;
begin
  s  := TStringList.Create;
  s  := GetRecord(ARecordID);
  Result := TStringList.Create;
  Result.Append(IntToStr(ARecordID));

  for i := 0 to 7 do
  begin
    m := s[i];
    k := pos('  ', s[i]);
    delete(m, k, k + 2);
    Result.Append(m);
  end;
end;

procedure TConflictForm.LeftTreeViewDblClick(Sender: TObject);
var
  Node      : TTreeNode;
  RightNode : TTreeNode;

begin
  Node := LeftTreeView.Selected;
  if (Node.GetFirstChild <> nil) then
  begin
    EditingManager.OpenFormEditingTable(
                                        ctEdit,
                                        high(MetaData.Tables),
                                        ParseNode(Integer(Node.Data))
                                        );
  end
  else begin
    RightNode := TObject(Node.Data) as TTreeNode;
    RightTreeView.Items.SelectOnlyThis(RightNode);
    RightNode.Expand(true);
    ActiveControl := RightTreeView;
  end;
end;

procedure TConflictForm.RightTreeViewDblClick(Sender: TObject);
var
  Tree : TTreeView;
  Conflict : TConflict;
begin
  Tree     := Sender as TTreeView;
  Conflict := TObject(Tree.Selected.Data) as TConflict;
  if (Conflict = nil) then exit;
  EditingManager.OpenFormEditingTable(
                                      ctEdit,
                                      high(MetaData.Tables),
                                      ParseNode(Conflict.RecordID)
                                      );
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
  TDateTimeConflict.Check(LeftTree,RightTree);
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
  Query           := DBDataModule.SQLQuery1;
  ExceptionalRows(
                  Query, SL, ConflictObjects, TTeacherConflict,
                  'weekday_id', 'pair_id', 'teacher_id'
                  );

  Node := RightTree.Items.Add(TTreeNode.Create(RightTree.Items), TTeacherConflict.Caption);
  AddToTrees(LeftTree, RightTree, Node, ConflictObjects);
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
                   Query, SL, ConflictObjects, TTeacherCourseConflict,
                   'teacher_id', 'subject_id', 'teachers_subjects'
                   );

  Node := RightTree.Items.Add(TTreeNode.Create(RightTree.Items), TTeacherCourseConflict.Caption);
  AddToTrees(LeftTree, RightTree, Node, ConflictObjects);
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
                  Query, SL, ConflictObjects, TGroupConflict,
                  'weekday_id', 'pair_id', 'group_id'
                  );

  Node := RightTree.Items.Add(TTreeNode.Create(RightTree.Items), TGroupConflict.Caption);
  AddToTrees(LeftTree, RightTree, Node, ConflictObjects);
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
                   Query, SL, ConflictObjects, TGroupCourseConflict,
                   'group_id', 'subject_id', 'groups_subjects'
                   );

  Node := RightTree.Items.Add(TTreeNode.Create(RightTree.Items), TGroupCourseConflict.Caption);
  AddToTrees(LeftTree, RightTree, Node, ConflictObjects);
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
                  Query, SL, ConflictObjects, TClassRoomConflict,
                  'weekday_id', 'pair_id', 'classroom_id'
                  );

  Node := RightTree.Items.Add(TTreeNode.Create(RightTree.Items), TClassRoomConflict.Caption);
  AddToTrees(LeftTree, RightTree, Node, ConflictObjects);
end;

class procedure TDateTimeConflict.Check(LeftTree, RightTree : TTreeView);
var
  Query           : TSQLQuery;
  SL              : TStringList;
  ConflictObjects : TStringList;
  Node            : TTreeNode;
begin
  SL              := TStringList.Create;
  ConflictObjects := TStringList.Create;
  Query           := DBDataModule.SQLQuery;
  TimeRows(
           Query, SL,
           ConflictObjects, TDateTimeConflict,
           'begincourse', 'endcourse'
           );
  Node := RightTree.Items.Add(TTreeNode.Create(RightTree.Items), TDateTimeConflict.Caption);
  AddToTrees(LeftTree, RightTree, Node, ConflictObjects);
end;

procedure AddToTrees(LeftTree, RightTree: TTreeView; AParentNode: TTreeNode;
                     AConflictObjects: TStringList);
var
  i, j: integer;
  Node, LeftNode: TTreeNode;
  Conflict: TConflict;
begin
  with AConflictObjects do begin
    for i := 0 to AConflictObjects.Count - 1 do begin
      Conflict := Objects[i] as TConflict;
      Node := RightTree.Items.AddChildObject(AParentNode, ConflictForm.GetRecord(Conflict.RecordID).text, Conflict);
      for j := 0 to Length(Conflict.ConflictID) - 1 do
        RightTree.Items.AddChildObject(Node, ConflictForm.GetRecord(Conflict.ConflictID[j].RecordID).text, Conflict.ConflictID[j]);


      LeftNode := LeftTree.Items.FindNodeWithData(Pointer(Conflict.RecordID));
      if LeftNode = nil then
        LeftNode := LeftTree.Items.AddObject(TTreeNode.Create(LeftTree.Items),
                    ConflictForm.GetRecord(Conflict.RecordID).text, Pointer(Conflict.RecordID));
      LeftTree.Items.AddChildObject(LeftNode, ConflictCaption(Conflict.ConflictType), Node);
    end;
  end;
end;

function ConflictCaption(AConflictType: TConflictClass): string;
begin
  if AConflictType = TTeacherConflict       then Result := TTeacherConflict.Caption;
  if AConflictType = TGroupConflict         then Result := TGroupConflict.Caption;
  if AConflictType = TClassroomConflict     then Result := TClassroomConflict.Caption;
  if AConflictType = TTeacherCourseConflict then Result := TTeacherCourseConflict.Caption;
  if AConflictType = TGroupCourseConflict   then Result := TGroupCourseConflict.Caption;
  if AConflictType = TDateTimeConflict      then Result := TDateTimeConflict.Caption;
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
                    TObject(Pointer(Integer(FieldByName('lid').AsInteger)))
                    );
      Next;
    end;
  end;

  for i := 0 to ASL.Count - 1 do
    AConflictObjects.AddObject(
                               IntToStr(Integer(Pointer(ASL.Objects[i]))),
                               TConflict.Create(Integer(Pointer(ASL.Objects[i])), AConflictType)
                               );

  with ASL do
    for i := 0 to Count - 1 do
      for j := 0 to Count - 1 do
        if (Strings[i] = Strings[j]) and (Objects[i] <> Objects[j]) then
        begin
          Conflict := AConflictObjects.Objects[i] as TConflict;
          SetLength(Conflict.ConflictID, Length(Conflict.ConflictID) + 1);
          Conflict.ConflictID[High(Conflict.ConflictID)] := AConflictObjects.Objects[j] as TConflict;
	end;
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
                               TConflict.Create(Integer(Pointer(ASL.Objects[i])), AConflictType)
                               );
end;

class procedure TConflict.TimeRows(AQuery : TSQLQuery; ASL, AConflictObjects : TStringList;
                                   AConflictType : TConflictClass; Field1, Field2 : string);
var
  i : integer;
begin
  AQuery.Close;
  with AQuery.SQL do begin
    Clear;
    Append('SELECT id, ' + Field1 + ', ' + Field2);
    Append('FROM LESSONS WHERE ' + Field1 + ' > ' + Field2);
  end;
  with AQuery do begin
    Open; First;
    while not EOF do begin
      ASL.AddObject(
        FieldByName(Field1).AsString + '#' +
        FieldByName(Field2).AsString,
        TObject(Pointer(Integer(FieldByName('id').AsInteger))));
      Next;
    end;
  end;
  for i := 0 to ASL.Count - 1 do
    AConflictObjects.AddObject(
                               IntToStr(Integer(Pointer(ASL.Objects[i]))),
                               TConflict.Create(Integer(Pointer(ASL.Objects[i])), AConflictType)
                               );
end;

{ /end }

initialization

TTeacherConflict.Caption       := 'ПРЕПОДОВАТЕЛИ';
TTeacherCourseConflict.Caption := 'ПРЕПОДОВАТЕЛЬ КУРИРУЕТ';
TGroupConflict.Caption         := 'ГРУППЫ';
TGroupCourseConflict.Caption   := 'ПРЕДМЕТ У ГРУППЫ';
TClassroomConflict.Caption     := 'КАБИНЕТ';
TDateTimeConflict.Caption      := 'ПЕРИОД';

end.


