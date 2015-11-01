unit Utimetable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  Grids, ExtCtrls, StdCtrls, CheckLst, Filters, DirectoryForms, Meta, SQLGen,
  ChangeFormData, DBConnection;

type

  { TTimeTableForm }

  TTimeTableForm = class(TForm)
    { /interface }
    ApplyButton       : TButton;
    ColComboBox       : TComboBox;
    RowLabel          : TLabel;
    RowComboBox       : TComboBox;
    DataListBox       : TCheckListBox;
    ColListBox        : TCheckListBox;
    ColLabel          : TLabel;
    RowListBox        : TCheckListBox;
    StringGrid        : TStringGrid;
    { /end }

    { /filter }
    FilterButton      : TButton;
    FilterPanel       : TPanel;
    FilterLabel       : TLabel;
    { /end }

    { /SQL }
    FDataSource       : TDataSource;
    FSQLQuery         : TSQLQuery;
    FSQLTransaction   : TSQLTransaction;
    { /end }

    procedure ApplyButtonClick(Sender : TObject);
    procedure DataListBoxItemClick(Sender: TObject; Index: integer);
    procedure FilterButtonClick(Sender: TObject);

    procedure FormCreate(Sender : TObject);
    procedure FormPaint(Sender: TObject);
    procedure RowListBoxItemClick(Sender: TObject; Index: integer);
    procedure StringGridDblClick(Sender : TObject);
    procedure StringGridDrawCell(Sender : TObject; aCol, aRow : Integer;
                                 aRect : TRect; aState : TGridDrawState);
    procedure StringGridMouseDown(Sender : TObject; Button : TMouseButton;
                                  Shift : TShiftState; X, Y : Integer);
    procedure StringGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure StringGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure UpdateRowsHeight(Index : integer);
    procedure FillGridData();
  private
    DirectoryFilter   : array of TDirectoryFilter;
    DataArray         : array of array of TStringList;
    ImgArray          : array [0..2] of TPicture;
    EditingManager    : TEditingManager;
    DragDropFlag      : boolean;
    MouseDownFlag     : Boolean;
    FilterNum         : integer;
    DefRowHeight      : integer;
    CurrRowHeight     : integer;
    Row               : integer;
    Col               : integer;
    kX, kY            : integer;
    cX, cY            : integer;

    procedure FillComboBox(AList : TStringList);
    procedure FillListBox(AColList, ARowList : TStringList);
    procedure UpdateHeaderVisible();
    procedure ChangeCaptionColumn(AColList, ARowList : TStringList);
    procedure DrawImg(ACanvas : TCanvas; ARect : TRect;
                      ACountItems : integer; ANum : integer);
    procedure AddNewFilter();
    procedure DrawArrow(ACanvas : TCanvas; ARect : TRect;
                        aRow : Integer; AText : String);
    procedure DrawText(ACanvas : TCanvas; ARect : TRect;
                       ASL : TStringList; Acnt : integer;
                       aRow, aCol : integer);

    { /editing }
    procedure InsertClick(Ax, Ay: integer);
    procedure DeleteClick(Ax, Ay: integer);
    procedure EditClick(Ax, Ay: integer);
    procedure DragDropRecord(EndX, EndY: integer);
    { /end }

    function GetCountCheckedItems() : integer;
    function GetListDataCell(Ax, Ay: integer): TStringList;
  public
    procedure SetParam(Sender : TObject);
  end;

var
  TimeTableForm : TTimeTableForm;

implementation

var
  ListNamesImg: array [0..2] of string =
    ('tt_add.png', 'tt_edit.png', 'tt_del.png');

  const Margin          = 2;
  const DefHeightFont   = 17;
  const DefCountStr     = 8;
  const DefWidthCol     = 350;
  const DefWidthImg     = 15;
{$R *.lfm}

{ TTimeTableForm }

procedure TTimeTableForm.FormCreate(Sender : TObject);
var
  i : integer;
begin
  SetLength(DirectoryFilter, 0);
  InvalidateEvent := @FormPaint;
  EditingManager  := TEditingManager.Create;
  FilterNum       := 1;
  DragDropFlag    := False;

  for i := 0 to high(ListNamesImg) do
  begin
    ImgArray[i] := TPicture.Create;
    ImgArray[i].LoadFromFile('icon\' + ListNamesImg[i]);
  end;
end;

procedure TTimeTableForm.FormPaint(Sender: TObject);
var
  i : integer;
begin
  if (DelFilterNum <> -1) then
  begin
    for i := DelFilterNum to high(DirectoryFilter) - 1 do
    begin
      DirectoryFilter[i]                := DirectoryFilter[i + 1];
      DirectoryFilter[i].Tag            := i;
      DirectoryFilter[i].FilPanel.Top   := PnlHeight * (i - 1) + BrdrSize;
    end;
    SetLength(DirectoryFilter, Length(DirectoryFilter) - 1);
    DelFilterNum   := -1;
    FilterNum      := FilterNum - 1;
  end;

  if (FilterChangeStatus = true) then
  begin
    FillGridData();
    FilterChangeStatus := false;
  end;
end;

procedure TTimeTableForm.RowListBoxItemClick(Sender: TObject; Index: integer);
begin
  UpdateHeaderVisible();
end;

procedure TTimeTableForm.StringGridDblClick(Sender : TObject);
var
  count : integer;
  SL    : TStringList;
begin
  if (Row = 0) or (Col = 0) then exit;
  SL := DataArray[Row - 1][Col - 1];

  with (StringGrid) do
  begin
    if RowHeights[Row] > DefRowHeight then
    begin
      if (SL = nil) then
      begin
        RowHeights[Row] := DefRowHeight;
        exit;
      end;
      count := round(SL.Count / DefCountStr);
      if RowHeights[Row] < DefRowHeight*count then
        RowHeights[Row] := DefRowHeight*count
      else
        RowHeights[Row] := DefRowHeight
    end
    else
    if (SL <> nil) then
      RowHeights[Row] := DefRowHeight * round(SL.Count / DefCountStr);
  end;
end;

procedure TTimeTableForm.StringGridDrawCell(Sender : TObject; aCol, aRow : Integer;
                                            aRect : TRect; aState : TGridDrawState);
var
  c, cnt : integer;
  SL           : TStringList;
  PairNum      : integer = 8;
begin
  cnt := GetCountCheckedItems + 1;

  if (length(DataArray) <> 0) and (aRow <> 0) and (aCol <> 0) then
    with (StringGrid) do
    begin

      Canvas.Draw(DefWidthCol + aRect.Left - ImgArray[0].Width - Margin, aRect.Top +
        Margin, ImgArray[0].Graphic);

      SL := DataArray[aRow - 1][aCol - 1];
      if (SL <> nil) then
      begin
        DrawImg(StringGrid.Canvas, aRect, cnt, 0);
        c := SL.Count div PairNum;
        DrawText(Canvas, aRect, Sl, cnt, aRow, aCol);

        if (StringGrid.RowHeights[aRow] < c * DefRowHeight) then
          DrawArrow(StringGrid.Canvas, aRect, aRow, ' ↓ ' +
                    IntToStr(c - RowHeights[aRow] div DefRowHeight));
      end;
  end;
end;

procedure TTimeTableForm.DrawText(ACanvas : TCanvas; ARect : TRect;
                                  ASL : TStringList; Acnt : integer;
                                  aRow, aCol : integer);
var
  i, j: integer;
begin
  j := -1;
  for i := 0 to ASL.Count - 1 do
    if (ASL[i] <> '') then
    begin
      if (DataListBox.Checked[i - (i div DefCountStr) * DefCountStr]) then
      begin
        inc(j);
        ACanvas.TextOut(ARect.Left + Margin, ARect.Top + j * DefHeightFont, ASL[i]);
      end;
    end
    else if (i <> DataArray[aRow - 1][aCol - 1].Count - 1) then
    begin
      inc(j);
      DrawImg (StringGrid.Canvas, ARect, Acnt, round(i / DefCountStr));
    end;
end;

procedure TTimeTableForm.DrawArrow(ACanvas : TCanvas; ARect : TRect;
                                   aRow : Integer; AText : String);
begin
  with (ACanvas) do
  begin
    Font.Bold;
    Font.Color  := clGreen;
    Font.Size   := 10;

    TextOut(DefWidthCol + aRect.Left - 27 - Margin,
            aRect.Top + StringGrid.RowHeights[aRow] - 20, AText);

    Font.Color  := clBlack;
    Font.Size   := 0;
  end;
end;

procedure TTimeTableForm.DrawImg(ACanvas: TCanvas; ARect: TRect;
                                 ACountItems: integer; ANum: integer);
var
  i: integer;
begin
  with ACanvas do
    for i := 1 to 2 do
    begin
      Draw(DefWidthCol + aRect.Left - ImgArray[i].Width - Margin,
           aRect.Top + (i) * ImgArray[i].Height + 2 * Margin +
           ACountItems * DefHeightFont * ANum, ImgArray[i].Graphic);
    end;
end;

procedure TTimeTableForm.StringGridMouseDown(Sender : TObject;
  Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  StringGrid.MouseToCell(X, Y, Col, Row);
  kX := x; kY := y;
  MouseDownFlag:= true;
end;

procedure TTimeTableForm.StringGridMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  cX := x; cY := y;
  if MouseDownFlag then DragDropFlag := true;
end;


procedure TTimeTableForm.DragDropRecord(EndX, EndY: integer);
var
  i            : integer;
  tX, tY       : integer;
  NumRec       : integer;
  ParamNum     :integer;
  DataCell, SL : TStringList;
  s: string;

  function ParsingDataCell(aRow, aCol, ANum: integer): TStringList;
  var
    i           : integer;
    SL, curr    : TStringList;
    s           : string;
  begin
    SL   := TStringList.Create;
    curr := DataArray[aRow - 1][aCol - 1];

    for i := DefCountStr * ANum to DefCountStr * (ANum + 1) - 2 do
    begin
      s := curr[i];
      delete(s, 1, pos(':', s) + 1);
      SL.Append(s);
    end;

    result:= SL;
  end;

begin
  tY     := kY - StringGrid.CellRect(Col, Row).Top;
  tX     := kX - StringGrid.CellRect(Col, Row).Left;
  NumRec := tY div DefRowHeight;

  DataCell := ParsingDataCell(Row, Col, NumRec);
  StringGrid.MouseToCell(EndX, EndY, Col, Row);

  DBDataModule.SQLQuery.Close;
  DBDataModule.SQLQuery.SQL.Text := 'DELETE FROM ' + MetaData.Tables[Tag].Name +
                                    ' WHERE ID = ' + DataCell[0];
  DBDataModule.SQLQuery.ExecSQL;

  DataCell[RowComboBox.ItemIndex + 1] := StringGrid.Cells[0, Row];
  DataCell[ColComboBox.ItemIndex + 1] := StringGrid.Cells[Col, 0];

  DBDataModule.SQLQuery.SQL.Text := SQLGenerator.GenInsertQuery(Tag).Text;
  DBDataModule.SQLQuery.ParamByName('p0').AsInteger := StrToInt(DataCell[0]);

  for i := 1 to high(MetaData.Tables[Tag].Fields) do
  begin
    s := 'p' + IntToStr(i);
    if MetaData.Tables[Tag].Fields[i].Reference <> nil then
    begin
      SL := MetaData.Tables[Tag].GetDataFieldOfIndex(i);
      ParamNum := SQLGenerator.GetId(Tag, i - 1, SL.IndexOf(DataCell[i]));
      DBDataModule.SQLQuery.ParamByName(s).AsInteger := ParamNum;
    end
    else
      DBDataModule.SQLQuery.ParamByName(s).AsString := DataCell[i];
  end;

  DBDataModule.SQLQuery.ExecSQL;
  DBDataModule.SQLTransaction.Commit;
  FillGridData();
end;

procedure TTimeTableForm.StringGridMouseUp(Sender: TObject; Button: TMouseButton;
                                           Shift: TShiftState; X, Y: Integer);
var
  i, count: integer;
  tCol, tRow: integer;
  Fy, Fx, NumCol, r: integer;
begin
  StringGrid.MouseToCell(x, y, tCol, tRow);

  if (tCol = 0) or (tRow = 0) then exit;

  if ((Row <> tRow) or (Col <> tCol)) and (DataArray[Row - 1][Col - 1] <> nil) then
  begin
    DragDropRecord(x, y);
    exit;
  end;

  CurrRowHeight := StringGrid.RowHeights[Row];

  StringGrid.MouseToCell(x, y, Col, Row);

  count := GetCountCheckedItems + 1;
  Fy    := y - StringGrid.CellRect(Col, Row).Top;
  Fx    := x - StringGrid.CellRect(Col, Row).Left;

  if (Fx < DefWidthCol - Margin) and (Fx > DefWidthCol - DefWidthImg - Margin) then
  begin
    if (Fy < DefWidthImg + Margin) and (Fy > Margin) then
      InsertClick(Fx, Fy)
    else begin
      NumCol := Fy div (count*DefHeightFont);
      r      := 0;

      for i := 1 to 2 do
        if (Fy > (Margin + DefWidthImg) * i + DefRowHeight * NumCol) and
           (Fy < (Margin + DefWidthImg) * (i + 1) + DefRowHeight * NumCol) then
        begin
          r := i;
          break;
        end;
       case r of
         1 : EditClick(Fx, Fy);
         2 : DeleteClick(Fx, Fy);
       end;
    end;
    FillGridData();
  end;
end;

function TTimeTableForm.GetCountCheckedItems() : integer;
var
  i: integer;
begin
  Result := 0;
  for i  := 0 to DataListBox.Count - 1 do
    if DataListBox.Checked[i] then inc(Result);
end;

procedure TTimeTableForm.ApplyButtonClick(Sender : TObject);
var
  RowSL, ColSL : TstringList;
begin
  RowSL   := MetaData.Tables[Tag].GetDataFieldOfIndex(RowComboBox.ItemIndex + 1);
  ColSL   := MetaData.Tables[Tag].GetDataFieldOfIndex(ColComboBox.ItemIndex + 1);

  FillListBox(ColSL, RowSL);
  FillGridData();
  UpdateHeaderVisible();
end;

procedure TTimeTableForm.DataListBoxItemClick(Sender: TObject; Index: integer);
begin
  if GetCountCheckedItems < 3 then
    DataListBox.Checked[Index] := true
  else
  begin
    DefRowHeight := (GetCountCheckedItems + 1) * DefHeightFont;
    UpdateRowsHeight(Index);
    StringGrid.Invalidate;
  end;
end;

procedure TTimeTableForm.UpdateRowsHeight(Index: integer);
var
  k, c: integer;
begin
  with (StringGrid) do
  begin
    for k := 1 to RowCount - 1 do
      if DefRowHeight >= RowHeights[k] then
        RowHeights[k] := DefRowHeight
      else
      begin
        if DataListBox.Checked[Index] then
          c := round(RowHeights[k] / (GetCountCheckedItems * DefHeightFont))
        else
          c := round(RowHeights[k] / ((GetCountCheckedItems + 2) * DefHeightFont));
        RowHeights[k] := c * DefRowHeight;
      end;
  end;
end;

procedure TTimeTableForm.FilterButtonClick(Sender: TObject);
var
  MaxNum: integer;
begin
  MaxNum := 16;
  if (FilterNum < MaxNum) then
  begin
    AddNewFilter();
  end;
end;

procedure TTimeTableForm.AddNewFilter();
begin
  SetLength(DirectoryFilter, FilterNum + 1);
  DirectoryFilter[FilterNum] := TDirectoryFilter.Create;
  DirectoryFilter[FilterNum].FormTag := Tag;
  DirectoryFilter[FilterNum].CreateFilter(FilterPanel, FilterNum, GenDATA(Tag));
  inc(FilterNum);
end;

procedure TTimeTableForm.UpdateHeaderVisible();
var
  i : integer;
begin
  with (StringGrid) do
  begin
    for i := 1 to RowCount - 1 do
      if not RowListBox.Checked[i - 1] then
        RowHeights[i] := 0
      else if StringGrid.RowHeights[i] = 0 then
        RowHeights[i] := DefRowHeight;

    for i := 1 to ColCount - 1 do
      if not ColListBox.Checked[i - 1] then
        ColWidths[i] := 0
      else
        ColWidths[i] := DefWidthCol;
  end;
end;

procedure TTimetableForm.ChangeCaptionColumn(AColList, ARowList : TStringList);
var
  i : integer;
begin
  for i := 1 to AColList.Count do
  begin
    StringGrid.ColWidths[i]  := DefWidthCol;
    StringGrid.Cells[i, 0]   := AColList[i - 1];
  end;

  for i := 1 to ARowList.Count do
  begin
    StringGrid.RowHeights[i] := DefRowHeight;
    StringGrid.Cells[0, i]   := ARowList[i - 1];
  end;

  with MetaData.Tables[Tag].Fields[RowComboBox.ItemIndex + 1] do
    StringGrid.ColWidths[0]  := Width;
end;
procedure TTimeTableForm.FillListBox(AColList, ARowList : TStringList);

  procedure FillBox (AListBox : TCheckListBox; AList : TStringList);
  var
    i     : integer;
    max   : integer = 0;
  begin
    AListBox.Clear;

    for i :=0 to AList.Count - 1 do
    begin
      AListBox.Items.Add(AList[i]);
      if (length(AList[i]) > max) then max := length(AList[i]);
    end;

    AListBox.CheckAll(cbChecked);
    if (max > 25) then max := 25;
  end;

begin
  FillBox(ColListBox, AColList);
  FillBox(RowListBox, ARowList);
end;

procedure TTimetableForm.FillGridData();
var
  ColSL, RowSL : TStringList;
  i, c, r      : integer;

  procedure FillCell(ARow, ACol : integer);
  var
    k  : integer;
    s  : string;
  begin
    if DataArray[aRow][aCol] = nil then
      DataArray[aRow][aCol] := TStringList.Create;

    for k :=0 to FSQLQuery.FieldCount - 1 do
    begin
      s := MetaData.Tables[Tag].Fields[k].Caption;
      DataArray[aRow][aCol].Append(s + ': ' + FSQLQuery.Fields[k].AsString);
    end;

    DataArray[aRow][aCol].Append('');
  end;

begin
  FSQLQuery.Close;
  SQLGenerator.GenFilters(Tag, DirectoryFilter, FSQLQuery);
  FSQLQuery.Open;

  RowSL := MetaData.Tables[Tag].GetDataFieldOfIndex(RowComboBox.ItemIndex + 1);
  ColSL := MetaData.Tables[Tag].GetDataFieldOfIndex(ColComboBox.ItemIndex + 1);

  StringGrid.ColCount := ColSL.Count + 1;
  StringGrid.RowCount := RowSL.Count + 1;

  SetLength(DataArray, 0);
  SetLength(DataArray, RowSL.Count);

  for i := 0 to RowSL.Count - 1 do
    SetLength(DataArray[i], ColSL.Count);

  RowSL.IndexOf(FSQLQuery.Fields[RowComboBox.ItemIndex + 1].AsString);

  while not FSQLQuery.EOF do
  begin

    r   := RowSL.IndexOf(FSQLQuery.Fields[RowComboBox.ItemIndex + 1].AsString);
    c   := ColSL.IndexOf(FSQLQuery.Fields[ColComboBox.ItemIndex + 1].AsString);

    FillCell(r, c);
    FSQLQuery.Next;
  end;

  ChangeCaptionColumn(ColSL, RowSL);
  UpdateHeaderVisible();
end;

procedure TTImeTableForm.SetParam(Sender : TObject);
var
  SL   : TStringList;
  i    : integer;
begin
  Tag      := 8;
  SL       := GenDATA(Tag);
  Caption  := MetaData.Tables[Tag].Caption;

  FillComboBox(SL);

  for i := 0 to high(MetaData.Tables[Tag].Fields) do
    DataListBox.Items.Add(MetaData.Tables[Tag].Fields[i].Caption);

  DataListBox.CheckAll(cbChecked);
  DataListBox.Checked[0] := false;

  DefRowHeight := (GetCountCheckedItems + 1) * DefHeightFont;
  ApplyButtonClick(Self);
end;

procedure TTimeTableForm.FillComboBox(AList: TStringList);
var
  i : integer;
begin
  for i :=0 to AList.Count - 1 do
  begin
    ColComboBox.Items.Add(AList.ValueFromIndex[i]);
    RowComboBox.Items.Add(AList.ValueFromIndex[i]);
  end;
  with (ColComboBox) do
  begin
    ItemIndex  := 2;
    ReadOnly   := true;
  end;

  with (RowComboBox) do
  begin
    ItemIndex  := 1;
    ReadOnly   := true;
  end;
end;

{ /Editing }

procedure TTimeTableForm.InsertClick(Ax, Ay: integer);
begin
  EditingManager.OpenFormEditingTable(ctInsert, Tag, GetListDataCell(Ax, Ay));
  EditingManager.SetItemIndex(RowComboBox.ItemIndex, Row - 1, ColComboBox.ItemIndex, Col - 1);

  if (Col > 0) and (Row > 0) then
    StringGrid.Selection := StringGrid.CellRect(Col, Row);

  StringGrid.RowHeights[Row] := CurrRowHeight;
end;

procedure TTimeTableForm.EditClick(Ax, Ay: integer);
begin
  EditingManager.OpenFormEditingTable (ctEdit, Tag, GetListDataCell(Ax, Ay));

  if (Col > 0) and (Row > 0) then
    StringGrid.Selection := StringGrid.CellRect(Col, Row);

  StringGrid.RowHeights[Row] := CurrRowHeight;
end;

procedure TTimeTableForm.DeleteClick(Ax, Ay: integer);
begin
  EditingManager.DeleteRecord(StrToInt(GetListDataCell(Ax, Ay)[0]), Tag);
  FillGridData();

  if (Col > 0) and (Row > 0) then
    StringGrid.Selection := StringGrid.CellRect(Col, Row);

  StringGrid.RowHeights[Row] := CurrRowHeight;
end;

function TTimeTableForm.GetListDataCell(Ax, Ay: integer): TStringList;
var
  i, k, RecordNum : integer;
  s               : string;
  SL              : TStringList;
begin
  SL        := TStringList.Create;
  RecordNum := AY div DefRowHeight;

  if DataArray[Row - 1][Col - 1] <> nil then
    for i := RecordNum * DefCountStr to RecordNum * DefCountStr + 6 do
    begin
      s := DataArray[Row - 1][Col - 1][i];
      k := pos(':', s);
      delete(s, 1, k + 1);
      SL.Append(s);
    end;
  result := SL;
end;

end.

