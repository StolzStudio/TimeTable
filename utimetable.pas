unit Utimetable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  Grids, ExtCtrls, StdCtrls, CheckLst, Filters, DirectoryForms, Meta, SQLGen;

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
    StringGrid        : TStringGrid;

    procedure ApplyButtonClick(Sender : TObject);
    procedure FilterPanelClick(Sender: TObject);
    procedure FormCreate(Sender : TObject);
    procedure StringGridDblClick(Sender : TObject);
    procedure StringGridDrawCell(Sender : TObject; aCol, aRow : Integer;
                                 aRect : TRect; aState : TGridDrawState);
    procedure StringGridMouseDown(Sender : TObject; Button : TMouseButton;
                                  Shift : TShiftState; X, Y : Integer);
    { /end }
  private
    DirectoryFilter   : array of TDirectoryFilter;
    DataArray         : array of array of TStringList;
    CurrentRowHeight  : integer;
    Row               : integer;
    Col               : integer;

    procedure FillComboBox(AList : TStringList);
    procedure FillListBox(AColList, ARowList : TStringList);
    procedure FillGridData();
    procedure UpdateHeaderVisible();
    procedure ChangeCaptionColumn(AColList, ARowList : TStringList);

    function GetCountCheckedItems() : integer;
  public
    procedure SetParam(Sender : TObject);
  end;

var
  TimeTableForm : TTimeTableForm;

implementation

  const Margin          = 2;
  const DefHeightFont   = 17;
  const DefCountStr     = 8;
  const DefWidthCol     = 350;
{$R *.lfm}

{ TTimeTableForm }

procedure TTimeTableForm.FormCreate(Sender : TObject);
begin
  SetLength(DirectoryFilter, 0);
end;

procedure TTimeTableForm.StringGridDblClick(Sender : TObject);
var
  count : integer;
begin
  if (Row = 0) or (Col = 0) then exit;

  with (StringGrid) do
  begin
    if RowHeights[Row] > CurrentRowHeight then
    begin
      if DataArray[Row - 1][Col - 1] = nil then
      begin
        RowHeights[Row] := CurrentRowHeight;
        exit;
      end;
      count:= round(DataArray[Row - 1][Col - 1].Count/DefCountStr);
      if RowHeights[Row] < CurrentRowHeight*count then
        RowHeights[Row] := CurrentRowHeight*count
      else
        RowHeights[Row] := CurrentRowHeight
    end
    else
    if (DataArray[Row - 1][Col - 1] <> nil) then
      RowHeights[Row] := CurrentRowHeight *
                         round(DataArray[Row - 1][Col - 1].Count / DefCountStr);
  end;
end;

procedure TTimeTableForm.StringGridDrawCell(Sender : TObject;
                                            aCol, aRow : Integer; aRect : TRect;
                                            aState : TGridDrawState);
var
  i, c, j, count : integer;
begin
  if (length(DataArray) <> 0) and (aRow <> 0) and (aCol <> 0) then
    with StringGrid.Canvas do
    begin
      count := GetCountCheckedItems + 1;

      if (DataArray[aRow - 1][aCol - 1] <> nil) and
         (DataArray[aRow - 1][aCol - 1].Count <> 0) then
      begin
        c  := DataArray[aRow - 1][aCol - 1].Count div 8;
        j  := -1;

        for i := 0 to DataArray[aRow - 1][aCol - 1].Count - 1 do
          if DataArray[aRow - 1][aCol - 1][i] <> '' then
          begin
            if (DataListBox.Checked[i - (i div DefCountStr) * DefCountStr]) then
            begin
              inc(j);
              TextOut(aRect.Left + Margin, aRect.Top + j * DefHeightFont,
                      DataArray[aRow - 1][aCol - 1][i]);
            end;
          end
          else
          if (i <> DataArray[aRow - 1][aCol - 1].Count - 1) then
          begin
            inc(j);
            TextOut(aRect.Left + Margin, aRect.Top + j * DefHeightFont,
                    DataArray[aRow - 1][aCol - 1][i]);
          end;

        if StringGrid.RowHeights[aRow] < c*CurrentRowHeight then
        begin
          Font.Bold;
          Font.Color  := clGreen;
          Font.Size   := 10;

          TextOut(DefWidthCol + aRect.Left - 27 - Margin,
                  aRect.Top + StringGrid.RowHeights[aRow] - 20, ' ↓ ' +
                  IntToStr(c - StringGrid.RowHeights[aRow] div CurrentRowHeight));

          Font.Color  := clBlack;
          Font.Size   := 0;
        end;
      end;
    end;
end;

procedure TTimeTableForm.StringGridMouseDown(Sender : TObject;
  Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  StringGrid.MouseToCell(X, Y, Col, Row);
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

procedure TTimeTableForm.FilterPanelClick(Sender: TObject);
begin

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
        RowHeights[i] := CurrentRowHeight;

    for i:= 1 to ColCount - 1 do
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
    StringGrid.RowHeights[i] := CurrentRowHeight;
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
      if length(AList[i]) > max then max := length(AList[i]);
    end;

    AListBox.CheckAll(cbChecked);
    if max > 25 then max := 25;
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

  with (SQLGenerator) do
    FSQLQuery.SQL.Append('ORDER BY ' + GetNameField(Tag, ColComboBox.ItemIndex) +
                         ', ' + GetNameField(Tag, RowComboBox.ItemIndex));

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

  StringGrid.Invalidate;
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

  CurrentRowHeight := (GetCountCheckedItems + 1) * DefHeightFont;
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

end.

