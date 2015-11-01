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

    procedure UpdateRowsHeight(Index : integer);
  private
    DirectoryFilter   : array of TDirectoryFilter;
    DataArray         : array of array of TStringList;
    ImgArray          : array [0..2] of TPicture;
    FilterNum         : integer;
    DefRowHeight  : integer;
    Row               : integer;
    Col               : integer;

    procedure FillComboBox(AList : TStringList);
    procedure FillListBox(AColList, ARowList : TStringList);
    procedure FillGridData();
    procedure UpdateHeaderVisible();
    procedure ChangeCaptionColumn(AColList, ARowList : TStringList);
    procedure DrawImg(ACanvas : TCanvas; ARect : TRect;
                      ACountItems : integer; ANum : integer);
    procedure AddNewFilter();
    procedure DrawArrow(ACanvas : TCanvas; ARect : TRect;
                        aRow : Integer; AText : String);
    procedure DrawText(ACanvas : TCanvas; ARect : TRect;
                       ASL : TStringList; Acnt : integer);

    function GetCountCheckedItems() : integer;
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
{$R *.lfm}

{ TTimeTableForm }

procedure TTimeTableForm.FormCreate(Sender : TObject);
var
  i : integer;
begin
  SetLength(DirectoryFilter, 0);
  InvalidateEvent := @FormPaint;
  FilterNum := 1;

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
        DrawText(Canvas, aRect, Sl, cnt);

        if (StringGrid.RowHeights[aRow] < c * DefRowHeight) then
          DrawArrow(StringGrid.Canvas, aRect, aRow, ' ↓ ' +
                    IntToStr(c - RowHeights[aRow] div DefRowHeight));
      end;
  end;
end;

procedure TTimeTableForm.DrawText(ACanvas : TCanvas; ARect : TRect;
                                  ASL : TStringList; Acnt : integer);
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
    else begin
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
    for i:= 1 to 2 do
    begin
      Draw(DefWidthCol + aRect.Left - ImgArray[i].Width - Margin,
        aRect.Top + (i)*ImgArray[i].Height + 2*Margin +
        ACountItems*DefHeightFont*ANum, ImgArray[i].Graphic);
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

end.

