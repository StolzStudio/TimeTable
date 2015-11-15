unit Conflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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

implementation


constructor TConflict.Create(ARecordID : integer; AConflictType : TConflictClass);
begin
  RecordID     := ARecordID;
  ConflictType := AconflictType;
end;

end.

