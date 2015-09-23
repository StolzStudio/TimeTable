unit ChangeFormData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type

  TChangeType = (ctEdit, ctInsert, ctDelete);

  { TFormData }

  TFormData = class(TForm)
  private
    FAction : TChangeType;
  public
    DataControl: array of TControl;
    procedure FormCreate(Sender: TObject);
    procedure CreateComboBox();
    procedure CreateEdit();
    procedure CreateApplyBtn();
  published
    property Action: TChangeType read FAction write FAction;
  end;

var
  FormData: TFormData;

implementation

{$R *.lfm}

{ TFormData }

procedure TFormData.FormCreate(Sender: TObject);
begin

end;

procedure TFormData.CreateComboBox();
begin

end;

procedure TFormData.CreateEdit();
begin

end;

procedure TFormData.CreateApplyBtn();
begin

end;

end.

