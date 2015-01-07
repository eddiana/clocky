unit FormClocky;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, ClassClockyWidget
  {$ifdef WINDOWS}
  ,Windows
  {$endif}
  {$ifdef LINUX}
  ,Gtk2, gdk2
  {$endif}
  ;

type

  { TfrmClockyMain }

  TfrmClockyMain = class(TForm)
	 Image1: TImage;
	 labClose: TLabel;
	 MenuItem10: TMenuItem;
	 MenuItem9: TMenuItem;
	 mProfile: TMenuItem;
	 popOptions: TPopupMenu;
	 Timer1: TTimer;
	 procedure FormActivate(Sender: TObject);
	 procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
	 procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
					 Shift: TShiftState; X, Y: Integer);
	 procedure FormMouseEnter(Sender: TObject);
	 procedure FormMouseLeave(Sender: TObject);
	 procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
	 procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
					 Shift: TShiftState; X, Y: Integer);
	 procedure FormPaint(Sender: TObject);
	 procedure FormShow(Sender: TObject);
	 procedure Image1Click(Sender: TObject);
	 procedure labCloseClick(Sender: TObject);
	 procedure labCloseMouseDown(Sender: TObject; Button: TMouseButton;
					 Shift: TShiftState; X, Y: Integer);
	 procedure labCloseMouseEnter(Sender: TObject);
	 procedure labCloseMouseLeave(Sender: TObject);
	 procedure MenuItem10Click(Sender: TObject);
	 procedure mProfile1Click(Sender: TObject);
	 procedure MenuItem7Click(Sender: TObject);
	 procedure mProfileClick(Sender: TObject);
	 procedure popOptionsPopup(Sender: TObject);
	 procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    OrigParentHWND: integer;

    isStarting: boolean;
    isDragging: boolean;
    DragStart: TPoint;
    DragOffset: TPoint;
    DragScreenStart: TPoint;
    OrigPlace: TPoint;

    clocky: TClockyWidget;

    procedure ProfileMenuClicked(Sender: TObject);

  end;

var
  frmClockyMain: TfrmClockyMain;

implementation

{$R *.lfm}

function pt( x: integer; y: integer): TPoint;
begin
   result.x := x;
   result.y := y;
end;

{ TfrmClockyMain }

procedure TfrmClockyMain.FormCreate(Sender: TObject);
var
  i, h: integer;
  nProfile: integer;
  mi: TMenuItem;
begin
   isStarting := true;
   width := 225;
   height := 190;

   isDragging := false;


   {$ifdef Windows}
   //hides the taskbar button
   h := GetParent( Self.Handle);
   ShowWindow(h, SW_HIDE);
   SetWindowLong(h, GWL_EXSTYLE, getWindowLong(h, GWL_EXSTYLE) or WS_EX_TOOLWINDOW);
   ShowWindow(h, SW_SHOW) ;
   //

   //set the parent to the desktop to pin the app
   OrigParentHWND := GetParent(Self.Handle);
   h := FindWindow('Progman', nil);
   Windows.SetParent(Self.Handle, h);
   {$endif}

   {$ifdef Linux}
   gtk_window_stick( PGtkWindow( self.Handle));
   //gtk_window_set_opacity( PGtkWindow( self.Handle), 0.2);
   gtk_window_set_type_hint( PGtkWindow( self.Handle), GDK_WINDOW_TYPE_HINT_DOCK);
   gtk_window_set_keep_below( PGtkWindow( self.Handle), true);
   {$endif}

   //menus
   for i := 1 to CLOCKY_PROFILE_COUNT do
   begin
      mi := TMenuItem.Create( mProfile);
      mi.Tag := i;
      mi.Caption := '(Unassigned)';
      mi.OnClick := @ProfileMenuClicked;
      mProfile.Add(mi);
	end;

   nProfile := 1;
   if ParamCount > 0 then
   begin
      TryStrToInt( ParamStr(1), nProfile);
	end;

   clocky := TClockyWidget.Create;
   clocky.ProfileID := nProfile;
   clocky.ExePath := ExtractFilePath( Application.ExeName);
   clocky.Left := Left;
   clocky.Top := Top;
   Clocky.Load;
   Left := Clocky.Left;
   Top := Clocky.Top;


end;

procedure TfrmClockyMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
   CanClose := true;

   {$ifdef Windows}
   Windows.SetParent(Self.Handle, OrigParentHWND);
   {$endif}


end;

procedure TfrmClockyMain.FormActivate(Sender: TObject);
begin

end;

procedure TfrmClockyMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
   Clocky.Left := Left;
   Clocky.Top := Top;
   Clocky.Save;
end;

procedure TfrmClockyMain.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   isDragging := true;
   OrigPlace := pt( Left, Top);
   DragStart := pt( x, y);
   DragScreenStart := Self.ClientToScreen( DragStart);


end;

procedure TfrmClockyMain.FormMouseEnter(Sender: TObject);
begin
end;

procedure TfrmClockyMain.FormMouseLeave(Sender: TObject);
begin
end;

procedure TfrmClockyMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
   p: TPoint;
   dx, dy: integer;
begin

   if isDragging then
   begin
      p := Self.ClientToScreen( pt( x, y));

      dx := p.x - DragScreenStart.x;
      dy := p.y - DragScreenStart.y;

      Left := OrigPlace.x + dx;
      Top := OrigPlace.y + dy;
	end;






   Clocky.LastMove := Clocky.TickCount;
   if not labClose.Visible then
      labClose.Visible := true;

end;

procedure TfrmClockyMain.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   isDragging := false;
end;

procedure TfrmClockyMain.FormPaint(Sender: TObject);
begin
   Clocky.Render( Canvas, Width, Height);

end;

procedure TfrmClockyMain.FormShow(Sender: TObject);
begin

end;

procedure TfrmClockyMain.Image1Click(Sender: TObject);
begin

end;

procedure TfrmClockyMain.labCloseClick(Sender: TObject);
begin
   popOptions.PopUp;
end;

procedure TfrmClockyMain.labCloseMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   labClose.Font.Color := clWhite;
end;

procedure TfrmClockyMain.labCloseMouseEnter(Sender: TObject);
begin
   labClose.Font.Color := clRed;
end;

procedure TfrmClockyMain.labCloseMouseLeave(Sender: TObject);
begin
   labClose.Font.Color := clMaroon;
end;

procedure TfrmClockyMain.MenuItem10Click(Sender: TObject);
begin
   close;
end;

procedure TfrmClockyMain.mProfile1Click(Sender: TObject);
var
   mi: TMenuItem;
begin
   mi := Sender as TMenuItem;

   Clocky.ProfileID := StrToInt( mi.Caption);
   Clocky.Load;
   Clocky.UpdateWeatherConditions;
   Invalidate;

end;

procedure TfrmClockyMain.ProfileMenuClicked(Sender: TObject);
var
   mi: TMenuItem;
begin
   mi := Sender as TMenuItem;

   Clocky.ProfileID := mi.Tag;
   Clocky.Load;
   Clocky.UpdateWeatherConditions;
   Invalidate;

end;



procedure TfrmClockyMain.MenuItem7Click(Sender: TObject);
begin

end;

procedure TfrmClockyMain.mProfileClick(Sender: TObject);
begin

end;

procedure TfrmClockyMain.popOptionsPopup(Sender: TObject);
var
   i: integer;
   mi: TMenuItem;
begin
   for i := 1 to mProfile.Count do
   begin
      mi := mProfile[i-1];
      mi.Caption := Clocky.GetProfileName( mi.Tag);
	end;

end;

procedure TfrmClockyMain.Timer1Timer(Sender: TObject);
begin
   if isStarting then
   begin
      isStarting := false;
	end;

   inc( Clocky.TickCount);
   if Clocky.TickCount > (Clocky.LastMove + 1) then
   begin
      labClose.Visible := false;
	end;

   if (Clocky.TickCount mod CLOCKY_CHECK_INTERVAL) = 0 then
   begin
      Clocky.UpdateWeatherConditions;
	end;
   if Clocky.TickCount = 5 then
   begin
      Clocky.UpdateWeatherConditions;
	end;


   invalidate;
end;


end.
