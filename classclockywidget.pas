unit ClassClockyWidget;

{$mode objfpc}{$H+}

interface

uses Classes, Graphics, SysUtils, httpsend, superobject, inifiles, clockyutils;

const CLOCKY_CHECK_INTERVAL = 15 * 60;
const CLOCKY_PROFILE_COUNT = 8;

type


 TClockyWeatherConditions = record
   CurrentTemp: integer;
   Description: string;
   Icon: string;

 end;


{ TClockyWidget }

 TClockyWidget = class( TObject)
   ProfileID: integer;
   ExePath: string;

   TickCount: int64;
   LastMove: int64;
   LastConditionsCheck: int64;

   LocalTimeZone: double; //ie, time zone where computer is
   LocationTimeZone: double; //location of where we're checking weather


   LocationTitle: string;
   Location: string; //whats passed to the api
   TimeFormat: string;
   DateFormat: string;
   BackgroundColor: integer;

   //
   Top: integer;
   Left: integer;


   //
   Conditions: TClockyWeatherConditions;

   //resources
   Shiny: TPortableNetworkGraphic;
   Icons: array[0..100] of TPortableNetworkGraphic;

   //API Keys
   OpenWeatherMapAPIKey: string;

   constructor create;

   function GetProfileName(n: integer): string;

   function GetTimeStr(): string;
   function GetDateStr(): string;

   procedure Render( c: TCanvas; width: integer; height: integer);

   procedure UpdateWeatherConditions;

   function iconOpenWeatherToVCloud( sOW: string): integer;

   procedure Load;
   procedure Save;

end;


implementation

{ TClockyWidget }

constructor TClockyWidget.create;
var
   i: integer;
begin
   ProfileID := 1;
   LocationTitle := 'State College';
   Location := 'State College,PA';
   TimeFormat := 'hh:nnam/pm';
   DateFormat := 'ddd, mmm dd, yyyy';
   BackgroundColor := $600000;

   Shiny := TPortableNetworkGraphic.Create;
   //Shiny.LoadFromFile( 'graphics/drawButton-clear.png');

   for i := 0 to 100 do
   begin
      icons[i] := nil;
	end;

end;

function TClockyWidget.GetProfileName(n: integer): string;
var
   ini: TIniFile;
   sFile: string;
begin

   sFile := GetAppConfigFile( false, true);
   ini := TIniFile.Create( sFile);
   result := ini.ReadString( 'Prof_' + IntToStr( n), 'LocationTitle', '(Unassigned)');
   ini.Free;

end;

function TClockyWidget.GetTimeStr: string;
begin
   result := FormatDateTime( TimeFormat, clockyTimeAtZone( LocalTimeZone, LocationTimeZone));
end;

function TClockyWidget.GetDateStr: string;
begin
   result := FormatDateTime( DateFormat, clockyTimeAtZone( LocalTimeZone, LocationTimeZone));
end;

procedure TClockyWidget.Render(c: TCanvas; width: integer; height: integer);
var
   tw: integer;
   x: integer;
   y: integer;
   s: string;
   png: TPortableNetworkGraphic;
   nIcon: integer;
   sIcon: string;
begin

   //background
   c.Brush.Style := bsSolid;
   c.Brush.Color := BackgroundColor; //RGBToColor( 0, 0, 24);
   c.FillRect( 0, 0, width, height);
   {$ifdef Windows}
   //doesnt seem to work on Linux
   c.Draw(0, 0, Shiny);
   {$endif}

   //Location Header
   c.Font.Name := 'Utah';
   c.Font.Color := clWhite;
   c.Font.Height := 20;
   c.Brush.Style := bsClear;
   tw := c.GetTextWidth( LocationTitle);
   x := round( (width/2) - (tw/2));
   c.TextOut( x, 10, LocationTitle);

   //Time
   c.Font.Height := 40;
   s := GetTimeStr;
   tw := c.GetTextWidth( s);
   x := round( (width/2) - (tw/2));
   c.TextOut( x, 30, s);

   //date
   c.Font.Height := 20;
   s := GetDateStr;
   tw := c.GetTextWidth( s);
   x := round( (width/2) - (tw/2));
   c.TextOut( x, 70, s);

   //temp
   c.Font.Height := 40;
   s := IntToStr( Conditions.CurrentTemp) + '°';
   tw := c.GetTextWidth( s);
   //x := round( (width/4) - (tw/2) + (width/2));
   x := round((width /2) + 5);
   c.TextOut( x, 110, s);

   //icon
   nIcon := iconOpenWeatherToVCloud( Conditions.Icon);
   if Icons[nIcon] = nil then
   begin
      Icons[nIcon] := TPortableNetworkGraphic.Create;
      Icons[nIcon].LoadFromFile( ExePath + 'icons/VClouds/' +IntToStr( nIcon) + '.png');
	end;

   if Icons[nIcon] <> nil then
   begin
      //c.Draw( 0, 100, Icons[nIcon]);
      //x := round( (width / 4) - 30);
      x :=  round((width / 2) - 82 - 5);
      c.StretchDraw( rect( x, 100, x+82, 160), Icons[nIcon]);
	end;

   //conditions
   c.Font.Height := 20;
   s := Conditions.Description;
   tw := c.GetTextWidth( s);
   x := round( (width/2) - (tw/2));
   c.TextOut( x, 160, s);


end;

procedure TClockyWidget.UpdateWeatherConditions;
var
   sURL: string;
   sj: string;
   rs: TStringList;
   j: ISuperObject;
   sa: TSuperArray;
begin

   sURL := 'http://api.openweathermap.org/data/2.5/weather?q=' + Location + '&units=imperial&APPID=' + OpenWeatherMapAPIKey;

   rs := TStringList.Create;
   try
      if HTTPGetText( sURL, rs) then
      begin
         sj := rs.Text;

         j := so( sj);


         Conditions.CurrentTemp:= j['main'].i['temp'];

         sa := j.A['weather'];

         if sa.Length > 0 then
         begin
            Conditions.Description:= sa[0].s['main'];
            Conditions.Icon:=  sa[0].s['icon'];
			end;

	   end;
	finally
     rs.Free;
	end;





end;

function TClockyWidget.iconOpenWeatherToVCloud(sOW: string): integer;
begin
   if sOW = '01d' then
      result := 32
   else if sOW = '01n' then
      result := 31
   else if sOW = '02d' then
      result := 30
   else if sOW = '02n' then
      result := 33
   else if sOW = '03d' then
      result := 28
   else if sOW = '03n' then
      result := 27
   else if sOW = '04d' then
      result := 26
   else if sOW = '04n' then
      result := 26
   else if sOW = '09d' then
      result := 9
   else if sOW = '09n' then
      result := 9
   else if sOW = '10d' then
      result := 0
   else if sOW = '10n' then
      result := 0
   else if sOW = '11d' then
      result := 4
   else if sOW = '11n' then
      result := 4
   else if sOW = '13d' then
      result := 14
   else if sOW = '13n' then
      result := 14
   else if sOW = '50d' then
      result := 20
   else if sOW = '50n' then
      result := 20
   else
     result := 100;





end;

procedure TClockyWidget.Load;
var
   sPath, sFile, sSect, sColor: string;
   ini: TIniFile;
begin

   sFile := GetAppConfigFile( false, true);
   sPath := ExtractFilePath( sFile);
   ForceDirectories( sPath);

   ini := TIniFile.Create( sFile);

   LocalTimeZone := ini.ReadFloat( 'clocky', 'LocalTimeZone', 0);
   OpenWeatherMapAPIKey := ini.ReadString( 'clocky', 'OpenWeatherMapAPIKey', '');

   sSect := 'Prof_' + IntToStr( ProfileID);

   LocationTitle := ini.ReadString( sSect, 'LocationTitle', 'State College');
   Location := ini.ReadString( sSect, 'Location', 'State College, PA');
   LocationTimeZone := ini.ReadFloat( sSect, 'LocationTimeZone', LocalTimeZone);

   Left := ini.ReadInteger( sSect, 'Left', 200);
   Top := ini.ReadInteger( sSect, 'Top', 200);

   sColor := ini.ReadString( sSect, 'BackgroundColor', '$00000000');
   try
      BackgroundColor := StringToColor( sColor);
   except
	end;

   ini.Free;



end;

procedure TClockyWidget.Save;
var
   sPath, sFile, sSect, sColor: string;
   ini: TIniFile;
begin


   sFile := GetAppConfigFile( false, true);
   sPath := ExtractFilePath( sFile);
   ForceDirectories( sPath);

   ini := TIniFile.Create( sFile);

   sSect := 'Prof_' + IntToStr( ProfileID);

   ini.WriteString( sSect, 'LocationTitle', LocationTitle);
   ini.WriteString( sSect, 'Location', Location);

   ini.WriteInteger( sSect, 'Left', Left);
   ini.WriteInteger( sSect, 'Top', Top);

   sColor := ColorToString( BackgroundColor);
   ini.WriteString( sSect, 'BackgroundColor', sColor);



   ini.Free;

end;

end.
