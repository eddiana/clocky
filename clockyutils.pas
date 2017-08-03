unit clockyutils;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

function clockyTimeAtZone( LocalTimeZone: double; LocationTimeZone: double): TDateTime;
function clockyFtoC(d: double): integer;

implementation

function clockyTimeAtZone( LocalTimeZone: double; LocationTimeZone: double): TDateTime;
begin
  result := now() + ((LocationTimeZone - LocalTimeZone)/24);
end;

function clockyFtoC(d: double): integer;
begin
  result := round( (d -32) * 5/9);
end;

end.

