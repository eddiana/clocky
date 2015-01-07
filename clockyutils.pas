unit clockyutils;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

function clockyTimeAtZone( LocalTimeZone: double; LocationTimeZone: double): TDateTime;

implementation

function clockyTimeAtZone( LocalTimeZone: double; LocationTimeZone: double): TDateTime;
begin
  result := now() + ((LocationTimeZone - LocalTimeZone)/24);
end;

end.

