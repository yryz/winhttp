//v0.1b
//20100903 支持Cookie过期过滤
unit CookieMgr_u;

interface
uses
  Windows, SysUtils, Classes;

type
  { Simple Cookies Manage By HouSoft 2010}
  TCookieMgr = class(TStringList)
  private
    FDelete: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    { name=value }
    procedure AddCookie(Properties: string);
    { name=value; domain=yryz.net; path=/ }
    procedure AddCookieSrc(Cookies: string);
    { name1=value1; name2=value2 }
    function GetCookies: string;
  end;

implementation
uses
  WinHttp;

{ TCookieMgr }

constructor TCookieMgr.Create;
begin
  inherited Create;
end;

destructor TCookieMgr.Destroy;
begin
  inherited Destroy;
end;

procedure TCookieMgr.AddCookie(Properties: string);
{ RFC2109 ;Nonsupport RFC2965}
const
  RFC2109_ATTR      : array[0..5] of array[0..7] of char =
    ('domain', 'expires', 'max-age', 'path', 'secure', 'version');
var
  i                 : Integer;
  sName, sValue     : string;
begin
  { don't Cookie attr }
  for i := Low(RFC2109_ATTR) to High(RFC2109_ATTR) do
    if StrLIComp(RFC2109_ATTR[i], PChar(Properties),
      StrLen(RFC2109_ATTR[i])) = 0 then Exit;

  i := Pos('=', Properties);
  if i <= 1 then Exit;

  sName := Copy(Properties, 1, i - 1);
  if FDelete then sValue := ''
  else sValue := Copy(Properties, i + 1, MaxInt);

  Values[sName] := sValue;
end;

procedure TCookieMgr.AddCookieSrc(Cookies: string);
var
  S                 : string;
  P1, P2            : PChar;
begin
  //过期
  S := THTTP.GetSubStr(Cookies, 'expires=', 'GMT');
  FDelete := (S <> '') and (THTTP.InternetTimeToBjTime(S) < Now);

  P1 := PChar(Cookies);
  P2 := P1;
  while True do
  begin
    if P2^ = ';' then
    begin                               //have values
      if P2 > P1 then
      begin
        S := Copy(P1, 1, P2 - P1);
        AddCookie(TrimLeft(S));
      end;
      Inc(P2);
      P1 := P2;
    end else
    begin
      if P2^ = #0 then
      begin
        if P2 > P1 then AddCookie(TrimLeft(P1)); //add cookie end
        Break;
      end else
        Inc(P2);
    end;
  end;
  FDelete := False;
end;

function TCookieMgr.GetCookies: string;
var
  i                 : Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do begin
    Result := Result + Strings[i];
    if i < Count - 1 then Result := Result + '; ';
  end;
end;

end.

