{
  -----------------------------------------------
  #         WinHttp v0.2i                       -
  #                                             -
  #        by: yryz                             -
  #     email: yryznet@qq.com                   -
  # home page: http://www.yryz.net              -
  -----------------------------------------------
}

unit WinHttp;

interface

uses
  Windows, SysUtils, Classes, WinHttp_h, ShellAPI, SHFolder, CookieMgr_u,
  FuncLib;

type
{$IFNDEF UNICODE}
  RawByteString = AnsiString;
{$ENDIF}
  TCodePage = (cpNone, cpGBK, cpUTF8, cpBig5, cpAuto);

{ HTTP DATA }
  TUrlEncodedData = class
  private
    FData: TStrings;
    FCodePage: TCodePage;
    FUrlEncoded: Boolean;
  public
    constructor Create(CodePage: TCodePage; UrlEncoded: Boolean);
    destructor Destroy; override;
    function Put(Name, Value: string): TUrlEncodedData; overload;
    function Put(Name: string; Value: Integer): TUrlEncodedData; overload;
    function ToString: RawByteString; {$IFDEF UNICODE}reintroduce;{$ENDIF}
    property Data: TStrings read FData;
  end;

const
  MULTIPART_CONTENTTYPE = 'multipart/form-data; boundary=---------------------------7da30d3390726';
  MULTIPART_DATA_CRLF   = '-----------------------------7da30d3390726'#13#10;
  MULTIPART_DATA_END    = '-----------------------------7da30d3390726--'#13#10;

type
  TMultipartData = class
  private
    FData: TBytes;
    FMaked: Boolean;
    FCodePage: TCodePage;
    procedure Append(AData: string); overload;
    procedure Append(const AData; ASize: Integer); overload;
  public
    constructor Create(CodePage: TCodePage);
    destructor Destroy; override;
    function Add(Name, Value: string): TMultipartData; overload;
    function Add(Name: string; Value: Integer): TMultipartData; overload;
    function Add(Name, FileName, ContentType: string; Value: TBytes): TMultipartData; overload;
    function Data: TBytes;
  end;

const
  RECVBUF_SIZE                  = 4096;
  DEFAULT_USER_AGENT: PWideChar =
    'Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1)';
  REQ_ACCEPT_TYPE_RAW: PWideChar             = 'Accept: */*';
  REQ_ACCEPT_LANGUAGE_RAW: PWideChar         = 'Accept-Language: zh-cn';
  REQ_CONTENT_TYPE_TEXT_RAW: PWideChar       = 'Content-Type: text/plain';
  REQ_CONTENT_TYPE_URLENCODED_RAW: PWideChar =
    'Content-Type: application/x-www-form-urlencoded';
  REQ_ACCEPT_ENCODING_RAW: PWideChar = 'Accept-Encoding: gzip, deflate';

type
  TCookieMgr = CookieMgr_u.TCookieMgr;

  THTTP = class
  private
    FStop: Boolean; // 是否停止下载
    FDomain: string;
    FReqContentType: string;
    { 可定义 }
    FAliveKeep: Boolean;
    FAutoRedirect: Boolean;
    FNrAutoRedirect: Integer;
    FReferer: string;
    FCustomHeader: string;
    { 响应 }
    FRawHeader: string;
    FLocation: string;
    FGMTDate: string;
    FStartTime, FUseTime: DWORD;
    FTotalSize: DWORD;

    FCodePage: TCodePage;
  protected
    FSession: HINTERNET; // HTTP句柄
    FConnect: HINTERNET; // HTTP句柄
    FRequest: HINTERNET; // HTTP句柄

    FUseProxy: Boolean;
    FProxyUser: PWideChar;
    FProxyPass: PWideChar;
    FUserAgent: PWideChar;

    FCookieMgr: TCookieMgr;
  protected
    function GetCookies: string;
    procedure SetCookies(const Value: string);
    function GetBJDate: TDateTime;
    procedure SetUserAgent(const Value: string);

    // 分析响应头
    procedure GetCookie();
    procedure GetGMTDate(); // 服务器GMT日期
    procedure GetLocation();
    procedure GetRawHeader();

    // 内容解码
    function ContentDecode(ASrc: RawByteString;
      codePage: TCodePage): RawByteString;
  public
    constructor Create(AProxy: string = ''; AUser: string = '';
      APass: string = ''; UserAgent: string='');
    destructor Destroy; override;

    class function GetSubStr(const Str, _Start, _End: string): string;
    class function GetSubStrEx(const Str, _Start, _End: string;
      var LastStr: string { 余下部分 } ): string;
    class function GetTempPath(): PChar;   // Sys Temp Path
    class function GetIeTempPath(): PChar; // IE Cache Path
    class function GetCookiePath(): PChar; // Cookie Path
    class function GetInternetTimeFromLocal(): PWideChar;
    // SystemTime To InternetTime
    class function InternetTimeToBjTime(sTime: string): TDateTime; // 北京时间 +8时区
    class function CrackUrl(const swUrl: WideString;
      var sHost, sPath: WideString; var nPort: Word): Boolean;

    class function CodePageEnc(ASrc: RawByteString; codePage: TCodePage)
      : RawByteString;
    class function CodePageDec(ASrc: RawByteString; codePage: TCodePage)
      : RawByteString;

    // URI编码,用于本地为GBK
    class function URLEnc(ASrc: RawByteString; codePage: TCodePage = cpNone)
      : RawByteString;
    class function URLDec(ASrc: RawByteString; codePage: TCodePage = cpNone)
      : RawByteString;

    procedure ClearData();
    function GetFullUrl(APath: string): string;
    { 使用下面函数要手动清理数据 }
    function SendRequest(const sUrl, sMode: string; dwContent: DWORD): Boolean;
    function SendPostData(lpData: Pointer; dwLen: DWORD): DWORD;
    function RecvReponse(): Boolean;
    function RecvContent(ResponseCodePage: TCodePage = cpNone;
      dwMaxLen: DWORD = INFINITE): RawByteString; overload; // 返回源文本内容
    function RecvContent(var Buf; Len: Integer): Integer; overload;
    function RecvContentToFile(hFile: THandle): DWORD; // 返回大小
	  function RecvContentToStream(var Stream: TStream): DWORD; // 返回大小
    procedure DoRequestEnd(isFinish: Boolean = True);  // 清理数据
    { end }
    // 超时设置
    function SetTimeouts(nResolveTimeout: Integer;
      nConnectTimeout: Integer;
      nSendTimeout: Integer;
      nReceiveTimeout: Integer
      ): Boolean;

    // Result : RawByteString 根据需要再解码
    function Get(const sUrl: string;
      ResponseCodePage: TCodePage = cpNone): RawByteString; overload;
    // Result File Size
    function Get(const sUrl, sSaveFile: string): DWORD; overload;
    function Get(const sUrl: string; var Stream: TStream): DWORD; overload;
     
    function Post(const sUrl: string; sData: RawByteString;
      ResponseCodePage: TCodePage = cpNone): RawByteString; overload;
    function Post(const sUrl: string; lpData: Pointer; dwLen: DWORD;
      ResponseCodePage: TCodePage = cpNone): RawByteString; overload;
    function Post(const sUrl: string; sData: RawByteString; sContentType: string;
      ResponseCodePage: TCodePage = cpNone): RawByteString; overload;
    function Post(const sUrl: string; lpData: Pointer; dwLen: DWORD; sContentType: string;
      ResponseCodePage: TCodePage = cpNone): RawByteString; overload;
    function Post(const sUrl: string; AData: TUrlEncodedData;
      AutoRedirect: Boolean;
      ResponseCodePage: TCodePage = cpNone): RawByteString; overload;
    function Post(const sUrl: string; AData: TMultipartData;
      AutoRedirect: Boolean;
      ResponseCodePage: TCodePage = cpNone): RawByteString; overload;

    procedure ClearCookie();
    procedure Abort;
  public
    property AliveKeep: Boolean read FAliveKeep write FAliveKeep;
    property AutoRedirect: Boolean read FAutoRedirect write FAutoRedirect;
    property NrAutoRedirect: Integer read FNrAutoRedirect write FNrAutoRedirect;
    property CustomHander: string read FCustomHeader write FCustomHeader;
    property UserAgent: string write SetUserAgent;
    property Cookies: string read GetCookies write SetCookies;
    property CookieMgr: TCookieMgr read FCookieMgr;
    property RawHeader: string read FRawHeader;
    property Referer: string read FReferer write FReferer;
    property Location: string read FLocation;
    property GMTDate: string read FGMTDate;
    property BJDate: TDateTime read GetBJDate;
    property GetSize: DWORD read FTotalSize;
    property GetUseTime: Dword read FUseTime;

    property CodePage: TCodePage read FCodePage;
  end;

procedure OutDebug(Str: string);

implementation

procedure OutDebug(Str: string);
var
  e: DWORD;
begin
  e := GetLastError;
  if e <> 0 then
    OutputDebugString(PChar(Str + Format(' error:%d', [e])))
  else
    OutputDebugString(PChar(Str));
end;

{ ------------------------------THTTP-------------------------------- }

constructor THTTP.Create;
begin
  FAutoRedirect := True;
  FNrAutoRedirect := 8; // 20100629 Fix
  if UserAgent='' then
    FUserAgent := DEFAULT_USER_AGENT
  else
    FUserAgent := StringToOleStr(UserAgent);

  if AProxy <> '' then
  begin
    FUseProxy := True;
    FProxyUser := StringToOleStr(AUser);
    FProxyPass := StringToOleStr(APass);
    FSession := WinHttpOpen(FUserAgent,
      WINHTTP_ACCESS_TYPE_NAMED_PROXY,
      StringToOleStr(AProxy),
      WINHTTP_NO_PROXY_BYPASS, 0);
  end
  else
    FSession := WinHttpOpen(FUserAgent,
      WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
      WINHTTP_NO_PROXY_NAME,
      WINHTTP_NO_PROXY_BYPASS, 0);

  if not Assigned(FSession) then
    raise Exception.Create('InternetOpen Error!');

  FCookieMgr := TCookieMgr.Create;
end;

destructor THTTP.Destroy;
begin
  FStop := True;
  DoRequestEnd(False);
  if Assigned(FSession) then
    WinHttpCloseHandle(FSession);
  if Assigned(FCookieMgr) then
    FCookieMgr.Free;
  inherited Destroy;
end;

procedure THTTP.ClearData();
begin
  FRawHeader := '';
  FLocation := '';
  FGMTDate := '';
  FTotalSize := 0;
  FUseTime := 0;
  FCodePage := cpNone;
end;

class function THTTP.CodePageDec(ASrc: RawByteString; codePage: TCodePage)
      : RawByteString;
begin
  case codePage of
    cpGBK:
      Result := ASrc;
    cpUTF8:
{$IFDEF UNICODE}
      Result := UTF8ToString(ASrc);
{$ELSE}
      Result := Utf8ToAnsi(ASrc);
{$ENDIF}
    cpBig5:
      Result := BIG5Decode(ASrc);
  else
    Result := ASrc;
  end;
end;

class function THTTP.CodePageEnc(ASrc: RawByteString; codePage: TCodePage)
      : RawByteString;
begin
  case codePage of
    cpGBK:
      Result := ASrc;
    cpUTF8:
      Result := UTF8Encode(ASrc);
    cpBig5:
      Result := BIG5Encode(ASrc);
  else
    Result := ASrc;
  end;
end;

function THTTP.SendRequest;
var
  n: Integer;
  dwFlags: DWORD;
  swHost, swPath: WideString;
  wPort: Word;
  pwszHeader, pwszHeaders: PWideChar;
  sHeader, sTmp: string;
begin
  ClearData;
  FStop := False;
  Result := False;
  FStartTime := GetTickCount();

  // 分析URL
  if not CrackUrl(sUrl, swHost, swPath, wPort) then
    Exit;

  if FAutoRedirect then
  begin                             // 重定向备用 20100827 Fix
    n := Pos('/', PChar(@sUrl[9])); // skip https://
    if n = 0 then
      FDomain := sUrl
    else
      FDomain := Copy(sUrl, 1, n + 7);
  end;

  if FCustomHeader <> '' then
  begin
    if FCustomHeader[Length(FCustomHeader)] <> #10 then
      FCustomHeader := FCustomHeader + #13#10;
    sTmp := GetSubStr(UpperCase(FCustomHeader), 'GET ', ' HTTP/1.');
    if sTmp <> '' then
      swPath := sTmp;
  end;

  // 创建连接
  FConnect := WinHttpConnect(FSession, PWideChar(swHost), wPort, 0);
  if FConnect = nil then
    Exit;

  // 创建请求
  dwFlags := WINHTTP_FLAG_REFRESH; // 不要代理缓存
  if wPort = INTERNET_DEFAULT_HTTPS_PORT then
    dwFlags := dwFlags or WINHTTP_FLAG_SECURE // SSL连接
  else
    dwFlags := dwFlags and not WINHTTP_FLAG_SECURE;

  FRequest := WinHttpOpenRequest(FConnect,
    StringToOleStr(sMode),
    PWideChar(swPath),
    nil,
    PWideChar(WideString(FReferer)),
    WINHTTP_DEFAULT_ACCEPT_TYPES,
    dwFlags);

  if FRequest = nil then
    Exit;

  // 代理
  if FUseProxy then
  begin
    WinHttpSetOption(FRequest,
      WINHTTP_OPTION_PROXY_USERNAME,
      FProxyUser,
      Length(FProxyUser));
    WinHttpSetOption(FRequest,
      WINHTTP_OPTION_PROXY_PASSWORD,
      FProxyPass,
      Length(FProxyPass));
  end;

  dwFlags := WINHTTP_DISABLE_COOKIES // 禁止WinHttp Cookie管理
    or WINHTTP_DISABLE_REDIRECTS;    // 禁止重定向

  // 手动处理COOKIE 20100629 Fix
  if not FAliveKeep then
    dwFlags := dwFlags
      or WINHTTP_DISABLE_KEEP_ALIVE; // 禁止KeepAlive

  WinHttpSetOption(FRequest,
    WINHTTP_OPTION_DISABLE_FEATURE,
    @dwFlags,
    SizeOf(dwFlags));

  // 常规头
  pwszHeaders := WINHTTP_NO_ADDITIONAL_HEADERS;
  dwFlags := WINHTTP_ADDREQ_FLAG_REPLACE or WINHTTP_ADDREQ_FLAG_ADD;

  { Accept }
  WinHttpAddRequestHeaders(FRequest,
    REQ_ACCEPT_TYPE_RAW,
    Length(REQ_ACCEPT_TYPE_RAW),
    dwFlags);

  { Accept-Language }
  WinHttpAddRequestHeaders(FRequest,
    REQ_ACCEPT_LANGUAGE_RAW,
    Length(REQ_ACCEPT_LANGUAGE_RAW),
    dwFlags);

  { Cookie }
  pwszHeader := StringToOleStr('Cookie: ' + FCookieMgr.GetCookies);
  WinHttpAddRequestHeaders(FRequest,
    pwszHeader,
    Length(pwszHeader),
    dwFlags);

  { Content-Type }
  if sMode = 'POST' then
    if FReqContentType = '' then
    begin
      WinHttpAddRequestHeaders(FRequest,
        REQ_CONTENT_TYPE_URLENCODED_RAW,
        Length(REQ_CONTENT_TYPE_URLENCODED_RAW),
        dwFlags);
      // Content-Type: application/x-www-form-urlencoded
    end
    else
    begin
      pwszHeader := StringToOleStr('Content-Type: ' + FReqContentType);
      WinHttpAddRequestHeaders(FRequest,
        pwszHeader,
        Length(pwszHeader),
        dwFlags);
    end;

  { Accept-Encoding }
  // WinHttpAddRequestHeaders(FRequest, REQ_ACCEPT_ENCODING_RAW, Length(REQ_ACCEPT_ENCODING_RAW), dwFlags);

  // 自定义头
  if FCustomHeader <> '' then
  begin
    sTmp := FCustomHeader;
    while True do
    begin
      sHeader := GetSubStrEx(sTmp, '', #13#10, sTmp);
      if sHeader <> '' then
        pwszHeader := StringToOleStr(sHeader)
      else if sTmp <> '' then
        pwszHeader := StringToOleStr(sTmp)
      else
        Break;

      WinHttpAddRequestHeaders(FRequest,
        pwszHeader,
        Length(pwszHeader),
        dwFlags);
    end;
  end;

  // 发送请求
  if not WinHttpSendRequest(FRequest,
    pwszHeaders,
    Length(pwszHeaders),
    nil,
    0,
    dwContent,
    0) then
  begin
    OutDebug('SendRequest Fail!');
    Exit;
  end;

  Result := True;
end;

function THTTP.RecvReponse;
begin
  Result := WinHttpReceiveResponse(FRequest, nil);
  if not Result then
  begin
    OutDebug('RecvReponse Fail!');
    Exit;
  end;

  GetCookie;
  GetGMTDate;
  GetLocation;
  GetRawHeader;

  // 重定向
  if FAutoRedirect and (FLocation <> '') and (FNrAutoRedirect > 0) then
  begin
    Dec(FNrAutoRedirect);
    DoRequestEnd(False);

    FLocation := GetFullUrl(FLocation);
    Result := SendRequest(FLocation, 'GET', 0) and RecvReponse;
  end;
end;

function THTTP.SendPostData;
begin
  if not WinHttpWriteData(FRequest,
    lpData,
    dwLen,
    @Result) then
    Result := 0;
end;

function THTTP.ContentDecode(ASrc: RawByteString; codePage: TCodePage)
  : RawByteString;
var
  i: Integer;
  sTmp: string;
  pCharSet: PChar;
begin
  // 自动识别编码
  if codePage = cpAuto then
  begin
    FCodePage := cpNone;

    // 从响应头获取 Content-Type: text/html; charset=utf-8
    sTmp := GetSubStr(LowerCase(FRawHeader), #$A'content-type:', #$D);
    if sTmp <> '' then // text/html; charset=utf-8
      pCharSet := StrPos(PChar(sTmp), 'charset=');

    if pCharSet = nil then // 在HTML中？
    begin
      sTmp := LowerCase(ASrc);
      pCharSet := StrPos(PChar(sTmp), 'charset=');

      // 验证是否属于<meta
      i := pCharSet - PChar(sTmp);
      while i > 0 do
      begin
        if sTmp[i] = '<' then
        begin
          if StrLComp(PChar(@sTmp[i + 1]), 'meta', 4) <> 0 then
            pCharSet := nil;
          Break;
        end;
        Dec(i);
      end;
    end;

    if pCharSet <> nil then // charset=utf-8
    begin
      Inc(pCharSet, 8);
      if pCharSet^ in ['"', ''''] then // THML5: <meta charset="UTF-8">
        Inc(pCharSet, 1);

      if (StrLComp(pCharSet, 'gbk', 3) = 0)
        or (StrLComp(pCharSet, 'gb2312', 6) = 0)
        or (StrLComp(pCharSet, 'gb18030', 7) = 0) then
        FCodePage := cpGBK
      else if (StrLComp(pCharSet, 'utf-8', 5) = 0) then
        FCodePage := cpUTF8
      else if (StrLComp(pCharSet, 'big5', 4) = 0) then
        FCodePage := cpBig5;
    end;
  end
  else
    FCodePage := codePage;

  Result := CodePageDec(ASrc, FCodePage);
end;

function THTTP.RecvContent(ResponseCodePage: TCodePage = cpNone;
  dwMaxLen: DWORD = INFINITE): RawByteString;
var
  dwLen: DWORD;
  szRcvBuf: array [0 .. RECVBUF_SIZE - 1] of AnsiChar;
begin
  Result := '';
  while
    (FTotalSize < dwMaxLen)
    and WinHttpReadData(FRequest,
    @szRcvBuf,
    SizeOf(szRcvBuf),
    @dwLen)
    and not FStop
    and (dwLen > 0) do
  begin
    Inc(FTotalSize, dwLen);

    if (FTotalSize > dwMaxLen) then
    begin
      Dec(dwLen, FTotalSize - dwMaxLen);
      FTotalSize := dwMaxLen;
    end;

    if dwLen < RECVBUF_SIZE then
      szRcvBuf[dwLen] := AnsiChar(0);

    Result := Result + PAnsiChar(@szRcvBuf);
  end;

  Result := ContentDecode(Result, ResponseCodePage);
end;

function THTTP.RecvContent(var Buf; Len: Integer): Integer;
begin
  WinHttpReadData(FRequest,
    @Buf,
    Len,
    @Result);
end;

function THTTP.RecvContentToFile;
var
  dwLen: DWORD;
  szRcvBuf: array [0 .. RECVBUF_SIZE - 1] of Byte;
begin
  while WinHttpReadData(FRequest,
    @szRcvBuf,
    SizeOf(szRcvBuf),
    @dwLen)
    and not FStop
    and (dwLen > 0)
    and (FileWrite(hFile, szRcvBuf, dwLen) >= 0) do
    Inc(FTotalSize, dwLen);

  Result := FTotalSize;
end;

function THTTP.RecvContentToStream;
var
  dwLen: DWORD;
  szRcvBuf: array [0 .. RECVBUF_SIZE - 1] of Byte;
begin
  while WinHttpReadData(FRequest,
    @szRcvBuf,
    SizeOf(szRcvBuf),
    @dwLen)
    and not FStop
    and (dwLen > 0)
    and (Stream.Write(szRcvBuf, dwLen) >= 0) do
    Inc(FTotalSize, dwLen);

  Result := FTotalSize;
end;

procedure THTTP.DoRequestEnd;
var
  tick: DWORD;
begin
  if Assigned(FRequest) then
  begin
    WinHttpCloseHandle(FRequest);
    FRequest := nil;
  end;

  if Assigned(FConnect) then
  begin
    WinHttpCloseHandle(FConnect);
    FConnect := nil;
  end;

  if not isFinish then
    Exit;

  if FUseTime = 0 then
  begin
    tick := GetTickCount;
    if tick > FStartTime then
      FUseTime := tick - FStartTime
    else
      FUseTime := DWORD(-1) - FStartTime + tick;
  end;
end;

procedure THTTP.ClearCookie();
begin
  FCookieMgr.Clear;
end;

function THTTP.Get(const sUrl: string;
  ResponseCodePage: TCodePage = cpNone): RawByteString;
begin
  Result := '';
  if SendRequest(sUrl, 'GET', 0) and RecvReponse then
    Result := RecvContent(ResponseCodePage);

  DoRequestEnd;
end;

function THTTP.Get(const sUrl, sSaveFile: string): DWORD;
var
  hFile: THandle;
begin
  hFile := 0;
  Result := 0;
  try
    hFile := FileCreate(sSaveFile);
    if Integer(hFile) > 0 then
    begin
      if SendRequest(sUrl, 'GET', 0)
        and RecvReponse then
        RecvContentToFile(hFile);

      Result := FTotalSize;
    end;
  finally
    if Integer(hFile) > 0 then
      FileClose(hFile);
    DoRequestEnd;
  end;
end;

function THTTP.Post(const sUrl: string; sData: RawByteString;
  ResponseCodePage: TCodePage = cpNone): RawByteString;
begin
  Result := Post(sUrl, Pointer(sData), Length(sData), ResponseCodePage);
end;

function THTTP.Post(const sUrl: string; lpData: Pointer; dwLen: DWORD;
  ResponseCodePage: TCodePage = cpNone): RawByteString;
begin
  Result := '';
  try
    if SendRequest(sUrl, 'POST', dwLen) then
    begin
      if (dwLen > 0)
        and (SendPostData(lpData, dwLen) <= 0) then
        Exit;

      if RecvReponse then
        Result := RecvContent(ResponseCodePage);
    end;
  finally
    DoRequestEnd;
  end;
end;

function THTTP.Post(const sUrl: string; sData: RawByteString;
  sContentType: string; ResponseCodePage: TCodePage): RawByteString;
begin
  Result := Post(sUrl, Pointer(sData), Length(sData), sContentType, ResponseCodePage);
end;

function THTTP.Post(const sUrl: string; lpData: Pointer; dwLen: DWORD;
  sContentType: string; ResponseCodePage: TCodePage): RawByteString;
begin
  FReqContentType := sContentType;
  Result := Post(sUrl, lpData, dwLen, ResponseCodePage);
  FReqContentType := '';
end;

procedure THTTP.Abort;
begin
  FStop := True;
end;

function THTTP.GetCookies: string;
begin
  Result := FCookieMgr.GetCookies;
end;

procedure THTTP.SetCookies(const Value: string);
begin
  FCookieMgr.Clear;
  FCookieMgr.AddCookieSrc(Value);
end;

function THTTP.Get(const sUrl: string; var Stream: TStream): DWORD;
begin
  Result := 0;
  try
    Stream.Seek(0, soFromBeginning);
    
    if SendRequest(sUrl, 'GET', 0)
      and RecvReponse then
      RecvContentToStream(Stream);

    Result := FTotalSize;
  finally
    DoRequestEnd;
  end;
end;

function THTTP.GetBJDate;
begin
  Result := InternetTimeToBjTime(FGMTDate);
end;

procedure THTTP.SetUserAgent(const Value: string);
var
  pwszHeader: PWideChar;
begin
  pwszHeader := StringToOleStr(Value);

  WinHttpSetOption(FSession,
    WINHTTP_OPTION_USER_AGENT,
    pwszHeader,
    Length(pwszHeader));
end;

function THTTP.SetTimeouts;
begin
  Result := WinHttpSetTimeouts(FSession,
    nResolveTimeout,
    nConnectTimeout,
    nSendTimeout,
    nReceiveTimeout);
end;

{ 静态类函数 }

class function THTTP.URLEnc(ASrc: RawByteString; codePage: TCodePage = cpNone)
      : RawByteString; // 20100923 Fix Unicode
const
  ANSI_HEX: array [0 .. 15] of AnsiChar = '0123456789ABCDEF';

var
  b: Byte;
  i: Integer;
  LTmp: RawByteString;
  pDst: PAnsiChar;
begin
  Result := '';
  LTmp := CodePageEnc(ASrc, codePage);

  SetLength(Result, Length(LTmp) * 3); // 预留
  pDst := Pointer(Result);

  for i := 1 to Length(LTmp) do
  begin
    if not(LTmp[i] in ['A' .. 'Z', 'a' .. 'z', '0' .. '9']) then
    begin
      b := Byte(LTmp[i]);
      pDst[0] := '%';
      pDst[1] := ANSI_HEX[b shr 4];
      pDst[2] := ANSI_HEX[b and $0F];
      Inc(pDst, 3);
    end
    else
    begin
      pDst^ := LTmp[i];
      Inc(pDst);
    end;
  end;
  SetLength(Result, pDst - Pointer(Result));
end;

class function THTTP.URLDec(ASrc: RawByteString; codePage: TCodePage = cpNone)
      : RawByteString;
var
  i: integer;
  ESC: RawByteString;
  CharCode: integer;

  LTmp: RawByteString;
begin
  LTmp := '';
  Result := '';

  ASrc := StringReplace(ASrc, '+', ' ', [rfReplaceAll]); { do not localize }
  i := 1;
  while i <= Length(ASrc) do
  begin
    if ASrc[i] <> '%' then { do not localize }
      LTmp := LTmp + ASrc[i]
    else
    begin
      Inc(i);                  // skip the % char
      ESC := Copy(ASrc, i, 2); // Copy the escape code
      Inc(i, 1);               // Then skip it.
      try
        CharCode := StrToInt('$' + ESC);
        if (CharCode > 0) and (CharCode < 256) then
        begin
          LTmp := LTmp + AnsiChar(CharCode);
        end;
      except
      end;
    end;
    Inc(i);
  end;

  Result := CodePageDec(LTmp, codePage);
end;

class function THTTP.GetSubStr;
// 2009-11-16
var
  n: Integer;
begin
  if _Start <> '' then
  begin
    n := pos(_Start, Str);
    if n = 0 then
    begin
      Result := '';
      Exit;
    end;
  end
  else
    n := 1;

  Result := copy(Str, n + length(_Start), length(Str));
  if _End = '' then
    n := length(Result) + 1
  else
    n := pos(_End, Result);

  Result := copy(Result, 1, n - 1);
end;

class function THTTP.GetSubStrEx;
// 2009-11-16
var
  n: Integer;
begin
  if _Start <> '' then
  begin
    n := pos(_Start, Str);
    if n = 0 then
    begin
      Result := '';
      LastStr := Str;
      Exit;
    end;
  end
  else
    n := 1;

  LastStr := copy(Str, n + length(_Start), length(Str));
  if _End = '' then
    n := length(Str) + 1
  else
    n := pos(_End, LastStr);

  Result := copy(LastStr, 1, n - 1);
  LastStr := copy(LastStr, n + length(_End), length(LastStr));
end;

class function THTTP.GetTempPath;
var
  szBuf: array [0 .. MAX_PATH - 1] of char;
begin
  if Windows.GetTempPath(MAX_PATH, szBuf) <> 0 then
    Result := szBuf
  else
    Result := '';
end;

class function THTTP.GetIeTempPath;
var
  szBuf: array [0 .. MAX_PATH - 1] of char;
begin
  if SUCCEEDED(SHGetFolderPath(0,
    CSIDL_INTERNET_CACHE or CSIDL_FLAG_CREATE,
    0,
    0,
    szBuf)) then
    Result := StrCat(PChar(@szBuf), '\')
  else
    Result := '';
end;

class function THTTP.GetCookiePath;
var
  szBuf: array [0 .. MAX_PATH - 1] of char;
begin
  if SUCCEEDED(SHGetFolderPath(0,
    CSIDL_COOKIES or CSIDL_FLAG_CREATE,
    0,
    0,
    szBuf)) then
    Result := StrCat(PChar(@szBuf), '\')
  else
    Result := '';
end;

class function THTTP.GetInternetTimeFromLocal; // SystemTime To InternetTime
var
  lpSysTime: TSYSTEMTIME;
  szDate: array [0 .. WINHTTP_TIME_FORMAT_BUFSIZE div SizeOf(WChar) - 1]
    of WChar;
begin
  GetLocalTime(lpSysTime);

  WinHttpTimeFromSystemTime(@lpSysTime,
    PWideChar(@szDate));

  Result := PWideChar(@szDate);
end;

class function THTTP.InternetTimeToBjTime;
var
  SysTime: TSYSTEMTIME;
begin
  Result := 0;
  try
    if (sTime <> '')
      and WinHttpTimeToSystemTime(StringToOleStr(sTime), @sysTime) then
      with SysTime do
        Result := EncodeDate(wYear, wMonth, wDay + (wHour + 8) div 24) // 20110616 Fix by hou
          + EncodeTime((wHour + 8) mod 24, wMinute, wSecond, wMilliseconds);
  except
    Result := MaxDateTime; // 一些不能识别的日期
  end;
end;

class function THTTP.CrackUrl;
var
  crackedURL: TURLComponents;
  szHost: array [0 .. 256 - 1] of WChar;
  szPath: array [0 .. 2048 - 1] of WChar;
begin
  ZeroMemory(@crackedURL, sizeof(TURLComponents));
  with crackedURL do
  begin
    dwStructSize := sizeof(TURLComponents);
    lpszHostName := @szHost;
    dwHostNameLength := SizeOf(szHost);
    lpszUrlPath := @szPath;
    dwUrlPathLength := SizeOf(szPath);
  end;

  Result := WinHttpCrackUrl(PWideChar(swUrl),
    Length(swUrl),
    0,
    @crackedURL);

  // 处理HTTP URL
  if Result then
  begin
    sHost := szHost;
    sPath := szPath;
    nPort := crackedURL.nPort;
  end;
end;

procedure THTTP.GetCookie;
var
  dwLen, dwIndex: DWORD;
  pwszBuf: PWideChar;
begin
  if not Assigned(FRequest) then
    Exit;

  dwLen := 0;
  dwIndex := 0;
  while True do
  begin // 多行Set-Cookie处理
    WinHttpQueryHeaders(FRequest,
      WINHTTP_QUERY_SET_COOKIE,
      nil,
      nil,
      @dwLen,
      @dwIndex);

    if (dwLen = 0) then
      Break;

    GetMem(pwszBuf, dwLen);
    try
      if not WinHttpQueryHeaders(FRequest,
        WINHTTP_QUERY_SET_COOKIE,
        nil,
        pwszBuf,
        @dwLen,
        @dwIndex) then // auto inc index
        Break;

      FCookieMgr.AddCookieSrc(WideCharToString(pwszBuf));
    finally
      FreeMemory(pwszBuf);
    end;
  end;
end;

procedure THTTP.GetGMTDate;
var
  dwLen: DWORD;
  pwszBuf: PWideChar;
begin
  FGMTDate := '';
  if not Assigned(FRequest) then
    Exit;

  dwLen := 0;

  WinHttpQueryHeaders(FRequest,
    WINHTTP_QUERY_DATE,
    nil,
    nil,
    @dwLen,
    nil);

  if (dwLen <> 0) then
  begin
    GetMem(pwszBuf, dwLen);

    WinHttpQueryHeaders(FRequest,
      WINHTTP_QUERY_DATE,
      nil,
      pwszBuf,
      @dwLen,
      nil);

    FGMTDate := WideCharToString(pwszBuf);
    FreeMemory(pwszBuf);
  end;
end;

procedure THTTP.GetLocation;
var
  dwLen: DWORD;
  pwszBuf: PWideChar;
begin
  FLocation := '';
  if not Assigned(FRequest) then
    Exit;

  dwLen := 0;

  WinHttpQueryHeaders(FRequest,
    WINHTTP_QUERY_LOCATION,
    nil,
    nil,
    @dwLen,
    nil);

  if (dwLen <> 0) then
  begin
    GetMem(pwszBuf, dwLen);

    WinHttpQueryHeaders(FRequest,
      WINHTTP_QUERY_LOCATION,
      nil,
      pwszBuf,
      @dwLen,
      nil);

    FLocation := WideCharToString(pwszBuf);
    FreeMemory(pwszBuf);
  end;
end;

procedure THTTP.GetRawHeader;
var
  dwLen: DWORD;
  pwszBuf: PWideChar;
begin
  FRawHeader := '';
  if not Assigned(FRequest) then
    Exit;

  dwLen := 0;

  WinHttpQueryHeaders(FRequest,
    WINHTTP_QUERY_RAW_HEADERS_CRLF,
    nil,
    nil,
    @dwLen,
    nil);

  if (dwLen <> 0) then
  begin
    GetMem(pwszBuf, dwLen);

    WinHttpQueryHeaders(FRequest,
      WINHTTP_QUERY_RAW_HEADERS_CRLF,
      nil,
      pwszBuf,
      @dwLen,
      nil);

    FRawHeader := WideCharToString(pwszBuf);
    FreeMemory(pwszBuf);
  end;
end;

function THTTP.GetFullUrl(APath: string): string;
begin
  if Pos('://', APath) = 0 then // http:// or https://
    Result := FDomain + '/' + APath
  else
    Result := APath;
end;

{ TUrlEncodedData }

constructor TUrlEncodedData.Create(CodePage: TCodePage; UrlEncoded: Boolean);
begin
  FCodePage := CodePage;
  FUrlEncoded := UrlEncoded;
  FData := TStringList.Create;
  FData.LineBreak := '&';
end;

destructor TUrlEncodedData.Destroy;
begin
  FData.Free;
  inherited;
end;

function TUrlEncodedData.Put(Name, Value: string): TUrlEncodedData;
begin
  if FUrlEncoded then
    Value := THTTP.URLEnc(Value, FCodePage);
  FData.Values[Name] := Value;
  Result := Self;
end;

function TUrlEncodedData.Put(Name: string; Value: Integer): TUrlEncodedData;
begin
  FData.Values[Name] := IntToStr(Value);
  Result := Self;
end;

function TUrlEncodedData.ToString: RawByteString;
begin
  Result := FData.Text;
  if not FUrlEncoded then
    Result := THTTP.CodePageEnc(Result, FCodePage);
end;

{ TMultipartData }

constructor TMultipartData.Create(CodePage: TCodePage);
begin
  FCodePage := CodePage;
end;

destructor TMultipartData.Destroy;
begin

  inherited;
end;

function TMultipartData.Add(Name, Value: string): TMultipartData;
begin
  Assert(not FMaked);
  Append(
    MULTIPART_DATA_CRLF
    + 'Content-Disposition: form-data; name="' + Name + '"'#13#10#13#10
    + Value + #13#10
    );
  Result := Self;
end;

function TMultipartData.Add(Name: string; Value: Integer): TMultipartData;
begin
  Add(Name, IntToStr(Value));
  Result := Self;
end;

function TMultipartData.Add(Name, FileName, ContentType: string; Value: TBytes): TMultipartData;
begin
  Assert(not FMaked);
  Append(
    MULTIPART_DATA_CRLF
    + 'Content-Disposition: form-data; name="' + Name + '"; filename="' + FileName + '"'#13#10
    + 'Content-Type: ' + ContentType + #13#10#13#10
    );
  Append(PByte(Value)^, Length(Value));
  Append(#13#10);
  Result := Self;
end;

function TMultipartData.Data: TBytes;
begin
  if not FMaked and (Length(FData) > 0) then
  begin
    Append(MULTIPART_DATA_END);
    FMaked := True;
  end;
  Result := FData;
end;

procedure TMultipartData.Append(AData: string);
var
  LData: RawByteString;
begin
  if Length(AData) = 0 then
    Exit;

  LData := THTTP.CodePageEnc(AData, FCodePage);

  Append(LData[1], Length(AData));
end;

procedure TMultipartData.Append(const AData; ASize: Integer);
var
  LLen: Integer;
begin
  LLen := Length(FData);
  SetLength(FData, LLen + ASize);
  Move(AData, FData[LLen], ASize);
end;

function THTTP.Post(const sUrl: string; AData: TUrlEncodedData;
  AutoRedirect: Boolean;
  ResponseCodePage: TCodePage): RawByteString;
var
  Old: Boolean;
begin
  Old := Self.AutoRedirect;
  Self.AutoRedirect := AutoRedirect;

  Result := Post(sUrl, AData.Data.Text, ResponseCodePage);

  Self.AutoRedirect := Old;
end;

function THTTP.Post(const sUrl: string; AData: TMultipartData;
  AutoRedirect: Boolean;
  ResponseCodePage: TCodePage): RawByteString;
var
  Old: Boolean;
begin
  Old := Self.AutoRedirect;
  Self.AutoRedirect := AutoRedirect;

  Result := Post(sUrl, PByte(AData.Data), Length(AData.Data),
    MULTIPART_CONTENTTYPE, ResponseCodePage);

  Self.AutoRedirect := Old;
end;

end.
