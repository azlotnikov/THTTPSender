unit HTTPSender;

interface

// (c) Z.Razor | zt.am | 2012

uses Windows, WinInet, {Classes,} Sysutils;

type
  THTTPCookie = record
    FDomain: String;
    FName: String;
    FValue: String;
    FExpires: String;
    FPath: String;
  end;

  THTTPCookieArray = array of THTTPCookie;

  THTTPCookieCollection = class
  private
    Cookies: THTTPCookieArray;
    function GetCookie(Index: Integer): THTTPCookie;
    procedure PutCookie(Index: Integer; Cookie: THTTPCookie);
  public
    property Items[Index: Integer]: THTTPCookie read GetCookie write PutCookie;
    function Add(Cookie: THTTPCookie; ReplaceIfExists: Boolean): Integer;
    function Delete(Index: Integer): Boolean; overload;
    function Delete(CookieName: string): Boolean; overload;
    function Search(CookieName: string): Integer;
    function Count: Integer;
    function AllItems: string;
    procedure Clear;
  end;

type
  THTTPResponse = record
    StatusCode: Integer;
    StatusText: string;
    RawHeaders: string;
    ContentLength: Integer;
    ContentEncoding: String;
    Location: String;
    Expires: String;
  end;

  THTTPHeaders = record
    ContentType: String;
    Accept: String;
    AcceptLanguage: String;
    AcceptEncoding: String;
    ExtraHeaders: string;
    Refferer: string;
    UserAgent: string;
  end;

  THTTPBasicAuth = record
    Username: string;
    Password: string;
  end;

type
  THTTPSender = class // (TComponent)
  private
    RResponse: THTTPResponse;
    RResponseText: AnsiString;
    RAllowCookies: Boolean;
    RAutoRedirects: Boolean;
    RConnectTimeout: Integer;
    RReadTimeout: Integer;
    RSendTimeout: Integer;
    RProxy: String;
    RProxyBypass: String;
    function URLEncode(const URL: string): string;
    function GetWinInetError(ErrorCode: Cardinal): string;
    function GetQueryInfo(hRequest: Pointer; Flag: Integer): String;
    function GetHeaders: PWideChar;
    procedure ProcessCookies(Data: string);
    procedure URLExecute(HTTPS: Boolean; const ServerName, Resource: string; Method: String;
      const PostData: AnsiString = '');
    procedure ParseURL(const lpszUrl: string; var Host, Resource: string);
  public
    Cookies: THTTPCookieCollection;
    Headers: THTTPHeaders;
    BasicAuth: THTTPBasicAuth;
    property Response: THTTPResponse read RResponse;
    property ResponseText: AnsiString read RResponseText;
    function Get(URL: String): AnsiString;
    function Post(URL: String; PostData: AnsiString): AnsiString;
    function Put(URL: String): AnsiString;
    procedure Free;
    constructor Create; // (AOwner:TComponent); override;
  published
    property Proxy: string read RProxy write RProxy;
    property ProxyBypass: string read RProxyBypass write RProxyBypass;
    property AllowCookies: Boolean read RAllowCookies write RAllowCookies;
    property AutoRedirects: Boolean read RAutoRedirects write RAutoRedirects;
    property ConnectTimeout: Integer read RConnectTimeout write RConnectTimeout;
    property ReadTimeout: Integer read RReadTimeout write RReadTimeout;
    property SendTimeout: Integer read RSendTimeout write RSendTimeout;
  end;

  // procedure Register;

implementation

{ THTTPSender }
function THTTPSender.URLEncode(const URL: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(URL) do begin
    case URL[i] of
      'A' .. 'Z', 'a' .. 'z', '0' .. '9', '-', '_', '.': Result := Result + URL[i];
    else Result := Result + '%' + IntToHex(Ord(URL[i]), 2);
    end;
  end;
end;

procedure THTTPSender.ParseURL(const lpszUrl: string; var Host, Resource: string);
var
  lpszScheme: array [0 .. INTERNET_MAX_SCHEME_LENGTH - 1] of Char;
  lpszHostName: array [0 .. INTERNET_MAX_HOST_NAME_LENGTH - 1] of Char;
  lpszUserName: array [0 .. INTERNET_MAX_USER_NAME_LENGTH - 1] of Char;
  lpszPassword: array [0 .. INTERNET_MAX_PASSWORD_LENGTH - 1] of Char;
  lpszUrlPath: array [0 .. INTERNET_MAX_PATH_LENGTH - 1] of Char;
  lpszExtraInfo: array [0 .. 1024 - 1] of Char;
  lpUrlComponents: TURLComponents;
begin
  ZeroMemory(@lpszScheme, SizeOf(lpszScheme));
  ZeroMemory(@lpszHostName, SizeOf(lpszHostName));
  ZeroMemory(@lpszUserName, SizeOf(lpszUserName));
  ZeroMemory(@lpszPassword, SizeOf(lpszPassword));
  ZeroMemory(@lpszUrlPath, SizeOf(lpszUrlPath));
  ZeroMemory(@lpszExtraInfo, SizeOf(lpszExtraInfo));
  ZeroMemory(@lpUrlComponents, SizeOf(TURLComponents));

  lpUrlComponents.dwStructSize := SizeOf(TURLComponents);
  lpUrlComponents.lpszScheme := lpszScheme;
  lpUrlComponents.dwSchemeLength := SizeOf(lpszScheme);
  lpUrlComponents.lpszHostName := lpszHostName;
  lpUrlComponents.dwHostNameLength := SizeOf(lpszHostName);
  lpUrlComponents.lpszUserName := lpszUserName;
  lpUrlComponents.dwUserNameLength := SizeOf(lpszUserName);
  lpUrlComponents.lpszPassword := lpszPassword;
  lpUrlComponents.dwPasswordLength := SizeOf(lpszPassword);
  lpUrlComponents.lpszUrlPath := lpszUrlPath;
  lpUrlComponents.dwUrlPathLength := SizeOf(lpszUrlPath);
  lpUrlComponents.lpszExtraInfo := lpszExtraInfo;
  lpUrlComponents.dwExtraInfoLength := SizeOf(lpszExtraInfo);

  InternetCrackUrl(PChar(lpszUrl), Length(lpszUrl), ICU_DECODE or ICU_ESCAPE, lpUrlComponents);

  Host := lpszHostName;
  Resource := lpszUrlPath;
end;

function THTTPSender.GetQueryInfo(hRequest: Pointer; Flag: Integer): String;
var
  code: String;
  size, Index: Cardinal;
begin
  Result := '';
  SetLength(code, 8);
  size := Length(code);
  index := 0;
  if HttpQueryInfo(hRequest, Flag, PChar(code), size, index) then Result := code
  else if GetLastError = ERROR_INSUFFICIENT_BUFFER then begin
    SetLength(code, size);
    size := Length(code);
    if HttpQueryInfo(hRequest, Flag, PChar(code), size, index) then Result := code;
  end;
end;

function THTTPSender.GetWinInetError(ErrorCode: Cardinal): string;
const
  winetdll = 'wininet.dll';
var
  Len: Integer;
  Buffer: PChar;
begin
  Len := FormatMessage(FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ALLOCATE_BUFFER or
    FORMAT_MESSAGE_IGNORE_INSERTS or FORMAT_MESSAGE_ARGUMENT_ARRAY, Pointer(GetModuleHandle(winetdll)), ErrorCode, 0,
    @Buffer, SizeOf(Buffer), nil);
  try
    while (Len > 0) and {$IFDEF UNICODE}(CharInSet(Buffer[Len - 1], [#0 .. #32, '.']))
{$ELSE}(Buffer[Len - 1] in [#0 .. #32, '.']) {$ENDIF} do Dec(Len);
    SetString(Result, Buffer, Len);
  finally
    LocalFree(HLOCAL(Buffer));
  end;
end;

procedure THTTPSender.URLExecute(HTTPS: Boolean; const ServerName, Resource: string; Method: String;
  const PostData: AnsiString = '');
const
  C_PROXYCONNECTION = 'Proxy-Connection: Keep-Alive'#10#13;
var
  hInet: HINTERNET;
  hConnect: HINTERNET;
  hRequest: HINTERNET;
  ErrorCode: Integer;
  lpvBuffer: PansiChar;
  lpdwBufferLength: DWORD;
  dwBytesRead: DWORD;
  lpdwNumberOfBytesAvailable: DWORD;
  ConnectPort: INTERNET_PORT;
  OpenTypeFlags: DWORD;
  OpenRequestFlags: DWORD;
  PostDataPointer: Pointer;
  PostDataLength: DWORD;
  Buffer: array [0 .. 1024] of AnsiChar;

  function ExtractHeaders: Boolean;
  var
    lpdwReserved: DWORD;

  begin
    Result := true;
    with RResponse do begin
      lpdwBufferLength := SizeOf(StatusCode);
      lpdwReserved := 0;
      Result := Result and HttpQueryInfo(hRequest, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @StatusCode,
        lpdwBufferLength, lpdwReserved);
      SetLength(StatusText, 1024);
      lpdwBufferLength := Length(StatusText);
      Result := Result and HttpQueryInfo(hRequest, HTTP_QUERY_STATUS_TEXT, @StatusText[1], lpdwBufferLength,
        lpdwReserved);
      lpdwBufferLength := SizeOf(ContentLength);
      if not HttpQueryInfo(hRequest, HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER, @ContentLength,
        lpdwBufferLength, lpdwReserved) then ContentLength := 0;
      SetLength(ContentEncoding, 1024);
      lpdwBufferLength := Length(ContentEncoding);
      if not HttpQueryInfo(hRequest, HTTP_QUERY_CONTENT_ENCODING, @ContentEncoding[1], lpdwBufferLength, lpdwReserved)
      then ContentEncoding := '';
      SetLength(Location, 1024);
      lpdwBufferLength := Length(Location);
      if not HttpQueryInfo(hRequest, HTTP_QUERY_LOCATION, @Location[1], lpdwBufferLength, lpdwReserved) then
          Location := '';
      SetLength(Expires, 1024);
      lpdwBufferLength := Length(Expires);
      if not HttpQueryInfo(hRequest, HTTP_QUERY_EXPIRES, @Expires[1], lpdwBufferLength, lpdwReserved) then
          Expires := '';
    end;
  end;

begin
  with RResponse do begin
    StatusCode := 0;
    StatusText := '';
    RawHeaders := '';
    ContentLength := 0;
    Expires := '';
  end;
  RResponseText := '';

  if RProxy <> '' then OpenTypeFlags := INTERNET_OPEN_TYPE_PROXY
  else OpenTypeFlags := INTERNET_OPEN_TYPE_PRECONFIG;

  hInet := InternetOpen(PChar(Headers.UserAgent), OpenTypeFlags, PChar(RProxy), PChar(RProxyBypass), 0);

  InternetSetOption(hInet, INTERNET_OPTION_CONNECT_TIMEOUT, @RConnectTimeout, SizeOf(RConnectTimeout));
  InternetSetOption(hInet, INTERNET_OPTION_RECEIVE_TIMEOUT, @RReadTimeout, SizeOf(RReadTimeout));
  InternetSetOption(hInet, INTERNET_OPTION_SEND_TIMEOUT, @RSendTimeout, SizeOf(RSendTimeout));

  if hInet = nil then begin
    ErrorCode := GetLastError;
    raise Exception.Create(Format('InternetOpen Error %d Description %s', [ErrorCode, GetWinInetError(ErrorCode)]));
  end;

  try
    if HTTPS then ConnectPort := INTERNET_DEFAULT_HTTPS_PORT
    else ConnectPort := INTERNET_DEFAULT_HTTP_PORT;
    hConnect := InternetConnect(hInet, PChar(ServerName), ConnectPort, PChar(BasicAuth.Username),
      PChar(BasicAuth.Password), INTERNET_SERVICE_HTTP, 0, 0);
    if hConnect = nil then begin
      ErrorCode := GetLastError;
      raise Exception.Create(Format('InternetConnect Error %d Description %s',
        [ErrorCode, GetWinInetError(ErrorCode)]));
    end;

    try
      if HTTPS then OpenRequestFlags := INTERNET_FLAG_SECURE
      else OpenRequestFlags := INTERNET_FLAG_RELOAD;
      if not RAutoRedirects then OpenRequestFlags := OpenRequestFlags or INTERNET_FLAG_NO_AUTO_REDIRECT;
      if not RAllowCookies then OpenRequestFlags := OpenRequestFlags or INTERNET_FLAG_NO_COOKIES;
      hRequest := HttpOpenRequest(hConnect, PChar(Method), PChar(Resource), HTTP_VERSION, PChar(Headers.Refferer), nil,
        OpenRequestFlags, 0);
      if hRequest = nil then begin
        ErrorCode := GetLastError;
        raise Exception.Create(Format('HttpOpenRequest Error %d Description %s',
          [ErrorCode, GetWinInetError(ErrorCode)]));
      end;

      if RProxy <> '' then
          HttpAddRequestHeaders(hRequest, C_PROXYCONNECTION, Length(C_PROXYCONNECTION), HTTP_ADDREQ_FLAG_ADD);

      try
        if Method = 'POST' then begin
          PostDataPointer := @PostData[1];
          PostDataLength := Length(PostData);
        end else begin
          PostDataPointer := nil;
          PostDataLength := 0;
        end;

        if not HTTPSendRequest(hRequest, GetHeaders, 0, PostDataPointer, PostDataLength) then begin
          ErrorCode := GetLastError;
          raise Exception.Create(Format('HttpSendRequest Error %d Description %s',
            [ErrorCode, GetWinInetError(ErrorCode)]));
        end;

        RResponse.RawHeaders := GetQueryInfo(hRequest, HTTP_QUERY_RAW_HEADERS_CRLF);
        if not RAllowCookies then ProcessCookies(RResponse.RawHeaders);

        if not ExtractHeaders then begin
          ErrorCode := GetLastError;
          raise Exception.Create(Format('HttpQueryInfo Error %d Description %s',
            [ErrorCode, GetWinInetError(ErrorCode)]));
        end;

        if RResponse.StatusCode = 200 then begin
          repeat
            if not InternetReadFile(hRequest, @Buffer, SizeOf(Buffer), dwBytesRead) then begin
              ErrorCode := GetLastError;
              raise Exception.Create(Format('InternetReadFile Error %d Description %s',
                [ErrorCode, GetWinInetError(ErrorCode)]));
            end;
            Buffer[dwBytesRead] := #0;
            lpvBuffer := PansiChar(@Buffer);
            RResponseText := RResponseText + AnsiString(lpvBuffer);
          until dwBytesRead = 0;
        end;
      finally
        InternetCloseHandle(hRequest);
      end;
    finally
      InternetCloseHandle(hConnect);
    end;
  finally
    InternetCloseHandle(hInet);
  end;
end;

constructor THTTPSender.Create; // (AOwner:TComponent);
begin
  // inherited;
  Cookies := THTTPCookieCollection.Create;
  RReadTimeout := 60000;
  RConnectTimeout := 60000;
  RSendTimeout := 60000;
  RProxy := '';
  RProxyBypass := '';
  with Headers do begin
    ContentType := 'application/x-www-form-urlencoded';
    Accept := '';
    AcceptLanguage := '';
    AcceptEncoding := '';
    ExtraHeaders := '';
    Refferer := '';
    UserAgent := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0)';
  end;
end;

procedure THTTPSender.Free;
begin
  Cookies.Free;
  Destroy;
end;

function THTTPSender.Get(URL: String): AnsiString;
var
  Host, Resource: string;
begin
  Result := '';
  ParseURL(URL, Host, Resource);
  if Pos('http', URL) = 1 then URLExecute((Pos('https', URL) = 1), Host, Resource, 'GET')
  else raise Exception.Create(Format('Unknown Protocol %s', [URL]));
  Result := RResponseText;
end;

function THTTPSender.GetHeaders: PWideChar;
begin
  Result := '';
  with Headers do begin
    if ContentType <> '' then Result := PChar(Format('%sContent-type: %s'#10#13, [Result, ContentType]));
    if AcceptLanguage <> '' then Result := PChar(Format('%sAccept-Language: %s'#10#13, [Result, AcceptLanguage]));
    if AcceptEncoding <> '' then Result := PChar(Format('%sAccept-Encoding: %s'#10#13, [Result, AcceptEncoding]));
    if Accept <> '' then Result := PChar(Format('%sAccept: %s'#10#13, [Result, Accept]));
    if ExtraHeaders <> '' then Result := PChar(Format('%s'#10#13'%s', [Result, ExtraHeaders]));
  end;
end;

function THTTPSender.Post(URL: String; PostData: AnsiString): AnsiString;
var
  Host, Resource: string;
begin
  Result := '';
  ParseURL(URL, Host, Resource);
  if Pos('http', URL) = 1 then URLExecute((Pos('https', URL) = 1), Host, Resource, 'POST', PostData)
  else raise Exception.Create(Format('Unknown Protocol %s', [URL]));
  Result := RResponseText;
end;

function Pars(const source, left, right: string): string;
var
  r, l: Integer;
begin
  l := Pos(left, source);
  r := Pos(right, (Copy(source, l + Length(left), Length(source) - l - Length(left)))) + l;
  if l = r then exit('');
  Result := Copy(source, l + Length(left), r - l - 1);
end;

procedure THTTPSender.ProcessCookies(Data: string);
const
  SetCookie = 'Set-Cookie:';

  function GetCookie(s: string): THTTPCookie;
  var
    t: string;
  begin
    with Result do begin
      FName := Copy(s, 1, Pos('=', s) - 1);
      FValue := Pars(s, '=', ';');
      FPath := Pars(s, 'path=', ';');
      FExpires := Pars(s, 'expires=', ';');
      FDomain := Pars(s, 'domain=', ';');
    end;
  end;

begin
  while Pos(SetCookie, Data) > 0 do begin
    Cookies.Add(GetCookie(Pars(Data, SetCookie, #10#13)), true);
    Delete(Data, Pos(SetCookie, Data), Length(SetCookie));
  end;
end;

function THTTPSender.Put(URL: String): AnsiString;
var
  Host, Resource: string;
begin
  Result := '';
  ParseURL(URL, Host, Resource);
  if Pos('http', URL) = 1 then URLExecute((Pos('https', URL) = 1), Host, Resource, 'PUT')
  else raise Exception.Create(Format('Unknown Protocol %s', [URL]));
  Result := RResponseText;
end;

{ THTTPCookieCollection }

function THTTPCookieCollection.Add(Cookie: THTTPCookie; ReplaceIfExists: Boolean): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(Cookies) do
    if (Cookies[i].FDomain = Cookie.FDomain) and (Cookies[i].FName = Cookie.FName) then begin
      Cookies[i] := Cookie;
      exit(i);
    end;
  SetLength(Cookies, Length(Cookies) + 1);
  Cookies[high(Cookies)] := Cookie;
end;

function THTTPCookieCollection.AllItems: string;
begin
  Result := '';
end;

procedure THTTPCookieCollection.Clear;
begin
  SetLength(Cookies, 0);
end;

function THTTPCookieCollection.Count: Integer;
begin
  Result := Length(Cookies);
end;

function THTTPCookieCollection.Delete(CookieName: string): Boolean;
begin

end;

function THTTPCookieCollection.Delete(Index: Integer): Boolean;
begin

end;

function THTTPCookieCollection.GetCookie(Index: Integer): THTTPCookie;
begin
  Result := Cookies[Index];
end;

procedure THTTPCookieCollection.PutCookie(Index: Integer; Cookie: THTTPCookie);
begin
  Cookies[Index] := Cookie;
end;

function THTTPCookieCollection.Search(CookieName: string): Integer;
begin

end;

end.
