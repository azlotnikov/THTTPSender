unit HTTPSender;

interface

// (c) Z.Razor | zt.am | 2012

uses Windows, WinInet, Classes, Sysutils;

type
  THTTPCookie = record
    FDomain: String;
    FName: String;
    FValue: String;
    FExpires: String;
    FPath: String;
    FHTTPOnly: boolean;
  end;

  THTTPCookieArray = array of THTTPCookie;

  THTTPCookieCollection = class(TPersistent)
  private
    Cookies: THTTPCookieArray;
    RCustomCookies: TStringList;
    function GetCookie(Index: Integer): THTTPCookie;
    procedure PutCookie(Index: Integer; Cookie: THTTPCookie);
  public
    property Items[Index: Integer]: THTTPCookie read GetCookie write PutCookie;
    function Add(Cookie: THTTPCookie; ReplaceIfExists: boolean): Integer;
    function DeleteCookie(Index: Integer): boolean;
    function Count: Integer;
    function GetCookies(Domain, Path: string): string;
    procedure Clear;
    constructor Create;
  published
    property CustomCookies: TStringList read RCustomCookies write RCustomCookies;

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

  THTTPHeaders = class(TPersistent)
  private
    RContentType: String;
    RAccept: String;
    RAcceptLanguage: String;
    RAcceptEncoding: String;
    RCustomHeaders: TStringList;
    RRefferer: string;
    RUserAgent: string;
  public
    constructor Create;
  published
    property ContentType: String read RContentType write RContentType;
    property Accept: String read RAccept write RAccept;
    property AcceptLanguage: String read RAcceptLanguage write RAcceptLanguage;
    property AcceptEncoding: String read RAcceptEncoding write RAcceptEncoding;
    property CustomHeaders: TStringList read RCustomHeaders write RCustomHeaders;
    property Refferer: String read RRefferer write RRefferer;
    property UserAgent: String read RUserAgent write RUserAgent;
  end;

  THTTPBasicAuth = class(TPersistent)
  private
    RUsername: string;
    RPassword: string;
  published
    property Username: string read RUsername write RUsername;
    property Password: string read RPassword write RPassword;
  end;

type
  TCookieAddEvent = procedure(Sender: TObject; Cookie: THTTPCookie) of object;
  TWorkBeginEvent = procedure(Sender: TObject; WorkCountMax: int64) of object;
  TWorkEvent = procedure(Sender: TObject; WorkCount: int64) of object;
  TWorkEndEvent = procedure(Sender: TObject) of object;

type
  THTTPSender = class(TComponent)
  private
    RResponse: THTTPResponse;
    RResponseText: AnsiString;
    RAllowCookies: boolean;
    RAutoRedirects: boolean;
    RConnectTimeout: Integer;
    RReadTimeout: Integer;
    RSendTimeout: Integer;
    RProxy: String;
    RProxyBypass: String;
    RUseIECookies: boolean;
    RHeaders: THTTPHeaders;
    RBasicAuth: THTTPBasicAuth;
    ROnCookieAdd: TCookieAddEvent;
    ROnWorkBegin: TWorkBeginEvent;
    ROnWork: TWorkEvent;
    ROnWorkEnd: TWorkEndEvent;
    RCookies: THTTPCookieCollection;
    function URLEncode(const URL: string): string;
    function GetWinInetError(ErrorCode: Cardinal): string;
    function GetQueryInfo(hRequest: Pointer; Flag: Integer): String;
    function GetHeaders: PWideChar;
    function GetAbout: string;
    procedure ProcessCookies(Data: string);
    procedure URLExecute(HTTPS: boolean; const ServerName, Resource, ExtraInfo: string; Method: String; Stream: TStream;
      const PostData: AnsiString = '');
    procedure ParseURL(const lpszUrl: string; var Host, Resource, ExtraInfo: string);
  public
    property Response: THTTPResponse read RResponse;
    property ResponseText: AnsiString read RResponseText;
    function Get(URL: String): string; overload;
    function Post(URL: String; PostData: AnsiString): string; overload;
    function Put(URL: String): string; overload;
    procedure Get(URL: String; Stream: TStream); overload;
    procedure Post(URL: String; PostData: AnsiString; Stream: TStream); overload;
    procedure Put(URL: String; Stream: TStream); overload;
    procedure Free;
    constructor Create(AOwner: TComponent); override;
  published
    property Cookies: THTTPCookieCollection read RCookies write RCookies;
    property Proxy: string read RProxy write RProxy;
    property ProxyBypass: string read RProxyBypass write RProxyBypass;
    property AllowCookies: boolean read RAllowCookies write RAllowCookies default true;
    property AutoRedirects: boolean read RAutoRedirects write RAutoRedirects default true;
    property ConnectTimeout: Integer read RConnectTimeout write RConnectTimeout default 60000;
    property ReadTimeout: Integer read RReadTimeout write RReadTimeout default 0;
    property SendTimeout: Integer read RSendTimeout write RSendTimeout default 0;
    property UseIECookies: boolean read RUseIECookies write RUseIECookies default true;
    property Headers: THTTPHeaders read RHeaders write RHeaders;
    property BasicAuth: THTTPBasicAuth read RBasicAuth write RBasicAuth;
    property OnCookieAdd: TCookieAddEvent read ROnCookieAdd write ROnCookieAdd;
    property OnWorkBegin: TWorkBeginEvent read ROnWorkBegin write ROnWorkBegin;
    property OnWork: TWorkEvent read ROnWork write ROnWork;
    property OnWorkEnd: TWorkEndEvent read ROnWorkEnd write ROnWorkEnd;
    property About: String read GetAbout;
  end;

procedure Register;

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

procedure THTTPSender.ParseURL(const lpszUrl: string; var Host, Resource, ExtraInfo: string);
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
  ExtraInfo := lpszExtraInfo;
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

procedure THTTPSender.URLExecute(HTTPS: boolean; const ServerName, Resource, ExtraInfo: string; Method: String;
  Stream: TStream; const PostData: AnsiString = '');
const
  C_PROXYCONNECTION = 'Proxy-Connection: Keep-Alive'#10#13;
  BuffSize = 1024;
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
  lpOtherHeaders: String;
  Buffer: Pointer;

  function ExtractHeaders: boolean;
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
  lpOtherHeaders := '';

  if RProxy <> '' then OpenTypeFlags := INTERNET_OPEN_TYPE_PROXY
  else OpenTypeFlags := INTERNET_OPEN_TYPE_PRECONFIG;

  hInet := InternetOpen(PChar(RHeaders.RUserAgent), OpenTypeFlags, PChar(RProxy), PChar(RProxyBypass), 0);

  if RConnectTimeout > 0 then
      InternetSetOption(hInet, INTERNET_OPTION_CONNECT_TIMEOUT, @RConnectTimeout, SizeOf(RConnectTimeout));
  if RReadTimeout > 0 then
      InternetSetOption(hInet, INTERNET_OPTION_RECEIVE_TIMEOUT, @RReadTimeout, SizeOf(RReadTimeout));
  if RSendTimeout > 0 then InternetSetOption(hInet, INTERNET_OPTION_SEND_TIMEOUT, @RSendTimeout, SizeOf(RSendTimeout));

  if hInet = nil then begin
    ErrorCode := GetLastError;
    raise Exception.Create(Format('InternetOpen Error %d Description %s', [ErrorCode, GetWinInetError(ErrorCode)]));
  end;

  try
    if HTTPS then ConnectPort := INTERNET_DEFAULT_HTTPS_PORT
    else ConnectPort := INTERNET_DEFAULT_HTTP_PORT;
    hConnect := InternetConnect(hInet, PChar(ServerName), ConnectPort, PChar(RBasicAuth.RUsername),
      PChar(RBasicAuth.RPassword), INTERNET_SERVICE_HTTP, 0, 0);
    if hConnect = nil then begin
      ErrorCode := GetLastError;
      raise Exception.Create(Format('InternetConnect Error %d Description %s',
        [ErrorCode, GetWinInetError(ErrorCode)]));
    end;

    try
      if HTTPS then OpenRequestFlags := INTERNET_FLAG_SECURE
      else OpenRequestFlags := INTERNET_FLAG_RELOAD;
      if not RAutoRedirects then OpenRequestFlags := OpenRequestFlags or INTERNET_FLAG_NO_AUTO_REDIRECT;
      if (not RUseIECookies) or (not RAllowCookies) then
          OpenRequestFlags := OpenRequestFlags or INTERNET_FLAG_NO_COOKIES;

      hRequest := HttpOpenRequest(hConnect, PChar(Method), PChar(Resource + ExtraInfo), HTTP_VERSION,
        PChar(RHeaders.RRefferer), nil, OpenRequestFlags, 0);
      if hRequest = nil then begin
        ErrorCode := GetLastError;
        raise Exception.Create(Format('HttpOpenRequest Error %d Description %s',
          [ErrorCode, GetWinInetError(ErrorCode)]));
      end;
      if RAllowCookies and (not RUseIECookies) then
          lpOtherHeaders := RCookies.GetCookies('.' + ServerName, Resource) + #10#13;
      if RProxy <> '' then lpOtherHeaders := lpOtherHeaders + C_PROXYCONNECTION + #10#13;

      try
        if Method = 'POST' then begin
          PostDataPointer := @PostData[1];
          PostDataLength := Length(PostData);
        end else begin
          PostDataPointer := nil;
          PostDataLength := 0;
        end;

        if not HTTPSendRequest(hRequest, PWideChar(GetHeaders + lpOtherHeaders), 0, PostDataPointer, PostDataLength)
        then begin
          ErrorCode := GetLastError;
          raise Exception.Create(Format('HttpSendRequest Error %d Description %s',
            [ErrorCode, GetWinInetError(ErrorCode)]));
        end;

        RResponse.RawHeaders := GetQueryInfo(hRequest, HTTP_QUERY_RAW_HEADERS_CRLF);
        if RAllowCookies and (not RUseIECookies) then ProcessCookies(RResponse.RawHeaders);

        if not ExtractHeaders then begin
          ErrorCode := GetLastError;
          raise Exception.Create(Format('HttpQueryInfo Error %d Description %s',
            [ErrorCode, GetWinInetError(ErrorCode)]));
        end;
        if Assigned(ROnWorkBegin) then ROnWorkBegin(self, Response.ContentLength);
        if RResponse.StatusCode = 200 then
          try
            Stream.Seek(0, 0);
            GetMem(Buffer, BuffSize);
            repeat
              if not InternetReadFile(hRequest, Buffer, BuffSize, dwBytesRead) then begin
                ErrorCode := GetLastError;
                raise Exception.Create(Format('InternetReadFile Error %d Description %s',
                  [ErrorCode, GetWinInetError(ErrorCode)]));
              end;
              if dwBytesRead > 0 then Stream.WriteBuffer(Buffer^, dwBytesRead);
              if Assigned(ROnWork) then ROnWork(self, Stream.size);
            until dwBytesRead = 0;
          finally
            FreeMem(Buffer);
          end;
        if Assigned(ROnWorkEnd) then ROnWorkEnd(self);
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

constructor THTTPSender.Create(AOwner: TComponent);
begin
  inherited;
  RCookies := THTTPCookieCollection.Create;
  RHeaders := THTTPHeaders.Create;
  BasicAuth := THTTPBasicAuth.Create;
  RReadTimeout := 0;
  RConnectTimeout := 60000;
  RSendTimeout := 0;
  RProxy := '';
  RProxyBypass := '';
  RUseIECookies := true;
  RAllowCookies := true;
  RAutoRedirects := true;
  with RHeaders do begin
    RContentType := 'application/x-www-form-urlencoded';
    RAccept := '';
    RAcceptLanguage := '';
    RAcceptEncoding := '';
    RCustomHeaders.Text := '';
    RRefferer := '';
    RUserAgent := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0)';
  end;
end;

procedure THTTPSender.Free;
begin
  RCookies.Free;
  Destroy;
end;

procedure THTTPSender.Get(URL: String; Stream: TStream);
var
  Host, Resource, ExtraInfo: string;
begin
  // URL := URLEncode(URL);
  RResponseText := '';
  ParseURL(URL, Host, Resource, ExtraInfo);
  if Pos('http', URL) = 1 then URLExecute((Pos('https', URL) = 1), Host, Resource, ExtraInfo, 'GET', Stream)
  else raise Exception.Create(Format('Unknown Protocol %s', [URL]));
end;

function THTTPSender.GetAbout: string;
begin
  Result := 'Z.Razor | zt.am | 05.07.12';
end;

function THTTPSender.Get(URL: String): string;
var
  StringStream: TStringStream;
begin
  Result := '';
  StringStream := TStringStream.Create('', TEncoding.ASCII);
  try
    Get(URL, StringStream);
    if StringStream.size > 0 then begin
      StringStream.Seek(0, 0);
      Result := StringStream.ReadString(StringStream.size);
      RResponseText := Result;
    end;
  finally
    StringStream.Free;
  end;
end;

function THTTPSender.GetHeaders: PWideChar;
begin
  Result := '';
  with RHeaders do begin
    if RContentType <> '' then Result := PChar(Format('%sContent-type: %s'#10#13, [Result, RContentType]));
    if RAcceptLanguage <> '' then Result := PChar(Format('%sAccept-Language: %s'#10#13, [Result, RAcceptLanguage]));
    if RAcceptEncoding <> '' then Result := PChar(Format('%sAccept-Encoding: %s'#10#13, [Result, RAcceptEncoding]));
    if RAccept <> '' then Result := PChar(Format('%sAccept: %s'#10#13, [Result, RAccept]));
    if RCustomHeaders.Text <> '' then Result := PChar(Format('%s'#10#13'%s'#10#13, [Result, RCustomHeaders.Text]));
  end;
end;

function THTTPSender.Post(URL: String; PostData: AnsiString): string;
var
  StringStream: TStringStream;
begin
  Result := '';
  StringStream := TStringStream.Create('', TEncoding.ASCII);
  try
    Post(URL, PostData, StringStream);
    if StringStream.size > 0 then begin
      StringStream.Seek(0, 0);
      Result := StringStream.ReadString(StringStream.size);
      RResponseText := Result;
    end;
  finally
    StringStream.Free;
  end;
end;

procedure THTTPSender.Post(URL: String; PostData: AnsiString; Stream: TStream);
var
  Host, Resource, ExtraInfo: string;
begin
  RResponseText := '';
  // URL := URLEncode(URL);
  ParseURL(URL, Host, Resource, ExtraInfo);
  if Pos('http', URL) = 1 then URLExecute((Pos('https', URL) = 1), Host, Resource, ExtraInfo, 'POST', Stream, PostData)
  else raise Exception.Create(Format('Unknown Protocol %s', [URL]));
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
var
  NCookie: THTTPCookie;

  function GetCookie(S: string): THTTPCookie;
  var
    t: string;
  begin
    with Result do begin
      FName := Copy(S, 1, Pos('=', S) - 1);
      FValue := Pars(S, '=', ';');
      FPath := Pars(S, 'path=', ';');
      FExpires := Pars(S, 'expires=', ';');
      FDomain := Pars(S, 'domain=', ';');
      FHTTPOnly := (Pos('; HttpOnly', S) > 0);
    end;
  end;

begin
  while Pos(SetCookie, Data) > 0 do begin
    NCookie := GetCookie(Pars(Data, SetCookie, #10#13));
    RCookies.Add(NCookie, true);
    if Assigned(ROnCookieAdd) then ROnCookieAdd(self, NCookie);
    Delete(Data, Pos(SetCookie, Data), Length(SetCookie));
  end;
end;

function THTTPSender.Put(URL: String): string;
var
  StringStream: TStringStream;
begin
  Result := '';
  StringStream := TStringStream.Create('', TEncoding.ASCII);
  try
    Put(URL, StringStream);
    if StringStream.size > 0 then begin
      StringStream.Seek(0, 0);
      Result := StringStream.ReadString(StringStream.size);
      RResponseText := Result;
    end;
  finally
    StringStream.Free;
  end;
end;

procedure THTTPSender.Put(URL: String; Stream: TStream);
var
  Host, Resource, ExtraInfo: string;
begin
  RResponseText := '';
  // URL := URLEncode(URL);
  ParseURL(URL, Host, Resource, ExtraInfo);
  if Pos('http', URL) = 1 then URLExecute((Pos('https', URL) = 1), Host, Resource, ExtraInfo, 'PUT', Stream)
  else raise Exception.Create(Format('Unknown Protocol %s', [URL]));
end;

{ THTTPCookieCollection }

function THTTPCookieCollection.Add(Cookie: THTTPCookie; ReplaceIfExists: boolean): Integer;
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

procedure THTTPCookieCollection.Clear;
begin
  SetLength(Cookies, 0);
end;

function THTTPCookieCollection.Count: Integer;
begin
  Result := Length(Cookies);
end;

constructor THTTPCookieCollection.Create;
begin
  inherited;
  RCustomCookies := TStringList.Create;
end;

function THTTPCookieCollection.DeleteCookie(Index: Integer): boolean;
var
  i: Integer;
begin
  Result := false;
  if (index < 0) or (index > high(Cookies)) then exit;
  for i := Index to High(Cookies) - 1 do Cookies[i] := Cookies[i + 1];
  SetLength(Cookies, Length(Cookies) - 1);
  Result := true;
end;

function THTTPCookieCollection.GetCookie(Index: Integer): THTTPCookie;
begin
  Result := Cookies[Index];
end;

function THTTPCookieCollection.GetCookies(Domain, Path: string): string;
var
  i: Integer;
begin
  for i := Length(Path) downto 1 do
    if (Path[i] = '/') and (i > 1) then begin
      Path := Copy(Path, 1, i);
      break;
    end;
  Result := 'Cookies:';
  for i := 0 to High(Cookies) do
    if Cookies[i].FDomain = Domain then Result := Format('%s %s=%s;', [Result, Cookies[i].FName, Cookies[i].FValue]);
  Result := Result + ' ' + RCustomCookies.Text;
  if Result[Length(Result) - 1] = ';' then Delete(Result, Length(Result) - 1, 2);
  if Length(Result) = 7 then Result := '';
end;

procedure THTTPCookieCollection.PutCookie(Index: Integer; Cookie: THTTPCookie);
begin
  Cookies[Index] := Cookie;
end;

procedure Register;
begin
  RegisterComponents('Internet', [THTTPSender]);
end;

{ THTTPHeaders }

constructor THTTPHeaders.Create;
begin
  inherited;
  RCustomHeaders := TStringList.Create;
end;

end.
