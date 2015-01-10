

# Module paris_response #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cookie-4">cookie/4</a></td><td></td></tr><tr><td valign="top"><a href="#delete_cookie-2">delete_cookie/2</a></td><td></td></tr><tr><td valign="top"><a href="#http_error-1">http_error/1</a></td><td>Equivalent to <a href="#http_error-2"><tt>http_error(Code, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#http_error-2">http_error/2</a></td><td></td></tr><tr><td valign="top"><a href="#redirect-1">redirect/1</a></td><td></td></tr><tr><td valign="top"><a href="#redirect-2">redirect/2</a></td><td></td></tr><tr><td valign="top"><a href="#render-2">render/2</a></td><td>
<pre>
  render(Type, [
    {template, my_template},
    {data, []},
    {headers, [...]},
    {status, 200}]).</pre></td></tr><tr><td valign="top"><a href="#ws_binary-3">ws_binary/3</a></td><td></td></tr><tr><td valign="top"><a href="#ws_close-3">ws_close/3</a></td><td></td></tr><tr><td valign="top"><a href="#ws_close-4">ws_close/4</a></td><td></td></tr><tr><td valign="top"><a href="#ws_hibernate-2">ws_hibernate/2</a></td><td></td></tr><tr><td valign="top"><a href="#ws_hibernate-3">ws_hibernate/3</a></td><td></td></tr><tr><td valign="top"><a href="#ws_json-3">ws_json/3</a></td><td></td></tr><tr><td valign="top"><a href="#ws_ok-2">ws_ok/2</a></td><td></td></tr><tr><td valign="top"><a href="#ws_ok-3">ws_ok/3</a></td><td></td></tr><tr><td valign="top"><a href="#ws_ping-3">ws_ping/3</a></td><td></td></tr><tr><td valign="top"><a href="#ws_pong-3">ws_pong/3</a></td><td></td></tr><tr><td valign="top"><a href="#ws_shutdown-2">ws_shutdown/2</a></td><td></td></tr><tr><td valign="top"><a href="#ws_terminate-0">ws_terminate/0</a></td><td></td></tr><tr><td valign="top"><a href="#ws_text-3">ws_text/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cookie-4"></a>

### cookie/4 ###

`cookie(Req, Name, Value, Opts) -> any()`


<a name="delete_cookie-2"></a>

### delete_cookie/2 ###

`delete_cookie(Req, Name) -> any()`


<a name="http_error-1"></a>

### http_error/1 ###

`http_error(Code) -> any()`

Equivalent to [`http_error(Code, [])`](#http_error-2).
<a name="http_error-2"></a>

### http_error/2 ###

`http_error(Code, Headers) -> any()`


<a name="redirect-1"></a>

### redirect/1 ###

`redirect(Path) -> any()`


<a name="redirect-2"></a>

### redirect/2 ###

`redirect(Format, Data) -> any()`


<a name="render-2"></a>

### render/2 ###

`render(Type, Options) -> any()`



```

  render(Type, [
    {template, my_template},
    {data, []},
    {headers, [...]},
    {status, 200}]).
```

<a name="ws_binary-3"></a>

### ws_binary/3 ###

`ws_binary(Req, State, Msg) -> any()`


<a name="ws_close-3"></a>

### ws_close/3 ###

`ws_close(Req, State, Msg) -> any()`


<a name="ws_close-4"></a>

### ws_close/4 ###

`ws_close(Req, State, Msg, Code) -> any()`


<a name="ws_hibernate-2"></a>

### ws_hibernate/2 ###

`ws_hibernate(Req, State) -> any()`


<a name="ws_hibernate-3"></a>

### ws_hibernate/3 ###

`ws_hibernate(Req, State, Timeout) -> any()`


<a name="ws_json-3"></a>

### ws_json/3 ###

`ws_json(Req, State, Msg) -> any()`


<a name="ws_ok-2"></a>

### ws_ok/2 ###

`ws_ok(Req, State) -> any()`


<a name="ws_ok-3"></a>

### ws_ok/3 ###

`ws_ok(Req, State, Timeout) -> any()`


<a name="ws_ping-3"></a>

### ws_ping/3 ###

`ws_ping(Req, State, Msg) -> any()`


<a name="ws_pong-3"></a>

### ws_pong/3 ###

`ws_pong(Req, State, Msg) -> any()`


<a name="ws_shutdown-2"></a>

### ws_shutdown/2 ###

`ws_shutdown(Req, X2) -> any()`


<a name="ws_terminate-0"></a>

### ws_terminate/0 ###

`ws_terminate() -> any()`


<a name="ws_text-3"></a>

### ws_text/3 ###

`ws_text(Req, State, Msg) -> any()`


