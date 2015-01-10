

# Module paris_mail #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#deliver-3">deliver/3</a></td><td>Equivalent to <a href="#deliver-4"><tt>deliver(Module, To, Data, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#deliver-4">deliver/4</a></td><td></td></tr><tr><td valign="top"><a href="#send-4">send/4</a></td><td>
Send an email.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="deliver-3"></a>

### deliver/3 ###

`deliver(Module, To, Data) -> any()`

Equivalent to [`deliver(Module, To, Data, [])`](#deliver-4).
<a name="deliver-4"></a>

### deliver/4 ###

`deliver(Module, To, Data, Options) -> any()`


<a name="send-4"></a>

### send/4 ###


<pre><code>
send(From::string() | binary(), To::string() | binary() | [string()] | [binary()], Subject::string() | binary(), Options::list()) -&gt; {ok, pid()} | {error, any()}
</code></pre>
<br />



Send an email


Options:

```

  {cc, string() | binary() | [string()] | [binary()]}
  {bcc, string() | binary() | [string()] | [binary()]}
  {template, atom(), [{atom(), any()}]}
  {body, string() | binary()}
  {attachment, [string()] | [binary()]}
  {callback, function()}
```


Example:

```

  send(
    "greg@example.com",
    ["bob@example.com", "john@example.com"]
    "This is a mail",
    [{cc, ["tania@example.com", "tom@example.com"]},
     {bcc, "jane@example.com"},
     {template, my_template, Data},
     {attachments, ["/home/greg/photo.png"]}
     {callback, Fun module:function/1}]).
```

