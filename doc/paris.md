

# Module paris #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-mail_conf">mail_conf()</a> ###



<pre><code>
mail_conf() = [<a href="#type-mail_option">mail_option()</a>]
</code></pre>





### <a name="type-mail_option">mail_option()</a> ###



<pre><code>
mail_option() = {relay, <a href="inet.md#type-hostname">inet:hostname()</a>} | {port, <a href="inet.md#type-port_number">inet:port_number()</a>} | {ssl, boolean()} | {username, string()} | {password, string()}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#app-0">app/0</a></td><td>
Return the application name.</td></tr><tr><td valign="top"><a href="#del-1">del/1</a></td><td>
Delete a value in the global cache.</td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>
Get a value from the global cache.</td></tr><tr><td valign="top"><a href="#mailconf-0">mailconf/0</a></td><td>
Return the mail configuration.</td></tr><tr><td valign="top"><a href="#plugins-0">plugins/0</a></td><td>
Return the list of plugins used by the application.</td></tr><tr><td valign="top"><a href="#port-0">port/0</a></td><td>
Return the application port.</td></tr><tr><td valign="top"><a href="#priv_dir-0">priv_dir/0</a></td><td>
Return the private directory of the application.</td></tr><tr><td valign="top"><a href="#set-2">set/2</a></td><td>
Set a value in the global cache.</td></tr><tr><td valign="top"><a href="#static-1">static/1</a></td><td>
Create a complete path to a static file.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="app-0"></a>

### app/0 ###


<pre><code>
app() -&gt; atom() | error
</code></pre>
<br />


Return the application name
<a name="del-1"></a>

### del/1 ###


<pre><code>
del(Name::any()) -&gt; any() | undefined
</code></pre>
<br />


Delete a value in the global cache
<a name="get-1"></a>

### get/1 ###


<pre><code>
get(Name::any()) -&gt; any() | undefined
</code></pre>
<br />


Get a value from the global cache
<a name="mailconf-0"></a>

### mailconf/0 ###


<pre><code>
mailconf() -&gt; <a href="#type-mail_conf">mail_conf()</a>
</code></pre>
<br />


Return the mail configuration
<a name="plugins-0"></a>

### plugins/0 ###


<pre><code>
plugins() -&gt; [atom()] | error
</code></pre>
<br />


Return the list of plugins used by the application
<a name="port-0"></a>

### port/0 ###


<pre><code>
port() -&gt; <a href="inet.md#type-port_number">inet:port_number()</a> | error
</code></pre>
<br />


Return the application port
<a name="priv_dir-0"></a>

### priv_dir/0 ###


<pre><code>
priv_dir() -&gt; <a href="file.md#type-filename">file:filename()</a> | error
</code></pre>
<br />


Return the private directory of the application
<a name="set-2"></a>

### set/2 ###


<pre><code>
set(Name::any(), Value::any()) -&gt; ok | error
</code></pre>
<br />


Set a value in the global cache
<a name="static-1"></a>

### static/1 ###


<pre><code>
static(File::string()) -&gt; <a href="file.md#type-filename">file:filename()</a> | error
</code></pre>
<br />


Create a complete path to a static file
