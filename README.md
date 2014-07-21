# Paris

Paris is a MVC framework 

> **This is a alpha version**. I plan evolutions that will involved major modifications in the API. So please **use this software for tests only**. The more you test, the more you report issues, the more we have chance to have something that's fit to your needs.
>
> Thanks in advance for your help.

## 10 seconds

> You need to have [git](http://git-scm.com/) and [erlang](http://erlang.org) installed.

1. First download [paris](https://github.com/emedia-project/paris.app/raw/master/paris), make it executable and add it on you `PATH`
2. Install templates by running 

        paris update

2. Create a new paris' project using :

        paris new my_project

4. _Et voilà_ 

        cd my_project
        make
        ./my_project start

  Visit `http://localhost:8080` with your favorite browser.

## Organisation

Here is the tree of generated files :

```
.
├── apps/
│   └── my_project/
│       ├── priv/
│       │   ├── ssl/
│       │   └── static/
│       │       └── img/
│       │           └── erlang.png
│       └── src/
│           ├── controller/
│           │   └── index.erl
│           ├── model/
│           ├── view/
│           │   └── index.html
│           ├── my_project.app.src
│           └── my_project.erl
├── config/
│   ├── my_project.config
│   └── vm.args
├── my_project
├── README.md
├── Makefile
├── rebar
└── rebar.config
```

* Controllers are in `apps/my_project/src/controller`.
* Views are in `apps/my_project/src/view`.
* Models are in `apps/my_project/src/model`.
* Static files are placed in `apps/my_project/priv/static`.
* `apps/my_project/src/my_project.app.src` is the OTP application file.
* `apps/my_project/src/my_project.erl` is the start file. If you need to use other applications, start them from this file.
* `config/my_project.config` is the configuration file. It is a standard Erlang [config](http://www.erlang.org/doc/man/config.html) file.
* `config/vm.args` is a file containing additionnal parameters to pass to the Erlang VM.
* `my_project` is the startup script. 

### Statup script 

The startup script (`my_project`) in the example above has the following options and commands :

**Options** :

* `-d` or `--development` : Start the server in development mode. In this mode, paris will detect every changes in your code and will recompile the sources on the fly.
* `-p` or `--production` : Start the server in production mode. 
* `-C` or `--compile` : Compile the code before starting the server.
* `-K` or `--clean` : Same has above, but do a `make clean` before.
* `-h` or `--help` : Display the help page

**Commands** :

* `start` : Start the server.
* `stop` : Stop the server.
* `console` : Start the server and give you access to the erlang shell
* `status` : Return the server status

### Configuration file

The configuration file is a standard Erlang [config](http://www.erlang.org/doc/man/config.html) file.

```
[
  { my_project, [
    {port, 8080},
    {ip, "0.0.0.0"},
    {max_conn, 100},
    {routes, [
    ]}
  ]},
  {lager, [
    {handlers, [
      {lager_console_backend, info},
      {lager_file_backend, [{file, "log/sample_error.log"}, {level, error}]},
      {lager_file_backend, [{file, "log/sample_console.log"}, {level, info}]}
    ]}
  ]}
].
```

You can use the following parameters :

* `port` : HTTP server port to use.
* `ip` : HTTP binding IP.
* `max_conn` : maximum connections for the HTTP server.
* `routes` : fixed routes (see [Controller][])

### Model

To create models, we have a separate ORM : [texas](https://github.com/emedia-project/texas).

### View

We use the [ErlyDTL](https://github.com/evanmiller/erlydtl) template engine. Read [this documentation](https://docs.djangoproject.com/en/1.6/topics/templates/). 

You **must** place the view templates in the `view` directory. If you want to change this, edit the `rebar.config` file and update the parameter the `doc_root`  of `erlydtl_opts`.

The templates have a `.html` extensions. You can also change this by modifying the `source_ext` parameters of `erlydtl_opts` in `rebar.config`.

### Controller

You can create two types of controllers.

#### REST Controller

#### Websocket Controller

## Plugins

Paris have the following pluging :

* [paris.rb](https://github.com/emedia-project/paris_rb) : This plugin allow you to create controllers with [Ruby](https://www.ruby-lang.org).
* [paris.ex](https://github.com/emedia-project/paris_ex) ; This plugin allow you to create controllers with [Elixir](http://elixir-lang.org/).

## Documentation

Modules:

* [paris_response](_doc/paris_response.md)

## Licences

paris is available for use under the following license, commonly known as the 3-clause (or "modified") BSD license:

Copyright (c) 2014 Gregoire Lejeune [gregoire.lejeune@free.fr]

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
