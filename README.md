# Paris

Paris is a MVC framework 

> **This is a alpha version**. I plan evolutions that will involved major modifications in the API. So please **use this software for tests only**. The more you test, the more you report issues, the more we have chance to have something that's fit to your needs.
>
> Thanks in advance for your help.

## 10 seconds

> You need to have [git](http://git-scm.com/) and [erlang](http://erlang.org) installed.

1. First install [rebar](https://github.com/rebar/rebar) on you `PATH`
2. Install [paris-templates](https://github.com/emedia-project/paris-templates) :

        \curl https://raw.github.com/emedia-project/paris-templates/master/install.sh | sh

3. Create a new paris' project using rebar :

        rebar create template=paris name=my_project

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
├── config
│   └── my_project.config
├── my_project
├── README.md
├── Makefile
├── rebar
└── rebar.config
```

* Controllers are in `apps/my_project/src/controller`.
* Views are in `apps/my_project/src/view`.
* Models are in `apps/my_project/src/model` (There is no ORM in **paris**, yet).
* Static files are placed in `apps/my_project/priv/static`.
* `apps/my_project/src/my_project.app.src` is the OTP application file.
* `apps/my_project/src/my_project.erl` is the start file. If you need to use other applications, start them from this file.
* `config/my_project.config` is the configuration file. It is a standard Erlang [config](http://www.erlang.org/doc/man/config.html) file.
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

There is no ORM in **paris**, yet. If you need one, you could help us and propose something ;)

### View

We use the [ErlyDTL](https://github.com/evanmiller/erlydtl) template engine. Read [this documentation](https://docs.djangoproject.com/en/1.6/topics/templates/). 

You **must** place the view templates in the `view` directory. If you want to change this, edit the `rebar.config` file and update the parameter the `doc_root`  of `erlydtl_opts`.

The templates have a `.html` extensions. You can also change this by modifying the `source_ext` parameters of `erlydtl_opts` in `rebar.config`.

### Controller

You can create two types of controllers.

#### REST Controller

#### Websocket Controller

## Documentation

Modules:

| Module | Description |
| -- | -- |
| [paris_response](_doc/paris_response.md) | Response helpers |
