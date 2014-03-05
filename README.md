# Paris

Paris is a MVC framework 

## 10 seconds

> You need to have [git](http://git-scm.com/) and [erlang](http://erlang.org) installed.

1. First install [rebar](https://github.com/rebar/rebar) on you `PATH`
2. Install [paris-templates](https://github.com/glejeune/paris-templates) :

        \curl https://raw.github.com/glejeune/paris-templates/master/install.sh | sh

3. Create a new paris' project using rebar :

        rebar create template=paris name=my_project

4. _Et voilà_ 

        cd my_project
        make
        ./my_project start

  Visit `http://localhost:8080` with your favorite browser.

## Statup script 

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

## MVC

### Model

### View

### Controller

