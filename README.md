# Paris

Paris is a MVC framework 

## 10 seconds

> You need to have [git](http://git-scm.com/) and [erlang](http://erlang.org) installed on your machine.

1. First install [rebar](https://github.com/rebar/rebar) on you `PATH`
2. Install [paris-templates](https://github.com/glejeune/paris-templates) :

        \curl https://raw.github.com/glejeune/paris-templates/master/install.sh | sh

3. Create a new paris' project using rebar :

        rebar create template=paris name=my_project

4. _Et voilà_ 

        cd my_project
        make
        ./start.sh

  Visit `http://localhost:8080` with your favorite browser.
