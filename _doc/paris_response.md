# paris_response

## `redirect(URI)`

* `URI` -> `string()`

Redirect (302) to the given `URI`

## `render_view(View, Variables, Headers)`

* `View` -> `atom()`
* `Variables` -> `[vars()]`
  * `vars()` -> `{term(), term()}`
* `Headers` -> `[header()]`
  * `header()` -> `{term(), term()}`

Render the `View` using `Variables`. Add `Headers` to the response.

## `render_view(View, Variables)`

Equivalent to `render_view(View, Variables, [])`

## `render_view(View)`

Equivalent to `render_view(View, [], [])`

## `render_text(Data, Headers)`

* `Data` -> `string()`
* `Headers` -> `[header()]`
  * `header()` -> `{term(), term()}`

Render text `Data`. Add `Headers` to the response.

## `render_text(Data)`

Equivalent to `render_text(Data, [])`
