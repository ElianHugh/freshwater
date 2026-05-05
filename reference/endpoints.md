# Reverse Routing

Access generated endpoint URL helpers.

## Usage

``` r
endpoints(route = NULL, api = NULL, refresh = FALSE)
```

## Arguments

- route:

  the route group to retrieve endpoints from; typically defined either
  via the file name, routeName or route in plumber2. If NULL, will
  return all endpoints for all routes.

- api:

  a
  [plumber2::plumber2](https://plumber2.posit.co/reference/plumber2-package.html)
  api object. If NULL, context is used to find the api.

- refresh:

  force refresh the registered routes. Useful if you have added routes
  after calling `endpoints()`.

## Value

If `route` is `NULL`, returns a list of route groups and their
endpoints. Otherwise returns a list of a route's endpoint accessors.

## Details

Alias rules:

- "/" endpoints become "index"

- GET endpoints are accessed directly, like index()

- non-GET endpoints require an accessor, like index\$delete()

- path parameters are removed from the alias and used to disambiguate
  overloaded helpers via named function args

- Reserved argument: `.query` argument constructs a query from a named
  list

- Reserved argument: `.anchor` constructs an anchor from a character
  scalar

For example:

- `GET /` -\> `index()`

- `POST /` -\> `index$post()`

- `GET /my/filter` -\> `my_filter()`

- `GET /users/:id` -\> `users(id = 1, .query = list(page = 2))`

- `GET /users/:id` -\> `users(id = 1, .anchor = "details")`

- `GET /users/:id` -\>
  `users(id = 1, .query = list(page = 2), .anchor = "details")`

- `DELETE /users/:id` -\> `users$delete(id = 1)`

- `DELETE /users/:name` -\> `users$delete(name = "Jim")`

## Examples

``` r
#* @plumber
function(api) {
   api |>
     api_freshwater()
}
#> function (api) 
#> {
#>     api_freshwater(api)
#> }
#> <environment: 0x55713fd5a3f8>

#* @get /
#* @serializer html
#* @routeName user
function() {
     endpoints("user")$index()
}
#> function () 
#> {
#>     endpoints("user")$index()
#> }
#> <environment: 0x55713fd5a3f8>
```
