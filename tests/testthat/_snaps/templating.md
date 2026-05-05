# error traces are maintained

    Error while rendering template:
    Caused by error in `dots_list()`:
    ! error

---

    Error while rendering template(s):
    * tmpl_parent()
       └─>tmpl()
          
    Caused by error in `dots_list()`:
    ! error

# map_tags errors on unsupported values

    Code
      map_tags(seq(3L), identity)
    Condition
      Error in `map_tags()`:
      ! Bad value returned from `.f`.
      i Expected either `NULL`, `character`, `shiny.tag` or `shiny.tag.list`.
      x Bad indices: 1, 2, 3
      x Bad classes: integer, integer, integer

