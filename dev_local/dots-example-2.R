dots_to_list <- \(envir) {
  assign("...", envir$...)
  list(...)
}

modify_dots <- \(updates = list(), envir = parent.frame()) {
  dots <- dots_to_list(envir)
  dots <- modifyList(dots, updates)
  do.call(\(...) {
    assign("...", environment()$..., envir = envir)
  }, dots)
}

foo <- \(...) {
  updates <- list(new = -999, a = NULL)
  modify_dots(updates)
  list(...)
}


### -----

foo <- \(...) {
  cat("-- START --------\n")
  print(list(...))

  cat("-- CHANGE 1: update 'a', add 'b' -------\n")
  s(a = 999, b = 2)
  print(list(...))

  cat("-- CHANGE 2: increment 'a', remove 'b' -------\n")
  s(a = g(a) + 1, b = NULL)
  print(list(...))
}

foo(a = 1)

.get_dots <- function(envir) {
  assign("...", envir$...)
  list(...)
}

g <- function(x) {
  x <- deparse(substitute(x))
  envir <- parent.frame()
  dots <- .get_dots(envir)
  dots[[x]]
}

s <- function(...) {
  envir <- parent.frame()
  dots <- modifyList(.get_dots(envir), list(...))
  do.call(\(...) { # trick to get a proper <...> object and assign it back to caller env
    assign("...", environment()$..., envir = envir)
  }, dots)
}


foo <- function(
    id = "id_a",
    at = Sys.time()) {
  message(stringr::str_glue("{id}: {at}"))
  Sys.sleep(1)
}


runtime_gateway <- function(
    fun = foo,
    run_mode = c("once", "while"),
    ... # Args to be passed to `fun`
    ) {
  run_mode <- match.arg(run_mode)

  if (run_mode == "once") {
    fun(...)
  } else if (run_mode == "while") {
    counter <- 0
    at_in_initial_dots <- !is.null(g(at))
    while (counter < 3) {
      if (at_in_initial_dots) {
        message("`at` was passed via ellipsis:")
        message(g(at))
        s(at = g(at) + 60) # --> increment `at` by 60
      } else {
        s(at = Sys.time() + 60) # --> set `at`
      }
      fun(...) # --> passing updated ... possible now
      counter <- counter + 1
    }
  }
}


runtime_gateway()
runtime_gateway(at = lubridate::ymd_hms("2020-02-21 10:30:00"))
runtime_gateway(run_mode = "while")
runtime_gateway(run_mode = "while", id = "id_b")
runtime_gateway(run_mode = "while", at = lubridate::ymd_hms("2020-02-21 10:30:00"))


