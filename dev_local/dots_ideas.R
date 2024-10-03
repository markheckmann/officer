# modify ... on the fly

add_to_dots <- \(..., .envir = NULL) {
  .envir <- if (is.null(.envir)) parent.frame() else .envir
  dots <- get("...", envir = environment())
  assign("...", dots, envir = .envir)
}


remove_from_dots <- \(dots, ..., .envir = NULL) {
  .envir <- if (is.null(.envir)) rlang::caller_env() else .envir
  nms <- list(...)
  dots <- dots[names(dots) != nms]
  do.call(\(...), dots)
}


assign_dots <- \(...) {
  dots <- get("...", envir = environment())
  assign("...", dots, envir = .envir)
}


list_to_dots <- \(dots) {
  do.call(dots, assign_dots, )
}


modify_dots <- function(...) {
  changes <- list(...)  # keep ... as list
  .dots <- get("...", envir = parent.frame())
  assign("...", .dots, envir = environment()) # use ... from caller env
  dots <- list(...) # make available here
  dots_new <- modifyList(dots, changes)
  .envir <- parent.frame()
  do.call(\(...) {}, dots_new)
  assign("...", dots_new, envir = parent.frame())
}


dots_from_env <- \(envir, ...) {
  .dots <- get("...", envir = envir)
  f <- \(...) {
    assign("...", .dots, envir = environment())
    list(...)
  }
  f()
}


dots_from_env <- \(envir) {
  browser()
  .dots <- get("...", envir = envir)
  assign("...", .dots, envir = environment())
  list(...)
}

foo <- \(...) {
  get("...", envir = environment())
}
class(foo(2))

#
foo <- \(...) {
  dots_from_env(environment())
  # modify_dots(c = 3, b=999)
  # list(...)
}

foo(a=1)

foo <- \(...) {
  add_to_dots(..., x = 3, y = 4)
  remove_from_dots(..., "c")
  list(...)
}

foo(a = 1, b = 2)

foo <- \(...) {
  print(ls(all.names = T))
  get("...", envir = environment())
}
foo()



#######

e <- new.env(parent = emptyenv())

dots_to_env <- function(dots, envir) {
  do.call(\(...) {
    assign("...", get("...", envir = environment()), envir = envir)
  }, dots)
}

dots_to_env(list(a=99, b=3, c = 4), envir = e)

ls(envir = e, all.names = T)

h <- function(..., .envir) {
  assign("...", .envir$..., envir = environment())
  list(...)
}

h(.envir = e)



#######

e <- new.env(parent = emptyenv())

dots_to_env <- function(dots, to_env) {
  do.call(\(...) {
    assign("...", get("...", envir = environment()), envir = to_env) # using get for a proper "..." object
  }, dots)
}

dots_to_env(list(a=99, b=3, c = 4), to_env =  e)
names(e$...)

h <- function(..., .envir) {
  assign("...", .envir$..., envir = environment())
  list(...)
}

h(.envir = e)


## MISC

# assign dots to env
f <- \(..., .to_env) {
  e <- environment()
  assign("...", e$..., envir = .to_env)
}

e <- new.env(parent = emptyenv())
f(a = 1, b = 6,.to_env = e)
names(e$...)


# assign list to dots
e <- new.env(parent = emptyenv())

dots_to_env <- function(dots, to_env) {
  do.call(\(...) {
    e <- environment()
    assign("...", e$..., envir = to_env)
  }, dots)
}

dots_to_env(list(a=99, b=3, c = 4), to_env =  e)
names(e$...)


# assign list to dots
# e <- new.env(parent = emptyenv())

.modify_dots <- function(dots, to_env) {
  do.call(\(...) {
    assign("...", environment()$..., envir = to_env)
  }, dots)
}

foo <- \(...) {
  updates <- list(new = -999, a = NULL)
  dots <- modifyList(list(...), updates)
  .modify_dots(dots, environment())
  list(...)
}

foo(a=1, new = 1, b = 2)



### ----

modify_dots <- function(dots, updates = list(), envir = parent.frame()) {
  dots <- modifyList(dots, updates)
  do.call(\(...) {
    assign("...", environment()$..., envir = envir)
  }, dots)
}

foo <- \(x, ...) {
  updates <- list(b = NULL, c = -999, d = 4)
  modify_dots(list(...), updates)
  list(...)
}

foo(a = 1, b = 2, c= 3)


### ----


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

foo()
foo(a=1, b=99)


### R6 class -----------------

# https://stackoverflow.com/questions/60338114/updating-values-of-three-dot-ellipsis-in-r


dots_updater <- R6::R6Class("dots_updater",
  public = list(
    dots = NULL,
    initialize = \() {
      private$env <- parent.frame(n = 2)  # env where object is initialized
      self$dots <- private$dots_to_list()  # load dots as a list to modify
    },
    update = \() {  # trick to update `...` inside function env
      do.call(\(...) {assign("...", environment()$..., envir = private$env)}, self$dots)
    },
    get = \(key) {
      self$dots[[key]]
    },
    set = \(...) {
      self$dots <- modifyList(self$dots, list(...))
      self$update()
    }
  ),
  private = list(
    env = NULL,
    dots_to_list = \() {  # get dots from ... env
      assign("...", private$env$...)
      list(...)
    }
  )
)


foo <- \(...) {
  cat("-- START --------\n")
  print(list(...))

  u <- dots_updater$new()
  cat("-- CHANGE 1: update 'a', add 'b' -------\n")
  u$set(a = 999, b = 2)
  print(list(...))

  cat("-- CHANGE 2: increment 'a', remove 'b' -------\n")
  u$set(a = u$get("a") + 1, b = NULL)
  print(list(...))
}
foo(a=1)



