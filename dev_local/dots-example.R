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


foo <- function(
    id = "id_a",
    at = Sys.time()
) {
  message(stringr::str_glue("{id}: {at}"))
  Sys.sleep(1)
}


runtime_gateway <- function(
    fun = foo,
    run_mode = c("once", "while"),
    ... # Args to be passed to `fun`
    ) {
  run_mode <- match.arg(run_mode)
  u <- dots_updater$new()  # --> init a dots_updater

  if (run_mode == "once") {
    fun(...)
  } else if (run_mode == "while") {
    counter <- 0
    browser()
    while (counter < 3) {
      if ("at" %in% names(u$dots)) {
        message("`at` was passed via ellipsis:")
        message(u$get("at"))
        u$set(at = u$get("at") + 60)  # --> use getter & setter
      } else {
        u$set(at = Sys.time() + 60)  # --> use getter
      }
      fun(...)  # --> passing updated ... possible now
      counter <- counter + 1
    }
  }
}


runtime_gateway()
#> id_a: 2020-02-21 14:22:07

runtime_gateway(at = lubridate::ymd_hms("2020-02-21 10:30:00"))
#> id_a: 2020-02-21 10:30:00

runtime_gateway(run_mode = "while")
#> id_a: 2020-02-21 14:23:09
#> id_a: 2020-02-21 14:23:10
#> id_a: 2020-02-21 14:23:11

runtime_gateway(run_mode = "while", id = "id_b")
#> id_b: 2020-02-21 14:23:12
#> id_b: 2020-02-21 14:23:13
#> id_b: 2020-02-21 14:23:14

runtime_gateway(run_mode = "while", at = lubridate::ymd_hms("2020-02-21 10:30:00"))
