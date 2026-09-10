#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Current UTC timestamp as POSIXct
#' @keywords internal
#' @noRd
now_utc <- function() {
  x <- Sys.time()
  attr(x, "tzone") <- "UTC"
  x
}

#' Current UTC timestamp as ISO-8601 character (for audit / fingerprint stability)
#' @keywords internal
#' @noRd
now_utc_chr <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Generate a short random hex suffix
#' @keywords internal
#' @noRd
random_suffix <- function(n = 6) {
  paste0(sample(c(0:9, letters[1:6]), n, replace = TRUE), collapse = "")
}

#' Generate an object identifier
#' @keywords internal
#' @noRd
new_id <- function(prefix) {
  paste0(
    toupper(prefix), "-",
    format(Sys.time(), "%Y%m%d%H%M%S"), "-",
    random_suffix()
  )
}

#' @keywords internal
#' @noRd
abort_aitrace <- function(message, class, ..., call = rlang::caller_env()) {
  rlang::abort(
    message = message,
    class = c(class, "aitrace_error"),
    ...,
    call = call
  )
}

#' @keywords internal
#' @noRd
assert_scalar_character <- function(x, arg = rlang::caller_arg(x),
                                     call = rlang::caller_env(),
                                     allow_null = FALSE) {
  if (allow_null && is.null(x)) return(invisible(NULL))
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    abort_aitrace(
      sprintf("`%s` must be a single (non-missing) character string, not %s.",
              arg, obj_desc(x)),
      class = "aitrace_error_type",
      call = call
    )
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
assert_choice <- function(x, choices, arg = rlang::caller_arg(x),
                           call = rlang::caller_env(), allow_null = FALSE) {
  if (allow_null && is.null(x)) return(invisible(NULL))
  assert_scalar_character(x, arg = arg, call = call)
  if (!x %in% choices) {
    abort_aitrace(
      sprintf(
        "`%s` must be one of %s, not \"%s\".",
        arg, paste(sprintf('"%s"', choices), collapse = ", "), x
      ),
      class = "aitrace_error_choice",
      call = call
    )
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
assert_class <- function(x, class, arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  if (!inherits(x, class)) {
    abort_aitrace(
      sprintf("`%s` must be a <%s> object, not %s.", arg, class[1], obj_desc(x)),
      class = "aitrace_error_type",
      call = call
    )
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
assert_posixct <- function(x, arg = rlang::caller_arg(x),
                            call = rlang::caller_env(), allow_null = FALSE) {
  if (allow_null && is.null(x)) return(invisible(NULL))
  if (!inherits(x, "POSIXt") || length(x) != 1L || is.na(x)) {
    abort_aitrace(
      sprintf("`%s` must be a single (non-missing) POSIXct/POSIXlt timestamp, not %s.",
              arg, obj_desc(x)),
      class = "aitrace_error_type",
      call = call
    )
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
assert_scalar_positive_integer <- function(x, arg = rlang::caller_arg(x),
                                            call = rlang::caller_env(), allow_null = FALSE) {
  if (allow_null && is.null(x)) return(invisible(NULL))
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) &&
    isTRUE(all.equal(x, round(x))) && x > 0
  if (!ok) {
    abort_aitrace(
      sprintf("`%s` must be a single positive integer, not %s.", arg, obj_desc(x)),
      class = "aitrace_error_type",
      call = call
    )
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
assert_list_of <- function(x, class, arg = rlang::caller_arg(x),
                            call = rlang::caller_env()) {
  if (!is.list(x)) {
    abort_aitrace(
      sprintf("`%s` must be a list of <%s> objects.", arg, class[1]),
      class = "aitrace_error_type",
      call = call
    )
  }
  ok <- vapply(x, inherits, logical(1), what = class)
  if (!all(ok)) {
    abort_aitrace(
      sprintf("`%s` must be a list where every element is a <%s> object.", arg, class[1]),
      class = "aitrace_error_type",
      call = call
    )
  }
  invisible(NULL)
}

#' @keywords internal
#' @noRd
obj_desc <- function(x) {
  if (is.null(x)) return("NULL")
  paste0("an object of class <", paste(class(x), collapse = "/"), ">")
}

#' @keywords internal
#' @noRd
`%in_or_null%` <- function(x, choices) is.null(x) || x %in% choices

#' @keywords internal
#' @noRd
compact <- function(x) x[!vapply(x, is.null, logical(1))]

#' @keywords internal
#' @noRd
list_by_id <- function(items) {
  ids <- vapply(items, function(x) x$id, character(1))
  stats::setNames(items, ids)
}
