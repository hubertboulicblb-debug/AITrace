# Restorer registry for JSON/YAML round-trips
.aitrace_restore_registry <- new.env(parent = emptyenv())

#' @keywords internal
#' @noRd
register_restorer <- function(type, fn) {
  assign(type, fn, envir = .aitrace_restore_registry)
}

#' @keywords internal
#' @noRd
get_restorer <- function(type) {
  if (!exists(type, envir = .aitrace_restore_registry, inherits = FALSE)) {
    abort_aitrace(
      sprintf("No restorer registered for AITrace object type \"%s\".", type),
      class = "aitrace_error_restore"
    )
  }
  get(type, envir = .aitrace_restore_registry, inherits = FALSE)
}

.aitrace_timestamp_fields <- c(
  "created", "updated", "timestamp", "date", "detected_at", "closed_at",
  "deployment_date", "review_date", "decided_at"
)

#' Construct the common AITrace object envelope
#' @keywords internal
#' @noRd
new_aitrace_object <- function(classname, id, fields = list(), owner = NULL,
                                status = NULL, metadata = list(),
                                provenance = list()) {
  ts <- now_utc()
  obj <- c(
    list(
      id         = id,
      type       = classname,
      version    = 1L,
      created    = ts,
      updated    = ts,
      owner      = owner,
      status     = status,
      metadata   = metadata,
      provenance = provenance
    ),
    fields
  )
  structure(obj, class = c(classname, "aitrace_object"))
}

#' Bump version and append a change-log entry
#' @param x An `aitrace_object`.
#' @param ... Not currently used.
#' @return The updated object, with `version` incremented and a change-log entry appended.
#' @export
bump_version <- function(x, ...) UseMethod("bump_version")

#' @export
bump_version.aitrace_object <- function(x, note = NULL, actor = NULL, ...) {
  x$version <- x$version + 1L
  x$updated <- now_utc()
  entry <- list(
    version   = x$version,
    timestamp = x$updated,
    note      = note %||% NA_character_,
    actor     = actor %||% NA_character_
  )
  x$metadata$change_log <- c(x$metadata$change_log %||% list(), list(entry))
  x
}

#' @export
bump_version.default <- function(x, ...) {
  abort_aitrace(
    sprintf("bump_version() is not defined for %s.", obj_desc(x)),
    class = "aitrace_error_type"
  )
}

#' Retrieve the change log as a tibble
#' @param x An `aitrace_object`.
#' @return A [tibble::tibble()] with columns `version`, `timestamp`, `note` and `actor`.
#' @export
change_log <- function(x) {
  assert_class(x, "aitrace_object")
  log <- x$metadata$change_log %||% list()
  if (length(log) == 0L) {
    return(tibble::tibble(
      version = integer(), timestamp = as.POSIXct(character()),
      note = character(), actor = character()
    ))
  }
  tibble::tibble(
    version   = vapply(log, function(e) as.integer(e$version), integer(1)),
    timestamp = do.call(c, lapply(log, function(e) e$timestamp)),
    note      = vapply(log, function(e) as.character(e$note %||% NA_character_), character(1)),
    actor     = vapply(log, function(e) as.character(e$actor %||% NA_character_), character(1))
  )
}

#' Convert an AITrace object to a plain list (for JSON/YAML)
#' @param x An `aitrace_object` (or, for the default method, a plain list).
#' @param ... Not currently used.
#' @return A plain list suitable for JSON/YAML serialisation.
#' @export
to_list <- function(x, ...) UseMethod("to_list")

#' @export
to_list.aitrace_object <- function(x, ...) {
  lst <- unclass(x)
  for (f in .aitrace_timestamp_fields) {
    if (!is.null(lst[[f]]) && inherits(lst[[f]], "POSIXt")) {
      lst[[f]] <- format(lst[[f]], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    }
  }
  if (!is.null(lst$metadata$change_log)) {
    lst$metadata$change_log <- lapply(lst$metadata$change_log, function(e) {
      if (inherits(e$timestamp, "POSIXt")) {
        e$timestamp <- format(e$timestamp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      }
      e
    })
  }
  # Recurse: fields that are themselves aitrace_object instances (or plain
  # lists of them) must be converted too, otherwise jsonlite::toJSON() hits
  # a raw S3 object it has no method for as soon as any slot (models, data,
  # evidence, ...) is non-empty.
  to_list_recurse(lst)
}

#' @export
to_list.default <- function(x, ...) to_list_recurse(x)

#' Recurse into plain lists, converting any nested aitrace_object elements
#'
#' Subsetting a classed list with `[` (as [canonicalize_system()] does)
#' drops the class attribute, so a plain list can still hold raw
#' aitrace_object elements. This walks such structures so serialisation
#' (JSON/YAML) and fingerprinting never see an unconverted object.
#' @keywords internal
#' @noRd
to_list_recurse <- function(x) {
  if (inherits(x, "aitrace_object")) return(to_list(x))
  # data.frame (e.g. an audit table, or a data.frame a caller stashed in
  # metadata) is a list under the hood; jsonlite handles it natively, so
  # leave it untouched rather than flattening it with lapply().
  if (is.data.frame(x)) return(x)
  if (!is.list(x)) return(x)
  lapply(x, to_list_recurse)
}

#' Serialise to JSON
#' @param x An object with a [to_list()] method (typically an `aitrace_object`).
#' @param pretty If `TRUE` (default), pretty-print the JSON output.
#' @param ... Not currently used.
#' @return A single character string of JSON (a `jsonlite::json` scalar).
#' @export
to_json <- function(x, pretty = TRUE, ...) {
  jsonlite::toJSON(to_list(x), auto_unbox = TRUE, null = "null",
                   pretty = pretty, digits = NA, ...)
}

#' Serialise to YAML
#' @param x An object with a [to_list()] method (typically an `aitrace_object`).
#' @param ... Not currently used.
#' @return A single character string of YAML.
#' @export
to_yaml <- function(x, ...) {
  yaml::as.yaml(to_list(x), ...)
}

#' Restore from JSON text
#' @param txt Character JSON text to restore from.
#' @return The restored AITrace object.
#' @export
from_json <- function(txt) {
  lst <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  restore_aitrace(lst)
}

#' @keywords internal
#' @noRd
restore_aitrace <- function(lst) {
  type <- lst$type %||% lst$class %||% NULL
  if (is.null(type)) {
    abort_aitrace("Cannot restore object: missing type field.",
                  class = "aitrace_error_restore")
  }
  get_restorer(type)(lst)
}

#' @keywords internal
#' @noRd
restore_envelope <- function(lst) {
  ts <- function(field) {
    v <- lst[[field]]
    if (is.null(v)) return(now_utc())
    as.POSIXct(v, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  list(
    id         = lst$id,
    version    = as.integer(lst$version %||% 1L),
    created    = ts("created"),
    updated    = ts("updated"),
    owner      = lst$owner %||% NULL,
    status     = lst$status %||% NULL,
    metadata   = restore_change_log(lst$metadata),
    provenance = lst$provenance %||% list()
  )
}

#' @keywords internal
#' @noRd
restore_change_log <- function(metadata) {
  if (is.null(metadata) || is.null(metadata$change_log)) return(metadata %||% list())
  metadata$change_log <- lapply(metadata$change_log, function(e) {
    e$timestamp <- as.POSIXct(e$timestamp, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    e
  })
  metadata
}

#' @export
print.aitrace_object <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
format.aitrace_object <- function(x, ...) {
  cli::format_inline("{.strong <{x$type}>} {x$id} (v{x$version}, status: {x$status %||% 'NA'})")
}
