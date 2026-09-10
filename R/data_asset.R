#' Construct a data asset
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param name Character name.
#' @param role One of [DATA_ROLE_LEVELS].
#' @param source Optional character source of the value or data.
#' @param completeness Optional numeric data completeness fraction.
#' @param row_count Optional integer row count.
#' @param owner Optional character owner identifier.
#' @param status New status value; validated against the relevant controlled vocabulary.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `ai_data` object.
#' @export
ai_data <- function(id = NULL, name, role = "TRAINING", source = NULL,
                    completeness = NULL, row_count = NULL,
                    owner = NULL, status = NULL,
                    metadata = list(), provenance = list()) {
  assert_scalar_character(name, arg = "name")
  assert_choice(role, DATA_ROLE_LEVELS, arg = "role")
  if (is.null(id)) id <- new_id("DAT")
  new_aitrace_object(
    "ai_data", id,
    fields = list(
      name         = name,
      role         = role,
      source       = source,
      completeness = completeness,
      row_count    = row_count
    ),
    owner = owner, status = status, metadata = metadata, provenance = provenance
  )
}

#' @keywords internal
#' @noRd
data_asset_restore <- function(lst) {
  env <- restore_envelope(lst)
  structure(
    c(env, list(
      name         = lst$name,
      role         = lst$role,
      source       = lst$source,
      completeness = lst$completeness,
      row_count    = lst$row_count
    )),
    class = c("ai_data", "aitrace_object")
  )
}

register_restorer("ai_data", data_asset_restore)

#' @export
format.ai_data <- function(x, ...) {
  cli::format_inline("{.strong <ai_data>} {x$id} ({x$name}, role: {x$role})")
}
