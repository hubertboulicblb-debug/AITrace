#' Construct an AI model component
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param name Character name.
#' @param model_type One of [MODEL_TYPE_LEVELS].
#' @param algorithm Optional character algorithm name.
#' @param model_version Optional character model version identifier.
#' @param metrics Optional named list of model metrics.
#' @param owner Optional character owner identifier.
#' @param status New status value; validated against the relevant controlled vocabulary.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `ai_model` object.
#' @export
ai_model <- function(id = NULL, name, model_type = "CLASSIFICATION",
                     algorithm = NULL, model_version = NULL,
                     metrics = list(), owner = NULL, status = NULL,
                     metadata = list(), provenance = list()) {
  assert_scalar_character(name, arg = "name")
  assert_choice(model_type, MODEL_TYPE_LEVELS, arg = "model_type")
  if (is.null(id)) id <- new_id("MOD")
  new_aitrace_object(
    "ai_model", id,
    fields = list(
      name          = name,
      model_type    = model_type,
      algorithm     = algorithm,
      model_version = model_version,
      metrics       = metrics
    ),
    owner = owner, status = status, metadata = metadata, provenance = provenance
  )
}

#' @keywords internal
#' @noRd
model_restore <- function(lst) {
  env <- restore_envelope(lst)
  structure(
    c(env, list(
      name          = lst$name,
      model_type    = lst$model_type,
      algorithm     = lst$algorithm,
      model_version = lst$model_version,
      metrics       = lst$metrics %||% list()
    )),
    class = c("ai_model", "aitrace_object")
  )
}

register_restorer("ai_model", model_restore)

#' @export
format.ai_model <- function(x, ...) {
  cli::format_inline(
    "{.strong <ai_model>} {x$id} ({x$name}, {x$model_type}, v{x$version})"
  )
}
