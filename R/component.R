#' Construct a generic AI component (prompt, retrieval, tool, guardrail, runtime)
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param kind Controlled kind/type value.
#' @param name Character name.
#' @param description Character description.
#' @param config Optional named list of configuration values.
#' @param owner Optional character owner identifier.
#' @param status New status value; validated against the relevant controlled vocabulary.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `ai_component` object.
#' @export
ai_component <- function(id = NULL, kind = "PROMPT", name,
                         description = NULL, config = list(),
                         owner = NULL, status = NULL,
                         metadata = list(), provenance = list()) {
  assert_scalar_character(name, arg = "name")
  assert_choice(kind, COMPONENT_KIND_LEVELS, arg = "kind")
  if (is.null(id)) id <- new_id(substr(kind, 1, 3))
  new_aitrace_object(
    "ai_component", id,
    fields = list(
      kind        = kind,
      name        = name,
      description = description,
      config      = config
    ),
    owner = owner, status = status, metadata = metadata, provenance = provenance
  )
}

#' @keywords internal
#' @noRd
component_restore <- function(lst) {
  env <- restore_envelope(lst)
  structure(
    c(env, list(
      kind        = lst$kind,
      name        = lst$name,
      description = lst$description,
      config      = lst$config %||% list()
    )),
    class = c("ai_component", "aitrace_object")
  )
}

register_restorer("ai_component", component_restore)

#' @export
format.ai_component <- function(x, ...) {
  cli::format_inline("{.strong <ai_component>} {x$id} [{x$kind}] {x$name}")
}
