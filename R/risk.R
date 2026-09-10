#' Construct a risk record
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param category Controlled category value.
#' @param description Character description.
#' @param likelihood One of [LIKELIHOOD_LEVELS].
#' @param impact One of [CHANGE_IMPACT_LEVELS].
#' @param control Optional character description of the risk control in place.
#' @param residual_likelihood Optional residual likelihood after controls; one of [LIKELIHOOD_LEVELS].
#' @param residual_impact Optional residual impact after controls; one of [IMPACT_LEVELS].
#' @param owner Optional character owner identifier.
#' @param status New status value; validated against the relevant controlled vocabulary.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `ai_risk` object.
#' @export
ai_risk <- function(id = NULL, category = "SAFETY", description,
                    likelihood = "POSSIBLE", impact = "MODERATE",
                    control = NULL,
                    residual_likelihood = NULL, residual_impact = NULL,
                    owner = NULL, status = "OPEN",
                    metadata = list(), provenance = list()) {
  assert_scalar_character(description, arg = "description")
  assert_choice(category, RISK_CATEGORIES, arg = "category")
  assert_choice(likelihood, LIKELIHOOD_LEVELS, arg = "likelihood")
  assert_choice(impact, IMPACT_LEVELS, arg = "impact")
  assert_choice(status, RISK_STATUS_LEVELS, arg = "status")
  if (!is.null(residual_likelihood)) assert_choice(residual_likelihood, LIKELIHOOD_LEVELS)
  if (!is.null(residual_impact)) assert_choice(residual_impact, IMPACT_LEVELS)
  if (is.null(id)) id <- new_id("RSK")
  new_aitrace_object(
    "ai_risk", id,
    fields = list(
      category            = category,
      description         = description,
      likelihood          = likelihood,
      impact              = impact,
      control             = control,
      residual_likelihood = residual_likelihood,
      residual_impact     = residual_impact
    ),
    owner = owner, status = status, metadata = metadata, provenance = provenance
  )
}

#' Map likelihood x impact to a risk class
#' @param likelihood One of [LIKELIHOOD_LEVELS].
#' @param impact One of [IMPACT_LEVELS].
#' @return A single character risk class, one of [RISK_CLASS_LEVELS].
#' @export
risk_severity <- function(likelihood, impact) {
  assert_choice(likelihood, LIKELIHOOD_LEVELS)
  assert_choice(impact, IMPACT_LEVELS)
  li <- level_rank(likelihood, LIKELIHOOD_LEVELS)
  im <- level_rank(impact, IMPACT_LEVELS)
  score <- li * im
  if (score <= 4) "LOW" else if (score <= 9) "MEDIUM" else if (score <= 16) "HIGH" else "CRITICAL"
}

#' Update risk status
#' @param risk An [ai_risk()].
#' @param status New status value; validated against the relevant controlled vocabulary.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @param note Optional character note recorded in the change log.
#' @return The updated `ai_risk`.
#' @export
set_risk_status <- function(risk, status, actor = NULL, note = NULL) {
  assert_class(risk, "ai_risk")
  assert_choice(status, RISK_STATUS_LEVELS, arg = "status")
  risk$status <- status
  bump_version(risk, note = note %||% sprintf("status -> %s", status), actor = actor)
}

#' @keywords internal
#' @noRd
risk_restore <- function(lst) {
  env <- restore_envelope(lst)
  structure(
    c(env, list(
      category            = lst$category,
      description         = lst$description,
      likelihood          = lst$likelihood,
      impact              = lst$impact,
      control             = lst$control,
      residual_likelihood = lst$residual_likelihood,
      residual_impact     = lst$residual_impact
    )),
    class = c("ai_risk", "aitrace_object")
  )
}

register_restorer("ai_risk", risk_restore)

#' @export
format.ai_risk <- function(x, ...) {
  sev <- risk_severity(x$likelihood, x$impact)
  cli::format_inline(
    "{.strong <ai_risk>} {x$id} [{x$category}/{sev}/{x$status}]"
  )
}
