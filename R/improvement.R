#' Construct an AI improvement record
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param system_id Identifier of the [ai_system()] this record belongs to.
#' @param domain One of [IMPROVEMENT_DOMAINS].
#' @param trigger One of [IMPROVEMENT_TRIGGERS].
#' @param problem Character description of the problem being addressed.
#' @param root_cause Character description of the identified root cause.
#' @param intervention Character description of the proposed intervention.
#' @param expected_effects Named list mapping metric names to an expected [EFFECT_DIRECTION_LEVELS] value.
#' @param priority One of [IMPROVEMENT_PRIORITY_LEVELS].
#' @param validation_method One of [VALIDATION_METHOD_LEVELS].
#' @param acceptance_criteria Optional character description of the acceptance criteria.
#' @param regression_criteria Optional character description of the regression criteria.
#' @param state New lifecycle state.
#' @param owner Optional character owner identifier.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `ai_improvement` object.
#' @export
ai_improvement <- function(id = NULL, system_id,
                           domain = "PREDICTIVE_PERFORMANCE",
                           trigger = "OTHER",
                           problem,
                           root_cause = NULL,
                           intervention = NULL,
                           expected_effects = list(),
                           priority = "MEDIUM",
                           validation_method = NULL,
                           acceptance_criteria = NULL,
                           regression_criteria = NULL,
                           state = "OBSERVED",
                           owner = NULL,
                           metadata = list(), provenance = list()) {
  assert_scalar_character(system_id, arg = "system_id")
  assert_scalar_character(problem, arg = "problem")
  assert_choice(domain, IMPROVEMENT_DOMAINS, arg = "domain")
  assert_choice(trigger, IMPROVEMENT_TRIGGERS, arg = "trigger")
  assert_choice(priority, IMPROVEMENT_PRIORITY_LEVELS, arg = "priority")
  assert_choice(state, IMPROVEMENT_LIFECYCLE_STATES, arg = "state")
  if (!is.null(validation_method)) assert_choice(validation_method, VALIDATION_METHOD_LEVELS)
  if (length(expected_effects) > 0) {
    if (is.null(names(expected_effects)) || any(names(expected_effects) == "")) {
      abort_aitrace("expected_effects must be a named list.", class = "aitrace_error_type")
    }
    for (nm in names(expected_effects)) {
      assert_choice(expected_effects[[nm]], EFFECT_DIRECTION_LEVELS, arg = paste0("expected_effects$", nm))
    }
  }
  if (is.null(id)) id <- new_id("IMP")
  new_aitrace_object(
    "ai_improvement", id,
    fields = list(
      system_id             = system_id,
      domain                = domain,
      trigger               = trigger,
      problem               = problem,
      root_cause            = root_cause,
      intervention          = intervention,
      expected_effects      = expected_effects,
      priority              = priority,
      validation_method     = validation_method,
      acceptance_criteria   = acceptance_criteria,
      regression_criteria   = regression_criteria,
      decision              = NULL,
      decision_maker        = NULL,
      decided_at            = NULL,
      deployment_date       = NULL,
      verification_result   = NULL,
      verification_note     = NULL,
      linked_incident_id    = NULL
    ),
    owner = owner, status = state, metadata = metadata, provenance = provenance
  )
}

#' Attach an improvement to a system
#' @param system An [ai_system()].
#' @param improvement An [ai_improvement()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
add_improvement <- function(system, improvement, actor = NULL) {
  assert_class(improvement, "ai_improvement")
  if (!identical(improvement$system_id, system$id)) {
    abort_aitrace(
      sprintf('improvement$system_id ("%s") does not match system$id ("%s").',
              improvement$system_id, system$id),
      class = "aitrace_error_mismatch"
    )
  }
  add_to_slot(system, "improvements", improvement,
              note = sprintf("improvement added: %s [%s]", improvement$id, improvement$domain),
              actor = actor)
}

#' Change improvement lifecycle state
#' @param improvement An [ai_improvement()].
#' @param state New lifecycle state.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @param note Optional character note recorded in the change log.
#' @return The updated `ai_improvement`.
#' @export
set_improvement_state <- function(improvement, state, actor = NULL, note = NULL) {
  assert_class(improvement, "ai_improvement")
  validate_transition(improvement$status, state, .improvement_lifecycle_transitions,
                       IMPROVEMENT_LIFECYCLE_STATES)
  improvement$status <- state
  bump_version(improvement, note = note %||% sprintf("state -> %s", state), actor = actor)
}

#' @export
set_root_cause.ai_improvement <- function(x, root_cause, actor = NULL, ...) {
  assert_scalar_character(root_cause, arg = "root_cause")
  x$root_cause <- root_cause
  bump_version(x, note = "root_cause recorded", actor = actor)
}

#' Record a governance decision on an improvement
#' @param improvement An [ai_improvement()].
#' @param decision One of [GOVERNANCE_DECISION_LEVELS].
#' @param rationale Character rationale (required, non-empty, for `OVERRIDE_POLICY`).
#' @param decision_maker Optional character identifying who made the decision.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_improvement`.
#' @export
record_decision <- function(improvement, decision, rationale = NULL,
                            decision_maker = NULL, actor = NULL) {
  assert_class(improvement, "ai_improvement")
  assert_choice(decision, GOVERNANCE_DECISION_LEVELS, arg = "decision")
  improvement$decision <- decision
  improvement$decision_maker <- decision_maker %||% actor
  improvement$decided_at <- now_utc()
  if (!is.null(rationale)) {
    improvement$metadata$decision_rationale <- rationale
  }
  bump_version(improvement, note = sprintf("decision: %s", decision), actor = actor)
}

#' Mark an improvement as deployed
#' @param improvement An [ai_improvement()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_improvement`.
#' @export
mark_deployed <- function(improvement, actor = NULL) {
  assert_class(improvement, "ai_improvement")
  improvement$deployment_date <- now_utc()
  if (improvement$status == "APPROVED") {
    improvement <- set_improvement_state(improvement, "DEPLOYED", actor = actor)
  } else {
    improvement <- bump_version(improvement, note = "deployment_date recorded", actor = actor)
  }
  improvement
}

#' Record verification result (explicit; never inferred)
#' @param improvement An [ai_improvement()].
#' @param result Result value; validated against the relevant controlled vocabulary.
#' @param note Optional character note recorded in the change log.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_improvement`.
#' @export
record_verification <- function(improvement, result, note = NULL, actor = NULL) {
  assert_class(improvement, "ai_improvement")
  assert_choice(result, VERIFICATION_RESULT_LEVELS, arg = "result")
  improvement$verification_result <- result
  improvement$verification_note <- note
  bump_version(improvement, note = sprintf("verification: %s", result), actor = actor)
}

#' Raise an improvement from an incident (closed loop)
#' @param system An [ai_system()].
#' @param incident_id Identifier of the related [ai_incident()].
#' @param intervention Character description of the proposed intervention.
#' @param domain One of [IMPROVEMENT_DOMAINS].
#' @param validation_method One of [VALIDATION_METHOD_LEVELS].
#' @param expected_effects Named list mapping metric names to an expected [EFFECT_DIRECTION_LEVELS] value.
#' @param acceptance_criteria Optional character description of the acceptance criteria.
#' @param regression_criteria Optional character description of the regression criteria.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`, carrying the new `ai_improvement` and the incident's updated link.
#' @export
raise_improvement_from_incident <- function(system, incident_id,
                                             intervention = NULL,
                                             domain = "PREDICTIVE_PERFORMANCE",
                                             validation_method = "OFFLINE_EVALUATION",
                                             expected_effects = list(),
                                             acceptance_criteria = NULL,
                                             regression_criteria = NULL,
                                             actor = NULL) {
  assert_class(system, "ai_system")
  inc <- system$incidents[[incident_id]]
  if (is.null(inc)) {
    abort_aitrace(sprintf("Incident \"%s\" not found on system.", incident_id),
                  class = "aitrace_error_not_found")
  }
  imp <- ai_improvement(
    system_id = system$id,
    domain = domain,
    trigger = "INCIDENT",
    problem = inc$description,
    root_cause = inc$root_cause,
    intervention = intervention,
    expected_effects = expected_effects,
    validation_method = validation_method,
    acceptance_criteria = acceptance_criteria,
    regression_criteria = regression_criteria
  )
  imp$linked_incident_id <- incident_id
  system <- add_improvement(system, imp, actor = actor)
  # also link back on the incident copy inside the system
  system$incidents[[incident_id]] <- link_improvement(system$incidents[[incident_id]], imp$id, actor = actor)
  system
}

#' Portfolio view of improvements
#' @param system An [ai_system()].
#' @return A [tibble::tibble()] summarising the system's improvements.
#' @export
improvement_portfolio <- function(system) {
  assert_class(system, "ai_system")
  items <- unname(system$improvements)
  if (length(items) == 0) {
    return(tibble::tibble(
      id = character(), domain = character(), priority = character(),
      status = character(), trigger = character()
    ))
  }
  tibble::tibble(
    id       = vapply(items, `[[`, character(1), "id"),
    domain   = vapply(items, `[[`, character(1), "domain"),
    priority = vapply(items, `[[`, character(1), "priority"),
    status   = vapply(items, `[[`, character(1), "status"),
    trigger  = vapply(items, `[[`, character(1), "trigger")
  )
}

#' Simple prioritisation helper (user-supplied weights; never claims objectivity)
#' @param system An [ai_system()].
#' @param weights Named numeric vector of prioritisation weights.
#' @return A [tibble::tibble()] as [improvement_portfolio()], with a `score` column added and sorted descending.
#' @export
prioritize_improvements <- function(system, weights = c(priority = 1, domain = 0)) {
  assert_class(system, "ai_system")
  tab <- improvement_portfolio(system)
  if (nrow(tab) == 0) return(tab)
  pr <- level_rank(tab$priority, IMPROVEMENT_PRIORITY_LEVELS)
  tab$score <- pr * (weights["priority"] %||% 1)
  tab <- tab[order(-tab$score, tab$id), ]
  attr(tab, "note") <- "Scores are user-configured weights only; not an objective ranking."
  tab
}

#' @keywords internal
#' @noRd
improvement_restore <- function(lst) {
  env <- restore_envelope(lst)
  ts <- function(v) {
    if (is.null(v)) return(NULL)
    as.POSIXct(v, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  structure(
    c(env, list(
      system_id           = lst$system_id,
      domain              = lst$domain,
      trigger             = lst$trigger,
      problem             = lst$problem,
      root_cause          = lst$root_cause,
      intervention        = lst$intervention,
      expected_effects    = lst$expected_effects %||% list(),
      priority            = lst$priority,
      validation_method   = lst$validation_method,
      acceptance_criteria = lst$acceptance_criteria,
      regression_criteria = lst$regression_criteria,
      decision            = lst$decision,
      decision_maker      = lst$decision_maker,
      decided_at          = ts(lst$decided_at),
      deployment_date     = ts(lst$deployment_date),
      verification_result = lst$verification_result,
      verification_note   = lst$verification_note,
      linked_incident_id  = lst$linked_incident_id
    )),
    class = c("ai_improvement", "aitrace_object")
  )
}

register_restorer("ai_improvement", improvement_restore)

#' @export
format.ai_improvement <- function(x, ...) {
  cli::format_inline(
    "{.strong <ai_improvement>} {x$id} [{x$domain}/{x$priority}/{x$status}]"
  )
}
