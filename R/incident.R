#' Construct an AI incident
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param system_id Identifier of the [ai_system()] this record belongs to.
#' @param category Controlled category value.
#' @param severity Controlled severity level.
#' @param description Character description.
#' @param state New lifecycle state.
#' @param owner Optional character owner identifier.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `ai_incident` object.
#' @export
ai_incident <- function(id = NULL, system_id, category = "PERFORMANCE_DEGRADATION",
                        severity = "MEDIUM", description,
                        state = "DETECTED", owner = NULL,
                        metadata = list(), provenance = list()) {
  assert_scalar_character(system_id, arg = "system_id")
  assert_scalar_character(description, arg = "description")
  assert_choice(category, INCIDENT_CATEGORIES, arg = "category")
  assert_choice(severity, INCIDENT_SEVERITY_LEVELS, arg = "severity")
  assert_choice(state, INCIDENT_LIFECYCLE_STATES, arg = "state")
  if (is.null(id)) id <- new_id("INC")
  new_aitrace_object(
    "ai_incident", id,
    fields = list(
      system_id            = system_id,
      category             = category,
      severity             = severity,
      description          = description,
      root_cause           = NULL,
      corrective_actions   = list(),
      preventive_actions   = list(),
      linked_improvement_id = NULL,
      detected_at          = now_utc(),
      closed_at            = NULL,
      closed_by            = NULL,
      resolution_summary   = NULL
    ),
    owner = owner, status = state, metadata = metadata, provenance = provenance
  )
}

#' Attach an incident to a system
#' @param system An [ai_system()].
#' @param incident An [ai_incident()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
add_incident <- function(system, incident, actor = NULL) {
  assert_class(incident, "ai_incident")
  if (!identical(incident$system_id, system$id)) {
    abort_aitrace(
      sprintf('incident$system_id ("%s") does not match system$id ("%s").',
              incident$system_id, system$id),
      class = "aitrace_error_mismatch"
    )
  }
  add_to_slot(system, "incidents", incident,
              note = sprintf("incident added: %s [%s/%s]", incident$id, incident$category, incident$severity),
              actor = actor)
}

#' Change incident lifecycle state
#' @param incident An [ai_incident()].
#' @param state New lifecycle state.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @param note Optional character note recorded in the change log.
#' @return The updated `ai_incident`.
#' @export
set_incident_state <- function(incident, state, actor = NULL, note = NULL) {
  assert_class(incident, "ai_incident")
  validate_transition(incident$status, state, .incident_lifecycle_transitions, INCIDENT_LIFECYCLE_STATES)
  incident$status <- state
  bump_version(incident, note = note %||% sprintf("state -> %s", state), actor = actor)
}

#' Record root cause (generic)
#' @param x An [ai_incident()] or [ai_improvement()].
#' @param root_cause Character description of the identified root cause.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @param ... Not currently used.
#' @return The updated `ai_incident` or `ai_improvement`.
#' @export
set_root_cause <- function(x, root_cause, actor = NULL, ...) UseMethod("set_root_cause")

#' @export
set_root_cause.ai_incident <- function(x, root_cause, actor = NULL, ...) {
  assert_scalar_character(root_cause, arg = "root_cause")
  x$root_cause <- root_cause
  x <- bump_version(x, note = "root_cause recorded", actor = actor)
  if (x$status %in% c("INVESTIGATING", "CONTAINED")) {
    x <- set_incident_state(x, "ROOT_CAUSE_IDENTIFIED", actor = actor)
  }
  x
}

#' @export
set_root_cause.default <- function(x, root_cause, actor = NULL, ...) {
  abort_aitrace(sprintf("set_root_cause() not defined for %s.", obj_desc(x)),
                class = "aitrace_error_type")
}

#' Add a corrective action
#' @param incident An [ai_incident()].
#' @param action Character description of the action taken.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_incident`.
#' @export
add_corrective_action <- function(incident, action, actor = NULL) {
  assert_class(incident, "ai_incident")
  assert_scalar_character(action, arg = "action")
  incident$corrective_actions <- c(incident$corrective_actions, list(action))
  bump_version(incident, note = "corrective action added", actor = actor)
}

#' Add a preventive action
#' @param incident An [ai_incident()].
#' @param action Character description of the action taken.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_incident`.
#' @export
add_preventive_action <- function(incident, action, actor = NULL) {
  assert_class(incident, "ai_incident")
  assert_scalar_character(action, arg = "action")
  incident$preventive_actions <- c(incident$preventive_actions, list(action))
  bump_version(incident, note = "preventive action added", actor = actor)
}

#' Link an improvement to an incident
#' @param incident An [ai_incident()].
#' @param improvement_id Identifier of the linked [ai_improvement()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_incident`.
#' @export
link_improvement <- function(incident, improvement_id, actor = NULL) {
  assert_class(incident, "ai_incident")
  assert_scalar_character(improvement_id, arg = "improvement_id")
  incident$linked_improvement_id <- improvement_id
  bump_version(incident, note = sprintf("linked improvement: %s", improvement_id), actor = actor)
}

#' Close an incident
#' @param incident An [ai_incident()].
#' @param summary Optional character resolution summary.
#' @param closed_by Optional character identifying who closed the incident.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_incident`.
#' @export
close_incident <- function(incident, summary = NULL, closed_by = NULL, actor = NULL) {
  assert_class(incident, "ai_incident")
  incident$resolution_summary <- summary
  incident$closed_at <- now_utc()
  incident$closed_by <- closed_by %||% actor
  set_incident_state(incident, "CLOSED", actor = actor, note = "incident closed")
}

#' Tabulate incidents
#' @param system An [ai_system()].
#' @return A [tibble::tibble()] summarising the system's incidents.
#' @export
incident_table <- function(system) {
  assert_class(system, "ai_system")
  items <- unname(system$incidents)
  if (length(items) == 0) {
    return(tibble::tibble(id = character(), category = character(),
                          severity = character(), status = character()))
  }
  tibble::tibble(
    id       = vapply(items, `[[`, character(1), "id"),
    category = vapply(items, `[[`, character(1), "category"),
    severity = vapply(items, `[[`, character(1), "severity"),
    status   = vapply(items, `[[`, character(1), "status")
  )
}

#' @keywords internal
#' @noRd
incident_restore <- function(lst) {
  env <- restore_envelope(lst)
  ts <- function(v) {
    if (is.null(v)) return(NULL)
    as.POSIXct(v, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  structure(
    c(env, list(
      system_id             = lst$system_id,
      category              = lst$category,
      severity              = lst$severity,
      description           = lst$description,
      root_cause            = lst$root_cause,
      corrective_actions    = lst$corrective_actions %||% list(),
      preventive_actions    = lst$preventive_actions %||% list(),
      linked_improvement_id = lst$linked_improvement_id,
      detected_at           = ts(lst$detected_at) %||% now_utc(),
      closed_at             = ts(lst$closed_at),
      closed_by             = lst$closed_by,
      resolution_summary    = lst$resolution_summary
    )),
    class = c("ai_incident", "aitrace_object")
  )
}

register_restorer("ai_incident", incident_restore)

#' @export
format.ai_incident <- function(x, ...) {
  cli::format_inline(
    "{.strong <ai_incident>} {x$id} [{x$category}/{x$severity}/{x$status}]"
  )
}
