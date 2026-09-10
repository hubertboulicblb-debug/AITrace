#' Construct a first-class AI system change record
#'
#' Bridges what changed (versions, components) with why (decision, improvement,
#' evidence, risk). Changes are governance metadata and do not alter the
#' material fingerprint.
#'
#' @param from_version,to_version Integer system versions bracketing the change.
#' @param change_type One of [CHANGE_TYPE_LEVELS].
#' @param changed_components Character vector of component ids affected.
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param system_id Identifier of the [ai_system()] this record belongs to.
#' @param from_fingerprint Material fingerprint before the change (see [ai_system_fingerprint()]).
#' @param to_fingerprint Material fingerprint after the change (see [ai_system_fingerprint()]).
#' @param change_status One of [CHANGE_STATUS_LEVELS].
#' @param reason Optional character explanation.
#' @param impact One of [CHANGE_IMPACT_LEVELS].
#' @param improvement_id Identifier of the linked [ai_improvement()].
#' @param decision_id Identifier of the related [ai_decision()].
#' @param risk_refs Character vector of related risk ids.
#' @param evidence_refs Character vector of related evidence ids.
#' @param evaluation_refs Character vector of related evaluation ids.
#' @param deployment_status One of [DEPLOYMENT_STATUS_LEVELS].
#' @param verification_status One of [VERIFICATION_RESULT_LEVELS].
#' @param owner Optional character owner identifier.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `ai_change` object.
#' @export
ai_change <- function(id = NULL,
                      system_id,
                      from_version = NULL,
                      to_version = NULL,
                      from_fingerprint = NULL,
                      to_fingerprint = NULL,
                      change_type = "OTHER",
                      change_status = "PROPOSED",
                      changed_components = character(0),
                      reason = NULL,
                      impact = "MINOR",
                      improvement_id = NULL,
                      decision_id = NULL,
                      risk_refs = character(0),
                      evidence_refs = character(0),
                      evaluation_refs = character(0),
                      deployment_status = NULL,
                      verification_status = NULL,
                      owner = NULL,
                      metadata = list(),
                      provenance = list()) {
  assert_scalar_character(system_id, arg = "system_id")
  assert_choice(change_type, CHANGE_TYPE_LEVELS, arg = "change_type")
  assert_choice(change_status, CHANGE_STATUS_LEVELS, arg = "change_status")
  assert_choice(impact, CHANGE_IMPACT_LEVELS, arg = "impact")
  if (!is.null(reason)) assert_scalar_character(reason, arg = "reason")
  if (!is.null(improvement_id)) assert_scalar_character(improvement_id, arg = "improvement_id")
  if (!is.null(decision_id)) assert_scalar_character(decision_id, arg = "decision_id")
  if (!is.null(deployment_status)) {
    assert_choice(deployment_status, DEPLOYMENT_STATUS_LEVELS,
                  arg = "deployment_status")
  }
  if (!is.null(verification_status)) {
    assert_choice(verification_status, VERIFICATION_RESULT_LEVELS, arg = "verification_status")
  }
  if (is.null(id)) id <- new_id("CHG")

  new_aitrace_object(
    "ai_change", id,
    fields = list(
      system_id           = system_id,
      from_version        = if (is.null(from_version)) NULL else as.integer(from_version),
      to_version          = if (is.null(to_version)) NULL else as.integer(to_version),
      from_fingerprint    = from_fingerprint,
      to_fingerprint      = to_fingerprint,
      change_type         = change_type,
      changed_components  = as.character(changed_components),
      reason              = reason,
      impact              = impact,
      improvement_id      = improvement_id,
      decision_id         = decision_id,
      risk_refs           = as.character(risk_refs),
      evidence_refs       = as.character(evidence_refs),
      evaluation_refs     = as.character(evaluation_refs),
      deployment_status   = deployment_status,
      verification_status = verification_status
    ),
    owner = owner, status = change_status,
    metadata = metadata, provenance = provenance
  )
}

#' Update change status
#' @param change An [ai_change()].
#' @param status New status value; validated against the relevant controlled vocabulary.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @param note Optional character note recorded in the change log.
#' @return The updated `ai_change`.
#' @export
set_change_status <- function(change, status, actor = NULL, note = NULL) {
  assert_class(change, "ai_change")
  assert_choice(status, CHANGE_STATUS_LEVELS, arg = "status")
  change$status <- status
  bump_version(change, note = note %||% sprintf("status -> %s", status), actor = actor)
}

#' Record deployment outcome on a change
#' @param change An [ai_change()].
#' @param deployment_status One of [DEPLOYMENT_STATUS_LEVELS].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_change`.
#' @export
set_change_deployment <- function(change, deployment_status, actor = NULL) {
  assert_class(change, "ai_change")
  assert_choice(deployment_status, DEPLOYMENT_STATUS_LEVELS,
                arg = "deployment_status")
  change$deployment_status <- deployment_status
  if (identical(deployment_status, "DEPLOYED") && change$status %in% c("APPROVED", "IN_PROGRESS")) {
    change$status <- "DEPLOYED"
  }
  bump_version(change, note = sprintf("deployment_status -> %s", deployment_status), actor = actor)
}

#' Record verification outcome on a change
#' @param change An [ai_change()].
#' @param verification_status One of [VERIFICATION_RESULT_LEVELS].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_change`.
#' @export
set_change_verification <- function(change, verification_status, actor = NULL) {
  assert_class(change, "ai_change")
  assert_choice(verification_status, VERIFICATION_RESULT_LEVELS, arg = "verification_status")
  change$verification_status <- verification_status
  if (identical(verification_status, "CONFIRMED") && identical(change$status, "DEPLOYED")) {
    change$status <- "VERIFIED"
  }
  bump_version(change, note = sprintf("verification_status -> %s", verification_status), actor = actor)
}

#' Attach a change record to a system
#' @param system An [ai_system()].
#' @param change An [ai_change()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
add_change <- function(system, change, actor = NULL) {
  assert_class(system, "ai_system")
  assert_class(change, "ai_change")
  if (!identical(change$system_id, system$id)) {
    abort_aitrace(
      sprintf('change$system_id ("%s") does not match system$id ("%s").',
              change$system_id, system$id),
      class = "aitrace_error_mismatch"
    )
  }
  add_to_slot(
    system, "changes", change,
    note = sprintf("change added: %s [%s/%s]", change$id, change$change_type, change$status),
    actor = actor
  )
}

#' Open a change against the current system state (captures from_version/fingerprint)
#'
#' After material mutations, call [close_change()] to stamp to_version / to_fingerprint.
#' @param system An [ai_system()].
#' @param change_type One of [CHANGE_TYPE_LEVELS].
#' @param reason Optional character explanation.
#' @param impact One of [CHANGE_IMPACT_LEVELS].
#' @param changed_components Character vector of component ids affected by the change.
#' @param improvement_id Identifier of the linked [ai_improvement()].
#' @param decision_id Identifier of the related [ai_decision()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`, carrying the newly opened `ai_change`.
#' @export
open_change <- function(system,
                        change_type = "OTHER",
                        reason = NULL,
                        impact = "MINOR",
                        changed_components = character(0),
                        improvement_id = NULL,
                        decision_id = NULL,
                        actor = NULL) {
  assert_class(system, "ai_system")
  ch <- ai_change(
    system_id = system$id,
    from_version = system$version,
    from_fingerprint = ai_system_fingerprint(system),
    change_type = change_type,
    change_status = "IN_PROGRESS",
    changed_components = changed_components,
    reason = reason,
    impact = impact,
    improvement_id = improvement_id,
    decision_id = decision_id
  )
  add_change(system, ch, actor = actor)
}

#' Close an open change by stamping the *resulting* system version/fingerprint
#'
#' Sequence: bump system (governance close) → stamp change with resulting
#' identity → store. Invariant: `change$to_version == system$version` and
#' `change$to_fingerprint == ai_system_fingerprint(system)` on return.
#' Material fingerprint is unchanged by this call (change is non-material).
#' @param system An [ai_system()].
#' @param change_id Identifier of the related [ai_change()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`, with the change stamped to its resulting version and fingerprint.
#' @export
close_change <- function(system, change_id, actor = NULL) {
  assert_class(system, "ai_system")
  assert_scalar_character(change_id, arg = "change_id")
  ch <- system$changes[[change_id]]
  if (is.null(ch)) {
    abort_aitrace(sprintf("Change \"%s\" not found on system.", change_id),
                  class = "aitrace_error_not_found")
  }
  # Governance close first → resulting revision
  system <- bump_version(system, note = sprintf("change closed: %s", change_id), actor = actor)
  # Stamp against the resulting system (not the pre-close revision)
  ch$to_version     <- as.integer(system$version)
  ch$to_fingerprint <- ai_system_fingerprint(system)
  if (identical(ch$status, "IN_PROGRESS")) {
    ch$status <- "APPROVED"
  }
  system$changes[[change_id]] <- bump_version(
    ch, note = "change closed with resulting version/fingerprint", actor = actor
  )
  system
}

#' Tabulate changes on a system
#' @param system An [ai_system()].
#' @return A [tibble::tibble()] summarising the system's changes.
#' @export
change_table <- function(system) {
  assert_class(system, "ai_system")
  items <- unname(system$changes %||% list())
  if (length(items) == 0) {
    return(tibble::tibble(
      id = character(), change_type = character(), status = character(),
      from_version = integer(), to_version = integer(), impact = character()
    ))
  }
  tibble::tibble(
    id           = vapply(items, `[[`, character(1), "id"),
    change_type  = vapply(items, `[[`, character(1), "change_type"),
    status       = vapply(items, `[[`, character(1), "status"),
    from_version = vapply(items, function(x) as.integer(x$from_version %||% NA_integer_), integer(1)),
    to_version   = vapply(items, function(x) as.integer(x$to_version %||% NA_integer_), integer(1)),
    impact       = vapply(items, `[[`, character(1), "impact")
  )
}

#' @keywords internal
#' @noRd
change_restore <- function(lst) {
  env <- restore_envelope(lst)
  structure(
    c(env, list(
      system_id           = lst$system_id,
      from_version        = if (is.null(lst$from_version)) NULL else as.integer(lst$from_version),
      to_version          = if (is.null(lst$to_version)) NULL else as.integer(lst$to_version),
      from_fingerprint    = lst$from_fingerprint,
      to_fingerprint      = lst$to_fingerprint,
      change_type         = lst$change_type,
      changed_components  = as.character(lst$changed_components %||% character(0)),
      reason              = lst$reason,
      impact              = lst$impact,
      improvement_id      = lst$improvement_id,
      decision_id         = lst$decision_id,
      risk_refs           = as.character(lst$risk_refs %||% character(0)),
      evidence_refs       = as.character(lst$evidence_refs %||% character(0)),
      evaluation_refs     = as.character(lst$evaluation_refs %||% character(0)),
      deployment_status   = lst$deployment_status,
      verification_status = lst$verification_status
    )),
    class = c("ai_change", "aitrace_object")
  )
}

register_restorer("ai_change", change_restore)

#' @export
format.ai_change <- function(x, ...) {
  cli::format_inline(
    "{.strong <ai_change>} {x$id} [{x$change_type}/{x$status}] v{x$from_version %||% '?'} -> v{x$to_version %||% '?'}"
  )
}
