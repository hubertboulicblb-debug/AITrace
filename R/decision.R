#' Construct a first-class governance decision
#'
#' A durable record of a governance action. Decisions are governance metadata
#' (not material composition) and do not change the system fingerprint.
#' Status is never inferred from evaluations; the caller records the outcome.
#'
#' @param decision_type One of [GOVERNANCE_DECISION_LEVELS].
#' @param system_id,system_version Binding to the governed system revision.
#' @param rationale Character explanation (required for OVERRIDE_POLICY).
#' @param evidence_refs,evaluation_refs,risk_refs Character vectors of ids.
#' @param actor,authority Who decided / under what authority.
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param system_fingerprint Material fingerprint the record is bound to (see [ai_system_fingerprint()]).
#' @param decision_status One of [DECISION_STATUS_LEVELS].
#' @param change_id Identifier of the related [ai_change()].
#' @param improvement_id Identifier of the linked [ai_improvement()].
#' @param decided_at When the decision was made (`POSIXct`); defaults to now (UTC).
#' @param owner Optional character owner identifier.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `ai_decision` object.
#' @export
ai_decision <- function(id = NULL,
                        system_id,
                        system_version = NULL,
                        system_fingerprint = NULL,
                        decision_type = "APPROVE",
                        decision_status = "RECORDED",
                        rationale = NULL,
                        evidence_refs = character(0),
                        evaluation_refs = character(0),
                        risk_refs = character(0),
                        change_id = NULL,
                        improvement_id = NULL,
                        actor = NULL,
                        authority = NULL,
                        decided_at = NULL,
                        owner = NULL,
                        metadata = list(),
                        provenance = list()) {
  assert_scalar_character(system_id, arg = "system_id")
  assert_choice(decision_type, GOVERNANCE_DECISION_LEVELS, arg = "decision_type")
  assert_choice(decision_status, DECISION_STATUS_LEVELS, arg = "decision_status")
  if (!is.null(rationale)) assert_scalar_character(rationale, arg = "rationale")
  if (identical(decision_type, "OVERRIDE_POLICY") &&
      (is.null(rationale) || !nzchar(rationale))) {
    abort_aitrace(
      "OVERRIDE_POLICY decisions require a non-empty rationale.",
      class = "aitrace_error_policy"
    )
  }
  if (!is.null(change_id)) assert_scalar_character(change_id, arg = "change_id")
  if (!is.null(improvement_id)) assert_scalar_character(improvement_id, arg = "improvement_id")
  if (!is.null(actor)) assert_scalar_character(actor, arg = "actor")
  if (!is.null(authority)) assert_scalar_character(authority, arg = "authority")
  if (is.null(decided_at)) decided_at <- now_utc()
  if (is.null(id)) id <- new_id("DEC")

  new_aitrace_object(
    "ai_decision", id,
    fields = list(
      system_id           = system_id,
      system_version      = if (is.null(system_version)) NULL else as.integer(system_version),
      system_fingerprint  = system_fingerprint,
      decision_type       = decision_type,
      rationale           = rationale,
      evidence_refs       = as.character(evidence_refs),
      evaluation_refs     = as.character(evaluation_refs),
      risk_refs           = as.character(risk_refs),
      change_id           = change_id,
      improvement_id      = improvement_id,
      actor               = actor,
      authority           = authority,
      decided_at          = decided_at
    ),
    owner = owner, status = decision_status,
    metadata = metadata, provenance = provenance
  )
}

#' Update decision status
#' @param decision One of [GOVERNANCE_DECISION_LEVELS].
#' @param status New status value; validated against the relevant controlled vocabulary.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @param note Optional character note recorded in the change log.
#' @return The updated `ai_decision`.
#' @export
set_decision_status <- function(decision, status, actor = NULL, note = NULL) {
  assert_class(decision, "ai_decision")
  assert_choice(status, DECISION_STATUS_LEVELS, arg = "status")
  decision$status <- status
  bump_version(decision, note = note %||% sprintf("status -> %s", status), actor = actor)
}

#' Attach a decision to a system (stamps version/fingerprint of resulting system)
#' @param system An [ai_system()].
#' @param decision One of [GOVERNANCE_DECISION_LEVELS].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`, with the decision stamped to its resulting version and fingerprint.
#' @export
add_decision <- function(system, decision, actor = NULL) {
  assert_class(system, "ai_system")
  assert_class(decision, "ai_decision")
  if (!identical(decision$system_id, system$id)) {
    abort_aitrace(
      sprintf('decision$system_id ("%s") does not match system$id ("%s").',
              decision$system_id, system$id),
      class = "aitrace_error_mismatch"
    )
  }
  did <- decision$id
  system <- add_to_slot(
    system, "decisions", decision,
    note = sprintf("decision added: %s [%s/%s]", decision$id, decision$decision_type, decision$status),
    actor = actor
  )
  # Bind to resulting revision (governance metadata; non-material)
  system$decisions[[did]]$system_version     <- as.integer(system$version)
  system$decisions[[did]]$system_fingerprint <- ai_system_fingerprint(system)
  system
}

#' Record a governance decision on a system in one step
#'
#' Convenience wrapper: builds [ai_decision()], attaches it, returns system.
#' Does not by itself change lifecycle state — use registry transitions for that.
#' @param system An [ai_system()].
#' @param decision_type One of [GOVERNANCE_DECISION_LEVELS].
#' @param rationale Character rationale (required, non-empty, for `OVERRIDE_POLICY`).
#' @param evidence_refs Character vector of related evidence ids.
#' @param evaluation_refs Character vector of related evaluation ids.
#' @param risk_refs Character vector of related risk ids.
#' @param change_id Identifier of the related [ai_change()].
#' @param improvement_id Identifier of the linked [ai_improvement()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @param authority Optional character identifying the authority under which the decision was made.
#' @param decision_status One of [DECISION_STATUS_LEVELS].
#' @return The updated `ai_system`.
#' @export
record_system_decision <- function(system,
                                   decision_type,
                                   rationale = NULL,
                                   evidence_refs = character(0),
                                   evaluation_refs = character(0),
                                   risk_refs = character(0),
                                   change_id = NULL,
                                   improvement_id = NULL,
                                   actor = NULL,
                                   authority = NULL,
                                   decision_status = "RECORDED") {
  assert_class(system, "ai_system")
  dec <- ai_decision(
    system_id = system$id,
    system_version = system$version,
    system_fingerprint = ai_system_fingerprint(system),
    decision_type = decision_type,
    decision_status = decision_status,
    rationale = rationale,
    evidence_refs = evidence_refs,
    evaluation_refs = evaluation_refs,
    risk_refs = risk_refs,
    change_id = change_id,
    improvement_id = improvement_id,
    actor = actor,
    authority = authority
  )
  add_decision(system, dec, actor = actor)
}

#' Tabulate decisions on a system
#' @param system An [ai_system()].
#' @return A [tibble::tibble()] summarising the system's decisions.
#' @export
decision_table <- function(system) {
  assert_class(system, "ai_system")
  items <- unname(system$decisions %||% list())
  if (length(items) == 0) {
    return(tibble::tibble(
      id = character(), decision_type = character(), status = character(),
      actor = character(), system_version = integer()
    ))
  }
  tibble::tibble(
    id              = vapply(items, `[[`, character(1), "id"),
    decision_type   = vapply(items, `[[`, character(1), "decision_type"),
    status          = vapply(items, `[[`, character(1), "status"),
    actor           = vapply(items, function(x) x$actor %||% NA_character_, character(1)),
    system_version  = vapply(items, function(x) as.integer(x$system_version %||% NA_integer_), integer(1))
  )
}

#' @keywords internal
#' @noRd
decision_restore <- function(lst) {
  env <- restore_envelope(lst)
  ts <- function(v) {
    if (is.null(v)) return(NULL)
    if (inherits(v, "POSIXt")) return(v)
    as.POSIXct(v, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  structure(
    c(env, list(
      system_id          = lst$system_id,
      system_version     = if (is.null(lst$system_version)) NULL else as.integer(lst$system_version),
      system_fingerprint = lst$system_fingerprint,
      decision_type      = lst$decision_type,
      rationale          = lst$rationale,
      evidence_refs      = as.character(lst$evidence_refs %||% character(0)),
      evaluation_refs    = as.character(lst$evaluation_refs %||% character(0)),
      risk_refs          = as.character(lst$risk_refs %||% character(0)),
      change_id          = lst$change_id,
      improvement_id     = lst$improvement_id,
      actor              = lst$actor,
      authority          = lst$authority,
      decided_at         = ts(lst$decided_at) %||% now_utc()
    )),
    class = c("ai_decision", "aitrace_object")
  )
}

register_restorer("ai_decision", decision_restore)

#' @export
format.ai_decision <- function(x, ...) {
  cli::format_inline(
    "{.strong <ai_decision>} {x$id} [{x$decision_type}/{x$status}]"
  )
}
