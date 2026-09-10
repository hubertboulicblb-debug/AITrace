.aitrace_system_slots <- list(
  models       = "ai_model",
  data         = "ai_data",
  prompts      = "ai_component",
  retrieval    = "ai_component",
  tools        = "ai_component",
  guardrails   = "ai_component",
  risks        = "ai_risk",
  evidence     = "ai_evidence",
  incidents    = "ai_incident",
  improvements = "ai_improvement",
  evidence_requirements = "evidence_requirement",
  evaluations = "evaluation",
  baseline_comparisons = "baseline_comparison",
  decisions = "ai_decision",
  changes = "ai_change",
  observations = "ai_observation",
  monitoring_specs = "monitoring_spec",
  drift_assessments = "drift_assessment",
  detections = "detection_signal"
)

#' Construct an AI system (complete governed object)
#'
#' The central domain object. An `ai_system` represents the complete
#' governed AI system — model, data, prompts, retrieval, tools, guardrails,
#' runtime, human oversight, evidence, risks, incidents and improvements.
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param name Character name.
#' @param purpose Character description of the system's purpose.
#' @param description Character description.
#' @param risk_class One of [RISK_CLASS_LEVELS].
#' @param lifecycle_state Initial lifecycle state; one of [SYSTEM_LIFECYCLE_STATES].
#' @param owner Optional character owner identifier.
#' @param business_owner Optional character business owner.
#' @param technical_owner Optional character technical owner.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `ai_system` object.
#' @export
ai_system <- function(id = NULL, name, purpose,
                      description = NULL,
                      risk_class = "MEDIUM",
                      lifecycle_state = "DRAFT",
                      owner = NULL,
                      business_owner = NULL, technical_owner = NULL,
                      metadata = list(), provenance = list()) {
  assert_scalar_character(name, arg = "name")
  assert_scalar_character(purpose, arg = "purpose")
  assert_choice(risk_class, RISK_CLASS_LEVELS, arg = "risk_class")
  assert_choice(lifecycle_state, SYSTEM_LIFECYCLE_STATES, arg = "lifecycle_state")
  if (is.null(id)) id <- new_id("SYS")
  new_aitrace_object(
    "ai_system", id,
    fields = list(
      name            = name,
      purpose         = purpose,
      description     = description,
      risk_class      = risk_class,
      business_owner  = business_owner,
      technical_owner = technical_owner,
      models          = list(),
      data            = list(),
      prompts         = list(),
      retrieval       = list(),
      tools           = list(),
      guardrails      = list(),
      runtime         = NULL,
      human_oversight = NULL,
      deployment_context = NULL,
      risks           = list(),
      evidence        = list(),
      incidents       = list(),
      improvements    = list(),
      evidence_requirements = list(),
      evaluations     = list(),
      baseline_comparisons = list(),
      decisions       = list(),
      changes         = list(),
      observations    = list(),
      monitoring_specs = list(),
      drift_assessments = list(),
      detections      = list()
    ),
    owner = owner, status = lifecycle_state,
    metadata = metadata, provenance = provenance
  )
}

#' @keywords internal
#' @noRd
add_to_slot <- function(system, slot, obj, note = NULL, actor = NULL) {
  assert_class(system, "ai_system")
  expected <- .aitrace_system_slots[[slot]]
  assert_class(obj, expected)
  if (obj$id %in% names(system[[slot]])) {
    abort_aitrace(
      sprintf("Object with id \"%s\" already present in slot \"%s\".", obj$id, slot),
      class = "aitrace_error_duplicate"
    )
  }
  system[[slot]][[obj$id]] <- obj
  bump_version(system, note = note %||% sprintf("%s added: %s", slot, obj$id), actor = actor)
}

#' Add a model to a system
#' @param system An [ai_system()].
#' @param model An [ai_model()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
add_model <- function(system, model, actor = NULL) {
  assert_class(model, "ai_model")
  add_to_slot(system, "models", model, note = sprintf("model added: %s", model$id), actor = actor)
}

#' Add a data asset
#' @param system An [ai_system()].
#' @param data An [ai_data()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
add_data <- function(system, data, actor = NULL) {
  assert_class(data, "ai_data")
  add_to_slot(system, "data", data, note = sprintf("data added: %s", data$id), actor = actor)
}

#' Add evidence (stamps version-binding provenance on the *resulting* system)
#'
#' Evidence is bound to the system identity, version and material fingerprint
#' **after** the attachment mutation. Invariant: newly attached evidence is
#' immediately applicable to the returned system (not stamped with the
#' pre-mutation version).
#'
#' Version = governed object revision; fingerprint = material composition
#' only (evidence itself is not material, so fingerprint is unchanged by
#' this call).
#' @param system An [ai_system()].
#' @param evidence An [ai_evidence()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`, with the evidence stamped to its resulting version and fingerprint.
#' @export
add_evidence <- function(system, evidence, actor = NULL) {
  assert_class(evidence, "ai_evidence")
  eid <- evidence$id
  system <- add_to_slot(
    system, "evidence", evidence,
    note = sprintf("evidence added: %s [%s/%s]",
                   evidence$id, evidence$category, evidence$status),
    actor = actor
  )
  # Stamp with the *resulting* version and material fingerprint
  system$evidence[[eid]]$system_id          <- system$id
  system$evidence[[eid]]$system_version     <- as.integer(system$version)
  system$evidence[[eid]]$system_fingerprint <- ai_system_fingerprint(system)
  system
}

#' Add a risk
#' @param system An [ai_system()].
#' @param risk An [ai_risk()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
add_risk <- function(system, risk, actor = NULL) {
  assert_class(risk, "ai_risk")
  add_to_slot(system, "risks", risk,
              note = sprintf("risk added: %s [%s]", risk$id, risk$category), actor = actor)
}

#' Add a prompt component
#' @param system An [ai_system()].
#' @param component An [ai_component()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
add_prompt <- function(system, component, actor = NULL) {
  assert_class(component, "ai_component")
  if (!identical(component$kind, "PROMPT")) {
    abort_aitrace("add_prompt() requires kind = \"PROMPT\".", class = "aitrace_error_type")
  }
  add_to_slot(system, "prompts", component, actor = actor)
}

#' Add a retrieval component
#' @param system An [ai_system()].
#' @param component An [ai_component()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
add_retrieval <- function(system, component, actor = NULL) {
  assert_class(component, "ai_component")
  if (!identical(component$kind, "RETRIEVAL")) {
    abort_aitrace("add_retrieval() requires kind = \"RETRIEVAL\".", class = "aitrace_error_type")
  }
  add_to_slot(system, "retrieval", component, actor = actor)
}

#' Add a tool component
#' @param system An [ai_system()].
#' @param component An [ai_component()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
add_tool <- function(system, component, actor = NULL) {
  assert_class(component, "ai_component")
  if (!identical(component$kind, "TOOL")) {
    abort_aitrace("add_tool() requires kind = \"TOOL\".", class = "aitrace_error_type")
  }
  add_to_slot(system, "tools", component, actor = actor)
}

#' Add a guardrail component
#' @param system An [ai_system()].
#' @param component An [ai_component()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
add_guardrail <- function(system, component, actor = NULL) {
  assert_class(component, "ai_component")
  if (!identical(component$kind, "GUARDRAIL")) {
    abort_aitrace("add_guardrail() requires kind = \"GUARDRAIL\".", class = "aitrace_error_type")
  }
  add_to_slot(system, "guardrails", component, actor = actor)
}

#' Set runtime
#' @param system An [ai_system()].
#' @param component An [ai_component()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
set_runtime <- function(system, component, actor = NULL) {
  assert_class(system, "ai_system")
  assert_class(component, "ai_component")
  if (!identical(component$kind, "RUNTIME")) {
    abort_aitrace("set_runtime() requires kind = \"RUNTIME\".", class = "aitrace_error_type")
  }
  system$runtime <- component
  bump_version(system, note = sprintf("runtime set: %s", component$id), actor = actor)
}

#' Set human oversight description
#' @param system An [ai_system()].
#' @param oversight Character description of the human oversight arrangement.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
set_human_oversight <- function(system, oversight, actor = NULL) {
  assert_class(system, "ai_system")
  assert_scalar_character(oversight, arg = "oversight")
  system$human_oversight <- oversight
  bump_version(system, note = "human_oversight updated", actor = actor)
}

#' Set deployment context
#' @param system An [ai_system()].
#' @param context Character description of the deployment context.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
set_deployment_context <- function(system, context, actor = NULL) {
  assert_class(system, "ai_system")
  system$deployment_context <- context
  bump_version(system, note = "deployment_context updated", actor = actor)
}

#' Remove a component by id from any slot
#' @param system An [ai_system()].
#' @param id Identifier of the component to remove.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
remove_component <- function(system, id, actor = NULL) {
  assert_class(system, "ai_system")
  assert_scalar_character(id, arg = "id")
  found <- FALSE
  for (slot in names(.aitrace_system_slots)) {
    if (id %in% names(system[[slot]])) {
      system[[slot]][[id]] <- NULL
      found <- TRUE
      break
    }
  }
  if (!found && !is.null(system$runtime) && identical(system$runtime$id, id)) {
    system$runtime <- NULL
    found <- TRUE
  }
  if (!found) {
    abort_aitrace(sprintf("No component with id \"%s\" found.", id),
                  class = "aitrace_error_not_found")
  }
  bump_version(system, note = sprintf("component removed: %s", id), actor = actor)
}

#' Get a component by id
#' @param system An [ai_system()].
#' @param id Identifier of the component to look up.
#' @return The matching component object, or `NULL` if not found.
#' @export
get_component <- function(system, id) {
  assert_class(system, "ai_system")
  for (slot in names(.aitrace_system_slots)) {
    if (id %in% names(system[[slot]])) return(system[[slot]][[id]])
  }
  if (!is.null(system$runtime) && identical(system$runtime$id, id)) return(system$runtime)
  NULL
}

# States that may only be reached through the registry control plane
# (evidence gates + audit). Domain-level set_lifecycle_state() refuses them.
.critical_lifecycle_states <- c("APPROVED", "DEPLOYED")

#' Change the system lifecycle state (domain-level)
#'
#' Non-critical transitions are applied directly. Critical transitions
#' (`APPROVED`, `DEPLOYED`) are **refused** at the domain level so that
#' evidence gates and the audit chain cannot be bypassed. Use the registry
#' operations [system_approve()] / [system_deploy()] (or
#' [registry_transition()]) for those states.
#'
#' @param system An [ai_system()].
#' @param state New lifecycle state.
#' @param actor,note Optional provenance for the change log.
#' @param .governance_context Internal flag used by the registry after
#' @return The updated `ai_system`.
#'   policy checks have passed. Not for external callers.
#' @export
set_lifecycle_state <- function(system, state, actor = NULL, note = NULL,
                                .governance_context = FALSE) {
  assert_class(system, "ai_system")
  validate_transition(system$status, state, .system_lifecycle_transitions, SYSTEM_LIFECYCLE_STATES)

  if (state %in% .critical_lifecycle_states && !isTRUE(.governance_context)) {
    abort_aitrace(
      sprintf(
        "Transition to \"%s\" is a critical governance transition and cannot be performed with set_lifecycle_state(). Use system_approve() / system_deploy() / registry_transition() so that evidence gates and the audit chain apply.",
        state
      ),
      class = "aitrace_error_policy"
    )
  }

  system$status <- state
  bump_version(system, note = note %||% sprintf("state -> %s", state), actor = actor)
}

#' States reachable from the current lifecycle state
#' @param system An [ai_system()].
#' @return A character vector of lifecycle states reachable from the current state.
#' @export
allowed_transitions <- function(system) {
  assert_class(system, "ai_system")
  from <- system$status
  allowed <- .system_lifecycle_transitions[[from]]
  if (is.null(allowed)) character(0) else allowed
}

#' Structural validation of an AI system
#'
#' Checks identity, controlled vocabularies, and basic composition.
#' Does not evaluate metrics or invent evidence outcomes.
#' @param system An [ai_system()].
#' @return A list with elements `ok` (logical) and `issues` (character).
#' @export
validate_system <- function(system) {
  assert_class(system, "ai_system")
  issues <- character(0)

  if (!nzchar(system$id %||% "")) issues <- c(issues, "missing id")
  if (!nzchar(system$name %||% "")) issues <- c(issues, "missing name")
  if (!nzchar(system$purpose %||% "")) issues <- c(issues, "missing purpose")
  if (!system$risk_class %in% RISK_CLASS_LEVELS)
    issues <- c(issues, sprintf("invalid risk_class: %s", system$risk_class))
  if (!system$status %in% SYSTEM_LIFECYCLE_STATES)
    issues <- c(issues, sprintf("invalid lifecycle state: %s", system$status))

  # Component type checks
  for (slot in names(.aitrace_system_slots)) {
    expected <- .aitrace_system_slots[[slot]]
    items <- system[[slot]]
    if (length(items) > 0) {
      bad <- !vapply(items, inherits, logical(1), what = expected)
      if (any(bad)) {
        issues <- c(issues, sprintf("slot \"%s\" contains non-<%s> objects", slot, expected))
      }
      ids <- vapply(items, `[[`, character(1), "id")
      if (anyDuplicated(ids)) {
        issues <- c(issues, sprintf("slot \"%s\" has duplicate component ids", slot))
      }
    }
  }

  list(ok = length(issues) == 0L, issues = issues)
}

#' Referential / traceability validation of an AI system
#'
#' Checks that nested objects belong to this system, linked references
#' resolve, and evidence applicability is coherent. Does not evaluate
#' metrics or invent outcomes.
#' @param system An [ai_system()].
#' @return A list with elements `ok` (logical), `issues` (character) and `warnings` (character).
#' @export
validate_traceability <- function(system) {
  assert_class(system, "ai_system")
  issues <- character(0)
  warnings <- character(0)

  # Evidence must belong to this system (when stamped)
  for (ev in system$evidence) {
    if (!is.null(ev$system_id) && !identical(ev$system_id, system$id)) {
      issues <- c(issues, sprintf("evidence %s system_id mismatch (%s)", ev$id, ev$system_id))
    }
    if (!is.null(ev$system_version) &&
        !identical(as.integer(ev$system_version), as.integer(system$version))) {
      warnings <- c(warnings, sprintf(
        "evidence %s bound to version %s; system is at version %s (may not apply to current composition)",
        ev$id, ev$system_version, system$version
      ))
    }
    if (!is.null(ev$system_fingerprint) &&
        !identical(ev$system_fingerprint, ai_system_fingerprint(system))) {
      warnings <- c(warnings, sprintf(
        "evidence %s fingerprint no longer matches current material composition",
        ev$id
      ))
    }
  }

  # Incidents must reference this system
  for (inc in system$incidents) {
    if (!identical(inc$system_id, system$id)) {
      issues <- c(issues, sprintf("incident %s system_id mismatch (%s)", inc$id, inc$system_id))
    }
    lid <- inc$linked_improvement_id
    if (!is.null(lid) && !lid %in% names(system$improvements)) {
      issues <- c(issues, sprintf("incident %s links to missing improvement %s", inc$id, lid))
    }
  }

  # Improvements must reference this system; linked incidents must exist
  for (imp in system$improvements) {
    if (!identical(imp$system_id, system$id)) {
      issues <- c(issues, sprintf("improvement %s system_id mismatch (%s)", imp$id, imp$system_id))
    }
    lid <- imp$linked_incident_id
    if (!is.null(lid) && !lid %in% names(system$incidents)) {
      issues <- c(issues, sprintf("improvement %s links to missing incident %s", imp$id, lid))
    }
  }

  # Decisions must reference this system
  for (dec in system$decisions %||% list()) {
    if (!identical(dec$system_id, system$id)) {
      issues <- c(issues, sprintf("decision %s system_id mismatch (%s)", dec$id, dec$system_id))
    }
    for (rid in dec$evidence_refs %||% character(0)) {
      if (!rid %in% names(system$evidence)) {
        warnings <- c(warnings, sprintf("decision %s refs missing evidence %s", dec$id, rid))
      }
    }
    for (rid in dec$evaluation_refs %||% character(0)) {
      if (!rid %in% names(system$evaluations)) {
        warnings <- c(warnings, sprintf("decision %s refs missing evaluation %s", dec$id, rid))
      }
    }
    if (!is.null(dec$change_id) && !dec$change_id %in% names(system$changes %||% list())) {
      warnings <- c(warnings, sprintf("decision %s refs missing change %s", dec$id, dec$change_id))
    }
  }

  # Changes must reference this system
  for (ch in system$changes %||% list()) {
    if (!identical(ch$system_id, system$id)) {
      issues <- c(issues, sprintf("change %s system_id mismatch (%s)", ch$id, ch$system_id))
    }
    if (!is.null(ch$decision_id) && !ch$decision_id %in% names(system$decisions %||% list())) {
      warnings <- c(warnings, sprintf("change %s refs missing decision %s", ch$id, ch$decision_id))
    }
    if (!is.null(ch$improvement_id) && !ch$improvement_id %in% names(system$improvements)) {
      warnings <- c(warnings, sprintf("change %s refs missing improvement %s", ch$id, ch$improvement_id))
    }
  }

  for (obs in system$observations %||% list()) {
    if (!is.null(obs$system_id) && !identical(obs$system_id, system$id)) {
      issues <- c(issues, sprintf("observation %s system_id mismatch (%s)", obs$id, obs$system_id))
    }
  }
  # Detection/drift cross-references are optional (NULL by default), but once
  # set they are structural pointers within the same system, not staleness
  # hints — so a set-but-dangling reference is an issue, not a warning.
  for (det in system$detections %||% list()) {
    if (!identical(det$system_id, system$id)) {
      issues <- c(issues, sprintf("detection %s system_id mismatch (%s)", det$id, det$system_id))
    }
    if (!is.null(det$incident_id) && !det$incident_id %in% names(system$incidents)) {
      issues <- c(issues, sprintf("detection %s refs missing incident %s", det$id, det$incident_id))
    }
    if (!is.null(det$observation_id) && !det$observation_id %in% names(system$observations %||% list())) {
      issues <- c(issues, sprintf("detection %s refs missing observation %s", det$id, det$observation_id))
    }
    if (!is.null(det$monitoring_id) && !det$monitoring_id %in% names(system$monitoring_specs %||% list())) {
      issues <- c(issues, sprintf("detection %s refs missing monitoring_spec %s", det$id, det$monitoring_id))
    }
    if (!is.null(det$drift_id) && !det$drift_id %in% names(system$drift_assessments %||% list())) {
      issues <- c(issues, sprintf("detection %s refs missing drift_assessment %s", det$id, det$drift_id))
    }
    if (!is.null(det$evaluation_id) && !det$evaluation_id %in% names(system$evaluations %||% list())) {
      issues <- c(issues, sprintf("detection %s refs missing evaluation %s", det$id, det$evaluation_id))
    }
  }
  for (dft in system$drift_assessments %||% list()) {
    if (!is.null(dft$system_id) && !identical(dft$system_id, system$id)) {
      issues <- c(issues, sprintf("drift %s system_id mismatch (%s)", dft$id, dft$system_id))
    }
    if (!is.null(dft$observation_id) && !dft$observation_id %in% names(system$observations %||% list())) {
      issues <- c(issues, sprintf("drift %s refs missing observation %s", dft$id, dft$observation_id))
    }
    if (!is.null(dft$monitoring_id) && !dft$monitoring_id %in% names(system$monitoring_specs %||% list())) {
      issues <- c(issues, sprintf("drift %s refs missing monitoring_spec %s", dft$id, dft$monitoring_id))
    }
  }

  # Cross-slot ID uniqueness
  all_ids <- character(0)
  for (slot in names(.aitrace_system_slots)) {
    items <- system[[slot]]
    if (length(items) > 0) {
      all_ids <- c(all_ids, vapply(items, `[[`, character(1), "id"))
    }
  }
  if (!is.null(system$runtime)) all_ids <- c(all_ids, system$runtime$id)
  if (anyDuplicated(all_ids)) {
    issues <- c(issues, sprintf("duplicate ids across slots: %s",
                                paste(unique(all_ids[duplicated(all_ids)]), collapse = ", ")))
  }

  list(ok = length(issues) == 0L, issues = issues, warnings = warnings)
}

#' Canonical material representation for fingerprinting
#'
#' **Contract:** a fingerprint identifies the *technical/material composition*
#' of an AI system (model, data, prompts, retrieval, tools, guardrails,
#' runtime, plus identity fields id/name/purpose/risk_class). It does **not**
#' include governance state (lifecycle, evidence, risks, incidents,
#' improvements, human oversight, deployment context, owners).
#'
#' All component collections are treated as unordered and sorted by id for
#' stability (order of attachment is not material).
#' @param system An [ai_system()].
#' @return A plain list holding only the material fields, with unordered collections sorted by id.
#' @export
canonicalize_system <- function(system) {
  assert_class(system, "ai_system")
  keep <- c("id", "name", "purpose", "risk_class",
            "models", "data", "prompts", "retrieval", "tools",
            "guardrails", "runtime")
  material <- system[intersect(names(system), keep)]
  # Sort all unordered collections for stable fingerprints
  for (slot in c("models", "data", "tools", "guardrails", "prompts", "retrieval")) {
    if (length(material[[slot]]) > 0) {
      material[[slot]] <- material[[slot]][order(names(material[[slot]]))]
    }
  }
  material
}

#' SHA-256 fingerprint of the system's material composition
#'
#' See [canonicalize_system()] for the explicit contract: material composition
#' only, not governance state.
#' @param system An [ai_system()].
#' @return A single character SHA-256 hex digest.
#' @export
ai_system_fingerprint <- function(system) {
  canonical <- canonicalize_system(system)
  digest::digest(
    jsonlite::toJSON(to_list(canonical), auto_unbox = TRUE, null = "null",
                     dataframe = "rows", digits = NA, pretty = FALSE),
    algo = "sha256"
  )
}

#' Verify a fingerprint
#' @param system An [ai_system()].
#' @param expected Character SHA-256 fingerprint to compare against.
#' @return A single logical value.
#' @export
fingerprint_verify <- function(system, expected) {
  identical(ai_system_fingerprint(system), expected)
}

#' Tabulate evidence attached to a system
#' @param system An [ai_system()].
#' @return A [tibble::tibble()] summarising the system's evidence.
#' @export
evidence_table <- function(system) {
  assert_class(system, "ai_system")
  items <- unname(system$evidence)
  if (length(items) == 0) {
    return(tibble::tibble(id = character(), category = character(),
                          status = character(), test = character(), metric = character()))
  }
  tibble::tibble(
    id       = vapply(items, `[[`, character(1), "id"),
    category = vapply(items, `[[`, character(1), "category"),
    status   = vapply(items, `[[`, character(1), "status"),
    test     = vapply(items, function(x) x$test %||% NA_character_, character(1)),
    metric   = vapply(items, function(x) x$metric %||% NA_character_, character(1))
  )
}

#' Tabulate risks
#' @param system An [ai_system()].
#' @return A [tibble::tibble()] summarising the system's risks.
#' @export
risk_table <- function(system) {
  assert_class(system, "ai_system")
  items <- unname(system$risks)
  if (length(items) == 0) {
    return(tibble::tibble(id = character(), category = character(),
                          severity = character(), status = character()))
  }
  tibble::tibble(
    id       = vapply(items, `[[`, character(1), "id"),
    category = vapply(items, `[[`, character(1), "category"),
    severity = vapply(items, function(x) risk_severity(x$likelihood, x$impact), character(1)),
    status   = vapply(items, `[[`, character(1), "status")
  )
}

#' @keywords internal
#' @noRd
system_restore <- function(lst) {
  env <- restore_envelope(lst)
  restore_slot <- function(slot, restorer) {
    items <- lst[[slot]] %||% list()
    if (length(items) == 0) return(list())
    # items may be named list of plain lists
    stats::setNames(lapply(items, restorer), vapply(items, `[[`, character(1), "id"))
  }
  structure(
    c(env, list(
      name            = lst$name,
      purpose         = lst$purpose,
      description     = lst$description,
      risk_class      = lst$risk_class,
      business_owner  = lst$business_owner,
      technical_owner = lst$technical_owner,
      models          = restore_slot("models", model_restore),
      data            = restore_slot("data", data_asset_restore),
      prompts         = restore_slot("prompts", component_restore),
      retrieval       = restore_slot("retrieval", component_restore),
      tools           = restore_slot("tools", component_restore),
      guardrails      = restore_slot("guardrails", component_restore),
      runtime         = if (!is.null(lst$runtime)) component_restore(lst$runtime) else NULL,
      human_oversight = lst$human_oversight,
      deployment_context = lst$deployment_context,
      risks           = restore_slot("risks", risk_restore),
      evidence        = restore_slot("evidence", evidence_restore),
      incidents       = restore_slot("incidents", incident_restore),
      improvements    = restore_slot("improvements", improvement_restore),
      evidence_requirements = restore_slot("evidence_requirements", evidence_requirement_restore),
      evaluations     = restore_slot("evaluations", evaluation_restore),
      baseline_comparisons = restore_slot("baseline_comparisons", baseline_comparison_restore),
      decisions       = restore_slot("decisions", decision_restore),
      changes         = restore_slot("changes", change_restore),
      observations    = restore_slot("observations", observation_restore),
      monitoring_specs = restore_slot("monitoring_specs", monitoring_spec_restore),
      drift_assessments = restore_slot("drift_assessments", drift_assessment_restore),
      detections      = restore_slot("detections", detection_signal_restore)
    )),
    class = c("ai_system", "aitrace_object")
  )
}

register_restorer("ai_system", system_restore)

#' @export
format.ai_system <- function(x, ...) {
  cli::format_inline(
    "{.strong <ai_system>} {x$id} ({x$name}, {x$risk_class}, {x$status}, v{x$version})"
  )
}
