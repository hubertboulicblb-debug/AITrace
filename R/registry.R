# ---------------------------------------------------------------------------
# Governance policy
# ---------------------------------------------------------------------------

#' Create a governance policy
#'
#' Defines which evidence categories must be present and PASS/ACCEPTED
#' before a system may move to APPROVED or DEPLOYED. Categories are
#' validated against [EVIDENCE_CATEGORIES].
#' @param approval_requires Character vector of [EVIDENCE_CATEGORIES] that must be PASS/ACCEPTED before APPROVED.
#' @param deployment_requires Character vector of [EVIDENCE_CATEGORIES] that must be PASS/ACCEPTED before DEPLOYED.
#' @return A new `aitrace_policy` object.
#' @export
aitrace_policy <- function(approval_requires = character(0),
                           deployment_requires = character(0)) {
  approval_requires   <- as.character(approval_requires)
  deployment_requires <- as.character(deployment_requires)
  for (cat in approval_requires) {
    assert_choice(cat, EVIDENCE_CATEGORIES, arg = "approval_requires")
  }
  for (cat in deployment_requires) {
    assert_choice(cat, EVIDENCE_CATEGORIES, arg = "deployment_requires")
  }
  structure(
    list(
      approval_requires   = approval_requires,
      deployment_requires = deployment_requires
    ),
    class = "aitrace_policy"
  )
}

#' Validate a policy object
#' @param policy An [aitrace_policy()], or `NULL` to use the registry's active policy.
#' @return A list with elements `ok` (logical) and `issues` (character).
#' @export
validate_policy <- function(policy) {
  if (!inherits(policy, "aitrace_policy")) {
    return(list(ok = FALSE, issues = "not an aitrace_policy object"))
  }
  issues <- character(0)
  for (cat in policy$approval_requires) {
    if (!cat %in% EVIDENCE_CATEGORIES)
      issues <- c(issues, sprintf("invalid approval requirement: %s", cat))
  }
  for (cat in policy$deployment_requires) {
    if (!cat %in% EVIDENCE_CATEGORIES)
      issues <- c(issues, sprintf("invalid deployment requirement: %s", cat))
  }
  list(ok = length(issues) == 0L, issues = issues)
}

#' Default policy used by new registries
#' @return An `aitrace_policy` object.
#' @export
default_approval_policy <- function() {
  aitrace_policy(
    approval_requires = c("PERFORMANCE", "SAFETY", "ROBUSTNESS",
                          "SECURITY", "RISK_ASSESSMENT"),
    deployment_requires = c("PERFORMANCE", "SAFETY", "ROBUSTNESS",
                            "SECURITY", "RISK_ASSESSMENT")
  )
}

# ---------------------------------------------------------------------------
# Audit hash chain (type-robust)
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.audit_canonical_row <- function(seq, timestamp, event, system_id,
                                  payload_hash, prev_hash) {
  # Force stable types so verification survives JSON/RDS round-trips
  list(
    seq          = as.integer(seq),
    timestamp    = as.character(timestamp),
    event        = as.character(event),
    system_id    = as.character(system_id),
    payload_hash = as.character(payload_hash),
    prev_hash    = as.character(prev_hash)
  )
}

#' @keywords internal
#' @noRd
.audit_row_hash <- function(row) {
  digest::digest(
    jsonlite::toJSON(row, auto_unbox = TRUE, null = "null", digits = NA, pretty = FALSE),
    algo = "sha256"
  )
}

#' Append an event to the registry audit chain
#' @param registry A registry created by [new_registry()].
#' @param event Character event label recorded in the audit chain.
#' @param system_id Identifier of the [ai_system()] this record belongs to.
#' @param payload Named list payload whose hash is recorded in the audit row.
#' @return The SHA-256 hash of the newly appended audit row, invisibly.
#' @export
audit_append <- function(registry, event, system_id = NA_character_,
                         payload = list()) {
  assert_class(registry, "aitrace_registry")
  n <- nrow(registry$audit)
  prev_hash <- if (n == 0L) "GENESIS" else registry$audit$hash[n]
  payload_hash <- digest::digest(
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", digits = NA, pretty = FALSE),
    algo = "sha256"
  )
  seq <- as.integer(n + 1L)
  ts  <- now_utc_chr()
  row <- .audit_canonical_row(seq, ts, event, system_id %||% NA_character_,
                               payload_hash, prev_hash)
  h <- .audit_row_hash(row)
  new_row <- data.frame(
    seq = seq, timestamp = ts, event = event,
    system_id = as.character(system_id %||% NA_character_),
    payload_hash = payload_hash, prev_hash = prev_hash, hash = h,
    stringsAsFactors = FALSE
  )
  registry$audit <- rbind(registry$audit, new_row)
  invisible(h)
}

#' Verify the integrity of the audit hash chain
#' @param registry A registry created by [new_registry()].
#' @return A single logical value: `TRUE` if the chain is intact.
#' @export
audit_verify_chain <- function(registry) {
  assert_class(registry, "aitrace_registry")
  adf <- registry$audit
  if (nrow(adf) == 0L) return(TRUE)
  for (i in seq_len(nrow(adf))) {
    expected_prev <- if (i == 1L) "GENESIS" else adf$hash[i - 1L]
    if (!identical(adf$prev_hash[i], expected_prev)) return(FALSE)
    row <- .audit_canonical_row(
      adf$seq[i], adf$timestamp[i], adf$event[i], adf$system_id[i],
      adf$payload_hash[i], adf$prev_hash[i]
    )
    if (!identical(.audit_row_hash(row), adf$hash[i])) return(FALSE)
  }
  TRUE
}

# ---------------------------------------------------------------------------
# Registry
# ---------------------------------------------------------------------------

#' Create a new governance registry
#'
#' The registry is the organisational control plane: it holds systems,
#' enforces evidence gates on critical transitions, records audited
#' policy overrides, and maintains a type-robust append-only audit chain.
#' @param policy An [aitrace_policy()], or `NULL` to use the registry's active policy.
#' @return A new `aitrace_registry` object.
#' @export
new_registry <- function(policy = default_approval_policy()) {
  e <- new.env(parent = emptyenv())
  e$systems  <- list()
  e$audit    <- data.frame(
    seq = integer(), timestamp = character(), event = character(),
    system_id = character(), payload_hash = character(),
    prev_hash = character(), hash = character(),
    stringsAsFactors = FALSE
  )
  e$schema_version <- "0.4.0"
  e$policy <- if (inherits(policy, "aitrace_policy")) policy else default_approval_policy()
  class(e) <- "aitrace_registry"
  e
}

#' Register an AI system
#'
#' Runs [validate_system()] before accepting the system.
#' @param registry A registry created by [new_registry()].
#' @param system An [ai_system()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The registered system's id, invisibly.
#' @export
registry_add <- function(registry, system, actor = "system") {
  assert_class(registry, "aitrace_registry")
  assert_class(system, "ai_system")
  if (system$id %in% names(registry$systems)) {
    abort_aitrace(sprintf("System ID already registered: %s", system$id),
                  class = "aitrace_error_duplicate")
  }
  chk <- validate_system(system)
  if (!chk$ok) {
    abort_aitrace(
      paste0("validate_system() failed: ", paste(chk$issues, collapse = "; ")),
      class = "aitrace_error_validation"
    )
  }
  # Move to REGISTERED if still DRAFT
  if (identical(system$status, "DRAFT")) {
    system <- set_lifecycle_state(system, "REGISTERED", actor = actor)
  }
  fp <- ai_system_fingerprint(system)
  registry$systems[[system$id]] <- system
  audit_append(registry, "SYSTEM_REGISTERED", system$id,
               list(name = system$name, fingerprint = fp, version = system$version))
  invisible(system$id)
}

#' Retrieve a system from the registry
#' @param registry A registry created by [new_registry()].
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @return The stored `ai_system`, or `NULL` if not found.
#' @export
registry_get <- function(registry, id) {
  assert_class(registry, "aitrace_registry")
  registry$systems[[id]]
}

#' List systems in the registry
#' @param registry A registry created by [new_registry()].
#' @return A [tibble::tibble()] summarising every registered system.
#' @export
registry_list <- function(registry) {
  assert_class(registry, "aitrace_registry")
  if (length(registry$systems) == 0L) {
    return(tibble::tibble(
      id = character(), name = character(), risk_class = character(),
      status = character(), version = integer(), fingerprint = character()
    ))
  }
  tibble::tibble(
    id          = vapply(registry$systems, `[[`, character(1), "id"),
    name        = vapply(registry$systems, `[[`, character(1), "name"),
    risk_class  = vapply(registry$systems, `[[`, character(1), "risk_class"),
    status      = vapply(registry$systems, `[[`, character(1), "status"),
    version     = vapply(registry$systems, `[[`, integer(1), "version"),
    fingerprint = vapply(registry$systems, ai_system_fingerprint, character(1))
  )
}

#' Check evidence completeness against a set of required categories
#'
#' An evidence record satisfies a requirement when:
#' 1. It is **applicable** to the current system (same id; matching
#'    version/fingerprint when stamped — see [evidence_applies_to()]),
#' 2. Its category matches a required category, and
#' 3. Its status is PASS or ACCEPTED.
#'
#' Status is never inferred from metrics. Evidence bound to an older
#' version or different material fingerprint does not satisfy the gate.
#' @param system An [ai_system()].
#' @param required_categories Character vector of [EVIDENCE_CATEGORIES] to check for.
#' @return A list with elements `ok`, `missing`, `incomplete` and `inapplicable`.
#' @export
check_evidence_completeness <- function(system, required_categories) {
  assert_class(system, "ai_system")
  if (length(required_categories) == 0L) {
    return(list(ok = TRUE, missing = character(0), incomplete = character(0),
                inapplicable = character(0)))
  }
  items <- unname(system$evidence)
  if (length(items) == 0L) {
    return(list(ok = FALSE, missing = required_categories, incomplete = character(0),
                inapplicable = character(0)))
  }

  applicable <- vapply(items, evidence_applies_to, logical(1), system = system)
  inapplicable_ids <- vapply(items[!applicable], `[[`, character(1), "id")

  items_ok <- items[applicable]
  if (length(items_ok) == 0L) {
    return(list(ok = FALSE, missing = required_categories, incomplete = character(0),
                inapplicable = inapplicable_ids))
  }

  cats     <- vapply(items_ok, `[[`, character(1), "category")
  statuses <- vapply(items_ok, `[[`, character(1), "status")
  missing  <- setdiff(required_categories, unique(cats))
  good     <- unique(cats[statuses %in% c("PASS", "ACCEPTED")])
  incomplete <- setdiff(intersect(required_categories, unique(cats)), good)
  list(ok = length(missing) == 0L && length(incomplete) == 0L,
       missing = missing, incomplete = incomplete,
       inapplicable = inapplicable_ids)
}

#' Transition a registered system (with optional policy gates)
#'
#' Critical transitions (APPROVED, DEPLOYED) evaluate the active policy.
#' Intentional exceptions require `policy_override = TRUE` and a non-empty
#' `reason`; they emit a `GOVERNANCE_POLICY_OVERRIDE` audit event.
#' @param registry A registry created by [new_registry()].
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param to Target lifecycle state.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @param reason Optional character explanation.
#' @param policy An [aitrace_policy()], or `NULL` to use the registry's active policy.
#' @param policy_override If `TRUE`, allow the transition despite failing evidence gates, provided `reason` is non-empty.
#' @return The updated `ai_system`, invisibly.
#' @export
registry_transition <- function(registry, id, to,
                                actor = "system", reason = "",
                                policy = NULL,
                                policy_override = FALSE) {
  assert_class(registry, "aitrace_registry")
  system <- registry_get(registry, id)
  if (is.null(system)) {
    abort_aitrace(sprintf("Unknown system: %s", id), class = "aitrace_error_not_found")
  }

  # Domain-level transition check
  validate_transition(system$status, to, .system_lifecycle_transitions, SYSTEM_LIFECYCLE_STATES)

  # Resolve policy
  if (is.null(policy)) policy <- registry$policy %||% default_approval_policy()
  if (!inherits(policy, "aitrace_policy")) {
    abort_aitrace("policy must be an aitrace_policy object or NULL.",
                  class = "aitrace_error_type")
  }

  is_critical <- to %in% c("APPROVED", "DEPLOYED")
  req <- character(0)
  if (identical(to, "APPROVED"))  req <- policy$approval_requires
  if (identical(to, "DEPLOYED"))  req <- policy$deployment_requires

  if (is_critical && length(req) > 0L) {
    chk <- check_evidence_completeness(system, req)
    if (!chk$ok) {
      if (!isTRUE(policy_override)) {
        msg <- character(0)
        if (length(chk$missing) > 0)
          msg <- c(msg, paste("missing evidence types:", paste(chk$missing, collapse = ", ")))
        if (length(chk$incomplete) > 0)
          msg <- c(msg, paste("incomplete (not PASS/ACCEPTED):", paste(chk$incomplete, collapse = ", ")))
        if (length(chk$inapplicable) > 0)
          msg <- c(msg, paste("inapplicable (stale version/fingerprint):",
                              paste(chk$inapplicable, collapse = ", ")))
        abort_aitrace(
          paste0("Evidence gate failed for transition to ", to, ": ",
                 paste(msg, collapse = "; "),
                 ". Use policy_override = TRUE with a non-empty reason to record an intentional exception."),
          class = "aitrace_error_policy"
        )
      }
      if (!nzchar(reason)) {
        abort_aitrace("policy_override = TRUE requires a non-empty reason.",
                      class = "aitrace_error_policy")
      }
      audit_append(registry, "GOVERNANCE_POLICY_OVERRIDE", id, list(
        to = to, actor = actor, reason = reason,
        missing = chk$missing, incomplete = chk$incomplete,
        inapplicable = chk$inapplicable
      ))
    }
  }

  from <- system$status
  # .governance_context = TRUE: policy/evidence checks have already run above.
  # `reason` defaults to "" (not NULL), so it must be blanked explicitly here
  # or the change-log note silently comes out empty instead of falling back
  # to set_lifecycle_state()'s "state -> X" default.
  system <- set_lifecycle_state(system, to, actor = actor,
                                note = if (nzchar(reason %||% "")) reason else NULL,
                                .governance_context = TRUE)
  registry$systems[[id]] <- system
  audit_append(registry, "STATE_TRANSITION", id,
               list(from = from, to = to, actor = actor, reason = reason,
                    policy_override = isTRUE(policy_override),
                    version = system$version,
                    fingerprint = ai_system_fingerprint(system)))
  invisible(system)
}

#' Approve a system (gated)
#' @param registry A registry created by [new_registry()].
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param policy An [aitrace_policy()], or `NULL` to use the registry's active policy.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @param reason Optional character explanation.
#' @param policy_override If `TRUE`, allow the transition despite failing evidence gates, provided `reason` is non-empty.
#' @return The updated `ai_system`, invisibly.
#' @export
system_approve <- function(registry, id, policy = NULL,
                           actor = "reviewer", reason = "",
                           policy_override = FALSE) {
  registry_transition(registry, id, "APPROVED",
                      actor = actor, reason = reason, policy = policy,
                      policy_override = policy_override)
}

#' Deploy a system (gated)
#' @param registry A registry created by [new_registry()].
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param policy An [aitrace_policy()], or `NULL` to use the registry's active policy.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @param reason Optional character explanation.
#' @param policy_override If `TRUE`, allow the transition despite failing evidence gates, provided `reason` is non-empty.
#' @return The updated `ai_system`, invisibly.
#' @export
system_deploy <- function(registry, id, policy = NULL,
                          actor = "operator", reason = "",
                          policy_override = FALSE) {
  registry_transition(registry, id, "DEPLOYED",
                      actor = actor, reason = reason, policy = policy,
                      policy_override = policy_override)
}

#' Suspend a system
#' @param registry A registry created by [new_registry()].
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @param reason Optional character explanation.
#' @return The updated `ai_system`, invisibly.
#' @export
system_suspend <- function(registry, id, actor = "operator", reason = "") {
  registry_transition(registry, id, "SUSPENDED", actor = actor, reason = reason)
}

#' Retire a system
#' @param registry A registry created by [new_registry()].
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @param reason Optional character explanation.
#' @return The updated `ai_system`, invisibly.
#' @export
system_retire <- function(registry, id, actor = "operator", reason = "") {
  registry_transition(registry, id, "RETIRED", actor = actor, reason = reason)
}

#' Update the system object stored in the registry (e.g. after domain mutations)
#'
#' Runs [validate_system()] before accepting the update. Does not re-evaluate
#' evidence gates (use [registry_transition()] for lifecycle moves).
#' @param registry A registry created by [new_registry()].
#' @param system An [ai_system()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @param note Optional character note recorded in the change log.
#' @return The updated `ai_system`, invisibly.
#' @export
registry_update <- function(registry, system, actor = "system", note = NULL) {
  assert_class(registry, "aitrace_registry")
  assert_class(system, "ai_system")
  if (!system$id %in% names(registry$systems)) {
    abort_aitrace(sprintf("System %s is not registered.", system$id),
                  class = "aitrace_error_not_found")
  }
  chk <- validate_system(system)
  if (!chk$ok) {
    abort_aitrace(
      paste0("validate_system() failed: ", paste(chk$issues, collapse = "; ")),
      class = "aitrace_error_validation"
    )
  }
  # Refuse silent critical-state promotion via update
  prev <- registry$systems[[system$id]]
  if (!identical(prev$status, system$status) &&
      system$status %in% .critical_lifecycle_states) {
    abort_aitrace(
      sprintf(
        "Cannot promote lifecycle to \"%s\" via registry_update(). Use system_approve() / system_deploy() / registry_transition().",
        system$status
      ),
      class = "aitrace_error_policy"
    )
  }
  registry$systems[[system$id]] <- system
  audit_append(registry, "SYSTEM_UPDATED", system$id,
               list(version = system$version,
                    fingerprint = ai_system_fingerprint(system),
                    note = note, actor = actor,
                    previous_status = prev$status, status = system$status))
  invisible(system)
}

# ---------------------------------------------------------------------------
# Persistence
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.registry_snapshot <- function(registry) {
  list(
    schema_version = registry$schema_version,
    policy = list(
      approval_requires   = registry$policy$approval_requires,
      deployment_requires = registry$policy$deployment_requires
    ),
    systems = lapply(registry$systems, to_list),
    audit   = registry$audit
  )
}

#' Save registry to RDS
#' @param registry A registry created by [new_registry()].
#' @param path File path to read from or write to.
#' @param verify If `TRUE` (default), verify audit chain integrity before proceeding.
#' @return The `path` written to, invisibly.
#' @export
registry_save <- function(registry, path, verify = TRUE) {
  assert_class(registry, "aitrace_registry")
  if (isTRUE(verify) && !audit_verify_chain(registry)) {
    abort_aitrace("Audit chain verification failed; refusing to save.",
                  class = "aitrace_error_audit")
  }
  saveRDS(registry, file = path)
  invisible(path)
}

#' Load registry from RDS
#' @param path File path to read from or write to.
#' @param verify If `TRUE` (default), verify audit chain integrity before proceeding.
#' @return The restored `aitrace_registry`.
#' @export
registry_load <- function(path, verify = TRUE) {
  reg <- readRDS(path)
  if (!inherits(reg, "aitrace_registry")) {
    abort_aitrace("File does not contain an aitrace_registry.", class = "aitrace_error_type")
  }
  # Normalise audit column types
  if (nrow(reg$audit) > 0) {
    reg$audit$seq          <- as.integer(reg$audit$seq)
    reg$audit$timestamp    <- as.character(reg$audit$timestamp)
    reg$audit$event        <- as.character(reg$audit$event)
    reg$audit$system_id    <- as.character(reg$audit$system_id)
    reg$audit$payload_hash <- as.character(reg$audit$payload_hash)
    reg$audit$prev_hash    <- as.character(reg$audit$prev_hash)
    reg$audit$hash         <- as.character(reg$audit$hash)
  }
  if (isTRUE(verify) && !audit_verify_chain(reg)) {
    abort_aitrace("Audit integrity verification failed on load.",
                  class = "aitrace_error_audit")
  }
  reg
}

#' Export registry to JSON
#' @param registry A registry created by [new_registry()].
#' @param path File path to read from or write to.
#' @param verify If `TRUE` (default), verify audit chain integrity before proceeding.
#' @return The `path` written to, invisibly.
#' @export
registry_export <- function(registry, path, verify = TRUE) {
  assert_class(registry, "aitrace_registry")
  if (isTRUE(verify) && !audit_verify_chain(registry)) {
    abort_aitrace("Audit chain verification failed; refusing to export.",
                  class = "aitrace_error_audit")
  }
  jsonlite::write_json(.registry_snapshot(registry), path = path,
                       auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  invisible(path)
}

#' Registry schema versions this package version can import
#'
#' A minimal explicit compatibility policy: `registry_import()` refuses a
#' snapshot with an unrecognised `schema_version` rather than silently
#' accepting it. This is not a migration framework — snapshots written by an
#' older AITrace release are only importable once a converter for that
#' version has been added here.
#' @keywords internal
#' @noRd
.aitrace_supported_import_schema_versions <- c("0.4.0")

#' Import registry from JSON
#'
#' Every restored system is run back through [validate_system()] before it
#' is accepted into the registry, exactly as [registry_add()] requires for a
#' freshly-registered system — an imported snapshot gets no less scrutiny
#' than one built in-process.
#' @param path File path to read from or write to.
#' @param verify If `TRUE` (default), verify audit chain integrity before proceeding.
#' @return The restored `aitrace_registry`.
#' @export
registry_import <- function(path, verify = TRUE) {
  snap <- jsonlite::read_json(path, simplifyVector = FALSE)
  reg <- new_registry(policy = default_approval_policy())
  if (!is.null(snap$policy)) {
    reg$policy <- aitrace_policy(
      approval_requires   = unlist(snap$policy$approval_requires %||% list()),
      deployment_requires = unlist(snap$policy$deployment_requires %||% list())
    )
  }
  imported_schema <- snap$schema_version %||% "0.4.0"
  if (!imported_schema %in% .aitrace_supported_import_schema_versions) {
    abort_aitrace(
      sprintf(
        "registry_import(): unsupported schema_version \"%s\" (this package version supports: %s).",
        imported_schema, paste(.aitrace_supported_import_schema_versions, collapse = ", ")
      ),
      class = "aitrace_error_validation"
    )
  }
  reg$schema_version <- imported_schema
  if (!is.null(snap$systems)) {
    for (nm in names(snap$systems)) {
      sys <- system_restore(snap$systems[[nm]])
      chk <- validate_system(sys)
      if (!chk$ok) {
        abort_aitrace(
          sprintf("registry_import(): restored system \"%s\" failed validate_system(): %s",
                  nm, paste(chk$issues, collapse = "; ")),
          class = "aitrace_error_validation"
        )
      }
      reg$systems[[nm]] <- sys
    }
  }
  if (!is.null(snap$audit) && length(snap$audit) > 0L) {
    adf <- as.data.frame(snap$audit, stringsAsFactors = FALSE)
    if ("seq" %in% names(adf))          adf$seq          <- as.integer(adf$seq)
    if ("timestamp" %in% names(adf))    adf$timestamp    <- as.character(adf$timestamp)
    if ("event" %in% names(adf))        adf$event        <- as.character(adf$event)
    if ("system_id" %in% names(adf))    adf$system_id    <- as.character(adf$system_id)
    if ("payload_hash" %in% names(adf)) adf$payload_hash <- as.character(adf$payload_hash)
    if ("prev_hash" %in% names(adf))    adf$prev_hash    <- as.character(adf$prev_hash)
    if ("hash" %in% names(adf))         adf$hash         <- as.character(adf$hash)
    reg$audit <- adf
  }
  if (isTRUE(verify) && !audit_verify_chain(reg)) {
    abort_aitrace("Audit integrity verification failed on import.",
                  class = "aitrace_error_audit")
  }
  reg
}
