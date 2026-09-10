#' Construct an evidence record
#'
#' Evidence is first-class. Status is never inferred from metric values.
#' When attached to a system via [add_evidence()], the evidence is stamped
#' with `system_id`, `system_version` and `system_fingerprint` so that
#' evidence gates can verify applicability to the exact governed version.
#'
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param category Controlled category value.
#' @param test Optional character test name.
#' @param metric Metric name (character scalar), e.g. "AUROC" or "latency_p99".
#' @param result Result value; validated against the relevant controlled vocabulary.
#' @param acceptance_criterion Optional character description of the acceptance criterion.
#' @param status New status value; validated against the relevant controlled vocabulary.
#' @param content Optional content whose SHA-256 hash is recorded as `content_hash`.
#' @param system_id Identifier of the [ai_system()] this record belongs to.
#' @param system_version System governance version the record is bound to.
#' @param system_fingerprint Material fingerprint the record is bound to (see [ai_system_fingerprint()]).
#' @param owner Optional character owner identifier.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `ai_evidence` object.
#' @export
ai_evidence <- function(id = NULL, category = "PERFORMANCE", test = NULL,
                        metric = NULL, result = NULL,
                        acceptance_criterion = NULL,
                        status = "PENDING", content = NULL,
                        system_id = NULL, system_version = NULL,
                        system_fingerprint = NULL,
                        owner = NULL, metadata = list(), provenance = list()) {
  assert_choice(category, EVIDENCE_CATEGORIES, arg = "category")
  assert_choice(status, EVIDENCE_STATUS_LEVELS, arg = "status")
  if (is.null(id)) id <- new_id("EVD")
  content_hash <- if (!is.null(content)) {
    digest::digest(content, algo = "sha256")
  } else NULL
  new_aitrace_object(
    "ai_evidence", id,
    fields = list(
      category              = category,
      test                  = test,
      metric                = metric,
      result                = result,
      acceptance_criterion  = acceptance_criterion,
      content_hash          = content_hash,
      system_id             = system_id,
      system_version        = system_version,
      system_fingerprint    = system_fingerprint
    ),
    owner = owner, status = status, metadata = metadata, provenance = provenance
  )
}

#' Update evidence status (explicit; never inferred)
#' @param evidence An [ai_evidence()].
#' @param status New status value; validated against the relevant controlled vocabulary.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @param note Optional character note recorded in the change log.
#' @return The updated `ai_evidence`.
#' @export
set_evidence_status <- function(evidence, status, actor = NULL, note = NULL) {
  assert_class(evidence, "ai_evidence")
  assert_choice(status, EVIDENCE_STATUS_LEVELS, arg = "status")
  evidence$status <- status
  bump_version(evidence, note = note %||% sprintf("status -> %s", status), actor = actor)
}

#' Whether evidence is applicable to a given system
#'
#' Applicable when:
#' 1. `system_id` matches, and
#' 2. material fingerprint matches (if stamped), and
#' 3. either the version matches **or** the fingerprint matches
#'    (governance-only version bumps that leave material composition
#'    unchanged do not invalidate evidence).
#'
#' Unstamped legacy evidence (no system_id) is not applicable (conservative).
#' A fingerprint mismatch always means not applicable (material change).
#' @param evidence An [ai_evidence()].
#' @param system An [ai_system()].
#' @return A single logical value.
#' @export
evidence_applies_to <- function(evidence, system) {
  assert_class(evidence, "ai_evidence")
  assert_class(system, "ai_system")
  if (is.null(evidence$system_id) || !identical(evidence$system_id, system$id)) {
    return(FALSE)
  }
  fp_match <- !is.null(evidence$system_fingerprint) &&
    identical(evidence$system_fingerprint, ai_system_fingerprint(system))
  ver_match <- !is.null(evidence$system_version) &&
    identical(as.integer(evidence$system_version), as.integer(system$version))

  # Fingerprint stamped but material composition changed → not applicable
  if (!is.null(evidence$system_fingerprint) && !fp_match) {
    return(FALSE)
  }
  # Applicable if version matches, or material fingerprint still matches
  if (ver_match || fp_match) return(TRUE)
  FALSE
}

#' @keywords internal
#' @noRd
evidence_restore <- function(lst) {
  env <- restore_envelope(lst)
  structure(
    c(env, list(
      category             = lst$category,
      test                 = lst$test,
      metric               = lst$metric,
      result               = lst$result,
      acceptance_criterion = lst$acceptance_criterion,
      content_hash         = lst$content_hash,
      system_id            = lst$system_id,
      system_version       = lst$system_version,
      system_fingerprint   = lst$system_fingerprint
    )),
    class = c("ai_evidence", "aitrace_object")
  )
}

register_restorer("ai_evidence", evidence_restore)

#' @export
format.ai_evidence <- function(x, ...) {
  bind <- if (!is.null(x$system_id)) {
    sprintf(" bound:%s@v%s", x$system_id, x$system_version %||% "?")
  } else ""
  cli::format_inline(
    "{.strong <ai_evidence>} {x$id} [{x$category}/{x$status}]{bind}"
  )
}
