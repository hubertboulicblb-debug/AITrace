#' Controlled vocabularies used throughout AITrace
#'
#' Every classification field is validated against one of these closed
#' character vectors. Invalid values raise an `aitrace_error_choice`.
#'
#' @name aitrace-vocabularies
#' @aliases vocabularies
NULL

#' @rdname aitrace-vocabularies
#' @export
RISK_CLASS_LEVELS <- c("LOW", "MEDIUM", "HIGH", "CRITICAL")

#' @rdname aitrace-vocabularies
#' @export
SYSTEM_LIFECYCLE_STATES <- c(
  "DRAFT",
  "REGISTERED",
  "DEVELOPMENT",
  "EVALUATING",
  "VALIDATION",
  "VALIDATED",
  "UNDER_REVIEW",
  "APPROVED",
  "DEPLOYED",
  "MONITORED",
  "REVIEW_REQUIRED",
  "REVALIDATION_REQUIRED",
  "SUSPENDED",
  "RETIRED"
)

# Combined lifecycle transitions (rich domain + hardened kernel paths).
.system_lifecycle_transitions <- list(
  DRAFT                   = c("REGISTERED", "DEVELOPMENT", "RETIRED"),
  REGISTERED              = c("DEVELOPMENT", "EVALUATING", "RETIRED"),
  DEVELOPMENT             = c("EVALUATING", "VALIDATION", "DRAFT", "RETIRED"),
  EVALUATING              = c("VALIDATED", "VALIDATION", "REVALIDATION_REQUIRED", "SUSPENDED"),
  VALIDATION              = c("VALIDATED", "APPROVED", "DEVELOPMENT", "RETIRED"),
  VALIDATED               = c("UNDER_REVIEW", "APPROVED", "EVALUATING"),
  UNDER_REVIEW            = c("APPROVED", "REVALIDATION_REQUIRED", "SUSPENDED", "VALIDATION"),
  APPROVED                = c("DEPLOYED", "SUSPENDED", "REVALIDATION_REQUIRED", "VALIDATION"),
  DEPLOYED                = c("MONITORED", "SUSPENDED", "RETIRED", "REVIEW_REQUIRED"),
  MONITORED               = c("DEPLOYED", "SUSPENDED", "REVIEW_REQUIRED", "VALIDATION", "RETIRED"),
  REVIEW_REQUIRED         = c("UNDER_REVIEW", "REVALIDATION_REQUIRED", "SUSPENDED"),
  REVALIDATION_REQUIRED   = c("EVALUATING", "VALIDATION", "SUSPENDED"),
  SUSPENDED               = c("DEPLOYED", "MONITORED", "VALIDATION", "UNDER_REVIEW", "RETIRED"),
  RETIRED                 = character(0)
)

#' @rdname aitrace-vocabularies
#' @export
EVIDENCE_CATEGORIES <- c(
  "PERFORMANCE", "SAFETY", "ROBUSTNESS", "SECURITY", "FAIRNESS",
  "CALIBRATION", "EXPLAINABILITY", "DATA_QUALITY", "DRIFT",
  "OPERATIONAL_PERFORMANCE", "VALIDATION", "REGULATORY", "HUMAN_FACTORS",
  "RISK_ASSESSMENT"
)

#' @rdname aitrace-vocabularies
#' @export
EVIDENCE_STATUS_LEVELS <- c("PENDING", "PASS", "FAIL", "WAIVED", "ACCEPTED", "NOT_APPLICABLE")

#' @rdname aitrace-vocabularies
#' @export
RISK_CATEGORIES <- c(
  "PERFORMANCE", "SAFETY", "SECURITY", "PRIVACY", "FAIRNESS",
  "ROBUSTNESS", "OPERATIONAL", "DATA", "MODEL", "GOVERNANCE",
  "REGULATORY", "HUMAN_FACTORS"
)

#' @rdname aitrace-vocabularies
#' @export
LIKELIHOOD_LEVELS <- c("RARE", "UNLIKELY", "POSSIBLE", "LIKELY", "ALMOST_CERTAIN")

#' @rdname aitrace-vocabularies
#' @export
IMPACT_LEVELS <- c("NEGLIGIBLE", "MINOR", "MODERATE", "MAJOR", "SEVERE")

#' @rdname aitrace-vocabularies
#' @export
RISK_STATUS_LEVELS <- c("OPEN", "MITIGATING", "MITIGATED", "ACCEPTED", "CLOSED")

#' @rdname aitrace-vocabularies
#' @export
MODEL_TYPE_LEVELS <- c(
  "CLASSIFICATION", "REGRESSION", "CLUSTERING", "RANKING",
  "GENERATIVE", "LLM", "RAG", "AGENT", "OTHER"
)

#' @rdname aitrace-vocabularies
#' @export
DATA_ROLE_LEVELS <- c("TRAINING", "VALIDATION", "TEST", "PRODUCTION", "REFERENCE", "OTHER")

#' @rdname aitrace-vocabularies
#' @export
COMPONENT_KIND_LEVELS <- c("PROMPT", "RETRIEVAL", "TOOL", "GUARDRAIL", "RUNTIME", "OTHER")

#' @rdname aitrace-vocabularies
#' @export
INCIDENT_CATEGORIES <- c(
  "PERFORMANCE_DEGRADATION", "SAFETY", "SECURITY", "DATA_QUALITY",
  "DRIFT", "OPERATIONAL", "HUMAN_FACTORS", "REGULATORY", "OTHER"
)

#' @rdname aitrace-vocabularies
#' @export
INCIDENT_SEVERITY_LEVELS <- c("LOW", "MEDIUM", "HIGH", "CRITICAL")

#' @rdname aitrace-vocabularies
#' @export
INCIDENT_LIFECYCLE_STATES <- c(
  "DETECTED", "TRIAGED", "INVESTIGATING", "CONTAINED",
  "ROOT_CAUSE_IDENTIFIED", "CORRECTIVE_ACTION_IN_PROGRESS",
  "RESOLVED", "CLOSED", "REOPENED"
)

.incident_lifecycle_transitions <- list(
  DETECTED                      = c("TRIAGED", "CLOSED"),
  TRIAGED                       = c("INVESTIGATING", "CLOSED"),
  INVESTIGATING                 = c("CONTAINED", "ROOT_CAUSE_IDENTIFIED", "CLOSED"),
  CONTAINED                     = c("ROOT_CAUSE_IDENTIFIED", "INVESTIGATING"),
  ROOT_CAUSE_IDENTIFIED         = c("CORRECTIVE_ACTION_IN_PROGRESS"),
  CORRECTIVE_ACTION_IN_PROGRESS = c("RESOLVED", "ROOT_CAUSE_IDENTIFIED"),
  RESOLVED                      = c("CLOSED", "REOPENED"),
  CLOSED                        = c("REOPENED"),
  REOPENED                      = c("TRIAGED", "INVESTIGATING")
)

#' @rdname aitrace-vocabularies
#' @export
IMPROVEMENT_DOMAINS <- c(
  "PREDICTIVE_PERFORMANCE", "DECISION_QUALITY", "CALIBRATION_UNCERTAINTY",
  "ROBUSTNESS", "SAFETY", "SECURITY", "FAIRNESS", "DRIFT",
  "DATA_QUALITY", "OPERATIONAL", "HUMAN_FACTORS", "COST", "OTHER"
)

#' @rdname aitrace-vocabularies
#' @export
IMPROVEMENT_TRIGGERS <- c(
  "INCIDENT", "MONITORING", "DRIFT", "AUDIT", "USER_FEEDBACK",
  "REGULATORY", "SCHEDULED_REVIEW", "OTHER"
)

#' @rdname aitrace-vocabularies
#' @export
IMPROVEMENT_PRIORITY_LEVELS <- c("LOW", "MEDIUM", "HIGH", "CRITICAL")

#' @rdname aitrace-vocabularies
#' @export
IMPROVEMENT_LIFECYCLE_STATES <- c(
  "OBSERVED", "DIAGNOSED", "PROPOSED", "PRIORITIZED", "DEVELOPING",
  "EVALUATING", "VALIDATED", "APPROVED", "DEPLOYED", "VERIFIED",
  "REJECTED", "DEFERRED", "SUSPENDED", "ROLLED_BACK"
)

.improvement_lifecycle_transitions <- list(
  OBSERVED     = c("DIAGNOSED", "REJECTED", "DEFERRED"),
  DIAGNOSED    = c("PROPOSED", "REJECTED", "DEFERRED"),
  PROPOSED     = c("PRIORITIZED", "REJECTED", "DEFERRED"),
  PRIORITIZED  = c("DEVELOPING", "DEFERRED", "REJECTED"),
  DEVELOPING   = c("EVALUATING", "SUSPENDED", "DEFERRED"),
  EVALUATING   = c("VALIDATED", "DEVELOPING", "REJECTED"),
  VALIDATED    = c("APPROVED", "REJECTED", "DEVELOPING"),
  APPROVED     = c("DEPLOYED", "SUSPENDED"),
  DEPLOYED     = c("VERIFIED", "ROLLED_BACK"),
  VERIFIED     = character(0),
  REJECTED     = character(0),
  DEFERRED     = c("PROPOSED", "PRIORITIZED", "REJECTED"),
  SUSPENDED    = c("DEVELOPING", "EVALUATING", "REJECTED"),
  ROLLED_BACK  = c("DEVELOPING", "REJECTED")
)

#' @rdname aitrace-vocabularies
#' @export
EFFECT_DIRECTION_LEVELS <- c("UP", "DOWN", "MAINTAIN")

#' @rdname aitrace-vocabularies
#' @export
VALIDATION_METHOD_LEVELS <- c(
  "OFFLINE_EVALUATION", "ONLINE_EXPERIMENT", "SHADOW", "CANARY",
  "HUMAN_REVIEW", "REGULATORY", "OTHER"
)

#' @rdname aitrace-vocabularies
#' @export
GOVERNANCE_DECISION_LEVELS <- c(
  "APPROVE", "REJECT", "DEFER", "REQUEST_CHANGES",
  "DEPLOY", "SUSPEND", "RETIRE", "REVALIDATE",
  "ACCEPT_RISK", "OVERRIDE_POLICY"
)

#' @rdname aitrace-vocabularies
#' @export
DECISION_STATUS_LEVELS <- c("PROPOSED", "RECORDED", "EXECUTED", "SUPERSEDED", "WITHDRAWN")

#' @rdname aitrace-vocabularies
#' @export
CHANGE_TYPE_LEVELS <- c(
  "MODEL", "DATA", "PROMPT", "RETRIEVAL", "TOOL", "GUARDRAIL",
  "RUNTIME", "CONFIGURATION", "PROCESS", "OTHER", "COMPOSITE"
)

#' @rdname aitrace-vocabularies
#' @export
CHANGE_STATUS_LEVELS <- c(
  "PROPOSED", "APPROVED", "REJECTED", "IN_PROGRESS",
  "DEPLOYED", "VERIFIED", "ROLLED_BACK", "CANCELLED"
)

#' @rdname aitrace-vocabularies
#' @export
DEPLOYMENT_STATUS_LEVELS <- c("PENDING", "DEPLOYED", "FAILED", "NOT_APPLICABLE")

#' @rdname aitrace-vocabularies
#' @export
VERIFICATION_RESULT_LEVELS <- c("CONFIRMED", "PARTIAL", "FAILED", "PENDING")

#' @rdname aitrace-vocabularies
#' @export
CHANGE_IMPACT_LEVELS <- c("NO_IMPACT", "MINOR", "MAJOR", "CRITICAL")

#' Validate a lifecycle state transition
#' @keywords internal
#' @noRd
validate_transition <- function(from, to, transitions, all_states,
                                 call = rlang::caller_env()) {
  assert_choice(to, all_states, arg = "to", call = call)
  if (is.null(from)) return(invisible(NULL))
  assert_choice(from, all_states, arg = "from", call = call)
  if (identical(from, to)) return(invisible(NULL))
  allowed <- transitions[[from]]
  if (is.null(allowed) || !to %in% allowed) {
    abort_aitrace(
      sprintf(
        "Cannot transition from \"%s\" to \"%s\". Allowed next state(s): %s.",
        from, to,
        if (length(allowed)) paste(sprintf('"%s"', allowed), collapse = ", ") else "(none - terminal state)"
      ),
      class = "aitrace_error_transition",
      call = call
    )
  }
  invisible(NULL)
}

#' Look up ordered rank of a controlled value
#' @keywords internal
#' @noRd
level_rank <- function(x, levels) match(x, levels)


#' @rdname aitrace-vocabularies
#' @export
OBSERVATION_KIND_LEVELS <- c(
  "METRIC", "PREDICTION", "FEATURE", "LATENCY", "THROUGHPUT",
  "ERROR_RATE", "DATA_QUALITY", "CUSTOM"
)

#' @rdname aitrace-vocabularies
#' @export
MONITORING_STATUS_LEVELS <- c("ACTIVE", "PAUSED", "RETIRED")

#' @rdname aitrace-vocabularies
#' @export
DRIFT_RESULT_LEVELS <- c("NO_DRIFT", "DRIFT_DETECTED", "INCONCLUSIVE")

#' @rdname aitrace-vocabularies
#' @export
DETECTION_SEVERITY_LEVELS <- c("INFO", "WARNING", "CRITICAL")

#' @rdname aitrace-vocabularies
#' @export
DETECTION_STATUS_LEVELS <- c("OPEN", "ACKNOWLEDGED", "RESOLVED", "SUPPRESSED")
