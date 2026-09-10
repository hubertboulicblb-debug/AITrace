#' AITrace: Industrial AI Governance, Assurance and Continuous Improvement
#'
#' @description
#' AITrace treats the governed object as the *complete* AI system.
#'
#' Architecture:
#' \itemize{
#'   \item \strong{0.2.2} — Evidence, traceability, audit, fingerprint (frozen)
#'   \item \strong{0.2.3} — Evaluation recording (frozen)
#'   \item \strong{0.3.0} — Decision + Change (frozen)
#'   \item \strong{0.4.0} — Observation, monitoring_spec, drift_assessment,
#'     detection_signal (recording; measurement in)
#'   \item \strong{0.4.1} — Hardening pass on the 0.4.0 layer: directional
#'     drift semantics, timestamp/version validation, a declarative
#'     monitoring_spec default, stricter detection/drift traceability, and
#'     registry_import() re-validation (current)
#' }
#'
#' No fabricated evidence, health scores, or automatic incident creation.
#'
#' @keywords internal
"_PACKAGE"
