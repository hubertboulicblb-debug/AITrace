#' Record a production / operational observation
#'
#' Observations are supplied measurements. AITrace never invents values,
#' health scores, or statistical significance. Missing values are not
#' stored as fabricated zeros.
#'
#' @param kind One of [OBSERVATION_KIND_LEVELS].
#' @param metric Metric name (e.g. "AUROC", "latency_p99").
#' @param value Observed value (numeric or character).
#' @param observed_at When the observation was taken (`POSIXct`; defaults to now UTC).
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param system_id Identifier of the [ai_system()] this record belongs to.
#' @param unit Optional character unit of measure.
#' @param source Optional character source of the value or data.
#' @param window_start Optional start of the observation window (`POSIXct`). If both
#'   `window_start` and `window_end` are supplied, `window_end` must not be earlier.
#' @param window_end Optional end of the observation window (`POSIXct`).
#' @param system_version Optional system governance version the record is bound to;
#'   if supplied, must be a single positive integer.
#' @param system_fingerprint Material fingerprint the record is bound to (see [ai_system_fingerprint()]).
#' @param owner Optional character owner identifier.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `ai_observation` object.
#' @export
ai_observation <- function(id = NULL,
                           system_id = NULL,
                           kind = "METRIC",
                           metric,
                           value,
                           unit = NULL,
                           source = NULL,
                           observed_at = NULL,
                           window_start = NULL,
                           window_end = NULL,
                           system_version = NULL,
                           system_fingerprint = NULL,
                           owner = NULL,
                           metadata = list(),
                           provenance = list()) {
  assert_choice(kind, OBSERVATION_KIND_LEVELS, arg = "kind")
  assert_scalar_character(metric, arg = "metric")
  ok <- (is.numeric(value) || is.character(value)) && length(value) == 1L && !is.na(value)
  if (!ok) {
    abort_aitrace("`value` must be a single non-missing numeric or character observation.",
                  class = "aitrace_error_type")
  }
  if (!is.null(unit)) assert_scalar_character(unit, arg = "unit")
  if (!is.null(source)) assert_scalar_character(source, arg = "source")
  if (!is.null(system_id)) assert_scalar_character(system_id, arg = "system_id")
  assert_posixct(observed_at, arg = "observed_at", allow_null = TRUE)
  assert_posixct(window_start, arg = "window_start", allow_null = TRUE)
  assert_posixct(window_end, arg = "window_end", allow_null = TRUE)
  if (!is.null(window_start) && !is.null(window_end) && window_end < window_start) {
    abort_aitrace("`window_end` must not be earlier than `window_start`.",
                  class = "aitrace_error_type")
  }
  assert_scalar_positive_integer(system_version, arg = "system_version", allow_null = TRUE)
  if (is.null(observed_at)) observed_at <- now_utc()
  if (is.null(id)) id <- new_id("OBS")
  new_aitrace_object(
    "ai_observation", id,
    fields = list(
      system_id          = system_id,
      kind               = kind,
      metric             = metric,
      value              = value,
      unit               = unit,
      source             = source,
      observed_at        = observed_at,
      window_start       = window_start,
      window_end         = window_end,
      system_version     = if (is.null(system_version)) NULL else as.integer(system_version),
      system_fingerprint = system_fingerprint
    ),
    owner = owner, status = "RECORDED",
    metadata = metadata, provenance = provenance
  )
}

#' Define what is being monitored for a system
#'
#' A monitoring specification is declarative. It does not compute health
#' scores or invent thresholds beyond the explicit criterion supplied.
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param system_id Identifier of the [ai_system()] this record belongs to.
#' @param name Character name.
#' @param metric Metric name (character scalar), e.g. "AUROC" or "latency_p99".
#' @param kind Controlled kind/type value.
#' @param operator One of [CRITERION_OPERATOR_LEVELS], or `NULL` for a purely
#'   declarative spec with no executable criterion (evaluates to `INCONCLUSIVE`
#'   via [evaluate_monitoring()] until both `operator` and `criterion` are set).
#' @param criterion Criterion value the observation is compared against. Leave
#'   `NULL` (the default) together with `operator = NULL` for a declarative spec.
#' @param upper Optional numeric upper bound; required for the `BETWEEN`/`OUTSIDE` operators.
#' @param unit Optional character unit of measure.
#' @param baseline_value Optional numeric baseline value the monitoring spec tracks.
#' @param drift_tolerance Optional non-negative numeric drift tolerance.
#' @param cadence Optional character monitoring cadence, e.g. "daily".
#' @param owner Optional character owner identifier.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `monitoring_spec` object.
#' @export
monitoring_spec <- function(id = NULL,
                            system_id = NULL,
                            name,
                            metric,
                            kind = "METRIC",
                            operator = NULL,
                            criterion = NULL,
                            upper = NULL,
                            unit = NULL,
                            baseline_value = NULL,
                            drift_tolerance = NULL,
                            cadence = NULL,
                            owner = NULL,
                            metadata = list(),
                            provenance = list()) {
  assert_scalar_character(name, arg = "name")
  assert_scalar_character(metric, arg = "metric")
  assert_choice(kind, OBSERVATION_KIND_LEVELS, arg = "kind")
  if (!is.null(operator)) assert_choice(operator, CRITERION_OPERATOR_LEVELS, arg = "operator")
  if (!is.null(criterion) && !is.null(operator)) {
    criterion_validate(operator, criterion, upper)
  }
  if (!is.null(unit)) assert_scalar_character(unit, arg = "unit")
  if (!is.null(cadence)) assert_scalar_character(cadence, arg = "cadence")
  if (!is.null(system_id)) assert_scalar_character(system_id, arg = "system_id")
  if (!is.null(drift_tolerance) &&
      (!is.numeric(drift_tolerance) || length(drift_tolerance) != 1L || is.na(drift_tolerance) || drift_tolerance < 0)) {
    abort_aitrace("`drift_tolerance` must be a single non-negative numeric, if supplied.",
                  class = "aitrace_error_type")
  }
  if (is.null(id)) id <- new_id("MON")
  new_aitrace_object(
    "monitoring_spec", id,
    fields = list(
      system_id       = system_id,
      name            = name,
      metric          = metric,
      kind            = kind,
      operator        = operator,
      criterion       = criterion,
      upper           = upper,
      unit            = unit,
      baseline_value  = baseline_value,
      drift_tolerance = drift_tolerance,
      cadence         = cadence
    ),
    owner = owner, status = "ACTIVE",
    metadata = metadata, provenance = provenance
  )
}

#' Assess drift between a baseline and a current observation
#'
#' Deterministic and conservative. Without numeric baseline and observed
#' values, result is INCONCLUSIVE. No p-values or effect sizes.
#'
#' With tolerance, `direction` controls which side of the baseline counts as
#' drift: `"MAINTAIN"` (default) flags any deviation whose absolute value
#' exceeds `tolerance`, `"UP"` flags only an increase beyond tolerance
#' (`observed - baseline > tolerance`), and `"DOWN"` flags only a decrease
#' beyond tolerance (`observed - baseline < -tolerance`); movement on the
#' other side is NO_DRIFT rather than treated as improvement or penalised.
#' Equal values are always NO_DRIFT. Without tolerance, unequal numeric
#' values are always INCONCLUSIVE regardless of `direction` (no invented
#' definition of meaningful change).
#' @param metric Metric name (character scalar), e.g. "AUROC" or "latency_p99".
#' @param baseline Baseline value (numeric or character scalar).
#' @param observed Observed value (numeric or character scalar).
#' @param tolerance Optional non-negative numeric drift tolerance.
#' @param direction One of [EFFECT_DIRECTION_LEVELS] describing which side of
#'   the baseline counts as drift (see Details). Does not affect the
#'   INCONCLUSIVE/NO_DRIFT cases, only which side of a tolerance breach is
#'   flagged as DRIFT_DETECTED.
#' @param unit Optional character unit of measure.
#' @param observation_id Identifier of the related [ai_observation()].
#' @param monitoring_id Identifier of the related [monitoring_spec()].
#' @param note Optional character note recorded in the change log.
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param owner Optional character owner identifier.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `drift_assessment` object.
#' @export
assess_drift <- function(metric,
                         baseline,
                         observed,
                         tolerance = NULL,
                         direction = "MAINTAIN",
                         unit = NULL,
                         observation_id = NULL,
                         monitoring_id = NULL,
                         note = NULL,
                         id = NULL,
                         owner = NULL,
                         metadata = list(),
                         provenance = list()) {
  assert_scalar_character(metric, arg = "metric")
  if (!is.null(tolerance) &&
      (!is.numeric(tolerance) || length(tolerance) != 1L || is.na(tolerance) || tolerance < 0)) {
    abort_aitrace("`tolerance` must be a single non-negative numeric, if supplied.",
                  class = "aitrace_error_type")
  }
  if (!is.null(direction)) assert_choice(direction, EFFECT_DIRECTION_LEVELS, arg = "direction")
  result <- "INCONCLUSIVE"
  delta <- NULL
  if (is.numeric(baseline) && length(baseline) == 1L && !is.na(baseline) &&
      is.numeric(observed) && length(observed) == 1L && !is.na(observed)) {
    delta <- as.numeric(observed) - as.numeric(baseline)
    if (identical(as.numeric(observed), as.numeric(baseline))) {
      result <- "NO_DRIFT"
    } else if (is.null(tolerance)) {
      # No declared definition of "meaningful change" → do not invent drift
      result <- "INCONCLUSIVE"
    } else {
      exceeds <- switch(direction %||% "MAINTAIN",
        UP       = delta > tolerance,
        DOWN     = delta < -tolerance,
        MAINTAIN = abs(delta) > tolerance
      )
      result <- if (isTRUE(exceeds)) "DRIFT_DETECTED" else "NO_DRIFT"
    }
  } else if (identical(baseline, observed)) {
    result <- "NO_DRIFT"
  }
  assert_choice(result, DRIFT_RESULT_LEVELS, arg = "result")
  if (is.null(id)) id <- new_id("DFT")
  new_aitrace_object(
    "drift_assessment", id,
    fields = list(
      metric             = metric,
      baseline           = baseline,
      observed           = observed,
      delta              = delta,
      tolerance          = tolerance,
      direction          = direction,
      result             = result,
      unit               = unit,
      observation_id     = observation_id,
      monitoring_id      = monitoring_id,
      note               = note,
      system_id          = NULL,
      system_version     = NULL,
      system_fingerprint = NULL
    ),
    owner = owner, status = result,
    metadata = metadata, provenance = provenance
  )
}

#' Record a detection signal raised from monitoring / drift
#'
#' A detection is an explicit signal. Linking to an incident is optional and
#' never automatic: callers create incidents separately.
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param system_id Identifier of the [ai_system()] this record belongs to.
#' @param severity Controlled severity level.
#' @param status New status value; validated against the relevant controlled vocabulary.
#' @param metric Metric name (character scalar), e.g. "AUROC" or "latency_p99".
#' @param message Character message describing the detection.
#' @param observation_id Identifier of the related [ai_observation()].
#' @param monitoring_id Identifier of the related [monitoring_spec()].
#' @param drift_id Identifier of the related [assess_drift()].
#' @param evaluation_id Identifier of the related [evaluation()].
#' @param incident_id Identifier of the related [ai_incident()].
#' @param detected_at When the signal or incident was detected (`POSIXct`); defaults to now (UTC).
#' @param owner Optional character owner identifier.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `detection_signal` object.
#' @export
detection_signal <- function(id = NULL,
                             system_id,
                             severity = "WARNING",
                             status = "OPEN",
                             metric = NULL,
                             message,
                             observation_id = NULL,
                             monitoring_id = NULL,
                             drift_id = NULL,
                             evaluation_id = NULL,
                             incident_id = NULL,
                             detected_at = NULL,
                             owner = NULL,
                             metadata = list(),
                             provenance = list()) {
  assert_scalar_character(system_id, arg = "system_id")
  assert_scalar_character(message, arg = "message")
  assert_choice(severity, DETECTION_SEVERITY_LEVELS, arg = "severity")
  assert_choice(status, DETECTION_STATUS_LEVELS, arg = "status")
  if (!is.null(metric)) assert_scalar_character(metric, arg = "metric")
  assert_posixct(detected_at, arg = "detected_at", allow_null = TRUE)
  if (is.null(detected_at)) detected_at <- now_utc()
  if (is.null(id)) id <- new_id("DET")
  new_aitrace_object(
    "detection_signal", id,
    fields = list(
      system_id       = system_id,
      severity        = severity,
      metric          = metric,
      message         = message,
      observation_id  = observation_id,
      monitoring_id   = monitoring_id,
      drift_id        = drift_id,
      evaluation_id   = evaluation_id,
      incident_id     = incident_id,
      detected_at     = detected_at
    ),
    owner = owner, status = status,
    metadata = metadata, provenance = provenance
  )
}

#' Link a detection to an existing incident (does not create the incident)
#' @param detection A [detection_signal()].
#' @param incident_id Identifier of the related [ai_incident()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `detection_signal`.
#' @export
link_detection_incident <- function(detection, incident_id, actor = NULL) {
  assert_class(detection, "detection_signal")
  assert_scalar_character(incident_id, arg = "incident_id")
  detection$incident_id <- incident_id
  bump_version(detection, note = sprintf("linked incident %s", incident_id), actor = actor)
}

#' Update detection status
#' @param detection A [detection_signal()].
#' @param status New status value; validated against the relevant controlled vocabulary.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @param note Optional character note recorded in the change log.
#' @return The updated `detection_signal`.
#' @export
set_detection_status <- function(detection, status, actor = NULL, note = NULL) {
  assert_class(detection, "detection_signal")
  assert_choice(status, DETECTION_STATUS_LEVELS, arg = "status")
  detection$status <- status
  bump_version(detection, note = note %||% sprintf("status -> %s", status), actor = actor)
}

# ---- system attachment -------------------------------------------------------

#' Attach an observation to a system
#' @param system An [ai_system()].
#' @param observation An [ai_observation()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`, with the observation stamped to its resulting version and fingerprint.
#' @export
add_observation <- function(system, observation, actor = NULL) {
  assert_class(system, "ai_system")
  assert_class(observation, "ai_observation")
  if (is.null(observation$system_id)) observation$system_id <- system$id
  if (!identical(observation$system_id, system$id)) {
    abort_aitrace(
      sprintf('observation$system_id ("%s") does not match system$id ("%s").',
              observation$system_id, system$id),
      class = "aitrace_error_mismatch"
    )
  }
  oid <- observation$id
  system <- add_to_slot(
    system, "observations", observation,
    note = sprintf("observation added: %s [%s=%s]", observation$id, observation$metric, observation$value),
    actor = actor
  )
  system$observations[[oid]]$system_version     <- as.integer(system$version)
  system$observations[[oid]]$system_fingerprint <- ai_system_fingerprint(system)
  system
}

#' Attach a monitoring specification to a system
#' @param system An [ai_system()].
#' @param spec A [monitoring_spec()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
add_monitoring_spec <- function(system, spec, actor = NULL) {
  assert_class(system, "ai_system")
  assert_class(spec, "monitoring_spec")
  if (is.null(spec$system_id)) spec$system_id <- system$id
  if (!identical(spec$system_id, system$id)) {
    abort_aitrace(
      sprintf('monitoring_spec$system_id ("%s") does not match system$id ("%s").',
              spec$system_id, system$id),
      class = "aitrace_error_mismatch"
    )
  }
  add_to_slot(
    system, "monitoring_specs", spec,
    note = sprintf("monitoring_spec added: %s [%s]", spec$id, spec$metric),
    actor = actor
  )
}

#' Attach a drift assessment to a system
#' @param system An [ai_system()].
#' @param assessment A `drift_assessment` object, as returned by [assess_drift()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`, with the assessment stamped to its resulting version and fingerprint.
#' @export
add_drift_assessment <- function(system, assessment, actor = NULL) {
  assert_class(system, "ai_system")
  assert_class(assessment, "drift_assessment")
  did <- assessment$id
  system <- add_to_slot(
    system, "drift_assessments", assessment,
    note = sprintf("drift_assessment added: %s [%s/%s]", assessment$id, assessment$metric, assessment$result),
    actor = actor
  )
  # Bind to resulting system identity (ops metadata; non-material)
  system$drift_assessments[[did]]$system_id          <- system$id
  system$drift_assessments[[did]]$system_version     <- as.integer(system$version)
  system$drift_assessments[[did]]$system_fingerprint <- ai_system_fingerprint(system)
  system
}

#' Attach a detection signal to a system
#' @param system An [ai_system()].
#' @param detection A [detection_signal()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
add_detection <- function(system, detection, actor = NULL) {
  assert_class(system, "ai_system")
  assert_class(detection, "detection_signal")
  if (!identical(detection$system_id, system$id)) {
    abort_aitrace(
      sprintf('detection$system_id ("%s") does not match system$id ("%s").',
              detection$system_id, system$id),
      class = "aitrace_error_mismatch"
    )
  }
  add_to_slot(
    system, "detections", detection,
    note = sprintf("detection added: %s [%s/%s]", detection$id, detection$severity, detection$status),
    actor = actor
  )
}

#' Evaluate a monitoring_spec against a supplied observation (recording only)
#'
#' Uses the same deterministic criterion engine as evaluation. Does not
#' invent observations or raise incidents automatically.
#' @param spec A [monitoring_spec()].
#' @param observation An [ai_observation()].
#' @return A list with elements `result` and the inputs it was derived from.
#' @export
evaluate_monitoring <- function(spec, observation) {
  assert_class(spec, "monitoring_spec")
  assert_class(observation, "ai_observation")
  if (!identical(spec$metric, observation$metric)) {
    abort_aitrace(
      sprintf("Metric mismatch: monitoring_spec=%s observation=%s",
              spec$metric, observation$metric),
      class = "aitrace_error_mismatch"
    )
  }
  if (is.null(spec$operator) || is.null(spec$criterion)) {
    return(list(result = "INCONCLUSIVE", reason = "monitoring_spec has no criterion"))
  }
  result <- criterion_evaluate(observation$value, spec$operator, spec$criterion, spec$upper)
  list(result = result, metric = spec$metric, observed = observation$value,
       operator = spec$operator, criterion = spec$criterion)
}

#' Tabulate observations attached to a system
#' @param system An [ai_system()].
#' @return A [tibble::tibble()] summarising the system's observations.
#' @export
observation_table <- function(system) {
  assert_class(system, "ai_system")
  items <- unname(system$observations %||% list())
  if (length(items) == 0) {
    return(tibble::tibble(id = character(), metric = character(), value = character(),
                          kind = character(), observed_at = as.POSIXct(character())))
  }
  tibble::tibble(
    id          = vapply(items, `[[`, character(1), "id"),
    metric      = vapply(items, `[[`, character(1), "metric"),
    value       = vapply(items, function(x) as.character(x$value), character(1)),
    kind        = vapply(items, `[[`, character(1), "kind"),
    observed_at = do.call(c, lapply(items, function(x) x$observed_at %||% as.POSIXct(NA)))
  )
}

#' Tabulate detection signals attached to a system
#' @param system An [ai_system()].
#' @return A [tibble::tibble()] summarising the system's detection signals.
#' @export
detection_table <- function(system) {
  assert_class(system, "ai_system")
  items <- unname(system$detections %||% list())
  if (length(items) == 0) {
    return(tibble::tibble(id = character(), severity = character(), status = character(),
                          metric = character(), message = character()))
  }
  tibble::tibble(
    id       = vapply(items, `[[`, character(1), "id"),
    severity = vapply(items, `[[`, character(1), "severity"),
    status   = vapply(items, `[[`, character(1), "status"),
    metric   = vapply(items, function(x) x$metric %||% NA_character_, character(1)),
    message  = vapply(items, `[[`, character(1), "message")
  )
}

#' Tabulate drift assessments attached to a system
#' @param system An [ai_system()].
#' @return A [tibble::tibble()] summarising the system's drift assessments.
#' @export
drift_table <- function(system) {
  assert_class(system, "ai_system")
  items <- unname(system$drift_assessments %||% list())
  if (length(items) == 0) {
    return(tibble::tibble(id = character(), metric = character(), result = character(),
                          baseline = character(), observed = character()))
  }
  tibble::tibble(
    id       = vapply(items, `[[`, character(1), "id"),
    metric   = vapply(items, `[[`, character(1), "metric"),
    result   = vapply(items, `[[`, character(1), "result"),
    baseline = vapply(items, function(x) as.character(x$baseline), character(1)),
    observed = vapply(items, function(x) as.character(x$observed), character(1))
  )
}

# restorers --------------------------------------------------------------------

observation_restore <- function(lst) {
  env <- restore_envelope(lst)
  ts <- function(v) {
    if (is.null(v)) return(NULL)
    if (inherits(v, "POSIXt")) return(v)
    as.POSIXct(v, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  structure(c(env, list(
    system_id = lst$system_id, kind = lst$kind, metric = lst$metric, value = lst$value,
    unit = lst$unit, source = lst$source, observed_at = ts(lst$observed_at) %||% now_utc(),
    window_start = ts(lst$window_start), window_end = ts(lst$window_end),
    system_version = if (is.null(lst$system_version)) NULL else as.integer(lst$system_version),
    system_fingerprint = lst$system_fingerprint
  )), class = c("ai_observation", "aitrace_object"))
}
register_restorer("ai_observation", observation_restore)

monitoring_spec_restore <- function(lst) {
  env <- restore_envelope(lst)
  structure(c(env, list(
    system_id = lst$system_id, name = lst$name, metric = lst$metric, kind = lst$kind,
    operator = lst$operator, criterion = lst$criterion, upper = lst$upper, unit = lst$unit,
    baseline_value = lst$baseline_value, drift_tolerance = lst$drift_tolerance, cadence = lst$cadence
  )), class = c("monitoring_spec", "aitrace_object"))
}
register_restorer("monitoring_spec", monitoring_spec_restore)

drift_assessment_restore <- function(lst) {
  env <- restore_envelope(lst)
  structure(c(env, list(
    metric = lst$metric, baseline = lst$baseline, observed = lst$observed, delta = lst$delta,
    tolerance = lst$tolerance, direction = lst$direction, result = lst$result, unit = lst$unit,
    observation_id = lst$observation_id, monitoring_id = lst$monitoring_id, note = lst$note,
    system_id = lst$system_id,
    system_version = if (is.null(lst$system_version)) NULL else as.integer(lst$system_version),
    system_fingerprint = lst$system_fingerprint
  )), class = c("drift_assessment", "aitrace_object"))
}
register_restorer("drift_assessment", drift_assessment_restore)

detection_signal_restore <- function(lst) {
  env <- restore_envelope(lst)
  ts <- function(v) {
    if (is.null(v)) return(NULL)
    if (inherits(v, "POSIXt")) return(v)
    as.POSIXct(v, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  structure(c(env, list(
    system_id = lst$system_id, severity = lst$severity, metric = lst$metric, message = lst$message,
    observation_id = lst$observation_id, monitoring_id = lst$monitoring_id, drift_id = lst$drift_id,
    evaluation_id = lst$evaluation_id, incident_id = lst$incident_id,
    detected_at = ts(lst$detected_at) %||% now_utc()
  )), class = c("detection_signal", "aitrace_object"))
}
register_restorer("detection_signal", detection_signal_restore)

#' @export
format.ai_observation <- function(x, ...) {
  cli::format_inline("{.strong <ai_observation>} {x$id} [{x$metric}={x$value}]")
}
#' @export
format.monitoring_spec <- function(x, ...) {
  cli::format_inline("{.strong <monitoring_spec>} {x$id} [{x$metric}]")
}
#' @export
format.drift_assessment <- function(x, ...) {
  cli::format_inline("{.strong <drift_assessment>} {x$id} [{x$metric}/{x$result}]")
}
#' @export
format.detection_signal <- function(x, ...) {
  cli::format_inline("{.strong <detection_signal>} {x$id} [{x$severity}/{x$status}]")
}
