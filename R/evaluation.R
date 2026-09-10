#' Evaluation result levels
#' @export
EVALUATION_RESULT_LEVELS <- c("PASS", "FAIL", "INCONCLUSIVE")

#' Criterion operators
#' @export
CRITERION_OPERATOR_LEVELS <- c("EQ", "NE", "LT", "LE", "GT", "GE", "BETWEEN", "OUTSIDE", "IN_SET", "NOT_IN_SET")

#' Baseline comparison result levels
#' @export
COMPARISON_RESULT_LEVELS <- c("IMPROVED", "NO_CHANGE", "REGRESSED", "INCONCLUSIVE")

assert_scalar_numeric_or_character <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env(), allow_na = FALSE) {
  ok <- (is.numeric(x) || is.character(x)) && length(x) == 1L && (allow_na || !is.na(x))
  if (!ok) {
    msg <- if (allow_na) {
      sprintf("`%s` must be a single numeric or character value (NA allowed), not %s.", arg, obj_desc(x))
    } else {
      sprintf("`%s` must be a single non-missing numeric or character value, not %s.", arg, obj_desc(x))
    }
    abort_aitrace(msg, class = "aitrace_error_type", call = call)
  }
  invisible(NULL)
}

criterion_validate <- function(operator, value, upper = NULL) {
  assert_choice(operator, CRITERION_OPERATOR_LEVELS, arg = "operator")
  if (operator %in% c("BETWEEN", "OUTSIDE")) {
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || !is.numeric(upper) || length(upper) != 1L || is.na(upper) || value > upper) {
      abort_aitrace("BETWEEN/OUTSIDE criteria require numeric lower value and numeric upper value with lower <= upper.", class = "aitrace_error_type")
    }
  } else if (operator %in% c("IN_SET", "NOT_IN_SET")) {
    if (length(value) < 1L || anyNA(value)) abort_aitrace("IN_SET/NOT_IN_SET criteria require at least one non-missing allowed value.", class = "aitrace_error_type")
  } else {
    assert_scalar_numeric_or_character(value, arg = "criterion")
  }
  invisible(NULL)
}

criterion_evaluate <- function(observed, operator, value, upper = NULL) {
  if (length(observed) != 1L || is.na(observed)) return("INCONCLUSIVE")
  tryCatch({
    ans <- switch(operator,
      EQ = observed == value, NE = observed != value,
      LT = observed < value, LE = observed <= value,
      GT = observed > value, GE = observed >= value,
      BETWEEN = observed >= value && observed <= upper,
      OUTSIDE = observed < value || observed > upper,
      IN_SET = observed %in% value, NOT_IN_SET = !(observed %in% value)
    )
    if (length(ans) != 1L || is.na(ans)) "INCONCLUSIVE" else if (isTRUE(ans)) "PASS" else "FAIL"
  }, error = function(e) "INCONCLUSIVE")
}

#' Define an evidence requirement
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param category Controlled category value.
#' @param name Character name.
#' @param metric Metric name (character scalar), e.g. "AUROC" or "latency_p99".
#' @param operator One of [CRITERION_OPERATOR_LEVELS].
#' @param criterion Criterion value the observation is compared against.
#' @param upper Optional numeric upper bound; required for the `BETWEEN`/`OUTSIDE` operators.
#' @param unit Optional character unit of measure.
#' @param description Character description.
#' @param owner Optional character owner identifier.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `evidence_requirement` object.
#' @export
evidence_requirement <- function(id = NULL, category = "PERFORMANCE", name, metric,
                                 operator = "GE", criterion, upper = NULL, unit = NULL,
                                 description = NULL, owner = NULL, metadata = list(), provenance = list()) {
  assert_choice(category, EVIDENCE_CATEGORIES, arg = "category")
  assert_scalar_character(name, arg = "name"); assert_scalar_character(metric, arg = "metric")
  criterion_validate(operator, criterion, upper)
  if (!is.null(unit)) assert_scalar_character(unit, arg = "unit")
  if (!is.null(description)) assert_scalar_character(description, arg = "description")
  if (is.null(id)) id <- new_id("REQ")
  new_aitrace_object("evidence_requirement", id, fields = list(
    category = category, name = name, metric = metric, operator = operator,
    criterion = criterion, upper = upper, unit = unit, description = description
  ), owner = owner, status = "ACTIVE", metadata = metadata, provenance = provenance)
}

#' Construct an evaluation record from an explicit observation and criterion
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param requirement_id Identifier of the related [evidence_requirement()].
#' @param evidence_id Identifier of the related [ai_evidence()].
#' @param metric Metric name (character scalar), e.g. "AUROC" or "latency_p99".
#' @param observed Observed value (numeric or character scalar).
#' @param unit Optional character unit of measure.
#' @param operator One of [CRITERION_OPERATOR_LEVELS].
#' @param criterion Criterion value the observation is compared against.
#' @param upper Optional numeric upper bound; required for the `BETWEEN`/`OUTSIDE` operators.
#' @param result Result value; validated against the relevant controlled vocabulary.
#' @param observed_at When the value was observed (`POSIXct`); defaults to now (UTC).
#' @param note Optional character note recorded in the change log.
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @param owner Optional character owner identifier.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `evaluation` object.
#' @export
evaluation <- function(id = NULL, requirement_id = NULL, evidence_id = NULL, metric,
                        observed, unit = NULL, operator = NULL, criterion = NULL, upper = NULL,
                        result = NULL, observed_at = NULL, note = NULL, actor = NULL,
                        owner = NULL, metadata = list(), provenance = list()) {
  # `observed` may be NA: criterion_evaluate() treats a missing observation
  # as INCONCLUSIVE by design (it must not be silently dropped or coerced
  # into a fabricated PASS/FAIL), so the constructor has to admit it too.
  assert_scalar_character(metric, arg = "metric"); assert_scalar_numeric_or_character(observed, arg = "observed", allow_na = TRUE)
  if (!is.null(requirement_id)) assert_scalar_character(requirement_id, arg = "requirement_id")
  if (!is.null(evidence_id)) assert_scalar_character(evidence_id, arg = "evidence_id")
  if (!is.null(unit)) assert_scalar_character(unit, arg = "unit")
  if (!is.null(operator)) {
    criterion_validate(operator, criterion, upper)
    derived <- criterion_evaluate(observed, operator, criterion, upper)
    if (!is.null(result) && !identical(result, derived)) abort_aitrace(sprintf("Supplied result \"%s\" conflicts with criterion-derived result \"%s\".", result, derived), class = "aitrace_error_evaluation")
    result <- derived
  }
  if (is.null(result)) result <- "INCONCLUSIVE"
  assert_choice(result, EVALUATION_RESULT_LEVELS, arg = "result")
  if (!is.null(observed_at) && !inherits(observed_at, "POSIXt")) assert_scalar_character(observed_at, arg = "observed_at")
  if (is.null(id)) id <- new_id("EVAL")
  new_aitrace_object("evaluation", id, fields = list(
    requirement_id = requirement_id, evidence_id = evidence_id, metric = metric,
    observed = observed, unit = unit, operator = operator, criterion = criterion,
    upper = upper, result = result, observed_at = observed_at, note = note, actor = actor
  ), owner = owner, status = result, metadata = metadata, provenance = provenance)
}

#' Evaluate an evidence requirement against an observed value
#' @param requirement An [evidence_requirement()].
#' @param observed Observed value (numeric or character scalar).
#' @param evidence_id Identifier of the related [ai_evidence()].
#' @param observed_at When the value was observed (`POSIXct`); defaults to now (UTC).
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @param note Optional character note recorded in the change log.
#' @return A new `evaluation` object.
#' @export
evaluate_requirement <- function(requirement, observed, evidence_id = NULL,
                                  observed_at = NULL, actor = NULL, note = NULL) {
  assert_class(requirement, "evidence_requirement")
  evaluation(requirement_id = requirement$id, evidence_id = evidence_id,
             metric = requirement$metric, observed = observed, unit = requirement$unit,
             operator = requirement$operator, criterion = requirement$criterion,
             upper = requirement$upper, observed_at = observed_at, actor = actor, note = note)
}

#' Compare supplied baseline and candidate observations
#' @param metric Metric name (character scalar), e.g. "AUROC" or "latency_p99".
#' @param baseline Baseline value (numeric or character scalar).
#' @param candidate Candidate (post-change) value (numeric or character scalar).
#' @param direction One of [EFFECT_DIRECTION_LEVELS] describing which direction counts as improvement.
#' @param unit Optional character unit of measure.
#' @param baseline_label Label for the baseline value in tables/output.
#' @param candidate_label Label for the candidate value in tables/output.
#' @param note Optional character note recorded in the change log.
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param owner Optional character owner identifier.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `baseline_comparison` object.
#' @export
compare_baseline <- function(metric, baseline, candidate, direction = "UP", unit = NULL,
                             baseline_label = "baseline", candidate_label = "candidate",
                             note = NULL, id = NULL, owner = NULL, metadata = list(), provenance = list()) {
  assert_scalar_character(metric, arg = "metric"); assert_scalar_numeric_or_character(baseline, arg = "baseline"); assert_scalar_numeric_or_character(candidate, arg = "candidate")
  assert_choice(direction, EFFECT_DIRECTION_LEVELS, arg = "direction")
  result <- "INCONCLUSIVE"
  if (is.numeric(baseline) && is.numeric(candidate)) {
    result <- if (candidate == baseline) "NO_CHANGE" else if (direction == "UP" && candidate > baseline) "IMPROVED" else if (direction == "DOWN" && candidate < baseline) "IMPROVED" else if (direction == "MAINTAIN") "REGRESSED" else "REGRESSED"
  } else if (identical(baseline, candidate)) result <- "NO_CHANGE"
  baseline_comparison(id = id, metric = metric, baseline = baseline, candidate = candidate,
                      direction = direction, result = result, unit = unit,
                      baseline_label = baseline_label, candidate_label = candidate_label,
                      note = note, owner = owner, metadata = metadata, provenance = provenance)
}

#' Construct a baseline comparison record
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param metric Metric name (character scalar), e.g. "AUROC" or "latency_p99".
#' @param baseline Baseline value (numeric or character scalar).
#' @param candidate Candidate (post-change) value (numeric or character scalar).
#' @param direction One of [EFFECT_DIRECTION_LEVELS] describing which direction counts as improvement.
#' @param result Result value; validated against the relevant controlled vocabulary.
#' @param unit Optional character unit of measure.
#' @param baseline_label Label for the baseline value in tables/output.
#' @param candidate_label Label for the candidate value in tables/output.
#' @param note Optional character note recorded in the change log.
#' @param owner Optional character owner identifier.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `baseline_comparison` object.
#' @export
baseline_comparison <- function(id = NULL, metric, baseline, candidate, direction = "UP",
                                result = "INCONCLUSIVE", unit = NULL, baseline_label = "baseline",
                                candidate_label = "candidate", note = NULL, owner = NULL,
                                metadata = list(), provenance = list()) {
  assert_scalar_character(metric, arg = "metric"); assert_scalar_numeric_or_character(baseline, arg = "baseline"); assert_scalar_numeric_or_character(candidate, arg = "candidate")
  assert_choice(direction, EFFECT_DIRECTION_LEVELS, arg = "direction"); assert_choice(result, COMPARISON_RESULT_LEVELS, arg = "result")
  if (!is.null(unit)) assert_scalar_character(unit, arg = "unit")
  assert_scalar_character(baseline_label, arg = "baseline_label"); assert_scalar_character(candidate_label, arg = "candidate_label")
  if (is.null(id)) id <- new_id("CMP")
  new_aitrace_object("baseline_comparison", id, fields = list(
    metric = metric, baseline = baseline, candidate = candidate, direction = direction,
    result = result, unit = unit, baseline_label = baseline_label,
    candidate_label = candidate_label, note = note
  ), owner = owner, status = result, metadata = metadata, provenance = provenance)
}

#' Assess an observed value against an explicit criterion
#' @param observed Observed value (numeric or character scalar).
#' @param operator One of [CRITERION_OPERATOR_LEVELS].
#' @param criterion Criterion value the observation is compared against.
#' @param upper Optional numeric upper bound; required for the `BETWEEN`/`OUTSIDE` operators.
#' @return A single character result, one of [EVALUATION_RESULT_LEVELS].
#' @export
assess_acceptance <- function(observed, operator, criterion, upper = NULL) {
  criterion_validate(operator, criterion, upper); criterion_evaluate(observed, operator, criterion, upper)
}

#' Construct a regression assessment record
#' @param id Optional identifier. Auto-generated (with a type-specific prefix) if `NULL`.
#' @param metric Metric name (character scalar), e.g. "AUROC" or "latency_p99".
#' @param baseline Baseline value (numeric or character scalar).
#' @param candidate Candidate (post-change) value (numeric or character scalar).
#' @param direction One of [EFFECT_DIRECTION_LEVELS] describing which direction counts as improvement.
#' @param unit Optional character unit of measure.
#' @param note Optional character note recorded in the change log.
#' @param owner Optional character owner identifier.
#' @param metadata Optional named list of free-form metadata.
#' @param provenance Optional named list recording where/how this record originated.
#' @return A new `baseline_comparison` object.
#' @export
regression_assessment <- function(id = NULL, metric = "regression_assessment", baseline, candidate, direction = "UP", unit = NULL, note = NULL, owner = NULL, metadata = list(), provenance = list()) {
  compare_baseline(metric = metric, baseline = baseline, candidate = candidate, direction = direction, unit = unit, note = note, id = id, owner = owner, metadata = metadata, provenance = provenance)
}

#' Assess a baseline/candidate pair using an explicit improvement direction
#' @param baseline Baseline value (numeric or character scalar).
#' @param candidate Candidate (post-change) value (numeric or character scalar).
#' @param direction One of [EFFECT_DIRECTION_LEVELS] describing which direction counts as improvement.
#' @return A new `baseline_comparison` object.
#' @export
assess_regression <- function(baseline, candidate, direction = "UP") {
  compare_baseline(metric = "regression_assessment", baseline = baseline, candidate = candidate, direction = direction)
}

# System attachment --------------------------------------------------------

#' Attach an evidence requirement to a system
#' @param system An [ai_system()].
#' @param requirement An [evidence_requirement()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
add_evidence_requirement <- function(system, requirement, actor = NULL) {
  add_to_slot(system, "evidence_requirements", requirement,
              note = sprintf("evidence requirement added: %s [%s/%s]", requirement$id, requirement$category, requirement$metric),
              actor = actor)
}

#' Attach an evaluation record to a system
#' @param system An [ai_system()].
#' @param evaluation An [evaluation()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
add_evaluation <- function(system, evaluation, actor = NULL) {
  assert_class(system, "ai_system"); assert_class(evaluation, "evaluation")
  if (is.null(system$evaluations)) system$evaluations <- list()
  if (evaluation$id %in% names(system$evaluations)) abort_aitrace(sprintf("Object with id \"%s\" already present in slot \"evaluations\".", evaluation$id), class = "aitrace_error_duplicate")
  system$evaluations[[evaluation$id]] <- evaluation
  bump_version(system, note = sprintf("evaluation added: %s [%s]", evaluation$id, evaluation$result), actor = actor)
}

#' Attach a baseline comparison record to a system
#' @param system An [ai_system()].
#' @param comparison A [baseline_comparison()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
add_baseline_comparison <- function(system, comparison, actor = NULL) {
  assert_class(system, "ai_system"); assert_class(comparison, "baseline_comparison")
  if (is.null(system$baseline_comparisons)) system$baseline_comparisons <- list()
  if (comparison$id %in% names(system$baseline_comparisons)) abort_aitrace(sprintf("Object with id \"%s\" already present in slot \"baseline_comparisons\".", comparison$id), class = "aitrace_error_duplicate")
  system$baseline_comparisons[[comparison$id]] <- comparison
  bump_version(system, note = sprintf("baseline comparison added: %s [%s]", comparison$id, comparison$result), actor = actor)
}

#' Attach a regression assessment (alias for [add_baseline_comparison()])
#' @param system An [ai_system()].
#' @param comparison A [baseline_comparison()].
#' @param actor Optional character identifying who performed the action; recorded in the change log.
#' @return The updated `ai_system`.
#' @export
add_regression_assessment <- function(system, comparison, actor = NULL) add_baseline_comparison(system, comparison, actor = actor)

#' Tabulate evaluation records attached to a system
#' @param system An [ai_system()].
#' @return A [tibble::tibble()] summarising the system's evaluation records.
#' @export
evaluation_table <- function(system) {
  assert_class(system, "ai_system"); items <- unname(system$evaluations %||% list())
  if (!length(items)) return(tibble::tibble(id=character(), requirement_id=character(), evidence_id=character(), metric=character(), observed=character(), result=character()))
  tibble::tibble(id=vapply(items, `[[`, character(1), "id"), requirement_id=vapply(items,function(x)x$requirement_id%||%NA_character_,character(1)), evidence_id=vapply(items,function(x)x$evidence_id%||%NA_character_,character(1)), metric=vapply(items,`[[`,character(1),"metric"), observed=vapply(items,function(x)paste(x$observed,collapse=", "),character(1)), result=vapply(items,`[[`,character(1),"result"))
}

#' Tabulate baseline comparison records attached to a system
#' @param system An [ai_system()].
#' @return A [tibble::tibble()] summarising the system's baseline comparisons.
#' @export
comparison_table <- function(system) {
  assert_class(system, "ai_system"); items <- unname(system$baseline_comparisons %||% list())
  if (!length(items)) return(tibble::tibble(id=character(), metric=character(), baseline=character(), candidate=character(), direction=character(), result=character()))
  tibble::tibble(id=vapply(items,`[[`,character(1),"id"), metric=vapply(items,`[[`,character(1),"metric"), baseline=vapply(items,function(x)paste(x$baseline,collapse=", "),character(1)), candidate=vapply(items,function(x)paste(x$candidate,collapse=", "),character(1)), direction=vapply(items,`[[`,character(1),"direction"), result=vapply(items,`[[`,character(1),"result"))
}

# Restorers ---------------------------------------------------------------
evidence_requirement_restore <- function(lst) {
  env <- restore_envelope(lst); structure(c(env,list(category=lst$category,name=lst$name,metric=lst$metric,operator=lst$operator,criterion=lst$criterion,upper=lst$upper,unit=lst$unit,description=lst$description)),class=c("evidence_requirement","aitrace_object"))
}
register_restorer("evidence_requirement", evidence_requirement_restore)

evaluation_restore <- function(lst) {
  env <- restore_envelope(lst); observed_at <- lst$observed_at
  if (!is.null(observed_at) && is.character(observed_at)) observed_at <- as.POSIXct(observed_at,format="%Y-%m-%dT%H:%M:%SZ",tz="UTC")
  structure(c(env,list(requirement_id=lst$requirement_id,evidence_id=lst$evidence_id,metric=lst$metric,observed=lst$observed,unit=lst$unit,operator=lst$operator,criterion=lst$criterion,upper=lst$upper,result=lst$result,observed_at=observed_at,note=lst$note,actor=lst$actor)),class=c("evaluation","aitrace_object"))
}
register_restorer("evaluation", evaluation_restore)

baseline_comparison_restore <- function(lst) {
  env <- restore_envelope(lst); structure(c(env,list(metric=lst$metric,baseline=lst$baseline,candidate=lst$candidate,direction=lst$direction,result=lst$result,unit=lst$unit,baseline_label=lst$baseline_label,candidate_label=lst$candidate_label,note=lst$note)),class=c("baseline_comparison","aitrace_object"))
}
register_restorer("baseline_comparison", baseline_comparison_restore)

#' @export
format.evidence_requirement <- function(x, ...) cli::format_inline("{.strong <evidence_requirement>} {x$id} [{x$category}/{x$metric} {x$operator} {x$criterion}]")
#' @export
format.evaluation <- function(x, ...) cli::format_inline("{.strong <evaluation>} {x$id} [{x$metric}/{x$result}]")
#' @export
format.baseline_comparison <- function(x, ...) cli::format_inline("{.strong <baseline_comparison>} {x$id} [{x$metric}/{x$result}]")
