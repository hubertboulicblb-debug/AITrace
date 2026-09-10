testthat::test_that("requirements evaluate explicitly to PASS/FAIL/INCONCLUSIVE", {
  req <- evidence_requirement(id = "REQ-1", category = "PERFORMANCE", name = "AUC target", metric = "auc", operator = "GE", criterion = 0.80)
  pass <- evaluate_requirement(req, 0.85)
  fail <- evaluate_requirement(req, 0.75)
  inc  <- evaluate_requirement(req, NA_real_)
  testthat::expect_identical(pass$result, "PASS")
  testthat::expect_identical(fail$result, "FAIL")
  testthat::expect_identical(inc$result, "INCONCLUSIVE")
})

testthat::test_that("conflicting explicit evaluation results are rejected", {
  testthat::expect_error(
    evaluation(metric = "auc", observed = 0.85, operator = "GE", criterion = 0.80, result = "FAIL"),
    class = "aitrace_error_evaluation"
  )
})

testthat::test_that("baseline comparison records supplied improvement direction", {
  cmp <- compare_baseline("accuracy", 0.80, 0.84, direction = "UP")
  testthat::expect_identical(cmp$result, "IMPROVED")
  cmp2 <- compare_baseline("error", 0.10, 0.08, direction = "DOWN")
  testthat::expect_identical(cmp2$result, "IMPROVED")
  cmp3 <- compare_baseline("accuracy", 0.80, 0.80, direction = "UP")
  testthat::expect_identical(cmp3$result, "NO_CHANGE")
})

testthat::test_that("system can record evaluation without changing material fingerprint", {
  sys <- ai_system(id = "SYS-1", name = "Demo", purpose = "Evaluation test")
  fp <- ai_system_fingerprint(sys)
  req <- evidence_requirement(id = "REQ-1", name = "Accuracy", metric = "accuracy", criterion = 0.8)
  sys <- add_evidence_requirement(sys, req)
  ev <- evaluate_requirement(req, 0.9)
  sys <- add_evaluation(sys, ev)
  testthat::expect_true("REQ-1" %in% names(sys$evidence_requirements))
  testthat::expect_equal(length(sys$evaluations), 1L)
  testthat::expect_identical(ai_system_fingerprint(sys), fp)
})

testthat::test_that("evaluation and comparison tables are empty-safe", {
  sys <- ai_system(id = "SYS-2", name = "Demo", purpose = "Tables")
  testthat::expect_equal(nrow(evaluation_table(sys)), 0)
  testthat::expect_equal(nrow(comparison_table(sys)), 0)
})


testthat::test_that("evaluation objects round-trip through JSON", {
  req <- evidence_requirement(id = "REQ-JSON", name = "Accuracy", metric = "accuracy", criterion = 0.8)
  ev <- evaluate_requirement(req, 0.9)
  restored <- from_json(to_json(ev, pretty = FALSE))
  testthat::expect_s3_class(restored, "evaluation")
  testthat::expect_identical(restored$result, "PASS")
  testthat::expect_identical(restored$metric, "accuracy")
})

testthat::test_that("traceability sees new evaluation slots", {
  sys <- ai_system(id = "SYS-3", name = "Demo", purpose = "Traceability")
  req <- evidence_requirement(id = "REQ-3", name = "Accuracy", metric = "accuracy", criterion = 0.8)
  sys <- add_evidence_requirement(sys, req)
  sys <- add_evaluation(sys, evaluate_requirement(req, 0.9))
  sys <- add_baseline_comparison(sys, compare_baseline("accuracy", 0.8, 0.9))
  testthat::expect_true(validate_system(sys)$ok)
  testthat::expect_true(validate_traceability(sys)$ok)
})
