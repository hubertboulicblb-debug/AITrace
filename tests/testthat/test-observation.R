test_that("ai_observation requires a real value", {
  expect_error(ai_observation(metric = "x", value = NA_real_), class = "aitrace_error_type")
  o <- ai_observation(metric = "latency_p99", value = 42.5, unit = "ms", kind = "LATENCY")
  expect_s3_class(o, "ai_observation")
  expect_equal(o$value, 42.5)
})

test_that("observation attachment binds version without changing fingerprint", {
  s <- ai_system(id = "SYS-O", name = "n", purpose = "p", risk_class = "LOW")
  fp <- ai_system_fingerprint(s)
  s <- add_observation(s, ai_observation(metric = "AUROC", value = 0.91, kind = "METRIC"))
  expect_equal(length(s$observations), 1)
  obs <- s$observations[[1]]
  expect_equal(obs$system_id, "SYS-O")
  expect_equal(as.integer(obs$system_version), as.integer(s$version))
  expect_identical(ai_system_fingerprint(s), fp)
})

test_that("assess_drift is conservative and deterministic", {
  d0 <- assess_drift("AUROC", baseline = 0.90, observed = 0.90, tolerance = 0.02)
  expect_equal(d0$result, "NO_DRIFT")
  d1 <- assess_drift("AUROC", baseline = 0.90, observed = 0.80, tolerance = 0.02)
  expect_equal(d1$result, "DRIFT_DETECTED")
  d2 <- assess_drift("AUROC", baseline = "a", observed = "b")
  expect_equal(d2$result, "INCONCLUSIVE")
  # no tolerance + unequal numerics → INCONCLUSIVE (not automatic drift)
  d3 <- assess_drift("latency", baseline = 100, observed = 101)
  expect_equal(d3$result, "INCONCLUSIVE")
  d4 <- assess_drift("latency", baseline = 100, observed = 100.0001, tolerance = 0.01)
  expect_equal(d4$result, "NO_DRIFT")
})

test_that("evaluate_monitoring uses criterion engine", {
  mon <- monitoring_spec(name = "AUROC floor", metric = "AUROC", operator = "GE", criterion = 0.85)
  ok <- ai_observation(metric = "AUROC", value = 0.91)
  bad <- ai_observation(metric = "AUROC", value = 0.70)
  expect_equal(evaluate_monitoring(mon, ok)$result, "PASS")
  expect_equal(evaluate_monitoring(mon, bad)$result, "FAIL")
})

test_that("detection can link to incident without inventing one", {
  s <- ai_system(id = "SYS-D", name = "n", purpose = "p", risk_class = "LOW")
  det <- detection_signal(system_id = "SYS-D", severity = "WARNING",
                          message = "AUROC below floor", metric = "AUROC")
  s <- add_detection(s, det)
  expect_null(s$detections[[1]]$incident_id)
  inc <- ai_incident(system_id = "SYS-D", description = "perf drop", severity = "MEDIUM")
  s <- add_incident(s, inc)
  s$detections[[1]] <- link_detection_incident(s$detections[[1]], inc$id)
  expect_equal(s$detections[[1]]$incident_id, inc$id)
})

test_that("closed loop recording path leaves fingerprint stable for ops metadata", {
  s <- ai_system(id = "SYS-L", name = "n", purpose = "p", risk_class = "LOW")
  fp <- ai_system_fingerprint(s)
  s <- add_monitoring_spec(s, monitoring_spec(name = "lat", metric = "latency_p99",
                                             operator = "LE", criterion = 100, unit = "ms"))
  s <- add_observation(s, ai_observation(metric = "latency_p99", value = 150, kind = "LATENCY"))
  dft <- assess_drift("latency_p99", baseline = 80, observed = 150, tolerance = 20)
  s <- add_drift_assessment(s, dft)
  s <- add_detection(s, detection_signal(system_id = "SYS-L", severity = "CRITICAL",
                                        message = "latency drift", metric = "latency_p99",
                                        drift_id = dft$id))
  expect_identical(ai_system_fingerprint(s), fp)
  expect_true(validate_system(s)$ok)
  expect_true(validate_traceability(s)$ok)
  expect_equal(nrow(observation_table(s)), 1)
  expect_equal(nrow(drift_table(s)), 1)
  expect_equal(nrow(detection_table(s)), 1)
})

test_that("JSON round-trip for observation and detection", {
  o <- ai_observation(id = "OBS-1", metric = "x", value = 1.2, kind = "METRIC")
  r <- from_json(to_json(o, pretty = FALSE))
  expect_s3_class(r, "ai_observation")
  expect_equal(r$value, 1.2)
  d <- detection_signal(id = "DET-1", system_id = "S", message = "m", severity = "INFO")
  r2 <- from_json(to_json(d, pretty = FALSE))
  expect_s3_class(r2, "detection_signal")
})

test_that("mismatch system_id rejected for observation and detection", {
  s <- ai_system(id = "SYS-1", name = "n", purpose = "p", risk_class = "LOW")
  expect_error(add_observation(s, ai_observation(system_id = "OTHER", metric = "m", value = 1)),
               class = "aitrace_error_mismatch")
  expect_error(add_detection(s, detection_signal(system_id = "OTHER", message = "m")),
               class = "aitrace_error_mismatch")
})


test_that("P1: assess_drift without tolerance is INCONCLUSIVE for unequal values", {
  expect_equal(assess_drift("m", 100, 101)$result, "INCONCLUSIVE")
  expect_equal(assess_drift("m", 0.9, 0.900001)$result, "INCONCLUSIVE")
  expect_equal(assess_drift("m", 100, 100)$result, "NO_DRIFT")
  expect_equal(assess_drift("m", 100, 105, tolerance = 10)$result, "NO_DRIFT")
  expect_equal(assess_drift("m", 100, 111, tolerance = 10)$result, "DRIFT_DETECTED")
})

test_that("drift assessment is stamped with system identity on attachment", {
  s <- ai_system(id = "SYS-DFT", name = "n", purpose = "p", risk_class = "LOW")
  dft <- assess_drift("AUROC", 0.9, 0.8, tolerance = 0.05)
  s <- add_drift_assessment(s, dft)
  bound <- s$drift_assessments[[1]]
  expect_equal(bound$system_id, "SYS-DFT")
  expect_equal(as.integer(bound$system_version), as.integer(s$version))
  expect_identical(bound$system_fingerprint, ai_system_fingerprint(s))
})


test_that("tolerance is validated at assess_drift entry", {
  expect_error(assess_drift("m", 100, 100, tolerance = -1), class = "aitrace_error_type")
  expect_error(assess_drift("m", 100, 101, tolerance = -0.01), class = "aitrace_error_type")
  # equal values with valid tolerance still NO_DRIFT
  expect_equal(assess_drift("m", 100, 100, tolerance = 0)$result, "NO_DRIFT")
})
