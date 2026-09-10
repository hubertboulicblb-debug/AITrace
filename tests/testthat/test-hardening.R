# 0.4.1 hardening pass: directional drift semantics, timestamp/version
# validation on the new recording objects, monitoring_spec's declarative
# default, stricter detection/drift traceability, and registry_import()
# scrutiny (schema version + validate_system()).

test_that("assess_drift direction controls which side of tolerance is drift", {
  # MAINTAIN (default): symmetric, either side beyond tolerance is drift
  expect_equal(assess_drift("m", 100, 111, tolerance = 10)$result, "DRIFT_DETECTED")
  expect_equal(assess_drift("m", 100, 89,  tolerance = 10)$result, "DRIFT_DETECTED")

  # UP: only an increase beyond tolerance counts as drift
  up_increase <- assess_drift("latency_p99", 100, 130, tolerance = 10, direction = "UP")
  expect_equal(up_increase$result, "DRIFT_DETECTED")
  up_decrease <- assess_drift("latency_p99", 100, 70, tolerance = 10, direction = "UP")
  expect_equal(up_decrease$result, "NO_DRIFT")

  # DOWN: only a decrease beyond tolerance counts as drift
  down_decrease <- assess_drift("AUROC", 0.90, 0.70, tolerance = 0.05, direction = "DOWN")
  expect_equal(down_decrease$result, "DRIFT_DETECTED")
  down_increase <- assess_drift("AUROC", 0.90, 0.99, tolerance = 0.05, direction = "DOWN")
  expect_equal(down_increase$result, "NO_DRIFT")

  # direction never invents drift when tolerance is absent, or masks equality
  expect_equal(assess_drift("m", 100, 101, direction = "UP")$result, "INCONCLUSIVE")
  expect_equal(assess_drift("m", 100, 100, tolerance = 5, direction = "UP")$result, "NO_DRIFT")

  expect_error(assess_drift("m", 100, 101, tolerance = 1, direction = "SIDEWAYS"),
               class = "aitrace_error_choice")
})

test_that("ai_observation validates timestamp types and window ordering", {
  expect_error(ai_observation(metric = "m", value = 1, observed_at = "2026-01-01"),
               class = "aitrace_error_type")
  expect_error(ai_observation(metric = "m", value = 1,
                              window_start = as.POSIXct("2026-01-02", tz = "UTC"),
                              window_end   = as.POSIXct("2026-01-01", tz = "UTC")),
               class = "aitrace_error_type")
  # valid POSIXct window is fine
  ok <- ai_observation(metric = "m", value = 1,
                       window_start = as.POSIXct("2026-01-01", tz = "UTC"),
                       window_end   = as.POSIXct("2026-01-02", tz = "UTC"))
  expect_s3_class(ok, "ai_observation")
})

test_that("ai_observation rejects a non-integer system_version instead of coercing to NA", {
  expect_error(ai_observation(metric = "m", value = 1, system_version = "abc"),
               class = "aitrace_error_type")
  expect_error(ai_observation(metric = "m", value = 1, system_version = -1),
               class = "aitrace_error_type")
  expect_error(ai_observation(metric = "m", value = 1, system_version = 1.5),
               class = "aitrace_error_type")
  ok <- ai_observation(metric = "m", value = 1, system_version = 3)
  expect_equal(ok$system_version, 3L)
})

test_that("detection_signal validates detected_at", {
  expect_error(
    detection_signal(system_id = "S", message = "m", detected_at = "2026-01-01"),
    class = "aitrace_error_type"
  )
})

test_that("monitoring_spec defaults to declarative (no criterion) rather than a silent GE", {
  spec <- monitoring_spec(name = "n", metric = "AUROC")
  expect_null(spec$operator)
  expect_null(spec$criterion)
  obs <- ai_observation(metric = "AUROC", value = 0.91)
  expect_equal(evaluate_monitoring(spec, obs)$result, "INCONCLUSIVE")
})

test_that("validate_traceability treats dangling detection/drift refs as issues", {
  s <- ai_system(id = "SYS-H", name = "n", purpose = "p", risk_class = "LOW")
  det <- detection_signal(system_id = "SYS-H", message = "m",
                          observation_id = "OBS-DOES-NOT-EXIST")
  s <- add_detection(s, det)
  tr <- validate_traceability(s)
  expect_false(tr$ok)
  expect_true(any(grepl("refs missing observation", tr$issues)))
})

test_that("registry_import rejects an unsupported schema_version", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  reg <- new_registry()
  s <- ai_system(id = "SYS-SCHEMA", name = "n", purpose = "p", risk_class = "LOW")
  registry_add(reg, s)
  registry_export(reg, tmp)
  snap <- jsonlite::read_json(tmp, simplifyVector = FALSE)
  snap$schema_version <- "9.9.9"
  jsonlite::write_json(snap, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  expect_error(registry_import(tmp), class = "aitrace_error_validation")
})

test_that("registry_import re-validates restored systems (duplicate component ids rejected)", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  reg <- new_registry()
  s <- ai_system(id = "SYS-DUP", name = "n", purpose = "p", risk_class = "LOW")
  s <- add_observation(s, ai_observation(id = "OBS-X", metric = "m", value = 1))
  registry_add(reg, s)
  registry_export(reg, tmp)
  snap <- jsonlite::read_json(tmp, simplifyVector = FALSE)
  obs <- snap$systems[["SYS-DUP"]]$observations[["OBS-X"]]
  snap$systems[["SYS-DUP"]]$observations[["OBS-Y"]] <- obs
  snap$systems[["SYS-DUP"]]$observations[["OBS-Y"]]$id <- "OBS-X"  # duplicate id, different key
  jsonlite::write_json(snap, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  expect_error(registry_import(tmp), class = "aitrace_error_validation")
})
