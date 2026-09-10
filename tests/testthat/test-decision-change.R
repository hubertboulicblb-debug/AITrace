test_that("ai_decision constructs and validates types", {
  d <- ai_decision(system_id = "SYS-1", decision_type = "APPROVE", actor = "qa")
  expect_s3_class(d, "ai_decision")
  expect_equal(d$status, "RECORDED")
  expect_error(ai_decision(system_id = "SYS-1", decision_type = "NOPE"),
               class = "aitrace_error_choice")
  expect_error(
    ai_decision(system_id = "SYS-1", decision_type = "OVERRIDE_POLICY"),
    class = "aitrace_error_policy"
  )
  d2 <- ai_decision(system_id = "SYS-1", decision_type = "OVERRIDE_POLICY",
                    rationale = "emergency #42", actor = "cro")
  expect_equal(d2$decision_type, "OVERRIDE_POLICY")
})

test_that("add_decision binds to resulting system version without changing fingerprint", {
  s <- ai_system(id = "SYS-D", name = "n", purpose = "p", risk_class = "LOW")
  fp0 <- ai_system_fingerprint(s)
  s <- record_system_decision(s, "APPROVE", rationale = "meets criteria",
                              actor = "qa", authority = "Governance Board")
  expect_equal(length(s$decisions), 1)
  dec <- s$decisions[[1]]
  expect_equal(dec$system_id, "SYS-D")
  expect_equal(as.integer(dec$system_version), as.integer(s$version))
  expect_identical(ai_system_fingerprint(s), fp0)
  tab <- decision_table(s)
  expect_equal(nrow(tab), 1)
  expect_equal(tab$decision_type[1], "APPROVE")
})

test_that("ai_change open/close captures versions and fingerprints", {
  s <- ai_system(id = "SYS-C", name = "n", purpose = "p", risk_class = "LOW")
  v0 <- s$version
  fp0 <- ai_system_fingerprint(s)
  s <- open_change(s, change_type = "MODEL", reason = "retrain", impact = "MAJOR")
  expect_equal(length(s$changes), 1)
  cid <- names(s$changes)[1]
  # from_version is stamped before the add_change version bump
  expect_equal(as.integer(s$changes[[cid]]$from_version), as.integer(v0))
  expect_true(as.integer(s$version) > as.integer(v0))
  # material mutation
  s <- add_model(s, ai_model(id = "M1", name = "m", model_type = "CLASSIFICATION"))
  fp1 <- ai_system_fingerprint(s)
  expect_false(identical(fp0, fp1))
  s <- close_change(s, cid)
  ch <- s$changes[[cid]]
  expect_equal(as.integer(ch$to_version), as.integer(s$version))
  expect_identical(ch$to_fingerprint, fp1)
  expect_false(identical(ch$from_fingerprint, ch$to_fingerprint))
})

test_that("change deployment and verification status", {
  ch <- ai_change(system_id = "SYS-1", change_type = "DATA", change_status = "APPROVED",
                  from_version = 1L, to_version = 2L)
  ch <- set_change_deployment(ch, "DEPLOYED")
  expect_equal(ch$deployment_status, "DEPLOYED")
  expect_equal(ch$status, "DEPLOYED")
  ch <- set_change_verification(ch, "CONFIRMED")
  expect_equal(ch$verification_status, "CONFIRMED")
  expect_equal(ch$status, "VERIFIED")
})

test_that("mismatch system_id rejected", {
  s <- ai_system(id = "SYS-1", name = "n", purpose = "p", risk_class = "LOW")
  d <- ai_decision(system_id = "SYS-OTHER", decision_type = "APPROVE")
  expect_error(add_decision(s, d), class = "aitrace_error_mismatch")
  c <- ai_change(system_id = "SYS-OTHER", change_type = "OTHER")
  expect_error(add_change(s, c), class = "aitrace_error_mismatch")
})

test_that("decision/change JSON round-trip", {
  d <- ai_decision(id = "DEC-1", system_id = "SYS-1", decision_type = "DEPLOY",
                   actor = "ops", rationale = "ready")
  r <- from_json(to_json(d, pretty = FALSE))
  expect_s3_class(r, "ai_decision")
  expect_equal(r$decision_type, "DEPLOY")

  c <- ai_change(id = "CHG-1", system_id = "SYS-1", change_type = "MODEL",
                 from_version = 1L, to_version = 2L, impact = "MAJOR")
  r2 <- from_json(to_json(c, pretty = FALSE))
  expect_s3_class(r2, "ai_change")
  expect_equal(as.integer(r2$from_version), 1L)
})

test_that("validate_traceability sees decision and change refs", {
  s <- ai_system(id = "SYS-T", name = "n", purpose = "p", risk_class = "LOW")
  s <- record_system_decision(s, "APPROVE", rationale = "ok", actor = "qa")
  s <- open_change(s, change_type = "PROCESS", reason = "sop update")
  expect_true(validate_system(s)$ok)
  tr <- validate_traceability(s)
  expect_true(tr$ok)
})

test_that("empty decision_table and change_table are safe", {
  s <- ai_system(id = "SYS-E", name = "n", purpose = "p", risk_class = "LOW")
  expect_equal(nrow(decision_table(s)), 0)
  expect_equal(nrow(change_table(s)), 0)
})

test_that("P0: close_change stamps to_version on resulting system revision", {
  s <- ai_system(id = "SYS-CLOSE", name = "n", purpose = "p", risk_class = "LOW")
  s <- open_change(s, change_type = "MODEL", reason = "retrain")
  cid <- names(s$changes)[1]
  s <- add_model(s, ai_model(id = "M-CLOSE", name = "m", model_type = "CLASSIFICATION"))
  fp_after_material <- ai_system_fingerprint(s)
  s <- close_change(s, cid)
  ch <- s$changes[[cid]]
  expect_equal(as.integer(ch$to_version), as.integer(s$version))
  expect_identical(ch$to_fingerprint, fp_after_material)
  expect_identical(ch$to_fingerprint, ai_system_fingerprint(s))
  expect_false(identical(ch$from_fingerprint, ch$to_fingerprint))
})
