test_that("domain objects construct and validate controlled fields", {
  s <- ai_system(id = "SYS-1", name = "n", purpose = "p", risk_class = "HIGH")
  expect_s3_class(s, "ai_system")
  expect_equal(s$status, "DRAFT")
  expect_error(ai_system(id = "SYS-1", name = "n", purpose = "p", risk_class = "NOPE"),
               class = "aitrace_error_choice")

  m <- ai_model(name = "m1", model_type = "CLASSIFICATION")
  expect_s3_class(m, "ai_model")
  d <- ai_data(name = "d1", role = "TRAINING")
  e <- ai_evidence(category = "PERFORMANCE", status = "PASS")
  r <- ai_risk(category = "SAFETY", description = "d", likelihood = "RARE", impact = "MINOR")
  expect_equal(risk_severity("RARE", "MINOR"), "LOW")
})

test_that("add_* attaches components and bumps version", {
  s <- ai_system(id = "SYS-1", name = "n", purpose = "p", risk_class = "LOW")
  m <- ai_model(id = "MOD-1", name = "m1", model_type = "CLASSIFICATION")
  d <- ai_data(id = "DAT-1", name = "d1", role = "TRAINING")
  e <- ai_evidence(id = "EVD-1", category = "PERFORMANCE", status = "PASS")
  r <- ai_risk(id = "RSK-1", category = "SAFETY", description = "d",
               likelihood = "RARE", impact = "MINOR")

  s2 <- s |> add_model(m) |> add_data(d) |> add_evidence(e) |> add_risk(r)
  expect_equal(s2$version, s$version + 4L)
  expect_identical(s2$models[["MOD-1"]], m)
  expect_equal(nrow(change_log(s2)), 4)
})

test_that("system fingerprint is stable and sensitive to material change", {
  s <- ai_system(id = "SYS-1", name = "n", purpose = "p", risk_class = "LOW")
  s <- add_model(s, ai_model(id = "MOD-1", name = "m1", model_type = "CLASSIFICATION"))
  fp1 <- ai_system_fingerprint(s)
  expect_true(fingerprint_verify(s, fp1))

  s2 <- add_data(s, ai_data(id = "DAT-1", name = "d1", role = "TRAINING"))
  fp2 <- ai_system_fingerprint(s2)
  expect_false(identical(fp1, fp2))
})

test_that("incident and improvement closed loop works", {
  s <- ai_system(id = "SYS-1", name = "n", purpose = "p", risk_class = "LOW")
  inc <- ai_incident(system_id = "SYS-1", category = "PERFORMANCE_DEGRADATION",
                     severity = "HIGH", description = "drop")
  s <- add_incident(s, inc)
  expect_equal(length(s$incidents), 1)

  # root cause can only be legitimately identified once an investigation is
  # under way (DETECTED -> TRIAGED -> INVESTIGATING); set_root_cause() only
  # auto-promotes to ROOT_CAUSE_IDENTIFIED from INVESTIGATING/CONTAINED.
  inc1 <- set_incident_state(s$incidents[[inc$id]], "TRIAGED")
  inc1 <- set_incident_state(inc1, "INVESTIGATING")
  inc2 <- set_root_cause(inc1, "sensor drift")
  expect_equal(inc2$status, "ROOT_CAUSE_IDENTIFIED")
  s$incidents[[inc$id]] <- inc2

  s <- raise_improvement_from_incident(
    s, inc$id,
    intervention = "retrain",
    expected_effects = list(performance = "UP")
  )
  expect_equal(length(s$improvements), 1)
  expect_equal(s$incidents[[inc$id]]$linked_improvement_id,
               names(s$improvements)[1])
})

test_that("registry enforces evidence gates and records overrides", {
  reg <- new_registry()
  s <- ai_system(id = "SYS-1", name = "n", purpose = "p", risk_class = "HIGH")
  # add required evidence
  for (cat in c("PERFORMANCE", "SAFETY", "ROBUSTNESS", "SECURITY", "RISK_ASSESSMENT")) {
    s <- add_evidence(s, ai_evidence(category = cat, status = "PASS"))
  }
  registry_add(reg, s)

  registry_transition(reg, "SYS-1", "DEVELOPMENT")
  registry_transition(reg, "SYS-1", "EVALUATING")
  registry_transition(reg, "SYS-1", "VALIDATED")
  registry_transition(reg, "SYS-1", "UNDER_REVIEW")

  system_approve(reg, "SYS-1", actor = "qa")
  expect_equal(registry_get(reg, "SYS-1")$status, "APPROVED")
  system_deploy(reg, "SYS-1")
  expect_equal(registry_get(reg, "SYS-1")$status, "DEPLOYED")
  expect_true(audit_verify_chain(reg))
})

test_that("policy_override requires reason and is audited", {
  reg <- new_registry()
  s <- ai_system(id = "SYS-2", name = "n", purpose = "p", risk_class = "LOW")
  # no evidence
  registry_add(reg, s)
  registry_transition(reg, "SYS-2", "DEVELOPMENT")
  registry_transition(reg, "SYS-2", "EVALUATING")
  registry_transition(reg, "SYS-2", "VALIDATED")
  registry_transition(reg, "SYS-2", "UNDER_REVIEW")

  expect_error(system_approve(reg, "SYS-2"), class = "aitrace_error_policy")
  expect_error(system_approve(reg, "SYS-2", policy_override = TRUE, reason = ""),
               class = "aitrace_error_policy")

  system_approve(reg, "SYS-2", policy_override = TRUE,
                 reason = "emergency change control #42", actor = "cro")
  expect_equal(registry_get(reg, "SYS-2")$status, "APPROVED")
  expect_true(any(reg$audit$event == "GOVERNANCE_POLICY_OVERRIDE"))
  expect_true(audit_verify_chain(reg))
})

test_that("failed evidence does not satisfy gate", {
  reg <- new_registry()
  s <- ai_system(id = "SYS-3", name = "n", purpose = "p", risk_class = "LOW")
  for (cat in c("PERFORMANCE", "ROBUSTNESS", "SECURITY", "RISK_ASSESSMENT")) {
    s <- add_evidence(s, ai_evidence(category = cat, status = "PASS"))
  }
  s <- add_evidence(s, ai_evidence(category = "SAFETY", status = "FAIL"))
  registry_add(reg, s)
  registry_transition(reg, "SYS-3", "DEVELOPMENT")
  registry_transition(reg, "SYS-3", "EVALUATING")
  registry_transition(reg, "SYS-3", "VALIDATED")
  registry_transition(reg, "SYS-3", "UNDER_REVIEW")
  expect_error(system_approve(reg, "SYS-3"), class = "aitrace_error_policy")
})

test_that("JSON export/import preserves audit integrity", {
  reg <- new_registry()
  s <- ai_system(id = "SYS-JSON", name = "n", purpose = "p", risk_class = "LOW")
  s <- add_evidence(s, ai_evidence(category = "SAFETY", status = "PASS"))
  registry_add(reg, s)
  registry_transition(reg, "SYS-JSON", "DEVELOPMENT")

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  registry_export(reg, tmp)
  reg2 <- registry_import(tmp)
  expect_true(audit_verify_chain(reg2))
  expect_equal(registry_list(reg)$id, registry_list(reg2)$id)
})

test_that("lifecycle transition validation works", {
  s <- ai_system(id = "SYS-1", name = "n", purpose = "p", risk_class = "LOW")
  expect_error(set_lifecycle_state(s, "APPROVED"), class = "aitrace_error_transition")
  s <- set_lifecycle_state(s, "REGISTERED")
  expect_equal(s$status, "REGISTERED")
})

test_that("set_lifecycle_state refuses critical transitions (P0)", {
  s <- ai_system(id = "SYS-P0", name = "n", purpose = "p", risk_class = "LOW")
  s <- set_lifecycle_state(s, "REGISTERED")
  s <- set_lifecycle_state(s, "DEVELOPMENT")
  s <- set_lifecycle_state(s, "EVALUATING")
  s <- set_lifecycle_state(s, "VALIDATED")
  s <- set_lifecycle_state(s, "UNDER_REVIEW")

  expect_error(set_lifecycle_state(s, "APPROVED"), class = "aitrace_error_policy")
  # UNDER_REVIEW -> DEPLOYED isn't even in the domain transition graph
  # (APPROVED must come first), so that jump is correctly refused as an
  # invalid *transition*, not as a bypassed *critical-governance* gate.
  expect_error(set_lifecycle_state(s, "DEPLOYED"), class = "aitrace_error_transition")

  # Exercise the critical-governance guard for DEPLOYED specifically: force
  # a (graph-valid) APPROVED state, as one would need to via the registry,
  # then confirm set_lifecycle_state() still refuses the bare APPROVED ->
  # DEPLOYED jump outside governance context.
  s_approved <- s
  s_approved$status <- "APPROVED"
  expect_error(set_lifecycle_state(s_approved, "DEPLOYED"), class = "aitrace_error_policy")

  # non-critical still works
  s2 <- set_lifecycle_state(s, "SUSPENDED")
  expect_equal(s2$status, "SUSPENDED")
})

test_that("registry can still reach APPROVED after gates (governance context)", {
  reg <- new_registry()
  s <- ai_system(id = "SYS-P0b", name = "n", purpose = "p", risk_class = "LOW")
  for (cat in c("PERFORMANCE", "SAFETY", "ROBUSTNESS", "SECURITY", "RISK_ASSESSMENT")) {
    s <- add_evidence(s, ai_evidence(category = cat, status = "PASS"))
  }
  registry_add(reg, s)
  registry_transition(reg, "SYS-P0b", "DEVELOPMENT")
  registry_transition(reg, "SYS-P0b", "EVALUATING")
  registry_transition(reg, "SYS-P0b", "VALIDATED")
  registry_transition(reg, "SYS-P0b", "UNDER_REVIEW")
  system_approve(reg, "SYS-P0b")
  expect_equal(registry_get(reg, "SYS-P0b")$status, "APPROVED")
})

test_that("registry_update rejects critical status promotion", {
  reg <- new_registry()
  s <- ai_system(id = "SYS-P0c", name = "n", purpose = "p", risk_class = "LOW")
  registry_add(reg, s)
  # forge a critical status on a detached copy
  s2 <- registry_get(reg, "SYS-P0c")
  s2$status <- "APPROVED"
  expect_error(registry_update(reg, s2), class = "aitrace_error_policy")
})

test_that("allowed_transitions and validate_system work", {
  s <- ai_system(id = "SYS-V", name = "n", purpose = "p", risk_class = "LOW")
  at <- allowed_transitions(s)
  expect_true("REGISTERED" %in% at || "DEVELOPMENT" %in% at)
  chk <- validate_system(s)
  expect_true(chk$ok)
})

test_that("evidence is version-bound and stale evidence fails the gate", {
  reg <- new_registry()
  s <- ai_system(id = "SYS-EV", name = "n", purpose = "p", risk_class = "LOW")
  for (cat in c("PERFORMANCE", "SAFETY", "ROBUSTNESS", "SECURITY", "RISK_ASSESSMENT")) {
    s <- add_evidence(s, ai_evidence(category = cat, status = "PASS"))
  }
  # evidence stamped at current version
  ev <- s$evidence[[1]]
  expect_equal(ev$system_id, "SYS-EV")
  expect_true(!is.null(ev$system_version))
  expect_true(!is.null(ev$system_fingerprint))
  expect_true(evidence_applies_to(ev, s))

  registry_add(reg, s)
  # material change invalidates fingerprints
  s2 <- registry_get(reg, "SYS-EV")
  s2 <- add_model(s2, ai_model(id = "MOD-X", name = "new", model_type = "CLASSIFICATION"))
  # old evidence no longer applies
  expect_false(evidence_applies_to(s2$evidence[[1]], s2))
  chk <- check_evidence_completeness(s2, default_approval_policy()$approval_requires)
  expect_false(chk$ok)
  expect_true(length(chk$inapplicable) > 0)
})

test_that("validate_traceability detects broken links", {
  s <- ai_system(id = "SYS-T", name = "n", purpose = "p", risk_class = "LOW")
  inc <- ai_incident(system_id = "SYS-T", description = "d", severity = "LOW")
  s <- add_incident(s, inc)
  # forge a broken improvement link
  s$incidents[[inc$id]]$linked_improvement_id <- "IMP-MISSING"
  tr <- validate_traceability(s)
  expect_false(tr$ok)
  expect_true(any(grepl("missing improvement", tr$issues)))
})

test_that("aitrace_policy rejects invalid categories", {
  expect_error(aitrace_policy(approval_requires = "NOT_A_CATEGORY"),
               class = "aitrace_error_choice")
  p <- default_approval_policy()
  expect_true(validate_policy(p)$ok)
})

test_that("registry_add validates system structure", {
  reg <- new_registry()
  s <- ai_system(id = "SYS-BAD", name = "n", purpose = "p", risk_class = "LOW")
  s$name <- ""  # break structure
  expect_error(registry_add(reg, s), class = "aitrace_error_validation")
})

test_that("fingerprint is order-independent for models", {
  s1 <- ai_system(id = "SYS-F", name = "n", purpose = "p", risk_class = "LOW")
  s1 <- add_model(s1, ai_model(id = "M1", name = "a", model_type = "CLASSIFICATION"))
  s1 <- add_model(s1, ai_model(id = "M2", name = "b", model_type = "REGRESSION"))
  s2 <- ai_system(id = "SYS-F", name = "n", purpose = "p", risk_class = "LOW")
  s2 <- add_model(s2, ai_model(id = "M2", name = "b", model_type = "REGRESSION"))
  s2 <- add_model(s2, ai_model(id = "M1", name = "a", model_type = "CLASSIFICATION"))
  # versions differ due to add order, so compare canonicalize material only
  # force same version for fair compare
  s1$version <- 3L; s2$version <- 3L
  expect_identical(ai_system_fingerprint(s1), ai_system_fingerprint(s2))
})

test_that("P0: newly attached evidence is immediately applicable (post-mutation stamp)", {
  s <- ai_system(id = "SYS-P0E", name = "n", purpose = "p", risk_class = "LOW")
  v0 <- s$version
  s <- add_evidence(s, ai_evidence(id = "EVD-1", category = "PERFORMANCE", status = "PASS"))
  ev <- s$evidence[["EVD-1"]]
  # stamped with resulting version, not pre-mutation
  expect_equal(as.integer(ev$system_version), as.integer(s$version))
  expect_true(as.integer(ev$system_version) > v0)
  expect_true(evidence_applies_to(ev, s))
  # fingerprint matches (evidence is non-material)
  expect_identical(ev$system_fingerprint, ai_system_fingerprint(s))
})

test_that("P0: fresh evidence satisfies the approval gate", {
  reg <- new_registry()
  s <- ai_system(id = "SYS-P0G", name = "n", purpose = "p", risk_class = "LOW")
  for (cat in c("PERFORMANCE", "SAFETY", "ROBUSTNESS", "SECURITY", "RISK_ASSESSMENT")) {
    s <- add_evidence(s, ai_evidence(category = cat, status = "PASS"))
  }
  chk <- check_evidence_completeness(s, default_approval_policy()$approval_requires)
  expect_true(chk$ok)
  expect_equal(length(chk$inapplicable), 0)

  registry_add(reg, s)
  registry_transition(reg, "SYS-P0G", "DEVELOPMENT")
  registry_transition(reg, "SYS-P0G", "EVALUATING")
  registry_transition(reg, "SYS-P0G", "VALIDATED")
  registry_transition(reg, "SYS-P0G", "UNDER_REVIEW")
  system_approve(reg, "SYS-P0G")
  expect_equal(registry_get(reg, "SYS-P0G")$status, "APPROVED")
})

test_that("evidence remains applicable after non-material governance change", {
  s <- ai_system(id = "SYS-NM", name = "n", purpose = "p", risk_class = "LOW")
  s <- add_evidence(s, ai_evidence(id = "EVD-NM", category = "SAFETY", status = "PASS"))
  fp_before <- ai_system_fingerprint(s)
  # lifecycle change is non-material
  s2 <- set_lifecycle_state(s, "REGISTERED")
  expect_identical(ai_system_fingerprint(s2), fp_before)
  # fingerprint still matches → evidence remains applicable despite version bump
  expect_true(evidence_applies_to(s2$evidence[["EVD-NM"]], s2))
})

test_that("fingerprint unchanged by evidence/risk; changes on material slot", {
  s <- ai_system(id = "SYS-FP", name = "n", purpose = "p", risk_class = "LOW")
  fp0 <- ai_system_fingerprint(s)
  s <- add_evidence(s, ai_evidence(category = "SAFETY", status = "PASS"))
  expect_identical(ai_system_fingerprint(s), fp0)
  s <- add_risk(s, ai_risk(category = "SAFETY", description = "d",
                           likelihood = "RARE", impact = "MINOR"))
  expect_identical(ai_system_fingerprint(s), fp0)
  s <- add_model(s, ai_model(id = "M1", name = "m", model_type = "CLASSIFICATION"))
  expect_false(identical(ai_system_fingerprint(s), fp0))
})
