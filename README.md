# AITrace

### Traceable governance infrastructure for AI systems

**AITrace** is an R package for building structured, auditable and traceable governance records around AI systems.

It provides a deterministic data model for connecting:

**AI System → Evidence → Evaluation → Decision → Change → Deployment → Verification**

and, from version 0.4.0 onward:

**Observation → Monitoring → Drift → Detection → Incident → Improvement**

AITrace is designed for environments where an AI system is not only expected to work, but where an organisation must also be able to demonstrate **what was evaluated, why a decision was made, what changed, what was deployed, and what happened afterwards**.

The package is particularly relevant to **pharmaceutical, healthcare, scientific, regulated and quality-sensitive environments**.

---

## Why AITrace?

AI governance is often fragmented across:

* model documentation
* validation reports
* spreadsheets
* tickets
* change-control systems
* monitoring dashboards
* experiment records
* deployment records
* incident-management systems

These records may exist independently while the critical relationships between them remain implicit.

AITrace provides a programmatic layer for making those relationships explicit.

Instead of treating governance as a collection of documents, AITrace treats it as a **traceable evidence graph**.

```text
                         ┌───────────────┐
                         │   AI SYSTEM   │
                         └───────┬───────┘
                                 │
                         material identity
                                 │
                  ┌──────────────▼──────────────┐
                  │           EVIDENCE           │
                  └──────────────┬──────────────┘
                                 │
                         evaluation criteria
                                 │
                  ┌──────────────▼──────────────┐
                  │         EVALUATION           │
                  └──────────────┬──────────────┘
                                 │
                         governance decision
                                 │
                  ┌──────────────▼──────────────┐
                  │           DECISION            │
                  └──────────────┬──────────────┘
                                 │
                            approved change
                                 │
                  ┌──────────────▼──────────────┐
                  │            CHANGE             │
                  └──────────────┬──────────────┘
                                 │
                            deployment
                                 │
                  ┌──────────────▼──────────────┐
                  │          DEPLOYMENT           │
                  └──────────────┬──────────────┘
                                 │
                            verification
                                 │
                  ┌──────────────▼──────────────┐
                  │         VERIFICATION         │
                  └─────────────────────────────┘


        Operational monitoring
                 │
                 ▼
        ┌─────────────────┐
        │   OBSERVATION   │
        └────────┬────────┘
                 │
                 ▼
        ┌─────────────────┐
        │    MONITORING   │
        └────────┬────────┘
                 │
                 ▼
        ┌─────────────────┐
        │      DRIFT      │
        └────────┬────────┘
                 │
                 ▼
        ┌─────────────────┐
        │    DETECTION    │
        └────────┬────────┘
                 │
                 ▼
        ┌─────────────────┐
        │    INCIDENT     │
        └─────────────────┘
```

`DEPLOYMENT` and `VERIFICATION` above are not separate object constructors —
they are status fields recorded directly on an `ai_change()` (see
[Main objects](#main-objects) and the worked example below).

---

# Core principles

AITrace is deliberately built around several principles.

### 1. Traceability over narrative

Governance should be reconstructable from structured records rather than relying exclusively on prose.

### 2. Evidence before decision

An approval should be connected to the evidence and evaluations on which it was based.

### 3. Material identity is distinct from operational observation

Changing an observation does not change the identity of the AI system.

Changing the model, data, component or other material system attributes can.

This distinction is fundamental to the AITrace fingerprint model.

### 4. Deterministic governance logic

AITrace records governance decisions and their rationale.

It does **not** attempt to replace human governance with an opaque autonomous approval engine.

### 5. Monitoring is not automatically an incident

A detected deviation is a signal.

It does not automatically become an incident, CAPA, rollback or deployment decision.

Those are separate governance steps — linking a `detection_signal()` to an
incident is an explicit call (`link_detection_incident()`), never implicit.

### 6. Historical records matter

AITrace is designed to preserve the relationship between a system version and the evidence, decisions and changes associated with that version.

---

# Architecture

AITrace 0.4.x contains two closely related layers.

## Decision and change layer

The core governance lifecycle is:

```text
Evidence
   ↓
Evaluation
   ↓
Decision
   ↓
Change
   ↓
Deployment
   ↓
Verification
```

This layer answers questions such as:

* What evidence supported the evaluation?
* Which requirements were evaluated?
* What was the observed result?
* Who made the governance decision?
* Why was a change authorised?
* Which system version was deployed?
* Was deployment subsequently verified?

---

## Operational observation layer

Version 0.4.0 introduces:

```text
Observation
    ↓
Monitoring specification
    ↓
Drift assessment
    ↓
Detection signal
    ↓
Incident / governance response
```

The corresponding API includes:

```r
ai_observation()
monitoring_spec()
assess_drift()
detection_signal()
```

This layer deliberately does not automatically make governance decisions.

Version 0.4.1 is a hardening pass on this layer (see
[Version status](#version-status)): it does not add new objects, but it does
change some default and validation behaviour described below.

---

# Main objects

| Object                                              | Purpose                                                       |
| ---------------------------------------------------- | -------------------------------------------------------------- |
| `ai_system()`                                        | Defines an AI system and its material identity                 |
| `ai_evidence()`                                      | Records supporting evidence                                    |
| `evidence_requirement()`                             | Defines evidence required by a governance process               |
| `evaluation()`                                       | Records an evaluation against a requirement                     |
| `baseline_comparison()` / `regression_assessment()`  | Records comparison against a baseline                           |
| `ai_decision()`                                      | Records a governance decision                                   |
| `ai_change()`                                        | Records an authorised system change, including its `deployment_status` and `verification_status` |
| `set_change_deployment()`                            | Updates a change's `deployment_status` (one of `DEPLOYMENT_STATUS_LEVELS`) |
| `set_change_verification()`                          | Updates a change's `verification_status` (one of `VERIFICATION_RESULT_LEVELS`) |
| `ai_observation()`                                   | Records an observed operational measurement                     |
| `monitoring_spec()`                                  | Defines what is monitored, and optionally a criterion/baseline/tolerance |
| `assess_drift()`                                     | Compares a baseline and an observed value                       |
| `detection_signal()`                                 | Records a resulting detection signal                            |
| `ai_incident()`                                      | Records an operational governance incident                      |
| `new_registry()`                                     | Creates a registry — the control-plane container for governed AI systems |

---

# Example

Create an AI system:

```r
library(AITrace)

system <- ai_system(
  name       = "Manufacturing Quality Prediction",
  purpose    = "Predict blend uniformity risk",
  risk_class = "MEDIUM",
  owner      = "Manufacturing Data Science"
)
```

`id` and `version` do not need to be supplied — `id` is auto-generated
(`system$id`) and `version` starts at `1L` and is bumped by governance
mutations.

Record an observation:

```r
obs <- ai_observation(
  system_id = system$id,
  metric    = "prediction_error",
  value     = 0.042
)
```

`observed_at` defaults to the current UTC time if not supplied, and must be
a `POSIXct` value if you do supply one.

Define what is being monitored:

```r
spec <- monitoring_spec(
  system_id       = system$id,
  name            = "Prediction error ceiling",
  metric          = "prediction_error",
  operator        = "LE",
  criterion       = 0.040,
  baseline_value  = 0.030,
  drift_tolerance = 0.010
)
```

Assess drift against the spec's baseline:

```r
drift <- assess_drift(
  metric    = obs$metric,
  baseline  = spec$baseline_value,
  observed  = obs$value,
  tolerance = spec$drift_tolerance
)
```

Record a detection signal when the drift result warrants one — a
`detection_signal()`'s own `status` is a workflow state (`OPEN`,
`ACKNOWLEDGED`, ...), not the drift result itself, so the drift result
belongs in `metric`/`message`/`drift_id`, not `status`:

```r
if (drift$result == "DRIFT_DETECTED") {
  signal <- detection_signal(
    system_id      = system$id,
    severity       = "WARNING",
    message        = sprintf("%s drifted beyond tolerance", obs$metric),
    metric         = obs$metric,
    observation_id = obs$id,
    monitoring_id  = spec$id,
    drift_id       = drift$id
  )
}
```

Attaching these to the system (`add_observation()`, `add_monitoring_spec()`,
`add_drift_assessment()`, `add_detection()`) binds them to the system's
current version/fingerprint and stores them on the `ai_system` object; the
standalone constructors above are also usable on their own for recording
and testing.

---

# Fingerprinting

AITrace distinguishes **material system identity** from operational governance records.

The system fingerprint is intended to change when material system characteristics change.

Examples include:

```text
Model
Data
Components
Configuration
Evidence
System version
```

Operational records such as:

```text
Observation
Monitoring result
Drift assessment
Detection signal
Incident
Decision
```

do not themselves redefine the material identity of the AI system.

This allows an organisation to answer two different questions:

> **What exactly was this AI system?**

and:

> **What happened to this AI system over time?**

Those questions should not be conflated.

---

# Traceability

AITrace provides traceability validation across governance objects via
`validate_traceability(system)`.

A typical lineage can be represented as:

```text
AI System
   │
   ├── Evidence
   │      │
   │      └── Evaluation
   │              │
   │              └── Decision
   │
   └── Change
          │
          └── Deployment
                 │
                 └── Verification
```

Operational monitoring extends this:

```text
AI System
   │
   └── Observation
          │
          └── Drift Assessment
                 │
                 └── Detection
                        │
                        └── Incident
```

The objective is not merely to store objects, but to make missing or
inconsistent relationships detectable. `validate_traceability()` returns a
list with `ok`, `issues` and `warnings`: a set-but-unresolved reference
(for example a `detection_signal` pointing at an `observation_id` that
does not exist on the system) is reported as an `issue`; a reference that
resolves but is bound to a stale system version or fingerprint is reported
as a `warning`.

---

# Audit trail

Registry operations maintain an auditable transition history.

The audit model is designed around:

* explicit state transitions
* actor attribution
* timestamps
* reasons
* previous and resulting states
* chained audit records (`audit_append()`)
* verification of audit-chain integrity (`audit_verify_chain()`)

The audit trail should be understood as **tamper-evident**, rather than mathematically immutable.

---

# Registry

A registry provides a higher-level container for governed AI systems.

```r
reg <- new_registry()

registry_add(reg, system)
```

A registry is exported to, and imported from, a JSON file on disk (not an
in-memory object):

```r
registry_export(reg, "registry_snapshot.json")

reg2 <- registry_import("registry_snapshot.json")
```

`registry_import()` is a controlled boundary: the audit chain is
cryptographically re-verified, and every restored system is re-run through
`validate_system()` before the registry is accepted — a structurally broken
or tampered snapshot is rejected rather than silently loaded. The snapshot's
`schema_version` is also checked against the versions this package release
can import; an unrecognised version is rejected explicitly rather than
guessed at.

---

# Serialization

AITrace objects are designed for deterministic serialization and restoration.

For example:

```r
json <- to_json(system)

restored <- from_json(json)
```

`from_json()` recovers the object's type from the serialized data itself —
there is no separate `type` argument to get out of sync with the payload.
This supports persistence, exchange and reproducible reconstruction of
governance records.

---

# Controlled vocabularies

Governance states and statuses are represented through controlled vocabularies rather than arbitrary strings wherever appropriate.

For example:

```r
DEPLOYMENT_STATUS_LEVELS
```

provides the controlled deployment-status vocabulary.

This reduces ambiguity in downstream reporting, validation and integration.

---

# What AITrace does not do

AITrace is intentionally **not**:

* an AI model-training framework
* an MLOps platform
* a model-serving platform
* a statistical validation engine
* an autonomous approval system
* an incident-management replacement
* a monitoring dashboard
* a substitute for organisational quality systems
* a substitute for human scientific or governance judgement

AITrace provides **governance infrastructure and traceability**.

It can sit underneath or alongside those systems.

---

# Regulated environments

AITrace can support governance architectures in environments where traceability, reproducibility and controlled change are important.

Potential applications include:

* pharmaceutical AI
* manufacturing analytics
* process analytical technology (PAT)
* laboratory analytics
* scientific decision-support systems
* quality systems
* regulated data science
* AI/ML validation
* model lifecycle governance
* post-deployment monitoring

AITrace itself does not make an AI system automatically GxP-compliant.

Compliance depends on the complete organisational process, intended use, risk assessment, validation strategy, infrastructure, controls and applicable regulations.

---

# Installation

From a source tarball:

```r
install.packages(
  "AITrace_0.4.1.tar.gz",
  repos = NULL,
  type = "source"
)
```

Or from a local development tree:

```r
devtools::install()
```

Then:

```r
library(AITrace)
```

---

# Development

Clone the repository and install the development dependencies.

Run the test suite:

```r
devtools::test()
```

Build the package:

```r
devtools::build()
```

Run package checks:

```bash
R CMD check --as-cran AITrace_0.4.1.tar.gz
```

The source tree is structured for standard R package tooling, with a
generated `NAMESPACE` and documentation maintained through **roxygen2**
(`Roxygen: list(markdown = TRUE)`, an explicit `Collate:` field in
`DESCRIPTION`), rather than either being hand-edited.

---

# Package quality

The 0.4.1 release has been developed with emphasis on:

* deterministic object construction
* explicit validation (including timestamp and version-field type checks)
* controlled vocabularies
* serialization/restoration
* traceability
* audit-chain verification
* regression testing (175 `testthat` tests as of 0.4.1)
* roxygen2-managed exports and documentation
* standard R package structure

`R CMD check --as-cran` runs with 0 errors and 0 warnings; the remaining
NOTEs on a disconnected build machine are network/clock-verification
artifacts of that environment, not package defects.

For formal deployment, users should perform their own environment-specific validation, including the applicable R version, package dependencies, infrastructure, intended use and organisational quality requirements.

---

# Version status

## 0.4.0 — Observation / monitoring / drift / detection (frozen)

Version 0.4.0 extended the governance foundation with an operational
observation and monitoring layer:

```text
ai_observation()
monitoring_spec()
assess_drift()
detection_signal()
```

The decision/change layer remains the foundation. The monitoring layer is
intentionally additive and does not automatically alter governance state.

## 0.4.1 — Hardening pass (current)

0.4.1 does not add new objects; it hardens the 0.4.0 layer:

* `assess_drift()`'s `direction` argument now actually affects the result
  (it previously had no effect on the calculation).
* `ai_observation()` and `detection_signal()` validate their timestamp
  fields as `POSIXct`, and `ai_observation()` validates `system_version`
  as a positive integer instead of silently coercing bad input to `NA`.
* `monitoring_spec()` defaults to a purely declarative spec
  (`operator = NULL`, `criterion = NULL`) rather than a default that looked
  executable but could never resolve to anything but `INCONCLUSIVE`.
* `validate_traceability()` treats a set-but-dangling detection/drift
  reference as an `issue`, consistent with how other structural references
  are already treated.
* `registry_import()` re-validates every restored system with
  `validate_system()` and checks the snapshot's `schema_version` against an
  explicit supported-versions list, rather than accepting anything that
  parses as JSON.

### Open (tracked in `ROADMAP.md`)

* A registry schema **migration** framework — 0.4.1 only rejects
  unrecognised `schema_version` values; it does not yet convert an older
  snapshot forward.
* An optional, still-explicit detection → incident helper.
* A state machine for detection lifecycle.
* Richer observation windows / batches.

A graphical monitoring cockpit is deliberately considered a later layer,
deferred until the 0.4.x recording/governance layer is stable, rather than
part of the core governance model.

---

# Design philosophy

AITrace follows a simple principle:

> **An AI system should be governable as a traceable object, not merely documented as a report.**

The package therefore focuses on the links between:

**Science → Data → AI → Evidence → Decision → Change → Deployment → Observation → Response**

The goal is to make those links explicit, inspectable and reproducible.

---

# License

AITrace is distributed under the **MIT License**.

See the `LICENSE` file for the complete license text.

---

# Author

**Hubert Boulic, MSc**

AI, Data Science, Scientific Analytics & Regulated Systems

---

# Contributing

Issues, technical feedback and contributions are welcome.

When proposing changes to the governance model, please consider:

1. whether the change affects material system identity;
2. whether it affects serialization;
3. whether it changes traceability semantics;
4. whether controlled vocabularies need updating;
5. whether backward compatibility is affected;
6. whether new regression tests are required.

Governance semantics should be treated as part of the package API, not merely implementation details.

---

# Disclaimer

AITrace is software infrastructure for AI governance and traceability.

It does not itself establish regulatory compliance, validate a particular AI system, determine fitness for intended use, or replace qualified scientific, statistical, quality, regulatory or governance review.

Users remain responsible for establishing and documenting the controls appropriate to their intended use and operating environment.
