# AITrace Roadmap

## Delivered
- **0.2.2** — Evidence, traceability, audit, fingerprint (FROZEN)
- **0.2.3** — Evaluation semantics (FROZEN)
- **0.3.0** — Decision + Change (FROZEN)
- **0.4.0** — Observation / monitoring / drift / detection recording (FROZEN)
- **0.4.1** — Hardening: directional drift semantics, timestamp/version
  validation, declarative monitoring_spec default, stricter detection/drift
  traceability, registry_import() re-validation and explicit schema-version
  policy (current)

## Next
### 0.4.x hardening (remaining)
- Registry schema migration framework (0.4.1 only rejects unrecognised
  versions; it does not yet convert older ones)
- Optional detection → incident helper (still explicit, never silent)
- State machine for detection lifecycle
- Richer observation windows / batches
- Production verification adapters (measurement in)

### Later
- Policy-as-code expansions
- Governance cockpit (Shiny) — deferred until the 0.4.x layer is stable
- Formal change-state transition graph
