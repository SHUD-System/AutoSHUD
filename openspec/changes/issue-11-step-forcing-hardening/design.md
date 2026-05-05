## Context

Issue #11 comes from real-data Step1-3 validation. The run succeeded only after report-local handling for a zero-length case1-US stream segment, a Step3 CRS object mismatch with current rSHUD, and a local station forcing file that covered 2001-2005 while the validation config declared one year. The permanent fix belongs primarily in AutoSHUD because these are pipeline input and publication contracts.

## Goals / Non-Goals

**Goals:**

- Make Step1 reject or safely remove stream features that cannot form valid Step3 river input.
- Make Step3 pass CRS values accepted by current rSHUD `ForcingCoverage()` and preserve existing local/dummy/gridded forcing branches.
- Ensure local station forcing used by Step3 matches the configured simulation window or fails clearly.
- Preserve ERA5 branch year-directory and discovery-limit behavior already introduced by `era5-forcing-branch`.
- Preserve Step3 forcing ID safety behavior: unsafe `meteoCov` IDs remain rejected, valid custom IDs continue to define forcing CSV basenames.
- Keep validation/report scripts out of product code.

**Non-Goals:**

- Do not productize WEM fallback raw derivation.
- Do not edit benchmark/source GIS or forcing data.
- Do not add hydrologic calibration or model accuracy criteria.
- Do not move rSHUD defensive river-network checks into AutoSHUD; rSHUD has a separate issue.

## Decisions

- Add small AutoSHUD helper functions near the affected Step scripts or under `Rfunction/` instead of importing the validation scripts. Rationale: the validation scripts include orchestration, source-copy patching, and reporting that are not runtime behavior.
- In Step1, validate stream geometry after reading and CRS assignment but before writing `pd.pcs$stm`. Features that are invalid/empty/zero-length after transform must be removed with a warning; if no features remain, fail with an actionable error. Alternative considered: fail on any bad feature. That is too strict for common GIS data where isolated degenerate lines can be safely removed.
- For missing stream CRS, use the configured/project CRS only when there is a defensible source CRS. If neither the stream nor watershed/project context can determine it, fail clearly rather than guessing.
- In Step3, normalize CRS inputs to `sf::crs` for rSHUD forcing coverage. Alternative considered: continue wrapping into `sp::CRS`; validation showed current rSHUD expects the sf-compatible path.
- For local station forcing (`Forcing >= 1`), enforce the project time window on report/output copies only. Source fixture files must never be mutated. If an existing file cannot be parsed safely, fail before writing misleading metadata.

## Risk Triage

Issue type: bugfix
Project profile: AutoSHUD
Blast radius: high
Fixture level: expanded
Why:
- Touches Step entrypoints and generated SHUD input.
- Touches geospatial CRS/shapefile behavior.
- Touches forcing CSV temporal boundaries and file publication.
- Must preserve legacy examples and ERA5 branch behavior.

## Risk Packs Considered

- Public API / CLI / script entry: selected - Step1 and Step3 are user-facing scripts.
- Config / project setup: selected - behavior depends on `startyear/endyear/STARTDAY/ENDDAY`, `fsp.stm`, `fsp.forc`, and ERA5 keys.
- File IO / path safety / overwrite: selected - local forcing must be enforced on output copies without mutating sources.
- Schema / columns / units / field names: selected - forcing CSV headers and meteoCov IDs must remain Step3-compatible.
- Geospatial / CRS / shapefile sidecars: selected - stream cleanup and CRS conversion are central.
- Time series / forcing / temporal boundaries: selected - local forcing must match configured window.
- Numerical stability / conservation / NaN: not selected - no solver/numerical simulation changes.
- Solver runtime / performance / threading: not selected - no SHUD solver runtime behavior changes.
- Resource limits / large input / discovery: selected - ERA5 discovery safeguards must not regress.
- Legacy compatibility / examples: selected - existing examples and forcing branches must continue.
- Error handling / rollback / partial outputs: selected - failures should not publish misleading or partial final outputs.
- Release / packaging / dependency compatibility: selected - no new heavy dependencies; current R package expectations must hold.
- Documentation / migration notes: selected - users need the new boundary documented.

## Required Evidence

- Focused R tests for stream cleanup and failure when no valid stream remains.
- Focused R tests for Step3 CRS normalization against a stub/current-compatible `ForcingCoverage()`.
- Focused R tests for local forcing year-window enforcement, including `STARTDAY/ENDDAY` sub-year windows and source file preservation.
- Focused R tests that unsafe `meteoCov` IDs remain rejected and valid custom IDs continue to drive forcing CSV basenames.
- Existing ERA5 tests, especially file discovery/year pattern/limit tests, must pass; missing requested year and no-grid-hit failure paths must remain explicit no-partial-output errors.
- Smoke command: `Rscript tests/test-era5-forcing.R`.

## Review Focus

- Does Step1 cleanup avoid silently changing valid river geometry?
- Does Step3 preserve meteoCov `ID` values and current rSHUD CRS compatibility?
- Does local forcing enforcement mutate only generated/output copies, never source fixture data?
- Do failures stop before misleading Step3 metadata is written?
