# 9035800 Step4-5 Acceptance

Run the repository-contained Step4-5 acceptance from the AutoSHUD repository
root:

```sh
Rscript tests/test-9035800-step45-acceptance.R
```

The harness generates temporary 9035800 Step1-3 model inputs, verifies the
required Step3 files, then runs real SHUD Step4 and Step5 smoke checks. Step4
requires an existing local SHUD source checkout. Resolution order is explicit
argument, `options("autoshud.shud_source")`, `AUTOSHUD_SHUD_SOURCE`,
`AUTOSHUD_SHUD_SOURCE_DIR`, project config, then `../SHUD` relative to this
repository.

For each accepted case Step4 runs `make clean` and then `make shud` in the
local SHUD source directory. It stages the freshly built `shud` executable into
the case run directory and runs `./shud 9035800` from that directory. The
acceptance fails if the local source, build, solver run, or required output
variables are unavailable.

Generated artifacts are kept under `.autoshud-artifacts/step45/<timestamp>/`
by default and are ignored by git. The Chinese report is written to:

```text
.autoshud-artifacts/step45/<timestamp>/reports/9035800-step45-acceptance-report.zh.md
```

The report uses sanitized placeholders such as `<RUN_ROOT>`, `<WORKSPACE>`,
and `<AUTOSHUD_REPO>` for paths. It records Step3 prerequisite checks, SHUD
source commit/status, build and solver commands, exit codes, retained log
locations, Step5 summary fields, pass/fail status, and known limits.

Known non-goals: this smoke test does not claim hydrologic accuracy,
calibration quality, or scientific validity of the short 9035800 run. It also
does not clone SHUD, vendor solver binaries, or commit generated outputs.
