# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repo is

A synthetic z/OS-style core banking system (COBOL, CICS, DB2, JCL, VSAM). It is a CoreStory demo. The parts that GnuCOBOL can run (batch, rule modules, utilities and unit tests) build and run locally, with shell scripts standing in for JCL and VSAM.

Do not change business-rule behaviour (thresholds, rates, differences between programs) unless asked. `docs/PROGRAMMING-STANDARDS.md` is the coding standard.

## Context

- Be extremely brief and concise. 
- Never use pleasantries, conversational filler, or introductory/concluding remarks (e.g., "Sure, I can help with that!" or "Let me know if you need anything else"). 
- Output code or direct answers immediately.
- Don't ask me how you are doing.
- Limit feedback to only what's necessary.

## Commands

Requires GnuCOBOL 3.x (`cobc` on PATH).

```bash
make build                                  # scripts/build.sh — compile everything
scripts/build.sh 'OD*'                      # compile only sources matching a glob
make test                                   # build + run all UT* suites
scripts/run-tests.sh UTODDECI               # build + run one suite (glob ok: 'UTOD*')
scripts/run-tests.sh --no-build UTODDECI    # run one suite without rebuilding
make data                                   # rebuild data/master + data/output from seed/input
make eod                                    # full end-of-day cycle
scripts/run-eod.sh --list                   # list EOD steps
scripts/run-eod.sh --from 08B               # restart EOD at a step (like JCL RESTART=)
make clean
```

- A build failure exits 8. Per-file compiler output goes to `build/log/<NAME>.log`.
- `make data` must be run before `make eod`. The EOD cycle mutates the masters and advances the business date in BANKCTL, so re-run `make data` to get back to the seeded 2026-09-30 state.
- To work on an isolated copy of the bank, set `CB_WORK=/some/dir` (for example `CB_WORK=/tmp/cbs-test scripts/init-data.sh`). Seed and input files are still read from the repo.
- Init overrides: `BUSDATE`, `PREVDATE` and `NEXTDATE` (default 20260930, which is month-end and quarter-end, so the first EOD runs the month-end steps).
- BANKCTL reference rates are `9(03)V9(06)` percentages. `init-data.sh` writes fed funds / prime / SOFR / ECR base as 4330000 / 7500000 / 4300000 / 2500000 (4.33 / 7.50 / 4.30 / 2.50%). `tests/fixtures/bankctl.dat` must match.

## Build model

`scripts/build.sh` compiles with `cobc -fixed -I src/copybooks`:
- `src/cobol/common`, `src/cobol/rules` and `tests/framework` are compiled as **dynamic modules** (`-m`) into `build/lib/`. They are resolved at run time through `COB_LIBRARY_PATH`.
- `src/cobol/batch`, `src/cobol/util` and `tests/unit` are compiled as **executables** (`-x`) into `build/bin/`.
- `src/cobol/online` (EXEC CICS/SQL), `src/bms`, `src/sql` and `jcl/` are **not compiled locally**. They exist for analysis only.

## Runtime model: DD names

Programs `ASSIGN TO "<DDNAME>"` only. `scripts/env.sh` exports `DD_<DDNAME>=<path>`, which plays the role of a JCL DD statement. Domain-specific DDs are defined in `scripts/env.d/NN-<domain>.sh`. Any script that runs programs sources `env.sh`. To run a single program by hand:

```bash
. scripts/env.sh && POSTTRAN
```

Unit tests override `DD_BANKCTL` with `tests/fixtures/bankctl.dat`.

The code is organised into domains, which are numbered consistently across `scripts/env.d`, `scripts/init.d`, `data/input/*-NN-*.psv` and `docs/NN-*.md`: 01 cif, 02 posting, 03 interest, 04 payments, 05 lending, 06 treasury, 07 compliance. To add a domain's data, you need an env.d DD file, an init.d loader (an `LD*` util program) and seed/input PSVs.

## Architecture

- **Rule modules** (`src/cobol/rules`) hold the business logic. They are called by both batch and online programs. Each module has a single 01 parameter block in a copybook (for example `FEECALC` ↔ `FEEPARM`) that ends with a `-RETURN-CODE` field (00 ok, 04 warn, 08 business decline, 12 error, 16 severe). Rule modules do **no file I/O**: the caller passes in everything they need.
- **Common modules**: `GETCTL` reads the BANKCTL control record. The business date must always come from `GETCTL`, never from the clock. `DATEUTIL` does date arithmetic. Programs call `ABENDRTN` on any unexpected file status.
- **EOD cycle** (`scripts/run-eod.sh`, which mirrors `jcl/CBSEOD.jcl` step for step): `EODCTL START` sets the ME/QE/YE flags. Then ACHIN and PAYPROC generate transactions, TRANEDIT edits TRANIN+ACHTRAN+PAYTRAN into TRANEDT/TRANREJ, and POSTTRAN posts to ACCTMST/HOLDMST and writes TRANPOST. After that come the lending, compliance, interest, overdraft and CD steps, the month-end-only 16x steps, the year-end step 17, the GL steps and `EODCTL END`. A step with RC > 4 stops the cycle. A program that isn't built is reported as MISSING with RC 4 and is skipped. On the seed data the full cycle ends with overall RC 4 (warnings from PAYPROC, TRANEDIT, GLEXTR and GLRECON).
- **TRANPOST is the system of record for the day.** Any batch program that changes a balance appends a TRANREC to TRANPOST (opened EXTEND). The GL (GLEXTR/GLTRIAL/GLRECON) and compliance monitors (CTRSCAN/STRUCTUR/AMLMON) read it. Cross-domain alerts are appended to ALERTS.
- **Online** programs are pseudo-conversational CICS programs. `CBMENU` is transaction CB00, and PF3 returns to it. Business rules are invoked through LINK or CALL with the same parm copybooks that batch uses.

## Conventions (from docs/PROGRAMMING-STANDARDS.md)

- Fixed format: code goes in columns 8–72 and column 7 is the indicator. Names are at most 8 characters and upper case. Every program starts with the standard header box.
- Paragraphs are numbered: `0000-MAIN`, `1000-` init, `2000-` main processing, `8000-` common routines, `9000-` termination and errors. Use explicit scope terminators.
- Dates are `9(08)` CCYYMMDD. Money is `S9(13)V99 COMP-3`. Rates are `9(03)V9(06)` percentages (4.100000 = 4.10%). Transaction amounts are always positive; direction comes from `TRAN-DR-CR`.
- Record fields are prefixed with the record mnemonic (`ACCT-`, `CUST-`, `TRAN-`, …).
- Unit tests: every rule module has a `UT` + ≤6-character driver in `tests/unit/` (for example `UTDATEUT` tests `DATEUTIL`). Drivers call `TESTLIB` (copybook `TESTPARM`) with these functions: `INIT`, `AEQ` (alphanumeric), `NEQ` (numeric), `TRUE` and `SUMM`. A new driver is picked up automatically by the build and by `run-tests.sh`.
