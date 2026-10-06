# Core Banking System (COBOL) — CoreStory Demo

A synthetic z/OS-style core banking system for **Granite Street Bank NA**, written in COBOL with CICS, DB2, JCL and VSAM artifacts. It is a CoreStory demo codebase: big enough to look like a real mainframe estate, small enough to read.

The parts that GnuCOBOL can run build and run on a laptop: batch programs, business-rule modules, data-load utilities and unit tests. Shell scripts stand in for JCL and VSAM. The online (CICS/DB2) programs, BMS maps, DDL and JCL are kept for analysis only.

## Quick start

You need GnuCOBOL 3.x (`cobc` on your `PATH`). On macOS, run `brew install gnucobol`.

```bash
make build    # compile everything into build/
make test     # build, then run every unit-test suite
make data     # build the master files from data/seed and data/input
make eod      # run the full end-of-day batch cycle
make clean    # remove build/, data/master and data/output
```

Run `make data` before `make eod`. The EOD cycle changes the master files and moves the business date forward, so run `make data` again to get back to the seeded state (business date **2026-09-30**). That date is both month-end and quarter-end, so the first EOD run includes the month-end steps.

### Other useful commands

```bash
scripts/build.sh 'OD*'                      # compile only the sources that match a glob
scripts/run-tests.sh UTODDECI               # build, then run one suite (globs work: 'UTOD*')
scripts/run-tests.sh --no-build UTODDECI    # run one suite without rebuilding
scripts/run-eod.sh --list                   # list the EOD steps
scripts/run-eod.sh --from 08B               # restart EOD at a step (like JCL RESTART=)
. scripts/env.sh && POSTTRAN                # run a single program by hand
```

- A failed build exits with code 8. The compiler output for each source file goes to `build/log/<NAME>.log`.
- To work on a separate copy of the bank, set `CB_WORK`, for example `CB_WORK=/tmp/cbs-test scripts/init-data.sh`. Seed and input files are still read from the repo.
- `BUSDATE`, `PREVDATE` and `NEXTDATE` override the initial business dates.

## Repository layout

| Path | Contents | Built locally |
|---|---|---|
| `src/cobol/rules/` | Business-rule modules, called by batch and online programs | Yes, as dynamic modules |
| `src/cobol/common/` | `GETCTL` (control record), `DATEUTIL` (date arithmetic), `ABENDRTN` (abend handling) | Yes, as dynamic modules |
| `src/cobol/batch/` | EOD and month-end batch programs | Yes, as executables |
| `src/cobol/util/` | `LD*` data-load utilities | Yes, as executables |
| `src/cobol/online/` | Pseudo-conversational CICS transactions (EXEC CICS / EXEC SQL) | No |
| `src/copybooks/` | Record layouts, rule parameter blocks, tables, DCLGEN, BMS symbolic maps | n/a |
| `src/bms/` | BMS mapsets (3270 screens) | No |
| `src/sql/` | DB2 DDL | No |
| `jcl/` | z/OS job streams: `CBSEOD` (the full cycle) plus 12 domain jobs (`CBSPOST`, `CBSINTRS`, `CBSCDMAT`, `CBSMTHIF`, `CBSYREND`, `CBSLOAN`, `CBSSWEEP`, `CBSMTHTX`, `CBSGL`, `CBSCOMPL`, `CBSMTHCF`, `CBSRELBL`) | No |
| `tests/unit/` | `UT*` unit-test drivers, one for each rule or common module | Yes |
| `tests/framework/` | `TESTLIB` assertion library | Yes |
| `scripts/` | Build, data-init, EOD and test drivers, plus per-domain `env.d/` and `init.d/` | — |
| `data/seed/`, `data/input/` | Pipe-separated seed masters and the day's input transactions | — |
| `docs/` | Programming standards | — |

Generated output goes to `build/` (`bin/`, `lib/`, `log/`), `data/master/` and `data/output/`. All three are git-ignored.

### Domains

Domains have the same number everywhere they appear: `scripts/env.d`, `scripts/init.d`, `data/input/*-NN-*.psv` and `docs/NN-*.md`.

| # | Domain | Examples |
|---|---|---|
| 01 | CIF (customers) | `KYCTIER`, `RISKSCOR`, `CUSTINQ`, `CUSTMNT` |
| 02 | Posting | `TRANEDIT`, `POSTTRAN`, `TRANVAL`, `HOLDREL`, `REGCCHLD` |
| 03 | Interest and fees | `INTACCR`, `INTCAP`, `INTCALC`, `RATETIER`, `FEECALC`, `CDPENLTY` |
| 04 | Payments | `ACHIN`, `PAYPROC`, `ACHVALID`, `PAYROUTE`, `PAYLIMIT`, `CUTOFF` |
| 05 | Lending | `LNPAYAP`, `LNACCR`, `LNBILL`, `LNDELQ`, `AMORTCLC`, `LNPRICE`, `CRDECIDE` |
| 06 | Treasury and GL | `SWEEPRUN`, `POOLCALC`, `ANLYSFEE`, `ECRCALC`, `GLEXTR`, `GLTRIAL`, `GLRECON`, `GLDERIVE` |
| 07 | Compliance | `CTRSCAN`, `STRUCTUR`, `AMLMON`, `OFACBAT`, `OFACSCRN`, `AMLRULES`, `EODCTL` |

`RELBAL` (relationship balances, EOD step 18) belongs to domain 01.

To add data for a domain, create an `env.d` DD file, an `init.d` loader (an `LD*` utility) and seed/input PSV files.

## How it works

### DD names instead of file paths

Programs only ever `ASSIGN TO "<DDNAME>"`. `scripts/env.sh` exports `DD_<DDNAME>=<path>`, which does the job of a JCL DD statement. Each domain defines its own DDs in `scripts/env.d/NN-<domain>.sh`. Every script that runs programs sources `env.sh` first.

### Rule modules

The business logic lives in `src/cobol/rules`. Each module takes one parameter block, defined in a copybook (for example, `FEECALC` takes `FEEPARM`). The block ends in a `-RETURN-CODE` field:

| RC | Meaning |
|---|---|
| 00 | OK |
| 04 | Warning |
| 08 | Business decline |
| 12 | Error |
| 16 | Severe |

Rule modules do **no file I/O**. The caller passes in everything a module needs, so batch and online programs share the same rules. The business date always comes from `GETCTL` (the BANKCTL control record) and never from the system clock.

### End-of-day cycle

`scripts/run-eod.sh` follows `jcl/CBSEOD.jcl` step for step:

1. **01 `EODCTL START`** sets the month-end, quarter-end and year-end flags.
2. **02–05** `ACHIN` and `PAYPROC` generate transactions. `TRANEDIT` edits them into TRANEDT/TRANREJ. `POSTTRAN` posts to ACCTMST/HOLDMST and writes TRANPOST.
3. **07–15** Loan payments, compliance scans, hold release, sweeps, interest and loan accrual, overdraft aging, CD maturity, loan billing and delinquency.
4. **16A–16G** (month-end only) Interest capitalization, fees, account analysis, pooling, statements, dormancy and KYC review.
5. **17** (year-end only) `INT1099`.
6. **18–20** Relationship balances, the GL extract, trial balance and reconciliation, and the OFAC batch screen.
7. **21 `EODCTL END`**.

A step that ends with RC > 4 stops the cycle. A step whose program isn't built is reported as `MISSING`, gets RC 4, and is skipped. Each step writes a log to `data/output/eod-<step>-<pgm>.log`.

On the seeded data a full cycle ends with overall RC 4. The RC 4 comes from warnings in `PAYPROC` (rejected payments), `TRANEDIT` (rejected items), `GLEXTR` (one item to suspense) and `GLRECON` (reconciliation breaks).

**TRANPOST is the system of record for the day.** Every batch program that changes a balance appends a transaction record to it. The GL and the compliance monitors read TRANPOST, and cross-domain alerts are appended to ALERTS.

### Online

The CICS programs are pseudo-conversational. `CBMENU` (transaction `CB00`) is the main menu, and PF3 returns to it. Transactions are grouped by area:

- `CB0x` customers and accounts
- `CB1x` transfers, teller and holds
- `CB2x` rates, CDs and fees
- `CB3x` payments
- `CB4x` lending
- `CB5x` treasury (cash position, sweep maintenance, GL inquiry)
- `CB6x` compliance (alert work queue, OFAC review)

The menu lists `CB30`–`CB32` (`PAYINIT`, `WIREAPPR`, `PAYINQ`), but those programs are not in the repo. Online programs call the same rule modules, with the same parameter copybooks, that batch uses.

## Testing

Each rule and common module has a unit-test driver named `UT` + up to 6 characters of the module name (`UTDATEUT` tests `DATEUTIL`). The drivers call `TESTLIB` (copybook `TESTPARM`) with these functions: `INIT`, `AEQ` (alphanumeric equal), `NEQ` (numeric equal), `TRUE` and `SUMM`. The build and `run-tests.sh` pick up new drivers automatically. The unit tests use their own control record, `tests/fixtures/bankctl.dat`.

## Coding standards

See [docs/PROGRAMMING-STANDARDS.md](docs/PROGRAMMING-STANDARDS.md). In short:

- Fixed format: code in columns 8–72, the indicator in column 7, names of at most 8 upper-case characters, and the standard header box at the top of every program.
- Numbered paragraphs (`0000-MAIN`, `1000-` init, `2000-` main, `8000-` common, `9000-` termination) and explicit scope terminators.
- Dates are `9(08)` CCYYMMDD. Money is `S9(13)V99 COMP-3`. Rates are `9(03)V9(06)` percentages (`4.100000` = 4.10%).
- Transaction amounts are always positive. `TRAN-DR-CR` gives the direction.

## Documentation

[docs/PROGRAMMING-STANDARDS.md](docs/PROGRAMMING-STANDARDS.md) is the coding standard.

All data in this repository is synthetic. Any resemblance to real customers, accounts or institutions is coincidental.
