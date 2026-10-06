# CBS Programming Standards

Core Banking System (CBS) — application development standards.
Maintained by Core Systems Engineering. Last revised 2019-03.

## 1. Source layout

| Location | Contents | Compiled locally |
|---|---|---|
| `src/cobol/online/` | CICS transaction programs (pseudo-conversational) | No (CICS/DB2 translators) |
| `src/cobol/batch/`  | Batch programs, executed from JCL job streams | Yes, as executables |
| `src/cobol/rules/`  | Called business-rule modules, shared by online and batch | Yes, as modules |
| `src/cobol/common/` | Called utilities (DATEUTIL, GETCTL, ABENDRTN) | Yes, as modules |
| `src/cobol/util/`   | Conversion and data-load utilities | Yes, as executables |
| `src/copybooks/`    | Record layouts, linkage areas, tables, DCLGEN, BMS symbolic maps | n/a |
| `src/bms/`          | BMS mapset macros (3270 screens) | No (assembler) |
| `src/sql/`          | DB2 DDL | No |
| `jcl/`              | Batch job streams and procs | No |
| `tests/unit/`       | Unit-test drivers (`UT*`) for rule and common modules | Yes |

## 2. Naming

* Program and copybook names are at most 8 characters, upper case.
* Unit-test drivers are named `UT` followed by up to 6 characters of the module name (`UTDATEUT` tests `DATEUTIL`).
* CICS transaction IDs are 4 characters. Mapsets are 7 characters ending in `S` and maps 7 characters ending in `M`. For example, mapset `ACINQS` contains map `ACINQM`.
* Record fields are prefixed with the record mnemonic (`ACCT-`, `CUST-`, `TRAN-`, ...).
* Paragraphs are numbered: `0000-MAIN`, `1000-` initialisation, `2000-` main processing, `8000-` common routines, `9000-` termination and error handling.

## 3. Format

* Fixed format: sequence area cols 1–6 blank, indicator col 7, code cols 8–72.
* Every program starts with the standard header box (PROGRAM, TYPE, PURPOSE, INPUT/OUTPUT, CHANGE LOG).
* Use `END-IF`, `END-EVALUATE` and other explicit scope terminators. `GO TO` is permitted only inside a paragraph range performed with `THRU`. This applies to legacy code only.

## 4. Data

* Dates are `PIC 9(08)` CCYYMMDD. Times are `PIC 9(06)` HHMMSS, or `9(04)` HHMM for cutoffs.
* Money is `PIC S9(13)V99 COMP-3` unless a narrower field is justified.
* Rates are percentages, `PIC 9(03)V9(06)`, so 4.100000 means 4.10%.
* Transaction amounts are always positive; direction comes from `TRAN-DR-CR`.
* The business date always comes from `CALL 'GETCTL'`, never from the system clock.

## 5. Files

Batch programs `ASSIGN TO` the DD name only (for example `ASSIGN TO "ACCTMST"`). The JCL DD statement, or `DD_<name>` in `scripts/env.sh` when running locally, binds the physical dataset.

| DD | Organisation | Record | Key |
|---|---|---|---|
| BANKCTL | line sequential | BANKCTL | — |
| CUSTMST | KSDS | CUSTREC | CUST-ID |
| ACCTMST | KSDS + AIX | ACCTREC | ACCT-NUMBER, alt ACCT-PRIMARY-CUST-ID (dups) |
| PRODMST | KSDS | PRODREC | PROD-CODE |
| HOLDMST | KSDS | HOLDREC | HOLD-KEY |
| TRANIN / TRANEDT / TRANPOST / TRANREJ / TRANHIST | sequential | TRANREC | — |
| ALERTS | sequential, `SELECT OPTIONAL`, opened `EXTEND` | ALERTREC | — |

Rules:
* Any batch program that changes a balance writes a TRANREC to `TRANPOST` (opened `EXTEND`). GL extraction and compliance monitoring read `TRANPOST`.
* Check file status after every OPEN, READ, WRITE and REWRITE. On an unexpected status, call `ABENDRTN`.

## 6. Called modules

* Linkage is a single 01-level parameter block defined in a copybook named after the module, with a trailing `-RETURN-CODE` field.
* Return codes: 00 OK, 04 warning, 08 declined (business "no"), 12 error, 16 severe.
* Rule modules must not do file I/O. Callers pass in everything the rule needs.

## 7. Online (CICS)

* Programs are pseudo-conversational: `EXEC CICS RETURN TRANSID(...) COMMAREA(...)`.
* `EIBCALEN = 0` means first entry, so send the map with `ERASE`.
* Handle errors with `RESP()` / `RESP2()`. Do not use `HANDLE CONDITION` in new code.
* PF3 returns to the main menu (`CBMENU`, transaction `CB00`). PF12 cancels.
* Business rules are invoked with `EXEC CICS LINK PROGRAM('module') COMMAREA(parm-block)` or with a static `CALL`.

## 8. Unit tests

Every rule module has a `UT*` driver in `tests/unit/` that uses `TESTLIB` (copybook `TESTPARM`):

```cobol
           MOVE 'NEQ' TO TST-FUNCTION
           MOVE 'OD FEE CAPPED AT 3 PER DAY' TO TST-CASE-NAME
           MOVE 3 TO TST-EXP-NUM
           MOVE ODD-FEE-COUNT TO TST-ACT-NUM
           CALL 'TESTLIB' USING TEST-PARMS
```

Run all suites with `scripts/run-tests.sh`.
