#!/usr/bin/env bash
#------------------------------------------------------------------------------
# init-data.sh - (re)build every master and input file from data/seed and
# data/input. Safe to re-run; it wipes $CB_WORK/master and $CB_WORK/output.
#
#   1. bank control record      -> data/master/bankctl.dat
#   2. PRODMST, CUSTMST, ACCTMST -> loaded by LDPROD / LDCUST / LDACCT
#   3. data/input/daytran-*.psv  -> TRANIN   (LDTRAN DAY)
#      data/input/tranhist-*.psv -> TRANHIST (LDTRAN HIST)
#   4. scripts/init.d/*.sh       -> domain-specific loaders (loans, payments,
#                                   sweeps, rates, ...)
#------------------------------------------------------------------------------
set -eu
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
. "$ROOT/scripts/env.sh"
BUSDATE="${BUSDATE:-20260930}"; PREVDATE="${PREVDATE:-20260929}"; NEXTDATE="${NEXTDATE:-20261001}"

rm -rf "$CB_WORK/master" "$CB_WORK/output"
mkdir -p "$CB_WORK/master" "$CB_WORK/output"

# Business date 2026-09-30 is the last business day of September and Q3, so
# the first EOD run exercises month-end and quarter-end processing.
#        id      bank date      prev       next     cyc ME QE YE ach  sdach wire  fedfunds prime   sofr    ecr     lastseq mode
printf '%-8s%-4s%08d%08d%08d%s%s%s%s%04d%04d%04d%09d%09d%09d%09d%012d%s%-20s\n' \
  BANKCTL1 GSBU "$BUSDATE" "$PREVDATE" "$NEXTDATE" O Y Y N 1600 1430 1745 \
  4330000 7500000 4300000 2500000 0 P "" > "$DD_BANKCTL"

LDPROD
LDCUST
LDACCT

cat "$CB_ROOT"/data/input/daytran-*.psv  2>/dev/null > "$DD_TRANSEED" || true
cat "$CB_ROOT"/data/input/tranhist-*.psv 2>/dev/null > "$DD_HISTSEED" || true
DD_TRANOUT="$DD_TRANIN"   DD_TRANSEED="$DD_TRANSEED" LDTRAN DAY
DD_TRANOUT="$DD_TRANHIST" DD_TRANSEED="$DD_HISTSEED" LDTRAN HIST

for f in "$ROOT"/scripts/init.d/*.sh; do
  if [ -f "$f" ]; then echo "--- $(basename "$f")"; . "$f"; fi
done
echo "init-data: complete (business date $BUSDATE)"
