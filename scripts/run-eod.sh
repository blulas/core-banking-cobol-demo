#!/usr/bin/env bash
#------------------------------------------------------------------------------
# run-eod.sh - end-of-day batch cycle driver. Local equivalent of the z/OS job
# stream jcl/CBSEOD.jcl: same steps, same order, same condition-code rules.
#
#   scripts/run-eod.sh                 run the full cycle
#   scripts/run-eod.sh --from 08B      restart at step 08B (like RESTART=)
#   scripts/run-eod.sh --list          list the steps and exit
#
# Rules (mirroring CBSEOD.jcl):
#   * each step logs to $CB_WORK/output/eod-<step>-<pgm>.log
#   * a step ending RC > 4 stops the cycle (COND=(4,LT)); later steps NOT RUN
#   * steps whose executable is not in build/bin are reported MISSING and
#     skipped; the cycle continues with overall RC at least 4
#   * month-end (16x) steps run only when BANKCTL CTL-MONTH-END-FLAG = Y and
#     the year-end step (17) only when CTL-YEAR-END-FLAG = Y. The flags are set
#     by step 01 EODCTL START. (The programs also self-check the flags.)
#   * today's TRANPOST/TRANEDT/TRANREJ/ACHTRAN/PAYTRAN are emptied before
#     step 02 (only when step 02 is part of the run - never on a restart
#     beyond 02, so a restart does not lose posted work)
#   * EODCTL START on a cycle already in status E returns RC 4 (restart)
#------------------------------------------------------------------------------
set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
. "$ROOT/scripts/env.sh"

#      STEP|PROGRAM |PARM |WHEN
STEPS=(
  "01|EODCTL|START|ALL"
  "02|ACHIN||ALL"
  "03|PAYPROC||ALL"
  "04|TRANEDIT||ALL"
  "05|POSTTRAN||ALL"
  "06|ACHRET||ALL"
  "07|LNPAYAP||ALL"
  "08A|CTRSCAN||ALL"
  "08B|STRUCTUR||ALL"
  "08C|AMLMON||ALL"
  "09|HOLDREL||ALL"
  "10|SWEEPRUN||ALL"
  "11|INTACCR||ALL"
  "12|LNACCR||ALL"
  "13|ODAGING||ALL"
  "14|CDMATUR||ALL"
  "15A|LNBILL||ALL"
  "15B|LNDELQ||ALL"
  "16A|INTCAP||ME"
  "16B|FEEASSES||ME"
  "16C|ANLYSFEE||ME"
  "16D|POOLCALC||ME"
  "16E|STMTGEN||ME"
  "16F|DORMSCAN||ME"
  "16G|KYCREVW||ME"
  "17|INT1099||YE"
  "18|RELBAL||ALL"
  "19A|GLEXTR||ALL"
  "19B|GLTRIAL||ALL"
  "19C|GLRECON||ALL"
  "20|OFACBAT||ALL"
  "21|EODCTL|END|ALL"
)

FROM=""
case "${1:-}" in
  --from) FROM="${2:-}"; [ -n "$FROM" ] || { echo "run-eod: --from needs a step id"; exit 16; } ;;
  --list) for s in "${STEPS[@]}"; do IFS='|' read -r id p parm when <<<"$s"
            printf '%-4s %-9s %-6s %s\n' "$id" "$p" "$parm" "$when"; done; exit 0 ;;
  "") ;;
  *) echo "usage: run-eod.sh [--from STEP] [--list]"; exit 16 ;;
esac

OUT="$CB_WORK/output"; mkdir -p "$OUT"
BIN="$ROOT/build/bin"

# flags from the bank control record (fixed layout - see copybook BANKCTL)
ctl_field() { cut -c"$1" "$DD_BANKCTL" 2>/dev/null | head -1; }
ctl_busdate() { ctl_field 13-20; }
ctl_status()  { ctl_field 37; }
ctl_me()      { ctl_field 38; }
ctl_ye()      { ctl_field 40; }

clear_daily_files() {
  echo "run-eod: clearing today's TRANPOST TRANEDT TRANREJ ACHTRAN PAYTRAN"
  for f in "$DD_TRANPOST" "$DD_TRANEDT" "$DD_TRANREJ" "$DD_ACHTRAN" "$DD_PAYTRAN"; do
    : > "$f"
  done
}

started="N"; [ -z "$FROM" ] && started="Y"
stop="N"; overall=0
summary=()

echo "=================================================================="
echo " CBS END-OF-DAY CYCLE   business date $(ctl_busdate)   status $(ctl_status)"
[ -n "$FROM" ] && echo " RESTART FROM STEP $FROM"
echo "=================================================================="

for s in "${STEPS[@]}"; do
  IFS='|' read -r id pgm parm when <<<"$s"
  if [ "$started" = "N" ]; then
    case "$id" in "$FROM"|"$FROM"[A-Z]) started="Y" ;; esac
    if [ "$started" = "N" ]; then summary+=("$id|$pgm|$parm|-|BYPASSED (RESTART)"); continue; fi
  fi
  if [ "$stop" = "Y" ]; then summary+=("$id|$pgm|$parm|-|NOT RUN"); continue; fi

  if [ "$when" = "ME" ] && [ "$(ctl_me)" != "Y" ]; then
    summary+=("$id|$pgm|$parm|-|SKIPPED (NOT MONTH-END)"); continue
  fi
  if [ "$when" = "YE" ] && [ "$(ctl_ye)" != "Y" ]; then
    summary+=("$id|$pgm|$parm|-|SKIPPED (NOT YEAR-END)"); continue
  fi

  [ "$id" = "02" ] && clear_daily_files

  log="$OUT/eod-$id-$pgm.log"
  if [ ! -x "$BIN/$pgm" ]; then
    echo "STEP $id $pgm - MISSING (no build/bin/$pgm) - skipped"
    echo "MISSING: $BIN/$pgm not built" > "$log"
    summary+=("$id|$pgm|$parm|-|MISSING")
    [ $overall -lt 4 ] && overall=4
    continue
  fi

  echo "STEP $id $pgm $parm"
  if [ -n "$parm" ]; then "$BIN/$pgm" "$parm" >"$log" 2>&1; else "$BIN/$pgm" >"$log" 2>&1; fi
  rc=$?
  sed 's/^/    /' "$log" | tail -5
  [ $rc -gt $overall ] && overall=$rc
  if [ $rc -gt 4 ]; then
    summary+=("$id|$pgm|$parm|$rc|ABEND/FAILED - CYCLE STOPPED")
    stop="Y"
  else
    summary+=("$id|$pgm|$parm|$rc|OK")
  fi
done

echo
echo "=================================================================="
echo " EOD STEP SUMMARY"
echo "------------------------------------------------------------------"
printf ' %-4s %-9s %-6s %-4s %s\n' STEP PROGRAM PARM RC STATUS
echo "------------------------------------------------------------------"
for line in "${summary[@]}"; do
  IFS='|' read -r id pgm parm rc st <<<"$line"
  printf ' %-4s %-9s %-6s %-4s %s\n' "$id" "$pgm" "$parm" "$rc" "$st"
done
echo "------------------------------------------------------------------"
echo " OVERALL RC $overall   business date now $(ctl_busdate)   status $(ctl_status)"
if [ "$stop" = "Y" ]; then
  echo " Fix the failing step, then restart with: scripts/run-eod.sh --from <step>"
fi
echo "=================================================================="
exit $overall
