#!/usr/bin/env bash
#------------------------------------------------------------------------------
# build.sh - compile the GnuCOBOL-runnable portion of the core banking system.
#
#   Called modules  (src/cobol/common, src/cobol/rules, tests/framework)
#       -> build/lib/<NAME>.dylib|.so   (resolved at run time via COB_LIBRARY_PATH)
#   Main programs   (src/cobol/batch, src/cobol/util, tests/unit)
#       -> build/bin/<NAME>
#
# Online programs (src/cobol/online) contain EXEC CICS / EXEC SQL and require
# the CICS and DB2 precompilers; they are NOT compiled here.
#
# Usage: scripts/build.sh [pattern]     e.g. scripts/build.sh 'OD*'
#------------------------------------------------------------------------------
set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"
PATTERN="${1:-*}"
COBC="${COBC:-cobc}"
COBFLAGS="${COBFLAGS:--fixed -I src/copybooks}"
MODEXT="$($COBC --info | awk -F': ' '/COB_MODULE_EXT/ {print $2}' | tr -d ' ')"
mkdir -p build/lib build/bin build/log

ok=0; failed=0; failed_list=()

compile() {   # $1 = mode (-m|-x)  $2 = source  $3 = output
  local log="build/log/$(basename "$2" .cbl).log"
  if $COBC $1 $COBFLAGS -o "$3" "$2" >"$log" 2>&1; then
    ok=$((ok+1))
    [ -s "$log" ] || rm -f "$log"
  else
    failed=$((failed+1)); failed_list+=("$2")
    sed 's/^/    /' "$log"
  fi
}

for dir in src/cobol/common src/cobol/rules tests/framework; do
  for src in $dir/$PATTERN.cbl; do
    [ -f "$src" ] || continue
    compile -m "$src" "build/lib/$(basename "$src" .cbl).$MODEXT"
  done
done
for dir in src/cobol/batch src/cobol/util tests/unit; do
  for src in $dir/$PATTERN.cbl; do
    [ -f "$src" ] || continue
    compile -x "$src" "build/bin/$(basename "$src" .cbl)"
  done
done

echo "build: $ok compiled, $failed failed"
if [ $failed -gt 0 ]; then
  printf '  FAILED: %s\n' "${failed_list[@]}"
  exit 8
fi
