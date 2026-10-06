#!/usr/bin/env bash
#------------------------------------------------------------------------------
# run-tests.sh - build, then run every unit test driver in build/bin/UT*.
# Exit code is non-zero if any suite fails.
#   scripts/run-tests.sh            run all suites
#   scripts/run-tests.sh UTODDECI   run one suite (glob allowed: 'UTOD*')
#   scripts/run-tests.sh --no-build UTODDECI   skip the build step
#------------------------------------------------------------------------------
set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
build_rc=0
if [ "${1:-}" = "--no-build" ]; then
  shift
else
  "$ROOT/scripts/build.sh" >/tmp/cbs-build.$$ 2>&1 || { build_rc=8; cat /tmp/cbs-build.$$; }
  rm -f /tmp/cbs-build.$$
fi
. "$ROOT/scripts/env.sh"
# unit tests use an isolated, known control record
export DD_BANKCTL="$ROOT/tests/fixtures/bankctl.dat"

suites=0; failed=0; failed_list=()
for t in "$ROOT"/build/bin/${1:-UT*}; do
  [ -x "$t" ] || continue
  suites=$((suites+1))
  if ! "$t"; then failed=$((failed+1)); failed_list+=("$(basename "$t")"); fi
done
echo "=================================================="
echo "suites: $suites  failed: $failed"
[ $failed -eq 0 ] || { printf '  FAILED: %s\n' "${failed_list[@]}"; exit 8; }
[ $build_rc -eq 0 ] || { echo "build had failures (see above)"; exit 8; }
