#!/usr/bin/env bash
#------------------------------------------------------------------------------
# 06-treasury.sh - DD names for Transaction Banking (sweeps, ZBA, notional
# pooling, account analysis) and the General Ledger.
# Sourced by env.sh after $M (masters), $O (outputs), $I, $S are defined.
#------------------------------------------------------------------------------
# --- masters (indexed) --------------------------------------------------------
export DD_SWEEPMST="$M/sweepmst.idx"         # sweep / ZBA definitions
export DD_POOLMST="$M/poolmst.idx"           # notional pool definitions
export DD_GLBAL="$M/glbal.idx"               # GL balances by account + cost center

# --- seed inputs ----------------------------------------------------------------
export DD_SWPSEED="$S/sweeps.psv"
export DD_POOLSEED="$S/pools.psv"
export DD_GLBSEED="$S/glbal.psv"

# --- account analysis public-funds credit carry-forward (GDG-style) ------------
# ANLYCFI = prior generation (0), ANLYCFO = new generation (+1)
export DD_ANLYCFI="${DD_ANLYCFI:-$S/anlycf.psv}"
export DD_ANLYCFO="$O/anlycf.psv"

# --- GL journal and reports -----------------------------------------------------
export DD_GLJRNL="$O/gljrnl.dat"             # GLEXTR -> GLTRIAL
export DD_SWEEPRPT="$O/sweeprpt.txt"         # SWEEPRUN sweep report
export DD_POOLRPT="$O/poolrpt.txt"           # POOLCALC pool interest report
export DD_ANLYSRPT="$O/anlysrpt.txt"         # ANLYSFEE analysis statements
export DD_GLEXRPT="$O/glexrpt.txt"           # GLEXTR extract summary
export DD_GLTBRPT="$O/gltbrpt.txt"           # GLTRIAL trial balance
export DD_GLRCRPT="$O/glrcrpt.txt"           # GLRECON sub-ledger reconciliation
