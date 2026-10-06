#!/usr/bin/env bash
#------------------------------------------------------------------------------
# 06-treasury.sh - load Transaction Banking / GL masters.
# Sourced by init-data.sh (set -e active, env loaded, programs on PATH).
#------------------------------------------------------------------------------
LDSWEEP
LDPOOL
LDGLBAL
