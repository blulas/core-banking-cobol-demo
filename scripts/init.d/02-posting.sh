#------------------------------------------------------------------------------
# 02-posting.sh - Posting domain loaders (sourced by init-data.sh).
#   LDHOLD builds HOLDMST from data/seed/holds.psv and applies the active holds
#   to ACCTMST available balances. HOLDMST is always created, even when the
#   seed file is empty or missing (HOLDSEED is SELECT OPTIONAL), because
#   POSTTRAN, HOLDREL and the online hold transactions open it I-O.
#------------------------------------------------------------------------------
LDHOLD
