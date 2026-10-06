#------------------------------------------------------------------------------
# 02-posting.sh - DD names for the Posting domain (transaction edit, posting,
# holds, overdraft). HOLDMST itself is defined in env.sh.
#------------------------------------------------------------------------------
export DD_HOLDSEED="$S/holds.psv"            # LDHOLD input (may be empty)
export DD_EDITRPT="$O/editrpt.txt"           # TRANEDIT control report
export DD_POSTRPT="$O/postrpt.txt"           # POSTTRAN daily totals report
export DD_HOLDRPT="$O/holdrpt.txt"           # HOLDREL release report
export DD_ODAGRPT="$O/odagrpt.txt"           # ODAGING aging report
