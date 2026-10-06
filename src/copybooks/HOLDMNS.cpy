      *================================================================*
      * COPYBOOK : HOLDMNS                                             *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET HOLDMNS MAP HOLDMNM       *
      *            TRANSACTION CB12 - ACCOUNT HOLD MAINTENANCE         *
      *            GENERATED FROM BMS SOURCE HOLDMNS - DO NOT EDIT     *
      *================================================================*
       01  HOLDMNMI.
           02  FILLER                      PIC X(12).
           02  TRANIDL                     PIC S9(4) COMP.
           02  TRANIDF                     PIC X.
           02  FILLER REDEFINES TRANIDF.
               03  TRANIDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  TRANIDI                     PIC X(004).
           02  TITLEL                      PIC S9(4) COMP.
           02  TITLEF                      PIC X.
           02  FILLER REDEFINES TITLEF.
               03  TITLEA                  PIC X.
           02  FILLER                      PIC X(02).
           02  TITLEI                      PIC X(040).
           02  CURDATEL                    PIC S9(4) COMP.
           02  CURDATEF                    PIC X.
           02  FILLER REDEFINES CURDATEF.
               03  CURDATEA                PIC X.
           02  FILLER                      PIC X(02).
           02  CURDATEI                    PIC X(010).
           02  ACCTNOL                     PIC S9(4) COMP.
           02  ACCTNOF                     PIC X.
           02  FILLER REDEFINES ACCTNOF.
               03  ACCTNOA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ACCTNOI                     PIC X(012).
           02  ACCTNML                     PIC S9(4) COMP.
           02  ACCTNMF                     PIC X.
           02  FILLER REDEFINES ACCTNMF.
               03  ACCTNMA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ACCTNMI                     PIC X(030).
           02  ASTATL                      PIC S9(4) COMP.
           02  ASTATF                      PIC X.
           02  FILLER REDEFINES ASTATF.
               03  ASTATA                  PIC X.
           02  FILLER                      PIC X(02).
           02  ASTATI                      PIC X(010).
           02  LEDGERL                     PIC S9(4) COMP.
           02  LEDGERF                     PIC X.
           02  FILLER REDEFINES LEDGERF.
               03  LEDGERA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LEDGERI                     PIC X(015).
           02  AVAILL                      PIC S9(4) COMP.
           02  AVAILF                      PIC X.
           02  FILLER REDEFINES AVAILF.
               03  AVAILA                  PIC X.
           02  FILLER                      PIC X(02).
           02  AVAILI                      PIC X(015).
           02  HOLDAML                     PIC S9(4) COMP.
           02  HOLDAMF                     PIC X.
           02  FILLER REDEFINES HOLDAMF.
               03  HOLDAMA                 PIC X.
           02  FILLER                      PIC X(02).
           02  HOLDAMI                     PIC X(015).
           02  FUNCL                       PIC S9(4) COMP.
           02  FUNCF                       PIC X.
           02  FILLER REDEFINES FUNCF.
               03  FUNCA                   PIC X.
           02  FILLER                      PIC X(02).
           02  FUNCI                       PIC X(001).
           02  OPROLEL                     PIC S9(4) COMP.
           02  OPROLEF                     PIC X.
           02  FILLER REDEFINES OPROLEF.
               03  OPROLEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  OPROLEI                     PIC X(003).
           02  HLINE1L                     PIC S9(4) COMP.
           02  HLINE1F                     PIC X.
           02  FILLER REDEFINES HLINE1F.
               03  HLINE1A                 PIC X.
           02  FILLER                      PIC X(02).
           02  HLINE1I                     PIC X(078).
           02  HLINE2L                     PIC S9(4) COMP.
           02  HLINE2F                     PIC X.
           02  FILLER REDEFINES HLINE2F.
               03  HLINE2A                 PIC X.
           02  FILLER                      PIC X(02).
           02  HLINE2I                     PIC X(078).
           02  HLINE3L                     PIC S9(4) COMP.
           02  HLINE3F                     PIC X.
           02  FILLER REDEFINES HLINE3F.
               03  HLINE3A                 PIC X.
           02  FILLER                      PIC X(02).
           02  HLINE3I                     PIC X(078).
           02  HLINE4L                     PIC S9(4) COMP.
           02  HLINE4F                     PIC X.
           02  FILLER REDEFINES HLINE4F.
               03  HLINE4A                 PIC X.
           02  FILLER                      PIC X(02).
           02  HLINE4I                     PIC X(078).
           02  HLINE5L                     PIC S9(4) COMP.
           02  HLINE5F                     PIC X.
           02  FILLER REDEFINES HLINE5F.
               03  HLINE5A                 PIC X.
           02  FILLER                      PIC X(02).
           02  HLINE5I                     PIC X(078).
           02  HLINE6L                     PIC S9(4) COMP.
           02  HLINE6F                     PIC X.
           02  FILLER REDEFINES HLINE6F.
               03  HLINE6A                 PIC X.
           02  FILLER                      PIC X(02).
           02  HLINE6I                     PIC X(078).
           02  HLINE7L                     PIC S9(4) COMP.
           02  HLINE7F                     PIC X.
           02  FILLER REDEFINES HLINE7F.
               03  HLINE7A                 PIC X.
           02  FILLER                      PIC X(02).
           02  HLINE7I                     PIC X(078).
           02  HLINE8L                     PIC S9(4) COMP.
           02  HLINE8F                     PIC X.
           02  FILLER REDEFINES HLINE8F.
               03  HLINE8A                 PIC X.
           02  FILLER                      PIC X(02).
           02  HLINE8I                     PIC X(078).
           02  RELSEQL                     PIC S9(4) COMP.
           02  RELSEQF                     PIC X.
           02  FILLER REDEFINES RELSEQF.
               03  RELSEQA                 PIC X.
           02  FILLER                      PIC X(02).
           02  RELSEQI                     PIC X(004).
           02  NTYPEL                      PIC S9(4) COMP.
           02  NTYPEF                      PIC X.
           02  FILLER REDEFINES NTYPEF.
               03  NTYPEA                  PIC X.
           02  FILLER                      PIC X(02).
           02  NTYPEI                      PIC X(002).
           02  NAMTL                       PIC S9(4) COMP.
           02  NAMTF                       PIC X.
           02  FILLER REDEFINES NAMTF.
               03  NAMTA                   PIC X.
           02  FILLER                      PIC X(02).
           02  NAMTI                       PIC X(013).
           02  NRELDTL                     PIC S9(4) COMP.
           02  NRELDTF                     PIC X.
           02  FILLER REDEFINES NRELDTF.
               03  NRELDTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NRELDTI                     PIC X(008).
           02  NRSNL                       PIC S9(4) COMP.
           02  NRSNF                       PIC X.
           02  FILLER REDEFINES NRSNF.
               03  NRSNA                   PIC X.
           02  FILLER                      PIC X(02).
           02  NRSNI                       PIC X(030).
           02  NREFL                       PIC S9(4) COMP.
           02  NREFF                       PIC X.
           02  FILLER REDEFINES NREFF.
               03  NREFA                   PIC X.
           02  FILLER                      PIC X(02).
           02  NREFI                       PIC X(020).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  HOLDMNMO REDEFINES HOLDMNMI.
           02  FILLER                      PIC X(12).
           02  FILLER                      PIC X(03).
           02  TRANIDC                     PIC X.
           02  TRANIDH                     PIC X.
           02  TRANIDO                     PIC X(004).
           02  FILLER                      PIC X(03).
           02  TITLEC                      PIC X.
           02  TITLEH                      PIC X.
           02  TITLEO                      PIC X(040).
           02  FILLER                      PIC X(03).
           02  CURDATEC                    PIC X.
           02  CURDATEH                    PIC X.
           02  CURDATEO                    PIC X(010).
           02  FILLER                      PIC X(03).
           02  ACCTNOC                     PIC X.
           02  ACCTNOH                     PIC X.
           02  ACCTNOO                     PIC X(012).
           02  FILLER                      PIC X(03).
           02  ACCTNMC                     PIC X.
           02  ACCTNMH                     PIC X.
           02  ACCTNMO                     PIC X(030).
           02  FILLER                      PIC X(03).
           02  ASTATC                      PIC X.
           02  ASTATH                      PIC X.
           02  ASTATO                      PIC X(010).
           02  FILLER                      PIC X(03).
           02  LEDGERC                     PIC X.
           02  LEDGERH                     PIC X.
           02  LEDGERO                     PIC X(015).
           02  FILLER                      PIC X(03).
           02  AVAILC                      PIC X.
           02  AVAILH                      PIC X.
           02  AVAILO                      PIC X(015).
           02  FILLER                      PIC X(03).
           02  HOLDAMC                     PIC X.
           02  HOLDAMH                     PIC X.
           02  HOLDAMO                     PIC X(015).
           02  FILLER                      PIC X(03).
           02  FUNCC                       PIC X.
           02  FUNCH                       PIC X.
           02  FUNCO                       PIC X(001).
           02  FILLER                      PIC X(03).
           02  OPROLEC                     PIC X.
           02  OPROLEH                     PIC X.
           02  OPROLEO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  HLINE1C                     PIC X.
           02  HLINE1H                     PIC X.
           02  HLINE1O                     PIC X(078).
           02  FILLER                      PIC X(03).
           02  HLINE2C                     PIC X.
           02  HLINE2H                     PIC X.
           02  HLINE2O                     PIC X(078).
           02  FILLER                      PIC X(03).
           02  HLINE3C                     PIC X.
           02  HLINE3H                     PIC X.
           02  HLINE3O                     PIC X(078).
           02  FILLER                      PIC X(03).
           02  HLINE4C                     PIC X.
           02  HLINE4H                     PIC X.
           02  HLINE4O                     PIC X(078).
           02  FILLER                      PIC X(03).
           02  HLINE5C                     PIC X.
           02  HLINE5H                     PIC X.
           02  HLINE5O                     PIC X(078).
           02  FILLER                      PIC X(03).
           02  HLINE6C                     PIC X.
           02  HLINE6H                     PIC X.
           02  HLINE6O                     PIC X(078).
           02  FILLER                      PIC X(03).
           02  HLINE7C                     PIC X.
           02  HLINE7H                     PIC X.
           02  HLINE7O                     PIC X(078).
           02  FILLER                      PIC X(03).
           02  HLINE8C                     PIC X.
           02  HLINE8H                     PIC X.
           02  HLINE8O                     PIC X(078).
           02  FILLER                      PIC X(03).
           02  RELSEQC                     PIC X.
           02  RELSEQH                     PIC X.
           02  RELSEQO                     PIC X(004).
           02  FILLER                      PIC X(03).
           02  NTYPEC                      PIC X.
           02  NTYPEH                      PIC X.
           02  NTYPEO                      PIC X(002).
           02  FILLER                      PIC X(03).
           02  NAMTC                       PIC X.
           02  NAMTH                       PIC X.
           02  NAMTO                       PIC X(013).
           02  FILLER                      PIC X(03).
           02  NRELDTC                     PIC X.
           02  NRELDTH                     PIC X.
           02  NRELDTO                     PIC X(008).
           02  FILLER                      PIC X(03).
           02  NRSNC                       PIC X.
           02  NRSNH                       PIC X.
           02  NRSNO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  NREFC                       PIC X.
           02  NREFH                       PIC X.
           02  NREFO                       PIC X(020).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
