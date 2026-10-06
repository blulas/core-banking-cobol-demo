      *================================================================*
      * COPYBOOK : XFERFNS                                             *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET XFERFNS MAP XFERFNM       *
      *            TRANSACTION CB10 - INTERNAL FUNDS TRANSFER          *
      *            GENERATED FROM BMS SOURCE XFERFNS - DO NOT EDIT     *
      *================================================================*
       01  XFERFNMI.
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
           02  FROMACL                     PIC S9(4) COMP.
           02  FROMACF                     PIC X.
           02  FILLER REDEFINES FROMACF.
               03  FROMACA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FROMACI                     PIC X(012).
           02  FROMNML                     PIC S9(4) COMP.
           02  FROMNMF                     PIC X.
           02  FILLER REDEFINES FROMNMF.
               03  FROMNMA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FROMNMI                     PIC X(030).
           02  FRPRDL                      PIC S9(4) COMP.
           02  FRPRDF                      PIC X.
           02  FILLER REDEFINES FRPRDF.
               03  FRPRDA                  PIC X.
           02  FILLER                      PIC X(02).
           02  FRPRDI                      PIC X(004).
           02  FRAVLL                      PIC S9(4) COMP.
           02  FRAVLF                      PIC X.
           02  FILLER REDEFINES FRAVLF.
               03  FRAVLA                  PIC X.
           02  FILLER                      PIC X(02).
           02  FRAVLI                      PIC X(017).
           02  FRODLL                      PIC S9(4) COMP.
           02  FRODLF                      PIC X.
           02  FILLER REDEFINES FRODLF.
               03  FRODLA                  PIC X.
           02  FILLER                      PIC X(02).
           02  FRODLI                      PIC X(012).
           02  TOACL                       PIC S9(4) COMP.
           02  TOACF                       PIC X.
           02  FILLER REDEFINES TOACF.
               03  TOACA                   PIC X.
           02  FILLER                      PIC X(02).
           02  TOACI                       PIC X(012).
           02  TONML                       PIC S9(4) COMP.
           02  TONMF                       PIC X.
           02  FILLER REDEFINES TONMF.
               03  TONMA                   PIC X.
           02  FILLER                      PIC X(02).
           02  TONMI                       PIC X(030).
           02  TOPRDL                      PIC S9(4) COMP.
           02  TOPRDF                      PIC X.
           02  FILLER REDEFINES TOPRDF.
               03  TOPRDA                  PIC X.
           02  FILLER                      PIC X(02).
           02  TOPRDI                      PIC X(004).
           02  TOAVLL                      PIC S9(4) COMP.
           02  TOAVLF                      PIC X.
           02  FILLER REDEFINES TOAVLF.
               03  TOAVLA                  PIC X.
           02  FILLER                      PIC X(02).
           02  TOAVLI                      PIC X(017).
           02  AMOUNTL                     PIC S9(4) COMP.
           02  AMOUNTF                     PIC X.
           02  FILLER REDEFINES AMOUNTF.
               03  AMOUNTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  AMOUNTI                     PIC X(013).
           02  MEMOL                       PIC S9(4) COMP.
           02  MEMOF                       PIC X.
           02  FILLER REDEFINES MEMOF.
               03  MEMOA                   PIC X.
           02  FILLER                      PIC X(02).
           02  MEMOI                       PIC X(030).
           02  CUSTIDL                     PIC S9(4) COMP.
           02  CUSTIDF                     PIC X.
           02  FILLER REDEFINES CUSTIDF.
               03  CUSTIDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CUSTIDI                     PIC X(010).
           02  CONFRML                     PIC S9(4) COMP.
           02  CONFRMF                     PIC X.
           02  FILLER REDEFINES CONFRMF.
               03  CONFRMA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CONFRMI                     PIC X(001).
           02  REGDCTL                     PIC S9(4) COMP.
           02  REGDCTF                     PIC X.
           02  FILLER REDEFINES REGDCTF.
               03  REGDCTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REGDCTI                     PIC X(003).
           02  REGDFEL                     PIC S9(4) COMP.
           02  REGDFEF                     PIC X.
           02  FILLER REDEFINES REGDFEF.
               03  REGDFEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REGDFEI                     PIC X(010).
           02  PROJBLL                     PIC S9(4) COMP.
           02  PROJBLF                     PIC X.
           02  FILLER REDEFINES PROJBLF.
               03  PROJBLA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PROJBLI                     PIC X(017).
           02  ODUSEDL                     PIC S9(4) COMP.
           02  ODUSEDF                     PIC X.
           02  FILLER REDEFINES ODUSEDF.
               03  ODUSEDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ODUSEDI                     PIC X(012).
           02  XFRREFL                     PIC S9(4) COMP.
           02  XFRREFF                     PIC X.
           02  FILLER REDEFINES XFRREFF.
               03  XFRREFA                 PIC X.
           02  FILLER                      PIC X(02).
           02  XFRREFI                     PIC X(020).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  XFERFNMO REDEFINES XFERFNMI.
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
           02  FROMACC                     PIC X.
           02  FROMACH                     PIC X.
           02  FROMACO                     PIC X(012).
           02  FILLER                      PIC X(03).
           02  FROMNMC                     PIC X.
           02  FROMNMH                     PIC X.
           02  FROMNMO                     PIC X(030).
           02  FILLER                      PIC X(03).
           02  FRPRDC                      PIC X.
           02  FRPRDH                      PIC X.
           02  FRPRDO                      PIC X(004).
           02  FILLER                      PIC X(03).
           02  FRAVLC                      PIC X.
           02  FRAVLH                      PIC X.
           02  FRAVLO                      PIC X(017).
           02  FILLER                      PIC X(03).
           02  FRODLC                      PIC X.
           02  FRODLH                      PIC X.
           02  FRODLO                      PIC X(012).
           02  FILLER                      PIC X(03).
           02  TOACC                       PIC X.
           02  TOACH                       PIC X.
           02  TOACO                       PIC X(012).
           02  FILLER                      PIC X(03).
           02  TONMC                       PIC X.
           02  TONMH                       PIC X.
           02  TONMO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  TOPRDC                      PIC X.
           02  TOPRDH                      PIC X.
           02  TOPRDO                      PIC X(004).
           02  FILLER                      PIC X(03).
           02  TOAVLC                      PIC X.
           02  TOAVLH                      PIC X.
           02  TOAVLO                      PIC X(017).
           02  FILLER                      PIC X(03).
           02  AMOUNTC                     PIC X.
           02  AMOUNTH                     PIC X.
           02  AMOUNTO                     PIC X(013).
           02  FILLER                      PIC X(03).
           02  MEMOC                       PIC X.
           02  MEMOH                       PIC X.
           02  MEMOO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  CUSTIDC                     PIC X.
           02  CUSTIDH                     PIC X.
           02  CUSTIDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CONFRMC                     PIC X.
           02  CONFRMH                     PIC X.
           02  CONFRMO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  REGDCTC                     PIC X.
           02  REGDCTH                     PIC X.
           02  REGDCTO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  REGDFEC                     PIC X.
           02  REGDFEH                     PIC X.
           02  REGDFEO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  PROJBLC                     PIC X.
           02  PROJBLH                     PIC X.
           02  PROJBLO                     PIC X(017).
           02  FILLER                      PIC X(03).
           02  ODUSEDC                     PIC X.
           02  ODUSEDH                     PIC X.
           02  ODUSEDO                     PIC X(012).
           02  FILLER                      PIC X(03).
           02  XFRREFC                     PIC X.
           02  XFRREFH                     PIC X.
           02  XFRREFO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
