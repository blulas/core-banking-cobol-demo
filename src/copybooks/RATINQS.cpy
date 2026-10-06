      *================================================================*
      * COPYBOOK : RATINQS                                               *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET RATINQS MAP RATINQM         *
      *            TRANSACTION CB20 - DEPOSIT RATE INQUIRY              *
      *            GENERATED FROM BMS SOURCE RATINQS - DO NOT EDIT     *
      *================================================================*
       01  RATINQMI.
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
           02  PRODL                       PIC S9(4) COMP.
           02  PRODF                       PIC X.
           02  FILLER REDEFINES PRODF.
               03  PRODA                   PIC X.
           02  FILLER                      PIC X(02).
           02  PRODI                       PIC X(004).
           02  PRDNAMEL                    PIC S9(4) COMP.
           02  PRDNAMEF                    PIC X.
           02  FILLER REDEFINES PRDNAMEF.
               03  PRDNAMEA                PIC X.
           02  FILLER                      PIC X(02).
           02  PRDNAMEI                    PIC X(030).
           02  WIBALL                      PIC S9(4) COMP.
           02  WIBALF                      PIC X.
           02  FILLER REDEFINES WIBALF.
               03  WIBALA                  PIC X.
           02  FILLER                      PIC X(02).
           02  WIBALI                      PIC X(015).
           02  WITERML                     PIC S9(4) COMP.
           02  WITERMF                     PIC X.
           02  FILLER REDEFINES WITERMF.
               03  WITERMA                 PIC X.
           02  FILLER                      PIC X(02).
           02  WITERMI                     PIC X(003).
           02  WISEGL                      PIC S9(4) COMP.
           02  WISEGF                      PIC X.
           02  FILLER REDEFINES WISEGF.
               03  WISEGA                  PIC X.
           02  FILLER                      PIC X(02).
           02  WISEGI                      PIC X(002).
           02  WIEMPL                      PIC S9(4) COMP.
           02  WIEMPF                      PIC X.
           02  FILLER REDEFINES WIEMPF.
               03  WIEMPA                  PIC X.
           02  FILLER                      PIC X(02).
           02  WIEMPI                      PIC X(001).
           02  WIRELL                      PIC S9(4) COMP.
           02  WIRELF                      PIC X.
           02  FILLER REDEFINES WIRELF.
               03  WIRELA                  PIC X.
           02  FILLER                      PIC X(02).
           02  WIRELI                      PIC X(015).
           02  ASOFL                       PIC S9(4) COMP.
           02  ASOFF                       PIC X.
           02  FILLER REDEFINES ASOFF.
               03  ASOFA                   PIC X.
           02  FILLER                      PIC X(02).
           02  ASOFI                       PIC X(010).
           02  RRATEL                      PIC S9(4) COMP.
           02  RRATEF                      PIC X.
           02  FILLER REDEFINES RRATEF.
               03  RRATEA                  PIC X.
           02  FILLER                      PIC X(02).
           02  RRATEI                      PIC X(010).
           02  RAPYL                       PIC S9(4) COMP.
           02  RAPYF                       PIC X.
           02  FILLER REDEFINES RAPYF.
               03  RAPYA                   PIC X.
           02  FILLER                      PIC X(02).
           02  RAPYI                       PIC X(010).
           02  RTIERL                      PIC S9(4) COMP.
           02  RTIERF                      PIC X.
           02  FILLER REDEFINES RTIERF.
               03  RTIERA                  PIC X.
           02  FILLER                      PIC X(02).
           02  RTIERI                      PIC X(004).
           02  TLINE01L                    PIC S9(4) COMP.
           02  TLINE01F                    PIC X.
           02  FILLER REDEFINES TLINE01F.
               03  TLINE01A                PIC X.
           02  FILLER                      PIC X(02).
           02  TLINE01I                    PIC X(070).
           02  TLINE02L                    PIC S9(4) COMP.
           02  TLINE02F                    PIC X.
           02  FILLER REDEFINES TLINE02F.
               03  TLINE02A                PIC X.
           02  FILLER                      PIC X(02).
           02  TLINE02I                    PIC X(070).
           02  TLINE03L                    PIC S9(4) COMP.
           02  TLINE03F                    PIC X.
           02  FILLER REDEFINES TLINE03F.
               03  TLINE03A                PIC X.
           02  FILLER                      PIC X(02).
           02  TLINE03I                    PIC X(070).
           02  TLINE04L                    PIC S9(4) COMP.
           02  TLINE04F                    PIC X.
           02  FILLER REDEFINES TLINE04F.
               03  TLINE04A                PIC X.
           02  FILLER                      PIC X(02).
           02  TLINE04I                    PIC X(070).
           02  TLINE05L                    PIC S9(4) COMP.
           02  TLINE05F                    PIC X.
           02  FILLER REDEFINES TLINE05F.
               03  TLINE05A                PIC X.
           02  FILLER                      PIC X(02).
           02  TLINE05I                    PIC X(070).
           02  TLINE06L                    PIC S9(4) COMP.
           02  TLINE06F                    PIC X.
           02  FILLER REDEFINES TLINE06F.
               03  TLINE06A                PIC X.
           02  FILLER                      PIC X(02).
           02  TLINE06I                    PIC X(070).
           02  TLINE07L                    PIC S9(4) COMP.
           02  TLINE07F                    PIC X.
           02  FILLER REDEFINES TLINE07F.
               03  TLINE07A                PIC X.
           02  FILLER                      PIC X(02).
           02  TLINE07I                    PIC X(070).
           02  TLINE08L                    PIC S9(4) COMP.
           02  TLINE08F                    PIC X.
           02  FILLER REDEFINES TLINE08F.
               03  TLINE08A                PIC X.
           02  FILLER                      PIC X(02).
           02  TLINE08I                    PIC X(070).
           02  TLINE09L                    PIC S9(4) COMP.
           02  TLINE09F                    PIC X.
           02  FILLER REDEFINES TLINE09F.
               03  TLINE09A                PIC X.
           02  FILLER                      PIC X(02).
           02  TLINE09I                    PIC X(070).
           02  TLINE10L                    PIC S9(4) COMP.
           02  TLINE10F                    PIC X.
           02  FILLER REDEFINES TLINE10F.
               03  TLINE10A                PIC X.
           02  FILLER                      PIC X(02).
           02  TLINE10I                    PIC X(070).
           02  BONUSL                      PIC S9(4) COMP.
           02  BONUSF                      PIC X.
           02  FILLER REDEFINES BONUSF.
               03  BONUSA                  PIC X.
           02  FILLER                      PIC X(02).
           02  BONUSI                      PIC X(077).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  RATINQMO REDEFINES RATINQMI.
           02  FILLER                      PIC X(12).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  TRANIDO                     PIC X(004).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  TITLEO                      PIC X(040).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  CURDATEO                    PIC X(010).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  PRODO                       PIC X(004).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  PRDNAMEO                    PIC X(030).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  WIBALO                      PIC X(015).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  WITERMO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  WISEGO                      PIC X(002).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  WIEMPO                      PIC X(001).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  WIRELO                      PIC X(015).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  ASOFO                       PIC X(010).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  RRATEO                      PIC X(010).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  RAPYO                       PIC X(010).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  RTIERO                      PIC X(004).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  TLINE01O                    PIC X(070).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  TLINE02O                    PIC X(070).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  TLINE03O                    PIC X(070).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  TLINE04O                    PIC X(070).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  TLINE05O                    PIC X(070).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  TLINE06O                    PIC X(070).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  TLINE07O                    PIC X(070).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  TLINE08O                    PIC X(070).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  TLINE09O                    PIC X(070).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  TLINE10O                    PIC X(070).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  BONUSO                      PIC X(077).
           02  FILLER                      PIC X(03).
           02  FILLER                      PIC X(02).
           02  MSGO                        PIC X(079).
