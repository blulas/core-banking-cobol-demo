      *================================================================*
      * COPYBOOK : TCOVRS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET TCOVRS MAP TCOVRM         *
      *            TRANSACTION TC66 - TRAVELERS CHEQUES                *
      *            GENERATED FROM BMS SOURCE TCOVRS - DO NOT EDIT      *
      *================================================================*
       01  TCOVRMI.
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
           02  KEYL                        PIC S9(4) COMP.
           02  KEYF                        PIC X.
           02  FILLER REDEFINES KEYF.
               03  KEYA                    PIC X.
           02  FILLER                      PIC X(02).
           02  KEYI                        PIC X(012).
           02  LASTSTL                     PIC S9(4) COMP.
           02  LASTSTF                     PIC X.
           02  FILLER REDEFINES LASTSTF.
               03  LASTSTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTSTI                     PIC X(010).
           02  NAMEL                       PIC S9(4) COMP.
           02  NAMEF                       PIC X.
           02  FILLER REDEFINES NAMEF.
               03  NAMEA                   PIC X.
           02  FILLER                      PIC X(02).
           02  NAMEI                       PIC X(030).
           02  DENOMCL                     PIC S9(4) COMP.
           02  DENOMCF                     PIC X.
           02  FILLER REDEFINES DENOMCF.
               03  DENOMCA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DENOMCI                     PIC X(001).
           02  REFUNDL                     PIC S9(4) COMP.
           02  REFUNDF                     PIC X.
           02  FILLER REDEFINES REFUNDF.
               03  REFUNDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REFUNDI                     PIC X(016).
           02  STATUSL                     PIC S9(4) COMP.
           02  STATUSF                     PIC X.
           02  FILLER REDEFINES STATUSF.
               03  STATUSA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STATUSI                     PIC X(002).
           02  SUSPENL                     PIC S9(4) COMP.
           02  SUSPENF                     PIC X.
           02  FILLER REDEFINES SUSPENF.
               03  SUSPENA                 PIC X.
           02  FILLER                      PIC X(02).
           02  SUSPENI                     PIC X(001).
           02  SHORTAL                     PIC S9(4) COMP.
           02  SHORTAF                     PIC X.
           02  FILLER REDEFINES SHORTAF.
               03  SHORTAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  SHORTAI                     PIC X(016).
           02  LASTPAL                     PIC S9(4) COMP.
           02  LASTPAF                     PIC X.
           02  FILLER REDEFINES LASTPAF.
               03  LASTPAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTPAI                     PIC X(010).
           02  FEEPCTL                     PIC S9(4) COMP.
           02  FEEPCTF                     PIC X.
           02  FILLER REDEFINES FEEPCTF.
               03  FEEPCTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FEEPCTI                     PIC X(010).
           02  COMMISL                     PIC S9(4) COMP.
           02  COMMISF                     PIC X.
           02  FILLER REDEFINES COMMISF.
               03  COMMISA                 PIC X.
           02  FILLER                      PIC X(02).
           02  COMMISI                     PIC X(016).
           02  NEXTREL                     PIC S9(4) COMP.
           02  NEXTREF                     PIC X.
           02  FILLER REDEFINES NEXTREF.
               03  NEXTREA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NEXTREI                     PIC X(010).
           02  MARGINL                     PIC S9(4) COMP.
           02  MARGINF                     PIC X.
           02  FILLER REDEFINES MARGINF.
               03  MARGINA                 PIC X.
           02  FILLER                      PIC X(02).
           02  MARGINI                     PIC X(010).
           02  RETRYCL                     PIC S9(4) COMP.
           02  RETRYCF                     PIC X.
           02  FILLER REDEFINES RETRYCF.
               03  RETRYCA                 PIC X.
           02  FILLER                      PIC X(02).
           02  RETRYCI                     PIC X(007).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  TCOVRMO REDEFINES TCOVRMI.
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
           02  KEYC                        PIC X.
           02  KEYH                        PIC X.
           02  KEYO                        PIC X(012).
           02  FILLER                      PIC X(03).
           02  LASTSTC                     PIC X.
           02  LASTSTH                     PIC X.
           02  LASTSTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  NAMEC                       PIC X.
           02  NAMEH                       PIC X.
           02  NAMEO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  DENOMCC                     PIC X.
           02  DENOMCH                     PIC X.
           02  DENOMCO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  REFUNDC                     PIC X.
           02  REFUNDH                     PIC X.
           02  REFUNDO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  STATUSC                     PIC X.
           02  STATUSH                     PIC X.
           02  STATUSO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  SUSPENC                     PIC X.
           02  SUSPENH                     PIC X.
           02  SUSPENO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  SHORTAC                     PIC X.
           02  SHORTAH                     PIC X.
           02  SHORTAO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  LASTPAC                     PIC X.
           02  LASTPAH                     PIC X.
           02  LASTPAO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  FEEPCTC                     PIC X.
           02  FEEPCTH                     PIC X.
           02  FEEPCTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  COMMISC                     PIC X.
           02  COMMISH                     PIC X.
           02  COMMISO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  NEXTREC                     PIC X.
           02  NEXTREH                     PIC X.
           02  NEXTREO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  MARGINC                     PIC X.
           02  MARGINH                     PIC X.
           02  MARGINO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  RETRYCC                     PIC X.
           02  RETRYCH                     PIC X.
           02  RETRYCO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
