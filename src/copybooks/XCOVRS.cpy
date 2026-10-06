      *================================================================*
      * COPYBOOK : XCOVRS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET XCOVRS MAP XCOVRM         *
      *            TRANSACTION XC59 - CHRISTMAS CLUB SAVINGS           *
      *            GENERATED FROM BMS SOURCE XCOVRS - DO NOT EDIT      *
      *================================================================*
       01  XCOVRMI.
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
           02  DESCL                       PIC S9(4) COMP.
           02  DESCF                       PIC X.
           02  FILLER REDEFINES DESCF.
               03  DESCA                   PIC X.
           02  FILLER                      PIC X(02).
           02  DESCI                       PIC X(030).
           02  STOPFLL                     PIC S9(4) COMP.
           02  STOPFLF                     PIC X.
           02  FILLER REDEFINES STOPFLF.
               03  STOPFLA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STOPFLI                     PIC X(001).
           02  CONTACL                     PIC S9(4) COMP.
           02  CONTACF                     PIC X.
           02  FILLER REDEFINES CONTACF.
               03  CONTACA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CONTACI                     PIC X(020).
           02  RETRYCL                     PIC S9(4) COMP.
           02  RETRYCF                     PIC X.
           02  FILLER REDEFINES RETRYCF.
               03  RETRYCA                 PIC X.
           02  FILLER                      PIC X(02).
           02  RETRYCI                     PIC X(007).
           02  GOALYTL                     PIC S9(4) COMP.
           02  GOALYTF                     PIC X.
           02  FILLER REDEFINES GOALYTF.
               03  GOALYTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  GOALYTI                     PIC X(016).
           02  ACCRUEL                     PIC S9(4) COMP.
           02  ACCRUEF                     PIC X.
           02  FILLER REDEFINES ACCRUEF.
               03  ACCRUEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ACCRUEI                     PIC X(016).
           02  PAYOUTL                     PIC S9(4) COMP.
           02  PAYOUTF                     PIC X.
           02  FILLER REDEFINES PAYOUTF.
               03  PAYOUTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PAYOUTI                     PIC X(016).
           02  BONUSAL                     PIC S9(4) COMP.
           02  BONUSAF                     PIC X.
           02  FILLER REDEFINES BONUSAF.
               03  BONUSAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  BONUSAI                     PIC X(016).
           02  MISSEDL                     PIC S9(4) COMP.
           02  MISSEDF                     PIC X.
           02  FILLER REDEFINES MISSEDF.
               03  MISSEDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  MISSEDI                     PIC X(016).
           02  NOTICEL                     PIC S9(4) COMP.
           02  NOTICEF                     PIC X.
           02  FILLER REDEFINES NOTICEF.
               03  NOTICEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICEI                     PIC X(010).
           02  NEXTDUL                     PIC S9(4) COMP.
           02  NEXTDUF                     PIC X.
           02  FILLER REDEFINES NEXTDUF.
               03  NEXTDUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NEXTDUI                     PIC X(010).
           02  PLANCDL                     PIC S9(4) COMP.
           02  PLANCDF                     PIC X.
           02  FILLER REDEFINES PLANCDF.
               03  PLANCDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PLANCDI                     PIC X(002).
           02  REASONL                     PIC S9(4) COMP.
           02  REASONF                     PIC X.
           02  FILLER REDEFINES REASONF.
               03  REASONA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REASONI                     PIC X(002).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  XCOVRMO REDEFINES XCOVRMI.
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
           02  DESCC                       PIC X.
           02  DESCH                       PIC X.
           02  DESCO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  STOPFLC                     PIC X.
           02  STOPFLH                     PIC X.
           02  STOPFLO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  CONTACC                     PIC X.
           02  CONTACH                     PIC X.
           02  CONTACO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  RETRYCC                     PIC X.
           02  RETRYCH                     PIC X.
           02  RETRYCO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  GOALYTC                     PIC X.
           02  GOALYTH                     PIC X.
           02  GOALYTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  ACCRUEC                     PIC X.
           02  ACCRUEH                     PIC X.
           02  ACCRUEO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  PAYOUTC                     PIC X.
           02  PAYOUTH                     PIC X.
           02  PAYOUTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  BONUSAC                     PIC X.
           02  BONUSAH                     PIC X.
           02  BONUSAO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  MISSEDC                     PIC X.
           02  MISSEDH                     PIC X.
           02  MISSEDO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  NOTICEC                     PIC X.
           02  NOTICEH                     PIC X.
           02  NOTICEO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  NEXTDUC                     PIC X.
           02  NEXTDUH                     PIC X.
           02  NEXTDUO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  PLANCDC                     PIC X.
           02  PLANCDH                     PIC X.
           02  PLANCDO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  REASONC                     PIC X.
           02  REASONH                     PIC X.
           02  REASONO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
