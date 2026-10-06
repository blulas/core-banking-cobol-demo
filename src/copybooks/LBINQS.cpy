      *================================================================*
      * COPYBOOK : LBINQS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET LBINQS MAP LBINQM         *
      *            TRANSACTION LB89 - LOCKBOX REMITTANCE               *
      *            GENERATED FROM BMS SOURCE LBINQS - DO NOT EDIT      *
      *================================================================*
       01  LBINQMI.
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
           02  ADDRL                       PIC S9(4) COMP.
           02  ADDRF                       PIC X.
           02  FILLER REDEFINES ADDRF.
               03  ADDRA                   PIC X.
           02  FILLER                      PIC X(02).
           02  ADDRI                       PIC X(030).
           02  FLOATAL                     PIC S9(4) COMP.
           02  FLOATAF                     PIC X.
           02  FILLER REDEFINES FLOATAF.
               03  FLOATAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FLOATAI                     PIC X(016).
           02  OPENDAL                     PIC S9(4) COMP.
           02  OPENDAF                     PIC X.
           02  FILLER REDEFINES OPENDAF.
               03  OPENDAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  OPENDAI                     PIC X(010).
           02  EXEMPTL                     PIC S9(4) COMP.
           02  EXEMPTF                     PIC X.
           02  FILLER REDEFINES EXEMPTF.
               03  EXEMPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXEMPTI                     PIC X(001).
           02  DEPOSIL                     PIC S9(4) COMP.
           02  DEPOSIF                     PIC X.
           02  FILLER REDEFINES DEPOSIF.
               03  DEPOSIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DEPOSII                     PIC X(016).
           02  CONTACL                     PIC S9(4) COMP.
           02  CONTACF                     PIC X.
           02  FILLER REDEFINES CONTACF.
               03  CONTACA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CONTACI                     PIC X(020).
           02  CASHONL                     PIC S9(4) COMP.
           02  CASHONF                     PIC X.
           02  FILLER REDEFINES CASHONF.
               03  CASHONA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CASHONI                     PIC X(016).
           02  LASTSTL                     PIC S9(4) COMP.
           02  LASTSTF                     PIC X.
           02  FILLER REDEFINES LASTSTF.
               03  LASTSTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTSTI                     PIC X(010).
           02  EFFECTL                     PIC S9(4) COMP.
           02  EFFECTF                     PIC X.
           02  FILLER REDEFINES EFFECTF.
               03  EFFECTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EFFECTI                     PIC X(010).
           02  NOTICEL                     PIC S9(4) COMP.
           02  NOTICEF                     PIC X.
           02  FILLER REDEFINES NOTICEF.
               03  NOTICEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICEI                     PIC X(001).
           02  FEEAMTL                     PIC S9(4) COMP.
           02  FEEAMTF                     PIC X.
           02  FILLER REDEFINES FEEAMTF.
               03  FEEAMTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FEEAMTI                     PIC X(016).
           02  DISCOUL                     PIC S9(4) COMP.
           02  DISCOUF                     PIC X.
           02  FILLER REDEFINES DISCOUF.
               03  DISCOUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DISCOUI                     PIC X(010).
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
       01  LBINQMO REDEFINES LBINQMI.
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
           02  ADDRC                       PIC X.
           02  ADDRH                       PIC X.
           02  ADDRO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  FLOATAC                     PIC X.
           02  FLOATAH                     PIC X.
           02  FLOATAO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  OPENDAC                     PIC X.
           02  OPENDAH                     PIC X.
           02  OPENDAO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  EXEMPTC                     PIC X.
           02  EXEMPTH                     PIC X.
           02  EXEMPTO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  DEPOSIC                     PIC X.
           02  DEPOSIH                     PIC X.
           02  DEPOSIO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  CONTACC                     PIC X.
           02  CONTACH                     PIC X.
           02  CONTACO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  CASHONC                     PIC X.
           02  CASHONH                     PIC X.
           02  CASHONO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  LASTSTC                     PIC X.
           02  LASTSTH                     PIC X.
           02  LASTSTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  EFFECTC                     PIC X.
           02  EFFECTH                     PIC X.
           02  EFFECTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  NOTICEC                     PIC X.
           02  NOTICEH                     PIC X.
           02  NOTICEO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  FEEAMTC                     PIC X.
           02  FEEAMTH                     PIC X.
           02  FEEAMTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  DISCOUC                     PIC X.
           02  DISCOUH                     PIC X.
           02  DISCOUO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  RETRYCC                     PIC X.
           02  RETRYCH                     PIC X.
           02  RETRYCO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
