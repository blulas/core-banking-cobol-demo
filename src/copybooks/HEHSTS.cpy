      *================================================================*
      * COPYBOOK : HEHSTS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET HEHSTS MAP HEHSTM         *
      *            TRANSACTION HE86 - HOME EQUITY LINES                *
      *            GENERATED FROM BMS SOURCE HEHSTS - DO NOT EDIT      *
      *================================================================*
       01  HEHSTMI.
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
           02  ANNUALL                     PIC S9(4) COMP.
           02  ANNUALF                     PIC X.
           02  FILLER REDEFINES ANNUALF.
               03  ANNUALA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ANNUALI                     PIC X(016).
           02  BALANCL                     PIC S9(4) COMP.
           02  BALANCF                     PIC X.
           02  FILLER REDEFINES BALANCF.
               03  BALANCA                 PIC X.
           02  FILLER                      PIC X(02).
           02  BALANCI                     PIC X(016).
           02  INTEREL                     PIC S9(4) COMP.
           02  INTEREF                     PIC X.
           02  FILLER REDEFINES INTEREF.
               03  INTEREA                 PIC X.
           02  FILLER                      PIC X(02).
           02  INTEREI                     PIC X(016).
           02  REGIONL                     PIC S9(4) COMP.
           02  REGIONF                     PIC X.
           02  FILLER REDEFINES REGIONF.
               03  REGIONA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REGIONI                     PIC X(003).
           02  DESCL                       PIC S9(4) COMP.
           02  DESCF                       PIC X.
           02  FILLER REDEFINES DESCF.
               03  DESCA                   PIC X.
           02  FILLER                      PIC X(02).
           02  DESCI                       PIC X(030).
           02  HOLDFLL                     PIC S9(4) COMP.
           02  HOLDFLF                     PIC X.
           02  FILLER REDEFINES HOLDFLF.
               03  HOLDFLA                 PIC X.
           02  FILLER                      PIC X(02).
           02  HOLDFLI                     PIC X(001).
           02  MATURIL                     PIC S9(4) COMP.
           02  MATURIF                     PIC X.
           02  FILLER REDEFINES MATURIF.
               03  MATURIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  MATURII                     PIC X(010).
           02  NAMEL                       PIC S9(4) COMP.
           02  NAMEF                       PIC X.
           02  FILLER REDEFINES NAMEF.
               03  NAMEA                   PIC X.
           02  FILLER                      PIC X(02).
           02  NAMEI                       PIC X(030).
           02  MINPAYL                     PIC S9(4) COMP.
           02  MINPAYF                     PIC X.
           02  FILLER REDEFINES MINPAYF.
               03  MINPAYA                 PIC X.
           02  FILLER                      PIC X(02).
           02  MINPAYI                     PIC X(016).
           02  AUTOREL                     PIC S9(4) COMP.
           02  AUTOREF                     PIC X.
           02  FILLER REDEFINES AUTOREF.
               03  AUTOREA                 PIC X.
           02  FILLER                      PIC X(02).
           02  AUTOREI                     PIC X(001).
           02  DRAWYTL                     PIC S9(4) COMP.
           02  DRAWYTF                     PIC X.
           02  FILLER REDEFINES DRAWYTF.
               03  DRAWYTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DRAWYTI                     PIC X(016).
           02  LIENPOL                     PIC S9(4) COMP.
           02  LIENPOF                     PIC X.
           02  FILLER REDEFINES LIENPOF.
               03  LIENPOA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LIENPOI                     PIC X(003).
           02  APPRAIL                     PIC S9(4) COMP.
           02  APPRAIF                     PIC X.
           02  FILLER REDEFINES APPRAIF.
               03  APPRAIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  APPRAII                     PIC X(016).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  HEHSTMO REDEFINES HEHSTMI.
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
           02  ANNUALC                     PIC X.
           02  ANNUALH                     PIC X.
           02  ANNUALO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  BALANCC                     PIC X.
           02  BALANCH                     PIC X.
           02  BALANCO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  INTEREC                     PIC X.
           02  INTEREH                     PIC X.
           02  INTEREO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  REGIONC                     PIC X.
           02  REGIONH                     PIC X.
           02  REGIONO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  DESCC                       PIC X.
           02  DESCH                       PIC X.
           02  DESCO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  HOLDFLC                     PIC X.
           02  HOLDFLH                     PIC X.
           02  HOLDFLO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  MATURIC                     PIC X.
           02  MATURIH                     PIC X.
           02  MATURIO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  NAMEC                       PIC X.
           02  NAMEH                       PIC X.
           02  NAMEO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  MINPAYC                     PIC X.
           02  MINPAYH                     PIC X.
           02  MINPAYO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  AUTOREC                     PIC X.
           02  AUTOREH                     PIC X.
           02  AUTOREO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  DRAWYTC                     PIC X.
           02  DRAWYTH                     PIC X.
           02  DRAWYTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  LIENPOC                     PIC X.
           02  LIENPOH                     PIC X.
           02  LIENPOO                     PIC X(003).
           02  FILLER                      PIC X(03).
           02  APPRAIC                     PIC X.
           02  APPRAIH                     PIC X.
           02  APPRAIO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
