      *================================================================*
      * COPYBOOK : OFACRVS                                             *
      * PURPOSE  : BMS SYMBOLIC MAP FOR MAPSET OFACRVS MAP OFACRVM     *
      * GENERATED FROM src/bms/OFACRVS.bms (DFHMSD TYPE=DSECT)         *
      *================================================================*
       01  OFACRVMI.
           02  FILLER                  PIC X(12).
           02  CURDATEL                COMP PIC S9(4).
           02  CURDATEF                PIC X.
           02  FILLER REDEFINES CURDATEF.
               03  CURDATEA            PIC X.
           02  CURDATEC                PIC X.
           02  CURDATEH                PIC X.
           02  CURDATEI                PIC X(10).
           02  USERIDL                 COMP PIC S9(4).
           02  USERIDF                 PIC X.
           02  FILLER REDEFINES USERIDF.
               03  USERIDA             PIC X.
           02  USERIDC                 PIC X.
           02  USERIDH                 PIC X.
           02  USERIDI                 PIC X(8).
           02  CUSTIDL                 COMP PIC S9(4).
           02  CUSTIDF                 PIC X.
           02  FILLER REDEFINES CUSTIDF.
               03  CUSTIDA             PIC X.
           02  CUSTIDC                 PIC X.
           02  CUSTIDH                 PIC X.
           02  CUSTIDI                 PIC X(10).
           02  CNAMEL                  COMP PIC S9(4).
           02  CNAMEF                  PIC X.
           02  FILLER REDEFINES CNAMEF.
               03  CNAMEA              PIC X.
           02  CNAMEC                  PIC X.
           02  CNAMEH                  PIC X.
           02  CNAMEI                  PIC X(52).
           02  CTYPEL                  COMP PIC S9(4).
           02  CTYPEF                  PIC X.
           02  FILLER REDEFINES CTYPEF.
               03  CTYPEA              PIC X.
           02  CTYPEC                  PIC X.
           02  CTYPEH                  PIC X.
           02  CTYPEI                  PIC X(1).
           02  CCTRYL                  COMP PIC S9(4).
           02  CCTRYF                  PIC X.
           02  FILLER REDEFINES CCTRYF.
               03  CCTRYA              PIC X.
           02  CCTRYC                  PIC X.
           02  CCTRYH                  PIC X.
           02  CCTRYI                  PIC X(2).
           02  CDOBL                   COMP PIC S9(4).
           02  CDOBF                   PIC X.
           02  FILLER REDEFINES CDOBF.
               03  CDOBA               PIC X.
           02  CDOBC                   PIC X.
           02  CDOBH                   PIC X.
           02  CDOBI                   PIC X(8).
           02  CTAXIDL                 COMP PIC S9(4).
           02  CTAXIDF                 PIC X.
           02  FILLER REDEFINES CTAXIDF.
               03  CTAXIDA             PIC X.
           02  CTAXIDC                 PIC X.
           02  CTAXIDH                 PIC X.
           02  CTAXIDI                 PIC X(9).
           02  CSTATL                  COMP PIC S9(4).
           02  CSTATF                  PIC X.
           02  FILLER REDEFINES CSTATF.
               03  CSTATA              PIC X.
           02  CSTATC                  PIC X.
           02  CSTATH                  PIC X.
           02  CSTATI                  PIC X(1).
           02  COFACL                  COMP PIC S9(4).
           02  COFACF                  PIC X.
           02  FILLER REDEFINES COFACF.
               03  COFACA              PIC X.
           02  COFACC                  PIC X.
           02  COFACH                  PIC X.
           02  COFACI                  PIC X(1).
           02  CLSCRNL                 COMP PIC S9(4).
           02  CLSCRNF                 PIC X.
           02  FILLER REDEFINES CLSCRNF.
               03  CLSCRNA             PIC X.
           02  CLSCRNC                 PIC X.
           02  CLSCRNH                 PIC X.
           02  CLSCRNI                 PIC X(8).
           02  SDNENTL                 COMP PIC S9(4).
           02  SDNENTF                 PIC X.
           02  FILLER REDEFINES SDNENTF.
               03  SDNENTA             PIC X.
           02  SDNENTC                 PIC X.
           02  SDNENTH                 PIC X.
           02  SDNENTI                 PIC X(6).
           02  SDNNAMEL                COMP PIC S9(4).
           02  SDNNAMEF                PIC X.
           02  FILLER REDEFINES SDNNAMEF.
               03  SDNNAMEA            PIC X.
           02  SDNNAMEC                PIC X.
           02  SDNNAMEH                PIC X.
           02  SDNNAMEI                PIC X(56).
           02  SDNPGML                 COMP PIC S9(4).
           02  SDNPGMF                 PIC X.
           02  FILLER REDEFINES SDNPGMF.
               03  SDNPGMA             PIC X.
           02  SDNPGMC                 PIC X.
           02  SDNPGMH                 PIC X.
           02  SDNPGMI                 PIC X(10).
           02  SDNSCORL                COMP PIC S9(4).
           02  SDNSCORF                PIC X.
           02  FILLER REDEFINES SDNSCORF.
               03  SDNSCORA            PIC X.
           02  SDNSCORC                PIC X.
           02  SDNSCORH                PIC X.
           02  SDNSCORI                PIC X(3).
           02  SDNRULEL                COMP PIC S9(4).
           02  SDNRULEF                PIC X.
           02  FILLER REDEFINES SDNRULEF.
               03  SDNRULEA            PIC X.
           02  SDNRULEC                PIC X.
           02  SDNRULEH                PIC X.
           02  SDNRULEI                PIC X(4).
           02  SDNCTRYL                COMP PIC S9(4).
           02  SDNCTRYF                PIC X.
           02  FILLER REDEFINES SDNCTRYF.
               03  SDNCTRYA            PIC X.
           02  SDNCTRYC                PIC X.
           02  SDNCTRYH                PIC X.
           02  SDNCTRYI                PIC X(2).
           02  ACCT01L                 COMP PIC S9(4).
           02  ACCT01F                 PIC X.
           02  FILLER REDEFINES ACCT01F.
               03  ACCT01A             PIC X.
           02  ACCT01C                 PIC X.
           02  ACCT01H                 PIC X.
           02  ACCT01I                 PIC X(72).
           02  ACCT02L                 COMP PIC S9(4).
           02  ACCT02F                 PIC X.
           02  FILLER REDEFINES ACCT02F.
               03  ACCT02A             PIC X.
           02  ACCT02C                 PIC X.
           02  ACCT02H                 PIC X.
           02  ACCT02I                 PIC X(72).
           02  ACCT03L                 COMP PIC S9(4).
           02  ACCT03F                 PIC X.
           02  FILLER REDEFINES ACCT03F.
               03  ACCT03A             PIC X.
           02  ACCT03C                 PIC X.
           02  ACCT03H                 PIC X.
           02  ACCT03I                 PIC X(72).
           02  ACCT04L                 COMP PIC S9(4).
           02  ACCT04F                 PIC X.
           02  FILLER REDEFINES ACCT04F.
               03  ACCT04A             PIC X.
           02  ACCT04C                 PIC X.
           02  ACCT04H                 PIC X.
           02  ACCT04I                 PIC X(72).
           02  ACCT05L                 COMP PIC S9(4).
           02  ACCT05F                 PIC X.
           02  FILLER REDEFINES ACCT05F.
               03  ACCT05A             PIC X.
           02  ACCT05C                 PIC X.
           02  ACCT05H                 PIC X.
           02  ACCT05I                 PIC X(72).
           02  DECISL                  COMP PIC S9(4).
           02  DECISF                  PIC X.
           02  FILLER REDEFINES DECISF.
               03  DECISA              PIC X.
           02  DECISC                  PIC X.
           02  DECISH                  PIC X.
           02  DECISI                  PIC X(1).
           02  REASONL                 COMP PIC S9(4).
           02  REASONF                 PIC X.
           02  FILLER REDEFINES REASONF.
               03  REASONA             PIC X.
           02  REASONC                 PIC X.
           02  REASONH                 PIC X.
           02  REASONI                 PIC X(60).
           02  MSGL                    COMP PIC S9(4).
           02  MSGF                    PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                PIC X.
           02  MSGC                    PIC X.
           02  MSGH                    PIC X.
           02  MSGI                    PIC X(78).
       01  OFACRVMO REDEFINES OFACRVMI.
           02  FILLER                  PIC X(12).
           02  FILLER                  PIC X(03).
           02  CURDATEC                PIC X.
           02  CURDATEH                PIC X.
           02  CURDATEO                PIC X(10).
           02  FILLER                  PIC X(03).
           02  USERIDC                 PIC X.
           02  USERIDH                 PIC X.
           02  USERIDO                 PIC X(8).
           02  FILLER                  PIC X(03).
           02  CUSTIDC                 PIC X.
           02  CUSTIDH                 PIC X.
           02  CUSTIDO                 PIC X(10).
           02  FILLER                  PIC X(03).
           02  CNAMEC                  PIC X.
           02  CNAMEH                  PIC X.
           02  CNAMEO                  PIC X(52).
           02  FILLER                  PIC X(03).
           02  CTYPEC                  PIC X.
           02  CTYPEH                  PIC X.
           02  CTYPEO                  PIC X(1).
           02  FILLER                  PIC X(03).
           02  CCTRYC                  PIC X.
           02  CCTRYH                  PIC X.
           02  CCTRYO                  PIC X(2).
           02  FILLER                  PIC X(03).
           02  CDOBC                   PIC X.
           02  CDOBH                   PIC X.
           02  CDOBO                   PIC X(8).
           02  FILLER                  PIC X(03).
           02  CTAXIDC                 PIC X.
           02  CTAXIDH                 PIC X.
           02  CTAXIDO                 PIC X(9).
           02  FILLER                  PIC X(03).
           02  CSTATC                  PIC X.
           02  CSTATH                  PIC X.
           02  CSTATO                  PIC X(1).
           02  FILLER                  PIC X(03).
           02  COFACC                  PIC X.
           02  COFACH                  PIC X.
           02  COFACO                  PIC X(1).
           02  FILLER                  PIC X(03).
           02  CLSCRNC                 PIC X.
           02  CLSCRNH                 PIC X.
           02  CLSCRNO                 PIC X(8).
           02  FILLER                  PIC X(03).
           02  SDNENTC                 PIC X.
           02  SDNENTH                 PIC X.
           02  SDNENTO                 PIC X(6).
           02  FILLER                  PIC X(03).
           02  SDNNAMEC                PIC X.
           02  SDNNAMEH                PIC X.
           02  SDNNAMEO                PIC X(56).
           02  FILLER                  PIC X(03).
           02  SDNPGMC                 PIC X.
           02  SDNPGMH                 PIC X.
           02  SDNPGMO                 PIC X(10).
           02  FILLER                  PIC X(03).
           02  SDNSCORC                PIC X.
           02  SDNSCORH                PIC X.
           02  SDNSCORO                PIC X(3).
           02  FILLER                  PIC X(03).
           02  SDNRULEC                PIC X.
           02  SDNRULEH                PIC X.
           02  SDNRULEO                PIC X(4).
           02  FILLER                  PIC X(03).
           02  SDNCTRYC                PIC X.
           02  SDNCTRYH                PIC X.
           02  SDNCTRYO                PIC X(2).
           02  FILLER                  PIC X(03).
           02  ACCT01C                 PIC X.
           02  ACCT01H                 PIC X.
           02  ACCT01O                 PIC X(72).
           02  FILLER                  PIC X(03).
           02  ACCT02C                 PIC X.
           02  ACCT02H                 PIC X.
           02  ACCT02O                 PIC X(72).
           02  FILLER                  PIC X(03).
           02  ACCT03C                 PIC X.
           02  ACCT03H                 PIC X.
           02  ACCT03O                 PIC X(72).
           02  FILLER                  PIC X(03).
           02  ACCT04C                 PIC X.
           02  ACCT04H                 PIC X.
           02  ACCT04O                 PIC X(72).
           02  FILLER                  PIC X(03).
           02  ACCT05C                 PIC X.
           02  ACCT05H                 PIC X.
           02  ACCT05O                 PIC X(72).
           02  FILLER                  PIC X(03).
           02  DECISC                  PIC X.
           02  DECISH                  PIC X.
           02  DECISO                  PIC X(1).
           02  FILLER                  PIC X(03).
           02  REASONC                 PIC X.
           02  REASONH                 PIC X.
           02  REASONO                 PIC X(60).
           02  FILLER                  PIC X(03).
           02  MSGC                    PIC X.
           02  MSGH                    PIC X.
           02  MSGO                    PIC X(78).
