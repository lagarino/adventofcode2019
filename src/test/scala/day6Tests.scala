import org.scalatest.{FlatSpec, Matchers}

class day6Tests extends FlatSpec with Matchers {

  "The first part" should "work for the first examples" in {
    val sut = Day6("COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L")
    sut.countOrbits() shouldBe 42
  }

  it should "work for the input" in {
    val sut = Day6("H1R)Z5F\nR6L)JYJ\nQVZ)B3R\nZD8)Y6T\nFWC)F2N\n3HH)HSX\n2PQ)NXT\nC7Q)7HM\n6KQ)B1V\nCVJ)SXY\nZZR)627\nHWF)5Q2\n7QR)BHQ\n1FF)SBD\nF9B)XXF\n415)C8L\nK38)LZ2\nQ3R)61L\nQ8V)TDC\nH5W)52V\nPQY)BDX\nC54)QSK\nPHC)JPX\nZLS)CRN\nPSR)T5C\nFWK)Q48\nBNL)FWC\nZPV)CR3\nHM5)C5J\nY4F)X6L\nFX1)1LX\nD7W)P4X\n5Y6)L55\nL2R)2WD\nF26)TPF\nYYH)F3Z\nXQB)GPY\n8W7)6PH\n53X)K5N\nMFG)KP7\nYTT)DJS\nNDT)NN3\nS7X)8HH\n5CR)BS6\nQH8)MKW\n8FY)LG7\nSC1)CC6\n8DX)N55\nT71)M2G\nSBD)T4B\n9B1)K4M\nKXB)YRK\nH3S)DP9\nY71)MW5\n43G)9NB\nYFT)BFG\nGSZ)H37\nQP8)VT7\n3CC)KJL\n8VN)Y5S\nCP6)GQB\nBX3)DQ4\nP4X)XJT\nZMK)7WW\nRVX)J55\nS9X)7MC\nZYY)T4P\nX6L)126\nT1W)H6H\nBS6)S7X\nYNK)KKR\n2BD)SM3\nPR8)MJ8\nL2H)2PQ\nKM2)1WX\nV8F)FX1\n92J)NWF\nCF4)XMV\n7G6)KXB\nZHP)K75\n9T5)Q7Y\nX71)HV5\nKK5)1X2\nJCB)DWH\nT8W)S9X\n2S6)J6M\n53K)H87\nF3J)JLL\n5Y6)VY5\nCQD)2ZY\n8W4)PX4\n4H2)KR4\nGWY)SNB\nF8J)5T9\nS3G)54J\nPQV)PKY\n96C)L67\nCP6)R66\nH5P)6SZ\nP9V)686\nCP8)S8Y\n6CQ)QRM\nPZK)DVV\nDK5)QWQ\nBPM)52D\nV8R)YYS\nCM7)2NF\nH8P)F11\nTC5)KXG\n7QK)6XG\nD9F)NZ7\n1D5)86C\nZP5)C54\nC6T)Q7K\n9KM)QPC\nLBC)HFX\nDK6)LZW\nWXC)2FC\nBX4)2XY\n6H7)2HJ\nDZ2)5RD\nKYB)H6T\n2QW)PVV\n2MD)BNP\nVMX)SLG\nD6K)RXB\nGHS)HGB\nQCT)T29\nX1B)VT4\nBN9)M79\nSXK)CSF\nSG7)YPY\nZPK)GHS\n6X4)L59\nB89)LP3\n9TZ)HW9\nYPY)M5P\nLS2)92L\nZ2T)QQM\nCK7)NKB\n3PY)VS8\n2MQ)NT7\nC6Z)8F9\nNWF)35C\nGYR)4NR\nZQG)W9H\n7D1)V8R\nY2X)F26\n8X8)7KF\nCN1)PZC\nBPW)PBY\n9WC)H8V\nZXH)GKH\nCF4)N5L\nC54)YJS\nRY1)GZC\nWMT)8CW\nMZP)9SH\nVZC)SKL\nY7T)C7V\nSYG)R14\nZLS)MLH\nMZT)G1Z\nNPR)VTV\nLCQ)PZK\nRNW)SXK\n8CW)8YN\nXXM)MFD\nYL3)8WB\nDTK)JFZ\nTKC)J8R\nMVL)TYJ\nQR7)SKG\n9GZ)BXM\nM4Y)P9V\n4XY)8QP\n9XC)X52\nCVF)PTH\n52S)D9L\nTCP)SBL\nSPG)KSV\nMTN)GWY\nVJL)N74\n4RT)LHJ\nRG6)5X3\nQ7Z)CP8\n5X3)3PY\nPVQ)TCF\nJR1)7QK\nDGV)YFL\nBQ9)B2Q\nW75)6KQ\n13H)QYX\nHQT)G2V\nHDV)7RG\nGV3)HFT\n627)Q5F\nYYS)RVQ\nCQH)BPW\nW3X)VXF\nSZN)SLF\nC7Q)8X8\n3SY)X64\nJG5)C6S\n4C3)PSR\nQ1W)RNW\nT6J)NPL\nT4L)D7L\nL8P)27X\nR66)TRQ\nPKY)YCL\nGQX)JVG\n2NM)RWX\nMP5)MR4\n5R2)GRN\nJRM)M34\nTHK)RPR\nGKN)46X\n1D1)8YS\nXYN)6SV\nSRV)GB3\nF9N)8FY\nSLG)72G\nJ2Y)W6P\n3RZ)K2W\nW85)CY9\nMJ8)2FT\n9B5)XW8\nPJD)6WB\nDBX)V2T\nM6X)VBM\nXX2)BTC\n1MN)XXM\nDLD)3Q2\n4SH)BPL\nT8Y)ZPV\n3T9)HMW\nC3Z)GDS\nYJ4)L2Z\nVJ7)Q7H\nD72)K1T\n5ZP)QP8\nXZD)894\n67Y)BKF\nZLD)YST\nNQ1)F5H\n2QB)M99\nWZ9)XC9\nCOM)RR1\nF3J)G1X\nSCR)L66\n46X)QT3\n4ZN)JDX\nJYJ)2QB\nXTB)349\nLM1)83X\n29M)L2H\nD5Y)VV1\nYTK)BKW\nN72)GD4\n1PB)8ZM\nV31)99V\nRR1)68N\nBM3)99C\nXRT)VM9\n6FK)675\n5P9)CMQ\nDSM)SJ5\nYST)KB5\n83N)6VW\nZCQ)5SM\nV1Q)2S6\nLZR)QBL\nH3G)9S1\n88G)YTK\nNPL)ZHP\nW44)NWY\n5KZ)1MC\n52V)9B5\nWL2)5P9\nJDX)V8F\nR16)ZWZ\nFVT)596\n8J4)FRL\n2HC)986\nNFM)KWB\nF2L)Y5X\nVPS)43K\nVX8)G8C\nDMH)RCD\nNCH)TPZ\n9KG)MMP\nRPR)CFW\nJWY)SC1\nXTB)H3G\n7FP)FMV\nHGB)6JL\n1P4)ZPK\nWCF)11H\nZR1)Y75\nNP5)MNZ\nGNT)8W7\nNGD)GNL\nQ98)F6V\nF32)7L4\nCFW)684\nT5C)QWC\nVV6)SDZ\n9QW)VKG\n7WZ)R16\nLZ2)NYL\nG2V)ZXH\nVL4)9LX\n9F5)CS5\nFYD)VX8\nR7C)2HC\nLQS)54R\n33W)F8J\nN9C)ZGL\n9ZB)5S8\nMLJ)8VY\nLVY)T1W\n1FC)JG5\nK75)LZR\nLZ9)2XS\n9P8)75M\nLW6)TWM\nK71)DGV\nR2C)GKX\n99C)ML2\nCM5)YKW\n5YK)HM5\n9D9)G9V\n2BT)74Z\nTCP)NDT\nL59)N3C\n3FG)ZH5\n3S8)96C\nJBB)9KG\nQHN)CS4\n9TZ)PRZ\nSM2)MHJ\nZ5J)5QY\nBNP)7HZ\nHDY)LBC\nQMV)7SK\nGT4)SN7\n1MC)MZP\n5C3)9WC\n596)L1H\nZNZ)CM5\nPBN)KLS\n2K7)BN9\nYJL)2VV\nMJX)TDX\nK2W)5ZP\n1F9)6KC\nBM8)D72\n79Q)ZTP\n7RC)88G\n7WW)JBB\n43Z)7G1\nFKV)3W9\nT1W)911\n5RD)D7C\nXBZ)NQ1\nC8L)F3M\nM79)YFY\nG2Q)LXS\nF6V)T8Y\n6XG)127\nCCV)2LJ\nD9T)Q5L\nZTP)XL3\nLG7)X1Q\nJHG)WBL\n8F9)3DB\nFNV)W7B\nHS2)PR8\n6PL)WPM\nHQH)9NT\nWV2)3FL\n1YM)Z51\nFCL)4KM\nVBM)8DX\nJXJ)MQD\nH6C)K46\n72G)9XC\nL66)Y2S\nHMW)XPP\n29T)LM1\nBPL)F3J\nVTV)TX8\n9WW)L2R\nZ51)GL2\nYD1)DZC\n4TY)BPM\nC84)GV3\nSFV)1ZV\nXJT)17T\nRK5)ZR8\n71K)DK5\nLTM)71K\nQNL)XQB\nWYV)N6B\nJDX)5JC\n6X9)CMF\nTRQ)RMY\n2S6)29M\n4CJ)CTD\nKG4)7BX\nJLL)LCN\nQYX)DMC\n1P4)C6T\nZVW)RKM\nXCQ)V31\nWM3)G6P\nHFX)TD9\n7RC)Y4G\nM5F)4WV\n6GP)2K7\nPYL)5XS\n88C)JMT\nVB3)Q1D\nRQS)JBS\nSMJ)VSH\n2WM)9RK\n9SJ)1D5\nSP5)MLJ\n9RK)2QW\n1JZ)R6X\nN4S)WGR\nXWS)J1K\nFGY)N79\nLM1)6DS\nNVW)323\nLSM)46Z\n97D)M6X\n256)V8V\nRCD)DHR\n3B4)3RR\nR49)8W6\n6BS)49C\nC29)BM8\nNFG)9WW\n9HC)F2L\nDVV)N4S\nCCP)R3W\nDF9)ZN9\nW16)BDD\nRND)CLC\nCMF)HST\nX1Q)C52\nMS4)5CY\nBSM)W2H\n971)482\n6JV)CBP\nNB8)HG6\nLQM)ZMK\n64H)DQJ\n81H)2WM\nP18)1QN\n1Z3)RH9\nQ84)QH1\nSJ5)QBB\n83X)W28\nXF3)D9T\nL7W)BPN\nPH6)81H\nQ5F)4GJ\n8DT)FKL\n55R)F4L\nR5T)5YJ\nTDC)VHJ\nWHB)FVT\nTX8)7WX\nRF8)35N\nGD4)K2S\n1ZV)2ST\nT19)7FP\nD6M)CYW\n5XL)NK4\n5JC)JWY\nG9X)CCV\nZ1J)4C4\nPQZ)HD9\nVY5)6LJ\n3HX)459\nH37)S1V\nZ7P)G78\nYTV)YKQ\n1QN)ZLD\nBFD)THK\nF52)FCQ\nR44)XRT\nYH7)YYH\nDMC)R7Z\nR5M)D12\n1MJ)XSC\n85C)CVJ\n7V2)K43\nM82)1H7\n8XZ)5PP\nNJJ)Q17\nXS1)M82\nHJQ)P6K\nMY4)NKS\nL83)3NW\n7HM)9G6\n3NP)S2N\n175)927\n99V)ZJN\n23H)PZW\n1BK)R5M\nQSK)XH1\nHPH)Z3Y\nKVK)P5K\nYKQ)WT5\nG6P)MFG\nRYK)8JP\nF2Q)BRM\nGNT)XS1\nYHQ)8W4\nHTH)KQM\nJ72)JF2\nXXF)67Y\n8VD)VFG\nXL3)3HX\nLL8)36C\nFCK)KVN\nX64)3NN\n1HL)7Q3\nY5X)1MF\nT5K)YL3\nFSZ)QNL\nP6K)DZN\nB3P)KYB\n1X2)ZNZ\nTBN)V48\nBXM)1PB\n8SD)GP8\nSLB)7BY\n911)Q9X\nDJ2)QN6\n3NR)YJL\n8HB)SAN\nL3L)52S\nGZC)R1M\nSTY)H1C\nH8G)GSK\nDY6)L3L\nC27)4SH\n2MH)ZG7\n9SH)W8T\n23L)C7Q\nVK8)MGC\nY2C)9P8\nZNR)DLW\nRQS)G9X\nPTH)NFM\nXJ8)83C\nWW5)2R2\nGMD)LVC\nWBP)KSQ\nXLH)Y71\n14T)9ZB\nQQM)VZX\nMJ8)H6Z\nRMX)WBP\nZD1)3CB\nBQZ)MY4\nTWK)53X\nJK1)F7Q\n7SK)85C\nFS9)CTK\n3Q2)9D7\n4KM)QMV\nGK6)9FK\n192)VRR\nHJJ)YHQ\nN5L)HS2\nGNL)LXK\nKSV)DMH\n9FK)YFT\nK7B)KXZ\n6DS)SRV\nK5T)L1L\nPH6)BVN\nK43)9T5\nKWW)5FF\n118)CN1\n2HJ)XJD\nNYL)STY\nMMP)9F5\n94C)W3X\nCP9)NJZ\n124)1MN\n3DK)3RZ\n1WX)4ZN\nP7Q)F52\n8T5)2FV\nCSF)4KZ\nR14)R4C\nKLS)F4H\nG6Y)CLB\nVHJ)ZCQ\nCLB)MS4\nY2Y)NCY\nFCL)J9N\n7L4)LTM\nYMP)D5Y\nK4M)5LW\nC8B)3PF\n2P5)VPS\n1ZZ)GMD\nT5G)H6R\nHW9)S9J\nVBX)FNN\n894)3NR\nJZJ)ZR1\n598)333\nP5K)8TP\nLHM)B4Y\nYV1)48F\nQWC)NMQ\nV8X)LH5\n53K)6FK\n9T5)YTT\nT46)PQZ\n5RD)38X\nQ1B)R2C\n8LB)SYG\nMCG)9CD\nF9H)N6R\nMCB)36H\nR55)ZQG\nKLL)NWG\nN79)3NP\nS65)D6K\nL6R)2LX\nNP2)XHX\nMPZ)2HT\nJFZ)C8X\nHT7)9XL\nKXZ)Q84\n6WB)WMM\nN84)DTK\n3FL)JHF\nCY9)DSM\n1QW)1YM\nJLD)4CJ\n6SZ)B6J\nB2Q)F44\nD3M)HQH\nFFP)PS9\n57T)1CV\nBL7)7V2\n691)M7Z\nCC6)7PS\nJBS)HTH\nLRC)3Z1\n7WX)1P4\nRH9)4JD\nMJK)KZC\nJJD)691\nXV1)8DG\nG7S)X5S\n7LW)6BS\nKP7)D7W\nBKV)6MQ\nSBX)1JX\nMR4)Z1M\nL2Z)97T\n8QP)NPR\nC7V)9X7\nPWQ)TWK\nMQD)WV2\nLCN)4QK\n3KP)T46\n8J4)PYV\nBPL)D8P\n42C)V5D\n83C)C2W\nF4H)ZNR\nBYW)MP5\n61L)CQH\nRZB)T4S\nFMV)2P5\nJ6M)FYD\nHTV)J3P\nD3G)JLD\nWGR)J1M\nNXT)PV2\nTD9)3L1\nTYJ)8KG\nBRN)RDX\nLHJ)V8X\nQ11)FGY\nY6T)F47\nL55)42C\n94C)23L\nR3P)DM5\n172)QFK\nN3C)T5K\n2VB)57T\nT4S)PWQ\nLZ4)JLG\nTRQ)RL4\nJ9N)SCR\nT4P)LT4\n3NN)3X8\nFCQ)HCF\nLR7)D6M\n2R2)P7Q\n9XL)69G\nJVD)5XL\n953)RZB\nL9Z)YXK\nHCF)YYB\nL7P)X71\nL99)QM8\n9R3)VKY\nNHP)ZD8\nS2N)DHN\n66J)175\n9LX)K6N\nV48)Z46\n6H7)WR6\nJCD)RG6\n8DG)7QR\nD7L)HGP\nJXJ)53P\nYJS)F32\nF32)T4L\nTCF)XF3\n73N)CXF\nGKX)ZVW\nVT1)75D\nM7Z)BL7\n323)K7B\nQVZ)D3G\nZ4H)3CC\nR5M)HY2\nPYY)WXC\n6JW)PZR\n2QL)WYV\n4RX)XFS\n7KF)3WD\n1Z5)QHN\nYH7)8F1\n6JL)N41\n132)V1Q\nCMQ)118\nQ5J)JCD\nWH9)PKS\n7G1)LQM\nXLV)HJJ\nG8C)37T\nJML)8LB\nB3R)4RT\nRNQ)FTV\nXC5)SLB\nMSJ)VW7\nHHB)QCT\n46Z)PBN\nH1C)VT1\nJNL)1ZZ\nNN3)2PL\n4QK)JZJ\n8JP)QR7\nJB8)FFP\nL99)J72\n7PS)G2Q\n5T9)FTP\nWPM)MZT\n8RQ)G3H\n65G)2H7\nNXB)9DR\nK5T)F9N\nFNN)YTM\nG1K)XV1\n6MQ)5R2\n74Z)WCF\n2VV)3H9\n649)Q8V\nVZX)HT7\n11H)66J\nKTF)H8G\nKB5)FKV\nG7G)MZZ\nCTD)44K\n3VR)C92\nG2V)7YL\nVWK)172\n9S1)R49\n15X)24K\nVJM)6TQ\n1SP)XLH\nGQH)1X3\nNJZ)PYY\nYWG)BX3\nHQH)HJQ\nVQS)3B4\n6CW)X3D\nNT7)83N\n1YQ)JM3\n2XY)HDY\nTJV)92J\nJPX)PJD\nJ1M)9K2\nC92)VS7\nZWZ)NP5\n1JX)W5Q\nNMS)FD9\nFVY)R5W\nPS9)T8W\nV2V)BSM\nSD3)Y2C\n68N)88C\nRJR)VBX\nM2G)LL8\nCRV)NCH\n8LW)7MJ\n675)2BD\n9NT)BQ9\n2H7)T6J\nCBP)KNW\nML2)Y94\nKQM)HPH\n15K)N5F\nCS4)N95\nG3H)S3G\nWW2)34B\n8PD)H6C\nPZS)W99\nS9J)F2Q\nLT4)R6L\nPX4)Q7Z\n5PP)9GZ\nFXT)DTX\nR5W)64H\nHGP)BM3\nZG2)FZH\nMHV)W6G\nYYB)4L5\n24K)WL2\nY2L)TCP\nFRL)C84\nRWX)ZJM\nK87)LQS\nPQY)5KZ\nFKJ)STT\nX7P)FQG\nCM7)415\nHG6)9S8\nC2W)FS9\nG1X)J7B\nWM6)RCB\nPYV)LHM\nQN6)14T\nWT5)2MD\nRL4)H3S\nVTV)NB8\n8ZM)MD4\nXMY)NLX\nCXF)NHC\n8F1)G6Y\nYFY)3DK\nYJ4)LZ4\n9FK)CVH\nFT7)9HC\nZH5)1JZ\n3Z1)Q98\nGYG)971\nJLG)FQ9\nH6Z)8DT\nJ8R)NFG\nN6R)HQX\nFPT)97D\n6MT)FSZ\nNCY)LZ9\nW2R)CJ9\nLYK)PH6\nRCB)SM2\nSBL)C16\nX52)W44\nSDZ)2FG\nH7Q)TBN\nRV6)7TR\n43K)GQX\n2SM)4C9\nC6S)7G6\nNVF)9TZ\nM34)CM7\nWS6)R2D\nJHF)L83\nDZC)HTV\nRCD)3PN\n9NB)98D\nG1X)699\nVLZ)2MH\nMNZ)CCR\n787)3K3\nJCB)1BK\n6SV)D9F\nDP9)L99\nR4C)9KM\nT29)RYZ\nR1M)PYL\nT2B)RGB\nNK4)J2Y\nL1H)CRT\n3PF)6CQ\nY5S)F9B\n986)CP6\nC16)B5H\n9GB)S65\n333)VV6\nP18)KVX\nLXS)98L\nS5C)4XY\nPRZ)R44\n8W6)JR1\nB4Y)12G\n349)Q77\nD9F)KM2\nT3C)QMD\nN6B)9PQ\n9G6)7XJ\nD12)NVF\nSN7)D82\nH8V)SSP\nBPN)5YW\n3CB)KFG\nBTC)TVN\nQT3)2KK\nSXY)T19\nR2D)1JG\n2VS)BN1\nR6X)JK1\nKFG)G5G\nLZR)F9H\n5QY)FWK\nY2S)WW5\nYJF)SF8\n94V)YMP\nXGT)KVK\n57S)S2G\n4JD)CCP\n118)G8D\nTPF)FV1\nLTM)JNL\nNVF)B89\nF44)94C\nF4L)8HB\nMS4)192\nNHC)RF8\n5YW)8LW\nDNB)S5C\nRTT)YQ4\n3RR)DF9\nGYG)8H4\n2DJ)PYS\nXNG)1QJ\nD7C)1YN\n9CD)XBZ\n86C)KF1\nRSF)NVW\n4KZ)SFV\nYTM)5SS\nW3X)Y2Y\nX52)FKJ\nBR7)58V\nJNQ)DB9\nR3P)13H\nBDD)TC5\nQ83)LSM\nFCK)7RC\n1YN)19K\n2KK)MCB\nQBV)NMY\nJF2)6VF\nHQX)5Y6\nQWQ)MSJ\nZ35)MHV\nKF1)LR2\n3DB)LQ3\n127)X1B\nLQM)DBX\nZG7)W85\nGVL)M4Y\n482)MJK\nYFL)RTT\nZ46)MJD\nL9L)1W9\n8GQ)D2G\n44V)MTN\nXH1)5C3\nF3M)QBV\nF47)6P8\nYM7)6JV\n2R5)W2R\nN5F)5MM\n4NR)C6Z\nNKB)Y1C\nKSQ)PY2\nQFK)JXJ\n5S8)7D3\nC6T)FZB\nLZW)VJ7\nXMV)KWW\nVWK)CVF\nZCR)79Q\nX3D)G1K\nJ1K)H7Q\nK46)Y7T\nH3G)53K\nYCL)5Y3\nWHB)1YQ\nBDX)GYG\nMKW)RNQ\nKJL)HDV\n9SL)B3P\nT46)RV6\nKXG)M24\n34B)Q1B\nHJM)J8J\nC8X)ZCR\nPZR)65G\n9YP)JML\n3DK)GN9\n6PH)W75\n69G)4CS\nQ48)HHB\n1CV)XNV\nD8P)JCB\nNMP)LJZ\nPZW)33W\n6VD)SM1\nNZ7)HHQ\nXW8)9SJ\nCRT)MH9\nC8L)HQS\nLQ3)H8P\n9S8)ZZR\nBFG)LYS\nQ17)2R5\nWR6)5CR\n49C)ZG2\nCRT)DK6\n5JC)QH8\n5YJ)2VB\n5SS)GYR\n1W9)L8P\n35C)DJ2\n4CS)1HL\nGG4)VWK\n1MC)T5G\nZ5F)5MJ\nY8P)XNG\nQ98)Q83\nW99)VZC\nDB9)RQS\nL1L)F23\n8B4)LYK\nYV1)YD1\nVW7)XZD\nL28)6VD\nPY6)XCQ\nKVX)MCG\n8VY)2SM\nH6Y)X7P\nQ7Y)LVY\nM7Z)HWF\nPP2)PY6\nCTK)CF4\nBVN)PZS\n36P)6X9\nSSP)RY1\nGP8)YTV\n3NW)8ZK\nFWK)RKG\nCCB)HZQ\nMJX)WH9\nG92)JRM\n7XJ)H1R\n8H4)BLZ\n2HT)7NY\nTPZ)8SD\nV2V)WHX\nVSH)WM6\nLR2)649\nD9L)RVX\nW2H)HJM\nPV2)HFQ\n43Z)GFF\n1TD)FPT\nNHL)SHF\n8F9)8VD\nGFF)1FF\nY1C)RMX\n8KG)DY6\n6ZR)SD3\nLVC)NYT\nHQS)X9N\nQ1D)BYW\nYD3)LW6\nR3W)H5P\nFV1)XLV\nLYW)VD4\nDZN)GV8\n1N3)C3Z\nJCD)R5T\nX9N)8VL\nWV2)Q3R\n17T)124\n8C2)VB3\nB1V)SZN\nNMQ)GT4\nBRN)55R\nX1B)9SN\n5LW)Y2L\nSRJ)RSF\nCCV)PP2\nSJQ)LRC\nRDX)XWS\nQM8)8XZ\nX3P)MN6\nGDS)V2M\nX5S)4CZ\nTDX)XGT\nGV8)NJJ\nBRM)1MJ\n2ZY)L9L\nW6P)BRN\nQMD)FCK\nW28)8DZ\nHZ8)XYV\nB5H)YOU\nGL2)3HH\n2WD)VL4\nNYT)6ZR\nCSF)C8B\nV7N)8VN\n21R)N9C\nXC9)94P\nFD9)5BS\nJ3P)ZNH\nHZ8)V2V\nQYM)8PD\nVT7)9GB\nXHX)RM5\nYXK)43G\n53P)G92\nHY2)953\n1N3)TKC\nF2N)3S8\nFTP)2YT\nVS8)RB5\nHD9)JR6\nXRX)VJM\nMN9)LYW\nM24)6JW\nQD7)B5B\nG6B)K38\nS2G)8RQ\nBN1)29T\n43G)6PL\n4WV)H5W\nRCB)CHQ\nH6R)4TY\nLP3)33B\nCVH)C29\nC5J)YNK\n3Q2)H65\n1QJ)KLL\nVKG)L9Z\nD2G)36P\nHFT)NHP\nVS7)Z2T\n36C)X3P\n4BM)1DG\nGN9)ZMW\nRGB)48B\nK38)1N3\nYRB)T71\n4R7)BNL\nZ63)4R7\n5Q2)4C1\nV4D)3VR\nDTX)1F9\nR6C)3LL\n2FC)H6Y\nYYS)YJ4\n34B)XMY\n7NY)KC7\nLYS)XTB\nJR6)V4D\nM5P)L28\nCR3)PHC\nVFG)LCQ\nY3G)23H\nG5G)FXT\n2FV)NGD\nBQ9)QD7\n5FF)N84\nRB5)YRB\nQBL)YZ9\n7BY)K5T\nNWY)D6H\nYRK)MN9\nDM5)FT7\n1LX)QYP\nKM8)3FG\n92L)NMP\nQCK)4RB\nMLH)JJD\n5CY)Z5J\nV2T)D3M\n4C4)SG7\nJ7B)FFL\nVKY)WS6\nVT4)JNQ\nY4G)YNQ\nMBP)QCK\n4C1)CRV\nJBB)KTF\nGMD)2MQ\nNWY)1Z5\nBHQ)C27\nMZZ)R55\n9D7)74F\nSKG)3T9\n1H7)DQY\nF7Q)JVD\nWPM)R6C\n2QL)256\nBKF)7LW\n3H9)15X\nVXF)6X4\nPN3)3KP\nDK5)VQS\nV5D)ZD1\n4C3)1TD\nCHQ)CCB\n9HC)CQD\n97T)WHB\n953)1FC\n3PN)GG4\n323)1SP\nDF7)T3C\nZ46)G7G\nF5H)DNB\nJ2Y)XYN\nG9V)FKK\nHHQ)GSZ\nQ9X)ZR9\nQ5L)PN3\nSTT)BKV\n7F9)CK7\nJML)7D1\nCQH)BQZ\nH8P)HQT\nRMY)WZ9\nC92)QKY\n9DR)RND\nQPC)K87\n74F)HZ8\n4RX)R8C\nTWM)44V\n6TQ)L6R\n7MC)6YR\nDMC)73N\nZR9)NHL\n8YS)VMX\nNZ7)KK5\nWMM)VLZ\nSM1)YD3\nVV1)MQM\n7BX)VK8\n1QW)SRJ\nHWP)YM7\n48B)5NF\n3PY)1Z3\nJ55)9YP\nPKS)LS2\nNP4)SP5\nNWF)V2W\nDY7)DVK\nFRC)KM8\nQ77)6H7\n7Q3)15K\nKKR)NP2\n6KC)G7S\n4RB)YR1\nKC7)Y4F\nZGL)FD4\nT4B)QD6\nQ83)BFD\n5LW)C1T\nG8D)SMJ\n5Y3)Q1W\nQ7K)4BM\nSM3)LR7\nBX4)SKH\n12G)SPG\nDQ4)SJQ\n216)1D1\n1MN)J24\nSF8)DZ2\nFQG)YWG\nHST)R3P\nF3Z)3SY\n5BS)4H2\nQBB)Y2X\nW8T)48T\nM4P)6CW\nJMT)6GP\nKVN)5S4\nH65)8C2\nGVX)WMT\nWVY)N72\n1DG)L7W\nMGC)FRC\nMFD)XJ8\nRKG)BX4\nNKS)4FP\nQH1)2QL\n7YF)PVQ\n4C9)2VS\nQKY)R7C\n83C)KG4\nV4H)2DJ\nGQB)DF7\nR7Z)GNT\nKZC)JHG\n5SM)W16\n2NF)6JN\nSHK)94V\nX2N)216\nRVQ)X2N\nDJQ)NP4\n8VL)Z7P\nKWB)8T5\nHJJ)MPZ\nK2S)CP9\nNMQ)Z63\n7YL)21R\n6LJ)8GQ\n58V)VJL\n3K3)PQV\nV2M)Z4H\nSLF)5H8\nS8Y)132\nPQZ)9QW\nCLC)L7P\nK6N)Q11\nHZQ)K71\nSKH)XX2\nHFQ)Q5J\n2ST)WVY\nD82)BR7\nCS5)DY7\n9K2)Z1J\nJ24)PQY\nPVV)GVL\nHV5)P18\nT4L)GKN\n2FG)YJF\nDHN)FCL\nJM3)4RX\nSNB)DJQ\nC1T)QYM\n5MM)M4P\nN74)RJR\n6P8)XRX\nQYP)6MT\nZMW)7YF\n8TP)GQH\nXFS)9R3\nFQ9)9B1\nH87)Y8P\nMH9)8B4\n4CZ)NXB\nDHR)7F9\nGB3)T2B\n8SD)1QL\n44K)57S\n2LJ)FVY\nXYV)GK6\n3LL)TJV\n5S4)9D9\nBKW)HWP\nDJS)ZYY\n8DZ)G6B\n699)WW2\nXS1)WM3\n48T)YV1\n7RG)Z6B\nFKL)9SL\nJ8J)598\nDQY)1QW\nWR6)MJX\nK5N)5BV\nFZB)4C3\nPBY)ZP5\nD6H)Z35\nZ35)2NM\n6JN)787\n38X)NMS\nV1Q)5YK\nW6G)XC5\nN5F)V4H\nZR8)M5F\nL7W)DLD\nDWH)7WZ\n9QW)Y3G\nQRM)MBP\nTD9)QVZ\nRTT)43Z\nH6T)FNV\n1X3)VHW\nN41)SBX\nMN6)MVL\nXF3)RK5\n8WB)2S4\nV8V)2BT\nT19)8J4\nF11)13C\n2PL)GVX\nGL2)JB8\n54J)ZLS\n2XS)V7N\nG1Z)SHK\nGKH)RYK\nYZ9)YH7\n7TR)VC9\n8HH)LFL")
    sut.countOrbits() shouldBe 204521
  }

  "The second part" should "work for the first examples" in {
    val sut = Day6("COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN")
    sut.findOrbitsBetweenPlanets("YOU", "SAN") shouldBe 4
  }

  it should "work for the input" in {
    val sut = Day6("H1R)Z5F\nR6L)JYJ\nQVZ)B3R\nZD8)Y6T\nFWC)F2N\n3HH)HSX\n2PQ)NXT\nC7Q)7HM\n6KQ)B1V\nCVJ)SXY\nZZR)627\nHWF)5Q2\n7QR)BHQ\n1FF)SBD\nF9B)XXF\n415)C8L\nK38)LZ2\nQ3R)61L\nQ8V)TDC\nH5W)52V\nPQY)BDX\nC54)QSK\nPHC)JPX\nZLS)CRN\nPSR)T5C\nFWK)Q48\nBNL)FWC\nZPV)CR3\nHM5)C5J\nY4F)X6L\nFX1)1LX\nD7W)P4X\n5Y6)L55\nL2R)2WD\nF26)TPF\nYYH)F3Z\nXQB)GPY\n8W7)6PH\n53X)K5N\nMFG)KP7\nYTT)DJS\nNDT)NN3\nS7X)8HH\n5CR)BS6\nQH8)MKW\n8FY)LG7\nSC1)CC6\n8DX)N55\nT71)M2G\nSBD)T4B\n9B1)K4M\nKXB)YRK\nH3S)DP9\nY71)MW5\n43G)9NB\nYFT)BFG\nGSZ)H37\nQP8)VT7\n3CC)KJL\n8VN)Y5S\nCP6)GQB\nBX3)DQ4\nP4X)XJT\nZMK)7WW\nRVX)J55\nS9X)7MC\nZYY)T4P\nX6L)126\nT1W)H6H\nBS6)S7X\nYNK)KKR\n2BD)SM3\nPR8)MJ8\nL2H)2PQ\nKM2)1WX\nV8F)FX1\n92J)NWF\nCF4)XMV\n7G6)KXB\nZHP)K75\n9T5)Q7Y\nX71)HV5\nKK5)1X2\nJCB)DWH\nT8W)S9X\n2S6)J6M\n53K)H87\nF3J)JLL\n5Y6)VY5\nCQD)2ZY\n8W4)PX4\n4H2)KR4\nGWY)SNB\nF8J)5T9\nS3G)54J\nPQV)PKY\n96C)L67\nCP6)R66\nH5P)6SZ\nP9V)686\nCP8)S8Y\n6CQ)QRM\nPZK)DVV\nDK5)QWQ\nBPM)52D\nV8R)YYS\nCM7)2NF\nH8P)F11\nTC5)KXG\n7QK)6XG\nD9F)NZ7\n1D5)86C\nZP5)C54\nC6T)Q7K\n9KM)QPC\nLBC)HFX\nDK6)LZW\nWXC)2FC\nBX4)2XY\n6H7)2HJ\nDZ2)5RD\nKYB)H6T\n2QW)PVV\n2MD)BNP\nVMX)SLG\nD6K)RXB\nGHS)HGB\nQCT)T29\nX1B)VT4\nBN9)M79\nSXK)CSF\nSG7)YPY\nZPK)GHS\n6X4)L59\nB89)LP3\n9TZ)HW9\nYPY)M5P\nLS2)92L\nZ2T)QQM\nCK7)NKB\n3PY)VS8\n2MQ)NT7\nC6Z)8F9\nNWF)35C\nGYR)4NR\nZQG)W9H\n7D1)V8R\nY2X)F26\n8X8)7KF\nCN1)PZC\nBPW)PBY\n9WC)H8V\nZXH)GKH\nCF4)N5L\nC54)YJS\nRY1)GZC\nWMT)8CW\nMZP)9SH\nVZC)SKL\nY7T)C7V\nSYG)R14\nZLS)MLH\nMZT)G1Z\nNPR)VTV\nLCQ)PZK\nRNW)SXK\n8CW)8YN\nXXM)MFD\nYL3)8WB\nDTK)JFZ\nTKC)J8R\nMVL)TYJ\nQR7)SKG\n9GZ)BXM\nM4Y)P9V\n4XY)8QP\n9XC)X52\nCVF)PTH\n52S)D9L\nTCP)SBL\nSPG)KSV\nMTN)GWY\nVJL)N74\n4RT)LHJ\nRG6)5X3\nQ7Z)CP8\n5X3)3PY\nPVQ)TCF\nJR1)7QK\nDGV)YFL\nBQ9)B2Q\nW75)6KQ\n13H)QYX\nHQT)G2V\nHDV)7RG\nGV3)HFT\n627)Q5F\nYYS)RVQ\nCQH)BPW\nW3X)VXF\nSZN)SLF\nC7Q)8X8\n3SY)X64\nJG5)C6S\n4C3)PSR\nQ1W)RNW\nT6J)NPL\nT4L)D7L\nL8P)27X\nR66)TRQ\nPKY)YCL\nGQX)JVG\n2NM)RWX\nMP5)MR4\n5R2)GRN\nJRM)M34\nTHK)RPR\nGKN)46X\n1D1)8YS\nXYN)6SV\nSRV)GB3\nF9N)8FY\nSLG)72G\nJ2Y)W6P\n3RZ)K2W\nW85)CY9\nMJ8)2FT\n9B5)XW8\nPJD)6WB\nDBX)V2T\nM6X)VBM\nXX2)BTC\n1MN)XXM\nDLD)3Q2\n4SH)BPL\nT8Y)ZPV\n3T9)HMW\nC3Z)GDS\nYJ4)L2Z\nVJ7)Q7H\nD72)K1T\n5ZP)QP8\nXZD)894\n67Y)BKF\nZLD)YST\nNQ1)F5H\n2QB)M99\nWZ9)XC9\nCOM)RR1\nF3J)G1X\nSCR)L66\n46X)QT3\n4ZN)JDX\nJYJ)2QB\nXTB)349\nLM1)83X\n29M)L2H\nD5Y)VV1\nYTK)BKW\nN72)GD4\n1PB)8ZM\nV31)99V\nRR1)68N\nBM3)99C\nXRT)VM9\n6FK)675\n5P9)CMQ\nDSM)SJ5\nYST)KB5\n83N)6VW\nZCQ)5SM\nV1Q)2S6\nLZR)QBL\nH3G)9S1\n88G)YTK\nNPL)ZHP\nW44)NWY\n5KZ)1MC\n52V)9B5\nWL2)5P9\nJDX)V8F\nR16)ZWZ\nFVT)596\n8J4)FRL\n2HC)986\nNFM)KWB\nF2L)Y5X\nVPS)43K\nVX8)G8C\nDMH)RCD\nNCH)TPZ\n9KG)MMP\nRPR)CFW\nJWY)SC1\nXTB)H3G\n7FP)FMV\nHGB)6JL\n1P4)ZPK\nWCF)11H\nZR1)Y75\nNP5)MNZ\nGNT)8W7\nNGD)GNL\nQ98)F6V\nF32)7L4\nCFW)684\nT5C)QWC\nVV6)SDZ\n9QW)VKG\n7WZ)R16\nLZ2)NYL\nG2V)ZXH\nVL4)9LX\n9F5)CS5\nFYD)VX8\nR7C)2HC\nLQS)54R\n33W)F8J\nN9C)ZGL\n9ZB)5S8\nMLJ)8VY\nLVY)T1W\n1FC)JG5\nK75)LZR\nLZ9)2XS\n9P8)75M\nLW6)TWM\nK71)DGV\nR2C)GKX\n99C)ML2\nCM5)YKW\n5YK)HM5\n9D9)G9V\n2BT)74Z\nTCP)NDT\nL59)N3C\n3FG)ZH5\n3S8)96C\nJBB)9KG\nQHN)CS4\n9TZ)PRZ\nSM2)MHJ\nZ5J)5QY\nBNP)7HZ\nHDY)LBC\nQMV)7SK\nGT4)SN7\n1MC)MZP\n5C3)9WC\n596)L1H\nZNZ)CM5\nPBN)KLS\n2K7)BN9\nYJL)2VV\nMJX)TDX\nK2W)5ZP\n1F9)6KC\nBM8)D72\n79Q)ZTP\n7RC)88G\n7WW)JBB\n43Z)7G1\nFKV)3W9\nT1W)911\n5RD)D7C\nXBZ)NQ1\nC8L)F3M\nM79)YFY\nG2Q)LXS\nF6V)T8Y\n6XG)127\nCCV)2LJ\nD9T)Q5L\nZTP)XL3\nLG7)X1Q\nJHG)WBL\n8F9)3DB\nFNV)W7B\nHS2)PR8\n6PL)WPM\nHQH)9NT\nWV2)3FL\n1YM)Z51\nFCL)4KM\nVBM)8DX\nJXJ)MQD\nH6C)K46\n72G)9XC\nL66)Y2S\nHMW)XPP\n29T)LM1\nBPL)F3J\nVTV)TX8\n9WW)L2R\nZ51)GL2\nYD1)DZC\n4TY)BPM\nC84)GV3\nSFV)1ZV\nXJT)17T\nRK5)ZR8\n71K)DK5\nLTM)71K\nQNL)XQB\nWYV)N6B\nJDX)5JC\n6X9)CMF\nTRQ)RMY\n2S6)29M\n4CJ)CTD\nKG4)7BX\nJLL)LCN\nQYX)DMC\n1P4)C6T\nZVW)RKM\nXCQ)V31\nWM3)G6P\nHFX)TD9\n7RC)Y4G\nM5F)4WV\n6GP)2K7\nPYL)5XS\n88C)JMT\nVB3)Q1D\nRQS)JBS\nSMJ)VSH\n2WM)9RK\n9SJ)1D5\nSP5)MLJ\n9RK)2QW\n1JZ)R6X\nN4S)WGR\nXWS)J1K\nFGY)N79\nLM1)6DS\nNVW)323\nLSM)46Z\n97D)M6X\n256)V8V\nRCD)DHR\n3B4)3RR\nR49)8W6\n6BS)49C\nC29)BM8\nNFG)9WW\n9HC)F2L\nDVV)N4S\nCCP)R3W\nDF9)ZN9\nW16)BDD\nRND)CLC\nCMF)HST\nX1Q)C52\nMS4)5CY\nBSM)W2H\n971)482\n6JV)CBP\nNB8)HG6\nLQM)ZMK\n64H)DQJ\n81H)2WM\nP18)1QN\n1Z3)RH9\nQ84)QH1\nSJ5)QBB\n83X)W28\nXF3)D9T\nL7W)BPN\nPH6)81H\nQ5F)4GJ\n8DT)FKL\n55R)F4L\nR5T)5YJ\nTDC)VHJ\nWHB)FVT\nTX8)7WX\nRF8)35N\nGD4)K2S\n1ZV)2ST\nT19)7FP\nD6M)CYW\n5XL)NK4\n5JC)JWY\nG9X)CCV\nZ1J)4C4\nPQZ)HD9\nVY5)6LJ\n3HX)459\nH37)S1V\nZ7P)G78\nYTV)YKQ\n1QN)ZLD\nBFD)THK\nF52)FCQ\nR44)XRT\nYH7)YYH\nDMC)R7Z\nR5M)D12\n1MJ)XSC\n85C)CVJ\n7V2)K43\nM82)1H7\n8XZ)5PP\nNJJ)Q17\nXS1)M82\nHJQ)P6K\nMY4)NKS\nL83)3NW\n7HM)9G6\n3NP)S2N\n175)927\n99V)ZJN\n23H)PZW\n1BK)R5M\nQSK)XH1\nHPH)Z3Y\nKVK)P5K\nYKQ)WT5\nG6P)MFG\nRYK)8JP\nF2Q)BRM\nGNT)XS1\nYHQ)8W4\nHTH)KQM\nJ72)JF2\nXXF)67Y\n8VD)VFG\nXL3)3HX\nLL8)36C\nFCK)KVN\nX64)3NN\n1HL)7Q3\nY5X)1MF\nT5K)YL3\nFSZ)QNL\nP6K)DZN\nB3P)KYB\n1X2)ZNZ\nTBN)V48\nBXM)1PB\n8SD)GP8\nSLB)7BY\n911)Q9X\nDJ2)QN6\n3NR)YJL\n8HB)SAN\nL3L)52S\nGZC)R1M\nSTY)H1C\nH8G)GSK\nDY6)L3L\nC27)4SH\n2MH)ZG7\n9SH)W8T\n23L)C7Q\nVK8)MGC\nY2C)9P8\nZNR)DLW\nRQS)G9X\nPTH)NFM\nXJ8)83C\nWW5)2R2\nGMD)LVC\nWBP)KSQ\nXLH)Y71\n14T)9ZB\nQQM)VZX\nMJ8)H6Z\nRMX)WBP\nZD1)3CB\nBQZ)MY4\nTWK)53X\nJK1)F7Q\n7SK)85C\nFS9)CTK\n3Q2)9D7\n4KM)QMV\nGK6)9FK\n192)VRR\nHJJ)YHQ\nN5L)HS2\nGNL)LXK\nKSV)DMH\n9FK)YFT\nK7B)KXZ\n6DS)SRV\nK5T)L1L\nPH6)BVN\nK43)9T5\nKWW)5FF\n118)CN1\n2HJ)XJD\nNYL)STY\nMMP)9F5\n94C)W3X\nCP9)NJZ\n124)1MN\n3DK)3RZ\n1WX)4ZN\nP7Q)F52\n8T5)2FV\nCSF)4KZ\nR14)R4C\nKLS)F4H\nG6Y)CLB\nVHJ)ZCQ\nCLB)MS4\nY2Y)NCY\nFCL)J9N\n7L4)LTM\nYMP)D5Y\nK4M)5LW\nC8B)3PF\n2P5)VPS\n1ZZ)GMD\nT5G)H6R\nHW9)S9J\nVBX)FNN\n894)3NR\nJZJ)ZR1\n598)333\nP5K)8TP\nLHM)B4Y\nYV1)48F\nQWC)NMQ\nV8X)LH5\n53K)6FK\n9T5)YTT\nT46)PQZ\n5RD)38X\nQ1B)R2C\n8LB)SYG\nMCG)9CD\nF9H)N6R\nMCB)36H\nR55)ZQG\nKLL)NWG\nN79)3NP\nS65)D6K\nL6R)2LX\nNP2)XHX\nMPZ)2HT\nJFZ)C8X\nHT7)9XL\nKXZ)Q84\n6WB)WMM\nN84)DTK\n3FL)JHF\nCY9)DSM\n1QW)1YM\nJLD)4CJ\n6SZ)B6J\nB2Q)F44\nD3M)HQH\nFFP)PS9\n57T)1CV\nBL7)7V2\n691)M7Z\nCC6)7PS\nJBS)HTH\nLRC)3Z1\n7WX)1P4\nRH9)4JD\nMJK)KZC\nJJD)691\nXV1)8DG\nG7S)X5S\n7LW)6BS\nKP7)D7W\nBKV)6MQ\nSBX)1JX\nMR4)Z1M\nL2Z)97T\n8QP)NPR\nC7V)9X7\nPWQ)TWK\nMQD)WV2\nLCN)4QK\n3KP)T46\n8J4)PYV\nBPL)D8P\n42C)V5D\n83C)C2W\nF4H)ZNR\nBYW)MP5\n61L)CQH\nRZB)T4S\nFMV)2P5\nJ6M)FYD\nHTV)J3P\nD3G)JLD\nWGR)J1M\nNXT)PV2\nTD9)3L1\nTYJ)8KG\nBRN)RDX\nLHJ)V8X\nQ11)FGY\nY6T)F47\nL55)42C\n94C)23L\nR3P)DM5\n172)QFK\nN3C)T5K\n2VB)57T\nT4S)PWQ\nLZ4)JLG\nTRQ)RL4\nJ9N)SCR\nT4P)LT4\n3NN)3X8\nFCQ)HCF\nLR7)D6M\n2R2)P7Q\n9XL)69G\nJVD)5XL\n953)RZB\nL9Z)YXK\nHCF)YYB\nL7P)X71\nL99)QM8\n9R3)VKY\nNHP)ZD8\nS2N)DHN\n66J)175\n9LX)K6N\nV48)Z46\n6H7)WR6\nJCD)RG6\n8DG)7QR\nD7L)HGP\nJXJ)53P\nYJS)F32\nF32)T4L\nTCF)XF3\n73N)CXF\nGKX)ZVW\nVT1)75D\nM7Z)BL7\n323)K7B\nQVZ)D3G\nZ4H)3CC\nR5M)HY2\nPYY)WXC\n6JW)PZR\n2QL)WYV\n4RX)XFS\n7KF)3WD\n1Z5)QHN\nYH7)8F1\n6JL)N41\n132)V1Q\nCMQ)118\nQ5J)JCD\nWH9)PKS\n7G1)LQM\nXLV)HJJ\nG8C)37T\nJML)8LB\nB3R)4RT\nRNQ)FTV\nXC5)SLB\nMSJ)VW7\nHHB)QCT\n46Z)PBN\nH1C)VT1\nJNL)1ZZ\nNN3)2PL\n4QK)JZJ\n8JP)QR7\nJB8)FFP\nL99)J72\n7PS)G2Q\n5T9)FTP\nWPM)MZT\n8RQ)G3H\n65G)2H7\nNXB)9DR\nK5T)F9N\nFNN)YTM\nG1K)XV1\n6MQ)5R2\n74Z)WCF\n2VV)3H9\n649)Q8V\nVZX)HT7\n11H)66J\nKTF)H8G\nKB5)FKV\nG7G)MZZ\nCTD)44K\n3VR)C92\nG2V)7YL\nVWK)172\n9S1)R49\n15X)24K\nVJM)6TQ\n1SP)XLH\nGQH)1X3\nNJZ)PYY\nYWG)BX3\nHQH)HJQ\nVQS)3B4\n6CW)X3D\nNT7)83N\n1YQ)JM3\n2XY)HDY\nTJV)92J\nJPX)PJD\nJ1M)9K2\nC92)VS7\nZWZ)NP5\n1JX)W5Q\nNMS)FD9\nFVY)R5W\nPS9)T8W\nV2V)BSM\nSD3)Y2C\n68N)88C\nRJR)VBX\nM2G)LL8\nCRV)NCH\n8LW)7MJ\n675)2BD\n9NT)BQ9\n2H7)T6J\nCBP)KNW\nML2)Y94\nKQM)HPH\n15K)N5F\nCS4)N95\nG3H)S3G\nWW2)34B\n8PD)H6C\nPZS)W99\nS9J)F2Q\nLT4)R6L\nPX4)Q7Z\n5PP)9GZ\nFXT)DTX\nR5W)64H\nHGP)BM3\nZG2)FZH\nMHV)W6G\nYYB)4L5\n24K)WL2\nY2L)TCP\nFRL)C84\nRWX)ZJM\nK87)LQS\nPQY)5KZ\nFKJ)STT\nX7P)FQG\nCM7)415\nHG6)9S8\nC2W)FS9\nG1X)J7B\nWM6)RCB\nPYV)LHM\nQN6)14T\nWT5)2MD\nRL4)H3S\nVTV)NB8\n8ZM)MD4\nXMY)NLX\nCXF)NHC\n8F1)G6Y\nYFY)3DK\nYJ4)LZ4\n9FK)CVH\nFT7)9HC\nZH5)1JZ\n3Z1)Q98\nGYG)971\nJLG)FQ9\nH6Z)8DT\nJ8R)NFG\nN6R)HQX\nFPT)97D\n6MT)FSZ\nNCY)LZ9\nW2R)CJ9\nLYK)PH6\nRCB)SM2\nSBL)C16\nX52)W44\nSDZ)2FG\nH7Q)TBN\nRV6)7TR\n43K)GQX\n2SM)4C9\nC6S)7G6\nNVF)9TZ\nM34)CM7\nWS6)R2D\nJHF)L83\nDZC)HTV\nRCD)3PN\n9NB)98D\nG1X)699\nVLZ)2MH\nMNZ)CCR\n787)3K3\nJCB)1BK\n6SV)D9F\nDP9)L99\nR4C)9KM\nT29)RYZ\nR1M)PYL\nT2B)RGB\nNK4)J2Y\nL1H)CRT\n3PF)6CQ\nY5S)F9B\n986)CP6\nC16)B5H\n9GB)S65\n333)VV6\nP18)KVX\nLXS)98L\nS5C)4XY\nPRZ)R44\n8W6)JR1\nB4Y)12G\n349)Q77\nD9F)KM2\nT3C)QMD\nN6B)9PQ\n9G6)7XJ\nD12)NVF\nSN7)D82\nH8V)SSP\nBPN)5YW\n3CB)KFG\nBTC)TVN\nQT3)2KK\nSXY)T19\nR2D)1JG\n2VS)BN1\nR6X)JK1\nKFG)G5G\nLZR)F9H\n5QY)FWK\nY2S)WW5\nYJF)SF8\n94V)YMP\nXGT)KVK\n57S)S2G\n4JD)CCP\n118)G8D\nTPF)FV1\nLTM)JNL\nNVF)B89\nF44)94C\nF4L)8HB\nMS4)192\nNHC)RF8\n5YW)8LW\nDNB)S5C\nRTT)YQ4\n3RR)DF9\nGYG)8H4\n2DJ)PYS\nXNG)1QJ\nD7C)1YN\n9CD)XBZ\n86C)KF1\nRSF)NVW\n4KZ)SFV\nYTM)5SS\nW3X)Y2Y\nX52)FKJ\nBR7)58V\nJNQ)DB9\nR3P)13H\nBDD)TC5\nQ83)LSM\nFCK)7RC\n1YN)19K\n2KK)MCB\nQBV)NMY\nJF2)6VF\nHQX)5Y6\nQWQ)MSJ\nZ35)MHV\nKF1)LR2\n3DB)LQ3\n127)X1B\nLQM)DBX\nZG7)W85\nGVL)M4Y\n482)MJK\nYFL)RTT\nZ46)MJD\nL9L)1W9\n8GQ)D2G\n44V)MTN\nXH1)5C3\nF3M)QBV\nF47)6P8\nYM7)6JV\n2R5)W2R\nN5F)5MM\n4NR)C6Z\nNKB)Y1C\nKSQ)PY2\nQFK)JXJ\n5S8)7D3\nC6T)FZB\nLZW)VJ7\nXMV)KWW\nVWK)CVF\nZCR)79Q\nX3D)G1K\nJ1K)H7Q\nK46)Y7T\nH3G)53K\nYCL)5Y3\nWHB)1YQ\nBDX)GYG\nMKW)RNQ\nKJL)HDV\n9SL)B3P\nT46)RV6\nKXG)M24\n34B)Q1B\nHJM)J8J\nC8X)ZCR\nPZR)65G\n9YP)JML\n3DK)GN9\n6PH)W75\n69G)4CS\nQ48)HHB\n1CV)XNV\nD8P)JCB\nNMP)LJZ\nPZW)33W\n6VD)SM1\nNZ7)HHQ\nXW8)9SJ\nCRT)MH9\nC8L)HQS\nLQ3)H8P\n9S8)ZZR\nBFG)LYS\nQ17)2R5\nWR6)5CR\n49C)ZG2\nCRT)DK6\n5JC)QH8\n5YJ)2VB\n5SS)GYR\n1W9)L8P\n35C)DJ2\n4CS)1HL\nGG4)VWK\n1MC)T5G\nZ5F)5MJ\nY8P)XNG\nQ98)Q83\nW99)VZC\nDB9)RQS\nL1L)F23\n8B4)LYK\nYV1)YD1\nVW7)XZD\nL28)6VD\nPY6)XCQ\nKVX)MCG\n8VY)2SM\nH6Y)X7P\nQ7Y)LVY\nM7Z)HWF\nPP2)PY6\nCTK)CF4\nBVN)PZS\n36P)6X9\nSSP)RY1\nGP8)YTV\n3NW)8ZK\nFWK)RKG\nCCB)HZQ\nMJX)WH9\nG92)JRM\n7XJ)H1R\n8H4)BLZ\n2HT)7NY\nTPZ)8SD\nV2V)WHX\nVSH)WM6\nLR2)649\nD9L)RVX\nW2H)HJM\nPV2)HFQ\n43Z)GFF\n1TD)FPT\nNHL)SHF\n8F9)8VD\nGFF)1FF\nY1C)RMX\n8KG)DY6\n6ZR)SD3\nLVC)NYT\nHQS)X9N\nQ1D)BYW\nYD3)LW6\nR3W)H5P\nFV1)XLV\nLYW)VD4\nDZN)GV8\n1N3)C3Z\nJCD)R5T\nX9N)8VL\nWV2)Q3R\n17T)124\n8C2)VB3\nB1V)SZN\nNMQ)GT4\nBRN)55R\nX1B)9SN\n5LW)Y2L\nSRJ)RSF\nCCV)PP2\nSJQ)LRC\nRDX)XWS\nQM8)8XZ\nX3P)MN6\nGDS)V2M\nX5S)4CZ\nTDX)XGT\nGV8)NJJ\nBRM)1MJ\n2ZY)L9L\nW6P)BRN\nQMD)FCK\nW28)8DZ\nHZ8)XYV\nB5H)YOU\nGL2)3HH\n2WD)VL4\nNYT)6ZR\nCSF)C8B\nV7N)8VN\n21R)N9C\nXC9)94P\nFD9)5BS\nJ3P)ZNH\nHZ8)V2V\nQYM)8PD\nVT7)9GB\nXHX)RM5\nYXK)43G\n53P)G92\nHY2)953\n1N3)TKC\nF2N)3S8\nFTP)2YT\nVS8)RB5\nHD9)JR6\nXRX)VJM\nMN9)LYW\nM24)6JW\nQD7)B5B\nG6B)K38\nS2G)8RQ\nBN1)29T\n43G)6PL\n4WV)H5W\nRCB)CHQ\nH6R)4TY\nLP3)33B\nCVH)C29\nC5J)YNK\n3Q2)H65\n1QJ)KLL\nVKG)L9Z\nD2G)36P\nHFT)NHP\nVS7)Z2T\n36C)X3P\n4BM)1DG\nGN9)ZMW\nRGB)48B\nK38)1N3\nYRB)T71\n4R7)BNL\nZ63)4R7\n5Q2)4C1\nV4D)3VR\nDTX)1F9\nR6C)3LL\n2FC)H6Y\nYYS)YJ4\n34B)XMY\n7NY)KC7\nLYS)XTB\nJR6)V4D\nM5P)L28\nCR3)PHC\nVFG)LCQ\nY3G)23H\nG5G)FXT\n2FV)NGD\nBQ9)QD7\n5FF)N84\nRB5)YRB\nQBL)YZ9\n7BY)K5T\nNWY)D6H\nYRK)MN9\nDM5)FT7\n1LX)QYP\nKM8)3FG\n92L)NMP\nQCK)4RB\nMLH)JJD\n5CY)Z5J\nV2T)D3M\n4C4)SG7\nJ7B)FFL\nVKY)WS6\nVT4)JNQ\nY4G)YNQ\nMBP)QCK\n4C1)CRV\nJBB)KTF\nGMD)2MQ\nNWY)1Z5\nBHQ)C27\nMZZ)R55\n9D7)74F\nSKG)3T9\n1H7)DQY\nF7Q)JVD\nWPM)R6C\n2QL)256\nBKF)7LW\n3H9)15X\nVXF)6X4\nPN3)3KP\nDK5)VQS\nV5D)ZD1\n4C3)1TD\nCHQ)CCB\n9HC)CQD\n97T)WHB\n953)1FC\n3PN)GG4\n323)1SP\nDF7)T3C\nZ46)G7G\nF5H)DNB\nJ2Y)XYN\nG9V)FKK\nHHQ)GSZ\nQ9X)ZR9\nQ5L)PN3\nSTT)BKV\n7F9)CK7\nJML)7D1\nCQH)BQZ\nH8P)HQT\nRMY)WZ9\nC92)QKY\n9DR)RND\nQPC)K87\n74F)HZ8\n4RX)R8C\nTWM)44V\n6TQ)L6R\n7MC)6YR\nDMC)73N\nZR9)NHL\n8YS)VMX\nNZ7)KK5\nWMM)VLZ\nSM1)YD3\nVV1)MQM\n7BX)VK8\n1QW)SRJ\nHWP)YM7\n48B)5NF\n3PY)1Z3\nJ55)9YP\nPKS)LS2\nNP4)SP5\nNWF)V2W\nDY7)DVK\nFRC)KM8\nQ77)6H7\n7Q3)15K\nKKR)NP2\n6KC)G7S\n4RB)YR1\nKC7)Y4F\nZGL)FD4\nT4B)QD6\nQ83)BFD\n5LW)C1T\nG8D)SMJ\n5Y3)Q1W\nQ7K)4BM\nSM3)LR7\nBX4)SKH\n12G)SPG\nDQ4)SJQ\n216)1D1\n1MN)J24\nSF8)DZ2\nFQG)YWG\nHST)R3P\nF3Z)3SY\n5BS)4H2\nQBB)Y2X\nW8T)48T\nM4P)6CW\nJMT)6GP\nKVN)5S4\nH65)8C2\nGVX)WMT\nWVY)N72\n1DG)L7W\nMGC)FRC\nMFD)XJ8\nRKG)BX4\nNKS)4FP\nQH1)2QL\n7YF)PVQ\n4C9)2VS\nQKY)R7C\n83C)KG4\nV4H)2DJ\nGQB)DF7\nR7Z)GNT\nKZC)JHG\n5SM)W16\n2NF)6JN\nSHK)94V\nX2N)216\nRVQ)X2N\nDJQ)NP4\n8VL)Z7P\nKWB)8T5\nHJJ)MPZ\nK2S)CP9\nNMQ)Z63\n7YL)21R\n6LJ)8GQ\n58V)VJL\n3K3)PQV\nV2M)Z4H\nSLF)5H8\nS8Y)132\nPQZ)9QW\nCLC)L7P\nK6N)Q11\nHZQ)K71\nSKH)XX2\nHFQ)Q5J\n2ST)WVY\nD82)BR7\nCS5)DY7\n9K2)Z1J\nJ24)PQY\nPVV)GVL\nHV5)P18\nT4L)GKN\n2FG)YJF\nDHN)FCL\nJM3)4RX\nSNB)DJQ\nC1T)QYM\n5MM)M4P\nN74)RJR\n6P8)XRX\nQYP)6MT\nZMW)7YF\n8TP)GQH\nXFS)9R3\nFQ9)9B1\nH87)Y8P\nMH9)8B4\n4CZ)NXB\nDHR)7F9\nGB3)T2B\n8SD)1QL\n44K)57S\n2LJ)FVY\nXYV)GK6\n3LL)TJV\n5S4)9D9\nBKW)HWP\nDJS)ZYY\n8DZ)G6B\n699)WW2\nXS1)WM3\n48T)YV1\n7RG)Z6B\nFKL)9SL\nJ8J)598\nDQY)1QW\nWR6)MJX\nK5N)5BV\nFZB)4C3\nPBY)ZP5\nD6H)Z35\nZ35)2NM\n6JN)787\n38X)NMS\nV1Q)5YK\nW6G)XC5\nN5F)V4H\nZR8)M5F\nL7W)DLD\nDWH)7WZ\n9QW)Y3G\nQRM)MBP\nTD9)QVZ\nRTT)43Z\nH6T)FNV\n1X3)VHW\nN41)SBX\nMN6)MVL\nXF3)RK5\n8WB)2S4\nV8V)2BT\nT19)8J4\nF11)13C\n2PL)GVX\nGL2)JB8\n54J)ZLS\n2XS)V7N\nG1Z)SHK\nGKH)RYK\nYZ9)YH7\n7TR)VC9\n8HH)LFL")
    sut.findOrbitsBetweenPlanets("YOU", "SAN") shouldBe 307
  }
}
