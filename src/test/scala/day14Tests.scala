import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class day14Tests extends FlatSpec with Matchers {

  "The first part" should "work for the first example" in {
    val system = Day14("10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL")
    system.calculateRequiredOre(ChemicalElements(mutable.Map("FUEL" -> 1))).requiredChemicals.head._2 shouldBe 31
  }

  it should "work for the second example" in {
    val system = Day14("9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL")
    system.calculateRequiredOre(ChemicalElements(mutable.Map("FUEL" -> 1))).requiredChemicals.head._2 shouldBe 165
  }

  it should "work for the third example" in {
    val system = Day14("157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")
    system.calculateRequiredOre(ChemicalElements(mutable.Map("FUEL" -> 1))).requiredChemicals.head._2 shouldBe 13312
  }

  it should "work for the fourth example" in {
    val system = Day14("2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\n17 NVRVD, 3 JNWZP => 8 VPVL\n53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\n22 VJHF, 37 MNCFX => 5 FWMGM\n139 ORE => 4 NVRVD\n144 ORE => 7 JNWZP\n5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\n5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\n145 ORE => 6 MNCFX\n1 NVRVD => 8 CXFTF\n1 VJHF, 6 MNCFX => 4 RFSQX\n176 ORE => 6 VJHF")
    system.calculateRequiredOre(ChemicalElements(mutable.Map("FUEL" -> 1))).requiredChemicals.head._2 shouldBe 180697
  }

  it should "work for the fifth example" in {
    val system = Day14("171 ORE => 8 CNZTR\n7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n114 ORE => 4 BHXH\n14 VRPVC => 6 BMBT\n6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n5 BMBT => 4 WPTQ\n189 ORE => 9 KTJDG\n1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n12 VRPVC, 27 CNZTR => 2 XDBXC\n15 KTJDG, 12 BHXH => 5 XCVML\n3 BHXH, 2 VRPVC => 7 MZWV\n121 ORE => 7 VRPVC\n7 XCVML => 6 RJRHP\n5 BHXH, 4 VRPVC => 5 LTCX")
    system.calculateRequiredOre(ChemicalElements(mutable.Map("FUEL" -> 1))).requiredChemicals.head._2 shouldBe 2210736
  }

  it should "work for the my own example" in {
    val system = Day14("2 ORE => 4 B\n3 ORE, 2 B => 2 A\n3 A, 5 B => 1 FUEL")
    system.calculateRequiredOre(ChemicalElements(mutable.Map("FUEL" -> 1))).requiredChemicals.head._2 shouldBe 12
  }

  it should "work for the my second own example" in {
    val system = Day14("2 ORE => 4 B\n3 ORE, 2 B, 1 C => 2 A\n1 ORE => 10 C\n3 A, 5 B, 3 C => 1 FUEL ")
    system.calculateRequiredOre(ChemicalElements(mutable.Map("FUEL" -> 1))).requiredChemicals.head._2 shouldBe 13
  }

  it should "work for the input" in {
    val system = Day14("11 BNMWF, 1 MRVFT, 10 PBNSF => 7 XSFVQ\n149 ORE => 4 SMSB\n1 XHQDX, 1 SVSTJ, 2 LDHX => 7 JMWQG\n12 MJCLX => 9 PBNSF\n132 ORE => 7 XPTXL\n15 TZMWG, 1 LDHX, 1 PDVR => 7 LBQB\n1 HJTD, 8 VFXHC => 2 SVSTJ\n5 LBHQ, 6 MTQCB => 4 MHBZ\n1 PRXT, 1 FWZN => 2 PBMPL\n1 XPTXL => 1 HMRGM\n10 XHPHR => 6 NSVJL\n3 QZQLZ, 3 MTQCB => 4 TZMWG\n5 LBHQ, 2 VPSDV => 3 ZFCD\n13 WPFP => 6 ZXMGK\n10 MHJMX, 75 LDHX, 52 JMWQG, 4 QWRB, 1 SVNVJ, 17 BNMWF, 18 GHVN => 1 FUEL\n4 PFQRG, 14 XVNL => 5 PDCV\n11 JMWQG, 10 ZBNCP => 6 NTJZH\n14 PBMPL, 12 PRXT, 9 MJQS => 9 XVNL\n9 GDNG, 13 LBQB => 9 QWRB\n1 CXNM => 6 PFQRG\n9 NTJZH, 7 BNMWF, 11 JCHP, 1 MHBZ, 1 SVSTJ, 9 XRDN => 5 SVNVJ\n1 XHPHR, 1 GSMP => 4 THRVR\n26 FWZN => 4 WPFP\n35 VJTFJ, 2 XSFVQ, 6 HJVN, 1 NSVJL, 1 JCHP, 3 MJCLX, 1 QZNCK => 6 GHVN\n1 WPFP, 3 XHPHR => 2 HJVN\n5 SMSB => 7 HNCDS\n111 ORE => 4 GSMP\n6 LBHQ => 8 GDNG\n2 GDNG, 5 MHBZ => 1 RNMKC\n15 THRVR, 4 NWNSH, 1 NSVJL => 7 FDVH\n2 HMRGM => 9 FWZN\n6 MJQS, 5 JRZXM => 5 NWNSH\n14 ZXMGK, 1 JTXWX => 6 DLWT\n1 MJQS, 3 FWZN, 2 PRXT => 1 JTXWX\n1 GSMP, 4 CXNM => 3 JRZXM\n151 ORE => 9 ZNPRL\n2 NTJZH, 1 DLWT, 3 ZBNCP => 9 MRVFT\n14 SWZCB, 1 VPSDV => 7 XRDN\n14 LBHQ, 16 FDVH, 9 PFQRG => 4 PRXT\n22 CXNM => 9 HJTD\n1 VFXHC, 1 MTQCB => 6 QZQLZ\n6 SWZCB, 2 PDCV, 17 RNMKC => 9 LTHFW\n4 ZNPRL => 6 CXNM\n2 CXNM => 3 LBHQ\n8 MHBZ, 2 QZQLZ, 2 LBQB => 3 VJTFJ\n3 ZFCD => 1 XHQDX\n1 VJTFJ, 7 MHBZ => 8 ZBNCP\n5 CXNM => 2 VPSDV\n7 MJQS => 9 VFXHC\n2 LTHFW, 11 HJVN, 4 XRDN, 8 MRVFT, 3 NSVJL, 3 SVSTJ, 5 XSFVQ, 13 RNMKC => 8 MHJMX\n2 HMRGM => 3 XHPHR\n1 GDNG, 19 PDVR => 3 SWZCB\n18 HMRGM, 10 HNCDS => 2 MJQS\n6 HNCDS, 2 HMRGM, 1 LBHQ => 3 MTQCB\n16 VJTFJ, 1 WPFP, 6 JMWQG => 6 BNMWF\n3 TZMWG, 1 FWZN => 7 PDVR\n10 ZXMGK => 4 QZNCK\n32 LBQB, 1 ZBNCP => 1 JCHP\n27 PDVR, 7 QZQLZ, 7 PBMPL => 3 MJCLX\n5 MHBZ, 12 ZFCD => 4 LDHX")
    system.calculateRequiredOre(ChemicalElements(mutable.Map("FUEL" -> 1))).requiredChemicals.head._2 shouldBe 485720
  }

  "The second part" should "work for the first example" in {
    val system = Day14("157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")
    system.calculateHowManyFuel(RequiredChemical("ORE", 1000000000000L), 82000000) shouldBe 82892753
  }

  it should "work for the third example" in {
    val system = Day14("171 ORE => 8 CNZTR\n7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n114 ORE => 4 BHXH\n14 VRPVC => 6 BMBT\n6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n5 BMBT => 4 WPTQ\n189 ORE => 9 KTJDG\n1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n12 VRPVC, 27 CNZTR => 2 XDBXC\n15 KTJDG, 12 BHXH => 5 XCVML\n3 BHXH, 2 VRPVC => 7 MZWV\n121 ORE => 7 VRPVC\n7 XCVML => 6 RJRHP\n5 BHXH, 4 VRPVC => 5 LTCX")
    system.calculateHowManyFuel(RequiredChemical("ORE", 1000000000000L), 460000) shouldBe 460664
  }

  it should "work for the input" in {
    val system = Day14("11 BNMWF, 1 MRVFT, 10 PBNSF => 7 XSFVQ\n149 ORE => 4 SMSB\n1 XHQDX, 1 SVSTJ, 2 LDHX => 7 JMWQG\n12 MJCLX => 9 PBNSF\n132 ORE => 7 XPTXL\n15 TZMWG, 1 LDHX, 1 PDVR => 7 LBQB\n1 HJTD, 8 VFXHC => 2 SVSTJ\n5 LBHQ, 6 MTQCB => 4 MHBZ\n1 PRXT, 1 FWZN => 2 PBMPL\n1 XPTXL => 1 HMRGM\n10 XHPHR => 6 NSVJL\n3 QZQLZ, 3 MTQCB => 4 TZMWG\n5 LBHQ, 2 VPSDV => 3 ZFCD\n13 WPFP => 6 ZXMGK\n10 MHJMX, 75 LDHX, 52 JMWQG, 4 QWRB, 1 SVNVJ, 17 BNMWF, 18 GHVN => 1 FUEL\n4 PFQRG, 14 XVNL => 5 PDCV\n11 JMWQG, 10 ZBNCP => 6 NTJZH\n14 PBMPL, 12 PRXT, 9 MJQS => 9 XVNL\n9 GDNG, 13 LBQB => 9 QWRB\n1 CXNM => 6 PFQRG\n9 NTJZH, 7 BNMWF, 11 JCHP, 1 MHBZ, 1 SVSTJ, 9 XRDN => 5 SVNVJ\n1 XHPHR, 1 GSMP => 4 THRVR\n26 FWZN => 4 WPFP\n35 VJTFJ, 2 XSFVQ, 6 HJVN, 1 NSVJL, 1 JCHP, 3 MJCLX, 1 QZNCK => 6 GHVN\n1 WPFP, 3 XHPHR => 2 HJVN\n5 SMSB => 7 HNCDS\n111 ORE => 4 GSMP\n6 LBHQ => 8 GDNG\n2 GDNG, 5 MHBZ => 1 RNMKC\n15 THRVR, 4 NWNSH, 1 NSVJL => 7 FDVH\n2 HMRGM => 9 FWZN\n6 MJQS, 5 JRZXM => 5 NWNSH\n14 ZXMGK, 1 JTXWX => 6 DLWT\n1 MJQS, 3 FWZN, 2 PRXT => 1 JTXWX\n1 GSMP, 4 CXNM => 3 JRZXM\n151 ORE => 9 ZNPRL\n2 NTJZH, 1 DLWT, 3 ZBNCP => 9 MRVFT\n14 SWZCB, 1 VPSDV => 7 XRDN\n14 LBHQ, 16 FDVH, 9 PFQRG => 4 PRXT\n22 CXNM => 9 HJTD\n1 VFXHC, 1 MTQCB => 6 QZQLZ\n6 SWZCB, 2 PDCV, 17 RNMKC => 9 LTHFW\n4 ZNPRL => 6 CXNM\n2 CXNM => 3 LBHQ\n8 MHBZ, 2 QZQLZ, 2 LBQB => 3 VJTFJ\n3 ZFCD => 1 XHQDX\n1 VJTFJ, 7 MHBZ => 8 ZBNCP\n5 CXNM => 2 VPSDV\n7 MJQS => 9 VFXHC\n2 LTHFW, 11 HJVN, 4 XRDN, 8 MRVFT, 3 NSVJL, 3 SVSTJ, 5 XSFVQ, 13 RNMKC => 8 MHJMX\n2 HMRGM => 3 XHPHR\n1 GDNG, 19 PDVR => 3 SWZCB\n18 HMRGM, 10 HNCDS => 2 MJQS\n6 HNCDS, 2 HMRGM, 1 LBHQ => 3 MTQCB\n16 VJTFJ, 1 WPFP, 6 JMWQG => 6 BNMWF\n3 TZMWG, 1 FWZN => 7 PDVR\n10 ZXMGK => 4 QZNCK\n32 LBQB, 1 ZBNCP => 1 JCHP\n27 PDVR, 7 QZQLZ, 7 PBMPL => 3 MJCLX\n5 MHBZ, 12 ZFCD => 4 LDHX")
    system.calculateHowManyFuel(RequiredChemical("ORE", 1000000000000L), 3840000L) shouldBe 3848998
  }
}
