import org.scalatest.{FlatSpec, Matchers}

class day1Tests extends FlatSpec with Matchers {
  "the first part" should "work for the first example" in {
    day1.calculateFuel("12") shouldBe 2
  }

  it should "work for the second example" in {
    day1.calculateFuel("14") shouldBe 2
  }

  it should "work for the third example" in {
    day1.calculateFuel("1969") shouldBe 654
  }

  it should "work for the fourth example" in {
    day1.calculateFuel("100756") shouldBe 33583
  }

  it should "work for the input" in {
    println(day1.calculateFuel("83285\n96868\n121640\n51455\n128067\n128390\n141809\n52325\n68310\n140707\n124520\n149678\n87961\n52040\n133133\n52203\n117483\n85643\n84414\n86558\n65402\n122692\n88565\n61895\n126271\n128802\n140363\n109764\n53600\n114391\n98973\n124467\n99574\n69140\n144856\n56809\n149944\n138738\n128823\n82776\n77557\n51994\n74322\n64716\n114506\n124074\n73096\n97066\n96731\n149307\n135626\n121413\n69575\n98581\n50570\n60754\n94843\n72165\n146504\n53290\n63491\n50936\n79644\n119081\n70218\n85849\n133228\n114550\n131943\n67288\n68499\n80512\n148872\n99264\n119723\n68295\n90348\n146534\n52661\n99146\n95993\n130363\n78956\n126736\n82065\n77227\n129950\n97946\n132345\n107137\n79623\n148477\n88928\n118911\n75277\n97162\n80664\n149742\n88983\n74518"))
  }

  "the second part" should "work for the first example" in {
    day1.calculateRecursiveFuel("14") shouldBe 2
  }

  it should "work for the third example" in {
    day1.calculateRecursiveFuel("1969") shouldBe 966
  }

  it should "work for the fourth example" in {
    day1.calculateRecursiveFuel("100756") shouldBe 50346
  }

  it should "work for the input" in {
    println(day1.calculateRecursiveFuel("83285\n96868\n121640\n51455\n128067\n128390\n141809\n52325\n68310\n140707\n124520\n149678\n87961\n52040\n133133\n52203\n117483\n85643\n84414\n86558\n65402\n122692\n88565\n61895\n126271\n128802\n140363\n109764\n53600\n114391\n98973\n124467\n99574\n69140\n144856\n56809\n149944\n138738\n128823\n82776\n77557\n51994\n74322\n64716\n114506\n124074\n73096\n97066\n96731\n149307\n135626\n121413\n69575\n98581\n50570\n60754\n94843\n72165\n146504\n53290\n63491\n50936\n79644\n119081\n70218\n85849\n133228\n114550\n131943\n67288\n68499\n80512\n148872\n99264\n119723\n68295\n90348\n146534\n52661\n99146\n95993\n130363\n78956\n126736\n82065\n77227\n129950\n97946\n132345\n107137\n79623\n148477\n88928\n118911\n75277\n97162\n80664\n149742\n88983\n74518"))
  }

  it should "work for the input summarized" in {
    println(day1.calculateRecursiveFuel("3289802"))
  }
}
