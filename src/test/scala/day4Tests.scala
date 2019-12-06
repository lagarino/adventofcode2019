import org.scalatest.{FlatSpec, Matchers}

class day4Tests extends FlatSpec with Matchers {
  "the function to validate the password" should "work for the first example" in {
    day4.isValidPassword("111111") shouldBe true
  }

  it should "work for the second example" in {
    day4.isValidPassword("122345") shouldBe true
  }

  it should "work for the third example" in {
    day4.isValidPassword("223450") shouldBe false
  }

  it should "work for the fourth example" in {
    day4.isValidPassword("123789") shouldBe false
  }

  it should "count the number of passwords within a range" in {
    day4.numberOfValidPasswordsBetween("156218", "652527") shouldBe 1694
  }

  "the enhanced function to validate the password" should "work for the first example" in {
    day4.isValidPasswordEnhanced("111111") shouldBe true
  }

  it should "work for the second example" in {
    day4.isValidPasswordEnhanced("112233") shouldBe true
  }

  it should "work for the third example" in {
    day4.isValidPasswordEnhanced("123444") shouldBe false
  }

  it should "work for the fourth example" in {
    day4.isValidPasswordEnhanced("111122") shouldBe true
  }

  it should "work for the fifth example" in {
    day4.isValidPasswordEnhanced("156789") shouldBe true
  }

  it should "count the number of passwords within a range" in {
    day4.numberOfEnhancedValidPasswordsBetween("156218", "652527") shouldBe 1148
  }
}
