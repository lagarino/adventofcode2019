

object day4 {
  def isValidPassword(password: String): Boolean = {
    password.length == 6 &&
      password.map(_.isDigit).forall(identity) &&
      password.map(_.toInt).sliding(2).forall(newDigit => newDigit(0) <= newDigit(1)) &&
      password.map(_.toInt).sliding(2).exists(newDigit => newDigit(0) == newDigit(1))
  }

  def numberOfValidPasswordsBetween(password1: String, password2: String): Int = {
    (password1.toInt to password2.toInt).count(pwd => isValidPassword(pwd.toString))
  }

  def numberOfEnhancedValidPasswordsBetween(password1: String, password2: String): Int = {
    val validPasswords = (password1.toInt to password2.toInt).filter(pwd => isValidPasswordEnhanced(pwd.toString))
    validPasswords.foreach(println(_))
    validPasswords.length
  }

  def isValidPasswordEnhanced(password: String): Boolean = {
    password.length == 6 &&
      password.map(_.isDigit).forall(identity) &&
      password.map(_.toInt).sliding(2).forall(newDigit => newDigit(0) <= newDigit(1)) &&
      password.map(_.toInt).sliding(2).exists(newDigit => newDigit(0) == newDigit(1)) &&
      group(password).exists(repetitions => repetitions.length == 2)
  }

  case class Accumulator(isValid: Boolean, lastChar: Char, repetitions: Int)

  private def group(password: String): List[String] = {
    if (password.isEmpty) Nil
    else password.takeWhile(_ == password.head) :: group(password.dropWhile(_ == password.head))
  }

}