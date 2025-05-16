package rock.cats
import scala.util.Try

object Lesson11_DataValidation {

  import cats.data.Validated
  val aValidValue: Validated[String, Int] = Validated.Valid(42) // "right" value
  //                           ^      ^
  //                         error   desire value

  val anInvalidValue: Validated[String, Int] =
    Validated.invalid("Something went wrong!") // "left" value

  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "meaning of life is too small")
  //                                                          ^       ^
  //                                                        valid  invalid

  // Validated es parecido a Either, pero tiene la capacidad de combinar los errores

  def prime(n: Int): Boolean = {
    def primeTR(x: Int): Boolean =
      if (x <= 1) true
      else n % x != 0 && primeTR(x - 1)
    if (n == 0 || n == 1 || n == -1) false
    else primeTR(Math.abs(n / 2))
  }

  // TODO: use either, combinar los errores en el Left del either
  /*
    - n must be a prime
    - n must be non-negative
    - n <= 100
    - n must be even
   */
  def testNumber(n: Int): Either[List[String], Int] = {
    val isPrime: List[String]     = if (prime(n)) List() else List(s"$n is not prime")
    val nonNegative: List[String] = if (n >= 0) List() else List(s"$n is negative")
    val lessThan100: List[String] = if (n <= 100) List() else List(s"$n is not less than 100")
    val even: List[String]        = if (n % 2 == 0) List() else List(s"$n is not even")

    // my solution
//    val errors = List(isPrime, nonNegative, lessThan100, even).reduce(_ ++ _)
//    if (errors.isEmpty) Right(n)
//    else Left(errors)

    // course solution
    if (n % 2 == 0 && n >= 0 && n <= 100 && prime(n)) Right(n)
    else Left(isPrime ++ nonNegative ++ lessThan100 ++ even)
  }

  // Using Validated class
  import cats.Semigroup
  import cats.instances.list._ // Semigroup[List] to combine List[String]
  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](
    Math.max
  ) // no se para que usa esto si el resultado deseado de la función no hay que combinarla
  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated
      .cond(n % 2 == 0, n, List(s"$n is not even"))
      .combine(Validated.cond(n >= 0, n, List(s"$n is negative")))
      .combine(Validated.cond(n <= 100, n, List(s"$n is not less than 100")))
      .combine(Validated.cond(prime(n), n, List(s"$n is not prime")))

  // chain
  aValidValue.andThen(_ => anInvalidValue)
  // test a valid value
  aValidValue.ensure(List("something went wrong"))(_ % 2 == 0)
  // transform
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)      // Validated[String, Int] leftMap hace el map sobre String
  aValidValue.bimap(_.length, _ + 1) // map sobre los dos valores
  // interoperate with stlib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] =
    Validated.fromOption(None, List("Nothing present here"))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("something".toInt))

  // backwards (hacia atrás)
  aValidValue.toOption
  aValidValue.toEither

  // TODO: form validation
  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]

    /** fields are
      *   - name
      *   - email
      *   - password
      *
      * rules are
      *   - name, email and password must be specified
      *   - name must not be blank
      *   - email must have @
      *   - password must have >= 10 characters
      *
      * return Valid with "Success"
      *
      * @param form
      *   Map[String, String] = Map[field, value]
      */
    // My solution
    def validateForm(form: Map[String, String]): FormValidation[String] = {
      def validName: FormValidation[String] =
        Validated
          .fromOption(form.get("name"), List("name must be defined"))
          .ensure(List("name must not be blank"))(!_.isBlank)
      def validEmail: FormValidation[String] =
        Validated
          .fromOption(form.get("email"), List("email must be defined"))
          .ensure(List("email must have @"))(email => email.exists(c => c == '@'))
      def validPassword: FormValidation[String] =
        Validated
          .fromOption(form.get("password"), List("password must be defined"))
          .ensure(List("password must have >= 10 characters"))(_.length >= 10)

      validName
        .combine(validEmail)
        .combine(validPassword)
        .map(_ => "Success")
    }
    // Course solution
    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"The field $fieldName must be specified."))

    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.length > 0, value, List(s"The field $fieldName must not be blank."))

    def emailProperForm(email: String): FormValidation[String] =
      Validated.cond(email.contains('@'), email, List("Email is invalid."))

    def passwordCheck(password: String): FormValidation[String] =
      Validated.cond(
        password.length >= 10,
        password,
        List("Password must be at least 10 characters long.")
      )

    import cats.instances.string._ // for combine
    def validateFormR(form: Map[String, String]): FormValidation[String] =
      getValue(form, "name")
        .andThen(name => nonBlank(name, "name"))
        .combine(getValue(form, "email").andThen(emailProperForm))
        .combine(getValue(form, "password").andThen(passwordCheck))
        .map(_ => "User registration complete.")
  }

  // extension methods
  import cats.syntax.validated._
  val aValidMeaningOfLife: Validated[List[String], Int] = 42.valid[List[String]]
  val anError: Validated[String, Int]                   = "Something went wrong".invalid[Int]

  def main(args: Array[String]): Unit = {
    println(testNumber(2))
    println(testNumber(4))
    println(testNumber(103))

    println(validateNumber(-5))
    println(validateNumber(505))

    val validForm =
      Map("name" -> "Juan", "email" -> "juan@navarrosoft.es", "password" -> "8o402i7634932")
    println(FormValidation.validateForm(validForm))
    println(FormValidation.validateFormR(validForm))

    val invalidForm = Map("name4" -> "Juan", "email" -> "juannavarrosoft.es", "password" -> "8o4")
    println(FormValidation.validateForm(invalidForm))
    println(FormValidation.validateFormR(invalidForm))
  }

}
