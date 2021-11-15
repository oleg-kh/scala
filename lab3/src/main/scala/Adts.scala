
import scala.util.{Failure, Try}

/** Реализуйте следующие функции.
  * 
  * List(1, 2) match {
  *   case head :: tail => ???
  *   case Nil          => ???
  *   case l            => ???
  * }
  * 
  * Option(1) match {
  *   case Some(a) => ???
  *   case None    => ???
  * }
  * 
  * Either.cond(true, 1, "right") match {
  *   case Left(i)  => ???
  *   case Right(s) => ???
  * }
  * 
  * Try(impureExpression()) match {
  *   case Success(a)     => ???
  *   case Failure(error) => ???
  * }
  * 
  * Try(impureExpression()).toEither
  * 
  */
object Adts {

  // a) Дан List[Int], верните элемент с индексом n
  def GetNth(list: List[Int], n: Int): Option[Int] = Some(list(n))

  // примените функцию из пункта (a) здесь, не изменяйте сигнатуру 
  def testGetNth(list: List[Int], n: Int): Option[Int] = GetNth(list,n)

  // b) Напишите функцию, увеличивающую число в два раза.
  def Double(n: Option[Int]): Option[Int] = Some(n.get * 2)


  // примените функцию из пункта (b) здесь, не изменяйте сигнатуру
  def testDouble(n: Option[Int]): Option[Int] = Double(n)

  // c) Напишите функцию, проверяющую является ли число типа Int четным. Если так, верните Right. В противном случае, верните Left("Нечетное число.").
  def isEven(n: Int): Either[String, Int] =n  match {
    case value: Int if value % 2 == 0 => Right(n)
    case _ => Left("Нечетное число.")
       }


  // примените функцию из пункта (c) здесь, не изменяйте сигнатуру
  def testIsEven(n: Int): Either[String, Int] = isEven(n)

  // d) Напишите функцию, реализующую безопасное деление целых чисел. Верните Right с результатом или Left("Вы не можете делить на ноль.").
  def safeDivide(a: Int, b: Int): Either[String, Int] = (a,b) match{
    case (a,b) if (b != 0) => Right(a / b)
    case _ => Left("Вы не можете делить на ноль.")
  }


  // примените функцию из пункта (d) здесь, не изменяйте сигнатуру
  def testSafeDivide(a: Int, b: Int): Either[String, Int] = safeDivide(a,b)

  // e) Обработайте исключения функции с побочным эффектом вернув 0.
  def goodOldJava(impure: String => Int, str: String): Try[Int] ={
    Try(impure(str)).recover{
      case _ => 0
    }
  }


  // примените функцию из пункта (e) здесь, не изменяйте сигнатуру
  def testGoodOldJava(impure: String => Int, str: String): Try[Int] = goodOldJava(impure,str)


  def main(args: Array[String]): Unit ={
    println("testGetNth(List(1,2,3,4,5),2)=>" + testGetNth(List(1,2,3,4,5),2))
    println("testDouble(Some(2))=>" + testDouble(Some(2)))
    println("testIsEven(2)=>" + testIsEven(2))
    println("testIsEven(3)=>" + testIsEven(3))
    println("testSafeDivide(4,2)=>" + testSafeDivide(4,2))
    println("testSafeDivide(4,0)=>" + testSafeDivide(4,0))
    println("testGoodOldJava((str:String)=>str.toInt,\"123\")=>" + testGoodOldJava((str:String)=>str.toInt,"123"))
    println("testGoodOldJava((str:String)=>str.toInt,\"asd\")=>" + testGoodOldJava((str:String)=>str.toInt,"asd"))
  }
}
