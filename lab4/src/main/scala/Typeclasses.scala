object Typeclasses {

  // a) Определите тайп-класс Reversable, который представляет в обратном порядке значения.

  trait Reversable[T]{
  def reverse(a: T): T
}

  // b) Реализуйте функцию Reverse для String.
  implicit object ReverseString extends Reversable[String]{
    override def reverse(a:String):String = a.reverse
  }

  def reverse[T:Reversable](a:T):T = implicitly[Reversable[T]].reverse(a)

  // примените тайп-класс-решение из пункта (a) здесь
  def testReversableString(str: String): String = reverse(str)

  // c) Определите тайп-класс Smash таким образом чтобы в нем была функция smash, которая выполняет операцию со значениями одного типа.

  trait Smash[T] {
    def smash(a: T, b: T): T
  }

    def smash[T : Smash](a: T, b: T): T = implicitly[Smash[T]].smash(a,b)

  // d) Реализуйте  функции Smash для типа Int и Double.
  //    Используйте сложение для типа Int у умножение для типа Double.

  implicit object SmashInt extends Smash[Int]{
    override def smash(a:Int,b:Int):Int = a + b
  }
    implicit object SmashDouble extends Smash[Double]{
      override def smash(a:Double,b:Double):Double = a * b
    }
    implicit object SmashString extends Smash[String]{
      override def smash(a:String,b:String):String = a + b
    }

  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashInt(a: Int, b: Int): Int = smash(a,b)

  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashDouble(a: Double, b: Double): Double = smash(a,b)


  // e) Реализуйте функцию Smash для типа String. Необходимо выполнить конкатенацию строк, которые будут получены в качестве параметра.

  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashString(a: String, b: String): String = smash(a,b)

  def main(args: Array[String]): Unit ={
    println("testReversableString=>" + testReversableString("Hello world"))
    println("testSmashInt=>" + testSmashInt(10,20))
    println("testSmashDouble=>" + testSmashDouble(10,2.2))
    println("testSmashString=>" + testSmashString("qwe","rty"))
  }
}

// Реализуйте тестовые функции с выводом на экран проверки разработанных функций.