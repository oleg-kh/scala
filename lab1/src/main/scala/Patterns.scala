/** Напишите решение в виде функции.
  * 
  * Синтаксис:
  *   val a: Int = ???
  * 
  *   a match {
  *     case 0 => true
  *     case _ => false
  *   }
  */
object PatternMatching {

  sealed trait Hand
  case object Rock    extends Hand
  case object Paper   extends Hand
  case object Scissor extends Hand

  sealed trait Result
  case object Win  extends Result
  case object Lose extends Result
  case object Draw extends Result

  sealed trait Food
  case object Meat       extends Food
  case object Vegetables extends Food
  case object Plants     extends Food

  sealed trait Animal {

    val name: String
    val food: Food
  }
  case class Mammal(name: String, food: Food, weight: Int) extends Animal
  case class Fish(name: String, food: Food)                extends Animal
  case class Bird(name: String, food: Food)                extends Animal



  /* a) Напишите функцию, которая ставит в соответствие числу строку слудеющим образом:
   * Если:
   *     1 => "it is one"
   *     2 => "it is two"
   *     3 => "it is three"
   *     иначе => "what's that"
   */

  def matching(value: Int): String = value match {
    case 1 => "it is one"
    case 2 => "it is two"
    case 3 => "it is three"
    case _ => "what's that"
  }

  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testIntToString(value: Int): String = matching(value)

  /* b) Напишите функцию которая возвращает true если переменная `value` принимает значение:
   *     "max" или "Max
   *     "moritz" или "Moritz"
   */

  def matching2(value: String): Boolean = value match {
    case "max" | "Max" | "moritz" | "Moritz" => true
    case _=> false
  }


  // примените функции из пункта (b) здесь, не изменяя сигнатуру
  def testIsMaxAndMoritz(value: String): Boolean = matching2(value)

  // c) Напишите функцию проверки является ли `value` четным

  def matching3(value: Int): Boolean = value match {
    case value: Int if value % 2 == 0 => true
    case value: Int if value % 2 != 0 => false
  }


  // примените функции из пункта (c) здесь, не изменяя сигнатуру
  def testIsEven(value: Int): Boolean = matching3(value)


  
  /* d) Напишите функцию, моделирующую игру в Камень ножницы бумага 
   *     1. камень побеждает ножницы
   *     2. ножницы побеждают бумагу
   *     3. бумага побеждает камень
   *    Выиграет ли игрок `a`?
   */

  def matching4(a: Hand, b: Hand): Result = (a,b) match {
    case (Paper, Rock) | (Scissor,Paper) | (Rock,Scissor) => Win
    case (Rock,Paper) | (Paper, Scissor) | (Scissor, Rock) => Lose
    case (Paper,Paper) | (Rock, Rock) | (Scissor,Scissor) => Draw
  }


  // примените вашу функцию из пункта (d) здесь, не изменяя сигнатуру
  def testWinsA(a: Hand, b: Hand): Result = matching4(a,b)



  // Примечание: используйте определение Animals

  // e) Верните вес (weight: Int) объекта Mammal, иначе верните -1.

  def matching5(animal: Animal): Int = animal match {
    case mammal: Mammal => mammal.weight
    case _ => -1
  }

  // примените функцию из пункта (e) здесь, не изменяйте сигнатуру
  def testExtractMammalWeight(animal: Animal): Int = matching5(animal)


  // f) Измените поле еда объектов классов Fishes и Birds на Plants, класс Mammels оставьте неизмененным.

  def matching6(animal: Animal): Animal = animal match {
    case Fish(_,_) =>  Fish(animal.name, Plants)
    case Bird(_,_) =>  Bird(animal.name, Plants)
    case _ => animal
  }

  // примените функцию из пункта (f) здесь, не изменяйте сигнатуру
  def testUpdateFood(animal: Animal): Animal = matching6(animal)

  def main(args: Array[String]): Unit ={
    val tiger = Mammal("tiger", Meat, 100)
    val pigeon = Bird("pigeon", Vegetables)

    println("testIntToString(2)=>" + testIntToString(2))
    println("testIsMaxAndMoritz(max)=>" + testIsMaxAndMoritz("max"))
    println("testIsEven(6)=>" + testIsEven(6))
    println("testWinsA(Paper, Scissor)=>" + testWinsA(Paper, Scissor))
    println("testExtractMammalWeight(tiger)=>" + testExtractMammalWeight(tiger))
    println("testUpdateFood(pigeon).food=>" + testUpdateFood(pigeon).food)
  }
}
