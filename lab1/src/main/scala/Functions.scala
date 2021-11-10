/** Напишите отдельные функции, решающие поставленную задачу.
  * 
  * Синтаксис:
  *   // метод
  *   def myFunction(param0: Int, param1: String): Double = // тело
  * 
  *   // значение
  *   val myFunction: (Int, String) => Double (param0, param1) => // тело
  */
object Functions {

  /* a) Напишите функцию, которая рассчитывает площадь окружности
   *    r^2 * Math.PI
   */

  def getCircleArea(radius: Double): Double = Math.pow(radius,2) * Math.PI


  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testCircle(r: Double): Double = getCircleArea(r)
  


  /* b) Напишите карированную функцию которая рассчитывает площадь прямоугольника a * b.
   */

  def GetRectangleArea(a: Double)(b: Double): Double = a * b


  // примените вашу функцию из пукта (b) здесь, не изменяя сигнатуру
  def testRectangleCurried(a: Double, b: Double): Double = GetRectangleArea(a)(b)


  // c) Напишите не карированную функцию для расчета площади прямоугольника.

  def GetRectangleArea2(a:Double,b:Double):Double = a * b;


  // примените вашу функцию из пункта (c) здесь, не изменяя сигнатуру
  def testRectangleUc(a: Double, b: Double): Double = GetRectangleArea2(a,b)

  def main(args: Array[String]): Unit = {
    println("testCircle(3)=>" + testCircle(3))
    println("testRectangleCurried(3,4)=>" + testRectangleCurried(3,4))
    println("testRectangleUc(3,4)=>" + testRectangleUc(3,4))
  }
}
