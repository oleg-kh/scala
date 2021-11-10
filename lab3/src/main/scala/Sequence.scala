

/** Напишите свои решения в тестовых функциях.
  * 
  * Seq(1, 2) match {
  *   case head +: tail => ???
  *   case Nil          => ???
  *   case s            => ???
  * }
  * 
  * https://www.scala-lang.org/api/2.12.0/scala/collection/Seq.html
  */
// Примечание: напишите функции с хвостовой рекурсией

object Sequence {

  /* a) Найдите последний элемент Seq.
   *    
   */
//  def testLastElement[A](seq: Seq[A]): Option[A] = Some(seq.drop(seq.length -1).head)
  def testLastElement[A](seq: Seq[A]): Option[A] = seq.lastOption
  /* b) Объедините две Seqs (то есть Seq(1, 2) и Seq(3, 4) образуют Seq((1, 3), (2, 4))) - если Seq длиннее игнорируйте оставшиеся элементы.
   *    
   */
  def testZip[A](a: Seq[A], b: Seq[A]): Seq[(A, A)] = a zip b

  /* c) Проверьте, выполняется ли условие для всех элементов в Seq.
   *    
   */
  def testForAll[A](seq: Seq[A])(cond: A => Boolean): Boolean = seq.forall(cond)

  /* d) Проверьте, является ли Seq палиндромом
   *    
   */
  def testPalindrom[A](seq: Seq[A]): Boolean = if (seq == seq.reverse) true else false

  /* e) Реализуйте flatMap используя foldLeft.
   *    
   */
  def testFlatMap[A, B](seq: Seq[A])(f: A => Seq[B]): Seq[B] = seq.foldLeft(Seq[B]())((acc,elem) => acc ++: f(elem))

  def main(args: Array[String]): Unit ={
    val seq = Seq(1,2,3,4,5)
    val seq2 = Seq(1,2,1)
    println("testLastElement=>" + testLastElement(seq))
    println("testZip=>" + testZip(seq,seq2))
    println("testForAll=>" + testForAll(seq)(num => num > 2))
    println("testForAll=>" + testForAll(seq)(num => num < 10))
    println("testPalindrom=>" + testPalindrom(seq))
    println("testPalindrom=>" + testPalindrom(seq2))
    println("testFlatMap=>" + testFlatMap(seq)(num => Seq (num * 2)))
  }
}
