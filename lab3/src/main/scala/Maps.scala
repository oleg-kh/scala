

/** Напишите вашу реализацию в тестовые функции.
  * 
  * https://docs.scala-lang.org/overviews/collections/maps.html
  */
object Maps {

  case class User(name: String, age: Int)

  /* a) В данной Seq[User] сгруппируйте пользователей по имени (`groupBy`) и вычислите средний возраст: `name -> averageAge`
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testGroupUsers(users: Seq[User]): Map[String, Int] = {
    val averageAge = users.map(_.age).sum / users.length
    users.groupBy(_.name).map(el => (el._1, averageAge))
  }

  /* b) Дана `Map[String, User]` состоящая из имен пользователей `User`, сколько имен пользователей, содержащихся в Map, содержат подстроку "Adam"?
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testNumberFrodos(map: Map[String, User]): Int = map.map(el => el._2.name).count(el => el.contains("Adam"))

  /* c) Удалите всех пользователей возраст которых менее 35 лет.
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testUnderaged(map: Map[String, User]): Map[String, User] = map.filter(el => el._2.age > 35)

  def main(args: Array[String]): Unit ={
    val users:Seq[User] = Seq(User("Alex",10),User("Bob",20),User("Charlie",30),User("Bobby",20))
    val map = Map("asd" -> User("Alex",10),"zxc" -> User("Adam",20),"123" -> User("Charlie",30),"qwe" ->User("Adamiums",40))
    println("testGroupUsers=>" + testGroupUsers(users))
    println("testUnderaged=>" + testUnderaged(map))
    println("testNumberFrodos=>" + testNumberFrodos(map))
  }
}
