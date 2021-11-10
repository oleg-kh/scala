
// 1
// Переменные res – это значения val или настоящие переменные var?
// val так как их нельзя переназначить

// 2
// "crazy" * 3 в REPL
println("#2")
println("crazy"*3)

// 3
// Что означает выражение 10 max 2? В каком классе определен метод max?
// возвращает наибольшее число, RichInt.
println("#3")
println(10 max 2)

// 4
// Используя число типа BigInt, вычислите 2^1024
println("#4")
println(BigInt(2) pow 1024)

// 5
// Что нужно импортировать, чтобы найти случайное простое число
// вызовом метода probablePrime(100, Random) без использования каких-либо
// префиксов перед именами probablePrime и Random?
println("#5")
import BigInt.probablePrime, util.Random
println(probablePrime(100, Random))

// 6
// Один из способов создать файл или каталог со случайным
// именемсостоит в том, чтобы сгенерировать случайное число типа BigInt и
// преобразовать его в систему счисления по основанию 36, в результате
// получится строка, такая как "qsnvbevtomcj38o06kul". Отыщите в Scaladoc
// методы, которые можно было бы использовать для этого.
println("#6")
println(BigInt(100, Random) toString 36)
  
// 7
// Как получить первый символ строки в языке Scala? А последний символ?
println("#7")
val str = "string"
val start = str(0)
val end = str(str.length -1)
println(start)
println(end)

// 8
// Что делают строковые функции take, drop, takeRight и dropRight?
// Какие преимущества и недостатки они имеют в сравнении с substring?
// у take, drop не будет ошибок если написать неправильные идексы(например больше чем длина строки или она пустая)
println("#8")
println(("stringstring" take 3, "stringstring" drop 3, "stringstring" takeRight 3, "stringstring" dropRight 3))

// 9
// Сигнум числа равен 1, если число положительное. -1 – если
// отрицательное, и 0 – если равно нулю. Напишите функцию, вычисляющую
// это значение.
println("#9")
def signum(x: Int) = {
  if (x < 0) -1 else if (x == 0) 0 else 1
}
println(signum(20));
println(signum(-20));
println(signum(0));
  
// 10
// Какое значение возвращает блок {}? Каков его тип?
// Тип = scala.Unit, значение = ()

// 11
// Напишите на языке Scala цикл, эквивалентный циклу на языке Java
// for (int i=10; i>=0; i--) System.out.println(i);
println("#11")
for (i <- 10 to (0, -1)) println(i)

// 12
// Напишите процедуру countdown (n: Int), которая выводит числа от n до 0
println("#12")
def countdown(n: Int): Unit = for (i <- n to (0, -1)) println(i)
countdown(3)

// 13
// Напишите цикл for для вычисления кодовых пунктов Юникода
// всех букв в строке. Например, произведение символов в строке «Hello» равно
// 9415087488L.
println("#13")
var result = 1
for (i <- "Hello") { result *= i.toInt }
println(result)

// 14
// Решите предыдущее упражнение без применения цикла.
// Напишите функцию product(s: String
println("#14")
def product(string: String) = {
  var result = 1
  string.foreach((i: Char) => result *= i)
  result
}
println(product("Hello"))

// 16
// Сделайте функцию из предыдущего упражнения рекурсивной.
println("#16")
def productRec(string: String) : Long = {
  if (string.size > 0)
    productRec(string.tail) * string.head.toLong
  else
    1
}
println(productRec("Hello"));

// 17
// Напишите функцию, вычисляющую xn, где n – целое число.
// Используйте следующее рекурсивное определение:
// • xn = y2, если n – четное и положительное число, где y=xn/2
// • xn = x*xn-1, если n – нечетное и положительное число.
// • x0=1.
// • xn=1/x-n, если n – отрицательное число.
// Не используйте инструкцию return.
  println("#17")
  def func(x: Double, n: Int): Double = {
	if (n == 0) 1
	else {
		if (n > 0) {
			if ( n % 2 == 0 && n > 2) {
				func(func(x, n/2), 2)
			} else {
				x * func(x, n - 1)
			}
		} else (1 / func(x, -n))
	}
}
printf("2^2=%f\n", func(2, 2));
printf("2^-1=%f\n", func(2, -1));
printf("2^0=%f\n", func(2, 0));

// 18
// f(m,n) - сумма всех натуральных чисел от m до n включительно, в
// десятичной записи которых нет одинаковых цифр.
println("#18")
def distinctDigits(n: Int): Boolean = {
   val s = n.toString
   s.length == s.distinct.length
 }

def sumDistinctDigits(m: Int, n: Int): Int = {
  (m to n).filter(distinctDigits).sum
}
println(sumDistinctDigits(11,13))

// 19
// Список содержит целые числа, а также другие списки, такие же
// как и первоначальный. Получить список, содержащий только целые числа из
// всех вложенных списков
println("#19")
def flatList(nestedList: List[Any]): List[Any]= {
  nestedList flatMap{case i: List[_] => flatList(i)
  case e => List(e)
  }
}
println(flatList((List(List(1, 1), 2, List(3, List(5, 8))))))


// 20
// f(n) - сумма цифр наибольшего простого делителя натурального
// числа n.
println("#20")
def largestPrimeFactor(b : BigInt) = {
  def loop(f:BigInt, n: BigInt): BigInt =
     if (f == n) n.toString().map(_-'0').sum else 
     if (n % f == 0) loop(f, n / f) 
     else loop(f + 1, n)
  loop (BigInt(2), b)
}
println(largestPrimeFactor(13195))

// 21
// Список содержит элементы одного, но любого типа. Получить
// список, содержащий каждый имеющийся элемент старого списка k раз
// подряд. Число k задается при выполнении программы.
println("#21")
def repeatElem(list: List[Any],n: Int) = {
  list.flatMap(List.fill(n)(_))
}
println(repeatElem(List('a','s','d'), 5))

// 24
// f(m,n) - наименьшее общее кратное натуральных чисел m и n.
println("#24")
def leastCmmonMltiple(a: BigInt, b: BigInt): BigInt = a * b / a.gcd(b)
println(leastCmmonMltiple(12,15))

// 25
// Список содержит элементы одного, но любого типа. Получить
// список, из элементов исходного, удаляя каждый k-й элемент. Число k
// задается при выполнении программы.
println("#25")
def removeNth[A](list: List[A], n: Int): List[A] = {
  list.zipWithIndex.filter((_, i) => (i + 1) % n != 0).map((e, _) => e )
  }
println(removeNth(List(1,2,3,4,5),2))  

// 27
// Список содержит элементы одного, но любого типа. Получить
// новый список, перемещая циклически каждый элемент на k позиций влево
// (при перемещении на одну позицию первый элемент становится последним,
// второй первым и так далее). Число k задается при выполнении программы.
// Если k отрицательное, то перемещение происходит вправо.
println("#27")
def moveToLeft[Any](k: Int, list: List[Any]): List[Any] ={
  val kK = if (list.isEmpty) 0 else k % list.length
  if (kK < 0) moveToLeft(kK + list.length, list)
  else (list drop kK) ::: (list take kK)
}
println(moveToLeft(2,List('a','b','c','d','e')))  

// 28
// f(n) - наибольшее совершенное число не превосходящее n.
// Совершенным называется натуральное число n равное сумме своих
// делителей, меньших n, например 6 = 1 + 2 + 3 ( f(6) = 6, f(7) = 6, ... ).
println("#28")
def maxPerfectNum(N: Int):Unit ={
  for (i <- (1 to N)) {
    var sum = 0;
    for (n <- (1 to i)){
      if(i % n == 0 && i != n){
        sum = sum + n
      }
    }
    if (sum == i){
     println(i)
    }
  }
}
maxPerfectNum(99)

// 29
// Список содержит элементы одного, но любого типа. Получить
// два списка из элементов исходного, выбирая в первый элементы с четными
// индексами, а во второй с нечетными.
println("#29")
def getTwoLits[A](list: List[A]) = {
  val odd= list.zipWithIndex.filter((_, i) => (i) % 2 == 0).map((e, _) => e )
  val even = list.zipWithIndex.filter((_, i) => (i) % 2 != 0).map((e, _) => e )
  (odd,even)
}
println(getTwoLits(List(1,2,3,4,5)))

// 30
// f(n) - наибольшее из чисел от 1 до n включительно, обладающее
// свойством: сумма цифр n в некоторой степени > 1 равна самому числу n.
// Пример: 512 = 8^3
import scala.math.pow
println("#30")
  def getNumber(n:Int) = {
  var magicNumber = 0;
  for (i <- 2 to n){
    for (j <- 2 to 50){
      var powNum = BigInt(i) pow j
      if (powNum.toString().map(_.getNumericValue).sum == i){
        if(i > magicNumber){
        magicNumber = i}
      }
    }
  }
  magicNumber
}
println(getNumber(10))

// 31
// Список в качестве элементов содержит кортежи типа: (n, s), где n
// — целые числа, а s — строки. Получить два списка из элементов исходного,
// выбирая в первый числа, а во второй строки из кортежей.
println("#31")
def getListsFromTuple(list: List[(Int, String)]) = {
  val ints = list.map((n,_) => n)
  val strings = list.map((_,s) => s)
  (ints,strings)
}
println(getListsFromTuple(List((1,"abc"),(2,"zxc"),(3,"asd"))))