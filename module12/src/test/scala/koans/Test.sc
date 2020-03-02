def fib(num : Int) : Long = {

  def loop(num : Int, next : Long = 1L, acc : Long = 0L) : Long = {
    num match {
      case 0 => acc
      case x => {
        println(x +  " " + next +  " " + acc )
        loop(x - 1, acc + next, next)
      }
    }
  }

  loop(num)
}

fib(5)
fib(20)


class ObscureMathUtil {
  def fib(num: Int): Long =
    num match {
      case 0 => 0L
      case 1 => 1L
      case x => {
        println(x + " " + (x - 1) + " " + (x - 2))
        fib(x - 1) + fib(x - 2)
      }
    }

}

val output = new ObscureMathUtil().fib(4)