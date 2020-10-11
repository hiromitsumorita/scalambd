package com.scalambd

object Main {
  val step = 0.01

  def hello: Unit = {
    println("Hello, This is TimeSeriesEdit.")
  }

  /* 初期値 */
  def init(x0:Double) = {
    Array(Array(0.0,x0))
  }
  
  /* 次周期に x に変化 */
  def next(xs:Array[Array[Double]], x:Double) = {
    val xl = xs.toList
    val t_last = xl.last(0)
    val x_last = xl.last(1)

    val xl_ret = xl ++ List(Array(t_last + step, x))
    xl_ret.toArray
  }

  /* dt 経過後に x に変化 */
  def after(xs:Array[Array[Double]], dt:Double, x:Double) = {
    val xl = xs.toList
    val t_last = xl.last(0)
    val x_last = xl.last(1)

    val x_pre = if (x_last != x) {
      List(Array(t_last + dt - step, x_last))
    }
    else {
      List.empty
    }

    val xl_ret = xl ++ x_pre ++ List(Array(t_last + dt,x))
    xl_ret.toArray
  }

  def append(xs:Array[Array[Double]], t:Double, x:Double) = {
    val xl = xs.toList
    val t_last = xl.last(0)
    val x_last = xl.last(1)

    val x_pre = if (t_last < t - 0.1 && x_last != x) {
      List(Array(t-0.1, x_last))
    }
    else {
      List.empty
    }

    val xl_ret = xl ++ x_pre ++ List(Array(t,x))
    xl_ret.toArray
  }

  def watch(y:Array[Double], t:Double) = {
    val n = (t / step).toInt
    y(n)
  }


  def main(args:Array[String]): Unit = {
    hello
  }
}


object EdgeCounter {
  def countup(n : Int, sw_p : Int, sw_c : Int) = {
    if (sw_p == 0 && sw_c != 0) {
      n+1
    } else {
      n
    }
  }

  def reset(n : Int, reset : Int) = {
    if (reset != 0) {
      n
    } else {
      0
    }
  }
}
