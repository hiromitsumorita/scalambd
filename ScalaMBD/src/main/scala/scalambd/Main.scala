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

  /* 現在の時刻 */
  def current_time(xs:Array[Array[Double]]) = {
    xs.toList.last(0)
  }

  /* 現在の値 */
  def current_value(xs:Array[Array[Double]]) = {
    xs.toList.last(1)
  }

  /* 次周期に x に変化 */
  def next(xs:Array[Array[Double]], x:Double) = {
    val xl = xs.toList
    val t_last = xl.last(0)
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

  def print(y:Array[Double], t:Double) = {
    val n = (t / step).toInt
    y(n)
  }

  def main(args:Array[String]): Unit = {
    hello
  }
}

object TimeSeries {
  val step = 0.01

  /* Initializing TimeSeries */
  /* 初期値 */
  def init(x0:Double) = {
    Array(Array(0.0,x0))
  }

  /* Current Time */
  /* 現在の時刻 */
  def current_time(xs:Array[Array[Double]]) = {
    xs.toList.last(0)
  }

  /* Current Value */
  /* 現在の値 */
  def current_value(xs:Array[Array[Double]]) = {
    xs.toList.last(1)
  }

  /* appending next value */
  /* 次周期に x に変化 */
  def next(xs:Array[Array[Double]], x:Double) = {
    val xl = xs.toList
    val t_last = xl.last(0)
    val x_last = xl.last(1)
    val xl_ret = xl ++ List(Array(t_last + step, x))
    xl_ret.toArray
  }

  /* appending event */
  def event(xs:Array[Array[Double]], x:Double) = {
    val xl = xs.toList
    val t_last = xl.last(0)
    val x_last = xl.last(1)
    val xl_ret = xl ++ List(Array(t_last + step, x), Array(t_last + step * 2.0, x_last))
    xl_ret.toArray
  }

  /* keeping value for dt seconds */
  /* dt sec 間値を保持 */
  def keep(xs:Array[Array[Double]], dt : Double) = {
    val xl = xs.toList
    val t_last = xl.last(0)
    val x_last = xl.last(1)

    val n_head = ((t_last + step) / step).toInt
    val n_tail = ((t_last + dt + step) / step).toInt
    val x_keep = if (n_head < n_tail) {
      Range(n_head,n_tail).toList.map(n => Array(n.toDouble * step, x_last))
    } else {
      List.empty
    }

    val xl_ret = xl ++ x_keep
    xl_ret.toArray
  }


  /* changing value after dt seconds */
  /* dt 経過後に x に変化 */
  def edge(xs:Array[Array[Double]], dt:Double, x:Double) = {
    val xl = xs.toList
    val t_last = xl.last(0)
    val x_last = xl.last(1)

    val n_head = ((t_last + step) / step).toInt
    val n_tail = ((t_last + dt) / step).toInt
    val x_pre = if (n_head < n_tail) {
      Range(n_head,n_tail).toList.map(n => Array(n.toDouble * step, x_last))
    } else {
      List.empty
    }

    val xl_ret = xl ++ x_pre ++ List(Array(t_last + dt,x))
    xl_ret.toArray
  }

  /* appending another timeseries */
  /* 時刻 t に x になる時系列を付加 */
  def append(xs:Array[Array[Double]], t:Double, x:Double) = {
    val xl = xs.toList
    val t_last = xl.last(0)
    val dt = t - t_last
    if (dt >= step) {
      edge(xs,dt,x)
    } else {
      xs
    }
  }

  /* Gain */
  def gain(c : Double, xs:Array[Array[Double]]) = {
    xs.map(x => Array(x(0),c*x(1)))
  }

  /* Bias(Offset) */
  def bias(b : Double, xs:Array[Array[Double]]) = {
    xs.map(x => Array(x(0),x(1)+b))
  }

  /* Unit Delay */
  def z_inv(x0 : Double, xs:Array[Array[Double]]) = {
    val n_last = (xs.toList.last(0) / step).toInt
    val xs_vals = xs.map(x => x(1)).toList
    val z_inv_val = (x0 :: xs_vals.dropRight(1)).toArray
    val samples = Range(0,n_last+1).toList
    samples.map(n => Array(n.toDouble/step, z_inv_val(n)))
  }

  /* Show TimeSeries */
  /* 出力された時系列(配列の値を表示) */
  def print(y:Array[Double], t:Double) = {
    val n = (t / step).toInt
    y(n)
  }

}


object Arith {
  def relu (x:Double) = {
    if (x >= 0.0) x else 0.0
  }

  def abs(x:Double) = {
    if (x >= 0.0) x else -x
  }
} 

class SFunc {
  def relu(x:Double):Double = if (x >= 0.0) x else 0.0
  def abs(x:Double):Double = if (x >= 0.0) x else 0.0
}