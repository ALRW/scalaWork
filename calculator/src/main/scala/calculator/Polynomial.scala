package calculator

import scala.math.sqrt

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal {
      a() * a() - 4 * b() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      delta() match {
        case x if x < 0 => Set.empty
        case _ => Set(
          (-1 * b() + sqrt(delta())) / 2 * a(),
          (-1 * b() - sqrt(delta())) / 2 * a()
        )
      }
    }
  }
}
