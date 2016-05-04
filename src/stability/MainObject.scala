package stability

import math.{sin, cos, pow}

/**
  * Created by altair on 03.05.16.
  */
object MainObject {

    def f(x: Double, k: Double ) : Double = {
        math.sqrt(
            math.pow(
                1 / 6 * x * x * x *
                    (math.pow(math.sin(k), 3) - 3 * math.sin(k) * math.pow(math.cos(k) - 1, 2))
                + x * x *
                    (-1 / 2 * math.pow(math.sin(k), 3) + (5 * math.sin(k)) / 2
                    + 3 / 2 * math.sin(k) * math.pow(math.cos(k), 2) - 4 * math.sin(k) * math.cos(k))
                + x *
                    (1 / 6 * (7 * math.sin(k) - 2 * sin(2 * k)) * (cos(k) - 1) -
                        1 / 6 * sin(k) * (-7 * cos(k) + 2 * cos(2 * k) + 11)), 2)+
            math.pow(
                1 / 6 * pow(x, 3) * (pow(cos(k) - 1, 3) - 3 * pow(sin(k), 2) * (cos(k) - 1)) +
                    x * x * (pow(sin(k), 2) * (cos(k) - 1) + (1 - cos(k) / 2) * (pow(cos(k) - 1, 2) - pow(sin(k), 2))) +
                x * (1 / 6 * sin(k) * (7 * sin(k) - 2 * sin(2 * k)) + 1 / 6 * (cos(k) - 1) * (-7 * cos(k) + 2 * cos(2 * k) + 11)) + 1,2)
        )
    }

    def f1(x:Double, k:Double):Double = {
        math.sqrt(
            pow(
                1/6 * pow(x,3) * (pow(sin(k), 3) - 3 * sin(k) * pow(1 - cos(k), 2)) +
                1/2 * pow(x, 2) * (5 * sin(k) - 4 * sin(2 * k) + sin(3 * k))+
                1/6 * x * (-18 * sin(k) + 9 * sin(2*k) - 2 * sin(3 * k))
            ,2)
                +
            pow(
                1/6 * pow(x, 3) * (3 * pow(sin(k), 2) * (1 - cos(k)) - pow(1 - cos(k), 3)) +
                1/2 * pow(x, 2) * (-5 * cos(k) + 4 * cos(2 * k) - cos(3 * k) + 2) +
                1/6 * x * (18 * cos(k) - 9 * cos(2 * k) + 2 * cos(3 * k) - 11) +
                1
            , 2)
        )
    }

    def main( args: Array[String] ) : Unit = {
        val mul = 1
        for(x1 <- (10 * mul).toInt to (20 * mul).toInt by 1) {
            val x = x1.toDouble / 10 / mul // tau / h
            var clear:Boolean = true
            for(k1 <- -100000 to 100000 if clear) {
                val k = k1.toDouble / 100
                if(f(x, k) > 1) {
                    println("error: x=" + x + " k=" + k + " f=" , f(x, k))
                    clear = false
                }
            }
            if(clear)
                println(x)
        }
    }
}
