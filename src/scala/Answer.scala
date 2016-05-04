package scala

import scala.collection.mutable

/**
 * Created by altair on 03.04.16.
 */
class Answer {

    private var analitical: mutable.MutableList[Double] = null;
    private var numerical : mutable.MutableList[Double] = null;

    def addAnaliticalFunction(f: (Double, Double) => Double): Unit = {
        analitical = new mutable.MutableList[Double];
        for(l <- 0 to 10) {
            analitical += f(l * 0.1, 1);
        }
    }

    def addNumericalAnswerList(answer: mutable.MutableList[Double]): Unit = {
        numerical = answer;
    }

    def dump = {
        var max: Double = Math.abs(analitical.head - numerical.head);
        for(x <- 0 to 10) {
            val dx = analitical(x) - numerical(x)
            printf("%3.2f | %2.8f | %2.8f | %e \n", x * 0.1, analitical(x), numerical(x), dx);
            if(Math.abs(dx) > max) {
                max = Math.abs(dx)
            }
        }
        println("Max delta: " + max);
    }
}
