package scala

/**
 * Created by altair on 02.04.16.
 */

import java.util.Scanner;

//Lab 5 Var 3 task 6
object MainObject {
    def nextStep(l: Int, n: Int, tau: Double, h: Double, U: (Int, Int) => Double ) : Double = {
        U(l, n) + (tau / h) / 6 * (2 * U(l - 3, n) - 9 * U(l - 2, n) + 18 * U(l - 1, n) - 11 * U(l, n)) +
        myMath.sqr(tau / h) / 2 * (- U(l - 3, n) + 4 * U(l - 2, n) - 5 * U(l - 1, n) + 2 * U(l, n)) -
        myMath.qube(tau / h) / 6 * (- U(l - 3, n) + 3 * U(l - 2, n) - 3 * U(l - 1, n) + U(l, n)) +
        tau * Math.cos(h * l) + tau * tau * Math.sin(h * l) / 2 - myMath.qube(tau) / 6 * Math.cos(h * l)
    }

    def nextStep13(l: Int, n: Int, tau: Double, h: Double, U: (Int, Int) => Double ) : Double = {
        U(l, n) + tau / h / 6 * (1 + tau / 2 + tau * tau / 6) * math.exp(tau * n) * (2 * U(l + 3, n) - 9 * U(l + 2, n) + 18 * U(l + 1, n) - 11 * U(l, n)) +
        myMath.sqr(tau / h) / 2 * (1 + tau) * math.exp(2 * tau * n) * (- U(l + 3, n) + 4 * U(l + 2, n) - 5 * U(l + 1, n) + 2 * U(l, n)) +
        myMath.qube(tau / h) / 6 * math.exp(3 * tau * n) * (U(l + 3, n) - 3 * U(l + 2, n) + 3 * U(l + 1, n) - U(l, n)) +
        tau * (tau * n + tau / 2)
    }

    def nextStep8(l: Int, n: Int, tau: Double, h: Double, U: (Int, Int) => Double ) : Double = {
        U(l, n) + (tau / h) / 3 * (2 * U(l - 3, n) - 9 * U(l - 2, n) + 18 * U(l - 1, n) - 11 * U(l, n)) +
            myMath.sqr(tau / h) * 2 * (- U(l - 3, n) + 4 * U(l - 2, n) - 5 * U(l - 1, n) + 2 * U(l, n)) -
            myMath.qube(tau / h) * 4 / 3 * (- U(l - 3, n) + 3 * U(l - 2, n) - 3 * U(l - 1, n) + U(l, n))
    }

    val a8 = new AnaliticalFunction(
        f = (t, x) => 2
    )

    val b8 = new AnaliticalFunction()

    val fi8 = new AnaliticalFunction(
        f = (t, x) => Math.cos(x),
        dfdx = (t, x) => -Math.sin(x),
        d2fdx2 = (t, x) => -Math.cos(x)
    )

    val psi8 = new AnaliticalFunction(
        f = (t, x) => Math.cos(2 * t),
        dfdt = (t, x) => -2 * Math.sin(2 * t),
        d2fdx2 = (t, x) => -4 * Math.cos(2 * t),
        d3fdt3 = (t, x) => 8 * Math.sin(2 * t)
    )

    def main(args : Array[String]): Unit = {

        val a = new AnaliticalFunction(f = (t, x) => 1);
        val b = new AnaliticalFunction(
            f = (t, x) => Math.cos(x),
            dfdx = (t, x) => -Math.sin(x),
            d2fdx2 = (t, x) => -Math.cos(x)
        );
        val fi = new AnaliticalFunction(
            f = (t, x)  => Math.log(1 + x * x) + Math.sin(x),
            dfdx = (t, x) => (2 * x) / (1 + x * x) + Math.cos(x),
            d2fdx2 = (t, x) => (2 * (1 - x * x)) / myMath.sqr(1 + x * x) - Math.sin(x)
        );
        val psi = new AnaliticalFunction(
            f = (t, x)  => Math.log(1 + t * t),
            dfdt = (t, x) => 2 * t / (1 + t * t),
            d2fdt2 = (t, x) => (2 * (1 - myMath.sqr(t))) / myMath.sqr(1 + t * t),
            d3fdt3 = (t, x) => (4 * myMath.qube(t) - 12 * t) / myMath.qube(1 + t * t)
        );

        val scan = new Scanner(System.in);
        print("Input L, K:");
        val L: Int = scan.nextInt();
        val K: Double = scan.nextDouble();
        val N: Int = (L / K).toInt;
        val task = new Task(N, L, a, b, fi, psi, nextStep);
        var answer = new Answer;
        answer.addAnaliticalFunction((x, t) => Math.sin(x) + Math.log(1 + myMath.sqr(x - t)) );
        //answer.addAnaliticalFunction((x, t) => Math.cos(x - 2 * t))
        //answer.addAnaliticalFunction((x, t) => myMath.sqr(x + math.exp(t)) + t * t / 2)
        answer.addNumericalAnswerList(task.solve());
        answer.dump;
        //Устойчивость L / N принадлежит к (0, 1/2) || (1 , 2) || (5/2, 3)
    }
}
