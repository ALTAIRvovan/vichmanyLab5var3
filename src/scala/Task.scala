package scala

import scala.collection.mutable

/**
 * Created by altair on 03.04.16.
 */


class Task(N: Int, L: Int,
           a: AnaliticalFunction,
           b: AnaliticalFunction,
           fi: AnaliticalFunction,
           psi: AnaliticalFunction,
           next: (Int, Int, Double, Double, (Int, Int) => Double ) => Double) //l, n, tau, h, getU(l, n)
{
    private val h = 1.0 / L;
    private val tau = 1.0 / N;
    private var grid = new CompulationGrid(L + 1, N + 1);
    private def isXRound(l : Int) = {
        val x1 = l * h * 10;
        if(Math.abs(x1 - Math.round(x1)) < h / 2)
            true
        else
            false
    }

    def solve(): mutable.MutableList[Double] = {
        println("h=" + h)
        println("tau=" + tau)
        assert(N >= 1);
        assert(L >= 2);
        assert(h > 0);
        assert(tau > 0);
        borderConditions();
        for(l <- 3 to L; n <- 1 to N) {
            grid.setValue(n, l, next(l, n - 1, tau, h, (x, y) => grid.getValue(y, x)))
        }
        var result = new mutable.MutableList[ Double ];
        for(l <- 0 to L if isXRound(l))
            result += grid.getValue(N, l);
        result
    }

    private def borderConditions() = {
        for(l <- 0 to L) {
            grid.setValue(0, l, fi.get(0, l * h));
        }
        for(t <- 1 to N) {
            grid.setValue(t, 0, psi.get(t * tau, 0));
        }
        for(t <- 1 to N) {
            val t1:Double = t * tau
            val u1:Double = (- psi.getDt(t1, 0) + b.get(t1, 0) ) / a.get(t1, 0)
            val u11 = (
                psi.getDt2(t1, 0) +
                (a.getDt(t1, 0) - a.get(t1, 0) * a.getDx(t1, 0)) / a.get(t1, 0) * (b.get(t1, 0) - psi.getDt(t1, 0)) -
                b.getDt(t1, 0) + a.get(t1, 0) * b.getDx(t1, 0) ) /
                myMath.sqr(a.get(t1, 0));
            val u111 = - 1 / myMath.qube(a.get(t1, 0)) * psi.getDt3(t1, 0) +
                3.0 / myMath.sqr(a.get(t1, 0)) * (a.getDt(t1, 0) - a.get(t1, 0) * a.getDx(t1, 0)) * u11 -
                1.0 / myMath.qube(a.get(t1, 0)) * (a.getDt2(t1, 0) - a.get(t1, 0) * a.getDtDx(t1, 0) + myMath.sqr(a.get(t1, 0)) * a.getD2x(t1, 0) -
                    2 * a.getDt(t1, 0) * a.getDx(t1, 0) + a.get(t1, 0) * myMath.sqr(a.getDx(t1, 0))) * u1 +
                1.0 / myMath.qube(a.get(t1, 0)) * (b.getDt2(t1, 0) - a.get(t1, 0) * b.getDtDx(t1, 0) + myMath.sqr(a.get(t1, 0)) * b.getD2x(t1, 0) -
                    2 * a.getDt(t1, 0) * b.getDx(t1, 0) + a.get(t1, 0) * a.getDx(t1, 0) * b.getDx(t1, 0));
            grid.setValue(t, 1, grid.getValue(t, 0) + u1 * h + u11 * myMath.sqr(h) / 2.0 + u111 * myMath.qube(h) / 6.0 );
            grid.setValue(t, 2, grid.getValue(t, 0) + u1 * 2 * h + u11 * 2 * myMath.sqr(h) + u111 * 4 * myMath.qube(h) / 3.0);
        }
    }
}
