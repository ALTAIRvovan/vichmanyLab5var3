package scala

/**
 * Created by altair on 03.04.16.
 */
class AnaliticalFunction(f: (Double, Double) => Double = (x, y) => 0,
                         dfdt : (Double, Double) => Double = (x, y) => 0,
                         dfdx : (Double, Double) => Double = (x, y) => 0,
                         d2fdx2 : (Double, Double) => Double = (x, y) => 0,
                         d2fdt2 : (Double, Double) => Double = (x, y) => 0,
                         d2fdxdt : (Double, Double) => Double = (x, y) => 0,
                         d3fdt3 : (Double, Double) => Double = (x, y) => 0) {
    def get(t: Double, x: Double) = f(t, x)
    def getDt(t: Double, x: Double) = dfdt(t, x)
    def getDx(t: Double, x: Double) = dfdx(t, x)
    def getD2x(t: Double, x: Double) = d2fdx2(t, x)
    def getDtDx(t: Double, x: Double) = d2fdxdt(t, x)
    def getDt2(t: Double, x: Double) = d2fdt2(t, x)
    def getDt3(t: Double, x: Double) = d3fdt3(t, x)
}
