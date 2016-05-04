package scala

/**
 * Created by altair on 02.04.16.
 */
class CompulationGrid(Ll: Int, Nn: Int) {
    private val L = Ll;
    private val N = Nn;
    private val Data  = new Array[ Array[Double] ](N)
        //Data.foreach((var elem: Array[Double]) => elem = new Array[Double](L))
    for(n <- 0 until N)
        this.Data(n) = new Array[Double](L);

    def setValue(n: Int, l: Int, Value: Double) = {
        this.Data(n)(l) = Value
    }

    def getValue(n: Int, l: Int) = this.Data(n)(l)

}
