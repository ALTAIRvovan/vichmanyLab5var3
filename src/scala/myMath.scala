package scala

/**
 * Created by altair on 03.04.16.
 */
object myMath {
    @inline def sqr(x: Double):Double = x * x
    @inline def qube(x: Double):Double = x * x * x
}
