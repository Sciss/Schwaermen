def totalLen(maxLen: Double, numSpk: Int, spc0: Double, init: Double): Double = {
  val spc = if (spc0 == 0) {
    val x = maxLen / numSpk
    println(s"spc = $x")
    x
  } else spc0
    
  def loop(rem: Int, res: Double): Double =
    if (rem == 0) res
    else loop(rem - 1, res + rem * spc)
    
  loop(numSpk, res = init /* 0.0 */)
}

val arm1 = 270 + 290 + 330               // 890
val arm2 = 200 + 110 + 150 + 345 + 220   // 1025
val arm3 = 235 + 255 + 300 + 230         // 1020

val armsSum = arm1 + arm2 + arm3         // 2935
val totalNumSpk = 108
val cmPerSpk = armsSum / totalNumSpk.toDouble
val numSpk1 = arm1 / cmPerSpk // 32.75
val numSpk2 = arm2 / cmPerSpk // 37.72
val numSpk3 = arm3 / cmPerSpk // 37.53
numSpk1 + numSpk2 + numSpk3

val numSpk1m = 33
val numSpk2m = 38
val numSpk3m = 37

assert(numSpk1m+numSpk2m+numSpk3m == totalNumSpk)

val totalLen1 = totalLen(arm1, numSpk1m, spc0 = 27, init = 0) // 27cm
val totalLen2 = totalLen(arm2, numSpk2m, spc0 = 27, init = 0) // 27cm
val totalLen3 = totalLen(arm3, numSpk3m, spc0 = 27, init = 0) // 27.5cm
val totalTotal = totalLen1 + totalLen2 + totalLen3
val totalMetres = (totalTotal / 100).ceil.toInt  // 542 metres

///////////

val totalLen1 = totalLen(arm1, numSpk1m, spc0 = 28, init = 28)
val totalLen2 = totalLen(arm2, numSpk2m, spc0 = 28, init = 28)
val totalLen3 = totalLen(arm3, numSpk3m, spc0 = 28, init = 28)=
val totalTotal = totalLen1 + totalLen2 + totalLen3
val totalMetres = (totalTotal / 100).ceil.toInt  // 563 metres

//
val arm2a = 200 + 110 + /* 150 + */ 345 + 220
val numSpk2a = arm2a / cmPerSpk // 37.72
val arm2b = 200 + 110
val numSpk2b = arm2b / cmPerSpk // 37.72
