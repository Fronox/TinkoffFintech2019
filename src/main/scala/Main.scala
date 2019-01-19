object Main {

  def testTask1(): Unit = {
    println(Stream.continually(scala.io.StdIn.readLine()).takeWhile(x => x != "" && x != null).map(x => x.toInt).sum)
  }

  def task1(): Unit = {
    val number: Int = scala.io.StdIn.readInt()
    val res: Int = (1 to number).count(x => x.toString == x.toString.reverse)
    println(res)
  }

  def task2(): Unit = {
    val str = scala.io.StdIn.readLine()
    val res = for(ch: Char <- str.distinct) yield {
      if(str.count(c => c == ch) > 1)
        ch
      else ""
    }
    println(res.mkString(""))
  }

  def string2Map(pol: String): Map[Int, Int] = {
    val s = pol.replaceAll("-", ".-").replaceAll("""\+""", ".")
    val sp = s.split("\\.")
    sp.filterNot(x => x == "").map{
      case str: String if str.contains("x^") =>
        val splittedStr = str.split("x\\^").map {
          case "" => 1
          case "-" => -1
          case s: String => s.toInt
        }
        (splittedStr(1), splittedStr(0))
      case "x" => (1, 1)
      case str: String if str.contains("x") =>
        val coef = str.split("x")(0) match {
          case "" => 1
          case "-" => -1
          case s: String => s.toInt
        }
        (1, coef)
      case str: String =>
        (0, str.toInt)
    }.toMap
  }

  def polMult(xs1: Map[Int, Int], xs2: Map[Int, Int]): Map[Int, Int] = {
    xs1.toList.flatMap{
      case (pow1, coef1) =>
        xs2.toList.map{
          case (pow2, coef2) =>
            (pow1 + pow2, coef1 * coef2)
        }
    }.groupBy(x => x._1).map(x => (x._1, x._2.map(l => l._2).sum))
  }

  def printPol(xs: Map[Int, Int]): Unit = {
    val sortedXs = xs.toList.sortBy(x => -x._1)

    val resHead: String = sortedXs.head match {
      case (0, coef) =>
        if (coef == 0) ""
        else coef.toString
      case (1, coef) =>
        if (coef == 0) ""
        else if (coef == 1) "x"
        else if (coef == -1) """-""" + "x"
        else coef.toString + "x"
      case (pow, coef) if pow > 1 =>
        if (coef == 0) ""
        else if (coef == 1) "x^" + pow.toString
        else if (coef == -1) "-" + "x^" + pow.toString
        else coef.toString + "x^" + pow.toString
    }

    val resTail: List[String] = sortedXs.tail.map{
      case (0, coef) =>
        if(coef == 0) ""
        else if(coef > 0) """+""" + coef.toString
        else coef.toString
      case (1, coef) =>
        if(coef == 0) ""
        else if(coef == 1) """+""" + "x"
        else if(coef == -1) "-" + "x"
        else if(coef > 0) """+""" + coef.toString + "x"
        else coef.toString + "x"
      case (pow, coef) if pow > 1 =>
        if(coef == 0) ""
        else if(coef == 1) """+""" + "x^" + pow.toString
        else if(coef == -1) "-" + "x^" + pow.toString
        else if (coef > 0) """+""" + coef.toString + "x^" + pow.toString
        else coef.toString + "x^" + pow.toString
    }

    println((resHead :: resTail).mkString(""))
  }

  def task3(): Unit = {
    val pol1: String = scala.io.StdIn.readLine()
    val pol2: String = scala.io.StdIn.readLine()

    val xs1: Map[Int, Int] = string2Map(pol1)
    val xs2: Map[Int, Int] = string2Map(pol2)

    val resXs = polMult(xs1, xs2)

    printPol(resXs)
  }

  def iter5(currPair: (Int, Int), rest: List[(Int, Int)], count: Int): Int = {
    val i0 = currPair._1
    val j0 = currPair._2

    val inter = rest.exists {
      case (i, j) =>
        i0 >= i && i0 <= j || j0 >= i && j0 <= j || i0 <= i && j <= j0
    }
    val newCount = if (inter) count - 1 else count

    if(rest.isEmpty)
      newCount
    else
      iter5(rest.head, rest.tail, newCount)
  }

  def task5(): Unit = {
    val len = scala.io.StdIn.readInt
    val dataCount = scala.io.StdIn.readInt
    val dataList = (0 until dataCount).map {n =>
      val Array(i, j) = scala.io.StdIn.readLine().split(" ").map(x => x.toInt)
      (i, j)
    }.toList

    val res = iter5(dataList.head, dataList.tail, dataCount)

    println(res)

  }

  def iter4(origStr: String, currStr: String, accSol: List[String]): List[String] = {
    if(origStr.isEmpty)
      currStr :: accSol
    else{
      origStr.head match {
        case '(' => iter4(origStr.tail, currStr + "(", accSol)
        case ')' => iter4(origStr.tail, currStr + ")", accSol)
        case '?' =>
          val newSol = iter4(origStr.tail, currStr + "(", accSol)
          iter4(origStr.tail, currStr + ")", newSol ::: accSol)
      }
    }
  }

  def isOk(str: String, open: Int): Boolean = {
    if (str.isEmpty)
      open == 0
    else str.head match {
      case '(' => isOk(str.tail, open + 1)
      case ')' => isOk(str.tail, open - 1)
    }
  }

  def task4(): Unit = {
    val data = scala.io.StdIn.readLine()
    println(iter4(data, "", List()).filter(x => x.contains("?")).count(x => isOk(x, 0)))
  }

  def iter7(x: Int, y: Int, curSol: List[String], solList: List[List[String]]): List[List[String]] = {
    if(x == y)
      curSol :: solList
    else {
      val solListS1: List[List[String]] = if(y - 3 > 0) {
        val step1Res = y - 3
        val updCurSol = "s1" :: curSol
        if(step1Res >= x)
          iter7(x, step1Res, updCurSol, solList)
        else
          solList
      }
      else List()

      val solListS2: List[List[String]]  = if (y % 4 == 0) {
        val step2Res = y / 4
        val updCurSol = "s2" :: curSol
        if(step2Res >= x)
          iter7(x, step2Res, updCurSol, solList)
        else solList
      }
      else List()

      solListS1 ::: solListS2 ::: solList
    }
  }

  def task7(): Unit = {
    val Array(x, y) = scala.io.StdIn.readLine().split(" ").map(el => el.toInt)
    val res = iter7(x, y, List(), List())
    if(res.isEmpty)
      println("-1")
    else {
      val sortedRes = res.sortBy(x => x.size)
      println(sortedRes.head.size)
    }
  }

  def main(args: Array[String]): Unit = {
    task3()
  }
}
