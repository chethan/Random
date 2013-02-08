package algo

class Multiplication {
  def simpleRecursive(a:Int,b:Int):Int ={
     def simpleRecursiveTemp(ls1:List[Int],ls2:List[Int]):Int = (ls1,ls2) match {
       case (h1::Nil,h2::Nil)=>h1*h2
       case (list1,list2) => {
         val half= list1.size/2
         val h1l1=list1.take(half)
         val h2l1=list1.drop(half)
         val h1l2=list2.take(half)
         val h2l2=list2.drop(half)
         Math.pow(10,list1.size).toInt*simpleRecursiveTemp(h1l1,h1l2)+ Math.pow(10,half).toInt*(simpleRecursiveTemp(h2l1,h1l2)+simpleRecursiveTemp(h1l1,h2l2))+ simpleRecursiveTemp(h2l1,h2l2)
       }

  }
   simpleRecursiveTemp(a.toString.map(_.asDigit).toList,b.toString.map(_.asDigit).toList)
  }

  def kartusba(a:Int,b:Int):Int ={
    def kartusbaTemp(ls1:List[Int],ls2:List[Int]):Int = (ls1,ls2) match {
      case (h1::Nil,h2::Nil)=>h1*h2
      case (list1,list2) => {
        val half= list1.size/2
        val h1l1=list1.take(half)
        val h2l1=list1.drop(half)
        val h1l2=list2.take(half)
        val h2l2=list2.drop(half)
        val mult1: Int =  kartusbaTemp(h1l1, h1l2)
        val mult2: Int = kartusbaTemp(h2l1, h2l2)
        val temp: Int = kartusbaTemp(toDigits(toNumber(h1l1) + toNumber(h2l1)), toDigits(toNumber(h1l2) + toNumber(h2l2))) - mult1 - mult2
        Math.pow(10, list1.size).toInt * mult1+ Math.pow(10, half).toInt * temp+ mult2
      }

    }
    kartusbaTemp(a.toString.map(_.asDigit).toList,b.toString.map(_.asDigit).toList)
  }

  def toNumber(ls:List[Int]):Int ={
    ls.foldLeft(0)((r,c)=>r*10+c)
  }

  def toDigits(i:Int):List[Int] = {
    i.toString.map(_.asDigit).toList
  }
}
