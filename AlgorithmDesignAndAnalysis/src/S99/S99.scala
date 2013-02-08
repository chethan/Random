package S99

 class S99 {
  def last[A](ls:List[A]):A = ls match {
    case h :: Nil => h
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }

  def penultimate[A](ls:List[A]):A = ls match{
    case h::_::Nil => h
    case _ :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }

  def findNth[A](n:Int,ls:List[A]):A = (n,ls) match {
    case (0,h::_) => h
    case (m,_::tail) => findNth(m-1,tail)
    case (_,_) => throw new NoSuchElementException
  }

  def count[A](ls:List[A]):Int = {
    def countTemp[A](acc:Int,curlist:List[A]):Int = curlist match {
      case Nil =>  acc
      case _::tail => countTemp(acc+1,tail)
    }
    countTemp(0,ls)
  }

   def reverse[A](ls:List[A]):List[A] = {
     def reverseTemp[A](revls:List[A],curls:List[A]):List[A] = curls match {
       case Nil => revls
       case h::tail=> reverseTemp(h::revls,tail)
     }
     reverseTemp(Nil,ls)
   }

   def isPalindrome[A](ls:List[A]):Boolean ={
     ls==reverse(ls)
   }

   def flatten(ls:List[Any]):List[Any]=ls flatMap {
     case h:List[_]=>flatten(h)
     case e=> List(e)
   }

   def compress[A](ls:List[A]):List[A] = {
     def compressTemp[A](compls:List[A],curls:List[A]):List[A] = (compls,curls) match{
       case (_,Nil)=>reverse(compls)
       case (Nil,h::tail)=>compressTemp(h::compls,tail)
       case (h1::tail1,h2::tail2) if h1==h2 => compressTemp(h1::tail1,tail2)
       case (h1::tail1,h2::tail2)  => compressTemp(h2::h1::tail1,tail2)
       case (_,_) => throw new NoSuchElementException
     }
     compressTemp(Nil,ls)
   }

}
