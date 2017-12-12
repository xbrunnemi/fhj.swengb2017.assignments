package at.fhj.swengb.apps.calculator

import scala.util.Try

/**
  * Companion object for our reverse polish notation calculator.
  */
object RpnCalculator {

  /**
    * Returns empty RpnCalculator if string is empty, otherwise pushes all operations
    * on the stack of the empty RpnCalculator.
    *
    * @param s a string representing a calculation, for example '1 2 +'
    * @return
    */
  def apply(s: String): Try[RpnCalculator] = {
    try{
      if(s.isEmpty){
        Try(RpnCalculator())
      } else {
        val list = s.split(' ')
        //Try(list.foldLeft(RpnCalculator(Nil))((acc,str)=> RpnCalculator(acc.stack :+ Op(str))))
        val emptyCal = RpnCalculator(Nil)
        emptyCal.push(list.toSeq.map(Op.apply))
      }
    }
    catch{
      case fail: Exception => Try[RpnCalculator] (throw fail)
    }


  }

}

/**
  * Reverse Polish Notation Calculator.
  *
  * @param stack a datastructure holding all operations
  */
case class RpnCalculator(stack: List[Op] = Nil) {

  /**
    * By pushing Op on the stack, the Op is potentially executed. If it is a Val, it the op instance is just put on the
    * stack, if not then the stack is examined and the correct operation is performed.
    *
    * @param op
    * @return
    */
  def push(op: Op): Try[RpnCalculator] ={
    op match {
      case Val(x) => {
        println(RpnCalculator(stack:+op).toString)
        Try(RpnCalculator(stack :+ op))
      }
      case _ => {
        if(stack.size >= 2) {
          val res: Val = {
            op match {
              case Add => {
                Add.eval(stack.init.last match {
                  case Val(x) => Val(x)
                  case _ => throw new NoSuchElementException("Addition nicht möglich")
                }, stack.last match {
                  case Val(x) => Val(x)
                  case _ => throw new NoSuchElementException("Addition nicht möglich")
                })
              }
              case Sub => {
                Sub.eval(stack.init.last match {
                  case Val(x) => Val(x)
                  case _ => throw new NoSuchElementException("Subtration nicht möglich")
                }, stack.last match {
                  case Val(x) => Val(x)
                  case _ => throw new NoSuchElementException("Subtration nicht möglich")
                })
              }
              case Mul => {
                Mul.eval(stack.init.last match {
                  case Val(x) => Val(x)
                  case _ => throw new NoSuchElementException("Multiplikation nicht möglich")
                }, stack.last match {
                  case Val(x) => Val(x)
                  case _ => throw new NoSuchElementException("Multiplikation nicht möglich")
                })
              }
              case Div => {
                Div.eval(stack.init.last match {
                  case Val(x) => Val(x)
                  case _ => throw new NoSuchElementException("Division nicht möglich")
                }, stack.last match {
                  case Val(x) => Val(x)
                  case _ => throw new NoSuchElementException("Division nicht möglich")
                })
              }
            }
          }
          println(Try(RpnCalculator(stack.init.init :+ res)).toString)
          Try(RpnCalculator(stack.init.init :+ res))
        }
        else Try(throw new NoSuchElementException)
      }
    }

  }

  /**
    * Pushes val's on the stack.
    *
    * If op is not a val, pop two numbers from the stack and apply the operation.
    *
    * @param op
    * @return
    */
  def push(op: Seq[Op]): Try[RpnCalculator] ={
    def pushRec(calcu:RpnCalculator, list:Seq[Op]): Try[RpnCalculator] = {
      if(list.size>0){
        val a = calcu.push(list.head)
        pushRec(a.get,list.tail)
      }
      else
        Try(calcu)
    }
    pushRec(this,op)
  }

  /**
    * Returns an tuple of Op and a RevPolCal instance with the remainder of the stack.
    *
    * @return
    */
  def pop(): (Op, RpnCalculator) = (peek(), RpnCalculator(stack.init))

  /**
    * If stack is nonempty, returns the top of the stack. If it is empty, this function throws a NoSuchElementException.
    *
    * @return
    */
  def peek(): Op = if(stack.size == 0) throw new NoSuchElementException else stack.last

  /**
    * returns the size of the stack.
    *
    * @return
    */
  def size: Int = stack.size
}