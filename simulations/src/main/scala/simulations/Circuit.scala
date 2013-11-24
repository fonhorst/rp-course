package simulations

import common._
import scala.math._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val w1,w2,w3 = new Wire
    inverter(a1, w1)
    inverter(a2, w2)
    andGate(w1,w2,w3)
    inverter(w3, output)
  }
  
  def trivialGate(a1:Wire, output:Wire){
     def trivialAction() {
      val a1Sig = a1.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig) }
    }
    a1 addAction trivialAction
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    
    //val inSignal = in.getSignal
    //val controlSignal = for {x <- c} yield x.getSignal
   
    def distributeSignal(in:Wire,ctrls:List[Wire],index:Int=0,position:Int=0):Unit= ctrls match {
//      case head::Nil =>{
//        val out1 = out(index)
//        andGate(in,head,out1)
//        
//        
//        val out2 = out(index + pow(2,position).toInt)
//        val invHead = new Wire
//        inverter(head,invHead)
//        andGate(in,invHead,out2)
//      }
    
    case Nil => trivialGate(in, out(index))
    case head::tail => {
        val out1 = new Wire
        distributeSignal(out1,tail,index + pow(2,position).toInt ,position + 1)
        andGate(in,head,out1)
        
        val out2 = new Wire
        val invHead = new Wire
        distributeSignal(out2,tail,index,position + 1)
        inverter(head,invHead)
        andGate(in,invHead,out2)
      }
    }
    distributeSignal(in,c)
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
