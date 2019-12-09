package chisel3.util.experimental

import chisel3._
import chisel3.util._

// Connect the fields of the two bundles, a and b, together using the trait c.
// The directionality is a := b
// Usage:
//    connectTrait(classOf(traitA), a, b)
object connectTrait {
  def apply(c: Class[_], a: Bundle, b: Bundle) = {

    val cA = a.getClass
    val cB = b.getClass

    // Just to make sure that both the classes actually implement the interface
    // we are trying to connect
    val a_match = cA.getInterfaces.map { intf => intf == c }.fold(false)(_||_)
    val b_match = cB.getInterfaces.map { intf => intf == c }.fold(false)(_||_)
    require(a_match && b_match, s"One of the classes does not have the required trait")

    // Filter out for the public members of the trait we want to connect
    val l = c.getMethods.filter { case m => ( m.getParameterTypes.isEmpty && m.getDeclaringClass.isAssignableFrom(c) )}

    // Now hook both of the classes up. It is assumed that the above check will
    // guarantee that the two bundles contain the same field.
    l.foreach { case l =>
      val A = cA.getMethod(l.getName).invoke(a).asInstanceOf[chisel3.Data]
      val B = cB.getMethod(l.getName).invoke(b).asInstanceOf[chisel3.Data]

      A := B
    }
  }
}

//////////////////
//
// Some test code for development
//
//////////////////

// trait traitA {
//     val fnord = Bool()
// }
// 
// class A extends Bundle with traitA {
//   val foo = Bool()
// }
// 
// class B extends Bundle with traitA {
//   val bar = Bool()
// }
// 
// 
// class Test extends Module {
//   val io = IO ( new Bundle{
//       val b = Input(new B)
//       val a = Output(new A)
//   } )
// 
//   io.a.foo   := DontCare
// 
//   connectTrait(classOf[traitA], io.a,io.b)
// 
// }
// 
// object Test extends App {
//       chisel3.Driver.execute(args, () => new Test)
// }
