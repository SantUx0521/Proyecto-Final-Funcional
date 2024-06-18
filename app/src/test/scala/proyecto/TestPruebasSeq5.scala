/**
 * Plantilla para pruebas
* @author Carlos Delgado
* @version 1.0
 */

package proyecto
import datos._

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestPruebasSeq5 extends AnyFunSuite{

  val itoObj = new Itinerario()
  val itsCurso = itoObj.itinerariosSalida(vuelosCurso , aeropuertosCurso)

  test("test 1") {
    val its1 = itsCurso("CTG", "PTY", 11, 40)
    assert(its1 === List(List(Vuelo("COPA", 1234, "CTG", 10, 0, "PTY", 11, 30, 0))))
  }

  test ("test 2") {
    val its2 = itsCurso("CTG", "PTY", 11, 55)
    assert(its2 === List(
      List(Vuelo("AVA", 4321, "CTG", 9, 30, "SMR", 10, 0, 0), Vuelo("COPA", 7631, "SMR", 10, 50, "PTY", 11, 50, 0))
    ))
  }

  test ("test 3") {
    val its3 = itsCurso("CTG", "PTY", 12, 30)
    assert(its3 === List(
      List(Vuelo("AVA", 4321, "CTG", 9, 30, "SMR", 10, 0, 0), Vuelo("COPA", 7631, "SMR", 10, 50, "PTY", 11, 50, 0))
    ))
  }
}
