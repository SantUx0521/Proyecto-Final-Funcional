/**
 * Plantilla para pruebas
* @author Carlos Delgado
* @version 1.0
 */
package proyecto

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestApp extends AnyFunSuite{
    test("testTaller4"){
        assert("Proyecto final" == App.saludo())
    }
}
