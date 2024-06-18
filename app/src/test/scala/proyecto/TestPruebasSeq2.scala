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
class TestPruebasSeq2 extends AnyFunSuite{

  val itoObj = new Itinerario()
  val itsCurso = itoObj.itinerariosTiempo(vuelosCurso , aeropuertosCurso)

  test("test1"){
      val its1 = itsCurso ( "MID" , "SVCS" )
      assert(its1 === List ( List ( Vuelo ("AIRVZLA", 601 ,"MID", 5 , 0 , "SVCS", 6 , 0 , 0 ) ) ) )
  }

  test("test2"){
      val its2 = itsCurso ( "CLO" , "SVCS" )
      assert(its2 === List ( ) )
  }

  test("test3") {
    val its3 = itsCurso ( "CLO" , "SVO" )
    assert(its3 === List(
      List ( Vuelo ("AVA", 9432 ,"CLO", 7 , 0 ,"SVO", 2 , 20 , 4 ) ) ,
      List ( Vuelo ("AVA", 9432 ,"CLO", 7 , 0 ,"BOG", 8 , 0 , 0 ) ,
        Vuelo ("IBERIA" , 505 ,"BOG", 18 , 0 ,"MAD", 12 , 0 , 0 ) ,
        Vuelo ("IBERIA" , 506 ,"MAD", 14 , 0 ,"SVO", 23 , 20 , 0 ) ) ,
      List ( Vuelo ("AVA", 9432 ,"CLO", 7 , 0 ,"BOG", 8 , 0 , 0 ) ,
        Vuelo ( "IBERIA" , 505 ,"BOG", 18 , 0 ,"MAD", 12 , 0 , 0 ) ,
        Vuelo ( "IBERIA" , 507 ,"MAD", 16 , 0 ,"SVO", 1 , 20 , 0 ) ) ,
      )
    )
  }

  test("test4") {
    val its4 = itsCurso ( "CLO" , "MEX" )
    assert(its4 === List(
        List ( Vuelo ("AVA", 9432 ,"CLO", 7 , 0 ,"BOG", 8 , 0 , 0 ) , 
              Vuelo ("VIVA", 756 ,"BOG", 9 , 0 ,"MDE", 10 , 0 , 0 ) ,  
              Vuelo ("VIVA", 769 ,"MDE", 11 , 0 ,"BAQ", 12 , 0 , 0 ) , 
              Vuelo ("AVA", 5643 ,"BAQ", 14 , 0 ,"MEX", 16 , 0 , 0 ) ),
        List ( Vuelo ("AVA", 9432 ,"CLO", 7 , 0 ,"BOG", 8 , 0 , 0 ) ,
              Vuelo ("LATAM", 787 ,"BOG", 17 , 0 ,"MEX", 19 , 0 , 0 ) )
        )
    )
  }

  test("test5") {
    val its5 = itsCurso ( "CTG" , "PTY" )
    assert(its5 === List(
        List(Vuelo("COPA", 1234, "CTG", 10, 0, "PTY", 11, 30, 0)),
        List(Vuelo("AVA", 4321, "CTG", 9, 30, "SMR", 10, 0, 0)), Vuelo("COPA", 7631, "SMR", 10, 50, "PTY", 11, 50, 0)
      )
    )
  }
}
