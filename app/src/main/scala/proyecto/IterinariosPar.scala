import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq

class ItinerariosPar() {
  type Aeropuertos = List[Aeropuerto]
  type Vuelos = List[Vuelo]
  type Itinerario = List[Vuelo]

  def itinerariosPar(vuelos: Vuelos, aeropuertos: Aeropuertos): (String, String) => List[Itinerario] = {
    (cod1: String, cod2: String) => {
      def buscarItinerarios(origen: String, destino: String, visitados: Set[String]): List[Itinerario] = {
        if (origen == destino) {
          List(List())
        } else {
          vuelos.par.filter(v => v.Org == origen && !visitados.contains(v.Dst)).flatMap { vuelo =>
            buscarItinerarios(vuelo.Dst, destino, visitados + origen).map(vuelo :: _)
          }.toList
        }
      }
      buscarItinerarios(cod1, cod2, Set())
    }
  }

  def itinerariosTiempoPar(vuelos: Vuelos, aeropuertos: Aeropuertos): (String, String) => List[Itinerario] = {
    def calcularTiempoTotal(itinerario: Itinerario): Double = {
      var tiempoTotal: Double = 0
      var horaLlegadaAnterior = 0

      for (i <- 0 until itinerario.length) {
        val vuelo = itinerario(i)
        val aeropuertoOrigen = aeropuertos.find(_.Cod == vuelo.Org).getOrElse(Aeropuerto("", 0, 0, 0))
        val aeropuertoDestino = aeropuertos.find(_.Cod == vuelo.Dst).getOrElse(Aeropuerto("", 0, 0, 0))

        val horaSalidaMinutos = vuelo.HS * 60 + vuelo.MS
        val horaLlegadaMinutos = vuelo.HL * 60 + vuelo.ML

        val diferenciaGMT = (aeropuertoDestino.GMT - aeropuertoOrigen.GMT) * 60
        val tiempoVuelo = horaLlegadaMinutos - horaSalidaMinutos - diferenciaGMT

        tiempoTotal += tiempoVuelo

        if (i > 0) {
          val tiempoEspera = horaSalidaMinutos - horaLlegadaAnterior
          tiempoTotal += tiempoEspera
        }

        horaLlegadaAnterior = horaLlegadaMinutos
      }

      tiempoTotal
    }

    def encontrarItinerariosTiempo(cod1: String, cod2: String): List[Itinerario] = {
      def encontrarTodosItinerarios(cod1: String, cod2: String, visitados: Set[String]): List[Itinerario] = {
        if (cod1 == cod2) {
          List()
        } else if (visitados.contains(cod1)) {
          List()
        } else {
          val vuelosDesdeCod1 = vuelos.filter(_.Org == cod1)
          vuelosDesdeCod1.par.flatMap { vuelo =>
            if (vuelo.Dst == cod2) {
              List(List(vuelo))
            } else {
              val nuevosVisitados = visitados + cod1
              val itinerariosRestantes = encontrarTodosItinerarios(vuelo.Dst, cod2, nuevosVisitados)
              itinerariosRestantes.map(vuelo :: _)
            }
          }.toList
        }
      }

      val todosItinerarios = encontrarTodosItinerarios(cod1, cod2, Set())
      todosItinerarios.sortBy(it => calcularTiempoTotal(it))
    }

    (cod1: String, cod2: String) => encontrarItinerariosTiempo(cod1, cod2)
  }

  def itinerariosEscalasPar(vuelos: Vuelos, aeropuertos: Aeropuertos): (String, String) => List[Itinerario] = {
    val aeropuertosMap = aeropuertos.map(a => (a.Cod, a)).toMap

    def encontrarItinerariosEscalas(cod1: String, cod2: String): List[Itinerario] = {
      def contarEscalas(itinerario: Itinerario): Int = {
        itinerario.map(_.Esc).sum + (itinerario.size - 1)
      }

      val todosItinerarios = itinerariosPar(vuelos, aeropuertos)(cod1, cod2)
      todosItinerarios.sortBy(it => contarEscalas(it)).take(3)
    }

    (cod1: String, cod2: String) => {
      if (!aeropuertosMap.contains(cod1) || !aeropuertosMap.contains(cod2)) {
        List()
      } else {
        encontrarItinerariosEscalas(cod1, cod2)
      }
    }
  }

  def itinerariosAirePar(vuelos: Vuelos, aeropuertos: Aeropuertos): (String, String) => List[Itinerario] = {
    def calcularDuracionVuelo(vuelo: Vuelo): Int = {
      val aeropuertoOrigen = aeropuertos.find(_.Cod == vuelo.Org).get
      val aeropuertoDestino = aeropuertos.find(_.Cod == vuelo.Dst).get

      val salidaEnMinutos = (vuelo.HS * 60) + vuelo.MS
      val llegadaEnMinutos = (vuelo.HL * 60) + vuelo.ML

      val diferenciaGMT = (aeropuertoDestino.GMT - aeropuertoOrigen.GMT) / 100
      val diferenciaGMTEnMinutos = (diferenciaGMT * 60).toInt

      val duracionEnMinutos = llegadaEnMinutos - (salidaEnMinutos + diferenciaGMTEnMinutos)

      if (duracionEnMinutos < 0) duracionEnMinutos + 1440 else duracionEnMinutos
    }

    def calcularTiempoTotal(itinerario: Itinerario): Int = {
      itinerario.map(v => calcularDuracionVuelo(v)).sum
    }

    def encontrarItinerariosAire(cod1: String, cod2: String): List[Itinerario] = {
      itinerariosPar(vuelos, aeropuertos)(cod1, cod2).par.toList.sortWith {
        case (it1, it2) =>
          val tiempoTotal1 = calcularTiempoTotal(it1)
          val tiempoTotal2 = calcularTiempoTotal(it2)
          tiempoTotal1 < tiempoTotal2
      }.take(3)
    }

    (cod1: String, cod2: String) => encontrarItinerariosAire(cod1, cod2)
  }

  def itinerariosSalidaPar(vuelos: Vuelos, aeropuertos: Aeropuertos): (String, String, Int, Int) => List[Itinerario] = {
    val buscarItinerariosFn = itinerariosPar(vuelos, aeropuertos)

    def convertirAMinutos(hora: Int, minutos: Int): Int = {
      hora * 60 + minutos
    }

    def calcularLapsoTiempo(horaLlegada: Int, horaCita: Int): Int = {
      val diferencia = horaCita - horaLlegada
      if (diferencia >= 0) diferencia else 1440 + diferencia
    }

    def esValido(itinerario: Itinerario, tiempoCita: Int): Boolean = {
      val horaLlegada = convertirAMinutos(itinerario.last.HL, itinerario.last.ML)
      horaLlegada <= tiempoCita || (horaLlegada < 1440 && tiempoCita < horaLlegada)
    }

    def encontrarItinerariosSalida(cod1: String, cod2: String, horaCita: Int, minCita: Int): List[Itinerario] = {
      val tiempoCita = convertirAMinutos(horaCita, minCita)
      val todosItinerarios = buscarItinerariosFn(cod1, cod2).par
      val itinerariosValidos = todosItinerarios.filter(it => esValido(it, tiempoCita)).toList

      val itinerariosOrdenados = itinerariosValidos.sortWith {
        case (it1, it2) =>
          val horaLlegada1 = convertirAMinutos(it1.last.HL, it1.last.ML)
          val horaLlegada2 = convertirAMinutos(it2.last.HL, it2.last.ML)

          val lapsoTiempo1 = calcularLapsoTiempo(horaLlegada1, tiempoCita)
          val lapsoTiempo2 = calcularLapsoTiempo(horaLlegada2, tiempoCita)

          if (lapsoTiempo1 == lapsoTiempo2) {
            val horaSalida1 = convertirAMinutos(it1.head.HS, it1.head.MS)
            val horaSalida2 = convertirAMinutos(it2.head.HS, it2.head.MS)
            horaSalida1 < horaSalida2
          } else {
            lapsoTiempo1 < lapsoTiempo2
          }
      }

      itinerariosOrdenados.take(3)
    }

    (cod1: String, cod2: String, horaCita: Int, minCita: Int) => encontrarItinerariosSalida(cod1, cod2, horaCita, minCita)
  }
}

// Clases auxiliares (Aeropuerto y Vuelo) para el funcionamiento del código
case class Aeropuerto(Cod: String, HS: Int, MS: Int, GMT: Int)
case class Vuelo(Org: String, Dst: String, HS: Int, MS: Int, HL: Int, ML: Int, Esc: Int)

