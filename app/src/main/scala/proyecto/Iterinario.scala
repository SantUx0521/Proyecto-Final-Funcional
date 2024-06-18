package proyecto

class Itinerario() {

  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]
  type Itinerario = List[Vuelo]

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    def buscarItinerarios(origen: String, destino: String, visitados: Set[String]): List[Itinerario] = {
      if (origen == destino) {
        List(List())
      } else {
        vuelos.filter(v => v.Org == origen && !visitados.contains(v.Dst)).flatMap { vuelo =>
          buscarItinerarios(vuelo.Dst, destino, visitados + origen).map(vuelo :: _)
        }
      }
    }

    (cod1: String, cod2: String) => buscarItinerarios(cod1, cod2, Set.empty)
  }

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    def encontrarItinerariosTiempo(cod1: String, cod2: String): List[Itinerario] = {
      def calcularTiempoTotal(itinerario: Itinerario): Int = {
        val primerVuelo = itinerario.head
        val ultimoVuelo = itinerario.last
        ultimoVuelo.HL * 60 + ultimoVuelo.ML - (primerVuelo.HS * 60 + primerVuelo.MS)
      }

      def encontrarTodosItinerarios(cod1: String, cod2: String, visitados: Set[String]): List[Itinerario] = {
        if (cod1 == cod2) {
          List()
        } else if (visitados.contains(cod1)) {
          List()
        } else {
          val vuelosDesdeCod1 = vuelos.filter(_.Org == cod1)
          vuelosDesdeCod1.flatMap { vuelo =>
            if (vuelo.Dst == cod2) {
              List(List(vuelo))
            } else {
              val nuevosVisitados = visitados + cod1
              val itinerariosRestantes = encontrarTodosItinerarios(vuelo.Dst, cod2, nuevosVisitados)
              itinerariosRestantes.map(vuelo :: _)
            }
          }
        }
      }

      val todosItinerarios = encontrarTodosItinerarios(cod1, cod2, Set())

      todosItinerarios.sortBy(calcularTiempoTotal).take(3)
    }

    (cod1: String, cod2: String) => encontrarItinerariosTiempo(cod1, cod2)
  }

  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    val aeropuertosMap = aeropuertos.map(a => (a.Cod, a)).toMap

    def encontrarItinerariosEscalas(cod1: String, cod2: String): List[Itinerario] = {
      def contarEscalas(itinerario: Itinerario): Int = itinerario.size - 1

      def encontrarTodosItinerarios(cod1: String, cod2: String, visitados: Set[String]): List[Itinerario] = {
        if (cod1 == cod2) {
          List()
        } else if (visitados.contains(cod1)) {
          List()
        } else {
          val vuelosDesdeCod1 = vuelos.filter(_.Org == cod1)
          vuelosDesdeCod1.flatMap { vuelo =>
            if (vuelo.Dst == cod2) {
              List(List(vuelo))
            } else {
              val nuevosVisitados = visitados + cod1
              val itinerariosRestantes = encontrarTodosItinerarios(vuelo.Dst, cod2, nuevosVisitados)
              itinerariosRestantes.map(vuelo :: _)
            }
          }
        }
      }

      val todosItinerarios = encontrarTodosItinerarios(cod1, cod2, Set())

      todosItinerarios.sortBy(contarEscalas).take(3)
    }

    (cod1: String, cod2: String) => {
      if (!aeropuertosMap.contains(cod1) || !aeropuertosMap.contains(cod2)) {
        List()
      } else {
        encontrarItinerariosEscalas(cod1, cod2)
      }
    }
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    def encontrarItinerariosAire(cod1: String, cod2: String): List[Itinerario] = {
      def encontrarTodosItinerarios(cod1: String, cod2: String, visitados: Set[String]): List[Itinerario] = {
        if (cod1 == cod2) {
          List()
        } else if (visitados.contains(cod1)) {
          List()
        } else {
          val vuelosDesdeCod1 = vuelos.filter(_.Org == cod1)
          vuelosDesdeCod1.flatMap { vuelo =>
            if (vuelo.Dst == cod2) {
              List(List(vuelo))
            } else {
              val nuevosVisitados = visitados + cod1
              val itinerariosRestantes = encontrarTodosItinerarios(vuelo.Dst, cod2, nuevosVisitados)
              itinerariosRestantes.map(vuelo :: _)
            }
          }
        }
      }

      encontrarTodosItinerarios(cod1, cod2, Set())
    }

    (cod1: String, cod2: String) => encontrarItinerariosAire(cod1, cod2)
  }

  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[Itinerario] = {
    def convertirAMinutos(hora: Int, minutos: Int): Int = hora * 60 + minutos

    def buscarItinerarios(cod1: String, cod2: String, horaLlegada: Int): List[Itinerario] = {
      def buscar(codigo: String, vuelosRestantes: List[Vuelo], itinerarioActual: Itinerario, horaActual: Int): List[Itinerario] = {
        if (codigo == cod2) {
          List(itinerarioActual)
        } else {
          vuelosRestantes.filter(v => v.Org == codigo && convertirAMinutos(v.HL, v.ML) <= horaActual).flatMap { vuelo =>
            val nuevaHoraActual = convertirAMinutos(vuelo.HL, vuelo.ML)
            buscar(vuelo.Dst, vuelosRestantes.filterNot(_ == vuelo), itinerarioActual :+ vuelo, nuevaHoraActual)
          }
        }
      }

      val vuelosPosibles = vuelos.filter(v => convertirAMinutos(v.HL, v.ML) <= horaLlegada)
      val resultados = buscar(cod1, vuelosPosibles, List(), horaLlegada)

      resultados.sortBy(it => -convertirAMinutos(it.head.HS, it.head.MS)).take(3)
    }

    (cod1: String, cod2: String, hora: Int, minutos: Int) => {
      val horaLlegada = convertirAMinutos(hora, minutos)
      buscarItinerarios(cod1, cod2, horaLlegada)
    }
  }
}

