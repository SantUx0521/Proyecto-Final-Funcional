package proyecto

class Itinerario() {

  // Alias para listas de aeropuertos y vuelos
  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]
  type Itinerario = List[Vuelo]

  // Función para encontrar todos los itinerarios posibles entre dos aeropuertos
  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    // Función interna recursiva para buscar itinerarios
    def buscarItinerarios(origen: String, destino: String, visitados: Set[String]): List[Itinerario] = {
      if (origen == destino) {
        List(List())  // Caso base: si el origen es igual al destino, retornar lista vacía
      } else {
        // Filtrar vuelos que salen del aeropuerto de origen y aún no han sido visitados
        vuelos.filter(v => v.Org == origen && !visitados.contains(v.Dst)).flatMap { vuelo =>
          // Llamada recursiva para continuar buscando itinerarios desde el destino actual del vuelo
          buscarItinerarios(vuelo.Dst, destino, visitados + origen).map(vuelo :: _)
        }
      }
    }

    // Retornar función que toma dos códigos de aeropuerto y retorna la lista de itinerarios
    (cod1: String, cod2: String) => buscarItinerarios(cod1, cod2, Set.empty)
  }

  // Función para encontrar itinerarios ordenados por tiempo total de vuelo
  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    // Función interna para encontrar itinerarios y ordenarlos por tiempo total de vuelo
    def encontrarItinerariosTiempo(cod1: String, cod2: String): List[Itinerario] = {

      // Función para calcular el tiempo total de un itinerario
      def calcularTiempoTotal(itinerario: Itinerario): Int = {
        val primerVuelo = itinerario.head
        val ultimoVuelo = itinerario.last
        // Calcular la diferencia en minutos entre la salida del primer vuelo y la llegada del último vuelo
        ultimoVuelo.HL * 60 + ultimoVuelo.ML - (primerVuelo.HS * 60 + primerVuelo.MS)
      }

      // Función recursiva para encontrar todos los itinerarios entre dos aeropuertos
      def encontrarTodosItinerarios(cod1: String, cod2: String, visitados: Set[String]): List[Itinerario] = {
        if (cod1 == cod2) {
          List()  // Si el origen es igual al destino, no hay itinerarios
        } else if (visitados.contains(cod1)) {
          List()  // Evitar ciclos en la búsqueda de itinerarios
        } else {
          val vuelosDesdeCod1 = vuelos.filter(_.Org == cod1)
          vuelosDesdeCod1.flatMap { vuelo =>
            if (vuelo.Dst == cod2) {
              List(List(vuelo))  // Itinerario directo encontrado
            } else {
              val nuevosVisitados = visitados + cod1
              val itinerariosRestantes = encontrarTodosItinerarios(vuelo.Dst, cod2, nuevosVisitados)
              itinerariosRestantes.map(vuelo :: _)  // Agregar vuelo actual al itinerario restante
            }
          }
        }
      }

      // Encontrar todos los itinerarios posibles entre cod1 y cod2
      val todosItinerarios = encontrarTodosItinerarios(cod1, cod2, Set())

      // Ordenar los itinerarios por tiempo total y tomar los primeros tres (o menos si hay menos de tres)
      todosItinerarios.sortBy(calcularTiempoTotal).take(3)
    }

    // Función externa que se devuelve para obtener itinerarios según el tiempo de vuelo
    (cod1: String, cod2: String) => encontrarItinerariosTiempo(cod1, cod2)
  }

  // Función para encontrar itinerarios ordenados por número de escalas
  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    // Crear un mapa de aeropuertos para búsqueda rápida por código
    val aeropuertosMap = aeropuertos.map(a => (a.Cod, a)).toMap

    // Función interna para encontrar itinerarios y ordenarlos por número de escalas
    def encontrarItinerariosEscalas(cod1: String, cod2: String): List[Itinerario] = {

      // Función auxiliar para contar el número de escalas en un itinerario
      def contarEscalas(itinerario: Itinerario): Int = itinerario.size - 1

      // Función recursiva para encontrar todos los itinerarios entre dos aeropuertos
      def encontrarTodosItinerarios(cod1: String, cod2: String, visitados: Set[String]): List[Itinerario] = {
        if (cod1 == cod2) {
          List()  // Si el origen es igual al destino, no hay itinerarios
        } else if (visitados.contains(cod1)) {
          List()  // Evitar ciclos en la búsqueda de itinerarios
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

      // Encontrar todos los itinerarios posibles entre cod1 y cod2
      val todosItinerarios = encontrarTodosItinerarios(cod1, cod2, Set())

      // Filtrar y ordenar los itinerarios según el número de escalas
      todosItinerarios.sortBy(contarEscalas).take(3)
    }

    // Función externa que se devuelve para obtener itinerarios según el número de escalas
    (cod1: String, cod2: String) => {
      if (!aeropuertosMap.contains(cod1) || !aeropuertosMap.contains(cod2)) {
        List()  // Si alguno de los aeropuertos no existe, retornar una lista vacía
      } else {
        encontrarItinerariosEscalas(cod1, cod2)
      }
    }
  }

  // Función para encontrar itinerarios aéreos sin ninguna ordenación específica
  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    // Función interna para encontrar itinerarios
    def encontrarItinerariosAire(cod1: String, cod2: String): List[Itinerario] = {

      // Función recursiva para encontrar todos los itinerarios entre dos aeropuertos
      def encontrarTodosItinerarios(cod1: String, cod2: String, visitados: Set[String]): List[Itinerario] = {
        if (cod1 == cod2) {
          List()  // Si el origen es igual al destino, no hay itinerarios
        } else if (visitados.contains(cod1)) {
          List()  // Evitar ciclos en la búsqueda de itinerarios
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

      // Encontrar todos los itinerarios posibles entre cod1 y cod2
      encontrarTodosItinerarios(cod1, cod2, Set())
    }

    // Función externa que se devuelve para obtener itinerarios
    (cod1: String, cod2: String) => encontrarItinerariosAire(cod1, cod2)
  }

  // Función para encontrar itinerarios según la hora de salida
  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[Itinerario] = {

    // Función para convertir horas y minutos en minutos totales
    def convertirAMinutos(hora: Int, minutos: Int): Int = hora * 60 + minutos

    // Función interna para buscar itinerarios dentro del tiempo disponible
    def buscarItinerarios(cod1: String, cod2: String, horaLlegada: Int): List[Itinerario] = {

      // Función recursiva para buscar itinerarios desde un aeropuerto dado
      def buscar(codigo: String, vuelosRestantes: List[Vuelo], itinerarioActual: Itinerario, horaActual: Int): List[Itinerario] = {
        if (codigo == cod2) {
          return List(itinerarioActual)
        }

        // Filtrar vuelos que salen del aeropuerto actual y que llegan antes de la hora actual
        vuelosRestantes.filter(v => v.Org == codigo && convertirAMinutos(v.HL, v.ML) <= horaActual)
          .flatMap { vuelo =>
            val nuevaHoraActual = convertirAMinutos(vuelo.HL, vuelo.ML)
            buscar(vuelo.Dst, vuelosRestantes.filterNot(_ == vuelo), itinerarioActual :+ vuelo, nuevaHoraActual)
          }
      }

      // Filtrar vuelos que llegan antes de la hora especificada
      val vuelosPosibles = vuelos.filter(v => convertirAMinutos(v.HL, v.ML) <= horaLlegada)
      val resultados = buscar(cod1, vuelosPosibles, List(), horaLlegada)

      // Filtrar y seleccionar el itinerario que tiene la hora de salida más tarde posible
      resultados.filter(it => convertirAMinutos(it.last.HL, it.last.ML) <= horaLlegada)
        .sortBy(it => -convertirAMinutos(it.head.HS, it.head.MS)).take(3)
    }

    // Función externa que se devuelve para obtener itinerarios según la hora de llegada
    (cod1: String, cod2: String, hora: Int, minutos: Int) => {
      val horaLlegada = convertirAMinutos(hora, minutos)
      buscarItinerarios(cod1, cod2, horaLlegada)
    }
  }
}