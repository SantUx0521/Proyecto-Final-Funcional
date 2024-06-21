package proyecto

import scala.collection.mutable
import proyecto.Aeropuerto

class Itinerario() {

  // Alias for lists of airports and flights
  type Aeropuertos = List[Aeropuerto]
  type Vuelos = List[Vuelo]
  type Itinerario = List[Vuelo]

  // Function to find all possible itineraries between two airports
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

  // Function to find itineraries sorted by total flight time
  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {

    def gmtOffset(cod: String): Double = {
      aeropuertos.find(_.Cod.equalsIgnoreCase(cod)).map(_.GMT).getOrElse(0.0)
    }

    def calcularTiempoVuelo(vuelo: Vuelo, aeropuerto: List[Aeropuerto]): Int = {
      val gmtOrg = gmtOffset(vuelo.Org)
      val gmtDst = gmtOffset(vuelo.Dst)
      val horaSalidaGMT = vuelo.HS * 60 + vuelo.MS - ((gmtOrg / 100.0) * 60).toInt
      val horaLlegadaGMT = vuelo.HL * 60 + vuelo.ML - ((gmtDst / 100.0) * 60).toInt
      val duracionVuelo = horaLlegadaGMT - horaSalidaGMT
      if (duracionVuelo < 0) duracionVuelo + 1440 else duracionVuelo
    }

    def calcularTiempoTotal(itinerario: List[Vuelo], aeropuerto: List[Aeropuerto]): Int = {
      itinerario match {
        case Nil => 0
        case vuelo :: Nil => calcularTiempoVuelo(vuelo, aeropuerto)
        case vuelo1 :: vuelo2 :: tail =>
          calcularTiempoVuelo(vuelo1, aeropuerto) +
            calcularTiempoEspera(vuelo1, vuelo2, aeropuerto) +
            calcularTiempoTotal(vuelo2 :: tail, aeropuerto)
      }
    }

    def calcularTiempoEspera(vuelo1: Vuelo, vuelo2: Vuelo, aeropuerto: List[Aeropuerto]): Int = {
      val horaLlegada = vuelo1.HL * 60 + vuelo1.ML
      val horaSalida = vuelo2.HS * 60 + vuelo2.MS
      if (horaSalida < horaLlegada) (24 * 60 - horaLlegada) + horaSalida
      else horaSalida - horaLlegada
    }

    (cod1: String, cod2: String) => {
      val allItineraries = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      val allItinerariesTime = allItineraries.map(itinerary => (itinerary, calcularTiempoTotal(itinerary, aeropuertos)))
      allItinerariesTime.sortBy(_._2).map(_._1).take(3)
    }
  }


  // Function to find itineraries sorted by number of stops
  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {

    val aeropuertosMap = aeropuertos.map(a => (a.Cod, a)).toMap

    def encontrarItinerariosEscalas(cod1: String, cod2: String): List[List[Vuelo]] = {
      def contarEscalas(itinerario: List[Vuelo]): Int = {
        itinerario.map(_.Esc).sum + (itinerario.size - 1)
      }

      val todosItinerarios = itinerarios(vuelos, aeropuertos)(cod1, cod2)
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

  // Function to find itineraries ordered by air time without any specific order
  def itinerariosAire(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {

    def calcularDuracionVuelo(vuelo: Vuelo): Int = {
      val aeropuertoOrigen = aeropuertos.find(_.Cod == vuelo.Org).get
      val aeropuertoDestino = aeropuertos.find(_.Cod == vuelo.Dst).get

      val salidaEnMinutos = vuelo.HS * 60 + vuelo.MS
      val llegadaEnMinutos = vuelo.HL * 60 + vuelo.ML

      val diferenciaGMT = (aeropuertoDestino.GMT - aeropuertoOrigen.GMT) / 100
      val diferenciaGMTEnMinutos = (diferenciaGMT * 60).toInt

      val duracionEnMinutos = llegadaEnMinutos - salidaEnMinutos - diferenciaGMTEnMinutos

      if (duracionEnMinutos < 0) duracionEnMinutos + 1440 else duracionEnMinutos
    }

    def calcularTiempoTotal(itinerario: List[Vuelo]): Int = {
      itinerario.map(calcularDuracionVuelo).sum
    }

    (cod1: String, cod2: String) => {
      def minimoAire(cod1: String, cod2: String): List[List[Vuelo]] = {
        val allItineraries = itinerarios(vuelos, aeropuertos)(cod1, cod2)
        val allItinerariesTime = allItineraries.map(it => (it, calcularTiempoTotal(it)))
        allItinerariesTime.sortBy(_._2).map(_._1).take(3)
      }

      minimoAire(cod1, cod2)
    }
  }

  // Function to find itinerarios based on departure time
  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => List[Vuelo] = {

    val buscarItinerariosFn = itinerarios(vuelos, aeropuertos)

    def convertirAMinutos(hora: Int, minutos: Int): Int = {
      hora * 60 + minutos
    }

    def calcularLapsoTiempo(horaLlegada: Int, horaCita: Int): Int = {
      val diferencia = horaCita - horaLlegada
      if (diferencia >= 0) diferencia else 1440 + diferencia
    }

    def esValido(itinerario: List[Vuelo], tiempoCita: Int): Boolean = {
      val horaLlegada = convertirAMinutos(itinerario.last.HL, itinerario.last.ML)
      horaLlegada <= tiempoCita || (horaLlegada < 1440 && tiempoCita < horaLlegada)
    }

    (origen: String, destino: String, horaCita: Int, minCita: Int) => {
      val tiempoCita = convertirAMinutos(horaCita, minCita)
      val todosItinerarios = buscarItinerariosFn(origen, destino)
      val itinerariosValidos = todosItinerarios.filter(it => esValido(it, tiempoCita))

      val itinerariosOrdenados = itinerariosValidos.sortBy { it =>
        val horaLlegada = convertirAMinutos(it.last.HL, it.last.ML)
        val lapsoTiempo = calcularLapsoTiempo(horaLlegada, tiempoCita)
        val horaSalida = convertirAMinutos(it.head.HS, it.head.MS)
        (lapsoTiempo, horaSalida)
      }

      itinerariosOrdenados.headOption.getOrElse(List.empty)
    }
  }
}