package proyecto

// Definición de la clase Aeropuerto con su código, coordenadas y GMT
case class Aeropuerto(Cod: String, X: Int, Y: Int, GMT: Double)

// Definición de la clase Vuelo con información sobre la aerolínea, número de vuelo, horarios, etc.
case class Vuelo(Aln: String, Num: Int, Org: String, HS: Int, MS:Int, Dst: String, HL: Int, ML: Int, Esc:Int)