package o1.adventure

/** The class `Adventure` represents text adventure games. An adventure consists of a player and
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game.
  *
  * N.B. This version of the class has a lot of “hard-coded” information that pertains to a very
  * specific adventure game that involves a small trip through a twisted forest. All newly created
  * instances of class `Adventure` are identical to each other. To create other kinds of adventure
  * games, you will need to modify or replace the source code of this class. */
class Adventure:

  /** the name of the game */
  val title = "Hesarin Approt"
  
  private val kotiAlussa = Area("Koti", "Aika lähteä approilemaan! Hesari näkyy idässä.")
  private val hesari1 = Area("Hesari", "Hesarilla ollaan! Pohjoisessa näkyy Wanha Apteekki ja Hesari jatkuu itään.")
  private val baari1 = Area("Wanha Apteekki", "Wanhassa Apteekissa ollaan! Pohjoisessa on baaritiski.")
  private val hesari2 = Area("Hesari", "Hesarilla ollaan! Etelässä näkyy Tenkka ja Hesari jatkuu itään ja länteen.")
  private val baari2 = Area("Tenkka", "Tenkassa ollaan! Eteleässä näkyy baaritiski.")
  private val hesari3 = Area("Hesari", "Hesarilla ollaan! Pohjoisessa näkyy Lepakkomies, etelässä Relaxing Bar ja Hesari jatkuu länteen.")
  private val baari3 = Area("Lepakkomies", "Lepakkomiehessä ollaan! Pohjoisessa näkyy baaritiski.")
  private val baari4 = Area("Relaxing Bar", "Relaxing Barissa ollaan! Etelässä näkyy baaritiski.")
  private val kotiTyhjinKäsin = Area("Koti", "Kotona, mutta passista puuttuu vielä leimoja eikä kukaan lähtenyt mukaani.")
  private val kotiIlmanPassia = Area("Koti", "Kotona, kumppani mukana, mutta passista puuttuu vielä leimoja.")
  private val kotiIlmanKumppania = Area("Koti", "Kotona, passi täynnä, mutta kukaan ei lähtenyt mukaani.")
  private val kotiLopussa = Area("Koti", "Kotona, kaikki kunnossa!")
  private val baaritiski1 = Area("Baaritiski", "Kaljaa, olkaa hyvä!")
  private val baaritiski2 = Area("Baaritiski", "Lonkeroa, olkaa hyvä!")
  private val baaritiski3 = Area("Baaritiski", "Siideriä, olkaa hyvä!")
  private val baaritiski4 = Area("Baaritiski", "Shottia, olkaa hyvä!")
  
  kotiAlussa. setNeighbors(Vector(                       "itä" -> hesari1                                       ))
  hesari1.    setNeighbors(Vector("pohjoinen" -> baari1, "itä" -> hesari2                                       ))
  baari1.     setNeighbors(Vector("pohjoinen" -> baaritiski1,             "etelä" -> hesari1                    ))
  baaritiski1.setNeighbors(Vector(                                        "etelä" -> baari1                     ))
  hesari2.    setNeighbors(Vector(                      "itä" -> hesari3, "etelä" -> baari2, "länsi" -> hesari1 ))
  baari2.     setNeighbors(Vector("pohjoinen" -> hesari2,                 "etelä" -> baaritiski2                ))
  baaritiski2.setNeighbors(Vector("pohjoinen" -> baari2                                                         ))
  hesari3.    setNeighbors(Vector("pohjoinen" -> baari3,                  "etelä" -> baari4, "länsi" -> hesari2 ))
  baari3.     setNeighbors(Vector("pohjoinen" -> baaritiski3,             "etelä" -> hesari3                    ))
  baaritiski3.setNeighbors(Vector(                                        "etelä" -> baari3                     ))
  baari4.     setNeighbors(Vector("pohjoinen" -> hesari3,                 "etelä" -> baaritiski4                ))
  baaritiski4.setNeighbors(Vector("pohjoinen" -> baari4                                                         ))
  baaritiski1. addItem(Item("kalja", ""))
  baaritiski2. addItem(Item("lonkero", ""))
  baaritiski3. addItem(Item("siideri", ""))
  baaritiski4. addItem(Item("shotti", ""))
  baari1.     addItem(Item("Potentiaalinen kumppani", ""))
  baari2.     addItem(Item("Potentiaalinen kumppani", ""))
  baari3.     addItem(Item("Potentiaalinen kumppani", ""))
  

  /** The character that the player controls in the game. */
  val player = Player(kotiAlussa)

  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0
  /** The maximum number of turns that this adventure game allows before time runs out. */
  val timeLimit = 30


  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = this.player.location == kotiLopussa && player.has("kumppani") && player.passinLeimat == "3"
  
  def leimojaPuuttuu = player.passinLeimat != "3"
  
  def kumppaniPuuttuu = !player.has("kumppani")
  
  
  private def vaihdaKoti(location: Area) = 
    val baarit = Set(baari1, baari2, baari3, baari4)
    val baaritiskit = Set(baaritiski1, baaritiski2, baaritiski3, baaritiski4)
  
    if location == kotiAlussa && leimojaPuuttuu && kumppaniPuuttuu then
      println("leimoja ja kumppani puuttuu")
      hesari1.setNeighbor("länsi", kotiTyhjinKäsin)
      
    if baarit.contains(location) && leimojaPuuttuu && !kumppaniPuuttuu then
      println("leimoja puuttuu ja kumppani on")
      hesari1.deleteNeighbor("länsi")
      hesari1.setNeighbor("länsi", kotiIlmanPassia)
      
    if baaritiskit.contains(location) && !leimojaPuuttuu && kumppaniPuuttuu then
      println("leimoja on ja kumppani puuttuu")
      hesari1.deleteNeighbor("länsi")
      hesari1.setNeighbor("länsi", kotiIlmanKumppania)
    
    if (baarit.contains(location) || baaritiskit.contains(location)) && !leimojaPuuttuu && !kumppaniPuuttuu then
      println("kaikki ok")
      hesari1.deleteNeighbor("länsi")
      hesari1.setNeighbor("länsi", kotiLopussa)
      
  def liikaaLeimoja = player.passinLeimat == "4"
  
  /** Determines whether the player has won, lost, or quit, thereby ending the game. */
  def isOver = this.isComplete || this.player.hasQuit || this.turnCount == this.timeLimit || liikaaLeimoja ||
    player.location == kotiTyhjinKäsin || player.location == kotiIlmanPassia || player.location == kotiIlmanKumppania

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = "Hesarin Approt ovat alkamassa. Tavoitteenasi on kiertää Hesarin baareja, kerätä appropassiin leimoja, " +
    "löytää jostakin baarista mukaan kumppani ja palata lopuksi kotiin. Onnea matkaan!"


  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on whether the player has completed their quest. */
  def goodbyeMessage =
    if this.isComplete then
      "Voitit pelin!"
    else if this.turnCount == this.timeLimit then
      "Et löytänyt kotiin!"
    else if liikaaLeimoja then
      "Joit liikaa... Heräät seuraavana aamuna putkasta..."
    else  // game over due to player quitting
      "Hävisit pelin!"


  /** Plays a turn by executing the given in-game command, such as “go west”. Returns a textual
    * report of what happened, or an error message if the command was unknown. In the latter
    * case, no turns elapse. */
  def playTurn(command: String): String =
    vaihdaKoti(player.location)
    val action = Action(command)
    val outcomeReport = action.execute(this.player)
    if outcomeReport.isDefined then
      this.turnCount += 1
    outcomeReport.getOrElse(s"""En ymmärtänyt käskyä: "$command".""")

end Adventure

