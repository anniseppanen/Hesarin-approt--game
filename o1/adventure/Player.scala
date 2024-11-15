package o1.adventure

import scala.collection.mutable.Map

/** A `Player` object represents a player character controlled by the real-life user
  * of the program.
  *
  * A player object’s state is mutable: the player’s location and possessions can change,
  * for instance.
  *
  * @param startingArea  the player’s initial location */
class Player(startingArea: Area):

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  private val items = Map[String, Item]("passi" -> Item("passi", "0"))
  
  /** Ottaa ylös tiedon siitä, mistä juoma on otettu. Baaritiskiltä ei voi lähteä, mikäli juomaa ei ole juotu. 
  * Kun juoma on juotu, baari = None ja tiskiltä voi lähteä. */
  private var baari: Option[Area] = None       
  
  def helppiä =
    "Käskyt: kävele {suunta}, ota {juoma}, juo {juoma}, passi, juomat, pokaa, kumppani"
  
  def has(itemName: String) = items.contains(itemName)
  
  /** Tällä käskyllä pelaaja voi tarkistaa, montako leimaa passissa on ts. montako juomaa hän on juonut. Passi-käskyllä 
    * selviää myös se, kuinka monta juomaa tarvitaan, että passi on täynnä. */
  def passi =
      "Passissa on " + items("passi").description + " leimaa.\nTarvitsen yhteensä kolme leimaa."
      
  /** Tällä käskyllä pelaaja voi tarkistaa, minkä juoman hän on ottanut. */
  def juoma =
    var juomingit = ""
    for nimi <- items.keys do 
      if nimi == "kalja" || nimi == "siideri" || nimi == "lonkero" || nimi == "shotti" then
        juomingit += nimi
    if juomingit.isEmpty then "Ei juomia"
    else juomingit

  /** Tällä käskyllä pelaaja liikkuu eri paikkoihin. Käsky on muotoa kävele {suunta} */
  def kävele(direction: String) =
    val destination = this.location.neighbor(direction)
    val suunta =
      if direction == "pohjoinen" then "pohjoiseen"
      else if direction == "itä" then "itään"
      else if direction == "etelä" then "etelään"
      else "länteen"
    if destination.isDefined && baari.isEmpty then
      this.currentLocation = destination.getOrElse(this.currentLocation)
      s"Kävelen $suunta."
    else if baari.isDefined then "Juoma pitää juoda ensin!"
    else s"Et voi mennä $suunta, siellä ei ole mitään kiinnostavaa."

  /** Tällä käskyllä pelaaja ottaa juoman baarimikolta, mutta ei vielä juo sitä. */
  def ota(juoma: String) =
    val otettu = location.removeItem(juoma)
    for juominki <- otettu do 
      items.put(juominki.name, juominki)
    if otettu.isDefined then
      baari = Some(location)
      "Kiitos juomasta!"
    else
      "En voi ottaa sitä."
      
  /** Pelaajan täytyy juoda juoma baaritiskillä, jotta baarimikko voi antaa pelaajalle leiman appropassiin. */
  def juo(juoma: String) =
    val juotu = items.remove(juoma)
    if juotu.isDefined && baari.isDefined then
      items("passi").description = (items("passi").description.toInt + 1).toString
      baari = None
      "Hyvvvvää!\nBaarimikko: Ja passia leimaan!"
    else
      "Ei pysty ei kykene."
  
  /** Tätä metodia käytetään muissa metodeissa hyväksi. */
  private def onkoKumppani = items.contains("kumppani")
  
  /** Tällä käskyllä pelaaja voi tarkistaa, onko hänellä jo kumppani. */
  def kumppani = if onkoKumppani then "Löytyy!" else "Ei vielä..."
  
  /** Tällä käskyllä yritetään saada kumppani mukaan. Käytännössä tämä tarkoittaa, että se lisätään items-listaan, mikäli pokaus onnistuu.
  * Pokaus onnistuu, jos juotuja juomia on kaksi ja pelaajalla ei ole aikaisempaa kumppania. Jos juotuja juomia on yksi, ei itsevarmuus
  * vielä riitä pokaamiseen. Jos juotuja juomia on kolme tai neljä, on pelaaja liian huppelissa pokaamiseen. */
  def pokaa =
    val pokattu = location.removeItem("Potentiaalinen kumppani")
    if pokattu.isDefined then
      if passi == "2" && !onkoKumppani then
        items.put("kumppani", Item("kumppani", ""))
        "Oisko mittää approhommii?\nPotentiaalinen kumppani: Tietenkin, oi ihanaa!"
      else if passi == "1" || passi == "0" then
        location.addItem(Item("Potentiaalinen kumppani", ""))
        "Itsevarmuus ei vielä riitä lähestymään potentiaalista kumppania... Täytyy juoda lisää."
      else if (passi == "3" || passi == "4") && !onkoKumppani then
        location.addItem(Item("Potentiaalinen kumppani", ""))
        "Lähtissshitkhö kentthies apprrhoilemaan ja sitten kothhiin kansshani?\nPotentiaalinen kumppani: En todellakaan noin huppelisen kanssa, phyi!"
      else
        location.addItem(Item("Potentiaalinen kumppani", ""))
        "Oisko mittää approhommii?\nPotentiaalinen kumppani: Sinulla on jo seuralainen, kauhea yritys!"
    else
      "Ei täällä ole ketään pokattavaa..."

  def hasQuit = this.quitCommandGiven

  def location = this.currentLocation

  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() =
    this.quitCommandGiven = true
    ""

  /** Returns a brief description of the player’s state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name

end Player

