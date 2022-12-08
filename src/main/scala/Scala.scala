import scala.io.Source
object Scala extends App {
  val cardsFile = Source.fromFile("src/main/resources/cards.csv")
  val transactionsFile = Source.fromFile("src/main/resources/transactions.csv")

  trait Card {
    val number: Long // vsetko toto budeme vidiet "zvonku" bez toho aby sme vedeli ci je to gold/blue card, inak by trebalo pattern matching
    val cvc: Int
    val owner: String
    val credit: Int
    val limit: Int
    // vytvaranie novej/upravenej karty si musia implementovat karty po svojom, trait nevie kto od neho dedi ani ako vytvorit dedicovu instanciu
    def processInternal(newCredit: Int): Card
    def processTransaction(cvc: Int, amount: Int): Card = {
      if (cvc != this.cvc) { // kontrola cvc, ak zlyha vraciame nezmenenu kartu
        println("wrong cvc")
        this
      } else if ((amount * -1) > limit) { // to iste ako pri cvc len s limitom
        println("above limit")
        this
      } else {
        val newCredit = credit + amount
        println(s"processed transaction for card nr: $number, current credit: $newCredit")
        processInternal(newCredit) // transackia presla, vraciame upravenu kartu
      }
    }
  }
  case class GoldCard(number: Long, cvc: Int, owner: String, credit: Int) extends Card {
    val limit = 1000
    def processInternal(newCredit: Int): Card = this.copy(credit = newCredit)
  }
  case class BlueCard(number: Long, cvc: Int, owner: String, credit: Int) extends Card {
    val limit = 500
    def processInternal(newCredit: Int): Card = this.copy(credit = newCredit)
  }
  // alternativna praca s kartami bez class
  //  val cards1 = cardsFile.getLines().map {line =>
  //    val Array(ctype, numberStr, cvcStr, owner, creditStr) = line.stripMargin.split(",")
  //    val (number, cvc, credit) = (numberStr.toLong, cvcStr.toInt, creditStr.toInt)
  //    (number, cvc, owner, credit, ctype(0))
  //  }.toList
  //
  //  val origCard = cards1.find(_._1 == "00000").get
  //  val newCards1 = cards1.filterNot(_._1 == "00000") :+ (origCard._1, origCard._2, origCard._3, origCard._4 + 1000, origCard._5)
  // vyzbierame karty do mapy s pouzitim foldLeft (tu sa mohlo pouzit aj map alebo for-yield)
  // 1. iteracia dostanete (cards = Map() - prazdna mapa, line - prvy riadok)
  // 2. iteracia dostanete (cards = Map(spracovana prva karta), line - druhy riadok)
  // 3. iteracia dostanete (cards = Map(spracovane prve dve karty, line - treti riadok)
  // Map je tzv. key-value store, ako dictionary v pythone, klucom je cislo karty a hodnotou karta
  val cards = cardsFile.getLines().foldLeft(Map[Long, Card]()) { (cards, line) =>
    line.stripMargin.split(",") match {
      case Array(ctype, numberStr, cvcStr, owner, creditStr) => // vyberame z pola stringov indexy 0,1,2,3,4
        val (number, cvc, credit) = (numberStr.toLong, cvcStr.toInt, creditStr.toInt) // konverzia typov
        val card =
          if (ctype == "g") // podla typu karty vytvorime instanciu prislusnej triedy
            GoldCard(number, cvc, owner, credit)
          else
            BlueCard(number, cvc, owner, credit)
        cards + (card.number -> card) // na konci vratime mapu s pridanou kartou, do dalsej iteracie pride tato mapa ako prvy operand v zatvorke na zaciatku
      case other =>
        println(s"Invalid csv line: $other")
        cards // nemusi tu byt, pretoze vsetky riadky v cards.csv su validne, v pripade ze by riadok nemal 5 stlpcov ako potrebujeme, posuva sa mapa z minulej iteracie
    }
  }
  println(cards)
  val cardsAfterTransactions = transactionsFile.getLines().foldLeft(cards) { (cards, line) =>
    line.split(",") match {
      case Array(cardNumberStr, amountStr, cvcStr) if !cardNumberStr.startsWith("#") => // namatchujeme do premennych stlpce transackie a vyhodime pripadny comment
        (cardNumberStr.toLongOption, cvcStr.toIntOption, amountStr.toIntOption) match { // ak mame spravny typy pokracujeme
          case (Some(cardNumber), Some(cvc), Some(amount)) =>
            cards.get(cardNumber) // ak sme nasli kartu
              .map(foundCard => cards + (foundCard.number -> foundCard.processTransaction(cvc, amount))) // urobime transackiu a posuvame mapu s upravenou kartou
              .getOrElse { // ak sme nenasli kartu, posuvame povodnu mapu, namiesto map/getOrElse ste mohli pouzit klasicky pattern matching
                println("card not found")
                cards
              }
          case _ => // ak nemame spravne typy, posuvame povodnu mapu
            println("Invalid csv col format")
            cards
        }
      case other => // ak nemame spravny pocet stlpcov v riadku, posuvame povodnu mapui
        println(s"Invalid csv line: $line")
        cards
    }
  }
  println(cardsAfterTransactions)
}