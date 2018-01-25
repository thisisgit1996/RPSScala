import scala.io.StdIn.readLine
import scala.util.Random

class RPSService {

  var rCount1 = 0
  var pCount1 = 0
  var sCount1 = 0
  var rCount2 = 0
  var pCount2 = 0
  var sCount2 = 0
  var computer1Wins = 0
  var computer2Wins = 0
  var playerWins = 0
  var result = ""
  var computer1Turn = ""
  var computer2Turn = ""
  var random = 0.0

  def startPVsAI(): Unit = {
    println("Hello and welcome to RPS Scala player vs AI.")
    var playerTurn = validate(readLine("Type end to finish the game, or type r for rock, p for paper, or s for scissors for your turn:"))
    while (playerTurn != "end") {
      computer1Turn = takeTurn1()
      result = determineResultPVsAI(playerTurn, computer1Turn.charAt(0).toString)
      result match {
        case "n" => println("It's a draw!")
        case "c" => println("Computer wins!"); computer1Wins += 1
        case "p" => println("Player wins!"); playerWins += 1
        case _ => println("Error! result doesn't match!!")
      }
      receive1(playerTurn)
      println("Current score is computer: " + computer1Wins + " player: " + playerWins)
      playerTurn = validate(readLine("\nType end to finish the game, or type r for rock, p for paper, or s for scissors for your next turn:"))
    }

  }

  def validate(choice: String): String = {
    choice.toLowerCase() match {
      case "r" => "r"
      case "rock" => "r"
      case "p" => "p"
      case "paper" => "p"
      case "s" => "s"
      case "scissors" => "s"
      case "end" => "end"
      case _ => validate(readLine("Stop typing like a dickhead. Type r, p or s:"))
    }
  }

  def receive1(turn: String): Unit =
    turn match {
      case "r" => rCount1 += 1
      case "p" => pCount1 += 1
      case "s" => sCount1 += 1
      case _ => println("Error! Received turn 1 is bad.")
    }

  def receive2(turn: String): Unit =
    turn match {
      case "r" => rCount2 += 1
      case "p" => pCount2 += 1
      case "s" => sCount2 += 1
      case _ => println("Error! Received turn 2 is bad.")
    }

  def takeTurn1(): String = {
    val highest = calculateMax1()
    if (Random.nextFloat() > 0.0) {
      highest match {
        case "r" => "p"
        case "p" => "s"
        case "s" => "r"
        case _ => "Error! Bad take turn."
      }
    }
    else chooseRandom()
  }

  def takeTurn2(): String = {
    val highest = calculateMax2()
    if (Random.nextFloat() > 0.5) {
      highest match {
        case "r" => "p"
        case "p" => "s"
        case "s" => "r"
        case _ => "Error! Bad take turn."
      }
    }
    else chooseRandom()
  }

  def chooseRandom(): String = {
    random = Random.nextFloat()
    if (random > 2/3) {
      "r"
    }
    else if (random >1/3) {
      "s"
    }
    else "p"
  }

  def calculateMax1(): String = {
    if (rCount1 > pCount1 && rCount1 > sCount1) "r"
    else if (pCount1 > rCount1 && pCount1 > sCount1) "p"
    else "s"
  }

  def calculateMax2(): String = {
    if (rCount2 > pCount2 && rCount2 > sCount2) "r"
    else if (pCount2 > rCount2 && pCount2 > sCount2) "p"
    else "s"
  }

  def determineResultPVsAI(playerTurn: String, computer1Turn: String): String = {
    (playerTurn, computer1Turn) match {
      case ("r", "r") => "n"
      case ("r", "p") => "c"
      case ("r", "s") => "p"
      case ("p", "r") => "p"
      case ("p", "p") => "n"
      case ("p", "s") => "c"
      case ("s", "r") => "c"
      case ("s", "p") => "p"
      case ("s", "s") => "n"
      case _ => println("Error! Determine Result is bad!"); "x"
    }
  }

  def determineResultAIVsAI(computer1Turn: String, computer2Turn: String): String = {
    (computer1Turn, computer2Turn) match {
      case ("r", "r") => "n"
      case ("r", "p") => "2"
      case ("r", "s") => "1"
      case ("p", "r") => "1"
      case ("p", "p") => "n"
      case ("p", "s") => "2"
      case ("s", "r") => "2"
      case ("s", "p") => "1"
      case ("s", "s") => "n"
      case _ => println("Error! Determine Result is bad!"); "x"
    }
  }

  def startAIVsAI(): Unit = {
    println("Hello and welcome to RPS Scala AI vs AI. Watch and learn.")
    while (true) {
      computer1Turn = takeTurn1()
      computer2Turn = takeTurn2()
      println(computer1Turn, computer2Turn)
      result = determineResultAIVsAI(computer1Turn.head.toString, computer2Turn.head.toString)
      result match {
        case "n" => println("It's a draw!")
        case "1" => println("Computer 1 wins!"); computer1Wins += 1
        case "2" => println("Computer 2 wins!"); computer2Wins += 1
        case _ => println("Error! result doesn't match!!")
      }
      receive1(computer2Turn.head.toString)
      receive2(computer1Turn.head.toString)
      println("Current score is computer 1: " + computer1Wins + " computer 2 : " + computer2Wins)
    }


  }

}
