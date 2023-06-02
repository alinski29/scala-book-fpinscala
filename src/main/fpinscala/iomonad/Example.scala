package fpinscala.iomonad

case class Player(name: String, score: Int)

/* The contest function couples the I/O code for displaying the result to the
pure logic for computing the winner.  */
def contest(p1: Player, p2: Player): Unit =
  if p1.score > p2.score then println(s"${p1.name} is the winner!")
  else if p2.score > p1.score then println(s"${p2.name} is the winner!")
  else println("It's a draw.")

// We can factor the logic into its own pure function, winner:
def winner(p1: Player, p2: Player): Option[Player] =
  if p1.score > p2.score then Some(p1)
  else if p1.score < p2.score then Some(p1)
  else None

def contest_1(p1: Player, p2: Player): Unit =
  winner(p1, p2) match
    case Some(Player(name, _)) => println(s"$name is the winner")
    case None                  => println("it's a draw")

def winnerMsg(p: Option[Player]): String =
  p.map { case Player(name, _) => s"$name is the winner!" }
    .getOrElse("It's a draw")

/* The insight here is that inside every function with side effects is
a pure function waiting to get out. */
def contest_2(p1: Player, p2: Player): Unit =
  println(winnerMsg(winner(p1, p2)))

/* Given an impure function f of type A => B, we can split f into two functions:
  - A pure function of type A => D, where D is some description of the result of f.
  - An impure function of type D => B , which can be thought of as an interpreter of these descriptions.
 */
