package model

abstract class Board {
  val wrongToken = new Token {
    override val x: Int = -10
    override val y: Int = -10
  }

  val thirdSize = 7

  /*     A,B,C,D,E,F,G,H,I,J,K,L,M,N,O
   *  1: X,X,X,X,X,O,O,O,O,O,X,X,X,X,X
   *  2: X,X,X,X,O,O,O,O,O,O,O,X,X,X,X
   *  3: X,X,X,O,O,O,O,O,O,O,O,O,X,X,X
   *  4: X,X,O,O,O,O,O,O,O,O,O,O,O,X,X
   *  5: X,O,O,O,O,O,O,O,O,O,O,O,O,O,X
   *  6: O,O,O,O,O,O,O,O,O,O,O,O,O,O,O
   *  7: O,O,O,O,O,O,O,O,O,O,O,O,O,O,O
   *  8: O,O,O,O,O,O,O,T,O,O,O,O,O,O,O
   *  9: O,O,O,O,O,O,O,O,O,O,O,O,O,O,O
   * 10: O,O,O,O,O,O,O,O,O,O,O,O,O,O,O
   * 11: X,O,O,O,O,O,O,O,O,O,O,O,O,O,X
   * 12: X,X,O,O,O,O,O,O,O,O,O,O,O,X,X
   * 13: X,X,X,O,O,O,O,O,O,O,O,O,X,X,X
   * 14: X,X,X,X,O,O,O,O,O,O,O,X,X,X,X
   * 15: X,X,X,X,X,O,O,O,O,O,X,X,X,X,X
  */
  val tokens: Map[(Int,Int), Token]

  def checkValidPosition(x: Int, y: Int): Boolean = {
    val nx = Math.abs(x - thirdSize)
    val ny = Math.abs(y - thirdSize)
    val somme = nx + ny
    (nx < (thirdSize + 1)) && (ny < (thirdSize + 1)) && (somme != 0) && (somme < 10)
  }

  def checkValidPosition(token: Token): Boolean = checkValidPosition(token.x, token.y)

  def unoptionToken(ot:Option[model.Token]) = {
    ot match {
      case Some(token) => token
      case _ => wrongToken
    }
  }

  def emptyCell(x:Int,y:Int):Boolean = {
    tokens.get(x,y).isEmpty
  }

  def normalMoves(token: Token):Seq[(Int,Int)] = token match {
    case dwarf:Dwarf => for {
      i <- 1 to 15
      j <- 0 to 7 if (true)
    } yield (0,0)
    case troll:Troll => for {
      i <- -1 to 1
      j <- -1 to 1 if (!(i == 0 && j==0)
                        && emptyCell(troll.x + i, troll.y + j)
                        && checkValidPosition(troll.x + i, troll.y + j))
    } yield (troll.x + i, troll.y + j)
  }

  def checkTrollShove(x: Int, y: Int):Boolean = {
    val tmp = for{
      i <- -1 to 1
      j <- -1 to 1 if ((i != 0 || j != 0)
                          && unoptionToken(tokens.get((x+i,y+j))).isInstanceOf[Dwarf])
    } yield tokens.get((x+i,y+j))
    !tmp.isEmpty
  }

  def shoveRange(troll: Troll, vx: Int, vy: Int): Seq[(Int,Int)] = {
    val x = troll.x
    val y = troll.y
    for {
      i <- 1 to 7 if {
        val tmp = for {
          j <- 1 to i
        } yield {
          (unoptionToken(tokens.get(x + j * vx, y + j * vy)).isInstanceOf[Troll]
              && checkValidPosition(x - j * vx, y - j * vy)
              && checkValidPosition(x - (j + 1) * vx, y - (j + 1) * vy)
              && emptyCell(x - j * vx, y - j * vy)
              && emptyCell(x - (j + 1) * vx, y - (j + 1) * vy))}
        !tmp.contains(false) && checkTrollShove(x - (i + 1) * vx, y - (i + 1) * vy)
      }
    } yield (x - (i + 1) * vx,y - (i + 1)  * vy)
  }

  def attackMoves(token: Token):Seq[(Int,Int)] = token match {
    case dwarf: Dwarf => for {
      i <- 1 to 15
      j <- 0 to 7 if true
    } yield (0, 0)
    case troll: Troll => (for {
      i <- -1 to 1
      j <- -1 to 1 if (!(i == 0 && j == 0)
                        && unoptionToken(tokens.get((troll.x + i, troll.y + j))).isInstanceOf[Troll]
                        && checkValidPosition(troll.x - 2 * i, troll.y - 2 * j)
                        && emptyCell(troll.x - i, troll.y - j)
                        && emptyCell(troll.x - 2 * i, troll.y - 2 * j)
                        && checkTrollShove(troll.x - 2 * i, troll.y - 2 * j))
    } yield shoveRange(troll, i, j)).flatten
  }

  def allMoves(token: Token):Seq[(Int,Int)] = {
    normalMoves(token) ++ attackMoves(token)
  }
}