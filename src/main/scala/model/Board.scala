package model

abstract class Board {
  val wrongToken = BadToken

  val halfSizeMinusOne = 7

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
    val nx = Math.abs(x - halfSizeMinusOne)
    val ny = Math.abs(y - halfSizeMinusOne)
    val somme = nx + ny
    (nx < (halfSizeMinusOne + 1)) && (ny < (halfSizeMinusOne + 1)) && (somme != 0) && (somme < 10)
  }

  def checkValidPosition(token: Token):Boolean = checkValidPosition(token.x, token.y)

  def unoptionToken(ot:Option[model.Token]):Token = {
    ot match {
      case Some(token) => token
      case _ => wrongToken
    }
  }

  def emptyCell(x:Int,y:Int):Boolean = tokens.get(x,y).isEmpty

  def allMoves(token: Token):Seq[(Int,Int)] = {
    token.allMoves(this)
  }
}