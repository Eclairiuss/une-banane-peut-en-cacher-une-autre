package model

abstract class Token(val x:Int, val y:Int) {
  def moveTo(nx:Int,ny:Int):Token

  def normalMoves(board: Board):Seq[(Int,Int)]
  def attackMoves(board: Board):Seq[(Int,Int)]

  def allMoves(board: Board):Seq[(Int,Int)] = {
    normalMoves(board) ++ attackMoves(board)
  }

  def moveN():Token = moveTo(x,y+1)
  def moveS():Token = moveTo(x,y-1)
  def moveE():Token = moveTo(x+1,y)
  def moveW():Token = moveTo(x-1,y)
  def moveNE():Token = moveTo(x+1,y+1)
  def moveSE():Token = moveTo(x+1,y-1)
  def moveNW():Token = moveTo(x-1,y+1)
  def moveSW():Token = moveTo(x-1,y-1)
}
