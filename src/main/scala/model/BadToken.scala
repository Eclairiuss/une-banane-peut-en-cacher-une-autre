package model

case object BadToken extends Token(-15,-15) {
  def moveTo(nx:Int,ny:Int):Token = this

  def normalMoves(board: Board):Seq[(Int,Int)] = Seq.empty
  def attackMoves(board: Board):Seq[(Int,Int)] = Seq.empty
}