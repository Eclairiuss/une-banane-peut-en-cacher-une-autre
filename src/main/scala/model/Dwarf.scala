package model

class Dwarf (x:Int, y:Int) extends Token(x, y) {
  def moveTo(nx:Int, ny:Int) = new Dwarf(nx,ny)

  def normalMoves(board: Board):Seq[(Int,Int)] = {
    for {
      i <- 1 to 15
      j <- 0 to 7 if (true)
    } yield (0,0)
  }

  def attackMoves(board: Board):Seq[(Int,Int)] = {
    for {
      i <- 1 to 15
      j <- 0 to 7 if true
    } yield (0, 0)
  }

  def move(f:()=>Token, n:Int):Dwarf = {
    if (n <= 0) {
      this
    } else {
      val nd = f().asInstanceOf[Dwarf]
      if (1 < n) {
        nd.move(f, n - 1)
      } else {
        nd
      }
    }
  }

  def moveN(n : Int):Dwarf = move(super.moveN,n)
  def moveS(n : Int):Dwarf = move(super.moveS,n)
  def moveE(n : Int):Dwarf = move(super.moveE,n)
  def moveW(n : Int):Dwarf = move(super.moveW,n)
  def moveNE(n : Int):Dwarf = move(super.moveNE,n)
  def moveSE(n : Int):Dwarf = move(super.moveSE,n)
  def moveNW(n : Int):Dwarf = move(super.moveNW,n)
  def moveSW(n : Int):Dwarf = move(super.moveSW,n)
}