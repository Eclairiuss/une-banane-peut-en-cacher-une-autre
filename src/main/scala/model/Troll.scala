package model

class Troll(x:Int, y:Int) extends Token(x, y) {
  def moveTo(nx:Int, ny:Int) = new Troll(nx,ny)
  def normalMoves(board: Board):Seq[(Int,Int)] = {
    for {
      i <- -1 to 1
      j <- -1 to 1 if (!(i == 0 && j==0)
      && board.emptyCell(x + i, y + j)
      && board.checkValidPosition(x + i, y + j))
    } yield (x + i, y + j)
  }

  def attackMoves(board: Board):Seq[(Int,Int)] = {
    (for {
      i <- -1 to 1
      j <- -1 to 1 if ((i != 0 || j != 0)
      && board.unoptionToken(board.tokens.get((x + i, y + j))).isInstanceOf[Troll])
    } yield (shoveRange(board, i, j))).flatten
  }


  def checkTrollShove(board: Board, nx: Int, ny: Int):Boolean = {
    val tmp = for{
      i <- -1 to 1
      j <- -1 to 1 if ((i != 0 || j != 0)
      && board.unoptionToken(board.tokens.get((nx+i,ny+j))).isInstanceOf[Dwarf])
    } yield board.tokens.get((nx+i,ny+j))
    !tmp.isEmpty
  }

  def shoveRange(board: Board, vx: Int, vy: Int): Seq[(Int,Int)] = {
    for {
      i <- 1 to 7 if {
        val tmp = for {
          j <- 1 to i
        } yield {
          (board.unoptionToken(board.tokens.get(x + j * vx, y + j * vy)).isInstanceOf[Troll]
            && board.checkValidPosition(x - j * vx, y - j * vy)
            && board.checkValidPosition(x - (j + 1) * vx, y - (j + 1) * vy)
            && board.emptyCell(x - j * vx, y - j * vy)
            && board.emptyCell(x - (j + 1) * vx, y - (j + 1) * vy))
        }
        (!tmp.contains(false)) && checkTrollShove(board, x - (i + 1) * vx, y - (i + 1) * vy)
      }
    } yield
      (x - (i + 1) * vx, y - (i + 1)  * vy)
  }
}