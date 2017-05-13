package model

abstract class Dwarf extends Token{
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