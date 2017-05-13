import model.{Board, Dwarf, Token}
import org.scalatest.FlatSpec
import scala.collection.immutable.Map
import utils.CSVParserBoardTokenCheck

class TokenSpec extends FlatSpec{
  "A Token" should "n't be at a wrong place !" in {
    val tmp = for {
      i <- -1 to 15
      j <- -1 to 15
    } yield new Dwarf {
      override val x = i
      override val y = j
    }

    val board:Board = new Board{
      override val tokens:Map[(Int,Int),Token] = Map[(Int, Int), Token]()
    }

    def transform(m:Map[(Int, Int), Boolean], d:Dwarf):Map[(Int, Int), Boolean] = m + ((d.x,d.y) -> board.checkValidPosition(d))

    val result:Map[(Int,Int),Boolean] = tmp.foldLeft(Map[(Int,Int),Boolean]())(transform)
    val expected = CSVParserBoardTokenCheck.values

    def test(boolean: Boolean, a:((Int,Int),Boolean)):Boolean = {
      boolean && result.contains(a._1)
    }

    val resultat = expected.foldLeft(true)(test)

    assert(resultat)
  }
}