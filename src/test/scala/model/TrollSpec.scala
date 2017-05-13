package model

import org.scalatest.FreeSpec

class TrollSpec extends FreeSpec{
  def test(seq:Seq[(Int, Int)], bool:Boolean, pair:(Int, Int)):Boolean = {
    bool && seq.contains(pair)
  }
  "A Troll" - {
    "can move where he can !" - {
      "normal !" in {
        val troll0 = new Troll {override val x: Int = 4; override val y: Int = 4}
        val board0 = new Board {
          override val tokens = Map((troll0.x, troll0.y) -> troll0)
        }
        val wanted0: Seq[(Int, Int)] = Seq((3, 3), (3, 4), (3, 5), (4, 3), (4, 5), (5, 3), (5, 4), (5, 5))
        val get0: Seq[(Int, Int)] = board0.allMoves(troll0)

        def test0(a: Boolean, b: (Int, Int)): Boolean = {test(wanted0, a, b)}

        assert(get0.length == wanted0.length && get0.foldLeft(true)(test0))
      }

      "next a troll !" in {
        val troll0 = new Troll {override val x: Int = 4; override val y: Int = 4}
        val troll1 = new Troll {override val x: Int = 3; override val y: Int = 3}
        val board1 = new Board {
          override val tokens = Map((troll0.x, troll0.y) -> troll0, (troll1.x, troll1.y) -> troll1)
        }
        val wanted1: Seq[(Int, Int)] = Seq((3, 4), (3, 5), (4, 3), (4, 5), (5, 3), (5, 4), (5, 5))
        val get1: Seq[(Int, Int)] = board1.allMoves(troll0)

        def test1(a: Boolean, b: (Int, Int)): Boolean = {test(wanted1, a, b)}

        assert(get1.length == wanted1.length && get1.foldLeft(true)(test1))
      }

      "next a dwarf !" in {
        val troll0 = new Troll {override val x: Int = 4; override val y: Int = 4}
        val dwarf0 = new Dwarf {override val x: Int = 3; override val y: Int = 3}
        val board2 = new Board {
          override val tokens = Map((troll0.x, troll0.y) -> troll0, (dwarf0.x, dwarf0.y) -> dwarf0)
        }
        val wanted2: Seq[(Int, Int)] = Seq((3, 4), (3, 5), (4, 3), (4, 5), (5, 3), (5, 4), (5, 5))
        val get2: Seq[(Int, Int)] = board2.allMoves(troll0)

        def test2(a: Boolean, b: (Int, Int)): Boolean = {test(wanted2, a, b)}

        assert(get2.length == wanted2.length && get2.foldLeft(true)(test2))
      }

      "in a vertical side (not a corner) !" in {
        val troll3 = new Troll {override val x: Int = 0; override val y: Int = 6}
        val board3 = new Board {
          override val tokens = Map((troll3.x, troll3.y) -> troll3)
        }
        val wanted3: Seq[(Int, Int)] = Seq((0, 5), (0, 7), (1, 5), (1, 6), (1, 7))
        val get3: Seq[(Int, Int)] = board3.allMoves(troll3)

        def test3(a: Boolean, b: (Int, Int)): Boolean = {test(wanted3, a, b)}

        assert(get3.length == wanted3.length && get3.foldLeft(true)(test3))
      }

      "in a horizontal side (not a corner) !" in {
        val troll4 = new Troll {override val x: Int = 6; override val y: Int = 0}
        val board4 = new Board {
          override val tokens = Map((troll4.x, troll4.y) -> troll4)
        }
        val wanted4: Seq[(Int, Int)] = Seq((5, 0), (7, 0), (5, 1), (6, 1), (7, 1))
        val get4: Seq[(Int, Int)] = board4.allMoves(troll4)

        def test4(a: Boolean, b: (Int, Int)): Boolean = {test(wanted4, a, b)}

        assert(get4.length == wanted4.length && get4.foldLeft(true)(test4))
      }

      "in a diagonal side (not a corner) !" in {
        val troll5 = new Troll {override val x: Int = 1; override val y: Int = 4}
        val board5 = new Board {
          override val tokens = Map((troll5.x, troll5.y) -> troll5)
        }
        val wanted5: Seq[(Int, Int)] = Seq((0, 5), (1, 5), (2, 3), (2, 4), (2, 5))
        val get5: Seq[(Int, Int)] = board5.allMoves(troll5)

        def test5(a: Boolean, b: (Int, Int)): Boolean = {test(wanted5, a, b)}

        assert(get5.length == wanted5.length && get5.foldLeft(true)(test5))
      }

      "in a corner !" in {
        val troll6 = new Troll {override val x: Int = 0; override val y: Int = 5}
        val board6 = new Board {
          override val tokens = Map((troll6.x, troll6.y) -> troll6)
        }
        val wanted6: Seq[(Int, Int)] = Seq((0, 6), (1, 4), (1, 5), (1, 6))
        val get6: Seq[(Int, Int)] = board6.allMoves(troll6)

        def test6(a: Boolean, b: (Int, Int)): Boolean = {test(wanted6, a, b)}

        assert(get6.length == wanted6.length && get6.foldLeft(true)(test6))
      }
    }
    "can shove" - {
      "to attack (simple) !" in {
        val troll1 = new Troll {override val x: Int = 1; override val y: Int = 6}
        val troll2 = new Troll {override val x: Int = 0; override val y: Int = 6}
        val dwarf = new Dwarf {override val x: Int = 4; override val y: Int = 6}
        val board = new Board {
          override val tokens = Map((troll1.x, troll1.y) -> troll1, (troll2.x, troll2.y) -> troll2, (dwarf.x, dwarf.y) -> dwarf)
        }
        val wanted: Seq[(Int, Int)] = Seq((0, 5), (0, 7), (1, 5), (1, 7), (2, 5), (2, 6), (2, 7), (3, 6))
        val get: Seq[(Int, Int)] = board.allMoves(troll1)

        def test2(a: Boolean, b: (Int, Int)): Boolean = {test(wanted, a, b)}
        assert(get.length == wanted.length && get.foldLeft(true)(test2))
      }

      "to attack (far jump) !" in {
        val troll1 = new Troll {override val x: Int = 2; override val y: Int = 6}
        val troll2 = new Troll {override val x: Int = 1; override val y: Int = 6}
        val troll3 = new Troll {override val x: Int = 0; override val y: Int = 6}
        val dwarf1 = new Dwarf {override val x: Int = 6; override val y: Int = 6}
        val board = new Board {
          override val tokens = Map((troll1.x, troll1.y) -> troll1, (troll2.x, troll2.y) -> troll2, (troll3.x, troll3.y) -> troll3, (dwarf1.x, dwarf1.y) -> dwarf1)
        }
        val wanted: Seq[(Int, Int)] = Seq((1, 5), (1, 7), (2, 5), (2, 7), (3, 5), (3, 6), (3, 7), (5, 6))
        val get: Seq[(Int, Int)] = board.allMoves(troll1)

        def test2(a: Boolean, b: (Int, Int)): Boolean = {test(wanted, a, b)}
        assert(get.length == wanted.length && get.foldLeft(true)(test2))
      }

      "to attack (far many possibilities) !" in {
        val troll1 = new Troll {override val x: Int = 2; override val y: Int = 6}
        val troll2 = new Troll {override val x: Int = 1; override val y: Int = 6}
        val troll3 = new Troll {override val x: Int = 0; override val y: Int = 6}
        val dwarf1 = new Dwarf {override val x: Int = 6; override val y: Int = 6}
        val dwarf2 = new Dwarf {override val x: Int = 5; override val y: Int = 7}
        val board = new Board {
          override val tokens = Map((troll1.x, troll1.y) -> troll1, (troll2.x, troll2.y) -> troll2, (troll3.x, troll3.y) -> troll3, (dwarf1.x, dwarf1.y) -> dwarf1, (dwarf2.x, dwarf2.y) -> dwarf2)
        }
        val wanted: Seq[(Int, Int)] = Seq((1, 5), (1, 7), (2, 5), (2, 7), (3, 5), (3, 6), (3, 7), (4, 6), (5, 6))
        val get: Seq[(Int, Int)] = board.allMoves(troll1)

        def test2(a: Boolean, b: (Int, Int)): Boolean = {test(wanted, a, b)}
        assert(get.length == wanted.length && get.foldLeft(true)(test2))
      }
    }
  }
}
