import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

class RobinSpec extends Spec with ShouldMatchers {

  describe ("the round robin thingo") {

    it("should generate lots of output with stupid bases"){
      val numberOfPlayers = 12
      for (round <- 1 to 11) {
        for (court <- 1 to 6) {
          print((if(court == 1) 1 else (round + court - 2) % (numberOfPlayers - 1) + 2),
                (numberOfPlayers - 1 + round - court) % (numberOfPlayers - 1) + 2)
        }
        println()
      }

     // for(i <- 0 to 6; r <- 0 to 20) { ((r+i) % n, (r+n-i) % n) }
    }

    it("should generate lots of output with base 0"){
      val numberOfPlayers = 12
      for (round <- 0 to 10) {
        for (court <- 0 to numberOfPlayers/2 - 1) {
          print(((if(court == 0) numberOfPlayers - 1 else (court + round) % (numberOfPlayers - 1))),
                (((numberOfPlayers - 1) - court + round) % (numberOfPlayers - 1)))
        }
        println()
      }
    }

    it("should store implement liz's dsl"){
      val participants = 12
      val weeks = 10
      for (frenzy <- 0 until weeks) {
        for (pair <- 0 until participants/2) {
          print(((if(pair == 0) participants - 1 else (pair + frenzy) % (participants - 1))),
                (((participants - 1) - pair + frenzy) % (participants - 1)))
        }
        println()
      }
    }

    it("should store implement liz's dsl again"){
      val participants = 12
      val weeks = 10
      val frenzyPairs = for {
        frenzy <- 0 until weeks
        pair <- 0 until participants/2
      } yield (((if(pair == 0) participants - 1 else (pair + frenzy) % (participants - 1))),
                (((participants - 1) - pair + frenzy) % (participants - 1)))
      frenzyPairs.foreach(print)
    }

    it("should store implement liz's dsl with nested fors"){
      val participants = 12
      val weeks = 10
      val frenzyPairs = for (frenzy <- 0 until weeks)
                      yield for(pair <- 0 until participants/2)
                            yield (((if(pair == 0) participants - 1 else (pair + frenzy) % (participants - 1))),
                                  (((participants - 1) - pair + frenzy) % (participants - 1)))
      frenzyPairs.foreach(println)
    }

    it("should store implement liz's dsl with nested random shizzle"){
      val participants = 12
      val rounds = 10
      val frenzyPairs = for (frenzy <- 0 until rounds)
                      yield for(pair <- 0 until participants/2)
                            yield (((if(pair == 0) participants - 1 else (pair + frenzy) % (participants - 1))),
                                  (((participants - 1) - pair + frenzy) % (participants - 1)))

      val (evenOnes,oddOnes) = frenzyPairs.toList.zipWithIndex.partition(_._2%2 == 0)
      evenOnes.zip(oddOnes).foreach(println)
    }


//        it("should store implement liz's dsl again with grouping"){
//          val participants = 12
//          val weeks = 10
//          val dForDougo = for {
//            frenzy <- 0 until weeks
//            pair <- 0 until participants/2
//          } yield (((if(pair == 0) participants - 1 else (pair + frenzy) % (participants - 1))),
//                    (((participants - 1) - pair + frenzy) % (participants - 1)))
//
//          def group[T](l: List[T]): List[List[T]] =
//            if(l == Nil) Nil
//            else {
//              val (grp,tail) = l.splitAt(participants)
//              grp::group(tail)
//            }
//
//          val list: List[int] = dForDougo.toList()
//          group(list) //.foreach(print)
//        }

  }

}