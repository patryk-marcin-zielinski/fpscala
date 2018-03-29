import org.scalatest.{FunSpec, FunSuite}

class StreamTest extends FunSpec {

  describe("Stream headOption") {
    describe(" for raw data type constructor") {
      var x = 1
      val stream = Cons[Int](() => {
        x = x + 1
        x
      }, () => Empty)
      it("should be different values") {
        val h1 = stream.headOption;
        val h2 = stream.headOption;
        assert(h1 != h2)
      }
    }
    describe(" for smart constructor ") {
      var x = 1
      val stream = Stream.cons[Int]({
        x = x + 1
        x
      }, Empty)
      it("should be different values") {
        val h1 = stream.headOption;
        val h2 = stream.headOption;
        assert(h1 == h2)
      }
    }
  }

  describe("Stream take nth") {
    describe("Given Stream(1,2,3)") {
      val stream123 = Stream(1, 2, 3);
      describe("take 2 element") {
        it("should result in Stream(1,2)") {
          assertResult(stream123.take(2).toList) {
            Stream(1, 2).toList
          }
          assertResult(Stream.takeViaUnfold(stream123, 2).toList) {
            List(1,2)
          }
        }
      }
    }

    describe("Given Stream(1)") {
      val stream123 = Stream(1);
      describe("take 2 element") {
        it("should result in Stream(1,2)") {
          assertResult(stream123.take(2).toList) {
            List(1)
          }
          assertResult(Stream.takeViaUnfold(stream123, 2).toList) {
            List(1)
          }
        }
      }
    }

    describe("Given End:Stream") {
      val stream123 = Empty
      describe("take 2 element") {
        it("should result in Stream(1,2)") {
          assertResult(stream123.take(2)) {
            Empty
          }
          assertResult(Stream.takeViaUnfold(stream123, 2)) {
            Empty
          }
        }
      }
    }

    describe("Given 3 element Stream with changing head") {
      var headCallNumber = 0
      var incX = () => {
        headCallNumber = headCallNumber + 1
        headCallNumber
      }
      val stream123 = Cons(incX, () => Cons(incX, () => Cons(incX, () => Empty)))
      describe("take 2 element") {
        it("should not evaluate last vale") {
          stream123.take(2).toList
          assertResult(2) {
            headCallNumber
          }
        }
      }
    }
  }

  describe("Stream drop nth") {
    describe("Given Stream(1,2,3)") {
      val stream123 = Stream(1, 2, 3);
      describe("drop 2 element") {
        it("should result in Stream(3)") {
          assertResult(stream123.drop(2).toList) {
            Stream(3).toList
          }
        }
      }
    }

    describe("Given 3 element Stream with changing head") {
      var headCallNumber = 0
      var incX = () => {
        headCallNumber = headCallNumber + 1
        headCallNumber
      }
      //      val stream123 = Stream(incX, incX, incX)
      val stream123 = Cons(incX, () => Cons(incX, () => Cons(incX, () => Empty)))
      describe("drop 1 element") {
        it("should evaluate just two last value") {
          stream123.drop(1).toList
          assertResult(2) {
            headCallNumber
          }
        }
      }
    }
  }


  describe("Stream takeWhile") {
    describe("Given Stream(1,2,3,4,5)") {
      val stream123 = Stream(1, 2, 3, 4, 5);
      describe("take all elements greater than 2") {
        it("should result in Stream(3,4,5)") {
          assertResult(stream123.takeWhile(_ < 2).toList) {
            Stream(1).toList
          }
        }
      }
    }

    describe("Given Stream(1,1,2,1,1,1) via takeWhile_2") {
      val stream123 = Stream[Int](1,1,2,1,1,1);
      describe("take all elements greater than 2") {
        val value = Stream.takeWhile_1(stream123)((a) => a == 1)
        it("should result in Stream(1,1)") {
          assertResult(value.toList) {
            List(1,1)
          }
        }
      }
    }

    describe("Given Stream(1,1,,2,1,1,1,) via takeWhile_2") {
      val stream123 = Stream[Int](1,1,2,1,1,1);
      describe("take all elements greater than 2") {
        val value = Stream.takeWhile_2(stream123)((a) => a == 1)
        it("should result in Stream(1,1)") {
          assertResult(value.toList) {
            List(1,1)
          }
        }
      }
    }

    describe("Given Stream(1,1,2,1,1,1) via takeWhileViaUnfold") {
      val stream123 = Stream[Int](1,1,2,1,1,1);
      describe("take all elements greater than 2") {
        val value = Stream.takeWhileViaUnfold(stream123) ((a) => a == 1)
        it("should result in Stream(1,1)") {
          assertResult(value.toList) {
            List(1,1)
          }
        }
      }
    }

    describe("Given 5 element Stream with changing head") {
      var headCallNumber = 0
      var incX = () => {
        headCallNumber = headCallNumber + 1
        lazy val test = headCallNumber;
        test
      }
      val stream123 = Cons(incX, () => Cons(incX, () => Cons(incX, () => Cons(incX, () => Cons(incX, () => Empty)))))
      describe("take while  x > 1 element") {
        it("should evaluate all values") {
          stream123.takeWhile(_ < 2)
          assertResult(1) {
            headCallNumber
          }
        }
      }
    }

    describe("Given infinity increment stream") {
      val stream123 = Stream.from(1);
      describe("take while  x < 10 element") {
        it("should return Stream(1,2,3,4,5,6,7,8,9") {
          assertResult(List(1,2,3,4,5,6,7,8,9)) {
            Stream.takeWhile_1(stream123)(_ < 10).toList
          }
          assertResult(List(1,2,3,4,5,6,7,8,9)) {
            Stream.takeWhile_2(stream123)(a => a < 10).toList
          }
          assertResult(List(1,2,3,4,5,6,7,8,9)) {
            Stream.takeWhileViaUnfold(stream123)(a => a < 10).toList
          }
        }
      }
    }
  }

  describe("Stream forAll") {
    describe("Given Stream(1,1,1,1,1)") {
      val stream123 = Stream(1, 1, 1, 1, 1);
      describe("match all element == 1") {
        it("should result in true") {
          assertResult(stream123.forAll(_ == 1)) {
            true
          }
        }
      }
    }

    describe("Given Stream(1,2,1,1,1)") {
      val stream123 = Stream(1, 2, 1, 1, 1);
      describe("match all element == 1") {
        it("should result in false") {
          assertResult(stream123.forAll(_ == 1)) {
            false
          }
        }
      }
    }

    describe("Given 3 element Stream with value 1") {
      var headCallNumber = 0
      var incX = () => {
        headCallNumber = headCallNumber + 1
        1
      }
      val stream123 = Cons(incX, () => Cons(incX, () => Cons(incX, () => Cons(() => 2, () => Cons(incX, () => Empty)))))
      describe("take while  x > 1 element") {
        it("should stop when reach element bigger than 1") {
          stream123.forAll((a) => {
            a == 1
          })
          assertResult(3) {
            headCallNumber
          }
        }
      }
    }
  }

  describe("Stream filter") {
    describe("Given Stream(1,2,1,3,1)") {
      val stream123 = Stream(1, 2, 1, 3, 1);
      describe("match all element == 1") {
        it("should result in Stream(1,1,1)") {
          assertResult(stream123.filter(_ == 1).toList) {
            List(1, 1, 1)
          }
        }
      }
    }


    describe("Given 3 element Stream with value 1") {
      var headCallNumber = 0
      var incX = () => {
        headCallNumber = headCallNumber + 1
        1
      }
      val stream123 = Cons(incX, () => Cons(incX, () => Cons(incX, () => Empty)))
      describe("filter x == 1 element") {
        it("should evaluate first element and leave rest for more evaluation") {
          stream123.filter((a) => {
            a == 1
          })
          assertResult(1) {
            headCallNumber
          }
        }
      }
    }
  }


  describe("Stream map") {
    describe("Given Stream(1,2,3,4,5)") {
      val stream123 = Stream(1, 2, 3, 4, 5);
      describe("add 1 to all element") {
        it("should result in Stream(1,1,1)") {
          assertResult(stream123.map(_ + 1).toList) {
            List(2, 3, 4, 5, 6)
          }
          assertResult(Stream.mapViaUnfold[Int, Int](stream123, _ + 1).toList) {
            List(2, 3, 4, 5, 6)
          }
        }
      }
    }


    describe("Given Stream(1,2,3,4,5)") {
      var headCallNumber = 0
      var incX = () => {
        headCallNumber = headCallNumber + 1
        headCallNumber
      }
      val stream123 = Cons(incX, () => Cons(incX, () => Cons(incX, () => Cons(incX, () => Cons(incX, () => Empty)))))
      describe("map x + 1 element") {
        it("should evaluate first element and leave rest for more evaluation") {
          stream123.map(_ + 1)
          assertResult(1) {
            headCallNumber
          }
        }
      }
    }

    describe("Given Stream(1,2,3,4,5)") {
      var headCallNumber = 0
      var incX = () => {
        headCallNumber = headCallNumber + 1
        headCallNumber
      }
      val stream123 = Cons(incX, () => Cons(incX, () => Cons(incX, () => Cons(incX, () => Cons(incX, () => Empty)))))
      describe("mapViaUnfold x + 1 element") {
        it("should evaluate first element and leave rest for more evaluation") {
          Stream.mapViaUnfold[Int, Int](stream123, _ + 1)
          assertResult(1) {
            headCallNumber
          }
        }
      }
    }


  }

  describe("Stream flatMap") {
    describe("Given Stream(1,2,3,4,5)") {
      val stream123 = Stream(1, 2, 3, 4, 5);
      describe("flatMap every element to return Stream(1)") {
        it("should result in Stream(1,1,1,1,1)") {
          assertResult(stream123.flatMap(_ => Stream(1)).toList) {
            List(1, 1, 1, 1, 1)
          }
        }
      }
    }


    describe("Given Stream(1,2,3,4,5)") {
      var headCallNumber = 0
      var incX = () => {
        headCallNumber = headCallNumber + 1
        headCallNumber
      }
      val stream123 = Cons(incX, () => Cons(incX, () => Cons(incX, () => Cons(incX, () => Cons(incX, () => Empty)))))
      describe("flatMap every element to return Stream(1)") {
        it("should evaluate first element and leave rest for more evaluation") {
          stream123.flatMap(_ => Stream(1))
          assertResult(1) {
            headCallNumber
          }
        }
      }
    }
  }

  describe("Stream append") {
    describe("Given Stream(1,2,3,4,5)") {
      val stream123 = Stream(1, 2, 3, 4, 5);
      describe("append to another Stream(6,7,8,9,10)") {
        val stream678 = Stream(6, 7, 8, 9, 10);
        it("should result in Stream(1,2,3,4,5,6,7,8,9,10)") {
          assertResult(stream123.append(stream678).toList) {
            List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
          }
        }
      }
    }


    describe("Given Stream with inc header") {
      var headCallNumber = 0: Int
      var incX = () => {
        headCallNumber = headCallNumber + 1
        headCallNumber
      }

      val stream123 = Cons(incX, () => Cons(incX, () => Cons(incX, () => Cons(incX, () => Cons(incX, () => Empty)))))
      describe("append to Stream of inc header") {
        val stream678 = Cons(incX, () => Cons(incX, () => Cons(incX, () => Cons(incX, () => Cons(incX, () => Empty)))))
        it("should evaluate first element and leave rest for more evaluation") {
          stream123.append(stream678)
          assertResult(1) {
            headCallNumber
          }
        }
      }
    }
  }

  describe("Stream constant") {
    describe("Given value 1 to constant function") {
      val infinityOnes = Stream.constant(1);
      it("should generate all ones") {
        assertResult(infinityOnes.take(10).toList) {
          Stream(1, 1, 1, 1, 1, 1, 1, 1, 1, 1).toList
        }
      }
    }
  }

  describe("Stream from") {
    describe("Given value 1 to constant function") {
      val infinityOnes = Stream.from(1);
      it("should generate natural order elements") {
        assertResult(infinityOnes.take(10).toList) {
          Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).toList
        }
      }
    }
  }

  describe("Stream fib") {
    describe("Given fibbonaci stream") {
      val fib = Stream.fib();
      it("should match 10 fibbonaci elements") {
        assertResult(fib.take(10).toList) {
          Stream(0, 1, 1, 2, 3, 5, 8, 13, 21, 34).toList
        }
      }
    }
  }

  describe("Stream zipWith") {
    describe("Given Stream(1,2,3) from(4) streams") {
      val s1 = Stream(1,2,3);
      val s2 = Stream.from(4)
      it("should add elements of streams") {
        assertResult(Stream.zipWithViaUnfold(s1,s2)(_ + _).toList) {
          Stream(5,7,9).toList
        }
      }
    }

    describe("Given Stream with inc header") {
      var headCallNumber = 0: Int
      var incX = () => {
        headCallNumber = headCallNumber + 1
        headCallNumber
      }

      val stream123 = Cons(incX, () => Cons(incX, () => Cons(incX, () => Cons(incX, () => Cons(incX, () => Empty)))))
      describe("zipWith infinity increment stream from(5)") {
        val stream678 = Stream.from(6)
        it("should evaluate first element and leave rest for more evaluation") {
          Stream.zipWithViaUnfold(stream123, stream678)(_ + _)
          assertResult(1) {
            headCallNumber
          }
        }
      }
    }
  }

  describe("Stream zipAll") {
    describe("Given Stream(1,2,3) from(4) streams") {
      val s1 = Stream(1, 2, 3)
      val s2 = Stream(4, 5, 6, 7)
      it("should add elements of streams") {
        assertResult(Stream.zipAll(s1, s2).toList) {
          Stream((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), Some(6)), (None, Some(7))).toList
        }
      }
    }
  }

  describe("Stream tails") {
    describe("Given Stream(1,2,3) from(4) streams") {
      val s1 = Stream(1, 2, 3)
      it("should tails to more stream") {
        assertResult(Stream.tails(s1).map(a => a.toList).toList) {
          List(List(1,2,3), List(2,3), List(3))
        }
      }
    }
  }

  describe("Stream scan") {
    describe("Given Stream(1,2,3) from(4) streams") {
      val s1 = Stream(1, 2, 3)
      it("should scan to List(6,5,3,0") {
        assertResult(Stream.scanRight_1(s1, 0)(_ + _).toList) {
          List(6,5,3,0)
        }
        assertResult(Stream.scanRight_2(s1, 0)(_ + _).toList) {
          List(6,5,3,0)
        }
      }
    }
  }
}
