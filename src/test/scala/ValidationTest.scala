import org.scalatest.FunSpec

class ValidationTest extends FunSpec{

  describe("Validation map a => a+1") {
    describe("applied to Error option") {
      val leftEither = Error(List("12323")) : Validation[String, Int]
      it("should be leave Error at it is") {
        assertResult(leftEither.map(a => a + 1)) {
          Error(List("12323"))
        }
      }
    }

    describe("applied to Value(1) validation") {
      val emptyOption = Value(1);
      it("should add one to Value(2)") {
        assertResult(emptyOption.map(a => a + 1)) {
          Value(2)
        }
      }
    }
  }

  describe("Validation orElse") {
    describe("applied to Right(1) option") {
      val rightEither = Right(1);
      it("should return untouch Right(1") {
        assertResult(rightEither.orElse(Right(2))) {
          Right(1)
        }
      }
    }

    describe("applied to Left(\"Test\") option") {
      val leftEither = Left("Test");
      it("should return default value") {
        assertResult(leftEither.orElse(Right(2))) {
          Right(2)
        }
      }
    }
  }


  describe("Either map2") {
    describe("applied to 2 Left Either") {
      val left1 = Left("Test1") : Either[String, Int]
      val left2 = Left("Test2") : Either[String, Int]
      it("should return Left(Test)") {
        assertResult(left1.map2(left2)((a,b) => a + b)) {
          Left("Test1")
        }
      }
    }

    describe("applied to Right(1) and Left(Test) Either") {
      val right1 = Right(1);
      val left1 = Left("Test")
      it("should return Left(Test)") {
        assertResult(right1.map2(left1)((a,b) => a+b)) {
          Left("Test")
        }
      }
    }

    describe("applied to Right(1) and Right(2) option") {
      val right1 = Right(1);
      val right2 = Right(2);
      it("should return Some(3)") {
        assertResult(right1.map2(right2)((a,b) => a+b)) {
          Right(3)
        }
      }
    }
  }

  describe("Either sequence") {
    describe("applied to Nil") {
      val emptyOption = None : Option[Int];
      it("should return Right(Nil)") {
        assertResult(Either.sequence(Nil)) {
          Right(Nil)
        }
      }
    }

    describe("applied to List(Left(Test))") {
      val left = List(Left("Test"));
      it("should return Left(Test) value") {
        assertResult(Either.sequence(left)) {
          Left("Test")
        }
      }
    }

    describe("applied to List(Right(1), Left(Test), Right(2)) either") {
      val nonEmpty = List(Right(1), Left("Test"), Right(2));
      it("should return Left(\"Test\") value") {
        assertResult(Either.sequence(nonEmpty)) {
          Left("Test")
        }
      }
    }

    describe("applied to List(Right(1), Left(Test1), Right(2), Left(Test2)) either") {
      val nonEmpty = List(Right(1), Left("Test1"), Right(2), Left("Test2"));
      it("should return first Left(\"Test2\") value") {
        assertResult(Either.sequence(nonEmpty)) {
          Left("Test2")
        }
      }
    }

    describe("applied to List(Right(1), Right(2), Right(3)) either") {
      val nonEmpty = List(Right(1), Right(2), Right(3));
      it("should return Some(List(1,2,3))") {
        assertResult(Either.sequence(nonEmpty)) {
          Right(List(1,2,3))
        }
      }
    }
  }

  describe("Either traverse") {
    describe("applied to Nil") {
      it("should return Some(Nil)") {
        assertResult(Either.traverse(Nil)(_ => Right(1))) {
          Right(Nil)
        }
      }
    }

    describe("applied to List(1,2,3) option") {
      val nonEmpty = List(1,2,3);
      it("should return Some(List(1,2,3)) value") {
        assertResult(Either.traverse(nonEmpty)(a => Right(a))) {
          Right(List(1,2,3))
        }
      }
    }
  }

}
