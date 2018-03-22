import org.scalatest.FunSpec

class OptionTest extends FunSpec{

  describe("Optional map") {
    describe("applied to empty option") {
      val emptyOption = None : Option[Int];
      it("should be empty") {
        assertResult(emptyOption.map(a => a + 1)) {
          None
        }
      }
    }

    describe("applied to Some option") {
      val emptyOption = Some(1);
      it("should add one") {
        assertResult(emptyOption.map(a => a + 1)) {
          Some(2)
        }
      }
    }
  }

  describe("Optional flatMap") {
    describe("applied to empty option") {
      val emptyOption = None : Option[Int];
      it("should be empty") {
        assertResult(emptyOption.flatMap(a => Some(a + 1))) {
          None
        }
      }
    }

    describe("applied to Some option") {
      val emptyOption = Some(1);
      it("should add one") {
        assertResult(emptyOption.flatMap(a => Some(a+1))) {
          Some(2)
        }
      }
    }
  }

  describe("Optional flatMap") {
    describe("applied to None option") {
      val emptyOption = None : Option[Int];
      it("should be None") {
        assertResult(emptyOption.flatMap(a => Some(a + 1))) {
          None
        }
      }
    }

    describe("applied to Some(1) option") {
      val emptyOption = Some(1);
      it("should applied function +1 return Some(2)") {
        assertResult(emptyOption.flatMap(a => Some(a+1))) {
          Some(2)
        }
      }
    }
  }


  describe("Optional getOrElse") {
    describe("applied to empty option") {
      val emptyOption = None : Option[Int];
      it("should return default") {
        assertResult(emptyOption.getOrElse(2)) {
          2
        }
      }
    }

    describe("applied to Some option") {
      val nonEmpty = Some(1);
      it("should return None value") {
        assertResult(nonEmpty.getOrElse(2)) {
          1
        }
      }
    }
  }

  describe("Optional filter") {
    describe("applied to empty option") {
      val emptyOption = None : Option[Int];
      it("should return None") {
        assertResult(emptyOption.filter((a) => a == 1)) {
          None
        }
      }
    }

    describe("applied to Some(1) option with take only value 1") {
      val nonEmpty = Some(1);
      it("should return None value") {
        assertResult(nonEmpty.filter((a) => a == 1)) {
          Some(1)
        }
      }
    }

    describe("applied to Some(1) option with take only value 2") {
      val nonEmpty = Some(1);
      it("should return None value") {
        assertResult(nonEmpty.filter((a) => a == 2)) {
          None
        }
      }
    }
  }

  describe("Optional orElse") {
    describe("applied to empty option") {
      val emptyOption = None : Option[Int];
      it("should return default") {
        assertResult(emptyOption.orElse(Some(2))) {
          Some(2)
        }
      }
    }

    describe("applied to Some(1) option") {
      val nonEmpty = Some(1);
      it("should return None value") {
        assertResult(nonEmpty.orElse(Some(2))) {
          Some(1)
        }
      }
    }
  }


  describe("Optional map2") {
    describe("applied to 2 empty option") {
      val emptyOption1 = None : Option[Int];
      val emptyOption2 = None : Option[Int];
      it("should return None") {
        assertResult(Option.map2(emptyOption1, emptyOption2)((a,b) => a+b)) {
          None
        }
      }
    }

    describe("applied to Some(1) and None option") {
      val nonEmpty = Some(1);
      val emptyOption = None : Option[Int];
      it("should return None value") {
        assertResult(Option.map2(nonEmpty, emptyOption)((a,b) => a+b)) {
          None
        }
      }
    }

    describe("applied to Some(1) and Some(2) option") {
      val Some1 = Some(1);
      val Some2 = Some(2);
      it("should return Some(3)") {
        assertResult(Option.map2(Some1, Some2)((a,b) => a+b)) {
          Some(3)
        }
      }
    }
  }

  describe("Optional sequence") {
    describe("applied to Nil") {
      val emptyOption = None : Option[Int];
      it("should return Some(Nil)") {
        assertResult(Option.sequence(Nil)) {
          Some(Nil)
        }
      }
    }

    describe("applied to List(None) option") {
      val nonEmpty = List(None);
      it("should return None value") {
        assertResult(Option.sequence(nonEmpty)) {
          None
        }
      }
    }

    describe("applied to List(Some(1), None, Some(2)) option") {
      val nonEmpty = List(Some(1), None, Some(2));
      it("should return None value") {
        assertResult(Option.sequence(nonEmpty)) {
          None
        }
      }
    }

    describe("applied to List(Some(1), Some(2), Some(3)) option") {
      val nonEmpty = List(Some(1), Some(2), Some(3));
      it("should return Some(List(1,2,3))") {
        assertResult(Option.sequence(nonEmpty)) {
          Some(List(1,2,3))
        }
      }
    }
  }

  describe("Optional traverse") {
    describe("applied to Nil") {
      val emptyOption = None : Option[Int];
      it("should return Some(Nil)") {
        assertResult(Option.traverse(Nil)(_ => Some(1))) {
          Some(Nil)
        }
      }
    }

    describe("applied to List(1,2,3) option") {
      val nonEmpty = List(1,2,3);
      it("should return Some(List(1,2,3)) value") {
        assertResult(Option.traverse(nonEmpty)(a => Some(a))) {
          Some(List(1,2,3))
        }
      }
    }
  }

}
