import org.scalatest.FunSpec

class TreeTest extends FunSpec{

  describe("Applying length function") {
    describe("to single leaf tree"){
      val empty = Leaf();
      it("should return 1 size") {
        assertResult(Tree.size(empty)){
          1
        }
      }
    }

    describe("to branch and 2 leafs tree"){
      val empty = Branch(Leaf(), Leaf());
      it("should return 3 length") {
        assertResult(Tree.size(empty)){
          3
        }
      }
    }
  }

  describe("Applying max function") {
    describe("to branch and 2 leafs tree"){
      val empty = Branch(Branch(Leaf(1), Leaf(10)), Branch(Leaf(21), Leaf(3)));
      it("should return 3 length") {
        assertResult(Tree.max(empty)((a,b) => if(a > b) a else b)){
          21
        }
      }
    }
  }

  describe("Applying depth function") {
    describe("to complicate tree"){
      val list = Branch(Branch(Leaf(1), Branch(Branch(Branch(Leaf(2), Leaf(8)), Leaf(10)), Leaf(2))), Branch(Leaf(21), Leaf(3)));
      it("should return 6 level") {
        assertResult(Tree.depth1(list)){
          6
        }
        assertResult(Tree.depth2(list)){
          6
        }
      }
    }
  }

  describe("Applying map function") {
    describe("to complicate tree"){
      val list = Branch(Branch(Leaf(1), Branch(Branch(Branch(Leaf(2), Leaf(8)), Leaf(10)), Leaf(2))), Branch(Leaf(21), Leaf(3)));
      it("should add 1 to every element") {
        assertResult(Tree.map(list)(a  => a+1)){
          Branch(Branch(Leaf(2), Branch(Branch(Branch(Leaf(3), Leaf(9)), Leaf(11)), Leaf(3))), Branch(Leaf(22), Leaf(4)))
        }
      }
    }
  }

}
