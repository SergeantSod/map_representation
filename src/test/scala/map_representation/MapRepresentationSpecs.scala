package map_representation

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSpec, Matchers}

class MapRepresentationSpecs extends FunSpec with Matchers with PropertyChecks with TypeCheckedTripleEquals {

  it("converts an empty case class instance to an empty map"){
    case class EmptyCaseClass()
    MapRepresentation[EmptyCaseClass].toMap(new EmptyCaseClass()) should ===(Map[String,Object]())
  }

  case class FlatCaseClass(doubleMember: Double, intMember: Int, booleanMember: Boolean, stringMember: String)

  implicit val arbitraryFlatInstance : Arbitrary[FlatCaseClass] = Arbitrary{ arbitrary[(Double, Int, Boolean, String)].map(FlatCaseClass.tupled) }

  implicit val arbitrarySymbol : Arbitrary[Symbol] = Arbitrary{ arbitrary[String] map (Symbol(_)) }

  it("converts a simple flat case class instance to a flat map with string keys"){
    forAll{ flat: FlatCaseClass =>
      val asMap = MapRepresentation[FlatCaseClass].toMap(flat)

      asMap should ===(
        Map(
          "doubleMember" -> flat.doubleMember,
          "intMember" -> flat.intMember,
          "booleanMember" -> flat.booleanMember,
          "stringMember" -> flat.stringMember
        )
      )
    }
  }

  it("leaves maps with string keys unmodified"){
    forAll{ map : Map[String, Double] =>
      val result = MapRepresentation[Map[String, Double]].toMap(map)
      result should ===(map)
    }
  }

  it("converts symbols keys in maps to strings"){
    forAll { someMap: Map[Symbol, String] =>
      val result = MapRepresentation[Map[Symbol, String]].toMap(someMap)
      result should ===(
        someMap map { case (key, value) =>
          key.name -> value
        }
      )
    }
  }

  case class CaseClassWithNestedMember(flatCaseClassMember: FlatCaseClass)

  implicit val arbitraryCaseClassWithNestedMembers : Arbitrary[CaseClassWithNestedMember] = Arbitrary{ arbitrary[FlatCaseClass].map(CaseClassWithNestedMember(_)) }

  it("converts instances of case classes with nested case class members"){
    forAll{ someInstance : CaseClassWithNestedMember =>

      val outerRepresentation = MapRepresentation[CaseClassWithNestedMember]
      val flatCaseClassRepresentation = MapRepresentation[FlatCaseClass]

      outerRepresentation.toMap(someInstance) should ===(
        Map(
          "flatCaseClassMember" -> flatCaseClassRepresentation.toMap(someInstance.flatCaseClassMember)
        )
      )
    }
  }

  case class CaseClassWithCollectionMembers(setMember: Set[FlatCaseClass], listMember: List[FlatCaseClass], mapMember: Map[String, FlatCaseClass])

  implicit val arbitraryCaseClassWithCollectionMembers : Arbitrary[CaseClassWithCollectionMembers] = Arbitrary{
    for {
      setMember <- arbitrary[Set[FlatCaseClass]]
      listMember <- arbitrary[List[FlatCaseClass]]
      mapMember <- arbitrary[Map[String,FlatCaseClass]]
    } yield CaseClassWithCollectionMembers(setMember, listMember, mapMember)
  }


  it("converts instances of case classes with collection members"){
    forAll{ someInstance : CaseClassWithCollectionMembers =>
      val outerRepresentation = MapRepresentation[CaseClassWithCollectionMembers]
      val flatCaseClassRepresentation = MapRepresentation[FlatCaseClass]

      outerRepresentation.toMap(someInstance) should ===(
        Map(
          "setMember" -> someInstance.setMember.map(flatCaseClassRepresentation.toMap),
          "listMember" -> someInstance.listMember.map(flatCaseClassRepresentation.toMap),
          "mapMember" -> someInstance.mapMember.mapValues(flatCaseClassRepresentation.toMap)
        )
      )
    }
  }

}
