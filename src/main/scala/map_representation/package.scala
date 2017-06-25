import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.FieldType

/**
  * Created by jp on 23.06.17.
  */
package object map_representation {

  /*
    Type class for representing something as an untyped Map[String, Any]
    Instances for case classes are derived automatically via shapeless.
    The conversions to Map is a deep conversion, i.e. any enclosing members are converted, too.
    Specifically, members of type Set, Map or Seq are converted recursively as well.
   */
  trait MapRepresentation[Value]{
    def toMap(value : Value): Map[String, Any]
  }

  object MapRepresentation{
    /*
      Factory method to allow summoning an instance of the type class ad hoc.
    */
    def apply[T:MapRepresentation]: MapRepresentation[T] = implicitly[MapRepresentation[T]]
  }

  /*
    Auxiliary type class for representing values when an enclosing object is converted to a Map[String, Any]
   */
  trait ValueRepresentation[-V]{
    def toValue(value : V): Any
  }

  /*
    Auxiliary type class for key types that will be converted to String keys if an enclosing Map is converted to a Map[String, Any]
   */
  trait KeyRepresentation[V]{
    def toKey(key: V): String
  }

  def representsItSelf[V]: ValueRepresentation[V] = new ValueRepresentation[V] {
    override def toValue(value: V): Any = value
  }

  implicit val stringValueRepresentation : ValueRepresentation[String] = representsItSelf
  implicit val intValueRepresentation : ValueRepresentation[Int] = representsItSelf
  implicit val booleanValueRepresentation : ValueRepresentation[Boolean] = representsItSelf
  implicit val doubleValueRepresentation : ValueRepresentation[Double] = representsItSelf

  implicit val stringKeyRepresentation : KeyRepresentation[String] = new KeyRepresentation[String] {
    override def toKey(key: String): String = key
  }

  implicit val symbolKeyRepresentation : KeyRepresentation[Symbol] = new KeyRepresentation[Symbol] {
    override def toKey(key: Symbol): String = key.name
  }

  //TODO: Figure out how to abstract from the collection class and DRY up with the Set case
  implicit def traversableValueRepresentation[V](implicit packageableContents: ValueRepresentation[V]): ValueRepresentation[Seq[V]] = new ValueRepresentation[Seq[V]] {
    override def toValue(value: Seq[V]): Any = {
      value map { element =>
        packageableContents toValue element
      }
    }
  }
  //TODO: Figure out how to abstract from the collection class and DRY up with the Seq case
  implicit def setValueRepresentation[V](implicit packageableContents: ValueRepresentation[V]): ValueRepresentation[Set[V]] = new ValueRepresentation[Set[V]] {
    override def toValue(value: Set[V]): Any = {
      value map { element =>
        packageableContents toValue element
      }
    }
  }

  implicit def objectAsMapValueRepresentation[V](implicit packageableContents: MapRepresentation[V]): ValueRepresentation[V] = new ValueRepresentation[V] {
    override def toValue(value: V): Any = {
      packageableContents toMap value
    }
  }

  implicit val hnilMapRepresentation : MapRepresentation[HNil] = new MapRepresentation[HNil]{
    override def toMap(value: HNil): Map[String, Any] = Map.empty
  }

  implicit def labelledGenericMapRepresentation[Value, Repr](implicit generic: LabelledGeneric.Aux[Value, Repr], hlistPackaging : Lazy[MapRepresentation[Repr]] ): MapRepresentation[Value] = new MapRepresentation[Value]{
    override def toMap(value: Value): Map[String, Any] = {
      val asGeneric: Repr = generic.to(value)
      hlistPackaging.value.toMap(asGeneric)
    }
  }

  implicit def hListMapRepresentation[Key <: Symbol, Head, Tail <: HList](implicit makeUntyped: Lazy[ValueRepresentation[Head]], witness: Witness.Aux[Key], tailPackaging: Lazy[MapRepresentation[Tail]]) : MapRepresentation[FieldType[Key, Head]::Tail] = new MapRepresentation[FieldType[Key, Head]::Tail]{
    override def toMap(value: FieldType[Key, Head]::Tail): Map[String, Any] = {
      val tailValue = value.tail
      val encoded: Map[String, Any] = tailPackaging.value.toMap(tailValue)
      val key : String = witness.value.name
      encoded + (key -> makeUntyped.value.toValue(value.head))
    }
  }

  implicit def anyMapRepresentation[Key, Value](implicit keyEncoding: KeyRepresentation[Key], valueRepresentation: ValueRepresentation[Value]): MapRepresentation[Map[Key,Value]] = new MapRepresentation[Map[Key,Value]]{
    override def toMap(value: Map[Key, Value]): Map[String, Any] = {
      value map { case (key, value) =>
        keyEncoding.toKey(key) -> valueRepresentation.toValue(value)
      }
    }
  }
}
