import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inr, LabelledGeneric, Poly1, Witness}
import shapeless.labelled.FieldType
import shapeless.poly._
import cats.Traverse._
import shapeless.syntax.std.tuple._

// A sealed trait (sum) and some case classes (products)
sealed trait Entity
case class Employee(name : String, uid: Int) extends Entity
case class Manager(name: String) extends Entity
case class Department(name: String, manager : Manager, employees: List[Employee]) extends Entity

object Data {
  // A girl (or boy) who's an Employee has no name...
  val a: Employee = new Employee("a", 100)
  val b: Employee = new Employee("b", 101)
  val c: Employee = new Employee("c", 102)
  val john: Manager = new Manager(name = "John Boss")
  val sales: Department = new Department("Sales", manager = john, employees = List(a, b, c))
}

// Let's play with Generics
object Main extends App {

  import Data._
  // Example heterogenous list (product)
  val hlist : HList = "words words words" :: 1 :: true :: HNil
  println(hlist)

  // Product representation (HList)
  // println(Generic[Department].to(sales))

  // Sum representation (super interesting, let's reverse engineer it?)
  // println(Generic[Entity].to(a))

  // What about tuples (still products)
  // println((a, b, c).tail)
  // println((1, a, c).head)

  // It's all sums & products in the end!
  // How to map on any product? How about sum? Discus...
}




















// Enter Polymorphic functions!
object polyToString extends Poly1 {

  // Indent a string
  private def indent(n : Int, s: String) : String = " ".repeat(n) + s

  implicit def caseDefault[A] = use((a: A) => {
    println(s"Warning: Using default function for ${a.toString}")
    a.toString
  })
  implicit def caseManager = use((m : Manager) => s"Manager-In-Chief: ${m.name}")
  implicit def caseEmployee = use((e : Employee) => s"EmployeeID: ${e.uid} * Name: ${e.name}")
  implicit def caseListEmployees = use ((le: List[Employee]) =>
    le.map(polyToString).map(indent(4, _)).mkString("\n"))

  implicit def caseDepartment = use((d: Department) =>
    s"Glorious department of ${d.name}\n"
      + indent(2, caseManager(d.manager : Manager)) + "\n"
      + polyToString(d.employees))

}

object PrePoly extends App {

  import Data._

  // And now we can map too
  println(Generic[Department].to(sales).map(polyToString))

  // Or even better
  // println(polyToString(sales))

  // But this is no good: polyToString depends on structure and fields of Employee, Department, Manager
  // aka: Shape. Not really shapeless yet.
  // Enter shapeless.ops.hlist...
}


















// Let's write any T to JSON
trait JsonWrites[-T] {
  def write(t: T): String
}

// Companion object
object JsonWrites {
  def apply[T](f: T => String) = new JsonWrites[T] {
    def write(t: T) = f(t)
  }

  // base type instances
  implicit val stringWrites = JsonWrites[String](identity)
  implicit val intWrites = JsonWrites[Int](_.toString)
  implicit val longWrites = JsonWrites[Long](_.toString)

  implicit def optionWrites[T](implicit tWrites: JsonWrites[T]) =
    JsonWrites[Option[T]](_.fold[String]("")(t => tWrites.write(t)))

  // Some nice syntax
  implicit class ToJsonOps[T](t: T)(implicit writes: JsonWrites[T]) {
    def toJson = writes.write(t)
  }
}

// Base values should work now
object FirstPoly extends App {

  import JsonWrites._

  // println("hello".toJson)

  // println(1.toJson)

  // println((Some(1): Option[Int]).toJson)

  // println((None: Option[Int]).toJson)
}


// Let's add the recursive step (products)
object JsonOps {

  // Base step
  implicit val hnilToJson = JsonWrites[HNil](_ => "")


  implicit def hconsToJson[Key <: Symbol, Head, Tail <: HList](
                                                                implicit key: Witness.Aux[Key],
                                                                headWrites: JsonWrites[Head],
                                                                tailWrites: JsonWrites[Tail])
  : JsonWrites[FieldType[Key, Head] :: Tail] =
    JsonWrites[FieldType[Key, Head] :: Tail] { l =>
      // compute the tail json:
      val json = tailWrites.write(l.tail)
      json + s"${key.value.name} : ${headWrites.write(l.head)},\n"
    }

  // Generic product to JSON
  implicit def lgenToJson[T, Repr](
                                    implicit lgen: LabelledGeneric.Aux[T, Repr],
                                    reprWrites: JsonWrites[Repr]) = JsonWrites[T] { t =>
    reprWrites.write(lgen.to(t))
  }

}

object SecondPoly extends App {
  import Data._
  import JsonOps._
  import JsonWrites._

  println(a.toJson)
  // println(john.toJson)
  // Ok, but what about our high-level object Entity? Next time, coproducts....
}
