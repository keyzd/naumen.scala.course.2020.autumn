object Exercises {
	trait Animal {
		def name: String
	}

	case class Cat(override val name: String) extends Animal

	case class Dog(override val name: String) extends Animal

	case class Shelter[+A <: Animal](animals: List[A] = Nil) {
		def +[B >: A <: Animal](animal: B): Shelter[B] = {
			Shelter[B](animals ++ List(animal))
		}

		def ++[C >: A <: Animal](shelter: Shelter[C]): Shelter[C] = {
			Shelter[C](animals ++ shelter.animals)
		}

		def getNames(): List[String] = {
			for(a <- animals)
				yield a.name
		}

		def feed(food: Food[A]): List[String] = {
			for(a <- animals)
				yield food.feed(a)
		}
	}

	trait Food[-A <: Animal] {
		def feed(animal: A): String
	}

	case object Meat extends Food[Animal] {
		override def feed(animal: Animal): String = {
			animal.name + " eats meat"
		}
	}

	case object Milk extends Food[Cat] {
		override def feed(animal: Cat): String = {
			animal.name + " eats milk"
		}
	}

	case object Bread extends Food[Dog] {
		override def feed(animal: Dog): String = {
			animal.name + " eats bread"
		}
	}
}
