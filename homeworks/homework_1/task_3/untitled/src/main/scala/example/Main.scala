package example

object Main extends App {
  def sayHello(greeting: String, name: String) = println(greeting+" Scala! This is "+name)

  val myName = "Vova"
  val greetings = Array("Hello", "Hi", "Zdarova")

  greetings.foreach(sayHello(_, myName))
  greetings.foreach(sayHello(_, myName.reverse))
}