package learnscala3
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scala.collection.mutable.Queue

case class Read()
case class Write(value: String)

class QueueActor extends Actor {
  val queue = Queue[String]()

  def receive = {
    case Read() => sender() ! queue.dequeue()
    case Write(value) => queue.enqueue(value)
  }
}

class ReaderActor(queueActor: ActorRef) extends Actor {
  def receive = {
    case Read() => queueActor ! Read()
    case value: String => println(s"Read value: $value")
  }
}

class WriterActor(queueActor: ActorRef) extends Actor {
  def receive = {
    case Write(value) => queueActor ! Write(value)
  }
}

def main_akka = {
  val system = ActorSystem("QueueSystem")
  val queueActor = system.actorOf(Props[QueueActor](), "queueActor")
  val reader1 = system.actorOf(Props(new ReaderActor(queueActor)), "reader1")
  val reader2 = system.actorOf(Props(new ReaderActor(queueActor)), "reader2")
  val writer1 = system.actorOf(Props(new WriterActor(queueActor)), "writer1")
  val writer2 = system.actorOf(Props(new WriterActor(queueActor)), "writer2")

  writer1 ! Write("Hello")
  writer2 ! Write("World")
  reader1 ! Read()
  reader2 ! Read()

  Thread.sleep(1000)
  system.terminate()
}
