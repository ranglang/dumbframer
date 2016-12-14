/**
  * Created by tian on 14/12/2016.
  */
import scala.util.Random
import org.scalatest.BeforeAndAfterAll
import org.scalatest.WordSpecLike
import org.scalatest.Matchers
import com.typesafe.config.ConfigFactory
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.stream.{ActorMaterializer, ClosedShape, SourceShape, UniformFanInShape}
import akka.stream.scaladsl.{GraphDSL, RunnableGraph, Sink, Source, Zip, ZipWith}
import akka.testkit.{DefaultTimeout, ImplicitSender, TestActors, TestKit}

import scala.concurrent.duration._
import scala.collection.immutable
import scala.concurrent.{Await, Future}

/**
  * a Test to show some TestKit examples
  */
class TestKitUsageSpec
  extends TestKit(ActorSystem(
    "TestKitUsageSpec",
    ConfigFactory.parseString(TestKitUsageSpec.config)))

    with DefaultTimeout
    with ImplicitSender
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll {

  import TestKitUsageSpec._

  val echoRef = system.actorOf(TestActors.echoActorProps)
  val forwardRef = system.actorOf(Props(classOf[ForwardingActor], testActor))
  val filterRef = system.actorOf(Props(classOf[FilteringActor], testActor))
  val randomHead = Random.nextInt(6)
  val randomTail = Random.nextInt(10)
  val headList = immutable.Seq().padTo(randomHead, "0")
  val tailList = immutable.Seq().padTo(randomTail, "1")
  val seqRef =
    system.actorOf(Props(classOf[SequencingActor], testActor, headList, tailList))

//  implicit val system = ActorSystem("QuickStart")
  implicit val materializer = ActorMaterializer()

  override def afterAll {
    shutdown()
  }

  "a" should {
    "2" in {
      val pairs = Source.fromGraph(GraphDSL.create() { implicit b =>
        import GraphDSL.Implicits._

        // prepare graph elements
        val zip = b.add(Zip[Int, Int]())
        def ints = Source.fromIterator(() => Iterator.from(1))

        // connect the graph
        ints.filter(_ % 2 != 0) ~> zip.in0
        ints.filter(_ % 2 == 0) ~> zip.in1

        // expose port
        SourceShape(zip.out)
      })

      val firstPair: Future[(Int, Int)] = pairs.runWith(Sink.head)
      Await.result(firstPair,3000.millis) should be ((1,2))
    }
    "a" in {
      val pickMaxOfThree = GraphDSL.create() { implicit b =>
        import GraphDSL.Implicits._

        val zip1 = b.add(ZipWith[Int, Int, Int](math.max _))
        val zip2 = b.add(ZipWith[Int, Int, Int](math.max _))
        zip1.out ~> zip2.in0

        UniformFanInShape(zip2.out, zip1.in0, zip1.in1, zip2.in1)
      }

      val resultSink = Sink.head[Int]

      val g = RunnableGraph.fromGraph(GraphDSL.create(resultSink) { implicit b => sink =>
        import GraphDSL.Implicits._

        // importing the partial graph will return its shape (inlets & outlets)
        val pm3 = b.add(pickMaxOfThree)

        Source.single(1) ~> pm3.in(0)
        Source.single(2) ~> pm3.in(1)
        Source.single(3) ~> pm3.in(2)
        pm3.out ~> sink.in
        ClosedShape
      })

      val max: Future[Int] = g.run()
      Await.result(max,3000.millis) should be (3)
    }
  }

  "An EchoActor" should {
    "Respond with the same message it receives" in {
      within(500 millis) {
        echoRef ! "test"
        expectMsg("test")
      }
    }
  }
  "A ForwardingActor" should {
    "Forward a message it receives" in {
      within(500 millis) {
        forwardRef ! "test"
        expectMsg("test")
      }
    }
  }
  "A FilteringActor" should {
    "Filter all messages, except expected messagetypes it receives" in {
      var messages = Seq[String]()
      within(500 millis) {
        filterRef ! "test"
        expectMsg("test")
        filterRef ! 1
        expectNoMsg
        filterRef ! "some"
        filterRef ! "more"
        filterRef ! 1
        filterRef ! "text"
        filterRef ! 1

        receiveWhile(500 millis) {
          case msg: String => messages = msg +: messages
        }
      }
      messages.length should be(3)
      messages.reverse should be(Seq("some", "more", "text"))
    }
  }
  "A SequencingActor" should {
    "receive an interesting message at some point " in {
      within(500 millis) {
        ignoreMsg {
          case msg: String => msg != "something"
        }
        seqRef ! "something"
        expectMsg("something")
        ignoreMsg {
          case msg: String => msg == "1"
        }
        expectNoMsg
        ignoreNoMsg
      }
    }
  }
}

object TestKitUsageSpec {
  // Define your test specific configuration here
  val config = """
    akka {
      loglevel = "WARNING"
    }
               """

  /**
    * An Actor that forwards every message to a next Actor
    */
  class ForwardingActor(next: ActorRef) extends Actor {
    def receive = {
      case msg => next ! msg
    }
  }

  /**
    * An Actor that only forwards certain messages to a next Actor
    */
  class FilteringActor(next: ActorRef) extends Actor {
    def receive = {
      case msg: String => next ! msg
      case _           => None
    }
  }

  /**
    * An actor that sends a sequence of messages with a random head list, an
    * interesting value and a random tail list. The idea is that you would
    * like to test that the interesting value is received and that you cant
    * be bothered with the rest
    */
  class SequencingActor(next: ActorRef, head: immutable.Seq[String],
                        tail: immutable.Seq[String]) extends Actor {
    def receive = {
      case msg => {
        head foreach { next ! _ }
        next ! msg
        tail foreach { next ! _ }
      }
    }
  }
}
