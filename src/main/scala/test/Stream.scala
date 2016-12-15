package test

import akka.stream._
import akka.stream.scaladsl._
import akka.{Done, NotUsed}
import akka.actor.{ActorSystem, Cancellable}
import akka.util.ByteString
import akka.testkit.TestActorRef
import scala.concurrent.duration._
import scala.concurrent.Await
import akka.pattern.ask

import org.scalatest._
import org.scalatest.durations._
import org.scalatest._

import scala.concurrent._
import scala.concurrent.duration._
import java.nio.file.Paths
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by tian on 13/12/2016.
  */
//object Stream extends  App {
//  implicit val system = ActorSystem("QuickStart")
//  implicit val materializer = ActorMaterializer()
//
//  val a = List(1,2,3)
//  val r = a.fold("")((left,right) => left.toString +right)
//  Console.println(r);
//
//  val r1 = a.reduce((a,b)=>a +b)
//  Console.println(r1);
//
//  val source: Source[Int, NotUsed] = Source(1 to 100)
//  source.runForeach(i => println(i))(materializer)
//
//  val factorials = source.scan(BigInt(1))((acc, next) => acc * next)
//
//  val result: Future[IOResult] =
//    factorials
//      .map(num => ByteString(s"$num\n"))
//      .runWith(FileIO.toPath(Paths.get("factorials.txt")))
//
//  def lineSink(filename: String): Sink[String, Future[IOResult]] =
//    Flow[String]
//      .map(s => ByteString(s + "\n"))
//      .toMat(FileIO.toPath(Paths.get(filename)))(Keep.right)
//
//  final case class Author(handle: String)
//
//  final case class Hashtag(name: String)
//
//  final case class Tweet(author: Author, timestamp: Long, body: String) {
//
//    def hashtags: Set[Hashtag] =
//      body.split(" ").collect { case t if t.startsWith("#") => Hashtag(t) }.toSet
//  }
//
//  val akkaTag = Hashtag("#12")
//  val tweets: Source[Tweet, NotUsed] = Source(1 to 100 map(int => Tweet(new Author("#"+ int.toString),1L, "#"+ int.toString + " "+"#"+ (int+100).toString)))
//
//  val hashtags: Source[Hashtag, NotUsed] = tweets.mapConcat(_.hashtags.toList)
//
//  val count: Flow[Tweet, Int, NotUsed] = Flow[Tweet].map(_ => 1)
//
//  val sumSink: Sink[Int, Future[Int]] = Sink.fold[Int, Int](0)(_ + _)
//
//  val counterGraph: RunnableGraph[Future[Int]] =
//    tweets
//      .via(count)
//      .toMat(sumSink)(Keep.right)
//
//  val sum: Future[Int] = counterGraph.run()
//
//  sum.foreach(c => println(s"Total tweets processed: $c"))
//
//}
