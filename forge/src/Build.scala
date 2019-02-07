package forge

import cats._
import cats.implicits._
import cats.Hash
import cats.data.State

 abstract class Store[I, K, V] {
  def getInfo: I
  def putInfo(i: I): Store[I, K, V]
  def getValue(k: K): V
  def putValue(k: K, v: V): Store[I, K, V]
  def getHash(k: K): Int
}

private[forge] case class MapStore[I, K, V: Hash](info: I, map: Map[K, V]) extends Store[I, K, V] {
  def getInfo: I = info
  def putInfo(i: I) = MapStore(i, map)
  def getValue(k: K): V = map(k)
  def putValue(k: K, v: V) = MapStore(info, map.updated(k, v))
  def getHash(k: K): Int = getValue(k).hash
}

object Store {
  def init[I, K: Eq, V: Hash](i: I, f: K => V): Store[I, K, V] = new Store[I, K, V] {
    def getInfo: I = i
    def putInfo(i: I): Store[I, K, V] = Store.init(i, f)
    def getValue(k: K): V = f(k)
    def putValue(k1: K, v: V) = Store.init[I, K, V](i, k2 => if (k1 === k2) v else f(k1))
    def getHash(k: K) = getValue(k).hash
  }
  def initMap[I, K, V: Hash](i: I): Store[I, K, V] =
    MapStore(i, Map.empty[K, V])
}

object Build {
  abstract class Task[K, V] {
    def run[F[_]: Applicative](fetch: K => F[V]): F[V]
  }

  abstract class TaskDescription[K, V] {
    def compute(target: K): Option[Task[K, V]]
  }

  abstract class System[I, K, V] {
    def build(tasks: TaskDescription[K, V], target: K, store: Store[I, K, V]): Store[I, K, V]
  }

  abstract class Rebuilder[K, V] {
    def rebuild(key: K, currentValue: V, recompute: Task[K, V]): Task[K, V]
  }

  abstract class Scheduler[I, K, V] {
    def schedule(rebuilder: Rebuilder[K, V]): System[I, K, V]
  }

  def busy[K, V] = new System[Unit, K, V] {
    def build(tasks: TaskDescription[K, V], target: K, store: Store[Unit, K, V]): Store[Unit, K, V] = {
      def fetch(k: K): State[Store[Unit, K, V], V] =
        tasks.compute(k) match {
          case None =>
            State.inspect[Store[Unit, K, V], V](_.getValue(k))
          case Some(task) =>
            for {
              v <- task.run(fetch(_))
              _ <- State.modify[Store[Unit, K, V]](_.putValue(k, v))
            } yield v
        }

      fetch(target)
        .runS(store)
        .value
    }
  }

  def sprsh1: TaskDescription[String, Int] = new TaskDescription[String, Int] {
    def compute(target: String): Option[Task[String, Int]] = {
      println(s"Computing $target")
      target match {
        case "B1" =>
          Some(new Task[String, Int] {
            def run[F[_]: Applicative](fetch: String => F[Int]): F[Int] = {
              (fetch("A1"), fetch("A2")).mapN(_ + _)
            }
          })
        case "B2" =>
          Some(new Task[String, Int] {
            def run[F[_]: Applicative](fetch: String => F[Int]): F[Int] = {
              fetch("B1").map(_ * 2)
            }
          })
        case _ =>
          None
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val store = Store.init[Unit, String, Int]((), k => if (k === "A1") 10 else 20)
    val result = busy[String, Int].build(sprsh1, "B2", store)
    println("B1: " + result.getValue("B1"))
    println("B2: " + result.getValue("B2"))
  }
}
