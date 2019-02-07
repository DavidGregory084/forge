package forge

import cats._
import cats.implicits._
import cats.mtl._
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
    def putInfo(ii: I): Store[I, K, V] = Store.init(ii, f)
    def getValue(k: K): V = f(k)
    def putValue(k1: K, v: V) = Store.init[I, K, V](i, k2 => if (k1 === k2) v else f(k2))
    def getHash(k: K) = getValue(k).hash
  }
  def initMap[I, K, V: Hash](i: I): Store[I, K, V] =
    MapStore(i, Map.empty[K, V])
}

object Build {
  abstract class Task[C[_[_]], K, V] {
    def run[F[_]: C](fetch: K => F[V]): F[V]
  }

  abstract class TaskDescription[C[_[_]], K, V] {
    def compute(target: K): Option[Task[C, K, V]]
  }

  abstract class System[C[_[_]], I, K, V] {
    def build(tasks: TaskDescription[C, K, V], target: K, store: Store[I, K, V]): Store[I, K, V]
  }

  abstract class Rebuilder[C[_[_]], IR, K, V] {
    def rebuild(key: K, currentValue: V, recompute: Task[C, K, V]): Task[MonadState[?[_], IR], K, V]
  }

  abstract class Scheduler[C[_[_]], I, IR, K, V] {
    def schedule(rebuilder: Rebuilder[C, IR, K, V]): System[C, I, K, V]
  }

  def busy[K, V] = new System[Applicative, Unit, K, V] {
    def build(tasks: TaskDescription[Applicative, K, V], target: K, store: Store[Unit, K, V]): Store[Unit, K, V] = {
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

  def sprsh1: TaskDescription[Applicative, String, Int] = new TaskDescription[Applicative, String, Int] {
    def compute(target: String): Option[Task[Applicative, String, Int]] = {
      println(s"Computing $target")
      target match {
        case "B1" =>
          Some(new Task[Applicative, String, Int] {
            def run[F[_]: Applicative](fetch: String => F[Int]): F[Int] = {
              (fetch("A1"), fetch("A2")).mapN(_ + _)
            }
          })
        case "B2" =>
          Some(new Task[Applicative, String, Int] {
            def run[F[_]: Applicative](fetch: String => F[Int]): F[Int] = {
              fetch("B1").map(_ * 2)
            }
          })
        case _ =>
          None
      }
    }
  }

  def sprsh2: TaskDescription[Monad, String, Int] = new TaskDescription[Monad, String, Int] {
    def compute(target: String): Option[Task[Monad, String, Int]] = {
      target match {
        case "B1" =>
          Some(new Task[Monad, String, Int] {
            def run[F[_]: Monad](fetch: String => F[Int]): F[Int] =
              for {
                c1 <- fetch("C1")
                r <- if (c1 === 1) fetch("B2") else fetch("A2")
              } yield r
          })
        case "B2" =>
          Some(new Task[Monad, String, Int] {
            def run[F[_]: Monad](fetch: String => F[Int]): F[Int] =
              for {
                c1 <- fetch("C1")
                r <- if (c1 === 1) fetch("A1") else fetch("B1")
              } yield r
          })
        case _ =>
          None
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val store = Store.init[Unit, String, Int]((), k => if (k === "A1") 10 else 20)
    val result1 = busy[String, Int].build(sprsh1, "B2", store)
    println("B1: " + result1.getValue("B1"))
    println("B2: " + result1.getValue("B2"))
    // val result2 = busy[String, Int].build(sprsh2, "B2", store)
    // println("B1: " + result2.getValue("B1"))
    // println("B2: " + result2.getValue("B2"))
  }
}
