package forge

import cats._
import cats.implicits._
import cats.mtl._
import cats.Hash
import cats.data.{ Const, State, WriterT }
import quiver._

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

final case class Trace[K, R](
  key: K,
  dependencies: List[(K, Int)],
  result: R
)

final case class VerifyingTrace[K: Eq](traces: Map[K, Trace[K, Int]]) {
  def recordTrace(trace: Trace[K, Int]): VerifyingTrace[K] =
    VerifyingTrace(traces.updated(trace.key, trace))

  def isUpToDate[M[_]](key: K, valueHash: Int, fetchHash: K => M[Int])(implicit M: Monad[M]): M[Boolean] = {
    traces.get(key) match {
      case None =>
        M.pure(false)
      case Some(Trace(storedKey, storedDeps, storedHash)) =>
        if (storedKey =!= key || storedHash =!= valueHash)
          M.pure(false)
        else
          storedDeps.forallM {
            case (depKey, depHash) =>
              fetchHash(depKey).map(_ === depHash)
          }
    }
  }
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

  def topoSort[K: Order](
    graph: Graph[K, Unit, Unit],
    roots: Vector[K],
    sortedNodes: Vector[K] = Vector.empty[K]
  ): Vector[K] = {
    if (roots.isEmpty)
      sortedNodes
    else graph.decomp(roots.head) match {
      case Decomp(Some(c), g) =>
        val newRoots = c.successors.filter(g.isRoot)
        topoSort(g, roots.tail ++ newRoots, sortedNodes :+ roots.head)
      case Decomp(None, g) =>
        topoSort(g, roots.tail, sortedNodes)
    }
  }

  def topological[I, K: Order, V] = new Scheduler[Applicative, I, I, K, V] {
    def schedule(rebuilder: Rebuilder[Applicative, I, K, V]): System[Applicative, I, K, V] = {
      new System[Applicative, I, K, V] {
        def build(tasks: TaskDescription[Applicative, K, V], target: K, store: Store[I, K, V]) = {
          def deps(key: K) = tasks.compute(key).map(dependencies).getOrElse(List.empty[K])

          def graph(key: K): Graph[K, Unit, Unit] = {
            val dependencyKeys = deps(key)
            val dependencySubgraphs = dependencyKeys.map(graph(_))
            val directDependencyGraph = empty[K, Unit, Unit]
              .addNode(LNode(key, ()))
              .addEdges(dependencyKeys.map(k => LEdge(key, k, ())))
            dependencySubgraphs.foldLeft(directDependencyGraph)(_ union _)
          }
          
          val dependencyGraph = graph(target)

          val buildQueue = topoSort(dependencyGraph, dependencyGraph.roots.toVector)

          def buildKey(key: K): State[Store[I, K, V], Unit] = tasks.compute(key) match {
            case None =>
              State.pure(())
            case Some(task) =>
              for {
                store <- State.get[Store[I, K, V]]
                value = store.getValue(key)
                newTask = rebuilder.rebuild(key, value, task)
                newValue = ???
                _ <- State.modify[Store[I, K, V]](_.putValue(key, newValue))
              } yield ()
          }

          buildQueue
            .traverse(buildKey)
            .runS(store)
            .value
        }
      }
    }
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

  def busyM[K, V] = new System[Monad, Unit, K, V] {
    def build(tasks: TaskDescription[Monad, K, V], target: K, store: Store[Unit, K, V]): Store[Unit, K, V] = {
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

  def dependencies[K, V](task: Task[Applicative, K, V]): List[K] =
    task.run(k => Const[List[K], V](List(k))).getConst

  def track[M[_]: Monad, K, V](task: Task[Monad, K, V], fetch: K => M[V]): M[(List[(K, V)], V)] = {
    val trackedFetch = task.run { k =>
      for {
        v <- WriterT.liftF[M, List[(K, V)], V](fetch(k))
        _ <- WriterT.tell[M, List[(K, V)]](List((k, v)))
      } yield v
    }

    trackedFetch.run
  }

  def main(args: Array[String]): Unit = {
    import monix.eval.{ Task => IO }
    import monix.execution.Scheduler.Implicits.global
    import scala.concurrent.duration.Duration

    val store1 = Store.init[Unit, String, Int]((), k => if (k === "A1") 10 else 20)

    val result1 = busy[String, Int].build(sprsh1, "B2", store1)

    println("B1: " + result1.getValue("B1"))
    println("B2: " + result1.getValue("B2"))

    val store2 = Store.init[Unit, String, Int]((), _ match {
      case "A1" => 10
      case "A2" => 20
      case "C1" => 1
      case _ => 20
    })

    val result2 = busyM[String, Int].build(sprsh2, "B1", store2)
    println("B1: " + result2.getValue("B1"))

    val result3 = busyM[String, Int].build(sprsh2, "B2", store2)
    println("B2: " + result3.getValue("B2"))

    println("B1 dependencies: " + dependencies(sprsh1.compute("B1").get).mkString(", "))
    println("B2 dependencies: " + dependencies(sprsh1.compute("B2").get).mkString(", "))

    def fetchIO(k: String) = for {
      _ <- IO.eval { print(k + ": ") }
      i <- IO.eval { io.StdIn.readLine() }
    } yield i.toInt

    println(track(sprsh2.compute("B1").get, fetchIO).runSyncUnsafe(Duration.Inf))
    println(track(sprsh2.compute("B2").get, fetchIO).runSyncUnsafe(Duration.Inf))
  }
}
