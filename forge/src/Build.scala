package forge

import cats.data.State

 abstract class Store[I, K, V] {
  def getInfo: I
  def putInfo(i: I): Store[I, K, V]
  def getValue(k: K): V
  def putValue(k: K, v: V): Store[I, K, V]
  def getHash(k: K): Int
}

object Build {
  abstract class Task[K, V] {
    def run[F[_]](fetch: K => F[V]): F[V]
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
}