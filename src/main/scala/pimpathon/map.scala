package pimpathon

import scala.{PartialFunction ⇒ ~>}
import scala.collection.{breakOut, mutable ⇒ M, GenTraversable, GenTraversableLike, GenTraversableOnce}
import scala.collection.immutable.{SortedMap, TreeMap}

import pimpathon.genTraversableLike.{GenTraversableLikeOfTuple2Mixin, GenTraversableLikePimpsMixin}

import pimpathon.any._
import pimpathon.function._
import pimpathon.multiMap._
import pimpathon.tuple._


object map {
  implicit class MapPimps[K, V](map: Map[K, V])
    extends GenTraversableLikePimpsMixin[(K, V), GenTraversable]
    with GenTraversableLikeOfTuple2Mixin[K, V] {

    def containsAny(ok: Option[K]): Boolean = ok.exists(map.contains)
    def containsAll(ok: Option[K]): Boolean = ok.forall(map.contains)
    def containsAny[GK <: GenTraversableOnce[K]](gk: GK): Boolean = gk.exists(map.contains)
    def containsAll[GK <: GenTraversableOnce[K]](gk: GK): Boolean = gk.forall(map.contains)
    def containsEntry(kv: (K, V)): Boolean = containsEntry(kv._1, kv._2)
    def containsEntry(k: K, v: V): Boolean = map.get(k).contains(v)

    def get(ok: Option[K]): Option[V]               = ok.flatMap(map.get)
    def getOrThrow(k: K, message: String): V        = getOrThrow(k, new IllegalArgumentException(message))
    def getOrThrow(k: K, exception: ⇒ Exception): V = map.getOrElse(k, throw exception)

    def findKey(p: Predicate[K]): Option[K]   = keyFor.matchingKey(p)
    def findValue(p: Predicate[V]): Option[V] = valueFor.matchingValue(p)

    def filterKeysNot(p: Predicate[K]): Map[K, V]   = map.filterNot(kv ⇒ p(kv._1))
    def filterValuesNot(p: Predicate[V]): Map[K, V] = map.filterNot(kv ⇒ p(kv._2))
    def filterValues(p: Predicate[V]): Map[K, V]    = map.filter(kv ⇒ p(kv._2))


    def keyExists(p: Predicate[K]): Boolean   = map.exists(kv ⇒ p(kv._1))
    def valueExists(p: Predicate[V]): Boolean = map.exists(kv ⇒ p(kv._2))

    def emptyTo(empty: ⇒ Map[K, V]): Map[K, V]            = uncons(empty, _ ⇒ map)
    def calcIfNonEmpty[A](f: Map[K, V] ⇒ A): Option[A]    = map.calcIf(_.nonEmpty)(f)
    def uncons[A](empty: ⇒ A, nonEmpty: Map[K, V] ⇒ A): A = if (map.isEmpty) empty else nonEmpty(map)

    def reverse(f: Set[K] ⇒ K): Map[V, K] = reverseToMultiMap.mapValuesEagerly(f)
    def reverseToMultiMap: MultiMap[Set, V, K] = map.map(_.swap)(breakOut)

    def sortBy[C: Ordering](f: K => C): SortedMap[K, V] = sorted(Ordering[C].on[K](f))
    def sorted(implicit ordering: Ordering[K]): SortedMap[K, V] = TreeMap.empty[K, V](ordering) ++ map

    def composeM[C](other: Map[C, K]): Map[C, V] = other.andThenM(map)
    def andThenM[W](other: Map[V, W]): Map[K, W] = updateValues(other.get _)

    def toMutable: M.Map[K, V] = mutable
    def mutable: M.Map[K, V] = M.Map.empty[K, V] ++ map

    def entryFor: MapAndThen[K, V, (K, V)] = new MapAndThen[K, V, (K, V)](map, identity[(K, V)])
    def keyFor:   MapAndThen[K, V, K]      = new MapAndThen[K, V, K](map, key)
    def valueFor: MapAndThen[K, V, V]      = new MapAndThen[K, V, V](map, value)

    def partitionKeysBy[C](pf: K ~> C): (Map[K, V], Map[C, V])   = partitionEntriesBy(pf.first[V])
    def partitionValuesBy[W](pf: V ~> W): (Map[K, V], Map[K, W]) = partitionEntriesBy(pf.second[K])

    def partitionEntriesBy[C, W](pf: (K, V) ~> (C, W)): (Map[K, V], Map[C, W]) =
      map.partition(pf.isUndefinedAt).tmap(identity, _.map(pf))

    def mapKeysEagerly[C](f: K ⇒ C): Map[C, V]         = map.map { case (k, v) ⇒ (f(k), v) }
    def mapValuesEagerly[W](f: V ⇒ W): Map[K, W]       = map.map { case (k, v) ⇒ (k, f(v)) }
    def mapEntries[C, W](f: K ⇒ V ⇒ (C, W)): Map[C, W] = map.map { case (k, v) ⇒ f(k)(v)   }

    def seqMapKeys[C](f: K ⇒ Option[C]): Option[Map[C, V]]                = seqMapEntries(k ⇒ v ⇒ f(k).map(_ → v))
    def seqMapValues[W](f: V ⇒ Option[W]): Option[Map[K, W]]              = seqMapEntries(k ⇒ v ⇒ f(v).map(k → _))
    def seqMapEntries[C, W](f: K ⇒ V ⇒ Option[(C, W)]): Option[Map[C, W]] = map.seqMap { case (k, v) ⇒ f(k)(v) }

    def collectKeys[C](pf: K ~> C): Map[C, V] = map.collect(pf.first)
    def collectValues[W](pf: V ~> W): Map[K, W] = map.collect(pf.second)

    def updateValue(key: K, f: V ⇒ Option[V]): Map[K, V] =
      map.get(key).flatMap(f).fold(map - key)(newValue ⇒ map + ((key, newValue)))

    def updateKeys[C](pf: K ~> C): Map[C, V] = updateKeys(pf.lift)
    def updateValues[W](pf: V ~> W): Map[K, W] = updateValues(pf.lift)

    def updateKeys[C](f: K ⇒ Option[C]): Map[C, V]   = map.flatMap(kv ⇒ f(kv._1).map(_ → kv._2))
    def updateValues[W](f: V ⇒ Option[W]): Map[K, W] = map.flatMap(kv ⇒ f(kv._2).map(kv._1 → _))

    protected def gtl: GenTraversableLike[(K, V), GenTraversable[(K, V)]] = map
    protected def cc: GenTraversable[(K, V)] = map
  }

  class MapAndThen[K, V, A](map: Map[K, V], andThen: ((K, V)) ⇒ A) {
    def maxValue(implicit O: Ordering[V]): Option[A] = map.calcIfNonEmpty(_.maxBy(value)).map(andThen)
    def minValue(implicit O: Ordering[V]): Option[A] = map.calcIfNonEmpty(_.minBy(value)).map(andThen)
    def maxKey(implicit O: Ordering[K]): Option[A] = map.calcIfNonEmpty(_.maxBy(key)).map(andThen)
    def minKey(implicit O: Ordering[K]): Option[A] = map.calcIfNonEmpty(_.minBy(key)).map(andThen)

    def matchingKey(p: Predicate[K]): Option[A]   = map.find(kv ⇒ p(kv._1)).map(andThen)
    def matchingValue(p: Predicate[V]): Option[A] = map.find(kv ⇒ p(kv._2)).map(andThen)
  }

  @inline private def key[K, V]:   ((K, V)) ⇒ K = (kv: (K, V)) ⇒ kv._1
  @inline private def value[K, V]: ((K, V)) ⇒ V = (kv: (K, V)) ⇒ kv._2
}