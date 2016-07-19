package pimpathon

import scala.collection.{mutable ⇒ M}

import pimpathon.function._


object mutableMap {
  implicit class MutableMapPimps[K, V](val map: M.Map[K, V]) extends AnyVal {
    def retainKeys(p: Predicate[K]): M.Map[K, V]   = map.retain((k, _) ⇒ p(k))
    def retainValues(p: Predicate[V]): M.Map[K, V] = map.retain((_, v) ⇒ p(v))
  }
}