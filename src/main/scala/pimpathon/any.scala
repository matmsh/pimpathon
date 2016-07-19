package pimpathon

import scala.{PartialFunction ⇒ ~>}
import scala.collection.generic.{Growable, Shrinkable}
import scala.util.Try

import pimpathon.boolean._
import pimpathon.function._


object any {
  implicit class AnyPimps[A](val a: A) extends AnyVal {
    def calc[B](f: A ⇒ B): B = f(a)
    def |>[B](f: A ⇒ B): B = f(a)
    def calcIf[B](p: Predicate[A])(f: A ⇒ B): Option[B] = p(a).option(f(a))
    def calcUnless[B](p: Predicate[A])(f: A ⇒ B): Option[B] = (!p(a)).option(f(a))
    def calcPF[B](pf: A ~> B): Option[B] = pf.lift(a)
    def transform(pf: A ~> A): A = pf.unify(a)

    def tapIf[Discarded](p: Predicate[A])(actions: (A ⇒ Discarded)*): A     = if (p(a)) tap(actions: _*) else a
    def tapUnless[Discarded](p: Predicate[A])(actions: (A ⇒ Discarded)*): A = if (p(a)) a else tap(actions: _*)

    def tapPF[Discarded](action: A ~> Discarded): A = { action.lift(a); a }

    def attempt[B](f: A ⇒ B): Try[B] = Try(f(a))

    def partialMatch[B](pf: A ~> B): Option[B] = PartialFunction.condOpt(a)(pf)

    def lpair[B](f: A ⇒ B): (B, A) = (f(a), a)
    def rpair[B](f: A ⇒ B): (A, B) = (a, f(a))

    def filterSelf(p: Predicate[A]): Option[A] = p(a).option(a)
    def ifSelf(p: Predicate[A]): Option[A] = p(a).option(a)

    def filterNotSelf(p: Predicate[A]): Option[A] = (!p(a)).option(a)
    def unlessSelf(p: Predicate[A]): Option[A] = (!p(a)).option(a)

    def isOneOf(as: A*): Boolean = as.contains(a)
    def isNotOneOf(as: A*): Boolean = !as.contains(a)

    def containedIn(s: Set[A]): Boolean = s.contains(a)
    def notContainedIn(s: Set[A]): Boolean = !s.contains(a)

    def passes: AnyCapturer[A] = new AnyCapturer[A](a, b ⇒ b.option(a))
    def fails: AnyCapturer[A]  = new AnyCapturer[A](a, b ⇒ (!b).option(a))

    def withFinally[B](f: A ⇒ Unit)(t: A ⇒ B): B = try t(a) finally f(a)
    def tryFinally[B](t: A ⇒ B)(f: A ⇒ Unit): B = try t(a) finally f(a)

    def cond[B](p: Predicate[A], ifTrue: A ⇒ B, ifFalse: A ⇒ B): B = if (p(a)) ifTrue(a) else ifFalse(a)

    def addTo(as: Growable[A]): A = tap(as += _)
    def removeFrom(as: Shrinkable[A]): A = tap(as -= _)

    def unfold[B](f: A ⇒ Option[(B, A)]): Stream[B] = f(a).fold(Stream.empty[B])(ba ⇒ ba._1 #:: ba._2.unfold(f))

    // These methods are aliased to suit individual preferences
    def update[Discarded](actions: (A ⇒ Discarded)*): A         = tap(actions: _*)
    def withSideEffect[Discarded](actions: (A ⇒ Discarded)*): A = tap(actions: _*)
    def tap[Discarded](actions: (A ⇒ Discarded)*): A            = { actions.foreach(action ⇒ action(a)); a }

    def bounded(lower: A, upper: A)(implicit na: Numeric[A]): A = na.min(na.max(lower, a), upper)
  }

  class AnyCapturer[A](a: A, andThen: Boolean ⇒ Option[A]) {
    def one(disjuncts: Predicate[A]*): Option[A]  = andThen(function.or(disjuncts: _*).apply(a))
    def all(conjuncts: Predicate[A]*): Option[A]  = andThen(function.and(conjuncts: _*).apply(a))
    def none(conjuncts: Predicate[A]*): Option[A] = andThen(function.nand(conjuncts: _*).apply(a))
    def some(disjuncts: Predicate[A]*): Option[A] = andThen(function.nor(disjuncts: _*).apply(a))
  }
}