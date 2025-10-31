package scalus.serialization.flat

import java.util.IdentityHashMap
import scala.collection.mutable.Map as MutableMap

type HSRIdentityHashMap = IdentityHashMap[HashConsedRef[?], HashConsedRef[?]]

/** When we read from the HashConsedRead.State, we can have forward references, which will be
  * resolved after the whole structure is readed. Here, HashConsedRef can hold object ro Ref or
  * object of some intermediate representation, which holds other refs.
  *
  * @tparam A
  */
trait HashConsedRef[+A <: AnyRef] {

    def isForward: Boolean = false

    /** Should be called after the decoding of the whole structures.
      *
      * @return
      *   valua of A
      */
    def finValue(hashConsed: HashConsed.State, level: Int, parents: HSRIdentityHashMap): A

}

object HashConsedRef {

    def fromData[A <: AnyRef](a: A): HashConsedRef[A] =
        HashConsed.ConstRef.fromData(a)

    def fromForward[A <: AnyRef](
        state: HashConsed.State,
        ihc: Int,
        tag: HashConsed.Tag
    ): HashConsedRef[A] =
        state.lookup(ihc, tag) match
            case None           => HashConsed.ForwardRef.create(state, ihc, tag)
            case Some(Left(fw)) =>
                HashConsed.ForwardRef.create(state, ihc, tag)
            case Some(Right(a)) => a.asInstanceOf[HashConsedRef[A]]

    def deferred[A <: AnyRef](
        op: (HashConsed.State, Int, HSRIdentityHashMap) => A
    ): HashConsedRef[A] =
        new HashConsedRef[A] {
            val createdAt = new Exception()
            var finishedValue: Option[A] = None

            override def finValue(
                hashConsed: HashConsed.State,
                level: Int,
                parents: IdentityHashMap[HashConsedRef[?], HashConsedRef[?]]
            ): A =
                finishedValue match
                    case Some(a) => a
                    case None    =>
                        if parents.get(this) != null then
                            println("cyclic reference for ex, created at:")
                            createdAt.printStackTrace()
                            throw IllegalStateException(
                              s"Cyclic reference, this= $this, parents=$parents"
                            )
                        parents.put(this, this)
                        val retval = op(hashConsed, level + 1, parents)
                        finishedValue = Some(retval)
                        parents.remove(this)
                        retval

        }

}

/** Objects are hash-consed during restoring from file. Note, that identityHashCode can be different
  * on the different VM-s, so it is not safe to reky on it. value-based hash-code now is not
  * determenistics on recuresive data structures with self-references. So, we use identityHashCode
  * on VN which store file and restore internal references on other VN.
  */
object HashConsed {

    opaque type Tag = Int

    def tag(value: Int): Tag = value

    class ForwardRefAcceptor(
        val ihc: Int,
        val tag: Tag,
        var setRefActions: List[HashConsedRef[?] => Unit]
    ) {

        def addAction(state: HashConsed.State, action: HashConsedRef[?] => Unit): Unit =
            state.refs.get((ihc, tag)) match
                case None =>
                    setRefActions = action :: setRefActions
                case Some(v) =>
                    action(v)

    }

    class CachedTaggedRef[A <: AnyRef](val tag: Tag, val ref: HashConsedRef[A])
        extends HashConsedRef[A] {

        private var data: A | Null = null

        def cache: A | Null = data

        override def finValue(hashConsed: State, level: Int, parents: HSRIdentityHashMap): A =
            if data == null then {
                // here we does not need check for cyclic references, because
                //   this is proxuy arround ref,
                data = ref.finValue(hashConsed, level + 1, parents)
            }
            data.asInstanceOf[A]

    }

    class ForwardValueAcceptor(var setValueActions: List[AnyRef => Unit])

    class MutRef[A <: AnyRef](var value: A | Null) extends HashConsedRef[A] {

        override def finValue(hashConsed: State, level: Int, parents: HSRIdentityHashMap): A =
            if value == null then throw IllegalStateException("Null reference during reading")
            value.asInstanceOf[A]

        def setValue(a: A): Unit =
            value = a

    }

    object MutRef {

        def fromData[A <: AnyRef](a: A): MutRef[A] =
            new MutRef[A](a)

    }

    class ForwardRef[A <: AnyRef](val ihc: Int, val tag: Tag) extends HashConsedRef[A] {

        // not thread-safe, but we are in single-threaded mode
        //  mb later use AtomicReference
        private var ref: HashConsedRef[?] | Null = null
        // var finRef: A | Null = null

        override def isForward: Boolean = true

        override def finValue(hashConsed: State, level: Int, parents: HSRIdentityHashMap): A =
            if ref == null then
                ref = hashConsed.lookup(ihc, tag) match
                    case None =>
                        throw IllegalStateException(s"Forward reference not creaded: $ihc, $tag")
                    case Some(Left(fw)) =>
                        throw IllegalStateException(s"Forward reference not resolved: $ihc, $tag")
                    case Some(Right(a)) =>
                        if parents.get(a) != null then
                            throw IllegalStateException(s"Cycled forward referenc: $ihc, $tag")
                        if a eq this then
                            throw IllegalStateException(
                              s"Forward reference not non-rec resolved: $ihc, $tag"
                            )
                        a
            parents.put(this, this)
            val retval = ref.finValue(hashConsed, level + 1, parents).asInstanceOf[A]
            parents.remove(this)
            retval

    }

    object ForwardRef {

        def create[A <: AnyRef](state: State, ihc: Int, tag: Tag): HashConsedRef[A] =
            val retval = new ForwardRef[A](ihc, tag)
            val acceptor =
                new ForwardRefAcceptor(ihc, tag, List((a: HashConsedRef[?]) => retval.ref = a))
            state.putForwardRef(acceptor)
            retval

    }

    case class ConstRef[A <: AnyRef](value: A) extends HashConsedRef[A] {

        override def finValue(hashConsed: State, level: Int, parents: HSRIdentityHashMap): A = value

    }

    object ConstRef {

        def fromData[A <: AnyRef](value: A): ConstRef[A] =
            new ConstRef(value)

    }

    /** @param forwardRefAcceptors
      *   \- set of forward references, which are not yet resolved from hashConded
      * @param refs
      *   \- set of references
      * @param forwardValueAcceptors
      *   \- set of callbacks, which should be called after the value is readed.
      */
    case class State(
        forwardRefAcceptors: MutableMap[(Int, Int), ForwardRefAcceptor],
        refs: MutableMap[(Int, Int), CachedTaggedRef[?]],
        forwardValueAcceptors: MutableMap[(Int, Int), ForwardValueAcceptor]
    )

    object State:
        def empty = State(MutableMap.empty, MutableMap.empty, MutableMap.empty)

    def putForwardRefAcceptor(state: State, fw: ForwardRefAcceptor): Unit =
        state.refs.get((fw.ihc, fw.tag)) match
            case None =>
                state.forwardRefAcceptors.get((fw.ihc, fw.tag)) match
                    case None =>
                        state.forwardRefAcceptors.put((fw.ihc, fw.tag), fw)
                    case Some(v) =>
                        v.setRefActions = fw.setRefActions ++ v.setRefActions
            case Some(ref) =>
                fw.setRefActions.foreach(_(ref))
                fw.setRefActions = Nil

    def putForwadValueAcceptor[A <: AnyRef](
        state: State,
        ihc: Int,
        tag: Tag,
        acceptor: A => Unit
    ): Unit =
        state.forwardValueAcceptors.get((ihc, tag)) match
            case None =>
                state.forwardValueAcceptors.put(
                  (ihc, tag),
                  ForwardValueAcceptor(List(acceptor.asInstanceOf[AnyRef => Unit]))
                )
            case Some(v) =>
                v.setValueActions = acceptor.asInstanceOf[AnyRef => Unit] :: v.setValueActions

    def setRef[A <: AnyRef](state: State, ihc: Int, tag: Tag, ra: HashConsedRef[A]): Unit =
        if ra.isForward then throw IllegalStateException("Forward reference in setRef")
        val key = (ihc, tag)
        state.refs.get(key) match
            case None =>
                state.refs.put(key, CachedTaggedRef(tag, ra))
            case Some(ref) =>
                throw IllegalStateException(s"Double setRef for $key")
        state.forwardRefAcceptors.get(key) match
            case None     =>
            case Some(fw) =>
                fw.setRefActions.foreach(_(ra))
                fw.setRefActions = Nil

    def lookupValue(s: State, ihc: Int, tag: Tag): Option[HashConsedRef[?]] =
        s.refs.get((ihc, tag))

    def lookup(s: State, ihc: Int, tag: Tag): Option[Either[ForwardRefAcceptor, HashConsedRef[?]]] =
        lookupValue(s, ihc, tag) match
            case None =>
                s.forwardRefAcceptors.get((ihc, tag)) match
                    case None     => None
                    case Some(fw) =>
                        Some(Left(fw))
            case Some(r) => Some(Right(r))

    def finishCallbacks(s: State, debug: Boolean = false): Unit =
        for (k, v) <- s.forwardValueAcceptors do {
            s.refs.get(k) match
                case None =>
                    throw IllegalStateException(s"Forward value acceptor without value: $k")
                case Some(ref) =>
                    val a = ref.finValue(s, 0, new HSRIdentityHashMap)
                    v.setValueActions.foreach(_(a))
                    v.setValueActions = Nil
        }

}

extension (s: HashConsed.State)

    def putForwardRef(ihc: Int, tag: HashConsed.Tag, action: AnyRef => Unit): Unit =
        HashConsed.putForwardRefAcceptor(s, HashConsed.ForwardRefAcceptor(ihc, tag, List(action)))

    def putForwardRef(fw: HashConsed.ForwardRefAcceptor): Unit =
        HashConsed.putForwardRefAcceptor(s, fw)

    def setRef[A <: AnyRef](ihc: Int, tag: HashConsed.Tag, a: HashConsedRef[A]): Unit =
        HashConsed.setRef(s, ihc, tag, a)

    def lookupValue(ihc: Int, tag: HashConsed.Tag): Option[HashConsedRef[?]] =
        HashConsed.lookupValue(s, ihc, tag)

    def lookup(
        ihc: Int,
        tag: HashConsed.Tag
    ): Option[Either[HashConsed.ForwardRefAcceptor, HashConsedRef[?]]] =
        HashConsed.lookup(s, ihc, tag)

    def putForwardValueAcceptor[A <: AnyRef](
        ihc: Int,
        tag: HashConsed.Tag,
        acceptor: A => Unit
    ): Unit =
        HashConsed.putForwadValueAcceptor(s, ihc, tag, acceptor)

    def finishCallbacks(debug: Boolean = false): Unit =
        HashConsed.finishCallbacks(s, debug)
