package scalus.cardano.n2n

/** JVM-specific impls for the N2N transport: `JvmAsyncByteChannel` over NIO2
  * `AsynchronousSocketChannel`, `JvmTimer` backed by `ScheduledExecutorService`, and a
  * loopback-socket stub responder for real-socket tests.
  */
package object jvm
