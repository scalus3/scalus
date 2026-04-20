package scalus.cardano.network

/** JVM-specific impls for the N2N transport: `JvmAsyncByteChannel` over NIO2
  * `AsynchronousSocketChannel` and a loopback-socket stub responder for real-socket tests.
  */
package object jvm
