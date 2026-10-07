package scalus.uplc.eval

import scala.collection.immutable.ArraySeq

private[eval] object CekEnvPlatform {

    /** An empty environment. */
    val empty: CekValEnv = ArraySeq.empty
}
