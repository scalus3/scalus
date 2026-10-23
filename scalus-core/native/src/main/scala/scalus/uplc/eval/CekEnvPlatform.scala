package scalus.uplc.eval

import scala.collection.immutable.ArraySeq

// Unused since the machine builds its environments as `CekEnv`. Kept because removing it would
// break binary compatibility (the object compiles to a public class).
private[eval] object CekEnvPlatform {

    /** An empty environment. */
    val empty: CekValEnv = ArraySeq.empty
}
