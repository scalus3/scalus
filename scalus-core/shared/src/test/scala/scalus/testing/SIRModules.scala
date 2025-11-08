package scalus.testing

import scalus.serialization.flat
import scalus.serialization.flat.DecoderState
import scalus.serialization.flat.FlatInstances.given
import scalus.sir.Module

object SIRModules {

    def load(name: String): Module = {
        val filename = name.replace('.', '/') + ".sir"
        val inputStream = getClass.getClassLoader.getResourceAsStream(filename)
        if inputStream == null then {
            throw new RuntimeException(s"Module $name not found")
        }
        val bytes = inputStream.readAllBytes()
        val decoderState = DecoderState(bytes)
        val module = flat.decode[Module](decoderState)
        module
    }

}
