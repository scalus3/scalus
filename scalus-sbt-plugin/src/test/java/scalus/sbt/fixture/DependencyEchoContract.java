package scalus.sbt.fixture;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

/**
 * Stands in for a compiled Scalus Contract in BlueprintGeneratorTest. It has the shape the
 * blueprint generator loads by reflection: a static MODULE$ and a blueprintJson() method.
 *
 * <p>Its JSON carries the text of a resource that only a dependency provides. A real contract's
 * script bytes also come from a dependency: the Scalus lowering and optimizer in scalus-core. Java
 * keeps the fixture free of the Scala runtime, which the generator's isolated class loader does not
 * supply.
 */
public final class DependencyEchoContract {
    public static final DependencyEchoContract MODULE$ = new DependencyEchoContract();

    public static final String RESOURCE = "scalus-sbt-test/dependency-output.txt";

    private DependencyEchoContract() {}

    public String blueprintJson() throws IOException {
        ClassLoader loader = DependencyEchoContract.class.getClassLoader();
        try (InputStream in = loader.getResourceAsStream(RESOURCE)) {
            if (in == null) throw new IOException("No dependency provides " + RESOURCE);
            String fromDependency = new String(in.readAllBytes(), StandardCharsets.UTF_8);
            return "{\"compiledCode\":\"" + fromDependency + "\"}";
        }
    }
}
