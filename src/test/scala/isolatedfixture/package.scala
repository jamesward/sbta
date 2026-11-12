package isolatedfixture

/** Top-level definitions: `inspect` lists them with the package. */
def greet(name: String): String = s"hello $name"

given defaultProbe: Probe = Probe()
