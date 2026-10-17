# ProGuard rules for the sbta jar (see project/SbtaShrink.scala). Shrinking only:
# nothing is renamed or optimized, so names seen at runtime are the source's.
-dontobfuscate
-dontoptimize
# Optional integrations the libraries probe for at runtime (log4j, slf4j, Brotli,
# BlockHound, Jetty ALPN, …) aren't in the jar; their references are expected.
-dontwarn **
-dontnote **
-keepattributes *

-keep class com.jamesward.sbta.Main { public static void main(java.lang.String[]); }

# Java enums: Enum.valueOf and switch maps call values() reflectively.
-keepclassmembers enum * { public static **[] values(); public static ** valueOf(java.lang.String); }

# Netty, whole (~1.5 MB of the jar): it reaches its own and the JDK's internals by
# reflection and method handles with computed names (ReflectionSpec lists ~45 such
# call sites), probes for optional libraries, and its native transports (epoll on
# Linux, kqueue on macOS) are JNI, which look up classes and members by name.
-keep class io.netty.** { *; }

# Scala 3 lazy vals: a class's static initializer looks up each lazy val's field by
# name (`findVarHandle(C.class, "x$lzy1", …)`), whether or not the lazy val is used.
-keepclassmembers class * { *** *$lzy*; }
