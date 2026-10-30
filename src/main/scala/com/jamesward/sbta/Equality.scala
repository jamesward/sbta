package com.jamesward.sbta

import java.nio.file.Path

/**
 * Paths are compared as values throughout (a build's root against a target's build, a
 * file against a module's sources): `Path.equals` is the file system's own equality (the
 * same file system and path; case-insensitive on Windows), which no property of a path
 * reproduces, so strict equality gets it here for `Path` alone.
 */
given CanEqual[Path, Path] = CanEqual.derived
