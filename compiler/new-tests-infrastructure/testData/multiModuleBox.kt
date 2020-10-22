// MODULE: a
// FILE: a.kt
fun foo(): String = "OK"

// MODULE: b
// DEPENDENCY: a BINARY
// FILE: b.kt
fun box(): String = foo()
