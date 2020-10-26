Module: a
FILE: a.kt
    public final fun foo(): R|kotlin/String| {
        ^foo String(OK)
    }
Module: b
FILE: b.kt
    public final fun box(): R|kotlin/String| {
        ^box R|/foo|()
    }
