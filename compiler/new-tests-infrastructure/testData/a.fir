Module: common
FILE: b.kt
    public final fun foo(): R|kotlin/String| {
        ^foo Q|Utils|.R|/Utils.create|()
    }
Module: main
FILE: a.kt
    public final fun test(): R|kotlin/Int| {
        ^test R|/foo|().R|kotlin/String.length|
    }
    public final fun test_2(): R|ft<kotlin/String, kotlin/String?>!| {
        ^test_2 Q|Utils|.R|/Utils.create|()
    }
