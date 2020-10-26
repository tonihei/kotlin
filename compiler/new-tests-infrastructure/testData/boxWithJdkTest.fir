FILE: boxWithJdkTest.kt
    public final fun box(): R|kotlin/String| {
        lval list: R|java/util/ArrayList<ft<kotlin/String, kotlin/String?>!>| = Q|java/util|.R|java/util/ArrayList.ArrayList|<R|ft<kotlin/String, kotlin/String?>!|>()
        R|<local>/list|.R|FakeOverride<java/util/ArrayList.add: R|kotlin/Boolean|>|(String(OK))
        ^box R|<local>/list|.R|FakeOverride<java/util/ArrayList.get: R|ft<kotlin/String, kotlin/String?>!|>|(Int(0))
    }
