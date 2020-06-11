// !LANGUAGE: +NewInference
// !DIAGNOSTICS: -UNUSED_VARIABLE -ASSIGNED_BUT_NEVER_ACCESSED_VARIABLE -UNUSED_VALUE -UNUSED_PARAMETER -UNUSED_EXPRESSION
// SKIP_TXT

// FILE: TestCase.kt
// TESTCASE NUMBER: 1
package testsCase1

open class A {
    operator fun invoke() = print("invoke")
}

enum class Super_2 {
    V1, V2;

    companion object values : A() {}

    private fun case() {
        <!DEBUG_INFO_CALL("fqName: testsCase1.A.invoke; typeCall: variable&invoke")!>values()<!>
    }

    enum class NestedWithCompanion {
        V1;

        companion object values : A() {}

        private fun case() {
            <!DEBUG_INFO_CALL("fqName: testsCase1.A.invoke; typeCall: variable&invoke")!>values()<!>
        }
    }

    enum class Nested {
        V1;

        private fun case() {
            <!DEBUG_INFO_CALL("fqName: testsCase1.Super_2.Nested.values; typeCall: function")!>values()<!>
        }
    }
}

// FILE: TestCase.kt
// TESTCASE NUMBER: 2
package testsCase2

open class A {
    operator fun invoke(value: String) = print("invoke $value")
}

enum class Super_2 {
    V1, V2;

    companion object valueOf : A() {}

    private fun case() {
        <!DEBUG_INFO_CALL("fqName: testsCase2.A.invoke; typeCall: variable&invoke")!>valueOf("")<!>
    }

    enum class NestedWithCompanion {
        V1;

        companion object valueOf : A() {}

        private fun case() {
            <!DEBUG_INFO_CALL("fqName: testsCase2.A.invoke; typeCall: variable&invoke")!>valueOf("")<!>
        }
    }

    enum class Nested {
        V1;

        private fun case() {
            <!DEBUG_INFO_CALL("fqName: testsCase2.Super_2.Nested.valueOf; typeCall: function")!>valueOf("")<!>
        }
    }
}


// FILE: TestCase.kt
// TESTCASE NUMBER: 3
package testsCase3

open class A {
    operator fun invoke() = print("invoke")
}

enum class Super_2 {
    V1, V2;

    object values : A() {}

    private fun case() {
        <!DEBUG_INFO_CALL("fqName: testsCase3.A.invoke; typeCall: variable&invoke")!>values()<!>
    }

    enum class NestedWithCompanion {
        V1;

        object values : A() {}

        private fun case() {
            <!DEBUG_INFO_CALL("fqName: testsCase3.A.invoke; typeCall: variable&invoke")!>values()<!>
        }
    }

    enum class Nested {
        V1;

        private fun case() {
            <!DEBUG_INFO_CALL("fqName: testsCase3.Super_2.Nested.values; typeCall: function")!>values()<!>
        }
    }
}

// FILE: TestCase.kt
// TESTCASE NUMBER: 4
package testsCase4

open class A {
    operator fun invoke(value: String) = print("invoke $value")
}
open class B {
    operator fun invoke(value: String) = print("invoke $value")
}

enum class Super_2 {
    V1, V2;

    object valueOf : A() {}

    private fun case() {
        <!DEBUG_INFO_CALL("fqName: testsCase4.A.invoke; typeCall: variable&invoke")!>valueOf("")<!>
    }

    enum class NestedWithCompanion {
        V1;

        object valueOf : B() {}

        private fun case() {
            <!DEBUG_INFO_CALL("fqName: testsCase4.B.invoke; typeCall: variable&invoke")!>valueOf("")<!>
        }
    }

    enum class Nested {
        V1;

        private fun case() {
            <!DEBUG_INFO_CALL("fqName: testsCase4.Super_2.Nested.valueOf; typeCall: function")!>valueOf("")<!>
        }
    }
}