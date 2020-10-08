/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.model

abstract class FrontendResults

abstract class BackendInitialInfo

sealed class ResultingArtifact {
    abstract class Source<R : FrontendResults> : ResultingArtifact()
    class KLib : ResultingArtifact()
    sealed class Binary : ResultingArtifact() {
        class Jvm : Binary()
        class Js : Binary()
        class Native : Binary()
    }
}

abstract class DependencyProvider<R : FrontendResults, out A : ResultingArtifact.Source<R>> {
    abstract fun getTestModule(name: String): TestModule

    abstract fun getSourceModule(name: String): A?
    abstract fun getCompiledKlib(name: String): ResultingArtifact.KLib?
    abstract fun getBinaryDependency(name: String): ResultingArtifact.Binary?

    abstract fun registerAnalyzedModule(name: String, results: R)
    abstract fun registerCompiledKLib(name: String, artifact: ResultingArtifact.KLib)
    abstract fun registerCompiledBinary(name: String, artifact: ResultingArtifact.Binary)
}
