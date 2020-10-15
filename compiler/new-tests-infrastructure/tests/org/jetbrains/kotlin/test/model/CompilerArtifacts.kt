/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.model

import org.jetbrains.kotlin.codegen.ClassFileFactory
import org.jetbrains.kotlin.test.backend.classic.ClassicBackendInputInfo
import org.jetbrains.kotlin.test.backend.ir.IrBackendInputInfo
import org.jetbrains.kotlin.test.components.Assertions
import org.jetbrains.kotlin.test.components.ConfigurationComponents
import org.jetbrains.kotlin.test.frontend.classic.ClassicFrontendSourceArtifacts
import org.jetbrains.kotlin.test.frontend.fir.FirSourceArtifact

sealed class ResultingArtifact {
    abstract class Source<R : Source<R>> : ResultingArtifact() {
        abstract val frontendKind: FrontendKind<R>
    }

    abstract class BackendInputInfo<I : BackendInputInfo<I>> : ResultingArtifact() {
        abstract val backendKind: BackendKind<I>
    }

    sealed class Binary<A : Binary<A>> : ResultingArtifact() {
        class Jvm(val classFileFactory: ClassFileFactory) : Binary<Jvm>() {
            override val artifactKind: ArtifactKind<Jvm>
                get() = ArtifactKind.Jvm
        }

        class Js : Binary<Js>() {
            override val artifactKind: ArtifactKind<Js>
                get() = ArtifactKind.Js
        }

        class Native : Binary<Native>() {
            override val artifactKind: ArtifactKind<Native>
                get() = ArtifactKind.Native
        }

        class KLib : Binary<KLib>() {
            override val artifactKind: ArtifactKind<KLib>
                get() = ArtifactKind.KLib
        }

        abstract val artifactKind: ArtifactKind<A>
    }
}

sealed class FrontendKind<R : ResultingArtifact.Source<R>> {
    object ClassicFrontend : FrontendKind<ClassicFrontendSourceArtifacts>()
    object FIR : FrontendKind<FirSourceArtifact>()

    companion object {
        fun fromString(string: String): FrontendKind<*>? {
            return when (string) {
                "ClassicFrontend" -> ClassicFrontend
                "FIR" -> FIR
                else -> null
            }
        }
    }
}

sealed class BackendKind<I : ResultingArtifact.BackendInputInfo<I>> {
    object ClassicBackend : BackendKind<ClassicBackendInputInfo>()
    object IrBackend : BackendKind<IrBackendInputInfo>()

    companion object {
        fun fromString(string: String): BackendKind<*>? {
            return when (string) {
                "ClassicBackend" -> ClassicBackend
                "IrBackend" -> IrBackend
                else -> null
            }
        }
    }
}

sealed class ArtifactKind<A : ResultingArtifact.Binary<A>> {
    object Jvm : ArtifactKind<ResultingArtifact.Binary.Jvm>()
    object Js : ArtifactKind<ResultingArtifact.Binary.Js>()
    object Native : ArtifactKind<ResultingArtifact.Binary.Native>()
    object KLib : ArtifactKind<ResultingArtifact.Binary.KLib>()

    companion object {
        fun fromString(string: String): ArtifactKind<*>? {
            return when (string) {
                "Jvm" -> Jvm
                "Js" -> Js
                "Native" -> Native
                "KLib" -> KLib
                else -> null
            }
        }
    }
}

abstract class DependencyProvider<R : ResultingArtifact.Source<R>>(
    val configurationComponents: ConfigurationComponents,
    testModules: List<TestModule>
) {
    protected val assertions: Assertions
        get() = configurationComponents.assertions

    private val testModulesByName: Map<String, TestModule> = testModules.map { it.name to it }.toMap()
    private val analyzedModules: MutableMap<String, R> = mutableMapOf()

    fun getTestModule(name: String): TestModule {
        return testModulesByName[name] ?: assertions.fail { "Module $name is not defined" }
    }

    fun getSourceModule(name: String): R? {
        return analyzedModules[name]
    }

    abstract fun getBinaryDependency(name: String): ResultingArtifact.Binary<*>?

    fun registerAnalyzedModule(name: String, results: R) {
        if (name in analyzedModules) {
            throw IllegalArgumentException("Analysis results of $name module already registered")
        }
        analyzedModules[name] = results
    }

    abstract fun registerCompiledBinary(name: String, artifact: ResultingArtifact.Binary<*>)
}
