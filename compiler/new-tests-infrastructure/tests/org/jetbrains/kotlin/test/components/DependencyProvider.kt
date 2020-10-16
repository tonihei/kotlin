/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.components

import org.jetbrains.kotlin.test.model.*

abstract class DependencyProvider : TestService {
    abstract fun getTestModule(name: String): TestModule

    abstract fun <R : ResultingArtifact.Source<R>> getSourceArtifact(module: TestModule, kind: FrontendKind<R>): R
    abstract fun <I : ResultingArtifact.BackendInputInfo<I>> getBackendInfo(module: TestModule, kind: BackendKind<I>): I
    abstract fun <A : ResultingArtifact.Binary<A>> getBinaryArtifact(module: TestModule, kind: ArtifactKind<A>): A
}

val TestServices.dependencyProvider: DependencyProvider by TestServices.testServiceAccessor()

class DependencyProviderImpl(
    val testServices: TestServices,
    testModules: List<TestModule>
) : DependencyProvider() {
    private val assertions: Assertions
        get() = testServices.assertions

    private val testModulesByName = testModules.map { it.name to it }.toMap()
    private val frontendArtifactsByModule: MutableMap<TestModule, MutableMap<FrontendKind<*>, ResultingArtifact.Source<*>>> = mutableMapOf()
    private val backendArtifactsByModule: MutableMap<TestModule, MutableMap<BackendKind<*>, ResultingArtifact.BackendInputInfo<*>>> = mutableMapOf()
    private val binaryArtifactsByModule: MutableMap<TestModule, MutableMap<ArtifactKind<*>, ResultingArtifact.Binary<*>>> = mutableMapOf()

    override fun getTestModule(name: String): TestModule {
        return testModulesByName[name] ?: assertions.fail { "Module $name is not defined" }
    }

    override fun <R : ResultingArtifact.Source<R>> getSourceArtifact(module: TestModule, kind: FrontendKind<R>): R {
        val artifact = frontendArtifactsByModule.getMap(module)[kind] ?: notFoundError(module, kind)
        @Suppress("UNCHECKED_CAST")
        return artifact as R
    }

    override fun <I : ResultingArtifact.BackendInputInfo<I>> getBackendInfo(module: TestModule, kind: BackendKind<I>): I {
        val artifact = backendArtifactsByModule.getMap(module)[kind] ?: notFoundError(module, kind)
        @Suppress("UNCHECKED_CAST")
        return artifact as I
    }

    override fun <A : ResultingArtifact.Binary<A>> getBinaryArtifact(module: TestModule, kind: ArtifactKind<A>): A {
        val artifact = binaryArtifactsByModule.getMap(module)[kind] ?: notFoundError(module, kind)
        @Suppress("UNCHECKED_CAST")
        return artifact as A
    }

    fun <R : ResultingArtifact.Source<R>> registerSourceArtifact(module: TestModule, artifact: ResultingArtifact.Source<R>) {
        val kind = artifact.frontendKind
        val previousValue = frontendArtifactsByModule.getMap(module).put(kind, artifact)
        if (previousValue != null) alreadyRegisteredError(module, kind)
    }

    fun <I : ResultingArtifact.BackendInputInfo<I>> registerBackendInfo(module: TestModule, artifact: ResultingArtifact.BackendInputInfo<I>) {
        val kind = artifact.backendKind
        val previousValue = backendArtifactsByModule.getMap(module).put(kind, artifact)
        if (previousValue != null) alreadyRegisteredError(module, kind)
    }

    fun <A : ResultingArtifact.Binary<A>> registerBinaryArtifact(module: TestModule, artifact: ResultingArtifact.Binary<A>) {
        val kind = artifact.artifactKind
        val previousValue = binaryArtifactsByModule.getMap(module).put(kind, artifact)
        if (previousValue != null) alreadyRegisteredError(module, kind)
    }

    private fun notFoundError(module: TestModule, kind: Any): Nothing {
        error("Artifact with kind $kind is not registered for module ${module.name}")
    }

    private fun alreadyRegisteredError(module: TestModule, kind: Any): Nothing {
        error("Artifact with kind $kind already registered for module ${module.name}")
    }

    private fun <K, V, R> MutableMap<K, MutableMap<V, R>>.getMap(key: K): MutableMap<V, R> {
        return getOrPut(key) { mutableMapOf() }
    }
}
