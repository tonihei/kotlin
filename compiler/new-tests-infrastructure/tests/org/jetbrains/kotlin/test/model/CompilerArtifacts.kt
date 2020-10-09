/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.model

import org.jetbrains.kotlin.test.components.Assertions
import org.jetbrains.kotlin.test.components.ConfigurationComponents

sealed class ResultingArtifact {
    abstract class Source : ResultingArtifact()
    abstract class BackendInitialInfo : ResultingArtifact()
    class KLib : ResultingArtifact()
    sealed class Binary : ResultingArtifact() {
        class Jvm : Binary()
        class Js : Binary()
        class Native : Binary()
    }
}

abstract class DependencyProvider<R : ResultingArtifact.Source>(
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

    abstract fun getCompiledKlib(name: String): ResultingArtifact.KLib?
    abstract fun getBinaryDependency(name: String): ResultingArtifact.Binary?

    fun registerAnalyzedModule(name: String, results: R) {
        if (name in analyzedModules) {
            throw IllegalArgumentException("Analysis results of $name module already registered")
        }
        analyzedModules[name] = results
    }

    abstract fun registerCompiledKLib(name: String, artifact: ResultingArtifact.KLib)
    abstract fun registerCompiledBinary(name: String, artifact: ResultingArtifact.Binary)
}
