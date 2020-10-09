/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.frontend.fir

import org.jetbrains.kotlin.fir.java.FirProjectSessionProvider
import org.jetbrains.kotlin.fir.session.FirJvmModuleInfo
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.test.components.ConfigurationComponents
import org.jetbrains.kotlin.test.model.DependencyProvider
import org.jetbrains.kotlin.test.model.ResultingArtifact
import org.jetbrains.kotlin.test.model.TestModule

class FirDependencyProvider(
    configurationComponents: ConfigurationComponents,
    testModules: List<TestModule>
) : DependencyProvider<FirSourceArtifact>(configurationComponents, testModules) {
    val firSessionProvider = FirProjectSessionProvider()

    private val analyzedModules = mutableMapOf<String, FirSourceArtifact>()
    private val builtinsByModule: MutableMap<TestModule, FirJvmModuleInfo> = mutableMapOf()
    private val firModuleInfoByModule: MutableMap<TestModule, FirJvmModuleInfo> = mutableMapOf()

    fun convertToFirModuleInfo(module: TestModule): FirJvmModuleInfo {
        return firModuleInfoByModule.getOrPut(module) {
            val dependencies = mutableListOf(builtinsModuleInfoForModule(module))
            module.dependencies.mapTo(dependencies) {
                convertToFirModuleInfo(getTestModule(it.moduleName))
            }
            FirJvmModuleInfo(
                module.name,
                dependencies
            )
        }
    }

    fun builtinsModuleInfoForModule(module: TestModule): FirJvmModuleInfo {
        return builtinsByModule.getOrPut(module) {
            FirJvmModuleInfo(Name.special("<built-ins>"), emptyList())
        }
    }

    override fun getSourceModule(name: String): FirSourceArtifact? {
        return analyzedModules[name]
    }

    override fun getCompiledKlib(name: String): ResultingArtifact.KLib? {
        TODO("Not yet implemented")
    }

    override fun getBinaryDependency(name: String): ResultingArtifact.Binary? {
        TODO("Not yet implemented")
    }

    override fun registerAnalyzedModule(name: String, results: FirSourceArtifact) {
        if (name in analyzedModules) {
            throw IllegalArgumentException("Analysis results of $name module already registered")
        }
        analyzedModules[name] = results
    }

    override fun registerCompiledKLib(name: String, artifact: ResultingArtifact.KLib) {
        TODO("Not yet implemented")
    }

    override fun registerCompiledBinary(name: String, artifact: ResultingArtifact.Binary) {
        TODO("Not yet implemented")
    }
}
