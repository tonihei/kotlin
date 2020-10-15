/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.frontend.classic

import org.jetbrains.kotlin.descriptors.ModuleDescriptor
import org.jetbrains.kotlin.descriptors.impl.ModuleDescriptorImpl
import org.jetbrains.kotlin.test.components.ConfigurationComponents
import org.jetbrains.kotlin.test.model.DependencyProvider
import org.jetbrains.kotlin.test.model.ResultingArtifact
import org.jetbrains.kotlin.test.model.TestModule

class ClassicDependencyProvider(
    configurationComponents: ConfigurationComponents,
    testModules: List<TestModule>
) : DependencyProvider<ClassicFrontendSourceArtifacts>(configurationComponents, testModules) {
    private val moduleDescriptorByModule = mutableMapOf<TestModule, ModuleDescriptorImpl>()

    fun getModuleDescriptor(testModule: TestModule): ModuleDescriptorImpl {
        return moduleDescriptorByModule[testModule] ?: assertions.fail { "Module descriptor for module ${testModule.name} not found" }
    }

    fun replaceModuleDescriptorForModule(testModule: TestModule, moduleDescriptor: ModuleDescriptor) {
        require(moduleDescriptor is ModuleDescriptorImpl)
        moduleDescriptorByModule[testModule] = moduleDescriptor
    }

    override fun getBinaryDependency(name: String): ResultingArtifact.Binary<*>? {
        TODO("Not yet implemented")
    }

    override fun registerCompiledBinary(name: String, artifact: ResultingArtifact.Binary<*>) {
        TODO("Not yet implemented")
    }
}
