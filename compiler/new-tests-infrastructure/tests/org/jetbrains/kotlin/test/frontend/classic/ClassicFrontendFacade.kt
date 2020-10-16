/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.frontend.classic

import org.jetbrains.kotlin.cli.jvm.compiler.NoScopeRecordCliBindingTrace
import org.jetbrains.kotlin.cli.jvm.compiler.TopDownAnalyzerFacadeForJVM
import org.jetbrains.kotlin.config.JVMConfigurationKeys
import org.jetbrains.kotlin.config.JvmTarget
import org.jetbrains.kotlin.config.languageVersionSettings
import org.jetbrains.kotlin.test.components.*
import org.jetbrains.kotlin.test.model.DependencyKind
import org.jetbrains.kotlin.test.model.FrontendFacade
import org.jetbrains.kotlin.test.model.FrontendKind
import org.jetbrains.kotlin.test.model.TestModule

class ClassicFrontendFacade(
    testServices: TestServices
) : FrontendFacade<ClassicFrontendSourceArtifacts>(testServices, FrontendKind.ClassicFrontend) {
    override val additionalServices: List<ServiceRegistrationData>
        get() = listOf(serviceRegistrationData(::ModuleDescriptorProvider))

    override fun analyze(module: TestModule): ClassicFrontendSourceArtifacts {
        val dependencyProvider = testServices.dependencyProvider
        val moduleDescriptorProvider = testServices.moduleDescriptorProvider
        val environment = testServices.kotlinCoreEnvironmentProvider.getKotlinCoreEnvironment(module)
        val project = environment.project

        val ktFilesMap = testServices.sourceFileProvider.getKtFilesForSourceFiles(module.files, project)
        val ktFiles = ktFilesMap.values
        val languageVersionSettings = module.languageVersionSettings

        val dependentDescriptors = module.dependencies.filter { it.kind == DependencyKind.Source }.map {
            val testModule = dependencyProvider.getTestModule(it.moduleName)
            moduleDescriptorProvider.getModuleDescriptor(testModule)
        }

        // TODO: add configuring JvmTarget
        val analysisResult = TopDownAnalyzerFacadeForJVM.analyzeFilesWithJavaIntegration(
            project,
            ktFiles,
            NoScopeRecordCliBindingTrace(),
            environment.configuration.copy().apply {
                this.languageVersionSettings = languageVersionSettings
                this.put(JVMConfigurationKeys.JVM_TARGET, JvmTarget.DEFAULT)
            },
            environment::createPackagePartProvider,
            explictModuleDependencyList = dependentDescriptors
        )
        moduleDescriptorProvider.replaceModuleDescriptorForModule(module, analysisResult.moduleDescriptor)
        return ClassicFrontendSourceArtifacts(
            ktFilesMap,
            analysisResult,
            project,
            languageVersionSettings
        )
    }
}
