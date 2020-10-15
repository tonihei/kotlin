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
import org.jetbrains.kotlin.test.components.ConfigurationComponents
import org.jetbrains.kotlin.test.components.getKtFilesForSourceFiles
import org.jetbrains.kotlin.test.model.DependencyKind
import org.jetbrains.kotlin.test.model.FrontendFacade
import org.jetbrains.kotlin.test.model.FrontendKind
import org.jetbrains.kotlin.test.model.TestModule

class ClassicFrontendFacade(
    configurationComponents: ConfigurationComponents
) : FrontendFacade<ClassicFrontendSourceArtifacts, ClassicDependencyProvider>(configurationComponents, FrontendKind.ClassicFrontend) {
    override fun analyze(module: TestModule, dependencyProvider: ClassicDependencyProvider): ClassicFrontendSourceArtifacts {
        val environment = configurationComponents.kotlinCoreEnvironmentProvider.getKotlinCoreEnvironment(module)
        val project = environment.project

        val ktFilesMap = configurationComponents.sourceFileProvider.getKtFilesForSourceFiles(module.files, project)
        val ktFiles = ktFilesMap.values
        val languageVersionSettings = module.languageVersionSettings

        val dependentDescriptors = module.dependencies.filter { it.kind == DependencyKind.Source }.map {
            val testModule = dependencyProvider.getTestModule(it.moduleName)
            dependencyProvider.getModuleDescriptor(testModule)
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
        dependencyProvider.replaceModuleDescriptorForModule(module, analysisResult.moduleDescriptor)
        return ClassicFrontendSourceArtifacts(
            ktFilesMap,
            analysisResult,
            project,
            languageVersionSettings
        )
    }
}
