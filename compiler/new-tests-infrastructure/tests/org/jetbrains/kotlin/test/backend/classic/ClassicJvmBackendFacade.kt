/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.backend.classic

import org.jetbrains.kotlin.codegen.ClassBuilderFactories
import org.jetbrains.kotlin.codegen.DefaultCodegenFactory
import org.jetbrains.kotlin.codegen.KotlinCodegenFacade
import org.jetbrains.kotlin.codegen.state.GenerationState
import org.jetbrains.kotlin.test.components.ConfigurationComponents
import org.jetbrains.kotlin.test.components.ServiceRegistrationData
import org.jetbrains.kotlin.test.components.TestServices
import org.jetbrains.kotlin.test.model.ArtifactKind
import org.jetbrains.kotlin.test.model.ResultingArtifact
import org.jetbrains.kotlin.test.model.TestModule

class ClassicJvmBackendFacade(
    configurationComponents: ConfigurationComponents
) : ClassicBackendFacade<ResultingArtifact.Binary.Jvm>(configurationComponents, ArtifactKind.Jvm) {
    override fun produce(
        module: TestModule,
        initialInfo: ClassicBackendInputInfo,
        testServices: TestServices
    ): ResultingArtifact.Binary.Jvm {
        val environment = configurationComponents.kotlinCoreEnvironmentProvider.getKotlinCoreEnvironment(module)
        val compilerConfiguration = environment.configuration
        val (psiFiles, bindingContext, moduleDescriptor, project, languageVersionSettings) = initialInfo
        // TODO: add configuring classBuilderFactory
        val generationState = GenerationState.Builder(
            project,
            ClassBuilderFactories.TEST,
            moduleDescriptor,
            bindingContext,
            psiFiles.toList(),
            compilerConfiguration
        ).codegenFactory(DefaultCodegenFactory).build()

        KotlinCodegenFacade.compileCorrectFiles(generationState)

        return ResultingArtifact.Binary.Jvm(generationState.factory)
    }
}
