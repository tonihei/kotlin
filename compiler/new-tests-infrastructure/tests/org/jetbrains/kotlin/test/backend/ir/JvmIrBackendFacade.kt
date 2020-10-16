/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.backend.ir

import org.jetbrains.kotlin.backend.common.phaser.PhaseConfig
import org.jetbrains.kotlin.backend.jvm.JvmBackendFacade
import org.jetbrains.kotlin.backend.jvm.JvmGeneratorExtensions
import org.jetbrains.kotlin.backend.jvm.JvmIrCodegenFactory
import org.jetbrains.kotlin.backend.jvm.jvmPhases
import org.jetbrains.kotlin.cli.common.CLIConfigurationKeys
import org.jetbrains.kotlin.cli.jvm.compiler.NoScopeRecordCliBindingTrace
import org.jetbrains.kotlin.codegen.ClassBuilderFactories
import org.jetbrains.kotlin.codegen.state.GenerationState
import org.jetbrains.kotlin.ir.descriptors.IrFunctionFactory
import org.jetbrains.kotlin.ir.util.generateTypicalIrProviderList
import org.jetbrains.kotlin.modules.TargetId
import org.jetbrains.kotlin.test.components.ConfigurationComponents
import org.jetbrains.kotlin.test.components.TestServices
import org.jetbrains.kotlin.test.model.ArtifactKind
import org.jetbrains.kotlin.test.model.ResultingArtifact
import org.jetbrains.kotlin.test.model.TestModule

class JvmIrBackendFacade(
    configurationComponents: ConfigurationComponents
) : IrBackendFacade<ResultingArtifact.Binary.Jvm>(configurationComponents, ArtifactKind.Jvm) {
    override fun produce(
        module: TestModule,
        initialInfo: IrBackendInputInfo,
        testServices: TestServices
    ): ResultingArtifact.Binary.Jvm {
        val environment = configurationComponents.kotlinCoreEnvironmentProvider.getKotlinCoreEnvironment(module)
        val compilerConfiguration = environment.configuration
        val (irModuleFragment, symbolTable, sourceManager, jvmBackendClassResolver, ktFiles, serializerFactory) = initialInfo

        val phaseConfig = compilerConfiguration.get(CLIConfigurationKeys.PHASE_CONFIG) ?: PhaseConfig(jvmPhases)
        val codegenFactory = JvmIrCodegenFactory(phaseConfig)

        val generationState = GenerationState.Builder(
            environment.project, ClassBuilderFactories.TEST,
            irModuleFragment.descriptor,
            NoScopeRecordCliBindingTrace().bindingContext,
            ktFiles.toList(),
            compilerConfiguration
        ).codegenFactory(
            codegenFactory
        ).withModule(
            module
        ).isIrBackend(
            true
        ).jvmBackendClassResolver(
            jvmBackendClassResolver
        ).build()

        irModuleFragment.irBuiltins.functionFactory = IrFunctionFactory(irModuleFragment.irBuiltins, symbolTable)
        val extensions = JvmGeneratorExtensions()
        val irProviders = generateTypicalIrProviderList(
            irModuleFragment.descriptor, irModuleFragment.irBuiltins, symbolTable, extensions = extensions
        )

        JvmBackendFacade.doGenerateFilesInternal(
            generationState,
            irModuleFragment,
            symbolTable,
            sourceManager,
            phaseConfig,
            irProviders,
            extensions,
            serializerFactory
        )
        generationState.factory.done()

        return ResultingArtifact.Binary.Jvm(generationState.factory)
    }

    private fun GenerationState.Builder.withModule(module: TestModule): GenerationState.Builder = apply {
        targetId(TargetId(module.name, "test"))
        moduleName(module.name)
    }
}
