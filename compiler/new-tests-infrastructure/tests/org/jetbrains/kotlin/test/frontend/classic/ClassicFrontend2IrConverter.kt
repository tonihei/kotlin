/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.frontend.classic

import org.jetbrains.kotlin.backend.common.serialization.signature.IdSignatureDescriptor
import org.jetbrains.kotlin.backend.jvm.JvmGeneratorExtensions
import org.jetbrains.kotlin.backend.jvm.codegen.DescriptorMetadataSerializer
import org.jetbrains.kotlin.codegen.JvmBackendClassResolverForModuleWithDependencies
import org.jetbrains.kotlin.ir.backend.js.lower.serialization.ir.JsManglerDesc
import org.jetbrains.kotlin.ir.backend.jvm.serialization.JvmManglerDesc
import org.jetbrains.kotlin.ir.declarations.impl.IrFactoryImpl
import org.jetbrains.kotlin.ir.declarations.persistent.PersistentIrFactory
import org.jetbrains.kotlin.ir.util.NameProvider
import org.jetbrains.kotlin.ir.util.SymbolTable
import org.jetbrains.kotlin.ir.util.generateTypicalIrProviderList
import org.jetbrains.kotlin.psi2ir.Psi2IrConfiguration
import org.jetbrains.kotlin.psi2ir.Psi2IrTranslator
import org.jetbrains.kotlin.resolve.AnalyzingUtils
import org.jetbrains.kotlin.test.backend.ir.IrBackendInputInfo
import org.jetbrains.kotlin.test.model.DependencyProvider
import org.jetbrains.kotlin.test.model.Frontend2BackendConverter
import org.jetbrains.kotlin.test.model.TestModule

class ClassicFrontend2IrConverter : Frontend2BackendConverter<ClassicFrontendSourceArtifacts, IrBackendInputInfo>() {
    override fun convert(
        module: TestModule,
        frontendResults: ClassicFrontendSourceArtifacts,
        dependencyProvider: DependencyProvider<ClassicFrontendSourceArtifacts>
    ): IrBackendInputInfo {
        val (psiFiles, analysisResult, _, languageVersionSettings) = frontendResults
        val psi2ir = Psi2IrTranslator(languageVersionSettings, Psi2IrConfiguration(ignoreErrors = false))
        val (bindingContext, moduleDescriptor, _) = analysisResult
        if (!psi2ir.configuration.ignoreErrors) {
            analysisResult.throwIfError()
            AnalyzingUtils.throwExceptionOnErrors(bindingContext)
        }
        // TODO: add configurable extensions
        val generatorExtensions = JvmGeneratorExtensions(generateFacades = false)
        val context = psi2ir.createGeneratorContext(
            moduleDescriptor,
            bindingContext,
            SymbolTable(IdSignatureDescriptor(JsManglerDesc), IrFactoryImpl, NameProvider.DEFAULT),
            generatorExtensions
        )
        val irProviders = generateTypicalIrProviderList(
            moduleDescriptor, context.irBuiltIns, context.symbolTable, extensions = generatorExtensions
        )
        // TODO: platforms
        val symbolTable = SymbolTable(IdSignatureDescriptor(JvmManglerDesc()), PersistentIrFactory)
        val generationContext = psi2ir.createGeneratorContext(moduleDescriptor, bindingContext, symbolTable)
        val irModuleFragment = psi2ir.generateModuleFragment(context, psiFiles.values, irProviders, emptyList())
        val sourceManager = generationContext.sourceManager
        val jvmBackendClassResolver = JvmBackendClassResolverForModuleWithDependencies(moduleDescriptor)
        return IrBackendInputInfo(
            irModuleFragment,
            symbolTable,
            sourceManager,
            jvmBackendClassResolver,
            psiFiles.values,
            ::DescriptorMetadataSerializer
        )
    }
}
