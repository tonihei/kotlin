/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test

import org.jetbrains.kotlin.test.directives.ModuleStructureExtractor
import org.jetbrains.kotlin.test.model.*
import org.jetbrains.kotlin.test.services.DependencyProviderImpl
import org.jetbrains.kotlin.test.services.registerDependencyProvider

class TestRunner(private val testConfiguration: TestConfiguration) {
    fun runTest(testDataFileName: String) {
        val services = testConfiguration.testServices
        val moduleStructure = ModuleStructureExtractor.splitTestDataByModules(
            testDataFileName,
            testConfiguration.directives,
            services
        )
        val modules = moduleStructure.modules
        val dependencyProvider = DependencyProviderImpl(services, modules)
        services.registerDependencyProvider(dependencyProvider)
        for (module in modules) {
            val frontendKind = module.frontendKind
            if (!frontendKind.shouldRunAnalysis) continue

            val frontendArtifacts: ResultingArtifact.Source<*> = testConfiguration.getFrontendFacade(frontendKind).analyze(module).also {
                dependencyProvider.registerSourceArtifact(module, it)
            }
            val frontendHandlers: List<FrontendResultsHandler<*>> = testConfiguration.getFrontendHandlers(frontendKind)
            for (frontendHandler in frontendHandlers) {
                frontendHandler.hackyProcess(module, frontendArtifacts)
            }

            val backendKind = module.targetBackend
            if (!backendKind.shouldRunAnalysis) continue

            val backendInputInfo = testConfiguration.getConverter(frontendKind, backendKind).hackyConvert(module, frontendArtifacts).also {
                dependencyProvider.registerBackendInfo(module, it)
            }

            val backendHandlers: List<BackendInitialInfoHandler<*>> = testConfiguration.getBackendHandlers(backendKind)
            for (backendHandler in backendHandlers) {
                backendHandler.hackyProcess(module, backendInputInfo)
            }

            for (artifactKind in moduleStructure.getTargetArtifactKinds(module)) {
                if (!artifactKind.shouldRunAnalysis) continue
                val binaryArtifact = testConfiguration.getBackendFacade(backendKind, artifactKind)
                    .hackyProduce(module, backendInputInfo).also {
                        dependencyProvider.registerBinaryArtifact(module, it)
                    }

                val binaryHandlers: List<ArtifactsResultsHandler<*>> = testConfiguration.getArtifactHandlers(artifactKind)
                for (binaryHandler in binaryHandlers) {
                    binaryHandler.hackyProcess(module, binaryArtifact)
                }
            }
        }
        for (frontendHandler in testConfiguration.getAllFrontendHandlers()) {
            frontendHandler.processAfterAllModules(moduleStructure)
        }
        for (backendHandler in testConfiguration.getAllBackendHandlers()) {
            backendHandler.processAfterAllModules(moduleStructure)
        }
        for (artifactHandler in testConfiguration.getAllArtifactHandlers()) {
            artifactHandler.processAfterAllModules(moduleStructure)
        }
    }
}

// ----------------------------------------------------------------------------------------------------------------
/*
 * Those `hackyProcess` methods are needed to hack kotlin type system. In common test case
 *   we have artifact of type ResultingArtifact<*> and handler of type AnalysisHandler<*> and actually
 *   at runtime types under `*` are same (that achieved by grouping handlers and facades by
 *   frontend/backend/artifact kind). But there is no way to tell that to compiler, so I unsafely cast types with `*`
 *   to types with Empty artifacts to make it compile. Since unsafe cast has no effort at runtime, it's safe to use it
 */

private fun FrontendResultsHandler<*>.hackyProcess(module: TestModule, artifact: ResultingArtifact.Source<*>) {
    @Suppress("UNCHECKED_CAST")
    (this as FrontendResultsHandler<ResultingArtifact.Source.Empty>).processModule(
        module,
        artifact as ResultingArtifact<ResultingArtifact.Source.Empty>
    )
}

private fun BackendInitialInfoHandler<*>.hackyProcess(module: TestModule, artifact: ResultingArtifact.BackendInputInfo<*>) {
    @Suppress("UNCHECKED_CAST")
    (this as BackendInitialInfoHandler<ResultingArtifact.BackendInputInfo.Empty>).processModule(
        module,
        artifact as ResultingArtifact<ResultingArtifact.BackendInputInfo.Empty>
    )
}

private fun ArtifactsResultsHandler<*>.hackyProcess(module: TestModule, artifact: ResultingArtifact.Binary<*>) {
    @Suppress("UNCHECKED_CAST")
    (this as ArtifactsResultsHandler<ResultingArtifact.Binary.Empty>).processModule(
        module,
        artifact as ResultingArtifact<ResultingArtifact.Binary.Empty>
    )
}

private fun Frontend2BackendConverter<*, *>.hackyConvert(
    module: TestModule,
    artifact: ResultingArtifact.Source<*>
): ResultingArtifact.BackendInputInfo<*> {
    @Suppress("UNCHECKED_CAST")
    return (this as Frontend2BackendConverter<ResultingArtifact.Source.Empty, ResultingArtifact.BackendInputInfo.Empty>).convert(
        module,
        artifact as ResultingArtifact.Source<ResultingArtifact.Source.Empty>
    )
}

private fun BackendFacade<*, *>.hackyProduce(
    module: TestModule,
    initialInfo: ResultingArtifact.BackendInputInfo<*>
): ResultingArtifact.Binary<*> {
    @Suppress("UNCHECKED_CAST")
    return (this as BackendFacade<ResultingArtifact.BackendInputInfo.Empty, ResultingArtifact.Binary.Empty>).produce(
        module,
        initialInfo as ResultingArtifact.BackendInputInfo<ResultingArtifact.BackendInputInfo.Empty>
    )
}

