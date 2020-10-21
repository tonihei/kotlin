/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test

import org.jetbrains.kotlin.test.directives.ModuleStructureExtractor
import org.jetbrains.kotlin.test.model.*
import org.jetbrains.kotlin.test.services.DependencyProviderImpl
import org.jetbrains.kotlin.test.services.registerDependencyProvider

abstract class TestRunner {
    abstract val testConfiguration: TestConfiguration

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
            if (!frontendKind.shouldRunAnalysis) {
                continue
            }
            val frontendFacade: FrontendFacade<*> = testConfiguration.getFrontendFacade(frontendKind)
            val frontendArtifacts = frontendFacade.analyze(module)
            dependencyProvider.registerSourceArtifact(module, frontendArtifacts)
            val frontendHandlers: List<FrontendResultsHandler<*>> = testConfiguration.getFrontendHandlers(frontendKind)
            for (frontendHandler in frontendHandlers) {
                hackyProcess(module, frontendHandler, frontendArtifacts)
            }
        }
        for (frontendHandler in testConfiguration.getAllFrontendHandlers()) {
            frontendHandler.processAfterAllModules(moduleStructure)
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

private fun hackyProcess(module: TestModule, handler: FrontendResultsHandler<*>, artifact: ResultingArtifact.Source<*>) {
    @Suppress("UNCHECKED_CAST")
    (handler as FrontendResultsHandler<ResultingArtifact.Source.Empty>).processModule(
        module,
        artifact as ResultingArtifact<ResultingArtifact.Source.Empty>
    )
}

private fun hackyProcess(module: TestModule, handler: BackendInitialInfoHandler<*>, artifact: ResultingArtifact.BackendInputInfo<*>) {
    @Suppress("UNCHECKED_CAST")
    (handler as BackendInitialInfoHandler<ResultingArtifact.BackendInputInfo.Empty>).processModule(
        module,
        artifact as ResultingArtifact<ResultingArtifact.BackendInputInfo.Empty>
    )
}

private fun hackyProcess(module: TestModule, handler: ArtifactsResultsHandler<*>, artifact: ResultingArtifact.Binary<*>) {
    @Suppress("UNCHECKED_CAST")
    (handler as ArtifactsResultsHandler<ResultingArtifact.Binary.Empty>).processModule(
        module,
        artifact as ResultingArtifact<ResultingArtifact.Binary.Empty>
    )
}
