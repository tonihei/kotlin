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
 * Those classes and [hackyProcess] methods are needed for hack kotlin type system. In common test case
 *   we have artifact of type ResultingArtifact<*> and handler of type AnalysisHandler<*> and actually
 *   at runtime types under `*` are same (that achieved by grouping handlers and facades by
 *   frontend/backend/artifact kind). But there is no way to tell that to compiler, so I created dummy kinds
 *   and dummy artifacts so I can unsafely cast types with `*` to types with dummies.
 */

private class DummySourceArtifact : ResultingArtifact.Source<DummySourceArtifact>() {
    override val frontendKind: FrontendKind<DummySourceArtifact>
        get() = DummyFrontendKind
}

private object DummyFrontendKind : FrontendKind<DummySourceArtifact>()

private fun hackyProcess(module: TestModule, handler: FrontendResultsHandler<*>, artifact: ResultingArtifact.Source<*>) {
    @Suppress("UNCHECKED_CAST")
    (handler as FrontendResultsHandler<DummySourceArtifact>).processModule(module, artifact as ResultingArtifact<DummySourceArtifact>)
}

private class DummyBackendInitialInfo : ResultingArtifact.BackendInputInfo<DummyBackendInitialInfo>() {
    override val backendKind: BackendKind<DummyBackendInitialInfo>
        get() = DummyBackendKind
}

private object DummyBackendKind : BackendKind<DummyBackendInitialInfo>()

private fun hackyProcess(module: TestModule, handler: BackendInitialInfoHandler<*>, artifact: ResultingArtifact.BackendInputInfo<*>) {
    @Suppress("UNCHECKED_CAST")
    (handler as BackendInitialInfoHandler<DummyBackendInitialInfo>).processModule(module, artifact as ResultingArtifact<DummyBackendInitialInfo>)
}

private class DummyBinaryArtifact : ResultingArtifact.Binary<DummyBinaryArtifact>() {
    override val artifactKind: ArtifactKind<DummyBinaryArtifact>
        get() = DummyArtifactKind
}

private object DummyArtifactKind : ArtifactKind<DummyBinaryArtifact>()

private fun hackyProcess(module: TestModule, handler: ArtifactsResultsHandler<*>, artifact: ResultingArtifact.Binary<*>) {
    @Suppress("UNCHECKED_CAST")
    (handler as ArtifactsResultsHandler<DummyBinaryArtifact>).processModule(module, artifact as ResultingArtifact<DummyBinaryArtifact>)
}
