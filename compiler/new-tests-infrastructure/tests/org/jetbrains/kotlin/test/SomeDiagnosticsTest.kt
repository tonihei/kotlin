/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test

import com.intellij.openapi.Disposable
import com.intellij.testFramework.TestDataPath
import org.jetbrains.kotlin.test.backend.classic.ClassicBackendInputInfo
import org.jetbrains.kotlin.test.backend.classic.ClassicJvmBackendFacade
import org.jetbrains.kotlin.test.backend.handlers.JvmBoxRunner
import org.jetbrains.kotlin.test.components.*
import org.jetbrains.kotlin.test.directives.ModuleStructureExtractor
import org.jetbrains.kotlin.test.directives.SimpleDirectivesContainer
import org.jetbrains.kotlin.test.frontend.classic.ClassicDependencyProvider
import org.jetbrains.kotlin.test.frontend.classic.ClassicFrontend2ClassicBackendConverter
import org.jetbrains.kotlin.test.frontend.classic.ClassicFrontendFacade
import org.jetbrains.kotlin.test.frontend.classic.ClassicFrontendSourceArtifacts
import org.jetbrains.kotlin.test.frontend.classic.handlers.DeclarationsDumpHandler
import org.jetbrains.kotlin.test.frontend.fir.FirDependencyProvider
import org.jetbrains.kotlin.test.frontend.fir.FirFrontendFacade
import org.jetbrains.kotlin.test.frontend.fir.handlers.FirDumpHandler
import org.jetbrains.kotlin.test.model.ResultingArtifact
import org.jetbrains.kotlin.test.model.TestModule
import org.jetbrains.kotlin.test.model.processModule
import org.junit.jupiter.api.Test

@TestMetadata("compiler/new-tests-infrastructure/testData")
@TestDataPath("\$PROJECT_ROOT")
class SomeDiagnosticsTest {

    @Test
    @TestMetadata("a.kt")
    fun testSimpleFir() {
        doFirTest("compiler/new-tests-infrastructure/testData/a.kt")
    }

    @Test
    @TestMetadata("a.kt")
    fun testSimpleClassicFrontend() {
        doClassicFrontendTest("compiler/new-tests-infrastructure/testData/a.kt")
    }

    @Test
    @TestMetadata("boxTest.kt")
    fun testSimpleClassicBackend() {
        doClassicFrontendTest("compiler/new-tests-infrastructure/testData/boxTest.kt", withBackend = true)
    }

    fun doClassicFrontendTest(fileName: String, withBackend: Boolean = false) {
        val components = createComponents()
        val globalFrontendHandlers = listOf(DeclarationsDumpHandler(components.assertions))
        val backendHandlers = listOf(JvmBoxRunner(components.assertions))
        val facade = ClassicFrontendFacade(components)

        val moduleStructure = ModuleStructureExtractor.splitTestDataByModules(
            testDataFileName = fileName,
            directivesContainer = SimpleDirectivesContainer.Empty,
            assertions = components.assertions
        )

        val dependencyProvider = ClassicDependencyProvider(components, moduleStructure.modules)

        val frontendResults = mutableMapOf<TestModule, ClassicFrontendSourceArtifacts>()
        for (module in moduleStructure.modules) {
            val analysisResults = facade.analyze(module, dependencyProvider)
            frontendResults[module] = analysisResults
            dependencyProvider.registerAnalyzedModule(module.name, analysisResults)
            globalFrontendHandlers.forEach { it.processModule(module, analysisResults) }
        }

        globalFrontendHandlers.forEach { it.processAfterAllModules(moduleStructure) }
        if (!withBackend) return

        val frontend2BackendConverter = ClassicFrontend2ClassicBackendConverter()
        val backendInitialInfos = mutableMapOf<TestModule, ClassicBackendInputInfo>()

        for ((module, sourceArtifact) in frontendResults) {
            val backendInfo = frontend2BackendConverter.convert(module, sourceArtifact, dependencyProvider)
            backendInitialInfos[module] = backendInfo
        }

        val backendFacade = ClassicJvmBackendFacade(components)
        val backendArtifacts = mutableMapOf<TestModule, ResultingArtifact.Binary.Jvm>()
        for ((module, backendInfo) in backendInitialInfos) {
            val jvm = backendFacade.produce(module, backendInfo)
            backendArtifacts[module] = jvm
            backendHandlers.forEach { it.processModule(module, jvm) }
        }
    }

    fun doFirTest(fileName: String) {
        val components = createComponents()

        val globalHandlers = listOf(FirDumpHandler(components.assertions))

        val facade = FirFrontendFacade(components)

        val moduleStructure = ModuleStructureExtractor.splitTestDataByModules(
            testDataFileName = fileName,
            directivesContainer = SimpleDirectivesContainer.Empty,
            assertions = components.assertions
        )

        val dependencyProvider = FirDependencyProvider(components, moduleStructure.modules)

        for (module in moduleStructure.modules) {
            val analysisResults = facade.analyze(module, dependencyProvider)
            dependencyProvider.registerAnalyzedModule(module.name, analysisResults)
            globalHandlers.forEach { it.processModule(module, analysisResults) }
        }

        globalHandlers.forEach { it.processAfterAllModules(moduleStructure) }
    }

    private fun createComponents() = ConfigurationComponents.build {
        kotlinCoreEnvironmentProvider = KotlinCoreEnvironmentProviderImpl(this, TestDisposable())
        sourceFileProvider = SourceFileProviderImpl(emptyList())
        languageVersionSettingsProvider = LanguageVersionSettingsProviderImpl()
        assertions = JUnit5Assertions
    }
}


class TestDisposable : Disposable {
    @Volatile
    var isDisposed = false
        private set

    override fun dispose() {
        isDisposed = true
    }
}
