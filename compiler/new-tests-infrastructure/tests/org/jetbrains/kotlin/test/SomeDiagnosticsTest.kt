/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test

import com.intellij.openapi.Disposable
import com.intellij.testFramework.TestDataPath
import org.jetbrains.kotlin.test.components.*
import org.jetbrains.kotlin.test.directives.ModuleStructureExtractor
import org.jetbrains.kotlin.test.directives.SimpleDirectivesContainer
import org.jetbrains.kotlin.test.frontend.classic.ClassicDependencyProvider
import org.jetbrains.kotlin.test.frontend.classic.ClassicFrontendFacade
import org.jetbrains.kotlin.test.frontend.classic.handlers.DeclarationsDumpHandler
import org.jetbrains.kotlin.test.frontend.fir.FirDependencyProvider
import org.jetbrains.kotlin.test.frontend.fir.FirFrontendFacade
import org.jetbrains.kotlin.test.frontend.fir.handlers.FirDumpHandler
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

    fun doClassicFrontendTest(fileName: String) {
        val components = createComponents()
        val globalHandlers = listOf(DeclarationsDumpHandler(components.assertions))
        val facade = ClassicFrontendFacade(components)

        val moduleStructure = ModuleStructureExtractor.splitTestDataByModules(
            testDataFileName = fileName,
            directivesContainer = SimpleDirectivesContainer.Empty,
            assertions = components.assertions
        )

        val dependencyProvider = ClassicDependencyProvider(components, moduleStructure.modules)

        for (module in moduleStructure.modules) {
            val analysisResults = facade.analyze(module, dependencyProvider)
            dependencyProvider.registerAnalyzedModule(module.name, analysisResults)
            globalHandlers.forEach { it.processModule(module, analysisResults) }
        }

        globalHandlers.forEach { it.processAfterAllModules(moduleStructure) }
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
