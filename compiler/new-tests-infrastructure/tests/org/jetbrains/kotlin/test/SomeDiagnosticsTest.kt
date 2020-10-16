/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test

//@TestMetadata("compiler/new-tests-infrastructure/testData")
//@TestDataPath("\$PROJECT_ROOT")
//class SomeDiagnosticsTest {
//
//    @Test
//    @TestMetadata("a.kt")
//    fun testSimpleFir() {
//        doFirTest("compiler/new-tests-infrastructure/testData/a.kt")
//    }
//
//    @Test
//    @TestMetadata("a.kt")
//    fun testSimpleClassicFrontend() {
//        doClassicFrontendTest("compiler/new-tests-infrastructure/testData/a.kt")
//    }
//
//    @Test
//    @TestMetadata("boxTest.kt")
//    fun testSimpleClassicBackend() {
//        doClassicFrontendTest("compiler/new-tests-infrastructure/testData/boxTest.kt", withBackend = true)
//    }
//
//    @Test
//    @TestMetadata("boxTest.kt")
//    fun testSimpleIrBackend() {
//        doIrBlackBoxTest("compiler/new-tests-infrastructure/testData/boxTest.kt", withBackend = true)
//    }
//
//    fun doClassicFrontendTest(fileName: String, withBackend: Boolean = false) {
//        val components = createComponents()
//        val globalFrontendHandlers = listOf(DeclarationsDumpHandler(components.assertions))
//        val backendHandlers = listOf(JvmBoxRunner(components.assertions))
//        val facade = ClassicFrontendFacade(components)
//
//        val moduleStructure = ModuleStructureExtractor.splitTestDataByModules(
//            testDataFileName = fileName,
//            directivesContainer = SimpleDirectivesContainer.Empty,
//            assertions = components.assertions
//        )
//
//        val dependencyProvider = ClassicDependencyProvider(components, moduleStructure.modules)
//
//        val frontendResults = mutableMapOf<TestModule, ClassicFrontendSourceArtifacts>()
//        for (module in moduleStructure.modules) {
//            val analysisResults = facade.analyze(module, dependencyProvider)
//            frontendResults[module] = analysisResults
//            dependencyProvider.registerAnalyzedModule(module.name, analysisResults)
//            globalFrontendHandlers.forEach { it.processModule(module, analysisResults) }
//        }
//
//        globalFrontendHandlers.forEach { it.processAfterAllModules(moduleStructure) }
//        if (!withBackend) return
//
//        val frontend2BackendConverter = ClassicFrontend2ClassicBackendConverter()
//        val backendInitialInfos = mutableMapOf<TestModule, ClassicBackendInputInfo>()
//
//        for ((module, sourceArtifact) in frontendResults) {
//            val backendInfo = frontend2BackendConverter.convert(module, sourceArtifact, dependencyProvider)
//            backendInitialInfos[module] = backendInfo
//        }
//
//        val backendFacade = ClassicJvmBackendFacade(components)
//        val backendArtifacts = mutableMapOf<TestModule, ResultingArtifact.Binary.Jvm>()
//        for ((module, backendInfo) in backendInitialInfos) {
//            val jvm = backendFacade.produce(module, backendInfo)
//            backendArtifacts[module] = jvm
//            backendHandlers.forEach { it.processModule(module, jvm) }
//        }
//    }
//
//    fun doIrBlackBoxTest(fileName: String, withBackend: Boolean = false) {
//        val components = createComponents()
//        val globalFrontendHandlers = listOf(DeclarationsDumpHandler(components.assertions))
//        val backendHandlers = listOf(JvmBoxRunner(components.assertions))
//        val facade = ClassicFrontendFacade(components)
//
//        val moduleStructure = ModuleStructureExtractor.splitTestDataByModules(
//            testDataFileName = fileName,
//            directivesContainer = SimpleDirectivesContainer.Empty,
//            assertions = components.assertions
//        )
//
//        val dependencyProvider = ClassicDependencyProvider(components, moduleStructure.modules)
//
//        val frontendResults = mutableMapOf<TestModule, ClassicFrontendSourceArtifacts>()
//        for (module in moduleStructure.modules) {
//            val analysisResults = facade.analyze(module, dependencyProvider)
//            frontendResults[module] = analysisResults
//            dependencyProvider.registerAnalyzedModule(module.name, analysisResults)
//            globalFrontendHandlers.forEach { it.processModule(module, analysisResults) }
//        }
//
//        globalFrontendHandlers.forEach { it.processAfterAllModules(moduleStructure) }
//        if (!withBackend) return
//
//        val frontend2BackendConverter = ClassicFrontend2IrConverter()
//        val backendInitialInfos = mutableMapOf<TestModule, IrBackendInputInfo>()
//
//        for ((module, sourceArtifact) in frontendResults) {
//            val backendInfo = frontend2BackendConverter.convert(module, sourceArtifact, dependencyProvider)
//            backendInitialInfos[module] = backendInfo
//        }
//
//        val backendFacade = JvmIrBackendFacade(components)
//        val backendArtifacts = mutableMapOf<TestModule, ResultingArtifact.Binary.Jvm>()
//        for ((module, backendInfo) in backendInitialInfos) {
//            val jvm = backendFacade.produce(module, backendInfo)
//            backendArtifacts[module] = jvm
//            backendHandlers.forEach { it.processModule(module, jvm) }
//        }
//    }
//
//
//    fun doFirTest(fileName: String) {
//        val components = createComponents()
//
//        val globalHandlers = listOf(FirDumpHandler(components.assertions))
//
//        val facade = FirFrontendFacade(components)
//
//        val moduleStructure = ModuleStructureExtractor.splitTestDataByModules(
//            testDataFileName = fileName,
//            directivesContainer = SimpleDirectivesContainer.Empty,
//            assertions = components.assertions
//        )
//
//        val dependencyProvider = FirDependencyProvider(components, moduleStructure.modules)
//
//        for (module in moduleStructure.modules) {
//            val analysisResults = facade.analyze(module, dependencyProvider)
//            dependencyProvider.registerAnalyzedModule(module.name, analysisResults)
//            globalHandlers.forEach { it.processModule(module, analysisResults) }
//        }
//
//        globalHandlers.forEach { it.processAfterAllModules(moduleStructure) }
//    }
//
//    private fun createComponents(): ConfigurationComponents = ConfigurationComponents.build {
//        kotlinCoreEnvironmentProvider = KotlinCoreEnvironmentProviderImpl(this, TestDisposable())
//        sourceFileProvider = SourceFileProviderImpl(emptyList())
//        languageVersionSettingsProvider = LanguageVersionSettingsProviderImpl()
//        assertions = JUnit5Assertions
//    }
//}
