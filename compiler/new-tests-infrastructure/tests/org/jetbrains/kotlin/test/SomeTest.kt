/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test

import com.intellij.testFramework.TestDataPath
import org.jetbrains.kotlin.platform.jvm.JvmPlatforms
import org.jetbrains.kotlin.test.backend.classic.ClassicJvmBackendFacade
import org.jetbrains.kotlin.test.backend.handlers.JvmBoxRunner
import org.jetbrains.kotlin.test.builders.TestConfigurationBuilder
import org.jetbrains.kotlin.test.builders.testRunner
import org.jetbrains.kotlin.test.frontend.classic.ClassicFrontend2ClassicBackendConverter
import org.jetbrains.kotlin.test.frontend.classic.ClassicFrontendFacade
import org.jetbrains.kotlin.test.frontend.classic.handlers.DeclarationsDumpHandler
import org.jetbrains.kotlin.test.frontend.fir.FirFrontendFacade
import org.jetbrains.kotlin.test.frontend.fir.handlers.FirDumpHandler
import org.jetbrains.kotlin.test.model.BackendKind
import org.jetbrains.kotlin.test.model.DependencyKind
import org.jetbrains.kotlin.test.model.FrontendKind
import org.jetbrains.kotlin.test.services.JUnit5Assertions
import org.jetbrains.kotlin.test.services.configuration.JvmEnvironmentConfigurator
import org.junit.jupiter.api.Test

@TestMetadata("compiler/new-tests-infrastructure/testData")
@TestDataPath("\$PROJECT_ROOT")
class SomeTest {
    private val firAnalysisConfiguration: TestConfigurationBuilder.() -> Unit = {
        globalDefaults {
            frontend = FrontendKind.FIR
            backend = BackendKind.NoBackend
            targetPlatform = JvmPlatforms.defaultJvmPlatform
            dependencyKind = DependencyKind.Source
        }

        assertions = JUnit5Assertions

        useConfigurators(::JvmEnvironmentConfigurator)

        useFrontendFacades(::FirFrontendFacade)
        useFrontendHandlers(::FirDumpHandler)
    }

    @Test
    @TestMetadata("a.kt")
    fun testFirAnalysisA() {
        testRunner(firAnalysisConfiguration).runTest("compiler/new-tests-infrastructure/testData/a.kt")
    }

    private val classicAnalysisConfiguration: TestConfigurationBuilder.() -> Unit = {
        globalDefaults {
            frontend = FrontendKind.ClassicFrontend
            backend = BackendKind.NoBackend
            targetPlatform = JvmPlatforms.defaultJvmPlatform
            dependencyKind = DependencyKind.Source
        }

        assertions = JUnit5Assertions

        useConfigurators(::JvmEnvironmentConfigurator)

        useFrontendFacades(::ClassicFrontendFacade)
        useFrontendHandlers(::DeclarationsDumpHandler)
    }

    @Test
    @TestMetadata("a.kt")
    fun testClassicAnalysisA() {
        testRunner(classicAnalysisConfiguration).runTest("compiler/new-tests-infrastructure/testData/a.kt")
    }

    private val blackBoxRunnerConfiguration: TestConfigurationBuilder.() -> Unit = {
        globalDefaults {
            frontend = FrontendKind.ClassicFrontend
            backend = BackendKind.ClassicBackend
            targetPlatform = JvmPlatforms.defaultJvmPlatform
            dependencyKind = DependencyKind.Binary
        }

        assertions = JUnit5Assertions

        useConfigurators(::JvmEnvironmentConfigurator)

        useFrontendFacades(::ClassicFrontendFacade)
        useFrontendHandlers(::DeclarationsDumpHandler)
        useFrontend2BackendConverters(::ClassicFrontend2ClassicBackendConverter)
        useBackendFacades(::ClassicJvmBackendFacade)
        useArtifactsHandlers(::JvmBoxRunner)
    }


    @Test
    @TestMetadata("boxTest.kt")
    fun testBBBoxTest() {
        testRunner(blackBoxRunnerConfiguration).runTest("compiler/new-tests-infrastructure/testData/boxTest.kt")
    }

    @Test
    @TestMetadata("boxWithRuntimeTest.kt")
    fun testBBBoxWithRuntimeTest() {
        testRunner(blackBoxRunnerConfiguration).runTest("compiler/new-tests-infrastructure/testData/boxWithRuntimeTest.kt")
    }

    @Test
    @TestMetadata("boxWithJdkTest.kt")
    fun testBBBoxWithJdkTest() {
        testRunner(blackBoxRunnerConfiguration).runTest("compiler/new-tests-infrastructure/testData/boxWithJdkTest.kt")
    }
}
