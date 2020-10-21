/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test

import com.intellij.testFramework.TestDataPath
import org.jetbrains.kotlin.platform.jvm.JvmPlatforms
import org.jetbrains.kotlin.test.backend.classic.ClassicJvmBackendFacade
import org.jetbrains.kotlin.test.backend.handlers.JvmBoxRunner
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
import org.junit.jupiter.api.Test

@TestMetadata("compiler/new-tests-infrastructure/testData")
@TestDataPath("\$PROJECT_ROOT")
class SomeFirAnalysisTest {
    private val testRunner = testRunner {
        globalDefaults {
            frontend = FrontendKind.FIR
            backend = BackendKind.NoBackend
            targetPlatform = JvmPlatforms.defaultJvmPlatform
            dependencyKind = DependencyKind.Source
        }

        assertions = JUnit5Assertions

        useFrontendFacades(::FirFrontendFacade)
        useFrontendHandlers(::FirDumpHandler)
    }

    @Test
    @TestMetadata("a.kt")
    fun testA() {
        testRunner.runTest("compiler/new-tests-infrastructure/testData/a.kt")
    }
}

@TestMetadata("compiler/new-tests-infrastructure/testData")
@TestDataPath("\$PROJECT_ROOT")
class SomeClassicAnalysisTest {
    private val testRunner = testRunner {
        globalDefaults {
            frontend = FrontendKind.ClassicFrontend
            backend = BackendKind.NoBackend
            targetPlatform = JvmPlatforms.defaultJvmPlatform
            dependencyKind = DependencyKind.Source
        }

        assertions = JUnit5Assertions

        useFrontendFacades(::ClassicFrontendFacade)
        useFrontendHandlers(::DeclarationsDumpHandler)
    }

    @Test
    @TestMetadata("a.kt")
    fun testA() {
        testRunner.runTest("compiler/new-tests-infrastructure/testData/a.kt")
    }
}

@TestMetadata("compiler/new-tests-infrastructure/testData")
@TestDataPath("\$PROJECT_ROOT")
class SomeClassicBlackBoxTest {
    private val testRunner = testRunner {
        globalDefaults {
            frontend = FrontendKind.ClassicFrontend
            backend = BackendKind.ClassicBackend
            targetPlatform = JvmPlatforms.defaultJvmPlatform
            dependencyKind = DependencyKind.Binary
        }

        assertions = JUnit5Assertions

        useFrontendFacades(::ClassicFrontendFacade)
        useFrontendHandlers(::DeclarationsDumpHandler)
        useFrontend2BackendConverters(::ClassicFrontend2ClassicBackendConverter)
        useBackendFacades(::ClassicJvmBackendFacade)
        useArtifactsHandlers(::JvmBoxRunner)
    }

    @Test
    @TestMetadata("boxTest.kt")
    fun testA() {
        testRunner.runTest("compiler/new-tests-infrastructure/testData/boxTest.kt")
    }
}
