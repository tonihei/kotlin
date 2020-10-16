/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.model

import org.jetbrains.kotlin.test.services.Assertions
import org.jetbrains.kotlin.test.services.TestServices
import org.jetbrains.kotlin.test.services.assertions

sealed class AnalysisHandler<in I>(val testServices: TestServices) {
    protected val assertions: Assertions
        get() = testServices.assertions

    abstract fun processModule(module: TestModule, info: I)

    abstract fun processAfterAllModules(moduleStructure: TestModuleStructure)
}

abstract class FrontendResultsHandler<R : ResultingArtifact.Source<R>>(
    testServices: TestServices,
    val frontendKind: FrontendKind<R>
) : AnalysisHandler<R>(testServices)

abstract class BackendInitialInfoHandler<I : ResultingArtifact.BackendInputInfo<I>>(
    testServices: TestServices,
    val backendKind: BackendKind<I>
) : AnalysisHandler<I>(testServices)

abstract class ArtifactsResultsHandler<A : ResultingArtifact.Binary<A>>(
    testServices: TestServices,
    val artifactKind: ArtifactKind<A>
) : AnalysisHandler<A>(testServices)

abstract class JvmBinaryArtifactsResultsHandler(
    testServices: TestServices
) : ArtifactsResultsHandler<ResultingArtifact.Binary.Jvm>(testServices, ArtifactKind.Jvm)

abstract class JsBinaryArtifactsResultsHandler(
    testServices: TestServices
) : ArtifactsResultsHandler<ResultingArtifact.Binary.Js>(testServices, ArtifactKind.Js)

abstract class NativeBinaryArtifactsResultsHandler(
    testServices: TestServices
) : ArtifactsResultsHandler<ResultingArtifact.Binary.Native>(testServices, ArtifactKind.Native)
