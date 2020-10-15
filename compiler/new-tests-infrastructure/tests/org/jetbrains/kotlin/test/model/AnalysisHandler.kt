/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.model

import org.jetbrains.kotlin.test.components.Assertions
import org.jetbrains.kotlin.test.components.ConfigurationComponents

sealed class AnalysisHandler<in I>(val configurationComponents: ConfigurationComponents) {
    protected val assertions: Assertions
        get() = configurationComponents.assertions

    abstract fun processModule(module: TestModule, info: I)

    abstract fun processAfterAllModules(moduleStructure: TestModuleStructure)
}

abstract class FrontendResultsHandler<in R : ResultingArtifact.Source>(
    configurationComponents: ConfigurationComponents,
    val frontendKind: FrontendKind
) : AnalysisHandler<R>(configurationComponents)

abstract class BackendInitialInfoHandler<in I : ResultingArtifact.BackendInputInfo>(
    configurationComponents: ConfigurationComponents,
    val backendKind: BackendKind
) : AnalysisHandler<I>(configurationComponents)

abstract class ArtifactsResultsHandler<in A : ResultingArtifact>(
    configurationComponents: ConfigurationComponents,
    val artifactKind: ArtifactKind
) : AnalysisHandler<A>(configurationComponents)

abstract class JvmBinaryArtifactsResultsHandler(
    configurationComponents: ConfigurationComponents
) : ArtifactsResultsHandler<ResultingArtifact.Binary.Jvm>(configurationComponents, ArtifactKind.Jvm)

abstract class JsBinaryArtifactsResultsHandler(
    configurationComponents: ConfigurationComponents
) : ArtifactsResultsHandler<ResultingArtifact.Binary.Js>(configurationComponents, ArtifactKind.Js)

abstract class NativeBinaryArtifactsResultsHandler(
    configurationComponents: ConfigurationComponents
) : ArtifactsResultsHandler<ResultingArtifact.Binary.Native>(configurationComponents, ArtifactKind.Native)
