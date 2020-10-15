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
    configurationComponents: ConfigurationComponents
) : AnalysisHandler<R>(configurationComponents)

abstract class BackendInitialInfoHandler<in I : ResultingArtifact.BackendInputInfo>(
    configurationComponents: ConfigurationComponents
) : AnalysisHandler<I>(configurationComponents)

abstract class ArtifactsResultsHandler<in A : ResultingArtifact>(
    configurationComponents: ConfigurationComponents
) : AnalysisHandler<A>(configurationComponents)

abstract class BinaryArtifactsResultsHandler<in I : ResultingArtifact.Binary>(
    configurationComponents: ConfigurationComponents
) : ArtifactsResultsHandler<I>(configurationComponents)

abstract class JvmBinaryArtifactsResultsHandler(
    configurationComponents: ConfigurationComponents
) : BinaryArtifactsResultsHandler<ResultingArtifact.Binary.Jvm>(configurationComponents)

abstract class JsBinaryArtifactsResultsHandler(
    configurationComponents: ConfigurationComponents
) : BinaryArtifactsResultsHandler<ResultingArtifact.Binary.Js>(configurationComponents)

abstract class NativeBinaryArtifactsResultsHandler(
    configurationComponents: ConfigurationComponents
) : BinaryArtifactsResultsHandler<ResultingArtifact.Binary.Native>(configurationComponents)
