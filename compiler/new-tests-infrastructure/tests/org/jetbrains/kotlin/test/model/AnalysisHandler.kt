/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.model

sealed class AnalysisHandler<in I> {
    abstract fun processModule(module: TestModule, info: I)

    abstract fun processAfterAllModules(moduleStructure: TestModuleStructure)
}

abstract class FrontendResultsHandler<in R : ResultingArtifact.Source> : AnalysisHandler<R>()
abstract class BackendInitialInfoHandler<in I : ResultingArtifact.BackendInputInfo> : AnalysisHandler<I>()

abstract class ArtifactsResultsHandler<in A : ResultingArtifact> : AnalysisHandler<A>()

abstract class BinaryArtifactsResultsHandler<in I : ResultingArtifact.Binary> : ArtifactsResultsHandler<I>()
abstract class JvmBinaryArtifactsResultsHandler : BinaryArtifactsResultsHandler<ResultingArtifact.Binary.Jvm>()
abstract class JsBinaryArtifactsResultsHandler : BinaryArtifactsResultsHandler<ResultingArtifact.Binary.Js>()
abstract class NativeBinaryArtifactsResultsHandler : BinaryArtifactsResultsHandler<ResultingArtifact.Binary.Native>()
