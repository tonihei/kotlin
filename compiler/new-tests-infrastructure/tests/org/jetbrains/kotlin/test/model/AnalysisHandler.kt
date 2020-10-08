/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.model

sealed class AnalysisHandler<in I> {
    abstract fun analyze(module: TestModule, info: I/*, someUtilitiesProvider */)
}

abstract class FrontendResultsHandler<in R : FrontendResults> : AnalysisHandler<R>()
abstract class BackendInitialInfoHandler<in I : BackendInitialInfo> : AnalysisHandler<I>()
abstract class ArtifactsResultsHandler<in A : ResultingArtifact> : AnalysisHandler<A>()
