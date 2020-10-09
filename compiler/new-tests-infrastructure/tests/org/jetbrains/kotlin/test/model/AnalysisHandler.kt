/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.model

sealed class AnalysisHandler<in I, in S> {
    abstract fun processModule(module: TestModule, info: I, state: S)
}

abstract class FrontendResultsHandler<in R : ResultingArtifact.Source, in S> : AnalysisHandler<R, S>()
abstract class BackendInitialInfoHandler<in I : ResultingArtifact.BackendInitialInfo, in S> : AnalysisHandler<I, S>()
abstract class ArtifactsResultsHandler<in A : ResultingArtifact, in S> : AnalysisHandler<A, S>()

abstract class AllModulesAnalysisHandler<in I, S, H : AnalysisHandler<I, S>> {
    protected abstract val moduleHandler: H
    protected abstract val state: S

    fun processModule(module: TestModule, info: I) {
        moduleHandler.processModule(module, info, state)
    }

    abstract fun processAfterAllModules(moduleStructure: TestModuleStructure)
}

fun <I> AnalysisHandler<I, Nothing?>.processModule(module: TestModule, info: I) {
    processModule(module, info, null)
}
