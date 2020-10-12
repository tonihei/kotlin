/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.model

sealed class AnalysisHandler<in I> {
    abstract fun processModule(module: TestModule, info: I)
}

abstract class FrontendResultsHandler<in R : ResultingArtifact.Source> : AnalysisHandler<R>()
abstract class BackendInitialInfoHandler<in I : ResultingArtifact.BackendInputInfo> : AnalysisHandler<I>()
abstract class ArtifactsResultsHandler<in A : ResultingArtifact> : AnalysisHandler<A>()

abstract class AllModulesAnalysisHandler<in I, H : AnalysisHandler<I>> {
    protected abstract val moduleHandler: H

    fun processModule(module: TestModule, info: I) {
        moduleHandler.processModule(module, info)
    }

    abstract fun processAfterAllModules(moduleStructure: TestModuleStructure)
}
