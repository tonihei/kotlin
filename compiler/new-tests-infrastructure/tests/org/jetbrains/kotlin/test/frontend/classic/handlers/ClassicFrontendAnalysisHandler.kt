/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.frontend.classic.handlers

import org.jetbrains.kotlin.test.frontend.classic.ClassicFrontendSourceArtifacts
import org.jetbrains.kotlin.test.model.FrontendKind
import org.jetbrains.kotlin.test.model.FrontendResultsHandler
import org.jetbrains.kotlin.test.model.ResultingArtifact
import org.jetbrains.kotlin.test.model.TestModule
import org.jetbrains.kotlin.test.services.TestServices

abstract class ClassicFrontendAnalysisHandler(
    testServices: TestServices
) : FrontendResultsHandler<ClassicFrontendSourceArtifacts>(testServices, FrontendKind.ClassicFrontend) {
    final override fun processModule(module: TestModule, info: ResultingArtifact<ClassicFrontendSourceArtifacts>) {
        processModule(module, info as ClassicFrontendSourceArtifacts)
    }

    abstract fun processModule(module: TestModule, info: ClassicFrontendSourceArtifacts)
}


