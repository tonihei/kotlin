/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.frontend.classic

import org.jetbrains.kotlin.test.backend.classic.ClassicBackendInputInfo
import org.jetbrains.kotlin.test.services.TestServices
import org.jetbrains.kotlin.test.model.*

class ClassicFrontend2ClassicBackendConverter(
    testServices: TestServices
) : Frontend2BackendConverter<ClassicFrontendSourceArtifacts, ClassicBackendInputInfo>(
    testServices,
    FrontendKind.ClassicFrontend,
    BackendKind.ClassicBackend
) {
    override fun convert(
        module: TestModule,
        frontendResults: ClassicFrontendSourceArtifacts
    ): ClassicBackendInputInfo {
        val (psiFiles, analysisResults, project, languageVersionSettings) = frontendResults
        return ClassicBackendInputInfo(
            psiFiles.values,
            analysisResults.bindingContext,
            analysisResults.moduleDescriptor,
            project,
            languageVersionSettings
        )
    }
}
