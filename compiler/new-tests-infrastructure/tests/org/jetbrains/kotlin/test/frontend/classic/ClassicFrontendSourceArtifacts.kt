/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.frontend.classic

import com.intellij.openapi.project.Project
import org.jetbrains.kotlin.analyzer.AnalysisResult
import org.jetbrains.kotlin.config.LanguageVersionSettings
import org.jetbrains.kotlin.descriptors.ModuleDescriptor
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.test.model.FrontendKind
import org.jetbrains.kotlin.test.model.ResultingArtifact
import org.jetbrains.kotlin.test.model.TestFile

data class ClassicFrontendSourceArtifacts(
    val psiFiles: Map<TestFile, KtFile>,
    val analysisResult: AnalysisResult,
    val project: Project,
    val languageVersionSettings: LanguageVersionSettings
) : ResultingArtifact.Source<ClassicFrontendSourceArtifacts>() {
    override val frontendKind: FrontendKind.ClassicFrontend
        get() = FrontendKind.ClassicFrontend
}
