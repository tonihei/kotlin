/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.model

import org.jetbrains.kotlin.platform.TargetPlatform
import java.io.File

data class TestModule(
    val name: String,
    val targetPlatform: TargetPlatform,
    val files: List<TestFile>,
    val dependencies: List<DependencyDescription>
)

// TODO: maybe String or File will be more useful
typealias FileContent = List<String>

data class TestFile(
    val name: String,
    val content: FileContent,
    val originalFile: File,
    val startLineNumberInOriginalFile: Int
)

enum class DependencyRelation {
    Dependency,
    DependsOn
}

enum class DependencyKind {
    Source,
    KLib,
    Binary
}

data class DependencyDescription(
    val moduleName: String,
    val kind: DependencyKind,
    val relation: DependencyRelation
)
