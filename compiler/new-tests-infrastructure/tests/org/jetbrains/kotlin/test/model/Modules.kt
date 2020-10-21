/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.model

import org.jetbrains.kotlin.config.LanguageVersionSettings
import org.jetbrains.kotlin.platform.TargetPlatform
import org.jetbrains.kotlin.platform.js.JsPlatforms
import org.jetbrains.kotlin.platform.jvm.JvmPlatforms
import org.jetbrains.kotlin.platform.konan.NativePlatforms
import org.jetbrains.kotlin.test.directives.RegisteredDirectives
import java.io.File

data class TestModuleStructure(
    val modules: List<TestModule>,
    val globalDirectives: RegisteredDirectives,
    val originalTestDataFiles: List<File>
) {
    @OptIn(ExperimentalStdlibApi::class)
    private val targetArtifactsByModule: Map<String, List<ArtifactKind<*>>> = buildMap {
        for (module in modules) {
            val result = mutableListOf<ArtifactKind<*>>()
            for (dependency in module.dependencies) {
                if (dependency.kind == DependencyKind.KLib) {
                    result += ArtifactKind.KLib
                }
            }
            module.targetPlatform.toArtifactKind()?.let { result += it }
            put(module.name, result)
        }
    }

    fun getTargetArtifactKinds(module: TestModule): List<ArtifactKind<*>> {
        return targetArtifactsByModule[module.name] ?: emptyList()
    }

    override fun toString(): String {
        return buildString {
            appendLine("Global directives:\n  $globalDirectives")
            modules.forEach {
                appendLine(it)
                appendLine()
            }
        }
    }

    companion object {
        private fun TargetPlatform.toArtifactKind(): ArtifactKind<*>? = when (this) {
            in JvmPlatforms.allJvmPlatforms -> ArtifactKind.Jvm
            in JsPlatforms.allJsPlatforms -> ArtifactKind.Js
            in NativePlatforms.allNativePlatforms -> ArtifactKind.Native
            else -> null
        }
    }
}

data class TestModule(
    val name: String,
    val targetPlatform: TargetPlatform,
    val frontendKind: FrontendKind<*>,
    val targetBackend: BackendKind<*>,
    val files: List<TestFile>,
    val dependencies: List<DependencyDescription>,
    val directives: RegisteredDirectives,
    val languageVersionSettings: LanguageVersionSettings
) {
    override fun toString(): String {
        return buildString {
            appendLine("Module: $name")
            appendLine("targetPlatform = $targetPlatform")
            appendLine("Dependencies:")
            dependencies.forEach { appendLine("  $it") }
            appendLine("Directives:\n  $directives")
            files.forEach { appendLine(it) }
        }
    }
}

// TODO: maybe String or File will be more useful
typealias FileContent = List<String>

data class TestFile(
    val name: String,
    val content: FileContent,
    val originalFile: File,
    val startLineNumberInOriginalFile: Int // line count starts with 0
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
