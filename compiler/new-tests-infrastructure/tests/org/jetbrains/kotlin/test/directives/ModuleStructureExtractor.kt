/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.directives

import org.jetbrains.kotlin.platform.TargetPlatform
import org.jetbrains.kotlin.test.components.*
import org.jetbrains.kotlin.test.builders.LanguageVersionSettingsBuilder
import org.jetbrains.kotlin.test.model.*
import java.io.File

class ModuleStructureExtractor private constructor(
    private val testDataFiles: List<File>,
    private val directivesContainer: DirectivesContainer,
    private val configurationComponents: ConfigurationComponents
) {
    companion object {
        fun splitTestDataByModules(
            testDataFileName: String,
            directivesContainer: DirectivesContainer,
            configurationComponents: ConfigurationComponents
        ): TestModuleStructure {
            val testDataFile = File(testDataFileName)
            val extractor = ModuleStructureExtractor(listOf(testDataFile), directivesContainer, configurationComponents)
            return extractor.splitTestDataByModules()
        }

        private val allowedExtensionsForFiles = listOf(".kt", ".java")
    }

    private val assertions: Assertions
        get() = configurationComponents.assertions

    private val defaultsProvider: DefaultsProvider
        get() = configurationComponents.defaultsProvider

    private lateinit var currentTestDataFile: File

    private val defaultFileName: String
        get() = currentTestDataFile.name

    private val defaultModuleName: String
        get() = "main"

    private var currentModuleName: String? = null
    private var currentModuleTargetPlatform: TargetPlatform? = null
    private var currentModuleFrontendKind: FrontendKind<*>? = null
    private var currentModuleBackendKind: BackendKind<*>? = null
    private var currentModuleLanguageVersionSettingsBuilder: LanguageVersionSettingsBuilder = initLanguageSettingsBuilder()
    private var dependenciesOfCurrentModule = mutableListOf<DependencyDescription>()
    private var filesOfCurrentModule = mutableListOf<TestFile>()

    private var currentFileName: String? = null
    private var linesOfCurrentFile = mutableListOf<String>()
    private var startLineNumberOfCurrentFile = 0

    private var directivesBuilder = RegisteredDirectivesBuilder(directivesContainer, assertions)

    private var globalDirectives: RegisteredDirectives? = null

    private val modules = mutableListOf<TestModule>()

    private val moduleDirectiveBuilder = RegisteredDirectivesBuilder(ModuleStructureDirectives, assertions)

    fun splitTestDataByModules(): TestModuleStructure {
        for (testDataFile in testDataFiles) {
            currentTestDataFile = testDataFile
            val lines = testDataFile.readLines()
            lines.forEachIndexed { lineNumber, line ->
                val rawDirective = RegisteredDirectivesBuilder.parseDirective(line)
                if (tryParseStructureDirective(rawDirective, lineNumber + 1)) return@forEachIndexed
                tryParseRegularDirective(rawDirective)
                linesOfCurrentFile.add(line)
            }
        }
        finishModule()
        return TestModuleStructure(modules, globalDirectives ?: RegisteredDirectives.Empty, testDataFiles)
    }

    /*
     * returns [true] means that passed directive was module directive and line is processed
     */
    private fun tryParseStructureDirective(rawDirective: RegisteredDirectivesBuilder.RawDirective?, lineNumber: Int): Boolean {
        if (rawDirective == null) return false
        val (directive, values) = moduleDirectiveBuilder.convertToRegisteredDirective(rawDirective) ?: return false
        when (directive) {
            ModuleStructureDirectives.module -> {
                /*
                 * There was previous module, so we should save it
                 */
                if (currentModuleName != null) {
                    finishModule()
                } else {
                    finishGlobalDirectives()
                }

                currentModuleName = values.first() as String
                @Suppress("UNCHECKED_CAST")
                currentModuleTargetPlatform = parseTargetPlatform(values.subList(1, values.size) as List<String>)
            }
            ModuleStructureDirectives.dependency,
            ModuleStructureDirectives.dependsOn -> {
                val name = values.first() as String
                val kind = values.getOrNull(1)?.let { valueOfOrNull(it as String) } ?: defaultsProvider.defaultDependencyKind
                val relation = when (directive) {
                    ModuleStructureDirectives.dependency -> DependencyRelation.Dependency
                    ModuleStructureDirectives.dependsOn -> DependencyRelation.DependsOn
                    else -> error("Should not be here")
                }
                dependenciesOfCurrentModule.add(DependencyDescription(name, kind, relation))
            }
            ModuleStructureDirectives.targetFrontend -> {
                val name = values.singleOrNull() as? String? ?: assertions.fail {
                    "Target frontend specified incorrectly\nUsage: ${directive.description}"
                }
                currentModuleFrontendKind = FrontendKind.fromString(name) ?: assertions.fail {
                    "Unknown frontend: $name"
                }
            }
            ModuleStructureDirectives.targetBackend -> {
                val name = values.singleOrNull() as? String ?: assertions.fail {
                    "Target backend specified incorrectly\nUsage: ${directive.description}"
                }
                currentModuleBackendKind = BackendKind.fromString(name) ?: assertions.fail {
                    "Unknown backend: $name"
                }
            }
            ModuleStructureDirectives.file -> {
                if (currentFileName != null) {
                    finishFile()
                } else {
                    resetFileCaches()
                }
                currentFileName = (values.first() as String).also(::validateFileName)
                startLineNumberOfCurrentFile = lineNumber
            }
            else -> return false
        }

        return true
    }

    private fun finishGlobalDirectives() {
        globalDirectives = directivesBuilder.build()
        resetModuleCaches()
        resetFileCaches()
    }

    private fun finishModule() {
        finishFile()
        modules += TestModule(
            name = currentModuleName ?: defaultModuleName,
            targetPlatform = currentModuleTargetPlatform ?: defaultsProvider.defaultPlatform,
            frontendKind = currentModuleFrontendKind ?: defaultsProvider.defaultFrontend,
            targetBackend = currentModuleBackendKind ?: defaultsProvider.defaultBackend,
            files = filesOfCurrentModule,
            dependencies = dependenciesOfCurrentModule,
            directives = directivesBuilder.build(),
            languageVersionSettings = currentModuleLanguageVersionSettingsBuilder.build()
        )
        resetModuleCaches()
    }

    private fun finishFile() {
        filesOfCurrentModule.add(
            TestFile(
                name = currentFileName ?: defaultFileName,
                content = linesOfCurrentFile,
                originalFile = currentTestDataFile,
                startLineNumberInOriginalFile = startLineNumberOfCurrentFile
            )
        )
        resetFileCaches()
    }

    private fun resetModuleCaches() {
        currentModuleName = null
        currentModuleTargetPlatform = null
        currentModuleFrontendKind = null
        currentModuleBackendKind = null
        currentModuleLanguageVersionSettingsBuilder = initLanguageSettingsBuilder()
        filesOfCurrentModule = mutableListOf()
        dependenciesOfCurrentModule = mutableListOf()
        directivesBuilder = RegisteredDirectivesBuilder(directivesContainer, assertions)
    }

    private fun resetFileCaches() {
        linesOfCurrentFile = mutableListOf()
        currentFileName = null
        startLineNumberOfCurrentFile = 0
    }

    private fun tryParseRegularDirective(rawDirective: RegisteredDirectivesBuilder.RawDirective?) {
        if (rawDirective == null) return
        val parsedDirective = directivesBuilder.convertToRegisteredDirective(rawDirective) ?: return
        if (parsedDirective.directive in LanguageSettingsDirectives) {
            configureLanguageDirective(rawDirective)
        }
        directivesBuilder.addParsedDirective(parsedDirective)
    }

    private fun configureLanguageDirective(rawDirective: RegisteredDirectivesBuilder.RawDirective) {
        // TODO: implement proper language settings parsing
    }

    private fun parseTargetPlatform(values: List<String>): TargetPlatform? {
        // TODO: implement proper target parsing
        return null
    }

    private fun validateFileName(fileName: String) {
        if (!allowedExtensionsForFiles.any { fileName.endsWith(it) }) {
            assertions.fail {
                "Filename $fileName is not valid. Allowed extensions: ${allowedExtensionsForFiles.toArrayString()}"
            }
        }
    }

    private fun initLanguageSettingsBuilder(): LanguageVersionSettingsBuilder {
        return defaultsProvider.newLanguageSettingsBuilder()
    }
}

fun Iterable<*>.toArrayString(): String = joinToString(separator = ", ", prefix = "[", postfix = "]")
fun Array<*>.toArrayString(): String = joinToString(separator = ", ", prefix = "[", postfix = "]")

inline fun <reified T : Enum<T>> valueOfOrNull(value: String): T? {
    for (enumValue in enumValues<T>()) {
        if (enumValue.name == value) {
            return enumValue
        }
    }
    return null
}
