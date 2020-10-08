/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.components

import org.jetbrains.kotlin.test.KotlinTestUtils
import org.jetbrains.kotlin.test.model.FileContent
import org.jetbrains.kotlin.test.model.TestFile
import java.io.File

abstract class SourceFilePreprocessor {
    abstract fun process(file: TestFile, content: FileContent): FileContent
}

abstract class SourceFileProvider {
    abstract val kotlinSourceDirectory: File
    abstract val javaSourceDirectory: File

    abstract fun getContentOfSourceFile(testFile: TestFile): String
    abstract fun getRealFileForSourceFile(testFile: TestFile): File
}

class SourceFileProviderImpl(val preprocessors: List<SourceFilePreprocessor>) : SourceFileProvider() {
    override val kotlinSourceDirectory: File = KotlinTestUtils.tmpDir("java-files")
    override val javaSourceDirectory: File = KotlinTestUtils.tmpDir("kotlin-files")

    private val contentOfFiles = mutableMapOf<TestFile, String>()
    private val realFileMap = mutableMapOf<TestFile, File>()

    override fun getContentOfSourceFile(testFile: TestFile): String {
        return contentOfFiles.getOrPut(testFile) {
            generateFinalContent(testFile)
        }
    }

    override fun getRealFileForSourceFile(testFile: TestFile): File {
        return realFileMap.getOrPut(testFile) {
            val directory = when {
                testFile.isKtFile -> kotlinSourceDirectory
                testFile.isJavaFile -> javaSourceDirectory
                else -> error("Unknown file type: ${testFile.name}")
            }
            directory.resolve(testFile.name).also {
                it.writeText(getContentOfSourceFile(testFile))
            }
        }
    }

    private fun generateFinalContent(testFile: TestFile): String {
        return preprocessors.fold(testFile.content) { content, preprocessor ->
            preprocessor.process(testFile, content)
        }.joinToString(System.lineSeparator())
    }
}

val TestFile.isKtFile: Boolean
    get() = name.endsWith(".kt") || name.endsWith(".kts")

val TestFile.isJavaFile: Boolean
    get() = name.endsWith(".java")
