/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.components

import com.intellij.rt.execution.junit.FileComparisonFailure
import org.jetbrains.kotlin.test.util.StringUtils.convertLineSeparators
import org.jetbrains.kotlin.test.util.StringUtils.trimTrailingWhitespacesAndAddNewlineAtEOF
import org.jetbrains.kotlin.utils.rethrow
import java.io.File
import java.io.IOException

abstract class Assertions {
    abstract fun assertEqualsToFile(message: String, expectedFile: File, actual: String, sanitizer: (String) -> String = { it })
    abstract fun fail(message: () -> String): Nothing
}

object JUnit5Assertions : Assertions() {
    override fun assertEqualsToFile(message: String, expectedFile: File, actual: String, sanitizer: (String) -> String) {
        try {
            val actualText = actual.trim { it <= ' ' }.convertLineSeparators().trimTrailingWhitespacesAndAddNewlineAtEOF()
            if (!expectedFile.exists()) {
                expectedFile.writeText(actualText)
                org.junit.jupiter.api.fail("Expected data file did not exist. Generating: $expectedFile")
            }
            val expected = expectedFile.readText().convertLineSeparators()
            val expectedText = expected.trim { it <= ' ' }.trimTrailingWhitespacesAndAddNewlineAtEOF()
            if (sanitizer.invoke(expectedText) != sanitizer.invoke(actualText)) {
                throw FileComparisonFailure(
                    message + ": " + expectedFile.name,
                    expected, actual, expectedFile.absolutePath
                )
            }
        } catch (e: IOException) {
            throw rethrow(e)
        }

    }

    override fun fail(message: () -> String): Nothing {
        org.junit.jupiter.api.fail(message)
    }
}
