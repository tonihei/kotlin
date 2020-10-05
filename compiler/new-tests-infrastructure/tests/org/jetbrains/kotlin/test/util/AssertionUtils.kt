/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.util

import com.intellij.rt.execution.junit.FileComparisonFailure
import org.jetbrains.kotlin.test.util.StringUtils.convertLineSeparators
import org.jetbrains.kotlin.test.util.StringUtils.trimTrailingWhitespacesAndAddNewlineAtEOF
import org.jetbrains.kotlin.utils.rethrow
import org.junit.jupiter.api.fail
import java.io.File
import java.io.IOException

object AssertionUtils {
    fun assertEqualsToFile(message: String, expectedFile: File, actual: String, sanitizer: (String) -> String = { it }) {
        try {
            val actualText = actual.trim { it <= ' ' }.convertLineSeparators().trimTrailingWhitespacesAndAddNewlineAtEOF()
            if (!expectedFile.exists()) {
                expectedFile.writeText(actualText)
                fail("Expected data file did not exist. Generating: $expectedFile")
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
}
