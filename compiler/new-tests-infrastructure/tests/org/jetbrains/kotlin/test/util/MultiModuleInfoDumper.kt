/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.util

import org.jetbrains.kotlin.test.model.TestModule

abstract class MultiModuleInfoDumper {
    abstract fun builderForModule(module: TestModule): StringBuilder
    abstract fun generateResultingDump(): String
}

// TODO: consider about tests with multiple testdata files
class MultiModuleInfoDumperImpl : MultiModuleInfoDumper() {
    private val builderByModule = LinkedHashMap<TestModule, StringBuilder>()

    override fun builderForModule(module: TestModule): StringBuilder {
        return builderByModule.getOrPut(module, ::StringBuilder)
    }

    override fun generateResultingDump(): String {
        builderByModule.values.singleOrNull()?.let { return it.toString() }
        return buildString {
            for ((module, builder) in builderByModule) {
                appendLine("Module: ${module.name}")
                append(builder)
            }
        }
    }
}
