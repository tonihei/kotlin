/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.components

class ConfigurationComponents {
    companion object {
        inline fun build(init: ConfigurationComponents.() -> Unit): ConfigurationComponents {
            return ConfigurationComponents().apply(init)
        }
    }

    lateinit var kotlinCoreEnvironmentProvider: KotlinCoreEnvironmentProvider
    lateinit var sourceFileProvider: SourceFileProvider
    lateinit var languageVersionSettingsProvider: LanguageVersionSettingsProvider
    lateinit var assertions: Assertions
}
