/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.components

import org.jetbrains.kotlin.config.LanguageVersionSettings
import org.jetbrains.kotlin.config.LanguageVersionSettingsImpl
import org.jetbrains.kotlin.test.model.TestModule

abstract class LanguageVersionSettingsProvider {
    abstract fun extractSettingsFromDirectives(module: TestModule): LanguageVersionSettings
}

class LanguageVersionSettingsProviderImpl : LanguageVersionSettingsProvider() {
    override fun extractSettingsFromDirectives(module: TestModule): LanguageVersionSettings {
        // TODO: implement proper parsing
        return LanguageVersionSettingsImpl.DEFAULT
    }
}
