/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.builders

import org.jetbrains.kotlin.config.*
import org.jetbrains.kotlin.test.components.DefaultsDsl

@DefaultsDsl
class LanguageVersionSettingsBuilder {
    companion object {
        fun fromExistingSettings(builder: LanguageVersionSettingsBuilder): LanguageVersionSettingsBuilder {
            return LanguageVersionSettingsBuilder().apply {
                languageVersion = builder.languageVersion
                apiVersion = builder.apiVersion
                specificFeatures += builder.specificFeatures
                analysisFlags += builder.analysisFlags
            }
        }
    }

    var languageVersion: LanguageVersion = LanguageVersion.LATEST_STABLE
    var apiVersion: ApiVersion = ApiVersion.LATEST_STABLE

    private val specificFeatures: MutableMap<LanguageFeature, LanguageFeature.State> = mutableMapOf()
    private val analysisFlags: MutableMap<AnalysisFlag<*>, Any?> = mutableMapOf()

    fun enable(feature: LanguageFeature) {
        specificFeatures[feature] = LanguageFeature.State.ENABLED
    }

    fun disable(feature: LanguageFeature) {
        specificFeatures[feature] = LanguageFeature.State.DISABLED
    }

    fun <T> withFlag(flag: AnalysisFlag<T>, value: T) {
        analysisFlags[flag] = value
    }

    fun build(): LanguageVersionSettings {
        return LanguageVersionSettingsImpl(languageVersion, apiVersion, analysisFlags, specificFeatures)
    }
}
