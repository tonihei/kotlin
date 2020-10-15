/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.components

import org.jetbrains.kotlin.config.*
import org.jetbrains.kotlin.fir.PrivateForInline
import org.jetbrains.kotlin.platform.TargetPlatform
import org.jetbrains.kotlin.platform.jvm.JvmPlatforms
import org.jetbrains.kotlin.test.TargetBackend
import org.jetbrains.kotlin.test.model.DependencyKind
import org.jetbrains.kotlin.test.model.TargetFrontend

/*
 * TODO:
 *   - default target artifact
 *   - default libraries
 */
class DefaultsProvider(
    val defaultBackend: TargetBackend,
    val defaultFrontend: TargetFrontend,
    val defaultLanguageSettings: LanguageVersionSettings,
    private val defaultLanguageSettingsBuilder: LanguageVersionSettingsBuilder,
    val defaultPlatform: TargetPlatform,
    val defaultDependencyKind: DependencyKind
) {
    fun newLanguageSettingsBuilder(): LanguageVersionSettingsBuilder {
        return LanguageVersionSettingsBuilder.fromExistingSettings(defaultLanguageSettingsBuilder)
    }
}

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

@DslMarker
annotation class DefaultsDsl

@DefaultsDsl
class DefaultsProviderBuilder {
    lateinit var backend: TargetBackend
    lateinit var frontend: TargetFrontend
    lateinit var targetPlatform: TargetPlatform
    lateinit var dependencyKind: DependencyKind

    @PrivateForInline
    var languageVersionSettings: LanguageVersionSettings? = null

    @PrivateForInline
    var languageVersionSettingsBuilder: LanguageVersionSettingsBuilder? = null

    @OptIn(PrivateForInline::class)
    inline fun languageSettings(init: LanguageVersionSettingsBuilder.() -> Unit) {
        languageVersionSettings = LanguageVersionSettingsBuilder().apply(init).also {
            languageVersionSettingsBuilder = it
        }.build()
    }

    @OptIn(PrivateForInline::class)
    fun build(): DefaultsProvider {
        return DefaultsProvider(
            backend,
            frontend,
            languageVersionSettings ?: LanguageVersionSettingsImpl(LanguageVersion.LATEST_STABLE, ApiVersion.LATEST_STABLE),
            languageVersionSettingsBuilder ?: LanguageVersionSettingsBuilder(),
            targetPlatform,
            dependencyKind
        )
    }
}
