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
    val defaultPlatform: TargetPlatform,
    val defaultDependencyKind: DependencyKind
)

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

    @DefaultsDsl
    class LanguageVersionSettingsBuilder {
        var languageVersion: LanguageVersion = LanguageVersion.LATEST_STABLE
        var apiVersion: ApiVersion = ApiVersion.LATEST_STABLE

        private val specificFeatures = mutableMapOf<LanguageFeature, LanguageFeature.State>()
        private val analysisFlags = mutableMapOf<AnalysisFlag<*>, Any?>()

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

    @OptIn(PrivateForInline::class)
    inline fun languageSettings(init: LanguageVersionSettingsBuilder.() -> Unit) {
        languageVersionSettings = LanguageVersionSettingsBuilder().apply(init).build()
    }

    @OptIn(PrivateForInline::class)
    fun build(): DefaultsProvider {
        return DefaultsProvider(
            backend,
            frontend,
            languageVersionSettings ?: LanguageVersionSettingsImpl(LanguageVersion.LATEST_STABLE, ApiVersion.LATEST_STABLE),
            targetPlatform,
            dependencyKind
        )
    }
}

inline fun globalDefaults(init: DefaultsProviderBuilder.() -> Unit): DefaultsProvider {
    return DefaultsProviderBuilder().apply(init).build()
}

fun test() {
    globalDefaults {
        languageSettings {
            languageVersion = LanguageVersion.LATEST_STABLE
            apiVersion = ApiVersion.LATEST_STABLE
            enable(LanguageFeature.TrailingCommas)
            disable(LanguageFeature.NewInference)
        }

        targetPlatform = JvmPlatforms.defaultJvmPlatform
        frontend = TargetFrontend.ClassicFrontend
        backend = TargetBackend.JVM_IR
    }
}
