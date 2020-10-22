/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.services

import org.jetbrains.kotlin.test.directives.ComposedRegisteredDirectives
import org.jetbrains.kotlin.test.directives.RegisteredDirectives
import org.jetbrains.kotlin.test.model.moduleStructure

class DefaultRegisteredDirectivesProvider(testServices: TestServices, defaultGlobalDirectives: RegisteredDirectives) : TestService {
    val defaultDirectives: RegisteredDirectives by lazy {
        ComposedRegisteredDirectives(testServices.moduleStructure.globalDirectives, defaultGlobalDirectives)
    }
}

private val TestServices.defaultRegisteredDirectivesProvider: DefaultRegisteredDirectivesProvider by TestServices.testServiceAccessor()

val TestServices.defaultDirectives: RegisteredDirectives
    get() = defaultRegisteredDirectivesProvider.defaultDirectives
