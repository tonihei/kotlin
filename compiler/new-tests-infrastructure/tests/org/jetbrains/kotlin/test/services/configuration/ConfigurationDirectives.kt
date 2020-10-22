/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.services.configuration

import org.jetbrains.kotlin.test.directives.SimpleDirectivesContainer

object ConfigurationDirectives : SimpleDirectivesContainer() {
    val kotlinConfigurationFlags = valueDirective(
        "KOTLIN_CONFIGURATION_FLAGS",
        "List of kotlin configuration flags"
    )
}
