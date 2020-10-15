/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.directives

import org.jetbrains.kotlin.test.TargetBackend
import org.jetbrains.kotlin.test.model.TargetFrontend

object ModuleStructureDirectives : SimpleDirectivesContainer() {
    val module = valueDirective(
        name = "MODULE",
        """
            Usage: // MODULE: {name} [targets...]
            Describes one module. If no targets are specified then <TODO>
        """.trimIndent()
    )

    val dependency = valueDirective(
        name = "DEPENDENCY",
        """
            Usage: // DEPENDENCY: {name} [SOURCE|KLIB|BINARY]
            Declares simple dependency on other module 
        """.trimIndent()
    )

    val dependsOn = valueDirective(
        name = "DEPENDS_ON",
        """
            Usage: // DEPENDS_ON: {name} [SOURCE|KLIB|BINARY]
            Declares dependency on other module witch may contains `expect`
             declarations which has corresponding `expect` declarations
             in current module
        """.trimIndent()
    )

    val file = valueDirective(
        name = "FILE",
        """
            Usage: // FILE: name.{kt|java}
            Declares file with specified name in current module
        """.trimIndent()
    )

    val targetFrontend = enumDirective<TargetFrontend>(
        name = "TARGET_FRONTEND",
        """
            Usage: // TARGET_FRONTEND: {Frontend}
            Declares frontend for analyzing current module 
        """.trimIndent()
    )

    val targetBackend = enumDirective<TargetBackend>(
        name = "TARGET_BACKEND",
        """
            Usage: // TARGET_BACKEND: {Backend}
            Declares backend for analyzing current module 
        """.trimIndent()
    )
}
