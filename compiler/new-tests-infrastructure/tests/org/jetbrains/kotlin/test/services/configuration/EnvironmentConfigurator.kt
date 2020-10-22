/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.services.configuration

import org.jetbrains.kotlin.cli.jvm.compiler.KotlinCoreEnvironment
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.test.directives.ComposedRegisteredDirectives
import org.jetbrains.kotlin.test.directives.DirectivesContainer
import org.jetbrains.kotlin.test.directives.RegisteredDirectives
import org.jetbrains.kotlin.test.model.TestModule
import org.jetbrains.kotlin.test.model.TestModuleStructure
import org.jetbrains.kotlin.test.model.moduleStructure
import org.jetbrains.kotlin.test.services.ServiceRegistrationData
import org.jetbrains.kotlin.test.services.TestServices
import org.jetbrains.kotlin.test.services.defaultDirectives

abstract class EnvironmentConfigurator(protected val testServices: TestServices) {
    open val directivesContainer: DirectivesContainer
        get() = DirectivesContainer.Empty

    open val additionalServices: List<ServiceRegistrationData>
        get() = emptyList()

    protected val moduleStructure: TestModuleStructure
        get() = testServices.moduleStructure

    protected val TestModule.allRegisteredDirectives: RegisteredDirectives
        get() = ComposedRegisteredDirectives(directives, testServices.defaultDirectives)

    open fun configureEnvironment(environment: KotlinCoreEnvironment, module: TestModule) {}

    open fun configureCompilerConfiguration(configuration: CompilerConfiguration, module: TestModule) {}
}
