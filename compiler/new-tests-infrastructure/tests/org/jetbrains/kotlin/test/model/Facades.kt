/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.model

import org.jetbrains.kotlin.test.components.ConfigurationComponents

abstract class FrontendFacade<R : ResultingArtifact.Source, in P : DependencyProvider<R>>(
    val configurationComponents: ConfigurationComponents
) {
    abstract fun analyze(module: TestModule, dependencyProvider: P): R
}

abstract class Frontend2BackendConverter<R : ResultingArtifact.Source, out I : ResultingArtifact.BackendInputInfo>(
    val configurationComponents: ConfigurationComponents
) {
    abstract fun convert(module: TestModule, frontendResults: R, dependencyProvider: DependencyProvider<R>): I
}

abstract class BackendFacade<in I : ResultingArtifact.BackendInputInfo, out R : ResultingArtifact.Binary>(
    val configurationComponents: ConfigurationComponents
) {
    abstract fun produce(module: TestModule, initialInfo: I): R
}
