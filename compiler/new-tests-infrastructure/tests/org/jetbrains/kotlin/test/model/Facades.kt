/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.model

import org.jetbrains.kotlin.test.components.ConfigurationComponents

abstract class FrontendFacade<R : ResultingArtifact.Source<R>>(
    val configurationComponents: ConfigurationComponents,
    val frontendKind: FrontendKind<R>
) {
    abstract fun analyze(module: TestModule): R
}

abstract class Frontend2BackendConverter<R : ResultingArtifact.Source<R>, I : ResultingArtifact.BackendInputInfo<I>>(
    val configurationComponents: ConfigurationComponents,
    val frontendKind: FrontendKind<R>,
    val backendKind: BackendKind<I>
) {
    abstract fun convert(module: TestModule, frontendResults: R, dependencyProvider: DependencyProvider<R>): I
}

abstract class BackendFacade<I : ResultingArtifact.BackendInputInfo<I>, A : ResultingArtifact.Binary<A>>(
    val configurationComponents: ConfigurationComponents,
    val backendKind: BackendKind<I>,
    val artifactKind: ArtifactKind<A>
) {
    abstract fun produce(module: TestModule, initialInfo: I): A
}
