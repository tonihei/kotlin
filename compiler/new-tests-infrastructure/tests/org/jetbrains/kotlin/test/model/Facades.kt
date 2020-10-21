/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.model

import org.jetbrains.kotlin.test.services.ServiceRegistrationData
import org.jetbrains.kotlin.test.services.TestServices

abstract class FrontendFacade<R : ResultingArtifact.Source<R>>(
    val testServices: TestServices,
    val frontendKind: FrontendKind<R>
) {
    abstract fun analyze(module: TestModule): R

    open val additionalServices: List<ServiceRegistrationData>
        get() = emptyList()
}

abstract class Frontend2BackendConverter<R : ResultingArtifact.Source<R>, I : ResultingArtifact.BackendInputInfo<I>>(
    val testServices: TestServices,
    val frontendKind: FrontendKind<R>,
    val backendKind: BackendKind<I>
) {
    abstract fun convert(module: TestModule, frontendResults: ResultingArtifact.Source<R>): I

    open val additionalServices: List<ServiceRegistrationData>
        get() = emptyList()
}

abstract class BackendFacade<I : ResultingArtifact.BackendInputInfo<I>, A : ResultingArtifact.Binary<A>>(
    val testServices: TestServices,
    val backendKind: BackendKind<I>,
    val artifactKind: ArtifactKind<A>
) {
    abstract fun produce(module: TestModule, initialInfo: ResultingArtifact.BackendInputInfo<I>): A

    open val additionalServices: List<ServiceRegistrationData>
        get() = emptyList()
}
