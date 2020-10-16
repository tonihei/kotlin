/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test

import com.intellij.openapi.Disposable
import org.jetbrains.kotlin.test.services.TestServices
import org.jetbrains.kotlin.test.model.*

abstract class TestConfiguration {
    abstract val rootDisposable: Disposable

    abstract val testServices: TestServices

    abstract fun <R : ResultingArtifact.Source<R>> getFrontendFacade(frontendKind: FrontendKind<R>): FrontendFacade<R>

    abstract fun <R : ResultingArtifact.Source<R>, I : ResultingArtifact.BackendInputInfo<I>> getConverter(
        frontendKind: FrontendKind<R>,
        backendKind: BackendKind<I>
    ): Frontend2BackendConverter<R, I>

    abstract fun <I : ResultingArtifact.BackendInputInfo<I>, A : ResultingArtifact.Binary<A>> getBackendFacade(
        backendKind: BackendKind<I>,
        artifactKind: ArtifactKind<A>
    ): BackendFacade<I, A>

    abstract fun <R : ResultingArtifact.Source<R>> getFrontendHandlers(frontendKind: FrontendKind<R>): List<FrontendResultsHandler<R>>
    abstract fun <I : ResultingArtifact.BackendInputInfo<I>> getBackendHandlers(backendKind: BackendKind<I>): List<BackendInitialInfoHandler<I>>
    abstract fun <A : ResultingArtifact.Binary<A>> getArtifactHandlers(artifactKind: ArtifactKind<A>): List<ArtifactsResultsHandler<A>>
}
