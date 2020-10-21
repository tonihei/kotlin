/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.backend.classic

import org.jetbrains.kotlin.test.model.*
import org.jetbrains.kotlin.test.services.TestServices

abstract class ClassicBackendFacade<A : ResultingArtifact.Binary<A>>(
    testServices: TestServices,
    artifactKind: ArtifactKind<A>
) : BackendFacade<ClassicBackendInputInfo, A>(testServices, BackendKind.ClassicBackend, artifactKind) {
    final override fun produce(module: TestModule, initialInfo: ResultingArtifact.BackendInputInfo<ClassicBackendInputInfo>): A {
        return produce(module, initialInfo as ClassicBackendInputInfo)
    }

    abstract fun produce(module: TestModule, initialInfo: ClassicBackendInputInfo): A
}
