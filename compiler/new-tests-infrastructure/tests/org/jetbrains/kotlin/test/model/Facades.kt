/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.model

abstract class FrontendFacade<R : FrontendResults, out A : ResultingArtifact.Source<R>, in P : DependencyProvider<R, A>> {
    abstract fun analyze(module: TestModule, dependencyProvider: P): R
}

abstract class Frontend2BackendConverter<R : FrontendResults, in A : ResultingArtifact.Source<R>, out I : BackendInitialInfo> {
    abstract fun convert(module: TestModule, frontendResults: R, dependencyProvider: DependencyProvider<R, A>): I
}

abstract class BackendFacade<in I : BackendInitialInfo, out R : ResultingArtifact> {
    abstract fun produce(initialInfo: I): R
}
