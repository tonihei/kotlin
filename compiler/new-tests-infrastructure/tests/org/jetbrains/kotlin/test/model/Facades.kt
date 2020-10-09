/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.model

abstract class FrontendFacade<R : ResultingArtifact.Source, in P : DependencyProvider<R>> {
    abstract fun analyze(module: TestModule, dependencyProvider: P): R
}

abstract class Frontend2BackendConverter<R : ResultingArtifact.Source, out I : ResultingArtifact.BackendInitialInfo> {
    abstract fun convert(module: TestModule, frontendResults: R, dependencyProvider: DependencyProvider<R>): I
}

abstract class BackendFacade<in I : ResultingArtifact.BackendInitialInfo, out R : ResultingArtifact.Binary> {
    abstract fun produce(initialInfo: I): R
}
