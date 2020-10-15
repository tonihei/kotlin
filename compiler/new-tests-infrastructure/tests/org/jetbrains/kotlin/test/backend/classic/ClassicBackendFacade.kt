/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.backend.classic

import org.jetbrains.kotlin.test.components.ConfigurationComponents
import org.jetbrains.kotlin.test.model.ArtifactKind
import org.jetbrains.kotlin.test.model.BackendFacade
import org.jetbrains.kotlin.test.model.BackendKind
import org.jetbrains.kotlin.test.model.ResultingArtifact

abstract class ClassicBackendFacade<R : ResultingArtifact.Binary>(
    configurationComponents: ConfigurationComponents,
    artifactKind: ArtifactKind
) : BackendFacade<ClassicBackendInputInfo, R>(configurationComponents, BackendKind.ClassicBackend, artifactKind)
