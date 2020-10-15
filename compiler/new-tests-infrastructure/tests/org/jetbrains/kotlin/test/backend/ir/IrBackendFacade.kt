/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.backend.ir

import org.jetbrains.kotlin.test.components.ConfigurationComponents
import org.jetbrains.kotlin.test.model.BackendFacade
import org.jetbrains.kotlin.test.model.ResultingArtifact

abstract class IrBackendFacade<out R : ResultingArtifact.Binary>(
    configurationComponents: ConfigurationComponents
) : BackendFacade<IrBackendInputInfo, R>(configurationComponents)
