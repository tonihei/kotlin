/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.backend.ir

import org.jetbrains.kotlin.backend.jvm.JvmBackendFacade
import org.jetbrains.kotlin.test.model.ArtifactKind
import org.jetbrains.kotlin.test.model.ResultingArtifact
import org.jetbrains.kotlin.test.model.TestModule
import org.jetbrains.kotlin.test.services.TestServices

class JvmIrBackendFacade(
    testServices: TestServices
) : IrBackendFacade<ResultingArtifact.Binary.Jvm>(testServices, ArtifactKind.Jvm) {
    override fun produce(
        module: TestModule,
        initialInfo: IrBackendInputInfo
    ): ResultingArtifact.Binary.Jvm {
        val (state, irModuleFragment, symbolTable, sourceManager, phaseConfig, irProviders, extensions, serializerFactory) = initialInfo

        JvmBackendFacade.doGenerateFilesInternal(
            state,
            irModuleFragment,
            symbolTable,
            sourceManager,
            phaseConfig,
            irProviders,
            extensions,
            serializerFactory
        )
        state.factory.done()

        return ResultingArtifact.Binary.Jvm(state.factory)
    }
}
