/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.frontend.fir

import org.jetbrains.kotlin.fir.backend.jvm.FirJvmBackendClassResolver
import org.jetbrains.kotlin.fir.backend.jvm.FirMetadataSerializer
import org.jetbrains.kotlin.test.backend.ir.IrBackendInputInfo
import org.jetbrains.kotlin.test.components.TestServices
import org.jetbrains.kotlin.test.model.*

class Fir2IrResultsConverter(
    testServices: TestServices
) : Frontend2BackendConverter<FirSourceArtifact, IrBackendInputInfo>(
    testServices,
    FrontendKind.FIR,
    BackendKind.IrBackend
) {
    override fun convert(
        module: TestModule,
        frontendResults: FirSourceArtifact
    ): IrBackendInputInfo {
        val (irModuleFragment, symbolTable, sourceManager, components) = frontendResults.firAnalyzerFacade.convertToIr()
        return IrBackendInputInfo(
            irModuleFragment,
            symbolTable,
            sourceManager,
            FirJvmBackendClassResolver(components),
            frontendResults.firAnalyzerFacade.ktFiles
        ) { context, irClass, _, serializationBindings, parent ->
            FirMetadataSerializer(frontendResults.session, context, irClass, serializationBindings, parent)
        }
    }
}
