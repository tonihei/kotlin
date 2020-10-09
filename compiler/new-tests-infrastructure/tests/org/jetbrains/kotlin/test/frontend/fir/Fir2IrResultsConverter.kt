/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.frontend.fir

import org.jetbrains.kotlin.fir.backend.jvm.FirJvmBackendClassResolver
import org.jetbrains.kotlin.test.backend.ir.IrBackendInputInfo
import org.jetbrains.kotlin.test.model.DependencyProvider
import org.jetbrains.kotlin.test.model.Frontend2BackendConverter
import org.jetbrains.kotlin.test.model.TestModule

class Fir2IrResultsConverter : Frontend2BackendConverter<FirSourceArtifact, IrBackendInputInfo>() {
    override fun convert(
        module: TestModule,
        frontendResults: FirSourceArtifact,
        dependencyProvider: DependencyProvider<FirSourceArtifact>
    ): IrBackendInputInfo {
        val (irModuleFragment, symbolTable, sourceManager, components) = frontendResults.firAnalyzerFacade.convertToIr()
        return IrBackendInputInfo(irModuleFragment, symbolTable, sourceManager, FirJvmBackendClassResolver(components))
    }
}
