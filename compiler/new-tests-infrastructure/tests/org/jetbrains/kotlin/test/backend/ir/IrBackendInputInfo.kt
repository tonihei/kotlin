/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.backend.ir

import org.jetbrains.kotlin.backend.jvm.MetadataSerializerFactory
import org.jetbrains.kotlin.codegen.JvmBackendClassResolver
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.util.SymbolTable
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi2ir.PsiSourceManager
import org.jetbrains.kotlin.test.model.BackendKind
import org.jetbrains.kotlin.test.model.ResultingArtifact

// IR backend (JVM, JS, Native)
data class IrBackendInputInfo(
    val irModuleFragment: IrModuleFragment,
    val symbolTable: SymbolTable,
    val sourceManager: PsiSourceManager,
    val jvmBackendClassResolver: JvmBackendClassResolver,
    val ktFiles: Collection<KtFile>,
    val serializerFactory: MetadataSerializerFactory
) : ResultingArtifact.BackendInputInfo<IrBackendInputInfo>() {
    override val backendKind: BackendKind.IrBackend
        get() = BackendKind.IrBackend
}
