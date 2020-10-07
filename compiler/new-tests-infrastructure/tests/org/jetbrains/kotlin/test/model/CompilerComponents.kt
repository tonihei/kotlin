/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.model

import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.backend.Fir2IrComponents
import org.jetbrains.kotlin.fir.declarations.FirFile
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.util.SymbolTable
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi2ir.PsiSourceManager
import org.jetbrains.kotlin.resolve.BindingContext

// ------------------------- Frontend -------------------------

sealed class FrontendResults

// FIR
class FirFrontendResults(
    val session: FirSession,
    val firFiles: Map<TestFile, FirFile>
) : FrontendResults()

// FE 1.0
class ClassicFrontendResults(
    val psiFiles: Map<TestFile, KtFile>,
    val bindingContext: BindingContext
) : FrontendResults()

// ------------------------- Backend -------------------------

sealed class BackendInitialInfo

// Old backend (JVM and JS)
class ClassicBackendInitialInfo(
    val psiFiles: List<KtFile>,
    val bindingContext: BindingContext
) : BackendInitialInfo()

// IR backend (JVM, JS, Native)
class IrBackendInitialInfo(
    val irModuleFragment: IrModuleFragment,
    val symbolTable: SymbolTable,
    val sourceManager: PsiSourceManager,
    val components: Fir2IrComponents
) : BackendInitialInfo()

// ------------------------- Dependencies -------------------------

sealed class ResultingArtifact {
    abstract class Source<R : FrontendResults> : ResultingArtifact()
    class KLib : ResultingArtifact()
    sealed class Binary : ResultingArtifact() {
        class Jvm : Binary()
        class Js : Binary()
        class Native : Binary()
    }
}

abstract class DependencyProvider<R : FrontendResults, out A : ResultingArtifact.Source<R>> {
    abstract fun getTestModule(name: String): TestModule

    abstract fun getSourceModule(name: String): A?
    abstract fun getCompiledKlib(name: String): ResultingArtifact.KLib?
    abstract fun getBinaryDependency(name: String): ResultingArtifact.Binary?

    abstract fun registerAnalyzedModule(name: String, results: R)
    abstract fun registerCompiledKLib(name: String, artifact: ResultingArtifact.KLib)
    abstract fun registerCompiledBinary(name: String, artifact: ResultingArtifact.Binary)
}

// ------------------------- Components -------------------------

abstract class FrontendFacade<R : FrontendResults, out A : ResultingArtifact.Source<R>, in P : DependencyProvider<R, A>> {
    abstract fun analyze(module: TestModule, dependencyProvider: P): R
}

abstract class Frontend2BackendConverter<R : FrontendResults, in A : ResultingArtifact.Source<R>, out I : BackendInitialInfo> {
    abstract fun convert(module: TestModule, frontendResults: R, dependencyProvider: DependencyProvider<R, A>): I
}

sealed class BackendFacade<in I : BackendInitialInfo, out R : ResultingArtifact> {
    abstract fun produce(initialInfo: I): R
}

/**
 * Will have two implementations: ClassicJvmBackendFacade and ClassicJsBackendFacade
 */
abstract class ClassicBackendFacade<R : ResultingArtifact.Binary> : BackendFacade<ClassicBackendInitialInfo, R>()

/**
 * Will have one implementation for KLib and three implementations for different binaries
 */
abstract class IrBackendFacade<out R : ResultingArtifact> : BackendFacade<IrBackendInitialInfo, R>()

// ------------------------- Handlers -------------------------

interface Renderable

sealed class AnalysisHandler<in I> {
    abstract fun analyze(module: TestModule, info: I/*, someUtilitiesProvider */)
}

abstract class FrontendResultsHandler<in R : FrontendResults> : AnalysisHandler<R>()

abstract class BackendInitialInfoHandler<in I : BackendInitialInfo> : AnalysisHandler<I>()

abstract class ArtifactsResultsHandler<in A : ResultingArtifact> : AnalysisHandler<A>()
