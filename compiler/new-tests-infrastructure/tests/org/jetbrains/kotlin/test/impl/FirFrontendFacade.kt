/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.impl

import com.intellij.psi.PsiElementFinder
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.search.ProjectScope
import org.jetbrains.kotlin.asJava.finder.JavaElementFinder
import org.jetbrains.kotlin.checkers.TestCheckerUtil
import org.jetbrains.kotlin.cli.jvm.compiler.KotlinCoreEnvironment
import org.jetbrains.kotlin.cli.jvm.compiler.TopDownAnalyzerFacadeForJVM
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.analysis.FirAnalyzerFacade
import org.jetbrains.kotlin.fir.declarations.FirFile
import org.jetbrains.kotlin.fir.java.FirProjectSessionProvider
import org.jetbrains.kotlin.fir.session.FirJvmModuleInfo
import org.jetbrains.kotlin.fir.session.FirSessionFactory
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.test.components.ConfigurationComponents
import org.jetbrains.kotlin.test.components.isKtFile
import org.jetbrains.kotlin.test.model.*
import org.junit.jupiter.api.fail

class FirSourceArtifact(
    private val results: FirFrontendResults
) : ResultingArtifact.Source<FirFrontendResults>() {
    val session: FirSession
        get() = results.session

    val firFiles: Collection<FirFile>
        get() = results.firFiles.values
}

class FirDependencyProvider(
    val configurationComponents: ConfigurationComponents,
    testModules: List<TestModule>
) : DependencyProvider<FirFrontendResults, FirSourceArtifact>() {
    val firSessionProvider = FirProjectSessionProvider()
    private val analyzedModules = mutableMapOf<String, FirSourceArtifact>()

    private val testModulesByName: Map<String, TestModule> = testModules.map { it.name to it }.toMap()
    private val builtinsByModule: MutableMap<TestModule, FirJvmModuleInfo> = mutableMapOf()
    private val firModuleInfoByModule: MutableMap<TestModule, FirJvmModuleInfo> = mutableMapOf()

    fun convertToFirModuleInfo(module: TestModule): FirJvmModuleInfo {
        return firModuleInfoByModule.getOrPut(module) {
            val dependencies = mutableListOf(builtinsModuleInfoForModule(module))
            module.dependencies.mapTo(dependencies) {
                convertToFirModuleInfo(getTestModule(it.moduleName))
            }
            FirJvmModuleInfo(
                module.name,
                dependencies
            )
        }
    }

    fun builtinsModuleInfoForModule(module: TestModule): FirJvmModuleInfo {
        return builtinsByModule.getOrPut(module) {
            FirJvmModuleInfo(Name.special("<built-ins>"), emptyList())
        }
    }

    override fun getTestModule(name: String): TestModule {
        return testModulesByName[name] ?: fail { "Module $name is not defined" }
    }

    override fun getSourceModule(name: String): FirSourceArtifact? {
        return analyzedModules[name]
    }

    override fun getCompiledKlib(name: String): ResultingArtifact.KLib? {
        TODO("Not yet implemented")
    }

    override fun getBinaryDependency(name: String): ResultingArtifact.Binary? {
        TODO("Not yet implemented")
    }

    override fun registerAnalyzedModule(name: String, results: FirFrontendResults) {
        if (name in analyzedModules) {
            throw IllegalArgumentException("Analysis results of $name module already registered")
        }
        analyzedModules[name] = FirSourceArtifact(results)
    }

    override fun registerCompiledKLib(name: String, artifact: ResultingArtifact.KLib) {
        TODO("Not yet implemented")
    }

    override fun registerCompiledBinary(name: String, artifact: ResultingArtifact.Binary) {
        TODO("Not yet implemented")
    }
}

class FirFrontendFacade(
    val configurationComponents: ConfigurationComponents
) : FrontendFacade<FirFrontendResults, FirSourceArtifact, FirDependencyProvider>() {
    override fun analyze(module: TestModule, dependencyProvider: FirDependencyProvider): FirFrontendResults {
        val environment = configurationComponents.kotlinCoreEnvironmentProvider.getKotlinCoreEnvironment(module)
        // TODO: add configurable parser

        val project = environment.project

        PsiElementFinder.EP.getPoint(project).unregisterExtension(JavaElementFinder::class.java)

        val ktFiles = module.files.filter { it.isKtFile }.map {
            TestCheckerUtil.createCheckAndReturnPsiFile(
                it.name,
                configurationComponents.sourceFileProvider.getContentOfSourceFile(it),
                project
            )
        }

        val sessionProvider = dependencyProvider.firSessionProvider

        val languageVersionSettings = configurationComponents.languageVersionSettingsProvider.extractSettingsFromDirectives(module)
        val builtinsModuleInfo = dependencyProvider.builtinsModuleInfoForModule(module)
        createSessionForBuiltins(builtinsModuleInfo, sessionProvider, environment)
        createSessionForBinaryDependencies(module, sessionProvider, environment)

        val sourcesScope = TopDownAnalyzerFacadeForJVM.newModuleSearchScope(project, ktFiles)
        val sourcesModuleInfo = dependencyProvider.convertToFirModuleInfo(module)
        val session = FirSessionFactory.createJavaModuleBasedSession(
            sourcesModuleInfo,
            sessionProvider,
            sourcesScope,
            project,
            languageVersionSettings = languageVersionSettings
        )

        val firAnalyzerFacade = FirAnalyzerFacade(session, languageVersionSettings, ktFiles)
        val firFiles = firAnalyzerFacade.runResolution()
        val filesMap = firFiles.mapNotNull { firFile ->
            val testFile = module.files.firstOrNull { it.name == firFile.name } ?: return@mapNotNull null
            testFile to firFile
        }.toMap()

        return FirFrontendResults(session, filesMap, firAnalyzerFacade)
    }

    private fun createSessionForBuiltins(
        builtinsModuleInfo: FirJvmModuleInfo,
        sessionProvider: FirProjectSessionProvider,
        environment: KotlinCoreEnvironment,
    ) {
        //For BuiltIns, registered in sessionProvider automatically
        val project = environment.project
        val allProjectScope = GlobalSearchScope.allScope(project)

        FirSessionFactory.createLibrarySession(
            builtinsModuleInfo, sessionProvider, allProjectScope, project,
            environment.createPackagePartProvider(allProjectScope)
        )
    }

    private fun createSessionForBinaryDependencies(
        module: TestModule,
        sessionProvider: FirProjectSessionProvider,
        environment: KotlinCoreEnvironment
    ) {
        val project = environment.project
        val librariesScope = ProjectScope.getLibrariesScope(project)
        val librariesModuleInfo = FirJvmModuleInfo.createForLibraries(module.name)
        FirSessionFactory.createLibrarySession(
            librariesModuleInfo,
            sessionProvider,
            librariesScope,
            project,
            environment.createPackagePartProvider(librariesScope)
        )
    }
}

