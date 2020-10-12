/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.frontend.classic.handlers

import org.jetbrains.kotlin.descriptors.DeclarationDescriptor
import org.jetbrains.kotlin.descriptors.ModuleDescriptor
import org.jetbrains.kotlin.descriptors.PackageViewDescriptor
import org.jetbrains.kotlin.name.SpecialNames
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.resolve.DescriptorUtils
import org.jetbrains.kotlin.test.components.Assertions
import org.jetbrains.kotlin.test.frontend.classic.ClassicFrontendSourceArtifacts
import org.jetbrains.kotlin.test.model.TestFile
import org.jetbrains.kotlin.test.model.TestModule
import org.jetbrains.kotlin.test.model.TestModuleStructure
import org.jetbrains.kotlin.test.util.DescriptorValidator
import org.jetbrains.kotlin.test.util.MultiModuleInfoDumper
import org.jetbrains.kotlin.test.util.MultiModuleInfoDumperImpl
import org.jetbrains.kotlin.test.util.RecursiveDescriptorComparator
import org.jetbrains.kotlin.test.util.RecursiveDescriptorComparator.RECURSIVE
import org.jetbrains.kotlin.utils.keysToMap
import java.util.function.Predicate

class DeclarationsDumpHandler(
    private val assertions: Assertions
) : ClassicFrontendAllModulesAnalysisHandler<MultiModuleInfoDumper>() {
    override val moduleHandler: ClassicFrontendAnalysisHandler<MultiModuleInfoDumper> = Handler()

    override val state: MultiModuleInfoDumper = MultiModuleInfoDumperImpl()

    override fun processAfterAllModules(moduleStructure: TestModuleStructure) {
        val resultDump = state.generateResultingDump()
        val testDataFile = moduleStructure.originalTestDataFiles.first()
        val expectedFileName = testDataFile.nameWithoutExtension + ".txt"
        val expectedFile = testDataFile.parentFile.resolve(expectedFileName)
        assertions.assertEqualsToFile(expectedFile, resultDump)
    }

    private inner class Handler : ClassicFrontendAnalysisHandler<MultiModuleInfoDumper>() {
        override fun processModule(module: TestModule, info: ClassicFrontendSourceArtifacts, state: MultiModuleInfoDumper) {
            val moduleDescriptor = info.analysisResult.moduleDescriptor
            val comparator = RecursiveDescriptorComparator(createdAffectedPackagesConfiguration(info.psiFiles, moduleDescriptor))
            val packages = info.psiFiles.values.map { it.packageFqName }
            val textByPackage = packages.keysToMap { StringBuilder() }

            for ((packageName, packageText) in textByPackage.entries) {
                val aPackage = moduleDescriptor.getPackage(packageName)
                assertions.assertFalse(aPackage.isEmpty())

                val actualSerialized = comparator.serializeRecursively(aPackage)
                packageText.append(actualSerialized)
            }
            val allPackagesText = textByPackage.values.joinToString("\n")
            state.builderForModule(module).appendLine(allPackagesText)
        }

        private fun createdAffectedPackagesConfiguration(
            psiFiles: Map<TestFile, KtFile>,
            moduleDescriptor: ModuleDescriptor
        ): RecursiveDescriptorComparator.Configuration {
            val packagesNames = psiFiles.values.mapTo(mutableSetOf()) {
                it.packageFqName.pathSegments().firstOrNull() ?: SpecialNames.ROOT_PACKAGE
            }
            val stepIntoFilter = Predicate<DeclarationDescriptor> { descriptor ->
                val module = DescriptorUtils.getContainingModuleOrNull(descriptor)
                if (module != moduleDescriptor) return@Predicate false

                if (descriptor is PackageViewDescriptor) {
                    val fqName = descriptor.fqName
                    return@Predicate fqName.isRoot || fqName.pathSegments().first() in packagesNames
                }

                true
            }
            return RECURSIVE.filterRecursion(stepIntoFilter)
                .withValidationStrategy(DescriptorValidator.ValidationVisitor.errorTypesAllowed())
                .checkFunctionContracts(true)
        }
    }
}
