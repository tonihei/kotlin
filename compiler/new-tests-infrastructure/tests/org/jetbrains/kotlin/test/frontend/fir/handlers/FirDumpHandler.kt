/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.frontend.fir.handlers

import org.jetbrains.kotlin.fir.render
import org.jetbrains.kotlin.test.frontend.fir.FirSourceArtifact
import org.jetbrains.kotlin.test.model.TestModule
import org.jetbrains.kotlin.test.model.TestModuleStructure
import org.jetbrains.kotlin.test.util.MultiModuleInfoDumper
import org.jetbrains.kotlin.test.util.MultiModuleInfoDumperImpl

class FirDumpHandler(
    testServices: TestServices
) : FirAnalysisHandler(testServices) {
    private val dumper: MultiModuleInfoDumper = MultiModuleInfoDumperImpl()

    override fun processModule(module: TestModule, info: FirSourceArtifact) {
        val builderForModule = dumper.builderForModule(module)
        val firFiles = info.firFiles
        firFiles.values.forEach { builderForModule.append(it.render()) }
    }

    override fun processAfterAllModules(moduleStructure: TestModuleStructure) {
        // TODO: change according to multiple testdata files
        val testDataFile = moduleStructure.originalTestDataFiles.first()
        val expectedFile = testDataFile.parentFile.resolve("${testDataFile.nameWithoutExtension}.fir")
        val actualText = dumper.generateResultingDump()
        assertions.assertEqualsToFile("Content is not equal", expectedFile, actualText)
    }
}
