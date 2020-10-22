/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.services.configuration

import com.intellij.openapi.util.SystemInfo
import org.jetbrains.kotlin.cli.jvm.config.addJavaSourceRoots
import org.jetbrains.kotlin.cli.jvm.config.addJvmClasspathRoot
import org.jetbrains.kotlin.cli.jvm.config.addJvmClasspathRoots
import org.jetbrains.kotlin.codegen.forTestCompile.ForTestCompileRuntime
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.config.JVMConfigurationKeys
import org.jetbrains.kotlin.config.JvmTarget
import org.jetbrains.kotlin.platform.jvm.JvmPlatforms
import org.jetbrains.kotlin.test.ConfigurationKind
import org.jetbrains.kotlin.test.KotlinTestUtils
import org.jetbrains.kotlin.test.TestJdkKind
import org.jetbrains.kotlin.test.directives.DirectivesContainer
import org.jetbrains.kotlin.test.directives.RegisteredDirectives
import org.jetbrains.kotlin.test.directives.SimpleDirectivesContainer
import org.jetbrains.kotlin.test.directives.joinToArrayString
import org.jetbrains.kotlin.test.model.BackendKind
import org.jetbrains.kotlin.test.model.DependencyKind
import org.jetbrains.kotlin.test.model.TestModule
import org.jetbrains.kotlin.test.services.*
import org.jetbrains.kotlin.test.services.jvm.CompiledJarManager
import org.jetbrains.kotlin.test.services.jvm.compiledJarManager
import java.io.File

class JvmEnvironmentConfigurator(testServices: TestServices) : EnvironmentConfigurator(testServices) {
    override val directivesContainer: DirectivesContainer
        get() = JvmEnvironmentConfigurationDirectives

    override val additionalServices: List<ServiceRegistrationData>
        get() = listOf(service(::CompiledJarManager))

    override fun configureCompilerConfiguration(configuration: CompilerConfiguration, module: TestModule) {
        if (module.targetPlatform !in JvmPlatforms.allJvmPlatforms) return
        val registeredDirectives = module.allRegisteredDirectives
        val targets = registeredDirectives[JvmEnvironmentConfigurationDirectives.jvmTarget]
        when (targets.size) {
            0 -> {}
            1 -> configuration.put(JVMConfigurationKeys.JVM_TARGET, targets.single())
            else -> error("Too many jvm targets passed: ${targets.joinToArrayString()}")
        }

        when (extractJdkKind(registeredDirectives)) {
            TestJdkKind.MOCK_JDK -> {
                configuration.addJvmClasspathRoot(KotlinTestUtils.findMockJdkRtJar())
                configuration.put(JVMConfigurationKeys.NO_JDK, true)
            }
            TestJdkKind.MODIFIED_MOCK_JDK -> {
                configuration.addJvmClasspathRoot(KotlinTestUtils.findMockJdkRtModified())
                configuration.put(JVMConfigurationKeys.NO_JDK, true)
            }
            TestJdkKind.FULL_JDK_6 -> {
                val jdk6 = System.getenv("JDK_16") ?: error("Environment variable JDK_16 is not set")
                configuration.put(JVMConfigurationKeys.JDK_HOME, File(jdk6))
            }
            TestJdkKind.FULL_JDK_9 -> {
                configuration.put(JVMConfigurationKeys.JDK_HOME, KotlinTestUtils.getJdk9Home())
            }
            TestJdkKind.FULL_JDK -> {
                if (SystemInfo.IS_AT_LEAST_JAVA9) {
                    configuration.put(JVMConfigurationKeys.JDK_HOME, File(System.getProperty("java.home")))
                }
            }
            TestJdkKind.ANDROID_API -> {
                configuration.addJvmClasspathRoot(KotlinTestUtils.findAndroidApiJar())
                configuration.put(JVMConfigurationKeys.NO_JDK, true)
            }
        }

        val configurationKind = extractConfigurationKind(registeredDirectives)

        if (configurationKind.withRuntime) {
            configuration.addJvmClasspathRoot(ForTestCompileRuntime.runtimeJarForTests())
            configuration.addJvmClasspathRoot(ForTestCompileRuntime.scriptRuntimeJarForTests())
            configuration.addJvmClasspathRoot(ForTestCompileRuntime.kotlinTestJarForTests())
        } else if (configurationKind.withMockRuntime) {
            configuration.addJvmClasspathRoot(ForTestCompileRuntime.minimalRuntimeJarForTests())
            configuration.addJvmClasspathRoot(ForTestCompileRuntime.scriptRuntimeJarForTests())
        }
        if (configurationKind.withReflection) {
            configuration.addJvmClasspathRoot(ForTestCompileRuntime.reflectJarForTests())
        }

        val isIr = module.targetBackend == BackendKind.IrBackend
        configuration.put(JVMConfigurationKeys.IR, isIr)

        module.javaFiles.takeIf { it.isNotEmpty() }?.let { javaFiles ->
            val files = javaFiles.map { testServices.sourceFileProvider.getRealFileForSourceFile(it) }
            configuration.addJavaSourceRoots(files)
        }

        configuration.registerModuleDependencies(module)
    }

    private fun extractJdkKind(registeredDirectives: RegisteredDirectives): TestJdkKind {
        val fullJdkEnabled = JvmEnvironmentConfigurationDirectives.fullJdk in registeredDirectives
        val jdkKinds = registeredDirectives[JvmEnvironmentConfigurationDirectives.jdkKind]

        if (fullJdkEnabled) {
            if (jdkKinds.isNotEmpty()) {
                error("FULL_JDK and JDK_KIND can not be used together")
            }
            return TestJdkKind.FULL_JDK
        }

        return when (jdkKinds.size) {
            0 -> TestJdkKind.MOCK_JDK
            1 -> jdkKinds.single()
            else -> error("Too many jdk kinds passed: ${jdkKinds.joinToArrayString()}")
        }
    }

    private fun extractConfigurationKind(registeredDirectives: RegisteredDirectives): ConfigurationKind {
        val withRuntime = JvmEnvironmentConfigurationDirectives.withRuntime in registeredDirectives
        val withReflect = JvmEnvironmentConfigurationDirectives.withReflect in registeredDirectives
        val noRuntime = JvmEnvironmentConfigurationDirectives.noRuntime in registeredDirectives
        if (noRuntime && withRuntime) {
            error("NO_RUNTIME and WITH_RUNTIME can not be used together")
        }
        if (withReflect && !withRuntime) {
            error("WITH_REFLECT may be used only with WITH_RUNTIME")
        }
        return when {
            withRuntime && withReflect -> ConfigurationKind.ALL
            withRuntime -> ConfigurationKind.NO_KOTLIN_REFLECT
            noRuntime -> ConfigurationKind.JDK_NO_RUNTIME
            else -> ConfigurationKind.JDK_ONLY
        }
    }

    private fun CompilerConfiguration.registerModuleDependencies(module: TestModule) {
        val dependencyProvider = testServices.dependencyProvider
        val modulesFromDependencies = module.dependencies
            .filter { it.kind == DependencyKind.Binary }
            .map { dependencyProvider.getTestModule(it.moduleName) }
            .takeIf { it.isNotEmpty() }
            ?: return
        val jarManager = testServices.compiledJarManager
        val dependenciesClassPath = modulesFromDependencies.map { jarManager.getCompiledJarForModule(it) }
        addJvmClasspathRoots(dependenciesClassPath)
    }
}

object JvmEnvironmentConfigurationDirectives : SimpleDirectivesContainer() {
    val jvmTarget = enumDirective<JvmTarget>("JVM_TARGET", "Target bytecode version")

    val jdkKind = enumDirective<TestJdkKind>("JDK_KIND", "JDK used in tests")
    val fullJdk = directive("FULL_JDK", "Add full java standard library to classpath")

    val withRuntime = directive("WITH_RUNTIME", "Add Kotlin runtime to classpath")
    val withReflect = directive("WITH_REFLECT", "Add Kotlin reflect to classpath")
    val noRuntime = directive("NO_RUNTIME", "Don't add any runtime libs to classpath")
}
