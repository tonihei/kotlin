/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.components

import org.jetbrains.kotlin.fir.utils.ArrayMapAccessor
import org.jetbrains.kotlin.fir.utils.ComponentArrayOwner
import org.jetbrains.kotlin.fir.utils.TypeRegistry
import kotlin.reflect.KClass

interface TestService

data class ServiceRegistrationData(
    val kClass: KClass<out TestService>,
    val serviceConstructor: (ConfigurationComponents, TestServices) -> TestService
)

inline fun <reified T : TestService> serviceRegistrationData(
    noinline serviceConstructor: (ConfigurationComponents, TestServices) -> T
): ServiceRegistrationData {
    return ServiceRegistrationData(T::class, serviceConstructor)
}

class TestServices(val configurationComponents: ConfigurationComponents) : ComponentArrayOwner<TestService, TestService>(){
    override val typeRegistry: TypeRegistry<TestService, TestService>
        get() = Companion

    companion object : TypeRegistry<TestService, TestService>() {
        inline fun <reified T : TestService> testServiceAccessor(): ArrayMapAccessor<TestService, TestService, T> {
            return generateAccessor(T::class)
        }
    }

    fun register(data: ServiceRegistrationData) {
        registerComponent(data.kClass, data.serviceConstructor(configurationComponents, this))
    }
}
