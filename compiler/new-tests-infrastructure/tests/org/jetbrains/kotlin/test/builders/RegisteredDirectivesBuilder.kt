/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.builders

import org.jetbrains.kotlin.test.directives.*

class RegisteredDirectivesBuilder {
    private val simpleDirectives: MutableList<SimpleDirective> = mutableListOf()
    private val stringValueDirectives: MutableMap<StringValueDirective, List<String>> = mutableMapOf()
    private val enumValueDirectives: MutableMap<EnumValueDirective<*>, List<Enum<*>>> = mutableMapOf()

    operator fun SimpleDirective.unaryPlus() {
        simpleDirectives += this
    }

    infix fun StringValueDirective.with(value: String) {
        with(listOf(value))
    }

    infix fun StringValueDirective.with(values: List<String>) {
        val alreadyRegistered = stringValueDirectives.put(this, values)
        if (alreadyRegistered != null) {
            error("Default values for $this directive already registered")
        }
    }

    infix fun <T : Enum<T>> EnumValueDirective<T>.with(value: T) {
        with(listOf(value))
    }

    infix fun <T : Enum<T>> EnumValueDirective<T>.with(values: List<T>) {
        val alreadyRegistered = enumValueDirectives.put(this, values)
        if (alreadyRegistered != null) {
            error("Default values for $this directive already registered")
        }
    }

    fun build(): RegisteredDirectives {
        return RegisteredDirectivesImpl(simpleDirectives, stringValueDirectives, enumValueDirectives)
    }
}
