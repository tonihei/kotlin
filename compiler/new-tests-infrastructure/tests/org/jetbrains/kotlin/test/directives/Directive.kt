/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.directives

// --------------------------- Directive declaration ---------------------------

sealed class Directive(val name: String, val description: String) {
    override fun toString(): String {
        return name
    }
}

class SimpleDirective(
    name: String,
    description: String
) : Directive(name, description)

class StringValueDirective(
    name: String,
    description: String
) : Directive(name, description)

class EnumValueDirective<T : Enum<T>>(
    name: String,
    description: String,
    val possibleValues: Array<T>,
) : Directive(name, description)

// --------------------------- Registered directive ---------------------------

class RegisteredDirectives(
    private val simpleDirectives: List<SimpleDirective>,
    private val stringValueDirectives: Map<StringValueDirective, List<String>>,
    private val enumValueDirectives: Map<EnumValueDirective<*>, List<Enum<*>>>
) {
    companion object {
        val Empty = RegisteredDirectives(emptyList(), emptyMap(), emptyMap())
    }

    operator fun contains(directive: SimpleDirective): Boolean {
        return directive in simpleDirectives
    }

    operator fun get(directive: StringValueDirective): List<String> {
        return stringValueDirectives[directive] ?: emptyList()
    }

    operator fun <T : Enum<T>> get(directive: EnumValueDirective<T>): List<T> {
        @Suppress("UNCHECKED_CAST")
        return enumValueDirectives[directive] as List<T>? ?: emptyList()
    }

    override fun toString(): String {
        return buildString {
            simpleDirectives.forEach { appendLine("  $it") }
            stringValueDirectives.forEach { (d, v) -> appendLine("  $d: ${v.toArrayString()}") }
            enumValueDirectives.forEach { (d, v) -> appendLine("  $d: ${v.toArrayString()}") }
        }
    }
}
