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

abstract class RegisteredDirectives {
    companion object {
        val Empty = RegisteredDirectivesImpl(emptyList(), emptyMap(), emptyMap())
    }

    abstract operator fun contains(directive: SimpleDirective): Boolean
    abstract operator fun get(directive: StringValueDirective): List<String>
    abstract operator fun <T : Enum<T>> get(directive: EnumValueDirective<T>): List<T>

    abstract fun isEmpty(): Boolean
}

class RegisteredDirectivesImpl(
    private val simpleDirectives: List<SimpleDirective>,
    private val stringValueDirectives: Map<StringValueDirective, List<String>>,
    private val enumValueDirectives: Map<EnumValueDirective<*>, List<Enum<*>>>
) : RegisteredDirectives() {
    override operator fun contains(directive: SimpleDirective): Boolean {
        return directive in simpleDirectives
    }

    override operator fun get(directive: StringValueDirective): List<String> {
        return stringValueDirectives[directive] ?: emptyList()
    }

    override operator fun <T : Enum<T>> get(directive: EnumValueDirective<T>): List<T> {
        @Suppress("UNCHECKED_CAST")
        return enumValueDirectives[directive] as List<T>? ?: emptyList()
    }

    override fun isEmpty(): Boolean {
        return simpleDirectives.isEmpty() && stringValueDirectives.isEmpty() && enumValueDirectives.isEmpty()
    }

    override fun toString(): String {
        return buildString {
            simpleDirectives.forEach { appendLine("  $it") }
            stringValueDirectives.forEach { (d, v) -> appendLine("  $d: ${v.joinToArrayString()}") }
            enumValueDirectives.forEach { (d, v) -> appendLine("  $d: ${v.joinToArrayString()}") }
        }
    }
}

class ComposedRegisteredDirectives private constructor(
    private val containers: List<RegisteredDirectives>
) : RegisteredDirectives() {
    companion object {
        operator fun invoke(vararg containers: RegisteredDirectives): RegisteredDirectives {
            val notEmptyContainers = containers.filterNot { it.isEmpty() }
            return when (notEmptyContainers.size) {
                0 -> Empty
                1 -> notEmptyContainers.single()
                else -> ComposedRegisteredDirectives(notEmptyContainers)
            }
        }
    }

    override fun contains(directive: SimpleDirective): Boolean {
        return containers.any { directive in it }
    }

    override fun get(directive: StringValueDirective): List<String> {
        for (container in containers) {
            container[directive].takeIf { it.isNotEmpty() }?.let { return it }
        }
        return emptyList()
    }

    override fun <T : Enum<T>> get(directive: EnumValueDirective<T>): List<T> {
        for (container in containers) {
            container[directive].takeIf { it.isNotEmpty() }?.let { return it }
        }
        return emptyList()
    }

    override fun isEmpty(): Boolean {
        return containers.all { it.isEmpty() }
    }
}
