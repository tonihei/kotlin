/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.directives

sealed class DirectivesContainer {
    abstract operator fun get(name: String): Directive?
}

abstract class SimpleDirectivesContainer : DirectivesContainer() {
    object Empty : SimpleDirectivesContainer()

    private val registeredDirectives: MutableMap<String, Directive> = mutableMapOf()

    override operator fun get(name: String): Directive? = registeredDirectives[name]

    protected fun directive(name: String, description: String): SimpleDirective {
        return SimpleDirective(name, description).also(this::registerDirective)
    }

    protected fun valueDirective(name: String, description: String): StringValueDirective {
        return StringValueDirective(name, description).also(this::registerDirective)
    }

    protected inline fun <reified T : Enum<T>> enumDirective(name: String, description: String): EnumValueDirective<T> {
        return EnumValueDirective<T>(name, description, enumValues()).also(this::registerDirective)
    }

    protected fun registerDirective(directive: Directive) {
        registeredDirectives[directive.name] = directive
    }

    override fun toString(): String {
        return buildString {
            appendLine("Directive container:")
            for (directive in registeredDirectives.values) {
                append("  ")
                appendLine(directive)
            }
        }
    }
}

class ComposableDirectivesContainer(private vararg val containers: DirectivesContainer) : DirectivesContainer() {
    override fun get(name: String): Directive? {
        for (container in containers) {
            container[name]?.let { return it }
        }
        return null
    }
}
