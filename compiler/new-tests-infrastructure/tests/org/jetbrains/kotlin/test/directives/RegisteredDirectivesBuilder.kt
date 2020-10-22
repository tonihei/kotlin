/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.directives

import org.jetbrains.kotlin.test.services.Assertions

class RegisteredDirectivesBuilder(private val container: DirectivesContainer, private val assertions: Assertions) {
    companion object {
        private val DIRECTIVE_PATTERN = Regex("""^//\s*[!]?([A-Z_]+)(:[ \t]*(.*))?$""")
        private val SPACES_PATTERN = Regex("""[,]?[ \t]+""")
        private const val NAME_GROUP = 1
        private const val VALUES_GROUP = 3

        fun parseDirective(line: String): RawDirective? {
            val result = DIRECTIVE_PATTERN.matchEntire(line)?.groupValues ?: return null
            val name = result.getOrNull(NAME_GROUP) ?: return null
            val values = result.getOrNull(VALUES_GROUP)?.split(SPACES_PATTERN)?.filter { it.isNotBlank() }?.takeIf { it.isNotEmpty() }
            return RawDirective(name, values)
        }
    }

    data class RawDirective(val name: String, val values: List<String>?)
    data class ParsedDirective(val directive: Directive, val values: List<*>)

    private val simpleDirectives = mutableListOf<SimpleDirective>()
    private val stringValueDirectives = mutableMapOf<StringValueDirective, MutableList<String>>()
    private val enumValueDirectives = mutableMapOf<EnumValueDirective<*>, MutableList<Enum<*>>>()

    /**
     * returns true means that line contain directive
     */
    fun parse(line: String): Boolean {
        val rawDirective = parseDirective(line) ?: return false
        val parsedDirective = convertToRegisteredDirective(rawDirective) ?: return false
        addParsedDirective(parsedDirective)
        return true
    }

    fun addParsedDirective(parsedDirective: ParsedDirective) {
        val (directive, values) = parsedDirective
        when (directive) {
            is SimpleDirective -> simpleDirectives += directive
            is StringValueDirective -> {
                val list = stringValueDirectives.getOrPut(directive, ::mutableListOf)
                @Suppress("UNCHECKED_CAST")
                list += values as List<String>
            }
            is EnumValueDirective<*> -> {
                val list = enumValueDirectives.getOrPut(directive, ::mutableListOf)
                @Suppress("UNCHECKED_CAST")
                list += values as List<Enum<*>>
            }
        }
    }

    fun convertToRegisteredDirective(rawDirective: RawDirective): ParsedDirective? {
        val (name, rawValues) = rawDirective
        val directive = container[name] ?: return null

        val values: List<*> = when (directive) {
            is SimpleDirective -> {
                if (rawValues != null) {
                    assertions.fail {
                        "Directive $directive should have no arguments, but ${rawValues.joinToString(", ")} are passed"
                    }
                }
                emptyList<Any?>()
            }

            is StringValueDirective -> {
                rawValues ?: emptyList<Any?>()
            }

            is EnumValueDirective<*> -> {
                if (rawValues == null) {
                    assertions.fail {
                        "Directive $directive must have at least one value"
                    }
                }
                rawValues.map {
                    directive.extractValue(it) ?: assertions.fail {
                        "$it is not valid value for $directive. Acceptable values: ${directive.possibleValues.joinToArrayString()}"
                    }
                }
            }
        }
        return ParsedDirective(directive, values)
    }

    private fun <T : Enum<T>> EnumValueDirective<T>.extractValue(name: String): T? {
        return possibleValues.firstOrNull { it.name == name }
    }

    fun build(): RegisteredDirectives {
        return RegisteredDirectivesImpl(simpleDirectives, stringValueDirectives, enumValueDirectives)
    }
}
