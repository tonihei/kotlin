/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.frontend.api.fir.symbols

import org.jetbrains.kotlin.descriptors.annotations.AnnotationUseSiteTarget
import org.jetbrains.kotlin.fir.declarations.FirAnnotatedDeclaration
import org.jetbrains.kotlin.fir.expressions.FirAnnotationCall
import org.jetbrains.kotlin.fir.expressions.FirConstExpression
import org.jetbrains.kotlin.fir.expressions.FirNamedArgumentExpression
import org.jetbrains.kotlin.fir.psi
import org.jetbrains.kotlin.fir.types.ConeClassLikeType
import org.jetbrains.kotlin.fir.types.coneType
import org.jetbrains.kotlin.idea.frontend.api.symbols.markers.KtAnnotationCall
import org.jetbrains.kotlin.idea.frontend.api.symbols.markers.KtAnnotationCallArgument
import org.jetbrains.kotlin.idea.frontend.api.symbols.markers.KtNamedAnnotationCallArgument
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.psi.KtCallElement

class KtFirAnnotationCall(
    override val classId: ClassId?,
    override val useSiteTarget: AnnotationUseSiteTarget?,
    override val psi: KtCallElement?,
    override val arguments: List<KtAnnotationCallArgument>
) : KtAnnotationCall() {
    companion object {

        private fun convertFrom(annotationCall: FirAnnotationCall) =
            KtFirAnnotationCall(
                (annotationCall.annotationTypeRef.coneType as? ConeClassLikeType)?.lookupTag?.classId,
                annotationCall.useSiteTarget,
                annotationCall.psi as? KtCallElement,
                annotationCall.argumentList.arguments.mapNotNull {
                    when (it) {
                        is FirConstExpression<*> -> KtAnnotationCallArgument(it.value)
                        is FirNamedArgumentExpression -> {
                            val expression = it.expression
                            require(expression is FirConstExpression<*>)
                            KtNamedAnnotationCallArgument(it.name.asString(), expression.value)
                        }
                        else -> null //TODO
                    }
                }
            )

        fun convertFrom(declaration: FirAnnotatedDeclaration) = declaration.annotations.map {
            it.argumentList.arguments
            convertFrom(it)
        }
    }
}