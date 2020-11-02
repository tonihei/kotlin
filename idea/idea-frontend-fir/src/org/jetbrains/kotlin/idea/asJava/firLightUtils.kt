/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.idea.asJava

import com.intellij.psi.*
import com.intellij.psi.impl.cache.TypeInfo
import com.intellij.psi.impl.compiled.ClsTypeElementImpl
import com.intellij.psi.impl.compiled.SignatureParsing
import com.intellij.psi.impl.compiled.StubBuildingVisitor
import org.jetbrains.annotations.NotNull
import org.jetbrains.annotations.Nullable
import org.jetbrains.kotlin.asJava.elements.KtLightElement
import org.jetbrains.kotlin.asJava.elements.KtLightMember
import org.jetbrains.kotlin.builtins.StandardNames
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.descriptors.Visibilities
import org.jetbrains.kotlin.descriptors.annotations.AnnotationUseSiteTarget
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.backend.jvm.jvmTypeMapper
import org.jetbrains.kotlin.fir.declarations.*
import org.jetbrains.kotlin.fir.isByte
import org.jetbrains.kotlin.fir.isInt
import org.jetbrains.kotlin.fir.isPrimitiveNumberOrUnsignedNumberType
import org.jetbrains.kotlin.fir.resolve.calls.isUnit
import org.jetbrains.kotlin.fir.symbols.StandardClassIds
import org.jetbrains.kotlin.fir.types.*
import org.jetbrains.kotlin.fir.types.impl.ConeClassLikeTypeImpl
import org.jetbrains.kotlin.fir.types.impl.ConeTypeParameterTypeImpl
import org.jetbrains.kotlin.idea.frontend.api.fir.symbols.KtFirClassOrObjectSymbol
import org.jetbrains.kotlin.idea.frontend.api.fir.symbols.KtFirSymbol
import org.jetbrains.kotlin.idea.frontend.api.fir.types.KtFirClassType
import org.jetbrains.kotlin.idea.frontend.api.fir.types.KtFirType
import org.jetbrains.kotlin.idea.frontend.api.symbols.*
import org.jetbrains.kotlin.idea.frontend.api.symbols.markers.*
import org.jetbrains.kotlin.idea.frontend.api.types.*
import org.jetbrains.kotlin.load.kotlin.TypeMappingMode
import org.jetbrains.kotlin.name.SpecialNames
import org.jetbrains.kotlin.types.model.SimpleTypeMarker
import java.text.StringCharacterIterator

internal fun <L : Any> L.invalidAccess(): Nothing =
    error("Cls delegate shouldn't be accessed for fir light classes! Qualified name: ${javaClass.name}")


private fun PsiElement.nonExistentType() = JavaPsiFacade.getElementFactory(project)
    .createTypeFromText("error.NonExistentClass", this)

internal fun KtTypedSymbol.asPsiType(parent: PsiElement, phase: FirResolvePhase): PsiType =
    type.asPsiType(this, parent, phase)

internal fun KtType.asPsiType(
    context: KtSymbol,
    parent: PsiElement,
    phase: FirResolvePhase
): PsiType {
    if (isUnit) return PsiType.VOID
    require(this is KtFirType)
    require(context is KtFirSymbol<*>)

    return context.firRef.withFir(phase) {
        coneType.asPsiType(it.session, TypeMappingMode.DEFAULT, parent)
    }
}

internal fun KtClassOrObjectSymbol.typeForClassSymbol(psiElement: PsiElement): PsiType {
    require(this is KtFirClassOrObjectSymbol)
    return firRef.withFir(FirResolvePhase.TYPES) { firClass ->
        ConeClassLikeTypeImpl(
            firClass.symbol.toLookupTag(),
            firClass.typeParameters.map { ConeTypeParameterTypeImpl(it.symbol.toLookupTag(), isNullable = false) }.toTypedArray(),
            isNullable = false
        ).asPsiType(firClass.session, TypeMappingMode.DEFAULT, psiElement)
    }
}

private fun ConeKotlinType.asPsiType(
    session: FirSession,
    mode: TypeMappingMode,
    psiContext: PsiElement,
): PsiType {

    if (this is ConeClassErrorType || this !is SimpleTypeMarker) return psiContext.nonExistentType()
    if (this.typeArguments.any { it is ConeClassErrorType }) return psiContext.nonExistentType()
    if (this is ConeClassLikeType) {
        val classId = classId
        if (classId != null && classId.shortClassName.asString() == SpecialNames.ANONYMOUS) return PsiType.NULL
    }

    val canonicalSignature = session.jvmTypeMapper.mapType(this, mode).descriptor
    if (canonicalSignature == "[L<error>;") return psiContext.nonExistentType()
    val signature = StringCharacterIterator(canonicalSignature)
    val javaType = SignatureParsing.parseTypeString(signature, StubBuildingVisitor.GUESSING_MAPPER)
    val typeInfo = TypeInfo.fromString(javaType, false)
    val typeText = TypeInfo.createTypeText(typeInfo) ?: return psiContext.nonExistentType()

    val typeElement = ClsTypeElementImpl(psiContext, typeText, '\u0000')
    return typeElement.type
}

private fun mapSupertype(psiContext: PsiElement, session: FirSession, supertype: ConeKotlinType, kotlinCollectionAsIs: Boolean = false) =
    supertype.asPsiType(
        session,
        if (kotlinCollectionAsIs) TypeMappingMode.SUPER_TYPE_KOTLIN_COLLECTIONS_AS_IS else TypeMappingMode.SUPER_TYPE,
        psiContext
    ) as? PsiClassType

internal fun KtClassType.mapSupertype(
    psiContext: PsiElement,
    kotlinCollectionAsIs: Boolean = false
): PsiClassType? {
    require(this is KtFirClassType)
    val contextSymbol = this.classSymbol
    require(contextSymbol is KtFirSymbol<*>)

    return contextSymbol.firRef.withFir {
        mapSupertype(
            psiContext,
            it.session,
            this.coneType,
            kotlinCollectionAsIs,
        )
    }
}

internal enum class NullabilityType {
    Nullable,
    NotNull,
    Unknown
}

internal val KtType.nullabilityType: NullabilityType
    get() =
        (this as? KtTypeWithNullability)?.let {
            if (it.nullability == KtTypeNullability.NULLABLE) NullabilityType.Nullable else NullabilityType.NotNull
        } ?: NullabilityType.Unknown


internal fun KtAnnotatedSymbol.hasJvmSyntheticAnnotation(): Boolean =
    hasAnnotation("kotlin/jvm/JvmSynthetic", null)

internal fun KtAnnotatedSymbol.getJvmNameFromAnnotation(annotationUseSiteTarget: AnnotationUseSiteTarget? = null): String? {
    val annotation = annotations.firstOrNull {
        val siteTarget = it.useSiteTarget
        (siteTarget == null || siteTarget == annotationUseSiteTarget) && it.classId?.asString() == "kotlin/jvm/JvmName"
    }

    return annotation?.let {
        it.arguments.firstOrNull()?.expression as? String
    }
}

internal fun KtAnnotatedSymbol.hasJvmFieldAnnotation(): Boolean =
    hasAnnotation("kotlin/jvm/JvmField", null)

internal fun KtAnnotatedSymbol.hasJvmOverloadsAnnotation(): Boolean =
    hasAnnotation("kotlin/jvm/JvmOverloads", null)

internal fun KtAnnotatedSymbol.hasJvmStaticAnnotation(): Boolean =
    hasAnnotation("kotlin/jvm/JvmStatic", null)

internal fun KtAnnotatedSymbol.hasInlineOnlyAnnotation(): Boolean =
    hasAnnotation("kotlin/internal/InlineOnly", null)

internal fun KtAnnotatedSymbol.hasAnnotation(classIdString: String, annotationUseSiteTarget: AnnotationUseSiteTarget?): Boolean =
    annotations.any {
        val siteTarget = it.useSiteTarget
        (siteTarget == null || siteTarget == annotationUseSiteTarget) && it.classId?.asString() == classIdString
    }


internal fun KtAnnotatedSymbol.computeAnnotations(
    parent: PsiElement,
    nullability: NullabilityType,
    annotationUseSiteTarget: AnnotationUseSiteTarget?
): List<PsiAnnotation> {

    if (nullability == NullabilityType.Unknown && annotations.isEmpty()) return emptyList()

    val nullabilityAnnotation = when (nullability) {
        NullabilityType.NotNull -> NotNull::class.java
        NullabilityType.Nullable -> Nullable::class.java
        else -> null
    }?.let {
        FirLightSimpleAnnotation(it.name, parent)
    }

    if (annotations.isEmpty()) {
        return if (nullabilityAnnotation != null) listOf(nullabilityAnnotation) else emptyList()
    }

    val result = mutableListOf<PsiAnnotation>()
    for (annotation in annotations) {

        val siteTarget = annotation.useSiteTarget
        if (siteTarget == null || siteTarget == annotationUseSiteTarget) {
            result.add(FirLightAnnotationForAnnotationCall(annotation, parent))
        }
    }

    if (nullabilityAnnotation != null) {
        result.add(nullabilityAnnotation)
    }

    return result
}

internal fun FirMemberDeclaration.computeSimpleModality(): Set<String> {
    require(this !is FirConstructor)

    val modifier = when (modality) {
        Modality.FINAL -> PsiModifier.FINAL
        Modality.ABSTRACT -> PsiModifier.ABSTRACT
        Modality.SEALED -> PsiModifier.ABSTRACT
        else -> null
    }

    return modifier?.let { setOf(it) } ?: emptySet()
}

internal fun KtSymbolWithModality<*>.computeSimpleModality(): String? = when (modality) {
    KtSymbolModality.SEALED -> PsiModifier.ABSTRACT
    KtCommonSymbolModality.FINAL -> PsiModifier.FINAL
    KtCommonSymbolModality.ABSTRACT -> PsiModifier.ABSTRACT
    KtCommonSymbolModality.OPEN -> null
    else -> throw NotImplementedError()
}

internal fun FirMemberDeclaration.computeModalityForMethod(isTopLevel: Boolean): Set<String> {
    require(this !is FirConstructor)

    val simpleModifier = computeSimpleModality()

    val withNative = if (isExternal) simpleModifier + PsiModifier.NATIVE else simpleModifier
    val withTopLevelStatic = if (isTopLevel) withNative + PsiModifier.STATIC else withNative

    return withTopLevelStatic
}

internal fun KtSymbolWithModality<KtCommonSymbolModality>.computeModalityForMethod(isTopLevel: Boolean): Set<String> {
    require(this !is KtClassLikeSymbol)

    val modality = mutableSetOf<String>()

    computeSimpleModality()?.run {
        modality.add(this)
    }

    if (this is KtFunctionSymbol && isExternal) {
        modality.add(PsiModifier.NATIVE)
    }
    if (isTopLevel) {
        modality.add(PsiModifier.STATIC)
    }

    return modality
}

internal fun FirMemberDeclaration.computeVisibility(isTopLevel: Boolean): String {
    return when (this.visibility) {
        // Top-level private class has PACKAGE_LOCAL visibility in Java
        // Nested private class has PRIVATE visibility
        Visibilities.Private -> if (isTopLevel) PsiModifier.PACKAGE_LOCAL else PsiModifier.PRIVATE
        Visibilities.Protected -> PsiModifier.PROTECTED
        else -> PsiModifier.PUBLIC
    }
}

internal fun KtSymbolWithVisibility.computeVisibility(isTopLevel: Boolean): String {
    return when (this.visibility) {
        // Top-level private class has PACKAGE_LOCAL visibility in Java
        // Nested private class has PRIVATE visibility
        KtSymbolVisibility.PRIVATE -> if (isTopLevel) PsiModifier.PACKAGE_LOCAL else PsiModifier.PRIVATE
        KtSymbolVisibility.PROTECTED -> PsiModifier.PROTECTED
        else -> PsiModifier.PUBLIC
    }
}

internal val FirTypeRef?.nullabilityForJava: ConeNullability
    get() = this?.coneTypeSafe?.run { if (isConstKind || isUnit) ConeNullability.UNKNOWN else nullability }
        ?: ConeNullability.UNKNOWN

internal val ConeKotlinType.isConstKind
    get() = (this as? ConeClassLikeType)?.toConstKind() != null

internal val FirTypeRef.coneTypeSafe: ConeKotlinType? get() = coneTypeSafe()

internal fun basicIsEquivalentTo(`this`: PsiElement?, that: PsiElement?): Boolean {
    if (`this` == null || that == null) return false
    if (`this` == that) return true

    if (`this` !is KtLightElement<*, *>) return false
    if (that !is KtLightElement<*, *>) return false
    if (`this`.kotlinOrigin?.isEquivalentTo(that.kotlinOrigin) == true) return true

    val thisMemberOrigin = (`this` as? KtLightMember<*>)?.lightMemberOrigin ?: return false
    if (thisMemberOrigin.isEquivalentTo(that)) return true

    val thatMemberOrigin = (that as? KtLightMember<*>)?.lightMemberOrigin ?: return false
    return thisMemberOrigin.isEquivalentTo(thatMemberOrigin)
}

internal fun KtType.getTypeNullability(context: KtSymbol, phase: FirResolvePhase): NullabilityType {

    if (nullabilityType != NullabilityType.NotNull) return nullabilityType

    if (isUnit) return NullabilityType.NotNull

    require(this is KtFirType)
    require(context is KtFirSymbol<*>)

    val coneType = coneType as? ConeClassLikeType ?: return NullabilityType.NotNull

    //TODO Move to Primitives.kt
    fun ConeClassLikeType.isBoolean(): Boolean = lookupTag.classId == StandardClassIds.Boolean
    fun ConeClassLikeType.isChar(): Boolean = lookupTag.classId == StandardClassIds.Char

    if (!coneType.isPrimitiveNumberOrUnsignedNumberType() &&
        !coneType.isChar() && !coneType.isBoolean()
    ) {
        return nullabilityType
    }

    if (coneType is ConeClassErrorType) return NullabilityType.NotNull
    if (coneType.typeArguments.any { it is ConeClassErrorType }) return NullabilityType.NotNull
    if (coneType.classId?.shortClassName?.asString() == SpecialNames.ANONYMOUS) return NullabilityType.NotNull

    val canonicalSignature = context.firRef.withFir(phase) {
        it.session.jvmTypeMapper.mapType(coneType, TypeMappingMode.DEFAULT).descriptor
    }

    if (canonicalSignature == "[L<error>;") return NullabilityType.NotNull

    val isNotPrimitiveType = canonicalSignature.startsWith("L") || canonicalSignature.startsWith("[")

    return if (isNotPrimitiveType) NullabilityType.NotNull else NullabilityType.Unknown
}