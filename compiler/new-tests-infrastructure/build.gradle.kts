plugins {
    kotlin("jvm")
    id("jps-compatible")
}

dependencies {
    testImplementation(project(":compiler:fir:entrypoint"))

    testImplementation(platform("org.junit:junit-bom:5.7.0"))
    testImplementation("org.junit.jupiter:junit-jupiter")

    testImplementation(intellijDep()) {
        includeJars("idea_rt", rootProject = rootProject)
        isTransitive = false
    }

    testImplementation(intellijDep()) {
        includeJars(
            "commons-lang3",
            "commons-io",
            rootProject = rootProject
        )
    }

    // This is needed only for using FileComparisonFailure, which relies on JUnit 3 classes
    testRuntimeOnly(commonDep("junit:junit"))
}

sourceSets {
    "main" { none() }
    "test" { projectDefault() }
}

projectTest(parallel = true) {
    dependsOn(":dist")
    workingDir = rootDir
    jvmArgs!!.removeIf { it.contains("-Xmx") }
    maxHeapSize = "3g"
}

testsJar()
