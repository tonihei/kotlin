// FULL_JDK

fun box(): String {
    val list = java.util.ArrayList<String>()
    list.add("OK")
    return list.get(0)
}
