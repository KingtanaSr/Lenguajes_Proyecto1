package org.example

class Define {
    protected var mapa = mutableMapOf<String, Any>()

    fun define(
        a: String,
        b: Any,
    ) {
        mapa[a] = b
    }

    fun asignada(a: String): Any? = mapa[a]
}
