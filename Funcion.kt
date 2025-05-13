package org.example

interface Funcion {
    fun evaluar(
        a: Int,
        b: Int,
        operacion: String,
    ): Any
}

class Operador : Funcion {
    override fun evaluar(
        a: Int,
        b: Int,
        operacion: String,
    ): Int =
        when (operacion) {
            "+" -> a + b
            "-" -> a - b
            "*" -> a * b
            "/" -> a / b
            "%" -> a % b
            else -> throw IllegalArgumentException("Operación no válida: $operacion")
        }
}

class Comparador : Funcion {
    override fun evaluar(
        a: Int,
        b: Int,
        operacion: String,
    ): Boolean =
        when (operacion) {
            "<" -> a < b
            ">" -> a > b
            "=" -> a == b
            ">=" -> a >= b
            "<=" -> a <= b
            "equal?" -> a == b
            else -> false
        }
}
