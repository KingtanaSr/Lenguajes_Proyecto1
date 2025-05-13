package org.example

interface Tokenizador {
    fun tokenizar(codigo: String): List<String>
}

class TokenizadorSimple : Tokenizador {
    override fun tokenizar(codigo: String): List<String> =
        codigo
            .replace("(", " ( ")
            .replace(")", " ) ")
            .trim()
            .split(Regex("\\s+"))
}
