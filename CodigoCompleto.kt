interface IParserExpr {
    fun parsear(entrada: String): List<Expresion>
}

interface IEvaluador {
    fun evaluar(expresion: Expresion, entorno: Entorno): Valor
}

interface IEntorno {
    fun definir(nombre: String, valor: Valor)
    fun buscar(nombre: String): Valor
    fun extender(): Entorno
}

interface IInterprete {
    fun run(codigo: String): String
}

sealed class Expresion {
    data class Atomo(val valor: String) : Expresion()
    data class Lista(val elementos: List<Expresion>) : Expresion()
}

class ParserSExpresion : IParserExpr {
    override fun parsear(entrada: String): List<Expresion> {
        val tokens = tokenizar(entrada).toMutableList()
        val expresiones = mutableListOf<Expresion>()
        while (tokens.isNotEmpty()) {
            expresiones.add(leerDesdeTokens(tokens))
        }
        return expresiones
    }

    private fun tokenizar(str: String): List<String> =
        str.replace("(", " ( ")
            .replace(")", " ) ")
            .split(Regex("\\s+")).filter { it.isNotEmpty() }

    private fun leerDesdeTokens(tokens: MutableList<String>): Expresion {
        if (tokens.isEmpty()) error("Fin inesperado")
        return when (val token = tokens.removeAt(0)) {
            "(" -> {
                val exprs = mutableListOf<Expresion>()
                while (true) {
                    if (tokens.isEmpty()) error("Fin inesperado")
                    if (tokens[0] == ")") {
                        tokens.removeAt(0)
                        break
                    }
                    exprs.add(leerDesdeTokens(tokens))
                }
                Expresion.Lista(exprs)
            }
            ")" -> error("')' inesperado")
            else -> Expresion.Atomo(token)
        }
    }
}

sealed class Valor {
    data class Numero(val valor: Int) : Valor() {
        override fun toString() = valor.toString()
    }
    data class Booleano(val valor: Boolean) : Valor() {
        override fun toString() = if (valor) "#t" else "#f"
    }
    data class Simbolo(val nombre: String) : Valor() {
        override fun toString() = nombre
    }
    data class Funcion(val parametros: List<String>, val cuerpo: Expresion, val entorno: Entorno) : Valor() {
        override fun toString() = "#<procedure>"
    }
    data class FuncionNativa(val nombre: String, val funcion: (List<Valor>) -> Valor) : Valor() {
        override fun toString() = "#<procedure>"
    }
    object Indefinido : Valor() {
        override fun toString() = ""
    }
}

abstract class FuncionIncorporada : Valor() {
    abstract fun llamar(args: List<Valor>): Valor
}

object Incorporadas {
    val funciones: Map<String, FuncionIncorporada> = mapOf(
        "+" to object : FuncionIncorporada() {
            override fun llamar(args: List<Valor>) = Valor.Numero(args.sumOf { (it as Valor.Numero).valor })
        },
        "-" to object : FuncionIncorporada() {
            override fun llamar(args: List<Valor>) =
                Valor.Numero(args.map { (it as Valor.Numero).valor }.reduce(Int::minus))
        },
        "*" to object : FuncionIncorporada() {
            override fun llamar(args: List<Valor>) =
                Valor.Numero(args.map { (it as Valor.Numero).valor }.reduce(Int::times))
        },
        "/" to object : FuncionIncorporada() {
            override fun llamar(args: List<Valor>) =
                Valor.Numero(args.map { (it as Valor.Numero).valor }.reduce(Int::div))
        },
        "%" to object : FuncionIncorporada() {
            override fun llamar(args: List<Valor>) =
                Valor.Numero(args.map { (it as Valor.Numero).valor }.reduce(Int::rem))
        },
        "equal?" to object : FuncionIncorporada() {
            override fun llamar(args: List<Valor>) = Valor.Booleano(args[0] == args[1])
        },
        "not" to object : FuncionIncorporada() {
            override fun llamar(args: List<Valor>) = Valor.Booleano(!(args[0] as Valor.Booleano).valor)
        },
        "and" to object : FuncionIncorporada() {
            override fun llamar(args: List<Valor>) = Valor.Booleano(args.all { (it as Valor.Booleano).valor })
        },
        "or" to object : FuncionIncorporada() {
            override fun llamar(args: List<Valor>) = Valor.Booleano(args.any { (it as Valor.Booleano).valor })
        },
        "<" to object : FuncionIncorporada() {
            override fun llamar(args: List<Valor>) =
                Valor.Booleano((args[0] as Valor.Numero).valor < (args[1] as Valor.Numero).valor)
        },
        "<=" to object : FuncionIncorporada() {
            override fun llamar(args: List<Valor>) =
                Valor.Booleano((args[0] as Valor.Numero).valor <= (args[1] as Valor.Numero).valor)
        },
        ">" to object : FuncionIncorporada() {
            override fun llamar(args: List<Valor>) =
                Valor.Booleano((args[0] as Valor.Numero).valor > (args[1] as Valor.Numero).valor)
        },
        ">=" to object : FuncionIncorporada() {
            override fun llamar(args: List<Valor>) =
                Valor.Booleano((args[0] as Valor.Numero).valor >= (args[1] as Valor.Numero).valor)
        }
    )
}

class Entorno(private val exterior: Entorno? = null) : IEntorno {
    private val valores = mutableMapOf<String, Valor>()

    override fun definir(nombre: String, valor: Valor) {
        valores[nombre] = valor
    }

    override fun buscar(nombre: String): Valor {
        return valores[nombre] ?: exterior?.buscar(nombre)
            ?: error("Identificador no definido: $nombre")
    }

    override fun extender(): Entorno = Entorno(this)
}

class Evaluador : IEvaluador {
    override fun evaluar(expresion: Expresion, entorno: Entorno): Valor = when (expresion) {
        is Expresion.Atomo -> interpretarAtomo(expresion.valor, entorno)
        is Expresion.Lista -> evaluarLista(expresion.elementos, entorno)
    }

    private fun interpretarAtomo(token: String, entorno: Entorno): Valor =
        when (token) {
            "#t" -> Valor.Booleano(true)
            "#f" -> Valor.Booleano(false)
            else -> token.toIntOrNull()?.let { Valor.Numero(it) } ?: entorno.buscar(token)
        }

    private fun evaluarLista(elementos: List<Expresion>, entorno: Entorno): Valor {
        if (elementos.isEmpty()) error("Expresión vacía")
        val primero = elementos.first()
        val argumentos = elementos.drop(1)

        if (primero is Expresion.Atomo) {
            return when (primero.valor) {
                "define" -> {
                    val nombre = (argumentos[0] as Expresion.Atomo).valor
                    val valor = evaluar(argumentos[1], entorno)
                    entorno.definir(nombre, valor)
                    Valor.Simbolo(nombre)
                }
                "lambda" -> {
                    val parametros = (argumentos[0] as Expresion.Lista).elementos.map { (it as Expresion.Atomo).valor }
                    val cuerpo = argumentos[1]
                    Valor.Funcion(parametros, cuerpo, entorno)
                }
                "if" -> {
                    val condicion = evaluar(argumentos[0], entorno)
                    val rama = if ((condicion as Valor.Booleano).valor) argumentos[1] else argumentos[2]
                    evaluar(rama, entorno)
                }
                else -> {
                    val funcion = evaluar(primero, entorno)
                    val valoresArgs = argumentos.map { evaluar(it, entorno) }
                    aplicarFuncion(funcion, valoresArgs)
                }
            }
        } else {
            val funcion = evaluar(primero, entorno)
            val valoresArgs = argumentos.map { evaluar(it, entorno) }
            return aplicarFuncion(funcion, valoresArgs)
        }
    }

    private fun aplicarFuncion(funcion: Valor, argumentos: List<Valor>): Valor {
        return when (funcion) {
            is Valor.Funcion -> {
                val entornoLocal = funcion.entorno.extender()
                funcion.parametros.zip(argumentos).forEach { (param, arg) -> entornoLocal.definir(param, arg) }
                evaluar(funcion.cuerpo, entornoLocal)
            }
            is FuncionIncorporada -> funcion.llamar(argumentos)
            else -> error("No es una función: $funcion")
        }
    }
}

class Interpreter : IInterprete {
    private val parser: IParserExpr = ParserSExpresion()
    private val evaluador: IEvaluador = Evaluador()
    private val entornoGlobal: Entorno = Entorno().apply {
        Incorporadas.funciones.forEach { (nombre, fn) -> definir(nombre, fn) }
    }

    override fun run(codigo: String): String {
        val expresiones = parser.parsear(codigo)
        var resultado: Valor = Valor.Indefinido
        for (expresion in expresiones) {
            resultado = evaluador.evaluar(expresion, entornoGlobal)
        }
        return resultado.toString()
    }
}

fun main() {
    val interpreter = Interpreter()
    val codigo = """
        (define fib (lambda (n) (if (or (equal? n 0) (equal? n 1)) 1 (+ (fib (- n 1)) (fib (- n 2))))) )
        (fib 5)
    """.trimIndent()

    println(interpreter.run(codigo)) // Debería imprimir: 8
}
