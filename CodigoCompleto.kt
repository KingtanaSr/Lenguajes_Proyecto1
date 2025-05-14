// Representa expresiones en notación S (en formato Racket)
sealed class Expr {
    data class Atom(val value: String) : Expr()
    data class ListExpr(val elements: List<Expr>) : Expr()
}

// Se encargar de tokenizar y parsear strings en objetos Expr
class SExprParser {
    fun parse(input: String): List<Expr> { // devuelve una lista de Exprs (expresiones).
        val tokens = tokenize(input).toMutableList()
        val exprs = mutableListOf<Expr>()
        while (tokens.isNotEmpty()) {
            exprs.add(readFromTokens(tokens))
        }
        return exprs
    }
	
    // divide el string en tokens manejando paréntesis.
    private fun tokenize(str: String): List<String> {
        return str.replace("(", " ( ")
            .replace(")", " ) ")
            .split(Regex("\\s+"))
            .filter { it.isNotEmpty() }
    }

    // convierte los tokens en objetos Expr de forma recursiva.
    private fun readFromTokens(tokens: MutableList<String>): Expr {
        if (tokens.isEmpty()) error("Unexpected EOF")
        val token = tokens.removeAt(0)
        return when (token) {
            "(" -> {
                val exprs = mutableListOf<Expr>()
                while (true) {
                    if (tokens.isEmpty()) error("Unexpected EOF")
                    if (tokens[0] == ")") {
                        tokens.removeAt(0)
                        break
                    }
                    exprs.add(readFromTokens(tokens))
                }
                Expr.ListExpr(exprs)
            }
            ")" -> error("Unexpected ')'")
            else -> Expr.Atom(token)
        }
    }
}

/* 	Las 2 clases anteriores juntas se encargan de hacer esto:
	Ejemplo:
	Entrada: (define x 42)
	Salida: ListExpr([Atom("define"), Atom("x"), Atom("42")])
*/

// 
sealed class Value {
 	data class Number(val value: Int) : Value() {
        override fun toString() = value.toString()
    }
    data class Bool(val value: Boolean) : Value() {
        override fun toString() = if (value) "#t" else "#f"
    }
    data class Symbol(val name: String) : Value() {
        override fun toString() = name
    }
    data class Function(val params: List<String>, val body: Expr, val env: Environment) : Value() {
        override fun toString() = "#<procedure>"
    }
    data class NativeFunction(val name: String, val fn: (List<Value>) -> Value) : Value() {
        override fun toString() = "#<procedure>"
    }
    object Undefined : Value() {
        override fun toString() = ""
    }

    override fun toString(): String = when (this) {
        is Number -> value.toString()
        is Bool -> if (value) "#t" else "#f"
        is Symbol -> name
        is Function, is NativeFunction, is BuiltinFunction -> "#<procedure>"
        Undefined -> ""
    }
}

abstract class BuiltinFunction : Value() {
    abstract fun call(args: List<Value>): Value
}

/* Es una tabla de símbolos con alcance léxico.
define(): define un nuevo nombre.
lookup(): busca un símbolo, recursivamente en entornos padres.
extend(): crea un nuevo entorno anidado (para funciones).
*/
class Environment(private val outer: Environment? = null) {
    private val values = mutableMapOf<String, Value>()

    fun define(name: String, value: Value) {
        values[name] = value
    }

    fun lookup(name: String): Value {
        return values[name] ?: outer?.lookup(name)
            ?: error("Unbound identifier: $name")
    }

    fun extend(): Environment = Environment(this)
}

/* Evalúa expresiones:
Si es Atom: puede ser un número, booleano o símbolo (variable).
Si es ListExpr: evalúa según el primer elemento. 
*/
class Evaluator {
    fun eval(expr: Expr, env: Environment): Value = when (expr) {
        is Expr.Atom -> parseAtom(expr.value, env)
        is Expr.ListExpr -> evalList(expr.elements, env)
    }

    private fun parseAtom(token: String, env: Environment): Value =
        when (token) {
            "#t" -> Value.Bool(true)
            "#f" -> Value.Bool(false)
            else -> token.toIntOrNull()?.let { Value.Number(it) }
                ?: env.lookup(token)
        }

    private fun evalList(elements: List<Expr>, env: Environment): Value {
        if (elements.isEmpty()) error("Empty expression")
        val first = elements.first()
        val args = elements.drop(1)

        if (first is Expr.Atom) {
            return when (first.value) {
                "define" -> { //define una variable.
                    val name = (args[0] as Expr.Atom).value
                    val value = eval(args[1], env)
                    env.define(name, value)
                    Value.Symbol(name)
                }
                "lambda" -> { //crea una función.
                    val params = (args[0] as Expr.ListExpr).elements.map { (it as Expr.Atom).value }
                    val body = args[1]
                    Value.Function(params, body, env)
                }
                "if" -> { //condicional.
                    val condition = eval(args[0], env)
                    val branch = if ((condition as Value.Bool).value) args[1] else args[2]
                    eval(branch, env)
                }
                else -> { // se asume una llamada a función, se evalúan los argumentos y se llama.
                    val func = eval(first, env)
                    val argVals = args.map { eval(it, env) }
                    applyFunction(func, argVals)
                }
            }
        } else {
            val func = eval(first, env)
            val argVals = args.map { eval(it, env) }
            return applyFunction(func, argVals)
        }
    }
	
    /*
    Si es una Function (lambda): crea un nuevo entorno, asigna los argumentos, y evalúa el cuerpo.
    Si es BuiltinFunction: llama al método call.
	Error si no es función.
    */
    private fun applyFunction(func: Value, args: List<Value>): Value {
        return when (func) {
            is Value.Function -> {
                val localEnv = func.env.extend()
                func.params.zip(args).forEach { (param, arg) -> localEnv.define(param, arg) }
                eval(func.body, localEnv)
            }
            is BuiltinFunction -> func.call(args)
            else -> error("Not a function: $func")
        }
    }
}

/* Define funciones nativas de Racket (Funciones incorporadas):
Aritméticas: +, -, *, /, %
Lógicas: and, or, not, equal?
Comparaciones: <, >, <=, >=
*/
object Builtins {
    val builtins: Map<String, BuiltinFunction> = mapOf(
        "+" to object : BuiltinFunction() {
            override fun call(args: List<Value>) =
                Value.Number(args.sumOf { (it as Value.Number).value })
        },
        "-" to object : BuiltinFunction() {
            override fun call(args: List<Value>) =
                Value.Number(args.map { (it as Value.Number).value }.reduce(Int::minus))
        },
        "*" to object : BuiltinFunction() {
            override fun call(args: List<Value>) =
                Value.Number(args.map { (it as Value.Number).value }.reduce(Int::times))
        },
        "/" to object : BuiltinFunction() {
            override fun call(args: List<Value>) =
                Value.Number(args.map { (it as Value.Number).value }.reduce(Int::div))
        },
        "%" to object : BuiltinFunction() {
            override fun call(args: List<Value>) =
                Value.Number(args.map { (it as Value.Number).value }.reduce(Int::rem))
        },
        "equal?" to object : BuiltinFunction() {
            override fun call(args: List<Value>) = Value.Bool(args[0] == args[1])
        },
        "not" to object : BuiltinFunction() {
            override fun call(args: List<Value>) =
                Value.Bool(!(args[0] as Value.Bool).value)
        },
        "and" to object : BuiltinFunction() {
            override fun call(args: List<Value>) =
                Value.Bool(args.all { (it as Value.Bool).value })
        },
        "or" to object : BuiltinFunction() {
            override fun call(args: List<Value>) =
                Value.Bool(args.any { (it as Value.Bool).value })
        },
        "<" to object : BuiltinFunction() {
            override fun call(args: List<Value>) =
                Value.Bool((args[0] as Value.Number).value < (args[1] as Value.Number).value)
        },
        "<=" to object : BuiltinFunction() {
            override fun call(args: List<Value>) =
                Value.Bool((args[0] as Value.Number).value <= (args[1] as Value.Number).value)
        },
        ">" to object : BuiltinFunction() {
            override fun call(args: List<Value>) =
                Value.Bool((args[0] as Value.Number).value > (args[1] as Value.Number).value)
        },
        ">=" to object : BuiltinFunction() {
            override fun call(args: List<Value>) =
                Value.Bool((args[0] as Value.Number).value >= (args[1] as Value.Number).value)
        }
    )
}

/* Se encarga de unir todo:
	Usa parser + evaluator + entorno global con funciones nativas.
	run(): toma código como string, lo parsea y evalúa en orden.
 */
class Interpreter {
    private val parser = SExprParser()
    private val evaluator = Evaluator()
    private val globalEnv = Environment().apply {
        Builtins.builtins.forEach { (name, fn) -> define(name, fn) }
    }

    fun run(code: String): String {
        val exprs = parser.parse(code)
        var result: Value = Value.Undefined
        for (expr in exprs) {
            result = evaluator.eval(expr, globalEnv)
        }
        return result.toString()
    }
}


fun main() {
    // given
  	val interpreter = Interpreter()
    val code = """
     	(define CERO 0)
       	CERO
   	""".trimIndent()

   	// when
  	val result = interpreter.run(code) //Debería imprimir 0
    println(result)
}
