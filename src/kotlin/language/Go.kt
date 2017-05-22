package language

import java.util.*
import clojure.lang.*
import kotlin.collections.*


val String.keyword: Keyword
    get() = Keyword.find(this)

val String.symbol: Symbol
    get() = Symbol.create(this)

// golang 语法
fun Render.comment(msg: Any) = +"// $msg"
fun Render.ns(name: Any) = +"package ${name.toString()}\n"
fun Render.import(pkgs: Iterable<Any>) = "import".paren {
        for (p in pkgs) {
            +"\"$p\""
        }
    }

fun Render.switch(target: Any, init: Render.() -> Unit) = "switch $target".brace(init)
fun Render.func(sig: String, init: Render.() -> Unit) = "func $sig".brace(init)

val Any.constructor : Any
    get() = when (this) {
        is IPersistentList -> seq().first()
        else -> this
    }

val Any.tag : Any
    get() = when (this) {
        is IPersistentList -> "${seq().first()}_TAG"
        else -> this
    }

fun Any.case() = if (this.toString() == ":else") {
        "default:"
    }
    else {
        "case $this:"
    }

fun Render.match(emit: IFn, value: Any, branchs: Iterable<Any>) {
    "switch $value".brace {
        val it = branchs.iterator()
        while (it.hasNext()) {
            val condition = it.next()
            val action = it.next()

            +condition.case()
            +"    ${emit.invoke(action)}"
        }
    }
}

data class Golang(val emit: IFn, val emitType: IFn, val goImport: IFn) {
    fun emitEnum(name: Any, values: Iterable<Any>) =
        render { enum(name, values) }

    fun emitStruct(name: Any, values: Iterable<Any>) =
        render { struct(name, values) }

    fun emitMatch(name: Any, values: Iterable<Any>) =
            render { match(emit, name, values) }

    fun Render.type(t1: Any, t2: Any) = +"type $t1 ${emitType.invoke(t2)} \n"

    private fun Render.structField(v: Any, t: Any) {
        when {
            v.toString().get(0) == '*' -> {
                goImport.invoke("sync")
                val vname = v.toString().drop(1)
                +"${vname}_lock sync.Mutex"
                +"$vname *$t"
            }
            else -> +"$v $t"
        }
    }

    // 增强语法
    fun Render.struct(name: Any, fields: Iterable<Any>) =
            "type $name struct".brace {
                val it = fields.iterator()
                while (it.hasNext()) {
                    val v = it.next()
                    val t = emitType.invoke(it.next())
                    structField(v, t)
                }
            }

    fun Render.enum(name: Any, values: Iterable<Any>) {
        type(name, "uint8".symbol)
        enumStruct(name, values)
        newline()
        func("(rs $name) String() string") {
            switch("rs") {
                for (v in values) {
                    +"case ${v.tag}: return \"${v.constructor}\""
                }
                +"""default: return "${name}Unknown" // Cannot panic."""
            }
        }
        newline()
    }

    fun Render.enumStruct(name: Any, values: Iterable<Any>) {
        fun enumValueList(idx: Int, vs: ISeq) {
            val constructor = vs.first()
            var fields = vs.next()
            var args = mutableListOf<String>()
            var params = StringBuilder()
            "type $constructor struct".brace {
                +name.toString()
                var i = 1
                while (fields != null) {
                    val t = emitType.invoke(fields.first())
                    args.add("_a$i $t ")
                    params.append(", _a$i")
                    +"_${i++} $t"
                    fields = fields.next()
                }
            }
            // new function
            func("New$constructor(${args.joinToString()}) $constructor") {
                +"return $constructor{${vs.tag}$params}"
            }
        }

        fun enumValue(idx: Int, value: Any) {
            +"const ${value.tag} = $name($idx)"
            when (value) {
                is IPersistentList ->
                    enumValueList(idx, value.seq())
            }
        }


        var idx = 0
        for (v in values) {
            enumValue(idx++, v)
        }
        newline()
    }

    private fun Any.fnType(b: StringBuilder) :StringBuilder {
        val types = (this as IPersistentList).seq()

        b.append("func(")
        var first = true
        var ps = types
        while (ps.count() > 1) {
            if (first) {
                first = false
            }
            else {
                b.append(", ")
            }
            b.append(emitType.invoke(ps.first()))
            ps = ps.next()
        }
        b.append(") ").append(emitType.invoke(ps.first()))

//    System.out.println("emitType " + this)
        return b
    }

    fun emitFnType(args: Any) = args.fnType(StringBuilder()).toString()
}

