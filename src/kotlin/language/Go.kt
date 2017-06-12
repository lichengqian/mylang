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

    fun Render.type(t1: Any, t2: Any) = +"type $t1 ${emitType.invoke(t2)} \n"
    fun Render.declInterface(t1: Any, init: Render.() ->Unit) = "type $t1 interface".brace(init)

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
        declInterface(name) {
            +"tag$name() uint8"
            +"String() string"
        }
        enumStruct(name, values)
        newline()
    }

    fun Render.enumStruct(name: Any, values: Iterable<Any>) {
        fun enumValueList(idx: Int, vs: ISeq) {
            val constructor = vs.first()
            var fields = vs.next()
            var args = mutableListOf<String>()
            var params = StringBuilder()
            "type $constructor struct".brace {
                var i = 1
                while (fields != null) {
                    val t = emitType.invoke(fields.first())
                    args.add("_a$i $t ")
                    params.append(", _a$i")
                    +"_${i++} $t"
                    fields = fields.next()
                }
            }
            // tag function
            "func (p *$constructor) tag$name() uint8".brace {
                +"return $idx"
            }
            // string function
            "func (p *$constructor) String() string".brace {
                +"return \"$constructor\""
            }
        }

        fun enumValue(idx: Int, value: Any) {
            when (value) {
                is IPersistentList ->
                    enumValueList(idx, value.seq())
                else -> {
                    val constructor = value
                    +"type $constructor struct{}"
                    // tag function
                    "func (p $constructor) tag$name() uint8".brace {
                        +"return $idx"
                    }
                    // string function
                    "func (p $constructor) String() string".brace {
                        +"return \"$constructor\""
                    }
                }

            }
        }


        var idx = 0
        for (v in values) {
            enumValue(idx++, v)
        }
        newline()
    }

    fun macro_encode(name: Any, fields: Iterable<Any>) = 
        render {
            "func encode$name(n $name) uint8".brace {
                +"return n.tag$name()"
            }
        }

    fun macro_decode(name: Any, fields: Iterable<Any>) = 
        render {
            "func decode$name(tag uint8) $name".brace {
                "switch tag ".brace {
                    var i = 0
                    for (field in fields) {
                        +"case $i: return $field{}"
                        i++
                    }
                    +"default: return nil"
                }
            }
        }

}

