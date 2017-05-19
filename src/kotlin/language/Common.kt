package language

/**
 * Created by liming on 2017/5/16.
 */
data class Render(val builder: StringBuilder, val indent: String) {
    fun newline() = builder.append("\n")

    operator fun String.unaryPlus() {
        builder.append(indent).append(this).append("\n")
    }

    fun indent(init: Render.()-> Unit) = Render(builder, indent + "    ").init()

    /**
     * { ... }
     */
    fun Any.brace(init: Render.()-> Unit) {
        +"$this {"
        indent(init)
        +"}"
    }

    /**
     * [ ... ]
     */
    fun Any.bracket(init: Render.()-> Unit) {
        +"$this ["
        indent(init)
        +"]"
    }

    /**
     * ( ... )
     */
    fun Any.paren(init: Render.()-> Unit) {
        +"$this ("
        indent(init)
        +")"
    }

    override fun toString() = builder.toString()
}

fun render(init: Render.()->Unit) : String {
    val r = Render(StringBuilder(), "")
    r.init()
    return r.toString()
}

