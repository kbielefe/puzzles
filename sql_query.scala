import java.util.Date

def string(value: String)(query: String): String = query.replaceFirst("\\?", "'" + value + "'")
def int(value: Int)(query: String): String = query.replaceFirst("\\?", value.toString)
def date(value: Date)(query: String): String = query.replaceFirst("\\?", "'" + value.toString + "'")

val columns = List("description", "timestamp", "quantity")
val fields: List[String => String] = List(string("Cool product"), date(new Date()), int(12))

val wheres = columns map {_ + " = ?"} mkString " AND "
val query = s"SELECT * FROM table WHERE $wheres;"
val prepared = fields.foldLeft(query)((x, f) => f(x))
println(prepared)
