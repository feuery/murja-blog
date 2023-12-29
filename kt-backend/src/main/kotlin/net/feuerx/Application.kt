package net.feuerx

import io.ktor.server.application.*
import io.ktor.server.engine.*
import io.ktor.server.netty.*
import net.feuerx.plugins.*
import kotliquery.HikariCP

fun main() {
    // Class.forName("org.postgresql.Driver")
    HikariCP.default("jdbc:postgresql://localhost:5432/blogdb", "blogadmin", "blog")
    embeddedServer(Netty, port = 8080, host = "0.0.0.0", module = Application::module)
        .start(wait = true)
}

fun Application.module() {
    configureSerialization()
    configureDatabases()
    // configureMonitoring()
    configureHTTP()
    configureSecurity()
    configureRouting()
}
