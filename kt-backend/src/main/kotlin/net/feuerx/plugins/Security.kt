package net.feuerx.plugins

import io.ktor.server.application.*
import io.ktor.http.HttpStatusCode
import io.ktor.server.auth.*
import io.ktor.server.http.content.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import io.ktor.server.sessions.*
import io.ktor.server.request.receive
import kotliquery.*
import net.feuerx.model.User
import net.feuerx.model.getSessionUserById
import net.feuerx.model.LoginRequest
import net.feuerx.model.login
import net.feuerx.model.SessionUser

fun Application.configureSecurity() {
    data class Murja_Session(
	var user: SessionUser? = null
    )
    
    install(Sessions) {
        cookie<Murja_Session>("MURJA_SESSION", SessionStorageMemory()) {
            cookie.extensions["SameSite"] = "lax"
        }
    }
    
    routing {
	get ("/api/login/session") {
	    sessionOf(HikariCP.dataSource()).use { db_session ->
		val session = call.sessions.get<Murja_Session>() ?: Murja_Session()
		val usr = session.user
		if (usr != null) {
		    call.respond(usr) //getSessionUserById(db_session, session.userId))
		}
		else {
		    call.respond(HttpStatusCode.Unauthorized)
		}
	    }
	}

	post("/api/login/login") {
	    sessionOf(HikariCP.dataSource()).use { db_session ->
		val session = call.sessions.get<Murja_Session>() ?: Murja_Session()
		val params = call.receive<LoginRequest>()

		val user = login(db_session, params.username, params.password)

		if(user != null) {
		    session.user = user
		    call.sessions.set<Murja_Session>(session)
		    call.respond(user)
		}
		else {
		    call.respond(HttpStatusCode.Unauthorized)
		}
	    }
	}
    }
}
