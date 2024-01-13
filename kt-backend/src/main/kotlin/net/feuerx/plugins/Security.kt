package net.feuerx.plugins

import com.auth0.jwt.JWT
import com.auth0.jwt.algorithms.Algorithm
import io.ktor.server.application.*
import io.ktor.http.HttpStatusCode
import io.ktor.server.auth.*
import io.ktor.server.auth.jwt.*
import io.ktor.server.http.content.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import io.ktor.server.sessions.*
import kotliquery.*
import net.feuerx.model.User
import net.feuerx.model.getSessionUserById

fun Application.configureSecurity() {
    data class Murja_Session(val userId: Long? = null)
    
    install(Sessions) {
        cookie<Murja_Session>("MURJA_SESSION") {
            cookie.extensions["SameSite"] = "lax"
        }
    }
    // Please read the jwt property from the config file if you are using EngineMain
    val jwtAudience = "murja-jwt"
    val jwtDomain = "https://feuerx.net/blog"
    val jwtRealm = "murja"
    val jwtSecret = java.util.UUID.randomUUID().toString()
    authentication {
        jwt {
            realm = jwtRealm
            verifier(
                JWT
                    .require(Algorithm.HMAC256(jwtSecret))
                    .withAudience(jwtAudience)
                    .withIssuer(jwtDomain)
                    .build()
            )
            validate { credential ->
                if (credential.payload.audience.contains(jwtAudience)) JWTPrincipal(credential.payload) else null
            }
        }
    }
    routing {
	get ("/api/login/session") {
	    sessionOf(HikariCP.dataSource()).use { db_session ->
		val session = call.sessions.get<Murja_Session>() ?: Murja_Session()
		if (session.userId != null) {
		    call.respond(getSessionUserById(db_session, session.userId))
		}
		else {
		    call.respond(HttpStatusCode.Unauthorized)
		}
	    }
	}
    }
}
